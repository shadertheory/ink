const std = @import("std");
const ink = @import("ink");
const inkui = @import("inkui").tui;
const ai = @import("ai.zig");
const compiler_sources = @import("compiler_sources");

const trace = ink.trace;
const mem_allocator = std.mem.Allocator;
const source_id = ink.compiler.source_id;

const phase_count = @typeInfo(trace.Phase).@"enum".fields.len;

const AnimMode = enum { intro, running, summary, outro };
const LayoutKind = enum { horizontal, square, vertical };
const LogKind = enum { plain, transform, ai };

const LineKey = struct {
    source_id: u64,
    line: usize,
};

const LineHint = struct {
    event_text: ?[]u8 = null,
    event_count: usize = 0,
    ai_text: ?[]u8 = null,
    ai_count: usize = 0,

    fn deinit(self: *LineHint, allocator: mem_allocator) void {
        if (self.event_text) |text| allocator.free(text);
        if (self.ai_text) |text| allocator.free(text);
    }
};

const PhaseStats = struct {
    start_ms: ?i64 = null,
    duration_ms: ?i64 = null,
    events: usize = 0,
};

const ByteFocus = struct {
    offset: usize,
    len: usize,
    ts_ms: i64,
};

const FocusSpan = struct {
    source_id: source_id,
    start: usize,
    end: usize,
};

const WrapRow = struct {
    end: usize,
    wrapped: bool,
};

const Counts = struct {
    tokens: usize = 0,
    parse_nodes: usize = 0,
    ast_nodes: usize = 0,
    macro_events: usize = 0,
    uir_nodes: usize = 0,
    mir_nodes: usize = 0,
    lir_nodes: usize = 0,
    instructions: usize = 0,
    byte_chunks: usize = 0,
    diagnostics: usize = 0,
    errors: usize = 0,
    warnings: usize = 0,
    notes: usize = 0,
};

const CompilerView = struct {
    path: ?[]const u8 = null,
    line: usize = 0,
    scroll: usize = 0,
};

const Ui = struct {
    term: inkui.Terminal,
    renderer: inkui.Renderer,
    canvas: inkui.Canvas,
    size: inkui.Size,
};

const Theme = struct {
    bg: inkui.Color,
    bg_alt: inkui.Color,
    fg: inkui.Color,
    dim: inkui.Color,
    accent: inkui.Color,
    accent2: inkui.Color,
    warn: inkui.Color,
    err: inkui.Color,
    panel_border: inkui.Style,
    panel_header: inkui.Style,
    panel_text: inkui.Style,
    panel_dim: inkui.Style,
    inlay: inkui.Style,
    highlight: inkui.Style,
    status: inkui.Style,
    status_dim: inkui.Style,
};

pub const RateConfig = struct {
    min_ms: u64,
    max_ms: u64,
};

pub const default_rate = RateConfig{
    .min_ms = 300,
    .max_ms = 2000,
};

const QueuedEvent = struct {
    event: trace.Event,
    tag: ?[]u8 = null,
    message: ?[]u8 = null,
    ts_ms: i64 = 0,

    fn init(allocator: mem_allocator, src: *const trace.Event, ts_ms: i64) !QueuedEvent {
        var item = QueuedEvent{ .event = src.*, .ts_ms = ts_ms };
        if (src.tag) |tag| {
            if (tag.len != 0) {
                item.tag = try allocator.dupe(u8, tag);
                item.event.tag = item.tag;
            } else {
                item.event.tag = null;
            }
        }
        if (src.message) |msg| {
            if (msg.len != 0) {
                item.message = try allocator.dupe(u8, msg);
                item.event.message = item.message;
            } else {
                item.event.message = null;
            }
        }
        return item;
    }

    fn deinit(self: *QueuedEvent, allocator: mem_allocator) void {
        if (self.tag) |buf| allocator.free(buf);
        if (self.message) |buf| allocator.free(buf);
    }
};

const EventQueue = struct {
    mutex: std.Thread.Mutex = .{},
    cond: std.Thread.Condition = .{},
    items: std.array_list.Managed(QueuedEvent),
    read_index: usize = 0,
    closed: bool = false,

    fn init(allocator: mem_allocator) EventQueue {
        return .{ .items = std.array_list.Managed(QueuedEvent).init(allocator) };
    }

    fn deinit(self: *EventQueue, allocator: mem_allocator) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        var idx = self.read_index;
        while (idx < self.items.items.len) : (idx += 1) {
            self.items.items[idx].deinit(allocator);
        }
        self.items.deinit();
    }

    fn len(self: *EventQueue) usize {
        self.mutex.lock();
        defer self.mutex.unlock();
        if (self.read_index >= self.items.items.len) return 0;
        return self.items.items.len - self.read_index;
    }

    fn push(self: *EventQueue, item: QueuedEvent) !void {
        self.mutex.lock();
        defer self.mutex.unlock();
        if (self.closed) return error.Closed;
        try self.items.append(item);
        self.cond.signal();
    }

    fn popWait(self: *EventQueue, stop: *std.atomic.Value(bool)) ?QueuedEvent {
        self.mutex.lock();
        defer self.mutex.unlock();
        while (self.read_index >= self.items.items.len) {
            if (self.closed or stop.load(.seq_cst)) return null;
            self.cond.wait(&self.mutex);
        }
        const item = self.items.items[self.read_index];
        self.read_index += 1;
        self.compact();
        return item;
    }

    fn popNoWait(self: *EventQueue) ?QueuedEvent {
        self.mutex.lock();
        defer self.mutex.unlock();
        if (self.read_index >= self.items.items.len) return null;
        const item = self.items.items[self.read_index];
        self.read_index += 1;
        self.compact();
        return item;
    }

    fn close(self: *EventQueue) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        self.closed = true;
        self.cond.signal();
    }

    fn compact(self: *EventQueue) void {
        if (self.read_index <= 1024 or self.read_index < self.items.items.len / 2) return;
        const remaining = self.items.items[self.read_index..];
        std.mem.copyForwards(QueuedEvent, self.items.items[0..remaining.len], remaining);
        self.items.items.len = remaining.len;
        self.read_index = 0;
    }
};

fn parseBool(text: []const u8) ?bool {
    const trimmed = std.mem.trim(u8, text, " \t\r\n");
    if (trimmed.len == 0) return null;
    if (std.ascii.eqlIgnoreCase(trimmed, "1") or
        std.ascii.eqlIgnoreCase(trimmed, "true") or
        std.ascii.eqlIgnoreCase(trimmed, "yes") or
        std.ascii.eqlIgnoreCase(trimmed, "on"))
    {
        return true;
    }
    if (std.ascii.eqlIgnoreCase(trimmed, "0") or
        std.ascii.eqlIgnoreCase(trimmed, "false") or
        std.ascii.eqlIgnoreCase(trimmed, "no") or
        std.ascii.eqlIgnoreCase(trimmed, "off"))
    {
        return false;
    }
    return null;
}

fn envBool(allocator: mem_allocator, name: []const u8, default_value: bool) bool {
    const val = std.process.getEnvVarOwned(allocator, name) catch |err| switch (err) {
        error.EnvironmentVariableNotFound => return default_value,
        else => return default_value,
    };
    defer allocator.free(val);
    return parseBool(val) orelse default_value;
}

pub const Anim = struct {
    allocator: mem_allocator,
    ui: ?Ui = null,
    source_cache: inkui.SourceCache,
    source_map: std.AutoHashMap(source_id, []const u8),
    source_view: inkui.SourceView,
    compiler_view: CompilerView,
    line_hints: std.AutoHashMap(LineKey, LineHint),
    phase_logs: [phase_count]inkui.LogBuffer,
    ir_log: inkui.LogBuffer,
    byte_log: inkui.LogBuffer,
    byte_bytes: std.array_list.Managed(u8),
    byte_focus: ?ByteFocus = null,
    focus_span: ?FocusSpan = null,
    diag_log: inkui.LogBuffer,
    ai_log: inkui.LogBuffer,
    summary_lines: std.array_list.Managed([]u8),
    counts: Counts = .{},
    phase_stats: [phase_count]PhaseStats,
    current_phase: trace.Phase = .front_end,
    stack: std.array_list.Managed([]const u8),
    ai_engine: ai.AiEngine,
    ai_notes: std.array_list.Managed(ai.AiNote),
    last_ai: ?[]u8 = null,
    last_llm: ?[]u8 = null,
    ollama_status: ?[]u8 = null,
    ollama_progress: ?f32 = null,
    ollama_active: bool = false,
    json: ?TraceJsonWriter = null,
    mode: AnimMode = .intro,
    start_ms: i64 = 0,
    mode_start_ms: i64 = 0,
    last_render_ms: i64 = 0,
    last_event_ms: i64 = 0,
    last_ai_event_ms: i64 = 0,
    next_render_ms: i64 = 0,
    compile_end_ms: ?i64 = null,
    rate_min_ms: u64 = default_rate.min_ms,
    rate_max_ms: u64 = default_rate.max_ms,
    rng: std.Random.DefaultPrng,
    event_queue: EventQueue,
    playback_thread: ?std.Thread = null,
    playback_stop: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),
    playback_async: bool = false,

    pub fn init(
        allocator: mem_allocator,
        sources: []const ink.compiler.source,
        enable_ui: bool,
        trace_path: ?[]const u8,
        ai_config: ai.AiConfig,
        rate: RateConfig,
        async_playback: bool,
    ) !Anim {
        _ = async_playback;
        const seed = @as(u64, @bitCast(std.time.milliTimestamp()));
        const rng = std.Random.DefaultPrng.init(seed);
        var source_cache = inkui.SourceCache.init(allocator);
        var source_map = std.AutoHashMap(source_id, []const u8).init(allocator);
        var first_path: ?[]u8 = null;
        for (sources) |src| {
            const file = try source_cache.put(src.path, src.text);
            try source_map.put(src.id, file.path);
            if (first_path == null) first_path = file.path;
        }
        // Compiler reflection panel disabled.

        var ai_cfg = ai_config;
        if (ai_cfg.use_ollama and ai_cfg.timeout_ms < 15000) {
            ai_cfg.timeout_ms = 15000;
        }

        var logs: [phase_count]inkui.LogBuffer = undefined;
        var stats: [phase_count]PhaseStats = undefined;
        var i: usize = 0;
        while (i < phase_count) : (i += 1) {
            logs[i] = inkui.LogBuffer.init(allocator, 0);
            stats[i] = .{};
        }

        var anim = Anim{
            .allocator = allocator,
            .source_cache = source_cache,
            .source_map = source_map,
            .source_view = .{},
            .compiler_view = .{},
            .line_hints = std.AutoHashMap(LineKey, LineHint).init(allocator),
            .phase_logs = logs,
            .ir_log = inkui.LogBuffer.init(allocator, 0),
            .byte_log = inkui.LogBuffer.init(allocator, 0),
            .diag_log = inkui.LogBuffer.init(allocator, 0),
            .ai_log = inkui.LogBuffer.init(allocator, 0),
            .byte_bytes = std.array_list.Managed(u8).init(allocator),
            .summary_lines = std.array_list.Managed([]u8).init(allocator),
            .phase_stats = stats,
            .stack = std.array_list.Managed([]const u8).init(allocator),
            .ai_engine = ai.AiEngine.init(allocator, ai_cfg),
            .ai_notes = std.array_list.Managed(ai.AiNote).init(allocator),
            .start_ms = std.time.milliTimestamp(),
            .mode_start_ms = std.time.milliTimestamp(),
            .next_render_ms = std.time.milliTimestamp(),
            .rate_min_ms = rate.min_ms,
            .rate_max_ms = rate.max_ms,
            .rng = rng,
            .event_queue = EventQueue.init(allocator),
            // Disable async playback to avoid rendering from a background thread.
            .playback_async = false,
        };

        if (first_path) |path| {
            anim.source_view.path = path;
            anim.source_view.line = 1;
        }

        if (trace_path) |path| {
            anim.json = try TraceJsonWriter.init(allocator, path);
        }

        if (enable_ui) {
            const ui_alt = envBool(allocator, "INKC_UI_ALT", false);
            const ui_clear = envBool(allocator, "INKC_UI_CLEAR", false);
            var term = try inkui.Terminal.initWithOptions(.{
                .raw = false,
                .alt = ui_alt,
                .wrap = false,
                .cursor = false,
                .mouse = false,
                .clear = ui_clear,
                .isig = true,
            });
            const size = term.size();
            const theme = makeTheme(.front_end, anim.mode_start_ms);
            const canvas = try inkui.Canvas.init(
                allocator,
                size.cols,
                size.rows,
                theme.panel_text,
            );
            const renderer = inkui.Renderer{ .writer = term.writer() };
            anim.ui = .{ .term = term, .renderer = renderer, .canvas = canvas, .size = size };
            if (anim.ui) |*ui| {
                // Fix writer buffer after copying Terminal into Anim.
                ui.term.out_writer = ui.term.out_file.writer(ui.term.out_buf[0..]);
                ui.renderer.writer = ui.term.writer();
            }
        }

        if (anim.ui != null and ai_cfg.enable and ai_cfg.use_ollama) {
            anim.ollama_active = true;
            setOllamaStatus(&anim, "checking model", null);
            renderFrame(&anim);
            ai.ensureOllamaReady(allocator, ai_cfg, &anim, ollamaProgress) catch {
                setOllamaStatus(&anim, "ollama error", null);
            };
            var attempt: u32 = 0;
            var handshake_ok = false;
            while (attempt < 3 and !handshake_ok) : (attempt += 1) {
                const status = if (attempt == 0) "llm handshake" else "llm handshake retry";
                setOllamaStatus(&anim, status, null);
                renderFrame(&anim);
                const response = ai.ollamaGenerate(allocator, ai_cfg, "Say READY in one short sentence.") catch null;
                if (response) |text| {
                    if (anim.last_llm) |old| allocator.free(old);
                    anim.last_llm = allocator.dupe(u8, text) catch null;
                    allocator.free(text);
                    handshake_ok = true;
                } else {
                    std.Thread.sleep(500 * std.time.ns_per_ms);
                }
            }
            anim.ollama_active = false;
            if (!handshake_ok) {
                setOllamaStatus(&anim, "llm handshake failed", null);
                if (anim.last_llm) |old| allocator.free(old);
                anim.last_llm = allocator.dupe(u8, "ollama handshake failed") catch null;
            } else {
                setOllamaStatus(&anim, "ready", 1.0);
            }
            renderFrame(&anim);
        }

        return anim;
    }

    pub fn startAi(self: *Anim) !void {
        try self.ai_engine.start();
    }

    pub fn startPlayback(self: *Anim) !void {
        if (!self.playback_async or self.ui == null) return;
        if (self.playback_thread != null) return;
        self.playback_thread = try std.Thread.spawn(.{}, playbackMain, .{self});
    }

    pub fn deinit(self: *Anim) void {
        if (self.playback_thread) |th| {
            self.playback_stop.store(true, .seq_cst);
            self.event_queue.close();
            th.join();
            self.playback_thread = null;
        }
        if (self.ui) |*ui| {
            ui.canvas.deinit();
            ui.term.deinit();
        }
        var it = self.line_hints.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
        }
        self.line_hints.deinit();
        var i: usize = 0;
        while (i < phase_count) : (i += 1) {
            self.phase_logs[i].deinit();
        }
        self.ir_log.deinit();
        self.byte_log.deinit();
        self.byte_bytes.deinit();
        self.diag_log.deinit();
        self.ai_log.deinit();
        for (self.summary_lines.items) |line| self.allocator.free(line);
        self.summary_lines.deinit();
        self.source_cache.deinit();
        self.source_map.deinit();
        self.stack.deinit();
        if (self.last_ai) |text| self.allocator.free(text);
        if (self.last_llm) |text| self.allocator.free(text);
        if (self.ollama_status) |text| self.allocator.free(text);
        for (self.ai_notes.items) |*note| note.deinit(self.allocator);
        self.ai_notes.deinit();
        self.ai_engine.deinit();
        if (self.json) |*writer| writer.deinit();
        self.event_queue.deinit(self.allocator);
    }

    pub fn sink(self: *Anim) trace.Sink {
        return .{ .ctx = self, .emit = onTraceEvent };
    }

    pub fn finish(self: *Anim, result: *const ink.compiler.compile_result) void {
        self.compile_end_ms = std.time.milliTimestamp();
        if (self.playback_thread) |th| {
            self.event_queue.close();
            th.join();
            self.playback_thread = null;
        }
        if (self.ui == null) return;
        drainAiNotes(self);
        self.mode = .summary;
        self.mode_start_ms = std.time.milliTimestamp();
        buildSummary(self, result) catch {};
        renderSummaryFrames(self);
        self.mode = .outro;
        self.mode_start_ms = std.time.milliTimestamp();
        renderOutroFrames(self);
        self.ai_engine.stop();
    }
};

fn onTraceEvent(ctx: *anyopaque, event: *const trace.Event) void {
    const self: *Anim = @ptrCast(@alignCast(ctx));
    if (self.playback_async) {
        enqueueEvent(self, event) catch {};
        return;
    }
    handleEvent(self, event) catch {};
}

fn phaseIndex(phase: trace.Phase) usize {
    return @intFromEnum(phase);
}

fn handleEvent(self: *Anim, event: *const trace.Event) !void {
    const now = std.time.milliTimestamp();
    try handleEventCore(self, event, now);
    maybeRender(self, now, event);
}

fn handleEventCore(self: *Anim, event: *const trace.Event, event_ms: i64) !void {
    self.last_event_ms = event_ms;

    switch (event.kind) {
        .phase_start => {
            self.current_phase = event.phase;
            var stats = &self.phase_stats[phaseIndex(event.phase)];
            stats.start_ms = event_ms;
        },
        .phase_end => {
            var stats = &self.phase_stats[phaseIndex(event.phase)];
            if (stats.start_ms) |start_ms| {
                stats.duration_ms = event_ms - start_ms;
            }
        },
        .stack_push => {
            if (event.tag) |tag| {
                _ = self.stack.append(tag) catch {};
            }
        },
        .stack_pop => {
            if (self.stack.items.len > 0) {
                _ = self.stack.pop();
            }
        },
        else => {},
    }

    var stats = &self.phase_stats[phaseIndex(event.phase)];
    stats.events += 1;

    updateCounts(self, event);
    if (event.kind == .bytecode) {
        updateByteView(self, event) catch {};
    }
    try maybeLogEvent(self, event);
    if (!self.ai_engine.config.enable) {
        try logAiBaseline(self, event);
    }
    try maybeUpdateFocus(self, event);
    try maybeUpdateCompilerView(self, event);
    if (self.ai_engine.config.enable) {
        try maybeEmitAi(self, event);
    }
    drainAiNotes(self);
}

fn maybeRender(self: *Anim, now: i64, event: *const trace.Event) void {
    if (self.ui == null) return;
    const force = event.kind == .diagnostic or event.kind == .phase_end or event.kind == .phase_start;
    if (force or now >= self.next_render_ms) {
        updateModeForRender(self, now);
        renderFrame(self);
        self.last_render_ms = now;
        self.next_render_ms = now + nextRenderDelayMs(self);
    }
}

fn updateModeForRender(self: *Anim, now: i64) void {
    if (self.mode == .intro and now - self.mode_start_ms > 250) {
        self.mode = .running;
        self.mode_start_ms = now;
    }
}

fn enqueueEvent(self: *Anim, event: *const trace.Event) !void {
    const ts_ms = std.time.milliTimestamp();
    var item = try QueuedEvent.init(self.allocator, event, ts_ms);
    self.event_queue.push(item) catch |err| {
        item.deinit(self.allocator);
        return err;
    };
}

fn renderTick(self: *Anim) i64 {
    if (self.ui == null) return 0;
    const now = std.time.milliTimestamp();
    updateModeForRender(self, now);
    renderFrame(self);
    self.last_render_ms = now;
    const delay = nextRenderDelayMs(self);
    self.next_render_ms = now + delay;
    return delay;
}

fn playbackBatchSize(queue_len: usize) usize {
    if (queue_len == 0) return 0;
    var target_frames: usize = queue_len / 200;
    if (target_frames < 6) target_frames = 6;
    if (target_frames > 24) target_frames = 24;
    return (queue_len + target_frames - 1) / target_frames;
}

fn playbackMain(self: *Anim) void {
    while (true) {
        const first_opt = self.event_queue.popWait(&self.playback_stop);
        if (first_opt == null) break;
        var first = first_opt.?;
        handleEventCore(self, &first.event, first.ts_ms) catch {};
        first.deinit(self.allocator);

        const queued = self.event_queue.len() + 1;
        const batch = playbackBatchSize(queued);
        var processed: usize = 1;
        while (processed < batch) : (processed += 1) {
            if (self.event_queue.popNoWait()) |next_item| {
                var item = next_item;
                handleEventCore(self, &item.event, item.ts_ms) catch {};
                item.deinit(self.allocator);
            } else break;
        }

        if (self.ui != null) {
            const delay_ms = renderTick(self);
            if (delay_ms > 0) {
                std.Thread.sleep(@as(u64, @intCast(delay_ms)) * std.time.ns_per_ms);
            }
        }
    }
}

fn nextRenderDelayMs(self: *Anim) i64 {
    const min_ms = self.rate_min_ms;
    const max_ms = if (self.rate_max_ms < min_ms) min_ms else self.rate_max_ms;
    if (max_ms == min_ms) return @as(i64, @intCast(min_ms));
    const span = max_ms - min_ms;
    const jitter = self.rng.random().uintLessThan(u64, span + 1);
    return @as(i64, @intCast(min_ms + jitter));
}

fn updateCounts(self: *Anim, event: *const trace.Event) void {
    switch (event.kind) {
        .token => self.counts.tokens += 1,
        .parse_node => self.counts.parse_nodes += 1,
        .ast_node => self.counts.ast_nodes += 1,
        .macro_event => self.counts.macro_events += 1,
        .uir_node => self.counts.uir_nodes += 1,
        .mir_node => self.counts.mir_nodes += 1,
        .lir_node => self.counts.lir_nodes += 1,
        .instruction => self.counts.instructions += 1,
        .bytecode => self.counts.byte_chunks += 1,
        .diagnostic => {
            self.counts.diagnostics += 1;
            if (event.tag) |tag| {
                if (std.mem.eql(u8, tag, "error")) self.counts.errors += 1;
                if (std.mem.eql(u8, tag, "warning")) self.counts.warnings += 1;
                if (std.mem.eql(u8, tag, "note")) self.counts.notes += 1;
            }
        },
        else => {},
    }
}

fn maybeLogEvent(self: *Anim, event: *const trace.Event) !void {
    if (event.kind == .stack_push or event.kind == .stack_pop) return;
    var buf: [512]u8 = undefined;
    const line = formatEventLine(event, &buf);
    const collapse = shouldCollapseEvent(event);
    try appendLogLine(&self.phase_logs[phaseIndex(event.phase)], line, collapse);
    switch (event.kind) {
        .uir_node, .mir_node, .lir_node, .instruction => try appendLogLine(&self.ir_log, line, collapse),
        .bytecode => try appendLogLine(&self.byte_log, line, collapse),
        .diagnostic => try appendLogLine(&self.diag_log, line, collapse),
        else => {},
    }
    if (self.json) |*writer| {
        writer.writeEvent(event, self.stack.items) catch {};
    }
}

fn logAiBaseline(self: *Anim, event: *const trace.Event) !void {
    if (self.ui == null) return;
    var buf: [768]u8 = undefined;
    const line = formatAiBaseline(self, event, &buf);
    if (line.len == 0) return;
    const collapse = shouldCollapseEvent(event);
    if (!std.unicode.utf8ValidateSlice(line)) {
        const safe = sanitizeUtf8(self.allocator, line) catch return;
        defer self.allocator.free(safe);
        appendLogLine(&self.ai_log, safe, collapse) catch {};
        return;
    }
    appendLogLine(&self.ai_log, line, collapse) catch {};
}

fn formatAiBaseline(self: *Anim, event: *const trace.Event, buf: []u8) []const u8 {
    const phase = @tagName(event.phase);
    const kind = @tagName(event.kind);
    const tag = event.tag orelse "";
    const msg = event.message orelse "";
    if (event.kind == .step and std.mem.eql(u8, tag, "macro_expand.node")) {
        if (event.source_id != null) {
            if (self.source_map.get(event.source_id.?)) |path| {
                return std.fmt.bufPrint(
                    buf,
                    "analysis {s} {s} {s} @{s}",
                    .{ phase, kind, tag, path },
                ) catch phase;
            }
        }
    }
    if (event.source_id != null and event.span != null) {
        if (sourceLineInfo(self, event.source_id.?, event.span.?) catch null) |info| {
            const snippet = truncateSlice(info.text, 120);
            if (self.source_map.get(event.source_id.?)) |path| {
                return std.fmt.bufPrint(
                    buf,
                    "analysis {s} {s} {s} @{s}:{d}:{d} {s}",
                    .{ phase, kind, tag, path, info.line_no, info.col, snippet },
                ) catch phase;
            }
            return std.fmt.bufPrint(
                buf,
                "analysis {s} {s} {s} @L{d}:{d} {s}",
                .{ phase, kind, tag, info.line_no, info.col, snippet },
            ) catch phase;
        }
    }
    if (event.file.len != 0 and event.line != 0) {
        if (msg.len != 0) {
            return std.fmt.bufPrint(
                buf,
                "analysis {s} {s} {s} @{s}:{d} {s}",
                .{ phase, kind, tag, event.file, event.line, truncateSlice(msg, 120) },
            ) catch phase;
        }
        return std.fmt.bufPrint(
            buf,
            "analysis {s} {s} {s} @{s}:{d}",
            .{ phase, kind, tag, event.file, event.line },
        ) catch phase;
    }
    if (msg.len != 0) {
        return std.fmt.bufPrint(
            buf,
            "analysis {s} {s} {s} {s}",
            .{ phase, kind, tag, truncateSlice(msg, 120) },
        ) catch phase;
    }
    if (tag.len != 0) {
        return std.fmt.bufPrint(buf, "analysis {s} {s} {s}", .{ phase, kind, tag }) catch phase;
    }
    return std.fmt.bufPrint(buf, "analysis {s} {s}", .{ phase, kind }) catch phase;
}

fn maybeUpdateFocus(self: *Anim, event: *const trace.Event) !void {
    if (event.source_id == null or event.span == null) return;
    const sid = event.source_id.?;
    const span = event.span.?;
    const path = self.source_map.get(sid) orelse return;
    const file_opt = self.source_cache.get(path) catch return;
    const file = file_opt orelse return;
    if (file.lines.len == 0) return;

    var start = if (span.start < span.end) span.start else span.end;
    var end = if (span.start < span.end) span.end else span.start;
    if (start > file.text.len) start = file.text.len;
    if (end > file.text.len) end = file.text.len;
    if (end < start) end = start;
    self.focus_span = .{ .source_id = sid, .start = start, .end = end };

    const start_idx = findLine(file.lines, start);
    const end_probe = if (end > start and end > 0) end - 1 else start;
    const end_idx = findLine(file.lines, end_probe);
    const focus_idx = (start_idx + end_idx) / 2;
    self.source_view.path = @constCast(file.path);
    self.source_view.line = focus_idx + 1;

    var hint_buf: [256]u8 = undefined;
    const hint_text = formatEventHint(event, &hint_buf);
    try updateLineHint(self, sid, start_idx + 1, hint_text, false);
}

fn maybeUpdateCompilerView(self: *Anim, event: *const trace.Event) !void {
    _ = self;
    _ = event;
}

fn maybeEmitAi(self: *Anim, event: *const trace.Event) !void {
    if (!self.ai_engine.config.enable) return;
    const req_kind: ai.AiRequestKind = switch (event.kind) {
        .diagnostic => .diagnostic,
        .phase_end => .phase_summary,
        else => .event,
    };
    if (req_kind == .event) {
        const now = std.time.milliTimestamp();
        if (self.last_ai_event_ms != 0 and now - self.last_ai_event_ms < 200) return;
        if (self.ai_engine.queueLen() > 8) return;
        self.last_ai_event_ms = now;
    }
    var req = ai.AiRequest{
        .kind = req_kind,
        .phase = event.phase,
        .tag = event.tag,
        .message = event.message,
        .source_id = event.source_id,
        .span = event.span,
        .timestamp_ms = std.time.milliTimestamp(),
    };

    if (event.source_id != null and event.span != null) {
        if (try sourceLineInfo(self, event.source_id.?, event.span.?)) |info| {
            req.source_path = try self.allocator.dupe(u8, info.path);
            req.source_line = try self.allocator.dupe(u8, info.text);
        }
    }

    // Compiler reflection disabled.

    if (self.stack.items.len > 0) {
        req.stack_text = try joinStack(self.allocator, self.stack.items);
    }

    self.ai_engine.push(req);
}

fn drainAiNotes(self: *Anim) void {
    self.ai_engine.drain(&self.ai_notes);
    if (self.ai_notes.items.len == 0) return;
    for (self.ai_notes.items) |note| {
        var safe_text = note.message;
        var safe_owned: ?[]u8 = null;
        if (!std.unicode.utf8ValidateSlice(note.message)) {
            safe_text = sanitizeUtf8(self.allocator, note.message) catch note.message;
            if (safe_text.ptr != note.message.ptr) {
                safe_owned = safe_text;
            }
        }
        const prefix = if (note.from_llm) "llm" else "ai";
        var buf: [512]u8 = undefined;
        const line = std.fmt.bufPrint(&buf, "{s}: {s}", .{ prefix, safe_text }) catch "ai";
        self.phase_logs[phaseIndex(note.phase)].add(line) catch {};
        var ai_buf: [640]u8 = undefined;
        const ai_line = std.fmt.bufPrint(&ai_buf, "{s} {s}: {s}", .{ prefix, @tagName(note.phase), safe_text }) catch safe_text;
        self.ai_log.add(ai_line) catch {};
        if (note.from_llm) {
            if (self.last_llm) |text| self.allocator.free(text);
            self.last_llm = self.allocator.dupe(u8, safe_text) catch null;
        }
        if (note.source_id != null and note.span != null) {
            if (sourceLineInfo(self, note.source_id.?, note.span.?) catch null) |info| {
                updateLineHint(self, note.source_id.?, info.line_no, safe_text, true) catch {};
            }
        }
        if (self.last_ai) |text| self.allocator.free(text);
        self.last_ai = self.allocator.dupe(u8, safe_text) catch null;
        var owned = note;
        owned.deinit(self.allocator);
        if (safe_owned) |buf_owned| self.allocator.free(buf_owned);
    }
    self.ai_notes.clearRetainingCapacity();
}

fn setOllamaStatus(self: *Anim, status: []const u8, progress: ?f32) void {
    if (self.ollama_status) |text| self.allocator.free(text);
    self.ollama_status = self.allocator.dupe(u8, status) catch null;
    if (progress) |val| self.ollama_progress = val;
}

fn ollamaProgress(ctx: ?*anyopaque, progress: ai.PullProgress) void {
    if (ctx == null) return;
    const self: *Anim = @ptrCast(@alignCast(ctx.?));
    var ratio: ?f32 = null;
    if (progress.completed != null and progress.total != null and progress.total.? > 0) {
        ratio = @as(f32, @floatFromInt(progress.completed.?)) / @as(f32, @floatFromInt(progress.total.?));
    }
    self.ollama_active = true;
    setOllamaStatus(self, progress.status, ratio);
    renderFrame(self);
}

fn renderFrame(self: *Anim) void {
    if (self.ui) |*ui| {
        const now = std.time.milliTimestamp();
        const size = ui.term.size();
        if (size.cols != ui.size.cols or size.rows != ui.size.rows) {
            ui.canvas.resize(size.cols, size.rows) catch {};
            ui.size = size;
        }
        // Rebind writer to the current Terminal storage in case Anim moved.
        ui.term.out_writer = ui.term.out_file.writer(ui.term.out_buf[0..]);
        ui.renderer.writer = ui.term.writer();
        const theme = makeTheme(self.current_phase, now);
        const rect_full = inkui.Rect{ .x = 0, .y = 0, .w = @intCast(ui.size.cols), .h = @intCast(ui.size.rows) };
        ui.canvas.fill(rect_full, ' ', theme.panel_text);
        drawPanels(self, &ui.canvas, theme);
        if (self.mode == .intro or self.mode == .outro) {
            drawParticles(self, &ui.canvas, theme, now);
        }
        ui.renderer.render(&ui.canvas) catch {};
    }
}

fn drawPanels(self: *Anim, canvas: *inkui.Canvas, theme: Theme) void {
    const size = canvasSize(canvas);
    const status_h: i32 = 2;
    const body_h = @max(@as(i32, 0), size.h - status_h);
    const status_rect = inkui.Rect{ .x = 0, .y = body_h, .w = size.w, .h = status_h };

    switch (layoutKind(size)) {
        .horizontal => {
            const widths = splitWidth(size.w, 55, 40);
            const left_w = widths[0];
            const right_w = widths[1];
            const right_x = left_w;

            var left_heights: [2]i32 = .{ 0, 0 };
            splitHeights(body_h, &.{ 66, 34 }, 4, left_heights[0..]);
            const source_h = left_heights[0];

            var right_heights: [3]i32 = .{ 0, 0, 0 };
            splitHeights(body_h, &.{ 34, 33, 33 }, 3, right_heights[0..]);
            const transform_h = right_heights[0];
            const ir_h = right_heights[1];
            const ai_h = right_heights[2];

            const source_rect = inkui.Rect{ .x = 0, .y = 0, .w = left_w, .h = source_h };
            const transform_rect = inkui.Rect{ .x = right_x, .y = 0, .w = right_w, .h = transform_h };
            const ir_rect = inkui.Rect{ .x = right_x, .y = transform_h, .w = right_w, .h = ir_h };
            const ai_rect = inkui.Rect{ .x = right_x, .y = transform_h + ir_h, .w = right_w, .h = ai_h };

            drawPanelFrame(canvas, source_rect, theme, inkui.Icons.file ++ " source");
            drawPanelFrame(canvas, transform_rect, theme, inkui.Icons.events ++ " transform");
            drawPanelFrame(canvas, ir_rect, theme, inkui.Icons.memory ++ " ir/byte");
            drawPanelFrame(canvas, ai_rect, theme, inkui.Icons.repl ++ " ai analysis");

            drawSourcePane(self, canvas, source_rect, theme);
            drawTransformPane(self, canvas, transform_rect, theme);
            drawIrPane(self, canvas, ir_rect, theme);
            drawAiPane(self, canvas, ai_rect, theme);
        },
        .square => {
            var heights: [2]i32 = .{ 0, 0 };
            splitHeights(body_h, &.{ 62, 38 }, 4, heights[0..]);
            const source_h = heights[0];
            const bottom_h = heights[1];

            const source_rect = inkui.Rect{ .x = 0, .y = 0, .w = size.w, .h = source_h };
            drawPanelFrame(canvas, source_rect, theme, inkui.Icons.file ++ " source");
            drawSourcePane(self, canvas, source_rect, theme);

            const row_h = @max(@as(i32, 4), @divTrunc(bottom_h, 2));
            const row_h2 = bottom_h - row_h;
            const col_w = @max(@as(i32, 24), @divTrunc(size.w, 2));
            const col_w2 = size.w - col_w;
            const row1_y = source_h;
            const row2_y = source_h + row_h;
            const col2_x = col_w;

            const transform_rect = inkui.Rect{ .x = 0, .y = row1_y, .w = col_w, .h = row_h };
            const ir_rect = inkui.Rect{ .x = 0, .y = row2_y, .w = col_w, .h = row_h2 };
            const ai_rect = inkui.Rect{ .x = col2_x, .y = row2_y, .w = col_w2, .h = row_h2 };

            drawPanelFrame(canvas, transform_rect, theme, inkui.Icons.events ++ " transform");
            drawPanelFrame(canvas, ir_rect, theme, inkui.Icons.memory ++ " ir/byte");
            drawPanelFrame(canvas, ai_rect, theme, inkui.Icons.repl ++ " ai analysis");

            drawTransformPane(self, canvas, transform_rect, theme);
            drawIrPane(self, canvas, ir_rect, theme);
            drawAiPane(self, canvas, ai_rect, theme);
        },
        .vertical => {
            var heights: [5]i32 = .{ 0, 0, 0, 0, 0 };
            splitHeights(body_h, &.{ 48, 13, 13, 13, 13 }, 3, heights[0..]);
            const source_h = heights[0];
            const transform_h = heights[1];
            const ir_h = heights[3];
            const ai_h = heights[4];

            var y: i32 = 0;
            const source_rect = inkui.Rect{ .x = 0, .y = y, .w = size.w, .h = source_h };
            y += source_h;
            const transform_rect = inkui.Rect{ .x = 0, .y = y, .w = size.w, .h = transform_h };
            y += transform_h;
            const ir_rect = inkui.Rect{ .x = 0, .y = y, .w = size.w, .h = ir_h };
            y += ir_h;
            const ai_rect = inkui.Rect{ .x = 0, .y = y, .w = size.w, .h = ai_h };

            drawPanelFrame(canvas, source_rect, theme, inkui.Icons.file ++ " source");
            drawPanelFrame(canvas, transform_rect, theme, inkui.Icons.events ++ " transform");
            drawPanelFrame(canvas, ir_rect, theme, inkui.Icons.memory ++ " ir/byte");
            drawPanelFrame(canvas, ai_rect, theme, inkui.Icons.repl ++ " ai analysis");

            drawSourcePane(self, canvas, source_rect, theme);
            drawTransformPane(self, canvas, transform_rect, theme);
            drawIrPane(self, canvas, ir_rect, theme);
            drawAiPane(self, canvas, ai_rect, theme);
        },
    }

    drawStatusBar(self, canvas, status_rect, theme);
}

fn drawPanelFrame(canvas: *inkui.Canvas, rect: inkui.Rect, theme: Theme, title: []const u8) void {
    if (rect.w <= 1 or rect.h <= 1) return;
    canvas.fill(rect, ' ', theme.panel_text);
    const border = theme.panel_border;
    var x: i32 = rect.x;
    while (x < rect.x + rect.w) : (x += 1) {
        canvas.putBorder(x, rect.y, inkui.Border.h, border);
        canvas.putBorder(x, rect.y + rect.h - 1, inkui.Border.h, border);
    }
    var y: i32 = rect.y;
    while (y < rect.y + rect.h) : (y += 1) {
        canvas.putBorder(rect.x, y, inkui.Border.v, border);
        canvas.putBorder(rect.x + rect.w - 1, y, inkui.Border.v, border);
    }
    canvas.putBorder(rect.x, rect.y, inkui.Border.tl, border);
    canvas.putBorder(rect.x + rect.w - 1, rect.y, inkui.Border.tr, border);
    canvas.putBorder(rect.x, rect.y + rect.h - 1, inkui.Border.bl, border);
    canvas.putBorder(rect.x + rect.w - 1, rect.y + rect.h - 1, inkui.Border.br, border);
    const header = inkui.Rect{ .x = rect.x + 1, .y = rect.y, .w = rect.w - 2, .h = 1 };
    canvas.fill(header, ' ', theme.panel_header);
    canvas.writeClipped(rect.x + 2, rect.y, rect.w - 4, title, theme.panel_header);
}

fn layoutKind(size: inkui.Rect) LayoutKind {
    if (size.w <= 0 or size.h <= 0) return .horizontal;
    const w = @as(i64, size.w);
    const h = @as(i64, size.h);
    const ratio100 = @divTrunc(w * 100, h);
    if (ratio100 >= 125) return .horizontal;
    if (ratio100 <= 80) return .vertical;
    return .square;
}

fn splitWidth(total: i32, left_pct: i32, min_left: i32) [2]i32 {
    if (total <= 0) return .{ 0, 0 };
    var left = @divTrunc(total * left_pct, 100);
    if (left < min_left) left = min_left;
    if (left > total) left = total;
    const right = total - left;
    return .{ left, right };
}

fn splitHeights(total: i32, weights: []const i32, min_h: i32, out: []i32) void {
    if (out.len == 0 or total <= 0) {
        for (out) |*h| h.* = 0;
        return;
    }
    var sum_w: i32 = 0;
    for (weights) |w| sum_w += w;
    var remaining = total;
    var i: usize = 0;
    while (i < out.len) : (i += 1) {
        if (i + 1 == out.len) {
            out[i] = remaining;
        } else {
            const w = if (i < weights.len) weights[i] else 1;
            out[i] = @divTrunc(total * w, sum_w);
            remaining -= out[i];
        }
    }
    var min_eff = min_h;
    const count = @as(i32, @intCast(out.len));
    if (min_eff < 0) min_eff = 0;
    if (total < min_eff * count) {
        min_eff = if (count > 0) @divTrunc(total, count) else 0;
    }
    var sum: i32 = 0;
    for (out) |*h| {
        if (h.* < min_eff) h.* = min_eff;
        sum += h.*;
    }
    if (sum < total) {
        out[0] += total - sum;
        return;
    }
    if (sum > total) {
        var excess = sum - total;
        while (excess > 0) {
            var max_idx: usize = 0;
            var max_val: i32 = out[0];
            var j: usize = 1;
            while (j < out.len) : (j += 1) {
                if (out[j] > max_val) {
                    max_val = out[j];
                    max_idx = j;
                }
            }
            if (out[max_idx] <= min_eff) break;
            out[max_idx] -= 1;
            excess -= 1;
        }
    }
}

fn drawSourcePane(self: *Anim, canvas: *inkui.Canvas, rect: inkui.Rect, theme: Theme) void {
    if (rect.w <= 2 or rect.h <= 1) return;
    const path = self.source_view.path orelse {
        canvas.writeClipped(rect.x + 1, rect.y + 1, rect.w - 2, "no source", theme.panel_dim);
        return;
    };
    const file = self.source_cache.get(path) catch null orelse {
        canvas.writeClipped(rect.x + 1, rect.y + 1, rect.w - 2, "source not found", theme.panel_dim);
        return;
    };
    const line_count = file.lines.len;
    if (line_count == 0) {
        canvas.writeClipped(rect.x + 1, rect.y + 1, rect.w - 2, "empty source", theme.panel_dim);
        return;
    }
    const visible_rows = @as(usize, @intCast(rect.h - 1));
    if (visible_rows == 0) return;
    if (self.source_view.line == 0) self.source_view.line = 1;
    if (self.source_view.line > line_count) self.source_view.line = line_count;
    self.source_view.scroll = centerScroll(self.source_view.line, line_count, visible_rows);
    const inner_x = rect.x + 1;
    const inner_w = rect.w - 2;
    const gutter_w: i32 = 1;
    const text_x = inner_x + gutter_w;
    const text_w = inner_w - gutter_w;
    if (text_w <= 0) return;
    const current_sid = sourceIdForPath(self, file.path);
    var has_focus = false;
    var focus_start: usize = 0;
    var focus_end: usize = 0;
    if (self.focus_span) |focus| {
        if (current_sid != null and focus.source_id == current_sid.?) {
            has_focus = true;
            focus_start = focus.start;
            focus_end = focus.end;
        }
    }

    const wrap_symbol = inkui.Icons.wrap;
    const wrap_symbol_back = inkui.Icons.wrap_back;
    var row: i32 = 0;
    var line_index = self.source_view.scroll;
    while (row < rect.h - 1 and line_index < line_count) {
        const line_no: i64 = @intCast(line_index + 1);
        const line_meta = file.lines[line_index];
        const slice = file.text[line_meta.start .. line_meta.start + line_meta.len];
        const is_current = self.source_view.line == line_index + 1;
        const line_style = if (!has_focus and is_current) theme.highlight else theme.panel_text;
        const gutter_style = theme.panel_dim;
        const row_y = rect.y + 1 + row;

        // gutter for first row (no line numbers)
        const mark_char: u21 = if (is_current) '>' else ' ';
        canvas.put(inner_x, row_y, mark_char, gutter_style);

        var span_start: usize = 0;
        var span_end: usize = 0;
        if (has_focus) {
            const line_start = line_meta.start;
            const line_end = line_meta.start + line_meta.len;
            if (focus_end > line_start and focus_start < line_end) {
                span_start = if (focus_start > line_start) focus_start - line_start else 0;
                span_end = if (focus_end < line_end) focus_end - line_start else line_meta.len;
                if (span_end <= span_start) {
                    if (span_start < line_meta.len) {
                        span_end = span_start + 1;
                    } else {
                        span_start = line_meta.len;
                        span_end = line_meta.len;
                    }
                }
            }
        }

        const remaining_rows = rect.h - 1 - row;
        const rows_drawn = drawWrappedLineSpan(
            canvas,
            text_x,
            row_y,
            text_w,
            remaining_rows,
            slice,
            span_start,
            span_end,
            line_style,
            theme.highlight,
            theme.inlay,
            wrap_symbol,
            wrap_symbol_back,
        );

        if (rows_drawn == 0) break;

        if (rows_drawn > 1) {
            var r: i32 = 1;
            while (r < rows_drawn) : (r += 1) {
                var gx: i32 = inner_x;
                const y = row_y + r;
                while (gx < inner_x + gutter_w) : (gx += 1) {
                    canvas.put(gx, y, ' ', gutter_style);
                }
            }
        }

        const wrap_prefix_w: i32 = 1 + 4;
        if (rows_drawn == 1 and wrappedRowCount(slice, text_w, wrap_prefix_w) == 1) {
            drawLineHint(self, canvas, rect, line_no, row, text_x, text_w, theme);
        }

        row += rows_drawn;
        line_index += 1;
    }
}

fn writeClippedExpandedSpan(
    canvas: *inkui.Canvas,
    x: i32,
    y: i32,
    max_w: i32,
    text: []const u8,
    span_start: usize,
    span_end: usize,
    base_style: inkui.Style,
    highlight_style: inkui.Style,
) void {
    if (max_w <= 0) return;
    const tab_width: i32 = 4;
    var col = x;
    var remaining = max_w;
    var offset: i32 = 0;
    var idx: usize = 0;
    while (remaining > 0) {
        const byte_index = idx;
        var cp = nextCodepointLossy(text, &idx) orelse break;
        const next_index = idx;
        const in_span = span_end > span_start and byte_index < span_end and next_index > span_start;
        const style = if (in_span) highlight_style else base_style;
        if (cp == '\t') {
            const advance = tab_width - @mod(offset, tab_width);
            var step: i32 = 0;
            while (step < advance and remaining > 0) : (step += 1) {
                canvas.put(col, y, ' ', style);
                col += 1;
                remaining -= 1;
                offset += 1;
            }
            continue;
        }
        if (cp < 0x20) cp = ' ';
        if (remaining < 1) break;
        canvas.put(col, y, cp, style);
        col += 1;
        remaining -= 1;
        offset += 1;
    }
}

fn expandedWidth(text: []const u8, max_w: i32) i32 {
    if (max_w <= 0) return 0;
    const tab_width: i32 = 4;
    var col: i32 = 0;
    var idx: usize = 0;
    while (col < max_w) {
        const cp = nextCodepointLossy(text, &idx) orelse break;
        if (cp == '\t') {
            const advance = tab_width - @mod(col, tab_width);
            col += advance;
        } else {
            col += 1;
        }
    }
    if (col > max_w) col = max_w;
    return col;
}

fn advanceWidth(text: []const u8, start: usize, max_w: i32) usize {
    if (start >= text.len or max_w <= 0) return start;
    const tab_width: i32 = 4;
    var col: i32 = 0;
    var idx = start;
    while (idx < text.len) {
        const prev_idx = idx;
        const cp_raw = nextCodepointLossy(text, &idx) orelse break;
        var width: i32 = 1;
        if (cp_raw == '\t') {
            width = tab_width - @mod(col, tab_width);
        }
        if (col + width > max_w) {
            return prev_idx;
        }
        col += width;
        if (col >= max_w) return idx;
    }
    return idx;
}

fn nextCodepointLossy(text: []const u8, idx: *usize) ?u21 {
    if (idx.* >= text.len) return null;
    const first = text[idx.*];
    const seq_len = std.unicode.utf8ByteSequenceLength(first) catch {
        idx.* += 1;
        return 0xfffd;
    };
    const len: usize = @intCast(seq_len);
    if (len == 1) {
        idx.* += 1;
        return @as(u21, first);
    }
    if (idx.* + len > text.len) {
        idx.* += 1;
        return 0xfffd;
    }
    const slice = text[idx.* .. idx.* + len];
    const cp = std.unicode.utf8Decode(slice) catch {
        idx.* += 1;
        return 0xfffd;
    };
    idx.* += len;
    return cp;
}

fn wrapRowEnd(text: []const u8, start: usize, max_w: i32) WrapRow {
    if (start >= text.len) return .{ .end = start, .wrapped = false };
    if (max_w <= 0) return .{ .end = start, .wrapped = true };
    const end_full = advanceWidth(text, start, max_w);
    if (end_full >= text.len) return .{ .end = end_full, .wrapped = false };
    if (max_w <= 1) {
        const end_one = advanceWidth(text, start, 1);
        const end_safe = if (end_one == start and start < text.len) text.len else end_one;
        return .{ .end = end_safe, .wrapped = end_safe < text.len };
    }
    var end_wrapped = advanceWidth(text, start, max_w - 1);
    if (end_wrapped == start) {
        end_wrapped = end_full;
    }
    return .{ .end = end_wrapped, .wrapped = end_wrapped < text.len };
}

fn wrappedRowCount(text: []const u8, max_w: i32, prefix_w: i32) usize {
    if (max_w <= 0) return 0;
    var effective_prefix = prefix_w;
    if (effective_prefix < 0) effective_prefix = 0;
    var count: usize = 0;
    var idx: usize = 0;
    var row_idx: usize = 0;
    while (idx < text.len) {
        const avail = max_w - (if (row_idx == 0) 0 else effective_prefix);
        if (avail <= 0) break;
        const row = wrapRowEnd(text, idx, avail);
        count += 1;
        if (row.end <= idx) break;
        idx = row.end;
        row_idx += 1;
    }
    if (count == 0) count = 1;
    return count;
}

fn drawWrappedLine(
    canvas: *inkui.Canvas,
    x: i32,
    y: i32,
    max_w: i32,
    max_rows: i32,
    text: []const u8,
    style: inkui.Style,
    wrap_style: inkui.Style,
    wrap_symbol: []const u8,
    wrap_symbol_back: []const u8,
    skip_rows: usize,
) i32 {
    if (max_w <= 0 or max_rows <= 0) return 0;
    if (text.len == 0) {
        if (skip_rows == 0) {
            canvas.writeClippedExpanded(x, y, max_w, "", style);
            return 1;
        }
        return 0;
    }
    var idx: usize = 0;
    var row_idx: usize = 0;
    var rendered: i32 = 0;
    const wrap_tab: i32 = 4;
    const prefix_w: i32 = 1 + wrap_tab;
    while (idx < text.len and rendered < max_rows) {
        const avail = max_w - (if (row_idx == 0) 0 else prefix_w);
        if (avail <= 0) break;
        const row = wrapRowEnd(text, idx, avail);
        const do_render = row_idx >= skip_rows;
        if (do_render) {
            const row_y = y + rendered;
            var text_x = x;
            if (row_idx > 0) {
                const sym = if (row_idx == 1) wrap_symbol else wrap_symbol_back;
                canvas.writeClipped(text_x, row_y, 1, sym, wrap_style);
                var s: i32 = 1;
                while (s < prefix_w) : (s += 1) {
                    canvas.put(text_x + s, row_y, ' ', wrap_style);
                }
                text_x += prefix_w;
            }
            canvas.writeClippedExpanded(text_x, row_y, avail, text[idx..row.end], style);
            rendered += 1;
        }
        row_idx += 1;
        if (row.end <= idx) break;
        idx = row.end;
    }
    return rendered;
}

fn drawWrappedLineSpan(
    canvas: *inkui.Canvas,
    x: i32,
    y: i32,
    max_w: i32,
    max_rows: i32,
    text: []const u8,
    span_start: usize,
    span_end: usize,
    base_style: inkui.Style,
    highlight_style: inkui.Style,
    wrap_style: inkui.Style,
    wrap_symbol: []const u8,
    wrap_symbol_back: []const u8,
) i32 {
    if (max_w <= 0 or max_rows <= 0) return 0;
    if (text.len == 0) {
        canvas.writeClippedExpanded(x, y, max_w, "", base_style);
        return 1;
    }
    var idx: usize = 0;
    var rendered: i32 = 0;
    var row_idx: usize = 0;
    const wrap_tab: i32 = 4;
    const prefix_w: i32 = 1 + wrap_tab;
    while (idx < text.len and rendered < max_rows) {
        const avail = max_w - (if (row_idx == 0) 0 else prefix_w);
        if (avail <= 0) break;
        const row = wrapRowEnd(text, idx, avail);
        const row_y = y + rendered;
        var text_x = x;
        if (row_idx > 0) {
            const sym = if (row_idx == 1) wrap_symbol else wrap_symbol_back;
            canvas.writeClipped(text_x, row_y, 1, sym, wrap_style);
            var s: i32 = 1;
            while (s < prefix_w) : (s += 1) {
                canvas.put(text_x + s, row_y, ' ', wrap_style);
            }
            text_x += prefix_w;
        }
        if (span_end > span_start and span_end > idx and span_start < row.end) {
            const local_start = if (span_start > idx) span_start - idx else 0;
            const local_end = if (span_end < row.end) span_end - idx else row.end - idx;
            if (local_end > local_start) {
                writeClippedExpandedSpan(
                    canvas,
                    text_x,
                    row_y,
                    avail,
                    text[idx..row.end],
                    local_start,
                    local_end,
                    base_style,
                    highlight_style,
                );
            } else {
                canvas.writeClippedExpanded(text_x, row_y, avail, text[idx..row.end], base_style);
            }
        } else {
            canvas.writeClippedExpanded(text_x, row_y, avail, text[idx..row.end], base_style);
        }
        rendered += 1;
        row_idx += 1;
        if (row.end <= idx) break;
        idx = row.end;
    }
    return rendered;
}

fn drawLineHint(
    self: *Anim,
    canvas: *inkui.Canvas,
    rect: inkui.Rect,
    line_no: i64,
    row: i32,
    text_x: i32,
    text_w: i32,
    theme: Theme,
) void {
    const path = self.source_view.path orelse return;
    const file = self.source_cache.get(path) catch null orelse return;
    const key = LineKey{ .source_id = sourceIdForPath(self, file.path) orelse return, .line = @intCast(line_no) };
    const hint = self.line_hints.get(key) orelse return;
    var buf: [256]u8 = undefined;
    const hint_text = formatHintText(hint, &buf);
    if (hint_text.len == 0) return;
    const line_idx = if (line_no > 0) @as(usize, @intCast(line_no - 1)) else 0;
    if (line_idx >= file.lines.len) return;
    const line_meta = file.lines[line_idx];
    const slice = file.text[line_meta.start .. line_meta.start + line_meta.len];
    const max_len = expandedWidth(slice, text_w);
    var hint_x = text_x + max_len;
    if (hint_x < rect.x + rect.w - 2) {
        hint_x += 1; // spacer between code and inlay
    }
    const max_hint_w = rect.w - (hint_x - rect.x) - 1;
    if (max_hint_w <= 0) return;
    canvas.writeClipped(hint_x, rect.y + 1 + row, max_hint_w, hint_text, theme.inlay);
}

fn drawTransformPane(self: *Anim, canvas: *inkui.Canvas, rect: inkui.Rect, theme: Theme) void {
    if (rect.w <= 2 or rect.h <= 1) return;
    const log = &self.phase_logs[phaseIndex(self.current_phase)];
    drawWrappedLogPane(canvas, rect, log.lines.items, theme, .transform, "no events");
}

fn logLineStyle(kind: LogKind, line: []const u8, theme: Theme) inkui.Style {
    switch (kind) {
        .plain => return theme.panel_text,
        .transform, .ai => {
            if (std.mem.startsWith(u8, line, "llm ")) return theme.highlight;
            if (std.mem.startsWith(u8, line, "analysis ")) return theme.panel_dim;
            return theme.panel_text;
        },
    }
}

fn drawWrappedLogLines(
    canvas: *inkui.Canvas,
    x: i32,
    y: i32,
    max_w: i32,
    max_rows: i32,
    lines: []const []u8,
    theme: Theme,
    kind: LogKind,
) void {
    if (max_w <= 0 or max_rows <= 0) return;
    if (lines.len == 0) return;
    const wrap_symbol = inkui.Icons.wrap;
    const wrap_symbol_back = inkui.Icons.wrap_back;
    const available: usize = @intCast(max_rows);
    var rows_used: usize = 0;
    var start_idx: usize = lines.len;
    var skip_rows: usize = 0;
    const wrap_prefix_w: i32 = 1 + 4;
    while (start_idx > 0 and rows_used < available) {
        const count = wrappedRowCount(lines[start_idx - 1], max_w, wrap_prefix_w);
        if (rows_used + count > available) {
            skip_rows = count - (available - rows_used);
            start_idx -= 1;
            break;
        }
        rows_used += count;
        start_idx -= 1;
    }

    var row_y = y;
    var first = true;
    var idx: usize = start_idx;
    while (idx < lines.len and row_y < y + max_rows) : (idx += 1) {
        const line = lines[idx];
        const style = logLineStyle(kind, line, theme);
        const skip = if (first) skip_rows else 0;
        const rows_left = y + max_rows - row_y;
        if (rows_left <= 0) break;
        const drawn = drawWrappedLine(canvas, x, row_y, max_w, rows_left, line, style, theme.inlay, wrap_symbol, wrap_symbol_back, skip);
        if (drawn <= 0) break;
        row_y += drawn;
        first = false;
    }
}

fn drawWrappedLogPane(
    canvas: *inkui.Canvas,
    rect: inkui.Rect,
    lines: []const []u8,
    theme: Theme,
    kind: LogKind,
    empty_text: []const u8,
) void {
    if (rect.w <= 2 or rect.h <= 1) return;
    const max_w = rect.w - 2;
    const max_rows = rect.h - 1;
    if (lines.len == 0) {
        canvas.writeClipped(rect.x + 1, rect.y + 1, max_w, empty_text, theme.panel_dim);
        return;
    }
    drawWrappedLogLines(canvas, rect.x + 1, rect.y + 1, max_w, max_rows, lines, theme, kind);
}

fn drawCompilerPane(self: *Anim, canvas: *inkui.Canvas, rect: inkui.Rect, theme: Theme) void {
    if (rect.w <= 2 or rect.h <= 1) return;
    const raw_path = self.compiler_view.path orelse {
        canvas.writeClipped(rect.x + 1, rect.y + 1, rect.w - 2, "compiler idle", theme.panel_dim);
        return;
    };
    const path = resolveCompilerPath(raw_path) orelse raw_path;
    const file = self.source_cache.get(path) catch null orelse {
        canvas.writeClipped(rect.x + 1, rect.y + 1, rect.w - 2, raw_path, theme.panel_dim);
        return;
    };
    const line_count = file.lines.len;
    if (line_count == 0) {
        canvas.writeClipped(rect.x + 1, rect.y + 1, rect.w - 2, "empty compiler", theme.panel_dim);
        return;
    }
    const visible_rows = @as(usize, @intCast(rect.h - 1));
    if (visible_rows == 0) return;
    if (self.compiler_view.line == 0) self.compiler_view.line = 1;
    if (self.compiler_view.line > line_count) self.compiler_view.line = line_count;
    self.compiler_view.scroll = centerScroll(self.compiler_view.line, line_count, visible_rows);

    const inner_x = rect.x + 1;
    const inner_w = rect.w - 2;
    const line_digits = countDigits(line_count);
    const num_w: i32 = @intCast(@max(@as(usize, 2), line_digits));
    const gutter_w = 2 + num_w + 1;
    const text_x = inner_x + gutter_w;
    const text_w = inner_w - gutter_w;
    if (text_w <= 0) return;

    const wrap_symbol = inkui.Icons.wrap;
    const wrap_symbol_back = inkui.Icons.wrap_back;
    var row: i32 = 0;
    var line_index = self.compiler_view.scroll;
    while (row < rect.h - 1 and line_index < line_count) {
        const line_no: i64 = @intCast(line_index + 1);
        const line_meta = file.lines[line_index];
        const slice = file.text[line_meta.start .. line_meta.start + line_meta.len];
        const is_current = self.compiler_view.line == line_index + 1;
        const line_style = if (is_current) theme.highlight else theme.panel_text;
        const gutter_style = theme.panel_dim;
        const row_y = rect.y + 1 + row;

        const mark_char: u21 = if (is_current) '>' else ' ';
        canvas.put(inner_x, row_y, mark_char, gutter_style);
        var num_buf: [16]u8 = undefined;
        const num_text = std.fmt.bufPrint(&num_buf, "{d}", .{line_no}) catch "";
        const pad = num_w - @as(i32, @intCast(num_text.len));
        const pad_x = inner_x + 1;
        var p: i32 = 0;
        while (p < pad) : (p += 1) {
            canvas.put(pad_x + p, row_y, ' ', gutter_style);
        }
        canvas.writeClipped(pad_x + pad, row_y, num_w, num_text, gutter_style);
        canvas.put(inner_x + 1 + num_w, row_y, ' ', gutter_style);

        const remaining_rows = rect.h - 1 - row;
        const rows_drawn = drawWrappedLine(
            canvas,
            text_x,
            row_y,
            text_w,
            remaining_rows,
            slice,
            line_style,
            theme.inlay,
            wrap_symbol,
            wrap_symbol_back,
            0,
        );
        if (rows_drawn == 0) break;

        if (rows_drawn > 1) {
            var r: i32 = 1;
            while (r < rows_drawn) : (r += 1) {
                var gx: i32 = inner_x;
                const y = row_y + r;
                while (gx < inner_x + gutter_w) : (gx += 1) {
                    canvas.put(gx, y, ' ', gutter_style);
                }
            }
        }

        row += rows_drawn;
        line_index += 1;
    }
}

fn drawIrPane(self: *Anim, canvas: *inkui.Canvas, rect: inkui.Rect, theme: Theme) void {
    if (rect.w <= 2 or rect.h <= 1) return;
    if (self.current_phase == .encode and self.byte_bytes.items.len > 0) {
        drawBytePane(self, canvas, rect, theme);
        return;
    }
    const log = if (self.current_phase == .encode) &self.byte_log else &self.ir_log;
    drawWrappedLogPane(canvas, rect, log.lines.items, theme, .plain, "no ir yet");
}

fn drawAiPane(self: *Anim, canvas: *inkui.Canvas, rect: inkui.Rect, theme: Theme) void {
    if (rect.w <= 2 or rect.h <= 1) return;
    const log = &self.ai_log;
    const wrap_symbol = inkui.Icons.wrap;
    const wrap_symbol_back = inkui.Icons.wrap_back;
    const max_w = rect.w - 2;
    var row_start: i32 = rect.y + 1;
    var visible_rows = rect.h - 1;
    if (self.ollama_active and self.ollama_status != null) {
        var line_buf: [256]u8 = undefined;
        const line = formatOllamaLine(self, rect.w - 2, &line_buf);
        const drawn = drawWrappedLine(canvas, rect.x + 1, row_start, max_w, visible_rows, line, theme.panel_dim, theme.inlay, wrap_symbol, wrap_symbol_back, 0);
        row_start += drawn;
        visible_rows -= drawn;
        if (visible_rows <= 0) return;
    }
    if (self.last_llm) |text| {
        var line_buf: [512]u8 = undefined;
        const line = std.fmt.bufPrint(&line_buf, "llm: {s}", .{text}) catch text;
        const drawn = drawWrappedLine(canvas, rect.x + 1, row_start, max_w, visible_rows, line, theme.highlight, theme.inlay, wrap_symbol, wrap_symbol_back, 0);
        row_start += drawn;
        visible_rows -= drawn;
        if (visible_rows <= 0) return;
    }
    if (log.lines.items.len == 0) {
        canvas.writeClipped(rect.x + 1, row_start, max_w, "analysis pending", theme.panel_dim);
        return;
    }
    drawWrappedLogLines(canvas, rect.x + 1, row_start, max_w, visible_rows, log.lines.items, theme, .ai);
}

fn drawBytePane(self: *Anim, canvas: *inkui.Canvas, rect: inkui.Rect, theme: Theme) void {
    if (rect.w <= 2 or rect.h <= 1) return;
    const bytes = self.byte_bytes.items;
    if (bytes.len == 0) {
        canvas.writeClipped(rect.x + 1, rect.y + 1, rect.w - 2, "no bytecode", theme.panel_dim);
        return;
    }
    const inner_x = rect.x + 1;
    const inner_w = rect.w - 2;
    const body_y = rect.y + 1;
    const body_h = rect.h - 1;
    const prefix_len: i32 = 12; // "0x00000000: "
    const bytes_per_line = calcBytesPerLine(inner_w, prefix_len);
    if (bytes_per_line == 0) {
        canvas.writeClipped(rect.x + 1, rect.y + 1, rect.w - 2, "byte view too narrow", theme.panel_dim);
        return;
    }
    const hex_block_len: i32 = @intCast(bytes_per_line * 3 - 1);
    const hex_start_x = inner_x + prefix_len;
    const ascii_start_x = hex_start_x + hex_block_len + 2;
    const focus = self.byte_focus;
    const focus_start = if (focus) |f| f.offset else 0;
    const focus_end = if (focus) |f| f.offset + f.len else 0;

    var row: i32 = 0;
    while (row < body_h) : (row += 1) {
        const offset = @as(usize, @intCast(row)) * bytes_per_line;
        if (offset >= bytes.len) break;
        const y = body_y + row;
        var addr_buf: [16]u8 = undefined;
        const addr_text = std.fmt.bufPrint(&addr_buf, "0x{x:0>8}: ", .{offset}) catch "";
        canvas.writeClipped(inner_x, y, inner_w, addr_text, theme.panel_dim);

        var i: usize = 0;
        while (i < bytes_per_line) : (i += 1) {
            const pos = offset + i;
            const hex_x = hex_start_x + @as(i32, @intCast(i * 3));
            if (pos < bytes.len) {
                const b = bytes[pos];
                const in_focus = focus != null and pos >= focus_start and pos < focus_end;
                const style = if (in_focus) theme.highlight else theme.panel_text;
                canvas.put(hex_x, y, hexDigit(b >> 4), style);
                canvas.put(hex_x + 1, y, hexDigit(b & 0x0f), style);
            } else {
                canvas.put(hex_x, y, ' ', theme.panel_text);
                canvas.put(hex_x + 1, y, ' ', theme.panel_text);
            }
            if (i + 1 < bytes_per_line) {
                canvas.put(hex_x + 2, y, ' ', theme.panel_text);
            }
        }

        canvas.put(hex_start_x + hex_block_len, y, ' ', theme.panel_text);
        canvas.put(hex_start_x + hex_block_len + 1, y, '|', theme.panel_dim);

        i = 0;
        while (i < bytes_per_line) : (i += 1) {
            const pos = offset + i;
            const ascii_x = ascii_start_x + @as(i32, @intCast(i));
            if (pos < bytes.len) {
                const b = bytes[pos];
                const ch: u8 = if (b >= 0x20 and b <= 0x7e) b else '.';
                const in_focus = focus != null and pos >= focus_start and pos < focus_end;
                const style = if (in_focus) theme.highlight else theme.panel_text;
                canvas.put(ascii_x, y, ch, style);
            } else {
                canvas.put(ascii_x, y, ' ', theme.panel_text);
            }
        }
        canvas.put(ascii_start_x + @as(i32, @intCast(bytes_per_line)), y, '|', theme.panel_dim);
    }
}

fn drawStatusBar(self: *Anim, canvas: *inkui.Canvas, rect: inkui.Rect, theme: Theme) void {
    if (rect.h <= 0) return;
    canvas.fill(rect, ' ', theme.status);
    const line0 = rect.y;
    const line1 = if (rect.h > 1) rect.y + 1 else rect.y;
    var timeline_buf: [256]u8 = undefined;
    const timeline = buildTimeline(self, timeline_buf[0..]);
    canvas.writeClipped(rect.x + 1, line0, rect.w - 2, timeline, theme.status);
    var buf: [512]u8 = undefined;
    const ai_queue = if (self.ai_engine.config.enable) self.ai_engine.queueLen() else 0;
    const status = std.fmt.bufPrint(
        &buf,
        "tok:{d} ast:{d} uir:{d} mir:{d} lir:{d} byte:{d} err:{d} warn:{d} aiq:{d}",
        .{ self.counts.tokens, self.counts.ast_nodes, self.counts.uir_nodes, self.counts.mir_nodes, self.counts.lir_nodes, self.counts.byte_chunks, self.counts.errors, self.counts.warnings, ai_queue },
    ) catch "";
    canvas.writeClipped(rect.x + 1, line1, rect.w - 2, status, theme.status_dim);
    if (self.last_ai) |text| {
        const ai_text = std.fmt.bufPrint(&buf, "ai: {s}", .{text}) catch text;
        const half_w = @divTrunc(rect.w, 2);
        canvas.writeClipped(rect.x + half_w, line1, @max(@as(i32, 0), half_w - 2), ai_text, theme.status_dim);
    }
}

fn buildTimeline(self: *Anim, buf: []u8) []const u8 {
    var stream = std.io.fixedBufferStream(buf);
    const writer = stream.writer();
    const phases = phaseOrder();
    for (phases, 0..) |phase, idx| {
        if (idx != 0) _ = writer.writeAll(" ") catch {};
        if (phase == self.current_phase) {
            _ = writer.writeAll("[") catch {};
            _ = writer.writeAll(@tagName(phase)) catch {};
            _ = writer.writeAll("]") catch {};
        } else {
            _ = writer.writeAll(@tagName(phase)) catch {};
        }
    }
    return stream.getWritten();
}

fn formatOllamaLine(self: *Anim, max_width: i32, buf: []u8) []const u8 {
    const status = self.ollama_status orelse "ollama";
    var stream = std.io.fixedBufferStream(buf);
    const writer = stream.writer();
    _ = writer.writeAll("ollama ") catch {};
    _ = writer.writeAll(status) catch {};
    if (self.ollama_progress) |progress| {
        const percent = @min(@as(u32, 100), @as(u32, @intFromFloat(progress * 100.0)));
        var bar_w: i32 = max_width - 18;
        if (bar_w < 6) bar_w = 0;
        if (bar_w > 24) bar_w = 24;
        if (bar_w > 0) {
            _ = writer.writeAll(" [") catch {};
            const bar_w_f = @as(f32, @floatFromInt(bar_w));
            var filled = @as(i32, @intFromFloat(progress * bar_w_f));
            if (filled < 0) filled = 0;
            if (filled > bar_w) filled = bar_w;
            var i: i32 = 0;
            while (i < bar_w) : (i += 1) {
                const ch: u8 = if (i < filled) '#' else '.';
                _ = writer.writeByte(ch) catch {};
            }
            _ = writer.writeAll("]") catch {};
        }
        _ = writer.print(" {d}%", .{percent}) catch {};
    }
    return stream.getWritten();
}

fn renderSummaryFrames(self: *Anim) void {
    var frame: usize = 0;
    while (frame < 12) : (frame += 1) {
        if (self.ui == null) break;
        drainAiNotes(self);
        renderSummary(self, frame, 12);
        std.Thread.sleep(30 * std.time.ns_per_ms);
    }
    const hold_ms = summaryHoldMs(self);
    if (self.ui != null and hold_ms > 0) {
        drainAiNotes(self);
        renderSummary(self, 11, 12);
        std.Thread.sleep(hold_ms * std.time.ns_per_ms);
    }
}

fn renderOutroFrames(self: *Anim) void {
    var frame: usize = 0;
    while (frame < 8) : (frame += 1) {
        if (self.ui == null) break;
        drainAiNotes(self);
        renderSummary(self, frame, 8);
        std.Thread.sleep(30 * std.time.ns_per_ms);
    }
}

fn summaryHoldMs(self: *Anim) u64 {
    const min_hold: u64 = 2000;
    const scaled = self.rate_max_ms * 2;
    const hold = if (scaled < min_hold) min_hold else scaled;
    return @min(@as(u64, 8000), hold);
}

fn renderSummary(self: *Anim, frame: usize, total: usize) void {
    if (self.ui) |*ui| {
        const now = std.time.milliTimestamp();
        const size = ui.term.size();
        if (size.cols != ui.size.cols or size.rows != ui.size.rows) {
            ui.canvas.resize(size.cols, size.rows) catch {};
            ui.size = size;
        }
        const theme = makeTheme(self.current_phase, now);
        const rect_full = inkui.Rect{ .x = 0, .y = 0, .w = @intCast(ui.size.cols), .h = @intCast(ui.size.rows) };
        ui.canvas.fill(rect_full, ' ', theme.panel_text);
        drawSummaryLayout(self, &ui.canvas, theme, frame, total);
        drawParticles(self, &ui.canvas, theme, now);
        ui.renderer.render(&ui.canvas) catch {};
    }
}

fn drawSummaryLayout(self: *Anim, canvas: *inkui.Canvas, theme: Theme, frame: usize, total: usize) void {
    const size = canvasSize(canvas);
    if (size.w < 40 or size.h < 8) {
        drawSummaryPanel(self, canvas, theme, frame, total);
        return;
    }
    const left_w = @max(@as(i32, 24), @divTrunc(size.w * 40, 100));
    const right_w = size.w - left_w;
    const summary_rect = inkui.Rect{ .x = 0, .y = 0, .w = left_w, .h = size.h };
    const ai_rect = inkui.Rect{ .x = left_w, .y = 0, .w = right_w, .h = size.h };
    drawSummaryBox(self, canvas, summary_rect, theme, frame, total);
    drawPanelFrame(canvas, ai_rect, theme, inkui.Icons.repl ++ " ai analysis");
    drawAiPane(self, canvas, ai_rect, theme);
}

fn drawSummaryPanel(self: *Anim, canvas: *inkui.Canvas, theme: Theme, frame: usize, total: usize) void {
    const size = canvasSize(canvas);
    const w = @max(@as(i32, 20), @divTrunc(size.w, 2));
    const h = @max(@as(i32, 8), @divTrunc(size.h, 3));
    const x = @divTrunc(size.w - w, 2);
    const y = @divTrunc(size.h - h, 2);
    const rect = inkui.Rect{ .x = x, .y = y, .w = w, .h = h };
    drawSummaryBox(self, canvas, rect, theme, frame, total);
}

fn drawSummaryBox(self: *Anim, canvas: *inkui.Canvas, rect: inkui.Rect, theme: Theme, frame: usize, total: usize) void {
    drawPanelFrame(canvas, rect, theme, "summary");
    const available = @as(usize, @intCast(rect.h - 2));
    var idx: usize = 0;
    var row: i32 = 0;
    const fade = @as(f32, @floatFromInt(frame + 1)) / @as(f32, @floatFromInt(total));
    const text_style = if (fade < 0.6) theme.panel_dim else theme.panel_text;
    while (row < rect.h - 2 and idx < self.summary_lines.items.len and idx < available) : ({
        row += 1;
        idx += 1;
    }) {
        const line = self.summary_lines.items[idx];
        canvas.writeClipped(rect.x + 1, rect.y + 1 + row, rect.w - 2, line, text_style);
    }
}

fn drawParticles(self: *Anim, canvas: *inkui.Canvas, theme: Theme, now: i64) void {
    _ = self;
    var prng = std.Random.DefaultPrng.init(@intCast(now));
    const rand = prng.random();
    const size = canvasSize(canvas);
    if (size.w <= 0 or size.h <= 0) return;
    var i: usize = 0;
    while (i < 40) : (i += 1) {
        const x = rand.intRangeAtMost(i32, 0, size.w - 1);
        const y = rand.intRangeAtMost(i32, 0, size.h - 1);
        canvas.put(x, y, '.', theme.panel_dim);
    }
}

fn buildSummary(self: *Anim, result: *const ink.compiler.compile_result) !void {
    for (self.summary_lines.items) |line| self.allocator.free(line);
    self.summary_lines.clearRetainingCapacity();

    var buf: [256]u8 = undefined;
    const end_ms = self.compile_end_ms orelse std.time.milliTimestamp();
    const total_ms = end_ms - self.start_ms;
    const status = if (result.ok) "ok" else "failed";
    const line0 = std.fmt.bufPrint(&buf, "status:{s} total:{d}ms", .{ status, total_ms }) catch "";
    try self.summary_lines.append(try self.allocator.dupe(u8, line0));
    const line1 = std.fmt.bufPrint(&buf, "errors:{d} warnings:{d} notes:{d} diags:{d}", .{ self.counts.errors, self.counts.warnings, self.counts.notes, self.counts.diagnostics }) catch "";
    try self.summary_lines.append(try self.allocator.dupe(u8, line1));
    const line2 = std.fmt.bufPrint(&buf, "tokens:{d} ast:{d} uir:{d} mir:{d} lir:{d}", .{ self.counts.tokens, self.counts.ast_nodes, self.counts.uir_nodes, self.counts.mir_nodes, self.counts.lir_nodes }) catch "";
    try self.summary_lines.append(try self.allocator.dupe(u8, line2));
    const line3 = std.fmt.bufPrint(&buf, "byte_chunks:{d} instr:{d}", .{ self.counts.byte_chunks, self.counts.instructions }) catch "";
    try self.summary_lines.append(try self.allocator.dupe(u8, line3));
    const mode_line = if (!self.ai_engine.config.enable)
        "ai_mode:none"
    else
        std.fmt.bufPrint(&buf, "ai_mode:{s}", .{@tagName(self.ai_engine.currentMode())}) catch "";
    try self.summary_lines.append(try self.allocator.dupe(u8, mode_line));
    if (self.last_ai) |text| {
        const line4 = std.fmt.bufPrint(&buf, "ai: {s}", .{text}) catch "";
        try self.summary_lines.append(try self.allocator.dupe(u8, line4));
    }
    if (self.last_llm) |text| {
        const line5 = std.fmt.bufPrint(&buf, "llm: {s}", .{text}) catch "";
        try self.summary_lines.append(try self.allocator.dupe(u8, line5));
    }

    for (phaseOrder()) |phase| {
        const stats = self.phase_stats[phaseIndex(phase)];
        if (stats.events == 0 and stats.duration_ms == null) continue;
        if (stats.duration_ms) |dur| {
            const line = std.fmt.bufPrint(&buf, "phase {s}: {d}ms ev:{d}", .{ @tagName(phase), dur, stats.events }) catch "";
            try self.summary_lines.append(try self.allocator.dupe(u8, line));
        } else {
            const line = std.fmt.bufPrint(&buf, "phase {s}: ev:{d}", .{ @tagName(phase), stats.events }) catch "";
            try self.summary_lines.append(try self.allocator.dupe(u8, line));
        }
    }
}

fn formatEventLine(event: *const trace.Event, buf: []u8) []const u8 {
    const phase_name = @tagName(event.phase);
    const tag = event.tag orelse "";
    const msg = event.message orelse "";
    return switch (event.kind) {
        .token => std.fmt.bufPrint(buf, "{s} tok#{d} {s}", .{ phase_name, event.index orelse 0, tag }) catch phase_name,
        .parse_node => std.fmt.bufPrint(buf, "{s} parse#{d} {s}", .{ phase_name, event.index orelse 0, tag }) catch phase_name,
        .ast_node => std.fmt.bufPrint(buf, "{s} ast#{d} {s}", .{ phase_name, event.index orelse 0, tag }) catch phase_name,
        .macro_event => std.fmt.bufPrint(buf, "{s} macro {s} {s}", .{ phase_name, tag, msg }) catch phase_name,
        .uir_node => std.fmt.bufPrint(buf, "{s} uir#{d} {s}", .{ phase_name, event.index orelse 0, tag }) catch phase_name,
        .mir_node => std.fmt.bufPrint(buf, "{s} mir#{d} {s}", .{ phase_name, event.index orelse 0, tag }) catch phase_name,
        .lir_node => std.fmt.bufPrint(buf, "{s} lir#{d} {s}", .{ phase_name, event.index orelse 0, tag }) catch phase_name,
        .instruction => std.fmt.bufPrint(buf, "{s} inst#{d} {s}", .{ phase_name, event.index orelse 0, tag }) catch phase_name,
        .bytecode => std.fmt.bufPrint(buf, "{s} byte@{d} {s}", .{ phase_name, event.index orelse 0, msg }) catch phase_name,
        .diagnostic => std.fmt.bufPrint(buf, "{s} diag {s} {s}", .{ phase_name, tag, msg }) catch phase_name,
        .note => std.fmt.bufPrint(buf, "{s} {s} {s}", .{ phase_name, tag, msg }) catch phase_name,
        .step => blk: {
            if (std.mem.eql(u8, tag, "macro_expand.node")) {
                break :blk std.fmt.bufPrint(buf, "{s} step {s}", .{ phase_name, tag }) catch phase_name;
            }
            break :blk std.fmt.bufPrint(buf, "{s} step {s} {d}/{d}", .{ phase_name, tag, event.index orelse 0, event.index2 orelse 0 }) catch phase_name;
        },
        .phase_start => std.fmt.bufPrint(buf, "{s} phase_start", .{phase_name}) catch phase_name,
        .phase_end => std.fmt.bufPrint(buf, "{s} phase_end", .{phase_name}) catch phase_name,
        else => std.fmt.bufPrint(buf, "{s} event", .{phase_name}) catch phase_name,
    };
}

fn formatEventHint(event: *const trace.Event, buf: []u8) []const u8 {
    const tag = event.tag orelse "";
    const msg = event.message orelse "";
    return switch (event.kind) {
        .token => std.fmt.bufPrint(buf, "tok {s}", .{tag}) catch tag,
        .parse_node => std.fmt.bufPrint(buf, "parse {s}", .{tag}) catch tag,
        .ast_node => std.fmt.bufPrint(buf, "ast {s}", .{tag}) catch tag,
        .macro_event => std.fmt.bufPrint(buf, "macro {s}", .{tag}) catch tag,
        .uir_node => std.fmt.bufPrint(buf, "uir {s}", .{tag}) catch tag,
        .mir_node => std.fmt.bufPrint(buf, "mir {s}", .{tag}) catch tag,
        .lir_node => std.fmt.bufPrint(buf, "lir {s}", .{tag}) catch tag,
        .instruction => std.fmt.bufPrint(buf, "inst {s}", .{tag}) catch tag,
        .bytecode => std.fmt.bufPrint(buf, "byte {s}", .{msg}) catch msg,
        .diagnostic => std.fmt.bufPrint(buf, "diag {s}", .{msg}) catch msg,
        .note => std.fmt.bufPrint(buf, "{s} {s}", .{ tag, msg }) catch tag,
        .step => std.fmt.bufPrint(buf, "{s} {d}", .{ tag, event.index orelse 0 }) catch tag,
        else => msg,
    };
}

fn shouldCollapseEvent(event: *const trace.Event) bool {
    if (event.kind != .step) return false;
    const tag = event.tag orelse return false;
    return std.mem.eql(u8, tag, "macro_expand.node");
}

fn appendLogLine(log: *inkui.LogBuffer, text: []const u8, collapse: bool) !void {
    if (!collapse) return log.add(text);
    if (log.lines.items.len == 0) return log.add(text);
    const last_idx = log.lines.items.len - 1;
    const last = log.lines.items[last_idx];
    if (std.mem.eql(u8, last, text)) {
        const bumped = try std.fmt.allocPrint(log.allocator, "{s} x2", .{text});
        log.allocator.free(last);
        log.lines.items[last_idx] = bumped;
        return;
    }
    if (parseRepeatCount(last, text)) |count| {
        const bumped = try std.fmt.allocPrint(log.allocator, "{s} x{d}", .{ text, count + 1 });
        log.allocator.free(last);
        log.lines.items[last_idx] = bumped;
        return;
    }
    return log.add(text);
}

fn parseRepeatCount(line: []const u8, prefix: []const u8) ?usize {
    if (!std.mem.startsWith(u8, line, prefix)) return null;
    if (line.len <= prefix.len + 2) return null;
    if (line[prefix.len] != ' ' or line[prefix.len + 1] != 'x') return null;
    const count_slice = line[prefix.len + 2 ..];
    return std.fmt.parseInt(usize, count_slice, 10) catch null;
}

fn updateLineHint(self: *Anim, sid: source_id, line_no: usize, text: []const u8, is_ai: bool) !void {
    const entry = try self.line_hints.getOrPut(LineKey{ .source_id = sid, .line = line_no });
    if (!entry.found_existing) {
        entry.value_ptr.* = .{};
    }
    var hint = entry.value_ptr;
    if (is_ai) {
        if (hint.ai_text) |old| self.allocator.free(old);
        hint.ai_text = try self.allocator.dupe(u8, text);
        hint.ai_count += 1;
    } else {
        if (hint.event_text) |old| self.allocator.free(old);
        hint.event_text = try self.allocator.dupe(u8, text);
        hint.event_count += 1;
    }
}

fn formatHintText(hint: LineHint, buf: []u8) []const u8 {
    var stream = std.io.fixedBufferStream(buf);
    const writer = stream.writer();
    var wrote = false;
    if (hint.event_text) |text| {
        _ = writer.writeAll(text) catch {};
        if (hint.event_count > 1) {
            _ = writer.print(" +{d}", .{hint.event_count - 1}) catch {};
        }
        wrote = true;
    }
    if (hint.ai_text) |text| {
        if (wrote) _ = writer.writeAll(" | ") catch {};
        _ = writer.writeAll("ai ") catch {};
        _ = writer.writeAll(text) catch {};
        if (hint.ai_count > 1) {
            _ = writer.print(" +{d}", .{hint.ai_count - 1}) catch {};
        }
    }
    return stream.getWritten();
}

fn updateByteView(self: *Anim, event: *const trace.Event) !void {
    const msg = event.message orelse return;
    const offset = @as(usize, @intCast(event.index orelse return));
    const len = if (event.index2) |val| @as(usize, val) else countHexBytes(msg);
    try ensureByteCapacity(self, offset + len);
    const written = parseHexInto(msg, self.byte_bytes.items[offset..]);
    self.byte_focus = .{ .offset = offset, .len = written, .ts_ms = std.time.milliTimestamp() };
}

fn ensureByteCapacity(self: *Anim, required: usize) !void {
    if (required <= self.byte_bytes.items.len) return;
    try self.byte_bytes.ensureTotalCapacity(required);
    const old_len = self.byte_bytes.items.len;
    self.byte_bytes.items.len = required;
    @memset(self.byte_bytes.items[old_len..], 0);
}

fn countHexBytes(text: []const u8) usize {
    var count: usize = 0;
    var hi: ?u8 = null;
    for (text) |ch| {
        if (hexNibble(ch)) |n| {
            if (hi == null) {
                hi = n;
            } else {
                count += 1;
                hi = null;
            }
        }
    }
    return count;
}

fn parseHexInto(text: []const u8, dst: []u8) usize {
    var out: usize = 0;
    var hi: ?u8 = null;
    for (text) |ch| {
        if (hexNibble(ch)) |n| {
            if (hi == null) {
                hi = n;
            } else {
                if (out >= dst.len) break;
                dst[out] = (hi.? << 4) | n;
                out += 1;
                hi = null;
            }
        }
    }
    return out;
}

fn truncateSlice(text: []const u8, max_len: usize) []const u8 {
    if (text.len <= max_len) return text;
    return text[0..max_len];
}

fn hexNibble(ch: u8) ?u8 {
    if (ch >= '0' and ch <= '9') return ch - '0';
    if (ch >= 'a' and ch <= 'f') return 10 + (ch - 'a');
    if (ch >= 'A' and ch <= 'F') return 10 + (ch - 'A');
    return null;
}

fn sanitizeUtf8(allocator: mem_allocator, input: []const u8) mem_allocator.Error![]u8 {
    if (std.unicode.utf8ValidateSlice(input)) {
        return allocator.dupe(u8, input);
    }
    var out = std.array_list.Managed(u8).init(allocator);
    errdefer out.deinit();
    var i: usize = 0;
    while (i < input.len) {
        const b = input[i];
        const seq_len = std.unicode.utf8ByteSequenceLength(b) catch {
            try out.append('?');
            i += 1;
            continue;
        };
        if (i + seq_len > input.len) {
            try out.append('?');
            break;
        }
        var ok = true;
        var j: usize = 1;
        while (j < seq_len) : (j += 1) {
            if ((input[i + j] & 0xc0) != 0x80) {
                ok = false;
                break;
            }
        }
        if (!ok) {
            try out.append('?');
            i += 1;
            continue;
        }
        try out.appendSlice(input[i .. i + seq_len]);
        i += seq_len;
    }
    return out.toOwnedSlice();
}

const LineInfo = struct {
    path: []const u8,
    line_no: usize,
    col: usize,
    text: []const u8,
    total_lines: usize,
};

fn loadCompilerSources(cache: *inkui.SourceCache) !void {
    for (compiler_sources.files) |file| {
        _ = try cache.put(file.path, file.text);
    }
}

fn pathBasename(path: []const u8) []const u8 {
    var idx = path.len;
    while (idx > 0) : (idx -= 1) {
        const ch = path[idx - 1];
        if (ch == '/' or ch == '\\') return path[idx..];
    }
    return path;
}

fn pathEndsWith(path: []const u8, suffix: []const u8) bool {
    if (suffix.len > path.len) return false;
    var i: usize = path.len;
    var j: usize = suffix.len;
    while (j > 0) {
        i -= 1;
        j -= 1;
        const pc = path[i];
        const sc = suffix[j];
        if (sc == '/' or sc == '\\') {
            if (pc != '/' and pc != '\\') return false;
            continue;
        }
        if (pc != sc) return false;
    }
    return true;
}

fn resolveCompilerPath(path: []const u8) ?[]const u8 {
    if (path.len == 0) return null;
    var best: ?[]const u8 = null;
    var best_len: usize = 0;
    for (compiler_sources.files) |file| {
        const rel = file.path;
        if (pathEndsWith(path, rel)) {
            if (rel.len > best_len) {
                best = rel;
                best_len = rel.len;
            }
            continue;
        }
        const base = pathBasename(rel);
        if (pathEndsWith(path, base)) {
            if (base.len > best_len) {
                best = rel;
                best_len = base.len;
            }
        }
    }
    return best;
}

fn sourceLineInfo(self: *Anim, sid: source_id, sp: ink.source.span) !?LineInfo {
    const path = self.source_map.get(sid) orelse return null;
    const file_opt = self.source_cache.get(path) catch return null;
    const file = file_opt orelse return null;
    if (file.lines.len == 0) return null;
    const offset = sp.start;
    const idx = findLine(file.lines, offset);
    const line = file.lines[idx];
    const col = if (offset >= line.start) offset - line.start + 1 else 1;
    return .{
        .path = file.path,
        .line_no = idx + 1,
        .col = col,
        .text = file.text[line.start .. line.start + line.len],
        .total_lines = file.lines.len,
    };
}

fn compilerLineInfo(self: *Anim, path: []const u8, line_no: usize) !?LineInfo {
    const lookup = resolveCompilerPath(path) orelse path;
    const file_opt = self.source_cache.get(lookup) catch return null;
    const file = file_opt orelse return null;
    if (file.lines.len == 0) return null;
    const idx = if (line_no > 0 and line_no <= file.lines.len) line_no - 1 else 0;
    const line = file.lines[idx];
    return .{
        .path = file.path,
        .line_no = idx + 1,
        .col = 1,
        .text = file.text[line.start .. line.start + line.len],
        .total_lines = file.lines.len,
    };
}

fn findLine(lines: []const inkui.SourceLine, offset: usize) usize {
    var lo: usize = 0;
    var hi: usize = lines.len;
    while (lo < hi) {
        const mid = (lo + hi) / 2;
        const line = lines[mid];
        if (offset < line.start) {
            hi = mid;
        } else if (offset > line.start + line.len) {
            lo = mid + 1;
        } else {
            return mid;
        }
    }
    if (lo == 0) return 0;
    return lo - 1;
}

fn centerScroll(line_no: usize, total: usize, view_rows: usize) usize {
    if (view_rows == 0 or total <= view_rows) return 0;
    const line_idx = if (line_no > 0) line_no - 1 else 0;
    const half = view_rows / 2;
    var top = if (line_idx > half) line_idx - half else 0;
    const max_top = total - view_rows;
    if (top > max_top) top = max_top;
    return top;
}

fn countDigits(num: usize) usize {
    var n = num;
    var count: usize = 1;
    while (n >= 10) : (n /= 10) {
        count += 1;
    }
    return count;
}

fn calcBytesPerLine(inner_w: i32, prefix_len: i32) usize {
    if (inner_w <= prefix_len + 3) return 0;
    var slots = @divTrunc(inner_w - prefix_len - 2, 4);
    if (slots < 1) return 0;
    if (slots > 16) slots = 16;
    return @intCast(slots);
}

fn sourceIdForPath(self: *Anim, path: []const u8) ?source_id {
    var it = self.source_map.iterator();
    while (it.next()) |entry| {
        if (std.mem.eql(u8, entry.value_ptr.*, path)) return entry.key_ptr.*;
    }
    return null;
}

fn canvasSize(canvas: *inkui.Canvas) inkui.Rect {
    return .{ .x = 0, .y = 0, .w = @intCast(canvas.width), .h = @intCast(canvas.height) };
}

fn phaseOrder() []const trace.Phase {
    return &[_]trace.Phase{
        .source_store,
        .module_graph,
        .lex,
        .parse,
        .ast,
        .macro_discover,
        .macro_compile,
        .macro_expand,
        .desugar,
        .resolve,
        .uir,
        .typecheck,
        .mir,
        .lir,
        .backend,
        .encode,
    };
}

fn makeTheme(phase: trace.Phase, now_ms: i64) Theme {
    const base_bg = inkui.Color{ .r = 0x12, .g = 0x10, .b = 0x14 };
    const bg_alt = inkui.Color{ .r = 0x18, .g = 0x16, .b = 0x1c };
    const fg = inkui.Color{ .r = 0xe8, .g = 0xe6, .b = 0xe8 };
    const dim = inkui.Color{ .r = 0x8a, .g = 0x87, .b = 0x90 };
    const warn = inkui.Color{ .r = 0xf0, .g = 0xb0, .b = 0x3b };
    const err = inkui.Color{ .r = 0xf0, .g = 0x4a, .b = 0x4a };
    const accent = phaseAccent(phase);
    const tease = phaseTease(accent, now_ms);
    const panel_border = inkui.Style{ .fg = tease, .bg = bg_alt };
    const panel_header = inkui.Style{ .fg = fg, .bg = accent, .bold = true };
    const panel_text = inkui.Style{ .fg = fg, .bg = bg_alt };
    const panel_dim = inkui.Style{ .fg = dim, .bg = bg_alt, .dim = true };
    const inlay_fg = inkui.Color{ .r = 0x9b, .g = 0xff, .b = 0x4d };
    const inlay = inkui.Style{ .fg = inlay_fg, .bg = bg_alt };
    const highlight = inkui.Style{ .fg = base_bg, .bg = err, .bold = true };
    const status = inkui.Style{ .fg = fg, .bg = base_bg };
    const status_dim = inkui.Style{ .fg = dim, .bg = base_bg };
    return .{
        .bg = base_bg,
        .bg_alt = bg_alt,
        .fg = fg,
        .dim = dim,
        .accent = accent,
        .accent2 = tease,
        .warn = warn,
        .err = err,
        .panel_border = panel_border,
        .panel_header = panel_header,
        .panel_text = panel_text,
        .panel_dim = panel_dim,
        .inlay = inlay,
        .highlight = highlight,
        .status = status,
        .status_dim = status_dim,
    };
}

fn phaseAccent(phase: trace.Phase) inkui.Color {
    return switch (phase) {
        .lex => .{ .r = 0x41, .g = 0xd6, .b = 0xc6 },
        .parse => .{ .r = 0x6f, .g = 0xd6, .b = 0x4f },
        .ast => .{ .r = 0x9a, .g = 0xdf, .b = 0x5a },
        .macro_discover, .macro_compile, .macro_expand => .{ .r = 0xdb, .g = 0x5f, .b = 0xf2 },
        .desugar => .{ .r = 0xf2, .g = 0x93, .b = 0x5f },
        .resolve => .{ .r = 0x4f, .g = 0xa6, .b = 0xff },
        .uir => .{ .r = 0xff, .g = 0xb0, .b = 0x4f },
        .typecheck => .{ .r = 0xff, .g = 0x86, .b = 0x2d },
        .mir => .{ .r = 0xff, .g = 0x6a, .b = 0x6a },
        .lir => .{ .r = 0xff, .g = 0x4f, .b = 0xaa },
        .backend => .{ .r = 0x6a, .g = 0xff, .b = 0x4f },
        .encode => .{ .r = 0xff, .g = 0x3a, .b = 0x3a },
        else => .{ .r = 0x79, .g = 0x6c, .b = 0xff },
    };
}

fn phaseTease(base: inkui.Color, now_ms: i64) inkui.Color {
    const t = @as(f64, @floatFromInt(now_ms)) / 1000.0;
    const s = (std.math.sin(t * 0.7) + 1.0) * 0.5;
    return mixColor(base, inkui.Color{ .r = 0xff, .g = 0xff, .b = 0xff }, @floatCast(s * 0.15));
}

fn mixColor(a: inkui.Color, b: inkui.Color, t: f64) inkui.Color {
    const clamped = if (t < 0.0) 0.0 else if (t > 1.0) 1.0 else t;
    return .{
        .r = @intCast(@as(u8, @intFromFloat(@as(f64, @floatFromInt(a.r)) * (1.0 - clamped) + @as(f64, @floatFromInt(b.r)) * clamped))),
        .g = @intCast(@as(u8, @intFromFloat(@as(f64, @floatFromInt(a.g)) * (1.0 - clamped) + @as(f64, @floatFromInt(b.g)) * clamped))),
        .b = @intCast(@as(u8, @intFromFloat(@as(f64, @floatFromInt(a.b)) * (1.0 - clamped) + @as(f64, @floatFromInt(b.b)) * clamped))),
    };
}

fn hexDigit(nibble: u8) u21 {
    return if (nibble < 10) @as(u21, '0') + nibble else @as(u21, 'a') + (nibble - 10);
}

fn joinStack(allocator: mem_allocator, stack: []const []const u8) ![]u8 {
    var buf = std.array_list.Managed(u8).init(allocator);
    errdefer buf.deinit();
    for (stack, 0..) |item, idx| {
        if (idx != 0) try buf.appendSlice(" -> ");
        try buf.appendSlice(item);
    }
    return buf.toOwnedSlice();
}

const TraceJsonWriter = struct {
    allocator: mem_allocator,
    file: std.fs.File,
    writer: std.fs.File.Writer,
    buf: []u8,

    fn init(allocator: mem_allocator, path: []const u8) !TraceJsonWriter {
        var file = try std.fs.cwd().createFile(path, .{ .truncate = true });
        const buf = try allocator.alloc(u8, 8192);
        errdefer allocator.free(buf);
        return .{
            .allocator = allocator,
            .file = file,
            .writer = file.writer(buf),
            .buf = buf,
        };
    }

    fn deinit(self: *TraceJsonWriter) void {
        self.file.close();
        self.allocator.free(self.buf);
    }

    fn writeEvent(self: *TraceJsonWriter, event: *const trace.Event, stack: []const []const u8) !void {
        var w = &self.writer.interface;
        try w.writeAll("{");
        try writeField(w, "kind", @tagName(event.kind), false);
        try writeField(w, "phase", @tagName(event.phase), true);
        if (event.file.len != 0) try writeField(w, "file", event.file, true);
        if (event.fn_name.len != 0) try writeField(w, "fn", event.fn_name, true);
        if (event.line != 0) try writeFieldNum(w, "line", event.line, true);
        if (event.column != 0) try writeFieldNum(w, "col", event.column, true);
        if (event.source_id) |sid| try writeFieldNum(w, "source_id", @intCast(sid), true);
        if (event.span) |sp| {
            try w.writeAll(",\"span\":{\"start\":");
            try w.print("{d},\"end\":{d}", .{ sp.start, sp.end });
            try w.writeAll("}");
        }
        if (event.index) |idx| try writeFieldNum(w, "index", idx, true);
        if (event.index2) |idx2| try writeFieldNum(w, "index2", idx2, true);
        if (event.tag) |tag| try writeField(w, "tag", tag, true);
        if (event.message) |msg| try writeField(w, "message", msg, true);
        if (stack.len > 0) {
            try w.writeAll(",\"stack\":[");
            for (stack, 0..) |frame, idx| {
                if (idx != 0) try w.writeAll(",");
                try writeJsonString(w, frame);
            }
            try w.writeAll("]");
        }
        try w.writeAll("}\n");
        try w.flush();
    }
};

fn writeField(writer: *std.Io.Writer, key: []const u8, value: []const u8, leading: bool) !void {
    if (leading) try writer.writeAll(",");
    try writeJsonString(writer, key);
    try writer.writeAll(":");
    try writeJsonString(writer, value);
}

fn writeFieldNum(writer: *std.Io.Writer, key: []const u8, value: u64, leading: bool) !void {
    if (leading) try writer.writeAll(",");
    try writeJsonString(writer, key);
    try writer.writeAll(":");
    try writer.print("{d}", .{value});
}

fn writeJsonString(writer: *std.Io.Writer, text: []const u8) !void {
    try writer.writeAll("\"");
    for (text) |ch| {
        switch (ch) {
            '\\' => try writer.writeAll("\\\\"),
            '"' => try writer.writeAll("\\\""),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            else => {
                if (ch < 0x20) {
                    try writer.print("\\u{X:0>4}", .{ch});
                } else {
                    var buf: [1]u8 = .{ch};
                    try writer.writeAll(buf[0..]);
                }
            },
        }
    }
    try writer.writeAll("\"");
}
