const std = @import("std");
const ink = @import("ink");

const trace = ink.trace;
const mem_allocator = std.mem.Allocator;

pub const AiMode = enum { rich, hybrid, terse };

pub const AiRequestKind = enum { event, phase_summary, diagnostic };

pub const PullProgress = struct {
    status: []const u8,
    completed: ?u64 = null,
    total: ?u64 = null,
};

pub const PullProgressFn = fn (ctx: ?*anyopaque, progress: PullProgress) void;

pub const AiRequest = struct {
    kind: AiRequestKind,
    phase: trace.Phase,
    tag: ?[]const u8 = null,
    message: ?[]const u8 = null,
    source_id: ?ink.compiler.source_id = null,
    span: ?ink.source.span = null,
    source_path: ?[]u8 = null,
    source_line: ?[]u8 = null,
    compiler_path: ?[]u8 = null,
    compiler_line: ?[]u8 = null,
    stack_text: ?[]u8 = null,
    timestamp_ms: i64 = 0,

    pub fn deinit(self: *AiRequest, allocator: mem_allocator) void {
        if (self.source_path) |buf| allocator.free(buf);
        if (self.source_line) |buf| allocator.free(buf);
        if (self.compiler_path) |buf| allocator.free(buf);
        if (self.compiler_line) |buf| allocator.free(buf);
        if (self.stack_text) |buf| allocator.free(buf);
    }
};

pub const AiNote = struct {
    kind: AiRequestKind,
    phase: trace.Phase,
    source_id: ?ink.compiler.source_id = null,
    span: ?ink.source.span = null,
    message: []u8,
    from_llm: bool = false,

    pub fn deinit(self: *AiNote, allocator: mem_allocator) void {
        allocator.free(self.message);
    }
};

pub const AiConfig = struct {
    enable: bool = false,
    use_ollama: bool = false,
    ollama_url: []const u8 = "http://127.0.0.1:11434",
    ollama_model: []const u8 = "devstral-small-2",
    timeout_ms: i64 = 4000,
    pull_timeout_ms: i64 = 300000,
    max_prompt_bytes: usize = 1400,
    max_response_bytes: usize = 8192,
    max_queue: usize = 0,
};

pub const AiEngine = struct {
    allocator: mem_allocator,
    config: AiConfig,
    req_queue: RequestQueue,
    out_queue: NoteQueue,
    thread: ?std.Thread = null,
    stop_flag: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),
    mode: AiMode = .rich,
    avg_latency_ms: f64 = 0.0,
    fail_count: u32 = 0,
    cache: std.StringHashMap([]u8),

    pub fn init(allocator: mem_allocator, config: AiConfig) AiEngine {
        return .{
            .allocator = allocator,
            .config = config,
            .req_queue = RequestQueue.init(allocator),
            .out_queue = NoteQueue.init(allocator),
            .cache = std.StringHashMap([]u8).init(allocator),
        };
    }

    pub fn start(self: *AiEngine) !void {
        if (!self.config.enable) return;
        self.thread = try std.Thread.spawn(.{}, workerMain, .{self});
    }

    pub fn stop(self: *AiEngine) void {
        self.stop_flag.store(true, .seq_cst);
        self.req_queue.clear(self.allocator);
        self.req_queue.signal();
        if (self.thread) |th| th.join();
        self.thread = null;
    }

    pub fn deinit(self: *AiEngine) void {
        self.stop();
        self.req_queue.deinit(self.allocator);
        self.out_queue.deinit(self.allocator);
        var it = self.cache.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.value_ptr.*);
        }
        self.cache.deinit();
    }

    pub fn push(self: *AiEngine, req: AiRequest) void {
        if (!self.config.enable) return;
        if (self.config.max_queue != 0 and self.req_queue.len() > self.config.max_queue and req.kind == .event) {
            var drop = req;
            drop.deinit(self.allocator);
            return;
        }
        self.req_queue.push(req) catch {
            var drop = req;
            drop.deinit(self.allocator);
        };
    }

    pub fn drain(self: *AiEngine, out: *std.array_list.Managed(AiNote)) void {
        if (!self.config.enable) return;
        self.out_queue.drain(out);
    }

    pub fn currentMode(self: *AiEngine) AiMode {
        return self.mode;
    }

    pub fn queueLen(self: *AiEngine) usize {
        return self.req_queue.len();
    }
};

pub fn ensureOllamaReady(
    allocator: mem_allocator,
    config: AiConfig,
    ctx: ?*anyopaque,
    progress_fn: ?*const PullProgressFn,
) !void {
    if (!config.enable or !config.use_ollama) return;
    const model = config.ollama_model;
    if (progress_fn) |cb| cb(ctx, .{ .status = "checking model" });
    const available = ollamaShow(allocator, config, model) catch false;
    if (available) {
        if (progress_fn) |cb| cb(ctx, .{ .status = "ready" });
        return;
    }
    if (progress_fn) |cb| cb(ctx, .{ .status = "pulling model" });
    try ollamaPull(allocator, config, model, ctx, progress_fn);
    if (progress_fn) |cb| cb(ctx, .{ .status = "ready" });
}

const RequestQueue = struct {
    mutex: std.Thread.Mutex = .{},
    cond: std.Thread.Condition = .{},
    items: std.array_list.Managed(AiRequest),
    read_index: usize = 0,

    fn init(allocator: mem_allocator) RequestQueue {
        return .{ .items = std.array_list.Managed(AiRequest).init(allocator) };
    }

    fn deinit(self: *RequestQueue, allocator: mem_allocator) void {
        var idx = self.read_index;
        while (idx < self.items.items.len) : (idx += 1) {
            self.items.items[idx].deinit(allocator);
        }
        self.items.deinit();
    }

    fn len(self: *RequestQueue) usize {
        self.mutex.lock();
        defer self.mutex.unlock();
        if (self.read_index >= self.items.items.len) return 0;
        return self.items.items.len - self.read_index;
    }

    fn push(self: *RequestQueue, req: AiRequest) !void {
        self.mutex.lock();
        defer self.mutex.unlock();
        try self.items.append(req);
        self.cond.signal();
    }

    fn popWait(self: *RequestQueue, stop: *std.atomic.Value(bool)) ?AiRequest {
        self.mutex.lock();
        defer self.mutex.unlock();
        while (self.read_index >= self.items.items.len) {
            if (stop.load(.seq_cst)) return null;
            self.cond.wait(&self.mutex);
        }
        const item = self.items.items[self.read_index];
        self.read_index += 1;
        if (self.read_index > 1024 and self.read_index >= self.items.items.len / 2) {
            const remaining = self.items.items[self.read_index..];
            std.mem.copyForwards(AiRequest, self.items.items[0..remaining.len], remaining);
            self.items.items.len = remaining.len;
            self.read_index = 0;
        }
        return item;
    }

    fn clear(self: *RequestQueue, allocator: mem_allocator) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        var idx = self.read_index;
        while (idx < self.items.items.len) : (idx += 1) {
            self.items.items[idx].deinit(allocator);
        }
        self.items.items.len = 0;
        self.read_index = 0;
    }

    fn signal(self: *RequestQueue) void {
        self.cond.signal();
    }
};

const NoteQueue = struct {
    mutex: std.Thread.Mutex = .{},
    items: std.array_list.Managed(AiNote),

    fn init(allocator: mem_allocator) NoteQueue {
        return .{ .items = std.array_list.Managed(AiNote).init(allocator) };
    }

    fn deinit(self: *NoteQueue, allocator: mem_allocator) void {
        for (self.items.items) |*note| note.deinit(allocator);
        self.items.deinit();
    }

    fn push(self: *NoteQueue, note: AiNote) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        self.items.append(note) catch {};
    }

    fn drain(self: *NoteQueue, out: *std.array_list.Managed(AiNote)) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        for (self.items.items) |note| {
            out.append(note) catch {};
        }
        self.items.clearRetainingCapacity();
    }
};

fn workerMain(self: *AiEngine) void {
    while (true) {
        const req_opt = self.req_queue.popWait(&self.stop_flag);
        if (req_opt == null) break;
        var req = req_opt.?;
        const start_ms = std.time.milliTimestamp();
        const note = processRequest(self, &req) catch null;
        const elapsed_ms = std.time.milliTimestamp() - start_ms;
        updateStats(self, elapsed_ms);
        req.deinit(self.allocator);
        if (note) |item| {
            self.out_queue.push(item);
        }
    }
}

fn processRequest(self: *AiEngine, req: *AiRequest) !AiNote {
    const use_llm = shouldUseLlm(self, req.kind);
    const signature = signatureFor(req, self.allocator) catch null;
    defer if (signature) |sig| self.allocator.free(sig);

    if (use_llm and signature != null) {
        if (self.cache.get(signature.?)) |cached| {
            const duped = try self.allocator.dupe(u8, cached);
            return .{
                .kind = req.kind,
                .phase = req.phase,
                .source_id = req.source_id,
                .span = req.span,
                .message = duped,
            };
        }
    }

    var message: ?[]u8 = null;
    var from_llm = false;
    var llm_err: ?anyerror = null;
    if (use_llm and self.config.use_ollama) {
        message = callOllama(self, req) catch |err| blk: {
            llm_err = err;
            break :blk null;
        };
        if (message != null) {
            from_llm = true;
            self.fail_count = 0;
        }
    }
    if (message == null) {
        if (llm_err) |err| {
            self.fail_count += 1;
            if (self.fail_count <= 3 or (self.fail_count % 50) == 0) {
                message = try std.fmt.allocPrint(self.allocator, "ollama error: {s}", .{@errorName(err)});
            } else {
                message = try templateMessage(self.allocator, req);
            }
        } else {
            message = try templateMessage(self.allocator, req);
        }
    }
    if (signature != null and use_llm and message != null) {
        const cached = try self.allocator.dupe(u8, message.?);
        _ = self.cache.put(signature.?, cached) catch {};
    }
    return .{
        .kind = req.kind,
        .phase = req.phase,
        .source_id = req.source_id,
        .span = req.span,
        .message = message.?,
        .from_llm = from_llm,
    };
}

fn shouldUseLlm(self: *AiEngine, kind: AiRequestKind) bool {
    if (!self.config.use_ollama) return false;
    return switch (kind) {
        .diagnostic, .phase_summary => true,
        .event => false,
    };
}

fn updateStats(self: *AiEngine, elapsed_ms: i64) void {
    if (elapsed_ms <= 0) return;
    const sample = @as(f64, @floatFromInt(elapsed_ms));
    if (self.avg_latency_ms == 0.0) {
        self.avg_latency_ms = sample;
    } else {
        self.avg_latency_ms = self.avg_latency_ms * 0.9 + sample * 0.1;
    }
    const backlog = self.req_queue.len();
    if (self.avg_latency_ms > 120 or backlog > 200) {
        self.mode = .terse;
    } else if (self.avg_latency_ms > 60 or backlog > 50) {
        self.mode = .hybrid;
    } else {
        self.mode = .rich;
    }
}

fn signatureFor(req: *AiRequest, allocator: mem_allocator) mem_allocator.Error!?[]u8 {
    _ = req;
    _ = allocator;
    return null;
}

fn templateMessage(allocator: mem_allocator, req: *AiRequest) mem_allocator.Error![]u8 {
    const phase = @tagName(req.phase);
    if (req.kind == .diagnostic) {
        if (req.message) |msg| {
            return std.fmt.allocPrint(allocator, "diag: {s}", .{msg});
        }
        return allocator.dupe(u8, "diag: issue");
    }
    if (req.kind == .phase_summary) {
        return std.fmt.allocPrint(allocator, "phase {s} complete", .{phase});
    }
    if (req.tag) |tag| {
        if (req.message) |msg| {
            return std.fmt.allocPrint(allocator, "{s}: {s} {s}", .{ phase, tag, msg });
        }
        return std.fmt.allocPrint(allocator, "{s}: {s}", .{ phase, tag });
    }
    return std.fmt.allocPrint(allocator, "{s}: step", .{phase});
}

fn callOllama(self: *AiEngine, req: *AiRequest) ![]u8 {
    const prompt = try buildPrompt(self.allocator, req, self.mode, self.config.max_prompt_bytes);
    defer self.allocator.free(prompt);
    return ollamaGenerateWithPrompt(self.allocator, self.config, prompt);
}

pub fn ollamaGenerate(allocator: mem_allocator, config: AiConfig, prompt: []const u8) ![]u8 {
    return ollamaGenerateWithPrompt(allocator, config, prompt);
}

fn ollamaGenerateWithPrompt(allocator: mem_allocator, config: AiConfig, prompt: []const u8) ![]u8 {
    var target = try parseOllamaUrl(allocator, config.ollama_url);
    defer target.deinit(allocator);

    const prompt_json = try jsonString(prompt, allocator);
    defer allocator.free(prompt_json);
    const req_body = try std.fmt.allocPrint(
        allocator,
        "{{\"model\":\"{s}\",\"prompt\":{s},\"stream\":false}}",
        .{ config.ollama_model, prompt_json },
    );
    defer allocator.free(req_body);

    const resp_body = try httpPostJson(
        allocator,
        target.host,
        target.port,
        "/api/generate",
        req_body,
        config.timeout_ms,
        config.max_response_bytes,
    );
    defer allocator.free(resp_body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, resp_body, .{});
    defer parsed.deinit();
    const obj = parsed.value.object;
    const resp_val = obj.get("response") orelse return error.InvalidResponse;
    if (resp_val != .string) return error.InvalidResponse;
    var text = try allocator.dupe(u8, resp_val.string);
    trimInPlace(&text);
    if (text.len == 0) {
        allocator.free(text);
        return error.InvalidResponse;
    }
    return text;
}

fn ollamaShow(allocator: mem_allocator, config: AiConfig, model: []const u8) !bool {
    var target = try parseOllamaUrl(allocator, config.ollama_url);
    defer target.deinit(allocator);
    const req_body = try std.fmt.allocPrint(allocator, "{{\"name\":\"{s}\"}}", .{model});
    defer allocator.free(req_body);
    const resp_body = try httpPostJson(
        allocator,
        target.host,
        target.port,
        "/api/show",
        req_body,
        config.timeout_ms,
        config.max_response_bytes,
    );
    defer allocator.free(resp_body);
    const parsed = std.json.parseFromSlice(std.json.Value, allocator, resp_body, .{}) catch return false;
    defer parsed.deinit();
    if (parsed.value != .object) return false;
    const obj = parsed.value.object;
    if (obj.get("error") != null) return false;
    return true;
}

fn ollamaPull(
    allocator: mem_allocator,
    config: AiConfig,
    model: []const u8,
    ctx: ?*anyopaque,
    progress_fn: ?*const PullProgressFn,
) !void {
    var target = try parseOllamaUrl(allocator, config.ollama_url);
    defer target.deinit(allocator);
    const body = try std.fmt.allocPrint(allocator, "{{\"model\":\"{s}\",\"stream\":true}}", .{model});
    defer allocator.free(body);
    try httpPostJsonStream(
        allocator,
        target.host,
        target.port,
        "/api/pull",
        body,
        config.pull_timeout_ms,
        ctx,
        progress_fn,
    );
}

fn buildPrompt(allocator: mem_allocator, req: *AiRequest, mode: AiMode, max_bytes: usize) mem_allocator.Error![]u8 {
    var buf = std.array_list.Managed(u8).init(allocator);
    errdefer buf.deinit();
    try buf.appendSlice("Explain the compiler step in plain language. ");
    switch (mode) {
        .rich => try buf.appendSlice("1-2 sentences.\n"),
        .hybrid => try buf.appendSlice("Short sentence.\n"),
        .terse => try buf.appendSlice("Very short.\n"),
    }
    try buf.appendSlice("Phase: ");
    try buf.appendSlice(@tagName(req.phase));
    try buf.appendSlice("\nKind: ");
    try buf.appendSlice(@tagName(req.kind));
    if (req.tag) |tag| {
        try buf.appendSlice("\nTag: ");
        try buf.appendSlice(tag);
    }
    if (req.message) |msg| {
        try buf.appendSlice("\nMsg: ");
        try buf.appendSlice(msg);
    }
    if (req.source_line) |line| {
        try buf.appendSlice("\nSrc: ");
        try buf.appendSlice(truncateLine(line, 120));
    }
    if (req.compiler_line) |line| {
        try buf.appendSlice("\nComp: ");
        try buf.appendSlice(truncateLine(line, 120));
    }
    if (req.stack_text) |stack| {
        try buf.appendSlice("\nStack: ");
        try buf.appendSlice(truncateLine(stack, 160));
    }
    if (buf.items.len > max_bytes) {
        buf.items.len = max_bytes;
    }
    return buf.toOwnedSlice();
}

fn truncateLine(line: []const u8, max_len: usize) []const u8 {
    if (line.len <= max_len) return line;
    return line[0..max_len];
}

fn trimInPlace(text: *[]u8) void {
    var start: usize = 0;
    while (start < text.*.len and std.ascii.isWhitespace(text.*[start])) start += 1;
    var end: usize = text.*.len;
    while (end > start and std.ascii.isWhitespace(text.*[end - 1])) end -= 1;
    if (start > 0 or end < text.*.len) {
        const trimmed = text.*[start..end];
        std.mem.copyForwards(u8, text.*[0..trimmed.len], trimmed);
        text.*.len = trimmed.len;
    }
}

const ParsedUrl = struct {
    host: []u8,
    port: u16,

    fn deinit(self: *ParsedUrl, allocator: mem_allocator) void {
        allocator.free(self.host);
    }
};

fn parseOllamaUrl(allocator: mem_allocator, url: []const u8) !ParsedUrl {
    var raw = url;
    if (std.mem.startsWith(u8, raw, "http://")) {
        raw = raw["http://".len..];
    } else if (std.mem.startsWith(u8, raw, "https://")) {
        raw = raw["https://".len..];
    }
    if (std.mem.indexOfAny(u8, raw, "/?")) |cut| {
        raw = raw[0..cut];
    }
    if (raw.len == 0) return error.InvalidUrl;

    var host: []const u8 = raw;
    var port: u16 = 11434;

    if (raw[0] == '[') {
        const end = std.mem.indexOfScalar(u8, raw, ']') orelse return error.InvalidUrl;
        host = raw[1..end];
        if (end + 1 < raw.len) {
            if (raw[end + 1] != ':') return error.InvalidUrl;
            const port_str = raw[end + 2 ..];
            if (port_str.len == 0) return error.InvalidUrl;
            port = std.fmt.parseInt(u16, port_str, 10) catch return error.InvalidUrl;
        }
    } else if (std.mem.lastIndexOfScalar(u8, raw, ':')) |colon| {
        const host_part = raw[0..colon];
        const port_part = raw[colon + 1 ..];
        if (host_part.len == 0 or port_part.len == 0) return error.InvalidUrl;
        host = host_part;
        port = std.fmt.parseInt(u16, port_part, 10) catch return error.InvalidUrl;
    } else {
        host = raw;
    }
    if (host.len == 0) return error.InvalidUrl;
    return .{ .host = try allocator.dupe(u8, host), .port = port };
}

fn connectTcp(allocator: mem_allocator, host: []const u8, port: u16) !std.net.Stream {
    if (@hasDecl(std.net, "tcpConnectToHost")) {
        return std.net.tcpConnectToHost(allocator, host, port);
    }
    if (@hasDecl(std.net.Address, "resolveIp")) {
        const address = try std.net.Address.resolveIp(host, port);
        return std.net.tcpConnectToAddress(address);
    }
    const address = try std.net.Address.parseIp(host, port);
    return std.net.tcpConnectToAddress(address);
}

fn httpPostJson(
    allocator: mem_allocator,
    host: []const u8,
    port: u16,
    path: []const u8,
    body: []const u8,
    timeout_ms: i64,
    max_bytes: usize,
) ![]u8 {
    var stream = try connectTcp(allocator, host, port);
    defer stream.close();

    const request = try std.fmt.allocPrint(allocator,
        "POST {s} HTTP/1.1\r\nHost: {s}\r\nContent-Type: application/json\r\nContent-Length: {d}\r\nConnection: close\r\n\r\n{s}",
        .{ path, host, body.len, body },
    );
    defer allocator.free(request);
    try stream.writeAll(request);

    try setNonBlocking(stream.handle);
    const data = try readAllWithTimeout(allocator, stream.handle, timeout_ms, max_bytes);
    defer allocator.free(data);
    return decodeHttpBody(allocator, data);
}

fn httpPostJsonStream(
    allocator: mem_allocator,
    host: []const u8,
    port: u16,
    path: []const u8,
    body: []const u8,
    timeout_ms: i64,
    ctx: ?*anyopaque,
    progress_fn: ?*const PullProgressFn,
) !void {
    var stream = try connectTcp(allocator, host, port);
    defer stream.close();
    const request = try std.fmt.allocPrint(allocator,
        "POST {s} HTTP/1.1\r\nHost: {s}\r\nContent-Type: application/json\r\nContent-Length: {d}\r\nConnection: close\r\n\r\n{s}",
        .{ path, host, body.len, body },
    );
    defer allocator.free(request);
    try stream.writeAll(request);

    try setNonBlocking(stream.handle);
    var pending = std.array_list.Managed(u8).init(allocator);
    defer pending.deinit();
    var buf: [4096]u8 = undefined;
    var headers_done = false;
    var last_progress = std.time.milliTimestamp();
    while (true) {
        const now = std.time.milliTimestamp();
        if (now - last_progress > timeout_ms) return error.Timeout;
        var fds = [_]std.posix.pollfd{
            .{ .fd = stream.handle, .events = std.posix.POLL.IN, .revents = 0 },
        };
        const wait_ms = @max(@as(i32, 1), @as(i32, @intCast(timeout_ms - (now - last_progress))));
        const res = std.posix.poll(&fds, wait_ms) catch return error.PollError;
        if (res == 0) continue;
        const n = std.posix.read(stream.handle, &buf) catch |err| switch (err) {
            error.WouldBlock => continue,
            else => return err,
        };
        if (n == 0) break;
        last_progress = std.time.milliTimestamp();
        try pending.appendSlice(buf[0..n]);

        while (true) {
            if (!headers_done) {
                if (std.mem.indexOf(u8, pending.items, "\r\n\r\n")) |idx| {
                    const body_start = idx + 4;
                    if (body_start < pending.items.len) {
                        const tail = pending.items[body_start..];
                        std.mem.copyForwards(u8, pending.items[0..tail.len], tail);
                        pending.items.len = tail.len;
                    } else {
                        pending.items.len = 0;
                    }
                    headers_done = true;
                    continue;
                }
                break;
            }
            if (std.mem.indexOfScalar(u8, pending.items, '\n')) |line_end| {
                const line_raw = pending.items[0..line_end];
                const line = std.mem.trim(u8, line_raw, " \r\n");
                if (line.len != 0) {
                    try handlePullLine(allocator, line, ctx, progress_fn);
                }
                const rest = pending.items[line_end + 1 ..];
                std.mem.copyForwards(u8, pending.items[0..rest.len], rest);
                pending.items.len = rest.len;
            } else {
                break;
            }
        }
    }
}

fn decodeHttpBody(allocator: mem_allocator, response: []const u8) ![]u8 {
    const header_end = std.mem.indexOf(u8, response, "\r\n\r\n") orelse {
        return allocator.dupe(u8, response);
    };
    const headers = response[0..header_end];
    const body = response[header_end + 4 ..];
    if (!headerHasToken(headers, "transfer-encoding", "chunked")) {
        return allocator.dupe(u8, body);
    }
    return dechunk(allocator, body);
}

fn headerHasToken(headers: []const u8, name: []const u8, token: []const u8) bool {
    var it = std.mem.splitSequence(u8, headers, "\r\n");
    _ = it.next(); // status line
    while (it.next()) |line| {
        if (line.len == 0) continue;
        const colon = std.mem.indexOfScalar(u8, line, ':') orelse continue;
        const header_name = std.mem.trim(u8, line[0..colon], " \t");
        if (!std.ascii.eqlIgnoreCase(header_name, name)) continue;
        const header_value = std.mem.trim(u8, line[colon + 1 ..], " \t");
        var vals = std.mem.splitScalar(u8, header_value, ',');
        while (vals.next()) |val| {
            const trimmed = std.mem.trim(u8, val, " \t");
            if (std.ascii.eqlIgnoreCase(trimmed, token)) return true;
        }
    }
    return false;
}

fn dechunk(allocator: mem_allocator, body: []const u8) ![]u8 {
    var out = std.array_list.Managed(u8).init(allocator);
    errdefer out.deinit();
    var idx: usize = 0;
    while (idx < body.len) {
        const line_end = std.mem.indexOfPos(u8, body, idx, "\r\n") orelse return error.InvalidResponse;
        var size_str = std.mem.trim(u8, body[idx..line_end], " \t");
        if (std.mem.indexOfScalar(u8, size_str, ';')) |semi| {
            size_str = size_str[0..semi];
        }
        const size = std.fmt.parseInt(usize, size_str, 16) catch return error.InvalidResponse;
        idx = line_end + 2;
        if (size == 0) break;
        if (idx + size > body.len) return error.InvalidResponse;
        try out.appendSlice(body[idx .. idx + size]);
        idx += size;
        if (idx + 1 >= body.len or body[idx] != '\r' or body[idx + 1] != '\n') {
            return error.InvalidResponse;
        }
        idx += 2;
    }
    return out.toOwnedSlice();
}

fn handlePullLine(
    allocator: mem_allocator,
    line: []const u8,
    ctx: ?*anyopaque,
    progress_fn: ?*const PullProgressFn,
) !void {
    const parsed = std.json.parseFromSlice(std.json.Value, allocator, line, .{}) catch return;
    defer parsed.deinit();
    if (parsed.value != .object) return;
    const obj = parsed.value.object;
    if (obj.get("error")) |err_val| {
        if (err_val == .string) return error.OllamaError;
        return error.OllamaError;
    }
    if (progress_fn == null) return;
    const status_val = obj.get("status") orelse return;
    if (status_val != .string) return;
    const status = status_val.string;
    var completed: ?u64 = null;
    var total: ?u64 = null;
    if (obj.get("completed")) |comp_val| {
        if (comp_val == .integer) completed = @intCast(comp_val.integer);
    }
    if (obj.get("total")) |total_val| {
        if (total_val == .integer) total = @intCast(total_val.integer);
    }
    const progress = PullProgress{
        .status = status,
        .completed = completed,
        .total = total,
    };
    if (progress_fn) |cb| cb(ctx, progress);
}

fn readAllWithTimeout(allocator: mem_allocator, fd: std.posix.fd_t, timeout_ms: i64, max_bytes: usize) ![]u8 {
    var out = std.array_list.Managed(u8).init(allocator);
    errdefer out.deinit();
    var buf: [4096]u8 = undefined;
    var last_progress = std.time.milliTimestamp();
    while (true) {
        const now = std.time.milliTimestamp();
        if (now - last_progress > timeout_ms) return error.Timeout;
        var fds = [_]std.posix.pollfd{
            .{ .fd = fd, .events = std.posix.POLL.IN, .revents = 0 },
        };
        const wait_ms = @max(@as(i32, 1), @as(i32, @intCast(timeout_ms - (now - last_progress))));
        const res = std.posix.poll(&fds, wait_ms) catch return error.PollError;
        if (res == 0) continue;
        const n = std.posix.read(fd, &buf) catch |err| switch (err) {
            error.WouldBlock => continue,
            else => return err,
        };
        if (n == 0) break;
        last_progress = std.time.milliTimestamp();
        try out.appendSlice(buf[0..n]);
        if (out.items.len > max_bytes) return error.ResponseTooLarge;
    }
    return out.toOwnedSlice();
}

fn setNonBlocking(fd: std.posix.fd_t) !void {
    const flags = try std.posix.fcntl(fd, std.posix.F.GETFL, 0);
    const nonblock = @as(@TypeOf(flags), 1) << @bitOffsetOf(std.posix.O, "NONBLOCK");
    _ = try std.posix.fcntl(fd, std.posix.F.SETFL, flags | nonblock);
}

fn jsonString(text: []const u8, allocator: mem_allocator) mem_allocator.Error![]u8 {
    var out = std.array_list.Managed(u8).init(allocator);
    errdefer out.deinit();
    try out.append('"');
    for (text) |ch| {
        switch (ch) {
            '\\' => try out.appendSlice("\\\\"),
            '"' => try out.appendSlice("\\\""),
            '\n' => try out.appendSlice("\\n"),
            '\r' => try out.appendSlice("\\r"),
            '\t' => try out.appendSlice("\\t"),
            else => {
                if (ch < 0x20) {
                    var buf: [6]u8 = undefined;
                    const esc = std.fmt.bufPrint(&buf, "\\u{X:0>4}", .{ch}) catch "\\u0000";
                    try out.appendSlice(esc);
                } else {
                    try out.append(ch);
                }
            },
        }
    }
    try out.append('"');
    return out.toOwnedSlice();
}
