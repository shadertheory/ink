const std = @import("std");
const ink = @import("ink");
const eval_graph = @import("eval_graph.zig");

const mem_allocator = std.mem.Allocator;

pub const EvalOutcome = union(enum) {
    ok: []u8,
    err: []u8,
};

pub const EvalState = struct {
    allocator: mem_allocator,
    root_dir: []const u8,
    graph: ?eval_graph.Graph = null,
    next_source_id: ink.compiler.source_id = 0,
    next_eval_id: usize = 0,
    repl: ?ReplContext = null,

    pub fn init(allocator: mem_allocator, root_dir: []const u8) EvalState {
        return .{
            .allocator = allocator,
            .root_dir = root_dir,
        };
    }

    pub fn deinit(self: *EvalState) void {
        if (self.graph) |*graph| {
            graph.deinit();
            self.graph = null;
        }
        if (self.repl) |*ctx| {
            ctx.deinit();
            self.repl = null;
        }
    }

    pub fn eval_frame(
        self: *EvalState,
        host: EvalHost,
        frame: FrameInfo,
        locals: []const ink.vm.inkb.debug_local,
        expr: []const u8,
    ) !EvalOutcome {
        return self.eval_with_locals(host, frame, locals, expr, .frame);
    }

    pub fn eval_repl(
        self: *EvalState,
        host: EvalHost,
        expr: []const u8,
    ) !EvalOutcome {
        var ctx = try self.ensure_repl(host);
        const locals = try ctx.locals_as_debug(self.allocator);
        defer self.allocator.free(locals);
        const frame = FrameInfo{
            .task_id = 0,
            .pc = ctx.executor.current.pc,
            .fp = ctx.fp,
            .sp = ctx.sp,
        };
        return self.eval_with_locals(host, frame, locals, expr, .repl);
    }

    fn ensure_graph(self: *EvalState) !*eval_graph.Graph {
        if (self.graph) |*graph| return graph;
        const graph = try eval_graph.build(self.allocator, self.root_dir);
        self.next_source_id = graph.next_source_id;
        self.graph = graph;
        return &self.graph.?;
    }

    fn ensure_repl(self: *EvalState, host: EvalHost) !*ReplContext {
        if (self.repl) |*ctx| return ctx;
        const ctx = try ReplContext.init(self.allocator, host);
        self.repl = ctx;
        return &self.repl.?;
    }

    const EvalMode = enum { frame, repl };

    fn eval_with_locals(
        self: *EvalState,
        host: EvalHost,
        frame: FrameInfo,
        locals: []const ink.vm.inkb.debug_local,
        expr: []const u8,
        mode: EvalMode,
    ) !EvalOutcome {
        if (expr.len == 0) {
            return .{ .err = try self.allocator.dupe(u8, "empty expression") };
        }

        const graph = try self.ensure_graph();
        var locals_for_eval = locals;
        var merged_locals: ?[]ink.vm.inkb.debug_local = null;
        defer if (merged_locals) |slice| self.allocator.free(slice);
        var repl_locals: ?[]ink.vm.inkb.debug_local = null;
        defer if (repl_locals) |slice| self.allocator.free(slice);
        var repl_ctx: ?*ReplContext = null;
        var exec: *ink.vm.bytecode.executor = undefined;
        var update_frame = false;

        if (mode == .frame) {
            exec = task_executor(host.scheduler, frame.task_id) orelse {
                return .{ .err = try self.allocator.dupe(u8, "no executor for task") };
            };
            repl_ctx = try self.ensure_repl(host);
            repl_locals = try repl_ctx.?.locals_as_debug(self.allocator);
            merged_locals = try merge_locals(self.allocator, locals, repl_locals.?);
            locals_for_eval = merged_locals.?;
            update_frame = true;
        } else {
            repl_ctx = try self.ensure_repl(host);
            exec = &repl_ctx.?.executor;
        }

        const eval_id = self.next_eval_id;
        self.next_eval_id += 1;
        var eval_source = try build_eval_source(self.allocator, &self.next_source_id, eval_id, locals_for_eval, expr, true);
        defer eval_source.deinit(self.allocator);

        var compile = try build_eval_compile(self.allocator, graph, &eval_source);
        defer compile.deinit(self.allocator);

        if (!compile.ok) {
            if (diagnostics_hit_show(compile.diagnostics, eval_source)) {
                var fallback_source = try build_eval_source(self.allocator, &self.next_source_id, eval_id, locals_for_eval, expr, false);
                defer fallback_source.deinit(self.allocator);
                var fallback_compile = try build_eval_compile(self.allocator, graph, &fallback_source);
                defer fallback_compile.deinit(self.allocator);
                if (!fallback_compile.ok) {
                    const msg = try format_diagnostics(self.allocator, fallback_compile, graph.sources.items);
                    return .{ .err = msg };
                }
                return try execute_eval(
                    self,
                    host,
                    exec,
                    frame,
                    locals,
                    &fallback_source,
                    &fallback_compile,
                    repl_ctx,
                    update_frame,
                );
            }
            const msg = try format_diagnostics(self.allocator, compile, graph.sources.items);
            return .{ .err = msg };
        }

        return try execute_eval(
            self,
            host,
            exec,
            frame,
            locals,
            &eval_source,
            &compile,
            repl_ctx,
            update_frame,
        );
    }
};

pub const EvalHost = struct {
    program: *const ink.vm.inkb.program,
    scheduler: *ink.runtime.scheduler.scheduler,
    debug: *ink.vm.debug.Controller,
    lib_dir: []const u8,
    debug_checks: bool,
    output: ?ink.vm.OutputSink = null,
};

pub const FrameInfo = struct {
    task_id: ink.runtime.scheduler.task_id,
    pc: usize,
    fp: usize,
    sp: usize,
};

const EvalFrame = struct {
    base: usize,
    fp: usize,
    sp: usize,
};

const EvalSource = struct {
    source_id: ink.source.source_id,
    func_name: []u8,
    text: []u8,
    show_span: ?ink.source.span = null,

    fn deinit(self: *const EvalSource, allocator: mem_allocator) void {
        allocator.free(self.func_name);
        allocator.free(self.text);
    }
};

const LocalWord = struct {
    name: []const u8,
    reg: u8,
    words: u8,
    type_name: []const u8,
};

const ReplContext = struct {
    allocator: mem_allocator,
    scheduler: *ink.runtime.scheduler.scheduler,
    memory: ink.vm.vm.tape,
    executor: ink.vm.bytecode.executor,
    fp: usize,
    sp: usize,
    locals: std.StringHashMap(LocalSlot),
    next_reg: u8,

    const LocalSlot = struct {
        reg: u8,
        words: u8,
        type_name: []const u8,
    };

    fn init(allocator: mem_allocator, host: EvalHost) !ReplContext {
        var memory = ink.vm.vm.tape.init(allocator, 1024 * 1024);
        var executor = ink.vm.bytecode.executor.init(
            .{ .pc = 0, .fp = 0, .sp = 1 },
            &memory,
            host.program.constants,
            host.program.data,
            host.program.foreigns,
            allocator,
            host.scheduler,
            host.lib_dir,
            host.program.bytecode,
            host.debug_checks,
            null,
            null,
            host.output,
        );
        executor.set_task_context(host.scheduler, 0);
        const fp: usize = 2;
        const sp: usize = 2 + ink.vm.register_count;
        executor.current.fp = fp;
        executor.current.sp = sp;
        executor.arg_base = sp;
        executor.arg_base_valid = false;
        executor.memory.write(0, 0);
        executor.memory.write(1, 0);
        return .{
            .allocator = allocator,
            .scheduler = host.scheduler,
            .memory = memory,
            .executor = executor,
            .fp = fp,
            .sp = sp,
            .locals = std.StringHashMap(LocalSlot).init(allocator),
            .next_reg = 1,
        };
    }

    fn deinit(self: *ReplContext) void {
        var it = self.locals.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.type_name);
        }
        self.locals.deinit();
        self.executor.deinit();
        self.allocator.free(self.memory.data);
    }

    fn locals_as_debug(self: *ReplContext, allocator: mem_allocator) ![]ink.vm.inkb.debug_local {
        var list = std.array_list.Managed(ink.vm.inkb.debug_local).init(allocator);
        defer list.deinit();
        var it = self.locals.iterator();
        while (it.next()) |entry| {
            const name = entry.key_ptr.*;
            const slot = entry.value_ptr.*;
            try list.append(.{
                .name = name,
                .reg = slot.reg,
                .type_name = slot.type_name,
            });
        }
        return try list.toOwnedSlice();
    }
};

fn build_eval_source(
    allocator: mem_allocator,
    next_source_id: *ink.compiler.source_id,
    eval_id: usize,
    locals: []const ink.vm.inkb.debug_local,
    expr: []const u8,
    emit_show: bool,
) !EvalSource {
    const func_name = try std.fmt.allocPrint(allocator, "__inkx_eval_{d}", .{eval_id});
    const helper_name = try std.fmt.allocPrint(allocator, "__inkx_to_string_{d}", .{eval_id});
    defer allocator.free(helper_name);
    const eval_source_id = next_source_id.*;
    next_source_id.* += 1;

    var params = std.ArrayListUnmanaged(u8){};
    defer params.deinit(allocator);
    for (locals) |local| {
        if (local.reg == 0) continue;
        const param = try std.fmt.allocPrint(allocator, "{s}: {s}", .{ local.name, local.type_name });
        defer allocator.free(param);
        if (params.items.len > 0) try params.appendSlice(allocator, ", ");
        try params.appendSlice(allocator, param);
    }

    var buf = std.ArrayListUnmanaged(u8){};
    defer buf.deinit(allocator);
    try buf.appendSlice(allocator, "fn ");
    try buf.appendSlice(allocator, helper_name);
    try buf.appendSlice(allocator, "<T: type>(value: T) -> string where T: show\n");
    try buf.appendSlice(allocator, "\tvalue.show()\n\n");
    try buf.appendSlice(allocator, "fn ");
    try buf.appendSlice(allocator, func_name);
    try buf.appendSlice(allocator, "(");
    try buf.appendSlice(allocator, params.items);
    try buf.appendSlice(allocator, ") -> string\n");
    var show_span: ?ink.source.span = null;
    if (emit_show) {
        try buf.appendSlice(allocator, "\tlet _inkx_internal_result =\n");
        const wrote = try append_block(&buf, allocator, "\t\t", expr);
        if (!wrote) {
            try buf.appendSlice(allocator, "\t\t\"\"\n");
        }
        const show_start = buf.items.len;
        try buf.appendSlice(allocator, "\t");
        try buf.appendSlice(allocator, helper_name);
        try buf.appendSlice(allocator, "(_inkx_internal_result)\n");
        show_span = .{ .start = show_start, .end = buf.items.len };
    } else {
        _ = try append_block(&buf, allocator, "\t", expr);
        try buf.appendSlice(allocator, "\t\"\"\n");
    }

    return .{
        .source_id = eval_source_id,
        .func_name = func_name,
        .text = try buf.toOwnedSlice(allocator),
        .show_span = show_span,
    };
}

fn append_block(
    buf: *std.ArrayListUnmanaged(u8),
    allocator: mem_allocator,
    indent: []const u8,
    expr: []const u8,
) !bool {
    var line_iter = std.mem.splitScalar(u8, expr, '\n');
    var wrote = false;
    while (line_iter.next()) |raw| {
        const line = std.mem.trimRight(u8, raw, "\r");
        if (line.len == 0) continue;
        wrote = true;
        try buf.appendSlice(allocator, indent);
        try buf.appendSlice(allocator, line);
        try buf.appendSlice(allocator, "\n");
    }
    return wrote;
}

fn build_eval_compile(
    allocator: mem_allocator,
    graph: *eval_graph.Graph,
    eval_source: *const EvalSource,
) !ink.compiler.compile_result {
    const eval_source_id = eval_source.source_id;
    const eval_path = try std.fmt.allocPrint(allocator, "<inkx_eval_{d}>", .{eval_source_id});
    errdefer allocator.free(eval_path);
    const eval_text_owned = try allocator.dupe(u8, eval_source.text);
    errdefer allocator.free(eval_text_owned);

    const src_count = graph.sources.items.len + 1;
    const sources = try allocator.alloc(ink.compiler.source, src_count);
    defer allocator.free(sources);
    std.mem.copyForwards(ink.compiler.source, sources[0..graph.sources.items.len], graph.sources.items);
    sources[src_count - 1] = .{ .id = eval_source_id, .path = eval_path, .text = eval_text_owned };

    const module_specs = try allocator.alloc(ink.compiler.module_spec, graph.modules.items.len);
    defer allocator.free(module_specs);
    std.mem.copyForwards(ink.compiler.module_spec, module_specs, graph.modules.items);

    var updated_slices = std.array_list.Managed([]ink.compiler.source_id).init(allocator);
    defer {
        for (updated_slices.items) |slice| allocator.free(slice);
        updated_slices.deinit();
    }

    for (module_specs, 0..) |*spec, idx| {
        if (!std.mem.eql(u8, spec.name, graph.root_module)) continue;
        const base = graph.modules.items[idx].sources;
        const slice = try allocator.alloc(ink.compiler.source_id, base.len + 1);
        std.mem.copyForwards(ink.compiler.source_id, slice[0..base.len], base);
        slice[base.len] = eval_source_id;
        spec.sources = slice;
        try updated_slices.append(slice);
    }

    var request = ink.compiler.compile_request{
        .sources = sources,
        .modules = module_specs,
        .root_module = graph.root_module,
        .target = .{ .kind = .vm },
        .debug_info = true,
    };
    if (graph.prelude) |prelude| {
        request.prelude = prelude;
    }

    const result = try ink.compiler.compile(allocator, request);
    allocator.free(eval_text_owned);
    allocator.free(eval_path);
    return result;
}

fn diagnostics_hit_show(diags: []const ink.diagnostic, eval_source: EvalSource) bool {
    const show_span = eval_source.show_span orelse return false;
    var saw_show = false;
    for (diags) |diag| {
        if (diag.span) |span| {
            if (diag.source_id != null and diag.source_id.? == eval_source.source_id) {
                if (spans_overlap(span, show_span)) return true;
            }
        }
        if (std.mem.indexOf(u8, diag.message, ".show") != null or
            std.mem.indexOf(u8, diag.message, "show(") != null or
            std.mem.indexOf(u8, diag.message, "show") != null)
        {
            saw_show = true;
        }
    }
    return saw_show;
}

fn spans_overlap(a: ink.source.span, b: ink.source.span) bool {
    return a.start < b.end and b.start < a.end;
}

fn execute_eval(
    self: *EvalState,
    host: EvalHost,
    exec: *ink.vm.bytecode.executor,
    frame: FrameInfo,
    locals: []const ink.vm.inkb.debug_local,
    eval_source: *const EvalSource,
    compile: *const ink.compiler.compile_result,
    repl_ctx: ?*ReplContext,
    update_frame: bool,
) !EvalOutcome {
    const debug = compile.debug orelse {
        return .{ .err = try self.allocator.dupe(u8, "eval missing debug info") };
    };


    const eval_func = find_eval_function(debug, eval_source.func_name) orelse {
        return .{ .err = try self.allocator.dupe(u8, "eval function not found") };
    };
    const eval_locals = debug.locals[eval_func.locals_start .. eval_func.locals_start + eval_func.locals_len];

    const eval_words = try locals_with_words(self.allocator, eval_locals);
    defer self.allocator.free(eval_words);

    const eval_frame_state = EvalFrame{
        .base = frame.sp,
        .fp = frame.sp + 2,
        .sp = frame.sp + 2 + ink.vm.register_count,
    };

    if (eval_frame_state.sp >= exec.heap_top) {
        return .{ .err = try self.allocator.dupe(u8, "eval stack overflow") };
    }

    const result_text = try run_eval(
        self.allocator,
        host,
        exec,
        eval_frame_state,
        frame,
        eval_words,
        locals,
        @intCast(eval_func.entry_pc),
        compile.*,
        repl_ctx,
        update_frame,
    );
    return .{ .ok = result_text };
}

fn format_diagnostics(
    allocator: mem_allocator,
    result: ink.compiler.compile_result,
    sources: []const ink.compiler.source,
) ![]u8 {
    var buf = std.ArrayListUnmanaged(u8){};
    defer buf.deinit(allocator);
    for (result.diagnostics, 0..) |diag, idx| {
        if (idx != 0) try buf.appendSlice(allocator, "\n");
        try buf.appendSlice(allocator, diag.message);
        if (diag.source_id) |sid| {
            if (source_path_for_id(sources, sid)) |path| {
                try buf.appendSlice(allocator, " (");
                try buf.appendSlice(allocator, path);
                try buf.appendSlice(allocator, ")");
            }
        }
    }
    return try buf.toOwnedSlice(allocator);
}

fn source_path_for_id(sources: []const ink.compiler.source, id: ink.compiler.source_id) ?[]const u8 {
    for (sources) |src| {
        if (src.id == id) return src.path;
    }
    return null;
}

fn find_eval_function(debug: ink.vm.inkb.debug_info, name: []const u8) ?ink.vm.inkb.debug_function {
    for (debug.functions) |func| {
        if (std.mem.eql(u8, func.name, name)) return func;
    }
    return null;
}

fn program_matches(program: *const ink.vm.inkb.program, result: ink.compiler.compile_result) bool {
    const bytecode = result.bytecode orelse return false;
    if (bytecode.len < program.bytecode.len) return false;
    if (!std.mem.eql(u8, bytecode[0..program.bytecode.len], program.bytecode)) return false;

    const constants = result.constants orelse return false;
    if (constants.len < program.constants.len) return false;
    if (!std.mem.eql(u64, constants[0..program.constants.len], program.constants)) return false;

    const data = result.data orelse return false;
    if (data.len < program.data.len) return false;
    var idx: usize = 0;
    while (idx < program.data.len) : (idx += 1) {
        const a = data[idx];
        const b = program.data[idx];
        if (a.kind != b.kind) return false;
        if (!std.mem.eql(u8, a.bytes, b.bytes)) return false;
    }

    const foreigns = result.foreigns orelse return false;
    if (foreigns.len < program.foreigns.len) return false;
    idx = 0;
    while (idx < program.foreigns.len) : (idx += 1) {
        if (!std.mem.eql(u8, foreigns[idx], program.foreigns[idx])) return false;
    }

    return true;
}

fn merge_locals(
    allocator: mem_allocator,
    base: []const ink.vm.inkb.debug_local,
    extra: []const ink.vm.inkb.debug_local,
) ![]ink.vm.inkb.debug_local {
    var seen = std.StringHashMap(void).init(allocator);
    defer seen.deinit();
    var list = std.array_list.Managed(ink.vm.inkb.debug_local).init(allocator);
    defer list.deinit();

    for (base) |local| {
        _ = try seen.put(local.name, {});
        try list.append(local);
    }
    for (extra) |local| {
        if (seen.contains(local.name)) continue;
        _ = try seen.put(local.name, {});
        try list.append(local);
    }
    return try list.toOwnedSlice();
}

fn locals_with_words(
    allocator: mem_allocator,
    locals: []const ink.vm.inkb.debug_local,
) ![]LocalWord {
    if (locals.len == 0) return allocator.alloc(LocalWord, 0);
    const list = try allocator.alloc(LocalWord, locals.len);
    const sorted = try allocator.alloc(ink.vm.inkb.debug_local, locals.len);
    defer allocator.free(sorted);
    std.mem.copyForwards(ink.vm.inkb.debug_local, sorted, locals);
    std.mem.sort(ink.vm.inkb.debug_local, sorted, {}, debug_local_less);
    var i: usize = 0;
    while (i < sorted.len) : (i += 1) {
        const current = sorted[i];
        var words: u8 = 1;
        if (i + 1 < sorted.len) {
            const next = sorted[i + 1];
            if (next.reg > current.reg) {
                words = @intCast(next.reg - current.reg);
            }
        } else {
            words = word_count_from_type(current.type_name);
        }
        list[i] = .{
            .name = current.name,
            .reg = current.reg,
            .words = words,
            .type_name = current.type_name,
        };
    }
    std.mem.sort(LocalWord, list, {}, local_word_less);
    return list;
}

fn local_word_less(_: void, lhs: LocalWord, rhs: LocalWord) bool {
    if (lhs.reg == rhs.reg) return std.mem.lessThan(u8, lhs.name, rhs.name);
    return lhs.reg < rhs.reg;
}

fn word_count_from_type(name: []const u8) u8 {
    if (std.mem.startsWith(u8, name, "dyn ")) return 2;
    if (std.mem.startsWith(u8, name, "dyn<")) return 2;
    if (std.mem.startsWith(u8, name, "atomic<")) return 2;
    if (std.mem.startsWith(u8, name, "[")) {
        const close_idx = std.mem.indexOfScalar(u8, name, ']') orelse return 1;
        const len_text = std.mem.trim(u8, name[1..close_idx], " ");
        if (len_text.len == 0) return 1;
        const elem_text = std.mem.trim(u8, name[close_idx + 1 ..], " ");
        const len_val = std.fmt.parseInt(u64, len_text, 10) catch return 1;
        const elem_words = word_count_from_type(elem_text);
        const total = len_val * @as(u64, elem_words);
        if (total == 0) return 1;
        if (total > std.math.maxInt(u8)) return std.math.maxInt(u8);
        return @intCast(total);
    }
    if (std.mem.startsWith(u8, name, "?") or std.mem.startsWith(u8, name, "!")) {
        return word_count_from_type(std.mem.trim(u8, name[1..], " "));
    }
    if (std.mem.startsWith(u8, name, "array<")) {
        var inner = name["array<".len..];
        if (std.mem.endsWith(u8, inner, ">")) {
            inner = inner[0 .. inner.len - 1];
        }
        var iter = std.mem.splitScalar(u8, inner, ',');
        const len_text = std.mem.trim(u8, iter.next() orelse return 1, " ");
        const ty_text = std.mem.trim(u8, iter.next() orelse return 1, " ");
        const len_val = std.fmt.parseInt(u64, len_text, 10) catch return 1;
        const elem_words = word_count_from_type(ty_text);
        const total = len_val * @as(u64, elem_words);
        if (total == 0) return 1;
        if (total > std.math.maxInt(u8)) return std.math.maxInt(u8);
        return @intCast(total);
    }
    return 1;
}

fn task_executor(
    scheduler: *ink.runtime.scheduler.scheduler,
    task_id: ink.runtime.scheduler.task_id,
) ?*ink.vm.bytecode.executor {
    const entry = scheduler.tasks.get(task_id) orelse return null;
    const view_fn = entry.debug_view orelse return null;
    const view = view_fn(entry.context);
    if (view.executor == null) return null;
    return @as(*ink.vm.bytecode.executor, @ptrCast(@alignCast(view.executor.?)));
}

fn run_eval(
    allocator: mem_allocator,
    host: EvalHost,
    exec: *ink.vm.bytecode.executor,
    eval_frame: EvalFrame,
    frame: FrameInfo,
    eval_locals: []const LocalWord,
    base_locals: []const ink.vm.inkb.debug_local,
    eval_entry_pc: usize,
    compile: ink.compiler.compile_result,
    repl_ctx: ?*ReplContext,
    update_frame: bool,
) ![]u8 {
    const old_state = exec.current;
    const old_arg_base = exec.arg_base;
    const old_arg_base_valid = exec.arg_base_valid;
    const old_pending_id = exec.pending_op_id;
    const old_pending_pc = exec.pending_op_pc;
    const old_suspend = exec.suspended;
    const old_suspend_op = exec.suspend_op_id;
    const old_suspend_has = exec.suspend_has_op;
    const old_halted = exec.halted;
    const old_debugger = exec.debugger;
    const old_code = exec.code;
    const old_constants = exec.constants;
    const old_data = exec.data;
    const old_foreigns = exec.foreign_names;
    const old_data_ptrs = exec.data_string_ptrs;
    const old_ret0 = exec.memory.read(0);

    const bytecode = compile.bytecode orelse return error.EvalFailed;
    const constants = compile.constants orelse return error.EvalFailed;
    const data = compile.data orelse return error.EvalFailed;
    const foreigns = compile.foreigns orelse return error.EvalFailed;

    exec.code = bytecode;
    exec.constants = constants;
    exec.data = data;
    exec.foreign_names = foreigns;
    exec.debugger = null;
    exec.halted = false;
    exec.pending_op_id = 0;
    exec.pending_op_pc = 0;
    exec.suspended = false;
    exec.suspend_op_id = 0;
    exec.suspend_has_op = false;

    const tmp_data_ptrs = try allocator.alloc(usize, data.len);
    @memset(tmp_data_ptrs, 0);
    exec.data_string_ptrs = tmp_data_ptrs;

    defer {
        exec.current = old_state;
        exec.arg_base = old_arg_base;
        exec.arg_base_valid = old_arg_base_valid;
        exec.pending_op_id = old_pending_id;
        exec.pending_op_pc = old_pending_pc;
        exec.suspended = old_suspend;
        exec.suspend_op_id = old_suspend_op;
        exec.suspend_has_op = old_suspend_has;
        exec.halted = old_halted;
        exec.debugger = old_debugger;
        exec.code = old_code;
        exec.constants = old_constants;
        exec.data = old_data;
        exec.foreign_names = old_foreigns;
        exec.data_string_ptrs = old_data_ptrs;
        allocator.free(tmp_data_ptrs);
        exec.memory.write(0, old_ret0);
    }

    exec.memory.write(eval_frame.base, 0);
    exec.memory.write(eval_frame.base + 1, 0);

    const base_word_list = try locals_with_words(allocator, base_locals);
    defer allocator.free(base_word_list);

    for (eval_locals) |local| {
        if (local.reg == 0) continue;
        if (std.mem.startsWith(u8, local.name, "_inkx_internal_")) {
            exec.memory.write(eval_frame.fp + local.reg, 0);
            continue;
        }
        if (find_local_word(base_word_list, local.name)) |base| {
            copy_words(
                exec.memory,
                frame.fp + base.reg,
                eval_frame.fp + local.reg,
                base.words,
            );
        } else if (repl_ctx) |ctx| {
            if (ctx.locals.get(local.name)) |slot| {
                copy_words_between(
                    &ctx.memory,
                    ctx.fp + slot.reg,
                    exec.memory,
                    eval_frame.fp + local.reg,
                    slot.words,
                );
            }
        }
    }

    exec.current.pc = eval_entry_pc;
    exec.current.fp = eval_frame.fp;
    exec.current.sp = eval_frame.sp;
    exec.arg_base = eval_frame.sp;
    exec.arg_base_valid = false;
    exec.set_task_context(host.scheduler, frame.task_id);
    host.debug.set_suppressed(true);
    defer host.debug.set_suppressed(false);

    while (!exec.halted) {
        _ = exec.step(exec.code, 1);
        if (exec.halted) break;
        const suspend_state = exec.take_suspend();
        switch (suspend_state) {
            .op => |op_id| {
                host.scheduler.wait(frame.task_id, op_id) catch return error.EvalFailed;
                try wait_ready(host.scheduler, frame.task_id);
            },
            .manual => {
                try wait_ready(host.scheduler, frame.task_id);
            },
            .none => {},
        }
    }

    const result_value = exec.memory.read(0);
    var result = read_string_value(allocator, exec, result_value) catch |err| switch (err) {
        error.InvalidString => try allocator.dupe(u8, "<invalid string>"),
        else => return err,
    };
    if (result.len == 0) {
        if (find_local_word(eval_locals, "_inkx_internal_result")) |result_local| {
            const raw = exec.memory.read(eval_frame.fp + result_local.reg);
            const fallback = try format_debug_value_exec(allocator, exec, raw, result_local.type_name);
            if (fallback.len > 0) {
                allocator.free(result);
                result = fallback;
            }
        }
    }

    if (repl_ctx) |ctx| {
        try sync_repl_locals(allocator, ctx, eval_locals, exec.memory, eval_frame.fp, base_word_list);
        if (exec == &ctx.executor) {
            ctx.sp = eval_frame.base;
        }
    }
    if (update_frame) {
        for (base_word_list) |local| {
            if (find_local_word(eval_locals, local.name)) |eval_local| {
                copy_words(
                    exec.memory,
                    eval_frame.fp + eval_local.reg,
                    frame.fp + local.reg,
                    local.words,
                );
            }
        }
    }

    return result;
}

fn find_local_word(list: []const LocalWord, name: []const u8) ?LocalWord {
    for (list) |local| {
        if (std.mem.eql(u8, local.name, name)) return local;
    }
    return null;
}

fn copy_words(mem: *ink.vm.vm.tape, src: usize, dst: usize, count: u8) void {
    var idx: u8 = 0;
    while (idx < count) : (idx += 1) {
        mem.write(dst + idx, mem.read(src + idx));
    }
}

fn copy_words_between(
    src_mem: *ink.vm.vm.tape,
    src: usize,
    dst_mem: *ink.vm.vm.tape,
    dst: usize,
    count: u8,
) void {
    var idx: u8 = 0;
    while (idx < count) : (idx += 1) {
        dst_mem.write(dst + idx, src_mem.read(src + idx));
    }
}

fn drop_ready(scheduler: *ink.runtime.scheduler.scheduler, task_id: ink.runtime.scheduler.task_id) bool {
    var idx: usize = 0;
    while (idx < scheduler.ready.items.len) {
        if (scheduler.ready.items[idx] == task_id) {
            _ = scheduler.ready.swapRemove(idx);
            return true;
        } else {
            idx += 1;
        }
    }
    return false;
}

fn wait_ready(scheduler: *ink.runtime.scheduler.scheduler, task_id: ink.runtime.scheduler.task_id) !void {
    while (true) {
        try scheduler.poll_once(null);
        if (drop_ready(scheduler, task_id)) return;
    }
}

fn read_string_value(
    allocator: mem_allocator,
    exec: *ink.vm.bytecode.executor,
    value: u64,
) ![]u8 {
    if (value < @as(u64, @intCast(exec.data.len))) {
        const idx: usize = @intCast(value);
        if (idx < exec.data.len and exec.data[idx].kind == .string) {
            return allocator.dupe(u8, exec.data[idx].bytes);
        }
    }
    const ptr: usize = @intCast(value);
    if (ptr + 2 > exec.memory.data.len) return error.InvalidString;
    const len: usize = @intCast(exec.memory.read(ptr));
    const cap: usize = @intCast(exec.memory.read(ptr + 1));
    const payload_ptr = ptr + 2;
    const words = bytes_to_words(cap);
    if (payload_ptr + words > exec.memory.data.len) return error.InvalidString;
    const raw = exec.memory.data[payload_ptr .. payload_ptr + words];
    const bytes = std.mem.sliceAsBytes(raw);
    if (len > bytes.len) return error.InvalidString;
    return allocator.dupe(u8, bytes[0..len]);
}

fn bytes_to_words(count: usize) usize {
    return if (count == 0) 0 else (count + 7) / 8;
}

fn format_debug_value_exec(
    allocator: mem_allocator,
    exec: *ink.vm.bytecode.executor,
    raw: u64,
    type_name: []const u8,
) ![]u8 {
    if (std.mem.eql(u8, type_name, "int")) {
        const val: i64 = @bitCast(raw);
        return std.fmt.allocPrint(allocator, "{d}", .{val});
    }
    if (std.mem.eql(u8, type_name, "uint")) {
        return std.fmt.allocPrint(allocator, "{d}", .{raw});
    }
    if (std.mem.eql(u8, type_name, "bool")) {
        return allocator.dupe(u8, if (raw != 0) "true" else "false");
    }
    if (std.mem.eql(u8, type_name, "float")) {
        const val: f64 = @bitCast(raw);
        return std.fmt.allocPrint(allocator, "{d}", .{val});
    }
    if (std.mem.eql(u8, type_name, "string")) {
        return read_string_value(allocator, exec, raw) catch allocator.dupe(u8, "<invalid string>");
    }
    if (std.mem.eql(u8, type_name, "unit")) {
        return allocator.dupe(u8, "");
    }
    return std.fmt.allocPrint(allocator, "0x{x}", .{raw});
}

fn sync_repl_locals(
    allocator: mem_allocator,
    ctx: *ReplContext,
    eval_locals: []const LocalWord,
    exec_mem: *ink.vm.vm.tape,
    eval_fp: usize,
    base_locals: []const LocalWord,
) !void {
    for (eval_locals) |local| {
        if (std.mem.startsWith(u8, local.name, "_inkx_internal_")) continue;
        if (find_local_word(base_locals, local.name) != null) continue;
        if (ctx.locals.getPtr(local.name)) |slot| {
            copy_words_between(exec_mem, eval_fp + local.reg, &ctx.memory, ctx.fp + slot.reg, slot.words);
            continue;
        }
        if (ctx.next_reg >= ink.vm.register_count) {
            return error.EvalFailed;
        }
        const new_reg = ctx.next_reg;
        const next_reg = new_reg + local.words;
        if (next_reg >= ink.vm.register_count) {
            return error.EvalFailed;
        }
        ctx.next_reg = next_reg;
        const name_dup = try allocator.dupe(u8, local.name);
        const type_dup = try allocator.dupe(u8, local.type_name);
        ctx.locals.put(name_dup, .{
            .reg = new_reg,
            .words = local.words,
            .type_name = type_dup,
        }) catch return error.OutOfMemory;
        copy_words_between(exec_mem, eval_fp + local.reg, &ctx.memory, ctx.fp + new_reg, local.words);
    }
}

fn debug_local_less(_: void, lhs: ink.vm.inkb.debug_local, rhs: ink.vm.inkb.debug_local) bool {
    if (lhs.reg == rhs.reg) return std.mem.lessThan(u8, lhs.name, rhs.name);
    return lhs.reg < rhs.reg;
}
