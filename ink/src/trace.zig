const std = @import("std");
const source = @import("source.zig");

pub const Phase = enum {
    front_end,
    source_store,
    module_graph,
    lex,
    parse,
    ast,
    macro_discover,
    macro_compile,
    macro_expand,
    desugar,
    resolve,
    uir,
    typecheck,
    mir,
    lir,
    backend,
    encode,
    pack,
    finish,
};

pub const EventKind = enum {
    phase_start,
    phase_end,
    stack_push,
    stack_pop,
    step,
    token,
    parse_node,
    ast_node,
    macro_event,
    uir_node,
    mir_node,
    lir_node,
    instruction,
    bytecode,
    diagnostic,
    note,
};

pub const Event = struct {
    kind: EventKind,
    phase: Phase,
    file: []const u8 = "",
    fn_name: []const u8 = "",
    line: u32 = 0,
    column: u32 = 0,
    source_id: ?source.source_id = null,
    span: ?source.span = null,
    index: ?u32 = null,
    index2: ?u32 = null,
    tag: ?[]const u8 = null,
    message: ?[]const u8 = null,
};

pub const Sink = struct {
    ctx: *anyopaque,
    emit: *const fn (*anyopaque, *const Event) void,
};

var sink: ?Sink = null;

pub fn set_sink(new_sink: ?Sink) void {
    sink = new_sink;
}

pub fn is_enabled() bool {
    return sink != null;
}

pub fn emitAt(loc: std.builtin.SourceLocation, event: Event) void {
    if (sink) |s| {
        var item = event;
        item.file = loc.file;
        item.fn_name = loc.fn_name;
        item.line = loc.line;
        item.column = loc.column;
        s.emit(s.ctx, &item);
    }
}

pub const Scope = struct {
    active: bool,
    phase: Phase,
    tag: []const u8,
    loc: std.builtin.SourceLocation,

    pub fn end(self: Scope) void {
        if (!self.active) return;
        emitAt(self.loc, .{
            .kind = .stack_pop,
            .phase = self.phase,
            .tag = self.tag,
        });
    }
};

pub fn scope(loc: std.builtin.SourceLocation, phase: Phase, tag: []const u8) Scope {
    if (sink == null) {
        return .{ .active = false, .phase = phase, .tag = tag, .loc = loc };
    }
    emitAt(loc, .{
        .kind = .stack_push,
        .phase = phase,
        .tag = tag,
    });
    return .{ .active = true, .phase = phase, .tag = tag, .loc = loc };
}

pub fn step(loc: std.builtin.SourceLocation, phase: Phase, tag: []const u8, index: usize, total: ?usize) void {
    emitAt(loc, .{
        .kind = .step,
        .phase = phase,
        .tag = tag,
        .index = clamp_index(index),
        .index2 = if (total) |val| clamp_index(val) else null,
    });
}

fn clamp_index(value: usize) ?u32 {
    if (value > std.math.maxInt(u32)) return std.math.maxInt(u32);
    return @intCast(value);
}
