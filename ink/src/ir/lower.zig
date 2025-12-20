pub const std = @import("std");
pub const node = @import("../lang/ast.zig").node;
pub const ir = @import("./core.zig").ir;
pub const hash_map = std.hash_map.AutoHashMap;
pub const array_list = std.array_list.Managed;
pub const span = struct { start: usize, end: usize };

pub const ast_identifier = struct { idx: u32 };
pub const ir_identifier = struct { idx: u32 };
pub const string_identifier = struct { idx: u32 };

pub const lower_error = error{
    out_of_memory,
    unexpected_tag,
    unresolved_name,
    type_mismatch,
};

pub const lower = struct {
    pub const self = @This();
    pub const grammer = struct {
        pub fn before(ctx: anytype, _: []const u8, _: node) void {
            _ = ctx;
        }

        pub fn after(ctx: anytype, _: []const u8, _: node, _: lower_error!result) void {
            _ = ctx;
        }

        pub fn on_missing(ctx: anytype, _: []const u8, _: node) lower_error!result {
            _ = ctx;
            return error.unexpected_tag;
        }
    };
    pub const result = struct { what: ir_identifier, where: span, emit: bool };

    pub fn context(comptime diagnosis: type) type {
        return struct {
            ast: []const *node,
            ir: array_list(ir),
            locations: array_list(span),
            cache: array_list(ir_identifier),
            sink: diagnosis,
        };
    }

    pub var intern_ship = hash_map(type, array_list(usize));
    pub var intern_subject = hash_map(usize, []const u8);
    ctx: context,

    pub fn lower_ast(this: *self, id: ast_identifier) lower_error!ir_identifier {
        const this_ctx = this.ctx;
        if (id.idx < this_ctx.cache.items.len) {
            if (this.cache.items[id.idx]) |cached| return cached;
        }

        const ret = try this.dispatch(this.ast[id.idx].*);
        if (ret.emit) {
            try this_ctx.append(this.allocator, ret.where) catch return error.out_of_memory;
        }
        if (id.idx < this.cache.items.len) {
            this.cache.items[id.idx] = ret.what;
        }
        return ret.what;
    }

    pub fn emit(this: *self, value: ir, where: span, emit_span: bool) lower_error!result {
        const idx = this.ir.items.len;
        try this.ir.append(self.allocator, value) catch return error.out_of_memory;
        return .{ .what = .{ .idx = @intCast(idx) }, .where = where, .emit = emit_span };
    }

    pub fn intern(this: *self, comptime what: type, value: what) type {
        intern_ship.init(this.ctx.allocator);
        var idx = undefined;
        if (intern_ship[what]) |ship| {
            idx = ship.items.count;
            ship.append(this.ctx.allocator, idx);
        } else {
            intern_ship[what] = array_list(usize).init(this.ctx.allocator);
            return intern(this, what, value);
        }
        intern_subject.init(this.ctx.allocator);
        intern_subject[idx] = value;
        return struct { idx };
    }
};
