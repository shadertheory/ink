const std = @import("std");
const ink = @import("ink");
const ir_mod = ink.ir;

pub const codegen_error = error{
    out_of_memory,
    unsupported_node,
    missing_main,
    unknown_identifier,
    unknown_function,
    register_overflow,
    constant_index_overflow,
};

pub const program = struct {
    instructions: []const ink.exe.instruction,
    constants: []const u64,
};

const function_info = struct {
    label: ink.exe.label_id,
    param_count: u8,
    decl: ir_mod.ir.function_decl,
};

const builder = struct {
    allocator: std.mem.Allocator,
    nodes: []const ir_mod.ir,
    strings: []const []const u8,
    roots: []const ir_mod.ir_identifier,
    instructions: std.array_list.Managed(ink.exe.instruction),
    constants: std.array_list.Managed(u64),
    const_map: std.AutoHashMap(u64, u32),
    functions: std.StringHashMap(function_info),
    next_label: u32,

    pub fn init(allocator: std.mem.Allocator, nodes: []const ir_mod.ir, strings: []const []const u8, roots: []const ir_mod.ir_identifier) builder {
        return .{
            .allocator = allocator,
            .nodes = nodes,
            .strings = strings,
            .roots = roots,
            .instructions = std.array_list.Managed(ink.exe.instruction).init(allocator),
            .constants = std.array_list.Managed(u64).init(allocator),
            .const_map = std.AutoHashMap(u64, u32).init(allocator),
            .functions = std.StringHashMap(function_info).init(allocator),
            .next_label = 1,
        };
    }

    pub fn deinit(self: *builder) void {
        self.instructions.deinit();
        self.constants.deinit();
        self.const_map.deinit();
        self.functions.deinit();
    }

    pub fn emit(self: *builder, inst: ink.exe.instruction) codegen_error!void {
        self.instructions.append(inst) catch return error.out_of_memory;
    }

    pub fn new_label(self: *builder) ink.exe.label_id {
        const id = self.next_label;
        self.next_label += 1;
        return id;
    }

    pub fn intern_const(self: *builder, value: u64) codegen_error!u32 {
        if (self.const_map.get(value)) |idx| return idx;
        if (self.constants.items.len >= 8192) return error.constant_index_overflow;
        const idx: u32 = @intCast(self.constants.items.len);
        self.constants.append(value) catch return error.out_of_memory;
        self.const_map.put(value, idx) catch return error.out_of_memory;
        return idx;
    }

    fn node(self: *builder, id: ir_mod.ir_identifier) ir_mod.ir {
        return self.nodes[@intCast(id.idx)];
    }

    fn string_value(self: *builder, id: ir_mod.string_identifier) []const u8 {
        if (id.idx < self.strings.len) return self.strings[id.idx];
        return "<missing>";
    }
};

const function_ctx = struct {
    b: *builder,
    locals: std.StringHashMap(u8),
    temp_base: u8,
    next_temp: u8,
    returned: bool,

    pub fn init(b: *builder, params: []const ir_mod.ir.function_decl.param) codegen_error!function_ctx {
        var locals = std.StringHashMap(u8).init(b.allocator);
        errdefer locals.deinit();

        var reg_index: u8 = 1;
        for (params) |param| {
            const name = b.string_value(param.name);
            locals.put(name, reg_index) catch return error.out_of_memory;
            reg_index += 1;
        }

        if (reg_index > 7) return error.register_overflow;

        return .{
            .b = b,
            .locals = locals,
            .temp_base = reg_index,
            .next_temp = reg_index,
            .returned = false,
        };
    }

    pub fn deinit(self: *function_ctx) void {
        self.locals.deinit();
    }

    fn alloc_temp(self: *function_ctx) codegen_error!u8 {
        if (self.next_temp > 7) return error.register_overflow;
        const reg = self.next_temp;
        self.next_temp += 1;
        return reg;
    }

    fn free_temp(self: *function_ctx, reg: u8) void {
        if (reg >= self.temp_base and reg + 1 == self.next_temp) {
            self.next_temp -= 1;
        }
    }

    fn is_temp(self: *function_ctx, reg: u8) bool {
        return reg >= self.temp_base;
    }

    fn emit(self: *function_ctx, inst: ink.exe.instruction) codegen_error!void {
        try self.b.emit(inst);
    }

    fn is_unit(self: *function_ctx, id: ir_mod.ir_identifier) bool {
        const node = self.b.node(id);
        return switch (node) {
            .identifier => |ident| std.mem.eql(u8, self.b.string_value(ident), "unit"),
            else => false,
        };
    }

    fn compile_expr(self: *function_ctx, id: ir_mod.ir_identifier) codegen_error!u8 {
        const node = self.b.node(id);
        return switch (node) {
            .integer => |value| blk: {
                const idx = try self.b.intern_const(@bitCast(value));
                const reg = try self.alloc_temp();
                try self.emit(.{ .load_const = .{ .dst = reg, .const_index = idx } });
                break :blk reg;
            },
            .float => return error.unsupported_node,
            .string => return error.unsupported_node,
            .boolean => |value| blk: {
                const idx = try self.b.intern_const(@intFromBool(value));
                const reg = try self.alloc_temp();
                try self.emit(.{ .load_const = .{ .dst = reg, .const_index = idx } });
                break :blk reg;
            },
            .identifier => |ident| blk: {
                const name = self.b.string_value(ident);
                if (self.locals.get(name)) |reg| break :blk reg;
                return error.unknown_identifier;
            },
            .unary => |un| switch (un.op) {
                .ret => {
                    const reg = try self.compile_expr(un.right);
                    try self.emit(.{ .ret_value = .{ .src = reg } });
                    self.returned = true;
                    return reg;
                },
                .neg => blk: {
                    const right_reg = try self.compile_expr(un.right);
                    const zero_idx = try self.b.intern_const(0);
                    const zero_reg = try self.alloc_temp();
                    try self.emit(.{ .load_const = .{ .dst = zero_reg, .const_index = zero_idx } });
                    const dst = if (self.is_temp(right_reg)) right_reg else try self.alloc_temp();
                    try self.emit(.{ .sub = .{ .dst = dst, .src_a = zero_reg, .src_b = right_reg } });
                    self.free_temp(zero_reg);
                    if (dst != right_reg and self.is_temp(right_reg)) self.free_temp(right_reg);
                    break :blk dst;
                },
                else => return error.unsupported_node,
            },
            .binary => |bin| switch (bin.op) {
                .call => try self.compile_call(id),
                .add, .sub, .mul, .div, .less_than, .less_or_equal, .greater_than, .greater_or_equal, .equal, .not_equal => blk: {
                    var left_reg = try self.compile_expr(bin.left);
                    if (left_reg == 0) {
                        const tmp = try self.alloc_temp();
                        try self.emit(.{ .move = .{ .dst = tmp, .src = left_reg } });
                        left_reg = tmp;
                    }

                    const right_reg = try self.compile_expr(bin.right);

                    var dst: u8 = undefined;
                    if (self.is_temp(left_reg)) {
                        dst = left_reg;
                    } else if (self.is_temp(right_reg)) {
                        dst = right_reg;
                    } else {
                        dst = try self.alloc_temp();
                    }

                    switch (bin.op) {
                        .add => try self.emit(.{ .add = .{ .dst = dst, .src_a = left_reg, .src_b = right_reg } }),
                        .sub => try self.emit(.{ .sub = .{ .dst = dst, .src_a = left_reg, .src_b = right_reg } }),
                        .mul => try self.emit(.{ .mul = .{ .dst = dst, .src_a = left_reg, .src_b = right_reg } }),
                        .div => try self.emit(.{ .div = .{ .dst = dst, .src_a = left_reg, .src_b = right_reg } }),
                        .less_than => try self.emit(.{ .compare_lt = .{ .dst = dst, .src_a = left_reg, .src_b = right_reg } }),
                        .less_or_equal => try self.emit(.{ .compare_le = .{ .dst = dst, .src_a = left_reg, .src_b = right_reg } }),
                        .greater_than => try self.emit(.{ .compare_gt = .{ .dst = dst, .src_a = left_reg, .src_b = right_reg } }),
                        .greater_or_equal => try self.emit(.{ .compare_ge = .{ .dst = dst, .src_a = left_reg, .src_b = right_reg } }),
                        .equal => try self.emit(.{ .compare_eq = .{ .dst = dst, .src_a = left_reg, .src_b = right_reg } }),
                        .not_equal => {
                            const eq_reg = dst;
                            try self.emit(.{ .compare_eq = .{ .dst = eq_reg, .src_a = left_reg, .src_b = right_reg } });
                            const zero_idx = try self.b.intern_const(0);
                            const zero_reg = try self.alloc_temp();
                            try self.emit(.{ .load_const = .{ .dst = zero_reg, .const_index = zero_idx } });
                            try self.emit(.{ .compare_eq = .{ .dst = eq_reg, .src_a = eq_reg, .src_b = zero_reg } });
                            self.free_temp(zero_reg);
                        },
                        else => return error.unsupported_node,
                    }

                    if (self.is_temp(right_reg) and dst != right_reg) self.free_temp(right_reg);
                    if (self.is_temp(left_reg) and dst != left_reg) self.free_temp(left_reg);
                    break :blk dst;
                },
                else => return error.unsupported_node,
            },
            .block => |items| {
                const count = items.len;
                var idx: usize = 0;
                var last_reg: u8 = 0;
                while (idx < count) : (idx += 1) {
                    const reg = try self.compile_expr(items[idx]);
                    if (idx + 1 == count) {
                        last_reg = reg;
                    } else if (self.is_temp(reg)) {
                        self.free_temp(reg);
                    }
                    if (self.returned) return reg;
                }
                return last_reg;
            },
            .if_expr => |ife| blk: {
                const cond_reg = try self.compile_expr(ife.condition);
                const else_label = self.b.new_label();
                const end_label = self.b.new_label();
                try self.emit(.{ .jump_if_false = .{ .condition = cond_reg, .target = else_label } });
                if (self.is_temp(cond_reg)) self.free_temp(cond_reg);

                const dst = try self.alloc_temp();
                const then_reg = try self.compile_expr(ife.then_branch);
                if (then_reg != dst) {
                    try self.emit(.{ .move = .{ .dst = dst, .src = then_reg } });
                }
                if (self.is_temp(then_reg)) self.free_temp(then_reg);
                try self.emit(.{ .jump = .{ .target = end_label } });

                try self.emit(.{ .label = .{ .id = else_label } });
                if (ife.else_branch) |else_ref| {
                    const else_reg = try self.compile_expr(else_ref);
                    if (else_reg != dst) {
                        try self.emit(.{ .move = .{ .dst = dst, .src = else_reg } });
                    }
                    if (self.is_temp(else_reg)) self.free_temp(else_reg);
                } else {
                    return error.unsupported_node;
                }

                try self.emit(.{ .label = .{ .id = end_label } });
                break :blk dst;
            },
            else => return error.unsupported_node,
        };
    }

    fn compile_call(self: *function_ctx, id: ir_mod.ir_identifier) codegen_error!u8 {
        var base_id = id;
        var args = std.array_list.Managed(ir_mod.ir_identifier).init(self.b.allocator);
        defer args.deinit();

        while (true) {
            const node = self.b.node(base_id);
            switch (node) {
                .binary => |bin| {
                    if (bin.op == .call) {
                        args.append(bin.right) catch return error.out_of_memory;
                        base_id = bin.left;
                        continue;
                    }
                },
                else => {},
            }
            break;
        }

        std.mem.reverse(ir_mod.ir_identifier, args.items);

        const base_node = self.b.node(base_id);
        const name = switch (base_node) {
            .identifier => |ident| self.b.string_value(ident),
            else => return error.unsupported_node,
        };

        if (args.items.len == 1 and self.is_unit(args.items[0])) {
            args.clearRetainingCapacity();
        }

        if (args.items.len > 7) return error.register_overflow;

        if (std.mem.eql(u8, name, "print")) {
            if (args.items.len != 1) return error.unsupported_node;
            const arg_reg = try self.compile_expr(args.items[0]);
            if (arg_reg != 0) {
                try self.emit(.{ .move = .{ .dst = 0, .src = arg_reg } });
            }
            if (self.is_temp(arg_reg)) self.free_temp(arg_reg);
            try self.emit(.{ .call_foreign = .{ .index = 0 } });
            return 0;
        }

        const info = self.b.functions.get(name) orelse return error.unknown_function;
        const arg_count: u8 = @intCast(args.items.len);
        if (info.param_count != arg_count) return error.unsupported_node;

        for (args.items, 0..) |arg_id, idx| {
            const arg_reg = try self.compile_expr(arg_id);
            const dst: u8 = @intCast(idx + 1);
            try self.emit(.{ .argument_set = .{ .dst = dst, .src = arg_reg } });
            if (self.is_temp(arg_reg)) self.free_temp(arg_reg);
        }

        try self.emit(.{ .call = .{ .target = info.label } });
        return 0;
    }
};

pub fn generate(
    allocator: std.mem.Allocator,
    nodes: []const ir_mod.ir,
    strings: []const []const u8,
    roots: []const ir_mod.ir_identifier,
) codegen_error!program {
    var b = builder.init(allocator, nodes, strings, roots);
    defer b.deinit();

    var functions = std.array_list.Managed(function_info).init(allocator);
    defer functions.deinit();

    for (roots) |root| {
        const node = nodes[@intCast(root.idx)];
        switch (node) {
            .decl => |decl| switch (decl) {
                .function => |func| {
                    const name = b.string_value(func.name);
                    const label = b.new_label();
                    const info = function_info{
                        .label = label,
                        .param_count = @intCast(func.params.len),
                        .decl = func,
                    };
                    b.functions.put(name, info) catch return error.out_of_memory;
                    functions.append(info) catch return error.out_of_memory;
                },
                else => {},
            },
            else => {},
        }
    }

    const main_info = b.functions.get("main") orelse return error.missing_main;

    try b.emit(.{ .call = .{ .target = main_info.label } });
    try b.emit(.{ .halt = {} });

    for (functions.items) |info| {
        try b.emit(.{ .label = .{ .id = info.label } });
        var ctx = try function_ctx.init(&b, info.decl.params);
        defer ctx.deinit();

        if (info.decl.body) |body_ref| {
            const result_reg = try ctx.compile_expr(body_ref);
            if (!ctx.returned) {
                try ctx.emit(.{ .ret_value = .{ .src = result_reg } });
            }
        } else {
            try ctx.emit(.{ .ret = {} });
        }
    }

    return .{
        .instructions = b.instructions.toOwnedSlice() catch return error.out_of_memory,
        .constants = b.constants.toOwnedSlice() catch return error.out_of_memory,
    };
}
