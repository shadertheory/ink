const std = @import("std");
const op = @import("op.zig");
const exe = @import("exe.zig");
const bytecode = @import("assembly.zig").bytecode;

pub const encode_error = error{
    OutOfMemory,
    label_not_found,
    const_index_too_large,
    register_too_large,
    unsupported_instruction,
    instruction_invalid,
};

pub fn encode(allocator: std.mem.Allocator, instructions: []const exe.instruction) encode_error![]u8 {
    var label_offsets = std.AutoHashMap(u32, usize).init(allocator);
    defer label_offsets.deinit();

    var offset: usize = 0;
    for (instructions) |inst| {
        switch (inst) {
            .label => |label| {
                label_offsets.put(label.id, offset) catch return error.OutOfMemory;
            },
            else => {
                offset += try instruction_size(inst);
            },
        }
    }

    var assembler = bytecode.assembler.init(allocator);
    errdefer assembler.deinit();

    for (instructions) |inst| {
        try emit_instruction(&assembler, inst, &label_offsets);
    }

    return assembler.finish();
}

fn instruction_size(inst: exe.instruction) encode_error!usize {
    switch (inst) {
        .label => return 0,
        .load_const => |op_load| {
            try ensure_reg_small(op_load.dst);
            if (op_load.const_index > 8191) return error.const_index_too_large;
            return bytecode.encoded_size(op.control.load_const, .{
                .src = 0,
                .dst = op_load.dst,
                .val = op_load.const_index,
            });
        },
        .move => |mv| {
            try ensure_reg_small(mv.dst);
            try ensure_reg_small(mv.src);
            return bytecode.encoded_size(op.control.move, .{ .src = mv.src, .dst = mv.dst });
        },
        .argument_set => |mv| {
            try ensure_reg_small(mv.dst);
            try ensure_reg_small(mv.src);
            return bytecode.encoded_size(op.control.argument_set, .{ .src = mv.src, .dst = mv.dst });
        },
        .add => |bin| return size_tri(op.int_math.binary_add, bin),
        .sub => |bin| return size_tri(op.int_math.binary_sub, bin),
        .mul => |bin| return size_tri(op.int_math.binary_multiply, bin),
        .div => |bin| return size_tri(op.int_math.binary_divide, bin),
        .compare_eq => |bin| return size_tri(op.int_math.compare_equal, bin),
        .compare_ne => return error.unsupported_instruction,
        .compare_lt => |bin| return size_tri(op.int_math.compare_less_than, bin),
        .compare_le => |bin| return size_tri(op.int_math.compare_less_than_or_equal, bin),
        .compare_gt => |bin| return size_tri(op.int_math.compare_greater_than, bin),
        .compare_ge => |bin| return size_tri(op.int_math.compare_greater_than_or_equal, bin),
        .jump => |jmp| {
            _ = jmp;
            return bytecode.encoded_size(op.control.jump_always, .{ .target_absolute = 0 });
        },
        .jump_if_true => |jmp| {
            try ensure_reg(jmp.condition);
            return bytecode.encoded_size(op.control.jump_if_true, .{ .condition_register = jmp.condition, .target_absolute = 0 });
        },
        .jump_if_false => |jmp| {
            try ensure_reg(jmp.condition);
            return bytecode.encoded_size(op.control.jump_if_false, .{ .condition_register = jmp.condition, .target_absolute = 0 });
        },
        .call => |call| {
            _ = call;
            return bytecode.encoded_size(op.control.call, .{ .target_absolute = 0 });
        },
        .call_foreign => |call| {
            if (call.index > 8191) return error.const_index_too_large;
            return bytecode.encoded_size(op.control.call_foreign, .{ .src = 0, .dst = 0, .val = call.index });
        },
        .ret => {
            return bytecode.encoded_size(op.control.ret, .{ .src = 0, .dst = 0 });
        },
        .ret_value => |retv| {
            try ensure_reg_small(retv.src);
            return bytecode.encoded_size(op.control.ret_value, .{ .src = retv.src, .dst = 0 });
        },
        .halt => {
            return bytecode.encoded_size(op.control.halt, .{ .src = 0, .dst = 0 });
        },
    }
}

fn emit_instruction(
    assembler: *bytecode.assembler,
    inst: exe.instruction,
    labels: *const std.AutoHashMap(u32, usize),
) encode_error!void {
    switch (inst) {
        .label => return,
        .load_const => |op_load| {
            try ensure_reg_small(op_load.dst);
            if (op_load.const_index > 8191) return error.const_index_too_large;
            try assembler.emit(op.control.load_const, .{
                .src = 0,
                .dst = op_load.dst,
                .val = op_load.const_index,
            });
        },
        .move => |mv| {
            try ensure_reg_small(mv.dst);
            try ensure_reg_small(mv.src);
            try assembler.emit(op.control.move, .{ .src = mv.src, .dst = mv.dst });
        },
        .argument_set => |mv| {
            try ensure_reg_small(mv.dst);
            try ensure_reg_small(mv.src);
            try assembler.emit(op.control.argument_set, .{ .src = mv.src, .dst = mv.dst });
        },
        .add => |bin| try emit_tri(assembler, op.int_math.binary_add, bin),
        .sub => |bin| try emit_tri(assembler, op.int_math.binary_sub, bin),
        .mul => |bin| try emit_tri(assembler, op.int_math.binary_multiply, bin),
        .div => |bin| try emit_tri(assembler, op.int_math.binary_divide, bin),
        .compare_eq => |bin| try emit_tri(assembler, op.int_math.compare_equal, bin),
        .compare_ne => return error.unsupported_instruction,
        .compare_lt => |bin| try emit_tri(assembler, op.int_math.compare_less_than, bin),
        .compare_le => |bin| try emit_tri(assembler, op.int_math.compare_less_than_or_equal, bin),
        .compare_gt => |bin| try emit_tri(assembler, op.int_math.compare_greater_than, bin),
        .compare_ge => |bin| try emit_tri(assembler, op.int_math.compare_greater_than_or_equal, bin),
        .jump => |jmp| {
            const target = labels.get(jmp.target) orelse return error.label_not_found;
            try assembler.emit(op.control.jump_always, .{ .target_absolute = target });
        },
        .jump_if_true => |jmp| {
            const target = labels.get(jmp.target) orelse return error.label_not_found;
            try ensure_reg(jmp.condition);
            try assembler.emit(op.control.jump_if_true, .{
                .condition_register = jmp.condition,
                .target_absolute = target,
            });
        },
        .jump_if_false => |jmp| {
            const target = labels.get(jmp.target) orelse return error.label_not_found;
            try ensure_reg(jmp.condition);
            try assembler.emit(op.control.jump_if_false, .{
                .condition_register = jmp.condition,
                .target_absolute = target,
            });
        },
        .call => |call| {
            const target = labels.get(call.target) orelse return error.label_not_found;
            try assembler.emit(op.control.call, .{ .target_absolute = target });
        },
        .call_foreign => |call| {
            if (call.index > 8191) return error.const_index_too_large;
            try assembler.emit(op.control.call_foreign, .{ .src = 0, .dst = 0, .val = call.index });
        },
        .ret => {
            try assembler.emit(op.control.ret, .{ .src = 0, .dst = 0 });
        },
        .ret_value => |retv| {
            try ensure_reg_small(retv.src);
            try assembler.emit(op.control.ret_value, .{ .src = retv.src, .dst = 0 });
        },
        .halt => {
            try assembler.emit(op.control.halt, .{ .src = 0, .dst = 0 });
        },
    }
}

fn ensure_reg_small(reg: u8) encode_error!void {
    if (reg > 7) return error.register_too_large;
}

fn ensure_reg(reg: u8) encode_error!void {
    _ = reg;
}

fn size_tri(operation: anytype, bin: anytype) encode_error!usize {
    try ensure_reg(bin.dst);
    try ensure_reg(bin.src_a);
    try ensure_reg(bin.src_b);
    return bytecode.encoded_size(operation, .{ .dst = bin.dst, .src_a = bin.src_a, .src_b = bin.src_b });
}

fn emit_tri(assembler: *bytecode.assembler, operation: anytype, bin: anytype) encode_error!void {
    try ensure_reg(bin.dst);
    try ensure_reg(bin.src_a);
    try ensure_reg(bin.src_b);
    try assembler.emit(operation, .{ .dst = bin.dst, .src_a = bin.src_a, .src_b = bin.src_b });
}
