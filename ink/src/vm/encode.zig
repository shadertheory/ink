const std = @import("std");
const op = @import("op.zig");
const exe = @import("exe.zig");
const bytecode = @import("assembly.zig").bytecode;
const max_register: u8 = 63;

pub const encode_error = error{
    OutOfMemory,
    label_not_found,
    const_index_too_large,
    register_too_large,
    unsupported_instruction,
    instruction_invalid,
};

pub fn compute_label_offsets(
    allocator: std.mem.Allocator,
    instructions: []const exe.instruction,
) encode_error!std.AutoHashMap(u32, usize) {
    var label_offsets = std.AutoHashMap(u32, usize).init(allocator);
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
    return label_offsets;
}

pub fn encode(allocator: std.mem.Allocator, instructions: []const exe.instruction) encode_error![]u8 {
    var label_offsets = try compute_label_offsets(allocator, instructions);
    defer label_offsets.deinit();

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
            try ensure_reg(op_load.dst);
            if (op_load.const_index > 8191) return error.const_index_too_large;
            return bytecode.encoded_size(op.control.load_const, .{
                .src = 0,
                .dst = op_load.dst,
                .val = op_load.const_index,
            });
        },
        .move => |mv| {
            try ensure_reg(mv.dst);
            try ensure_reg(mv.src);
            return bytecode.encoded_size(op.control.move, .{ .src = mv.src, .dst = mv.dst });
        },
        .argument_set => |mv| {
            try ensure_reg(mv.dst);
            try ensure_reg(mv.src);
            return bytecode.encoded_size(op.control.argument_set, .{ .src = mv.src, .dst = mv.dst });
        },
        .add => |bin| return size_tri(op.int_math.binary_add, bin),
        .sub => |bin| return size_tri(op.int_math.binary_sub, bin),
        .mul => |bin| return size_tri(op.int_math.binary_multiply, bin),
        .div => |bin| return size_tri(op.int_math.binary_divide, bin),
        .rem => |bin| return size_tri(op.int_math.binary_remainder, bin),
        .min => |bin| return size_tri(op.int_math.binary_minimum, bin),
        .max => |bin| return size_tri(op.int_math.binary_maximum, bin),
        .bit_and => |bin| return size_tri(op.int_math.bitwise_and, bin),
        .bit_or => |bin| return size_tri(op.int_math.bitwise_or, bin),
        .bit_xor => |bin| return size_tri(op.int_math.bitwise_xor, bin),
        .bit_shl => |bin| return size_tri(op.int_math.bitwise_shift_left, bin),
        .bit_shr => |bin| return size_tri(op.int_math.bitwise_shift_right, bin),
        .bit_sar => |bin| return size_tri(op.int_math.bitwise_shift_arithmetic, bin),
        .bit_rol => |bin| return size_tri(op.int_math.bitwise_rotate_left, bin),
        .bit_ror => |bin| return size_tri(op.int_math.bitwise_rotate_right, bin),
        .bit_not => |bin| return size_tri(op.int_math.unary_bitwise_not, bin),
        .int_neg => |bin| return size_tri(op.int_math.unary_negate, bin),
        .int_abs => |bin| return size_tri(op.int_math.unary_absolute, bin),
        .compare_eq => |bin| return size_tri(op.int_math.compare_equal, bin),
        .compare_ne => return error.unsupported_instruction,
        .compare_lt => |bin| return size_tri(op.int_math.compare_less_than, bin),
        .compare_le => |bin| return size_tri(op.int_math.compare_less_than_or_equal, bin),
        .compare_gt => |bin| return size_tri(op.int_math.compare_greater_than, bin),
        .compare_ge => |bin| return size_tri(op.int_math.compare_greater_than_or_equal, bin),
        .fadd => |bin| return size_tri(op.float_math.binary_add, bin),
        .fsub => |bin| return size_tri(op.float_math.binary_sub, bin),
        .fmul => |bin| return size_tri(op.float_math.binary_multiply, bin),
        .fdiv => |bin| return size_tri(op.float_math.binary_divide, bin),
        .frem => |bin| return size_tri(op.float_math.binary_remainder, bin),
        .fmin => |bin| return size_tri(op.float_math.binary_minimum, bin),
        .fmax => |bin| return size_tri(op.float_math.binary_maximum, bin),
        .fcompare_eq => |bin| return size_tri(op.float_math.compare_eq, bin),
        .fcompare_lt => |bin| return size_tri(op.float_math.compare_lt, bin),
        .fcompare_gt => |bin| return size_tri(op.float_math.compare_gt, bin),
        .fneg => |bin| return size_tri(op.float_math.unary_negate, bin),
        .fabs => |bin| return size_tri(op.float_math.unary_absolute, bin),
        .fsqrt => |bin| return size_tri(op.float_math.unary_sqrt, bin),
        .fsin => |bin| return size_tri(op.float_math.unary_sine, bin),
        .fcos => |bin| return size_tri(op.float_math.unary_cosine, bin),
        .ftan => |bin| return size_tri(op.float_math.unary_tangent, bin),
        .fasin => |bin| return size_tri(op.float_math.unary_arcsine, bin),
        .facos => |bin| return size_tri(op.float_math.unary_arccosine, bin),
        .fatan => |bin| return size_tri(op.float_math.unary_arctangent, bin),
        .ffloor => |bin| return size_tri(op.float_math.unary_floor, bin),
        .fceil => |bin| return size_tri(op.float_math.unary_ceil, bin),
        .fround => |bin| return size_tri(op.float_math.unary_round, bin),
        .ftrunc => |bin| return size_tri(op.float_math.unary_truncate, bin),
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
        .call_register => |call| {
            try ensure_reg(call.src);
            try ensure_reg(call.dst);
            return bytecode.encoded_size(op.control.call_register, .{ .src = call.src, .dst = call.dst });
        },
        .call_foreign => |call| {
            if (call.index > 8191) return error.const_index_too_large;
            return bytecode.encoded_size(op.control.call_foreign, .{ .src = 0, .dst = 0, .val = call.index });
        },
        .task_spawn => |spawn| {
            try ensure_reg(spawn.dst);
            if (spawn.argc > max_register) return error.register_too_large;
            return bytecode.encoded_size(op.control.task_spawn, .{
                .dst = spawn.dst,
                .argc = spawn.argc,
                .target_absolute = 0,
            });
        },
        .task_await => |await_inst| {
            try ensure_reg(await_inst.dst);
            try ensure_reg(await_inst.src);
            return bytecode.encoded_size(op.control.task_await, .{ .src = await_inst.src, .dst = await_inst.dst });
        },
        .task_await_any => |await_any| {
            try ensure_reg(await_any.dst);
            try ensure_reg(await_any.src);
            return bytecode.encoded_size(op.control.task_await_any, .{
                .dst = await_any.dst,
                .src = await_any.src,
                .val = await_any.count,
            });
        },
        .task_cancel => |cancel_inst| {
            try ensure_reg(cancel_inst.src);
            return bytecode.encoded_size(op.control.task_cancel, .{ .src = cancel_inst.src, .dst = 0 });
        },
        .ret => {
            return bytecode.encoded_size(op.control.ret, .{ .src = 0, .dst = 0 });
        },
        .ret_value => |retv| {
            try ensure_reg(retv.src);
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
            try ensure_reg(op_load.dst);
            if (op_load.const_index > 8191) return error.const_index_too_large;
            try assembler.emit(op.control.load_const, .{
                .src = 0,
                .dst = op_load.dst,
                .val = op_load.const_index,
            });
        },
        .move => |mv| {
            try ensure_reg(mv.dst);
            try ensure_reg(mv.src);
            try assembler.emit(op.control.move, .{ .src = mv.src, .dst = mv.dst });
        },
        .argument_set => |mv| {
            try ensure_reg(mv.dst);
            try ensure_reg(mv.src);
            try assembler.emit(op.control.argument_set, .{ .src = mv.src, .dst = mv.dst });
        },
        .add => |bin| try emit_tri(assembler, op.int_math.binary_add, bin),
        .sub => |bin| try emit_tri(assembler, op.int_math.binary_sub, bin),
        .mul => |bin| try emit_tri(assembler, op.int_math.binary_multiply, bin),
        .div => |bin| try emit_tri(assembler, op.int_math.binary_divide, bin),
        .rem => |bin| try emit_tri(assembler, op.int_math.binary_remainder, bin),
        .min => |bin| try emit_tri(assembler, op.int_math.binary_minimum, bin),
        .max => |bin| try emit_tri(assembler, op.int_math.binary_maximum, bin),
        .bit_and => |bin| try emit_tri(assembler, op.int_math.bitwise_and, bin),
        .bit_or => |bin| try emit_tri(assembler, op.int_math.bitwise_or, bin),
        .bit_xor => |bin| try emit_tri(assembler, op.int_math.bitwise_xor, bin),
        .bit_shl => |bin| try emit_tri(assembler, op.int_math.bitwise_shift_left, bin),
        .bit_shr => |bin| try emit_tri(assembler, op.int_math.bitwise_shift_right, bin),
        .bit_sar => |bin| try emit_tri(assembler, op.int_math.bitwise_shift_arithmetic, bin),
        .bit_rol => |bin| try emit_tri(assembler, op.int_math.bitwise_rotate_left, bin),
        .bit_ror => |bin| try emit_tri(assembler, op.int_math.bitwise_rotate_right, bin),
        .bit_not => |bin| try emit_tri(assembler, op.int_math.unary_bitwise_not, bin),
        .int_neg => |bin| try emit_tri(assembler, op.int_math.unary_negate, bin),
        .int_abs => |bin| try emit_tri(assembler, op.int_math.unary_absolute, bin),
        .compare_eq => |bin| try emit_tri(assembler, op.int_math.compare_equal, bin),
        .compare_ne => return error.unsupported_instruction,
        .compare_lt => |bin| try emit_tri(assembler, op.int_math.compare_less_than, bin),
        .compare_le => |bin| try emit_tri(assembler, op.int_math.compare_less_than_or_equal, bin),
        .compare_gt => |bin| try emit_tri(assembler, op.int_math.compare_greater_than, bin),
        .compare_ge => |bin| try emit_tri(assembler, op.int_math.compare_greater_than_or_equal, bin),
        .fadd => |bin| try emit_tri(assembler, op.float_math.binary_add, bin),
        .fsub => |bin| try emit_tri(assembler, op.float_math.binary_sub, bin),
        .fmul => |bin| try emit_tri(assembler, op.float_math.binary_multiply, bin),
        .fdiv => |bin| try emit_tri(assembler, op.float_math.binary_divide, bin),
        .frem => |bin| try emit_tri(assembler, op.float_math.binary_remainder, bin),
        .fmin => |bin| try emit_tri(assembler, op.float_math.binary_minimum, bin),
        .fmax => |bin| try emit_tri(assembler, op.float_math.binary_maximum, bin),
        .fcompare_eq => |bin| try emit_tri(assembler, op.float_math.compare_eq, bin),
        .fcompare_lt => |bin| try emit_tri(assembler, op.float_math.compare_lt, bin),
        .fcompare_gt => |bin| try emit_tri(assembler, op.float_math.compare_gt, bin),
        .fneg => |bin| try emit_tri(assembler, op.float_math.unary_negate, bin),
        .fabs => |bin| try emit_tri(assembler, op.float_math.unary_absolute, bin),
        .fsqrt => |bin| try emit_tri(assembler, op.float_math.unary_sqrt, bin),
        .fsin => |bin| try emit_tri(assembler, op.float_math.unary_sine, bin),
        .fcos => |bin| try emit_tri(assembler, op.float_math.unary_cosine, bin),
        .ftan => |bin| try emit_tri(assembler, op.float_math.unary_tangent, bin),
        .fasin => |bin| try emit_tri(assembler, op.float_math.unary_arcsine, bin),
        .facos => |bin| try emit_tri(assembler, op.float_math.unary_arccosine, bin),
        .fatan => |bin| try emit_tri(assembler, op.float_math.unary_arctangent, bin),
        .ffloor => |bin| try emit_tri(assembler, op.float_math.unary_floor, bin),
        .fceil => |bin| try emit_tri(assembler, op.float_math.unary_ceil, bin),
        .fround => |bin| try emit_tri(assembler, op.float_math.unary_round, bin),
        .ftrunc => |bin| try emit_tri(assembler, op.float_math.unary_truncate, bin),
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
        .call_register => |call| {
            try ensure_reg(call.src);
            try ensure_reg(call.dst);
            try assembler.emit(op.control.call_register, .{ .src = call.src, .dst = call.dst });
        },
        .call_foreign => |call| {
            if (call.index > 8191) return error.const_index_too_large;
            try assembler.emit(op.control.call_foreign, .{ .src = 0, .dst = 0, .val = call.index });
        },
        .task_spawn => |spawn| {
            const target = labels.get(spawn.target) orelse return error.label_not_found;
            try ensure_reg(spawn.dst);
            if (spawn.argc > 7) return error.register_too_large;
            try assembler.emit(op.control.task_spawn, .{
                .dst = spawn.dst,
                .argc = spawn.argc,
                .target_absolute = target,
            });
        },
        .task_await => |await_inst| {
            try ensure_reg(await_inst.dst);
            try ensure_reg(await_inst.src);
            try assembler.emit(op.control.task_await, .{ .src = await_inst.src, .dst = await_inst.dst });
        },
        .task_await_any => |await_any| {
            try ensure_reg(await_any.dst);
            try ensure_reg(await_any.src);
            try assembler.emit(op.control.task_await_any, .{
                .dst = await_any.dst,
                .src = await_any.src,
                .val = await_any.count,
            });
        },
        .task_cancel => |cancel_inst| {
            try ensure_reg(cancel_inst.src);
            try assembler.emit(op.control.task_cancel, .{ .src = cancel_inst.src, .dst = 0 });
        },
        .ret => {
            try assembler.emit(op.control.ret, .{ .src = 0, .dst = 0 });
        },
        .ret_value => |retv| {
            try ensure_reg(retv.src);
            try assembler.emit(op.control.ret_value, .{ .src = retv.src, .dst = 0 });
        },
        .halt => {
            try assembler.emit(op.control.halt, .{ .src = 0, .dst = 0 });
        },
    }
}

fn ensure_reg(reg: u8) encode_error!void {
    if (reg > max_register) return error.register_too_large;
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
