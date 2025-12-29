pub const std = @import("std");
pub const op = @import("./op.zig");
pub const mem = std.mem;
pub const mem_sort = mem.sort;
pub const instruction = @import("ink").vm.instruction;
pub const array_list = std.array_list.Managed;
pub const mem_allocator = std.mem.Allocator;
pub const int_fitting_range = std.math.IntFittingRange;
pub const struct_field = std.builtin.Type.StructField;
pub const tuple = std.meta.Tuple;
pub const tape = @import("./core.zig").machine.tape;
const small = packed struct {
    opcode: u8,
    src: u3,
    dst: u3,
};

const standard = packed struct {
    opcode: u8,
    src: u4,
    dst: u3,
};

const wide = packed struct {
    opcode: u10,
    src: u5,
    dst: u3,
};

const huge = packed struct {
    opcode: u12,
    src: u6,
    dst: u3,
};

const imm_val = packed struct {
    opcode: u10,
    src: u5,
    dst: u3,
    val: u13,
};

const jmp_loc_abs = packed struct {
    opcode: u8,
    target_absolute: u24,
};

const jmp_loc_rel = packed struct {
    opcode: u8,
    target_relative: i24,
};

const jmp_loc_ind = packed struct {
    opcode: u8,
    target_register: u24,
};

const tri = packed struct {
    opcode: u10,
    dst: u8,
    src_a: u8,
    src_b: u8,
};

const imm_val_dual = packed struct {
    opcode: u8,
    dst: u8,
    src: u8,
    val: u8,
};

const jmp_cond_abs = packed struct {
    opcode: u8,
    condition_register: u8,
    target_absolute: u24,
};

const jmp_cond_rel = packed struct {
    opcode: u8,
    condition_register: u8,
    target_relative: i24,
};

fn struct_field_bit_size_less_than(context: type, lhs: type, rhs: type) bool {
    _ = context;
    return @bitSizeOf(lhs) < @bitSizeOf(rhs);
}

fn create_accessor_type(comptime config: anytype) type {
    return comptime blk: {
        // 1. Use an ArrayList to build the list of fields.
        //    This is the most robust way to build a collection at comptime.
        const fields_list_total = @intFromBool(config.include_vm_field) + @typeInfo(@TypeOf(config.fields)).@"struct".fields.len;
        var fields_list = [_]std.builtin.Type.StructField{undefined} ** fields_list_total;
        var fields_list_count = 0;

        // 2. Conditionally add the `vm` field to the list.
        if (config.include_vm_field) {
            // .append can fail (OOM), so we must catch. At comptime, we error out.
            fields_list[fields_list_count] = .{
                .name = "vm",
                .type = *config.vm_type,
                .default_value_ptr = null,
                .is_comptime = false,
                .alignment = @alignOf(*config.vm_type),
            };
            fields_list_count += 1;
        }

        // 3. Add all custom data fields to the list.
        const custom_fields_info = @typeInfo(@TypeOf(config.fields));
        for (custom_fields_info.@"struct".fields) |field_info| {
            const field_type = @field(config.fields, field_info.name);
            fields_list[fields_list_count] = .{
                .name = field_info.name,
                .type = field_type,
                .default_value_ptr = null,
                .is_comptime = false,
                .alignment = @alignOf(field_type),
            };
        }

        // 5. Create the final struct type using the .items slice from the ArrayLists.
        const final_struct = @Type(.{
            .@"struct" = .{
                .layout = .auto,
                .fields = &fields_list,
                .decls = &[0]std.builtin.Type.Declaration{},
                .is_tuple = false,
            },
        });

        break :blk final_struct;
    };
}

pub fn accessors(comptime vm_type: type) type {
    return struct {
        // --- Method Implementations ---
        // We define the logic once and reuse it where possible.
        // `self: anytype` allows these functions to be used in different generated structs.

        pub const destination_register = create_accessor_type(.{
            .vm_type = vm_type,
            .include_vm_field = true,
            .fields = .{ .index = usize },
        });

        /// Accesses a register for reading only.
        pub const source_register = create_accessor_type(.{
            .vm_type = vm_type,
            .include_vm_field = true,
            .fields = .{ .index = usize },
        });

        /// Wraps a raw u64 value.
        pub const immediate_value = create_accessor_type(.{
            .vm_type = vm_type,
            .include_vm_field = false, // Does not need the vm
            .fields = .{ .raw_value = u64 },
        });

        /// Jumps to an absolute program counter address.
        pub const absolute_jump = create_accessor_type(.{
            .vm_type = vm_type,
            .include_vm_field = true,
            .fields = .{ .target_address = usize },
        });

        /// Jumps forward or backward by a relative offset.
        pub const relative_jump = create_accessor_type(.{
            .vm_type = vm_type,
            .include_vm_field = true,
            .fields = .{ .offset = isize },
        });

        /// Jumps to an address stored in a register.
        pub const indirect_jump = create_accessor_type(.{
            .vm_type = vm_type,
            .include_vm_field = true,
            .fields = .{ .register_index = usize },
        });
    };
}

pub fn control_operators(comptime vm: type) type {
    return struct {
        const self = @This();
        const register_count = 8;
        fn reg_ptr(machine: *vm, index: usize) *u64 {
            const addr = machine.current.fp + index;
            return machine.memory.access(addr);
        }

        fn no_operation(machine: *vm, src: usize, dst: usize) void {
            _ = machine;
            _ = src;
            _ = dst;
        }

        fn halt(machine: *vm, src: usize, dst: usize) void {
            _ = src;
            _ = dst;
            machine.halted = true;
        }

        fn load_const(machine: *vm, src: usize, dst: usize, val: usize) void {
            _ = src;
            if (val >= machine.constants.len) {
                machine.memory.write(machine.current.fp + dst, 0);
                return;
            }
            machine.memory.write(machine.current.fp + dst, machine.constants[val]);
        }

        fn move(machine: *vm, src: usize, dst: usize) void {
            machine.memory.write(machine.current.fp + dst, machine.memory.read(machine.current.fp + src));
        }

        fn jump_always(machine: *vm, target_absolute: usize) void {
            machine.current.pc = target_absolute;
        }

        fn jump_if_true(machine: *vm, condition_register: usize, target_absolute: usize) void {
            if (machine.memory.read(machine.current.fp + condition_register) != 0) {
                machine.current.pc = target_absolute;
            }
        }

        fn jump_if_false(machine: *vm, condition_register: usize, target_absolute: usize) void {
            if (machine.memory.read(machine.current.fp + condition_register) == 0) {
                machine.current.pc = target_absolute;
            }
        }

        fn conditional_move(machine: *vm, dst: usize, src_a: usize, src_b: usize) void {
            if (machine.memory.read(machine.current.fp + src_b) != 0) {
                argument_set(machine, src_a, dst);
            }
        }

        fn conditional_move_value(machine: *vm, dst: usize, src: usize, val: usize) void {
            if (machine.memory.read(machine.current.fp + src) != 0) {
                machine.memory.write(machine.current.fp + dst, @intCast(val));
            }
        }

        fn argument_set(machine: *vm, src: usize, dst: usize) void {
            if (!machine.arg_base_valid) {
                machine.arg_base = machine.current.sp;
                machine.arg_base_valid = true;
            }
            const addr = machine.arg_base + 2 + dst;
            if (addr >= machine.current.sp) {
                machine.current.sp = addr + 1;
            }
            machine.memory.write(addr, machine.memory.read(machine.current.fp + src));
        }

        fn call(machine: *vm, target_absolute: usize) void {
            const base = if (machine.arg_base_valid) machine.arg_base else machine.current.sp;
            machine.memory.write(base, machine.current.pc + machine.last_inst_size);
            machine.memory.write(base + 1, machine.current.fp);
            machine.current.fp = base + 2;
            const frame_end = base + 2 + register_count;
            if (machine.current.sp < frame_end) {
                machine.current.sp = frame_end;
            }
            machine.arg_base = machine.current.sp;
            machine.arg_base_valid = false;
            machine.current.pc = target_absolute;
        }

        fn call_indirect(machine: *vm, src: usize) void {
            const target = machine.memory.read(machine.current.fp + src);
            call(machine, @intCast(target));
        }

        fn call_register(machine: *vm, src: usize, dst: usize) void {
            _ = dst;
            call_indirect(machine, src);
        }

        fn call_tail(machine: *vm, target_absolute: usize) void {
            jump_always(machine, target_absolute);
        }

        fn call_foreign(machine: *vm, src: usize, dst: usize, val: usize) void {
            _ = src;
            _ = dst;
            switch (val) {
                0 => {
                    var buffer: [256]u8 = undefined;
                    var out_file = std.fs.File.stdout().writer(buffer[0..]);
                    var out = &out_file.interface;
                    out.print("{d}\n", .{machine.memory.read(machine.current.fp + 0)}) catch {};
                    out.flush() catch {};
                },
                else => {},
            }
        }

        fn ret(machine: *vm, src: usize, dst: usize) void {
            _ = src;
            _ = dst;
            const ret_pc = machine.memory.read(machine.current.fp - 2);
            const prev_fp = machine.memory.read(machine.current.fp - 1);
            machine.current.sp = machine.current.fp - 2;
            machine.current.fp = prev_fp;
            machine.current.pc = ret_pc;
        }

        fn ret_value(machine: *vm, src: usize, dst: usize) void {
            _ = dst;
            const value = machine.memory.read(machine.current.fp + src);
            const ret_pc = machine.memory.read(machine.current.fp - 2);
            const prev_fp = machine.memory.read(machine.current.fp - 1);
            machine.memory.write(prev_fp + 0, value);
            machine.current.sp = machine.current.fp - 2;
            machine.current.fp = prev_fp;
            machine.current.pc = ret_pc;
        }

        fn method_virtual_get(machine: *vm, src: usize, dst: usize) void {
            _ = machine;
            _ = src;
            _ = dst;
        }
    };
}

pub fn int_math_operators(comptime vm: type) type {
    return struct {
        fn reg(machine: *vm, index: usize) u64 {
            return machine.memory.read(machine.current.fp + index);
        }

        fn set(machine: *vm, index: usize, value: u64) void {
            machine.memory.write(machine.current.fp + index, value);
        }

        fn compare_equal(machine: *vm, dst: usize, src_a: usize, src_b: usize) void {
            set(machine, dst, @intFromBool(reg(machine, src_a) == reg(machine, src_b)));
        }
        fn compare_less_than(machine: *vm, dst: usize, src_a: usize, src_b: usize) void {
            set(machine, dst, @intFromBool(@as(i64, @bitCast(reg(machine, src_a))) < @as(i64, @bitCast(reg(machine, src_b)))));
        }
        fn compare_less_than_or_equal(machine: *vm, dst: usize, src_a: usize, src_b: usize) void {
            set(machine, dst, @intFromBool(@as(i64, @bitCast(reg(machine, src_a))) <= @as(i64, @bitCast(reg(machine, src_b)))));
        }
        fn compare_greater_than(machine: *vm, dst: usize, src_a: usize, src_b: usize) void {
            set(machine, dst, @intFromBool(@as(i64, @bitCast(reg(machine, src_a))) > @as(i64, @bitCast(reg(machine, src_b)))));
        }
        fn compare_greater_than_or_equal(machine: *vm, dst: usize, src_a: usize, src_b: usize) void {
            set(machine, dst, @intFromBool(@as(i64, @bitCast(reg(machine, src_a))) >= @as(i64, @bitCast(reg(machine, src_b)))));
        }

        fn unary_negate(machine: *vm, dst: usize, src_a: usize, _: usize) void {
            const val: i64 = @bitCast(reg(machine, src_a));
            set(machine, dst, @bitCast(-val));
        }
        fn unary_absolute(machine: *vm, dst: usize, src_a: usize, _: usize) void {
            const val: i64 = @bitCast(reg(machine, src_a));
            set(machine, dst, @bitCast(@abs(val)));
        }
        fn unary_bitwise_not(machine: *vm, dst: usize, src_a: usize, _: usize) void {
            set(machine, dst, ~reg(machine, src_a));
        }

        fn binary_add(machine: *vm, dst: usize, src_a: usize, src_b: usize) void {
            const a: i64 = @bitCast(reg(machine, src_a));
            const b: i64 = @bitCast(reg(machine, src_b));
            set(machine, dst, @bitCast(a + b));
        }
        fn binary_sub(machine: *vm, dst: usize, src_a: usize, src_b: usize) void {
            const a: i64 = @bitCast(reg(machine, src_a));
            const b: i64 = @bitCast(reg(machine, src_b));
            set(machine, dst, @bitCast(a - b));
        }
        fn binary_multiply(machine: *vm, dst: usize, src_a: usize, src_b: usize) void {
            const a: i64 = @bitCast(reg(machine, src_a));
            const b: i64 = @bitCast(reg(machine, src_b));
            set(machine, dst, @bitCast(a * b));
        }
        fn binary_divide(machine: *vm, dst: usize, src_a: usize, src_b: usize) void {
            const a: i64 = @bitCast(reg(machine, src_a));
            const b: i64 = @bitCast(reg(machine, src_b));
            set(machine, dst, @bitCast(@divTrunc(a, b)));
        }
        fn binary_remainder(machine: *vm, dst: usize, src_a: usize, src_b: usize) void {
            const a: i64 = @bitCast(reg(machine, src_a));
            const b: i64 = @bitCast(reg(machine, src_b));
            set(machine, dst, @bitCast(@mod(a, b)));
        }
        fn binary_minimum(machine: *vm, dst: usize, src_a: usize, src_b: usize) void {
            const a: i64 = @bitCast(reg(machine, src_a));
            const b: i64 = @bitCast(reg(machine, src_b));
            set(machine, dst, @bitCast(@min(a, b)));
        }
        fn binary_maximum(machine: *vm, dst: usize, src_a: usize, src_b: usize) void {
            const a: i64 = @bitCast(reg(machine, src_a));
            const b: i64 = @bitCast(reg(machine, src_b));
            set(machine, dst, @bitCast(@max(a, b)));
        }

        fn bitwise_and(machine: *vm, dst: usize, src_a: usize, src_b: usize) void {
            set(machine, dst, reg(machine, src_a) & reg(machine, src_b));
        }
        fn bitwise_or(machine: *vm, dst: usize, src_a: usize, src_b: usize) void {
            set(machine, dst, reg(machine, src_a) | reg(machine, src_b));
        }
        fn bitwise_xor(machine: *vm, dst: usize, src_a: usize, src_b: usize) void {
            set(machine, dst, reg(machine, src_a) ^ reg(machine, src_b));
        }
        fn bitwise_shift_left(machine: *vm, dst: usize, src_a: usize, src_b: usize) void {
            const shift: u6 = @intCast(reg(machine, src_b));
            set(machine, dst, reg(machine, src_a) << shift);
        }
        fn bitwise_shift_right(machine: *vm, dst: usize, src_a: usize, src_b: usize) void {
            const shift: u6 = @intCast(reg(machine, src_b));
            set(machine, dst, reg(machine, src_a) >> shift);
        }
        fn bitwise_shift_arithmetic(machine: *vm, dst: usize, src_a: usize, src_b: usize) void {
            const shift: u6 = @intCast(reg(machine, src_b));
            const val: i64 = @bitCast(reg(machine, src_a));
            set(machine, dst, @bitCast(val >> shift));
        }
        fn bitwise_rotate_left(machine: *vm, dst: usize, src_a: usize, src_b: usize) void {
            const shift: u6 = @intCast(reg(machine, src_b));
            set(machine, dst, std.math.rotl(u64, reg(machine, src_a), shift));
        }
        fn bitwise_rotate_right(machine: *vm, dst: usize, src_a: usize, src_b: usize) void {
            const shift: u6 = @intCast(reg(machine, src_b));
            set(machine, dst, std.math.rotr(u64, reg(machine, src_a), shift));
        }
    };
}
pub const bytecode = assembly(.{ .control = op.control, .int_math = op.int_math }, .{ small, standard, wide, huge, imm_val, jmp_loc_abs, jmp_loc_rel, jmp_loc_ind, imm_val_dual, tri, jmp_cond_abs, jmp_cond_rel }, .{ .control = control_operators, .int_math = int_math_operators });

pub fn assembly(comptime operation_sets: anytype, comptime formats: anytype, comptime logic_map: anytype) type {
    const operation_map = struct {
        const sets = @typeInfo(@TypeOf(operation_sets)).@"struct".fields;
        const count = blk: {
            var sum = 0;
            for (sets) |set| sum += @typeInfo(set.defaultValue().?).@"enum".fields.len;
            break :blk sum;
        };

        const tag_type = int_fitting_range(0, count - 1);

        fn get(operation: anytype) tag_type {
            const op_type = @TypeOf(operation);
            var base: usize = 0;
            inline for (sets) |set| {
                const ty = set.defaultValue().?;
                if (ty == op_type) return @intCast(base + @intFromEnum(operation));
                base += @typeInfo(ty).@"enum".fields.len;
            }
            @compileError("Unknown instruction passed to assembler");
        }
    };
    const fields = @typeInfo(@TypeOf(formats)).@"struct".fields;

    // 2. Create a specific, mutable array of the correct size
    var sorted_formats_fn: [fields.len]type = undefined;

    // 3. Copy the fields into your mutable array
    inline for (fields, 0..) |field, i| {
        sorted_formats_fn[i] = field.defaultValue().?;
    }

    // 4. Sort the mutable array (pass the slice using &)
    // Note: Ensure `struct_field` is the Type, e.g. std.builtin.Type.StructField
    //
    mem_sort(type, &sorted_formats_fn, void, struct_field_bit_size_less_than);

    const sorted_formats = sorted_formats_fn;
    return struct {
        pub const executor = struct {
            const self = @This();

            pub const state = struct {
                pc: usize,
                fp: usize,
                sp: usize,
            };

            current: state,
            memory: *tape,
            constants: []const u64,
            halted: bool,
            arg_base: usize,
            arg_base_valid: bool,
            last_inst_size: usize,

            pub fn stack_push(this: *self, val: u64) void {
                this.memory.write(this.current.sp, val);
                this.current.sp += 1;
            }

            pub fn init(start: state, memory: *tape, constants: []const u64) self {
                return .{
                    .current = start,
                    .memory = memory,
                    .constants = constants,
                    .halted = false,
                    .arg_base = start.sp,
                    .arg_base_valid = false,
                    .last_inst_size = 0,
                };
            }

            pub fn step(this: *self, code: []const u8, budget: usize) *state {
                const format_bits = std.math.log2_int_ceil(usize, sorted_formats.len);
                const format_mask = (@as(usize, 1) << @intCast(format_bits)) - 1;
                var health = budget;

                while (health > 0 and !this.halted) {
                    const raw = code[this.current.pc];
                    const format_index = raw & format_mask;
                    inline for (sorted_formats, 0..) |format, idx| {
                    if (idx == format_index) {
                        const size = @sizeOf(format);
                        const inst = mem.bytesToValue(format, code[this.current.pc..][0..size]);
                        const opcode_val: usize = @intCast(inst.opcode);
                        const operation: u8 = @intCast(opcode_val >> @intCast(format_bits));
                        const pc_start = this.current.pc;
                        this.last_inst_size = size;
                        this.dispatch(operation, inst);
                        if (this.current.pc == pc_start) {
                                this.current.pc += size;
                            }
                            health -= 1;
                            break;
                        }
                    }
                }
                return &this.current;
            }
            pub fn dispatch(this: *self, operation: u8, instruction_payload: anytype) void {
                comptime var opcode_base_index = 0;

                inline for (operation_map.sets) |instruction_set| {
                    const op_codes_enum = instruction_set.defaultValue().?;
                    const set_name = instruction_set.name;
                    const set_length = @typeInfo(op_codes_enum).@"enum".fields.len;

                    if (operation >= opcode_base_index and operation < opcode_base_index + set_length) {
                        const implementation_factory = @field(logic_map, set_name);
                        const implementation_struct = implementation_factory(self);

                        inline for (@typeInfo(op_codes_enum).@"enum".fields) |op_code| {
                            const local_index = op_code.value;
                            const target_opcode = opcode_base_index + local_index;

                            if (operation == target_opcode) {
                                const function_name = op_code.name;

                                if (!@hasDecl(implementation_struct, function_name)) {
                                    @compileError("missing implementation for opcode: " ++ set_name ++ "." ++ function_name);
                                }

                                const implementation_function = @field(implementation_struct, function_name);
                                const function_params = @typeInfo(@TypeOf(implementation_function)).@"fn".params;

                                var function_arguments: std.meta.ArgsTuple(@TypeOf(implementation_function)) = undefined;
                                function_arguments[0] = this;

                                const instruction_payload_type_info = @typeInfo(@TypeOf(instruction_payload));
                                if (instruction_payload_type_info.@"struct".fields.len == function_params.len) {
                                    inline for (function_params[1..], 1..) |param, index| {
                                        const function_param_type_info = @typeInfo(param.type.?);
                                        const name = instruction_payload_type_info.@"struct".fields[index].name;
                                        switch (function_param_type_info) {
                                            .int => {
                                                function_arguments[index] = @intCast(@field(instruction_payload, name));
                                            },
                                            .@"union" => {
                                                @field(function_arguments[index], name) = @field(instruction_payload, name);
                                                @compileError("unhandled " ++ @typeName(param.type.?));
                                            },
                                            .type => {},
                                            else => {
                                                @compileError("unhandled type" ++ @typeName(@TypeOf(param.type.?)));
                                            },
                                        }
                                    }
                                    @call(.auto, implementation_function, function_arguments);
                                    return;
                                }
                            }
                        }
                    }

                    opcode_base_index += set_length;
                }

                unreachable;
            }

            fn param_types(comptime params: []const std.builtin.Type.Fn.Param) [params.len - 1]type {
                if (params.len == 0) @compileError("Implementation function must take at least 1 argument (the VM)");

                var types: [params.len - 1]type = undefined;

                for (params[1..], 0..) |param, i| {
                    types[i] = param.type.?;
                }

                return types;
            }
        };
        pub const assembler = struct {
            const self = @This();
            buffer: array_list(u8),

            pub fn init(allocator: mem_allocator) self {
                return .{ .buffer = array_list(u8).init(allocator) };
            }

            pub fn deinit(this: *self) void {
                this.buffer.deinit();
            }

            pub fn emit(this: *self, operation: anytype, args: anytype) !void {
                inline for (sorted_formats, 0..) |format, format_index| {
                    if (is_compatible(format, args)) {
                        const opcode_type = @typeInfo(format).@"struct".fields[0].type;
                        const op_size: opcode_type = @intCast(operation_map.get(operation));
                        const format_bits: opcode_type = @intCast(std.math.log2_int_ceil(opcode_type, sorted_formats.len));
                        const header = (@as(opcode_type, @intCast(format_index))) | (op_size) << @intCast(format_bits);
                        var inst: format = std.mem.zeroes(format);
                        inst.opcode = @intCast(header);

                        inline for (std.meta.fields(format)) |ff| {
                            if (!mem.eql(u8, ff.name, "opcode")) {
                                if (@hasField(@TypeOf(args), ff.name)) {
                                    @field(inst, ff.name) = @intCast(@field(args, ff.name));
                                }
                            }
                        }

                        try this.buffer.appendSlice(mem.asBytes(&inst));
                        return;
                    }
                }

                return error.instruction_invalid;
            }

            fn is_compatible(comptime format: type, args: anytype) bool {
                const arg_info = @typeInfo(@TypeOf(args)).@"struct";
                const format_info = @typeInfo(format).@"struct";
                inline for (arg_info.fields) |af| {
                    if (!@hasField(format, af.name)) return false;
                }
                inline for (format_info.fields) |ff| {
                    if (@hasField(@TypeOf(args), ff.name)) {
                        const val = @field(args, ff.name);
                        const bits = @typeInfo(ff.type).int.bits;
                        if (val > (1 << bits) - 1) return false;
                    }
                }
                return true;
            }

            pub fn finish(this: *self) []u8 {
                const ret = this.buffer.toOwnedSlice();
                this.deinit();
                return ret catch unreachable;
            }
        };
        pub fn encoded_size(operation: anytype, args: anytype) usize {
            _ = operation;
            inline for (sorted_formats) |format| {
                if (assembler.is_compatible(format, args)) {
                    return @sizeOf(format);
                }
            }
            unreachable;
        }
    };
}

test "bytecode basic" {
    var assembler = bytecode.assembler.init(std.testing.allocator);
    try assembler.emit(op.control.no_operation, .{ .src = 0, .dst = 0 });
    try assembler.emit(op.control.halt, .{ .src = 0, .dst = 0 });
    const done = assembler.finish();
    std.debug.assert(done.len > 0);
}

test "bytecode execute" {
    var assembler = bytecode.assembler.init(std.testing.allocator);
    try assembler.emit(op.control.no_operation, .{ .src = 0, .dst = 0 });
    try assembler.emit(op.control.halt, .{ .src = 0, .dst = 0 });
    const bytes = assembler.finish();
    var test_tape = @import("./core.zig").machine.tape.init(std.testing.allocator, 64);
    var exe = bytecode.executor.init(.{ .pc = 0, .fp = 0, .sp = 1 }, &test_tape, &[_]u64{});
    _ = exe.step(bytes, 20);
}
