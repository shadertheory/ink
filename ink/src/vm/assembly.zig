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
pub const access = @import("./core.zig").machine.fundamental;
pub const tape = @import("./core").machine.tape;
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
    opcode: u6,
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
    opcode: u8,
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

pub const jump_loc = union { target_absolute: usize, target_register: usize, target_relative: isize };

pub fn control_operators(comptime vm: type) type {
    return struct {
        const self = @This();
        fn no_operation(machine: *vm) void {
            _ = machine;
            std.debug.print("\nno op\n", .{});
            return;
        }
        fn halt(machine: *vm) void {
            _ = machine;
            std.debug.print("\nhalt\n", .{});
            return;
        }
        fn jump_always(machine: *vm, jump: jump_loc) void {
            switch (jump) {
                .target_absolute => |t| {
                    machine.pc = t;
                },
                .target_register => |r| {
                    machine.pc = machine.register[r];
                },
                .target_relative => |s| {
                    if (s >= 0) machine.pc += s else machine.pc -= -s;
                },
            }
            std.debug.print("\njump to {d}\n", .{jump.target_absolute});
            return;
        }
        fn jump_if(machine: *vm, condition: bool, condition_register: usize, jump: jump_loc) void {
            const val = *self.reg(condition_register);
            if (val == @intFromBool(condition)) {
                return;
            }
            self.jump_always(machine, jump);
        }
        fn jump_if_true(machine: *vm, condition_register: usize, jump: jump_loc) void {
            self.jump_if(machine, true, condition_register, jump);
        }
        fn jump_if_false(machine: *vm, condition_register: usize, jump: jump_loc) void {
            self.jump_if(machine, false, condition_register, jump);
        }
        fn reg(machine: *vm, index: usize) *u64 {
            const addr = machine.fp + index;
            if (addr >= machine.current.sp) {
                return 0;
            }
            return &machine.current.memory.data[addr];
        }
        fn push(machine: *vm, val: u64) *u64 {
            const item = self.peek();
            item.* = val;
            machine.current.sp += 1;
            return item;
        }
        fn pop(machine: *vm) u64 {
            const item = *self.peek();
            machine.current.sp -= 1;
            return item;
        }
        fn peek(machine: *vm) *u64 {
            return machine.current.memory.access(machine.current.sp);
        }
        fn conditional_move(machine: *vm, dst: usize, src_a: usize, src_b: usize) void {
            if (machine.get(src_b).* != @intFromBool(false)) {
                argument_set(machine, dst, src_a);
            }
        }
        fn conditional_move_value(machine: *vm, dst: usize, src: usize, val: usize) void {
            if (machine.get(src).* != @intFromBool(false)) {
                machine.reg(dst).* = @intCast(val);
            }
        }
        fn argument_set(machine: *vm, dst: usize, src: usize) void {
            machine.reg(dst).* = machine.reg(src).*;
        }
        fn call(machine: *vm, jump: jump_loc) void {
            machine.current.memory.allocate(&machine.current.sp, vm.state, machine.current.*);
            call_tail(machine, jump);
        }
        fn call_indirect(machine: *vm, src: usize) void {
            call(machine, .{ .target_absolute = reg(src) });
        }
        fn call_tail(machine: *vm, jump: jump_loc) void {
            jump_always(machine, jump);
        }
        fn call_foreign(machine: *vm, table_index: usize) void {
            _ = machine;
            _ = table_index;
            //TODO
            unreachable;
        }
        fn ret(machine: *vm) void {
            machine.current.* = machine.current.memory.free(&machine.memory.current.sp, vm.state);
        }
    };
}

pub const bytecode = assembly(.{ .control = op.control }, .{ small, standard, wide, huge, imm_val, jmp_loc_abs, jmp_loc_rel, jmp_loc_ind, imm_val_dual, tri, jmp_cond_abs, jmp_cond_rel }, .{ .control = control_operators });

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
        const executor = struct {
            const self = @This();

            pub const state = struct {
                pc: tape.index,
                fp: tape.index,
                sp: tape.index,
            };

            current: state,

            pub fn stack_push(this: *self, val: u64) void {
                this.memory.write(self.current.sp, val);
                this.current.sp += @sizeOf(u64);
            }

            pub fn init(start: state) self {
                return .{ .current = start };
            }

            pub fn step(this: *self, code: []const u8, budget: usize) *state {
                const format_bits = std.math.log2_int_ceil(usize, sorted_formats.len);
                const format_mask = (@as(usize, 1) << @intCast(format_bits)) - 1;
                var health = budget;

                while (health > 0) {
                    const raw = code[this.pc];
                    const format_index = raw & format_mask;
                    const operation = raw >> @intCast(format_bits);
                    inline for (sorted_formats, 0..) |format, idx| {
                        if (idx == format_index) {
                            const size = @sizeOf(format);
                            const inst = mem.bytesToValue(format, code[this.pc..][0..size]);
                            const pc_start = this.pc;
                            this.dispatch(operation, inst);
                            if (this.pc == pc_start) {
                                this.pc += size;
                            }
                            health -= 1;
                            break;
                        }
                    }
                }
                return &this.state;
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
                                        const function_param_type_info = @typeInfo(@TypeOf(param.type.?));
                                        const name = instruction_payload_type_info.@"struct".fields[index].name;
                                        switch (function_param_type_info) {
                                            .int => {
                                                function_arguments[index] = @field(instruction_payload, name);
                                                break;
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
        const assembler = struct {
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

            fn finish(this: *self) []u8 {
                const ret = this.buffer.toOwnedSlice();
                this.deinit();
                return ret catch unreachable;
            }
        };
    };
}

test "bytecode basic" {
    var assembler = bytecode.assembler.init(std.testing.allocator);
    try assembler.emit(op.control.no_operation, .{ .target_absolute = 700000 });
    try assembler.emit(op.control.halt, .{});
    const done = assembler.finish();
    const jmp: *align(1) jmp_loc_abs = std.mem.bytesAsValue(jmp_loc_abs, done[0..@sizeOf(jmp_loc_abs)]);
    std.debug.assert(jmp.target_absolute == 700000);
}

test "bytecode execute" {
    var assembler = bytecode.assembler.init(std.testing.allocator);
    try assembler.emit(op.control.no_operation, .{});
    try assembler.emit(op.control.jump_always, .{ .target_absolute = 0 });
    try assembler.emit(op.control.jump_always, .{ .target_relative = 3 });
    try assembler.emit(op.control.halt, .{});
    const bytes = assembler.finish();
    const reg = [_]u64{0} ** 256;
    var exe = bytecode.executor.init(reg, 0);
    exe.step(bytes, 20);
}
