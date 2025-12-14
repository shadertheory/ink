pub const std = @import("std");
pub const op = @import("./op.zig");
pub const mem = std.mem;
pub const mem_sort = mem.sort;
pub const instruction = @import("root").vm.instruction;
pub const array_list = std.array_list.Managed;
pub const mem_allocator = std.mem.Allocator;
pub const int_fitting_range = std.math.IntFittingRange;
pub const struct_field = std.builtin.Type.StructField;
pub const tuple = std.meta.Tuple;
pub const access = @import("./core.zig").machine.fundamental;



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
    jmp_abs: u24,
};
fn struct_field_bit_size_less_than(context: type, lhs: type, rhs: type) bool {
    _ = context;
        return @bitSizeOf(lhs) < @bitSizeOf(rhs);
}

pub fn accessors(comptime vm_type: type) type {
    return struct {
        pub const dst = struct {
            vm: *vm_type,
            index: usize,
            pub fn set(self: dst, value: u64) void { self.vm.registers[self.index] = value; }
            pub fn get(self: dst) u64 { return self.vm.registers[self.index]; }
        };

        pub const src = struct {
            vm: *vm_type,
            index: usize,
            pub fn get(self: src) u64 { return self.vm.registers[self.index]; }
        };

        pub const val = struct {
            raw: u64,
            pub fn get(self: val) u64 { return self.raw; }
        };

        pub const jmp_abs = struct {
            vm: *vm_type,
            target: usize,

            pub fn to(self: jmp_abs) void {
                self.vm.pc = self.target;
            }
        };

        pub const jmp_rel = struct {
            vm: *vm_type,
            offset: isize,

            pub fn skip(self: jmp_rel) void {
                if (self.offset < 0) {
                    const abs_off: usize = @intCast(-self.offset);
                    self.vm.pc -= abs_off;
                } else {
                    const abs_off: usize = @intCast(self.offset);
                    self.vm.pc += abs_off;
                }
            }
        };

        pub const jmp_ind = struct {
            vm: *vm_type,
            reg_index: usize,

            pub fn to(self: jmp_ind) void {
                const target = self.vm.registers[self.reg_index];
                self.vm.pc = @intCast(target);
            }
        };
    };
}

pub fn control_operators(comptime vm: type) type {
    return struct {
        fn no_operation(machine: *vm) void { _ = machine; return; } 
        fn halt(machine: *vm) void { _ = machine; return; } 
    };
}

pub const bytecode = assembly(.{ .control = op.control }, .{ small, standard, wide, huge, imm_val, jmp_loc_abs  }, .{ .control = control_operators });

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
    mem_sort(type, &sorted_formats_fn, void, struct_field_bit_size_less_than);

    const sorted_formats = sorted_formats_fn;
    return struct {
        const executor = struct {
            const self = @This();

            registers: [256]u64,
            pc: usize,

            pub fn init(registers: [256]u64, pc_start: usize) self {
                return . { .registers = registers, .pc = pc_start };
            }

            pub fn run(this: *self, code: []const u8, budget: usize) void {
                const raw = code[this.pc];
                const format_index = raw >> 6;
                const operation = raw & 0x3F;
                var health = budget;

                while(health > 0) {
                    inline for (sorted_formats, 0..) |format, idx| {
                        if (idx == format_index) {
                            const size = @sizeOf(format);
                            const inst = mem.bytesToValue(format, code[this.pc..][0..size]);
                            this.dispatch(operation, inst);
                            this.pc += size;
                            health -= 1;
                            break;
                        }
                    }
               }
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

                                inline for (function_params[1..], 1..) |param, index| {
                                    const param_type = param.type.?;
                                    if (@hasDecl(param_type, "from_instruction")) {
                                        function_arguments[index] = param_type.from_instruction(this, instruction_payload);
                                    } else {
                                        @compileError("parameter type " ++ @typeName(param_type) ++ " in " ++ function_name ++ " missing from_instruction method");
                                    }
                                }

                                @call(.auto, implementation_function, function_arguments);
                                return;
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
                        const format_bits: opcode_type = @intCast(std.math.log2_int_ceil(opcode_type, ((1 << sorted_formats.len) - 1)));
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
                        const bits = @typeInfo(ff.type).@"int".bits;
                        std.debug.print("{u}", .{val});
                        std.debug.print("{u}", .{bits});
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
    try assembler.emit(op.control.no_operation, .{ .jmp_abs =  700000 });
    try assembler.emit(op.control.halt, .{});
    const done = assembler.finish();
    const jmp: *align(1) jmp_loc_abs = std.mem.bytesAsValue(jmp_loc_abs, done[0..@sizeOf(jmp_loc_abs)]);
    std.debug.assert(jmp.jmp_abs == 700000);
}

test "bytecode execute" {
    var assembler = bytecode.assembler.init(std.testing.allocator);
    try assembler.emit(op.control.no_operation, .{ .jmp_abs =  700000 });
    try assembler.emit(op.control.halt, .{});
    const bytes = assembler.finish();
    const reg = [_]u64{0} ** 256;
    var exe = bytecode.executor.init(reg, 0);
    exe.run(bytes, 2);
}
