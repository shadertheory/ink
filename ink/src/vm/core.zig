pub const std = @import("std");
pub const mem_allocator = @import("std").mem.Allocator;
pub const instruction = @import("./exe.zig").instruction;
pub const array_list = @import("std").array_list.Managed;
pub const bytecode = @import("./assembly.zig").bytecode;

pub const machine = 
     struct {
        const self = @This();

        constant_count: u64,
        trap_count: u64,
        code_count: u64,

        memory: tape,

        instruction: tape.index, 
        frame: tape.index,
        stack: tape.index,
        trap: tape.index,

        pub fn init(allocator: mem_allocator, program: []const instruction, constants: []u8) machine {
            _ = constants;
            const memory = tape.init(1024 * 1024);

            const assembler = bytecode.assembler.init(allocator);
            for (program) |_| {
            }
            const program_bytecode = assembler.finish();
            
            const inst = memory.allocate(u8, program_bytecode);

            const stack_size = 8192;
            const stack = memory.allocate(u64, [_]u64{0} ** stack_size);

            const trap = memory.allocate(u64, [_]u64{0});

            const frame = memory.allocate(u64, [_]u64{0});

            return machine {
                .memory = memory,
                .instruction = inst,
                .frame = frame,
                .stack = stack,
                .trap = trap,
            };
        }

        pub const tape = struct {
            pub const word = u64;
            data: []word,
            head: usize,
        
            pub fn init(allocator: mem_allocator, byte_count: usize) tape {
                return tape {
                    .data = allocator.alloc(u8, byte_count / @sizeOf(word)),
                };
            }

            pub fn allocate(this: *tape, comptime ty: type, data: []ty) tape.index {
                const byte_payload: []u8 = @ptrCast(data.ptr);
                const byte_data: []u8 = @ptrCast(this.data.ptr);
                const align_req = @alignOf(ty) - @rem(this.head, @alignOf(ty));
                this.head += align_req;
                const ret = tape.index { .where = this.head };
                std.mem.copyForwards(u8, byte_data[this.head..][0..byte_payload.len], byte_payload);
                this.head += byte_payload.len;
                return ret;
            }

            pub const index = struct { where: u64 };
        };
    };

