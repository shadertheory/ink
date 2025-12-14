pub const mem_allocator = @import("std").mem.Allocator;
pub const instruction = @import("./exe.zig").instruction;
pub const accessors = @import("./assembly.zig").accessors;
pub const array_list = @import("std").array_list.Managed;
pub const assembler = @import("./assembly.zig").bytecode.assembler;

pub const machine = 
     struct {
        const self = @This();

        pub const fundamental = accessors(self);

        constant_count: u64,
        trap_count: u64,
        code_count: u64,

        memory: tape,

        instruction: tape.index, 
        frame: tape.index,
        stack: tape.index,
        trap: tape.index,

        pub fn init(allocator: mem_allocator, bytecode: anytype, program: []const u8, constants: []u8) machine {
            _ = program;
            _ = allocator;
            _ = constants;
            const memory = tape.init(1024 * 1024);
            
            const inst = memory.allocate(u8, bytecode.len);

            const stack_size = 8192;
            const stack = memory.allocate(u64, stack_size);

            const trap = memory.allocate(u64, 1);

            const frame = memory.allocate(u64, 1);

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
        
            pub fn init(allocator: mem_allocator, byte_count: usize) tape {
                return tape {
                    .data = allocator.alloc(u8, byte_count / @bitSizeOf(word)),
                };
            }

            pub const index = struct { where: u64 };
        };
    };

