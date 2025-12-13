pub const mem_allocator = @import("std").mem.Allocator;
pub const instruction = @import("root").vm.instruction;
pub const accessors = @import("root").vm.accessors;
pub const array_list = @import("std").array_list.Managed;
pub const assembler = @import("root").vm.assembler;

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

        pub fn init(allocator: mem_allocator, bytecode: anytype, program: []const u8, constants: []value) machine {
            const memory = tape.init(1024 * 1024);
            
            const instruction = memory.allocate(u8, bytecode.len);

            const stack_size = 8192;
            const stack = memory.allocate(u64, stack_size);

            const trap = unreachable;//TODO; 

            return machine {
                .memory = memory,
                .instruction = instruction,
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

