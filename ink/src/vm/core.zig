pub const mem_allocator = @import("std").mem.Allocator;
pub const instruction = @import("root").vm.instruction;

u
pub fn handle(comptime ty: type) type {
    return struct { target: type = ty };
}

pub const machine = struct { 
    pub const header = struct {
        constant_count: u64,
        trap_count: u64,
        instruction_count: u64,
    };
    pub const control_flow = enum { .cont, .halt };
    pub const tape = struct {
        pub const word = packed union {
            raw: u64,

            half: packed struct {
                alpha: single,
                bravo: single,
            },

            full: double,
        };

        data: []word,
    
        pub fn init(allocator: mem_allocator, byte_count: usize) tape {
            return tape {
                .data = allocator.alloc(u8, byte_count / @bitSizeOf(word)),
            };
        }

        pub const index = struct { where: u64 };
    };
    pub const single = packed struct { 
        operation: u8, 
        alpha: register,
        bravo: register,
        info: packed union { 
            sub_operation: u8,
            immediete: u8,
            charlie: register,
        },
    };
    pub const double = packed struct {
        operation: u8,
        alpha: register,
        immediete: u48, 
    };
    memory: tape,
    def: header,

    instruction: tape.index, 
    frame: tape.index,
    stack: tape.index,
    trap: tape.index,
    pub fn init(allocator: mem_allocator, program_instructions: []instruction, constants: []value) machine {
        const memory = tape.init(1024 * 1024);

        const instruction_code = pack_code(program_instructions);
        const program_index = tape.allocate(code, program.len);
        const instruction = program_index.view(program).function("main");
        const frame = instruction;

        const stack_size = 8192;
        const stack = tape.allocate(u64, stack_size);

        const trap = linked_list(trap_condition).setup(&tape);

        const frames = call_stack.init(allocator); 

        return machine {
            .frames = frames,
            .memory = memory,
            .instruction = instruction,
            .frame = frame,
            .stack = stack,
            .trap = trap,
        };
    }

    pub fn step(self: *machine) control_flow {
        self.instruct() catch |trap_condition| {
            self.trap.add(@intFromEnum(trap_condition)).write(value, .{.boolean = true});
            return .halt;
        };
    } 

    fn pack_code(run: []instruction) []code {

    }

    fn instruct(self: *machine) !control_flow {
        const code = self.tape.read(code, self.instruction);
        self.instruction.advance();

         

        return .cont;
    }


    pub const register = struct {
        which: u8,
    };
};

