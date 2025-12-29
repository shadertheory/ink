pub const std = @import("std");
pub const mem_allocator = @import("std").mem.Allocator;
pub const instruction = @import("./exe.zig").instruction;
pub const array_list = @import("std").array_list.Managed;
pub const bytecode = @import("./assembly.zig").bytecode;

pub const machine =
    struct {
        const self = @This();
        const executor = bytecode.executor;

        memory: tape,

        processor: executor,
        code: []const u8,
        constants: []const u64,

        pub fn init(allocator: mem_allocator, program: []const u8, constants: []const u64) machine {
            var memory = tape.init(allocator, 1024 * 1024);
            const state = executor.state{
                .pc = 0,
                .fp = 0,
                .sp = 1,
            };

            return machine{
                .processor = executor.init(state, &memory, constants),
                .memory = memory,
                .code = program,
                .constants = constants,
            };
        }

        pub fn step(this: *machine, budget: usize) *executor.state {
            return this.processor.step(this.code, budget);
        }

        pub const index = usize;

        pub const register = struct { value: index };
        pub const word = u64;

        pub const tape = struct {
            pub const index = usize;
            data: []u64,
            head: usize,

            pub fn init(allocator: mem_allocator, word_count: usize) tape {
                return tape{
                    .data = allocator.alloc(u64, word_count) catch unreachable,
                    .head = 0,
                };
            }

            pub fn access(this: *tape, slot: usize) *u64 {
                return &this.data[slot];
            }

            pub fn read(this: *tape, slot: usize) u64 {
                return this.data[slot];
            }

            pub fn write(this: *tape, slot: usize, value: u64) void {
                this.data[slot] = value;
            }

            pub const address = struct { where: @This().index };
        };
    };
