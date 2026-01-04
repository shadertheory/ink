pub const std = @import("std");
pub const mem_allocator = @import("std").mem.Allocator;
pub const instruction = @import("./exe.zig").instruction;
pub const array_list = @import("std").array_list.Managed;
pub const bytecode = @import("./assembly.zig").bytecode;
const runtime = @import("../runtime/scheduler.zig");
const inkb = @import("inkb.zig");

pub const machine =
    struct {
        const self = @This();
        const executor = bytecode.executor;

        allocator: mem_allocator,
        memory: *tape,

        processor: executor,
        code: []const u8,
        constants: []const u64,
        data: []const inkb.data_entry,

        pub fn init(
            allocator: mem_allocator,
            program: []const u8,
            constants: []const u64,
            data: []const inkb.data_entry,
            foreigns: []const []const u8,
            scheduler: ?*runtime.scheduler,
            lib_dir: ?[]const u8,
        ) machine {
            const state = executor.state{
                .pc = 0,
                .fp = 0,
                .sp = 1,
            };

            var instance = machine{
                .processor = undefined,
                .allocator = allocator,
                .memory = allocator.create(tape) catch unreachable,
                .code = program,
                .constants = constants,
                .data = data,
            };
            instance.memory.* = tape.init(allocator, 1024 * 1024);
            instance.processor = executor.init(state, instance.memory, constants, data, foreigns, allocator, scheduler, lib_dir, program);
            return instance;
        }

        pub fn step(this: *machine, budget: usize) *executor.state {
            return this.processor.step(this.code, budget);
        }

        pub fn deinit(this: *machine) void {
            this.processor.deinit();
            this.allocator.free(this.memory.data);
            this.allocator.destroy(this.memory);
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
