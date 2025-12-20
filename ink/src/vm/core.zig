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

        pub fn init(allocator: mem_allocator, program: []const instruction, constants: []u8) machine {
            _ = constants;
            const memory = tape.init(allocator, 8192 * 1024);

            const assembler = bytecode.assembler.init(allocator);
            for (program) |_| {}
            const program_bytecode = assembler.finish();

            const inst = memory.allocate(u8, program_bytecode);

            const stack_size = 8192;

            const sp = memory.allocate(u64, [_]u64{0} ** stack_size);

            return machine{
                .processor = executor.init(.{
                    .sp = sp,
                    .fp = sp,
                    .pc = inst,
                }),
                .memory = memory,
            };
        }

        pub const index = struct { value: u64 };

        pub const register = struct { value: index };

        pub const tape = struct {
            pub const word = u64;
            data: []word,
            head: usize,

            pub fn init(allocator: mem_allocator, byte_count: usize) tape {
                return tape{
                    .data = allocator.alloc(u8, byte_count / @sizeOf(word)),
                };
            }

            pub fn allocate(this: *tape, comptime ty: type, data: []ty) tape.index {
                return this.push(&this.head, ty, data);
            }

            pub fn push(this: *tape, head: *usize, comptime ty: type, data: []ty) tape.index {
                const byte_payload: []u8 = @ptrCast(data.ptr);
                const byte_data: []u8 = @ptrCast(this.data.ptr);
                const align_req = @alignOf(ty) - @rem(head, @alignOf(ty));
                head += align_req;
                const ret = tape.index{ .where = head };
                std.mem.copyForwards(u8, byte_data[head..][0..byte_payload.len], byte_payload);
                head += byte_payload.len;
                return ret;
            }

            pub const address = struct { where: index };
        };
    };
