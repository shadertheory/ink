const builtin = @import("builtin");

pub const backend = switch (builtin.os.tag) {
    .macos, .ios, .tvos, .watchos, .visionos => @import("async_kqueue.zig"),
    .linux => @import("async_uring.zig"),
    else => @import("async_stub.zig"),
};

pub const reactor = backend.reactor;
pub const op_kind = backend.op_kind;
pub const completion = backend.completion;
