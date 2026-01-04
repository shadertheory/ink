const std = @import("std");
const posix = std.posix;

pub const op_kind = enum(u8) {
    read,
    write,
    accept,
    timer,
};

pub const completion = struct {
    id: u32,
    kind: op_kind,
    result: isize,
    err: ?anyerror,
    user_data: u64,
};

pub const reactor = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !reactor {
        _ = allocator;
        return error.Unsupported;
    }

    pub fn deinit(self: *reactor) void {
        _ = self;
    }

    pub fn submit_read(self: *reactor, fd: posix.fd_t, buf: []u8, user_data: u64) !u32 {
        _ = self;
        _ = fd;
        _ = buf;
        _ = user_data;
        return error.Unsupported;
    }

    pub fn submit_write(self: *reactor, fd: posix.fd_t, buf: []u8, user_data: u64) !u32 {
        _ = self;
        _ = fd;
        _ = buf;
        _ = user_data;
        return error.Unsupported;
    }

    pub fn submit_accept(self: *reactor, fd: posix.fd_t, user_data: u64) !u32 {
        _ = self;
        _ = fd;
        _ = user_data;
        return error.Unsupported;
    }

    pub fn submit_timer(self: *reactor, timeout_ns: u64, user_data: u64) !u32 {
        _ = self;
        _ = timeout_ns;
        _ = user_data;
        return error.Unsupported;
    }

    pub fn cancel(self: *reactor, op_id: u32) bool {
        _ = self;
        _ = op_id;
        return false;
    }

    pub fn poll(self: *reactor, timeout_ns: ?u64) ![]const completion {
        _ = self;
        _ = timeout_ns;
        return error.Unsupported;
    }
};
