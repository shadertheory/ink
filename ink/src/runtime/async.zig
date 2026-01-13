const builtin = @import("builtin");
const std = @import("std");
const common = @import("async_common.zig");
const fd = @import("fd.zig");
const sim = @import("async_sim.zig");

const real_backend = switch (builtin.os.tag) {
    .macos, .ios, .tvos, .watchos, .visionos => @import("async_kqueue.zig"),
    .linux => @import("async_uring.zig"),
    else => @import("async_stub.zig"),
};

pub const op_kind = common.op_kind;
pub const completion = common.completion;
pub const fd_kind = fd.fd_kind;
pub const fd_info = fd.fd_info;
pub const SimReport = sim.Report;

pub const Mode = enum {
    real,
    sim,
};

pub const Config = struct {
    mode: Mode = .real,
    sim: ?sim.Config = null,
};

pub const reactor = struct {
    allocator: std.mem.Allocator,
    mode: Mode,
    real: ?real_backend.reactor,
    sim: ?sim.reactor,

    pub fn init(allocator: std.mem.Allocator, config: Config) !reactor {
        return switch (config.mode) {
            .real => .{
                .allocator = allocator,
                .mode = .real,
                .real = try real_backend.reactor.init(allocator),
                .sim = null,
            },
            .sim => blk: {
                const sim_cfg = config.sim orelse return error.InvalidConfig;
                break :blk .{
                    .allocator = allocator,
                    .mode = .sim,
                    .real = null,
                    .sim = try sim.reactor.init(allocator, sim_cfg),
                };
            },
        };
    }

    pub fn deinit(self: *reactor) void {
        switch (self.mode) {
            .real => if (self.real) |*real_rt| real_rt.deinit(),
            .sim => if (self.sim) |*sim_rt| sim_rt.deinit(),
        }
    }

    pub fn submit_read(self: *reactor, fd_value: u64, buf: []u8, user_data: u64) !u32 {
        return switch (self.mode) {
            .real => self.real.?.submit_read(fd.decode(fd_value).raw, buf, user_data),
            .sim => self.sim.?.submit_read(fd_value, buf, user_data),
        };
    }

    pub fn submit_write(self: *reactor, fd_value: u64, buf: []u8, user_data: u64) !u32 {
        return switch (self.mode) {
            .real => self.real.?.submit_write(fd.decode(fd_value).raw, buf, user_data),
            .sim => self.sim.?.submit_write(fd_value, buf, user_data),
        };
    }

    pub fn submit_accept(self: *reactor, fd_value: u64, user_data: u64) !u32 {
        return switch (self.mode) {
            .real => self.real.?.submit_accept(fd.decode(fd_value).raw, user_data),
            .sim => self.sim.?.submit_accept(fd_value, user_data),
        };
    }

    pub fn submit_timer(self: *reactor, timeout_ns: u64, user_data: u64) !u32 {
        return switch (self.mode) {
            .real => self.real.?.submit_timer(timeout_ns, user_data),
            .sim => self.sim.?.submit_timer(timeout_ns, user_data),
        };
    }

    pub fn cancel(self: *reactor, op_id: u32) bool {
        return switch (self.mode) {
            .real => self.real.?.cancel(op_id),
            .sim => self.sim.?.cancel(op_id),
        };
    }

    pub fn poll(self: *reactor, timeout_ns: ?u64) ![]const completion {
        return switch (self.mode) {
            .real => self.real.?.poll(timeout_ns),
            .sim => self.sim.?.poll(timeout_ns),
        };
    }

    pub fn now_ns(self: *reactor) u64 {
        return switch (self.mode) {
            .real => monotonic_now_ns(),
            .sim => self.sim.?.now_ns(),
        };
    }

    pub fn report(self: *reactor, allocator: std.mem.Allocator) !?sim.Report {
        return switch (self.mode) {
            .real => null,
            .sim => try self.sim.?.report(allocator),
        };
    }
};

fn monotonic_now_ns() u64 {
    const now: i128 = std.time.nanoTimestamp();
    if (now <= 0) return 0;
    const max_u64: i128 = @intCast(std.math.maxInt(u64));
    if (now > max_u64) return std.math.maxInt(u64);
    return @intCast(now);
}
