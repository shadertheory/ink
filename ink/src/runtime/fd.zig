const std = @import("std");

pub const fd_kind = enum(u8) {
    unknown = 0,
    fs = 1,
    tcp = 2,
    udp = 3,
};

pub const fd_kind_shift: u6 = 60;
pub const fd_raw_mask: u64 = (@as(u64, 1) << fd_kind_shift) - 1;

pub const fd_info = struct {
    raw: std.posix.fd_t,
    kind: fd_kind,
};

pub fn encode(raw: std.posix.fd_t, kind: fd_kind) u64 {
    const raw_u64: u64 = if (raw < 0) 0 else @intCast(raw);
    return (raw_u64 & fd_raw_mask) | (@as(u64, @intFromEnum(kind)) << fd_kind_shift);
}

pub fn decode(value: u64) fd_info {
    const raw_u64 = value & fd_raw_mask;
    const kind_val: u8 = @intCast(value >> fd_kind_shift);
    const kind: fd_kind = switch (kind_val) {
        @intFromEnum(fd_kind.fs) => .fs,
        @intFromEnum(fd_kind.tcp) => .tcp,
        @intFromEnum(fd_kind.udp) => .udp,
        else => .unknown,
    };
    const raw_fd: std.posix.fd_t = if (raw_u64 > std.math.maxInt(std.posix.fd_t))
        std.math.maxInt(std.posix.fd_t)
    else
        @intCast(raw_u64);
    return .{ .raw = raw_fd, .kind = kind };
}

pub fn strip(value: u64) u64 {
    return value & fd_raw_mask;
}
