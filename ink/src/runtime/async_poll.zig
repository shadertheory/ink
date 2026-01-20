const std = @import("std");
const posix = std.posix;
const common = @import("async_common.zig");

pub const op_kind = common.op_kind;
pub const completion = common.completion;

const pending_op = struct {
    kind: op_kind,
    fd: posix.fd_t,
    buf: []u8,
    deadline_ns: u64,
    user_data: u64,
};

const key = struct {
    fd: posix.fd_t,
    events: i16,
};

const op_queue = struct {
    ids: std.ArrayListUnmanaged(u32) = .{},

    fn deinit(self: *op_queue, allocator: std.mem.Allocator) void {
        self.ids.deinit(allocator);
    }

    fn push(self: *op_queue, allocator: std.mem.Allocator, id: u32) !void {
        try self.ids.append(allocator, id);
    }

    fn pop(self: *op_queue) ?u32 {
        if (self.ids.items.len == 0) return null;
        return self.ids.orderedRemove(0);
    }

    fn remove(self: *op_queue, id: u32) bool {
        const idx = std.mem.indexOfScalar(u32, self.ids.items, id) orelse return false;
        _ = self.ids.swapRemove(idx);
        return true;
    }

    fn len(self: *op_queue) usize {
        return self.ids.items.len;
    }
};

pub const reactor = struct {
    allocator: std.mem.Allocator,
    next_id: u32,
    pending: std.AutoHashMap(u32, pending_op),
    queues: std.AutoHashMap(key, op_queue),
    ready: std.ArrayListUnmanaged(completion),

    pub fn init(allocator: std.mem.Allocator) !reactor {
        return .{
            .allocator = allocator,
            .next_id = 1,
            .pending = std.AutoHashMap(u32, pending_op).init(allocator),
            .queues = std.AutoHashMap(key, op_queue).init(allocator),
            .ready = .{},
        };
    }

    pub fn deinit(self: *reactor) void {
        var it = self.queues.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
        }
        self.queues.deinit();
        self.pending.deinit();
        self.ready.deinit(self.allocator);
    }

    pub fn submit_read(self: *reactor, fd: posix.fd_t, buf: []u8, user_data: u64) !u32 {
        try set_nonblocking(fd);
        const id = self.alloc_id();
        try self.pending.put(id, .{
            .kind = .read,
            .fd = fd,
            .buf = buf,
            .deadline_ns = 0,
            .user_data = user_data,
        });
        try self.enqueue(.{ .fd = fd, .events = posix.POLL.IN }, id);
        return id;
    }

    pub fn submit_write(self: *reactor, fd: posix.fd_t, buf: []u8, user_data: u64) !u32 {
        try set_nonblocking(fd);
        const id = self.alloc_id();
        try self.pending.put(id, .{
            .kind = .write,
            .fd = fd,
            .buf = buf,
            .deadline_ns = 0,
            .user_data = user_data,
        });
        try self.enqueue(.{ .fd = fd, .events = posix.POLL.OUT }, id);
        return id;
    }

    pub fn submit_accept(self: *reactor, fd: posix.fd_t, user_data: u64) !u32 {
        try set_nonblocking(fd);
        const id = self.alloc_id();
        try self.pending.put(id, .{
            .kind = .accept,
            .fd = fd,
            .buf = &[_]u8{},
            .deadline_ns = 0,
            .user_data = user_data,
        });
        try self.enqueue(.{ .fd = fd, .events = posix.POLL.IN }, id);
        return id;
    }

    pub fn submit_timer(self: *reactor, timeout_ns: u64, user_data: u64) !u32 {
        const id = self.alloc_id();
        const now = monotonic_now_ns();
        try self.pending.put(id, .{
            .kind = .timer,
            .fd = -1,
            .buf = &[_]u8{},
            .deadline_ns = now + timeout_ns,
            .user_data = user_data,
        });
        return id;
    }

    pub fn cancel(self: *reactor, op_id: u32) bool {
        const entry = self.pending.fetchRemove(op_id) orelse return false;
        const op = entry.value;
        if (op.kind == .timer) return true;
        const map_key: key = .{ .fd = op.fd, .events = poll_events_for(op.kind) };
        if (self.queues.getPtr(map_key)) |queue| {
            _ = queue.remove(op_id);
            if (queue.len() == 0) {
                queue.deinit(self.allocator);
                _ = self.queues.remove(map_key);
            }
        }
        return true;
    }

    pub fn poll(self: *reactor, timeout_ns: ?u64) ![]const completion {
        self.ready.clearRetainingCapacity();
        const now = monotonic_now_ns();
        const next_timer = self.next_timer_deadline(now);

        var wait_ns: ?u64 = null;
        if (timeout_ns) |req| wait_ns = req;
        if (next_timer) |deadline| {
            if (deadline <= now) {
                try self.complete_timers(now);
                return self.ready.items;
            }
            const until = deadline - now;
            wait_ns = if (wait_ns) |cur| @min(cur, until) else until;
        }

        const poll_ms = if (wait_ns) |ns| ns_to_ms(ns) else -1;

        var poll_fds = std.ArrayListUnmanaged(posix.pollfd){};
        defer poll_fds.deinit(self.allocator);
        try poll_fds.ensureTotalCapacity(self.allocator, self.queues.count());
        var it = self.queues.iterator();
        while (it.next()) |entry| {
            poll_fds.appendAssumeCapacity(.{
                .fd = entry.key_ptr.fd,
                .events = @intCast(entry.key_ptr.events),
                .revents = 0,
            });
        }

        if (poll_fds.items.len == 0) {
            if (wait_ns) |ns| {
                if (ns > 0) std.time.sleep(ns);
            }
        } else {
            _ = posix.poll(poll_fds.items, poll_ms) catch {};
        }

        for (poll_fds.items) |pfd| {
            if (pfd.revents == 0) continue;
            const map_key: key = .{ .fd = pfd.fd, .events = @intCast(pfd.events) };
            try self.handle_ready(map_key);
        }

        try self.complete_timers(monotonic_now_ns());
        return self.ready.items;
    }

    fn handle_ready(self: *reactor, map_key: key) !void {
        const queue = self.queues.getPtr(map_key) orelse return;
        while (queue.len() > 0) {
            const op_id = queue.pop() orelse break;
            const op = self.pending.get(op_id) orelse continue;
            const completed = try self.try_complete(op_id, op);
            if (!completed) {
                try queue.push(self.allocator, op_id);
                break;
            }
            _ = self.pending.remove(op_id);
        }
        if (queue.len() == 0) {
            queue.deinit(self.allocator);
            _ = self.queues.remove(map_key);
        }
    }

    fn try_complete(self: *reactor, op_id: u32, op: pending_op) !bool {
        switch (op.kind) {
            .read => {
                const res = posix.read(op.fd, op.buf) catch |err| {
                    if (err == error.WouldBlock or err == error.TryAgain) return false;
                    try self.ready.append(self.allocator, .{
                        .id = op_id,
                        .kind = op.kind,
                        .result = -1,
                        .err = err,
                        .user_data = op.user_data,
                    });
                    return true;
                };
                try self.ready.append(self.allocator, .{
                    .id = op_id,
                    .kind = op.kind,
                    .result = @intCast(res),
                    .err = null,
                    .user_data = op.user_data,
                });
                return true;
            },
            .write => {
                const res = posix.write(op.fd, op.buf) catch |err| {
                    if (err == error.WouldBlock or err == error.TryAgain) return false;
                    try self.ready.append(self.allocator, .{
                        .id = op_id,
                        .kind = op.kind,
                        .result = -1,
                        .err = err,
                        .user_data = op.user_data,
                    });
                    return true;
                };
                try self.ready.append(self.allocator, .{
                    .id = op_id,
                    .kind = op.kind,
                    .result = @intCast(res),
                    .err = null,
                    .user_data = op.user_data,
                });
                return true;
            },
            .accept => {
                const sock = posix.accept(op.fd, null, null, posix.SOCK.NONBLOCK) catch |err| {
                    if (err == error.WouldBlock or err == error.TryAgain) return false;
                    try self.ready.append(self.allocator, .{
                        .id = op_id,
                        .kind = op.kind,
                        .result = -1,
                        .err = err,
                        .user_data = op.user_data,
                    });
                    return true;
                };
                try self.ready.append(self.allocator, .{
                    .id = op_id,
                    .kind = op.kind,
                    .result = @intCast(sock),
                    .err = null,
                    .user_data = op.user_data,
                });
                return true;
            },
            .timer => return false,
        }
    }

    fn enqueue(self: *reactor, map_key: key, op_id: u32) !void {
        var entry = try self.queues.getOrPut(map_key);
        if (!entry.found_existing) {
            entry.value_ptr.* = .{};
        }
        try entry.value_ptr.push(self.allocator, op_id);
    }

    fn alloc_id(self: *reactor) u32 {
        const id = self.next_id;
        self.next_id +%= 1;
        if (self.next_id == 0) self.next_id = 1;
        return id;
    }

    fn next_timer_deadline(self: *reactor, now: u64) ?u64 {
        var it = self.pending.iterator();
        var best: ?u64 = null;
        while (it.next()) |entry| {
            if (entry.value_ptr.kind != .timer) continue;
            const deadline = entry.value_ptr.deadline_ns;
            if (deadline <= now) return deadline;
            best = if (best) |b| @min(b, deadline) else deadline;
        }
        return best;
    }

    fn complete_timers(self: *reactor, now: u64) !void {
        var expired = std.ArrayListUnmanaged(u32){};
        defer expired.deinit(self.allocator);
        var it = self.pending.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.kind != .timer) continue;
            if (entry.value_ptr.deadline_ns <= now) {
                try expired.append(self.allocator, entry.key_ptr.*);
            }
        }
        for (expired.items) |op_id| {
            const entry = self.pending.fetchRemove(op_id) orelse continue;
            try self.ready.append(self.allocator, .{
                .id = op_id,
                .kind = .timer,
                .result = 0,
                .err = null,
                .user_data = entry.value.user_data,
            });
        }
    }
};

fn poll_events_for(kind: op_kind) i16 {
    return switch (kind) {
        .read, .accept => posix.POLL.IN,
        .write => posix.POLL.OUT,
        .timer => posix.POLL.IN,
    };
}

fn set_nonblocking(fd: posix.fd_t) !void {
    const flags = try posix.fcntl(fd, posix.F.GETFL, 0);
    const nonblock = @as(usize, 1) << @bitOffsetOf(posix.O, "NONBLOCK");
    _ = try posix.fcntl(fd, posix.F.SETFL, flags | nonblock);
}

fn monotonic_now_ns() u64 {
    const now: i128 = std.time.nanoTimestamp();
    if (now <= 0) return 0;
    const max_u64: i128 = @intCast(std.math.maxInt(u64));
    if (now > max_u64) return std.math.maxInt(u64);
    return @intCast(now);
}

fn ns_to_ms(ns: u64) i32 {
    if (ns == 0) return 0;
    const ms = (ns + 999_999) / 1_000_000;
    if (ms > std.math.maxInt(i32)) return std.math.maxInt(i32);
    return @intCast(ms);
}
