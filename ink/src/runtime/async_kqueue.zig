const std = @import("std");
const posix = std.posix;
const c = std.c;

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

const pending_op = struct {
    kind: op_kind,
    fd: posix.fd_t,
    buf: []u8,
    timeout_ns: u64,
    user_data: u64,
};

const key = struct {
    fd: posix.fd_t,
    filter: i16,
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
    kq: posix.fd_t,
    next_id: u32,
    pending: std.AutoHashMap(u32, pending_op),
    queues: std.AutoHashMap(key, op_queue),
    ready: std.ArrayListUnmanaged(completion),

    pub fn init(allocator: std.mem.Allocator) !reactor {
        const kq = try posix.kqueue();
        return .{
            .allocator = allocator,
            .kq = kq,
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
        posix.close(self.kq);
    }

    pub fn submit_read(self: *reactor, fd: posix.fd_t, buf: []u8, user_data: u64) !u32 {
        try set_nonblocking(fd);
        const id = self.alloc_id();
        try self.pending.put(id, .{
            .kind = .read,
            .fd = fd,
            .buf = buf,
            .timeout_ns = 0,
            .user_data = user_data,
        });
        try self.enqueue(.{ .fd = fd, .filter = filter_read }, id);
        return id;
    }

    pub fn submit_write(self: *reactor, fd: posix.fd_t, buf: []u8, user_data: u64) !u32 {
        try set_nonblocking(fd);
        const id = self.alloc_id();
        try self.pending.put(id, .{
            .kind = .write,
            .fd = fd,
            .buf = buf,
            .timeout_ns = 0,
            .user_data = user_data,
        });
        try self.enqueue(.{ .fd = fd, .filter = filter_write }, id);
        return id;
    }

    pub fn submit_accept(self: *reactor, fd: posix.fd_t, user_data: u64) !u32 {
        try set_nonblocking(fd);
        const id = self.alloc_id();
        try self.pending.put(id, .{
            .kind = .accept,
            .fd = fd,
            .buf = &[_]u8{},
            .timeout_ns = 0,
            .user_data = user_data,
        });
        try self.enqueue(.{ .fd = fd, .filter = filter_read }, id);
        return id;
    }

    pub fn submit_timer(self: *reactor, timeout_ns: u64, user_data: u64) !u32 {
        const id = self.alloc_id();
        try self.pending.put(id, .{
            .kind = .timer,
            .fd = -1,
            .buf = &[_]u8{},
            .timeout_ns = timeout_ns,
            .user_data = user_data,
        });
        const ms = ns_to_ms(timeout_ns);
        const change = posix.Kevent{
            .ident = @intCast(id),
            .filter = filter_timer,
            .flags = c.EV.ADD | c.EV.ONESHOT,
            .fflags = 0,
            .data = @intCast(ms),
            .udata = @intCast(id),
        };
        var changes = [1]posix.Kevent{change};
        _ = try posix.kevent(self.kq, changes[0..], &[_]posix.Kevent{}, null);
        return id;
    }

    pub fn cancel(self: *reactor, op_id: u32) bool {
        const entry = self.pending.fetchRemove(op_id) orelse return false;
        const op = entry.value;
        switch (op.kind) {
            .timer => {
                const change = posix.Kevent{
                    .ident = @intCast(op_id),
                    .filter = filter_timer,
                    .flags = c.EV.DELETE,
                    .fflags = 0,
                    .data = 0,
                    .udata = 0,
                };
                var changes = [1]posix.Kevent{change};
                _ = posix.kevent(self.kq, changes[0..], &[_]posix.Kevent{}, null) catch {};
            },
            else => {
                const map_key: key = .{ .fd = op.fd, .filter = filter_for(op.kind) };
                if (self.queues.getPtr(map_key)) |queue| {
                    _ = queue.remove(op_id);
                    if (queue.len() == 0) {
                        self.unregister_key(map_key);
                        queue.deinit(self.allocator);
                        _ = self.queues.remove(map_key);
                    }
                }
            },
        }
        return true;
    }

    pub fn poll(self: *reactor, timeout_ns: ?u64) ![]const completion {
        self.ready.clearRetainingCapacity();
        var events: [128]posix.Kevent = undefined;
        var timeout_spec: posix.timespec = undefined;
        const timeout_ptr = if (timeout_ns) |ns| blk: {
            timeout_spec = ns_to_timespec(ns);
            break :blk &timeout_spec;
        } else null;
        const count = try posix.kevent(self.kq, &[_]posix.Kevent{}, events[0..], timeout_ptr);
        for (events[0..count]) |event| {
            try self.handle_event(event);
        }
        return self.ready.items;
    }

    fn handle_event(self: *reactor, event: posix.Kevent) !void {
        if (event.filter == filter_timer) {
            const id: u32 = @intCast(event.ident);
            const entry = self.pending.fetchRemove(id) orelse return;
            try self.ready.append(self.allocator, .{
                .id = id,
                .kind = entry.value.kind,
                .result = 0,
                .err = null,
                .user_data = entry.value.user_data,
            });
            return;
        }

        if (event.filter != filter_read and event.filter != filter_write) return;

        const fd: posix.fd_t = @intCast(event.ident);
        const map_key: key = .{ .fd = fd, .filter = event.filter };
        const queue = self.queues.getPtr(map_key) orelse return;
        const op_id = queue.pop() orelse return;
        const op = self.pending.get(op_id) orelse return;
        const completed = try self.try_complete(op_id, op);
        if (!completed) {
            try queue.push(self.allocator, op_id);
            return;
        }
        _ = self.pending.remove(op_id);
        if (queue.len() == 0) {
            self.unregister_key(map_key);
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
            try self.register_key(map_key);
        }
        try entry.value_ptr.push(self.allocator, op_id);
    }

    fn register_key(self: *reactor, map_key: key) !void {
        const change = posix.Kevent{
            .ident = @intCast(map_key.fd),
            .filter = map_key.filter,
            .flags = c.EV.ADD | c.EV.CLEAR,
            .fflags = 0,
            .data = 0,
            .udata = 0,
        };
        var changes = [1]posix.Kevent{change};
        _ = try posix.kevent(self.kq, changes[0..], &[_]posix.Kevent{}, null);
    }

    fn unregister_key(self: *reactor, map_key: key) void {
        const change = posix.Kevent{
            .ident = @intCast(map_key.fd),
            .filter = map_key.filter,
            .flags = c.EV.DELETE,
            .fflags = 0,
            .data = 0,
            .udata = 0,
        };
        var changes = [1]posix.Kevent{change};
        _ = posix.kevent(self.kq, changes[0..], &[_]posix.Kevent{}, null) catch {};
    }

    fn alloc_id(self: *reactor) u32 {
        const id = self.next_id;
        self.next_id +%= 1;
        if (self.next_id == 0) self.next_id = 1;
        return id;
    }
};

const filter_read: i16 = @intCast(c.EVFILT.READ);
const filter_write: i16 = @intCast(c.EVFILT.WRITE);
const filter_timer: i16 = @intCast(c.EVFILT.TIMER);

fn filter_for(kind: op_kind) i16 {
    return switch (kind) {
        .read, .accept => filter_read,
        .write => filter_write,
        .timer => filter_timer,
    };
}

fn set_nonblocking(fd: posix.fd_t) !void {
    const flags = try posix.fcntl(fd, posix.F.GETFL, 0);
    const nonblock = @as(usize, 1) << @bitOffsetOf(posix.O, "NONBLOCK");
    _ = try posix.fcntl(fd, posix.F.SETFL, flags | nonblock);
}

fn ns_to_ms(ns: u64) u64 {
    if (ns == 0) return 0;
    return (ns + 999_999) / 1_000_000;
}

    fn ns_to_timespec(ns: u64) posix.timespec {
        return .{
            .sec = @intCast(ns / 1_000_000_000),
            .nsec = @intCast(ns % 1_000_000_000),
        };
    }
