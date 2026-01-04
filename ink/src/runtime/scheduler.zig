const std = @import("std");
const async = @import("async.zig");

pub const poll_status = enum(u8) {
    ready,
    pending,
    done,
};

pub const task_id = u32;
pub const task_wait_base: u32 = 0x8000_0000;

pub fn task_wait_op(id: task_id) u32 {
    return task_wait_base | @as(u32, @intCast(id));
}

pub const task = struct {
    context: *anyopaque,
    poll: *const fn (*anyopaque, *scheduler, task_id) poll_status,
    on_cancel: ?*const fn (*anyopaque) void = null,
};

pub const scheduler = struct {
    allocator: std.mem.Allocator,
    reactor: async.reactor,
    next_task_id: task_id,
    tasks: std.AutoHashMap(task_id, task),
    ready: std.ArrayListUnmanaged(task_id),
    waiting: std.AutoHashMap(u32, std.ArrayListUnmanaged(task_id)),
    completions: std.AutoHashMap(u32, async.completion),
    cancelled: std.AutoHashMap(task_id, void),
    task_results: std.AutoHashMap(task_id, u64),
    parents: std.AutoHashMap(task_id, task_id),
    children: std.AutoHashMap(task_id, std.ArrayListUnmanaged(task_id)),
    joining: std.AutoHashMap(task_id, void),

    pub fn init(allocator: std.mem.Allocator) !scheduler {
        return .{
            .allocator = allocator,
            .reactor = try async.reactor.init(allocator),
            .next_task_id = 1,
            .tasks = std.AutoHashMap(task_id, task).init(allocator),
            .ready = .{},
            .waiting = std.AutoHashMap(u32, std.ArrayListUnmanaged(task_id)).init(allocator),
            .completions = std.AutoHashMap(u32, async.completion).init(allocator),
            .cancelled = std.AutoHashMap(task_id, void).init(allocator),
            .task_results = std.AutoHashMap(task_id, u64).init(allocator),
            .parents = std.AutoHashMap(task_id, task_id).init(allocator),
            .children = std.AutoHashMap(task_id, std.ArrayListUnmanaged(task_id)).init(allocator),
            .joining = std.AutoHashMap(task_id, void).init(allocator),
        };
    }

    pub fn deinit(self: *scheduler) void {
        self.reactor.deinit();
        self.tasks.deinit();
        self.ready.deinit(self.allocator);
        var wait_it = self.waiting.iterator();
        while (wait_it.next()) |entry| {
            entry.value_ptr.*.deinit(self.allocator);
        }
        self.waiting.deinit();
        self.completions.deinit();
        self.cancelled.deinit();
        self.task_results.deinit();
        self.parents.deinit();
        var child_it = self.children.iterator();
        while (child_it.next()) |entry| {
            entry.value_ptr.*.deinit(self.allocator);
        }
        self.children.deinit();
        self.joining.deinit();
    }

    pub fn spawn(self: *scheduler, new_task: task) !task_id {
        return self.spawn_child(0, new_task);
    }

    pub fn spawn_child(self: *scheduler, parent: task_id, new_task: task) !task_id {
        const id = self.alloc_id();
        try self.tasks.put(id, new_task);
        try self.ready.append(self.allocator, id);
        if (parent != 0) {
            try self.parents.put(id, parent);
            if (self.children.getPtr(parent)) |list| {
                try list.append(self.allocator, id);
            } else {
                var list: std.ArrayListUnmanaged(task_id) = .{};
                try list.append(self.allocator, id);
                try self.children.put(parent, list);
            }
        }
        return id;
    }

    pub fn cancel(self: *scheduler, id: task_id) void {
        if (self.cancelled.contains(id)) return;
        self.cancelled.put(id, {}) catch return;
        if (self.tasks.get(id)) |entry| {
            if (entry.on_cancel) |handler| handler(entry.context);
        }
        self.cancel_children(id);
    }

    pub fn is_cancelled(self: *scheduler, id: task_id) bool {
        return self.cancelled.contains(id);
    }

    pub fn wait(self: *scheduler, id: task_id, op_id: u32) !void {
        if (self.waiting.getPtr(op_id)) |list| {
            try list.append(self.allocator, id);
        } else {
            var list: std.ArrayListUnmanaged(task_id) = .{};
            try list.append(self.allocator, id);
            try self.waiting.put(op_id, list);
        }
    }

    pub fn cancel_wait(self: *scheduler, id: task_id, op_id: u32) void {
        const list_ptr = self.waiting.getPtr(op_id) orelse return;
        var idx: usize = 0;
        while (idx < list_ptr.items.len) : (idx += 1) {
            if (list_ptr.items[idx] == id) {
                _ = list_ptr.swapRemove(idx);
                if (list_ptr.items.len == 0) {
                    list_ptr.deinit(self.allocator);
                    _ = self.waiting.remove(op_id);
                }
                return;
            }
        }
    }

    pub fn task_result_ready(self: *scheduler, id: task_id) bool {
        return self.task_results.contains(id);
    }

    pub fn task_result(self: *scheduler, id: task_id) ?u64 {
        return self.task_results.get(id);
    }

    pub fn complete_task(self: *scheduler, id: task_id, result: u64) void {
        self.task_results.put(id, result) catch return;
        self.resume_waiters(task_wait_op(id));
    }

    pub fn has_children(self: *scheduler, id: task_id) bool {
        if (self.children.get(id)) |list| return list.items.len > 0;
        return false;
    }

    pub fn begin_join(self: *scheduler, id: task_id) void {
        if (self.joining.contains(id)) return;
        self.joining.put(id, {}) catch return;
        self.cancel_children(id);
        if (!self.has_children(id)) {
            _ = self.joining.remove(id);
            self.ready.append(self.allocator, id) catch {};
        }
    }

    pub fn take_completion(self: *scheduler, op_id: u32) ?async.completion {
        const entry = self.completions.fetchRemove(op_id) orelse return null;
        return entry.value;
    }

    pub fn sleep(self: *scheduler, id: task_id, timeout_ns: u64) !void {
        const op_id = try self.reactor.submit_timer(timeout_ns, @intCast(id));
        try self.wait(id, op_id);
    }

    pub fn run(self: *scheduler) !void {
        while (self.tasks.count() > 0) {
            if (self.ready.items.len == 0) {
                try self.poll_io(null);
                if (self.ready.items.len == 0) continue;
            }

            const id = self.ready.orderedRemove(0);
            const entry = self.tasks.get(id) orelse continue;
            const status = entry.poll(entry.context, self, id);
            switch (status) {
                .ready => try self.ready.append(self.allocator, id),
                .pending => {},
                .done => {
                    self.note_task_done(id);
                    _ = self.tasks.remove(id);
                    _ = self.cancelled.remove(id);
                },
            }
        }
    }

    fn poll_io(self: *scheduler, timeout_ns: ?u64) !void {
        const completed = try self.reactor.poll(timeout_ns);
        for (completed) |item| {
            self.completions.put(item.id, item) catch continue;
            self.resume_waiters(item.id);
        }
    }

    fn resume_waiters(self: *scheduler, op_id: u32) void {
        if (self.waiting.fetchRemove(op_id)) |entry| {
            for (entry.value.items) |task_id_value| {
                self.ready.append(self.allocator, task_id_value) catch {};
            }
            var list = entry.value;
            list.deinit(self.allocator);
        }
    }

    fn cancel_children(self: *scheduler, id: task_id) void {
        const list = self.children.get(id) orelse return;
        for (list.items) |child| {
            self.cancel(child);
        }
    }

    fn note_task_done(self: *scheduler, id: task_id) void {
        if (self.parents.fetchRemove(id)) |entry| {
            const parent = entry.value;
            if (self.children.getPtr(parent)) |list| {
                var idx: usize = 0;
                while (idx < list.items.len) : (idx += 1) {
                    if (list.items[idx] == id) {
                        _ = list.swapRemove(idx);
                        break;
                    }
                }
                if (list.items.len == 0) {
                    list.deinit(self.allocator);
                    _ = self.children.remove(parent);
                    if (self.joining.fetchRemove(parent)) |_| {
                        self.ready.append(self.allocator, parent) catch {};
                    }
                }
            }
        }
    }

    fn alloc_id(self: *scheduler) task_id {
        const id = self.next_task_id;
        self.next_task_id +%= 1;
        if (self.next_task_id == 0) self.next_task_id = 1;
        return id;
    }
};
