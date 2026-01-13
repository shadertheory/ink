const std = @import("std");
const async_common = @import("async_common.zig");

pub const op_kind = async_common.op_kind;
const op_kind_count: usize = @typeInfo(op_kind).@"enum".fields.len;

pub const DebugFunction = struct {
    entry_pc: usize,
    name: []const u8,
};

pub const CallNode = struct {
    label: []const u8,
    count: u64,
    children: std.StringHashMap(*CallNode),
    io_counts: [op_kind_count]u64,
};

pub const TaskNode = struct {
    label: []const u8,
    count: u64,
    children: std.StringHashMap(*TaskNode),
    call_root: ?*CallNode,
};

pub const ProgramTree = struct {
    root: ?*TaskNode,

    pub fn deinit(self: *ProgramTree, allocator: std.mem.Allocator) void {
        if (self.root) |node| {
            free_task_node(allocator, node);
            self.root = null;
        }
    }
};

pub const ProgramTrace = struct {
    allocator: std.mem.Allocator,
    tree: ProgramTree,
    task_states: std.AutoHashMap(u32, TaskState),
    fn_map: std.AutoHashMap(usize, []const u8),
    entry_name: []const u8,
    finished: bool = false,

    pub fn init(allocator: std.mem.Allocator, functions: []const DebugFunction) !ProgramTrace {
        var fn_map = std.AutoHashMap(usize, []const u8).init(allocator);
        errdefer fn_map.deinit();
        const entry_name: []const u8 = "entry";
        for (functions) |func| {
            try fn_map.put(func.entry_pc, func.name);
        }
        const root = try make_task_node(allocator, "<root>", false);
        return .{
            .allocator = allocator,
            .tree = .{ .root = root },
            .task_states = std.AutoHashMap(u32, TaskState).init(allocator),
            .fn_map = fn_map,
            .entry_name = entry_name,
            .finished = false,
        };
    }

    pub fn deinit(self: *ProgramTrace) void {
        if (self.finished) return;
        self.cleanup_task_states();
        self.fn_map.deinit();
        self.tree.deinit(self.allocator);
        self.finished = true;
    }

    pub fn finish(self: *ProgramTrace) ProgramTree {
        self.cleanup_task_states();
        self.fn_map.deinit();
        const tree = self.tree;
        self.tree.root = null;
        self.finished = true;
        return tree;
    }

    pub fn note_task_start(self: *ProgramTrace, task_id: u32, parent_id: u32, entry_pc: usize) void {
        const root = self.tree.root orelse return;
        const name = self.lookup_name(entry_pc);
        var parent_node = root;
        if (parent_id != 0) {
            if (self.task_states.get(parent_id)) |state| {
                parent_node = state.node;
            }
        }

        const task_node = self.get_or_create_task(parent_node, name) catch return;
        task_node.count += 1;
        const call_root = task_node.call_root orelse return;
        call_root.count += 1;

        var stack: std.ArrayListUnmanaged(*CallNode) = .{};
        stack.append(self.allocator, call_root) catch {};

        if (self.task_states.fetchRemove(task_id)) |entry| {
            var state = entry.value;
            state.stack.deinit(self.allocator);
        }
        self.task_states.put(task_id, .{ .node = task_node, .stack = stack }) catch {
            stack.deinit(self.allocator);
        };
    }

    pub fn note_call(self: *ProgramTrace, task_id: u32, target_pc: usize) void {
        const state = self.task_states.getPtr(task_id) orelse return;
        const parent = if (state.stack.items.len == 0)
            (state.node.call_root orelse return)
        else
            state.stack.items[state.stack.items.len - 1];
        const name = self.lookup_name(target_pc);
        const child = self.get_or_create_call(parent, name) catch return;
        child.count += 1;
        state.stack.append(self.allocator, child) catch {};
    }

    pub fn note_tail_call(self: *ProgramTrace, task_id: u32, target_pc: usize) void {
        const state = self.task_states.getPtr(task_id) orelse return;
        if (state.stack.items.len != 0) {
            _ = state.stack.pop();
        }
        self.note_call(task_id, target_pc);
    }

    pub fn note_return(self: *ProgramTrace, task_id: u32) void {
        const state = self.task_states.getPtr(task_id) orelse return;
        if (state.stack.items.len == 0) return;
        _ = state.stack.pop();
        if (state.stack.items.len == 0) {
            state.stack.deinit(self.allocator);
            _ = self.task_states.remove(task_id);
        }
    }

    pub fn note_io(self: *ProgramTrace, task_id: u32, kind: op_kind) void {
        const state = self.task_states.getPtr(task_id) orelse return;
        if (state.stack.items.len == 0) return;
        const current = state.stack.items[state.stack.items.len - 1];
        current.io_counts[@intFromEnum(kind)] += 1;
    }

    fn cleanup_task_states(self: *ProgramTrace) void {
        var it = self.task_states.iterator();
        while (it.next()) |entry| {
            const state = @constCast(entry.value_ptr);
            state.stack.deinit(self.allocator);
        }
        self.task_states.deinit();
    }

    fn lookup_name(self: *ProgramTrace, entry_pc: usize) []const u8 {
        if (self.fn_map.get(entry_pc)) |name| return name;
        if (entry_pc == 0) return self.entry_name;
        return "<unknown>";
    }

    fn get_or_create_task(self: *ProgramTrace, parent: *TaskNode, name: []const u8) !*TaskNode {
        if (parent.children.get(name)) |node| return node;
        const task_node = try make_task_node(self.allocator, name, true);
        try parent.children.put(task_node.label, task_node);
        return task_node;
    }

    fn get_or_create_call(self: *ProgramTrace, parent: *CallNode, name: []const u8) !*CallNode {
        if (parent.children.get(name)) |node| return node;
        const call_node = try make_call_node(self.allocator, name);
        try parent.children.put(call_node.label, call_node);
        return call_node;
    }
};

pub fn make_task_node(
    allocator: std.mem.Allocator,
    label: []const u8,
    with_call_root: bool,
) !*TaskNode {
    const node = try allocator.create(TaskNode);
    errdefer allocator.destroy(node);
    const duped = try allocator.dupe(u8, label);
    errdefer allocator.free(duped);
    node.* = .{
        .label = duped,
        .count = 0,
        .children = std.StringHashMap(*TaskNode).init(allocator),
        .call_root = null,
    };
    if (with_call_root) {
        node.call_root = try make_call_node(allocator, label);
    }
    return node;
}

pub fn make_call_node(allocator: std.mem.Allocator, label: []const u8) !*CallNode {
    const node = try allocator.create(CallNode);
    errdefer allocator.destroy(node);
    const duped = try allocator.dupe(u8, label);
    errdefer allocator.free(duped);
    node.* = .{
        .label = duped,
        .count = 0,
        .children = std.StringHashMap(*CallNode).init(allocator),
        .io_counts = [_]u64{0} ** op_kind_count,
    };
    return node;
}

fn free_task_node(allocator: std.mem.Allocator, node: *TaskNode) void {
    var it = node.children.iterator();
    while (it.next()) |entry| {
        free_task_node(allocator, entry.value_ptr.*);
    }
    node.children.deinit();
    if (node.call_root) |call_root| {
        free_call_node(allocator, call_root);
    }
    allocator.free(node.label);
    allocator.destroy(node);
}

fn free_call_node(allocator: std.mem.Allocator, node: *CallNode) void {
    var it = node.children.iterator();
    while (it.next()) |entry| {
        free_call_node(allocator, entry.value_ptr.*);
    }
    node.children.deinit();
    allocator.free(node.label);
    allocator.destroy(node);
}

const TaskState = struct {
    node: *TaskNode,
    stack: std.ArrayListUnmanaged(*CallNode),
};
