const std = @import("std");
const scheduler = @import("../runtime/scheduler.zig");

pub const task_id = scheduler.task_id;

pub const PauseReason = enum {
    entry,
    breakpoint,
    step,
    pause,
};

pub const StepMode = enum {
    none,
    in,
    over,
    out,
};

pub const Pause = struct {
    task_id: task_id,
    pc: usize,
    reason: PauseReason,
};

pub const Controller = struct {
    allocator: std.mem.Allocator,
    breakpoints: std.AutoHashMap(usize, void),
    paused: std.AutoHashMap(task_id, void),
    paused_list: std.ArrayListUnmanaged(task_id),
    pause: ?Pause = null,
    step_mode: StepMode = .none,
    step_task: task_id = 0,
    step_depth: usize = 0,
    step_pending: bool = false,
    skip_break_task: task_id = 0,
    skip_break_pc: usize = 0,
    stop_on_entry: bool = false,
    stop_on_entry_done: bool = false,
    manual_pause: bool = false,
    suppressed: bool = false,

    pub fn init(allocator: std.mem.Allocator) Controller {
        return .{
            .allocator = allocator,
            .breakpoints = std.AutoHashMap(usize, void).init(allocator),
            .paused = std.AutoHashMap(task_id, void).init(allocator),
            .paused_list = .{},
        };
    }

    pub fn deinit(self: *Controller) void {
        self.breakpoints.deinit();
        self.paused.deinit();
        self.paused_list.deinit(self.allocator);
    }

    pub fn set_stop_on_entry(self: *Controller, enabled: bool) void {
        self.stop_on_entry = enabled;
        self.stop_on_entry_done = false;
    }

    pub fn set_breakpoints(self: *Controller, pcs: []const usize) !void {
        self.breakpoints.clearRetainingCapacity();
        for (pcs) |pc| {
            try self.breakpoints.put(pc, {});
        }
    }

    pub fn clear_breakpoints(self: *Controller) void {
        self.breakpoints.clearRetainingCapacity();
    }

    pub fn request_continue(self: *Controller) void {
        if (self.pause) |pause| {
            if (pause.reason == .breakpoint) {
                self.skip_break_task = pause.task_id;
                self.skip_break_pc = pause.pc;
            }
        }
        self.pause = null;
        self.step_mode = .none;
        self.step_pending = false;
        self.manual_pause = false;
    }

    pub fn request_pause(self: *Controller) void {
        self.manual_pause = true;
    }

    pub fn set_suppressed(self: *Controller, enabled: bool) void {
        self.suppressed = enabled;
    }

    pub fn request_step(self: *Controller, mode: StepMode, task: task_id, depth: usize) void {
        self.pause = null;
        self.step_mode = mode;
        self.step_task = task;
        self.step_depth = depth;
        self.step_pending = true;
        self.manual_pause = false;
    }

    pub fn before_instruction(self: *Controller, task: task_id, pc: usize, depth: usize) bool {
        _ = depth;
        if (self.suppressed) return false;
        if (self.pause != null) return true;
        if (self.stop_on_entry and !self.stop_on_entry_done) {
            self.stop_on_entry_done = true;
            return self.set_pause(task, pc, .entry);
        }
        if (self.manual_pause) {
            self.manual_pause = false;
            return self.set_pause(task, pc, .pause);
        }
        if (self.skip_break_task == task and self.skip_break_pc == pc) {
            self.skip_break_task = 0;
            self.skip_break_pc = 0;
            return false;
        }
        if (self.breakpoints.contains(pc)) {
            return self.set_pause(task, pc, .breakpoint);
        }
        return false;
    }

    pub fn after_instruction(self: *Controller, task: task_id, pc: usize, depth: usize) bool {
        if (self.suppressed) return false;
        if (self.pause != null) return true;
        if (self.step_mode == .none or task != self.step_task) return false;
        if (self.step_pending) {
            self.step_pending = false;
            if (self.step_mode == .in) {
                return self.set_pause(task, pc, .step);
            }
            return false;
        }
        switch (self.step_mode) {
            .in => return self.set_pause(task, pc, .step),
            .over => if (depth <= self.step_depth) return self.set_pause(task, pc, .step),
            .out => if (depth < self.step_depth) return self.set_pause(task, pc, .step),
            .none => {},
        }
        return false;
    }

    pub fn peek_pause(self: *const Controller) ?Pause {
        return self.pause;
    }

    pub fn paused_tasks(self: *const Controller) []const task_id {
        return self.paused_list.items;
    }

    pub fn clear_paused(self: *Controller) void {
        self.paused.clearRetainingCapacity();
        self.paused_list.clearRetainingCapacity();
    }

    fn set_pause(self: *Controller, task: task_id, pc: usize, reason: PauseReason) bool {
        if (self.pause == null) {
            self.pause = .{ .task_id = task, .pc = pc, .reason = reason };
        }
        if (!self.paused.contains(task)) {
            self.paused.put(task, {}) catch return true;
            self.paused_list.append(self.allocator, task) catch {};
        }
        self.step_mode = .none;
        self.step_pending = false;
        return true;
    }
};
