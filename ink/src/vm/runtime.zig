const std = @import("std");
const core = @import("core.zig");
const inkb = @import("inkb.zig");
const scheduler = @import("../runtime/scheduler.zig");

pub const default_step_budget: usize = 10_000;

pub const vm_task = struct {
    machine: *core.machine,
    task_id: scheduler.task_id = 0,
    step_budget: usize = default_step_budget,
};

pub fn spawn_vm(sched: *scheduler.scheduler, task: *vm_task) !scheduler.task_id {
    const id = try sched.spawn(.{
        .context = task,
        .poll = poll_vm_task,
        .on_cancel = on_cancel_vm_task,
    });
    task.task_id = id;
    return id;
}

pub fn run_program(
    allocator: std.mem.Allocator,
    program: *const inkb.program,
    lib_dir: ?[]const u8,
) !void {
    var sched = try scheduler.scheduler.init(allocator);
    defer sched.deinit();

    var vm = core.machine.init(
        allocator,
        program.bytecode,
        program.constants,
        program.data,
        program.foreigns,
        &sched,
        lib_dir,
    );
    defer vm.deinit();

    var task = vm_task{ .machine = &vm };
    _ = try spawn_vm(&sched, &task);
    try sched.run();
}

fn poll_vm_task(ctx: *anyopaque, sched: *scheduler.scheduler, id: scheduler.task_id) scheduler.poll_status {
    const task: *vm_task = @ptrCast(@alignCast(ctx));
    const machine = task.machine;

    if (sched.is_cancelled(id)) {
        machine.processor.halted = true;
    }

    machine.processor.set_task_context(sched, id);
    _ = machine.step(task.step_budget);

    if (machine.processor.halted) {
        if (sched.has_children(id)) {
            sched.begin_join(id);
            return .pending;
        }
        const result = machine.processor.return_value();
        sched.complete_task(id, result);
        return .done;
    }

    return switch (machine.processor.take_suspend()) {
        .op => |op_id| blk: {
            sched.wait(id, op_id) catch return .done;
            break :blk .pending;
        },
        .manual => .pending,
        .none => .ready,
    };
}

fn on_cancel_vm_task(ctx: *anyopaque) void {
    const task: *vm_task = @ptrCast(@alignCast(ctx));
    const sched = task.machine.processor.scheduler orelse return;
    const pending = task.machine.processor.pending_op_id;
    if (pending != 0) {
        sched.cancel_wait(task.task_id, pending);
        if (pending < scheduler.task_wait_base) {
            _ = sched.reactor.cancel(pending);
        }
        task.machine.processor.pending_op_id = 0;
        task.machine.processor.pending_op_pc = 0;
    }
    sched.ready.append(sched.allocator, task.task_id) catch {};
}
