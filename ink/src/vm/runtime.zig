const std = @import("std");
const core = @import("core.zig");
const inkb = @import("inkb.zig");
const scheduler = @import("../runtime/scheduler.zig");
const async = @import("../runtime/async.zig");
const trace = @import("../runtime/trace.zig");

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

pub const RunConfig = struct {
    reactor: async.Config = .{},
    debug_checks: bool = true,
    sim_report: ?*async.SimReport = null,
};

pub fn run_program(
    allocator: std.mem.Allocator,
    program: *const inkb.program,
    lib_dir: ?[]const u8,
) !void {
    try run_program_with_config(allocator, program, lib_dir, .{});
}

pub fn run_program_with_config(
    allocator: std.mem.Allocator,
    program: *const inkb.program,
    lib_dir: ?[]const u8,
    config: RunConfig,
) !void {
    var program_trace: ?trace.ProgramTrace = null;
    var trace_ptr: ?*trace.ProgramTrace = null;
    var trace_finished = false;
    if (config.sim_report != null) {
        if (program.debug) |dbg| {
            const debug_funcs = try allocator.alloc(trace.DebugFunction, dbg.functions.len);
            defer allocator.free(debug_funcs);
            for (dbg.functions, 0..) |func, idx| {
                debug_funcs[idx] = .{
                    .entry_pc = @intCast(func.entry_pc),
                    .name = func.name,
                };
            }
            program_trace = try trace.ProgramTrace.init(allocator, debug_funcs);
            trace_ptr = &program_trace.?;
        }
    }
    defer if (program_trace) |*trace_state| if (!trace_finished) trace_state.deinit();

    var sched = try scheduler.scheduler.init(allocator, config.reactor);
    defer sched.deinit();

    var vm = core.machine.init(
        allocator,
        program.bytecode,
        program.constants,
        program.data,
        program.foreigns,
        &sched,
        lib_dir,
        config.debug_checks,
        trace_ptr,
    );
    defer vm.deinit();

    var task = vm_task{ .machine = &vm };
    const root_id = try spawn_vm(&sched, &task);
    if (trace_ptr) |trace_ctx| {
        trace_ctx.note_task_start(@intCast(root_id), 0, 0);
    }
    try sched.run();

    if (config.sim_report) |out| {
        if (try sched.reactor.report(allocator)) |report| {
            var final = report;
            if (trace_ptr) |trace_ctx| {
                final.program_tree = trace_ctx.finish();
                trace_finished = true;
            }
            out.* = final;
        }
    }
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
