const std = @import("std");
const ink = @import("ink");
const builtin = @import("builtin");

const mem_allocator = std.mem.Allocator;

const Cli = struct {
    const Options = struct {
        input_path: []const u8,
        scenario: ?[]const u8 = null,
        replay_path: ?[]const u8 = null,
        seed: ?u64 = null,
        samples: ?u64 = null,
        run_all: bool = false,
    };

    const ParseError = error{InvalidArgs};

    fn parse(args: []const []const u8, p: *std.Io.Writer) ParseError!Options {
        var input_path: ?[]const u8 = null;
        var scenario: ?[]const u8 = null;
        var replay_path: ?[]const u8 = null;
        var seed: ?u64 = null;
        var samples: ?u64 = null;
        var run_all = false;

        var i: usize = 1;
        while (i < args.len) : (i += 1) {
            const arg = args[i];
            if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
                print_usage(p, args[0]) catch {};
                return error.InvalidArgs;
            }
            if (std.mem.eql(u8, arg, "--scenario")) {
                if (i + 1 >= args.len) {
                    p.print("error: missing value for {s}\n", .{arg}) catch {};
                    print_usage(p, args[0]) catch {};
                    return error.InvalidArgs;
                }
                scenario = args[i + 1];
                i += 1;
                continue;
            }
            if (std.mem.eql(u8, arg, "--all")) {
                run_all = true;
                continue;
            }
            if (std.mem.eql(u8, arg, "--replay")) {
                if (i + 1 >= args.len) {
                    p.print("error: missing value for {s}\n", .{arg}) catch {};
                    print_usage(p, args[0]) catch {};
                    return error.InvalidArgs;
                }
                replay_path = args[i + 1];
                i += 1;
                continue;
            }
            if (std.mem.eql(u8, arg, "--seed")) {
                if (i + 1 >= args.len) {
                    p.print("error: missing value for {s}\n", .{arg}) catch {};
                    print_usage(p, args[0]) catch {};
                    return error.InvalidArgs;
                }
                seed = std.fmt.parseInt(u64, args[i + 1], 10) catch {
                    p.print("error: invalid seed: {s}\n", .{args[i + 1]}) catch {};
                    print_usage(p, args[0]) catch {};
                    return error.InvalidArgs;
                };
                i += 1;
                continue;
            }
            if (std.mem.eql(u8, arg, "--samples")) {
                if (i + 1 >= args.len) {
                    p.print("error: missing value for {s}\n", .{arg}) catch {};
                    print_usage(p, args[0]) catch {};
                    return error.InvalidArgs;
                }
                samples = std.fmt.parseInt(u64, args[i + 1], 10) catch {
                    p.print("error: invalid samples: {s}\n", .{args[i + 1]}) catch {};
                    print_usage(p, args[0]) catch {};
                    return error.InvalidArgs;
                };
                i += 1;
                continue;
            }
            if (input_path == null) {
                input_path = arg;
            } else {
                p.print("error: unexpected argument: {s}\n", .{arg}) catch {};
                print_usage(p, args[0]) catch {};
                return error.InvalidArgs;
            }
        }

        if (input_path == null) {
            print_usage(p, args[0]) catch {};
            return error.InvalidArgs;
        }

        return .{
            .input_path = input_path.?,
            .scenario = scenario,
            .replay_path = replay_path,
            .seed = seed,
            .samples = samples,
            .run_all = run_all,
        };
    }

    fn print_usage(p: *std.Io.Writer, exe_name: []const u8) !void {
        try p.print("Usage: {s} [--scenario <name>] [--all] [--seed <n>] [--samples <n>] [--replay <path>] <program.inkb>\n", .{exe_name});
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator: mem_allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var err_memory = [_]u8{0} ** 8192;
    var err_file_writer = std.fs.File.stderr().writer(err_memory[0..]);
    var err_writer = &err_file_writer.interface;
    const options = Cli.parse(args, err_writer) catch |err| {
        if (err == error.InvalidArgs) {
            err_writer.flush() catch {};
            return;
        }
        return err;
    };

    var program = try ink.vm.inkb.read_file(allocator, options.input_path);
    defer program.deinit(allocator);

    const abs_path = try std.fs.cwd().realpathAlloc(allocator, options.input_path);
    defer allocator.free(abs_path);
    const program_dir = std.fs.path.dirname(abs_path) orelse ".";
    const lib_dir = if (is_quill_lib_dir(program_dir))
        try allocator.dupe(u8, program_dir)
    else
        try std.fs.path.join(allocator, &.{ program_dir, ".quill", "lib" });
    defer allocator.free(lib_dir);

    const root_dir = try resolve_project_root(allocator, program_dir);
    defer allocator.free(root_dir);
    const manifest_path = try resolve_simulator_manifest(allocator, root_dir);
    defer allocator.free(manifest_path);
    const manifest_name = std.fs.path.basename(manifest_path);

    var sim_cfg = ink.sim.parse(allocator, manifest_path) catch |err| {
        switch (err) {
            error.FileNotFound => err_writer.print("error: {s} not found\n", .{manifest_name}) catch {},
            error.MissingSimulator => err_writer.print("error: {s} missing top-level simulator config\n", .{manifest_name}) catch {},
            error.MissingSimImport => err_writer.print("error: {s} missing import sim for simulator config\n", .{manifest_name}) catch {},
            error.MissingScenarioName => err_writer.print("error: {s} missing simulator scenario name\n", .{manifest_name}) catch {},
            error.InvalidSimulator => err_writer.print("error: invalid simulator config format in {s}\n", .{manifest_name}) catch {},
            else => return err,
        }
        err_writer.flush() catch {};
        std.process.exit(1);
    };
    defer sim_cfg.deinit();

    var scenario_names: ?[]const []const u8 = null;
    if (!options.run_all) {
        if (options.scenario) |raw| {
            scenario_names = try parse_scenario_list(allocator, raw);
        }
    }
    defer if (scenario_names != null) allocator.free(scenario_names.?);

    const capture_report = sim_cfg.report != null;
    var runs = ink.sim_runtime.build_runs(allocator, root_dir, sim_cfg, .{
        .scenario_names = scenario_names,
        .base_seed = options.seed,
        .sample_override = options.samples,
        .capture_report = capture_report,
    }) catch |err| {
        switch (err) {
            error.UnknownScenario => err_writer.writeAll("error: unknown scenario\n") catch {},
            error.ScenarioCycle => err_writer.writeAll("error: scenario composition cycle\n") catch {},
            error.InvalidSweep => err_writer.writeAll("error: invalid sweep configuration\n") catch {},
            error.InvalidSimulator => err_writer.writeAll("error: invalid simulator config\n") catch {},
            else => return err,
        }
        err_writer.flush() catch {};
        std.process.exit(1);
    };
    defer runs.deinit();

    var reports = std.array_list.Managed(ink.runtime.async.SimReport).init(allocator);
    defer {
        for (reports.items) |*report| report.deinit(allocator);
        reports.deinit();
    }

    if (options.replay_path) |path| {
        for (runs.runs) |*run| {
            run.config.replay_path = path;
        }
    }

    const concurrency = parse_concurrency(sim_cfg.concurrency) catch {
        err_writer.writeAll("error: invalid simulator concurrency\n") catch {};
        err_writer.flush() catch {};
        std.process.exit(1);
    };
    const worker_count = if (runs.runs.len == 0) 0 else @min(concurrency, runs.runs.len);

    if (worker_count <= 1) {
        for (runs.runs) |*run| {
            var report: ink.runtime.async.SimReport = undefined;
            const run_cfg = ink.vm.runtime.RunConfig{
                .reactor = .{ .mode = .sim, .sim = run.config },
                .debug_checks = true,
                .sim_report = if (capture_report) &report else null,
            };
            try ink.vm.runtime.run_program_with_config(allocator, &program, lib_dir, run_cfg);
            if (capture_report) {
                try reports.append(report);
            }
        }
    } else {
        var pool: std.Thread.Pool = undefined;
        try pool.init(.{ .allocator = allocator, .n_jobs = worker_count });
        defer pool.deinit();
        var wait_group = std.Thread.WaitGroup{};
        var report_lock = std.Thread.Mutex{};
        var err_lock = std.Thread.Mutex{};
        var first_err: ?anyerror = null;

        for (runs.runs) |*run| {
            const job = RunJob{
                .allocator = allocator,
                .program = &program,
                .lib_dir = lib_dir,
                .run = run,
                .capture_report = capture_report,
                .reports = &reports,
                .reports_lock = &report_lock,
                .err_lock = &err_lock,
                .first_err = &first_err,
            };
            pool.spawnWg(&wait_group, run_job, .{job});
        }

        wait_group.wait();
        if (first_err) |err| return err;
    }

    if (capture_report) {
        if (sim_cfg.report) |report_cfg| {
            try ink.sim_report.emit_reports(allocator, reports.items, runs.runs, report_cfg, .{
                .root_dir = root_dir,
                .manifest_path = manifest_path,
            });
        }
    }
}

fn is_quill_lib_dir(path: []const u8) bool {
    const unix_suffix = "/.quill/lib";
    if (std.mem.endsWith(u8, path, unix_suffix)) return true;
    if (builtin.os.tag != .windows) return false;
    const win_suffix = "\\.quill\\lib";
    return std.mem.endsWith(u8, path, win_suffix);
}

fn resolve_project_root(allocator: mem_allocator, program_dir: []const u8) ![]const u8 {
    if (is_quill_lib_dir(program_dir)) {
        const parent = std.fs.path.dirname(program_dir) orelse program_dir;
        const root = std.fs.path.dirname(parent) orelse parent;
        return try allocator.dupe(u8, root);
    }
    return try allocator.dupe(u8, program_dir);
}

fn resolve_simulator_manifest(allocator: mem_allocator, root_dir: []const u8) ![]const u8 {
    const sim_path = try std.fs.path.join(allocator, &.{ root_dir, "simulator.ink" });
    if (std.fs.cwd().access(sim_path, .{})) {
        return sim_path;
    } else |err| switch (err) {
        error.FileNotFound => {
            allocator.free(sim_path);
            return try std.fs.path.join(allocator, &.{ root_dir, "package.ink" });
        },
        else => return err,
    }
}

fn parse_scenario_list(allocator: mem_allocator, raw: []const u8) ![]const []const u8 {
    var list = std.array_list.Managed([]const u8).init(allocator);
    errdefer list.deinit();
    var iter = std.mem.tokenizeAny(u8, raw, ", \t");
    while (iter.next()) |token| {
        if (token.len == 0) continue;
        try list.append(token);
    }
    return try list.toOwnedSlice();
}

fn parse_concurrency(raw: []const u8) !usize {
    if (raw.len == 0) return 1;
    if (std.mem.eql(u8, raw, "half")) {
        const count = std.Thread.getCpuCount() catch 1;
        return @max(@as(usize, 1), count / 2);
    }
    if (std.mem.eql(u8, raw, "all")) {
        return std.Thread.getCpuCount() catch 1;
    }
    const parsed = std.fmt.parseInt(usize, raw, 10) catch return error.InvalidConcurrency;
    if (parsed == 0) return error.InvalidConcurrency;
    return parsed;
}

const RunJob = struct {
    allocator: mem_allocator,
    program: *const ink.vm.inkb.program,
    lib_dir: []const u8,
    run: *ink.sim_runtime.Run,
    capture_report: bool,
    reports: *std.array_list.Managed(ink.runtime.async.SimReport),
    reports_lock: *std.Thread.Mutex,
    err_lock: *std.Thread.Mutex,
    first_err: *?anyerror,
};

fn run_job(job: RunJob) void {
    var report: ink.runtime.async.SimReport = undefined;
    const run_cfg = ink.vm.runtime.RunConfig{
        .reactor = .{ .mode = .sim, .sim = job.run.config },
        .debug_checks = true,
        .sim_report = if (job.capture_report) &report else null,
    };
    const run_result = ink.vm.runtime.run_program_with_config(job.allocator, job.program, job.lib_dir, run_cfg);
    if (run_result) |_| {
        if (job.capture_report) {
            job.reports_lock.lock();
            const append_result = job.reports.append(report);
            job.reports_lock.unlock();
            append_result catch |err| {
                report.deinit(job.allocator);
                record_run_error(job.err_lock, job.first_err, err);
            };
        }
    } else |err| {
        record_run_error(job.err_lock, job.first_err, err);
    }
}

fn record_run_error(lock: *std.Thread.Mutex, slot: *?anyerror, err: anyerror) void {
    lock.lock();
    defer lock.unlock();
    if (slot.* == null) {
        slot.* = err;
    }
}
