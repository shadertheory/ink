const std = @import("std");
const ink = @import("ink");
const graph = @import("graph.zig");
const target_mod = ink.target;

const mem_allocator = std.mem.Allocator;
const builtin = @import("builtin");
const array_list = std.array_list.Managed;

const Command = enum { build, run, sim, init, new, env, install, help };

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator: mem_allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        var err_buf: [1024]u8 = undefined;
        var err_writer = std.fs.File.stderr().writer(&err_buf);
        try print_usage(&err_writer.interface, args[0]);
        try err_writer.interface.flush();
        return;
    }

    const cmd = parse_command(args[1]);
    switch (cmd) {
        .build => handle_build(allocator, args[2..], false, null) catch |err| try handle_usage_error(err, args[0]),
        .run => handle_build(allocator, args[2..], true, null) catch |err| try handle_usage_error(err, args[0]),
        .sim => handle_build(allocator, args[2..], true, "sim") catch |err| try handle_usage_error(err, args[0]),
        .init => handle_init(allocator, args[2..]) catch |err| try handle_usage_error(err, args[0]),
        .new => handle_new(allocator, args[2..]) catch |err| try handle_usage_error(err, args[0]),
        .env => handle_env(allocator) catch |err| try handle_usage_error(err, args[0]),
        .install => handle_install(allocator, args[2..]) catch |err| try handle_usage_error(err, args[0]),
        .help => {
            var out_buf: [1024]u8 = undefined;
            var out_writer = std.fs.File.stdout().writer(&out_buf);
            try print_usage(&out_writer.interface, args[0]);
            try out_writer.interface.flush();
        },
    }
}

fn parse_command(arg: []const u8) Command {
    if (std.mem.eql(u8, arg, "build")) return .build;
    if (std.mem.eql(u8, arg, "run")) return .run;
    if (std.mem.eql(u8, arg, "sim")) return .sim;
    if (std.mem.eql(u8, arg, "init")) return .init;
    if (std.mem.eql(u8, arg, "new")) return .new;
    if (std.mem.eql(u8, arg, "env")) return .env;
    if (std.mem.eql(u8, arg, "install")) return .install;
    return .help;
}

fn print_usage(writer: *std.Io.Writer, exe_name: []const u8) !void {
    try writer.print(
        "Usage: {s} <command> [options]\n\n",
        .{exe_name},
    );
    try writer.writeAll(
        "Commands:\n" ++
            "  build        Compile the current package\n" ++
            "  run          Compile and run the current package\n" ++
            "  sim          Compile and run using the sim profile\n" ++
            "  init         Initialize a package in the current directory\n" ++
            "  new          Create a new package in a directory\n" ++
            "  env          Print PATH instructions for zig-out/bin\n" ++
            "  install      Install tools to a prefix\n" ++
        "Options:\n" ++
            "  --manifest <path>  Path to package.ink or package dir (default: .)\n" ++
            "  --out <path>       Output .inkb path (build only)\n" ++
            "  --profile <name>   Build profile to use (build/run)\n" ++
            "  --target <target>  Target backend (build/run)\n" ++
            "  --prefix <path>    Install prefix (install only)\n",
    );
}

fn handle_usage_error(err: anyerror, exe_name: []const u8) !void {
    if (err != error.InvalidArgs) return err;
    var err_buf: [1024]u8 = undefined;
    var err_writer = std.fs.File.stderr().writer(&err_buf);
    try print_usage(&err_writer.interface, exe_name);
    try err_writer.interface.flush();
}

fn resolve_manifest_root(allocator: mem_allocator, path: []const u8) ![]const u8 {
    const abs = try std.fs.cwd().realpathAlloc(allocator, path);
    errdefer allocator.free(abs);
    const stat = try std.fs.cwd().statFile(abs);
    if (stat.kind == .directory) {
        return abs;
    }
    const dir = std.fs.path.dirname(abs) orelse return error.InvalidArgs;
    const duped = try allocator.dupe(u8, dir);
    allocator.free(abs);
    return duped;
}

fn handle_build(allocator: mem_allocator, args: []const []const u8, run_after: bool, forced_profile: ?[]const u8) !void {
    var manifest_path: []const u8 = ".";
    var out_path: ?[]const u8 = null;
    var target_text: ?[]const u8 = null;
    var profile: ?[]const u8 = forced_profile;

    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--manifest")) {
            if (i + 1 >= args.len) return error.InvalidArgs;
            manifest_path = args[i + 1];
            i += 1;
            continue;
        }
        if (std.mem.eql(u8, arg, "--out")) {
            if (i + 1 >= args.len) return error.InvalidArgs;
            out_path = args[i + 1];
            i += 1;
            continue;
        }
        if (std.mem.eql(u8, arg, "--target") or std.mem.eql(u8, arg, "-t")) {
            if (i + 1 >= args.len) return error.InvalidArgs;
            target_text = args[i + 1];
            i += 1;
            continue;
        }
        if (std.mem.eql(u8, arg, "--profile")) {
            if (i + 1 >= args.len) return error.InvalidArgs;
            if (forced_profile != null) return error.InvalidArgs;
            profile = args[i + 1];
            i += 1;
            continue;
        }
        return error.InvalidArgs;
    }

    var target_spec: target_mod.target_spec = .{ .kind = .vm };
    if (target_text) |text| {
        target_spec = target_mod.parse_target(text) catch {
            var err_buf: [256]u8 = undefined;
            var err_writer = std.fs.File.stderr().writer(&err_buf);
            err_writer.interface.print("error: invalid target: {s}\n", .{text}) catch {};
            err_writer.interface.flush() catch {};
            return error.InvalidArgs;
        };
    }
    if (run_after and target_spec.kind != .vm) {
        var err_buf: [256]u8 = undefined;
        var err_writer = std.fs.File.stderr().writer(&err_buf);
        err_writer.interface.writeAll("error: run is only supported for the vm target\n") catch {};
        err_writer.interface.flush() catch {};
        return error.InvalidArgs;
    }

    const root_dir = try resolve_manifest_root(allocator, manifest_path);
    defer allocator.free(root_dir);

    var dep_graph = graph.build(allocator, root_dir) catch |err| {
        var err_buf: [1024]u8 = undefined;
        var err_writer = std.fs.File.stderr().writer(&err_buf);
        switch (err) {
            error.FileNotFound => try err_writer.interface.print(
                "error: package.ink not found under {s}\n",
                .{root_dir},
            ),
            error.MissingName => try err_writer.interface.writeAll("error: package.ink missing package name\n"),
            error.MissingVersion => try err_writer.interface.writeAll("error: package.ink missing package version\n"),
            error.MissingModuleSources => try err_writer.interface.writeAll("error: module missing sources\n"),
            error.MissingDepPath => try err_writer.interface.writeAll("error: dependency missing path\n"),
            error.MissingRegistryName => try err_writer.interface.writeAll("error: registry missing name\n"),
            error.MissingRegistryUrl => try err_writer.interface.writeAll("error: registry missing url\n"),
            error.InvalidManifest => try err_writer.interface.writeAll("error: invalid package.ink format\n"),
            error.MissingSources => try err_writer.interface.writeAll("error: module sources not found\n"),
            error.InvalidSources => try err_writer.interface.writeAll("error: invalid module sources path\n"),
            error.DuplicateModuleName => try err_writer.interface.writeAll("error: duplicate module name\n"),
            error.RegistryUnavailable => try err_writer.interface.writeAll("error: registry dependencies are not supported yet\n"),
            else => return err,
        }
        try err_writer.interface.flush();
        return err;
    };
    defer dep_graph.deinit();

    const final_out = try resolve_output_path(allocator, root_dir, dep_graph.root_module, out_path);
    defer allocator.free(final_out);

    const inkc_path = try find_tool_path(allocator, "inkc");
    defer allocator.free(inkc_path);
    const inkvm_path = if (run_after) try find_tool_path(allocator, "inkvm") else null;
    defer if (inkvm_path != null) allocator.free(inkvm_path.?);

    try std.fs.cwd().makePath(std.fs.path.dirname(final_out) orelse ".");

    var extra_args = array_list([]const u8).init(allocator);
    defer extra_args.deinit();
    try extra_args.append("--manifest");
    try extra_args.append(root_dir);
    try extra_args.append("--output");
    try extra_args.append(final_out);
    if (profile) |name| {
        try extra_args.append("--profile");
        try extra_args.append(name);
    }
    if (target_text) |text| {
        try extra_args.append("--target");
        try extra_args.append(text);
    }
    const inkc_args = try build_tool_args(allocator, inkc_path, extra_args.items);
    defer allocator.free(inkc_args);

    const compile_term = try run_tool(allocator, inkc_args, root_dir);
    if (!term_ok(compile_term)) return error.CompileFailed;

    if (run_after) {
        if (profile != null and std.mem.eql(u8, profile.?, "sim")) {
            const inkvm_args = try build_tool_args(allocator, inkvm_path.?, &.{ "--sim", final_out });
            defer allocator.free(inkvm_args);
            const run_term = try run_tool(allocator, inkvm_args, root_dir);
            if (!term_ok(run_term)) return error.RunFailed;
            return;
        }
        const inkvm_args = try build_tool_args(allocator, inkvm_path.?, &.{final_out});
        defer allocator.free(inkvm_args);
        const run_term = try run_tool(allocator, inkvm_args, root_dir);
        if (!term_ok(run_term)) return error.RunFailed;
    }
}

fn handle_init(allocator: mem_allocator, args: []const []const u8) !void {
    if (args.len != 0) return error.InvalidArgs;
    try write_project_templates(allocator, ".");
}

fn handle_new(allocator: mem_allocator, args: []const []const u8) !void {
    if (args.len != 1) return error.InvalidArgs;
    const dir = args[0];
    try std.fs.cwd().makePath(dir);
    try write_project_templates(allocator, dir);
}

fn write_project_templates(allocator: mem_allocator, root_dir: []const u8) !void {
    const package_template =
        "import build\n\n" ++
        "fn package()\n" ++
        "\tbuild::package\n" ++
        "\t\tname = \"app\"\n" ++
        "\t\tversion = \"0.1.0\"\n" ++
        "\t\troot = \"src\"\n" ++
        "\t\tprofile = build::profile\n" ++
        "\t\t\tname = \"debug\"\n" ++
        "\t\t\ttarget = \"vm\"\n" ++
        "\t\tprofile = build::profile\n" ++
        "\t\t\tname = \"release\"\n" ++
        "\t\t\ttarget = \"vm\"\n" ++
        "\t\tprofile = build::profile\n" ++
        "\t\t\tname = \"sim\"\n" ++
        "\t\t\ttarget = \"vm\"\n" ++
        "\t\tmodule = build::module\n" ++
        "\t\t\tname = \"app\"\n" ++
        "\t\t\tsources = \"src\"\n";
    const simulator_template =
        "import sim\n\n" ++
        "fn simulator()\n" ++
        "\tsim::simulator\n" ++
        "\t\tseed = 0\n" ++
        "\t\tconcurrency = \"half\"\n" ++
        "\t\tsnapshots = sim::snapshots\n" ++
        "\t\t\tsteps = 1\n" ++
        "\t\t\tmode = \"full+delta\"\n" ++
        "\t\t\tcompress = \"zstd\"\n" ++
        "\t\tvalidation = sim::validation\n" ++
        "\t\t\tlevel = \"strict\"\n" ++
        "\t\tscenario = sim::scenario\n" ++
        "\t\t\tname = \"default\"\n" ++
        "\t\t\tcomponents = sim::components\n" ++
        "\t\t\t\ttcp = \"mock\"\n" ++
        "\t\t\t\tudp = \"mock\"\n" ++
        "\t\t\t\tfs = \"real\"\n" ++
        "\t\t\t\tclock = \"sim\"\n" ++
        "\t\t\t\trng = \"sim\"\n" ++
        "\t\t\t\talloc = \"sim\"\n" ++
        "\t\t\t\tscheduler = \"sim\"\n" ++
        "\t\t\tfaults = sim::faults\n";
    const main_template =
        "fn main() -> int\n" ++
        "\t0\n";

    const package_path = try std.fs.path.join(allocator, &.{ root_dir, "package.ink" });
    defer allocator.free(package_path);
    try write_new_file(package_path, package_template);

    const simulator_path = try std.fs.path.join(allocator, &.{ root_dir, "simulator.ink" });
    defer allocator.free(simulator_path);
    try write_new_file(simulator_path, simulator_template);

    const src_dir = try std.fs.path.join(allocator, &.{ root_dir, "src" });
    defer allocator.free(src_dir);
    try std.fs.cwd().makePath(src_dir);

    const main_path = try std.fs.path.join(allocator, &.{ root_dir, "src", "main.ink" });
    defer allocator.free(main_path);
    try write_new_file(main_path, main_template);
}

fn write_new_file(path: []const u8, contents: []const u8) !void {
    var file = try std.fs.cwd().createFile(path, .{ .exclusive = true });
    defer file.close();
    try file.writeAll(contents);
}

fn resolve_output_path(
    allocator: mem_allocator,
    root_dir: []const u8,
    name: []const u8,
    provided: ?[]const u8,
) ![]const u8 {
    if (provided) |path| return try allocator.dupe(u8, path);
    const out_dir = try std.fs.path.join(allocator, &.{ root_dir, ".quill", "lib" });
    defer allocator.free(out_dir);
    const out_path = try std.fmt.allocPrint(allocator, "{s}/{s}.inkb", .{ out_dir, name });
    return out_path;
}

fn build_tool_args(
    allocator: mem_allocator,
    exe_path: []const u8,
    extra: []const []const u8,
) ![]const []const u8 {
    const args = try allocator.alloc([]const u8, extra.len + 1);
    args[0] = exe_path;
    std.mem.copyForwards([]const u8, args[1..], extra);
    return args;
}

fn run_tool(
    allocator: mem_allocator,
    argv: []const []const u8,
    cwd: ?[]const u8,
) !std.process.Child.Term {
    var child = std.process.Child.init(argv, allocator);
    child.stdin_behavior = .Inherit;
    child.stdout_behavior = .Inherit;
    child.stderr_behavior = .Inherit;
    child.cwd = cwd;
    try child.spawn();
    return child.wait();
}

fn term_ok(term: std.process.Child.Term) bool {
    return switch (term) {
        .Exited => |code| code == 0,
        else => false,
    };
}

fn find_tool_path(allocator: mem_allocator, tool: []const u8) ![]const u8 {
    const tool_name = try tool_filename(allocator, tool);
    defer allocator.free(tool_name);

    const self_path = try std.fs.selfExePathAlloc(allocator);
    defer allocator.free(self_path);
    if (std.fs.path.dirname(self_path)) |dir| {
        const candidate = try std.fs.path.join(allocator, &.{ dir, tool_name });
        if (std.fs.cwd().openFile(candidate, .{})) |file| {
            file.close();
            return candidate;
        } else |_| {
            allocator.free(candidate);
        }
    }

    if (try find_workspace_root(allocator)) |root| {
        defer allocator.free(root);
        if (try find_tool_binary(allocator, root, tool)) |path| {
            return path;
        }
    }

    if (try find_in_path(allocator, tool_name)) |path| {
        return path;
    }

    return error.ToolNotFound;
}

fn find_in_path(allocator: mem_allocator, tool_name: []const u8) !?[]const u8 {
    const env_name = if (builtin.os.tag == .windows) "Path" else "PATH";
    const raw = std.process.getEnvVarOwned(allocator, env_name) catch return null;
    defer allocator.free(raw);
    const sep = if (builtin.os.tag == .windows) ';' else ':';
    var it = std.mem.splitScalar(u8, raw, sep);
    while (it.next()) |dir| {
        const trimmed = std.mem.trim(u8, dir, " \t");
        if (trimmed.len == 0) continue;
        const candidate = std.fs.path.join(allocator, &.{ trimmed, tool_name }) catch continue;
        if (std.fs.cwd().openFile(candidate, .{})) |file| {
            file.close();
            return candidate;
        } else |_| {
            allocator.free(candidate);
        }
    }
    return null;
}

fn print_diagnostics(writer: *std.Io.Writer, diags: []const ink.diagnostic) !void {
    for (diags) |diag| {
        const label = severity_label(diag.danger);
        if (diag.span) |span| {
            try writer.print("{s}: {s} ({d}..{d})\n", .{ label, diag.message, span.start, span.end });
        } else {
            try writer.print("{s}: {s}\n", .{ label, diag.message });
        }
    }
}

fn severity_label(danger: ink.severity) []const u8 {
    return switch (danger) {
        .note => "note",
        .warn => "warn",
        .@"error" => "error",
    };
}

fn handle_env(allocator: mem_allocator) !void {
    const cwd = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(cwd);
    const bin_dir = try std.fs.path.join(allocator, &.{ cwd, "zig-out", "bin" });
    defer allocator.free(bin_dir);

    var out_buf: [4096]u8 = undefined;
    var out_writer = std.fs.File.stdout().writer(&out_buf);
    const writer = &out_writer.interface;
    if (builtin.os.tag == .windows) {
        try writer.print("set PATH={s};%PATH%\n", .{bin_dir});
    } else {
        try writer.print("export PATH=\"{s}:$PATH\"\n", .{bin_dir});
    }
    try writer.flush();
}

fn handle_install(allocator: mem_allocator, args: []const []const u8) !void {
    var prefix: ?[]const u8 = null;
    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--prefix")) {
            if (i + 1 >= args.len) return error.InvalidArgs;
            prefix = args[i + 1];
            i += 1;
            continue;
        }
        return error.InvalidArgs;
    }

    const install_prefix = if (prefix) |p| p else try default_prefix(allocator);
    defer if (prefix == null) allocator.free(install_prefix);

    const bin_dir = try std.fs.path.join(allocator, &.{ install_prefix, "bin" });
    defer allocator.free(bin_dir);
    try std.fs.cwd().makePath(bin_dir);

    var dest_dir = try std.fs.cwd().openDir(bin_dir, .{});
    defer dest_dir.close();

    const self_path = try std.fs.selfExePathAlloc(allocator);
    defer allocator.free(self_path);
    const quill_name = try tool_filename(allocator, "quill");
    defer allocator.free(quill_name);
    try copy_tool(std.fs.cwd(), self_path, dest_dir, quill_name);

    if (try find_workspace_root(allocator)) |root| {
        defer allocator.free(root);
        if (try find_tool_binary(allocator, root, "inkc")) |inkc_path| {
            defer allocator.free(inkc_path);
            const inkc_name = try tool_filename(allocator, "inkc");
            defer allocator.free(inkc_name);
            try copy_tool(std.fs.cwd(), inkc_path, dest_dir, inkc_name);
        }
        if (try find_tool_binary(allocator, root, "inkvm")) |inkvm_path| {
            defer allocator.free(inkvm_path);
            const inkvm_name = try tool_filename(allocator, "inkvm");
            defer allocator.free(inkvm_name);
            try copy_tool(std.fs.cwd(), inkvm_path, dest_dir, inkvm_name);
        }
    }
}

fn default_prefix(allocator: mem_allocator) ![]const u8 {
    if (builtin.os.tag == .windows) {
        return try std.fs.getAppDataDir(allocator, "quill");
    }
    const home = std.process.getEnvVarOwned(allocator, "HOME") catch return error.MissingHome;
    defer allocator.free(home);
    return try std.fs.path.join(allocator, &.{ home, ".local" });
}

fn find_workspace_root(allocator: mem_allocator) !?[]const u8 {
    var dir = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(dir);

    while (true) {
        if (has_dir(dir, "inkc") and has_dir(dir, "inkvm") and has_dir(dir, "quill")) {
            return try allocator.dupe(u8, dir);
        }
        const parent = std.fs.path.dirname(dir) orelse break;
        if (std.mem.eql(u8, parent, dir)) break;
        const next_dir = try allocator.dupe(u8, parent);
        allocator.free(dir);
        dir = next_dir;
    }
    return null;
}

fn has_dir(base: []const u8, name: []const u8) bool {
    const path = std.fs.path.join(std.heap.page_allocator, &.{ base, name }) catch return false;
    defer std.heap.page_allocator.free(path);
    if (std.fs.cwd().openDir(path, .{})) |found_dir| {
        var dir_handle = found_dir;
        dir_handle.close();
        return true;
    } else |_| {
        return false;
    }
}

fn find_tool_binary(allocator: mem_allocator, root: []const u8, tool: []const u8) !?[]const u8 {
    const tool_name = try tool_filename(allocator, tool);
    defer allocator.free(tool_name);
    const path = try std.fs.path.join(allocator, &.{ root, tool, "zig-out", "bin", tool_name });
    if (std.fs.cwd().openFile(path, .{})) |file| {
        file.close();
        return path;
    } else |_| {
        allocator.free(path);
        return null;
    }
}

fn copy_tool(
    source_dir: std.fs.Dir,
    source_path: []const u8,
    dest_dir: std.fs.Dir,
    dest_name: []const u8,
) !void {
    try source_dir.copyFile(source_path, dest_dir, dest_name, .{});
}

fn tool_filename(allocator: mem_allocator, base: []const u8) ![]const u8 {
    const suffix = if (builtin.os.tag == .windows) ".exe" else "";
    return std.fmt.allocPrint(allocator, "{s}{s}", .{ base, suffix });
}
