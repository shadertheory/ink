const std = @import("std");
const ink = @import("ink");
const builtin = @import("builtin");

const mem_allocator = std.mem.Allocator;

const Cli = struct {
    const Options = struct {
        input_path: []const u8,
    };

    const ParseError = error{InvalidArgs};

    fn parse(args: []const []const u8, p: *std.Io.Writer) ParseError!Options {
        var input_path: ?[]const u8 = null;

        var i: usize = 1;
        while (i < args.len) : (i += 1) {
            const arg = args[i];
            if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
                print_usage(p, args[0]) catch {};
                return error.InvalidArgs;
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

        return .{ .input_path = input_path.? };
    }

    fn print_usage(p: *std.Io.Writer, exe_name: []const u8) !void {
        try p.print("Usage: {s} <program.inkb>\n", .{exe_name});
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

    try ink.vm.runtime.run_program(allocator, &program, lib_dir);
}

fn is_quill_lib_dir(path: []const u8) bool {
    const unix_suffix = "/.quill/lib";
    if (std.mem.endsWith(u8, path, unix_suffix)) return true;
    if (builtin.os.tag != .windows) return false;
    const win_suffix = "\\.quill\\lib";
    return std.mem.endsWith(u8, path, win_suffix);
}
