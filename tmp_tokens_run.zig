const std = @import("std");
const ink = @import("ink");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    if (args.len < 2) {
        std.debug.print("usage: {s} <file>\n", .{args[0]});
        return;
    }

    const path = args[1];
    const data = try std.fs.cwd().readFileAlloc(alloc, path, 10 * 1024 * 1024);
    defer alloc.free(data);

    var lex = try ink.lexer.init(data);
    var i: usize = 0;
    while (try lex.next()) |tok| {
        std.debug.print("{d}: {s} [{d},{d}] `{s}`\n", .{ i, @tagName(tok.which), tok.where.start, tok.where.end, data[tok.where.start..tok.where.end] });
        i += 1;
        if (tok.which == .end_of_file) break;
    }
}
