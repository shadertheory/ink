const std = @import("std");
const ink = @import("ink");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const path = "/tmp/ink_test.ink";
    const data = try std.fs.cwd().readFileAlloc(allocator, path, 1 << 20);
    defer allocator.free(data);

    var lex = try ink.lexer.init(data);
    var tokens = std.ArrayList(ink.token).empty;
    defer tokens.deinit(allocator);

    while (try lex.next()) |tok| {
        try tokens.append(allocator, tok);
        if (tok.which == .end_of_file) break;
    }

    var parse = try ink.peg_parser.parse(allocator, tokens.items);
    defer parse.deinit();
    std.debug.print("parse ok: {any}\n", .{parse.ok});
    if (!parse.ok) {
        if (parse.@"error") |info| {
            std.debug.print("parse error at token index {d}, found {s}\n", .{ info.position, if (info.found) |found| @tagName(found) else "end_of_file" });
        } else {
            std.debug.print("parse error: no info\n", .{});
        }
    }

    for (tokens.items, 0..) |tok, idx| {
        std.debug.print("{d}: {s} [{d},{d}]\n", .{ idx, @tagName(tok.which), tok.where.start, tok.where.end });
    }
}
