const std = @import("std");
const ink = @import("ink");

pub fn main() !void {
    const source = "fn main() -> int\n\t0\n";
    var lex = try ink.lexer.init(source);
    var tokens = std.ArrayList(ink.token).init(std.heap.page_allocator);
    defer tokens.deinit();

    while (try lex.next()) |tok| {
        try tokens.append(tok);
        if (tok.which == .end_of_file) break;
    }

    const out = std.io.getStdOut().writer();
    for (tokens.items, 0..) |tok, idx| {
        try out.print("{d}: {s} [{d},{d}]\n", .{ idx, @tagName(tok.which), tok.where.start, tok.where.end });
    }
}
