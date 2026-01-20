const std = @import("std");
const ink = @import("ink");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);
    if (args.len < 2) {
        std.debug.print("usage: tmp_parse_expr <file>\n", .{});
        return;
    }
    const data = try std.fs.cwd().readFileAlloc(alloc, args[1], 1 << 20);
    defer alloc.free(data);

    var lex = try ink.lexer.init(data);
    var tokens = std.array_list.Managed(ink.token).init(alloc);
    defer tokens.deinit();
    while (try lex.next()) |tok| {
        try tokens.append(tok);
        if (tok.which == .end_of_file) break;
    }

    const slice = tokens.items[90..];
    var parse = try ink.peg_parser.parse_from(alloc, slice, ink.peg.nonterminal_kind.expr);
    defer parse.deinit();
    std.debug.print("expr ok: {any}\n", .{parse.ok});
    if (!parse.ok) {
        if (parse.@"error") |info| {
            std.debug.print("error at {d}\n", .{info.position});
            if (info.position < slice.len) {
                const tok = slice[info.position];
                std.debug.print("found {s} '{s}'\n", .{@tagName(tok.which), tok.what.string});
            }
            std.debug.print("expected:\n", .{});
            for (info.expected) |expected| {
                std.debug.print("- {s}\n", .{@tagName(expected)});
            }
        }
    }
}
