const std = @import("std");
const ink = @import("ink");

fn print_expected(info: anytype) void {
    std.debug.print("expected:\n", .{});
    for (info.expected) |expected| {
        std.debug.print("- {s}\n", .{@tagName(expected)});
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);
    if (args.len < 2) {
        std.debug.print("usage: tmp_parse_decl <file>\n", .{});
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

    var parse_mod = try ink.peg_parser.parse_from(alloc, tokens.items, ink.peg.nonterminal_kind.mod_decl);
    defer parse_mod.deinit();
    std.debug.print("mod_decl ok: {any}\n", .{parse_mod.ok});
    if (!parse_mod.ok) {
        if (parse_mod.@"error") |info| {
            std.debug.print("mod_decl error at {d}\n", .{info.position});
            if (info.position < tokens.items.len) {
                const tok = tokens.items[info.position];
                std.debug.print("found {s} '{s}'\n", .{@tagName(tok.which), tok.what.string});
            }
            print_expected(info);
        }
    }

    var parse_decl = try ink.peg_parser.parse_from(alloc, tokens.items, ink.peg.nonterminal_kind.decl);
    defer parse_decl.deinit();
    std.debug.print("decl ok: {any}\n", .{parse_decl.ok});
    if (!parse_decl.ok) {
        if (parse_decl.@"error") |info| {
            std.debug.print("decl error at {d}\n", .{info.position});
            if (info.position < tokens.items.len) {
                const tok = tokens.items[info.position];
                std.debug.print("found {s} '{s}'\n", .{@tagName(tok.which), tok.what.string});
            }
            print_expected(info);
        }
    }
}
