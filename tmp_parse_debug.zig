const std = @import("std");
const ink = @import("ink");

const array_list = std.array_list;
const heap = std.heap;

pub fn main() !void {
    var gpa = heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len < 2) {
        std.debug.print("usage: tmp_parse_debug <path>\n", .{});
        return;
    }

    const path = args[1];
    const source = try std.fs.cwd().readFileAlloc(allocator, path, 1 << 20);
    defer allocator.free(source);

    var lexer = try ink.lexer.init(source);
    var tokens = array_list.Managed(ink.token).init(allocator);
    defer tokens.deinit();

    while (try lexer.next()) |tok| {
        try tokens.append(tok);
        if (tok.which == .end_of_file) break;
    }

    var parse = try ink.peg_parser.parse(allocator, tokens.items);
    defer parse.deinit();

    std.debug.print("tokens: {d}\n", .{tokens.items.len});
    for (tokens.items, 0..) |tok, idx| {
        std.debug.print("{d}: {s} '{s}'\n", .{ idx, @tagName(tok.which), tok.what.string });
    }

    if (!parse.ok) {
        if (parse.@"error") |info| {
            std.debug.print("parse error at {d}\n", .{info.position});
            if (info.position < tokens.items.len) {
                const tok = tokens.items[info.position];
                std.debug.print("found: {s} '{s}'\n", .{ @tagName(tok.which), tok.what.string });
            }
            std.debug.print("expected:\n", .{});
            for (info.expected) |expected| {
                std.debug.print("- {s}\n", .{@tagName(expected)});
            }

            if (info.position < tokens.items.len) {
                const slice = tokens.items[info.position..];
                var type_parse = try ink.peg_parser.parse_from(
                    allocator,
                    slice,
                    ink.peg.nonterminal_kind.type_args,
                );
                defer type_parse.deinit();
                std.debug.print("type_args ok: {any}\n", .{type_parse.ok});
                if (!type_parse.ok) {
                    if (type_parse.@"error") |type_info| {
                        std.debug.print("type_args error at {d}\n", .{type_info.position});
                        if (type_info.position < slice.len) {
                            const type_tok = slice[type_info.position];
                            std.debug.print("type_args found: {s} '{s}'\n", .{
                                @tagName(type_tok.which),
                                type_tok.what.string,
                            });
                        }
                        std.debug.print("type_args expected:\n", .{});
                        for (type_info.expected) |expected| {
                            std.debug.print("- {s}\n", .{@tagName(expected)});
                        }
                    }
                }
            }
        } else {
            std.debug.print("parse failed without error info\n", .{});
        }
    } else {
        std.debug.print("parse ok\n", .{});
    }
}
