const std = @import("std");
const ink = @import("ink");

const array_list = std.array_list;
const heap = std.heap;

fn print_node_tree(node: *const ink.node, depth: usize) void {
    var i: usize = 0;
    while (i < depth) : (i += 1) {
        std.debug.print("  ", .{});
    }
    switch (node.*) {
        .identifier => |id| std.debug.print("ident {s}\n", .{id.string}),
        .integer => |val| std.debug.print("int {d}\n", .{val.value}),
        .binary => |bin| {
            std.debug.print("binary {s}\n", .{@tagName(bin.op)});
            print_node_tree(ink.ast.deref(bin.left), depth + 1);
            print_node_tree(ink.ast.deref(bin.right), depth + 1);
        },
        .unary => |un| {
            std.debug.print("unary {s}\n", .{@tagName(un.op)});
            print_node_tree(ink.ast.deref(un.right), depth + 1);
        },
        .if_expr => |ife| {
            std.debug.print("if_expr\n", .{});
            print_node_tree(ink.ast.deref(ife.condition), depth + 1);
            print_node_tree(ink.ast.deref(ife.then_branch), depth + 1);
            if (ife.else_branch) |ref| print_node_tree(ink.ast.deref(ref), depth + 1);
        },
        .block => |blk| {
            std.debug.print("block\n", .{});
            for (blk.items) |item_ref| {
                print_node_tree(ink.ast.deref(item_ref), depth + 1);
            }
        },
        .decl => |decl| {
            std.debug.print("decl {s}\n", .{@tagName(decl)});
        },
        else => std.debug.print("{s}\n", .{@tagName(node.*)}),
    }
}

fn line_col(source: []const u8, pos: usize) struct { line: usize, col: usize } {
    var line: usize = 1;
    var col: usize = 1;
    var i: usize = 0;
    while (i < pos and i < source.len) : (i += 1) {
        if (source[i] == '\n') {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    return .{ .line = line, .col = col };
}

fn find_identifier(node: *const ink.node, name: []const u8, source: []const u8, depth: usize) void {
    switch (node.*) {
        .identifier => |id| {
            if (std.mem.eql(u8, id.string, name)) {
                const lc = line_col(source, id.where.start);
                var i: usize = 0;
                while (i < depth) : (i += 1) {
                    std.debug.print("  ", .{});
                }
                std.debug.print("found ident {s} at {d}:{d}\n", .{ name, lc.line, lc.col });
            }
        },
        .binary => |bin| {
            find_identifier(ink.ast.deref(bin.left), name, source, depth + 1);
            find_identifier(ink.ast.deref(bin.right), name, source, depth + 1);
        },
        .unary => |un| find_identifier(ink.ast.deref(un.right), name, source, depth + 1),
        .block => |blk| {
            for (blk.items) |item_ref| {
                find_identifier(ink.ast.deref(item_ref), name, source, depth + 1);
            }
        },
        .decl => |decl| switch (decl) {
            .function => |func| if (func.body) |body_ref| find_identifier(ink.ast.deref(body_ref), name, source, depth + 1),
            .@"const" => |c| find_identifier(ink.ast.deref(c.value), name, source, depth + 1),
            .@"var" => |v| find_identifier(ink.ast.deref(v.value), name, source, depth + 1),
            else => {},
        },
        .if_expr => |ife| {
            find_identifier(ink.ast.deref(ife.condition), name, source, depth + 1);
            find_identifier(ink.ast.deref(ife.then_branch), name, source, depth + 1);
            if (ife.else_branch) |ref| find_identifier(ink.ast.deref(ref), name, source, depth + 1);
        },
        else => {},
    }
}

fn contains_identifier(node: *const ink.node, name: []const u8) bool {
    switch (node.*) {
        .identifier => |id| return std.mem.eql(u8, id.string, name),
        .binary => |bin| return contains_identifier(ink.ast.deref(bin.left), name) or
            contains_identifier(ink.ast.deref(bin.right), name),
        .unary => |un| return contains_identifier(ink.ast.deref(un.right), name),
        .block => |blk| {
            for (blk.items) |item_ref| {
                if (contains_identifier(ink.ast.deref(item_ref), name)) return true;
            }
            return false;
        },
        .decl => |decl| switch (decl) {
            .function => |func| return if (func.body) |body_ref| contains_identifier(ink.ast.deref(body_ref), name) else false,
            .@"const" => |c| return contains_identifier(ink.ast.deref(c.value), name),
            .@"var" => |v| return contains_identifier(ink.ast.deref(v.value), name),
            else => return false,
        },
        .if_expr => |ife| return contains_identifier(ink.ast.deref(ife.condition), name) or
            contains_identifier(ink.ast.deref(ife.then_branch), name) or
            (if (ife.else_branch) |ref| contains_identifier(ink.ast.deref(ref), name) else false),
        else => return false,
    }
}

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

    const print_tokens = false;
    if (print_tokens) {
        std.debug.print("tokens: {d}\n", .{tokens.items.len});
        for (tokens.items, 0..) |tok, idx| {
            std.debug.print("{d}: {s} '{s}'\n", .{ idx, @tagName(tok.which), tok.what.string });
        }
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
        var builder = ink.peg_ast.builder.init(parse.arena.allocator(), tokens.items, &parse.tree, source);
        const nodes = builder.build_program(parse.root.?) catch |err| {
            std.debug.print("ast build error: {s}\n", .{@errorName(err)});
            return;
        };
        for (nodes) |node| {
            if (node.* != .decl) continue;
            if (node.decl != .function) continue;
            const func = node.decl.function;
            if (!std.mem.eql(u8, func.name.string, "list_new") and
                !std.mem.eql(u8, func.name.string, "list_reserve") and
                !std.mem.eql(u8, func.name.string, "timeout"))
            {
                continue;
            }
            std.debug.print("function {s}\n", .{func.name.string});
            if (func.body) |body_ref| {
                const body = ink.ast.deref(body_ref);
                if (body.* != .block) {
                    std.debug.print("  body tag {s}\n", .{@tagName(body.*)});
                    continue;
                }
                find_identifier(body, "let", source, 1);
                for (body.block.items, 0..) |item_ref, idx| {
                    const item = ink.ast.deref(item_ref);
                    std.debug.print("  item {d}: {s}", .{ idx, @tagName(item.*) });
                    if (item.* == .decl) {
                        switch (item.decl) {
                            .@"const" => |c| std.debug.print(" const {s}\n", .{c.name.string}),
                            .@"var" => |v| std.debug.print(" var {s}\n", .{v.name.string}),
                            .function => |f| std.debug.print(" fn {s}\n", .{f.name.string}),
                            else => std.debug.print("\n", .{}),
                        }
                    } else if (item.* == .identifier) {
                        std.debug.print(" ident {s}\n", .{item.identifier.string});
                    } else if (item.* == .binary) {
                        const bin = item.binary;
                        std.debug.print(" op {s}\n", .{@tagName(bin.op)});
                        const left = ink.ast.deref(bin.left);
                        if (bin.op == .assign and left.* == .identifier) {
                            if (std.mem.eql(u8, left.identifier.string, "data_ptr") or
                                std.mem.eql(u8, left.identifier.string, "new_ptr"))
                            {
                                print_node_tree(item, 2);
                            }
                        }
                    } else {
                        std.debug.print("\n", .{});
                    }
                    if (contains_identifier(item, "let")) {
                        std.debug.print("    item contains ident let\n", .{});
                        print_node_tree(item, 2);
                    }
                }
            }
        }
    }
}
