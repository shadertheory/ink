const std = @import("std");
const ink = @import("ink");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // --- Args Parsing ---
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: {s} <source_file>\n", .{args[0]});
        return;
    }

    const source_path = args[1];
    const source = try std.fs.cwd().readFileAlloc(allocator, source_path, 1_000_000);
    defer allocator.free(source);

    // --- Lexer Setup ---
    var lexer = try ink.lexer.init(source);
    var tokens: [1024]ink.token = undefined;
    var i: usize = 0;

    while (true) {
        if (i >= tokens.len) break;

        // Safety: handle potential null from lexer
        if (try lexer.next()) |tok| {
            tokens[i] = tok;
            i += 1;
            if (tok.which == .end_of_file) break;
        } else {
            break;
        }
    }

    // --- Parser Setup ---
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var parser = try ink.parser.parse(source, tokens[0..i], arena.allocator());

    // --- Parsing & Printing Loop ---
    while (!parser.at_end()) {
        const node = try parser.process();

        // Use the debug printer wrapper
        var branches: [128]bool = [_]bool{false} ** 128;
        print_node_tree(node, 0, branches[0..], true);
        std.debug.print("\n", .{});
    }
}

// --- Pretty Print Function (Uses std.debug.print) ---

fn print_tree_prefix(depth: usize, branches: []const bool, is_last: bool) void {
    if (depth == 0) return;

    var i: usize = 0;
    while (i + 1 < depth) : (i += 1) {
        if (branches[i]) {
            std.debug.print("│  ", .{});
        } else {
            std.debug.print("   ", .{});
        }
    }

    std.debug.print(if (is_last) "└─ " else "├─ ", .{});
}

fn print_tree_line(depth: usize, branches: []const bool, is_last: bool, comptime fmt: []const u8, args: anytype) void {
    print_tree_prefix(depth, branches, is_last);
    std.debug.print(fmt, args);
    std.debug.print("\n", .{});
}

fn print_generic_param_list(label: []const u8, params: []const ink.node.generic_param, depth: usize, branches: []bool, is_last: bool) void {
    print_tree_line(depth, branches, is_last, "{s}", .{label});
    branches[depth] = !is_last;

    if (params.len == 0) {
        print_tree_line(depth + 1, branches, true, "<none>", .{});
        return;
    }

    for (params, 0..) |param, idx| {
        const param_is_last = idx + 1 == params.len;
        print_tree_line(depth + 1, branches, param_is_last, "Param: {s}", .{param.name.string});
        branches[depth + 1] = !param_is_last;

        print_tree_line(depth + 2, branches, false, "Kind: {s}", .{switch (param.kind) {
            .type => "type",
            .value => "value",
        }});

        print_tree_line(depth + 2, branches, false, "Constraint", .{});
        branches[depth + 2] = true;
        if (param.constraint) |con| {
            print_node_tree(con, depth + 3, branches, true);
        } else {
            print_tree_line(depth + 3, branches, true, "<none>", .{});
        }

        print_tree_line(depth + 2, branches, true, "Default", .{});
        branches[depth + 2] = false;
        if (param.default) |def| {
            print_node_tree(def, depth + 3, branches, true);
        } else {
            print_tree_line(depth + 3, branches, true, "<none>", .{});
        }
    }
}

fn print_function_decl(func: ink.node.function_decl, depth: usize, branches: []bool, is_last: bool) void {
    print_tree_line(depth, branches, is_last, "Function Decl: {s}", .{func.name.string});
    branches[depth] = !is_last;

    print_generic_param_list("Generics", func.generics, depth + 1, branches, false);

    print_tree_line(depth + 1, branches, false, "Params", .{});
    branches[depth + 1] = true;
    if (func.params.len == 0) {
        print_tree_line(depth + 2, branches, true, "<none>", .{});
    } else {
        for (func.params, 0..) |param, idx| {
            const param_is_last = idx + 1 == func.params.len;
            print_tree_line(depth + 2, branches, param_is_last, "Param: {s}", .{param.name.string});
            branches[depth + 2] = !param_is_last;
            print_node_tree(param.ty, depth + 3, branches, true);
        }
    }

    print_tree_line(depth + 1, branches, false, "Return Type", .{});
    branches[depth + 1] = true;
    if (func.return_type) |rt| {
        print_node_tree(rt, depth + 2, branches, true);
    } else {
        print_tree_line(depth + 2, branches, true, "<none>", .{});
    }

    print_tree_line(depth + 1, branches, false, "Where Clause", .{});
    branches[depth + 1] = true;
    if (func.where_clause.len == 0) {
        print_tree_line(depth + 2, branches, true, "<none>", .{});
    } else {
        for (func.where_clause, 0..) |req, idx| {
            const req_is_last = idx + 1 == func.where_clause.len;
            print_tree_line(depth + 2, branches, req_is_last, "Requirement: {s}", .{req.name.string});
            branches[depth + 2] = !req_is_last;
            print_node_tree(req.constraint, depth + 3, branches, true);
        }
    }

    print_tree_line(depth + 1, branches, true, "Body", .{});
    branches[depth + 1] = false;
    if (func.body) |body| {
        print_node_tree(body, depth + 2, branches, true);
    } else {
        print_tree_line(depth + 2, branches, true, "<none>", .{});
    }
}

fn print_node_tree(node: *const ink.node, depth: usize, branches: []bool, is_last: bool) void {
    switch (node.*) {
        .integer => |val| {
            print_tree_line(depth, branches, is_last, "Integer: {}", .{val});
        },
        .float => |val| {
            print_tree_line(depth, branches, is_last, "Float: {d}", .{val});
        },
        .identifier => |name| {
            print_tree_line(depth, branches, is_last, "Identifier: \"{s}\"", .{name.string});
        },

        .decl => |decl| switch (decl) {
            .function => |func| print_function_decl(func, depth, branches, is_last),
            .trait => |trait_decl| {
                print_tree_line(depth, branches, is_last, "Trait Decl: {s}", .{trait_decl.name.string});
                branches[depth] = !is_last;

                print_generic_param_list("Generics", trait_decl.generics, depth + 1, branches, false);

                print_tree_line(depth + 1, branches, true, "Items", .{});
                branches[depth + 1] = false;
                if (trait_decl.items.len == 0) {
                    print_tree_line(depth + 2, branches, true, "<none>", .{});
                } else {
                    for (trait_decl.items, 0..) |item, idx| {
                        const item_is_last = idx + 1 == trait_decl.items.len;
                        print_tree_line(depth + 2, branches, item_is_last, "Trait Item", .{});
                        branches[depth + 2] = !item_is_last;
                        switch (item) {
                            .function => |func| print_function_decl(func, depth + 3, branches, true),
                            .assoc_type => |assoc| {
                                print_tree_line(depth + 3, branches, true, "Associated Type: {s}", .{assoc.name.string});
                                branches[depth + 3] = false;
                                print_tree_line(depth + 4, branches, true, "Value", .{});
                                branches[depth + 4] = false;
                                if (assoc.value) |val| {
                                    print_node_tree(val, depth + 5, branches, true);
                                } else {
                                    print_tree_line(depth + 5, branches, true, "<none>", .{});
                                }
                            },
                        }
                    }
                }
            },
            .concept => |concept_decl| {
                print_tree_line(depth, branches, is_last, "Concept Decl: {s}", .{concept_decl.name.string});
                branches[depth] = !is_last;

                print_generic_param_list("Generics", concept_decl.generics, depth + 1, branches, false);

                print_tree_line(depth + 1, branches, true, "Requires", .{});
                branches[depth + 1] = false;
                if (concept_decl.requires.len == 0) {
                    print_tree_line(depth + 2, branches, true, "<none>", .{});
                } else {
                    for (concept_decl.requires, 0..) |req, idx| {
                        const req_is_last = idx + 1 == concept_decl.requires.len;
                        print_node_tree(req, depth + 2, branches, req_is_last);
                    }
                }
            },
            .sum => |sum_decl| {
                print_tree_line(depth, branches, is_last, "Sum Decl: {s}", .{sum_decl.name.string});
                branches[depth] = !is_last;

                print_generic_param_list("Generics", sum_decl.generics, depth + 1, branches, false);

                print_tree_line(depth + 1, branches, true, "Variants", .{});
                branches[depth + 1] = false;
                if (sum_decl.variants.len == 0) {
                    print_tree_line(depth + 2, branches, true, "<none>", .{});
                } else {
                    for (sum_decl.variants, 0..) |variant, idx| {
                        const variant_is_last = idx + 1 == sum_decl.variants.len;
                        print_tree_line(depth + 2, branches, variant_is_last, "Variant: {s}", .{variant.name.string});
                        branches[depth + 2] = !variant_is_last;
                        print_tree_line(depth + 3, branches, true, "Payload", .{});
                        branches[depth + 3] = false;
                        if (variant.payload) |payload| {
                            print_node_tree(payload, depth + 4, branches, true);
                        } else {
                            print_tree_line(depth + 4, branches, true, "<none>", .{});
                        }
                    }
                }
            },
            .@"enum" => |enum_decl| {
                print_tree_line(depth, branches, is_last, "Enum Decl: {s}", .{enum_decl.name.string});
                branches[depth] = !is_last;

                print_tree_line(depth + 1, branches, true, "Cases", .{});
                branches[depth + 1] = false;
                if (enum_decl.cases.len == 0) {
                    print_tree_line(depth + 2, branches, true, "<none>", .{});
                } else {
                    for (enum_decl.cases, 0..) |case_id, idx| {
                        const case_is_last = idx + 1 == enum_decl.cases.len;
                        print_tree_line(depth + 2, branches, case_is_last, "Case: {s}", .{case_id.string});
                    }
                }
            },
        },

        .binary => |bin| {
            print_tree_line(depth, branches, is_last, "Binary Op: {}", .{bin.op});
            branches[depth] = !is_last;
            print_node_tree(bin.left, depth + 1, branches, false);
            print_node_tree(bin.right, depth + 1, branches, true);
        },

        .unary => |un| {
            print_tree_line(depth, branches, is_last, "Unary Op: {}", .{un.op});
            branches[depth] = !is_last;
            print_node_tree(un.right, depth + 1, branches, true);
        },

        .if_expr => |ife| {
            const has_else = ife.else_branch != null;
            print_tree_line(depth, branches, is_last, "If Expr", .{});
            branches[depth] = !is_last;

            print_tree_line(depth + 1, branches, false, "Condition", .{});
            branches[depth + 1] = true;
            print_node_tree(ife.condition, depth + 2, branches, true);

            print_tree_line(depth + 1, branches, !has_else, "Then", .{});
            branches[depth + 1] = has_else;
            print_node_tree(ife.then_branch, depth + 2, branches, true);

            if (ife.else_branch) |el| {
                print_tree_line(depth + 1, branches, true, "Else", .{});
                branches[depth + 1] = false;
                print_node_tree(el, depth + 2, branches, true);
            }
        },

        .match_expr => |me| {
            const has_arms = me.arms.len != 0;
            print_tree_line(depth, branches, is_last, "Match Expr", .{});
            branches[depth] = !is_last;

            print_tree_line(depth + 1, branches, !has_arms, "Target", .{});
            branches[depth + 1] = has_arms;
            print_node_tree(me.target, depth + 2, branches, true);

            for (me.arms, 0..) |arm, idx| {
                const arm_is_last = idx + 1 == me.arms.len;
                print_tree_line(depth + 1, branches, arm_is_last, "Arm", .{});
                branches[depth + 1] = !arm_is_last;

                print_tree_line(depth + 2, branches, false, "Pattern", .{});
                branches[depth + 2] = true;
                print_node_tree(arm.pattern, depth + 3, branches, true);

                print_tree_line(depth + 2, branches, true, "Body", .{});
                branches[depth + 2] = false;
                print_node_tree(arm.body, depth + 3, branches, true);
            }
        },

        .associate => |assoc| {
            print_tree_line(depth, branches, is_last, "Associate: {s}", .{assoc.name.string});
            branches[depth] = !is_last;
            print_tree_line(depth + 1, branches, true, "Value", .{});
            branches[depth + 1] = false;
            if (assoc.value) |val| {
                print_node_tree(val, depth + 2, branches, true);
            } else {
                print_tree_line(depth + 2, branches, true, "<none>", .{});
            }
        },

        .type => |ty| switch (ty) {
            .self => {
                print_tree_line(depth, branches, is_last, "Type: self", .{});
            },
            .name => |name| {
                print_tree_line(depth, branches, is_last, "Type: {s}", .{name.string});
            },
            .optional => |opt| {
                print_tree_line(depth, branches, is_last, "Type: optional", .{});
                branches[depth] = !is_last;
                print_node_tree(opt, depth + 1, branches, true);
            },
            .applied => |ap| {
                print_tree_line(depth, branches, is_last, "Type: applied {s}", .{ap.base.string});
                branches[depth] = !is_last;
                print_tree_line(depth + 1, branches, true, "Args", .{});
                branches[depth + 1] = false;
                if (ap.args.len == 0) {
                    print_tree_line(depth + 2, branches, true, "<none>", .{});
                } else {
                    for (ap.args, 0..) |arg, idx| {
                        const arg_is_last = idx + 1 == ap.args.len;
                        print_node_tree(arg, depth + 2, branches, arg_is_last);
                    }
                }
            },
        },
    }
}
