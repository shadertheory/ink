const std = @import("std");
const ink = @import("ink");
const peg = @import("peg.zig");
const peg_parser = @import("peg_parser.zig");
const token = @import("token.zig").token;
const spec = @import("spec.zig");

pub const mem_allocator = std.mem.Allocator;

pub const build_error = error{
    out_of_memory,
    parse_integer,
    parse_float,
    build_failed,
};

pub const error_kind = enum {
    unexpected_node,
    unexpected_token,
    unsupported_construct,
    unsupported_operator,
    unsupported_generic_target,
    empty_block,
    multiple_statements,
    string_literal,
};

pub const error_info = struct {
    kind: error_kind,
    position: usize,
};

pub const builder = struct {
    allocator: mem_allocator,
    tokens: []const token,
    tree: *const peg_parser.parse_tree,
    last_error: ?error_info = null,

    pub fn init(allocator: mem_allocator, tokens: []const token, tree: *const peg_parser.parse_tree) builder {
        return .{
            .allocator = allocator,
            .tokens = tokens,
            .tree = tree,
            .last_error = null,
        };
    }

    pub fn build_program(self: *builder, root: peg_parser.node_id) build_error![]const *ink.node {
        if (!self.is_nonterminal(root, .program)) {
            return self.fail(.unexpected_node, root);
        }

        var items = std.array_list.Managed(*ink.node).init(self.allocator);
        for (self.child_nodes(root)) |child| {
            if (self.is_nonterminal(child, .stmt)) {
                items.append(try self.build_stmt(child)) catch return error.out_of_memory;
            }
        }
        return items.toOwnedSlice() catch return error.out_of_memory;
    }

    fn build_stmt(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        if (!self.is_nonterminal(id, .stmt)) {
            return self.fail(.unexpected_node, id);
        }

        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .decl)) {
                return self.build_decl(child);
            }
            if (self.is_nonterminal(child, .expr)) {
                return self.build_expr(child);
            }
        }
        return self.fail(.unexpected_node, id);
    }

    fn build_decl(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        if (!self.is_nonterminal(id, .decl)) {
            return self.fail(.unexpected_node, id);
        }

        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .function_decl)) return self.build_function_decl(child);
            if (self.is_nonterminal(child, .struct_decl)) return self.build_struct_decl(child);
            if (self.is_nonterminal(child, .trait_decl)) return self.build_trait_decl(child);
            if (self.is_nonterminal(child, .concept_decl)) return self.build_concept_decl(child);
            if (self.is_nonterminal(child, .sum_decl)) return self.build_sum_decl(child);
            if (self.is_nonterminal(child, .enum_decl)) return self.build_enum_decl(child);
            if (self.is_nonterminal(child, .impl_decl)) return self.build_impl_decl(child);
            if (self.is_nonterminal(child, .const_decl)) return self.build_const_decl(child);
            if (self.is_nonterminal(child, .var_decl)) return self.build_var_decl(child);
        }
        return self.fail(.unexpected_node, id);
    }

    fn build_expr(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        if (!self.is_nonterminal(id, .expr)) {
            return self.fail(.unexpected_node, id);
        }

        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .if_expr)) return self.build_if_expr(child);
            if (self.is_nonterminal(child, .match_expr)) return self.build_match_expr(child);
            if (self.is_nonterminal(child, .return_expr)) return self.build_return_expr(child);
            if (self.is_nonterminal(child, .assign)) return self.build_assign(child);
        }
        return self.fail(.unexpected_node, id);
    }

    fn build_if_expr(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const expr_children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= expr_children.len or !self.is_terminal(expr_children[idx], .expr_if)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        if (idx >= expr_children.len or !self.is_nonterminal(expr_children[idx], .expr)) {
            return self.fail(.unexpected_node, id);
        }
        const condition = try self.build_expr(expr_children[idx]);
        idx += 1;

        if (idx >= expr_children.len or !self.is_nonterminal(expr_children[idx], .branch)) {
            return self.fail(.unexpected_node, id);
        }
        const then_branch = try self.build_branch(expr_children[idx]);
        idx += 1;

        var else_branch: ?*ink.node = null;
        while (idx < expr_children.len) : (idx += 1) {
            if (self.is_terminal(expr_children[idx], .expr_else)) {
                if (idx + 1 >= expr_children.len or !self.is_nonterminal(expr_children[idx + 1], .branch)) {
                    return self.fail(.unexpected_node, id);
                }
                else_branch = try self.build_branch(expr_children[idx + 1]);
                break;
            }
        }

        return self.new_node(.{ .if_expr = .{
            .condition = ink.ast.ref(condition),
            .then_branch = ink.ast.ref(then_branch),
            .else_branch = ink.ast.ref_opt(else_branch),
        } });
    }

    fn build_branch(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .block)) {
                return self.build_block_expr(child);
            }
            if (self.is_nonterminal(child, .expr)) {
                return self.build_expr(child);
            }
        }
        return self.fail(.unexpected_node, id);
    }

    fn build_block_expr(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        var items = std.array_list.Managed(*ink.node).init(self.allocator);
        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .stmt)) {
                items.append(try self.build_stmt(child)) catch return error.out_of_memory;
            }
        }

        if (items.items.len == 0) return self.fail(.empty_block, id);
        const slice = items.toOwnedSlice() catch return error.out_of_memory;
        return self.new_node(.{ .block = .{ .items = ink.ast.ref_slice(slice) } });
    }

    fn build_match_expr(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_terminal(children[idx], .expr_match)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        if (idx >= children.len or !self.is_nonterminal(children[idx], .expr)) {
            return self.fail(.unexpected_node, id);
        }
        const target = try self.build_expr(children[idx]);
        idx += 1;

        var arms = std.array_list.Managed(ink.ast.match_arm).init(self.allocator);
        while (idx < children.len) : (idx += 1) {
            const child = children[idx];
            if (self.is_nonterminal(child, .match_arm)) {
                arms.append(try self.build_match_arm(child)) catch return error.out_of_memory;
                continue;
            }
            if (self.is_nonterminal(child, .match_arm_block)) {
                try self.collect_match_arm_block(child, &arms);
            }
        }

        return self.new_node(.{ .match_expr = .{
            .target = ink.ast.ref(target),
            .arms = arms.toOwnedSlice() catch return error.out_of_memory,
        } });
    }

    fn collect_match_arm_block(
        self: *builder,
        id: peg_parser.node_id,
        arms: *std.array_list.Managed(ink.ast.match_arm),
    ) build_error!void {
        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .match_arm)) {
                arms.append(try self.build_match_arm(child)) catch return error.out_of_memory;
            }
        }
    }

    fn build_match_arm(self: *builder, id: peg_parser.node_id) build_error!ink.ast.match_arm {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_nonterminal(children[idx], .expr)) {
            return self.fail(.unexpected_node, id);
        }
        const pattern = try self.build_expr(children[idx]);
        idx += 1;

        if (idx >= children.len or !self.is_terminal(children[idx], .arrow)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        if (idx >= children.len or !self.is_nonterminal(children[idx], .expr)) {
            return self.fail(.unexpected_node, id);
        }
        const body = try self.build_expr(children[idx]);

        return .{
            .pattern = ink.ast.ref(pattern),
            .body = ink.ast.ref(body),
        };
    }

    fn build_return_expr(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len < 2 or !self.is_terminal(children[0], .stmt_return)) {
            return self.fail(.unexpected_node, id);
        }
        if (!self.is_nonterminal(children[1], .expr)) {
            return self.fail(.unexpected_node, id);
        }
        const expr = try self.build_expr(children[1]);
        return self.new_node(.{ .unary = .{
            .op = .ret,
            .right = ink.ast.ref(expr),
        } });
    }

    fn build_assign(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_nonterminal(children[idx], .pipe)) {
            return self.fail(.unexpected_node, id);
        }
        const left = try self.build_pipe(children[idx]);
        idx += 1;

        while (idx < children.len) : (idx += 1) {
            if (self.is_terminal(children[idx], .assign)) {
                if (idx + 1 >= children.len or !self.is_nonterminal(children[idx + 1], .assign)) {
                    return self.fail(.unexpected_node, id);
                }
                const right = try self.build_assign(children[idx + 1]);
                return self.new_node(.{ .binary = .{
                    .left = ink.ast.ref(left),
                    .op = .assign,
                    .right = ink.ast.ref(right),
                } });
            }
        }

        return left;
    }

    fn build_pipe(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_nonterminal(children[idx], .coalesce)) {
            return self.fail(.unexpected_node, id);
        }
        var expr = try self.build_coalesce(children[idx]);
        idx += 1;

        while (idx < children.len) : (idx += 1) {
            const child = children[idx];
            if (self.is_terminal(child, .pipe)) {
                if (idx + 1 >= children.len or !self.is_nonterminal(children[idx + 1], .coalesce)) {
                    return self.fail(.unexpected_node, id);
                }
                const right = try self.build_coalesce(children[idx + 1]);
                expr = try self.new_node(.{ .binary = .{
                    .left = ink.ast.ref(expr),
                    .op = .pipe,
                    .right = ink.ast.ref(right),
                } });
                idx += 1;
                continue;
            }
        }
        return expr;
    }

    fn build_coalesce(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_nonterminal(children[idx], .logical_or)) {
            return self.fail(.unexpected_node, id);
        }
        var expr = try self.build_logical_or(children[idx]);
        idx += 1;

        while (idx < children.len) : (idx += 1) {
            if (self.is_terminal(children[idx], .coalesce)) {
                if (idx + 1 >= children.len or !self.is_nonterminal(children[idx + 1], .logical_or)) {
                    return self.fail(.unexpected_node, id);
                }
                const right = try self.build_logical_or(children[idx + 1]);
                expr = try self.new_node(.{ .binary = .{
                    .left = ink.ast.ref(expr),
                    .op = .coalesce,
                    .right = ink.ast.ref(right),
                } });
                idx += 1;
                continue;
            }
        }
        return expr;
    }

    fn build_logical_or(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_nonterminal(children[idx], .logical_and)) {
            return self.fail(.unexpected_node, id);
        }
        var expr = try self.build_logical_and(children[idx]);
        idx += 1;

        while (idx < children.len) : (idx += 1) {
            if (self.is_terminal(children[idx], .logical_or) or self.is_terminal(children[idx], .logical_xor)) {
                if (idx + 1 >= children.len or !self.is_nonterminal(children[idx + 1], .logical_and)) {
                    return self.fail(.unexpected_node, id);
                }
                const right = try self.build_logical_and(children[idx + 1]);
                const op: ink.binary = if (self.is_terminal(children[idx], .logical_or)) .logical_or else .logical_xor;
                expr = try self.new_node(.{ .binary = .{
                    .left = ink.ast.ref(expr),
                    .op = op,
                    .right = ink.ast.ref(right),
                } });
                idx += 1;
                continue;
            }
        }
        return expr;
    }

    fn build_logical_and(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_nonterminal(children[idx], .comparison)) {
            return self.fail(.unexpected_node, id);
        }
        var expr = try self.build_comparison(children[idx]);
        idx += 1;

        while (idx < children.len) : (idx += 1) {
            if (self.is_terminal(children[idx], .logical_and)) {
                if (idx + 1 >= children.len or !self.is_nonterminal(children[idx + 1], .comparison)) {
                    return self.fail(.unexpected_node, id);
                }
                const right = try self.build_comparison(children[idx + 1]);
                expr = try self.new_node(.{ .binary = .{
                    .left = ink.ast.ref(expr),
                    .op = .logical_and,
                    .right = ink.ast.ref(right),
                } });
                idx += 1;
                continue;
            }
        }
        return expr;
    }

    fn build_comparison(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_nonterminal(children[idx], .sum)) {
            return self.fail(.unexpected_node, id);
        }
        var expr = try self.build_sum(children[idx]);
        idx += 1;

        while (idx < children.len) : (idx += 1) {
            const child = children[idx];
            if (self.is_comparison_op(child)) {
                if (idx + 1 >= children.len or !self.is_nonterminal(children[idx + 1], .sum)) {
                    return self.fail(.unexpected_node, id);
                }
                const right = try self.build_sum(children[idx + 1]);
                const op = try self.binary_from_token(self.token_of(child).which);
                expr = try self.new_node(.{ .binary = .{
                    .left = ink.ast.ref(expr),
                    .op = op,
                    .right = ink.ast.ref(right),
                } });
                idx += 1;
                continue;
            }
        }
        return expr;
    }

    fn build_sum(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_nonterminal(children[idx], .product)) {
            return self.fail(.unexpected_node, id);
        }
        var expr = try self.build_product(children[idx]);
        idx += 1;

        while (idx < children.len) : (idx += 1) {
            const child = children[idx];
            if (self.is_terminal(child, .plus) or self.is_terminal(child, .minus)) {
                if (idx + 1 >= children.len or !self.is_nonterminal(children[idx + 1], .product)) {
                    return self.fail(.unexpected_node, id);
                }
                const right = try self.build_product(children[idx + 1]);
                const op = try self.binary_from_token(self.token_of(child).which);
                expr = try self.new_node(.{ .binary = .{
                    .left = ink.ast.ref(expr),
                    .op = op,
                    .right = ink.ast.ref(right),
                } });
                idx += 1;
                continue;
            }
        }
        return expr;
    }

    fn build_product(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_nonterminal(children[idx], .unary)) {
            return self.fail(.unexpected_node, id);
        }
        var expr = try self.build_unary(children[idx]);
        idx += 1;

        while (idx < children.len) : (idx += 1) {
            const child = children[idx];
            if (self.is_terminal(child, .asterisk) or self.is_terminal(child, .slash)) {
                if (idx + 1 >= children.len or !self.is_nonterminal(children[idx + 1], .unary)) {
                    return self.fail(.unexpected_node, id);
                }
                const right = try self.build_unary(children[idx + 1]);
                const op = try self.binary_from_token(self.token_of(child).which);
                expr = try self.new_node(.{ .binary = .{
                    .left = ink.ast.ref(expr),
                    .op = op,
                    .right = ink.ast.ref(right),
                } });
                idx += 1;
                continue;
            }
        }
        return expr;
    }

    fn build_unary(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len == 0) return self.fail(.unexpected_node, id);

        const first = children[0];
        if (self.is_terminal(first, .minus) or self.is_terminal(first, .bang) or self.is_terminal(first, .logical_not)) {
            if (children.len < 2 or !self.is_nonterminal(children[1], .unary)) {
                return self.fail(.unexpected_node, id);
            }
            const right = try self.build_unary(children[1]);
            const op = try self.unary_from_token(self.token_of(first).which);
            return self.new_node(.{ .unary = .{
                .op = op,
                .right = ink.ast.ref(right),
            } });
        }

        for (children) |child| {
            if (self.is_nonterminal(child, .postfix)) {
                return self.build_postfix(child);
            }
        }

        return self.fail(.unexpected_node, id);
    }

    fn build_postfix(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len == 0) return self.fail(.unexpected_node, id);

        var idx: usize = 0;
        if (!self.is_nonterminal(children[idx], .primary)) {
            return self.fail(.unexpected_node, id);
        }
        var expr = try self.build_primary(children[idx]);
        idx += 1;

        while (idx < children.len) : (idx += 1) {
            const child = children[idx];
            if (self.is_nonterminal(child, .generic_args)) {
                const args = try self.collect_expr_args(child);
                expr = try self.apply_generic(expr, args, child);
                continue;
            }
            if (self.is_nonterminal(child, .call_suffix)) {
                const args = try self.collect_call_args(child);
                expr = try self.apply_call(expr, args);
                continue;
            }
            if (self.is_nonterminal(child, .access_suffix)) {
                expr = try self.apply_access(expr, child);
                continue;
            }
            if (self.is_nonterminal(child, .index_suffix)) {
                expr = try self.apply_index(expr, child);
                continue;
            }
        }

        return expr;
    }

    fn collect_call_args(self: *builder, id: peg_parser.node_id) build_error![]const *ink.node {
        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .arg_list)) {
                return self.collect_expr_args(child);
            }
        }
        return &[_]*ink.node{};
    }

    fn apply_call(self: *builder, base: *ink.node, args: []const *ink.node) build_error!*ink.node {
        if (args.len == 0) {
            const unit = try self.new_node(.{ .identifier = .{ .string = "unit", .owner = .ref } });
            return self.new_node(.{ .binary = .{
                .left = ink.ast.ref(base),
                .op = .call,
                .right = ink.ast.ref(unit),
            } });
        }

        var expr = base;
        for (args) |arg| {
            expr = try self.new_node(.{ .binary = .{
                .left = ink.ast.ref(expr),
                .op = .call,
                .right = ink.ast.ref(arg),
            } });
        }
        return expr;
    }

    fn apply_generic(
        self: *builder,
        base: *ink.node,
        args: []const *ink.node,
        err_node: peg_parser.node_id,
    ) build_error!*ink.node {
        switch (base.*) {
            .identifier => |id| {
                return self.new_node(.{ .type = .{
                    .applied = .{
                        .base = id,
                        .args = ink.ast.ref_slice(args),
                    },
                } });
            },
            .type => |ty| switch (ty) {
                .name => |name| {
                    return self.new_node(.{ .type = .{
                        .applied = .{
                            .base = name,
                            .args = ink.ast.ref_slice(args),
                        },
                    } });
                },
                else => {},
            },
            else => {},
        }
        return self.fail(.unsupported_generic_target, err_node);
    }

    fn apply_access(self: *builder, base: *ink.node, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len < 2) return self.fail(.unexpected_node, id);
        if (!self.is_terminal(children[0], .dot) and
            !self.is_terminal(children[0], .double_colon) and
            !self.is_terminal(children[0], .question_dot))
        {
            return self.fail(.unexpected_node, id);
        }
        if (!self.is_terminal(children[1], .identifier)) return self.fail(.unexpected_node, id);
        const field = self.token_of(children[1]).what;
        const field_node = try self.new_node(.{ .identifier = field });
        return self.new_node(.{ .binary = .{
            .left = ink.ast.ref(base),
            .op = .access,
            .right = ink.ast.ref(field_node),
        } });
    }

    fn apply_index(self: *builder, base: *ink.node, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len < 3) return self.fail(.unexpected_node, id);
        if (!self.is_terminal(children[0], .bracket_left)) return self.fail(.unexpected_node, id);
        if (!self.is_nonterminal(children[1], .expr)) return self.fail(.unexpected_node, id);
        if (!self.is_terminal(children[2], .bracket_right)) return self.fail(.unexpected_node, id);
        const index_expr = try self.build_expr(children[1]);
        return self.new_node(.{ .binary = .{
            .left = ink.ast.ref(base),
            .op = .index,
            .right = ink.ast.ref(index_expr),
        } });
    }

    fn collect_expr_args(self: *builder, id: peg_parser.node_id) build_error![]const *ink.node {
        var args = std.array_list.Managed(*ink.node).init(self.allocator);
        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .expr)) {
                args.append(try self.build_expr(child)) catch return error.out_of_memory;
            }
        }
        return args.toOwnedSlice() catch return error.out_of_memory;
    }

    fn build_primary(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len == 0) return self.fail(.unexpected_node, id);

        const first = children[0];
        if (self.is_terminal(first, .number)) {
            return self.build_number(first);
        }
        if (self.is_terminal(first, .identifier)) {
            return self.new_node(.{ .identifier = self.token_of(first).what });
        }
        if (self.is_terminal(first, .logical_true) or self.is_terminal(first, .logical_false) or self.is_terminal(first, .this)) {
            return self.new_node(.{ .identifier = self.token_of(first).what });
        }
        if (self.is_terminal(first, .string)) {
            return self.new_node(.{ .string = self.token_of(first).what });
        }
        if (self.is_terminal(first, .paren_left)) {
            for (children) |child| {
                if (self.is_nonterminal(child, .expr)) {
                    return self.build_expr(child);
                }
            }
        }
        return self.fail(.unexpected_node, id);
    }

    fn build_number(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const slice = self.token_of(id).what;
        const is_float = std.mem.indexOfScalar(u8, slice.string, '.') != null or
            std.mem.indexOfScalar(u8, slice.string, 'e') != null or
            std.mem.indexOfScalar(u8, slice.string, 'E') != null;
        if (is_float) {
            const val = std.fmt.parseFloat(f32, slice.string) catch return error.parse_float;
            return self.new_node(.{ .float = val });
        }
        const val = std.fmt.parseInt(i64, slice.string, 10) catch return error.parse_integer;
        return self.new_node(.{ .integer = val });
    }

    fn build_function_decl(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_terminal(children[idx], .function)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        if (idx >= children.len or !self.is_terminal(children[idx], .identifier)) {
            return self.fail(.unexpected_node, id);
        }
        const name = self.token_of(children[idx]).what;
        idx += 1;

        var generics: []const ink.ast.generic_param = &.{};
        if (idx < children.len and self.is_nonterminal(children[idx], .generic_params)) {
            generics = try self.build_generic_params(children[idx]);
            idx += 1;
        }

        if (idx >= children.len or !self.is_terminal(children[idx], .paren_left)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        var params: []const ink.ast.param = &.{};
        if (idx < children.len and self.is_nonterminal(children[idx], .param_list)) {
            params = try self.build_param_list(children[idx]);
            idx += 1;
        }

        if (idx >= children.len or !self.is_terminal(children[idx], .paren_right)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        var return_type: ?*ink.node = null;
        if (idx < children.len and self.is_nonterminal(children[idx], .return_type)) {
            return_type = try self.build_return_type(children[idx]);
            idx += 1;
        }

        var where_clause: []const ink.ast.where_req = &.{};
        if (idx < children.len and self.is_nonterminal(children[idx], .where_clause)) {
            where_clause = try self.build_where_clause(children[idx]);
            idx += 1;
        }

        var body: ?*ink.node = null;
        if (idx < children.len and self.is_nonterminal(children[idx], .block)) {
            body = try self.build_block_expr(children[idx]);
        }

        return self.new_node(.{ .decl = .{ .function = .{
            .name = name,
            .generics = generics,
            .params = params,
            .return_type = ink.ast.ref_opt(return_type),
            .where_clause = where_clause,
            .body = ink.ast.ref_opt(body),
        } } });
    }

    fn build_struct_decl(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_terminal(children[idx], .@"struct")) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        if (idx >= children.len or !self.is_terminal(children[idx], .identifier)) {
            return self.fail(.unexpected_node, id);
        }
        const name = self.token_of(children[idx]).what;
        idx += 1;

        var generics: []const ink.ast.generic_param = &.{};
        if (idx < children.len and self.is_nonterminal(children[idx], .generic_params)) {
            generics = try self.build_generic_params(children[idx]);
            idx += 1;
        }

        var fields: []const ink.ast.struct_field = &.{};
        if (idx < children.len and self.is_nonterminal(children[idx], .struct_body)) {
            fields = try self.build_struct_body(children[idx]);
        }

        return self.new_node(.{ .decl = .{ .@"struct" = .{
            .name = name,
            .generics = generics,
            .fields = fields,
        } } });
    }

    fn build_trait_decl(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_terminal(children[idx], .trait)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        if (idx >= children.len or !self.is_terminal(children[idx], .identifier)) {
            return self.fail(.unexpected_node, id);
        }
        const name = self.token_of(children[idx]).what;
        idx += 1;

        var generics: []const ink.ast.generic_param = &.{};
        if (idx < children.len and self.is_nonterminal(children[idx], .generic_params)) {
            generics = try self.build_generic_params(children[idx]);
            idx += 1;
        }

        var items: []const ink.ast.trait_item = &.{};
        if (idx < children.len and self.is_nonterminal(children[idx], .trait_body)) {
            items = try self.build_trait_body(children[idx]);
        }

        return self.new_node(.{ .decl = .{ .trait = .{
            .name = name,
            .generics = generics,
            .items = items,
        } } });
    }

    fn build_concept_decl(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_terminal(children[idx], .concept)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        if (idx >= children.len or !self.is_terminal(children[idx], .identifier)) {
            return self.fail(.unexpected_node, id);
        }
        const name = self.token_of(children[idx]).what;
        idx += 1;

        var generics: []const ink.ast.generic_param = &.{};
        if (idx < children.len and self.is_nonterminal(children[idx], .generic_params)) {
            generics = try self.build_generic_params(children[idx]);
            idx += 1;
        }

        var requires: []const ink.ast.node_ref = &.{};
        if (idx < children.len and self.is_nonterminal(children[idx], .concept_body)) {
            requires = try self.build_concept_body(children[idx]);
        }

        return self.new_node(.{ .decl = .{ .concept = .{
            .name = name,
            .generics = generics,
            .requires = requires,
        } } });
    }

    fn build_sum_decl(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_terminal(children[idx], .sum)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        if (idx >= children.len or !self.is_terminal(children[idx], .identifier)) {
            return self.fail(.unexpected_node, id);
        }
        const name = self.token_of(children[idx]).what;
        idx += 1;

        var generics: []const ink.ast.generic_param = &.{};
        if (idx < children.len and self.is_nonterminal(children[idx], .generic_params)) {
            generics = try self.build_generic_params(children[idx]);
            idx += 1;
        }

        var variants: []const ink.ast.sum_variant = &.{};
        if (idx < children.len and self.is_nonterminal(children[idx], .sum_body)) {
            variants = try self.build_sum_body(children[idx]);
        }

        return self.new_node(.{ .decl = .{ .sum = .{
            .name = name,
            .generics = generics,
            .variants = variants,
        } } });
    }

    fn build_enum_decl(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_terminal(children[idx], .@"enum")) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        if (idx >= children.len or !self.is_terminal(children[idx], .identifier)) {
            return self.fail(.unexpected_node, id);
        }
        const name = self.token_of(children[idx]).what;
        idx += 1;

        var cases: []const ink.identifier = &.{};
        if (idx < children.len and self.is_nonterminal(children[idx], .enum_body)) {
            cases = try self.build_enum_body(children[idx]);
        }

        return self.new_node(.{ .decl = .{ .@"enum" = .{
            .name = name,
            .cases = cases,
        } } });
    }

    fn build_impl_decl(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_terminal(children[idx], .impl)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        if (idx >= children.len or !self.is_terminal(children[idx], .identifier)) {
            return self.fail(.unexpected_node, id);
        }
        const by_trait = self.token_of(children[idx]).what;
        idx += 1;

        if (idx >= children.len or !self.is_terminal(children[idx], .@"for")) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        if (idx >= children.len or !self.is_terminal(children[idx], .identifier)) {
            return self.fail(.unexpected_node, id);
        }
        const for_struct = self.token_of(children[idx]).what;
        idx += 1;

        var functions: []const ink.ast.function_decl = &.{};
        if (idx < children.len and self.is_nonterminal(children[idx], .impl_body)) {
            functions = try self.build_impl_body(children[idx]);
        }

        return self.new_node(.{ .decl = .{ .impl = .{
            .by_trait = by_trait,
            .for_struct = for_struct,
            .functions = functions,
        } } });
    }

    fn build_const_decl(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_terminal(children[idx], .constant)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        if (idx >= children.len or !self.is_terminal(children[idx], .identifier)) {
            return self.fail(.unexpected_node, id);
        }
        const name = self.token_of(children[idx]).what;
        idx += 1;

        var ty: ?*ink.node = null;
        if (idx < children.len and self.is_terminal(children[idx], .colon)) {
            idx += 1;
            if (idx >= children.len or !self.is_nonterminal(children[idx], .type_expr)) {
                return self.fail(.unexpected_node, id);
            }
            ty = try self.build_type_expr(children[idx]);
            idx += 1;
        }

        if (idx >= children.len or !self.is_terminal(children[idx], .assign)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        if (idx >= children.len or !self.is_nonterminal(children[idx], .expr)) {
            return self.fail(.unexpected_node, id);
        }
        const value = try self.build_expr(children[idx]);

        return self.new_node(.{ .decl = .{ .@"const" = .{
            .name = name,
            .ty = ink.ast.ref_opt(ty),
            .value = ink.ast.ref(value),
        } } });
    }

    fn build_var_decl(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_terminal(children[idx], .variable)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        if (idx >= children.len or !self.is_terminal(children[idx], .identifier)) {
            return self.fail(.unexpected_node, id);
        }
        const name = self.token_of(children[idx]).what;
        idx += 1;

        var ty: ?*ink.node = null;
        if (idx < children.len and self.is_terminal(children[idx], .colon)) {
            idx += 1;
            if (idx >= children.len or !self.is_nonterminal(children[idx], .type_expr)) {
                return self.fail(.unexpected_node, id);
            }
            ty = try self.build_type_expr(children[idx]);
            idx += 1;
        }

        if (idx >= children.len or !self.is_terminal(children[idx], .assign)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        if (idx >= children.len or !self.is_nonterminal(children[idx], .expr)) {
            return self.fail(.unexpected_node, id);
        }
        const value = try self.build_expr(children[idx]);

        return self.new_node(.{ .decl = .{ .@"var" = .{
            .name = name,
            .ty = ink.ast.ref_opt(ty),
            .value = ink.ast.ref(value),
        } } });
    }

    fn build_struct_body(self: *builder, id: peg_parser.node_id) build_error![]const ink.ast.struct_field {
        var fields = std.array_list.Managed(ink.ast.struct_field).init(self.allocator);
        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .struct_field)) {
                fields.append(try self.build_struct_field(child)) catch return error.out_of_memory;
            }
        }
        return fields.toOwnedSlice() catch return error.out_of_memory;
    }

    fn build_struct_field(self: *builder, id: peg_parser.node_id) build_error!ink.ast.struct_field {
        const children = self.child_nodes(id);
        if (children.len < 3) return self.fail(.unexpected_node, id);
        if (!self.is_terminal(children[0], .identifier)) return self.fail(.unexpected_node, id);
        if (!self.is_terminal(children[1], .colon)) return self.fail(.unexpected_node, id);
        if (!self.is_nonterminal(children[2], .type_expr)) return self.fail(.unexpected_node, id);
        const name = self.token_of(children[0]).what;
        const ty = try self.build_type_expr(children[2]);
        return .{ .name = name, .ty = ink.ast.ref(ty) };
    }

    fn build_trait_body(self: *builder, id: peg_parser.node_id) build_error![]const ink.ast.trait_item {
        var items = std.array_list.Managed(ink.ast.trait_item).init(self.allocator);
        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .trait_item)) {
                items.append(try self.build_trait_item(child)) catch return error.out_of_memory;
            }
        }
        return items.toOwnedSlice() catch return error.out_of_memory;
    }

    fn build_trait_item(self: *builder, id: peg_parser.node_id) build_error!ink.ast.trait_item {
        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .function_decl)) {
                const func_node = try self.build_function_decl(child);
                if (func_node.* != .decl or func_node.decl != .function) {
                    return self.fail(.unexpected_node, id);
                }
                return .{ .function = func_node.decl.function };
            }
            if (self.is_nonterminal(child, .assoc_type_decl)) {
                return .{ .assoc_type = try self.build_assoc_type_decl(child) };
            }
        }
        return self.fail(.unexpected_node, id);
    }

    fn build_assoc_type_decl(self: *builder, id: peg_parser.node_id) build_error!ink.ast.associated_type_decl {
        const children = self.child_nodes(id);
        if (children.len < 2 or !self.is_terminal(children[0], .type)) {
            return self.fail(.unexpected_node, id);
        }
        if (!self.is_terminal(children[1], .identifier)) {
            return self.fail(.unexpected_node, id);
        }
        const name = self.token_of(children[1]).what;
        var value: ?*ink.node = null;
        if (children.len > 2) {
            if (self.is_terminal(children[2], .assign)) {
                if (children.len < 4 or !self.is_nonterminal(children[3], .type_expr)) {
                    return self.fail(.unexpected_node, id);
                }
                value = try self.build_type_expr(children[3]);
            }
        }
        return .{ .name = name, .value = ink.ast.ref_opt(value) };
    }

    fn build_concept_body(self: *builder, id: peg_parser.node_id) build_error![]const ink.ast.node_ref {
        var reqs = std.array_list.Managed(ink.ast.node_ref).init(self.allocator);
        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .requires_clause)) {
                const req = try self.build_requires_clause(child);
                reqs.append(ink.ast.ref(req)) catch return error.out_of_memory;
            }
        }
        return reqs.toOwnedSlice() catch return error.out_of_memory;
    }

    fn build_requires_clause(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len < 2 or !self.is_terminal(children[0], .requires)) {
            return self.fail(.unexpected_node, id);
        }
        if (!self.is_nonterminal(children[1], .type_expr)) {
            return self.fail(.unexpected_node, id);
        }
        return self.build_type_expr(children[1]);
    }

    fn build_sum_body(self: *builder, id: peg_parser.node_id) build_error![]const ink.ast.sum_variant {
        var variants = std.array_list.Managed(ink.ast.sum_variant).init(self.allocator);
        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .sum_variant)) {
                variants.append(try self.build_sum_variant(child)) catch return error.out_of_memory;
            }
        }
        return variants.toOwnedSlice() catch return error.out_of_memory;
    }

    fn build_sum_variant(self: *builder, id: peg_parser.node_id) build_error!ink.ast.sum_variant {
        const children = self.child_nodes(id);
        if (children.len < 1 or !self.is_terminal(children[0], .identifier)) {
            return self.fail(.unexpected_node, id);
        }
        const name = self.token_of(children[0]).what;
        var payload: ?*ink.node = null;
        var idx: usize = 1;
        if (idx < children.len and self.is_terminal(children[idx], .paren_left)) {
            idx += 1;
            if (idx < children.len and self.is_nonterminal(children[idx], .type_expr)) {
                payload = try self.build_type_expr(children[idx]);
            }
        }
        return .{ .name = name, .payload = ink.ast.ref_opt(payload) };
    }

    fn build_enum_body(self: *builder, id: peg_parser.node_id) build_error![]const ink.identifier {
        var cases = std.array_list.Managed(ink.identifier).init(self.allocator);
        for (self.child_nodes(id)) |child| {
            if (self.is_terminal(child, .identifier)) {
                cases.append(self.token_of(child).what) catch return error.out_of_memory;
            }
        }
        return cases.toOwnedSlice() catch return error.out_of_memory;
    }

    fn build_impl_body(self: *builder, id: peg_parser.node_id) build_error![]const ink.ast.function_decl {
        var funcs = std.array_list.Managed(ink.ast.function_decl).init(self.allocator);
        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .function_decl)) {
                const func_node = try self.build_function_decl(child);
                if (func_node.* != .decl or func_node.decl != .function) {
                    return self.fail(.unexpected_node, id);
                }
                funcs.append(func_node.decl.function) catch return error.out_of_memory;
            }
        }
        return funcs.toOwnedSlice() catch return error.out_of_memory;
    }

    fn build_generic_params(self: *builder, id: peg_parser.node_id) build_error![]const ink.ast.generic_param {
        var params = std.array_list.Managed(ink.ast.generic_param).init(self.allocator);
        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .generic_param)) {
                params.append(try self.build_generic_param(child)) catch return error.out_of_memory;
            }
        }
        return params.toOwnedSlice() catch return error.out_of_memory;
    }

    fn build_generic_param(self: *builder, id: peg_parser.node_id) build_error!ink.ast.generic_param {
        const children = self.child_nodes(id);
        if (children.len < 1 or !self.is_terminal(children[0], .identifier)) {
            return self.fail(.unexpected_node, id);
        }
        const name = self.token_of(children[0]).what;
        var constraint: ?*ink.node = null;
        var default: ?*ink.node = null;
        var idx: usize = 1;

        if (idx < children.len and self.is_terminal(children[idx], .colon)) {
            if (idx + 1 >= children.len or !self.is_nonterminal(children[idx + 1], .type_expr)) {
                return self.fail(.unexpected_node, id);
            }
            constraint = try self.build_type_expr(children[idx + 1]);
            idx += 2;
        }

        if (idx < children.len and self.is_terminal(children[idx], .assign)) {
            if (idx + 1 >= children.len or !self.is_nonterminal(children[idx + 1], .type_expr)) {
                return self.fail(.unexpected_node, id);
            }
            default = try self.build_type_expr(children[idx + 1]);
        }

        var kind: ink.ast.generic_kind = .type;
        if (constraint) |c| {
            if (c.* == .type and c.type == .name) {
                if (self.is_value_kind(c.type.name.string)) {
                    kind = .value;
                }
            }
        }

        return .{
            .name = name,
            .kind = kind,
            .constraint = ink.ast.ref_opt(constraint),
            .default = ink.ast.ref_opt(default),
        };
    }

    fn build_param_list(self: *builder, id: peg_parser.node_id) build_error![]const ink.ast.param {
        var params = std.array_list.Managed(ink.ast.param).init(self.allocator);
        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .param)) {
                params.append(try self.build_param(child)) catch return error.out_of_memory;
            }
        }
        return params.toOwnedSlice() catch return error.out_of_memory;
    }

    fn build_param(self: *builder, id: peg_parser.node_id) build_error!ink.ast.param {
        const children = self.child_nodes(id);
        if (children.len < 1) return self.fail(.unexpected_node, id);

        if (self.is_terminal(children[0], .this)) {
            const name = self.token_of(children[0]).what;
            var ty: *ink.node = undefined;
            if (children.len >= 3 and self.is_terminal(children[1], .colon) and self.is_nonterminal(children[2], .type_expr)) {
                ty = try self.build_type_expr(children[2]);
            } else {
                ty = try self.new_node(.{ .type = .self });
            }
            return .{ .name = name, .ty = ink.ast.ref(ty) };
        }

        if (!self.is_terminal(children[0], .identifier)) {
            return self.fail(.unexpected_node, id);
        }
        if (children.len < 3 or !self.is_terminal(children[1], .colon) or !self.is_nonterminal(children[2], .type_expr)) {
            return self.fail(.unexpected_node, id);
        }
        const name = self.token_of(children[0]).what;
        const ty = try self.build_type_expr(children[2]);
        return .{ .name = name, .ty = ink.ast.ref(ty) };
    }

    fn build_return_type(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len < 2 or !self.is_terminal(children[0], .arrow)) {
            return self.fail(.unexpected_node, id);
        }
        if (!self.is_nonterminal(children[1], .type_expr)) {
            return self.fail(.unexpected_node, id);
        }
        return self.build_type_expr(children[1]);
    }

    fn build_where_clause(self: *builder, id: peg_parser.node_id) build_error![]const ink.ast.where_req {
        const children = self.child_nodes(id);
        if (children.len < 4 or !self.is_terminal(children[0], .where)) {
            return self.fail(.unexpected_node, id);
        }
        if (!self.is_terminal(children[1], .identifier)) return self.fail(.unexpected_node, id);
        if (!self.is_terminal(children[2], .colon)) return self.fail(.unexpected_node, id);
        if (!self.is_nonterminal(children[3], .type_expr)) return self.fail(.unexpected_node, id);
        const name = self.token_of(children[1]).what;
        const constraint = try self.build_type_expr(children[3]);
        var list = std.array_list.Managed(ink.ast.where_req).init(self.allocator);
        list.append(.{ .name = name, .constraint = ink.ast.ref(constraint) }) catch return error.out_of_memory;
        return list.toOwnedSlice() catch return error.out_of_memory;
    }

    fn build_type_expr(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len == 0) return self.fail(.unexpected_node, id);

        if (self.is_terminal(children[0], .question)) {
            if (children.len < 2 or !self.is_nonterminal(children[1], .type_expr)) {
                return self.fail(.unexpected_node, id);
            }
            const inner = try self.build_type_expr(children[1]);
            return self.new_node(.{ .type = .{ .optional = ink.ast.ref(inner) } });
        }

        if (self.is_terminal(children[0], .self)) {
            return self.new_node(.{ .type = .self });
        }

        var base: ?ink.identifier = null;
        var args: ?[]const *ink.node = null;
        for (children) |child| {
            if (self.is_nonterminal(child, .type_atom)) {
                base = try self.build_type_atom(child);
            } else if (self.is_nonterminal(child, .type_args)) {
                args = try self.collect_expr_args(child);
            }
        }

        if (base == null) return self.fail(.unexpected_node, id);
        if (args) |list| {
            return self.new_node(.{ .type = .{ .applied = .{
                .base = base.?,
                .args = ink.ast.ref_slice(list),
            } } });
        }
        return self.new_node(.{ .type = .{ .name = base.? } });
    }

    fn build_type_atom(self: *builder, id: peg_parser.node_id) build_error!ink.identifier {
        const children = self.child_nodes(id);
        if (children.len == 0) return self.fail(.unexpected_node, id);
        if (self.is_terminal(children[0], .type) or self.is_terminal(children[0], .identifier)) {
            return self.token_of(children[0]).what;
        }
        return self.fail(.unexpected_node, id);
    }

    fn is_value_kind(self: *builder, name: []const u8) bool {
        _ = self;
        inline for (spec.generic_value_kinds) |k| {
            if (std.mem.eql(u8, name, k)) return true;
        }
        return false;
    }

    fn binary_from_token(self: *builder, kind: token.kind) build_error!ink.binary {
        _ = self;
        return switch (kind) {
            .plus => .add,
            .minus => .sub,
            .asterisk => .mul,
            .slash => .div,
            .pipe => .pipe,
            .less_than => .less_than,
            .greater_than => .greater_than,
            .less_or_equal => .less_or_equal,
            .greater_or_equal => .greater_or_equal,
            .equal => .equal,
            .not_equal => .not_equal,
            else => error.build_failed,
        };
    }

    fn unary_from_token(self: *builder, kind: token.kind) build_error!ink.unary {
        _ = self;
        return switch (kind) {
            .minus => .neg,
            .bang, .logical_not => .not,
            else => error.build_failed,
        };
    }

    fn is_comparison_op(self: *builder, id: peg_parser.node_id) bool {
        if (self.tree.nodes.items[id].symbol.kind != .terminal) return false;
        const kind: token.kind = @enumFromInt(self.tree.nodes.items[id].symbol.value);
        return switch (kind) {
            .less_than, .greater_than, .less_or_equal, .greater_or_equal, .equal, .not_equal => true,
            else => false,
        };
    }

    fn new_node(self: *builder, data: ink.node) build_error!*ink.node {
        const n = self.allocator.create(ink.node) catch return error.out_of_memory;
        n.* = data;
        return n;
    }

    fn node(self: *builder, id: peg_parser.node_id) *const peg_parser.parse_tree.node {
        return &self.tree.nodes.items[id];
    }

    fn child_nodes(self: *builder, id: peg_parser.node_id) []const peg_parser.node_id {
        return self.node(id).children;
    }

    fn is_terminal(self: *builder, id: peg_parser.node_id, kind: token.kind) bool {
        const n = self.tree.nodes.items[id];
        return n.symbol.kind == .terminal and n.symbol.value == @intFromEnum(kind);
    }

    fn is_nonterminal(self: *builder, id: peg_parser.node_id, nt: peg.nonterminal_kind) bool {
        const n = self.tree.nodes.items[id];
        return n.symbol.kind == .nonterminal and n.symbol.value == @intFromEnum(nt);
    }

    fn token_of(self: *builder, id: peg_parser.node_id) token {
        const idx = self.tree.nodes.items[id].start;
        return self.tokens[idx];
    }

    fn fail(self: *builder, kind: error_kind, id: peg_parser.node_id) build_error {
        const pos = self.tree.nodes.items[id].start;
        if (self.last_error == null or pos >= self.last_error.?.position) {
            self.last_error = .{ .kind = kind, .position = pos };
        }
        return error.build_failed;
    }
};
