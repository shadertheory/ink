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
    invalid_duration_literal,
};

pub const error_info = struct {
    kind: error_kind,
    position: usize,
};

pub const builder = struct {
    allocator: mem_allocator,
    tokens: []const token,
    tree: *const peg_parser.parse_tree,
    source: []const u8,
    last_error: ?error_info = null,
    allow_interpolation: bool = true,

    pub fn init(allocator: mem_allocator, tokens: []const token, tree: *const peg_parser.parse_tree, source: []const u8) builder {
        return .{
            .allocator = allocator,
            .tokens = tokens,
            .tree = tree,
            .source = source,
            .last_error = null,
            .allow_interpolation = true,
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

    pub fn build_expr_root(self: *builder, root: peg_parser.node_id) build_error!*ink.node {
        return self.build_expr(root);
    }

    fn parse_attributes(
        self: *builder,
        children: []const peg_parser.node_id,
        idx: *usize,
    ) build_error![]const ink.ast.attribute {
        if (idx.* < children.len and self.is_nonterminal(children[idx.*], .attribute_list)) {
            const attrs = try self.build_attribute_list(children[idx.*]);
            idx.* += 1;
            return attrs;
        }
        return &[_]ink.ast.attribute{};
    }

    fn build_attribute_list(self: *builder, id: peg_parser.node_id) build_error![]const ink.ast.attribute {
        var attrs = std.array_list.Managed(ink.ast.attribute).init(self.allocator);
        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .attribute)) {
                attrs.append(try self.build_attribute(child)) catch return error.out_of_memory;
            }
        }
        return attrs.toOwnedSlice() catch return error.out_of_memory;
    }

    fn build_attribute(self: *builder, id: peg_parser.node_id) build_error!ink.ast.attribute {
        const children = self.child_nodes(id);
        var name: ?ink.identifier = null;
        var args: ?ink.identifier = null;

        for (children) |child| {
            if (self.is_name_node(child)) {
                if (name != null) return self.fail(.unexpected_node, id);
                name = self.name_token_of(child).?.what;
                continue;
            }
            if (self.is_nonterminal(child, .attribute_args)) {
                args = try self.collect_attribute_args(child);
                continue;
            }
        }

        if (name == null) return self.fail(.unexpected_node, id);

        return .{
            .name = name.?,
            .args = args,
            .where = self.node_location(id),
        };
    }

    fn collect_attribute_args(self: *builder, id: peg_parser.node_id) build_error!?ink.identifier {
        const children = self.child_nodes(id);
        var left: ?token = null;
        var right: ?token = null;
        for (children) |child| {
            if (self.is_terminal(child, .paren_left)) {
                left = self.token_of(child);
                continue;
            }
            if (self.is_terminal(child, .paren_right)) {
                right = self.token_of(child);
                continue;
            }
        }
        if (left == null or right == null) return null;
        const start = left.?.where.end;
        const end = right.?.where.start;
        if (end > start and start < self.source.len) {
            const slice_end = if (end > self.source.len) self.source.len else end;
            if (slice_end > start) {
                const raw = self.source[start..slice_end];
                const trimmed = std.mem.trim(u8, raw, " \t\r\n");
                if (trimmed.len != 0) {
                    return .{
                        .string = raw,
                        .owner = .ref,
                        .where = .{ .start = start, .end = slice_end },
                    };
                }
            }
        }

        return self.collect_attribute_args_from_tokens(left.?, right.?);
    }

    fn collect_attribute_args_from_tokens(
        self: *builder,
        left: token,
        right: token,
    ) build_error!?ink.identifier {
        if (right.where.start <= left.where.end) return null;

        var buf = std.ArrayList(u8).empty;
        errdefer buf.deinit(self.allocator);
        const writer = buf.writer(self.allocator);

        var first = true;
        for (self.tokens) |tok| {
            if (tok.where.start < left.where.end or tok.where.end > right.where.start) continue;
            switch (tok.which) {
                .end_of_file, .illegal, .new_line, .indent, .dedent => continue,
                else => {},
            }
            if (!first) {
                writer.writeByte(' ') catch return error.out_of_memory;
            }
            first = false;
            try write_attribute_arg_token(writer, tok);
        }

        if (first) return null;
        const owned = buf.toOwnedSlice(self.allocator) catch return error.out_of_memory;
        return .{
            .string = owned,
            .owner = .ref,
            .where = .{ .start = left.where.end, .end = right.where.start },
        };
    }

    fn write_attribute_arg_token(writer: anytype, tok: token) build_error!void {
        switch (tok.which) {
            .identifier, .label, .number => return writer.writeAll(tok.what.string) catch return error.out_of_memory,
            .string => {
                writer.writeByte('"') catch return error.out_of_memory;
                writer.writeAll(tok.what.string) catch return error.out_of_memory;
                return writer.writeByte('"') catch return error.out_of_memory;
            },
            else => {},
        }
        if (token_lexeme(tok.which)) |lex| {
            return writer.writeAll(lex) catch return error.out_of_memory;
        }
    }

    fn token_lexeme(kind: token.kind) ?[]const u8 {
        inline for (spec.keyword_lexemes) |lex| {
            if (kind == @field(token.kind, lex.kind)) return lex.text;
        }
        inline for (spec.symbol_lexemes) |lex| {
            if (kind == @field(token.kind, lex.kind)) return lex.text;
        }
        return null;
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
            if (self.is_nonterminal(child, .enum_decl)) return self.build_enum_decl(child);
            if (self.is_nonterminal(child, .impl_decl)) return self.build_impl_decl(child);
            if (self.is_nonterminal(child, .import_decl)) return self.build_import_decl(child);
            if (self.is_nonterminal(child, .const_decl)) return self.build_const_decl(child);
            if (self.is_nonterminal(child, .var_decl)) return self.build_var_decl(child);
            if (self.is_nonterminal(child, .type_decl)) return self.build_type_decl(child);
        }
        return self.fail(.unexpected_node, id);
    }

    fn build_expr(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        if (!self.is_nonterminal(id, .expr)) {
            return self.fail(.unexpected_node, id);
        }

        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .label_expr)) return self.build_label_expr(child);
            if (self.is_nonterminal(child, .if_expr)) return self.build_if_expr(child);
            if (self.is_nonterminal(child, .match_expr)) return self.build_match_expr(child);
            if (self.is_nonterminal(child, .select_expr)) return self.build_select_expr(child);
            if (self.is_nonterminal(child, .with_expr)) return self.build_with_expr(child);
            if (self.is_nonterminal(child, .loop_expr)) return self.build_loop_expr(child);
            if (self.is_nonterminal(child, .while_in_expr)) return self.build_while_in_expr(child);
            if (self.is_nonterminal(child, .while_expr)) return self.build_while_expr(child);
            if (self.is_nonterminal(child, .until_expr)) return self.build_until_expr(child);
            if (self.is_nonterminal(child, .repeat_expr)) return self.build_repeat_expr(child);
            if (self.is_nonterminal(child, .for_expr)) return self.build_for_expr(child);
            if (self.is_nonterminal(child, .each_expr)) return self.build_each_expr(child);
            if (self.is_nonterminal(child, .break_expr)) return self.build_break_expr(child);
            if (self.is_nonterminal(child, .continue_expr)) return self.build_continue_expr(child);
            if (self.is_nonterminal(child, .yield_expr)) return self.build_yield_expr(child);
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

    fn build_with_expr(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_terminal(children[idx], .with)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        while (idx < children.len and self.is_nonterminal(children[idx], .layout)) : (idx += 1) {}
        const arena_token = if (idx < children.len) self.name_token_of(children[idx]) else null;
        if (arena_token == null) {
            return self.fail(.unexpected_node, id);
        }
        const arena_name = arena_token.?.what;
        if (!std.mem.eql(u8, arena_name.string, "arena")) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        while (idx < children.len and self.is_nonterminal(children[idx], .layout)) : (idx += 1) {}
        const name_token = if (idx < children.len) self.name_token_of(children[idx]) else null;
        if (name_token == null) {
            return self.fail(.unexpected_node, id);
        }
        const name = name_token.?.what;
        idx += 1;

        while (idx < children.len and self.is_nonterminal(children[idx], .layout)) : (idx += 1) {}
        if (idx >= children.len or !self.is_nonterminal(children[idx], .branch)) {
            return self.fail(.unexpected_node, id);
        }
        const body = try self.build_branch(children[idx]);

        return self.new_node(.{ .with_expr = .{
            .name = name,
            .body = ink.ast.ref(body),
        } });
    }

    fn build_label_expr(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_terminal(children[idx], .label)) {
            return self.fail(.unexpected_node, id);
        }
        const name = self.token_of(children[idx]).what;
        idx += 1;

        while (idx < children.len and self.is_nonterminal(children[idx], .layout)) : (idx += 1) {}
        if (idx >= children.len) return self.fail(.unexpected_node, id);

        var body: ?*ink.node = null;
        const body_id = children[idx];
        if (self.is_nonterminal(body_id, .loop_expr)) body = try self.build_loop_expr(body_id);
        if (self.is_nonterminal(body_id, .while_in_expr)) body = try self.build_while_in_expr(body_id);
        if (self.is_nonterminal(body_id, .while_expr)) body = try self.build_while_expr(body_id);
        if (self.is_nonterminal(body_id, .until_expr)) body = try self.build_until_expr(body_id);
        if (self.is_nonterminal(body_id, .repeat_expr)) body = try self.build_repeat_expr(body_id);
        if (self.is_nonterminal(body_id, .for_expr)) body = try self.build_for_expr(body_id);
        if (self.is_nonterminal(body_id, .each_expr)) body = try self.build_each_expr(body_id);
        if (self.is_nonterminal(body_id, .block)) body = try self.build_block_expr(body_id);

        if (body == null) return self.fail(.unexpected_node, id);
        return self.new_node(.{ .label_expr = .{
            .name = name,
            .body = ink.ast.ref(body.?),
        } });
    }

    fn build_loop_expr(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len < 2 or !self.is_terminal(children[0], .loop)) {
            return self.fail(.unexpected_node, id);
        }
        if (!self.is_nonterminal(children[1], .branch)) {
            return self.fail(.unexpected_node, id);
        }
        const body = try self.build_branch(children[1]);
        return self.new_node(.{ .loop_expr = .{
            .body = ink.ast.ref(body),
        } });
    }

    fn build_while_expr(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len < 3 or !self.is_terminal(children[0], .@"while")) {
            return self.fail(.unexpected_node, id);
        }
        if (!self.is_nonterminal(children[1], .expr) or !self.is_nonterminal(children[2], .branch)) {
            return self.fail(.unexpected_node, id);
        }
        const condition = try self.build_expr(children[1]);
        const body = try self.build_branch(children[2]);
        return self.new_node(.{ .while_expr = .{
            .condition = ink.ast.ref(condition),
            .body = ink.ast.ref(body),
        } });
    }

    fn build_while_in_expr(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len < 5 or !self.is_terminal(children[0], .@"while")) {
            return self.fail(.unexpected_node, id);
        }
        if (!self.is_nonterminal(children[1], .pattern)) return self.fail(.unexpected_node, id);
        if (!self.is_terminal(children[2], .in)) return self.fail(.unexpected_node, id);
        if (!self.is_nonterminal(children[3], .expr)) return self.fail(.unexpected_node, id);
        if (!self.is_nonterminal(children[4], .branch)) return self.fail(.unexpected_node, id);

        const pattern = try self.build_pattern(children[1]);
        const iter = try self.build_expr(children[3]);
        const body = try self.build_branch(children[4]);
        return self.new_node(.{ .while_in_expr = .{
            .pattern = ink.ast.ref(pattern),
            .iter = ink.ast.ref(iter),
            .body = ink.ast.ref(body),
        } });
    }

    fn build_until_expr(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len < 3 or !self.is_terminal(children[0], .until)) {
            return self.fail(.unexpected_node, id);
        }
        if (!self.is_nonterminal(children[1], .expr) or !self.is_nonterminal(children[2], .branch)) {
            return self.fail(.unexpected_node, id);
        }
        const condition = try self.build_expr(children[1]);
        const body = try self.build_branch(children[2]);
        return self.new_node(.{ .until_expr = .{
            .condition = ink.ast.ref(condition),
            .body = ink.ast.ref(body),
        } });
    }

    fn build_repeat_expr(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len < 3 or !self.is_terminal(children[0], .repeat)) {
            return self.fail(.unexpected_node, id);
        }
        if (!self.is_nonterminal(children[1], .expr) or !self.is_nonterminal(children[2], .branch)) {
            return self.fail(.unexpected_node, id);
        }
        const count = try self.build_expr(children[1]);
        const body = try self.build_branch(children[2]);
        return self.new_node(.{ .repeat_expr = .{
            .count = ink.ast.ref(count),
            .body = ink.ast.ref(body),
        } });
    }

    fn build_for_expr(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len < 5 or !self.is_terminal(children[0], .@"for")) {
            return self.fail(.unexpected_node, id);
        }
        if (!self.is_nonterminal(children[1], .pattern)) return self.fail(.unexpected_node, id);
        if (!self.is_terminal(children[2], .in)) return self.fail(.unexpected_node, id);
        if (!self.is_nonterminal(children[3], .expr)) return self.fail(.unexpected_node, id);
        if (!self.is_nonterminal(children[4], .branch)) return self.fail(.unexpected_node, id);

        const pattern = try self.build_pattern(children[1]);
        const iter = try self.build_expr(children[3]);
        const body = try self.build_branch(children[4]);
        return self.new_node(.{ .for_expr = .{
            .pattern = ink.ast.ref(pattern),
            .iter = ink.ast.ref(iter),
            .body = ink.ast.ref(body),
        } });
    }

    fn build_each_expr(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len < 5 or !self.is_terminal(children[0], .each)) {
            return self.fail(.unexpected_node, id);
        }
        if (!self.is_nonterminal(children[1], .pattern)) return self.fail(.unexpected_node, id);
        if (!self.is_terminal(children[2], .in)) return self.fail(.unexpected_node, id);
        if (!self.is_nonterminal(children[3], .expr)) return self.fail(.unexpected_node, id);
        if (!self.is_nonterminal(children[4], .branch)) return self.fail(.unexpected_node, id);

        const pattern = try self.build_pattern(children[1]);
        const iter = try self.build_expr(children[3]);
        const body = try self.build_branch(children[4]);
        return self.new_node(.{ .each_expr = .{
            .pattern = ink.ast.ref(pattern),
            .iter = ink.ast.ref(iter),
            .body = ink.ast.ref(body),
        } });
    }

    fn build_break_expr(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len == 0 or !self.is_terminal(children[0], .stmt_break)) {
            return self.fail(.unexpected_node, id);
        }
        var label: ?ink.identifier = null;
        var value: ?*ink.node = null;

        for (children[1..]) |child| {
            if (self.is_terminal(child, .label)) {
                if (label != null) return self.fail(.unexpected_node, id);
                label = self.token_of(child).what;
                continue;
            }
            if (self.is_nonterminal(child, .expr)) {
                if (value != null) return self.fail(.unexpected_node, id);
                value = try self.build_expr(child);
                continue;
            }
        }

        return self.new_node(.{ .break_expr = .{
            .label = label,
            .value = ink.ast.ref_opt(value),
        } });
    }

    fn build_continue_expr(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len == 0 or !self.is_terminal(children[0], .stmt_continue)) {
            return self.fail(.unexpected_node, id);
        }
        var label: ?ink.identifier = null;
        if (children.len > 1 and self.is_terminal(children[1], .label)) {
            label = self.token_of(children[1]).what;
        }
        return self.new_node(.{ .continue_expr = .{ .label = label } });
    }

    fn build_yield_expr(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len == 0 or !self.is_terminal(children[0], .yield)) {
            return self.fail(.unexpected_node, id);
        }
        var value: ?*ink.node = null;
        for (children[1..]) |child| {
            if (self.is_nonterminal(child, .expr)) {
                value = try self.build_expr(child);
                break;
            }
        }
        return self.new_node(.{ .yield_expr = .{ .value = ink.ast.ref_opt(value) } });
    }

    fn build_atomic_expr(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len < 3 or !self.is_terminal(children[0], .atomic)) {
            return self.fail(.unexpected_node, id);
        }
        var value: ?*ink.node = null;
        var ordering: ?ink.identifier = null;
        for (children[1..]) |child| {
            if (self.is_nonterminal(child, .expr)) {
                if (value != null) return self.fail(.unexpected_node, id);
                value = try self.build_expr(child);
                continue;
            }
            if (self.is_name_node(child)) {
                if (ordering != null) return self.fail(.unexpected_node, id);
                ordering = self.name_token_of(child).?.what;
                continue;
            }
        }
        if (value == null or ordering == null) return self.fail(.unexpected_node, id);
        return self.new_node(.{ .atomic_expr = .{
            .value = ink.ast.ref(value.?),
            .ordering = ordering.?,
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

    fn build_record_expr(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len == 0) return self.fail(.unexpected_node, id);

        for (children) |child| {
            if (self.is_nonterminal(child, .record_block)) {
                return self.build_record_node_from_block(child);
            }
        }

        return self.fail(.unexpected_node, id);
    }

    fn build_record_node_from_block(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const items = try self.build_record_block(id);
        return self.new_node(.{ .record = .{ .items = items } });
    }

    fn build_record_block(self: *builder, id: peg_parser.node_id) build_error![]const ink.ast.associate {
        var items = std.array_list.Managed(ink.ast.associate).init(self.allocator);
        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .record_field)) {
                items.append(try self.build_record_field(child)) catch return error.out_of_memory;
            }
        }
        return items.toOwnedSlice() catch return error.out_of_memory;
    }

    fn build_record_field(self: *builder, id: peg_parser.node_id) build_error!ink.ast.associate {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        const name_token = if (idx < children.len) self.name_token_of(children[idx]) else null;
        if (name_token == null) {
            return self.fail(.unexpected_node, id);
        }
        const name = name_token.?.what;
        idx += 1;

        if (idx >= children.len or !self.is_terminal(children[idx], .assign)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        if (idx >= children.len or !self.is_nonterminal(children[idx], .expr)) {
            return self.fail(.unexpected_node, id);
        }
        const value = try self.build_expr(children[idx]);
        return .{ .name = name, .value = ink.ast.ref_opt(value) };
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

    fn build_select_expr(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_terminal(children[idx], .expr_select)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        var arms = std.array_list.Managed(ink.ast.select_arm).init(self.allocator);
        while (idx < children.len) : (idx += 1) {
            const child = children[idx];
            if (self.is_nonterminal(child, .select_arm)) {
                arms.append(try self.build_select_arm(child)) catch return error.out_of_memory;
                continue;
            }
            if (self.is_nonterminal(child, .select_arm_block)) {
                try self.collect_select_arm_block(child, &arms);
            }
        }

        return self.new_node(.{ .select_expr = .{
            .arms = arms.toOwnedSlice() catch return error.out_of_memory,
        } });
    }

    fn collect_select_arm_block(
        self: *builder,
        id: peg_parser.node_id,
        arms: *std.array_list.Managed(ink.ast.select_arm),
    ) build_error!void {
        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .select_arm)) {
                arms.append(try self.build_select_arm(child)) catch return error.out_of_memory;
            }
        }
    }

    fn build_select_arm(self: *builder, id: peg_parser.node_id) build_error!ink.ast.select_arm {
        const children = self.child_nodes(id);
        var idx: usize = 0;

        if (idx >= children.len or !self.is_terminal(children[idx], .case)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        while (idx < children.len and self.is_nonterminal(children[idx], .layout)) : (idx += 1) {}

        var detached = false;
        if (idx < children.len and self.is_terminal(children[idx], .detached)) {
            detached = true;
            idx += 1;
            while (idx < children.len and self.is_nonterminal(children[idx], .layout)) : (idx += 1) {}
        }

        const name_token = if (idx < children.len) self.name_token_of(children[idx]) else null;
        if (name_token == null) {
            return self.fail(.unexpected_node, id);
        }
        const name_value = name_token.?.what;
        const name = if (std.mem.eql(u8, name_value.string, "_")) null else name_value;
        idx += 1;

        while (idx < children.len and self.is_nonterminal(children[idx], .layout)) : (idx += 1) {}
        if (idx >= children.len or !self.is_terminal(children[idx], .assign)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        while (idx < children.len and self.is_nonterminal(children[idx], .layout)) : (idx += 1) {}
        if (idx >= children.len or !self.is_terminal(children[idx], .await)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        while (idx < children.len and self.is_nonterminal(children[idx], .layout)) : (idx += 1) {}
        if (idx >= children.len or !self.is_nonterminal(children[idx], .expr)) {
            return self.fail(.unexpected_node, id);
        }
        const task_expr = try self.build_expr(children[idx]);
        idx += 1;

        while (idx < children.len and self.is_nonterminal(children[idx], .layout)) : (idx += 1) {}
        if (idx >= children.len or !self.is_terminal(children[idx], .arrow)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        while (idx < children.len and self.is_nonterminal(children[idx], .layout)) : (idx += 1) {}
        if (idx >= children.len or !self.is_nonterminal(children[idx], .expr)) {
            return self.fail(.unexpected_node, id);
        }
        const body_expr = try self.build_expr(children[idx]);

        return .{
            .name = name,
            .task = ink.ast.ref(task_expr),
            .body = ink.ast.ref(body_expr),
            .detached = detached,
        };
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
        var pattern: ?*ink.node = null;
        var body: ?*ink.node = null;
        var saw_arrow = false;

        for (children) |child| {
            if (self.is_nonterminal(child, .layout)) continue;
            if (self.is_terminal(child, .arrow)) {
                if (saw_arrow) return self.fail(.unexpected_node, id);
                saw_arrow = true;
                continue;
            }
            if (!saw_arrow and self.is_nonterminal(child, .pattern)) {
                if (pattern != null) return self.fail(.unexpected_node, id);
                pattern = try self.build_pattern(child);
                continue;
            }
            if (saw_arrow and self.is_nonterminal(child, .expr)) {
                if (body != null) return self.fail(.unexpected_node, id);
                body = try self.build_expr(child);
                continue;
            }
        }

        if (pattern == null or body == null or !saw_arrow) {
            return self.fail(.unexpected_node, id);
        }

        return .{
            .pattern = ink.ast.ref(pattern.?),
            .body = ink.ast.ref(body.?),
        };
    }

    fn build_pattern(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .pattern_prefix)) {
                return self.build_pattern_prefix(child);
            }
            if (self.is_nonterminal(child, .pattern_postfix)) {
                return self.build_pattern_postfix(child);
            }
            if (self.is_nonterminal(child, .pattern_primary)) {
                return self.build_pattern_primary(child);
            }
        }
        return self.fail(.unexpected_node, id);
    }

    fn build_pattern_prefix(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .pattern_ref)) {
                return self.build_pattern_ref(child);
            }
            if (self.is_nonterminal(child, .pattern_postfix)) {
                return self.build_pattern_postfix(child);
            }
        }
        return self.fail(.unexpected_node, id);
    }

    fn build_pattern_ref(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len < 2 or !self.is_terminal(children[0], .ref)) {
            return self.fail(.unexpected_node, id);
        }

        var idx: usize = 1;
        var op: ink.unary = .ref;
        if (idx < children.len and self.is_terminal(children[idx], .mut)) {
            op = .ref_mut;
            idx += 1;
        }

        if (idx >= children.len or !self.is_nonterminal(children[idx], .pattern)) {
            return self.fail(.unexpected_node, id);
        }

        const right = try self.build_pattern(children[idx]);
        return self.new_node(.{ .unary = .{
            .op = op,
            .right = ink.ast.ref(right),
        } });
    }

    fn build_pattern_postfix(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len == 0) return self.fail(.unexpected_node, id);

        var idx: usize = 0;
        if (!self.is_nonterminal(children[idx], .pattern_primary)) {
            return self.fail(.unexpected_node, id);
        }
        var expr = try self.build_pattern_primary(children[idx]);
        idx += 1;

        while (idx < children.len) : (idx += 1) {
            const child = children[idx];
            if (self.is_nonterminal(child, .pattern_call_suffix)) {
                const args = try self.collect_pattern_call_args(child);
                expr = try self.apply_call(expr, args);
                continue;
            }
            if (self.is_nonterminal(child, .pattern_access_suffix)) {
                expr = try self.apply_access(expr, child);
                continue;
            }
        }

        return expr;
    }

    fn collect_pattern_call_args(self: *builder, id: peg_parser.node_id) build_error![]const *ink.node {
        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .pattern_arg_list)) {
                return self.collect_pattern_args(child);
            }
        }
        return &[_]*ink.node{};
    }

    fn collect_pattern_args(self: *builder, id: peg_parser.node_id) build_error![]const *ink.node {
        var args = std.array_list.Managed(*ink.node).init(self.allocator);
        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .pattern)) {
                args.append(try self.build_pattern(child)) catch return error.out_of_memory;
            }
        }
        return args.toOwnedSlice() catch return error.out_of_memory;
    }

    fn build_pattern_primary(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len == 0) return self.fail(.unexpected_node, id);

        for (children) |child| {
            if (self.is_nonterminal(child, .pattern_group)) {
                return self.build_pattern_group(child);
            }
        }

        const first = children[0];
        if (self.is_terminal(first, .number)) {
            return self.build_number(first);
        }
        if (self.is_terminal(first, .logical_true) or self.is_terminal(first, .logical_false) or self.is_terminal(first, .this)) {
            return self.new_node(.{ .identifier = self.token_of(first).what });
        }
        if (self.is_terminal(first, .asterisk)) {
            return self.new_node(.{ .identifier = self.token_of(first).what });
        }
        if (self.is_name_node(first)) {
            return self.new_node(.{ .identifier = self.name_token_of(first).?.what });
        }
        if (self.is_terminal(first, .string)) {
            const value = self.token_of(first).what;
            if (try self.build_interpolated_arg(value)) |interp| return interp;
            return self.new_node(.{ .string = value });
        }

        return self.fail(.unexpected_node, id);
    }

    fn build_pattern_group(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .pattern)) {
                return self.build_pattern(child);
            }
        }
        return self.fail(.unexpected_node, id);
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
            if (self.is_assignment_op(children[idx]) or self.is_nonterminal(children[idx], .assign_op)) {
                if (idx + 1 >= children.len or !self.is_nonterminal(children[idx + 1], .assign)) {
                    return self.fail(.unexpected_node, id);
                }
                const right = try self.build_assign(children[idx + 1]);
                const op = try self.assignment_from_node(children[idx]);
                return self.new_node(.{ .binary = .{
                    .left = ink.ast.ref(left),
                    .op = op,
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
        if (idx >= children.len or !self.is_nonterminal(children[idx], .bitwise_or)) {
            return self.fail(.unexpected_node, id);
        }
        var expr = try self.build_bitwise_or(children[idx]);
        idx += 1;

        while (idx < children.len) : (idx += 1) {
            if (self.is_terminal(children[idx], .logical_and)) {
                if (idx + 1 >= children.len or !self.is_nonterminal(children[idx + 1], .bitwise_or)) {
                    return self.fail(.unexpected_node, id);
                }
                const right = try self.build_bitwise_or(children[idx + 1]);
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

    fn build_bitwise_or(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_nonterminal(children[idx], .bitwise_xor)) {
            return self.fail(.unexpected_node, id);
        }
        var expr = try self.build_bitwise_xor(children[idx]);
        idx += 1;

        while (idx < children.len) : (idx += 1) {
            const child = children[idx];
            if (self.is_terminal(child, .bar)) {
                if (idx + 1 >= children.len or !self.is_nonterminal(children[idx + 1], .bitwise_xor)) {
                    return self.fail(.unexpected_node, id);
                }
                const right = try self.build_bitwise_xor(children[idx + 1]);
                expr = try self.new_node(.{ .binary = .{
                    .left = ink.ast.ref(expr),
                    .op = .bit_or,
                    .right = ink.ast.ref(right),
                } });
                idx += 1;
                continue;
            }
        }
        return expr;
    }

    fn build_bitwise_xor(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_nonterminal(children[idx], .bitwise_and)) {
            return self.fail(.unexpected_node, id);
        }
        var expr = try self.build_bitwise_and(children[idx]);
        idx += 1;

        while (idx < children.len) : (idx += 1) {
            const child = children[idx];
            if (self.is_terminal(child, .caret)) {
                if (idx + 1 >= children.len or !self.is_nonterminal(children[idx + 1], .bitwise_and)) {
                    return self.fail(.unexpected_node, id);
                }
                const right = try self.build_bitwise_and(children[idx + 1]);
                expr = try self.new_node(.{ .binary = .{
                    .left = ink.ast.ref(expr),
                    .op = .bit_xor,
                    .right = ink.ast.ref(right),
                } });
                idx += 1;
                continue;
            }
        }
        return expr;
    }

    fn build_bitwise_and(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_nonterminal(children[idx], .comparison)) {
            return self.fail(.unexpected_node, id);
        }
        var expr = try self.build_comparison(children[idx]);
        idx += 1;

        while (idx < children.len) : (idx += 1) {
            const child = children[idx];
            if (self.is_terminal(child, .ampersand)) {
                if (idx + 1 >= children.len or !self.is_nonterminal(children[idx + 1], .comparison)) {
                    return self.fail(.unexpected_node, id);
                }
                const right = try self.build_comparison(children[idx + 1]);
                expr = try self.new_node(.{ .binary = .{
                    .left = ink.ast.ref(expr),
                    .op = .bit_and,
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
        if (idx >= children.len or !self.is_nonterminal(children[idx], .shift)) {
            return self.fail(.unexpected_node, id);
        }
        var expr = try self.build_shift(children[idx]);
        idx += 1;

        while (idx < children.len) : (idx += 1) {
            const child = children[idx];
            if (self.is_comparison_op(child)) {
                if (idx + 1 >= children.len or !self.is_nonterminal(children[idx + 1], .shift)) {
                    return self.fail(.unexpected_node, id);
                }
                const right = try self.build_shift(children[idx + 1]);
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

    fn build_shift(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_nonterminal(children[idx], .sum)) {
            return self.fail(.unexpected_node, id);
        }
        var expr = try self.build_sum(children[idx]);
        idx += 1;

        while (idx < children.len) : (idx += 1) {
            const child = children[idx];
            if (self.is_terminal(child, .shift_left) or self.is_terminal(child, .shift_right)) {
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
            if (self.is_terminal(child, .asterisk) or self.is_terminal(child, .slash) or self.is_terminal(child, .percent)) {
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
        if (self.is_terminal(first, .ampersand)) {
            var idx: usize = 1;
            var op: ink.unary = .borrow;
            if (idx < children.len and self.is_terminal(children[idx], .mut)) {
                op = .borrow_mut;
                idx += 1;
            }
            if (idx >= children.len or !self.is_nonterminal(children[idx], .unary)) {
                return self.fail(.unexpected_node, id);
            }
            const right = try self.build_unary(children[idx]);
            return self.new_node(.{ .unary = .{
                .op = op,
                .right = ink.ast.ref(right),
            } });
        }

        if (self.is_terminal(first, .minus) or self.is_terminal(first, .bang) or self.is_terminal(first, .logical_not) or
            self.is_terminal(first, .tilde) or self.is_terminal(first, .asterisk) or self.is_terminal(first, .dynamic) or
            self.is_terminal(first, .@"comptime") or self.is_terminal(first, .box) or self.is_terminal(first, .sleep) or
            self.is_terminal(first, .timeout) or self.is_terminal(first, .deadline) or self.is_terminal(first, .spawn) or
            self.is_terminal(first, .await) or self.is_terminal(first, .@"try"))
        {
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
            if (self.is_nonterminal(child, .cast_suffix)) {
                const cast_children = self.child_nodes(child);
                var ty_node: ?*ink.node = null;
                for (cast_children) |cast_child| {
                    if (self.is_nonterminal(cast_child, .type_expr)) {
                        ty_node = try self.build_type_expr(cast_child);
                        break;
                    }
                }
                if (ty_node == null) return self.fail(.unexpected_node, child);
                expr = try self.new_node(.{ .binary = .{
                    .left = ink.ast.ref(expr),
                    .op = .@"as",
                    .right = ink.ast.ref(ty_node.?),
                } });
                continue;
            }
            if (self.is_nonterminal(child, .index_suffix)) {
                expr = try self.apply_index(expr, child);
                continue;
            }
            if (self.is_nonterminal(child, .record_block)) {
                const record_node = try self.build_record_node_from_block(child);
                var args = [_]*ink.node{record_node};
                expr = try self.apply_call(expr, args[0..]);
                continue;
            }
            if (self.is_terminal(child, .question)) {
                expr = try self.new_node(.{ .unary = .{
                    .op = .unwrap_optional,
                    .right = ink.ast.ref(expr),
                } });
                continue;
            }
            if (self.is_nonterminal(child, .macro_suffix)) {
                const body = try self.macro_body_location(child);
                const where = self.node_location(id);
                expr = try self.new_node(.{ .macro_call = .{
                    .target = ink.ast.ref(expr),
                    .body = body,
                    .where = where,
                } });
                continue;
            }
        }

        return expr;
    }

    fn macro_body_location(self: *builder, id: peg_parser.node_id) build_error!ink.location {
        var block_id = id;
        if (self.is_nonterminal(id, .macro_suffix)) {
            for (self.child_nodes(id)) |child| {
                if (self.is_nonterminal(child, .macro_block)) {
                    block_id = child;
                    break;
                }
            }
        }
        const children = self.child_nodes(block_id);
        var indent_tok: ?token = null;
        var dedent_tok: ?token = null;
        for (children) |child| {
            if (self.is_terminal(child, .indent)) {
                indent_tok = self.token_of(child);
                continue;
            }
            if (self.is_terminal(child, .dedent)) {
                dedent_tok = self.token_of(child);
                continue;
            }
        }
        if (indent_tok == null or dedent_tok == null) {
            return self.fail(.unexpected_node, id);
        }
        return .{
            .start = indent_tok.?.where.end,
            .end = dedent_tok.?.where.start,
        };
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

    fn build_interpolated_arg(self: *builder, str: ink.identifier) build_error!?*ink.node {
        if (!self.allow_interpolation) return null;
        if (!self.has_interpolation_marker(str.string)) return null;

        var parts = std.array_list.Managed(*ink.node).init(self.allocator);
        var literal_buf = std.ArrayListUnmanaged(u8){};
        var literal_start: ?usize = null;
        var literal_end: usize = 0;
        defer literal_buf.deinit(self.allocator);

        var found_expr = false;
        var i: usize = 0;
        while (i < str.string.len) {
            const ch = str.string[i];
            if (ch == '{') {
                if (i + 1 < str.string.len and str.string[i + 1] == '{') {
                    literal_buf.append(self.allocator, '{') catch return error.out_of_memory;
                    if (literal_start == null) literal_start = i;
                    literal_end = i + 2;
                    i += 2;
                    continue;
                }
                try self.flush_interpolated_literal(&parts, &literal_buf, str, literal_start, literal_end);
                literal_start = null;
                literal_end = 0;
                const end = self.find_interpolation_end(str, i + 1) orelse return self.fail_string_literal(str);
                const raw_expr = str.string[i + 1 .. end];
                const trimmed_left = std.mem.trimLeft(u8, raw_expr, " \t\r\n");
                const trimmed = std.mem.trimRight(u8, trimmed_left, " \t\r\n");
                if (trimmed.len == 0) return self.fail_string_literal(str);
                const leading_ws = raw_expr.len - trimmed_left.len;
                const expr_start = str.where.start + i + 1 + leading_ws;
                const expr_node = try self.parse_interpolation_expr(trimmed, str, expr_start);
                parts.append(expr_node) catch return error.out_of_memory;
                found_expr = true;
                i = end + 1;
                continue;
            }
            if (ch == '}') {
                if (i + 1 < str.string.len and str.string[i + 1] == '}') {
                    literal_buf.append(self.allocator, '}') catch return error.out_of_memory;
                    if (literal_start == null) literal_start = i;
                    literal_end = i + 2;
                    i += 2;
                    continue;
                }
                return self.fail_string_literal(str);
            }
            literal_buf.append(self.allocator, ch) catch return error.out_of_memory;
            if (literal_start == null) literal_start = i;
            literal_end = i + 1;
            i += 1;
        }

        try self.flush_interpolated_literal(&parts, &literal_buf, str, literal_start, literal_end);

        if (!found_expr) return null;

        const name_ident = ink.identifier{ .string = "interpolate", .owner = .ref };
        return self.new_node(.{ .intrinsic = .{
            .name = name_ident,
            .args = ink.ast.ref_slice(parts.items),
        } });
    }

    fn has_interpolation_marker(self: *builder, text: []const u8) bool {
        _ = self;
        var i: usize = 0;
        while (i < text.len) : (i += 1) {
            if (text[i] == '{') {
                if (i + 1 < text.len and text[i + 1] == '{') {
                    i += 1;
                    continue;
                }
                return true;
            }
        }
        return false;
    }

    fn flush_interpolated_literal(
        self: *builder,
        parts: *std.array_list.Managed(*ink.node),
        literal_buf: *std.ArrayListUnmanaged(u8),
        source: ink.identifier,
        src_start: ?usize,
        src_end: usize,
    ) build_error!void {
        if (literal_buf.items.len == 0) return;
        const text = self.allocator.dupe(u8, literal_buf.items) catch return error.out_of_memory;
        literal_buf.clearRetainingCapacity();
        const start = src_start orelse 0;
        const ident = ink.identifier{
            .string = text,
            .owner = .ref,
            .where = .{
                .start = source.where.start + start,
                .end = source.where.start + src_end,
            },
        };
        const part_node = try self.new_node(.{ .string = ident });
        parts.append(part_node) catch return error.out_of_memory;
    }

    fn find_interpolation_end(self: *builder, str: ink.identifier, start: usize) ?usize {
        _ = self;
        var i = start;
        var in_string = false;
        var escaped = false;
        var depth: usize = 0;
        while (i < str.string.len) : (i += 1) {
            const ch = str.string[i];
            if (escaped) {
                escaped = false;
                continue;
            }
            if (ch == '\\') {
                escaped = true;
                continue;
            }
            if (ch == '"') {
                in_string = !in_string;
                continue;
            }
            if (!in_string) {
                if (ch == '{') {
                    depth += 1;
                    continue;
                }
                if (ch == '}') {
                    if (depth == 0) return i;
                    depth -= 1;
                }
            }
        }
        return null;
    }

    fn parse_interpolation_expr(
        self: *builder,
        expr_src: []const u8,
        err_loc: ink.identifier,
        base_offset: usize,
    ) build_error!*ink.node {
        const owned = self.allocator.dupe(u8, expr_src) catch return error.out_of_memory;
        const tokens = try self.lex_inline_expr_tokens(owned, err_loc, base_offset);
        var parsed = peg_parser.parse_from(self.allocator, tokens, peg.nonterminal_kind.expr) catch return self.fail_string_literal(err_loc);
        defer parsed.deinit();
        if (!parsed.ok or parsed.root == null) return self.fail_string_literal(err_loc);

        var inline_builder = builder.init(self.allocator, tokens, &parsed.tree, owned);
        const expr_node = inline_builder.build_expr(parsed.root.?) catch return self.fail_string_literal(err_loc);
        return expr_node;
    }

    fn lex_inline_expr_tokens(
        self: *builder,
        source: []const u8,
        err_loc: ink.identifier,
        base_offset: usize,
    ) build_error![]const token {
        var lexer = ink.lexer.init(source) catch return self.fail_string_literal(err_loc);
        var tokens = std.array_list.Managed(token).init(self.allocator);
        while (true) {
            const maybe_tok = lexer.next() catch return self.fail_string_literal(err_loc);
            if (maybe_tok) |tok| {
                if (tok.which == .end_of_file) break;
                var shifted = tok;
                shifted.where.start += base_offset;
                shifted.where.end += base_offset;
                shifted.what.where.start += base_offset;
                shifted.what.where.end += base_offset;
                tokens.append(shifted) catch return error.out_of_memory;
            } else break;
        }
        return tokens.toOwnedSlice() catch return error.out_of_memory;
    }

    fn token_index_for_start(self: *builder, start: usize) ?usize {
        for (self.tokens, 0..) |tok, idx| {
            if (tok.where.start == start and tok.which == .string) return idx;
        }
        return null;
    }

    fn fail_string_literal(self: *builder, ident: ink.identifier) build_error {
        const pos = self.token_index_for_start(ident.where.start) orelse 0;
        if (self.last_error == null or pos >= self.last_error.?.position) {
            self.last_error = .{ .kind = .string_literal, .position = pos };
        }
        return error.build_failed;
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
        const field_token = self.name_token_of(children[1]) orelse return self.fail(.unexpected_node, id);
        const field = field_token.what;
        const op: ink.binary = if (self.is_terminal(children[0], .double_colon)) .scope_access else .access;
        const field_node = try self.new_node(.{ .identifier = field });
        return self.new_node(.{ .binary = .{
            .left = ink.ast.ref(base),
            .op = op,
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

        for (children) |child| {
            if (self.is_nonterminal(child, .intrinsic_call)) {
                return self.build_intrinsic_call(child);
            }
            if (self.is_nonterminal(child, .atomic_expr)) {
                return self.build_atomic_expr(child);
            }
            if (self.is_nonterminal(child, .duration_literal)) {
                return self.build_duration_literal(child);
            }
            if (self.is_nonterminal(child, .block)) {
                return self.build_block_expr(child);
            }
        }

        const first = children[0];
        if (self.is_terminal(first, .number)) {
            return self.build_number(first);
        }
        if (self.is_terminal(first, .logical_true) or self.is_terminal(first, .logical_false) or self.is_terminal(first, .this)) {
            return self.new_node(.{ .identifier = self.token_of(first).what });
        }
        if (self.is_name_node(first)) {
            return self.new_node(.{ .identifier = self.name_token_of(first).?.what });
        }
        if (self.is_terminal(first, .label)) {
            return self.new_node(.{ .identifier = self.token_of(first).what });
        }
        if (self.is_terminal(first, .string)) {
            const value = self.token_of(first).what;
            if (try self.build_interpolated_arg(value)) |interp| return interp;
            return self.new_node(.{ .string = value });
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

    fn build_intrinsic_call(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var name: ?ink.identifier = null;
        var args: []const *ink.node = &[_]*ink.node{};

        for (children) |child| {
            if (self.is_name_node(child)) {
                if (name != null) return self.fail(.unexpected_node, id);
                name = self.name_token_of(child).?.what;
                continue;
            }
            if (self.is_nonterminal(child, .arg_list)) {
                args = try self.collect_expr_args(child);
                continue;
            }
        }

        if (name == null) return self.fail(.unexpected_node, id);

        return self.new_node(.{ .intrinsic = .{
            .name = name.?,
            .args = ink.ast.ref_slice(args),
        } });
    }

    fn build_duration_literal(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len == 0) return self.fail(.invalid_duration_literal, id);

        var total: i64 = 0;
        var start: ?usize = null;
        var end: usize = 0;

        for (children) |child| {
            if (!self.is_nonterminal(child, .duration_item)) continue;
            const item_children = self.child_nodes(child);
            if (item_children.len < 2) return self.fail(.invalid_duration_literal, child);
            if (!self.is_terminal(item_children[0], .number) or !self.is_terminal(item_children[1], .identifier)) {
                return self.fail(.invalid_duration_literal, child);
            }

            const number_token = self.token_of(item_children[0]);
            const unit_token = self.token_of(item_children[1]);

            if (unit_token.where.start == number_token.where.end) {
                return self.fail(.invalid_duration_literal, item_children[1]);
            }

            if (start == null) start = number_token.where.start;
            end = unit_token.where.end;

            const digits = number_token.what.string;
            if (std.mem.indexOfScalar(u8, digits, '.') != null or
                std.mem.indexOfScalar(u8, digits, 'e') != null or
                std.mem.indexOfScalar(u8, digits, 'E') != null)
            {
                return self.fail(.invalid_duration_literal, item_children[0]);
            }

            const value = std.fmt.parseInt(i64, digits, 10) catch return error.parse_integer;
            const unit_ns = self.duration_unit_ns(unit_token.what.string) orelse
                return self.fail(.invalid_duration_literal, item_children[1]);

            const mul_info = @mulWithOverflow(value, unit_ns);
            if (mul_info[1] != 0) return self.fail(.invalid_duration_literal, item_children[0]);
            const add_info = @addWithOverflow(total, mul_info[0]);
            if (add_info[1] != 0) return self.fail(.invalid_duration_literal, item_children[0]);
            total = add_info[0];
        }

        if (start == null) return self.fail(.invalid_duration_literal, id);
        const where = ink.location{ .start = start.?, .end = end };
        return self.new_node(.{ .duration = .{ .value = total, .where = where } });
    }

    fn build_number(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const tok = self.token_of(id);
        const slice = tok.what;
        const is_float = std.mem.indexOfScalar(u8, slice.string, '.') != null or
            std.mem.indexOfScalar(u8, slice.string, 'e') != null or
            std.mem.indexOfScalar(u8, slice.string, 'E') != null;
        if (is_float) {
            const val = std.fmt.parseFloat(f32, slice.string) catch return error.parse_float;
            return self.new_node(.{ .float = .{ .value = val, .where = tok.where } });
        }
        const val = std.fmt.parseInt(i64, slice.string, 10) catch return error.parse_integer;
        return self.new_node(.{ .integer = .{ .value = val, .where = tok.where } });
    }

    fn build_function_decl(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        const attributes = try self.parse_attributes(children, &idx);
        var is_comptime = false;
        if (idx < children.len and self.is_terminal(children[idx], .@"comptime")) {
            is_comptime = true;
            idx += 1;
        }
        if (idx >= children.len or !self.is_terminal(children[idx], .function)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        const name_token = if (idx < children.len) self.name_token_of(children[idx]) else null;
        if (name_token == null) {
            return self.fail(.unexpected_node, id);
        }
        const name = name_token.?.what;
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
            .attributes = attributes,
            .is_comptime = is_comptime,
            .name = name,
            .generics = generics,
            .params = params,
            .return_type = ink.ast.ref_opt(return_type),
            .where_clause = where_clause,
            .body = ink.ast.ref_opt(body),
            .where = self.node_location(id),
        } } });
    }

    fn build_struct_decl(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        const attributes = try self.parse_attributes(children, &idx);
        if (idx >= children.len or !self.is_terminal(children[idx], .@"struct")) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        const name_token = if (idx < children.len) self.name_token_of(children[idx]) else null;
        if (name_token == null) {
            return self.fail(.unexpected_node, id);
        }
        const name = name_token.?.what;
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
            .attributes = attributes,
            .name = name,
            .generics = generics,
            .fields = fields,
            .where = self.node_location(id),
        } } });
    }

    fn build_trait_decl(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        const attributes = try self.parse_attributes(children, &idx);
        var is_auto = false;
        if (idx < children.len and self.is_terminal(children[idx], .auto)) {
            is_auto = true;
            idx += 1;
        }
        if (idx >= children.len or !self.is_terminal(children[idx], .trait)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        const name_token = if (idx < children.len) self.name_token_of(children[idx]) else null;
        if (name_token == null) {
            return self.fail(.unexpected_node, id);
        }
        const name = name_token.?.what;
        idx += 1;

        var generics: []const ink.ast.generic_param = &.{};
        if (idx < children.len and self.is_nonterminal(children[idx], .generic_params)) {
            generics = try self.build_generic_params(children[idx]);
            idx += 1;
        }

        var items: []const ink.ast.trait_item = &.{};
        var requires: []const ink.ast.node_ref = &.{};
        if (idx < children.len and self.is_nonterminal(children[idx], .trait_body)) {
            const body = try self.build_trait_body(children[idx]);
            items = body.items;
            requires = body.requires;
        }

        return self.new_node(.{ .decl = .{ .trait = .{
            .attributes = attributes,
            .is_auto = is_auto,
            .name = name,
            .generics = generics,
            .items = items,
            .requires = requires,
            .where = self.node_location(id),
        } } });
    }

    fn build_enum_decl(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        const attributes = try self.parse_attributes(children, &idx);
        if (idx >= children.len or !self.is_terminal(children[idx], .@"enum")) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        const name_token = if (idx < children.len) self.name_token_of(children[idx]) else null;
        if (name_token == null) {
            return self.fail(.unexpected_node, id);
        }
        const name = name_token.?.what;
        idx += 1;

        var generics: []const ink.ast.generic_param = &.{};
        if (idx < children.len and self.is_nonterminal(children[idx], .generic_params)) {
            generics = try self.build_generic_params(children[idx]);
            idx += 1;
        }

        var variants: []const ink.ast.sum_variant = &.{};
        if (idx < children.len and self.is_nonterminal(children[idx], .enum_body)) {
            variants = try self.build_sum_body(children[idx]);
        }

        return self.new_node(.{ .decl = .{ .@"enum" = .{
            .attributes = attributes,
            .name = name,
            .generics = generics,
            .variants = variants,
            .where = self.node_location(id),
        } } });
    }

    fn build_impl_decl(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        const attributes = try self.parse_attributes(children, &idx);
        if (idx >= children.len or !self.is_terminal(children[idx], .impl)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        var negative = false;
        if (idx < children.len and self.is_terminal(children[idx], .bang)) {
            negative = true;
            idx += 1;
        }

        const trait_token = if (idx < children.len) self.name_token_of(children[idx]) else null;
        if (trait_token == null) {
            return self.fail(.unexpected_node, id);
        }
        const by_trait = trait_token.?.what;
        idx += 1;

        if (idx >= children.len or !self.is_terminal(children[idx], .@"for")) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        const struct_token = if (idx < children.len) self.name_token_of(children[idx]) else null;
        if (struct_token == null) {
            return self.fail(.unexpected_node, id);
        }
        const for_struct = struct_token.?.what;
        idx += 1;

        var functions: []const ink.ast.function_decl = &.{};
        if (idx < children.len and self.is_nonterminal(children[idx], .impl_body)) {
            functions = try self.build_impl_body(children[idx]);
        }

        return self.new_node(.{ .decl = .{ .impl = .{
            .attributes = attributes,
            .negative = negative,
            .by_trait = by_trait,
            .for_struct = for_struct,
            .functions = functions,
            .where = self.node_location(id),
        } } });
    }

    fn build_import_decl(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        const attributes = try self.parse_attributes(children, &idx);
        if (idx >= children.len or !self.is_terminal(children[idx], .import)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;
        const first_name = self.qualified_name_of(children[idx]);
        if (first_name == null) {
            return self.fail(.unexpected_node, id);
        }
        const first = first_name.?;
        idx += 1;
        var alias: ?ink.identifier = null;
        if (idx + 1 < children.len and self.is_terminal(children[idx], .as)) {
            const alias_token = self.name_token_of(children[idx + 1]);
            if (alias_token == null) {
                return self.fail(.unexpected_node, id);
            }
            alias = alias_token.?.what;
            idx += 2;
        }
        var module = first;
        var item: ?ink.identifier = null;
        if (idx + 1 < children.len and self.is_terminal(children[idx], .from)) {
            const module_name = self.qualified_name_of(children[idx + 1]);
            if (module_name == null) {
                return self.fail(.unexpected_node, id);
            }
            module = module_name.?;
            item = first;
            idx += 2;
        }
        return self.new_node(.{ .decl = .{ .import = .{
            .attributes = attributes,
            .module = module,
            .item = item,
            .alias = alias,
            .where = self.node_location(id),
        } } });
    }

    fn build_const_decl(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        const attributes = try self.parse_attributes(children, &idx);
        if (idx >= children.len or !self.is_terminal(children[idx], .constant)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        const name_token = if (idx < children.len) self.name_token_of(children[idx]) else null;
        if (name_token == null) {
            return self.fail(.unexpected_node, id);
        }
        const name = name_token.?.what;
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
            .attributes = attributes,
            .name = name,
            .ty = ink.ast.ref_opt(ty),
            .value = ink.ast.ref(value),
            .where = self.node_location(id),
        } } });
    }

    fn build_var_decl(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        const attributes = try self.parse_attributes(children, &idx);
        if (idx >= children.len or !self.is_terminal(children[idx], .mut)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        const name_token = if (idx < children.len) self.name_token_of(children[idx]) else null;
        if (name_token == null) {
            return self.fail(.unexpected_node, id);
        }
        const name = name_token.?.what;
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
            .attributes = attributes,
            .name = name,
            .ty = ink.ast.ref_opt(ty),
            .value = ink.ast.ref(value),
            .where = self.node_location(id),
        } } });
    }

    fn build_type_decl(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        const attributes = try self.parse_attributes(children, &idx);
        if (idx >= children.len or !self.is_terminal(children[idx], .type)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        const name_token = if (idx < children.len) self.name_token_of(children[idx]) else null;
        if (name_token == null) {
            return self.fail(.unexpected_node, id);
        }
        const name = name_token.?.what;
        idx += 1;

        var generics: []const ink.ast.generic_param = &.{};
        if (idx < children.len and self.is_nonterminal(children[idx], .generic_params)) {
            generics = try self.build_generic_params(children[idx]);
            idx += 1;
        }

        if (idx >= children.len or !self.is_terminal(children[idx], .assign)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;

        if (idx >= children.len or !self.is_nonterminal(children[idx], .type_expr)) {
            return self.fail(.unexpected_node, id);
        }
        const value = try self.build_type_expr(children[idx]);

        return self.new_node(.{ .decl = .{ .type_alias = .{
            .attributes = attributes,
            .name = name,
            .generics = generics,
            .value = ink.ast.ref(value),
            .where = self.node_location(id),
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
        const name_token = self.name_token_of(children[0]);
        if (name_token == null) return self.fail(.unexpected_node, id);
        if (!self.is_terminal(children[1], .colon)) return self.fail(.unexpected_node, id);
        if (!self.is_nonterminal(children[2], .type_expr)) return self.fail(.unexpected_node, id);
        const name = name_token.?.what;
        const ty = try self.build_type_expr(children[2]);
        return .{ .name = name, .ty = ink.ast.ref(ty) };
    }

    const trait_body_parts = struct {
        items: []const ink.ast.trait_item,
        requires: []const ink.ast.node_ref,
    };

    fn build_trait_body(self: *builder, id: peg_parser.node_id) build_error!trait_body_parts {
        var items = std.array_list.Managed(ink.ast.trait_item).init(self.allocator);
        var reqs = std.array_list.Managed(ink.ast.node_ref).init(self.allocator);
        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .trait_body_item)) {
                for (self.child_nodes(child)) |item_child| {
                    if (self.is_nonterminal(item_child, .trait_item)) {
                        items.append(try self.build_trait_item(item_child)) catch return error.out_of_memory;
                        continue;
                    }
                    if (self.is_nonterminal(item_child, .requires_clause)) {
                        const req_list = try self.build_requires_clause(item_child);
                        for (req_list) |req| {
                            reqs.append(req) catch return error.out_of_memory;
                        }
                    }
                }
                continue;
            }
            if (self.is_nonterminal(child, .trait_item)) {
                items.append(try self.build_trait_item(child)) catch return error.out_of_memory;
                continue;
            }
            if (self.is_nonterminal(child, .requires_clause)) {
                const req_list = try self.build_requires_clause(child);
                for (req_list) |req| {
                    reqs.append(req) catch return error.out_of_memory;
                }
            }
        }
        return .{
            .items = items.toOwnedSlice() catch return error.out_of_memory,
            .requires = reqs.toOwnedSlice() catch return error.out_of_memory,
        };
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
        var idx: usize = 0;
        const attributes = try self.parse_attributes(children, &idx);
        if (children.len < 2 or !self.is_terminal(children[idx], .type)) {
            return self.fail(.unexpected_node, id);
        }
        idx += 1;
        const name_token = if (idx < children.len) self.name_token_of(children[idx]) else null;
        if (name_token == null) {
            return self.fail(.unexpected_node, id);
        }
        const name = name_token.?.what;
        idx += 1;
        var value: ?*ink.node = null;
        if (idx < children.len) {
            if (self.is_terminal(children[idx], .assign)) {
                if (idx + 1 >= children.len or !self.is_nonterminal(children[idx + 1], .type_expr)) {
                    return self.fail(.unexpected_node, id);
                }
                value = try self.build_type_expr(children[idx + 1]);
            }
        }
        return .{
            .attributes = attributes,
            .name = name,
            .value = ink.ast.ref_opt(value),
            .where = self.node_location(id),
        };
    }

    fn build_requires_clause(self: *builder, id: peg_parser.node_id) build_error![]const ink.ast.node_ref {
        const children = self.child_nodes(id);
        if (children.len < 2 or !self.is_terminal(children[0], .requires)) {
            return self.fail(.unexpected_node, id);
        }
        var reqs = std.array_list.Managed(ink.ast.node_ref).init(self.allocator);
        for (children[1..]) |child| {
            if (self.is_nonterminal(child, .type_expr)) {
                const req_node = try self.build_type_expr(child);
                reqs.append(ink.ast.ref(req_node)) catch return error.out_of_memory;
            }
        }
        if (reqs.items.len == 0) return self.fail(.unexpected_node, id);
        return reqs.toOwnedSlice() catch return error.out_of_memory;
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
        const name_token = if (children.len > 0) self.name_token_of(children[0]) else null;
        if (name_token == null) {
            return self.fail(.unexpected_node, id);
        }
        const name = name_token.?.what;
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
            if (self.is_name_node(child)) {
                cases.append(self.name_token_of(child).?.what) catch return error.out_of_memory;
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
        if (children.len < 1) {
            return self.fail(.unexpected_node, id);
        }
        const name_token = self.name_token_of(children[0]);
        if (name_token == null and !self.is_terminal(children[0], .label)) {
            return self.fail(.unexpected_node, id);
        }
        const name = if (name_token) |tok| tok.what else self.token_of(children[0]).what;
        var constraint: ?*ink.node = null;
        var default: ?*ink.node = null;
        var is_pack = false;
        var idx: usize = 1;

        if (idx < children.len and self.is_terminal(children[idx], .colon)) {
            if (idx + 1 >= children.len or !self.is_nonterminal(children[idx + 1], .type_expr)) {
                return self.fail(.unexpected_node, id);
            }
            constraint = try self.build_type_expr(children[idx + 1]);
            idx += 2;
            if (idx < children.len and self.is_terminal(children[idx], .ellipsis)) {
                is_pack = true;
                idx += 1;
            }
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
            .is_pack = is_pack,
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

        if (self.is_terminal(children[0], .this) or self.is_terminal(children[0], .self)) {
            const name = self.token_of(children[0]).what;
            var ty: *ink.node = undefined;
            if (children.len >= 3 and self.is_terminal(children[1], .colon) and self.is_nonterminal(children[2], .type_expr)) {
                ty = try self.build_type_expr(children[2]);
            } else {
                ty = try self.new_node(.{ .type = .self });
            }
            return .{ .name = name, .ty = ink.ast.ref(ty), .variadic = false };
        }

        const name_token = self.name_token_of(children[0]);
        if (name_token == null) {
            return self.fail(.unexpected_node, id);
        }
        if (children.len < 3 or !self.is_terminal(children[1], .colon) or !self.is_nonterminal(children[2], .type_expr)) {
            return self.fail(.unexpected_node, id);
        }
        const name = name_token.?.what;
        const ty = try self.build_type_expr(children[2]);
        const variadic = children.len > 3 and self.is_terminal(children[3], .ellipsis);
        return .{ .name = name, .ty = ink.ast.ref(ty), .variadic = variadic };
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
        var idx: usize = 1;
        var list = std.array_list.Managed(ink.ast.where_req).init(self.allocator);
        while (idx < children.len) {
            const name_token = self.name_token_of(children[idx]);
            if (name_token == null and !self.is_terminal(children[idx], .label)) {
                return self.fail(.unexpected_node, id);
            }
            if (idx + 2 >= children.len or !self.is_terminal(children[idx + 1], .colon) or !self.is_nonterminal(children[idx + 2], .type_expr)) {
                return self.fail(.unexpected_node, id);
            }
            const name = if (name_token) |tok| tok.what else self.token_of(children[idx]).what;
            const constraint = try self.build_type_expr(children[idx + 2]);
            list.append(.{ .name = name, .constraint = ink.ast.ref(constraint) }) catch return error.out_of_memory;
            idx += 3;
            if (idx >= children.len) break;
            if (!self.is_terminal(children[idx], .comma)) return self.fail(.unexpected_node, id);
            idx += 1;
        }
        return list.toOwnedSlice() catch return error.out_of_memory;
    }

    fn synthetic_ident(self: *builder, name: []const u8) ink.identifier {
        _ = self;
        return .{ .string = name, .owner = .ref, .where = .{ .start = 0, .end = 0 } };
    }

    fn build_type_expr(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        if (!self.is_nonterminal(id, .type_expr)) {
            return self.fail(.unexpected_node, id);
        }
        for (self.child_nodes(id)) |child| {
            if (self.is_nonterminal(child, .type_arrow)) return self.build_type_arrow(child);
        }
        return self.fail(.unexpected_node, id);
    }

    fn build_type_arrow(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_nonterminal(children[idx], .type_union)) {
            return self.fail(.unexpected_node, id);
        }
        const left = try self.build_type_union(children[idx]);
        idx += 1;

        if (idx < children.len and self.is_terminal(children[idx], .arrow)) {
            if (idx + 1 >= children.len or !self.is_nonterminal(children[idx + 1], .type_arrow)) {
                return self.fail(.unexpected_node, id);
            }
            const right = try self.build_type_arrow(children[idx + 1]);
            var args = self.allocator.alloc(*ink.node, 2) catch return error.out_of_memory;
            args[0] = left;
            args[1] = right;
            return self.new_node(.{ .type = .{ .applied = .{
                .base = self.synthetic_ident("fn"),
                .args = ink.ast.ref_slice(args),
            } } });
        }

        return left;
    }

    fn build_type_union(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_nonterminal(children[idx], .type_intersect)) {
            return self.fail(.unexpected_node, id);
        }

        var items = std.array_list.Managed(*ink.node).init(self.allocator);
        items.append(try self.build_type_intersect(children[idx])) catch return error.out_of_memory;
        idx += 1;

        while (idx < children.len) : (idx += 1) {
            const child = children[idx];
            if (self.is_terminal(child, .bar)) {
                if (idx + 1 >= children.len or !self.is_nonterminal(children[idx + 1], .type_intersect)) {
                    return self.fail(.unexpected_node, id);
                }
                items.append(try self.build_type_intersect(children[idx + 1])) catch return error.out_of_memory;
                idx += 1;
            }
        }

        if (items.items.len == 1) {
            return items.items[0];
        }

        const args = items.toOwnedSlice() catch return error.out_of_memory;
        return self.new_node(.{ .type = .{ .applied = .{
            .base = self.synthetic_ident("union"),
            .args = ink.ast.ref_slice(args),
        } } });
    }

    fn build_type_intersect(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var idx: usize = 0;
        if (idx >= children.len or !self.is_nonterminal(children[idx], .type_prefix)) {
            return self.fail(.unexpected_node, id);
        }

        var items = std.array_list.Managed(*ink.node).init(self.allocator);
        items.append(try self.build_type_prefix(children[idx])) catch return error.out_of_memory;
        idx += 1;

        while (idx < children.len) : (idx += 1) {
            const child = children[idx];
            if (self.is_terminal(child, .ampersand)) {
                if (idx + 1 >= children.len or !self.is_nonterminal(children[idx + 1], .type_prefix)) {
                    return self.fail(.unexpected_node, id);
                }
                items.append(try self.build_type_prefix(children[idx + 1])) catch return error.out_of_memory;
                idx += 1;
            }
        }

        if (items.items.len == 1) {
            return items.items[0];
        }

        const args = items.toOwnedSlice() catch return error.out_of_memory;
        return self.new_node(.{ .type = .{ .applied = .{
            .base = self.synthetic_ident("intersect"),
            .args = ink.ast.ref_slice(args),
        } } });
    }

    fn build_type_prefix(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len == 0) return self.fail(.unexpected_node, id);

        if (self.is_terminal(children[0], .bang)) {
            if (children.len < 2 or !self.is_nonterminal(children[1], .type_prefix)) {
                return self.fail(.unexpected_node, id);
            }
            const inner = try self.build_type_prefix(children[1]);
            var args = self.allocator.alloc(*ink.node, 1) catch return error.out_of_memory;
            args[0] = inner;
            return self.new_node(.{ .type = .{ .applied = .{
                .base = self.synthetic_ident("not"),
                .args = ink.ast.ref_slice(args),
            } } });
        }
        if (self.is_terminal(children[0], .question)) {
            if (children.len < 2 or !self.is_nonterminal(children[1], .type_prefix)) {
                return self.fail(.unexpected_node, id);
            }
            const inner = try self.build_type_prefix(children[1]);
            return self.new_node(.{ .type = .{ .optional = ink.ast.ref(inner) } });
        }
        if (self.is_nonterminal(children[0], .type_ref)) {
            return self.build_type_ref(children[0]);
        }
        if (self.is_nonterminal(children[0], .type_array)) {
            return self.build_type_array(children[0]);
        }
        if (self.is_nonterminal(children[0], .type_dyn)) {
            return self.build_type_dyn(children[0]);
        }
        if (self.is_nonterminal(children[0], .type_postfix)) {
            return self.build_type_postfix(children[0]);
        }

        return self.fail(.unexpected_node, id);
    }

    fn build_type_array(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var size_expr: ?*ink.node = null;
        var elem: ?*ink.node = null;
        for (children) |child| {
            if (self.is_nonterminal(child, .expr)) {
                size_expr = try self.build_expr(child);
            } else if (self.is_nonterminal(child, .type_prefix)) {
                elem = try self.build_type_prefix(child);
            }
        }

        if (elem == null) return self.fail(.unexpected_node, id);

        if (size_expr) |size| {
            var args = self.allocator.alloc(*ink.node, 2) catch return error.out_of_memory;
            args[0] = size;
            args[1] = elem.?;
            return self.new_node(.{ .type = .{ .applied = .{
                .base = self.synthetic_ident("array"),
                .args = ink.ast.ref_slice(args),
            } } });
        }

        var args = self.allocator.alloc(*ink.node, 1) catch return error.out_of_memory;
        args[0] = elem.?;
        return self.new_node(.{ .type = .{ .applied = .{
            .base = self.synthetic_ident("slice"),
            .args = ink.ast.ref_slice(args),
        } } });
    }

    fn build_type_ref(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len < 2 or !self.is_terminal(children[0], .ampersand)) {
            return self.fail(.unexpected_node, id);
        }

        var idx: usize = 1;
        var lifetime: ?*ink.node = null;
        if (idx < children.len and self.is_terminal(children[idx], .label)) {
            lifetime = try self.new_node(.{ .identifier = self.token_of(children[idx]).what });
            idx += 1;
        }
        var base: []const u8 = "ref";
        if (idx < children.len and self.is_terminal(children[idx], .mut)) {
            base = "ref_mut";
            idx += 1;
        }
        if (idx < children.len and self.is_terminal(children[idx], .label)) {
            if (lifetime != null) return self.fail(.unexpected_node, id);
            lifetime = try self.new_node(.{ .identifier = self.token_of(children[idx]).what });
            idx += 1;
        }
        if (idx >= children.len or !self.is_nonterminal(children[idx], .type_prefix)) {
            return self.fail(.unexpected_node, id);
        }

        const inner = try self.build_type_prefix(children[idx]);
        const arg_len: usize = if (lifetime == null) 1 else 2;
        var args = self.allocator.alloc(*ink.node, arg_len) catch return error.out_of_memory;
        if (lifetime) |life| {
            args[0] = life;
            args[1] = inner;
        } else {
            args[0] = inner;
        }
        return self.new_node(.{ .type = .{ .applied = .{
            .base = self.synthetic_ident(base),
            .args = ink.ast.ref_slice(args),
        } } });
    }

    fn build_type_dyn(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len < 2 or !self.is_terminal(children[0], .dyn)) {
            return self.fail(.unexpected_node, id);
        }
        if (!self.is_nonterminal(children[1], .type_intersect)) {
            return self.fail(.unexpected_node, id);
        }
        const inner = try self.build_type_intersect(children[1]);
        return self.new_node(.{ .type = .{ .dyn = ink.ast.ref(inner) } });
    }

    fn build_type_postfix(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var base_node: ?*ink.node = null;
        var args: ?[]const *ink.node = null;
        for (children) |child| {
            if (self.is_nonterminal(child, .type_primary)) {
                base_node = try self.build_type_primary(child);
            } else if (self.is_nonterminal(child, .type_args)) {
                args = try self.collect_expr_args(child);
            }
        }

        if (base_node == null) return self.fail(.unexpected_node, id);
        if (args == null) return base_node.?;

        const base_ident = switch (base_node.?.*) {
            .type => |ty| switch (ty) {
                .name => |name_id| name_id,
                else => return self.fail(.unexpected_node, id),
            },
            else => return self.fail(.unexpected_node, id),
        };

        return self.new_node(.{ .type = .{ .applied = .{
            .base = base_ident,
            .args = ink.ast.ref_slice(args.?),
        } } });
    }

    fn build_type_primary(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        if (children.len == 0) return self.fail(.unexpected_node, id);
        for (children) |child| {
            if (self.is_nonterminal(child, .type_tuple)) return self.build_type_tuple(child);
            if (self.is_terminal(child, .self)) return self.new_node(.{ .type = .self });
            if (self.is_nonterminal(child, .type_atom)) {
                const base = try self.build_type_atom(child);
                return self.new_node(.{ .type = .{ .name = base } });
            }
            if (self.is_nonterminal(child, .type_group)) {
                const group_children = self.child_nodes(child);
                for (group_children) |gchild| {
                    if (self.is_nonterminal(gchild, .type_expr)) return self.build_type_expr(gchild);
                }
            }
        }
        return self.fail(.unexpected_node, id);
    }

    fn build_type_tuple(self: *builder, id: peg_parser.node_id) build_error!*ink.node {
        const children = self.child_nodes(id);
        var items = std.array_list.Managed(*ink.node).init(self.allocator);
        for (children) |child| {
            if (self.is_nonterminal(child, .type_expr)) {
                items.append(try self.build_type_expr(child)) catch return error.out_of_memory;
            }
        }

        const args = items.toOwnedSlice() catch return error.out_of_memory;
        return self.new_node(.{ .type = .{ .applied = .{
            .base = self.synthetic_ident("tuple"),
            .args = ink.ast.ref_slice(args),
        } } });
    }

    fn build_type_atom(self: *builder, id: peg_parser.node_id) build_error!ink.identifier {
        const children = self.child_nodes(id);
        if (children.len == 0) return self.fail(.unexpected_node, id);
        if (self.is_terminal(children[0], .type) or self.is_terminal(children[0], .ref) or
            self.is_terminal(children[0], .box) or self.is_terminal(children[0], .atomic))
        {
            return self.token_of(children[0]).what;
        }
        const name_token = self.name_token_of(children[0]) orelse return self.fail(.unexpected_node, id);
        return name_token.what;
    }

    fn duration_unit_ns(self: *builder, name: []const u8) ?i64 {
        _ = self;
        const ns_per_micro: i64 = 1_000;
        const ns_per_milli: i64 = 1_000_000;
        const ns_per_sec: i64 = 1_000_000_000;
        const ns_per_min: i64 = 60 * ns_per_sec;
        const ns_per_hour: i64 = 60 * ns_per_min;
        const ns_per_day: i64 = 24 * ns_per_hour;

        if (std.mem.eql(u8, name, "nano")) return 1;
        if (std.mem.eql(u8, name, "micro")) return ns_per_micro;
        if (std.mem.eql(u8, name, "milli")) return ns_per_milli;
        if (std.mem.eql(u8, name, "sec")) return ns_per_sec;
        if (std.mem.eql(u8, name, "min")) return ns_per_min;
        if (std.mem.eql(u8, name, "hour")) return ns_per_hour;
        if (std.mem.eql(u8, name, "day")) return ns_per_day;
        return null;
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
            .percent => .mod,
            .ampersand => .bit_and,
            .bar => .bit_or,
            .caret => .bit_xor,
            .shift_left => .shl,
            .shift_right => .shr,
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
            .tilde => .bit_not,
            .ampersand => .borrow,
            .asterisk => .deref,
            .box => .box,
            .dynamic => .dynamic,
            .@"comptime" => .@"comptime",
            .sleep => .sleep,
            .timeout => .timeout,
            .deadline => .deadline,
            .spawn => .spawn,
            .await => .await,
            .@"try" => .@"try",
            else => error.build_failed,
        };
    }

    fn is_assignment_op(self: *builder, id: peg_parser.node_id) bool {
        if (self.tree.nodes.items[id].symbol.kind != .terminal) return false;
        const kind: token.kind = @enumFromInt(self.tree.nodes.items[id].symbol.value);
        return switch (kind) {
            .assign,
            .plus_assign,
            .minus_assign,
            .asterisk_assign,
            .slash_assign,
            .percent_assign,
            .ampersand_assign,
            .bar_assign,
            .caret_assign,
            .shift_left_assign,
            .shift_right_assign,
            => true,
            else => false,
        };
    }

    fn assignment_from_token(self: *builder, kind: token.kind) build_error!ink.binary {
        _ = self;
        return switch (kind) {
            .assign => .assign,
            .plus_assign => .assign_add,
            .minus_assign => .assign_sub,
            .asterisk_assign => .assign_mul,
            .slash_assign => .assign_div,
            .percent_assign => .assign_mod,
            .ampersand_assign => .assign_bit_and,
            .bar_assign => .assign_bit_or,
            .caret_assign => .assign_bit_xor,
            .shift_left_assign => .assign_shl,
            .shift_right_assign => .assign_shr,
            else => error.build_failed,
        };
    }

    fn assignment_from_node(self: *builder, id: peg_parser.node_id) build_error!ink.binary {
        if (self.is_terminal(id, .assign) or self.is_terminal(id, .plus_assign) or self.is_terminal(id, .minus_assign) or
            self.is_terminal(id, .asterisk_assign) or self.is_terminal(id, .slash_assign) or self.is_terminal(id, .percent_assign) or
            self.is_terminal(id, .ampersand_assign) or self.is_terminal(id, .bar_assign) or self.is_terminal(id, .caret_assign) or
            self.is_terminal(id, .shift_left_assign) or self.is_terminal(id, .shift_right_assign))
        {
            return self.assignment_from_token(self.token_of(id).which);
        }

        if (self.is_nonterminal(id, .assign_op)) {
            for (self.child_nodes(id)) |child| {
                if (self.tree.nodes.items[child].symbol.kind != .terminal) continue;
                return self.assignment_from_token(self.token_of(child).which);
            }
        }

        return error.build_failed;
    }

    fn is_keyword_kind(self: *builder, kind: token.kind) bool {
        _ = self;
        inline for (spec.keyword_lexemes) |lex| {
            if (kind == @field(token.kind, lex.kind)) return true;
        }
        return false;
    }

    fn is_name_token(self: *builder, id: peg_parser.node_id) bool {
        if (self.tree.nodes.items[id].symbol.kind != .terminal) return false;
        const kind: token.kind = @enumFromInt(self.tree.nodes.items[id].symbol.value);
        return kind == .identifier or self.is_keyword_kind(kind);
    }

    fn name_token_of(self: *builder, id: peg_parser.node_id) ?token {
        if (self.is_nonterminal(id, .name)) {
            for (self.child_nodes(id)) |child| {
                if (self.is_name_token(child)) return self.token_of(child);
            }
            return null;
        }
        if (self.is_name_token(id)) return self.token_of(id);
        return null;
    }

    fn qualified_name_of(self: *builder, id: peg_parser.node_id) ?ink.identifier {
        if (!self.is_nonterminal(id, .qualified_name)) {
            const tok = self.name_token_of(id) orelse return null;
            return tok.what;
        }

        const children = self.child_nodes(id);
        var parts = std.array_list.Managed([]const u8).init(self.allocator);
        defer parts.deinit();

        var start: usize = 0;
        var end: usize = 0;
        for (children) |child| {
            const tok = self.name_token_of(child) orelse continue;
            if (parts.items.len == 0) start = tok.where.start;
            end = tok.where.end;
            parts.append(tok.what.string) catch return null;
        }

        if (parts.items.len == 0) return null;

        var total: usize = 0;
        for (parts.items, 0..) |part, idx| {
            total += part.len;
            if (idx > 0) total += 2;
        }
        var buf = self.allocator.alloc(u8, total) catch return null;
        var offset: usize = 0;
        for (parts.items, 0..) |part, idx| {
            if (idx > 0) {
                buf[offset] = ':';
                buf[offset + 1] = ':';
                offset += 2;
            }
            std.mem.copyForwards(u8, buf[offset .. offset + part.len], part);
            offset += part.len;
        }

        return .{
            .string = buf,
            .owner = .ref,
            .where = .{ .start = start, .end = end },
        };
    }

    fn is_name_node(self: *builder, id: peg_parser.node_id) bool {
        return self.name_token_of(id) != null;
    }

    fn is_comparison_op(self: *builder, id: peg_parser.node_id) bool {
        if (self.tree.nodes.items[id].symbol.kind != .terminal) return false;
        const kind: token.kind = @enumFromInt(self.tree.nodes.items[id].symbol.value);
        return switch (kind) {
            .less_than, .greater_than, .less_or_equal, .greater_or_equal, .equal, .not_equal => true,
            else => false,
        };
    }

    fn node_location(self: *builder, id: peg_parser.node_id) ink.location {
        const parse_node = self.tree.nodes.items[id];
        if (parse_node.start >= self.tokens.len or parse_node.end == 0) return .{ .start = 0, .end = 0 };
        const start_tok = self.tokens[parse_node.start];
        const end_index = if (parse_node.end > 0) parse_node.end - 1 else parse_node.start;
        if (end_index >= self.tokens.len) return .{ .start = start_tok.where.start, .end = start_tok.where.end };
        const end_tok = self.tokens[end_index];
        return .{ .start = start_tok.where.start, .end = end_tok.where.end };
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

    fn is_foreign_name(self: *builder, id: peg_parser.node_id) bool {
        return self.is_name_node(id);
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
