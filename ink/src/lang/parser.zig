pub const resolver = @import("../resolver.zig").resolver;
pub const ink = @import("ink");
pub const ast = ink.ast;
pub const token = ink.token;
pub const node = ink.node;
pub const std = @import("std");
const spec = @import("spec.zig");
pub const mem_allocator = std.mem.Allocator;
const array_list = std.ArrayList;
const precedence = ink.precedence;
const match_list = std.ArrayList(ast.match_arm); // Helper based on context

pub const parser_error = error{ out_of_memory, // Corresponds to mem.Allocator.Error.NoMem
    parse_float, // Corresponds to std.fmt.ParseError (which includes InvalidCharacter, Overflow, etc.)
    parse_integer, unexpected_identifier, unclosed_group, unexpected_arrow, expected_identifier, expected_group, unexpected_operator, unexpected_token, failed_dispatch };

pub const parser = struct {
    source: []const u8,
    tokens: []const token,
    block: mem_allocator,
    current: usize,
    last_error: ?error_info,
    furthest_error_pos: usize,

    pub const engine = resolver(token, parser, parser_error!*node, grammer);
    pub const error_info = struct {
        kind: parser_error,
        expected: ?token.kind,
        found: token,
    };
    const prefix_kinds = blk: {
        var kinds: [spec.parser_prefix_kinds.len]token.kind = undefined;
        for (spec.parser_prefix_kinds, 0..) |kind_name, i| {
            kinds[i] = @field(token.kind, kind_name);
        }
        break :blk kinds;
    };
    const unary_ops = blk: {
        var entries: [spec.parser_unary_ops.len]struct { kind: token.kind, op: ink.unary } = undefined;
        for (spec.parser_unary_ops, 0..) |entry, i| {
            entries[i] = .{
                .kind = @field(token.kind, entry.kind),
                .op = @field(ink.unary, entry.op),
            };
        }
        break :blk entries;
    };
    const binary_ops = blk: {
        var entries: [spec.parser_binary_ops.len]struct { kind: token.kind, op: ink.binary } = undefined;
        for (spec.parser_binary_ops, 0..) |entry, i| {
            entries[i] = .{
                .kind = @field(token.kind, entry.kind),
                .op = @field(ink.binary, entry.op),
            };
        }
        break :blk entries;
    };

    fn expect(self: *parser, k: token.kind) parser_error!void {
        if (self.current_token().which != k) return self.record_error(parser_error.unexpected_token, k);
        self.advance();
    }

    fn parse_ident(self: *parser) parser_error!ink.identifier {
        if (self.current_token().which != .identifier) return self.record_error(parser_error.unexpected_identifier, .identifier);
        const id = self.current_token().what;
        self.advance();
        return id;
    }

    fn parse_param_name(self: *parser) parser_error!struct { name: ink.identifier, is_this: bool } {
        if (self.current_token().which == .this) {
            const id = self.current_token().what;
            self.advance();
            return .{ .name = id, .is_this = true };
        }
        return .{ .name = try self.parse_ident(), .is_this = false };
    }

    fn eat_newlines(self: *parser) void {
        while (self.current_token().which == .new_line) self.advance();
    }

    fn is_value_kind(name: []const u8) bool {
        inline for (spec.generic_value_kinds) |k| {
            if (std.mem.eql(u8, name, k)) return true;
        }
        return false;
    }

    fn parse_generic_params(p: *parser) parser_error![]const ast.generic_param {
        var params = array_list(ast.generic_param).empty;

        if (p.current_token().which != .less_than) {
            return params.toOwnedSlice(p.block) catch parser_error.out_of_memory;
        }

        p.advance();

        while (p.current_token().which != .greater_than and p.current_token().which != .end_of_file) {
            const name = try p.parse_ident();

            var kind: ast.generic_kind = .type;
            var constraint: ?*node = null;
            var default: ?*node = null;

            if (p.current_token().which == .colon) {
                p.advance();
                constraint = try p.parse_type();

                if (constraint) |c| {
                    if (c.* == .type and c.type == .name) {
                        if (is_value_kind(c.type.name.string)) {
                            kind = .value;
                        }
                    }
                }
            }

            if (p.current_token().which == .assign) {
                p.advance();
                if (kind == .value) {
                    default = try p.parse_expr(.none);
                } else {
                    default = try p.parse_type();
                }
            }

            try (params.append(p.block, .{
                .name = name,
                .kind = kind,
                .constraint = ast.ref_opt(constraint),
                .default = ast.ref_opt(default),
            }) catch parser_error.out_of_memory);

            if (p.current_token().which == .comma) p.advance();
        }

        try p.expect(.greater_than);

        return params.toOwnedSlice(p.block) catch parser_error.out_of_memory;
    }
    fn parse_type(p: *parser) parser_error!*node {
        if (p.current_token().which == .question) {
            p.advance();
            const inner = try p.parse_type();
            return try p.new_node(.{ .type = .{ .optional = ast.ref(inner) } });
        }

        if (p.current_token().which == .self) {
            p.advance();
            return try p.new_node(.{ .type = .self });
        }

        var base: ink.identifier = undefined;
        if (p.current_token().which == .type) {
            base = p.current_token().what;
            p.advance();
        } else {
            base = try p.parse_ident();
        }

        if (p.current_token().which == .less_than) {
            p.advance(); // <
            var args = array_list(*node).empty;
            while (p.current_token().which != .greater_than and
                p.current_token().which != .end_of_file)
            {
                const arg = try p.parse_type();
                try (args.append(p.block, arg) catch
                    parser_error.out_of_memory);
                if (p.current_token().which == .comma) p.advance();
            }
            try p.expect(.greater_than);
            const args_slice = try (args.toOwnedSlice(p.block) catch
                parser_error.out_of_memory);
            return try p.new_node(.{ .type = .{ .applied = .{ .base = base, .args = ast.ref_slice(args_slice) } } });
        }

        return try p.new_node(.{ .type = .{ .name = base } });
    }
    const grammer = struct {
        pub fn number(p: *parser, t: token) parser_error!*node {
            const slice = t.what;
            try expect(p, t.which);

            const is_float = std.mem.indexOfScalar(u8, slice.string, '.') != null or
                std.mem.indexOfScalar(u8, slice.string, 'e') != null or
                std.mem.indexOfScalar(u8, slice.string, 'E') != null;

            if (is_float) {
                const val = std.fmt.parseFloat(f32, slice.string) catch
                    return p.record_error_at(parser_error.parse_float, null, t);
                return try p.new_node(.{ .float = try val });
            } else {
                // Parse as base-10 integer
                const val = std.fmt.parseInt(i64, slice.string, 10) catch
                    return p.record_error_at(parser_error.parse_integer, null, t);
                return try p.new_node(.{ .integer = try val });
            }
        }

        pub fn identifier(p: *parser, t: token) parser_error!*node {
            try expect(p, t.which);
            return try p.new_node(.{ .identifier = t.what });
        }

        pub fn expr_if(p: *parser, t: token) parser_error!*node {
            try expect(p, t.which);
            const condition = try p.parse_expr(.none);

            var then_indented = false;
            p.eat_newlines();
            if (p.current_token().which == .indent) {
                then_indented = true;
                p.advance();
            }
            const then_branch = try p.parse_expr(.none);
            if (then_indented) {
                p.eat_newlines();
                if (p.current_token().which == .dedent) p.advance();
            }

            var else_branch: ?*node = null;
            if (p.current_token().which == .expr_else) {
                p.advance(); // Advance past 'else'
                var else_indented = false;
                p.eat_newlines();
                if (p.current_token().which == .indent) {
                    else_indented = true;
                    p.advance();
                }
                else_branch = try p.parse_expr(.none);
                if (else_indented) {
                    p.eat_newlines();
                    if (p.current_token().which == .dedent) p.advance();
                }
            }
            return try p.new_node(.{ .if_expr = .{
                .condition = ast.ref(condition),
                .then_branch = ast.ref(then_branch),
                .else_branch = ast.ref_opt(else_branch),
            } });
        }

        pub fn parse_match(p: *parser, t: token) parser_error!*node {
            try expect(p, t.which);

            const target = try p.parse_expr(.none);

            var arms = match_list.empty; // Use std.ArrayList directly and pass allocator

            while (p.current_token().which != .end_of_file) {
                const pattern = try p.parse_expr(.none);

                if (p.current_token().which != .arrow) return p.record_error(parser_error.unexpected_arrow, .arrow);
                p.advance(); // Advance past '=>'

                const body = try p.parse_expr(.none);

                try (arms.append(p.block, .{
                    .pattern = ast.ref(pattern),
                    .body = ast.ref(body),
                }) catch parser_error.out_of_memory);

                // After parsing a match arm, accept either an indent directly
                // (lexer emits indent instead of newline when depth increases),
                // or a newline followed by an indent.
                if (p.current_token().which == .indent) {
                    p.advance();
                    continue;
                }
                if (p.current_token().which == .new_line) {
                    p.advance();
                    if (p.current_token().which != .indent) break; // If no indent, end of match arms
                    p.advance(); // Advance past the indent token
                } else {
                    break; // If not a newline, we're done with match arms
                }
            }

            return try p.new_node(.{ .match_expr = .{
                .target = ast.ref(target),
                .arms = try (arms.toOwnedSlice(p.block) catch parser_error.out_of_memory),
            } });
        }

        pub fn function(p: *parser, t: token) parser_error!*node {
            try expect(p, t.which);

            const name = try p.parse_ident();
            const generics = try parse_generic_params(p);

            try expect(p, .paren_left);

            var params = array_list(ast.param).empty;
            while (p.current_token().which != .paren_right and p.current_token().which != .end_of_file) {
                const param = try p.parse_param_name();
                var param_ty: *node = undefined;
                if (param.is_this and p.current_token().which != .colon) {
                    param_ty = try p.new_node(.{ .type = .self });
                } else {
                    try expect(p, .colon);
                    param_ty = try p.parse_type();
                }

                try (params.append(p.block, .{ .name = param.name, .ty = ast.ref(param_ty) }) catch parser_error.out_of_memory);

                if (p.current_token().which == .comma) p.advance();
            }
            try p.expect(.paren_right);

            var return_type: ?*node = null;
            if (p.current_token().which == .arrow) {
                p.advance();
                return_type = try p.parse_type();
            }

            var where_clause = array_list(ast.where_req).empty;
            if (p.current_token().which == .where) {
                p.advance();
                const wname = try p.parse_ident();
                try p.expect(.colon);
                const wcon = try p.parse_type();
                try (where_clause.append(p.block, .{ .name = wname, .constraint = ast.ref(wcon) }) catch parser_error.out_of_memory);
            }

            p.eat_newlines();
            if (p.current_token().which != .indent) {
                return try p.new_node(.{
                    .decl = .{ .function = .{
                        .name = name,
                        .generics = generics,
                        .params = try (params.toOwnedSlice(p.block) catch parser_error.out_of_memory),
                        .return_type = ast.ref_opt(return_type),
                        .where_clause = try (where_clause.toOwnedSlice(p.block) catch parser_error.out_of_memory),
                        .body = null,
                    } },
                });
            }

            p.advance(); // indent
            const body = try p.parse_expr(.none);

            p.eat_newlines();
            if (p.current_token().which == .dedent) p.advance();

            return try p.new_node(.{
                .decl = .{ .function = .{
                    .name = name,
                    .generics = generics,
                    .params = try (params.toOwnedSlice(p.block) catch parser_error.out_of_memory),
                    .return_type = ast.ref_opt(return_type),
                    .where_clause = try (where_clause.toOwnedSlice(p.block) catch parser_error.out_of_memory),
                    .body = ast.ref(body),
                } },
            });
        }

        pub fn @"struct"(p: *parser, t: token) parser_error!*node {
            try expect(p, t.which);
            const name = try p.parse_ident();
            const generics = try parse_generic_params(p);

            p.eat_newlines();
            if (p.current_token().which != .indent) {
                return try p.new_node(.{
                    .decl = .{ .@"struct" = .{
                        .name = name,
                        .generics = generics,
                        .fields = &.{},
                    } },
                });
            }

            p.advance(); // indent
            var fields = array_list(ast.struct_field).empty;
            while (p.current_token().which != .dedent and p.current_token().which != .end_of_file) {
                if (p.current_token().which == .new_line) {
                    p.advance();
                    continue;
                }
                const field_name = try p.parse_ident();
                try p.expect(.colon);
                const field_ty = try p.parse_type();
                try (fields.append(p.block, .{ .name = field_name, .ty = ast.ref(field_ty) }) catch parser_error.out_of_memory);
            }

            if (p.current_token().which == .dedent) p.advance();

            return try p.new_node(.{
                .decl = .{ .@"struct" = .{
                    .name = name,
                    .generics = generics,
                    .fields = try (fields.toOwnedSlice(p.block) catch parser_error.out_of_memory),
                } },
            });
        }

        pub fn stmt_return(p: *parser, _: token) parser_error!*node {
            const op = try p.token_kind_to_unary(p.current_token());
            p.advance();
            const right = try p.parse_expr(.none);
            return try p.new_node(.{ .unary = .{ .op = op, .right = ast.ref(right) } });
        }
        pub fn group(p: *parser, closing: token.kind) parser_error!*node {
            p.advance(); // Advance past the opening parenthesis/bracket
            if (p.current_token().which == closing) {
                p.advance(); // Consume closing
                return try p.new_node(.{ .identifier = .{ .string = "unit", .owner = .ref } }); // Return void unit
            }
            const inner = try p.parse_expr(.none);

            if (p.current_token().which != closing) return p.record_error(parser_error.unclosed_group, closing);

            p.advance(); // Advance past the closing parenthesis/bracket

            return inner;
        }
        pub fn paren_left(p: *parser, _: token) parser_error!*node {
            return grammer.group(p, .paren_right);
        }
        pub fn bracket_left(p: *parser, _: token) parser_error!*node {
            return grammer.group(p, .bracket_right);
        }
        pub fn unary(p: *parser, _: token) parser_error!*node {
            const op_token = p.current_token();
            const op = try p.token_kind_to_unary(op_token);
            p.advance(); // Advance past the unary operator

            const right = try p.parse_expr(.unary); // Precedence for unary operators

            return try p.new_node(.{ .unary = .{ .op = op, .right = ast.ref(right) } });
        }
        pub fn bang(p: *parser, _: token) parser_error!*node {
            return grammer.unary(p, p.current_token());
        }
        pub fn minus(p: *parser, _: token) parser_error!*node {
            return grammer.unary(p, p.current_token());
        }
        pub fn logical_not(p: *parser, _: token) parser_error!*node {
            return grammer.unary(p, p.current_token());
        }
        pub fn logical_true(p: *parser, t: token) parser_error!*node {
            p.advance();
            return try p.new_node(.{ .identifier = t.what });
        }
        pub fn logical_false(p: *parser, t: token) parser_error!*node {
            p.advance();
            return try p.new_node(.{ .identifier = t.what });
        }
        pub fn this(p: *parser, t: token) parser_error!*node {
            p.advance();
            return try p.new_node(.{ .identifier = t.what });
        }
        pub fn expr_match(p: *parser, _: token) parser_error!*node {
            return grammer.parse_match(p, p.current_token());
        }

        pub fn impl(p: *parser, t: token) parser_error!*node {
            try expect(p, t.which);
            const by_trait = try p.parse_ident();
            try p.expect(.@"for");
            const for_struct = try p.parse_ident();

            p.eat_newlines();
            if (p.current_token().which != .indent) {
                return try p.new_node(.{
                    .decl = .{ .impl = .{
                        .by_trait = by_trait,
                        .for_struct = for_struct,
                        .functions = &.{},
                    } },
                });
            }

            p.advance(); // indent
            var functions = array_list(ast.function_decl).empty;
            while (p.current_token().which != .dedent and p.current_token().which != .end_of_file) {
                if (p.current_token().which == .new_line) {
                    p.advance();
                    continue;
                }
                if (p.current_token().which != .function) return p.record_error(parser_error.unexpected_token, .function);
                const func_node = try grammer.function(p, p.current_token());
                if (func_node.* != .decl or func_node.decl != .function) {
                    return p.record_error(parser_error.unexpected_token, null);
                }
                try (functions.append(p.block, func_node.decl.function) catch parser_error.out_of_memory);
            }

            if (p.current_token().which == .dedent) p.advance();

            return try p.new_node(.{
                .decl = .{ .impl = .{
                    .by_trait = by_trait,
                    .for_struct = for_struct,
                    .functions = try (functions.toOwnedSlice(p.block) catch parser_error.out_of_memory),
                } },
            });
        }

        pub fn trait(p: *parser, t: token) parser_error!*node {
            try expect(p, t.which);
            const name = try p.parse_ident();
            const generics = try parse_generic_params(p);

            p.eat_newlines();
            if (p.current_token().which != .indent) {
                return try p.new_node(.{
                    .decl = .{ .trait = .{
                        .name = name,
                        .generics = generics,
                        .items = &.{},
                    } },
                });
            }

            p.advance(); // indent
            var items = array_list(ast.trait_item).empty;
            while (p.current_token().which != .dedent and p.current_token().which != .end_of_file) {
                if (p.current_token().which == .new_line) {
                    p.advance();
                    continue;
                }

                switch (p.current_token().which) {
                    .function => {
                        const func_node = try grammer.function(p, p.current_token());
                        if (func_node.* != .decl or func_node.decl != .function) {
                            return p.record_error(parser_error.unexpected_token, null);
                        }
                        try (items.append(p.block, .{ .function = func_node.decl.function }) catch parser_error.out_of_memory);
                    },
                    .type => {
                        p.advance();
                        const assoc_name = try p.parse_ident();
                        var assoc_value: ?*node = null;
                        if (p.current_token().which == .assign) {
                            p.advance();
                            assoc_value = try p.parse_type();
                        }
                        try (items.append(p.block, .{ .assoc_type = .{ .name = assoc_name, .value = ast.ref_opt(assoc_value) } }) catch parser_error.out_of_memory);
                    },
                    else => return p.record_error(parser_error.unexpected_token, null),
                }
            }

            if (p.current_token().which == .dedent) p.advance();

            return try p.new_node(.{
                .decl = .{ .trait = .{
                    .name = name,
                    .generics = generics,
                    .items = try (items.toOwnedSlice(p.block) catch parser_error.out_of_memory),
                } },
            });
        }

        pub fn concept(p: *parser, t: token) parser_error!*node {
            try expect(p, t.which);
            const name = try p.parse_ident();
            const generics = try parse_generic_params(p);

            p.eat_newlines();
            if (p.current_token().which != .indent) {
                return try p.new_node(.{
                    .decl = .{ .concept = .{
                        .name = name,
                        .generics = generics,
                        .requires = &.{},
                    } },
                });
            }

            p.advance(); // indent
            var requires = array_list(*node).empty;
            while (p.current_token().which != .dedent and p.current_token().which != .end_of_file) {
                if (p.current_token().which == .new_line) {
                    p.advance();
                    continue;
                }
                try expect(p, .requires);
                const req = try p.parse_type();
                try (requires.append(p.block, req) catch parser_error.out_of_memory);
            }

            if (p.current_token().which == .dedent) p.advance();

            const requires_slice = try (requires.toOwnedSlice(p.block) catch parser_error.out_of_memory);
            return try p.new_node(.{
                .decl = .{ .concept = .{
                    .name = name,
                    .generics = generics,
                    .requires = ast.ref_slice(requires_slice),
                } },
            });
        }

        pub fn sum(p: *parser, t: token) parser_error!*node {
            try expect(p, t.which);
            const name = try p.parse_ident();
            const generics = try parse_generic_params(p);

            p.eat_newlines();
            if (p.current_token().which != .indent) {
                return try p.new_node(.{
                    .decl = .{ .sum = .{
                        .name = name,
                        .generics = generics,
                        .variants = &.{},
                    } },
                });
            }

            p.advance(); // indent
            var variants = array_list(ast.sum_variant).empty;
            while (p.current_token().which != .dedent and p.current_token().which != .end_of_file) {
                if (p.current_token().which == .new_line) {
                    p.advance();
                    continue;
                }
                const variant_name = try p.parse_ident();
                var payload: ?*node = null;
                if (p.current_token().which == .paren_left) {
                    p.advance();
                    if (p.current_token().which != .paren_right) {
                        payload = try p.parse_type();
                    }
                    if (p.current_token().which != .paren_right) return p.record_error(parser_error.unclosed_group, .paren_right);
                    p.advance();
                }
                try (variants.append(p.block, .{ .name = variant_name, .payload = ast.ref_opt(payload) }) catch parser_error.out_of_memory);
            }

            if (p.current_token().which == .dedent) p.advance();

            return try p.new_node(.{
                .decl = .{ .sum = .{
                    .name = name,
                    .generics = generics,
                    .variants = try (variants.toOwnedSlice(p.block) catch parser_error.out_of_memory),
                } },
            });
        }

        pub fn @"enum"(p: *parser, t: token) parser_error!*node {
            try expect(p, t.which);
            const name = try p.parse_ident();

            p.eat_newlines();
            if (p.current_token().which != .indent) {
                return try p.new_node(.{
                    .decl = .{ .@"enum" = .{
                        .name = name,
                        .cases = &.{},
                    } },
                });
            }

            p.advance(); // indent
            var cases = array_list(ink.identifier).empty;
            while (p.current_token().which != .dedent and p.current_token().which != .end_of_file) {
                if (p.current_token().which == .new_line) {
                    p.advance();
                    continue;
                }
                const case_name = try p.parse_ident();
                try (cases.append(p.block, case_name) catch parser_error.out_of_memory);
            }

            if (p.current_token().which == .dedent) p.advance();

            return try p.new_node(.{
                .decl = .{ .@"enum" = .{
                    .name = name,
                    .cases = try (cases.toOwnedSlice(p.block) catch parser_error.out_of_memory),
                } },
            });
        }
    };

    fn peek_token(self: *parser) token {
        return self.tokens[self.current + 1];
    }

    fn current_token(self: *parser) token {
        return self.tokens[self.current];
    }

    fn advance(self: *parser) void {
        self.current += 1;
    }

    pub fn at_end(self: *parser) bool {
        return self.current_token().which == .end_of_file;
    }

    pub fn parse(source: []const u8, tokens: []const token, block: mem_allocator) parser_error!parser {
        return parser{
            .source = source,
            .tokens = tokens,
            .block = block,
            .current = 0,
            .last_error = null,
            .furthest_error_pos = 0,
        };
    }

    pub fn process(self: *parser) parser_error!*node {
        self.skip_layout();
        const subject = try self.parse_expr(.none);

        if (self.current_token().which == .new_line) self.advance();

        return subject;
    }

    fn parse_expr(self: *parser, minima: precedence) parser_error!*node {
        self.skip_layout();
        var left = try self.parse_prefix();

        while (precedence.cmp(minima, token.precedence_of(self.current_token().which))) {
            left = try self.parse_infix(left);
        }

        return left;
    }

    fn parse_prefix(self: *parser) parser_error!*node {
        self.skip_layout();
        if (!is_prefix_kind(self.current_token().which)) {
            return self.record_error(parser_error.unexpected_token, null);
        }
        return try engine.dispatch(self, self.current_token());
    }

    fn is_prefix_kind(kind: token.kind) bool {
        inline for (prefix_kinds) |k| {
            if (kind == k) return true;
        }
        return false;
    }

    fn parse_infix(self: *parser, left: *node) parser_error!*node {
        const op = self.current_token().which;
        self.advance();
        if (op == .paren_left) {
            if (self.current_token().which == .paren_right) {
                self.advance();
                const unit_arg = try self.new_node(.{ .identifier = .{ .string = "unit", .owner = .ref } });
                const bin = try token_kind_to_binary(op);
                return try self.new_node(.{ .binary = .{ .left = ast.ref(left), .op = bin, .right = ast.ref(unit_arg) } });
            }

            const right = try self.parse_expr(.none);

            if (self.current_token().which != .paren_right) {
                return self.record_error(parser_error.unclosed_group, .paren_right);
            }
            self.advance(); // Consume closing ')'

            const bin = try token_kind_to_binary(op);
            return try self.new_node(.{ .binary = .{ .left = ast.ref(left), .op = bin, .right = ast.ref(right) } });
        }
        const right = try self.parse_expr(token.precedence_of(op));
        const bin = try token_kind_to_binary(op);
        return try self.new_node(.{ .binary = .{ .left = ast.ref(left), .op = bin, .right = ast.ref(right) } });
    }

    fn token_kind_to_binary(kind: token.kind) parser_error!ink.binary {
        inline for (binary_ops) |entry| {
            if (entry.kind == kind) return entry.op;
        }
        std.debug.print("unexpected operator {any}", .{kind});
        return parser_error.unexpected_operator;
    }

    fn token_kind_to_unary(kind: token.kind) parser_error!ink.unary {
        inline for (unary_ops) |entry| {
            if (entry.kind == kind) return entry.op;
        }
        std.debug.print("unexpected operator {any}", .{kind});
        return parser_error.unexpected_operator;
    }

    fn skip_layout(self: *parser) void {
        while (true) {
            switch (self.current_token().which) {
                .new_line, .dedent => self.advance(),
                else => return,
            }
        }
    }

    fn record_error(self: *parser, kind: parser_error, expected: ?token.kind) parser_error {
        return self.record_error_at(kind, expected, self.current_token());
    }

    fn record_error_at(self: *parser, kind: parser_error, expected: ?token.kind, found: token) parser_error {
        const pos = found.where.start;
        if (pos >= self.furthest_error_pos) {
            self.furthest_error_pos = pos;
            self.last_error = .{
                .kind = kind,
                .expected = expected,
                .found = found,
            };
        }
        return kind;
    }

    // Use parser_error for all fallible return types
    fn new_node(self: *parser, data: node) parser_error!*node {
        const n = try (self.block.create(node) catch parser_error.out_of_memory); // Map allocator errors to ParserError.out_of_memory
        n.* = data;
        return n;
    }
};
