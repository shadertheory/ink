pub const resolver = @import("../resolver.zig").resolver;
pub const ink = @import("ink");
pub const token = ink.token;
pub const node = ink.node;
pub const std = @import("std");
pub const mem_allocator = std.mem.Allocator;
const array_list = std.ArrayList;
const precedence = ink.precedence;
const match_list = std.ArrayList(node.match_arm); // Helper based on context

pub const parser_error = error{ out_of_memory, // Corresponds to mem.Allocator.Error.NoMem
    parse_float, // Corresponds to std.fmt.ParseError (which includes InvalidCharacter, Overflow, etc.)
    parse_integer, unexpected_identifier, unclosed_group, unexpected_arrow, expected_identifier, expected_group, unexpected_operator, unexpected_token, failed_dispatch };

pub const parser = struct {
    source: []const u8,
    tokens: []const token,
    block: mem_allocator,
    current: usize,

    pub const engine = resolver(token, parser, parser_error!*node, grammer);

    fn expect(self: *parser, k: token.kind) parser_error!void {
        if (self.current_token().which != k) return parser_error.unexpected_token;
        self.advance();
    }

    fn parse_ident(self: *parser) parser_error!ink.identifier {
        if (self.current_token().which != .identifier) return parser_error.unexpected_identifier;
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

    const value_kinds = [_][]const u8{ "int", "bool", "float" };
    fn is_value_kind(name: []const u8) bool {
        inline for (value_kinds) |k| {
            if (std.mem.eql(u8, name, k)) return true;
        }
        return false;
    }

    fn parse_generic_params(p: *parser) parser_error![]const node.generic_param {
        var params = array_list(node.generic_param).empty;

        if (p.current_token().which != .less_than) {
            return params.toOwnedSlice(p.block) catch parser_error.out_of_memory;
        }

        p.advance();

        while (p.current_token().which != .greater_than and p.current_token().which != .end_of_file) {
            const name = try p.parse_ident();

            var kind: node.generic_param.generic_kind = .type;
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
                .constraint = constraint,
                .default = default,
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
            return try p.new_node(.{ .type = .{ .optional = inner } });
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
            return try p.new_node(.{ .type = .{ .applied = .{ .base = base, .args = try (args.toOwnedSlice(p.block) catch
                parser_error.out_of_memory) } } });
        }

        return try p.new_node(.{ .type = .{ .name = base } });
    }
    const grammer = struct {
        const value_kinds = [_][]const u8{ "int", "bool", "float" };
        pub fn number(p: *parser, t: token) parser_error!*node {
            const slice = t.what;
            try expect(p, t.which);

            const is_float = std.mem.indexOfScalar(u8, slice.string, '.') != null or
                std.mem.indexOfScalar(u8, slice.string, 'e') != null or
                std.mem.indexOfScalar(u8, slice.string, 'E') != null;

            if (is_float) {
                const val = std.fmt.parseFloat(f32, slice.string) catch parser_error.parse_float;
                return try p.new_node(.{ .float = try val });
            } else {
                // Parse as base-10 integer
                const val = std.fmt.parseInt(i64, slice.string, 10) catch parser_error.parse_integer;
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
            return try p.new_node(.{ .if_expr = .{ .condition = condition, .then_branch = then_branch, .else_branch = else_branch } });
        }

        pub fn parse_match(p: *parser, t: token) parser_error!*node {
            try expect(p, t.which);

            const target = try p.parse_expr(.none);

            var arms = match_list.empty; // Use std.ArrayList directly and pass allocator

            while (p.current_token().which != .end_of_file) {
                const pattern = try p.parse_expr(.none);

                if (p.current_token().which != .arrow) return parser_error.unexpected_arrow;
                p.advance(); // Advance past '=>'

                const body = try p.parse_expr(.none);

                try (arms.append(p.block, .{ .pattern = pattern, .body = body }) catch parser_error.out_of_memory);

                // After parsing a match arm, try expect a newline or end of file.
                // If newline, try expect indentation for the next arm.
                if (p.current_token().which == .new_line) {
                    p.advance();
                    if (p.current_token().which != .indent) break; // If no indent, end of match arms
                    p.advance(); // Advance past the indent token
                } else {
                    break; // If not a newline, we're done with match arms
                }
            }

            return try p.new_node(.{ .match_expr = .{ .target = target, .arms = try (arms.toOwnedSlice(p.block) catch parser_error.out_of_memory) } });
        }

        pub fn function(p: *parser, t: token) parser_error!*node {
            try expect(p, t.which);

            const name = try p.parse_ident();
            const generics = try parse_generic_params(p);

            try expect(p, .paren_left);

            var params = array_list(node.function_decl.param).empty;
            while (p.current_token().which != .paren_right and p.current_token().which != .end_of_file) {
                const param = try p.parse_param_name();
                var param_ty: *node = undefined;
                if (param.is_this and p.current_token().which != .colon) {
                    param_ty = try p.new_node(.{ .type = .self });
                } else {
                    try expect(p, .colon);
                    param_ty = try p.parse_type();
                }

                try (params.append(p.block, .{ .name = param.name, .ty = param_ty }) catch parser_error.out_of_memory);

                if (p.current_token().which == .comma) p.advance();
            }
            try p.expect(.paren_right);

            var return_type: ?*node = null;
            if (p.current_token().which == .arrow) {
                p.advance();
                return_type = try p.parse_type();
            }

            var where_clause = array_list(node.function_decl.where_req).empty;
            if (p.current_token().which == .where) {
                p.advance();
                const wname = try p.parse_ident();
                try p.expect(.colon);
                const wcon = try p.parse_type();
                try (where_clause.append(p.block, .{ .name = wname, .constraint = wcon }) catch parser_error.out_of_memory);
            }

            p.eat_newlines();
            if (p.current_token().which != .indent) {
                return try p.new_node(.{
                    .decl = .{ .function = .{
                        .name = name,
                        .generics = generics,
                        .params = try (params.toOwnedSlice(p.block) catch parser_error.out_of_memory),
                        .return_type = return_type,
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
                    .return_type = return_type,
                    .where_clause = try (where_clause.toOwnedSlice(p.block) catch parser_error.out_of_memory),
                    .body = body,
                } },
            });
        }

        pub fn stmt_return(p: *parser, _: token) parser_error!*node {
            const op = try token_kind_to_unary(p.current_token().which);
            p.advance();
            const right = try p.parse_expr(.none);
            return try p.new_node(.{ .unary = .{ .op = op, .right = right } });
        }
        pub fn group(p: *parser) parser_error!*node {
            p.advance(); // Advance past the opening parenthesis/bracket
            if (p.current_token().which == .paren_right or p.current_token().which == .bracket_right) {
                p.advance(); // Consume closing
                return try p.new_node(.{ .identifier = .{ .string = "unit", .owner = .ref } }); // Return void unit
            }
            const inner = try p.parse_expr(.none);

            if (p.current_token().which != .paren_right and p.current_token().which != .bracket_right) { // Handle both types of closing delimiters
                return parser_error.unclosed_group;
            }

            p.advance(); // Advance past the closing parenthesis/bracket

            return inner;
        }
        pub fn paren_left(p: *parser, _: token) parser_error!*node {
            return grammer.group(p);
        }
        pub fn bracket_left(p: *parser, _: token) parser_error!*node {
            return grammer.group(p);
        }
        pub fn unary(p: *parser, _: token) parser_error!*node {
            const op = try token_kind_to_unary(p.current_token().which);
            p.advance(); // Advance past the unary operator

            const right = try p.parse_expr(.unary); // Precedence for unary operators

            return try p.new_node(.{ .unary = .{ .op = op, .right = right } });
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
        pub fn expr_match(p: *parser, _: token) parser_error!*node {
            return grammer.parse_match(p, p.current_token());
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
            var items = array_list(node.trait_decl.trait_item).empty;
            while (p.current_token().which != .dedent and p.current_token().which != .end_of_file) {
                if (p.current_token().which == .new_line) {
                    p.advance();
                    continue;
                }

                switch (p.current_token().which) {
                    .function => {
                        const func_node = try grammer.function(p, p.current_token());
                        if (func_node.* != .decl or func_node.decl != .function) {
                            return parser_error.unexpected_token;
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
                        try (items.append(p.block, .{ .assoc_type = .{ .name = assoc_name, .value = assoc_value } }) catch parser_error.out_of_memory);
                    },
                    else => return parser_error.unexpected_token,
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

            return try p.new_node(.{
                .decl = .{ .concept = .{
                    .name = name,
                    .generics = generics,
                    .requires = try (requires.toOwnedSlice(p.block) catch parser_error.out_of_memory),
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
            var variants = array_list(node.sum_decl.sum_variant).empty;
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
                    if (p.current_token().which != .paren_right) return parser_error.unclosed_group;
                    p.advance();
                }
                try (variants.append(p.block, .{ .name = variant_name, .payload = payload }) catch parser_error.out_of_memory);
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
        return parser{ .source = source, .tokens = tokens, .block = block, .current = 0 };
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
        return try engine.dispatch(self, self.current_token());
    }

    fn parse_infix(self: *parser, left: *node) parser_error!*node {
        const op = self.current_token().which;
        self.advance();
        if (op == .paren_left) {
            if (self.current_token().which == .paren_right) {
                self.advance();
                const unit_arg = try self.new_node(.{ .identifier = .{ .string = "unit", .owner = .ref } });
                const bin = try token_kind_to_binary(op);
                return try self.new_node(.{ .binary = .{ .left = left, .op = bin, .right = unit_arg } });
            }

            const right = try self.parse_expr(.none);

            if (self.current_token().which != .paren_right) return parser_error.unclosed_group;
            self.advance(); // Consume closing ')'

            const bin = try token_kind_to_binary(op);
            return try self.new_node(.{ .binary = .{ .left = left, .op = bin, .right = right } });
        }
        const right = try self.parse_expr(token.precedence_of(op));
        const bin = try token_kind_to_binary(op);
        return try self.new_node(.{ .binary = .{ .left = left, .op = bin, .right = right } });
    }

    fn token_kind_to_binary(kind: token.kind) parser_error!ink.binary {
        return switch (kind) {
            .plus => .add,
            .minus => .sub,
            .asterisk => .mul,
            .slash => .div,
            .less_than => .less_than,
            .greater_than => .greater_than,
            .less_or_equal => .less_or_equal,
            .greater_or_equal => .greater_or_equal,
            .equal => .equal,
            .not_equal => .not_equal,
            .paren_left => .call,
            else => parser_error.unexpected_operator,
        };
    }

    fn token_kind_to_unary(kind: token.kind) parser_error!ink.unary {
        return switch (kind) {
            .minus => .neg,
            .bang, .logical_not => .not,
            .stmt_return => .ret,
            else => parser_error.unexpected_operator,
        };
    }

    fn skip_layout(self: *parser) void {
        while (true) {
            switch (self.current_token().which) {
                .new_line, .dedent => self.advance(),
                else => return,
            }
        }
    }

    // Use parser_error for all fallible return types
    fn new_node(self: *parser, data: node) parser_error!*node {
        const n = try (self.block.create(node) catch parser_error.out_of_memory); // Map allocator errors to ParserError.out_of_memory
        n.* = data;
        return n;
    }
};
