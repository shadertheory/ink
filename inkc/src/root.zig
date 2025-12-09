const std = @import("std");
const ink = @import("ink");

const string_map = std.StaticStringMap;
const token_map = string_map(token.kind);
const mem_allocator = std.mem.Allocator;
const array_list = std.array_list.Managed;
const match_list = array_list(node.match_arm);
const mem = std.mem;
const literal = ink.op.literal;
const identifier = ink.op.identifier;
const value = ink.op.value;
const location = ink.op.location; 
const tuple = std.meta.Tuple;
const trait_identifier = ink.trait_identifier;
const type_identifier = ink.type_identifier;

// Define the unified error set for the parser
pub const parser_error = error {
    out_of_memory, // Corresponds to mem.Allocator.Error.NoMem
    parse_float,  // Corresponds to std.fmt.ParseError (which includes InvalidCharacter, Overflow, etc.)
    parse_integer,
    unclosed_group,
    expected_arrow,
    expected_identifier,
    expected_group
};




pub const parser = struct {
    source: []const u8,
    tokens: []const token,
    block: mem_allocator,
    current: usize,
    
    fn peek_token(self: *parser) token {
        return self.tokens[self.current + 1];
    }

    fn current_token(self: *parser) token {
        return self.tokens[self.current];
    }

    fn advance(self: *parser) void {
        self.current += 1;
    }

    pub fn parse(source: []const u8, tokens: []const token, block: mem_allocator) parser_error!parser {
        return parser { .source = source, .tokens = tokens, .block = block, .current = 0};
    }

    pub fn process(self: *parser) parser_error!*node {
        const subject = try self.parse_expr(.none);
        
        if (self.current_token().which == .new_line) self.advance();

        return subject;
    }

    fn parse_expr(self: *parser, minima: precedence) parser_error!*node {
        var left = try self.parse_prefix();

        while (precedence.cmp(minima, token.precedence_of(self.current_token().which))) {
            left = try self.parse_infix(left);
        }

        return left;
    }

    fn parse_prefix(self: *parser) parser_error!*node {
        return switch (self.current_token().which) {
            .number => try self.parse_number(),
            .identifier => try self.parse_identifier(),
            .paren_left, .bracket_left => try self.parse_group(),
            .minus, .bang => try self.parse_unary(),
            .expr_if => try self.parse_if_expr(),
            .expr_match => try self.parse_match(),
            .function => try self.parse_function(),
            .stmt_return => try self.parse_return(),
            else => {
                var stdout_buffer: [1024]u8 = undefined;

                var writer =  std.fs.File.stdout().writer(&stdout_buffer);
                try (writer.interface.print("FAILED PARSING TOKEN `{s}`\n", .{self.current_token().what.string}) catch parser_error.expected_arrow);
                try (writer.interface.flush() catch parser_error.expected_arrow);
                unreachable;
            }
        };
    }

    fn parse_infix(self: *parser, left: *node) parser_error!*node {
        const op = self.current_token().which; 
        self.advance(); 
        if (op == .paren_left) {
            if (self.current_token().which == .paren_right) {
                 self.advance(); 
                 const unit_arg = try self.new_node(.{ .identifier = .{ .string = "unit", .owner = .ref } });
                 return try self.new_node(.{ .binary = .{ .left = left, .op = op, .right = unit_arg } });
            }

            const right = try self.parse_expr(.none);
            
            if (self.current_token().which != .paren_right) return parser_error.unclosed_group;
            self.advance(); // Consume closing ')'
            
            return try self.new_node(.{ .binary = .{ .left = left, .op = op, .right = right } });
        }
        const right = try self.parse_expr(token.precedence_of(op));
        return try self.new_node(.{ .binary = .{ .left = left, .op = op, .right = right } });
    }

    fn parse_return(self: *parser) parser_error!*node {
        const op = self.current_token().which; // stmt_return
        self.advance();
        const right = try self.parse_expr(.none);
        return try self.new_node(.{ .unary = .{ .op = op, .right = right } });
    }

    fn parse_function(self: *parser) parser_error!*node {
        self.advance();

        const function_name = self.current_token();

        if (function_name.which != .identifier) return parser_error.expected_identifier;

        self.advance();

        if(self.current_token().which != .paren_left) return parser_error.expected_group;

        self.advance();

        var params = array_list(node.function.param).init(self.block);

        while (self.current_token().which != .paren_right and self.current_token().which != .end_of_file) {
            const param_name = self.current_token().what;
            self.advance();

            if (self.current_token().which == .colon) {
                self.advance();
                const param_ty = self.current_token().what;
                self.advance(); 
                try (params.append(.{ .name = param_name, .ty = param_ty }) catch parser_error.out_of_memory);
            } else {
                 // Handle error or inferred types
            }       

            if (self.current_token().which == .comma) self.advance();
        }

        self.advance();

        var return_type: ?identifier = null;
        if (self.current_token().which == .arrow) {
            self.advance();
            return_type = self.current_token().what;
            self.advance();
        }

        if (self.current_token().which == .new_line) self.advance();

        if (self.current_token().which == .indent) {
            self.advance(); // Enter indent
        }

        const body = try self.parse_expr(.none);

        if (self.current_token().which == .new_line) self.advance();
        if (self.current_token().which == .dedent) self.advance();

        return try self.new_node(.{ 
            .decl = .{ 
                .name = function_name.what, 
                .params = try (params.toOwnedSlice() catch parser_error.out_of_memory), 
                .return_type = return_type,
                .body = body
            } 
        });
    }

    fn parse_number(self: *parser) parser_error!*node {
        const slice = self.current_token().what;
        self.advance(); // Advance after getting the token

        const is_float = std.mem.indexOfScalar(u8, slice.string, '.') != null or 
                         std.mem.indexOfScalar(u8, slice.string, 'e') != null or
                         std.mem.indexOfScalar(u8, slice.string, 'E') != null;

        if (is_float) {
            const val = std.fmt.parseFloat(f32, slice.string) catch parser_error.parse_float;
            return try self.new_node(.{ .float = try val });
        } else {
            // Parse as base-10 integer
            const val =  std.fmt.parseInt(i64, slice.string, 10) catch parser_error.parse_integer;
            return try self.new_node(.{ .integer = try val });
        } 
    }

    fn parse_identifier(self: *parser) parser_error!*node {
        const t = self.current_token();
        self.advance(); // Advance after getting the token
        return try self.new_node(.{ .identifier = t.what });
    }

    fn parse_group(self: *parser) parser_error!*node {
        self.advance(); // Advance past the opening parenthesis/bracket
        if (self.current_token().which == .paren_right or self.current_token().which == .bracket_right) {
            self.advance(); // Consume closing
            return try self.new_node(.{ .identifier = .{ .string = "unit", .owner = .ref }  }); // Return void unit
        }
        const inner = try self.parse_expr(.none);

        if (self.current_token().which != .paren_right and self.current_token().which != .bracket_right) { // Handle both types of closing delimiters
            return parser_error.unclosed_group;
        }

        self.advance(); // Advance past the closing parenthesis/bracket

        return inner;
    }

    fn parse_unary(self: *parser) parser_error!*node {
        const op = self.current_token().which;
        self.advance(); // Advance past the unary operator

        const right = try self.parse_expr(.unary); // Precedence for unary operators

        return try self.new_node(.{.unary = .{.op = op, .right = right}});
    }

    fn parse_if_expr(self: *parser) parser_error!*node {
        self.advance(); // Advance past 'if'
        const condition = try self.parse_expr(.none);
        const then_branch = try self.parse_expr(.none);
        var else_branch: ?*node = null;
        if (self.current_token().which == .expr_else) {
            self.advance(); // Advance past 'else'
            else_branch = try self.parse_expr(.none);
        }
        return try self.new_node(.{ .if_expr = .{ .condition = condition, .then_branch = then_branch, .else_branch = else_branch } });
    }

    fn parse_match(self: *parser) parser_error!*node {
        self.advance(); // Advance past 'match'

        const target = try self.parse_expr(.none);

        var arms = match_list.init(self.block); // Use std.ArrayList directly and pass allocator

        while (self.current_token().which != .end_of_file) {
            const pattern = try self.parse_expr(.none);

            if (self.current_token().which != .arrow) return parser_error.expected_arrow;
            self.advance(); // Advance past '=>'

            const body = try self.parse_expr(.none);
            
            try (arms.append(.{ .pattern = pattern, .body = body }) catch parser_error.out_of_memory);

            // After parsing a match arm, expect a newline or end of file.
            // If newline, expect indentation for the next arm.
            if (self.current_token().which == .new_line) {
                self.advance();
                if (self.current_token().which != .indent) break; // If no indent, end of match arms
                self.advance(); // Advance past the indent token
            } else {
                break; // If not a newline, we're done with match arms
            }
        }

        return try self.new_node(.{ 
            .match_expr = .{ 
                .target = target, 
                .arms = try (arms.toOwnedSlice() catch parser_error.out_of_memory) 
            } 
        });

    }

    // Use parser_error for all fallible return types
    fn new_node(self: *parser, data: node) parser_error!*node {
        const n = try (self.block.create(node) catch parser_error.out_of_memory); // Map allocator errors to ParserError.out_of_memory
        n.* = data;
        return n;
    }
};

pub const high_level = struct {
    functions: array_list(function_decl),
    structs: array_list(struct_decl),
    entry_point: usize,
};

pub const type_reference = struct {
    id: trait_identifier,
    params: []const type_identifier,
};

pub const trait_reference = struct {
    id: u64,
};

pub const struct_decl = struct {
    name: identifier,
    data: []const field,

    pub const field = struct {
        name: identifier,
        what: type_reference,
    };
};

pub const generic_parameter = struct {
    name: identifier,
    contraints: []const trait_reference,
};

pub const argument = struct {
    name: identifier,
    what: type_reference,
    mutable: bool,
};

pub const type_kind = union(enum) {
    name: identifier,
    generic: identifier,
    reference: *type_reference,
    parameter: struct { base: identifier, params: []const type_reference },
    unit,
    bang,
    self,
    any,
};

pub const function_decl = struct {
    name: identifier,
    generics: []const generic_parameter,
    params: []const argument,
    ret: type_reference,
    body: []const statement,
};

pub const block = struct {
    []const statement,
};

pub const statement = union(enum) {
    declare: struct {
        name: identifier,
        explicit: ?type_reference,
        mutable: bool,
        value: expression,
    },
    assignment: struct {
        target: identifier,
        value: expression,
    },
    flow_return: ?expression,
    flow_if: struct {
        condition: expression,
        then_block: block,
        else_block: ?block,
    },
    flow_loop: struct {
        condition: expression,
        do: block,
    },
    match: struct {
        subject: expression,
        cases: []const match_case,
    },
    standalone: expression,

    pub const match_case = struct {
        against: pattern,
        body: block,

        pub const pattern = union(enum) {
            wildcard,           // `_` (Matches anything)
            lit: literal,
            variant: struct {   
                name: []identifier,   // "Some"
                capture: ?[]identifier,   // "val" (Variable to bind data to)
                },
            };
        };
    };


pub const expression = union(enum) {
    lit: literal,
    ident: identifier,
    binary: struct {
        lhs: *const expression,
        op: ink.binary,
        rhs: *const expression,
    },
    call: struct {
        name: identifier,
        args: []const expression
    },
    init: struct {
        name: identifier,
        assign: []const struct { name: identifier, value: expression },
    },
    access: struct {
        target: *const expression,
        field: identifier,
    }
};

pub const compiler_stage_state = enum {
    done,
    eval,
};

fn compiler(comptime stages: []const type) type {
    return struct {
        process: tuple(&stages), 
        counter: usize,
        ast: []const node,
        program: []const u64,

        pub fn compile(ast: []const node, process: tuple(&stages)) compiler {
            return compiler { .ast = ast, .process = process, .counter = 0 };
        } 

        pub fn transform(self: *compiler) !bool {
            switch (self.process[self.counter].process(self.ast, self.program)) {
                .done => {
                    if (self.counter + 1 >= self.process.len) {
                        return true; 
                    }
                    return false;
                },
                .eval => return false,
            }
        }

        pub fn complete(self: *compiler) []const u64 {
            return self.program;
        }
    };
}

// Function to print the string value of an identifier
pub fn print_identifier_string(id: identifier) void {
    std.debug.print("Identifier: {s}\\n", .{id.string});
}

