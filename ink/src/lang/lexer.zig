pub const token = struct {
    which: kind,
    where: location,
    what: identifier,

    pub const kind = enum {
        end_of_file,
        illegal,

        indent,
        dedent,
        new_line,

        identifier,
        string,
        number,
        comma,
        colon,
        dot,

        function,
        constant,
        variable,
        expr_if,
        expr_else,
        expr_match,
        stmt_return,
        logical_or,
        logical_and,
        logical_xor,
        logical_not,
        logical_false,
        logical_true,

        paren_left,
        paren_right,
        bracket_left,
        bracket_right,

        assign,
        plus,
        minus,
        asterisk,
        slash,
        bang,

        less_than,
        greater_than,

        arrow
    };

    pub fn precedence_of(this: kind) precedence {
        return switch (this) {
            .assign => .assignment,
            .logical_or, .logical_xor => .logical_or,
            .logical_and => .logical_and, 
            .less_than, .greater_than => .comparison,
            .plus, .minus => .sum,
            .asterisk, .slash => .product,
            .bang, .logical_not => .unary,
            .paren_left, .bracket_left, .dot => .call,
            else => .none,
        };
    } 
};

pub const precedence = enum(u8) {
    none = 0,
    assignment,
    logical_or,
    logical_and,
    comparison,
    sum,
    product,
    unary,
    call,
    primary,

    pub fn value(self: precedence)  usize {
        return @as(usize, @intFromEnum(self));
    }

    pub fn cmp(lhs: precedence, rhs: precedence)  bool {
        return lhs.value() < rhs.value();
    }
};

const keywords = token_map.initComptime(&.{
    .{ "fn", .function },
    .{ "const", .constant },
    .{ "var", .variable },
    .{ "if", .expr_if },
    .{ "else", .expr_else },
    .{ "match", .expr_match },
    .{ "return", .stmt_return },
    .{ "or", .logical_or },
    .{ "and", .logical_and },
    .{ "xor", .logical_xor },
    .{ "not", .logical_not },
    .{ "false", .logical_false },
    .{ "true", .logical_true },
}); 

pub const lexer = struct {
	source: []const u8,
        source_len: usize,
	current: u8,
	head: usize,
	tail: usize,
        indent: stack,
        dedent: u8,
        cursor: u8,
        
        const stack = [255]u8;

        pub fn init(source: []const u8) !lexer {
            const source_len = source.len;

            var instance = lexer {
                .source = source,
                .source_len = source_len,
                .current = 0,
                .head = 0,
                .tail = 0,
                .indent = undefined,
                .cursor = undefined,
                .dedent = 0,
            };

            instance.read_char();
            instance.push(0);

            return instance;
        }

        fn push(self: *lexer, i: u8) void {
            self.indent[self.cursor] = i;
            self.cursor += 1;
        }

        fn pop(self: *lexer) void {
            self.indent[self.cursor] = undefined;
            self.cursor -= 1;
        }

        fn read_char(self: *lexer) void {
		if (self.tail >= self.source.len) {
			self.current = 0;
		} else {
			self.current = self.source[self.tail];
		}
		self.head = self.tail;
		self.tail += 1;
	}

        fn peek_char(self: *lexer) u8 {
		if (self.tail >= self.source.len) return 0;
		return self.source[self.tail];
            }

            fn delineate_source_slice(self: *lexer, where: location) identifier {
                return identifier{
                    
                        .string = self.source[where.start .. where.end],
                    .owner = .ref,
                };
            }
            fn delineate(self: *lexer, which: token.kind) token {
                return self.delineate_from(which, self.head);
            }

            fn delineate_from(self: *lexer, which: token.kind, start: usize) token {
                const where = location { .start = start, .end = self.head};
                const what = self.delineate_source_slice(where);
                return token { .which = which, .where = where, .what = what };
            }

            fn match_newline(self: *lexer) ?token {
                const start = self.head;
                self.read_char();

                var tab_count: u8 = 0;
                while (self.current == '\t') {
                    tab_count += 1;
                    self.read_char();
                }

                const current_depth = self.indent[self.cursor - 1];

                if (tab_count > current_depth) {
                    self.push(tab_count);
                    return self.delineate_from(.indent, start);
                } else if (tab_count < current_depth) {
                    var pops: u8 = 0;
                    while (self.indent[self.cursor - 1] > tab_count) {
                        self.pop();
                        pops += 1;
                    }
                    
                    if (pops > 1) {
                        self.dedent = pops - 1;
                    }
                    return self.delineate_from(.dedent, start);
                }

                return self.delineate_from(.new_line, start);
            }

            fn match_whitespace_token(self: *lexer) ?token {
                    if (self.current == ' ') {
                        return null;
                    }

                    if (self.current == '\n') {
                        return self.match_newline();
                    }

                    if (self.current == '\t') {
                        self.read_char();
                        return null; 
                    }

                    return null; 
            }

            fn match_simple_token_greedy(self: *lexer) ?token {
                const start = self.head;
                var kind: ?token.kind = null;

                if (self.current == '-' and self.peek_char() == '>') {
                    self.read_char(); 
                    self.read_char(); 
                    kind = .arrow;
                }

                if (kind == null) {
                    kind = switch(self.current) {
                        ']' => .bracket_right,
                        '[' => .bracket_left,
                        ')' => .paren_right,
                        '(' => .paren_left,
                        '+' => .plus,
                        '*' => .asterisk,
                        '/' => .slash,
                        '=' => .assign,
                        ':' => .colon, 
                        '!' => .bang,
                        '<' => .less_than,
                        '>' => .greater_than,
                        ',' => .comma,
                        '.' => .dot,
                        '-' => .minus,
                        else => null
                    };
                    
                    if (kind != null) {
                        self.read_char();
                    }
                }

                if (kind) |k| {
                    return self.delineate_from(k, start);
                }
                return null;
            }      

            fn match_alpha_token(self: *lexer) ?token {
                switch(self.current) {
                    'a'...'z', 'A'...'Z', '_' =>  {
                            const start = self.head;
			while (is_alpha(self.current) or is_digit(self.current)) {
				self.read_char();
			}
                        const text = self.source[start..self.head];
                        const kind = keywords.get(text) orelse .identifier;
                        return self.delineate_from(kind, start);
		},
                else => return null,
            }
        }

        fn match_number_token(self: *lexer) ?token {
            const start = self.head;
            switch (self.current) {
                '0'...'9' => {
			while (is_digit(self.current)) {
				self.read_char();
			}
                        return self.delineate_from(.number, start);
                },
                else => return null,
            }
        }

        fn match_end_of_file(self: *lexer) ?token {
            if (self.current == 0) {
                return self.delineate(.end_of_file);
            }
            return null;
        }

        fn done(self: *lexer) bool {
            return self.head >= self.source_len;
        }

        fn is_alpha(c: u8) bool {
                return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
        }

        fn is_digit(c: u8) bool {
                return c >= '0' and c <= '9';
        }

        pub fn next(self: *lexer) !?token {
            if (self.dedent > 0) {
                self.dedent -= 1;
                return self.delineate(.dedent);
            }

            //Flush pending tokens.
            if (self.match_whitespace_token()) |whitespace| return whitespace;
            if (self.match_simple_token_greedy()) |simple| return simple;
            if (self.match_alpha_token()) |alpha| return alpha;
            if (self.match_number_token()) |number| return number;
            if (self.match_end_of_file()) |eof| return eof;
            if (self.done()) return null;

            self.read_char();
           
            return self.next(); 
        } 
};
