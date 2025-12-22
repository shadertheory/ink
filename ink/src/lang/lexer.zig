const location = @import("ink").location;
const identifier = @import("ink").identifier;
const std = @import("std");
const spec = @import("spec.zig");
const token = @import("token.zig").token;
const token_map = std.StaticStringMap(token.kind);

const keyword_entries = blk: {
    var entries: [spec.keyword_lexemes.len]struct { []const u8, token.kind } = undefined;
    for (spec.keyword_lexemes, 0..) |lex, i| {
        entries[i] = .{ lex.text, @field(token.kind, lex.kind) };
    }
    break :blk entries;
};
const keywords = token_map.initComptime(&keyword_entries);

const symbol_entries = blk: {
    var entries: [spec.symbol_lexemes.len]struct { text: []const u8, kind: token.kind } = undefined;
    for (spec.symbol_lexemes, 0..) |lex, i| {
        entries[i] = .{ .text = lex.text, .kind = @field(token.kind, lex.kind) };
    }
    break :blk entries;
};

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

        var instance = lexer{
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
            self.current = spec.eof_char;
        } else {
            self.current = self.source[self.tail];
        }
        self.head = self.tail;
        self.tail += 1;
    }

    fn peek_char(self: *lexer) u8 {
        if (self.tail >= self.source.len) return spec.eof_char;
        return self.source[self.tail];
    }

    fn delineate_source_slice(self: *lexer, where: location) identifier {
        return identifier{
            .string = self.source[where.start..where.end],
            .owner = .ref,
        };
    }
    fn delineate(self: *lexer, which: token.kind) token {
        return self.delineate_from(which, self.head);
    }

    fn delineate_from(self: *lexer, which: token.kind, start: usize) token {
        const where = location{ .start = start, .end = self.head };
        const what = self.delineate_source_slice(where);
        return token{ .which = which, .where = where, .what = what };
    }

    fn match_newline(self: *lexer) ?token {
        const start = self.head;
        self.read_char();

        var tab_count: u8 = 0;
        while (self.current == spec.whitespace.tab) {
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
        if (self.current == spec.whitespace.space) {
            return null;
        }

        if (self.current == spec.whitespace.newline) {
            return self.match_newline();
        }

        if (self.current == spec.whitespace.tab) {
            self.read_char();
            return null;
        }

        return null;
    }

    fn match_simple_token_greedy(self: *lexer) ?token {
        const start = self.head;
        inline for (symbol_entries) |entry| {
            if (std.mem.startsWith(u8, self.source[self.head..], entry.text)) {
                var i: usize = 0;
                while (i < entry.text.len) : (i += 1) {
                    self.read_char();
                }
                return self.delineate_from(entry.kind, start);
            }
        }
        return null;
    }

    fn match_alpha_token(self: *lexer) ?token {
        if (!is_identifier_start(self.current)) return null;
        const start = self.head;
        while (is_identifier_continue(self.current)) {
            self.read_char();
        }
        const text = self.source[start..self.head];
        std.debug.print("\nfound text {s}\n", .{text});
        const kind = keywords.get(text) orelse .identifier;
        return self.delineate_from(kind, start);
    }

    fn match_number_token(self: *lexer) ?token {
        const start = self.head;
        if (!is_digit(self.current)) return null;
        while (is_digit(self.current)) {
            self.read_char();
        }

        if (self.current == spec.number.decimal_separator and is_digit(self.peek_char())) {
            self.read_char();
            while (is_digit(self.current)) {
                self.read_char();
            }
        }

        if (std.mem.indexOfScalar(u8, spec.number.exponent_markers, self.current) != null) {
            const potential = self.peek_char();
            if (is_digit(potential)) {
                self.read_char();
                while (is_digit(self.current)) {
                    self.read_char();
                }
            } else if (std.mem.indexOfScalar(u8, spec.number.exponent_signs, potential) != null) {
                const after_sign = if (self.tail >= self.source.len) spec.eof_char else self.source[self.tail];
                if (is_digit(after_sign)) {
                    self.read_char(); // exponent marker
                    self.read_char(); // sign
                    while (is_digit(self.current)) {
                        self.read_char();
                    }
                }
            }
        }
        return self.delineate_from(.number, start);
    }

    fn match_end_of_file(self: *lexer) ?token {
        if (self.current == spec.eof_char) {
            return self.delineate(.end_of_file);
        }
        return null;
    }

    fn done(self: *lexer) bool {
        return self.head >= self.source_len;
    }

    fn is_digit(c: u8) bool {
        return std.mem.indexOfScalar(u8, spec.digit_chars, c) != null;
    }

    fn is_identifier_start(c: u8) bool {
        return std.mem.indexOfScalar(u8, spec.identifier_start, c) != null;
    }

    fn is_identifier_continue(c: u8) bool {
        return std.mem.indexOfScalar(u8, spec.identifier_continue, c) != null;
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
