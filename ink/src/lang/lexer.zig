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
    line_start: bool,

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
            .cursor = 0,
            .dedent = 0,
            .line_start = true,
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
            .where = where,
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

        self.line_start = true;
        if (self.current == spec.whitespace.newline or self.current == spec.eof_char) {
            return self.delineate_from(.new_line, start);
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

    fn match_leading_spaces(self: *lexer) ?token {
        if (!self.line_start or self.current != spec.whitespace.space) return null;
        const start = self.head;
        while (self.current == spec.whitespace.space) {
            self.read_char();
        }
        return self.delineate_from(.illegal, start);
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
        const kind = keywords.get(text) orelse .identifier;
        return self.delineate_from(kind, start);
    }

    fn match_label_token(self: *lexer) ?token {
        if (self.current != '\'') return null;
        const start = self.head;
        const next_char = self.peek_char();
        if (!is_identifier_start(next_char)) {
            self.read_char();
            return self.delineate_from(.illegal, start);
        }
        self.read_char();
        while (is_identifier_continue(self.current)) {
            self.read_char();
        }
        return self.delineate_from(.label, start);
    }

    fn match_char_token(self: *lexer) ?token {
        if (self.current != '\'') return null;
        const start_quote = self.head;
        const start = self.head + 1;
        if (self.tail >= self.source.len) {
            self.read_char();
            return self.delineate_from(.illegal, start_quote);
        }

        const first = self.source[self.tail];
        if (first == spec.whitespace.newline) {
            self.read_char();
            return self.delineate_from(.illegal, start_quote);
        }

        var end_idx: usize = 0;
        if (first == '\\') {
            const esc_idx = self.tail + 1;
            if (esc_idx >= self.source.len) {
                self.read_char();
                return self.delineate_from(.illegal, start_quote);
            }
            const esc = self.source[esc_idx];
            switch (esc) {
                'x' => {
                    if (esc_idx + 2 >= self.source.len) {
                        self.read_char();
                        return self.delineate_from(.illegal, start_quote);
                    }
                    if (!is_hex_digit(self.source[esc_idx + 1]) or !is_hex_digit(self.source[esc_idx + 2])) {
                        self.read_char();
                        return self.delineate_from(.illegal, start_quote);
                    }
                    end_idx = esc_idx + 3;
                },
                'u' => {
                    if (esc_idx + 1 >= self.source.len) {
                        self.read_char();
                        return self.delineate_from(.illegal, start_quote);
                    }
                    if (self.source[esc_idx + 1] == '{') {
                        var i: usize = esc_idx + 2;
                        var digits: usize = 0;
                        while (i < self.source.len and self.source[i] != '}') : (i += 1) {
                            if (!is_hex_digit(self.source[i])) {
                                self.read_char();
                                return self.delineate_from(.illegal, start_quote);
                            }
                            digits += 1;
                        }
                        if (i >= self.source.len or digits == 0) {
                            self.read_char();
                            return self.delineate_from(.illegal, start_quote);
                        }
                        end_idx = i + 1;
                    } else {
                        if (esc_idx + 4 >= self.source.len) {
                            self.read_char();
                            return self.delineate_from(.illegal, start_quote);
                        }
                        if (!is_hex_digit(self.source[esc_idx + 1]) or !is_hex_digit(self.source[esc_idx + 2]) or
                            !is_hex_digit(self.source[esc_idx + 3]) or !is_hex_digit(self.source[esc_idx + 4]))
                        {
                            self.read_char();
                            return self.delineate_from(.illegal, start_quote);
                        }
                        end_idx = esc_idx + 5;
                    }
                },
                'n', 'r', 't', '0', '\\', '\'', '"' => end_idx = esc_idx + 1,
                else => {
                    self.read_char();
                    return self.delineate_from(.illegal, start_quote);
                },
            }
            if (end_idx >= self.source.len or self.source[end_idx] != '\'') {
                self.read_char();
                return self.delineate_from(.illegal, start_quote);
            }
        } else {
            end_idx = self.tail + 1;
            if (end_idx >= self.source.len or self.source[end_idx] != '\'') {
                if (is_identifier_start(first)) return null;
                self.read_char();
                return self.delineate_from(.illegal, start_quote);
            }
        }

        while (self.head < end_idx) {
            self.read_char();
        }
        self.read_char();
        const where = location{ .start = start, .end = end_idx };
        return token{ .which = .character, .where = where, .what = self.delineate_source_slice(where) };
    }

    fn match_string_token(self: *lexer) ?token {
        if (self.current != '"') return null;
        const start_quote = self.head;
        const start = self.head + 1;
        var escaped = false;
        while (true) {
            self.read_char();
            if (self.current == spec.eof_char or self.current == spec.whitespace.newline) {
                return self.delineate_from(.illegal, start_quote);
            }
            if (escaped) {
                escaped = false;
                continue;
            }
            if (self.current == '\\') {
                escaped = true;
                continue;
            }
            if (self.current == '"') break;
        }
        const end = self.head;
        self.read_char();
        const where = location{ .start = start, .end = end };
        return token{ .which = .string, .where = where, .what = self.delineate_source_slice(where) };
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
            if (self.cursor > 1) {
                var pops: u8 = 0;
                while (self.cursor > 1) {
                    self.pop();
                    pops += 1;
                }
                if (pops > 1) {
                    self.dedent = pops - 1;
                }
                return self.delineate(.dedent);
            }
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

    fn is_hex_digit(c: u8) bool {
        if (c >= '0' and c <= '9') return true;
        if (c >= 'a' and c <= 'f') return true;
        if (c >= 'A' and c <= 'F') return true;
        return false;
    }

    pub fn next(self: *lexer) !?token {
        if (self.dedent > 0) {
            self.dedent -= 1;
            return self.delineate(.dedent);
        }

        if (self.match_leading_spaces()) |illegal| return illegal;

        //Flush pending tokens.
        if (self.match_whitespace_token()) |whitespace| return whitespace;
        if (self.match_simple_token_greedy()) |simple| {
            self.line_start = false;
            return simple;
        }
        if (self.match_string_token()) |string| {
            self.line_start = false;
            return string;
        }
        if (self.match_char_token()) |char_tok| {
            self.line_start = false;
            return char_tok;
        }
        if (self.match_label_token()) |label| {
            self.line_start = false;
            return label;
        }
        if (self.match_alpha_token()) |alpha| {
            self.line_start = false;
            return alpha;
        }
        if (self.match_number_token()) |number| {
            self.line_start = false;
            return number;
        }
        if (self.match_end_of_file()) |eof| return eof;
        if (self.done()) return null;

        self.read_char();

        return self.next();
    }
};
