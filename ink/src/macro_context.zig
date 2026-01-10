const std = @import("std");
const ink = @import("root.zig");
const source = @import("source.zig");
const lexer_mod = @import("lang/lexer.zig");
const token_mod = @import("lang/token.zig");

pub const delimiter = enum(u8) { paren, bracket, block };
pub const token_tree_kind = enum(u8) { token, group };

pub const token_kind = enum(u8) {
    new_line,
    identifier,
    label,
    string,
    number,
    comma,
    colon,
    dot,
    ellipsis,
    hash,
    at_sign,
    function,
    constant,
    variable,
    expr_if,
    expr_else,
    expr_match,
    expr_select,
    case,
    detached,
    stmt_return,
    stmt_break,
    stmt_continue,
    yield,
    loop,
    @"while",
    until,
    repeat,
    @"for",
    each,
    sleep,
    timeout,
    deadline,
    spawn,
    await,
    @"try",
    atomic,
    auto,
    box,
    logical_or,
    logical_and,
    logical_xor,
    logical_not,
    logical_false,
    logical_true,
    assign,
    plus_assign,
    minus_assign,
    asterisk_assign,
    slash_assign,
    percent_assign,
    ampersand_assign,
    bar_assign,
    caret_assign,
    shift_left_assign,
    shift_right_assign,
    plus,
    minus,
    asterisk,
    slash,
    percent,
    ampersand,
    bar,
    caret,
    shift_left,
    shift_right,
    bang,
    tilde,
    less_than,
    greater_than,
    less_or_equal,
    greater_or_equal,
    equal,
    not_equal,
    @"enum",
    type,
    arrow,
    question,
    question_dot,
    coalesce,
    range,
    range_inclusive,
    double_colon,
    pipe,
    in,
    trait,
    impl,
    as,
    import,
    from,
    with,
    dynamic,
    @"comptime",
    @"struct",
    where,
    self,
    this,
    mut,
    ref,
    requires,
    dyn,
};

const span_info = struct {
    source_id: source.source_id,
    start: usize,
    end: usize,
};

const token_info = struct {
    kind: token_kind,
    span: u32,
    symbol: u32,
};

const group_info = struct {
    delimiter: delimiter,
    span: u32,
    stream: u32,
};

const tree_info = union(enum) {
    token: u32,
    group: u32,
};

const stream_info = struct {
    items: []const u32,
};

const group_builder = struct {
    delimiter: delimiter,
    start_span: u32,
    items: std.ArrayListUnmanaged(u32) = .{},
};

pub const macro_context = struct {
    allocator: std.mem.Allocator,
    spans: std.ArrayListUnmanaged(span_info) = .{},
    symbols: std.ArrayListUnmanaged([]const u8) = .{},
    symbol_map: std.StringHashMap(u32),
    tokens: std.ArrayListUnmanaged(token_info) = .{},
    groups: std.ArrayListUnmanaged(group_info) = .{},
    trees: std.ArrayListUnmanaged(tree_info) = .{},
    streams: std.ArrayListUnmanaged(stream_info) = .{},
    call_sites: std.ArrayListUnmanaged(u32) = .{},
    diags: ?*std.array_list.Managed(ink.diagnostic),
    diag_messages: ?*std.array_list.Managed([]const u8) = null,
    failed: bool = false,
    next_virtual_source: source.source_id = 1,

    pub fn init(
        allocator: std.mem.Allocator,
        diags: ?*std.array_list.Managed(ink.diagnostic),
        diag_messages: ?*std.array_list.Managed([]const u8),
    ) macro_context {
        return .{
            .allocator = allocator,
            .symbol_map = std.StringHashMap(u32).init(allocator),
            .diags = diags,
            .diag_messages = diag_messages,
        };
    }

    pub fn deinit(self: *macro_context) void {
        for (self.symbols.items) |sym| self.allocator.free(sym);
        self.symbols.deinit(self.allocator);
        self.symbol_map.deinit();
        for (self.streams.items) |stream| {
            self.allocator.free(stream.items);
        }
        self.spans.deinit(self.allocator);
        self.tokens.deinit(self.allocator);
        self.groups.deinit(self.allocator);
        self.trees.deinit(self.allocator);
        self.streams.deinit(self.allocator);
        self.call_sites.deinit(self.allocator);
    }

    pub fn reset_errors(self: *macro_context) void {
        self.failed = false;
    }

    pub fn push_call_site(self: *macro_context, span_id: u32) void {
        self.call_sites.append(self.allocator, span_id) catch {};
    }

    pub fn pop_call_site(self: *macro_context) void {
        _ = self.call_sites.pop();
    }

    pub fn span_here(self: *macro_context) u32 {
        if (self.call_sites.items.len == 0) return 0;
        return self.call_sites.items[self.call_sites.items.len - 1];
    }

    pub fn span_source(self: *macro_context, span_id: u32) ?source.source_id {
        const info = self.span_info_of(span_id) orelse return null;
        return info.source_id;
    }

    pub fn span_start(self: *macro_context, span_id: u32) ?usize {
        const info = self.span_info_of(span_id) orelse return null;
        return info.start;
    }

    pub fn span_end(self: *macro_context, span_id: u32) ?usize {
        const info = self.span_info_of(span_id) orelse return null;
        return info.end;
    }

    pub fn span_join(self: *macro_context, left: u32, right: u32) u32 {
        if (left == 0) return right;
        if (right == 0) return left;
        const left_info = self.span_info_of(left) orelse return left;
        const right_info = self.span_info_of(right) orelse return left;
        if (left_info.source_id != right_info.source_id) return left;
        const start = if (left_info.start < right_info.start) left_info.start else right_info.start;
        const end = if (left_info.end > right_info.end) left_info.end else right_info.end;
        return self.new_span(left_info.source_id, start, end);
    }

    pub fn add_error(self: *macro_context, span_id: u32, message: []const u8) void {
        self.failed = true;
        const diags = self.diags orelse return;
        var stored = message;
        if (self.diag_messages) |messages| {
            const duped = self.allocator.dupe(u8, message) catch {
                return;
            };
            messages.append(duped) catch {
                self.allocator.free(duped);
                return;
            };
            stored = duped;
        }
        var diag = ink.diagnostic{ .danger = .@"error", .message = stored };
        if (self.span_info_of(span_id)) |info| {
            diag.span = .{ .start = info.start, .end = info.end };
            diag.source_id = info.source_id;
        }
        _ = diags.append(diag) catch {};
    }

    pub fn span_from_location(self: *macro_context, source_id: source.source_id, loc: ink.location) u32 {
        return self.new_span(source_id, loc.start, loc.end);
    }

    pub fn intern_symbol(self: *macro_context, text: []const u8) u32 {
        if (self.symbol_map.get(text)) |id| return id;
        const duped = self.allocator.dupe(u8, text) catch return 0;
        const id: u32 = @intCast(self.symbols.items.len + 1);
        self.symbols.append(self.allocator, duped) catch return 0;
        self.symbol_map.put(duped, id) catch return 0;
        return id;
    }

    pub fn symbol_text(self: *macro_context, symbol_id: u32) ?[]const u8 {
        if (symbol_id == 0) return null;
        const idx = symbol_id - 1;
        if (idx >= self.symbols.items.len) return null;
        return self.symbols.items[idx];
    }

    pub fn token_stream_len(self: *macro_context, stream_id: u32) usize {
        const stream = self.stream_info_of(stream_id) orelse return 0;
        return stream.items.len;
    }

    pub fn token_stream_get(self: *macro_context, stream_id: u32, index: usize) u32 {
        const stream = self.stream_info_of(stream_id) orelse return 0;
        if (index >= stream.items.len) return 0;
        return stream.items[index];
    }

    pub fn token_stream_slice(self: *macro_context, stream_id: u32, start: usize, end: usize) u32 {
        const stream = self.stream_info_of(stream_id) orelse return 0;
        if (start >= stream.items.len) return 0;
        const safe_end = if (end > stream.items.len) stream.items.len else end;
        if (safe_end <= start) return 0;
        return self.new_stream(stream.items[start..safe_end]) catch 0;
    }

    pub fn token_stream_concat(self: *macro_context, left: u32, right: u32) u32 {
        const left_stream = self.stream_info_of(left) orelse return 0;
        const right_stream = self.stream_info_of(right) orelse return 0;
        const total = left_stream.items.len + right_stream.items.len;
        const combined = self.allocator.alloc(u32, total) catch return 0;
        std.mem.copyForwards(u32, combined[0..left_stream.items.len], left_stream.items);
        std.mem.copyForwards(u32, combined[left_stream.items.len..], right_stream.items);
        return self.new_stream_owned(combined);
    }

    pub fn token_stream_push(self: *macro_context, stream_id: u32, tree_id: u32) u32 {
        const stream = self.stream_info_of(stream_id) orelse return 0;
        const combined = self.allocator.alloc(u32, stream.items.len + 1) catch return 0;
        std.mem.copyForwards(u32, combined[0..stream.items.len], stream.items);
        combined[stream.items.len] = tree_id;
        return self.new_stream_owned(combined);
    }

    pub fn token_stream_empty(self: *macro_context) u32 {
        const items = self.allocator.alloc(u32, 0) catch return 0;
        return self.new_stream_owned(items);
    }

    pub fn token_kind_of(self: *macro_context, token_id: u32) ?token_kind {
        const info = self.token_info_of(token_id) orelse return null;
        return info.kind;
    }

    pub fn token_symbol(self: *macro_context, token_id: u32) u32 {
        const info = self.token_info_of(token_id) orelse return 0;
        return info.symbol;
    }

    pub fn token_span(self: *macro_context, token_id: u32) u32 {
        const info = self.token_info_of(token_id) orelse return 0;
        return info.span;
    }

    pub fn token_new(self: *macro_context, kind: token_kind, span_id: u32, symbol_id: u32) u32 {
        const id: u32 = @intCast(self.tokens.items.len + 1);
        self.tokens.append(self.allocator, .{ .kind = kind, .span = span_id, .symbol = symbol_id }) catch return 0;
        return id;
    }

    pub fn group_new(self: *macro_context, delim: delimiter, span_id: u32, stream_id: u32) u32 {
        const id: u32 = @intCast(self.groups.items.len + 1);
        self.groups.append(self.allocator, .{ .delimiter = delim, .span = span_id, .stream = stream_id }) catch return 0;
        return id;
    }

    pub fn tree_from_token(self: *macro_context, token_id: u32) u32 {
        const id: u32 = @intCast(self.trees.items.len + 1);
        self.trees.append(self.allocator, .{ .token = token_id }) catch return 0;
        return id;
    }

    pub fn tree_from_group(self: *macro_context, group_id: u32) u32 {
        const id: u32 = @intCast(self.trees.items.len + 1);
        self.trees.append(self.allocator, .{ .group = group_id }) catch return 0;
        return id;
    }

    pub fn tree_kind(self: *macro_context, tree_id: u32) ?token_tree_kind {
        const info = self.tree_info_of(tree_id) orelse return null;
        return switch (info) {
            .token => .token,
            .group => .group,
        };
    }

    pub fn tree_token(self: *macro_context, tree_id: u32) u32 {
        const info = self.tree_info_of(tree_id) orelse return 0;
        return switch (info) {
            .token => |tok| tok,
            else => 0,
        };
    }

    pub fn tree_group(self: *macro_context, tree_id: u32) u32 {
        const info = self.tree_info_of(tree_id) orelse return 0;
        return switch (info) {
            .group => |grp| grp,
            else => 0,
        };
    }

    pub fn tree_span(self: *macro_context, tree_id: u32) u32 {
        const info = self.tree_info_of(tree_id) orelse return 0;
        return switch (info) {
            .token => |tok| self.token_span(tok),
            .group => |grp| self.group_span(grp),
        };
    }

    pub fn group_delimiter(self: *macro_context, group_id: u32) ?delimiter {
        const info = self.group_info_of(group_id) orelse return null;
        return info.delimiter;
    }

    pub fn group_stream(self: *macro_context, group_id: u32) u32 {
        const info = self.group_info_of(group_id) orelse return 0;
        return info.stream;
    }

    pub fn group_span(self: *macro_context, group_id: u32) u32 {
        const info = self.group_info_of(group_id) orelse return 0;
        return info.span;
    }

    pub fn token_stream_from_source(
        self: *macro_context,
        tokens: []const token_mod.token,
        source_id: source.source_id,
        start: usize,
        end: usize,
    ) u32 {
        return self.build_stream_from_tokens(tokens, source_id, start, end) catch 0;
    }

    pub fn token_stream_from_inline(
        self: *macro_context,
        text: []const u8,
        source_id: source.source_id,
        base_offset: usize,
    ) u32 {
        return self.build_stream_from_inline(text, source_id, base_offset) catch 0;
    }

    pub fn token_stream_to_tokens(
        self: *macro_context,
        stream_id: u32,
        fallback: ink.location,
        allocator: std.mem.Allocator,
    ) ![]token_mod.token {
        var out = std.array_list.Managed(token_mod.token).init(allocator);
        try self.append_stream_tokens(&out, stream_id, fallback);
        const eof_loc = fallback;
        try out.append(.{
            .which = .end_of_file,
            .where = eof_loc,
            .what = .{ .string = "", .owner = .ref, .where = eof_loc },
        });
        return out.toOwnedSlice();
    }

    pub fn token_stream_from_quote(self: *macro_context, text: []const u8) u32 {
        if (self.span_info_of(self.span_here())) |call_span| {
            return self.build_stream_from_inline(text, call_span.source_id, call_span.start) catch 0;
        }
        const source_id = self.next_virtual_source;
        self.next_virtual_source += 1;
        return self.build_stream_from_inline(text, source_id, 0) catch 0;
    }

    fn span_info_of(self: *macro_context, span_id: u32) ?span_info {
        if (span_id == 0) return null;
        const idx = span_id - 1;
        if (idx >= self.spans.items.len) return null;
        return self.spans.items[idx];
    }

    fn token_info_of(self: *macro_context, token_id: u32) ?token_info {
        if (token_id == 0) return null;
        const idx = token_id - 1;
        if (idx >= self.tokens.items.len) return null;
        return self.tokens.items[idx];
    }

    fn group_info_of(self: *macro_context, group_id: u32) ?group_info {
        if (group_id == 0) return null;
        const idx = group_id - 1;
        if (idx >= self.groups.items.len) return null;
        return self.groups.items[idx];
    }

    fn tree_info_of(self: *macro_context, tree_id: u32) ?tree_info {
        if (tree_id == 0) return null;
        const idx = tree_id - 1;
        if (idx >= self.trees.items.len) return null;
        return self.trees.items[idx];
    }

    fn stream_info_of(self: *macro_context, stream_id: u32) ?stream_info {
        if (stream_id == 0) return null;
        const idx = stream_id - 1;
        if (idx >= self.streams.items.len) return null;
        return self.streams.items[idx];
    }

    fn new_span(self: *macro_context, source_id: source.source_id, start: usize, end: usize) u32 {
        const id: u32 = @intCast(self.spans.items.len + 1);
        self.spans.append(self.allocator, .{ .source_id = source_id, .start = start, .end = end }) catch return 0;
        return id;
    }

    fn new_stream(self: *macro_context, items: []const u32) !u32 {
        const owned = try self.allocator.dupe(u32, items);
        const id: u32 = @intCast(self.streams.items.len + 1);
        self.streams.append(self.allocator, .{ .items = owned }) catch return 0;
        return id;
    }

    fn new_stream_owned(self: *macro_context, items: []const u32) u32 {
        const id: u32 = @intCast(self.streams.items.len + 1);
        self.streams.append(self.allocator, .{ .items = items }) catch return 0;
        return id;
    }

    fn build_stream_from_inline(
        self: *macro_context,
        text: []const u8,
        source_id: source.source_id,
        base_offset: usize,
    ) !u32 {
        var lex = try lexer_mod.lexer.init(text);
        var tokens = std.array_list.Managed(token_mod.token).init(self.allocator);
        defer tokens.deinit();
        while (true) {
            const maybe_tok = lex.next() catch break;
            if (maybe_tok) |tok| {
                if (tok.which == .end_of_file) break;
                var shifted = tok;
                shifted.where = .{
                    .start = tok.where.start + base_offset,
                    .end = tok.where.end + base_offset,
                };
                shifted.what.where = shifted.where;
                try tokens.append(shifted);
            } else break;
        }
        return self.build_stream_from_tokens(tokens.items, source_id, base_offset, base_offset + text.len);
    }

    fn build_stream_from_tokens(
        self: *macro_context,
        tokens: []const token_mod.token,
        source_id: source.source_id,
        start: usize,
        end: usize,
    ) !u32 {
        var stack = std.array_list.Managed(group_builder).init(self.allocator);
        defer {
            for (stack.items) |*frame| {
                frame.items.deinit(self.allocator);
            }
            stack.deinit();
        }

        var root_items = std.ArrayListUnmanaged(u32){};
        defer root_items.deinit(self.allocator);

        for (tokens) |tok| {
            if (tok.where.start < start or tok.where.end > end) continue;
            switch (tok.which) {
                .indent => {
                    const span_id = self.new_span(source_id, tok.where.start, tok.where.end);
                    try stack.append(.{ .delimiter = .block, .start_span = span_id });
                },
                .dedent => {
                    const span_id = self.new_span(source_id, tok.where.start, tok.where.end);
                    if (stack.items.len == 0 or stack.items[stack.items.len - 1].delimiter != .block) {
                        self.add_error(span_id, "macro token stream dedent without indent");
                        continue;
                    }
                    var frame = stack.pop().?;
                    defer frame.items.deinit(self.allocator);
                    const stream_id = try self.new_stream(frame.items.items);
                    const joined = self.span_join(frame.start_span, span_id);
                    const group_id = self.group_new(frame.delimiter, joined, stream_id);
                    const tree_id = self.tree_from_group(group_id);
                    try append_tree_to_parent(&root_items, &stack, self.allocator, tree_id);
                },
                .paren_left => {
                    const span_id = self.new_span(source_id, tok.where.start, tok.where.end);
                    try stack.append(.{ .delimiter = .paren, .start_span = span_id });
                },
                .paren_right => {
                    const span_id = self.new_span(source_id, tok.where.start, tok.where.end);
                    if (stack.items.len == 0 or stack.items[stack.items.len - 1].delimiter != .paren) {
                        self.add_error(span_id, "macro token stream mismatched ')'");
                        continue;
                    }
                    var frame = stack.pop().?;
                    defer frame.items.deinit(self.allocator);
                    const stream_id = try self.new_stream(frame.items.items);
                    const joined = self.span_join(frame.start_span, span_id);
                    const group_id = self.group_new(frame.delimiter, joined, stream_id);
                    const tree_id = self.tree_from_group(group_id);
                    try append_tree_to_parent(&root_items, &stack, self.allocator, tree_id);
                },
                .bracket_left => {
                    const span_id = self.new_span(source_id, tok.where.start, tok.where.end);
                    try stack.append(.{ .delimiter = .bracket, .start_span = span_id });
                },
                .bracket_right => {
                    const span_id = self.new_span(source_id, tok.where.start, tok.where.end);
                    if (stack.items.len == 0 or stack.items[stack.items.len - 1].delimiter != .bracket) {
                        self.add_error(span_id, "macro token stream mismatched ']'");
                        continue;
                    }
                    var frame = stack.pop().?;
                    defer frame.items.deinit(self.allocator);
                    const stream_id = try self.new_stream(frame.items.items);
                    const joined = self.span_join(frame.start_span, span_id);
                    const group_id = self.group_new(frame.delimiter, joined, stream_id);
                    const tree_id = self.tree_from_group(group_id);
                    try append_tree_to_parent(&root_items, &stack, self.allocator, tree_id);
                },
                .end_of_file, .illegal => {},
                else => {
                    const kind = token_kind_from_ink(tok.which) orelse continue;
                    const span_id = self.new_span(source_id, tok.where.start, tok.where.end);
                    const symbol_id = symbol_from_token(self, tok);
                    const tok_id = self.token_new(kind, span_id, symbol_id);
                    const tree_id = self.tree_from_token(tok_id);
                    try append_tree_to_parent(&root_items, &stack, self.allocator, tree_id);
                },
            }
        }

        if (stack.items.len != 0) {
            const top = stack.items[stack.items.len - 1];
            self.add_error(top.start_span, "macro token stream unterminated group");
        }

        return self.new_stream(root_items.items);
    }

    fn append_tree_to_parent(
        root_items: *std.ArrayListUnmanaged(u32),
        stack: *std.array_list.Managed(group_builder),
        allocator: std.mem.Allocator,
        tree_id: u32,
    ) !void {
        if (stack.items.len == 0) {
            try root_items.append(allocator, tree_id);
        } else {
            try stack.items[stack.items.len - 1].items.append(allocator, tree_id);
        }
    }

    fn symbol_from_token(self: *macro_context, tok: token_mod.token) u32 {
        return switch (tok.which) {
            .identifier, .label, .string, .number => self.intern_symbol(tok.what.string),
            else => 0,
        };
    }

    fn token_kind_from_ink(kind: token_mod.token.kind) ?token_kind {
        return switch (kind) {
            .new_line => .new_line,
            .identifier => .identifier,
            .label => .label,
            .string => .string,
            .number => .number,
            .comma => .comma,
            .colon => .colon,
            .dot => .dot,
            .ellipsis => .ellipsis,
            .hash => .hash,
            .at_sign => .at_sign,
            .function => .function,
            .constant => .constant,
            .variable => .variable,
            .expr_if => .expr_if,
            .expr_else => .expr_else,
            .expr_match => .expr_match,
            .expr_select => .expr_select,
            .case => .case,
            .detached => .detached,
            .stmt_return => .stmt_return,
            .stmt_break => .stmt_break,
            .stmt_continue => .stmt_continue,
            .yield => .yield,
            .loop => .loop,
            .@"while" => .@"while",
            .until => .until,
            .repeat => .repeat,
            .@"for" => .@"for",
            .each => .each,
            .sleep => .sleep,
            .timeout => .timeout,
            .deadline => .deadline,
            .spawn => .spawn,
            .await => .await,
            .@"try" => .@"try",
            .atomic => .atomic,
            .auto => .auto,
            .box => .box,
            .logical_or => .logical_or,
            .logical_and => .logical_and,
            .logical_xor => .logical_xor,
            .logical_not => .logical_not,
            .logical_false => .logical_false,
            .logical_true => .logical_true,
            .assign => .assign,
            .plus_assign => .plus_assign,
            .minus_assign => .minus_assign,
            .asterisk_assign => .asterisk_assign,
            .slash_assign => .slash_assign,
            .percent_assign => .percent_assign,
            .ampersand_assign => .ampersand_assign,
            .bar_assign => .bar_assign,
            .caret_assign => .caret_assign,
            .shift_left_assign => .shift_left_assign,
            .shift_right_assign => .shift_right_assign,
            .plus => .plus,
            .minus => .minus,
            .asterisk => .asterisk,
            .slash => .slash,
            .percent => .percent,
            .ampersand => .ampersand,
            .bar => .bar,
            .caret => .caret,
            .shift_left => .shift_left,
            .shift_right => .shift_right,
            .bang => .bang,
            .tilde => .tilde,
            .less_than => .less_than,
            .greater_than => .greater_than,
            .less_or_equal => .less_or_equal,
            .greater_or_equal => .greater_or_equal,
            .equal => .equal,
            .not_equal => .not_equal,
            .@"enum" => .@"enum",
            .type => .type,
            .arrow => .arrow,
            .question => .question,
            .question_dot => .question_dot,
            .coalesce => .coalesce,
            .range => .range,
            .range_inclusive => .range_inclusive,
            .double_colon => .double_colon,
            .pipe => .pipe,
            .in => .in,
            .trait => .trait,
            .impl => .impl,
            .as => .as,
            .import => .import,
            .from => .from,
            .with => .with,
            .dynamic => .dynamic,
            .@"comptime" => .@"comptime",
            .@"struct" => .@"struct",
            .where => .where,
            .self => .self,
            .this => .this,
            .mut => .mut,
            .ref => .ref,
            .requires => .requires,
            .dyn => .dyn,
            .indent, .dedent, .paren_left, .paren_right, .bracket_left, .bracket_right, .end_of_file, .illegal => null,
        };
    }

    fn ink_kind_from_macro(kind: token_kind) token_mod.token.kind {
        return switch (kind) {
            .new_line => .new_line,
            .identifier => .identifier,
            .label => .label,
            .string => .string,
            .number => .number,
            .comma => .comma,
            .colon => .colon,
            .dot => .dot,
            .ellipsis => .ellipsis,
            .hash => .hash,
            .at_sign => .at_sign,
            .function => .function,
            .constant => .constant,
            .variable => .variable,
            .expr_if => .expr_if,
            .expr_else => .expr_else,
            .expr_match => .expr_match,
            .expr_select => .expr_select,
            .case => .case,
            .detached => .detached,
            .stmt_return => .stmt_return,
            .stmt_break => .stmt_break,
            .stmt_continue => .stmt_continue,
            .yield => .yield,
            .loop => .loop,
            .@"while" => .@"while",
            .until => .until,
            .repeat => .repeat,
            .@"for" => .@"for",
            .each => .each,
            .sleep => .sleep,
            .timeout => .timeout,
            .deadline => .deadline,
            .spawn => .spawn,
            .await => .await,
            .@"try" => .@"try",
            .atomic => .atomic,
            .auto => .auto,
            .box => .box,
            .logical_or => .logical_or,
            .logical_and => .logical_and,
            .logical_xor => .logical_xor,
            .logical_not => .logical_not,
            .logical_false => .logical_false,
            .logical_true => .logical_true,
            .assign => .assign,
            .plus_assign => .plus_assign,
            .minus_assign => .minus_assign,
            .asterisk_assign => .asterisk_assign,
            .slash_assign => .slash_assign,
            .percent_assign => .percent_assign,
            .ampersand_assign => .ampersand_assign,
            .bar_assign => .bar_assign,
            .caret_assign => .caret_assign,
            .shift_left_assign => .shift_left_assign,
            .shift_right_assign => .shift_right_assign,
            .plus => .plus,
            .minus => .minus,
            .asterisk => .asterisk,
            .slash => .slash,
            .percent => .percent,
            .ampersand => .ampersand,
            .bar => .bar,
            .caret => .caret,
            .shift_left => .shift_left,
            .shift_right => .shift_right,
            .bang => .bang,
            .tilde => .tilde,
            .less_than => .less_than,
            .greater_than => .greater_than,
            .less_or_equal => .less_or_equal,
            .greater_or_equal => .greater_or_equal,
            .equal => .equal,
            .not_equal => .not_equal,
            .@"enum" => .@"enum",
            .type => .type,
            .arrow => .arrow,
            .question => .question,
            .question_dot => .question_dot,
            .coalesce => .coalesce,
            .range => .range,
            .range_inclusive => .range_inclusive,
            .double_colon => .double_colon,
            .pipe => .pipe,
            .in => .in,
            .trait => .trait,
            .impl => .impl,
            .as => .as,
            .import => .import,
            .from => .from,
            .with => .with,
            .dynamic => .dynamic,
            .@"comptime" => .@"comptime",
            .@"struct" => .@"struct",
            .where => .where,
            .self => .self,
            .this => .this,
            .mut => .mut,
            .ref => .ref,
            .requires => .requires,
            .dyn => .dyn,
        };
    }

    fn append_stream_tokens(
        self: *macro_context,
        out: *std.array_list.Managed(token_mod.token),
        stream_id: u32,
        fallback: ink.location,
    ) error{OutOfMemory}!void {
        const stream = self.stream_info_of(stream_id) orelse return;
        for (stream.items) |tree_id| {
            try self.append_tree_tokens(out, tree_id, fallback);
        }
    }

    fn append_tree_tokens(
        self: *macro_context,
        out: *std.array_list.Managed(token_mod.token),
        tree_id: u32,
        fallback: ink.location,
    ) error{OutOfMemory}!void {
        const info = self.tree_info_of(tree_id) orelse return;
        switch (info) {
            .token => |tok_id| {
                const tok_info = self.token_info_of(tok_id) orelse return;
                const kind = ink_kind_from_macro(tok_info.kind);
                const span = self.span_info_of(tok_info.span) orelse span_info{ .source_id = 0, .start = fallback.start, .end = fallback.end };
                const loc = ink.location{ .start = span.start, .end = span.end };
                const symbol_text_value = self.symbol_text(tok_info.symbol) orelse "";
                const ident = ink.identifier{ .string = symbol_text_value, .owner = .ref, .where = loc };
                try out.append(.{ .which = kind, .where = loc, .what = ident });
            },
            .group => |grp_id| try self.append_group_tokens(out, grp_id, fallback),
        }
    }

    fn append_group_tokens(
        self: *macro_context,
        out: *std.array_list.Managed(token_mod.token),
        group_id: u32,
        fallback: ink.location,
    ) error{OutOfMemory}!void {
        const grp = self.group_info_of(group_id) orelse return;
        const span = self.span_info_of(grp.span) orelse span_info{ .source_id = 0, .start = fallback.start, .end = fallback.end };
        const start_loc = ink.location{ .start = span.start, .end = span.start };
        const end_loc = ink.location{ .start = span.end, .end = span.end };
        switch (grp.delimiter) {
            .paren => {
                try out.append(.{ .which = .paren_left, .where = start_loc, .what = .{ .string = "", .owner = .ref, .where = start_loc } });
                try self.append_stream_tokens(out, grp.stream, fallback);
                try out.append(.{ .which = .paren_right, .where = end_loc, .what = .{ .string = "", .owner = .ref, .where = end_loc } });
            },
            .bracket => {
                try out.append(.{ .which = .bracket_left, .where = start_loc, .what = .{ .string = "", .owner = .ref, .where = start_loc } });
                try self.append_stream_tokens(out, grp.stream, fallback);
                try out.append(.{ .which = .bracket_right, .where = end_loc, .what = .{ .string = "", .owner = .ref, .where = end_loc } });
            },
            .block => {
                try out.append(.{ .which = .indent, .where = start_loc, .what = .{ .string = "", .owner = .ref, .where = start_loc } });
                try self.append_stream_tokens(out, grp.stream, fallback);
                try out.append(.{ .which = .dedent, .where = end_loc, .what = .{ .string = "", .owner = .ref, .where = end_loc } });
            },
        }
    }

};
