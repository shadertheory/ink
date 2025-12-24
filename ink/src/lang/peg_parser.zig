const std = @import("std");
const peg = @import("peg.zig");
const token = @import("token.zig").token;

pub const mem_allocator = std.mem.Allocator;

const expr = peg.expr;
const expr_id = peg.expr_id;
const grammar = peg.grammar;
const nonterminal_kind = peg.nonterminal_kind;

pub const node_id = usize;

pub const parse_tree = struct {
    nodes: std.ArrayListUnmanaged(node) = .{},

    pub const node = struct {
        symbol: peg.symbol,
        start: usize,
        end: usize,
        children: []const node_id,
    };

    pub fn init() parse_tree {
        return .{};
    }

    pub fn deinit(self: *parse_tree, allocator: mem_allocator) void {
        self.nodes.deinit(allocator);
    }

    pub fn add(
        self: *parse_tree,
        allocator: mem_allocator,
        symbol: peg.symbol,
        start: usize,
        end: usize,
        children: []const node_id,
    ) mem_allocator.Error!node_id {
        const id = self.nodes.items.len;
        try self.nodes.append(allocator, .{
            .symbol = symbol,
            .start = start,
            .end = end,
            .children = children,
        });
        return id;
    }
};

pub const parse_error_info = struct {
    position: usize,
    expected: []const token.kind,
    found: ?token.kind,
};

pub const parse_result = struct {
    arena: std.heap.ArenaAllocator,
    tree: parse_tree,
    root: ?node_id,
    ok: bool,
    @"error": ?parse_error_info,

    pub fn deinit(self: *parse_result) void {
        self.tree.deinit(self.arena.allocator());
        self.arena.deinit();
    }
};

const parse_error = mem_allocator.Error;

const memo_key = struct {
    nonterminal: nonterminal_kind,
    position: usize,
};

const memo_entry = struct {
    in_progress: bool,
    ok: bool,
    next: usize,
    node: ?node_id,
};

const outcome = struct {
    ok: bool,
    next: usize,
    node: ?node_id,
};

const nodes_outcome = struct {
    ok: bool,
    next: usize,
    nodes: []const node_id,
};

const parser = struct {
    allocator: mem_allocator,
    grammar: grammar,
    tokens: []const token,
    tree: *parse_tree,
    memo: std.AutoHashMap(memo_key, memo_entry),
    furthest_pos: usize,
    expected: std.AutoHashMap(token.kind, void),

    fn record_expected(self: *parser, pos: usize, kind: token.kind) !void {
        if (pos > self.furthest_pos) {
            self.furthest_pos = pos;
            self.expected.clearRetainingCapacity();
        }
        if (pos == self.furthest_pos) {
            _ = try self.expected.put(kind, {});
        }
    }

    fn build_error_info(self: *parser, allocator: mem_allocator) !parse_error_info {
        var expected_list = std.array_list.Managed(token.kind).init(allocator);
        var it = self.expected.iterator();
        while (it.next()) |entry| {
            try expected_list.append(entry.key_ptr.*);
        }
        const found = if (self.furthest_pos < self.tokens.len) self.tokens[self.furthest_pos].which else null;
        return .{
            .position = self.furthest_pos,
            .expected = try expected_list.toOwnedSlice(),
            .found = found,
        };
    }

    fn parse_nonterminal(self: *parser, nt: nonterminal_kind, pos: usize, record_errors: bool) parse_error!outcome {
        const key = memo_key{ .nonterminal = nt, .position = pos };
        if (self.memo.get(key)) |entry| {
            if (entry.in_progress) {
                return .{ .ok = false, .next = pos, .node = null };
            }
            return .{ .ok = entry.ok, .next = entry.next, .node = entry.node };
        }

        try self.memo.put(key, .{ .in_progress = true, .ok = false, .next = pos, .node = null });
        const rule_idx = self.grammar.rule_index[@intFromEnum(nt)];
        const rule_expr_id = self.grammar.rules[rule_idx].expr;
        const parse_outcome = try self.parse_expr(rule_expr_id, pos, record_errors);
        var node_idx: ?node_id = null;
        if (parse_outcome.ok) {
            node_idx = try self.tree.add(self.allocator, peg.symbol.nonterminal(nt), pos, parse_outcome.next, parse_outcome.nodes);
        }
        _ = try self.memo.put(key, .{
            .in_progress = false,
            .ok = parse_outcome.ok,
            .next = parse_outcome.next,
            .node = node_idx,
        });
        return .{ .ok = parse_outcome.ok, .next = parse_outcome.next, .node = node_idx };
    }

    fn parse_expr(self: *parser, expr_idx: expr_id, pos: usize, record_errors: bool) parse_error!nodes_outcome {
        const empty_nodes = &[_]node_id{};
        const expr_value = self.grammar.exprs[expr_idx];
        switch (expr_value) {
            .terminal => |kind| {
                if (pos >= self.tokens.len) {
                    if (record_errors) try self.record_expected(pos, kind);
                    return .{ .ok = false, .next = pos, .nodes = empty_nodes };
                }
                if (self.tokens[pos].which != kind) {
                    if (record_errors) try self.record_expected(pos, kind);
                    return .{ .ok = false, .next = pos, .nodes = empty_nodes };
                }
                const node_idx = try self.tree.add(self.allocator, peg.symbol.terminal(kind), pos, pos + 1, empty_nodes);
                const nodes = try self.allocator.alloc(node_id, 1);
                nodes[0] = node_idx;
                return .{ .ok = true, .next = pos + 1, .nodes = nodes };
            },
            .nonterminal => |nt| {
                const res = try self.parse_nonterminal(nt, pos, record_errors);
                if (!res.ok) {
                    return .{ .ok = false, .next = pos, .nodes = empty_nodes };
                }
                const nodes = try self.allocator.alloc(node_id, 1);
                nodes[0] = res.node.?;
                return .{ .ok = true, .next = res.next, .nodes = nodes };
            },
            .sequence => |items| {
                var cur = pos;
                var nodes = std.ArrayListUnmanaged(node_id){};
                for (items) |item| {
                    const res = try self.parse_expr(item, cur, record_errors);
                    if (!res.ok) return .{ .ok = false, .next = pos, .nodes = empty_nodes };
                    cur = res.next;
                    if (res.nodes.len != 0) {
                        try nodes.appendSlice(self.allocator, res.nodes);
                    }
                }
                return .{ .ok = true, .next = cur, .nodes = try nodes.toOwnedSlice(self.allocator) };
            },
            .choice => |items| {
                for (items) |item| {
                    const res = try self.parse_expr(item, pos, record_errors);
                    if (res.ok) return res;
                }
                return .{ .ok = false, .next = pos, .nodes = empty_nodes };
            },
            .zero_or_more => |item| {
                var cur = pos;
                var nodes = std.ArrayListUnmanaged(node_id){};
                while (true) {
                    const res = try self.parse_expr(item, cur, false);
                    if (!res.ok) break;
                    if (res.next == cur) break;
                    cur = res.next;
                    if (res.nodes.len != 0) {
                        try nodes.appendSlice(self.allocator, res.nodes);
                    }
                }
                return .{ .ok = true, .next = cur, .nodes = try nodes.toOwnedSlice(self.allocator) };
            },
            .one_or_more => |item| {
                var cur = pos;
                var nodes = std.ArrayListUnmanaged(node_id){};
                const first = try self.parse_expr(item, cur, record_errors);
                if (!first.ok) return .{ .ok = false, .next = pos, .nodes = empty_nodes };
                cur = first.next;
                if (first.nodes.len != 0) {
                    try nodes.appendSlice(self.allocator, first.nodes);
                }
                while (true) {
                    const res = try self.parse_expr(item, cur, false);
                    if (!res.ok) break;
                    if (res.next == cur) break;
                    cur = res.next;
                    if (res.nodes.len != 0) {
                        try nodes.appendSlice(self.allocator, res.nodes);
                    }
                }
                return .{ .ok = true, .next = cur, .nodes = try nodes.toOwnedSlice(self.allocator) };
            },
            .optional => |item| {
                const res = try self.parse_expr(item, pos, false);
                if (!res.ok) return .{ .ok = true, .next = pos, .nodes = empty_nodes };
                return res;
            },
            .and_pred => |item| {
                const res = try self.parse_expr(item, pos, false);
                return .{ .ok = res.ok, .next = pos, .nodes = empty_nodes };
            },
            .not_pred => |item| {
                const res = try self.parse_expr(item, pos, false);
                return .{ .ok = !res.ok, .next = pos, .nodes = empty_nodes };
            },
        }
    }
};

pub fn parse(allocator: mem_allocator, tokens: []const token) !parse_result {
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();
    const arena_alloc = arena.allocator();

    var tree = parse_tree.init();

    var p = parser{
        .allocator = arena_alloc,
        .grammar = try peg.build(arena_alloc),
        .tokens = tokens,
        .tree = &tree,
        .memo = std.AutoHashMap(memo_key, memo_entry).init(arena_alloc),
        .furthest_pos = 0,
        .expected = std.AutoHashMap(token.kind, void).init(arena_alloc),
    };

    const parse_outcome = try p.parse_nonterminal(p.grammar.start, 0, true);
    const ok = parse_outcome.ok and parse_outcome.next == tokens.len;
    var error_info: ?parse_error_info = null;
    if (!ok) {
        error_info = try p.build_error_info(arena_alloc);
    }

    return .{
        .arena = arena,
        .tree = tree,
        .root = if (ok) parse_outcome.node else null,
        .ok = ok,
        .@"error" = error_info,
    };
}
