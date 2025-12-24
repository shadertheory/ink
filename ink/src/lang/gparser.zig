const std = @import("std");
const grammar_spec = @import("grammar.zig");
const token = @import("token.zig").token;

pub const mem_allocator = std.mem.Allocator;

const Grammar = grammar_spec.Grammar;
const Nonterminal = grammar_spec.Nonterminal;
const Symbol = grammar_spec.Symbol;

pub const parse_error = error{
    out_of_memory,
    parse_failed,
};

const State = struct {
    prod_index: usize,
    dot: usize,
    start: usize,
};

const StateSet = struct {
    states: std.ArrayList(State),
    index: std.AutoHashMap(State, usize),

    fn init(allocator: mem_allocator) StateSet {
        return .{
            .states = std.ArrayList(State).init(allocator),
            .index = std.AutoHashMap(State, usize).init(allocator),
        };
    }

    fn deinit(self: *StateSet) void {
        self.states.deinit();
        self.index.deinit();
    }

    fn add(self: *StateSet, state: State) !bool {
        if (self.index.contains(state)) {
            return false;
        }
        const idx = self.states.items.len;
        try self.states.append(state);
        try self.index.put(state, idx);
        return true;
    }
};

const Chart = struct {
    sets: []StateSet,

    fn init(allocator: mem_allocator, count: usize) !Chart {
        const sets = try allocator.alloc(StateSet, count);
        for (sets) |*set| {
            set.* = StateSet.init(allocator);
        }
        return .{ .sets = sets };
    }

    fn deinit(self: *Chart, allocator: mem_allocator) void {
        for (self.sets) |*set| {
            set.deinit();
        }
        allocator.free(self.sets);
    }
};

pub const NodeId = usize;

pub const Forest = struct {
    allocator: mem_allocator,
    nodes: std.ArrayList(Node),
    index: std.AutoHashMap(NodeKey, NodeId),

    pub const Family = struct {
        production: ?usize,
        children: []const NodeId,
    };

    pub const Node = struct {
        symbol: Symbol,
        start: usize,
        end: usize,
        families: std.ArrayListUnmanaged(Family),
    };

    const NodeKey = struct {
        symbol: Symbol,
        start: usize,
        end: usize,
    };

    pub fn init(allocator: mem_allocator) Forest {
        return .{
            .allocator = allocator,
            .nodes = std.ArrayList(Node).init(allocator),
            .index = std.AutoHashMap(NodeKey, NodeId).init(allocator),
        };
    }

    pub fn deinit(self: *Forest) void {
        for (self.nodes.items) |*node| {
            for (node.families.items) |family| {
                if (family.children.len != 0) {
                    self.allocator.free(family.children);
                }
            }
            node.families.deinit(self.allocator);
        }
        self.nodes.deinit();
        self.index.deinit();
    }

    pub fn get_or_create(self: *Forest, symbol: Symbol, start: usize, end: usize) !NodeId {
        const key = NodeKey{ .symbol = symbol, .start = start, .end = end };
        if (self.index.get(key)) |id| {
            return id;
        }
        const id = self.nodes.items.len;
        try self.nodes.append(.{
            .symbol = symbol,
            .start = start,
            .end = end,
            .families = .{},
        });
        try self.index.put(key, id);
        return id;
    }

    pub fn add_family(self: *Forest, node_id: NodeId, production: ?usize, children: []const NodeId) !void {
        const node = &self.nodes.items[node_id];
        try node.families.append(self.allocator, .{
            .production = production,
            .children = children,
        });
    }
};

pub const ParseErrorInfo = struct {
    position: usize,
    expected: []const token.kind,
    found: ?token.kind,
};

pub const ParseResult = struct {
    arena: std.heap.ArenaAllocator,
    forest: Forest,
    accepted: bool,
    @"error": ?ParseErrorInfo,

    pub fn deinit(self: *ParseResult) void {
        self.forest.deinit();
        self.arena.deinit();
    }
};

pub const Parser = struct {
    allocator: mem_allocator,
    grammar: *const Grammar,

    pub fn init(allocator: mem_allocator, grammar: *const Grammar) Parser {
        return .{ .allocator = allocator, .grammar = grammar };
    }

    pub fn parse(self: *Parser, tokens: []const token) parse_error!ParseResult {
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        errdefer arena.deinit();
        const alloc = arena.allocator();

        var chart = try Chart.init(alloc, tokens.len + 1);
        errdefer chart.deinit(alloc);

        const nt_count = @typeInfo(Nonterminal).@"enum".fields.len;
        var prod_map = try alloc.alloc(std.ArrayList(usize), nt_count);
        for (prod_map) |*list| {
            list.* = std.ArrayList(usize).init(alloc);
        }
        for (self.grammar.productions, 0..) |prod, idx| {
            const nt_index = @intFromEnum(prod.lhs);
            try prod_map[nt_index].append(idx);
        }

        const start_nt_index = @intFromEnum(self.grammar.start);
        for (prod_map[start_nt_index].items) |prod_index| {
            _ = try chart.sets[0].add(.{ .prod_index = prod_index, .dot = 0, .start = 0 });
        }

        var pos: usize = 0;
        while (pos < chart.sets.len) : (pos += 1) {
            var i: usize = 0;
            while (i < chart.sets[pos].states.items.len) : (i += 1) {
                const state = chart.sets[pos].states.items[i];
                const prod = self.grammar.productions[state.prod_index];
                if (state.dot >= prod.rhs.len) {
                    const completed_lhs = prod.lhs;
                    const origin = state.start;
                    for (chart.sets[origin].states.items) |origin_state| {
                        const origin_prod = self.grammar.productions[origin_state.prod_index];
                        if (origin_state.dot >= origin_prod.rhs.len) continue;
                        const next_sym = origin_prod.rhs[origin_state.dot];
                        if (next_sym.kind == .nonterminal and next_sym.nonterminal_kind() == completed_lhs) {
                            _ = try chart.sets[pos].add(.{
                                .prod_index = origin_state.prod_index,
                                .dot = origin_state.dot + 1,
                                .start = origin_state.start,
                            });
                        }
                    }
                    continue;
                }

                const next = prod.rhs[state.dot];
                switch (next.kind) {
                    .nonterminal => {
                        const nt_index = @intFromEnum(next.nonterminal_kind());
                        for (prod_map[nt_index].items) |next_prod_index| {
                            _ = try chart.sets[pos].add(.{ .prod_index = next_prod_index, .dot = 0, .start = pos });
                        }
                    },
                    .terminal => {
                        if (pos >= tokens.len) continue;
                        if (tokens[pos].which == next.terminal_kind()) {
                            _ = try chart.sets[pos + 1].add(.{
                                .prod_index = state.prod_index,
                                .dot = state.dot + 1,
                                .start = state.start,
                            });
                        }
                    },
                }
            }
        }

        const accepted = blk: {
            const end_set = &chart.sets[tokens.len];
            for (end_set.states.items) |state| {
                const prod = self.grammar.productions[state.prod_index];
                if (prod.lhs == self.grammar.start and state.start == 0 and state.dot == prod.rhs.len) {
                    break :blk true;
                }
            }
            break :blk false;
        };

        var forest = Forest.init(alloc);
        var error_info: ?ParseErrorInfo = null;

        if (accepted) {
            try build_forest(alloc, self.grammar, tokens, &chart, &forest);
        } else {
            error_info = try build_error_info(alloc, self.grammar, tokens, &chart);
        }

        chart.deinit(alloc);

        return .{
            .arena = arena,
            .forest = forest,
            .accepted = accepted,
            .@"error" = error_info,
        };
    }
};

fn build_error_info(
    allocator: mem_allocator,
    grammar: *const Grammar,
    tokens: []const token,
    chart: *const Chart,
) !ParseErrorInfo {
    var furthest: usize = 0;
    for (chart.sets, 0..) |set, idx| {
        if (set.states.items.len != 0) furthest = idx;
    }

    const kind_count = @typeInfo(token.kind).@"enum".fields.len;
    var expected_flags = try allocator.alloc(bool, kind_count);
    defer allocator.free(expected_flags);
    std.mem.set(bool, expected_flags, false);

    for (chart.sets[furthest].states.items) |state| {
        const prod = grammar.productions[state.prod_index];
        if (state.dot >= prod.rhs.len) continue;
        const next = prod.rhs[state.dot];
        if (next.kind == .terminal) {
            expected_flags[@intFromEnum(next.terminal_kind())] = true;
        }
    }

    var count: usize = 0;
    for (expected_flags) |flag| {
        if (flag) count += 1;
    }

    var expected = try allocator.alloc(token.kind, count);
    var i: usize = 0;
    for (expected_flags, 0..) |flag, idx| {
        if (!flag) continue;
        expected[i] = @enumFromInt(idx);
        i += 1;
    }

    const found = if (furthest < tokens.len) tokens[furthest].which else null;
    return .{
        .position = furthest,
        .expected = expected,
        .found = found,
    };
}

fn build_forest(
    allocator: mem_allocator,
    grammar: *const Grammar,
    tokens: []const token,
    chart: *const Chart,
    forest: *Forest,
) !void {
    const nt_count = @typeInfo(Nonterminal).@"enum".fields.len;
    var completed = try allocator.alloc([]std.ArrayListUnmanaged(usize), nt_count);
    for (completed) |*nt_entries| {
        nt_entries.* = try allocator.alloc(std.ArrayListUnmanaged(usize), tokens.len + 1);
        for (nt_entries.*) |*list| {
            list.* = .{};
        }
    }

    for (chart.sets, 0..) |set, end_pos| {
        for (set.states.items) |state| {
            const prod = grammar.productions[state.prod_index];
            if (state.dot != prod.rhs.len) continue;
            const nt_index = @intFromEnum(prod.lhs);
            try completed[nt_index][state.start].append(allocator, end_pos);
        }
    }

    var built = std.AutoHashMap(Forest.NodeKey, void).init(allocator);
    var building = std.AutoHashMap(Forest.NodeKey, void).init(allocator);

    const root_symbol = Symbol.nonterminal(grammar.start);
    _ = try build_symbol(
        allocator,
        grammar,
        tokens,
        completed,
        forest,
        &built,
        &building,
        root_symbol,
        0,
        tokens.len,
    );
}

fn build_symbol(
    allocator: mem_allocator,
    grammar: *const Grammar,
    tokens: []const token,
    completed: []const []std.ArrayListUnmanaged(usize),
    forest: *Forest,
    built: *std.AutoHashMap(Forest.NodeKey, void),
    building: *std.AutoHashMap(Forest.NodeKey, void),
    symbol: Symbol,
    start: usize,
    end: usize,
) !?NodeId {
    if (symbol.kind == .terminal) {
        if (start + 1 != end) return null;
        if (start >= tokens.len) return null;
        if (tokens[start].which != symbol.terminal_kind()) return null;
        return try forest.get_or_create(symbol, start, end);
    }

    const nt = symbol.nonterminal_kind();
    const nt_index = @intFromEnum(nt);
    if (!has_completed(completed[nt_index][start].items, end)) return null;

    const key = Forest.NodeKey{ .symbol = symbol, .start = start, .end = end };
    if (built.contains(key)) return try forest.get_or_create(symbol, start, end);
    if (building.contains(key)) return null;
    try building.put(key, {});

    const node_id = try forest.get_or_create(symbol, start, end);
    for (grammar.productions, 0..) |prod, prod_index| {
        if (prod.lhs != nt) continue;
        try expand_rhs(
            allocator,
            grammar,
            tokens,
            completed,
            forest,
            built,
            building,
            prod_index,
            0,
            start,
            end,
            &.{},
            node_id,
        );
    }

    _ = building.remove(key);
    try built.put(key, {});
    return node_id;
}

fn expand_rhs(
    allocator: mem_allocator,
    grammar: *const Grammar,
    tokens: []const token,
    completed: []const []std.ArrayListUnmanaged(usize),
    forest: *Forest,
    built: *std.AutoHashMap(Forest.NodeKey, void),
    building: *std.AutoHashMap(Forest.NodeKey, void),
    prod_index: usize,
    rhs_index: usize,
    pos: usize,
    end: usize,
    children: []const NodeId,
    parent: NodeId,
) !void {
    const prod = grammar.productions[prod_index];
    if (rhs_index >= prod.rhs.len) {
        if (pos == end) {
            try forest.add_family(parent, prod_index, children);
        }
        return;
    }

    const sym = prod.rhs[rhs_index];
    if (sym.kind == .terminal) {
        if (pos + 1 > end) return;
        if (pos >= tokens.len) return;
        if (tokens[pos].which != sym.terminal_kind()) return;
        const child_id = try forest.get_or_create(sym, pos, pos + 1);
        const next_children = try append_child(allocator, children, child_id);
        try expand_rhs(
            allocator,
            grammar,
            tokens,
            completed,
            forest,
            built,
            building,
            prod_index,
            rhs_index + 1,
            pos + 1,
            end,
            next_children,
            parent,
        );
        return;
    }

    const nt_index = @intFromEnum(sym.nonterminal_kind());
    for (completed[nt_index][pos].items) |mid| {
        if (mid > end) continue;
        const child_id = try build_symbol(
            allocator,
            grammar,
            tokens,
            completed,
            forest,
            built,
            building,
            sym,
            pos,
            mid,
        ) orelse continue;
        const next_children = try append_child(allocator, children, child_id);
        try expand_rhs(
            allocator,
            grammar,
            tokens,
            completed,
            forest,
            built,
            building,
            prod_index,
            rhs_index + 1,
            mid,
            end,
            next_children,
            parent,
        );
    }
}

fn append_child(allocator: mem_allocator, children: []const NodeId, child: NodeId) ![]const NodeId {
    var next = try allocator.alloc(NodeId, children.len + 1);
    if (children.len != 0) {
        std.mem.copyForwards(NodeId, next[0..children.len], children);
    }
    next[children.len] = child;
    return next;
}

fn has_completed(list: []const usize, target: usize) bool {
    for (list) |value| {
        if (value == target) return true;
    }
    return false;
}
