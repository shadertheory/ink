const token = @import("token.zig").token;

pub const Nonterminal = enum {
    program,
    expr,
    if_expr,
    match_expr,
    match_arm_list,
    match_arm,
    assign,
    logical_or,
    logical_and,
    comparison,
    sum,
    product,
    unary,
    postfix,
    call,
    arg_list,
    primary,
};

pub const SymbolKind = enum { terminal, nonterminal };

pub const Symbol = struct {
    kind: SymbolKind,
    value: u32,

    pub fn terminal(k: token.kind) Symbol {
        return .{ .kind = .terminal, .value = @intFromEnum(k) };
    }

    pub fn nonterminal(nt: Nonterminal) Symbol {
        return .{ .kind = .nonterminal, .value = @intFromEnum(nt) };
    }

    pub fn is_terminal(self: Symbol) bool {
        return self.kind == .terminal;
    }

    pub fn terminal_kind(self: Symbol) token.kind {
        return @enumFromInt(self.value);
    }

    pub fn nonterminal_kind(self: Symbol) Nonterminal {
        return @enumFromInt(self.value);
    }
};

pub const Production = struct {
    lhs: Nonterminal,
    rhs: []const Symbol,
};

pub const Grammar = struct {
    start: Nonterminal,
    productions: []const Production,
};

const t = Symbol.terminal;
const n = Symbol.nonterminal;

const productions = [_]Production{
    .{ .lhs = .program, .rhs = &.{ n(.expr), t(.end_of_file) } },

    .{ .lhs = .expr, .rhs = &.{ n(.if_expr) } },
    .{ .lhs = .expr, .rhs = &.{ n(.match_expr) } },
    .{ .lhs = .expr, .rhs = &.{ n(.assign) } },

    .{ .lhs = .if_expr, .rhs = &.{ t(.expr_if), n(.expr), n(.expr) } },
    .{ .lhs = .if_expr, .rhs = &.{ t(.expr_if), n(.expr), t(.expr_else), n(.expr) } },

    .{ .lhs = .match_expr, .rhs = &.{ t(.expr_match), n(.expr), n(.match_arm_list) } },
    .{ .lhs = .match_arm_list, .rhs = &.{ n(.match_arm_list), n(.match_arm) } },
    .{ .lhs = .match_arm_list, .rhs = &.{ n(.match_arm) } },
    .{ .lhs = .match_arm, .rhs = &.{ n(.expr), t(.arrow), n(.expr) } },

    .{ .lhs = .assign, .rhs = &.{ n(.logical_or) } },
    .{ .lhs = .assign, .rhs = &.{ t(.identifier), t(.assign), n(.assign) } },

    .{ .lhs = .logical_or, .rhs = &.{ n(.logical_or), t(.logical_or), n(.logical_and) } },
    .{ .lhs = .logical_or, .rhs = &.{ n(.logical_and) } },

    .{ .lhs = .logical_and, .rhs = &.{ n(.logical_and), t(.logical_and), n(.comparison) } },
    .{ .lhs = .logical_and, .rhs = &.{ n(.comparison) } },

    .{ .lhs = .comparison, .rhs = &.{ n(.comparison), t(.less_than), n(.sum) } },
    .{ .lhs = .comparison, .rhs = &.{ n(.comparison), t(.greater_than), n(.sum) } },
    .{ .lhs = .comparison, .rhs = &.{ n(.comparison), t(.less_or_equal), n(.sum) } },
    .{ .lhs = .comparison, .rhs = &.{ n(.comparison), t(.greater_or_equal), n(.sum) } },
    .{ .lhs = .comparison, .rhs = &.{ n(.comparison), t(.equal), n(.sum) } },
    .{ .lhs = .comparison, .rhs = &.{ n(.comparison), t(.not_equal), n(.sum) } },
    .{ .lhs = .comparison, .rhs = &.{ n(.sum) } },

    .{ .lhs = .sum, .rhs = &.{ n(.sum), t(.plus), n(.product) } },
    .{ .lhs = .sum, .rhs = &.{ n(.sum), t(.minus), n(.product) } },
    .{ .lhs = .sum, .rhs = &.{ n(.product) } },

    .{ .lhs = .product, .rhs = &.{ n(.product), t(.asterisk), n(.unary) } },
    .{ .lhs = .product, .rhs = &.{ n(.product), t(.slash), n(.unary) } },
    .{ .lhs = .product, .rhs = &.{ n(.unary) } },

    .{ .lhs = .unary, .rhs = &.{ t(.minus), n(.unary) } },
    .{ .lhs = .unary, .rhs = &.{ t(.bang), n(.unary) } },
    .{ .lhs = .unary, .rhs = &.{ t(.logical_not), n(.unary) } },
    .{ .lhs = .unary, .rhs = &.{ t(.stmt_return), n(.unary) } },
    .{ .lhs = .unary, .rhs = &.{ n(.postfix) } },

    .{ .lhs = .postfix, .rhs = &.{ n(.postfix), n(.call) } },
    .{ .lhs = .postfix, .rhs = &.{ n(.primary) } },

    .{ .lhs = .call, .rhs = &.{ t(.paren_left), n(.arg_list), t(.paren_right) } },
    .{ .lhs = .call, .rhs = &.{ t(.paren_left), t(.paren_right) } },

    .{ .lhs = .arg_list, .rhs = &.{ n(.arg_list), t(.comma), n(.expr) } },
    .{ .lhs = .arg_list, .rhs = &.{ n(.expr) } },

    .{ .lhs = .primary, .rhs = &.{ t(.number) } },
    .{ .lhs = .primary, .rhs = &.{ t(.identifier) } },
    .{ .lhs = .primary, .rhs = &.{ t(.logical_true) } },
    .{ .lhs = .primary, .rhs = &.{ t(.logical_false) } },
    .{ .lhs = .primary, .rhs = &.{ t(.this) } },
    .{ .lhs = .primary, .rhs = &.{ t(.paren_left), n(.expr), t(.paren_right) } },
};

pub const grammar = Grammar{
    .start = .program,
    .productions = &productions,
};
