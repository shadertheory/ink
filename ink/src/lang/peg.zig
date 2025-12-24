const std = @import("std");
const token = @import("token.zig").token;

pub const mem_allocator = std.mem.Allocator;

pub const nonterminal_kind = enum {
    program,
    layout,
    stmt,
    decl,
    expr,
    branch,
    if_expr,
    match_expr,
    match_arm,
    match_arm_block,
    return_expr,
    assign,
    pipe,
    coalesce,
    logical_or,
    logical_and,
    comparison,
    sum,
    product,
    unary,
    postfix,
    primary,
    call_suffix,
    access_suffix,
    index_suffix,
    arg_list,
    generic_args,
    function_decl,
    trait_decl,
    trait_body,
    trait_item,
    assoc_type_decl,
    concept_decl,
    concept_body,
    requires_clause,
    sum_decl,
    sum_body,
    sum_variant,
    enum_decl,
    enum_body,
    struct_decl,
    struct_body,
    struct_field,
    impl_decl,
    impl_body,
    const_decl,
    var_decl,
    param_list,
    param,
    generic_params,
    generic_param,
    return_type,
    where_clause,
    block,
    type_expr,
    type_atom,
    type_args,
};

pub const symbol_kind = enum { terminal, nonterminal };

pub const symbol = struct {
    kind: symbol_kind,
    value: u32,

    pub fn terminal(kind: token.kind) symbol {
        return .{ .kind = .terminal, .value = @intFromEnum(kind) };
    }

    pub fn nonterminal(kind: nonterminal_kind) symbol {
        return .{ .kind = .nonterminal, .value = @intFromEnum(kind) };
    }
};

pub const expr = union(enum) {
    sequence: []const expr_id,
    choice: []const expr_id,
    zero_or_more: expr_id,
    one_or_more: expr_id,
    optional: expr_id,
    and_pred: expr_id,
    not_pred: expr_id,
    terminal: token.kind,
    nonterminal: nonterminal_kind,
};

pub const expr_id = usize;

pub const rule_def = struct {
    lhs: nonterminal_kind,
    expr: expr_id,
};

pub const grammar = struct {
    start: nonterminal_kind,
    rules: []const rule_def,
    exprs: []const expr,
    rule_index: []const usize,
};

pub const builder = struct {
    allocator: mem_allocator,
    exprs: std.array_list.Managed(expr),
    rules: std.array_list.Managed(rule_def),

    pub fn init(allocator: mem_allocator) builder {
        return .{
            .allocator = allocator,
            .exprs = std.array_list.Managed(expr).init(allocator),
            .rules = std.array_list.Managed(rule_def).init(allocator),
        };
    }

    pub fn deinit(self: *builder) void {
        self.exprs.deinit();
        self.rules.deinit();
    }

    fn push(self: *builder, value: expr) !expr_id {
        const id = self.exprs.items.len;
        try self.exprs.append(value);
        return id;
    }

    pub fn terminal(self: *builder, kind: token.kind) !expr_id {
        return self.push(.{ .terminal = kind });
    }

    pub fn nonterminal(self: *builder, kind: nonterminal_kind) !expr_id {
        return self.push(.{ .nonterminal = kind });
    }

    pub fn sequence(self: *builder, items: []const expr_id) !expr_id {
        const duped = try self.allocator.dupe(expr_id, items);
        return self.push(.{ .sequence = duped });
    }

    pub fn choice(self: *builder, items: []const expr_id) !expr_id {
        const duped = try self.allocator.dupe(expr_id, items);
        return self.push(.{ .choice = duped });
    }

    pub fn zero_or_more(self: *builder, item: expr_id) !expr_id {
        return self.push(.{ .zero_or_more = item });
    }

    pub fn one_or_more(self: *builder, item: expr_id) !expr_id {
        return self.push(.{ .one_or_more = item });
    }

    pub fn optional(self: *builder, item: expr_id) !expr_id {
        return self.push(.{ .optional = item });
    }

    pub fn and_pred(self: *builder, item: expr_id) !expr_id {
        return self.push(.{ .and_pred = item });
    }

    pub fn not_pred(self: *builder, item: expr_id) !expr_id {
        return self.push(.{ .not_pred = item });
    }

    pub fn rule(self: *builder, lhs: nonterminal_kind, expr_idx: expr_id) !void {
        try self.rules.append(.{ .lhs = lhs, .expr = expr_idx });
    }

    pub fn finish(self: *builder, start: nonterminal_kind) !grammar {
        const rules = try self.rules.toOwnedSlice();
        const exprs = try self.exprs.toOwnedSlice();
        const nt_count = @typeInfo(nonterminal_kind).@"enum".fields.len;
        const rule_index = try self.allocator.alloc(usize, nt_count);
        const missing = std.math.maxInt(usize);
        for (rule_index) |*slot| slot.* = missing;
        for (rules, 0..) |subrule, idx| {
            rule_index[@intFromEnum(subrule.lhs)] = idx;
        }
        for (rule_index) |idx| {
            if (idx == missing) return error.missing_rule;
        }
        return .{
            .start = start,
            .rules = rules,
            .exprs = exprs,
            .rule_index = rule_index,
        };
    }
};

fn t(b: *builder, kind: token.kind) !expr_id {
    return b.terminal(kind);
}

fn n(b: *builder, kind: nonterminal_kind) !expr_id {
    return b.nonterminal(kind);
}

pub fn build(allocator: mem_allocator) !grammar {
    var b = builder.init(allocator);
    errdefer b.deinit();

    const layout = try b.zero_or_more(try t(&b, .new_line));
    try b.rule(.layout, layout);

    const block = try b.sequence(&.{
        try n(&b, .layout),
        try t(&b, .indent),
        try n(&b, .layout),
        try n(&b, .stmt),
        try b.zero_or_more(try b.sequence(&.{
            try n(&b, .layout),
            try n(&b, .stmt),
        })),
        try n(&b, .layout),
        try t(&b, .dedent),
    });
    try b.rule(.block, block);

    const branch = try b.choice(&.{
        try n(&b, .block),
        try n(&b, .expr),
    });
    try b.rule(.branch, branch);

    const return_expr = try b.sequence(&.{
        try t(&b, .stmt_return),
        try n(&b, .expr),
    });
    try b.rule(.return_expr, return_expr);

    const if_expr = try b.sequence(&.{
        try t(&b, .expr_if),
        try n(&b, .expr),
        try n(&b, .branch),
        try b.optional(try b.sequence(&.{
            try n(&b, .layout),
            try t(&b, .expr_else),
            try n(&b, .branch),
        })),
    });
    try b.rule(.if_expr, if_expr);

    const match_arm = try b.sequence(&.{
        try n(&b, .expr),
        try t(&b, .arrow),
        try n(&b, .expr),
    });
    try b.rule(.match_arm, match_arm);

    const match_arm_block = try b.sequence(&.{
        try n(&b, .layout),
        try t(&b, .indent),
        try n(&b, .match_arm),
        try b.zero_or_more(try b.sequence(&.{
            try n(&b, .layout),
            try n(&b, .match_arm),
        })),
        try n(&b, .layout),
        try t(&b, .dedent),
    });
    try b.rule(.match_arm_block, match_arm_block);

    const match_head = try b.sequence(&.{
        try t(&b, .expr_match),
        try n(&b, .expr),
    });

    const match_expr = try b.choice(&.{
        try b.sequence(&.{
            match_head,
            try n(&b, .match_arm),
            try b.optional(try n(&b, .match_arm_block)),
        }),
        try b.sequence(&.{
            match_head,
            try n(&b, .match_arm_block),
        }),
    });
    try b.rule(.match_expr, match_expr);

    const coalesce = try b.sequence(&.{
        try n(&b, .logical_or),
        try b.zero_or_more(try b.sequence(&.{
            try t(&b, .coalesce),
            try n(&b, .logical_or),
        })),
    });
    try b.rule(.coalesce, coalesce);

    const pipe_expr = try b.sequence(&.{
        try n(&b, .coalesce),
        try b.zero_or_more(try b.sequence(&.{
            try t(&b, .pipe),
            try n(&b, .coalesce),
        })),
    });
    try b.rule(.pipe, pipe_expr);

    const assign = try b.sequence(&.{
        try n(&b, .pipe),
        try b.optional(try b.sequence(&.{
            try t(&b, .assign),
            try n(&b, .assign),
        })),
    });
    try b.rule(.assign, assign);

    const logical_or = try b.sequence(&.{
        try n(&b, .logical_and),
        try b.zero_or_more(try b.sequence(&.{
            try b.choice(&.{
                try t(&b, .logical_or),
                try t(&b, .logical_xor),
            }),
            try n(&b, .logical_and),
        })),
    });
    try b.rule(.logical_or, logical_or);

    const logical_and = try b.sequence(&.{
        try n(&b, .comparison),
        try b.zero_or_more(try b.sequence(&.{
            try t(&b, .logical_and),
            try n(&b, .comparison),
        })),
    });
    try b.rule(.logical_and, logical_and);

    const comparison = try b.sequence(&.{
        try n(&b, .sum),
        try b.zero_or_more(try b.sequence(&.{
            try b.choice(&.{
                try t(&b, .less_than),
                try t(&b, .greater_than),
                try t(&b, .less_or_equal),
                try t(&b, .greater_or_equal),
                try t(&b, .equal),
                try t(&b, .not_equal),
            }),
            try n(&b, .sum),
        })),
    });
    try b.rule(.comparison, comparison);

    const sum = try b.sequence(&.{
        try n(&b, .product),
        try b.zero_or_more(try b.sequence(&.{
            try b.choice(&.{
                try t(&b, .plus),
                try t(&b, .minus),
            }),
            try n(&b, .product),
        })),
    });
    try b.rule(.sum, sum);

    const product = try b.sequence(&.{
        try n(&b, .unary),
        try b.zero_or_more(try b.sequence(&.{
            try b.choice(&.{
                try t(&b, .asterisk),
                try t(&b, .slash),
            }),
            try n(&b, .unary),
        })),
    });
    try b.rule(.product, product);

    const unary = try b.choice(&.{
        try b.sequence(&.{ try t(&b, .minus), try n(&b, .unary) }),
        try b.sequence(&.{ try t(&b, .bang), try n(&b, .unary) }),
        try b.sequence(&.{ try t(&b, .logical_not), try n(&b, .unary) }),
        try n(&b, .postfix),
    });
    try b.rule(.unary, unary);

    const generic_args = try b.sequence(&.{
        try t(&b, .less_than),
        try n(&b, .expr),
        try b.zero_or_more(try b.sequence(&.{
            try t(&b, .comma),
            try n(&b, .expr),
        })),
        try t(&b, .greater_than),
    });
    try b.rule(.generic_args, generic_args);

    const call_suffix = try b.sequence(&.{
        try t(&b, .paren_left),
        try b.optional(try n(&b, .arg_list)),
        try t(&b, .paren_right),
    });
    try b.rule(.call_suffix, call_suffix);

    const access_suffix = try b.sequence(&.{
        try b.choice(&.{
            try t(&b, .dot),
            try t(&b, .double_colon),
            try t(&b, .question_dot),
        }),
        try t(&b, .identifier),
    });
    try b.rule(.access_suffix, access_suffix);

    const index_suffix = try b.sequence(&.{
        try t(&b, .bracket_left),
        try n(&b, .expr),
        try t(&b, .bracket_right),
    });
    try b.rule(.index_suffix, index_suffix);

    const postfix = try b.sequence(&.{
        try n(&b, .primary),
        try b.zero_or_more(try b.choice(&.{
            try n(&b, .generic_args),
            try n(&b, .call_suffix),
            try n(&b, .access_suffix),
            try n(&b, .index_suffix),
        })),
    });
    try b.rule(.postfix, postfix);

    const arg_list = try b.sequence(&.{
        try n(&b, .expr),
        try b.zero_or_more(try b.sequence(&.{
            try t(&b, .comma),
            try n(&b, .expr),
        })),
    });
    try b.rule(.arg_list, arg_list);

    const primary = try b.choice(&.{
        try t(&b, .number),
        try t(&b, .string),
        try t(&b, .identifier),
        try t(&b, .logical_true),
        try t(&b, .logical_false),
        try t(&b, .this),
        try b.sequence(&.{
            try t(&b, .paren_left),
            try n(&b, .expr),
            try t(&b, .paren_right),
        }),
    });
    try b.rule(.primary, primary);

    const type_args = try b.sequence(&.{
        try t(&b, .less_than),
        try n(&b, .expr),
        try b.zero_or_more(try b.sequence(&.{
            try t(&b, .comma),
            try n(&b, .expr),
        })),
        try t(&b, .greater_than),
    });
    try b.rule(.type_args, type_args);

    const type_atom = try b.choice(&.{
        try t(&b, .type),
        try t(&b, .identifier),
    });
    try b.rule(.type_atom, type_atom);

    const type_expr = try b.choice(&.{
        try b.sequence(&.{ try t(&b, .question), try n(&b, .type_expr) }),
        try t(&b, .self),
        try b.sequence(&.{
            try n(&b, .type_atom),
            try b.optional(try n(&b, .type_args)),
        }),
    });
    try b.rule(.type_expr, type_expr);

    const generic_param = try b.sequence(&.{
        try t(&b, .identifier),
        try b.optional(try b.sequence(&.{
            try t(&b, .colon),
            try n(&b, .type_expr),
        })),
        try b.optional(try b.sequence(&.{
            try t(&b, .assign),
            try n(&b, .type_expr),
        })),
    });
    try b.rule(.generic_param, generic_param);

    const generic_params = try b.sequence(&.{
        try t(&b, .less_than),
        try n(&b, .generic_param),
        try b.zero_or_more(try b.sequence(&.{
            try t(&b, .comma),
            try n(&b, .generic_param),
        })),
        try t(&b, .greater_than),
    });
    try b.rule(.generic_params, generic_params);

    const param = try b.choice(&.{
        try b.sequence(&.{
            try t(&b, .this),
            try b.optional(try b.sequence(&.{
                try t(&b, .colon),
                try n(&b, .type_expr),
            })),
        }),
        try b.sequence(&.{
            try t(&b, .identifier),
            try t(&b, .colon),
            try n(&b, .type_expr),
        }),
    });
    try b.rule(.param, param);

    const param_list = try b.sequence(&.{
        try n(&b, .param),
        try b.zero_or_more(try b.sequence(&.{
            try t(&b, .comma),
            try n(&b, .param),
        })),
    });
    try b.rule(.param_list, param_list);

    const return_type = try b.sequence(&.{
        try t(&b, .arrow),
        try n(&b, .type_expr),
    });
    try b.rule(.return_type, return_type);

    const where_clause = try b.sequence(&.{
        try t(&b, .where),
        try t(&b, .identifier),
        try t(&b, .colon),
        try n(&b, .type_expr),
    });
    try b.rule(.where_clause, where_clause);

    const function_decl = try b.sequence(&.{
        try t(&b, .function),
        try t(&b, .identifier),
        try b.optional(try n(&b, .generic_params)),
        try t(&b, .paren_left),
        try b.optional(try n(&b, .param_list)),
        try t(&b, .paren_right),
        try b.optional(try n(&b, .return_type)),
        try b.optional(try n(&b, .where_clause)),
        try b.optional(try n(&b, .block)),
    });
    try b.rule(.function_decl, function_decl);

    const assoc_type_decl = try b.sequence(&.{
        try t(&b, .type),
        try t(&b, .identifier),
        try b.optional(try b.sequence(&.{
            try t(&b, .assign),
            try n(&b, .type_expr),
        })),
    });
    try b.rule(.assoc_type_decl, assoc_type_decl);

    const trait_item = try b.choice(&.{
        try n(&b, .function_decl),
        try n(&b, .assoc_type_decl),
    });
    try b.rule(.trait_item, trait_item);

    const trait_body = try b.sequence(&.{
        try n(&b, .layout),
        try t(&b, .indent),
        try b.zero_or_more(try b.sequence(&.{
            try n(&b, .layout),
            try n(&b, .trait_item),
        })),
        try n(&b, .layout),
        try t(&b, .dedent),
    });
    try b.rule(.trait_body, trait_body);

    const trait_decl = try b.sequence(&.{
        try t(&b, .trait),
        try t(&b, .identifier),
        try b.optional(try n(&b, .generic_params)),
        try b.optional(try n(&b, .trait_body)),
    });
    try b.rule(.trait_decl, trait_decl);

    const requires_clause = try b.sequence(&.{
        try t(&b, .requires),
        try n(&b, .type_expr),
    });
    try b.rule(.requires_clause, requires_clause);

    const concept_body = try b.sequence(&.{
        try n(&b, .layout),
        try t(&b, .indent),
        try b.zero_or_more(try b.sequence(&.{
            try n(&b, .layout),
            try n(&b, .requires_clause),
        })),
        try n(&b, .layout),
        try t(&b, .dedent),
    });
    try b.rule(.concept_body, concept_body);

    const concept_decl = try b.sequence(&.{
        try t(&b, .concept),
        try t(&b, .identifier),
        try b.optional(try n(&b, .generic_params)),
        try b.optional(try n(&b, .concept_body)),
    });
    try b.rule(.concept_decl, concept_decl);

    const sum_variant = try b.sequence(&.{
        try t(&b, .identifier),
        try b.optional(try b.sequence(&.{
            try t(&b, .paren_left),
            try b.optional(try n(&b, .type_expr)),
            try t(&b, .paren_right),
        })),
    });
    try b.rule(.sum_variant, sum_variant);

    const sum_body = try b.sequence(&.{
        try n(&b, .layout),
        try t(&b, .indent),
        try b.zero_or_more(try b.sequence(&.{
            try n(&b, .layout),
            try n(&b, .sum_variant),
        })),
        try n(&b, .layout),
        try t(&b, .dedent),
    });
    try b.rule(.sum_body, sum_body);

    const sum_decl = try b.sequence(&.{
        try t(&b, .sum),
        try t(&b, .identifier),
        try b.optional(try n(&b, .generic_params)),
        try b.optional(try n(&b, .sum_body)),
    });
    try b.rule(.sum_decl, sum_decl);

    const enum_body = try b.sequence(&.{
        try n(&b, .layout),
        try t(&b, .indent),
        try b.zero_or_more(try b.sequence(&.{
            try n(&b, .layout),
            try t(&b, .identifier),
        })),
        try n(&b, .layout),
        try t(&b, .dedent),
    });
    try b.rule(.enum_body, enum_body);

    const enum_decl = try b.sequence(&.{
        try t(&b, .@"enum"),
        try t(&b, .identifier),
        try b.optional(try n(&b, .enum_body)),
    });
    try b.rule(.enum_decl, enum_decl);

    const struct_field = try b.sequence(&.{
        try t(&b, .identifier),
        try t(&b, .colon),
        try n(&b, .type_expr),
    });
    try b.rule(.struct_field, struct_field);

    const struct_body = try b.sequence(&.{
        try n(&b, .layout),
        try t(&b, .indent),
        try b.zero_or_more(try b.sequence(&.{
            try n(&b, .layout),
            try n(&b, .struct_field),
        })),
        try n(&b, .layout),
        try t(&b, .dedent),
    });
    try b.rule(.struct_body, struct_body);

    const struct_decl = try b.sequence(&.{
        try t(&b, .@"struct"),
        try t(&b, .identifier),
        try b.optional(try n(&b, .generic_params)),
        try b.optional(try n(&b, .struct_body)),
    });
    try b.rule(.struct_decl, struct_decl);

    const impl_body = try b.sequence(&.{
        try n(&b, .layout),
        try t(&b, .indent),
        try b.zero_or_more(try b.sequence(&.{
            try n(&b, .layout),
            try n(&b, .function_decl),
        })),
        try n(&b, .layout),
        try t(&b, .dedent),
    });
    try b.rule(.impl_body, impl_body);

    const impl_decl = try b.sequence(&.{
        try t(&b, .impl),
        try t(&b, .identifier),
        try t(&b, .@"for"),
        try t(&b, .identifier),
        try b.optional(try n(&b, .impl_body)),
    });
    try b.rule(.impl_decl, impl_decl);

    const const_decl = try b.sequence(&.{
        try t(&b, .constant),
        try t(&b, .identifier),
        try b.optional(try b.sequence(&.{
            try t(&b, .colon),
            try n(&b, .type_expr),
        })),
        try t(&b, .assign),
        try n(&b, .expr),
    });
    try b.rule(.const_decl, const_decl);

    const var_decl = try b.sequence(&.{
        try t(&b, .variable),
        try t(&b, .identifier),
        try b.optional(try b.sequence(&.{
            try t(&b, .colon),
            try n(&b, .type_expr),
        })),
        try t(&b, .assign),
        try n(&b, .expr),
    });
    try b.rule(.var_decl, var_decl);

    const decl = try b.choice(&.{
        try n(&b, .function_decl),
        try n(&b, .struct_decl),
        try n(&b, .trait_decl),
        try n(&b, .concept_decl),
        try n(&b, .sum_decl),
        try n(&b, .enum_decl),
        try n(&b, .impl_decl),
        try n(&b, .const_decl),
        try n(&b, .var_decl),
    });
    try b.rule(.decl, decl);

    const expr_rule = try b.choice(&.{
        try n(&b, .if_expr),
        try n(&b, .match_expr),
        try n(&b, .return_expr),
        try n(&b, .assign),
    });
    try b.rule(.expr, expr_rule);

    const stmt = try b.choice(&.{
        try n(&b, .decl),
        try n(&b, .expr),
    });
    try b.rule(.stmt, stmt);

    const program = try b.sequence(&.{
        try n(&b, .layout),
        try n(&b, .stmt),
        try b.zero_or_more(try b.sequence(&.{
            try n(&b, .layout),
            try n(&b, .stmt),
        })),
        try n(&b, .layout),
        try t(&b, .end_of_file),
    });
    try b.rule(.program, program);

    return b.finish(.program);
}
