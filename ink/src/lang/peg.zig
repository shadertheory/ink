const std = @import("std");
const spec = @import("spec.zig");
const token = @import("token.zig").token;

pub const mem_allocator = std.mem.Allocator;

pub const nonterminal_kind = enum {
    program,
    layout,
    name,
    qualified_name,
    attribute_list,
    attribute,
    attribute_args,
    token_stream_paren,
    token_stream_paren_item,
    stmt,
    decl,
    expr,
    label_expr,
    branch,
    if_expr,
    match_expr,
    match_arm,
    match_arm_block,
    select_expr,
    with_expr,
    loop_expr,
    while_expr,
    while_in_expr,
    until_expr,
    repeat_expr,
    for_expr,
    each_expr,
    break_expr,
    continue_expr,
    yield_expr,
    atomic_expr,
    select_arm,
    select_arm_block,
    pattern,
    pattern_prefix,
    pattern_ref,
    pattern_postfix,
    pattern_primary,
    pattern_group,
    pattern_arg_list,
    pattern_call_suffix,
    pattern_access_suffix,
    return_expr,
    assign,
    assign_op,
    pipe,
    coalesce,
    logical_or,
    logical_and,
    bitwise_or,
    bitwise_xor,
    bitwise_and,
    comparison,
    shift,
    sum,
    product,
    unary,
    postfix,
    macro_suffix,
    macro_block,
    token_stream_indent,
    token_stream_indent_item,
    duration_literal,
    duration_item,
    intrinsic_call,
    primary,
    call_suffix,
    access_suffix,
    cast_suffix,
    index_suffix,
    arg_list,
    generic_args,
    record_block,
    record_field,
    function_decl,
    trait_decl,
    trait_body,
    trait_body_item,
    trait_item,
    assoc_type_decl,
    requires_clause,
    sum_variant,
    enum_decl,
    enum_body,
    struct_decl,
    struct_body,
    struct_field,
    impl_decl,
    import_decl,
    impl_body,
    const_decl,
    var_decl,
    type_decl,
    param_list,
    param,
    generic_params,
    generic_param,
    return_type,
    where_clause,
    block,
    type_expr,
    type_arrow,
    type_union,
    type_intersect,
    type_prefix,
    type_ref,
    type_dyn,
    type_postfix,
    type_primary,
    type_group,
    type_tuple,
    type_array,
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

    const name = blk: {
        var items = std.array_list.Managed(expr_id).init(allocator);
        try items.append(try t(&b, .identifier));
        inline for (spec.keyword_lexemes) |lex| {
            try items.append(try t(&b, @field(token.kind, lex.kind)));
        }
        const choice = try b.choice(items.items);
        items.deinit();
        break :blk choice;
    };
    try b.rule(.name, name);

    const name_token = try n(&b, .name);
    const qualified_name = try b.sequence(&.{
        name_token,
        try b.zero_or_more(try b.sequence(&.{ try t(&b, .double_colon), name_token })),
    });
    try b.rule(.qualified_name, qualified_name);

    const token_stream_paren_token = blk: {
        var items = std.array_list.Managed(expr_id).init(allocator);
        inline for (@typeInfo(token.kind).@"enum".fields) |field| {
            const kind = @field(token.kind, field.name);
            if (kind == .paren_left or kind == .paren_right or kind == .end_of_file or kind == .illegal) continue;
            try items.append(try t(&b, kind));
        }
        const choice = try b.choice(items.items);
        items.deinit();
        break :blk choice;
    };

    const token_stream_paren_group = try b.sequence(&.{
        try t(&b, .paren_left),
        try n(&b, .token_stream_paren),
        try t(&b, .paren_right),
    });

    const token_stream_paren_item = try b.choice(&.{
        token_stream_paren_group,
        token_stream_paren_token,
    });
    try b.rule(.token_stream_paren_item, token_stream_paren_item);

    const token_stream_paren = try b.zero_or_more(try n(&b, .token_stream_paren_item));
    try b.rule(.token_stream_paren, token_stream_paren);

    const attribute_args = try b.sequence(&.{
        try t(&b, .paren_left),
        try n(&b, .token_stream_paren),
        try t(&b, .paren_right),
    });
    try b.rule(.attribute_args, attribute_args);

    const attribute = try b.sequence(&.{
        try t(&b, .hash),
        try t(&b, .bracket_left),
        try n(&b, .layout),
        name_token,
        try n(&b, .layout),
        try b.optional(try n(&b, .attribute_args)),
        try n(&b, .layout),
        try t(&b, .bracket_right),
    });
    try b.rule(.attribute, attribute);

    const attribute_list = try b.sequence(&.{
        try n(&b, .attribute),
        try n(&b, .layout),
        try b.zero_or_more(try b.sequence(&.{
            try n(&b, .attribute),
            try n(&b, .layout),
        })),
    });
    try b.rule(.attribute_list, attribute_list);

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

    const record_field = try b.sequence(&.{
        name_token,
        try t(&b, .assign),
        try n(&b, .expr),
    });
    try b.rule(.record_field, record_field);

    const record_block = try b.sequence(&.{
        try n(&b, .layout),
        try t(&b, .indent),
        try n(&b, .layout),
        try n(&b, .record_field),
        try b.zero_or_more(try b.sequence(&.{
            try n(&b, .layout),
            try n(&b, .record_field),
        })),
        try n(&b, .layout),
        try t(&b, .dedent),
    });
    try b.rule(.record_block, record_block);

    const token_stream_indent_token = blk: {
        var items = std.array_list.Managed(expr_id).init(allocator);
        inline for (@typeInfo(token.kind).@"enum".fields) |field| {
            const kind = @field(token.kind, field.name);
            if (kind == .indent or kind == .dedent or kind == .end_of_file or kind == .illegal) continue;
            try items.append(try t(&b, kind));
        }
        const choice = try b.choice(items.items);
        items.deinit();
        break :blk choice;
    };

    const token_stream_indent_block = try b.sequence(&.{
        try n(&b, .layout),
        try t(&b, .indent),
        try n(&b, .token_stream_indent),
        try t(&b, .dedent),
    });

    const token_stream_indent_item = try b.choice(&.{
        token_stream_indent_block,
        token_stream_indent_token,
    });
    try b.rule(.token_stream_indent_item, token_stream_indent_item);

    const token_stream_indent = try b.zero_or_more(try n(&b, .token_stream_indent_item));
    try b.rule(.token_stream_indent, token_stream_indent);

    const macro_block = try b.sequence(&.{
        try n(&b, .layout),
        try t(&b, .indent),
        try n(&b, .token_stream_indent),
        try t(&b, .dedent),
    });
    try b.rule(.macro_block, macro_block);

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

    const with_expr = try b.sequence(&.{
        try t(&b, .with),
        try n(&b, .layout),
        name_token,
        try n(&b, .layout),
        name_token,
        try n(&b, .branch),
    });
    try b.rule(.with_expr, with_expr);

    const loop_expr = try b.sequence(&.{
        try t(&b, .loop),
        try n(&b, .branch),
    });
    try b.rule(.loop_expr, loop_expr);

    const while_in_expr = try b.sequence(&.{
        try t(&b, .@"while"),
        try n(&b, .pattern),
        try t(&b, .in),
        try n(&b, .expr),
        try n(&b, .branch),
    });
    try b.rule(.while_in_expr, while_in_expr);

    const while_expr = try b.sequence(&.{
        try t(&b, .@"while"),
        try n(&b, .expr),
        try n(&b, .branch),
    });
    try b.rule(.while_expr, while_expr);

    const until_expr = try b.sequence(&.{
        try t(&b, .until),
        try n(&b, .expr),
        try n(&b, .branch),
    });
    try b.rule(.until_expr, until_expr);

    const repeat_expr = try b.sequence(&.{
        try t(&b, .repeat),
        try n(&b, .expr),
        try n(&b, .branch),
    });
    try b.rule(.repeat_expr, repeat_expr);

    const for_expr = try b.sequence(&.{
        try t(&b, .@"for"),
        try n(&b, .pattern),
        try t(&b, .in),
        try n(&b, .expr),
        try n(&b, .branch),
    });
    try b.rule(.for_expr, for_expr);

    const each_expr = try b.sequence(&.{
        try t(&b, .each),
        try n(&b, .pattern),
        try t(&b, .in),
        try n(&b, .expr),
        try n(&b, .branch),
    });
    try b.rule(.each_expr, each_expr);

    const label_expr = try b.sequence(&.{
        try t(&b, .label),
        try n(&b, .layout),
        try b.choice(&.{
            try n(&b, .loop_expr),
            try n(&b, .while_in_expr),
            try n(&b, .while_expr),
            try n(&b, .until_expr),
            try n(&b, .repeat_expr),
            try n(&b, .for_expr),
            try n(&b, .each_expr),
            try n(&b, .block),
        }),
    });
    try b.rule(.label_expr, label_expr);

    const break_expr = try b.sequence(&.{
        try t(&b, .stmt_break),
        try b.optional(try t(&b, .label)),
        try b.optional(try n(&b, .expr)),
    });
    try b.rule(.break_expr, break_expr);

    const continue_expr = try b.sequence(&.{
        try t(&b, .stmt_continue),
        try b.optional(try t(&b, .label)),
    });
    try b.rule(.continue_expr, continue_expr);

    const yield_expr = try b.sequence(&.{
        try t(&b, .yield),
        try b.optional(try n(&b, .expr)),
    });
    try b.rule(.yield_expr, yield_expr);

    const atomic_expr = try b.sequence(&.{
        try t(&b, .atomic),
        try n(&b, .expr),
        name_token,
    });
    try b.rule(.atomic_expr, atomic_expr);

    const pattern_group = try b.sequence(&.{
        try t(&b, .paren_left),
        try n(&b, .layout),
        try n(&b, .pattern),
        try n(&b, .layout),
        try t(&b, .paren_right),
    });
    try b.rule(.pattern_group, pattern_group);

    const pattern_primary = try b.choice(&.{
        try n(&b, .pattern_group),
        try t(&b, .asterisk),
        try t(&b, .number),
        try t(&b, .string),
        try t(&b, .logical_true),
        try t(&b, .logical_false),
        try t(&b, .this),
        name_token,
    });
    try b.rule(.pattern_primary, pattern_primary);

    const pattern_arg_list = try b.sequence(&.{
        try n(&b, .pattern),
        try b.zero_or_more(try b.sequence(&.{
            try n(&b, .layout),
            try t(&b, .comma),
            try n(&b, .layout),
            try n(&b, .pattern),
        })),
    });
    try b.rule(.pattern_arg_list, pattern_arg_list);

    const pattern_call_suffix = try b.sequence(&.{
        try t(&b, .paren_left),
        try n(&b, .layout),
        try b.optional(try n(&b, .pattern_arg_list)),
        try n(&b, .layout),
        try t(&b, .paren_right),
    });
    try b.rule(.pattern_call_suffix, pattern_call_suffix);

    const access_name = name_token;

    const pattern_access_suffix = try b.sequence(&.{
        try b.choice(&.{
            try t(&b, .dot),
            try t(&b, .double_colon),
            try t(&b, .question_dot),
        }),
        access_name,
    });
    try b.rule(.pattern_access_suffix, pattern_access_suffix);

    const pattern_postfix = try b.sequence(&.{
        try n(&b, .pattern_primary),
        try b.zero_or_more(try b.choice(&.{
            try n(&b, .pattern_call_suffix),
            try n(&b, .pattern_access_suffix),
        })),
    });
    try b.rule(.pattern_postfix, pattern_postfix);

    const pattern_ref = try b.sequence(&.{
        try t(&b, .ref),
        try b.optional(try t(&b, .mut)),
        try n(&b, .pattern),
    });
    try b.rule(.pattern_ref, pattern_ref);

    const pattern_prefix = try b.choice(&.{
        try n(&b, .pattern_ref),
        try n(&b, .pattern_postfix),
    });
    try b.rule(.pattern_prefix, pattern_prefix);

    const pattern = try n(&b, .pattern_prefix);
    try b.rule(.pattern, pattern);

    const match_arm = try b.sequence(&.{
        try n(&b, .pattern),
        try n(&b, .layout),
        try t(&b, .arrow),
        try n(&b, .layout),
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

    const select_arm = try b.sequence(&.{
        try t(&b, .case),
        try n(&b, .layout),
        try b.optional(try b.sequence(&.{
            try t(&b, .detached),
            try n(&b, .layout),
        })),
        name_token,
        try n(&b, .layout),
        try t(&b, .assign),
        try n(&b, .layout),
        try t(&b, .await),
        try n(&b, .layout),
        try n(&b, .expr),
        try n(&b, .layout),
        try t(&b, .arrow),
        try n(&b, .layout),
        try n(&b, .expr),
    });
    try b.rule(.select_arm, select_arm);

    const select_arm_block = try b.sequence(&.{
        try n(&b, .layout),
        try t(&b, .indent),
        try n(&b, .select_arm),
        try b.zero_or_more(try b.sequence(&.{
            try n(&b, .layout),
            try n(&b, .select_arm),
        })),
        try n(&b, .layout),
        try t(&b, .dedent),
    });
    try b.rule(.select_arm_block, select_arm_block);

    const select_head = try t(&b, .expr_select);

    const select_expr = try b.choice(&.{
        try b.sequence(&.{
            select_head,
            try n(&b, .select_arm),
            try b.optional(try n(&b, .select_arm_block)),
        }),
        try b.sequence(&.{
            select_head,
            try n(&b, .select_arm_block),
        }),
    });
    try b.rule(.select_expr, select_expr);

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

    const assign_op = try b.choice(&.{
        try t(&b, .assign),
        try t(&b, .plus_assign),
        try t(&b, .minus_assign),
        try t(&b, .asterisk_assign),
        try t(&b, .slash_assign),
        try t(&b, .percent_assign),
        try t(&b, .ampersand_assign),
        try t(&b, .bar_assign),
        try t(&b, .caret_assign),
        try t(&b, .shift_left_assign),
        try t(&b, .shift_right_assign),
    });
    try b.rule(.assign_op, assign_op);

    const assign = try b.sequence(&.{
        try n(&b, .pipe),
        try b.optional(try b.sequence(&.{
            try n(&b, .assign_op),
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
        try n(&b, .bitwise_or),
        try b.zero_or_more(try b.sequence(&.{
            try t(&b, .logical_and),
            try n(&b, .bitwise_or),
        })),
    });
    try b.rule(.logical_and, logical_and);

    const bitwise_or = try b.sequence(&.{
        try n(&b, .bitwise_xor),
        try b.zero_or_more(try b.sequence(&.{
            try t(&b, .bar),
            try n(&b, .bitwise_xor),
        })),
    });
    try b.rule(.bitwise_or, bitwise_or);

    const bitwise_xor = try b.sequence(&.{
        try n(&b, .bitwise_and),
        try b.zero_or_more(try b.sequence(&.{
            try t(&b, .caret),
            try n(&b, .bitwise_and),
        })),
    });
    try b.rule(.bitwise_xor, bitwise_xor);

    const bitwise_and = try b.sequence(&.{
        try n(&b, .comparison),
        try b.zero_or_more(try b.sequence(&.{
            try t(&b, .ampersand),
            try n(&b, .comparison),
        })),
    });
    try b.rule(.bitwise_and, bitwise_and);

    const comparison_guard = try b.not_pred(try b.choice(&.{
        try t(&b, .indent),
        try t(&b, .dedent),
        try t(&b, .new_line),
    }));
    const comparison_op = try b.sequence(&.{
        try b.choice(&.{
            try t(&b, .less_than),
            try t(&b, .greater_than),
            try t(&b, .less_or_equal),
            try t(&b, .greater_or_equal),
            try t(&b, .equal),
            try t(&b, .not_equal),
        }),
        comparison_guard,
    });
    const comparison = try b.sequence(&.{
        try n(&b, .shift),
        try b.zero_or_more(try b.sequence(&.{
            comparison_op,
            try n(&b, .shift),
        })),
    });
    try b.rule(.comparison, comparison);

    const shift = try b.sequence(&.{
        try n(&b, .sum),
        try b.zero_or_more(try b.sequence(&.{
            try b.choice(&.{
                try t(&b, .shift_left),
                try t(&b, .shift_right),
            }),
            try n(&b, .sum),
        })),
    });
    try b.rule(.shift, shift);

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
                try t(&b, .percent),
            }),
            try n(&b, .unary),
        })),
    });
    try b.rule(.product, product);

    const unary = try b.choice(&.{
        try b.sequence(&.{ try t(&b, .minus), try n(&b, .unary) }),
        try b.sequence(&.{ try t(&b, .bang), try n(&b, .unary) }),
        try b.sequence(&.{ try t(&b, .logical_not), try n(&b, .unary) }),
        try b.sequence(&.{ try t(&b, .tilde), try n(&b, .unary) }),
        try b.sequence(&.{ try t(&b, .asterisk), try n(&b, .unary) }),
        try b.sequence(&.{ try t(&b, .ampersand), try b.optional(try t(&b, .mut)), try n(&b, .unary) }),
        try b.sequence(&.{ try t(&b, .@"comptime"), try n(&b, .unary) }),
        try b.sequence(&.{ try t(&b, .box), try n(&b, .unary) }),
        try b.sequence(&.{ try t(&b, .sleep), try n(&b, .unary) }),
        try b.sequence(&.{ try t(&b, .timeout), try n(&b, .unary) }),
        try b.sequence(&.{ try t(&b, .deadline), try n(&b, .unary) }),
        try b.sequence(&.{ try t(&b, .spawn), try n(&b, .unary) }),
        try b.sequence(&.{ try t(&b, .await), try n(&b, .unary) }),
        try b.sequence(&.{ try t(&b, .@"try"), try n(&b, .unary) }),
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
        access_name,
    });
    try b.rule(.access_suffix, access_suffix);

    const cast_suffix = try b.sequence(&.{
        try t(&b, .as),
        try n(&b, .type_expr),
    });
    try b.rule(.cast_suffix, cast_suffix);

    const index_suffix = try b.sequence(&.{
        try t(&b, .bracket_left),
        try n(&b, .expr),
        try t(&b, .bracket_right),
    });
    try b.rule(.index_suffix, index_suffix);

    const macro_suffix = try b.sequence(&.{
        try t(&b, .bang),
        try n(&b, .macro_block),
    });
    try b.rule(.macro_suffix, macro_suffix);

    const postfix = try b.sequence(&.{
        try n(&b, .primary),
        try b.zero_or_more(try b.choice(&.{
            try n(&b, .generic_args),
            try n(&b, .call_suffix),
            try n(&b, .access_suffix),
            try n(&b, .cast_suffix),
            try n(&b, .index_suffix),
            try n(&b, .record_block),
            try t(&b, .question),
        })),
        try b.optional(try n(&b, .macro_suffix)),
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

    const duration_item = try b.sequence(&.{
        try t(&b, .number),
        try t(&b, .identifier),
    });
    try b.rule(.duration_item, duration_item);

    const duration_literal = try b.sequence(&.{
        try n(&b, .duration_item),
        try b.zero_or_more(try n(&b, .duration_item)),
    });
    try b.rule(.duration_literal, duration_literal);

    const intrinsic_call = try b.sequence(&.{
        try t(&b, .at_sign),
        name_token,
        try t(&b, .paren_left),
        try b.optional(try n(&b, .arg_list)),
        try t(&b, .paren_right),
    });
    try b.rule(.intrinsic_call, intrinsic_call);

    const primary = try b.choice(&.{
        try n(&b, .intrinsic_call),
        try n(&b, .atomic_expr),
        try n(&b, .duration_literal),
        try t(&b, .number),
        try t(&b, .string),
        try t(&b, .logical_true),
        try t(&b, .logical_false),
        try t(&b, .this),
        name_token,
        try t(&b, .label),
        try n(&b, .block),
        try b.sequence(&.{
            try t(&b, .paren_left),
            try n(&b, .expr),
            try t(&b, .paren_right),
        }),
    });
    try b.rule(.primary, primary);

    const type_arg_guard = try b.and_pred(try b.choice(&.{
        try t(&b, .comma),
        try t(&b, .greater_than),
    }));
    const type_args = try b.sequence(&.{
        try t(&b, .less_than),
        try n(&b, .expr),
        type_arg_guard,
        try b.zero_or_more(try b.sequence(&.{
            try t(&b, .comma),
            try n(&b, .expr),
            type_arg_guard,
        })),
        try t(&b, .greater_than),
    });
    try b.rule(.type_args, type_args);

    const type_atom = try b.choice(&.{
        try t(&b, .type),
        try t(&b, .ref),
        try t(&b, .box),
        try t(&b, .atomic),
        name_token,
    });
    try b.rule(.type_atom, type_atom);

    const type_tuple = try b.sequence(&.{
        try t(&b, .paren_left),
        try n(&b, .type_expr),
        try t(&b, .comma),
        try n(&b, .type_expr),
        try b.zero_or_more(try b.sequence(&.{
            try t(&b, .comma),
            try n(&b, .type_expr),
        })),
        try t(&b, .paren_right),
    });
    try b.rule(.type_tuple, type_tuple);

    const type_group = try b.sequence(&.{
        try t(&b, .paren_left),
        try n(&b, .type_expr),
        try t(&b, .paren_right),
    });
    try b.rule(.type_group, type_group);

    const type_primary = try b.choice(&.{
        try n(&b, .type_tuple),
        try t(&b, .self),
        try n(&b, .type_atom),
        try n(&b, .type_group),
    });
    try b.rule(.type_primary, type_primary);

    const type_postfix = try b.sequence(&.{
        try n(&b, .type_primary),
        try b.optional(try n(&b, .type_args)),
    });
    try b.rule(.type_postfix, type_postfix);

    const type_array = try b.sequence(&.{
        try t(&b, .bracket_left),
        try b.optional(try n(&b, .expr)),
        try t(&b, .bracket_right),
        try n(&b, .type_prefix),
    });
    try b.rule(.type_array, type_array);

    const type_ref = try b.sequence(&.{
        try t(&b, .ampersand),
        try b.optional(try t(&b, .label)),
        try b.optional(try t(&b, .mut)),
        try b.optional(try t(&b, .label)),
        try n(&b, .type_prefix),
    });
    try b.rule(.type_ref, type_ref);

    const type_dyn = try b.sequence(&.{
        try t(&b, .dyn),
        try n(&b, .type_intersect),
    });
    try b.rule(.type_dyn, type_dyn);

    const type_prefix = try b.choice(&.{
        try b.sequence(&.{ try t(&b, .bang), try n(&b, .type_prefix) }),
        try b.sequence(&.{ try t(&b, .question), try n(&b, .type_prefix) }),
        try n(&b, .type_ref),
        try n(&b, .type_dyn),
        try n(&b, .type_array),
        try n(&b, .type_postfix),
    });
    try b.rule(.type_prefix, type_prefix);

    const type_intersect = try b.sequence(&.{
        try n(&b, .type_prefix),
        try b.zero_or_more(try b.sequence(&.{
            try t(&b, .ampersand),
            try n(&b, .type_prefix),
        })),
    });
    try b.rule(.type_intersect, type_intersect);

    const type_union = try b.sequence(&.{
        try n(&b, .type_intersect),
        try b.zero_or_more(try b.sequence(&.{
            try t(&b, .bar),
            try n(&b, .type_intersect),
        })),
    });
    try b.rule(.type_union, type_union);

    const type_arrow = try b.sequence(&.{
        try n(&b, .type_union),
        try b.optional(try b.sequence(&.{
            try t(&b, .arrow),
            try n(&b, .type_arrow),
        })),
    });
    try b.rule(.type_arrow, type_arrow);

    const type_expr = try n(&b, .type_arrow);
    try b.rule(.type_expr, type_expr);

    const generic_param = try b.sequence(&.{
        try b.choice(&.{
            name_token,
            try t(&b, .label),
        }),
        try b.optional(try b.sequence(&.{
            try t(&b, .colon),
            try n(&b, .type_expr),
            try b.optional(try t(&b, .ellipsis)),
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
            try b.choice(&.{
                try t(&b, .this),
                try t(&b, .self),
            }),
            try b.optional(try b.sequence(&.{
                try t(&b, .colon),
                try n(&b, .type_expr),
            })),
        }),
        try b.sequence(&.{
            name_token,
            try t(&b, .colon),
            try n(&b, .type_expr),
            try b.optional(try t(&b, .ellipsis)),
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
        try b.choice(&.{
            name_token,
            try t(&b, .label),
        }),
        try t(&b, .colon),
        try n(&b, .type_expr),
        try b.zero_or_more(try b.sequence(&.{
            try t(&b, .comma),
            try b.choice(&.{
                name_token,
                try t(&b, .label),
            }),
            try t(&b, .colon),
            try n(&b, .type_expr),
        })),
    });
    try b.rule(.where_clause, where_clause);

    const function_decl = try b.sequence(&.{
        try b.optional(try n(&b, .attribute_list)),
        try b.optional(try t(&b, .@"comptime")),
        try t(&b, .function),
        access_name,
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
        try b.optional(try n(&b, .attribute_list)),
        try t(&b, .type),
        name_token,
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

    const trait_body_item = try b.choice(&.{
        try n(&b, .requires_clause),
        try n(&b, .trait_item),
    });
    try b.rule(.trait_body_item, trait_body_item);

    const trait_body = try b.sequence(&.{
        try n(&b, .layout),
        try t(&b, .indent),
        try b.zero_or_more(try b.sequence(&.{
            try n(&b, .layout),
            try n(&b, .trait_body_item),
        })),
        try n(&b, .layout),
        try t(&b, .dedent),
    });
    try b.rule(.trait_body, trait_body);

    const trait_decl = try b.sequence(&.{
        try b.optional(try n(&b, .attribute_list)),
        try b.optional(try t(&b, .auto)),
        try t(&b, .trait),
        name_token,
        try b.optional(try n(&b, .generic_params)),
        try b.optional(try n(&b, .trait_body)),
    });
    try b.rule(.trait_decl, trait_decl);

    const requires_clause = try b.sequence(&.{
        try t(&b, .requires),
        try n(&b, .type_expr),
        try b.zero_or_more(try b.sequence(&.{
            try t(&b, .comma),
            try n(&b, .type_expr),
        })),
    });
    try b.rule(.requires_clause, requires_clause);

    const sum_variant = try b.sequence(&.{
        name_token,
        try b.optional(try b.sequence(&.{
            try t(&b, .paren_left),
            try b.optional(try n(&b, .type_expr)),
            try t(&b, .paren_right),
        })),
    });
    try b.rule(.sum_variant, sum_variant);

    const enum_body = try b.sequence(&.{
        try n(&b, .layout),
        try t(&b, .indent),
        try b.zero_or_more(try b.sequence(&.{
            try n(&b, .layout),
            try n(&b, .sum_variant),
        })),
        try n(&b, .layout),
        try t(&b, .dedent),
    });
    try b.rule(.enum_body, enum_body);

    const enum_decl = try b.sequence(&.{
        try b.optional(try n(&b, .attribute_list)),
        try t(&b, .@"enum"),
        name_token,
        try b.optional(try n(&b, .generic_params)),
        try b.optional(try n(&b, .enum_body)),
    });
    try b.rule(.enum_decl, enum_decl);

    const struct_field = try b.sequence(&.{
        name_token,
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
        try b.optional(try n(&b, .attribute_list)),
        try t(&b, .@"struct"),
        name_token,
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
        try b.optional(try n(&b, .attribute_list)),
        try t(&b, .impl),
        try b.optional(try t(&b, .bang)),
        name_token,
        try t(&b, .@"for"),
        name_token,
        try b.optional(try n(&b, .impl_body)),
    });
    try b.rule(.impl_decl, impl_decl);

    const import_decl = try b.sequence(&.{
        try b.optional(try n(&b, .attribute_list)),
        try t(&b, .import),
        try n(&b, .qualified_name),
        try b.optional(try b.sequence(&.{
            try t(&b, .as),
            name_token,
        })),
        try b.optional(try b.sequence(&.{
            try t(&b, .from),
            name_token,
        })),
    });
    try b.rule(.import_decl, import_decl);

    const const_decl = try b.sequence(&.{
        try b.optional(try n(&b, .attribute_list)),
        try t(&b, .constant),
        name_token,
        try b.optional(try b.sequence(&.{
            try t(&b, .colon),
            try n(&b, .type_expr),
        })),
        try t(&b, .assign),
        try n(&b, .expr),
    });
    try b.rule(.const_decl, const_decl);

    const var_decl = try b.sequence(&.{
        try b.optional(try n(&b, .attribute_list)),
        try t(&b, .mut),
        name_token,
        try b.optional(try b.sequence(&.{
            try t(&b, .colon),
            try n(&b, .type_expr),
        })),
        try t(&b, .assign),
        try n(&b, .expr),
    });
    try b.rule(.var_decl, var_decl);

    const type_decl = try b.sequence(&.{
        try b.optional(try n(&b, .attribute_list)),
        try t(&b, .type),
        name_token,
        try b.optional(try n(&b, .generic_params)),
        try t(&b, .assign),
        try n(&b, .type_expr),
    });
    try b.rule(.type_decl, type_decl);

    const decl = try b.choice(&.{
        try n(&b, .function_decl),
        try n(&b, .struct_decl),
        try n(&b, .trait_decl),
        try n(&b, .enum_decl),
        try n(&b, .impl_decl),
        try n(&b, .import_decl),
        try n(&b, .const_decl),
        try n(&b, .var_decl),
        try n(&b, .type_decl),
    });
    try b.rule(.decl, decl);

    const expr_rule = try b.choice(&.{
        try n(&b, .label_expr),
        try n(&b, .if_expr),
        try n(&b, .match_expr),
        try n(&b, .select_expr),
        try n(&b, .with_expr),
        try n(&b, .loop_expr),
        try n(&b, .while_in_expr),
        try n(&b, .while_expr),
        try n(&b, .until_expr),
        try n(&b, .repeat_expr),
        try n(&b, .for_expr),
        try n(&b, .each_expr),
        try n(&b, .break_expr),
        try n(&b, .continue_expr),
        try n(&b, .yield_expr),
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
