const core = @import("core.zig");

pub const ir = core.ir;
pub const ir_identifier = core.ir_identifier;
pub const string_identifier = core.string_identifier;

pub fn print(
    writer: anytype,
    nodes: []const ir,
    strings: []const []const u8,
    roots: []const ir_identifier,
) !void {
    try writer.print("(roots", .{});
    for (roots) |root| {
        try writer.print(" {d}", .{root.idx});
    }
    try writer.print(")\n", .{});

    try writer.print("(strings\n", .{});
    for (strings, 0..) |value, idx| {
        try writer.print("  ({d} \"{s}\")\n", .{ idx, value });
    }
    try writer.print(")\n", .{});

    try writer.print("(nodes\n", .{});
    for (nodes, 0..) |node, idx| {
        try print_node(writer, idx, node, strings);
    }
    try writer.print(")\n", .{});
}

fn print_node(
    writer: anytype,
    idx: usize,
    node: ir,
    strings: []const []const u8,
) !void {
    switch (node) {
        .integer => |value| {
            try writer.print("  ({d} integer {d})\n", .{ idx, value });
        },
        .float => |value| {
            try writer.print("  ({d} float {d})\n", .{ idx, value });
        },
        .boolean => |value| {
            try writer.print("  ({d} boolean {s})\n", .{ idx, if (value) "true" else "false" });
        },
        .identifier => |id| {
            try writer.print("  ({d} identifier \"{s}\")\n", .{ idx, string_value(strings, id) });
        },
        .unary => |un| {
            try writer.print("  ({d} unary {s} {d})\n", .{ idx, @tagName(un.op), un.right.idx });
        },
        .binary => |bin| {
            try writer.print(
                "  ({d} binary {s} {d} {d})\n",
                .{ idx, @tagName(bin.op), bin.left.idx, bin.right.idx },
            );
        },
        .block => |items| {
            try writer.print("  ({d} block", .{idx});
            try print_id_list(writer, "items", items);
            try writer.print(")\n", .{});
        },
        .if_expr => |ife| {
            try writer.print("  ({d} if {d} {d} ", .{ idx, ife.condition.idx, ife.then_branch.idx });
            try print_id_opt(writer, ife.else_branch);
            try writer.print(")\n", .{});
        },
        .match_expr => |me| {
            try writer.print("  ({d} match {d}", .{ idx, me.target.idx });
            try print_match_arms(writer, me.arms);
            try writer.print(")\n", .{});
        },
        .associate => |assoc| {
            try writer.print("  ({d} associate \"{s}\" ", .{ idx, string_value(strings, assoc.name) });
            try print_id_opt(writer, assoc.value);
            try writer.print(")\n", .{});
        },
        .type => |ty| {
            try writer.print("  ({d} type", .{idx});
            switch (ty) {
                .self => try writer.print(" self", .{}),
                .name => |name| try writer.print(" name \"{s}\"", .{string_value(strings, name)}),
                .optional => |inner| try writer.print(" optional {d}", .{inner.idx}),
                .applied => |applied| {
                    try writer.print(" applied \"{s}\"", .{string_value(strings, applied.base)});
                    try print_id_list(writer, "args", applied.args);
                },
            }
            try writer.print(")\n", .{});
        },
        .decl => |decl| {
            try writer.print("  ({d} decl ", .{idx});
            switch (decl) {
                .function => |func| {
                    try print_function_decl(writer, func, strings);
                },
                .@"struct" => |st| {
                    try print_struct_decl(writer, st, strings);
                },
                .trait => |tr| {
                    try print_trait_decl(writer, tr, strings);
                },
                .concept => |concept| {
                    try print_concept_decl(writer, concept, strings);
                },
                .sum => |sum| {
                    try print_sum_decl(writer, sum, strings);
                },
                .@"enum" => |en| {
                    try print_enum_decl(writer, en, strings);
                },
                .@"impl" => |im| {
                    try print_impl_decl(writer, im, strings);
                },
            }
            try writer.print(")\n", .{});
        },
    }
}

fn print_function_decl(writer: anytype, func: ir.function_decl, strings: []const []const u8) !void {
    try writer.print("function (name \"{s}\")", .{string_value(strings, func.name)});
    try print_generic_params(writer, func.generics, strings);
    try print_params(writer, func.params, strings);
    try writer.print(" (return ", .{});
    try print_id_opt(writer, func.return_type);
    try writer.print(")", .{});
    try print_where_clause(writer, func.where_clause, strings);
    try writer.print(" (body ", .{});
    try print_id_opt(writer, func.body);
    try writer.print(")", .{});
}

fn print_struct_decl(writer: anytype, st: ir.struct_decl, strings: []const []const u8) !void {
    try writer.print("struct (name \"{s}\")", .{string_value(strings, st.name)});
    try print_generic_params(writer, st.generics, strings);
    try print_struct_fields(writer, st.fields, strings);
}

fn print_trait_decl(writer: anytype, tr: ir.trait_decl, strings: []const []const u8) !void {
    try writer.print("trait (name \"{s}\")", .{string_value(strings, tr.name)});
    try print_generic_params(writer, tr.generics, strings);
    try print_trait_items(writer, tr.items, strings);
}

fn print_concept_decl(writer: anytype, concept: ir.concept_decl, strings: []const []const u8) !void {
    try writer.print("concept (name \"{s}\")", .{string_value(strings, concept.name)});
    try print_generic_params(writer, concept.generics, strings);
    try print_id_list(writer, "requires", concept.requires);
}

fn print_sum_decl(writer: anytype, sum: ir.sum_decl, strings: []const []const u8) !void {
    try writer.print("sum (name \"{s}\")", .{string_value(strings, sum.name)});
    try print_generic_params(writer, sum.generics, strings);
    try print_sum_variants(writer, sum.variants, strings);
}

fn print_enum_decl(writer: anytype, en: ir.enum_decl, strings: []const []const u8) !void {
    try writer.print("enum (name \"{s}\")", .{string_value(strings, en.name)});
    try writer.print(" (cases", .{});
    for (en.cases) |case_id| {
        try writer.print(" \"{s}\"", .{string_value(strings, case_id)});
    }
    try writer.print(")", .{});
}

fn print_impl_decl(writer: anytype, im: ir.impl_decl, strings: []const []const u8) !void {
    try writer.print(
        "impl (trait \"{s}\") (for \"{s}\")",
        .{ string_value(strings, im.by_trait), string_value(strings, im.for_struct) },
    );
    try writer.print(" (functions", .{});
    for (im.functions) |func| {
        try writer.print(" (", .{});
        try print_function_decl(writer, func, strings);
        try writer.print(")", .{});
    }
    try writer.print(")", .{});
}

fn print_generic_params(writer: anytype, generics: []const ir.generic_param, strings: []const []const u8) !void {
    try writer.print(" (generics", .{});
    for (generics) |param| {
        try writer.print(" (param \"{s}\" {s} ", .{ string_value(strings, param.name), @tagName(param.kind) });
        try print_id_opt(writer, param.constraint);
        try writer.print(" ", .{});
        try print_id_opt(writer, param.default);
        try writer.print(")", .{});
    }
    try writer.print(")", .{});
}

fn print_params(writer: anytype, params: []const ir.function_decl.param, strings: []const []const u8) !void {
    try writer.print(" (params", .{});
    for (params) |param| {
        try writer.print(" (param \"{s}\" {d})", .{ string_value(strings, param.name), param.ty.idx });
    }
    try writer.print(")", .{});
}

fn print_where_clause(writer: anytype, clauses: []const ir.function_decl.where_req, strings: []const []const u8) !void {
    try writer.print(" (where", .{});
    for (clauses) |req| {
        try writer.print(" (req \"{s}\" {d})", .{ string_value(strings, req.name), req.constraint.idx });
    }
    try writer.print(")", .{});
}

fn print_struct_fields(writer: anytype, fields: []const ir.struct_decl.field, strings: []const []const u8) !void {
    try writer.print(" (fields", .{});
    for (fields) |field| {
        try writer.print(" (field \"{s}\" {d})", .{ string_value(strings, field.name), field.ty.idx });
    }
    try writer.print(")", .{});
}

fn print_trait_items(writer: anytype, items: []const ir.trait_decl.trait_item, strings: []const []const u8) !void {
    try writer.print(" (items", .{});
    for (items) |item| {
        switch (item) {
            .function => |func| {
                try writer.print(" (", .{});
                try print_function_decl(writer, func, strings);
                try writer.print(")", .{});
            },
            .assoc_type => |assoc| {
                try writer.print(" (assoc_type \"{s}\" ", .{string_value(strings, assoc.name)});
                try print_id_opt(writer, assoc.value);
                try writer.print(")", .{});
            },
        }
    }
    try writer.print(")", .{});
}

fn print_sum_variants(writer: anytype, variants: []const ir.sum_decl.sum_variant, strings: []const []const u8) !void {
    try writer.print(" (variants", .{});
    for (variants) |variant| {
        try writer.print(" (variant \"{s}\" ", .{string_value(strings, variant.name)});
        try print_id_opt(writer, variant.payload);
        try writer.print(")", .{});
    }
    try writer.print(")", .{});
}

fn print_match_arms(writer: anytype, arms: []const ir.match_arm) !void {
    try writer.print(" (arms", .{});
    for (arms) |arm| {
        try writer.print(" (arm {d} {d})", .{ arm.pattern.idx, arm.body.idx });
    }
    try writer.print(")", .{});
}

fn print_id_list(writer: anytype, label: []const u8, ids: []const ir_identifier) !void {
    try writer.print(" ({s}", .{label});
    for (ids) |id| {
        try writer.print(" {d}", .{id.idx});
    }
    try writer.print(")", .{});
}

fn print_id_opt(writer: anytype, id: ?ir_identifier) !void {
    if (id) |val| {
        try writer.print("{d}", .{val.idx});
    } else {
        try writer.print("nil", .{});
    }
}

fn string_value(strings: []const []const u8, id: string_identifier) []const u8 {
    if (id.idx < strings.len) return strings[id.idx];
    return "<missing>";
}
