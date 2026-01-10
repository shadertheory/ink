const std = @import("std");
const core = @import("core.zig");
const uir_core = @import("../uir/core.zig");
const uir_build = @import("../uir/build.zig");
const source = @import("../source.zig");

pub const mir = core.mir;
pub const mir_identifier = core.mir_identifier;
pub const string_identifier = core.string_identifier;
pub const mem_allocator = std.mem.Allocator;

const uir_type_expr = @TypeOf(@as(uir_core.uir, undefined).type);
const mir_type_expr = @TypeOf(@as(mir, undefined).type);
const uir_decl = @TypeOf(@as(uir_core.uir, undefined).decl);
const mir_decl = @TypeOf(@as(mir, undefined).decl);

pub const lower_error = error{OutOfMemory};

pub const build_result = struct {
    nodes: []const mir,
    strings: []const []const u8,
    roots: []const mir_identifier,
    spans: []const ?source.span,
    sources: []const source.source_id,
};

pub fn lower(allocator: mem_allocator, uir_result: uir_build.build_result) lower_error!build_result {
    const nodes = try allocator.alloc(mir, uir_result.nodes.len);
    const spans = try allocator.alloc(?source.span, uir_result.spans.len);
    const sources = try allocator.alloc(source.source_id, uir_result.sources.len);
    const strings = try allocator.alloc([]const u8, uir_result.strings.len);
    const roots = try allocator.alloc(mir_identifier, uir_result.roots.len);

    std.mem.copyForwards(?source.span, spans, uir_result.spans);
    std.mem.copyForwards(source.source_id, sources, uir_result.sources);
    std.mem.copyForwards([]const u8, strings, uir_result.strings);

    for (uir_result.roots, 0..) |root, idx| {
        roots[idx] = convert_id(root);
    }

    for (uir_result.nodes, 0..) |node, idx| {
        nodes[idx] = try convert_node(allocator, node);
    }

    return .{
        .nodes = nodes,
        .strings = strings,
        .roots = roots,
        .spans = spans,
        .sources = sources,
    };
}

fn convert_id(id: uir_core.uir_identifier) mir_identifier {
    return .{ .idx = id.idx };
}

fn convert_opt_id(id: ?uir_core.uir_identifier) ?mir_identifier {
    return if (id) |value| convert_id(value) else null;
}

fn convert_string_id(id: uir_core.string_identifier) string_identifier {
    return .{ .idx = id.idx };
}

fn convert_opt_string_id(id: ?uir_core.string_identifier) ?string_identifier {
    return if (id) |value| convert_string_id(value) else null;
}

fn convert_ids(allocator: mem_allocator, ids: []const uir_core.uir_identifier) lower_error![]const mir_identifier {
    if (ids.len == 0) return &[_]mir_identifier{};
    const out = try allocator.alloc(mir_identifier, ids.len);
    for (ids, 0..) |id, idx| {
        out[idx] = convert_id(id);
    }
    return out;
}

fn convert_record_fields(
    allocator: mem_allocator,
    fields: []const uir_core.record_literal.record_field,
) lower_error![]const core.record_literal.record_field {
    if (fields.len == 0) return &[_]core.record_literal.record_field{};
    const out = try allocator.alloc(core.record_literal.record_field, fields.len);
    for (fields, 0..) |field, idx| {
        out[idx] = .{ .name = convert_string_id(field.name), .value = convert_id(field.value) };
    }
    return out;
}

fn convert_match_arms(
    allocator: mem_allocator,
    arms: []const uir_core.uir.match_arm,
) lower_error![]const mir.match_arm {
    if (arms.len == 0) return &[_]mir.match_arm{};
    const out = try allocator.alloc(mir.match_arm, arms.len);
    for (arms, 0..) |arm, idx| {
        out[idx] = .{ .pattern = convert_id(arm.pattern), .body = convert_id(arm.body) };
    }
    return out;
}

fn convert_select_arms(
    allocator: mem_allocator,
    arms: []const uir_core.uir.select_arm,
) lower_error![]const mir.select_arm {
    if (arms.len == 0) return &[_]mir.select_arm{};
    const out = try allocator.alloc(mir.select_arm, arms.len);
    for (arms, 0..) |arm, idx| {
        out[idx] = .{
            .name = convert_opt_string_id(arm.name),
            .task = convert_id(arm.task),
            .body = convert_id(arm.body),
            .detached = arm.detached,
        };
    }
    return out;
}

fn convert_intrinsic(allocator: mem_allocator, call: uir_core.intrinsic) lower_error!core.intrinsic {
    return .{ .name = convert_string_id(call.name), .args = try convert_ids(allocator, call.args) };
}

fn convert_record_literal(
    allocator: mem_allocator,
    rec: uir_core.record_literal,
) lower_error!core.record_literal {
    return .{ .type_name = convert_string_id(rec.type_name), .fields = try convert_record_fields(allocator, rec.fields) };
}

fn convert_type(allocator: mem_allocator, ty: uir_type_expr) lower_error!mir_type_expr {
    return switch (ty) {
        .self => .{ .self = {} },
        .name => |name| .{ .name = convert_string_id(name) },
        .optional => |inner| .{ .optional = convert_id(inner) },
        .dyn => |inner| .{ .dyn = convert_id(inner) },
        .applied => |ap| .{ .applied = .{ .base = convert_string_id(ap.base), .args = try convert_ids(allocator, ap.args) } },
    };
}

fn convert_generic_kind(kind: uir_core.uir.generic_param.generic_kind) mir.generic_param.generic_kind {
    return @enumFromInt(@intFromEnum(kind));
}

fn convert_generic_params(
    allocator: mem_allocator,
    params: []const uir_core.uir.generic_param,
) lower_error![]const mir.generic_param {
    if (params.len == 0) return &[_]mir.generic_param{};
    const out = try allocator.alloc(mir.generic_param, params.len);
    for (params, 0..) |param, idx| {
        out[idx] = .{
            .name = convert_string_id(param.name),
            .kind = convert_generic_kind(param.kind),
            .constraint = convert_opt_id(param.constraint),
            .default = convert_opt_id(param.default),
            .is_pack = param.is_pack,
        };
    }
    return out;
}

fn convert_function_params(
    allocator: mem_allocator,
    params: []const uir_core.uir.function_decl.param,
) lower_error![]const mir.function_decl.param {
    if (params.len == 0) return &[_]mir.function_decl.param{};
    const out = try allocator.alloc(mir.function_decl.param, params.len);
    for (params, 0..) |param, idx| {
        out[idx] = .{ .name = convert_string_id(param.name), .ty = convert_id(param.ty), .variadic = param.variadic };
    }
    return out;
}

fn convert_where_clause(
    allocator: mem_allocator,
    clauses: []const uir_core.uir.function_decl.where_req,
) lower_error![]const mir.function_decl.where_req {
    if (clauses.len == 0) return &[_]mir.function_decl.where_req{};
    const out = try allocator.alloc(mir.function_decl.where_req, clauses.len);
    for (clauses, 0..) |req, idx| {
        out[idx] = .{ .name = convert_string_id(req.name), .constraint = convert_id(req.constraint) };
    }
    return out;
}

fn convert_function_decl(
    allocator: mem_allocator,
    func: uir_core.uir.function_decl,
) lower_error!mir.function_decl {
    return .{
        .name = convert_string_id(func.name),
        .generics = try convert_generic_params(allocator, func.generics),
        .params = try convert_function_params(allocator, func.params),
        .return_type = convert_opt_id(func.return_type),
        .where_clause = try convert_where_clause(allocator, func.where_clause),
        .body = convert_opt_id(func.body),
        .span = func.span,
        .source_id = func.source_id,
    };
}

fn convert_function_list(
    allocator: mem_allocator,
    funcs: []const uir_core.uir.function_decl,
) lower_error![]const mir.function_decl {
    if (funcs.len == 0) return &[_]mir.function_decl{};
    const out = try allocator.alloc(mir.function_decl, funcs.len);
    for (funcs, 0..) |func, idx| {
        out[idx] = try convert_function_decl(allocator, func);
    }
    return out;
}

fn convert_struct_fields(
    allocator: mem_allocator,
    fields: []const uir_core.uir.struct_decl.field,
) lower_error![]const mir.struct_decl.field {
    if (fields.len == 0) return &[_]mir.struct_decl.field{};
    const out = try allocator.alloc(mir.struct_decl.field, fields.len);
    for (fields, 0..) |field, idx| {
        out[idx] = .{ .name = convert_string_id(field.name), .ty = convert_id(field.ty) };
    }
    return out;
}

fn convert_struct_decl(
    allocator: mem_allocator,
    decl: uir_core.uir.struct_decl,
) lower_error!mir.struct_decl {
    return .{
        .name = convert_string_id(decl.name),
        .generics = try convert_generic_params(allocator, decl.generics),
        .fields = try convert_struct_fields(allocator, decl.fields),
        .is_record = decl.is_record,
    };
}

fn convert_type_decl(
    allocator: mem_allocator,
    decl: uir_core.uir.type_decl,
) lower_error!mir.type_decl {
    return .{
        .name = convert_string_id(decl.name),
        .generics = try convert_generic_params(allocator, decl.generics),
        .value = convert_id(decl.value),
    };
}

fn convert_const_decl(
    allocator: mem_allocator,
    decl: uir_core.uir.const_decl,
) lower_error!mir.const_decl {
    _ = allocator;
    return .{ .name = convert_string_id(decl.name), .ty = convert_opt_id(decl.ty), .value = convert_id(decl.value) };
}

fn convert_var_decl(
    allocator: mem_allocator,
    decl: uir_core.uir.var_decl,
) lower_error!mir.var_decl {
    _ = allocator;
    return .{ .name = convert_string_id(decl.name), .ty = convert_opt_id(decl.ty), .value = convert_id(decl.value) };
}

fn convert_assoc_type_decl(
    decl: uir_core.uir.associated_type_decl,
) mir.associated_type_decl {
    return .{ .name = convert_string_id(decl.name), .value = convert_opt_id(decl.value) };
}

fn convert_trait_items(
    allocator: mem_allocator,
    items: []const uir_core.uir.trait_decl.trait_item,
) lower_error![]const mir.trait_decl.trait_item {
    if (items.len == 0) return &[_]mir.trait_decl.trait_item{};
    const out = try allocator.alloc(mir.trait_decl.trait_item, items.len);
    for (items, 0..) |item, idx| {
        out[idx] = switch (item) {
            .function => |func| .{ .function = try convert_function_decl(allocator, func) },
            .assoc_type => |assoc| .{ .assoc_type = convert_assoc_type_decl(assoc) },
        };
    }
    return out;
}

fn convert_trait_decl(
    allocator: mem_allocator,
    decl: uir_core.uir.trait_decl,
) lower_error!mir.trait_decl {
    return .{
        .is_auto = decl.is_auto,
        .name = convert_string_id(decl.name),
        .generics = try convert_generic_params(allocator, decl.generics),
        .items = try convert_trait_items(allocator, decl.items),
        .requires = try convert_ids(allocator, decl.requires),
    };
}

fn convert_enum_variants(
    allocator: mem_allocator,
    variants: []const uir_core.uir.enum_variant,
) lower_error![]const mir.enum_variant {
    if (variants.len == 0) return &[_]mir.enum_variant{};
    const out = try allocator.alloc(mir.enum_variant, variants.len);
    for (variants, 0..) |variant, idx| {
        out[idx] = .{ .name = convert_string_id(variant.name), .payload = convert_opt_id(variant.payload) };
    }
    return out;
}

fn convert_enum_decl(
    allocator: mem_allocator,
    decl: uir_core.uir.enum_decl,
) lower_error!mir.enum_decl {
    return .{
        .name = convert_string_id(decl.name),
        .generics = try convert_generic_params(allocator, decl.generics),
        .variants = try convert_enum_variants(allocator, decl.variants),
    };
}

fn convert_impl_decl(
    allocator: mem_allocator,
    decl: uir_core.uir.impl_decl,
) lower_error!mir.impl_decl {
    return .{
        .negative = decl.negative,
        .for_struct = convert_string_id(decl.for_struct),
        .by_trait = convert_string_id(decl.by_trait),
        .functions = try convert_function_list(allocator, decl.functions),
    };
}

fn convert_decl(allocator: mem_allocator, decl: uir_decl) lower_error!mir_decl {
    return switch (decl) {
        .function => |func| .{ .function = try convert_function_decl(allocator, func) },
        .@"struct" => |st| .{ .@"struct" = try convert_struct_decl(allocator, st) },
        .trait => |tr| .{ .trait = try convert_trait_decl(allocator, tr) },
        .@"enum" => |en| .{ .@"enum" = try convert_enum_decl(allocator, en) },
        .@"impl" => |im| .{ .@"impl" = try convert_impl_decl(allocator, im) },
        .type_alias => |ty| .{ .type_alias = try convert_type_decl(allocator, ty) },
        .@"const" => |c| .{ .@"const" = try convert_const_decl(allocator, c) },
        .@"var" => |v| .{ .@"var" = try convert_var_decl(allocator, v) },
    };
}

fn convert_node(allocator: mem_allocator, node: uir_core.uir) lower_error!mir {
    return switch (node) {
        .integer => |value| .{ .integer = value },
        .float => |value| .{ .float = value },
        .duration => |value| .{ .duration = value },
        .string => |value| .{ .string = convert_string_id(value) },
        .boolean => |value| .{ .boolean = value },
        .identifier => |value| .{ .identifier = convert_string_id(value) },
        .unary => |un| .{ .unary = .{ .op = un.op, .right = convert_id(un.right) } },
        .binary => |bin| .{ .binary = .{ .left = convert_id(bin.left), .op = bin.op, .right = convert_id(bin.right) } },
        .block => |items| .{ .block = try convert_ids(allocator, items) },
        .intrinsic => |call| .{ .intrinsic = try convert_intrinsic(allocator, call) },
        .if_expr => |ife| .{ .if_expr = .{
            .condition = convert_id(ife.condition),
            .then_branch = convert_id(ife.then_branch),
            .else_branch = convert_opt_id(ife.else_branch),
        } },
        .match_expr => |me| .{ .match_expr = .{ .target = convert_id(me.target), .arms = try convert_match_arms(allocator, me.arms) } },
        .select_expr => |se| .{ .select_expr = .{ .arms = try convert_select_arms(allocator, se.arms) } },
        .label_expr => |le| .{ .label_expr = .{ .name = convert_string_id(le.name), .body = convert_id(le.body) } },
        .loop_expr => |le| .{ .loop_expr = .{ .body = convert_id(le.body) } },
        .while_expr => |we| .{ .while_expr = .{ .condition = convert_id(we.condition), .body = convert_id(we.body) } },
        .while_in_expr => |we| .{ .while_in_expr = .{
            .pattern = convert_id(we.pattern),
            .iter = convert_id(we.iter),
            .body = convert_id(we.body),
        } },
        .until_expr => |ue| .{ .until_expr = .{ .condition = convert_id(ue.condition), .body = convert_id(ue.body) } },
        .repeat_expr => |re| .{ .repeat_expr = .{ .count = convert_id(re.count), .body = convert_id(re.body) } },
        .for_expr => |fe| .{ .for_expr = .{ .pattern = convert_id(fe.pattern), .iter = convert_id(fe.iter), .body = convert_id(fe.body) } },
        .each_expr => |ee| .{ .each_expr = .{ .pattern = convert_id(ee.pattern), .iter = convert_id(ee.iter), .body = convert_id(ee.body) } },
        .break_expr => |be| .{ .break_expr = .{ .label = convert_opt_string_id(be.label), .value = convert_opt_id(be.value) } },
        .continue_expr => |ce| .{ .continue_expr = .{ .label = convert_opt_string_id(ce.label) } },
        .yield_expr => |ye| .{ .yield_expr = .{ .value = convert_opt_id(ye.value) } },
        .atomic_expr => |ae| .{ .atomic_expr = .{ .value = convert_id(ae.value), .ordering = convert_string_id(ae.ordering) } },
        .associate => |assoc| .{ .associate = .{ .name = convert_string_id(assoc.name), .value = convert_opt_id(assoc.value) } },
        .record_literal => |rec| .{ .record_literal = try convert_record_literal(allocator, rec) },
        .type => |ty| .{ .type = try convert_type(allocator, ty) },
        .decl => |decl| .{ .decl = try convert_decl(allocator, decl) },
    };
}
