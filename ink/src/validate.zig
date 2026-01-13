const std = @import("std");
const ink = @import("root.zig");
const diag_mod = @import("diagnostic.zig");
const source = @import("source.zig");

const mem_allocator = std.mem.Allocator;
const diagnostic = diag_mod.diagnostic;
const node_ref = ink.ast.node_ref;
const uir_build = ink.uir_build;
const uir_core = ink.uir;
const mir_core = ink.mir;
const mir_lower = ink.mir_lower;

pub fn validate_macro_ast(
    allocator: mem_allocator,
    nodes: []const *ink.node,
    registry: []const *ink.node,
    diags: *std.array_list.Managed(diagnostic),
    source_id: ?source.source_id,
    span: ?source.span,
) !bool {
    return validate_ast_refs(
        allocator,
        nodes,
        registry,
        diags,
        source_id,
        span,
        "macro output contains invalid node reference",
        "macro output contains null node reference",
        "macro output contains node outside registry",
    );
}

pub fn validate_ast(
    allocator: mem_allocator,
    nodes: []const *ink.node,
    diags: *std.array_list.Managed(diagnostic),
    source_id: ?source.source_id,
    span: ?source.span,
) !bool {
    return validate_ast_refs(
        allocator,
        nodes,
        null,
        diags,
        source_id,
        span,
        "invalid node reference in AST",
        "null node reference in AST",
        "node outside AST registry",
    );
}

pub fn validate_ast_with_registry(
    allocator: mem_allocator,
    nodes: []const *ink.node,
    registry: []const *ink.node,
    diags: *std.array_list.Managed(diagnostic),
    source_id: ?source.source_id,
    span: ?source.span,
) !bool {
    return validate_ast_refs(
        allocator,
        nodes,
        registry,
        diags,
        source_id,
        span,
        "invalid node reference in AST",
        "null node reference in AST",
        "node outside AST registry",
    );
}

pub fn validate_node_refs_with_set(
    value: anytype,
    registry: *const std.AutoHashMap(usize, void),
    diags: *std.array_list.Managed(diagnostic),
    source_id: ?source.source_id,
    span: ?source.span,
    invalid_msg: []const u8,
    null_msg: []const u8,
) bool {
    return validate_any(value, registry, diags, source_id, span, invalid_msg, null_msg);
}

fn validate_ast_refs(
    allocator: mem_allocator,
    nodes: []const *ink.node,
    registry: ?[]const *ink.node,
    diags: *std.array_list.Managed(diagnostic),
    source_id: ?source.source_id,
    span: ?source.span,
    invalid_msg: []const u8,
    null_msg: []const u8,
    missing_msg: []const u8,
) !bool {
    var registry_set: ?std.AutoHashMap(usize, void) = null;
    if (registry) |items| {
        var set = std.AutoHashMap(usize, void).init(allocator);
        errdefer set.deinit();
        for (items) |node| {
            const addr = @intFromPtr(node);
            _ = try set.put(addr, {});
        }
        registry_set = set;
    }
    defer if (registry_set) |*set| set.deinit();

    const registry_ptr: ?*const std.AutoHashMap(usize, void) = if (registry_set) |*set| set else null;

    for (nodes) |node| {
        const node_span = span_for_node_shallow(node) orelse span;
        if (registry_set) |*set| {
            if (!set.contains(@intFromPtr(node))) {
                try diags.append(.{
                    .danger = .@"error",
                    .message = missing_msg,
                    .span = node_span,
                    .source_id = source_id,
                });
                return false;
            }
        }
        if (!validate_any(node.*, registry_ptr, diags, source_id, node_span, invalid_msg, null_msg)) {
            return false;
        }
    }
    return true;
}

fn span_from_location(loc: ink.location) source.span {
    return .{ .start = loc.start, .end = loc.end };
}

fn decl_where(decl: ink.ast.decl) ink.location {
    return switch (decl) {
        .function => |f| f.where,
        .@"struct" => |s| s.where,
        .trait => |t| t.where,
        .@"enum" => |e| e.where,
        .impl => |i| i.where,
        .import => |i| i.where,
        .type_alias => |t| t.where,
        .@"const" => |c| c.where,
        .@"var" => |v| v.where,
    };
}

fn span_for_node_shallow(node: *const ink.node) ?source.span {
    return switch (node.*) {
        .identifier => |id| span_from_location(id.where),
        .string => |str| span_from_location(str.where),
        .integer => |value| span_from_location(value.where),
        .float => |value| span_from_location(value.where),
        .duration => |value| span_from_location(value.where),
        .macro_call => |mc| span_from_location(mc.where),
        .decl => |decl| span_from_location(decl_where(decl)),
        else => null,
    };
}

fn contains_node_ref(comptime T: type) bool {
    if (T == node_ref) return true;
    switch (@typeInfo(T)) {
        .optional => |info| return contains_node_ref(info.child),
        .pointer => |ptr| {
            if (ptr.size == .slice) {
                return contains_node_ref(ptr.child);
            }
            return false;
        },
        .array => |info| return contains_node_ref(info.child),
        .@"struct" => |info| {
            inline for (info.fields) |field| {
                if (contains_node_ref(field.type)) return true;
            }
            return false;
        },
        .@"union" => |info| {
            if (info.tag_type == null) return false;
            inline for (info.fields) |field| {
                if (contains_node_ref(field.type)) return true;
            }
            return false;
        },
        else => return false,
    }
}

fn validate_any(
    value: anytype,
    registry: ?*const std.AutoHashMap(usize, void),
    diags: *std.array_list.Managed(diagnostic),
    source_id: ?source.source_id,
    span: ?source.span,
    invalid_msg: []const u8,
    null_msg: []const u8,
) bool {
    const T = @TypeOf(value);
    if (!contains_node_ref(T)) return true;
    if (T == node_ref) {
        return validate_node_ref(value, registry, diags, source_id, span, invalid_msg, null_msg);
    }
    switch (@typeInfo(T)) {
        .optional => |_| {
            if (value) |payload| {
                return validate_any(payload, registry, diags, source_id, span, invalid_msg, null_msg);
            }
            return true;
        },
        .pointer => |ptr| {
            if (ptr.size == .slice) {
                if (value.len > 0 and @intFromPtr(value.ptr) == 0) {
                    diags.append(.{
                        .danger = .@"error",
                        .message = null_msg,
                        .span = span,
                        .source_id = source_id,
                    }) catch {};
                    return false;
                }
                return true;
            }
            return true;
        },
        .@"struct" => |info| {
            inline for (info.fields) |field| {
                const field_value = @field(value, field.name);
                if (!validate_any(field_value, registry, diags, source_id, span, invalid_msg, null_msg)) return false;
            }
            return true;
        },
        .@"union" => |info| {
            if (info.tag_type == null) return true;
            switch (value) {
                inline else => |payload| {
                    return validate_any(payload, registry, diags, source_id, span, invalid_msg, null_msg);
                },
            }
        },
        .array => |info| {
            if (contains_node_ref(info.child)) {
                for (value) |item| {
                    if (!validate_any(item, registry, diags, source_id, span, invalid_msg, null_msg)) return false;
                }
            }
            return true;
        },
        else => return true,
    }
}

fn validate_node_ref(
    ref: node_ref,
    registry: ?*const std.AutoHashMap(usize, void),
    diags: *std.array_list.Managed(diagnostic),
    source_id: ?source.source_id,
    span: ?source.span,
    invalid_msg: []const u8,
    null_msg: []const u8,
) bool {
    const addr = @intFromPtr(ref);
    if (addr == 0) {
        diags.append(.{
            .danger = .@"error",
            .message = null_msg,
            .span = span,
            .source_id = source_id,
        }) catch {};
        return false;
    }
    if (registry) |set| {
        if (!set.contains(addr)) {
            diags.append(.{
                .danger = .@"error",
                .message = invalid_msg,
                .span = span,
                .source_id = source_id,
            }) catch {};
            return false;
        }
    }
    return true;
}

pub fn validate_uir(
    result: uir_build.build_result,
    diags: *std.array_list.Managed(diagnostic),
) !bool {
    if (result.nodes.len != result.spans.len) {
        try diags.append(.{ .danger = .@"error", .message = "uir validation error: spans length mismatch", .span = null });
        return false;
    }
    if (result.nodes.len != result.sources.len) {
        try diags.append(.{ .danger = .@"error", .message = "uir validation error: sources length mismatch", .span = null });
        return false;
    }
    for (result.roots) |root| {
        if (root.idx >= result.nodes.len) {
            try diags.append(.{ .danger = .@"error", .message = "uir validation error: root index out of bounds", .span = null });
            return false;
        }
    }
    for (result.nodes, 0..) |node, idx| {
        if (!validate_uir_node(result, node, idx, diags)) return false;
    }
    return true;
}

fn validate_uir_node(
    result: uir_build.build_result,
    node: uir_core.uir,
    idx: usize,
    diags: *std.array_list.Managed(diagnostic),
) bool {
    const nodes_len = result.nodes.len;
    const strings_len = result.strings.len;

    const report = struct {
        fn err(bag: *std.array_list.Managed(diagnostic), res: uir_build.build_result, node_idx: usize, msg: []const u8) bool {
            const span = if (node_idx < res.spans.len) res.spans[node_idx] else null;
            const source_id = if (node_idx < res.sources.len) res.sources[node_idx] else null;
            bag.append(.{
                .danger = .@"error",
                .message = msg,
                .span = span,
                .source_id = source_id,
            }) catch {};
            return false;
        }
    }.err;

    const ok_id = struct {
        fn one(id: uir_core.uir_identifier, len: usize) bool {
            return id.idx < len;
        }
        fn opt(id: ?uir_core.uir_identifier, len: usize) bool {
            return if (id) |val| val.idx < len else true;
        }
        fn many(ids: []const uir_core.uir_identifier, len: usize) bool {
            for (ids) |id| {
                if (id.idx >= len) return false;
            }
            return true;
        }
    };
    const ok_str = struct {
        fn one(id: uir_core.string_identifier, len: usize) bool {
            return id.idx < len;
        }
        fn opt(id: ?uir_core.string_identifier, len: usize) bool {
            return if (id) |val| val.idx < len else true;
        }
    };

    switch (node) {
        .string => |id| if (!ok_str.one(id, strings_len)) return report(diags, result, idx, "uir validation error: string index out of bounds"),
        .identifier => |id| if (!ok_str.one(id, strings_len)) return report(diags, result, idx, "uir validation error: identifier index out of bounds"),
        .none => {},
        .unary => |un| if (!ok_id.one(un.right, nodes_len)) return report(diags, result, idx, "uir validation error: unary operand out of bounds"),
        .binary => |bin| {
            if (!ok_id.one(bin.left, nodes_len)) return report(diags, result, idx, "uir validation error: binary left out of bounds");
            if (!ok_id.one(bin.right, nodes_len)) return report(diags, result, idx, "uir validation error: binary right out of bounds");
        },
        .block => |items| if (!ok_id.many(items, nodes_len)) return report(diags, result, idx, "uir validation error: block item out of bounds"),
        .intrinsic => |call| {
            if (!ok_str.one(call.name, strings_len)) return report(diags, result, idx, "uir validation error: intrinsic name out of bounds");
            if (!ok_id.many(call.args, nodes_len)) return report(diags, result, idx, "uir validation error: intrinsic arg out of bounds");
        },
        .if_expr => |ife| {
            if (!ok_id.one(ife.condition, nodes_len)) return report(diags, result, idx, "uir validation error: if condition out of bounds");
            if (!ok_id.one(ife.then_branch, nodes_len)) return report(diags, result, idx, "uir validation error: if then branch out of bounds");
            if (!ok_id.opt(ife.else_branch, nodes_len)) return report(diags, result, idx, "uir validation error: if else branch out of bounds");
        },
        .match_expr => |me| {
            if (!ok_id.one(me.target, nodes_len)) return report(diags, result, idx, "uir validation error: match target out of bounds");
            for (me.arms) |arm| {
                if (!ok_id.one(arm.pattern, nodes_len)) return report(diags, result, idx, "uir validation error: match pattern out of bounds");
                if (!ok_id.one(arm.body, nodes_len)) return report(diags, result, idx, "uir validation error: match body out of bounds");
            }
        },
        .select_expr => |se| {
            for (se.arms) |arm| {
                if (!ok_str.opt(arm.name, strings_len)) return report(diags, result, idx, "uir validation error: select name out of bounds");
                if (!ok_id.one(arm.task, nodes_len)) return report(diags, result, idx, "uir validation error: select task out of bounds");
                if (!ok_id.one(arm.body, nodes_len)) return report(diags, result, idx, "uir validation error: select body out of bounds");
            }
        },
        .label_expr => |le| {
            if (!ok_str.one(le.name, strings_len)) return report(diags, result, idx, "uir validation error: label name out of bounds");
            if (!ok_id.one(le.body, nodes_len)) return report(diags, result, idx, "uir validation error: label body out of bounds");
        },
        .loop_expr => |le| if (!ok_id.one(le.body, nodes_len)) return report(diags, result, idx, "uir validation error: loop body out of bounds"),
        .while_expr => |we| {
            if (!ok_id.one(we.condition, nodes_len)) return report(diags, result, idx, "uir validation error: while condition out of bounds");
            if (!ok_id.one(we.body, nodes_len)) return report(diags, result, idx, "uir validation error: while body out of bounds");
        },
        .while_in_expr => |we| {
            if (!ok_id.one(we.pattern, nodes_len)) return report(diags, result, idx, "uir validation error: while pattern out of bounds");
            if (!ok_id.one(we.iter, nodes_len)) return report(diags, result, idx, "uir validation error: while iter out of bounds");
            if (!ok_id.one(we.body, nodes_len)) return report(diags, result, idx, "uir validation error: while body out of bounds");
        },
        .until_expr => |ue| {
            if (!ok_id.one(ue.condition, nodes_len)) return report(diags, result, idx, "uir validation error: until condition out of bounds");
            if (!ok_id.one(ue.body, nodes_len)) return report(diags, result, idx, "uir validation error: until body out of bounds");
        },
        .repeat_expr => |re| {
            if (!ok_id.one(re.count, nodes_len)) return report(diags, result, idx, "uir validation error: repeat count out of bounds");
            if (!ok_id.one(re.body, nodes_len)) return report(diags, result, idx, "uir validation error: repeat body out of bounds");
        },
        .for_expr => |fe| {
            if (!ok_id.one(fe.pattern, nodes_len)) return report(diags, result, idx, "uir validation error: for pattern out of bounds");
            if (!ok_id.one(fe.iter, nodes_len)) return report(diags, result, idx, "uir validation error: for iter out of bounds");
            if (!ok_id.one(fe.body, nodes_len)) return report(diags, result, idx, "uir validation error: for body out of bounds");
        },
        .each_expr => |ee| {
            if (!ok_id.one(ee.pattern, nodes_len)) return report(diags, result, idx, "uir validation error: each pattern out of bounds");
            if (!ok_id.one(ee.iter, nodes_len)) return report(diags, result, idx, "uir validation error: each iter out of bounds");
            if (!ok_id.one(ee.body, nodes_len)) return report(diags, result, idx, "uir validation error: each body out of bounds");
        },
        .break_expr => |be| {
            if (!ok_str.opt(be.label, strings_len)) return report(diags, result, idx, "uir validation error: break label out of bounds");
            if (!ok_id.opt(be.value, nodes_len)) return report(diags, result, idx, "uir validation error: break value out of bounds");
        },
        .continue_expr => |ce| if (!ok_str.opt(ce.label, strings_len)) return report(diags, result, idx, "uir validation error: continue label out of bounds"),
        .yield_expr => |ye| if (!ok_id.opt(ye.value, nodes_len)) return report(diags, result, idx, "uir validation error: yield value out of bounds"),
        .atomic_expr => |ae| {
            if (!ok_id.one(ae.value, nodes_len)) return report(diags, result, idx, "uir validation error: atomic value out of bounds");
            if (!ok_str.one(ae.ordering, strings_len)) return report(diags, result, idx, "uir validation error: atomic ordering out of bounds");
        },
        .associate => |assoc| {
            if (!ok_str.one(assoc.name, strings_len)) return report(diags, result, idx, "uir validation error: associate name out of bounds");
            if (!ok_id.opt(assoc.value, nodes_len)) return report(diags, result, idx, "uir validation error: associate value out of bounds");
        },
        .record_literal => |rec| {
            if (!ok_str.one(rec.type_name, strings_len)) return report(diags, result, idx, "uir validation error: record type out of bounds");
            for (rec.fields) |field| {
                if (!ok_str.one(field.name, strings_len)) return report(diags, result, idx, "uir validation error: record field name out of bounds");
                if (!ok_id.one(field.value, nodes_len)) return report(diags, result, idx, "uir validation error: record field value out of bounds");
            }
        },
        .type => |ty| switch (ty) {
            .self => {},
            .name => |id| if (!ok_str.one(id, strings_len)) return report(diags, result, idx, "uir validation error: type name out of bounds"),
            .optional => |id| if (!ok_id.one(id, nodes_len)) return report(diags, result, idx, "uir validation error: optional type out of bounds"),
            .dyn => |id| if (!ok_id.one(id, nodes_len)) return report(diags, result, idx, "uir validation error: dyn type out of bounds"),
            .applied => |ap| {
                if (!ok_str.one(ap.base, strings_len)) return report(diags, result, idx, "uir validation error: applied type base out of bounds");
                if (!ok_id.many(ap.args, nodes_len)) return report(diags, result, idx, "uir validation error: applied type arg out of bounds");
            },
        },
        .decl => |decl| switch (decl) {
            .@"struct" => |st| {
                if (!ok_str.one(st.name, strings_len)) return report(diags, result, idx, "uir validation error: struct name out of bounds");
                for (st.generics) |param| {
                    if (!ok_str.one(param.name, strings_len)) return report(diags, result, idx, "uir validation error: generic name out of bounds");
                    if (!ok_id.opt(param.constraint, nodes_len)) return report(diags, result, idx, "uir validation error: generic constraint out of bounds");
                    if (!ok_id.opt(param.default, nodes_len)) return report(diags, result, idx, "uir validation error: generic default out of bounds");
                }
                for (st.fields) |field| {
                    if (!ok_str.one(field.name, strings_len)) return report(diags, result, idx, "uir validation error: struct field name out of bounds");
                    if (!ok_id.one(field.ty, nodes_len)) return report(diags, result, idx, "uir validation error: struct field type out of bounds");
                }
            },
            .function => |func| {
                if (!ok_str.one(func.name, strings_len)) return report(diags, result, idx, "uir validation error: function name out of bounds");
                for (func.generics) |param| {
                    if (!ok_str.one(param.name, strings_len)) return report(diags, result, idx, "uir validation error: function generic name out of bounds");
                    if (!ok_id.opt(param.constraint, nodes_len)) return report(diags, result, idx, "uir validation error: function generic constraint out of bounds");
                    if (!ok_id.opt(param.default, nodes_len)) return report(diags, result, idx, "uir validation error: function generic default out of bounds");
                }
                for (func.params) |param| {
                    if (!ok_str.one(param.name, strings_len)) return report(diags, result, idx, "uir validation error: param name out of bounds");
                    if (!ok_id.one(param.ty, nodes_len)) return report(diags, result, idx, "uir validation error: param type out of bounds");
                }
                if (!ok_id.opt(func.return_type, nodes_len)) return report(diags, result, idx, "uir validation error: return type out of bounds");
                for (func.where_clause) |req| {
                    if (!ok_str.one(req.name, strings_len)) return report(diags, result, idx, "uir validation error: where name out of bounds");
                    if (!ok_id.one(req.constraint, nodes_len)) return report(diags, result, idx, "uir validation error: where constraint out of bounds");
                }
                if (!ok_id.opt(func.body, nodes_len)) return report(diags, result, idx, "uir validation error: function body out of bounds");
            },
            .trait => |tr| {
                if (!ok_str.one(tr.name, strings_len)) return report(diags, result, idx, "uir validation error: trait name out of bounds");
                for (tr.generics) |param| {
                    if (!ok_str.one(param.name, strings_len)) return report(diags, result, idx, "uir validation error: trait generic name out of bounds");
                    if (!ok_id.opt(param.constraint, nodes_len)) return report(diags, result, idx, "uir validation error: trait generic constraint out of bounds");
                    if (!ok_id.opt(param.default, nodes_len)) return report(diags, result, idx, "uir validation error: trait generic default out of bounds");
                }
                for (tr.items) |item| {
                    switch (item) {
                        .function => |func| {
                            if (!ok_str.one(func.name, strings_len)) return report(diags, result, idx, "uir validation error: trait fn name out of bounds");
                            for (func.params) |param| {
                                if (!ok_str.one(param.name, strings_len)) return report(diags, result, idx, "uir validation error: trait fn param name out of bounds");
                                if (!ok_id.one(param.ty, nodes_len)) return report(diags, result, idx, "uir validation error: trait fn param type out of bounds");
                            }
                            if (!ok_id.opt(func.return_type, nodes_len)) return report(diags, result, idx, "uir validation error: trait fn return type out of bounds");
                            for (func.where_clause) |req| {
                                if (!ok_str.one(req.name, strings_len)) return report(diags, result, idx, "uir validation error: trait fn where name out of bounds");
                                if (!ok_id.one(req.constraint, nodes_len)) return report(diags, result, idx, "uir validation error: trait fn where constraint out of bounds");
                            }
                            if (!ok_id.opt(func.body, nodes_len)) return report(diags, result, idx, "uir validation error: trait fn body out of bounds");
                        },
                        .assoc_type => |assoc| {
                            if (!ok_str.one(assoc.name, strings_len)) return report(diags, result, idx, "uir validation error: assoc type name out of bounds");
                            if (!ok_id.opt(assoc.value, nodes_len)) return report(diags, result, idx, "uir validation error: assoc type value out of bounds");
                        },
                    }
                }
                if (!ok_id.many(tr.requires, nodes_len)) return report(diags, result, idx, "uir validation error: trait requires out of bounds");
            },
            .@"enum" => |en| {
                if (!ok_str.one(en.name, strings_len)) return report(diags, result, idx, "uir validation error: enum name out of bounds");
                for (en.generics) |param| {
                    if (!ok_str.one(param.name, strings_len)) return report(diags, result, idx, "uir validation error: enum generic name out of bounds");
                    if (!ok_id.opt(param.constraint, nodes_len)) return report(diags, result, idx, "uir validation error: enum generic constraint out of bounds");
                    if (!ok_id.opt(param.default, nodes_len)) return report(diags, result, idx, "uir validation error: enum generic default out of bounds");
                }
                for (en.variants) |variant| {
                    if (!ok_str.one(variant.name, strings_len)) return report(diags, result, idx, "uir validation error: enum variant name out of bounds");
                    if (!ok_id.opt(variant.payload, nodes_len)) return report(diags, result, idx, "uir validation error: enum variant payload out of bounds");
                }
            },
            .impl => |impl_decl| {
                if (!ok_str.one(impl_decl.for_struct, strings_len)) return report(diags, result, idx, "uir validation error: impl target out of bounds");
                if (!ok_str.one(impl_decl.by_trait, strings_len)) return report(diags, result, idx, "uir validation error: impl trait out of bounds");
                for (impl_decl.functions) |func| {
                    if (!ok_str.one(func.name, strings_len)) return report(diags, result, idx, "uir validation error: impl fn name out of bounds");
                    for (func.params) |param| {
                        if (!ok_str.one(param.name, strings_len)) return report(diags, result, idx, "uir validation error: impl fn param name out of bounds");
                        if (!ok_id.one(param.ty, nodes_len)) return report(diags, result, idx, "uir validation error: impl fn param type out of bounds");
                    }
                    if (!ok_id.opt(func.return_type, nodes_len)) return report(diags, result, idx, "uir validation error: impl fn return type out of bounds");
                    for (func.where_clause) |req| {
                        if (!ok_str.one(req.name, strings_len)) return report(diags, result, idx, "uir validation error: impl fn where name out of bounds");
                        if (!ok_id.one(req.constraint, nodes_len)) return report(diags, result, idx, "uir validation error: impl fn where constraint out of bounds");
                    }
                    if (!ok_id.opt(func.body, nodes_len)) return report(diags, result, idx, "uir validation error: impl fn body out of bounds");
                }
            },
            .type_alias => |ty| {
                if (!ok_str.one(ty.name, strings_len)) return report(diags, result, idx, "uir validation error: type alias name out of bounds");
                for (ty.generics) |param| {
                    if (!ok_str.one(param.name, strings_len)) return report(diags, result, idx, "uir validation error: type alias generic name out of bounds");
                    if (!ok_id.opt(param.constraint, nodes_len)) return report(diags, result, idx, "uir validation error: type alias generic constraint out of bounds");
                    if (!ok_id.opt(param.default, nodes_len)) return report(diags, result, idx, "uir validation error: type alias generic default out of bounds");
                }
                if (!ok_id.one(ty.value, nodes_len)) return report(diags, result, idx, "uir validation error: type alias value out of bounds");
            },
            .@"const" => |con| {
                if (!ok_str.one(con.name, strings_len)) return report(diags, result, idx, "uir validation error: const name out of bounds");
                if (!ok_id.opt(con.ty, nodes_len)) return report(diags, result, idx, "uir validation error: const type out of bounds");
                if (!ok_id.one(con.value, nodes_len)) return report(diags, result, idx, "uir validation error: const value out of bounds");
            },
            .@"var" => |var_decl| {
                if (!ok_str.one(var_decl.name, strings_len)) return report(diags, result, idx, "uir validation error: var name out of bounds");
                if (!ok_id.opt(var_decl.ty, nodes_len)) return report(diags, result, idx, "uir validation error: var type out of bounds");
                if (!ok_id.one(var_decl.value, nodes_len)) return report(diags, result, idx, "uir validation error: var value out of bounds");
            },
        },
        else => {},
    }
    return true;
}

pub fn validate_mir(
    result: mir_lower.build_result,
    diags: *std.array_list.Managed(diagnostic),
) !bool {
    if (result.nodes.len != result.spans.len) {
        try diags.append(.{ .danger = .@"error", .message = "mir validation error: spans length mismatch", .span = null });
        return false;
    }
    if (result.nodes.len != result.sources.len) {
        try diags.append(.{ .danger = .@"error", .message = "mir validation error: sources length mismatch", .span = null });
        return false;
    }
    for (result.roots) |root| {
        if (root.idx >= result.nodes.len) {
            try diags.append(.{ .danger = .@"error", .message = "mir validation error: root index out of bounds", .span = null });
            return false;
        }
    }
    for (result.nodes, 0..) |node, idx| {
        if (!validate_mir_node(result, node, idx, diags)) return false;
    }
    return true;
}

fn validate_mir_node(
    result: mir_lower.build_result,
    node: mir_core.mir,
    idx: usize,
    diags: *std.array_list.Managed(diagnostic),
) bool {
    const nodes_len = result.nodes.len;
    const strings_len = result.strings.len;

    const report = struct {
        fn err(bag: *std.array_list.Managed(diagnostic), res: mir_lower.build_result, node_idx: usize, msg: []const u8) bool {
            const span = if (node_idx < res.spans.len) res.spans[node_idx] else null;
            const source_id = if (node_idx < res.sources.len) res.sources[node_idx] else null;
            bag.append(.{
                .danger = .@"error",
                .message = msg,
                .span = span,
                .source_id = source_id,
            }) catch {};
            return false;
        }
    }.err;

    const ok_id = struct {
        fn one(id: mir_core.mir_identifier, len: usize) bool {
            return id.idx < len;
        }
        fn opt(id: ?mir_core.mir_identifier, len: usize) bool {
            return if (id) |val| val.idx < len else true;
        }
        fn many(ids: []const mir_core.mir_identifier, len: usize) bool {
            for (ids) |id| {
                if (id.idx >= len) return false;
            }
            return true;
        }
    };
    const ok_str = struct {
        fn one(id: mir_core.string_identifier, len: usize) bool {
            return id.idx < len;
        }
        fn opt(id: ?mir_core.string_identifier, len: usize) bool {
            return if (id) |val| val.idx < len else true;
        }
    };

    switch (node) {
        .string => |id| if (!ok_str.one(id, strings_len)) return report(diags, result, idx, "mir validation error: string index out of bounds"),
        .identifier => |id| if (!ok_str.one(id, strings_len)) return report(diags, result, idx, "mir validation error: identifier index out of bounds"),
        .unary => |un| if (!ok_id.one(un.right, nodes_len)) return report(diags, result, idx, "mir validation error: unary operand out of bounds"),
        .binary => |bin| {
            if (!ok_id.one(bin.left, nodes_len)) return report(diags, result, idx, "mir validation error: binary left out of bounds");
            if (!ok_id.one(bin.right, nodes_len)) return report(diags, result, idx, "mir validation error: binary right out of bounds");
        },
        .block => |items| if (!ok_id.many(items, nodes_len)) return report(diags, result, idx, "mir validation error: block item out of bounds"),
        .intrinsic => |call| {
            if (!ok_str.one(call.name, strings_len)) return report(diags, result, idx, "mir validation error: intrinsic name out of bounds");
            if (!ok_id.many(call.args, nodes_len)) return report(diags, result, idx, "mir validation error: intrinsic arg out of bounds");
        },
        .if_expr => |ife| {
            if (!ok_id.one(ife.condition, nodes_len)) return report(diags, result, idx, "mir validation error: if condition out of bounds");
            if (!ok_id.one(ife.then_branch, nodes_len)) return report(diags, result, idx, "mir validation error: if then branch out of bounds");
            if (!ok_id.opt(ife.else_branch, nodes_len)) return report(diags, result, idx, "mir validation error: if else branch out of bounds");
        },
        .match_expr => |me| {
            if (!ok_id.one(me.target, nodes_len)) return report(diags, result, idx, "mir validation error: match target out of bounds");
            for (me.arms) |arm| {
                if (!ok_id.one(arm.pattern, nodes_len)) return report(diags, result, idx, "mir validation error: match pattern out of bounds");
                if (!ok_id.one(arm.body, nodes_len)) return report(diags, result, idx, "mir validation error: match body out of bounds");
            }
        },
        .select_expr => |se| {
            for (se.arms) |arm| {
                if (!ok_str.opt(arm.name, strings_len)) return report(diags, result, idx, "mir validation error: select name out of bounds");
                if (!ok_id.one(arm.task, nodes_len)) return report(diags, result, idx, "mir validation error: select task out of bounds");
                if (!ok_id.one(arm.body, nodes_len)) return report(diags, result, idx, "mir validation error: select body out of bounds");
            }
        },
        .label_expr => |le| {
            if (!ok_str.one(le.name, strings_len)) return report(diags, result, idx, "mir validation error: label name out of bounds");
            if (!ok_id.one(le.body, nodes_len)) return report(diags, result, idx, "mir validation error: label body out of bounds");
        },
        .loop_expr => |le| if (!ok_id.one(le.body, nodes_len)) return report(diags, result, idx, "mir validation error: loop body out of bounds"),
        .while_expr => |we| {
            if (!ok_id.one(we.condition, nodes_len)) return report(diags, result, idx, "mir validation error: while condition out of bounds");
            if (!ok_id.one(we.body, nodes_len)) return report(diags, result, idx, "mir validation error: while body out of bounds");
        },
        .while_in_expr => |we| {
            if (!ok_id.one(we.pattern, nodes_len)) return report(diags, result, idx, "mir validation error: while pattern out of bounds");
            if (!ok_id.one(we.iter, nodes_len)) return report(diags, result, idx, "mir validation error: while iter out of bounds");
            if (!ok_id.one(we.body, nodes_len)) return report(diags, result, idx, "mir validation error: while body out of bounds");
        },
        .until_expr => |ue| {
            if (!ok_id.one(ue.condition, nodes_len)) return report(diags, result, idx, "mir validation error: until condition out of bounds");
            if (!ok_id.one(ue.body, nodes_len)) return report(diags, result, idx, "mir validation error: until body out of bounds");
        },
        .repeat_expr => |re| {
            if (!ok_id.one(re.count, nodes_len)) return report(diags, result, idx, "mir validation error: repeat count out of bounds");
            if (!ok_id.one(re.body, nodes_len)) return report(diags, result, idx, "mir validation error: repeat body out of bounds");
        },
        .for_expr => |fe| {
            if (!ok_id.one(fe.pattern, nodes_len)) return report(diags, result, idx, "mir validation error: for pattern out of bounds");
            if (!ok_id.one(fe.iter, nodes_len)) return report(diags, result, idx, "mir validation error: for iter out of bounds");
            if (!ok_id.one(fe.body, nodes_len)) return report(diags, result, idx, "mir validation error: for body out of bounds");
        },
        .each_expr => |ee| {
            if (!ok_id.one(ee.pattern, nodes_len)) return report(diags, result, idx, "mir validation error: each pattern out of bounds");
            if (!ok_id.one(ee.iter, nodes_len)) return report(diags, result, idx, "mir validation error: each iter out of bounds");
            if (!ok_id.one(ee.body, nodes_len)) return report(diags, result, idx, "mir validation error: each body out of bounds");
        },
        .break_expr => |be| {
            if (!ok_str.opt(be.label, strings_len)) return report(diags, result, idx, "mir validation error: break label out of bounds");
            if (!ok_id.opt(be.value, nodes_len)) return report(diags, result, idx, "mir validation error: break value out of bounds");
        },
        .continue_expr => |ce| if (!ok_str.opt(ce.label, strings_len)) return report(diags, result, idx, "mir validation error: continue label out of bounds"),
        .yield_expr => |ye| if (!ok_id.opt(ye.value, nodes_len)) return report(diags, result, idx, "mir validation error: yield value out of bounds"),
        .atomic_expr => |ae| {
            if (!ok_id.one(ae.value, nodes_len)) return report(diags, result, idx, "mir validation error: atomic value out of bounds");
            if (!ok_str.one(ae.ordering, strings_len)) return report(diags, result, idx, "mir validation error: atomic ordering out of bounds");
        },
        .associate => |assoc| {
            if (!ok_str.one(assoc.name, strings_len)) return report(diags, result, idx, "mir validation error: associate name out of bounds");
            if (!ok_id.opt(assoc.value, nodes_len)) return report(diags, result, idx, "mir validation error: associate value out of bounds");
        },
        .record_literal => |rec| {
            if (!ok_str.one(rec.type_name, strings_len)) return report(diags, result, idx, "mir validation error: record type out of bounds");
            for (rec.fields) |field| {
                if (!ok_str.one(field.name, strings_len)) return report(diags, result, idx, "mir validation error: record field name out of bounds");
                if (!ok_id.one(field.value, nodes_len)) return report(diags, result, idx, "mir validation error: record field value out of bounds");
            }
        },
        .type => |ty| switch (ty) {
            .self => {},
            .name => |id| if (!ok_str.one(id, strings_len)) return report(diags, result, idx, "mir validation error: type name out of bounds"),
            .optional => |id| if (!ok_id.one(id, nodes_len)) return report(diags, result, idx, "mir validation error: optional type out of bounds"),
            .dyn => |id| if (!ok_id.one(id, nodes_len)) return report(diags, result, idx, "mir validation error: dyn type out of bounds"),
            .applied => |ap| {
                if (!ok_str.one(ap.base, strings_len)) return report(diags, result, idx, "mir validation error: applied type base out of bounds");
                if (!ok_id.many(ap.args, nodes_len)) return report(diags, result, idx, "mir validation error: applied type arg out of bounds");
            },
        },
        .decl => |decl| switch (decl) {
            .@"struct" => |st| {
                if (!ok_str.one(st.name, strings_len)) return report(diags, result, idx, "mir validation error: struct name out of bounds");
                for (st.generics) |param| {
                    if (!ok_str.one(param.name, strings_len)) return report(diags, result, idx, "mir validation error: generic name out of bounds");
                    if (!ok_id.opt(param.constraint, nodes_len)) return report(diags, result, idx, "mir validation error: generic constraint out of bounds");
                    if (!ok_id.opt(param.default, nodes_len)) return report(diags, result, idx, "mir validation error: generic default out of bounds");
                }
                for (st.fields) |field| {
                    if (!ok_str.one(field.name, strings_len)) return report(diags, result, idx, "mir validation error: struct field name out of bounds");
                    if (!ok_id.one(field.ty, nodes_len)) return report(diags, result, idx, "mir validation error: struct field type out of bounds");
                }
            },
            .function => |func| {
                if (!ok_str.one(func.name, strings_len)) return report(diags, result, idx, "mir validation error: function name out of bounds");
                for (func.generics) |param| {
                    if (!ok_str.one(param.name, strings_len)) return report(diags, result, idx, "mir validation error: function generic name out of bounds");
                    if (!ok_id.opt(param.constraint, nodes_len)) return report(diags, result, idx, "mir validation error: function generic constraint out of bounds");
                    if (!ok_id.opt(param.default, nodes_len)) return report(diags, result, idx, "mir validation error: function generic default out of bounds");
                }
                for (func.params) |param| {
                    if (!ok_str.one(param.name, strings_len)) return report(diags, result, idx, "mir validation error: param name out of bounds");
                    if (!ok_id.one(param.ty, nodes_len)) return report(diags, result, idx, "mir validation error: param type out of bounds");
                }
                if (!ok_id.opt(func.return_type, nodes_len)) return report(diags, result, idx, "mir validation error: return type out of bounds");
                for (func.where_clause) |req| {
                    if (!ok_str.one(req.name, strings_len)) return report(diags, result, idx, "mir validation error: where name out of bounds");
                    if (!ok_id.one(req.constraint, nodes_len)) return report(diags, result, idx, "mir validation error: where constraint out of bounds");
                }
                if (!ok_id.opt(func.body, nodes_len)) return report(diags, result, idx, "mir validation error: function body out of bounds");
            },
            .trait => |tr| {
                if (!ok_str.one(tr.name, strings_len)) return report(diags, result, idx, "mir validation error: trait name out of bounds");
                for (tr.generics) |param| {
                    if (!ok_str.one(param.name, strings_len)) return report(diags, result, idx, "mir validation error: trait generic name out of bounds");
                    if (!ok_id.opt(param.constraint, nodes_len)) return report(diags, result, idx, "mir validation error: trait generic constraint out of bounds");
                    if (!ok_id.opt(param.default, nodes_len)) return report(diags, result, idx, "mir validation error: trait generic default out of bounds");
                }
                for (tr.items) |item| {
                    switch (item) {
                        .function => |func| {
                            if (!ok_str.one(func.name, strings_len)) return report(diags, result, idx, "mir validation error: trait fn name out of bounds");
                            for (func.params) |param| {
                                if (!ok_str.one(param.name, strings_len)) return report(diags, result, idx, "mir validation error: trait fn param name out of bounds");
                                if (!ok_id.one(param.ty, nodes_len)) return report(diags, result, idx, "mir validation error: trait fn param type out of bounds");
                            }
                            if (!ok_id.opt(func.return_type, nodes_len)) return report(diags, result, idx, "mir validation error: trait fn return type out of bounds");
                            for (func.where_clause) |req| {
                                if (!ok_str.one(req.name, strings_len)) return report(diags, result, idx, "mir validation error: trait fn where name out of bounds");
                                if (!ok_id.one(req.constraint, nodes_len)) return report(diags, result, idx, "mir validation error: trait fn where constraint out of bounds");
                            }
                            if (!ok_id.opt(func.body, nodes_len)) return report(diags, result, idx, "mir validation error: trait fn body out of bounds");
                        },
                        .assoc_type => |assoc| {
                            if (!ok_str.one(assoc.name, strings_len)) return report(diags, result, idx, "mir validation error: assoc type name out of bounds");
                            if (!ok_id.opt(assoc.value, nodes_len)) return report(diags, result, idx, "mir validation error: assoc type value out of bounds");
                        },
                    }
                }
                if (!ok_id.many(tr.requires, nodes_len)) return report(diags, result, idx, "mir validation error: trait requires out of bounds");
            },
            .@"enum" => |en| {
                if (!ok_str.one(en.name, strings_len)) return report(diags, result, idx, "mir validation error: enum name out of bounds");
                for (en.generics) |param| {
                    if (!ok_str.one(param.name, strings_len)) return report(diags, result, idx, "mir validation error: enum generic name out of bounds");
                    if (!ok_id.opt(param.constraint, nodes_len)) return report(diags, result, idx, "mir validation error: enum generic constraint out of bounds");
                    if (!ok_id.opt(param.default, nodes_len)) return report(diags, result, idx, "mir validation error: enum generic default out of bounds");
                }
                for (en.variants) |variant| {
                    if (!ok_str.one(variant.name, strings_len)) return report(diags, result, idx, "mir validation error: enum variant name out of bounds");
                    if (!ok_id.opt(variant.payload, nodes_len)) return report(diags, result, idx, "mir validation error: enum variant payload out of bounds");
                }
            },
            .impl => |impl_decl| {
                if (!ok_str.one(impl_decl.for_struct, strings_len)) return report(diags, result, idx, "mir validation error: impl target out of bounds");
                if (!ok_str.one(impl_decl.by_trait, strings_len)) return report(diags, result, idx, "mir validation error: impl trait out of bounds");
                for (impl_decl.functions) |func| {
                    if (!ok_str.one(func.name, strings_len)) return report(diags, result, idx, "mir validation error: impl fn name out of bounds");
                    for (func.params) |param| {
                        if (!ok_str.one(param.name, strings_len)) return report(diags, result, idx, "mir validation error: impl fn param name out of bounds");
                        if (!ok_id.one(param.ty, nodes_len)) return report(diags, result, idx, "mir validation error: impl fn param type out of bounds");
                    }
                    if (!ok_id.opt(func.return_type, nodes_len)) return report(diags, result, idx, "mir validation error: impl fn return type out of bounds");
                    for (func.where_clause) |req| {
                        if (!ok_str.one(req.name, strings_len)) return report(diags, result, idx, "mir validation error: impl fn where name out of bounds");
                        if (!ok_id.one(req.constraint, nodes_len)) return report(diags, result, idx, "mir validation error: impl fn where constraint out of bounds");
                    }
                    if (!ok_id.opt(func.body, nodes_len)) return report(diags, result, idx, "mir validation error: impl fn body out of bounds");
                }
            },
            .type_alias => |ty| {
                if (!ok_str.one(ty.name, strings_len)) return report(diags, result, idx, "mir validation error: type alias name out of bounds");
                for (ty.generics) |param| {
                    if (!ok_str.one(param.name, strings_len)) return report(diags, result, idx, "mir validation error: type alias generic name out of bounds");
                    if (!ok_id.opt(param.constraint, nodes_len)) return report(diags, result, idx, "mir validation error: type alias generic constraint out of bounds");
                    if (!ok_id.opt(param.default, nodes_len)) return report(diags, result, idx, "mir validation error: type alias generic default out of bounds");
                }
                if (!ok_id.one(ty.value, nodes_len)) return report(diags, result, idx, "mir validation error: type alias value out of bounds");
            },
            .@"const" => |con| {
                if (!ok_str.one(con.name, strings_len)) return report(diags, result, idx, "mir validation error: const name out of bounds");
                if (!ok_id.opt(con.ty, nodes_len)) return report(diags, result, idx, "mir validation error: const type out of bounds");
                if (!ok_id.one(con.value, nodes_len)) return report(diags, result, idx, "mir validation error: const value out of bounds");
            },
            .@"var" => |var_decl| {
                if (!ok_str.one(var_decl.name, strings_len)) return report(diags, result, idx, "mir validation error: var name out of bounds");
                if (!ok_id.opt(var_decl.ty, nodes_len)) return report(diags, result, idx, "mir validation error: var type out of bounds");
                if (!ok_id.one(var_decl.value, nodes_len)) return report(diags, result, idx, "mir validation error: var value out of bounds");
            },
        },
        else => {},
    }
    return true;
}

pub fn validate_vm_program(
    allocator: mem_allocator,
    program: ink.lir_vm.program,
    foreigns_len: usize,
    diags: *std.array_list.Managed(diagnostic),
) !bool {
    var labels = std.AutoHashMap(ink.exe.label_id, void).init(allocator);
    defer labels.deinit();

    for (program.instructions) |inst| {
        if (inst == .label) {
            const id = inst.label.id;
            _ = try labels.put(id, {});
        }
    }

    const reg_limit = ink.vm.register_count;
    for (program.instructions, 0..) |inst, idx| {
        const report = struct {
            fn err(bag: *std.array_list.Managed(diagnostic), inst_idx: usize, msg: []const u8) bool {
                bag.append(.{
                    .danger = .@"error",
                    .message = msg,
                    .span = null,
                    .source_id = null,
                }) catch {};
                _ = inst_idx;
                return false;
            }
        }.err;

        const reg_ok = struct {
            fn one(reg: u8, limit: u8) bool {
                return reg < limit;
            }
        }.one;

        switch (inst) {
            .load_const => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (op.const_index >= program.constants.len) return report(diags, idx, "lir validation error: constant index out of bounds");
            },
            .move => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .argument_set => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .add => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .sub => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .mul => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .div => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .rem => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .min => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .max => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .bit_and => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .bit_or => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .bit_xor => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .bit_shl => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .bit_shr => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .bit_sar => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .bit_rol => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .bit_ror => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .bit_not => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .int_neg => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .int_abs => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .compare_eq => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .compare_ne => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .compare_lt => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .compare_le => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .compare_gt => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .compare_ge => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .fadd => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .fsub => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .fmul => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .fdiv => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .frem => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .fmin => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .fmax => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .fcompare_eq => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .fcompare_lt => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .fcompare_gt => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .fneg => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .fabs => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .fsqrt => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .fsin => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .fcos => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .ftan => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .fasin => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .facos => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .fatan => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .ffloor => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .fceil => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .fround => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .ftrunc => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid dst register");
                if (!reg_ok(op.src_a, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
                if (!reg_ok(op.src_b, reg_limit)) return report(diags, idx, "lir validation error: invalid src register");
            },
            .jump => |op| {
                if (!labels.contains(op.target)) return report(diags, idx, "lir validation error: jump target missing");
            },
            .jump_if_true => |op| {
                if (!reg_ok(op.condition, reg_limit)) return report(diags, idx, "lir validation error: invalid condition register");
                if (!labels.contains(op.target)) return report(diags, idx, "lir validation error: jump target missing");
            },
            .jump_if_false => |op| {
                if (!reg_ok(op.condition, reg_limit)) return report(diags, idx, "lir validation error: invalid condition register");
                if (!labels.contains(op.target)) return report(diags, idx, "lir validation error: jump target missing");
            },
            .call => |op| {
                if (!labels.contains(op.target)) return report(diags, idx, "lir validation error: call target missing");
            },
            .call_register => |op| {
                if (!reg_ok(op.src, reg_limit)) return report(diags, idx, "lir validation error: invalid call src register");
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid call dst register");
            },
            .call_foreign => |op| {
                if (op.index >= foreigns_len) return report(diags, idx, "lir validation error: foreign index out of bounds");
            },
            .task_spawn => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid task dst register");
                if (!labels.contains(op.target)) return report(diags, idx, "lir validation error: task target missing");
                _ = op.argc;
            },
            .task_await => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid await dst register");
                if (!reg_ok(op.src, reg_limit)) return report(diags, idx, "lir validation error: invalid await src register");
            },
            .task_await_any => |op| {
                if (!reg_ok(op.dst, reg_limit)) return report(diags, idx, "lir validation error: invalid await dst register");
                if (!reg_ok(op.src, reg_limit)) return report(diags, idx, "lir validation error: invalid await src register");
                _ = op.count;
            },
            .task_cancel => |op| {
                if (!reg_ok(op.src, reg_limit)) return report(diags, idx, "lir validation error: invalid cancel src register");
            },
            .ret_value => |op| {
                if (!reg_ok(op.src, reg_limit)) return report(diags, idx, "lir validation error: invalid ret src register");
            },
            else => {},
        }
    }

    return true;
}
