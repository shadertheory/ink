const std = @import("std");
const ink = @import("ink");
const spec = @import("./spec.zig");
const specification = spec.specification;
const node_union = spec.node_union;

pub const node_ref = *anyopaque;

fn type_spec_name(comptime item: spec.type_spec) []const u8 {
    return switch (item) {
        .@"struct" => |s| s.name,
        .@"union" => |u| u.name,
        .@"enum" => |e| e.name,
    };
}

fn spec_index_for_name(comptime name: []const u8) usize {
    inline for (specification, 0..) |item, i| {
        if (std.mem.eql(u8, type_spec_name(item), name)) {
            return i;
        }
    }
    @compileError("unknown type spec: " ++ name);
}

fn builtin_type(comptime b: spec.fundamental) type {
    return switch (b) {
        .i64 => i64,
        .f32 => f32,
        .unit => void,
        .identifier => ink.identifier,
        .unary => ink.unary,
        .binary => ink.binary,
    };
}

fn union_field_alignment(comptime field_type: type) comptime_int {
    return if (field_type == void) 1 else @alignOf(field_type);
}

fn resolve_named(comptime container: type, comptime name: []const u8) type {
    if (!@hasDecl(container, name)) {
        @compileError("unknown type reference: " ++ name);
    }
    return @field(container, name);
}

fn type_for_ref(comptime r: spec.type_ref, comptime container: type) type {
    return switch (r) {
        .builtin => |b| builtin_type(b),
        .named => |name| resolve_named(container, name),
        .ptr => |child| if (child.* == .named and std.mem.eql(u8, child.named, "node"))
            node_ref
        else
            *type_for_ref(child.*, container),
        .opt => |child| if (child.* == .named and std.mem.eql(u8, child.named, "node"))
            ?node_ref
        else
            ?type_for_ref(child.*, container),
        .slice => |slice| blk: {
            if (slice.child.* == .named and std.mem.eql(u8, slice.child.named, "node")) {
                break :blk if (slice.@"const") []const node_ref else []node_ref;
            }
            break :blk if (slice.@"const")
                []const type_for_ref(slice.child.*, container)
            else
                []type_for_ref(slice.child.*, container);
        },
    };
}

fn build_struct(comptime s: spec.struct_spec, comptime container: type) type {
    return comptime blk: {
        if (s.fields.len == 0) {
            break :blk @Type(.{
                .@"struct" = .{
                    .layout = .auto,
                    .fields = &.{},
                    .decls = &.{},
                    .is_tuple = false,
                },
            });
        }

        var fields: [s.fields.len]std.builtin.Type.StructField = undefined;
        for (s.fields, 0..) |field, i| {
            const field_type = type_for_ref(field.ty, container);
            fields[i] = .{
                .name = @ptrCast(field.name),
                .type = field_type,
                .default_value_ptr = null,
                .is_comptime = false,
                .alignment = @alignOf(field_type),
            };
        }

        break :blk @Type(.{
            .@"struct" = .{
                .layout = .auto,
                .fields = &fields,
                .decls = &.{},
                .is_tuple = false,
            },
        });
    };
}

fn build_union(comptime u: spec.union_spec, comptime container: type) type {
    return comptime blk: {
        if (u.fields.len == 0) {
            break :blk @Type(.{
                .@"union" = .{
                    .layout = .auto,
                    .tag_type = null,
                    .fields = &.{},
                    .decls = &.{},
                },
            });
        }

        var fields: [u.fields.len]std.builtin.Type.UnionField = undefined;
        var tag_fields: [u.fields.len]std.builtin.Type.EnumField = undefined;
        for (u.fields, 0..) |field, i| {
            const field_type = type_for_ref(field.ty, container);
            fields[i] = .{
                .name = @ptrCast(field.name),
                .type = field_type,
                .alignment = union_field_alignment(field_type),
            };
            tag_fields[i] = .{
                .name = @ptrCast(field.name),
                .value = i,
            };
        }

        const tag_type = @Type(.{
            .@"enum" = .{
                .tag_type = std.math.IntFittingRange(0, u.fields.len - 1),
                .fields = &tag_fields,
                .decls = &.{},
                .is_exhaustive = true,
            },
        });

        break :blk @Type(.{
            .@"union" = .{
                .layout = .auto,
                .tag_type = tag_type,
                .fields = &fields,
                .decls = &.{},
            },
        });
    };
}

fn build_enum(comptime e: spec.enum_spec) type {
    if (e.tags.len == 0) {
        return @Type(.{
            .@"enum" = .{
                .tag_type = u0,
                .fields = &.{},
                .decls = &.{},
                .is_exhaustive = true,
            },
        });
    }

    var fields: [e.tags.len]std.builtin.Type.EnumField = undefined;
    inline for (e.tags, 0..) |tag, i| {
        fields[i] = .{
            .name = @ptrCast(tag),
            .value = i,
        };
    }

    return @Type(.{
        .@"enum" = .{
            .tag_type = std.math.IntFittingRange(0, e.tags.len - 1),
            .fields = &fields,
            .decls = &.{},
            .is_exhaustive = true,
        },
    });
}

fn build_named(comptime container: type, comptime name: []const u8) type {
    const idx = spec_index_for_name(name);
    return switch (specification[idx]) {
        .@"struct" => |s| build_struct(s, container),
        .@"union" => |u| build_union(u, container),
        .@"enum" => |e| build_enum(e),
    };
}

const Generated = struct {
    const Self = @This();

    pub const node = build_union(node_union, Self);
    pub const generic_kind = build_named(Self, "generic_kind");
    pub const match_arm = build_named(Self, "match_arm");
    pub const param = build_named(Self, "param");
    pub const where_req = build_named(Self, "where_req");
    pub const function_decl = build_named(Self, "function_decl");
    pub const struct_field = build_named(Self, "struct_field");
    pub const struct_decl = build_named(Self, "struct_decl");
    pub const impl_decl = build_named(Self, "impl_decl");
    pub const const_decl = build_named(Self, "const_decl");
    pub const var_decl = build_named(Self, "var_decl");
    pub const trait_item = build_named(Self, "trait_item");
    pub const associated_type_decl = build_named(Self, "associated_type_decl");
    pub const trait_decl = build_named(Self, "trait_decl");
    pub const concept_decl = build_named(Self, "concept_decl");
    pub const sum_decl = build_named(Self, "sum_decl");
    pub const sum_variant = build_named(Self, "sum_variant");
    pub const enum_decl = build_named(Self, "enum_decl");
    pub const generic_param = build_named(Self, "generic_param");
    pub const unary_expr = build_named(Self, "unary_expr");
    pub const binary_expr = build_named(Self, "binary_expr");
    pub const if_expr = build_named(Self, "if_expr");
    pub const match_expr = build_named(Self, "match_expr");
    pub const block_expr = build_named(Self, "block_expr");
    pub const associate = build_named(Self, "associate");
    pub const type_applied = build_named(Self, "type_applied");
    pub const type_expr = build_named(Self, "type_expr");
    pub const decl = build_named(Self, "decl");
};

pub const node = Generated.node;
pub const generic_kind = Generated.generic_kind;
pub const match_arm = Generated.match_arm;
pub const param = Generated.param;
pub const where_req = Generated.where_req;
pub const function_decl = Generated.function_decl;
pub const struct_field = Generated.struct_field;
pub const struct_decl = Generated.struct_decl;
pub const impl_decl = Generated.impl_decl;
pub const const_decl = Generated.const_decl;
pub const var_decl = Generated.var_decl;
pub const trait_item = Generated.trait_item;
pub const associated_type_decl = Generated.associated_type_decl;
pub const trait_decl = Generated.trait_decl;
pub const concept_decl = Generated.concept_decl;
pub const sum_decl = Generated.sum_decl;
pub const sum_variant = Generated.sum_variant;
pub const enum_decl = Generated.enum_decl;
pub const generic_param = Generated.generic_param;
pub const unary_expr = Generated.unary_expr;
pub const binary_expr = Generated.binary_expr;
pub const if_expr = Generated.if_expr;
pub const match_expr = Generated.match_expr;
pub const block_expr = Generated.block_expr;
pub const associate = Generated.associate;
pub const type_applied = Generated.type_applied;
pub const type_expr = Generated.type_expr;
pub const decl = Generated.decl;

pub inline fn ref(ptr: *node) node_ref {
    return @ptrCast(ptr);
}

pub inline fn ref_opt(ptr: ?*node) ?node_ref {
    return if (ptr) |p| ref(p) else null;
}

pub inline fn ref_slice(ptrs: []const *node) []const node_ref {
    return @as([*]const node_ref, @ptrCast(ptrs.ptr))[0..ptrs.len];
}

pub inline fn deref(ptr: node_ref) *node {
    return @ptrCast(@alignCast(ptr));
}

pub inline fn deref_opt(ptr: ?node_ref) ?*node {
    return if (ptr) |p| deref(p) else null;
}
