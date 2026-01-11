const std = @import("std");
const ink = @import("root.zig");
const uir_mod = ink.uir;
const diag = @import("diagnostic.zig");
const type_key_mod = @import("type_key.zig");
const source = @import("source.zig");

const type_key = type_key_mod.type_key;
const type_key_eq = type_key_mod.type_key_eq;
const type_key_base_name = type_key_mod.type_key_base_name;

const array_list = std.array_list.Managed;
const string_map = std.hash_map.StringHashMap;
const diagnostic = diag.diagnostic;

pub const typecheck_error = error{
    OutOfMemory,
};

pub const typecheck_result = struct {
    types: []const type_key,
    owned_slices: []const []const type_key,

    pub fn deinit(self: *typecheck_result, allocator: std.mem.Allocator) void {
        for (self.owned_slices) |slice| {
            allocator.free(slice);
        }
        allocator.free(self.owned_slices);
        allocator.free(self.types);
    }
};

const TypeId = u32;
const uir_unary = @TypeOf((uir_mod.uir{ .unary = undefined }).unary);
const uir_binary = @TypeOf((uir_mod.uir{ .binary = undefined }).binary);
const uir_if = @TypeOf((uir_mod.uir{ .if_expr = undefined }).if_expr);
const uir_intrinsic = @TypeOf((uir_mod.uir{ .intrinsic = undefined }).intrinsic);
const uir_record_literal = @TypeOf((uir_mod.uir{ .record_literal = undefined }).record_literal);
const uir_decl = @TypeOf((uir_mod.uir{ .decl = undefined }).decl);

const Type = union(enum) {
    tvar: TypeVar,
    name: []const u8,
    dyn_trait: []const u8,
    applied: struct {
        base: []const u8,
        args: []const TypeId,
    },
    ref: struct {
        mutable: bool,
        inner: TypeId,
    },
};

const TypeVar = struct {
    binding: ?TypeId = null,
    constraints: std.ArrayListUnmanaged(trait_constraint) = .{},
};

const Change = union(enum) {
    bind: struct { var_id: TypeId, prev: ?TypeId },
    constraints_len: struct { var_id: TypeId, prev_len: usize },
};

const type_ctx = struct {
    allocator: std.mem.Allocator,
    types: array_list(Type),
    changes: array_list(Change),
    owned_type_slices: array_list([]const TypeId),

    fn init(allocator: std.mem.Allocator) type_ctx {
        return .{
            .allocator = allocator,
            .types = array_list(Type).init(allocator),
            .changes = array_list(Change).init(allocator),
            .owned_type_slices = array_list([]const TypeId).init(allocator),
        };
    }

    fn deinit(self: *type_ctx) void {
        for (self.types.items) |*item| {
            if (item.* == .tvar) {
                item.tvar.constraints.deinit(self.allocator);
            }
        }
        for (self.owned_type_slices.items) |slice| {
            self.allocator.free(slice);
        }
        self.owned_type_slices.deinit();
        self.changes.deinit();
        self.types.deinit();
    }

    fn new_var(self: *type_ctx) TypeId {
        const id: TypeId = @intCast(self.types.items.len);
        self.types.append(.{ .tvar = .{} }) catch unreachable;
        return id;
    }

    fn named(self: *type_ctx, name: []const u8) TypeId {
        const id: TypeId = @intCast(self.types.items.len);
        self.types.append(.{ .name = name }) catch unreachable;
        return id;
    }

    fn dyn_trait(self: *type_ctx, name: []const u8) TypeId {
        const id: TypeId = @intCast(self.types.items.len);
        self.types.append(.{ .dyn_trait = name }) catch unreachable;
        return id;
    }

    fn applied(self: *type_ctx, base: []const u8, args: []const TypeId) TypeId {
        const owned = self.allocator.alloc(TypeId, args.len) catch unreachable;
        std.mem.copyForwards(TypeId, owned, args);
        self.owned_type_slices.append(owned) catch unreachable;
        const id: TypeId = @intCast(self.types.items.len);
        self.types.append(.{ .applied = .{ .base = base, .args = owned } }) catch unreachable;
        return id;
    }

    fn ref_type(self: *type_ctx, inner: TypeId, mutable: bool) TypeId {
        const id: TypeId = @intCast(self.types.items.len);
        self.types.append(.{ .ref = .{ .mutable = mutable, .inner = inner } }) catch unreachable;
        return id;
    }

    fn checkpoint(self: *type_ctx) usize {
        return self.changes.items.len;
    }

    fn rollback(self: *type_ctx, checkpoint_idx: usize) void {
        var idx = self.changes.items.len;
        while (idx > checkpoint_idx) {
            idx -= 1;
            const change = self.changes.items[idx];
            switch (change) {
                .bind => |b| {
                    self.types.items[b.var_id].tvar.binding = b.prev;
                },
                .constraints_len => |c| {
                    self.types.items[c.var_id].tvar.constraints.shrinkRetainingCapacity(c.prev_len);
                },
            }
        }
        self.changes.items.len = checkpoint_idx;
    }

    fn resolve(self: *type_ctx, id: TypeId) TypeId {
        const node = self.types.items[id];
        switch (node) {
            .tvar => |v| if (v.binding) |bound| return self.resolve(bound),
            else => {},
        }
        return id;
    }

    fn occurs(self: *type_ctx, needle: TypeId, haystack: TypeId) bool {
        const resolved = self.resolve(haystack);
        if (resolved == needle) return true;
        const node = self.types.items[resolved];
        return switch (node) {
            .tvar => false,
            .name => false,
            .dyn_trait => false,
            .ref => |r| self.occurs(needle, r.inner),
            .applied => |ap| blk: {
                for (ap.args) |arg| {
                    if (self.occurs(needle, arg)) break :blk true;
                }
                break :blk false;
            },
        };
    }

    fn add_constraint(self: *type_ctx, id: TypeId, constraint: trait_constraint) void {
        const resolved = self.resolve(id);
        if (self.types.items[resolved] != .tvar) return;
        const var_ptr = &self.types.items[resolved].tvar;
        for (var_ptr.constraints.items) |existing| {
            if (existing.negative == constraint.negative and std.mem.eql(u8, existing.name, constraint.name)) return;
        }
        const prev_len = var_ptr.constraints.items.len;
        self.changes.append(.{ .constraints_len = .{ .var_id = resolved, .prev_len = prev_len } }) catch unreachable;
        var_ptr.constraints.append(self.allocator, constraint) catch unreachable;
    }

    fn bind_var(self: *type_ctx, id: TypeId, to: TypeId) void {
        const resolved = self.resolve(id);
        if (resolved == to) return;
        const prev = self.types.items[resolved].tvar.binding;
        self.changes.append(.{ .bind = .{ .var_id = resolved, .prev = prev } }) catch unreachable;
        self.types.items[resolved].tvar.binding = to;
    }

    fn unify(self: *type_ctx, a_id: TypeId, b_id: TypeId) bool {
        const a = self.resolve(a_id);
        const b = self.resolve(b_id);
        if (a == b) return true;

        const a_node = self.types.items[a];
        const b_node = self.types.items[b];
        if (a_node == .tvar) {
            if (self.occurs(a, b)) return false;
            self.bind_var(a, b);
            return true;
        }
        if (b_node == .tvar) {
            if (self.occurs(b, a)) return false;
            self.bind_var(b, a);
            return true;
        }

        switch (a_node) {
            .name => |name_a| return b_node == .name and std.mem.eql(u8, name_a, b_node.name),
            .dyn_trait => |name_a| return b_node == .dyn_trait and std.mem.eql(u8, name_a, b_node.dyn_trait),
            .ref => |ref_a| {
                if (b_node != .ref) return false;
                if (ref_a.mutable != b_node.ref.mutable) return false;
                return self.unify(ref_a.inner, b_node.ref.inner);
            },
            .applied => |ap_a| {
                if (b_node != .applied) return false;
                const ap_b = b_node.applied;
                if (!std.mem.eql(u8, ap_a.base, ap_b.base)) return false;
                if (ap_a.args.len != ap_b.args.len) return false;
                for (ap_a.args, 0..) |arg, idx| {
                    if (!self.unify(arg, ap_b.args[idx])) return false;
                }
                return true;
            },
            else => return false,
        }
    }
};

const function_info = struct {
    decl: uir_mod.uir.function_decl,
    impl_for: ?[]const u8,
};

const trait_method = struct {
    name: []const u8,
    return_type: ?uir_mod.uir_identifier,
};

const trait_constraint = struct {
    name: []const u8,
    negative: bool,
};

const constraint_failure = struct {
    generic_name: ?[]const u8,
    constraint: trait_constraint,
    ty: type_key,
};

const arg_mismatch = struct {
    index: usize,
    expected: type_key,
    actual: type_key,
};

const call_failure = union(enum) {
    arity: struct {
        expected: usize,
        found: usize,
    },
    arg_mismatch: arg_mismatch,
    constraint: constraint_failure,
};

const trait_info = struct {
    methods: []const trait_method,
    requires: []const trait_constraint,
    is_auto: bool,
    generics: []const uir_mod.uir.generic_param,
};

const struct_info = struct {
    fields: []const uir_mod.uir.struct_decl.field,
    is_record: bool,
    generics: []const uir_mod.uir.generic_param,
};

const loop_scope = struct {
    label: ?[]const u8,
    result: TypeId,
    allow_continue: bool,
};

const typecheck_ctx = struct {
    allocator: std.mem.Allocator,
    nodes: []const uir_mod.uir,
    strings: []const []const u8,
    spans: []const ?source.span,
    node_sources: []const source.source_id,
    diags: *array_list(diagnostic),
    types: type_ctx,
    node_types: []TypeId,
    functions: string_map(array_list(function_info)),
    traits: string_map(trait_info),
    trait_impls: string_map(array_list([]const u8)),
    trait_neg_impls: string_map(array_list([]const u8)),
    structs: string_map(struct_info),
    owned_type_key_slices: array_list([]const type_key),
    loop_stack: std.ArrayListUnmanaged(loop_scope),
    pending_label: ?[]const u8,

    fn init(
        allocator: std.mem.Allocator,
        nodes: []const uir_mod.uir,
        strings: []const []const u8,
        spans: []const ?source.span,
        node_sources: []const source.source_id,
        diags: *array_list(diagnostic),
    ) typecheck_ctx {
        const node_types = allocator.alloc(TypeId, nodes.len) catch unreachable;
        @memset(node_types, std.math.maxInt(TypeId));
        return .{
            .allocator = allocator,
            .nodes = nodes,
            .strings = strings,
            .spans = spans,
            .node_sources = node_sources,
            .diags = diags,
            .types = type_ctx.init(allocator),
            .node_types = node_types,
            .functions = string_map(array_list(function_info)).init(allocator),
            .traits = string_map(trait_info).init(allocator),
            .trait_impls = string_map(array_list([]const u8)).init(allocator),
            .trait_neg_impls = string_map(array_list([]const u8)).init(allocator),
            .structs = string_map(struct_info).init(allocator),
            .owned_type_key_slices = array_list([]const type_key).init(allocator),
            .loop_stack = .{},
            .pending_label = null,
        };
    }

    fn deinit(self: *typecheck_ctx) void {
        self.types.deinit();
        self.allocator.free(self.node_types);
        var it = self.functions.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.*.deinit();
        }
        self.functions.deinit();
        var trait_it = self.traits.iterator();
        while (trait_it.next()) |entry| {
            self.allocator.free(entry.value_ptr.methods);
            self.allocator.free(entry.value_ptr.requires);
        }
        self.traits.deinit();
        var impl_it = self.trait_impls.iterator();
        while (impl_it.next()) |entry| {
            entry.value_ptr.*.deinit();
        }
        self.trait_impls.deinit();
        var neg_impl_it = self.trait_neg_impls.iterator();
        while (neg_impl_it.next()) |entry| {
            entry.value_ptr.*.deinit();
        }
        self.trait_neg_impls.deinit();
        self.structs.deinit();
        for (self.owned_type_key_slices.items) |slice| {
            self.allocator.free(slice);
        }
        self.owned_type_key_slices.deinit();
        self.loop_stack.deinit(self.allocator);
    }

    fn string_value(self: *typecheck_ctx, id: uir_mod.string_identifier) []const u8 {
        return self.strings[id.idx];
    }

    fn span_for_node(self: *typecheck_ctx, id: uir_mod.uir_identifier) ?source.span {
        const idx: usize = @intCast(id.idx);
        if (idx >= self.spans.len) return null;
        return self.spans[idx];
    }

    fn source_for_node(self: *typecheck_ctx, id: uir_mod.uir_identifier) ?source.source_id {
        const idx: usize = @intCast(id.idx);
        if (idx >= self.node_sources.len) return null;
        return self.node_sources[idx];
    }

    fn push_loop(self: *typecheck_ctx, label: ?[]const u8, result: TypeId, allow_continue: bool) void {
        self.loop_stack.append(self.allocator, .{
            .label = label,
            .result = result,
            .allow_continue = allow_continue,
        }) catch {};
    }

    fn pop_loop(self: *typecheck_ctx) void {
        if (self.loop_stack.items.len == 0) return;
        self.loop_stack.items.len -= 1;
    }

    fn find_break_scope(self: *typecheck_ctx, label: ?[]const u8) ?*loop_scope {
        var idx = self.loop_stack.items.len;
        while (idx > 0) {
            idx -= 1;
            const scope = &self.loop_stack.items[idx];
            if (label) |lab| {
                if (scope.label != null and std.mem.eql(u8, scope.label.?, lab)) return scope;
            } else if (scope.allow_continue) {
                return scope;
            }
        }
        return null;
    }

    fn find_continue_scope(self: *typecheck_ctx, label: ?[]const u8) ?*loop_scope {
        var idx = self.loop_stack.items.len;
        while (idx > 0) {
            idx -= 1;
            const scope = &self.loop_stack.items[idx];
            if (!scope.allow_continue) continue;
            if (label) |lab| {
                if (scope.label != null and std.mem.eql(u8, scope.label.?, lab)) return scope;
            } else {
                return scope;
            }
        }
        return null;
    }

    fn to_type_key(self: *typecheck_ctx, id: TypeId) type_key {
        return type_key_from_type_id(self, id);
    }

    fn infer_expr(
        self: *typecheck_ctx,
        id: uir_mod.uir_identifier,
        self_name: ?[]const u8,
        generics: *string_map(TypeId),
        locals: *string_map(TypeId),
        return_type: ?TypeId,
    ) TypeId {
        const idx: usize = @intCast(id.idx);
        if (self.node_types[idx] != std.math.maxInt(TypeId)) {
            return self.node_types[idx];
        }
        const ty = infer_node(self, id, self_name, generics, locals, return_type);
        self.node_types[idx] = ty;
        return ty;
    }

    fn type_name_from_type_node(self: *typecheck_ctx, id: uir_mod.uir_identifier) ?[]const u8 {
        const node = self.nodes[@intCast(id.idx)];
        return switch (node) {
            .type => |ty| switch (ty) {
                .name => |name_id| self.string_value(name_id),
                .applied => |ap| self.string_value(ap.base),
                else => null,
            },
            .identifier => |ident| self.string_value(ident),
            else => null,
        };
    }

    fn type_name_from_type_id(self: *typecheck_ctx, id: TypeId) ?[]const u8 {
        const resolved = self.types.resolve(id);
        return switch (self.types.types.items[resolved]) {
            .name => |name| name,
            .dyn_trait => |name| name,
            .applied => |ap| ap.base,
            else => null,
        };
    }

    fn constraint_from_type_node(self: *typecheck_ctx, id: uir_mod.uir_identifier) ?trait_constraint {
        const node = self.nodes[@intCast(id.idx)];
        if (node == .type and node.type == .applied) {
            const ap = node.type.applied;
            if (std.mem.eql(u8, self.string_value(ap.base), "not") and ap.args.len >= 1) {
                const inner_name = self.type_name_from_type_node(ap.args[0]) orelse return null;
                return .{ .name = inner_name, .negative = true };
            }
        }
        const name = self.type_name_from_type_node(id) orelse return null;
        return .{ .name = name, .negative = false };
    }

    fn is_unsized_marker(self: *typecheck_ctx, id: uir_mod.uir_identifier) bool {
        const node = self.nodes[@intCast(id.idx)];
        if (node != .type) return false;
        if (node.type != .optional) return false;
        const inner_name = self.type_name_from_type_node(node.type.optional) orelse return false;
        return std.mem.eql(u8, inner_name, "sized");
    }

    fn add_type_generic_constraints(
        self: *typecheck_ctx,
        generics: []const uir_mod.uir.generic_param,
        where_clause: []const uir_mod.uir.function_decl.where_req,
        local_generics: *string_map(TypeId),
    ) void {
        var unsized_allowed = string_map(void).init(self.allocator);
        defer unsized_allowed.deinit();

        for (generics) |gen| {
            if (gen.kind != .type) continue;
            const name = self.string_value(gen.name);
            const var_id = self.types.new_var();
            local_generics.put(name, var_id) catch return;
            if (gen.constraint) |constraint_id| {
                if (self.is_unsized_marker(constraint_id)) {
                    unsized_allowed.put(name, {}) catch {};
                    continue;
                }
                if (self.constraint_from_type_node(constraint_id)) |constraint| {
                    if (constraint.negative and std.mem.eql(u8, constraint.name, "sized")) {
                        unsized_allowed.put(name, {}) catch {};
                    }
                    self.types.add_constraint(var_id, constraint);
                }
            }
        }

        for (where_clause) |req| {
            const name = self.string_value(req.name);
            if (local_generics.get(name)) |var_id| {
                if (self.is_unsized_marker(req.constraint)) {
                    unsized_allowed.put(name, {}) catch {};
                    continue;
                }
                if (self.constraint_from_type_node(req.constraint)) |constraint| {
                    if (constraint.negative and std.mem.eql(u8, constraint.name, "sized")) {
                        unsized_allowed.put(name, {}) catch {};
                    }
                    self.types.add_constraint(var_id, constraint);
                }
            }
        }

        for (generics) |gen| {
            if (gen.kind != .type) continue;
            const name = self.string_value(gen.name);
            if (unsized_allowed.contains(name)) continue;
            if (local_generics.get(name)) |var_id| {
                self.types.add_constraint(var_id, .{ .name = "sized", .negative = false });
            }
        }
    }

    fn append_dyn_trait_name(
        self: *typecheck_ctx,
        name: []const u8,
        base: *?[]const u8,
        extra: *array_list([]const u8),
    ) void {
        _ = self;
        if (base.* == null) {
            base.* = name;
            return;
        }
        for (extra.items) |item| {
            if (std.mem.eql(u8, item, name)) return;
        }
        extra.append(name) catch {};
    }

    fn collect_dyn_traits(
        self: *typecheck_ctx,
        id: uir_mod.uir_identifier,
        base: *?[]const u8,
        pos: *array_list([]const u8),
        neg: *array_list([]const u8),
    ) void {
        const node = self.nodes[@intCast(id.idx)];
        switch (node) {
            .type => |ty| switch (ty) {
                .name => |name_id| {
                    self.append_dyn_trait_name(self.string_value(name_id), base, pos);
                },
                .applied => |ap| {
                    const base_name = self.string_value(ap.base);
                    if (std.mem.eql(u8, base_name, "intersect")) {
                        for (ap.args) |arg| {
                            self.collect_dyn_traits(arg, base, pos, neg);
                        }
                        return;
                    }
                    if (std.mem.eql(u8, base_name, "not") and ap.args.len >= 1) {
                        if (self.type_name_from_type_node(ap.args[0])) |inner_name| {
                            for (neg.items) |item| {
                                if (std.mem.eql(u8, item, inner_name)) return;
                            }
                            neg.append(inner_name) catch {};
                        }
                        return;
                    }
                    self.append_dyn_trait_name(base_name, base, pos);
                },
                else => {},
            },
            .identifier => |ident| self.append_dyn_trait_name(self.string_value(ident), base, pos),
            else => {},
        }
    }

    fn type_from_type_node(
        self: *typecheck_ctx,
        id: uir_mod.uir_identifier,
        self_name: ?[]const u8,
        generics: *string_map(TypeId),
    ) TypeId {
        const node = self.nodes[@intCast(id.idx)];
        return switch (node) {
            .type => |ty| switch (ty) {
                .self => if (self_name) |name| self.types.named(name) else self.types.named("self"),
                .name => |name_id| blk: {
                    const name = self.string_value(name_id);
                    if (generics.get(name)) |var_id| break :blk var_id;
                    break :blk self.types.named(name);
                },
                .dyn => |ref| blk: {
                    var pos = array_list([]const u8).init(self.allocator);
                    defer pos.deinit();
                    var neg = array_list([]const u8).init(self.allocator);
                    defer neg.deinit();
                    var base: ?[]const u8 = null;
                    self.collect_dyn_traits(ref, &base, &pos, &neg);
                    const base_name = base orelse break :blk self.types.new_var();
                    if (pos.items.len == 0 and neg.items.len == 0) {
                        break :blk self.types.dyn_trait(base_name);
                    }
                    const arg_count = 1 + pos.items.len + neg.items.len;
                    var args = self.allocator.alloc(TypeId, arg_count) catch break :blk self.types.new_var();
                    defer self.allocator.free(args);
                    args[0] = self.types.named(base_name);
                    var idx: usize = 1;
                    for (pos.items) |name| {
                        args[idx] = self.types.named(name);
                        idx += 1;
                    }
                    for (neg.items) |name| {
                        const inner = self.types.named(name);
                        args[idx] = self.types.applied("not", &.{inner});
                        idx += 1;
                    }
                    break :blk self.types.applied("dyn", args);
                },
                .optional => |ref| blk: {
                    const inner = self.type_from_type_node(ref, self_name, generics);
                    break :blk self.types.applied("optional", &.{inner});
                },
                .applied => |ap| blk: {
                    var args = self.allocator.alloc(TypeId, ap.args.len) catch return self.types.new_var();
                    defer self.allocator.free(args);
                    for (ap.args, 0..) |arg, idx| {
                        args[idx] = self.type_from_type_node(arg, self_name, generics);
                    }
                    break :blk self.types.applied(self.string_value(ap.base), args);
                },
            },
            .identifier => |ident| blk: {
                const name = self.string_value(ident);
                if (generics.get(name)) |var_id| break :blk var_id;
                break :blk self.types.named(name);
            },
            else => self.types.new_var(),
        };
    }

    fn add_function(self: *typecheck_ctx, func: uir_mod.uir.function_decl, impl_for: ?[]const u8) !void {
        const name = self.string_value(func.name);
        if (self.functions.getPtr(name)) |group| {
            try group.append(.{ .decl = func, .impl_for = impl_for });
        } else {
            var group = array_list(function_info).init(self.allocator);
            try group.append(.{ .decl = func, .impl_for = impl_for });
            try self.functions.put(name, group);
        }
    }

    fn try_unify_call(
        self: *typecheck_ctx,
        info: function_info,
        arg_types: []const TypeId,
        self_name: ?[]const u8,
        generics: *string_map(TypeId),
        locals: *string_map(TypeId),
        fail: ?*call_failure,
    ) bool {
        _ = self_name;
        _ = locals;
        _ = generics;
        var local_generics = string_map(TypeId).init(self.allocator);
        defer local_generics.deinit();
        self.add_type_generic_constraints(info.decl.generics, info.decl.where_clause, &local_generics);

        const params = info.decl.params;
        if (params.len != arg_types.len) {
            if (fail) |slot| {
                slot.* = .{ .arity = .{ .expected = params.len, .found = arg_types.len } };
            }
            return false;
        }
        for (params, 0..) |param, idx| {
            const param_ty = self.type_from_type_node(param.ty, info.impl_for, &local_generics);
            const param_resolved = self.types.resolve(param_ty);
            if (self.types.types.items[param_resolved] == .dyn_trait) {
                const arg_resolved = self.types.resolve(arg_types[idx]);
                if (self.types.types.items[arg_resolved] != .dyn_trait) {
                    if (fail) |slot| {
                        slot.* = .{ .arg_mismatch = .{
                            .index = idx,
                            .expected = self.to_type_key(param_ty),
                            .actual = self.to_type_key(arg_types[idx]),
                        } };
                    }
                    return false;
                }
            } else if (self.types.types.items[param_resolved] == .applied and
                std.mem.eql(u8, self.types.types.items[param_resolved].applied.base, "dyn"))
            {
                const arg_resolved = self.types.resolve(arg_types[idx]);
                if (self.types.types.items[arg_resolved] != .applied or
                    !std.mem.eql(u8, self.types.types.items[arg_resolved].applied.base, "dyn"))
                {
                    if (fail) |slot| {
                        slot.* = .{ .arg_mismatch = .{
                            .index = idx,
                            .expected = self.to_type_key(param_ty),
                            .actual = self.to_type_key(arg_types[idx]),
                        } };
                    }
                    return false;
                }
            }
            if (!self.types.unify(param_ty, arg_types[idx])) {
                if (fail) |slot| {
                    slot.* = .{ .arg_mismatch = .{
                        .index = idx,
                        .expected = self.to_type_key(param_ty),
                        .actual = self.to_type_key(arg_types[idx]),
                    } };
                }
                return false;
            }
        }
        var constraint_fail: constraint_failure = undefined;
        if (!constraints_ok(self, &local_generics, if (fail != null) &constraint_fail else null)) {
            if (fail) |slot| {
                slot.* = .{ .constraint = constraint_fail };
            }
            return false;
        }
        return true;
    }

    fn return_type_for(
        self: *typecheck_ctx,
        info: function_info,
        self_name: ?[]const u8,
        generics: *string_map(TypeId),
    ) TypeId {
        _ = self_name;
        if (info.decl.return_type) |ret| {
            return self.type_from_type_node(ret, info.impl_for, generics);
        }
        return self.types.named("unit");
    }

    fn from_type_key(self: *typecheck_ctx, key: type_key) TypeId {
        return switch (key) {
            .name => |name| self.types.named(name),
            .dyn_trait => |name| self.types.dyn_trait(name),
        .applied => |ap| blk: {
            var args = self.allocator.alloc(TypeId, ap.args.len) catch return self.types.new_var();
            defer self.allocator.free(args);
            for (ap.args, 0..) |arg, idx| {
                args[idx] = self.from_type_key(arg);
            }
            break :blk self.types.applied(ap.base, args);
        },
            else => self.types.new_var(),
        };
    }
};

pub fn check(
    allocator: std.mem.Allocator,
    nodes: []const uir_mod.uir,
    strings: []const []const u8,
    roots: []const uir_mod.uir_identifier,
    spans: []const ?source.span,
    node_sources: []const source.source_id,
    diags: *array_list(diagnostic),
) typecheck_error!typecheck_result {
    var ctx = typecheck_ctx.init(allocator, nodes, strings, spans, node_sources, diags);
    defer ctx.deinit();

    add_builtin_traits(&ctx);
    try collect_env(&ctx, roots);
    check_trait_impls(&ctx, roots);
    try infer_roots(&ctx, roots);

    const out = try allocator.alloc(type_key, nodes.len);
    for (nodes, 0..) |_, idx| {
        const ty_id = ctx.node_types[idx];
        if (ty_id == std.math.maxInt(TypeId)) {
            out[idx] = .unknown;
        } else {
            out[idx] = ctx.to_type_key(ty_id);
        }
    }
    const owned = ctx.owned_type_key_slices.toOwnedSlice() catch return error.OutOfMemory;
    ctx.owned_type_key_slices = array_list([]const type_key).init(allocator);

    return .{ .types = out, .owned_slices = owned };
}

fn collect_env(ctx: *typecheck_ctx, roots: []const uir_mod.uir_identifier) !void {
    for (roots) |root| {
        const node = ctx.nodes[@intCast(root.idx)];
        if (node != .decl) continue;
        switch (node.decl) {
            .@"struct" => |st| {
                const name = ctx.string_value(st.name);
                if (!ctx.structs.contains(name)) {
                    ctx.structs.put(name, .{
                        .fields = st.fields,
                        .is_record = st.is_record,
                        .generics = st.generics,
                    }) catch return error.OutOfMemory;
                }
            },
            .trait => |tr| {
                const trait_name = ctx.string_value(tr.name);
                if (ctx.traits.contains(trait_name)) continue;
                var methods = std.array_list.Managed(trait_method).init(ctx.allocator);
                var reqs = std.array_list.Managed(trait_constraint).init(ctx.allocator);
                for (tr.items) |item| {
                    switch (item) {
                        .function => |func| {
                            methods.append(.{
                                .name = ctx.string_value(func.name),
                                .return_type = func.return_type,
                            }) catch return error.OutOfMemory;
                        },
                        .assoc_type => {},
                    }
                }
                for (tr.requires) |req| {
                    const constraint = ctx.constraint_from_type_node(req) orelse continue;
                    reqs.append(constraint) catch return error.OutOfMemory;
                }
                const method_slice = methods.toOwnedSlice() catch return error.OutOfMemory;
                const req_slice = reqs.toOwnedSlice() catch return error.OutOfMemory;
                ctx.traits.put(trait_name, .{
                    .methods = method_slice,
                    .requires = req_slice,
                    .is_auto = tr.is_auto,
                    .generics = tr.generics,
                }) catch return error.OutOfMemory;
            },
            .function => |func| try ctx.add_function(func, null),
            .impl => |impl| {
                const trait_name = ctx.string_value(impl.by_trait);
                const type_name = ctx.string_value(impl.for_struct);
                if (!impl.negative) {
                    for (impl.functions) |func| {
                        try ctx.add_function(func, type_name);
                    }
                }
                if (ctx.traits.contains(trait_name)) {
                    if (impl.negative) {
                        var list = ctx.trait_neg_impls.getPtr(trait_name);
                        if (list == null) {
                            var new_list = array_list([]const u8).init(ctx.allocator);
                            new_list.append(type_name) catch return error.OutOfMemory;
                            ctx.trait_neg_impls.put(trait_name, new_list) catch return error.OutOfMemory;
                        } else {
                            list.?.append(type_name) catch return error.OutOfMemory;
                        }
                    } else {
                        var list = ctx.trait_impls.getPtr(trait_name);
                        if (list == null) {
                            var new_list = array_list([]const u8).init(ctx.allocator);
                            new_list.append(type_name) catch return error.OutOfMemory;
                            ctx.trait_impls.put(trait_name, new_list) catch return error.OutOfMemory;
                        } else {
                            list.?.append(type_name) catch return error.OutOfMemory;
                        }
                    }
                }
            },
            else => {},
        }
    }
}

fn add_builtin_traits(ctx: *typecheck_ctx) void {
    const builtin_names = [_][]const u8{ "send", "sync", "sized" };
    for (builtin_names) |name| {
        if (ctx.traits.contains(name)) continue;
        const methods = ctx.allocator.alloc(trait_method, 0) catch return;
        const reqs = ctx.allocator.alloc(trait_constraint, 0) catch return;
        ctx.traits.put(name, .{
            .methods = methods,
            .requires = reqs,
            .is_auto = true,
            .generics = &.{},
        }) catch return;
    }
}

fn infer_roots(ctx: *typecheck_ctx, roots: []const uir_mod.uir_identifier) !void {
    for (roots) |root| {
        const node = ctx.nodes[@intCast(root.idx)];
        if (node != .decl) continue;
        switch (node.decl) {
            .function => |func| {
                if (func.body) |body| {
                    _ = try infer_function(ctx, func, null, body);
                }
            },
            .impl => |impl| {
                if (impl.negative) continue;
                const type_name = ctx.string_value(impl.for_struct);
                for (impl.functions) |func| {
                    if (func.body) |body| {
                        _ = try infer_function(ctx, func, type_name, body);
                    }
                }
            },
            else => {},
        }
    }
}

fn infer_function(
    ctx: *typecheck_ctx,
    func: uir_mod.uir.function_decl,
    self_name: ?[]const u8,
    body_id: uir_mod.uir_identifier,
) typecheck_error!TypeId {
    var locals = string_map(TypeId).init(ctx.allocator);
    defer locals.deinit();

    var generic_bindings = string_map(TypeId).init(ctx.allocator);
    defer generic_bindings.deinit();

    ctx.add_type_generic_constraints(func.generics, func.where_clause, &generic_bindings);

    const return_type = if (func.return_type) |ret|
        ctx.type_from_type_node(ret, self_name, &generic_bindings)
    else
        ctx.types.new_var();

    for (func.params) |param| {
        const param_name = ctx.string_value(param.name);
        const ty_id = ctx.type_from_type_node(param.ty, self_name, &generic_bindings);
        locals.put(param_name, ty_id) catch return error.OutOfMemory;
    }

    const body_type = ctx.infer_expr(body_id, self_name, &generic_bindings, &locals, return_type);
    _ = ctx.types.unify(body_type, return_type);
    ctx.node_types[@intCast(body_id.idx)] = body_type;

    _ = try run_borrow_check(ctx, body_id, &locals);
    return body_type;
}

fn run_borrow_check(ctx: *typecheck_ctx, body_id: uir_mod.uir_identifier, locals: *string_map(TypeId)) typecheck_error!void {
    var ref_locals = string_map(void).init(ctx.allocator);
    defer ref_locals.deinit();
    var it = locals.iterator();
    while (it.next()) |entry| {
        const ty = ctx.to_type_key(entry.value_ptr.*);
        if (is_ref_type(ty)) {
            ref_locals.put(entry.key_ptr.*, {}) catch return error.OutOfMemory;
        }
    }

    var checker = borrow_checker.init(ctx, &ref_locals);
    defer checker.deinit();
    _ = try checker.check_expr(body_id, true);
}

fn is_ref_type(key: type_key) bool {
    const base = type_key_base_name(key) orelse return false;
    return std.mem.eql(u8, base, "ref") or std.mem.eql(u8, base, "ref_mut");
}

const borrow_checker = struct {
    ctx: *typecheck_ctx,
    ref_locals: *string_map(void),
    loans: array_list(loan),
    ref_bindings: string_map(usize),

    const loan = struct {
        place: ?[]const u8,
        mutable: bool,
        temporary: bool,
        active: bool,
    };

    fn init(ctx: *typecheck_ctx, ref_locals: *string_map(void)) borrow_checker {
        return .{
            .ctx = ctx,
            .ref_locals = ref_locals,
            .loans = array_list(loan).init(ctx.allocator),
            .ref_bindings = string_map(usize).init(ctx.allocator),
        };
    }

    fn deinit(self: *borrow_checker) void {
        self.loans.deinit();
        self.ref_bindings.deinit();
    }

    fn check_expr(self: *borrow_checker, id: uir_mod.uir_identifier, scoped: bool) typecheck_error!void {
        defer if (scoped) self.expire_temporary();
        const node = self.ctx.nodes[@intCast(id.idx)];
        switch (node) {
            .decl => |decl| switch (decl) {
                .@"const" => |c| try self.check_decl(c.name, c.value),
                .@"var" => |v| try self.check_decl(v.name, v.value),
                else => {},
            },
            .block => |items| try self.check_block(items),
            .binary => |bin| try self.check_binary(bin),
            .unary => |un| try self.check_unary(un),
            .if_expr => |ife| try self.check_if_expr(ife),
            .match_expr => |_| {},
            .select_expr => |_| {},
            else => {
                _ = try self.check_subexprs(id);
            },
        }
    }

    fn check_block(self: *borrow_checker, items: []const uir_mod.uir_identifier) typecheck_error!void {
        if (items.len == 0) return;
        var live_after_sets = try self.ctx.allocator.alloc(string_map(void), items.len);
        defer {
            for (live_after_sets) |*set| set.deinit();
            self.ctx.allocator.free(live_after_sets);
        }

        var live = string_map(void).init(self.ctx.allocator);
        defer live.deinit();

        var idx = items.len;
        while (idx > 0) {
            idx -= 1;
            live_after_sets[idx] = try clone_void_map(self.ctx.allocator, &live);
            try self.collect_ref_uses(items[idx], &live);
        }

        for (items, 0..) |stmt_id, stmt_idx| {
            try self.check_expr(stmt_id, true);
            self.expire_unused(&live_after_sets[stmt_idx]);
        }
    }

    fn check_decl(self: *borrow_checker, name_id: uir_mod.string_identifier, value_id: uir_mod.uir_identifier) typecheck_error!void {
        const name = self.ctx.string_value(name_id);
        const value_node = self.ctx.nodes[@intCast(value_id.idx)];
        if (value_node == .unary and (value_node.unary.op == .borrow or value_node.unary.op == .borrow_mut)) {
            const loan_idx = try self.create_loan(value_node.unary, value_id, false);
            self.bind_ref(name, loan_idx);
            return;
        }
        if (value_node == .identifier) {
            const value_name = self.ctx.string_value(value_node.identifier);
            if (self.ref_bindings.get(value_name)) |loan_idx| {
                self.bind_ref(name, loan_idx);
                return;
            }
        }
        try self.check_expr(value_id, false);
        self.drop_binding(name);
    }

    fn check_binary(self: *borrow_checker, bin: uir_binary) typecheck_error!void {
        switch (bin.op) {
            .assign,
            .assign_add,
            .assign_sub,
            .assign_mul,
            .assign_div,
            .assign_mod,
            .assign_bit_and,
            .assign_bit_or,
            .assign_bit_xor,
            .assign_shl,
            .assign_shr,
            => {
                try self.check_expr(bin.right, false);
                if (self.place_from_expr(bin.left)) |place| {
                    self.check_write(place, bin.left);
                    if (self.ref_locals.contains(place)) {
                        self.drop_binding(place);
                    }
                }
            },
            else => {
                try self.check_expr(bin.left, false);
                try self.check_expr(bin.right, false);
            },
        }
    }

    fn check_unary(self: *borrow_checker, un: uir_unary) typecheck_error!void {
        switch (un.op) {
            .borrow, .borrow_mut => {
                try self.check_expr(un.right, false);
                _ = try self.create_loan(un, un.right, true);
            },
            else => try self.check_expr(un.right, false),
        }
    }

    fn check_if_expr(self: *borrow_checker, ife: uir_if) typecheck_error!void {
        try self.check_expr(ife.condition, false);
        var saved = try self.snapshot();

        var then_used = try self.branch_used_refs(ife.then_branch);
        defer then_used.deinit();
        self.prune_to_used(&then_used);
        try self.check_expr(ife.then_branch, true);

        var then_snapshot = try self.snapshot();
        self.restore(&saved);

        if (ife.else_branch) |else_ref| {
            var else_used = try self.branch_used_refs(else_ref);
            defer else_used.deinit();
            self.prune_to_used(&else_used);
            try self.check_expr(else_ref, true);
            self.merge_from_snapshot(&then_snapshot);
        } else {
            self.deinit_state(&then_snapshot);
        }
    }

    fn check_subexprs(self: *borrow_checker, id: uir_mod.uir_identifier) typecheck_error!void {
        const node = self.ctx.nodes[@intCast(id.idx)];
        switch (node) {
            .unary => |un| try self.check_expr(un.right, false),
            .binary => |bin| {
                try self.check_expr(bin.left, false);
                try self.check_expr(bin.right, false);
            },
            .block => |items| for (items) |item| try self.check_expr(item, true),
            .if_expr => |ife| {
                try self.check_expr(ife.condition, false);
                try self.check_expr(ife.then_branch, true);
                if (ife.else_branch) |ref| try self.check_expr(ref, true);
            },
            .label_expr => |le| try self.check_expr(le.body, true),
            .loop_expr => |le| try self.check_expr(le.body, true),
            .while_expr => |we| {
                try self.check_expr(we.condition, false);
                try self.check_expr(we.body, true);
            },
            .while_in_expr => |we| {
                try self.check_expr(we.iter, false);
                try self.check_expr(we.body, true);
            },
            .until_expr => |ue| {
                try self.check_expr(ue.condition, false);
                try self.check_expr(ue.body, true);
            },
            .repeat_expr => |re| {
                try self.check_expr(re.count, false);
                try self.check_expr(re.body, true);
            },
            .for_expr => |fe| {
                try self.check_expr(fe.iter, false);
                try self.check_expr(fe.body, true);
            },
            .each_expr => |ee| {
                try self.check_expr(ee.iter, false);
                try self.check_expr(ee.body, true);
            },
            .break_expr => |be| if (be.value) |ref| try self.check_expr(ref, false),
            .yield_expr => |ye| if (ye.value) |ref| try self.check_expr(ref, false),
            .atomic_expr => |ae| try self.check_expr(ae.value, false),
            .intrinsic => |call| for (call.args) |arg| try self.check_expr(arg, false),
            .record_literal => |rec| for (rec.fields) |field| try self.check_expr(field.value, false),
            else => {},
        }
    }

    fn create_loan(self: *borrow_checker, un: uir_unary, target_id: uir_mod.uir_identifier, temporary: bool) typecheck_error!usize {
        const mutable = un.op == .borrow_mut;
        const place = self.place_from_expr(target_id);
        if (place == null) {
            try self.report(target_id, "cannot borrow temporary");
        }
        if (place) |p| {
            if (mutable) {
                if (self.has_any_borrow(p)) {
                    try self.report(target_id, "mutable borrow conflicts with existing borrow");
                }
            } else {
                if (self.has_mut_borrow(p)) {
                    try self.report(target_id, "immutable borrow conflicts with mutable borrow");
                }
            }
        }
        const idx = self.loans.items.len;
        self.loans.append(.{
            .place = place,
            .mutable = mutable,
            .temporary = temporary,
            .active = true,
        }) catch return error.OutOfMemory;
        return idx;
    }

    fn bind_ref(self: *borrow_checker, name: []const u8, loan_idx: usize) void {
        _ = self.ref_bindings.put(name, loan_idx) catch {};
    }

    fn check_write(self: *borrow_checker, place: []const u8, span_id: uir_mod.uir_identifier) void {
        for (self.loans.items) |ln| {
            if (!ln.active) continue;
            if (ln.place == null) continue;
            if (std.mem.eql(u8, ln.place.?, place)) {
                _ = self.report(span_id, "assignment conflicts with active borrow") catch {};
                return;
            }
        }
    }

    fn has_mut_borrow(self: *borrow_checker, place: []const u8) bool {
        for (self.loans.items) |ln| {
            if (!ln.active) continue;
            if (!ln.mutable) continue;
            if (ln.place != null and std.mem.eql(u8, ln.place.?, place)) return true;
        }
        return false;
    }

    fn has_any_borrow(self: *borrow_checker, place: []const u8) bool {
        for (self.loans.items) |ln| {
            if (!ln.active) continue;
            if (ln.place != null and std.mem.eql(u8, ln.place.?, place)) return true;
        }
        return false;
    }

    fn place_from_expr(self: *borrow_checker, id: uir_mod.uir_identifier) ?[]const u8 {
        const node = self.ctx.nodes[@intCast(id.idx)];
        return switch (node) {
            .identifier => |ident| self.ctx.string_value(ident),
            .binary => |bin| switch (bin.op) {
                .access, .index => self.place_from_expr(bin.left),
                else => null,
            },
            else => null,
        };
    }

    fn collect_ref_uses(self: *borrow_checker, id: uir_mod.uir_identifier, out: *string_map(void)) typecheck_error!void {
        const node = self.ctx.nodes[@intCast(id.idx)];
        switch (node) {
            .identifier => |ident| {
                const name = self.ctx.string_value(ident);
                if (self.ref_locals.contains(name)) {
                    if (!out.contains(name)) try out.put(name, {});
                }
            },
            .unary => |un| try self.collect_ref_uses(un.right, out),
            .binary => |bin| {
                try self.collect_ref_uses(bin.left, out);
                try self.collect_ref_uses(bin.right, out);
            },
            .block => |items| for (items) |item| try self.collect_ref_uses(item, out),
            .if_expr => |ife| {
                try self.collect_ref_uses(ife.condition, out);
                try self.collect_ref_uses(ife.then_branch, out);
                if (ife.else_branch) |ref| try self.collect_ref_uses(ref, out);
            },
            .label_expr => |le| try self.collect_ref_uses(le.body, out),
            .loop_expr => |le| try self.collect_ref_uses(le.body, out),
            .while_expr => |we| {
                try self.collect_ref_uses(we.condition, out);
                try self.collect_ref_uses(we.body, out);
            },
            .while_in_expr => |we| {
                try self.collect_ref_uses(we.iter, out);
                try self.collect_ref_uses(we.body, out);
            },
            .until_expr => |ue| {
                try self.collect_ref_uses(ue.condition, out);
                try self.collect_ref_uses(ue.body, out);
            },
            .repeat_expr => |re| {
                try self.collect_ref_uses(re.count, out);
                try self.collect_ref_uses(re.body, out);
            },
            .for_expr => |fe| {
                try self.collect_ref_uses(fe.iter, out);
                try self.collect_ref_uses(fe.body, out);
            },
            .each_expr => |ee| {
                try self.collect_ref_uses(ee.iter, out);
                try self.collect_ref_uses(ee.body, out);
            },
            .break_expr => |be| if (be.value) |ref| try self.collect_ref_uses(ref, out),
            .yield_expr => |ye| if (ye.value) |ref| try self.collect_ref_uses(ref, out),
            .atomic_expr => |ae| try self.collect_ref_uses(ae.value, out),
            .intrinsic => |call| for (call.args) |arg| try self.collect_ref_uses(arg, out),
            .record_literal => |rec| for (rec.fields) |field| try self.collect_ref_uses(field.value, out),
            else => {},
        }
    }

    fn expire_unused(self: *borrow_checker, live_after: *string_map(void)) void {
        var stale = array_list([]const u8).init(self.ctx.allocator);
        defer stale.deinit();

        var it = self.ref_bindings.iterator();
        while (it.next()) |entry| {
            if (live_after.contains(entry.key_ptr.*)) continue;
            stale.append(entry.key_ptr.*) catch {};
        }

        for (stale.items) |name| {
            self.drop_binding(name);
        }
    }

    fn drop_binding(self: *borrow_checker, name: []const u8) void {
        const loan_idx = self.ref_bindings.get(name) orelse return;
        _ = self.ref_bindings.remove(name);
        if (!self.loan_in_use(loan_idx)) {
            if (loan_idx < self.loans.items.len) {
                self.loans.items[loan_idx].active = false;
            }
        }
    }

    fn loan_in_use(self: *borrow_checker, loan_idx: usize) bool {
        var it = self.ref_bindings.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.* == loan_idx) return true;
        }
        return false;
    }

    fn expire_temporary(self: *borrow_checker) void {
        for (self.loans.items) |*ln| {
            if (ln.temporary and ln.active) {
                ln.active = false;
            }
        }
    }

    fn snapshot(self: *borrow_checker) !borrow_state {
        const count = self.loans.items.len;
        const active_flags = try self.ctx.allocator.alloc(bool, count);
        for (self.loans.items, 0..) |ln, idx| {
            active_flags[idx] = ln.active;
        }
        return borrow_state{
            .active_flags = active_flags,
            .ref_bindings = try clone_string_map(self.ctx.allocator, &self.ref_bindings),
        };
    }

    fn restore(self: *borrow_checker, state: *borrow_state) void {
        self.ref_bindings.deinit();
        self.ref_bindings = state.ref_bindings;

        const saved_len = state.active_flags.len;
        for (self.loans.items, 0..) |*ln, idx| {
            if (idx < saved_len) {
                ln.active = state.active_flags[idx];
            } else {
                ln.active = false;
            }
        }
        self.ctx.allocator.free(state.active_flags);
    }

    fn merge_from_snapshot(self: *borrow_checker, state: *borrow_state) void {
        var it = state.ref_bindings.iterator();
        while (it.next()) |entry| {
            if (!self.ref_bindings.contains(entry.key_ptr.*)) {
                _ = self.ref_bindings.put(entry.key_ptr.*, entry.value_ptr.*) catch {};
            }
        }

        const saved_len = state.active_flags.len;
        for (self.loans.items, 0..) |*ln, idx| {
            const from_branch = if (idx < saved_len) state.active_flags[idx] else false;
            ln.active = ln.active or from_branch;
        }
        state.ref_bindings.deinit();
        self.ctx.allocator.free(state.active_flags);
    }

    fn deinit_state(self: *borrow_checker, state: *borrow_state) void {
        state.ref_bindings.deinit();
        self.ctx.allocator.free(state.active_flags);
    }

    fn prune_to_used(self: *borrow_checker, used: *string_map(void)) void {
        var it = self.ref_bindings.iterator();
        while (it.next()) |entry| {
            if (used.contains(entry.key_ptr.*)) continue;
            _ = self.ref_bindings.remove(entry.key_ptr.*);
        }
    }

    fn branch_used_refs(self: *borrow_checker, id: uir_mod.uir_identifier) typecheck_error!string_map(void) {
        var used = string_map(void).init(self.ctx.allocator);
        try self.collect_ref_uses(id, &used);
        return used;
    }

    fn report(self: *borrow_checker, id: uir_mod.uir_identifier, message: []const u8) typecheck_error!void {
        try self.ctx.diags.append(.{
            .danger = .@"error",
            .message = message,
            .span = self.ctx.span_for_node(id),
            .source_id = self.ctx.source_for_node(id),
        });
    }
};

const borrow_state = struct {
    active_flags: []bool,
    ref_bindings: string_map(usize),
};

fn clone_string_map(allocator: std.mem.Allocator, src: *string_map(usize)) !string_map(usize) {
    var out = string_map(usize).init(allocator);
    var it = src.iterator();
    while (it.next()) |entry| {
        try out.put(entry.key_ptr.*, entry.value_ptr.*);
    }
    return out;
}

fn clone_void_map(allocator: std.mem.Allocator, src: *string_map(void)) !string_map(void) {
    var out = string_map(void).init(allocator);
    var it = src.iterator();
    while (it.next()) |entry| {
        try out.put(entry.key_ptr.*, {});
    }
    return out;
}

fn infer_binary_operator(name: []const u8) ?[]const u8 {
    if (std.mem.eql(u8, name, "+")) return "add";
    if (std.mem.eql(u8, name, "-")) return "sub";
    if (std.mem.eql(u8, name, "*")) return "mul";
    if (std.mem.eql(u8, name, "/")) return "div";
    if (std.mem.eql(u8, name, "%")) return "mod";
    if (std.mem.eql(u8, name, "&")) return "bit_and";
    if (std.mem.eql(u8, name, "|")) return "bit_or";
    if (std.mem.eql(u8, name, "^")) return "bit_xor";
    if (std.mem.eql(u8, name, "<<")) return "shl";
    if (std.mem.eql(u8, name, ">>")) return "shr";
    if (std.mem.eql(u8, name, "==")) return "eq";
    if (std.mem.eql(u8, name, "!=")) return "ne";
    if (std.mem.eql(u8, name, "<")) return "lt";
    if (std.mem.eql(u8, name, "<=")) return "le";
    if (std.mem.eql(u8, name, ">")) return "gt";
    if (std.mem.eql(u8, name, ">=")) return "ge";
    return null;
}

fn binary_operator_method_name(op: ink.binary) ?[]const u8 {
    return switch (op) {
        .add => "add",
        .sub => "sub",
        .mul => "mul",
        .div => "div",
        .mod => "mod",
        .bit_and => "bit_and",
        .bit_or => "bit_or",
        .bit_xor => "bit_xor",
        .shl => "shl",
        .shr => "shr",
        .equal => "eq",
        .not_equal => "ne",
        .less_than => "lt",
        .less_or_equal => "le",
        .greater_than => "gt",
        .greater_or_equal => "ge",
        else => null,
    };
}

fn unary_operator_method_name(op: ink.unary) ?[]const u8 {
    return switch (op) {
        .neg => "neg",
        .not => "not",
        .bit_not => "bit_not",
        else => null,
    };
}

fn is_builtin_numeric(name: []const u8) bool {
    return std.mem.eql(u8, name, "int") or std.mem.eql(u8, name, "uint") or std.mem.eql(u8, name, "float");
}

fn is_builtin_bool(name: []const u8) bool {
    return std.mem.eql(u8, name, "bool");
}

fn is_builtin_string(name: []const u8) bool {
    return std.mem.eql(u8, name, "string");
}

fn is_builtin_type(name: []const u8) bool {
    return is_builtin_numeric(name) or is_builtin_bool(name) or is_builtin_string(name);
}

fn is_qualified_name(name: []const u8) bool {
    return std.mem.indexOf(u8, name, "::") != null;
}

fn append_type_key(ctx: *typecheck_ctx, buf: *array_list(u8), key: type_key) void {
    switch (key) {
        .name => |name| {
            _ = buf.appendSlice(name) catch {};
        },
        .dyn_trait => |name| {
            _ = buf.appendSlice("dyn ") catch {};
            _ = buf.appendSlice(name) catch {};
        },
        .applied => |ap| {
            if (std.mem.eql(u8, ap.base, "dyn") and ap.args.len >= 1) {
                _ = buf.appendSlice("dyn ") catch {};
                append_type_key(ctx, buf, ap.args[0]);
                for (ap.args[1..]) |arg| {
                    _ = buf.appendSlice(" & ") catch {};
                    if (arg == .applied and std.mem.eql(u8, arg.applied.base, "not") and arg.applied.args.len >= 1) {
                        _ = buf.appendSlice("!") catch {};
                        append_type_key(ctx, buf, arg.applied.args[0]);
                    } else {
                        append_type_key(ctx, buf, arg);
                    }
                }
                return;
            }
            if (std.mem.eql(u8, ap.base, "slice") and ap.args.len == 1) {
                _ = buf.appendSlice("[]") catch {};
                append_type_key(ctx, buf, ap.args[0]);
                return;
            }
            if (std.mem.eql(u8, ap.base, "ref") and ap.args.len == 1) {
                _ = buf.appendSlice("&") catch {};
                append_type_key(ctx, buf, ap.args[0]);
                return;
            }
            if (std.mem.eql(u8, ap.base, "ref_mut") and ap.args.len == 1) {
                _ = buf.appendSlice("&mut ") catch {};
                append_type_key(ctx, buf, ap.args[0]);
                return;
            }
            if (std.mem.eql(u8, ap.base, "box") and ap.args.len == 1) {
                _ = buf.appendSlice("box ") catch {};
                append_type_key(ctx, buf, ap.args[0]);
                return;
            }
            if (std.mem.eql(u8, ap.base, "array") and ap.args.len >= 2) {
                _ = buf.appendSlice("[") catch {};
                append_type_key(ctx, buf, ap.args[0]);
                _ = buf.appendSlice("]") catch {};
                append_type_key(ctx, buf, ap.args[1]);
                return;
            }
            if (std.mem.eql(u8, ap.base, "not") and ap.args.len == 1) {
                _ = buf.appendSlice("!") catch {};
                append_type_key(ctx, buf, ap.args[0]);
                return;
            }
            _ = buf.appendSlice(ap.base) catch {};
            if (ap.args.len == 0) return;
            _ = buf.appendSlice("<") catch {};
            for (ap.args, 0..) |arg, idx| {
                if (idx > 0) _ = buf.appendSlice(", ") catch {};
                append_type_key(ctx, buf, arg);
            }
            _ = buf.appendSlice(">") catch {};
        },
        else => {
            _ = buf.appendSlice("unknown") catch {};
        },
    }
}

fn append_call_signature(
    ctx: *typecheck_ctx,
    buf: *array_list(u8),
    name: []const u8,
    arg_types: []const TypeId,
    receiver: ?TypeId,
) void {
    if (receiver) |recv| {
        append_type_key(ctx, buf, ctx.to_type_key(recv));
        _ = buf.appendSlice(".") catch {};
    }
    _ = buf.appendSlice(name) catch {};
    _ = buf.appendSlice("(") catch {};
    const skip: usize = if (receiver != null) 1 else 0;
    if (arg_types.len > skip) {
        for (arg_types[skip..], 0..) |arg_type, idx| {
            if (idx > 0) _ = buf.appendSlice(", ") catch {};
            append_type_key(ctx, buf, ctx.to_type_key(arg_type));
        }
    }
    _ = buf.appendSlice(")") catch {};
}

fn is_numeric_key(key: type_key) bool {
    return switch (key) {
        .name => |name| is_builtin_numeric(name),
        .applied => |ap| is_builtin_numeric(ap.base),
        else => false,
    };
}

fn report_type_mismatch(
    ctx: *typecheck_ctx,
    message: []const u8,
    left: type_key,
    right: type_key,
    span_id: ?uir_mod.uir_identifier,
) void {
    var buf = array_list(u8).init(ctx.allocator);
    defer buf.deinit();
    _ = buf.appendSlice(message) catch return;
    _ = buf.appendSlice(": ") catch return;
    append_type_key(ctx, &buf, left);
    _ = buf.appendSlice(" vs ") catch return;
    append_type_key(ctx, &buf, right);
    const full = buf.toOwnedSlice() catch return;
    const span = if (span_id) |id| ctx.span_for_node(id) else null;
    const source_id = if (span_id) |id| ctx.source_for_node(id) else null;
    ctx.diags.append(.{
        .danger = .@"error",
        .message = full,
        .span = span,
        .source_id = source_id,
    }) catch {};
}

fn report_trait_violation(
    ctx: *typecheck_ctx,
    message: []const u8,
    ty: type_key,
    trait_name: []const u8,
    span_id: uir_mod.uir_identifier,
) void {
    var buf = array_list(u8).init(ctx.allocator);
    defer buf.deinit();
    _ = buf.appendSlice(message) catch return;
    _ = buf.appendSlice(": ") catch return;
    append_type_key(ctx, &buf, ty);
    _ = buf.appendSlice(" is not ") catch return;
    _ = buf.appendSlice(trait_name) catch return;
    const full = buf.toOwnedSlice() catch return;
    ctx.diags.append(.{
        .danger = .@"error",
        .message = full,
        .span = ctx.span_for_node(span_id),
        .source_id = ctx.source_for_node(span_id),
    }) catch {};
}

fn report_expected_kind(
    ctx: *typecheck_ctx,
    expected: []const u8,
    actual: type_key,
    span_id: uir_mod.uir_identifier,
) void {
    var buf = array_list(u8).init(ctx.allocator);
    defer buf.deinit();
    _ = buf.appendSlice("expected ") catch return;
    _ = buf.appendSlice(expected) catch return;
    _ = buf.appendSlice(", got ") catch return;
    append_type_key(ctx, &buf, actual);
    const full = buf.toOwnedSlice() catch return;
    ctx.diags.append(.{
        .danger = .@"error",
        .message = full,
        .span = ctx.span_for_node(span_id),
        .source_id = ctx.source_for_node(span_id),
    }) catch {};
}

fn report_unknown_field(
    ctx: *typecheck_ctx,
    struct_name: []const u8,
    field_name: []const u8,
    span_id: uir_mod.uir_identifier,
) void {
    var buf = array_list(u8).init(ctx.allocator);
    defer buf.deinit();
    _ = buf.appendSlice("unknown field ") catch return;
    _ = buf.appendSlice(field_name) catch return;
    _ = buf.appendSlice(" on ") catch return;
    _ = buf.appendSlice(struct_name) catch return;
    const full = buf.toOwnedSlice() catch return;
    ctx.diags.append(.{
        .danger = .@"error",
        .message = full,
        .span = ctx.span_for_node(span_id),
        .source_id = ctx.source_for_node(span_id),
    }) catch {};
}

fn report_unknown_call(
    ctx: *typecheck_ctx,
    name: []const u8,
    arg_types: []const TypeId,
    receiver: ?TypeId,
    call_id: uir_mod.uir_identifier,
) void {
    var buf = array_list(u8).init(ctx.allocator);
    defer buf.deinit();
    if (receiver != null) {
        _ = buf.appendSlice("unknown method ") catch return;
    } else {
        _ = buf.appendSlice("unknown function ") catch return;
    }
    append_call_signature(ctx, &buf, name, arg_types, receiver);
    const full = buf.toOwnedSlice() catch return;
    ctx.diags.append(.{
        .danger = .@"error",
        .message = full,
        .span = ctx.span_for_node(call_id),
        .source_id = ctx.source_for_node(call_id),
    }) catch {};
}

fn append_trait_requirements(
    ctx: *typecheck_ctx,
    buf: *array_list(u8),
    trait_name: []const u8,
) void {
    const info = ctx.traits.get(trait_name) orelse return;
    if (info.requires.len == 0) return;
    _ = buf.appendSlice(" (requires ") catch return;
    for (info.requires, 0..) |req, idx| {
        if (idx > 0) _ = buf.appendSlice(", ") catch return;
        if (req.negative) _ = buf.appendSlice("not ") catch return;
        _ = buf.appendSlice(req.name) catch return;
    }
    _ = buf.appendSlice(")") catch return;
}

fn report_call_failure(
    ctx: *typecheck_ctx,
    name: []const u8,
    arg_types: []const TypeId,
    receiver: ?TypeId,
    call_id: uir_mod.uir_identifier,
    failure: ?call_failure,
    arity_options: []const usize,
) void {
    var buf = array_list(u8).init(ctx.allocator);
    defer buf.deinit();
    if (failure) |fail| {
        switch (fail) {
            .constraint => |con| {
                _ = buf.appendSlice("constraint not satisfied in call to ") catch return;
                append_call_signature(ctx, &buf, name, arg_types, receiver);
                _ = buf.appendSlice(": ") catch return;
                if (con.generic_name) |gen| {
                    _ = buf.appendSlice(gen) catch return;
                    _ = buf.appendSlice(" = ") catch return;
                }
                append_type_key(ctx, &buf, con.ty);
                if (con.constraint.negative) {
                    _ = buf.appendSlice(" must not satisfy ") catch return;
                } else {
                    _ = buf.appendSlice(" does not satisfy ") catch return;
                }
                _ = buf.appendSlice(con.constraint.name) catch return;
                append_trait_requirements(ctx, &buf, con.constraint.name);
            },
            .arg_mismatch => |arg| {
                const receiver_mismatch = receiver != null and arg.index == 0;
                if (receiver_mismatch) {
                    _ = buf.appendSlice("receiver type mismatch in call to ") catch return;
                } else {
                    _ = buf.appendSlice("argument ") catch return;
                    const display_idx: usize = if (receiver != null) arg.index - 1 else arg.index;
                    buf.writer().print("{d}", .{display_idx + 1}) catch return;
                    _ = buf.appendSlice(" in call to ") catch return;
                }
                append_call_signature(ctx, &buf, name, arg_types, receiver);
                _ = buf.appendSlice(": expected ") catch return;
                append_type_key(ctx, &buf, arg.expected);
                _ = buf.appendSlice(", got ") catch return;
                append_type_key(ctx, &buf, arg.actual);
            },
            .arity => |arity| {
                _ = buf.appendSlice("invalid call to ") catch return;
                append_call_signature(ctx, &buf, name, arg_types, receiver);
                _ = buf.appendSlice(": expected ") catch return;
                if (arity_options.len > 1) {
                    _ = buf.appendSlice("one of ") catch return;
                    for (arity_options, 0..) |count, idx| {
                        if (idx > 0) _ = buf.appendSlice(", ") catch return;
                        buf.writer().print("{d}", .{count}) catch return;
                    }
                } else {
                    buf.writer().print("{d}", .{arity.expected}) catch return;
                }
                _ = buf.appendSlice(" arguments, got ") catch return;
                buf.writer().print("{d}", .{arity.found}) catch return;
            },
        }
    } else {
        _ = buf.appendSlice("no matching overload for ") catch return;
        append_call_signature(ctx, &buf, name, arg_types, receiver);
    }
    const full = buf.toOwnedSlice() catch return;
    ctx.diags.append(.{
        .danger = .@"error",
        .message = full,
        .span = ctx.span_for_node(call_id),
        .source_id = ctx.source_for_node(call_id),
    }) catch {};
}

fn report_missing_impl_method(
    ctx: *typecheck_ctx,
    trait_name: []const u8,
    type_name: []const u8,
    method_name: []const u8,
    span_id: uir_mod.uir_identifier,
) void {
    var buf = array_list(u8).init(ctx.allocator);
    defer buf.deinit();
    _ = buf.appendSlice("impl for trait ") catch return;
    _ = buf.appendSlice(trait_name) catch return;
    _ = buf.appendSlice(" on ") catch return;
    _ = buf.appendSlice(type_name) catch return;
    _ = buf.appendSlice(" missing method ") catch return;
    _ = buf.appendSlice(method_name) catch return;
    const full = buf.toOwnedSlice() catch return;
    ctx.diags.append(.{
        .danger = .@"error",
        .message = full,
        .span = ctx.span_for_node(span_id),
        .source_id = ctx.source_for_node(span_id),
    }) catch {};
}

fn report_extra_impl_method(
    ctx: *typecheck_ctx,
    trait_name: []const u8,
    type_name: []const u8,
    method_name: []const u8,
    span: ?source.span,
    source_id: ?source.source_id,
) void {
    var buf = array_list(u8).init(ctx.allocator);
    defer buf.deinit();
    _ = buf.appendSlice("impl for trait ") catch return;
    _ = buf.appendSlice(trait_name) catch return;
    _ = buf.appendSlice(" on ") catch return;
    _ = buf.appendSlice(type_name) catch return;
    _ = buf.appendSlice(" defines unknown method ") catch return;
    _ = buf.appendSlice(method_name) catch return;
    const full = buf.toOwnedSlice() catch return;
    ctx.diags.append(.{
        .danger = .@"error",
        .message = full,
        .span = span,
        .source_id = source_id,
    }) catch {};
}

fn impl_has_method(ctx: *typecheck_ctx, funcs: []const uir_mod.uir.function_decl, name: []const u8) bool {
    for (funcs) |func| {
        if (std.mem.eql(u8, ctx.string_value(func.name), name)) return true;
    }
    return false;
}

fn trait_has_method(info: trait_info, name: []const u8) bool {
    for (info.methods) |method| {
        if (std.mem.eql(u8, method.name, name)) return true;
    }
    return false;
}

fn function_decl_span(ctx: *typecheck_ctx, func: uir_mod.uir.function_decl) ?source.span {
    if (func.span) |span| return span;
    if (func.body) |id| {
        if (ctx.span_for_node(id)) |span| return span;
    }
    if (func.return_type) |id| {
        if (ctx.span_for_node(id)) |span| return span;
    }
    if (func.params.len > 0) {
        if (ctx.span_for_node(func.params[0].ty)) |span| return span;
    }
    return null;
}

fn check_trait_impls(ctx: *typecheck_ctx, roots: []const uir_mod.uir_identifier) void {
    for (roots) |root| {
        const node = ctx.nodes[@intCast(root.idx)];
        if (node != .decl) continue;
        if (node.decl != .impl) continue;
        const impl = node.decl.impl;
        if (impl.negative) continue;

        const trait_name = ctx.string_value(impl.by_trait);
        const type_name = ctx.string_value(impl.for_struct);
        const info = ctx.traits.get(trait_name) orelse continue;

        for (impl.functions) |func| {
            const method_name = ctx.string_value(func.name);
            if (!trait_has_method(info, method_name)) {
                const span = function_decl_span(ctx, func) orelse ctx.span_for_node(root);
                const source_id: ?source.source_id = func.source_id;
                report_extra_impl_method(ctx, trait_name, type_name, method_name, span, source_id);
            }
        }

        for (info.methods) |method| {
            if (!impl_has_method(ctx, impl.functions, method.name)) {
                report_missing_impl_method(ctx, trait_name, type_name, method.name, root);
            }
        }
    }
}

fn type_satisfies_trait_root(ctx: *typecheck_ctx, ty: type_key, trait_name: []const u8) bool {
    var visited = array_list([]const u8).init(ctx.allocator);
    defer visited.deinit();
    var visited_types = string_map(void).init(ctx.allocator);
    defer visited_types.deinit();
    return type_satisfies_trait(ctx, ty, trait_name, &visited, &visited_types);
}

fn ensure_bool(ctx: *typecheck_ctx, ty: TypeId, span_id: uir_mod.uir_identifier) void {
    const key = ctx.to_type_key(ty);
    if (key == .unknown) {
        _ = ctx.types.unify(ty, ctx.types.named("bool"));
        return;
    }
    if (!is_builtin_bool(type_key_base_name(key) orelse "")) {
        report_expected_kind(ctx, "bool", key, span_id);
    }
}

fn ensure_numeric(ctx: *typecheck_ctx, ty: TypeId, span_id: uir_mod.uir_identifier) void {
    const key = ctx.to_type_key(ty);
    if (key == .unknown) {
        _ = ctx.types.unify(ty, ctx.types.named("int"));
        return;
    }
    if (!is_numeric_key(key)) {
        report_expected_kind(ctx, "numeric type", key, span_id);
    }
}

fn ensure_duration(ctx: *typecheck_ctx, ty: TypeId, span_id: uir_mod.uir_identifier) void {
    const key = ctx.to_type_key(ty);
    if (key == .unknown) {
        _ = ctx.types.unify(ty, ctx.types.named("duration"));
        return;
    }
    if (type_key_base_name(key)) |name| {
        if (std.mem.eql(u8, name, "duration")) return;
    }
    report_expected_kind(ctx, "duration", key, span_id);
}

fn ensure_deadline_source(ctx: *typecheck_ctx, ty: TypeId, span_id: uir_mod.uir_identifier) void {
    const key = ctx.to_type_key(ty);
    if (key == .unknown) {
        _ = ctx.types.unify(ty, ctx.types.named("duration"));
        return;
    }
    if (type_key_base_name(key)) |name| {
        if (std.mem.eql(u8, name, "duration") or std.mem.eql(u8, name, "instant") or std.mem.eql(u8, name, "deadline")) {
            return;
        }
    }
    report_expected_kind(ctx, "duration, instant, or deadline", key, span_id);
}

fn atomic_inner_type(ctx: *typecheck_ctx, ty: TypeId) ?TypeId {
    const resolved = ctx.types.resolve(ty);
    const node = ctx.types.types.items[resolved];
    return switch (node) {
        .applied => |ap| blk: {
            if (!std.mem.eql(u8, ap.base, "atomic")) break :blk null;
            if (ap.args.len != 1) break :blk null;
            break :blk ap.args[0];
        },
        else => null,
    };
}

fn wrap_atomic(ctx: *typecheck_ctx, inner: TypeId) TypeId {
    return ctx.types.applied("atomic", &.{inner});
}

fn task_inner_type(ctx: *typecheck_ctx, ty: TypeId) ?TypeId {
    const resolved = ctx.types.resolve(ty);
    const node = ctx.types.types.items[resolved];
    return switch (node) {
        .applied => |ap| blk: {
            if (!std.mem.eql(u8, ap.base, "task")) break :blk null;
            if (ap.args.len != 1) break :blk null;
            break :blk ap.args[0];
        },
        else => null,
    };
}

fn wrap_task(ctx: *typecheck_ctx, inner: TypeId) TypeId {
    return ctx.types.applied("task", &.{inner});
}

fn default_loop_result(ctx: *typecheck_ctx, result: TypeId) void {
    const resolved = ctx.types.resolve(result);
    const node = ctx.types.types.items[resolved];
    if (node == .tvar and node.tvar.binding == null) {
        _ = ctx.types.unify(result, ctx.types.named("unit"));
    }
}

fn is_unit_type(key: type_key) bool {
    return switch (key) {
        .name => |name| std.mem.eql(u8, name, "unit"),
        else => false,
    };
}

fn is_unit_expr(ctx: *typecheck_ctx, id: uir_mod.uir_identifier) bool {
    const node = ctx.nodes[@intCast(id.idx)];
    return switch (node) {
        .identifier => |ident| std.mem.eql(u8, ctx.string_value(ident), "unit"),
        else => false,
    };
}

fn element_type_from_container(ctx: *typecheck_ctx, key: type_key) ?type_key {
    _ = ctx;
    return switch (key) {
        .applied => |ap| blk: {
            if (std.mem.eql(u8, ap.base, "slice")) {
                if (ap.args.len < 1) break :blk null;
                break :blk ap.args[0];
            }
            if (std.mem.eql(u8, ap.base, "array")) {
                if (ap.args.len < 2) break :blk null;
                break :blk ap.args[1];
            }
            break :blk null;
        },
        else => null,
    };
}

fn infer_call_base(ctx: *typecheck_ctx, base_id: uir_mod.uir_identifier) ?struct { name: []const u8, receiver: ?uir_mod.uir_identifier } {
    const node = ctx.nodes[@intCast(base_id.idx)];
    return switch (node) {
        .identifier => |ident| .{ .name = ctx.string_value(ident), .receiver = null },
        .binary => |bin| blk: {
            if (bin.op != .access) return null;
            const field_node = ctx.nodes[@intCast(bin.right.idx)];
            if (field_node != .identifier) return null;
            break :blk .{ .name = ctx.string_value(field_node.identifier), .receiver = bin.left };
        },
        else => null,
    };
}

fn infer_call(ctx: *typecheck_ctx, id: uir_mod.uir_identifier, self_name: ?[]const u8, generics: *string_map(TypeId), locals: *string_map(TypeId)) TypeId {
    var base_id = id;
    var args_buf: [7]uir_mod.uir_identifier = undefined;
    var arg_count: usize = 0;

    while (true) {
        const node = ctx.nodes[@intCast(base_id.idx)];
        switch (node) {
            .binary => |bin| {
                if (bin.op == .call) {
                    if (arg_count >= args_buf.len) return ctx.types.new_var();
                    args_buf[arg_count] = bin.right;
                    arg_count += 1;
                    base_id = bin.left;
                    continue;
                }
            },
            else => {},
        }
        break;
    }

    var i: usize = 0;
    while (i < arg_count / 2) : (i += 1) {
        const tmp = args_buf[i];
        args_buf[i] = args_buf[arg_count - 1 - i];
        args_buf[arg_count - 1 - i] = tmp;
    }

    if (arg_count == 1 and is_unit_expr(ctx, args_buf[0])) {
        arg_count = 0;
    }

    const arg_ids = args_buf[0..arg_count];
    if (infer_call_base(ctx, base_id)) |base| {
        if (base.receiver) |recv| {
            const recv_type = ctx.infer_expr(recv, self_name, generics, locals, null);
            const arg_types = ctx.allocator.alloc(TypeId, arg_count) catch return ctx.types.new_var();
            defer ctx.allocator.free(arg_types);
            for (arg_ids, 0..) |arg, idx2| {
                arg_types[idx2] = ctx.infer_expr(arg, self_name, generics, locals, null);
            }
            return resolve_method_call(ctx, recv_type, base.name, arg_types, self_name, generics, locals, id);
        }
        const arg_types = ctx.allocator.alloc(TypeId, arg_count) catch return ctx.types.new_var();
        defer ctx.allocator.free(arg_types);
        for (arg_ids, 0..) |arg, idx2| {
            arg_types[idx2] = ctx.infer_expr(arg, self_name, generics, locals, null);
        }
        return resolve_overload(ctx, base.name, arg_types, self_name, generics, locals, id, null);
    }

    return ctx.types.new_var();
}

fn infer_access(ctx: *typecheck_ctx, left_id: uir_mod.uir_identifier, right_id: uir_mod.uir_identifier, self_name: ?[]const u8, generics: *string_map(TypeId), locals: *string_map(TypeId)) TypeId {
    const base_type = ctx.infer_expr(left_id, self_name, generics, locals, null);
    const resolved = ctx.types.resolve(base_type);
    const node = ctx.types.types.items[resolved];
    if (node == .name) {
        if (ctx.structs.get(node.name)) |info| {
            const field_name = ctx.string_value(ctx.nodes[@intCast(right_id.idx)].identifier);
            for (info.fields) |field| {
                if (std.mem.eql(u8, ctx.string_value(field.name), field_name)) {
                    return ctx.type_from_type_node(field.ty, node.name, generics);
                }
            }
            report_unknown_field(ctx, node.name, field_name, right_id);
        }
    }
    return ctx.types.new_var();
}

fn infer_index(
    ctx: *typecheck_ctx,
    id: uir_mod.uir_identifier,
    left_id: uir_mod.uir_identifier,
    right_id: uir_mod.uir_identifier,
    self_name: ?[]const u8,
    generics: *string_map(TypeId),
    locals: *string_map(TypeId),
) TypeId {
    const base_type = ctx.infer_expr(left_id, self_name, generics, locals, null);
    const base_key = ctx.to_type_key(base_type);
    if (element_type_from_container(ctx, base_key)) |elem| {
        return ctx.from_type_key(elem);
    }
    const index_type = ctx.infer_expr(right_id, self_name, generics, locals, null);
    const method = "index";
    return resolve_method_call(ctx, base_type, method, &.{index_type}, self_name, generics, locals, id);
}

fn infer_operator(
    ctx: *typecheck_ctx,
    id: uir_mod.uir_identifier,
    op: ink.binary,
    left_id: uir_mod.uir_identifier,
    right_id: uir_mod.uir_identifier,
    self_name: ?[]const u8,
    generics: *string_map(TypeId),
    locals: *string_map(TypeId),
) TypeId {
    const left = ctx.infer_expr(left_id, self_name, generics, locals, null);
    const right = ctx.infer_expr(right_id, self_name, generics, locals, null);
    const left_atomic = atomic_inner_type(ctx, left);
    const right_atomic = atomic_inner_type(ctx, right);
    const left_inner = left_atomic orelse left;
    const right_inner = right_atomic orelse right;
    const has_atomic = left_atomic != null or right_atomic != null;
    if (binary_operator_method_name(op)) |method| {
        const left_key = ctx.to_type_key(left_inner);
        const left_base = type_key_base_name(left_key);
        const left_non_builtin = blk: {
            if (left_key != .unknown) break :blk left_base != null and !is_builtin_type(left_base.?);
            const resolved = ctx.types.resolve(left_inner);
            if (ctx.types.types.items[resolved] == .tvar and ctx.types.types.items[resolved].tvar.constraints.items.len > 0) {
                break :blk true;
            }
            break :blk false;
        };
        const call_id = if (left_non_builtin) id else null;
        const method_ty = resolve_method_call(ctx, left_inner, method, &.{right_inner}, self_name, generics, locals, call_id);
        if (ctx.types.types.items[ctx.types.resolve(method_ty)] != .tvar) {
            return if (has_atomic) wrap_atomic(ctx, method_ty) else method_ty;
        }
        if (left_non_builtin) {
            const fallback = switch (op) {
                .equal,
                .not_equal,
                .less_than,
                .less_or_equal,
                .greater_than,
                .greater_or_equal,
                .logical_and,
                .logical_or,
                .logical_xor,
                => ctx.types.named("bool"),
                else => left_inner,
            };
            return if (has_atomic) wrap_atomic(ctx, fallback) else fallback;
        }
    }
    switch (op) {
        .equal, .not_equal, .less_than, .less_or_equal, .greater_than, .greater_or_equal => {
            const left_key = ctx.to_type_key(left_inner);
            const right_key = ctx.to_type_key(right_inner);
            if (!ctx.types.unify(left_inner, right_inner) and left_key != .unknown and right_key != .unknown) {
                report_type_mismatch(ctx, "cannot compare", left_key, right_key, left_id);
            }
            if (op == .less_than or op == .less_or_equal or op == .greater_than or op == .greater_or_equal) {
                if (left_key != .unknown and !is_numeric_key(left_key)) {
                    report_type_mismatch(ctx, "comparison requires numeric left operand", left_key, right_key, left_id);
                }
                if (right_key != .unknown and !is_numeric_key(right_key)) {
                    report_type_mismatch(ctx, "comparison requires numeric right operand", left_key, right_key, right_id);
                }
            }
            const out = ctx.types.named("bool");
            return if (has_atomic) wrap_atomic(ctx, out) else out;
        },
        .logical_and, .logical_or, .logical_xor => {
            ensure_bool(ctx, left_inner, left_id);
            ensure_bool(ctx, right_inner, right_id);
            const out = ctx.types.named("bool");
            return if (has_atomic) wrap_atomic(ctx, out) else out;
        },
        else => {
            ensure_numeric(ctx, left_inner, left_id);
            ensure_numeric(ctx, right_inner, right_id);
            const left_key = ctx.to_type_key(left_inner);
            const right_key = ctx.to_type_key(right_inner);
            if (!ctx.types.unify(left_inner, right_inner) and left_key != .unknown and right_key != .unknown) {
                report_type_mismatch(ctx, "operator type mismatch", left_key, right_key, id);
            }
            return if (has_atomic) wrap_atomic(ctx, left_inner) else left_inner;
        },
    }
}

fn collect_call_args(
    ctx: *typecheck_ctx,
    id: uir_mod.uir_identifier,
    args: *array_list(uir_mod.uir_identifier),
    base_out: *uir_mod.uir_identifier,
) bool {
    var base_id = id;
    var saw_call = false;
    while (true) {
        const node = ctx.nodes[@intCast(base_id.idx)];
        if (node == .binary and node.binary.op == .call) {
            args.append(node.binary.right) catch return false;
            saw_call = true;
            base_id = node.binary.left;
            continue;
        }
        break;
    }
    base_out.* = base_id;
    if (!saw_call) return false;
    std.mem.reverse(uir_mod.uir_identifier, args.items);
    if (args.items.len == 1 and is_unit_expr(ctx, args.items[0])) {
        args.items.len = 0;
    }
    return true;
}

fn infer_unary(ctx: *typecheck_ctx, un: uir_unary, self_name: ?[]const u8, generics: *string_map(TypeId), locals: *string_map(TypeId), return_type: ?TypeId) TypeId {
    const right = ctx.infer_expr(un.right, self_name, generics, locals, return_type);
    switch (un.op) {
        .borrow => return ctx.types.ref_type(right, false),
        .borrow_mut => return ctx.types.ref_type(right, true),
        .ref => return ctx.types.ref_type(right, false),
        .ref_mut => return ctx.types.ref_type(right, true),
        .deref => {
            if (atomic_inner_type(ctx, right)) |inner| return inner;
            const resolved = ctx.types.resolve(right);
            const node = ctx.types.types.items[resolved];
            if (node == .ref) return node.ref.inner;
            if (node == .applied and std.mem.eql(u8, node.applied.base, "box") and node.applied.args.len >= 1) {
                return node.applied.args[0];
            }
            return ctx.types.named("int");
        },
        .box => return ctx.types.applied("box", &.{right}),
        .sleep => {
            ensure_deadline_source(ctx, right, un.right);
            return ctx.types.applied("result", &.{ ctx.types.named("int"), ctx.types.named("io_error") });
        },
        .timeout => {
            ensure_duration(ctx, right, un.right);
            return ctx.types.named("deadline");
        },
        .deadline => {
            ensure_deadline_source(ctx, right, un.right);
            return ctx.types.named("deadline");
        },
        .spawn => {
            var args = array_list(uir_mod.uir_identifier).init(ctx.allocator);
            defer args.deinit();
            var base_id = un.right;
            if (!collect_call_args(ctx, un.right, &args, &base_id)) {
                ctx.diags.append(.{
                    .danger = .@"error",
                    .message = "spawn expects a call expression",
                    .span = ctx.span_for_node(un.right),
                    .source_id = ctx.source_for_node(un.right),
                }) catch {};
                return wrap_task(ctx, right);
            }
            if (infer_call_base(ctx, base_id)) |base| {
                if (base.receiver) |recv_id| {
                    const recv_ty = ctx.infer_expr(recv_id, self_name, generics, locals, return_type);
                    const recv_key = ctx.to_type_key(recv_ty);
                    if (recv_key != .unknown and !type_satisfies_trait_root(ctx, recv_key, "send")) {
                        report_trait_violation(ctx, "spawn requires send receiver", recv_key, "send", recv_id);
                    }
                }
            }
            for (args.items) |arg_id| {
                const arg_ty = ctx.infer_expr(arg_id, self_name, generics, locals, return_type);
                const arg_key = ctx.to_type_key(arg_ty);
                if (arg_key == .unknown) continue;
                if (!type_satisfies_trait_root(ctx, arg_key, "send")) {
                    report_trait_violation(ctx, "spawn requires send argument", arg_key, "send", arg_id);
                }
            }
            const ret_key = ctx.to_type_key(right);
            if (ret_key != .unknown and !type_satisfies_trait_root(ctx, ret_key, "send")) {
                report_trait_violation(ctx, "spawn requires send return", ret_key, "send", un.right);
            }
            return wrap_task(ctx, right);
        },
        .await => {
            if (task_inner_type(ctx, right)) |inner| return inner;
            const inner = ctx.types.new_var();
            _ = ctx.types.unify(right, wrap_task(ctx, inner));
            return inner;
        },
        .not => {
            if (atomic_inner_type(ctx, right)) |inner| {
                ensure_bool(ctx, inner, un.right);
                return wrap_atomic(ctx, ctx.types.named("bool"));
            }
            ensure_bool(ctx, right, un.right);
            return ctx.types.named("bool");
        },
        .bit_not, .neg => {
            if (atomic_inner_type(ctx, right)) |inner| {
                ensure_numeric(ctx, inner, un.right);
                return wrap_atomic(ctx, inner);
            }
            ensure_numeric(ctx, right, un.right);
            return right;
        },
        .ret => {
            if (return_type) |ret| _ = ctx.types.unify(ret, right);
            return right;
        },
        else => return right,
    }
}

fn infer_assign(ctx: *typecheck_ctx, left_id: uir_mod.uir_identifier, right_id: uir_mod.uir_identifier, self_name: ?[]const u8, generics: *string_map(TypeId), locals: *string_map(TypeId), return_type: ?TypeId) TypeId {
    const right = ctx.infer_expr(right_id, self_name, generics, locals, return_type);
    const left = ctx.infer_expr(left_id, self_name, generics, locals, return_type);
    if (atomic_inner_type(ctx, left)) |inner| {
        if (atomic_inner_type(ctx, right)) |right_inner| {
            if (!ctx.types.unify(inner, right_inner)) {
                const expected_key = ctx.to_type_key(inner);
                const actual_key = ctx.to_type_key(right_inner);
                if (expected_key != .unknown and actual_key != .unknown) {
                    report_type_mismatch(ctx, "assignment type mismatch", expected_key, actual_key, right_id);
                }
            }
        } else {
            if (!ctx.types.unify(inner, right)) {
                const expected_key = ctx.to_type_key(inner);
                const actual_key = ctx.to_type_key(right);
                if (expected_key != .unknown and actual_key != .unknown) {
                    report_type_mismatch(ctx, "assignment type mismatch", expected_key, actual_key, right_id);
                }
            }
        }
        return left;
    }
    if (!ctx.types.unify(left, right)) {
        const expected_key = ctx.to_type_key(left);
        const actual_key = ctx.to_type_key(right);
        if (expected_key != .unknown and actual_key != .unknown) {
            report_type_mismatch(ctx, "assignment type mismatch", expected_key, actual_key, right_id);
        }
    }
    return left;
}

fn infer_decl(ctx: *typecheck_ctx, decl: uir_decl, self_name: ?[]const u8, generics: *string_map(TypeId), locals: *string_map(TypeId), return_type: ?TypeId) TypeId {
    switch (decl) {
        .@"const" => |c| {
            const value = ctx.infer_expr(c.value, self_name, generics, locals, return_type);
            if (c.ty) |ty| {
                const annotation = ctx.type_from_type_node(ty, self_name, generics);
                if (!ctx.types.unify(value, annotation)) {
                    const expected_key = ctx.to_type_key(annotation);
                    const actual_key = ctx.to_type_key(value);
                    if (expected_key != .unknown and actual_key != .unknown) {
                        report_type_mismatch(ctx, "type annotation mismatch", expected_key, actual_key, c.value);
                    }
                }
                locals.put(ctx.string_value(c.name), annotation) catch {};
                return annotation;
            }
            locals.put(ctx.string_value(c.name), value) catch {};
            return value;
        },
        .@"var" => |v| {
            const value = ctx.infer_expr(v.value, self_name, generics, locals, return_type);
            if (v.ty) |ty| {
                const annotation = ctx.type_from_type_node(ty, self_name, generics);
                if (!ctx.types.unify(value, annotation)) {
                    const expected_key = ctx.to_type_key(annotation);
                    const actual_key = ctx.to_type_key(value);
                    if (expected_key != .unknown and actual_key != .unknown) {
                        report_type_mismatch(ctx, "type annotation mismatch", expected_key, actual_key, v.value);
                    }
                }
                locals.put(ctx.string_value(v.name), annotation) catch {};
                return annotation;
            }
            locals.put(ctx.string_value(v.name), value) catch {};
            return value;
        },
        else => return ctx.types.new_var(),
    }
}

fn infer_block(ctx: *typecheck_ctx, items: []const uir_mod.uir_identifier, self_name: ?[]const u8, generics: *string_map(TypeId), locals: *string_map(TypeId), return_type: ?TypeId) TypeId {
    var last = ctx.types.named("unit");
    for (items) |item| {
        last = ctx.infer_expr(item, self_name, generics, locals, return_type);
    }
    return last;
}

fn bind_pattern_type(
    ctx: *typecheck_ctx,
    pattern_id: uir_mod.uir_identifier,
    elem_type: TypeId,
    locals: *string_map(TypeId),
) void {
    const node = ctx.nodes[@intCast(pattern_id.idx)];
    if (node != .identifier) return;
    const name = ctx.string_value(node.identifier);
    if (std.mem.eql(u8, name, "_") or std.mem.eql(u8, name, "*")) return;
    locals.put(name, elem_type) catch {};
}

fn infer_loop_like(
    ctx: *typecheck_ctx,
    label: ?[]const u8,
    body_id: uir_mod.uir_identifier,
    self_name: ?[]const u8,
    generics: *string_map(TypeId),
    locals: *string_map(TypeId),
    return_type: ?TypeId,
) TypeId {
    const result = ctx.types.new_var();
    ctx.push_loop(label, result, true);
    _ = ctx.infer_expr(body_id, self_name, generics, locals, return_type);
    ctx.pop_loop();
    default_loop_result(ctx, result);
    return result;
}

fn infer_label_block(
    ctx: *typecheck_ctx,
    label: []const u8,
    body_id: uir_mod.uir_identifier,
    self_name: ?[]const u8,
    generics: *string_map(TypeId),
    locals: *string_map(TypeId),
    return_type: ?TypeId,
) TypeId {
    const result = ctx.types.new_var();
    ctx.push_loop(label, result, false);
    const body_ty = ctx.infer_expr(body_id, self_name, generics, locals, return_type);
    _ = ctx.types.unify(result, body_ty);
    ctx.pop_loop();
    default_loop_result(ctx, result);
    return result;
}

fn infer_if(ctx: *typecheck_ctx, ife: uir_if, self_name: ?[]const u8, generics: *string_map(TypeId), locals: *string_map(TypeId), return_type: ?TypeId) TypeId {
    const cond = ctx.infer_expr(ife.condition, self_name, generics, locals, return_type);
    ensure_bool(ctx, cond, ife.condition);
    const then_ty = ctx.infer_expr(ife.then_branch, self_name, generics, locals, return_type);
    if (ife.else_branch) |ref| {
        const else_ty = ctx.infer_expr(ref, self_name, generics, locals, return_type);
        _ = ctx.types.unify(then_ty, else_ty);
    }
    return then_ty;
}

fn infer_intrinsic(ctx: *typecheck_ctx, call: uir_intrinsic, self_name: ?[]const u8, generics: *string_map(TypeId), locals: *string_map(TypeId)) TypeId {
    const name = ctx.string_value(call.name);
    for (call.args) |arg| {
        _ = ctx.infer_expr(arg, self_name, generics, locals, null);
    }
    if (std.mem.eql(u8, name, "alloc")) return ctx.types.named("int");
    if (std.mem.eql(u8, name, "free")) return ctx.types.named("unit");
    if (std.mem.eql(u8, name, "deref")) return ctx.types.named("int");
    if (std.mem.eql(u8, name, "type_words")) return ctx.types.named("int");
    if (std.mem.eql(u8, name, "store")) return ctx.types.named("unit");
    if (std.mem.eql(u8, name, "result_ok")) return ctx.types.named("int");
    if (std.mem.eql(u8, name, "result_err")) return ctx.types.named("int");
    if (std.mem.eql(u8, name, "result_is_ok")) return ctx.types.named("bool");
    if (std.mem.eql(u8, name, "result_unwrap")) return ctx.types.new_var();
    if (std.mem.eql(u8, name, "result_unwrap_err")) return ctx.types.new_var();
    if (std.mem.eql(u8, name, "interpolate")) return ctx.types.named("string");
    return ctx.types.new_var();
}

fn infer_record_literal(ctx: *typecheck_ctx, rec: uir_record_literal, self_name: ?[]const u8, generics: *string_map(TypeId), locals: *string_map(TypeId)) TypeId {
    const type_name = ctx.string_value(rec.type_name);
    const ty = ctx.types.named(type_name);
    if (ctx.structs.get(type_name)) |info| {
        for (rec.fields) |field| {
            var found = false;
            for (info.fields) |decl_field| {
                if (!std.mem.eql(u8, ctx.string_value(decl_field.name), ctx.string_value(field.name))) continue;
                found = true;
                const value_ty = ctx.infer_expr(field.value, self_name, generics, locals, null);
                const field_ty = ctx.type_from_type_node(decl_field.ty, type_name, generics);
                if (!ctx.types.unify(value_ty, field_ty)) {
                    report_type_mismatch(
                        ctx,
                        "field type mismatch",
                        ctx.to_type_key(field_ty),
                        ctx.to_type_key(value_ty),
                        field.value,
                    );
                }
                break;
            }
            if (!found) {
                report_unknown_field(ctx, type_name, ctx.string_value(field.name), field.value);
            }
        }
    }
    return ty;
}

fn infer_decl_node(ctx: *typecheck_ctx, node: uir_mod.uir, self_name: ?[]const u8, generics: *string_map(TypeId), locals: *string_map(TypeId), return_type: ?TypeId) TypeId {
    return switch (node) {
        .decl => |decl| infer_decl(ctx, decl, self_name, generics, locals, return_type),
        else => ctx.types.new_var(),
    };
}

fn infer_node(ctx: *typecheck_ctx, id: uir_mod.uir_identifier, self_name: ?[]const u8, generics: *string_map(TypeId), locals: *string_map(TypeId), return_type: ?TypeId) TypeId {
    const node = ctx.nodes[@intCast(id.idx)];
    return switch (node) {
        .integer => ctx.types.named("int"),
        .float => ctx.types.named("float"),
        .duration => ctx.types.named("duration"),
        .boolean => ctx.types.named("bool"),
        .string => ctx.types.named("string"),
        .identifier => |ident| {
            const name = ctx.string_value(ident);
            if (locals.get(name)) |ty| return ty;
            return ctx.types.new_var();
        },
        .unary => |un| infer_unary(ctx, un, self_name, generics, locals, return_type),
        .binary => |bin| switch (bin.op) {
            .call => infer_call(ctx, id, self_name, generics, locals),
            .access => infer_access(ctx, bin.left, bin.right, self_name, generics, locals),
            .index => infer_index(ctx, id, bin.left, bin.right, self_name, generics, locals),
            .@"as" => blk: {
                const left_ty = ctx.infer_expr(bin.left, self_name, generics, locals, return_type);
                const target_ty = ctx.type_from_type_node(bin.right, self_name, generics);
                const target_res = ctx.types.resolve(target_ty);
                const target_node = ctx.types.types.items[target_res];
                const left_key = type_key_from_type_id(ctx, left_ty);
                const target_key = type_key_from_type_id(ctx, target_ty);
                var dyn_name: ?[]const u8 = null;
                switch (target_node) {
                    .dyn_trait => |name| dyn_name = name,
                    .applied => |ap| {
                        if (std.mem.eql(u8, ap.base, "dyn") and ap.args.len >= 1) {
                            dyn_name = ctx.type_name_from_type_id(ap.args[0]);
                        }
                    },
                    else => {},
                }
                if (dyn_name) |name| {
                    const constraint = trait_constraint{ .name = name, .negative = false };
                    if (left_key != .unknown and !constraint_satisfied(ctx, left_key, constraint)) {
                        report_type_mismatch(ctx, "cannot cast to dyn", left_key, target_key, id);
                    }
                }
                break :blk target_ty;
            },
            .assign,
            .assign_add,
            .assign_sub,
            .assign_mul,
            .assign_div,
            .assign_mod,
            .assign_bit_and,
            .assign_bit_or,
            .assign_bit_xor,
            .assign_shl,
            .assign_shr,
            => infer_assign(ctx, bin.left, bin.right, self_name, generics, locals, return_type),
            else => infer_operator(ctx, id, bin.op, bin.left, bin.right, self_name, generics, locals),
        },
        .label_expr => |le| blk: {
            const label_name = ctx.string_value(le.name);
            const body_node = ctx.nodes[@intCast(le.body.idx)];
            switch (body_node) {
                .loop_expr,
                .while_expr,
                .while_in_expr,
                .until_expr,
                .repeat_expr,
                .for_expr,
                .each_expr,
                => {
                    const prev = ctx.pending_label;
                    ctx.pending_label = label_name;
                    const out = ctx.infer_expr(le.body, self_name, generics, locals, return_type);
                    ctx.pending_label = prev;
                    break :blk out;
                },
                else => break :blk infer_label_block(ctx, label_name, le.body, self_name, generics, locals, return_type),
            }
        },
        .loop_expr => |le| blk: {
            const label = ctx.pending_label;
            ctx.pending_label = null;
            break :blk infer_loop_like(ctx, label, le.body, self_name, generics, locals, return_type);
        },
        .while_expr => |we| blk: {
            const label = ctx.pending_label;
            ctx.pending_label = null;
            const cond = ctx.infer_expr(we.condition, self_name, generics, locals, return_type);
            ensure_bool(ctx, cond, we.condition);
            break :blk infer_loop_like(ctx, label, we.body, self_name, generics, locals, return_type);
        },
        .while_in_expr => |we| blk: {
            const label = ctx.pending_label;
            ctx.pending_label = null;
            const iter_ty = ctx.infer_expr(we.iter, self_name, generics, locals, return_type);
            const iter_key = ctx.to_type_key(iter_ty);
            const elem_ty = if (element_type_from_container(ctx, iter_key)) |elem| ctx.from_type_key(elem) else ctx.types.new_var();
            bind_pattern_type(ctx, we.pattern, elem_ty, locals);
            break :blk infer_loop_like(ctx, label, we.body, self_name, generics, locals, return_type);
        },
        .until_expr => |ue| blk: {
            const label = ctx.pending_label;
            ctx.pending_label = null;
            const cond = ctx.infer_expr(ue.condition, self_name, generics, locals, return_type);
            ensure_bool(ctx, cond, ue.condition);
            break :blk infer_loop_like(ctx, label, ue.body, self_name, generics, locals, return_type);
        },
        .repeat_expr => |re| blk: {
            const label = ctx.pending_label;
            ctx.pending_label = null;
            const count = ctx.infer_expr(re.count, self_name, generics, locals, return_type);
            ensure_numeric(ctx, count, re.count);
            break :blk infer_loop_like(ctx, label, re.body, self_name, generics, locals, return_type);
        },
        .for_expr => |fe| blk: {
            const label = ctx.pending_label;
            ctx.pending_label = null;
            const iter_ty = ctx.infer_expr(fe.iter, self_name, generics, locals, return_type);
            const iter_key = ctx.to_type_key(iter_ty);
            const elem_ty = if (element_type_from_container(ctx, iter_key)) |elem| ctx.from_type_key(elem) else ctx.types.new_var();
            bind_pattern_type(ctx, fe.pattern, elem_ty, locals);
            break :blk infer_loop_like(ctx, label, fe.body, self_name, generics, locals, return_type);
        },
        .each_expr => |ee| blk: {
            const label = ctx.pending_label;
            ctx.pending_label = null;
            const iter_ty = ctx.infer_expr(ee.iter, self_name, generics, locals, return_type);
            const iter_key = ctx.to_type_key(iter_ty);
            const elem_ty = if (element_type_from_container(ctx, iter_key)) |elem| ctx.from_type_key(elem) else ctx.types.new_var();
            bind_pattern_type(ctx, ee.pattern, elem_ty, locals);
            break :blk infer_loop_like(ctx, label, ee.body, self_name, generics, locals, return_type);
        },
        .break_expr => |be| blk: {
            const label = if (be.label) |lab| ctx.string_value(lab) else null;
            const scope = ctx.find_break_scope(label);
            if (scope == null) {
                ctx.diags.append(.{
                    .danger = .@"error",
                    .message = "break used outside of a loop",
                    .span = ctx.span_for_node(id),
                    .source_id = ctx.source_for_node(id),
                }) catch {};
                break :blk ctx.types.named("unit");
            }
            if (be.value) |ref| {
                const value_ty = ctx.infer_expr(ref, self_name, generics, locals, return_type);
                _ = ctx.types.unify(scope.?.result, value_ty);
            } else {
                _ = ctx.types.unify(scope.?.result, ctx.types.named("unit"));
            }
            break :blk scope.?.result;
        },
        .continue_expr => |ce| blk: {
            const label = if (ce.label) |lab| ctx.string_value(lab) else null;
            if (ctx.find_continue_scope(label) == null) {
                ctx.diags.append(.{
                    .danger = .@"error",
                    .message = "continue used outside of a loop",
                    .span = ctx.span_for_node(id),
                    .source_id = ctx.source_for_node(id),
                }) catch {};
            }
            break :blk ctx.types.named("unit");
        },
        .yield_expr => |ye| blk: {
            if (ye.value) |ref| _ = ctx.infer_expr(ref, self_name, generics, locals, return_type);
            break :blk ctx.types.named("unit");
        },
        .atomic_expr => |ae| blk: {
            const inner = ctx.infer_expr(ae.value, self_name, generics, locals, return_type);
            break :blk wrap_atomic(ctx, inner);
        },
        .block => |items| infer_block(ctx, items, self_name, generics, locals, return_type),
        .if_expr => |ife| infer_if(ctx, ife, self_name, generics, locals, return_type),
        .intrinsic => |call| infer_intrinsic(ctx, call, self_name, generics, locals),
        .record_literal => |rec| infer_record_literal(ctx, rec, self_name, generics, locals),
        .decl => infer_decl_node(ctx, node, self_name, generics, locals, return_type),
        else => ctx.types.new_var(),
    };
}

fn resolve_type_vars(ctx: *typecheck_ctx, id: TypeId) type_key {
    return ctx.to_type_key(id);
}

fn pick_better(score: usize, best_score: ?usize) bool {
    if (best_score == null) return true;
    return score > best_score.?;
}

fn match_score(ctx: *typecheck_ctx, arg_types: []const TypeId) usize {
    var score: usize = 0;
    for (arg_types) |arg| {
        const resolved = ctx.types.resolve(arg);
        switch (ctx.types.types.items[resolved]) {
            .tvar => {},
            .dyn_trait => score += 1,
            .name, .applied, .ref => score += 2,
        }
    }
    return score;
}

fn resolve_overload(
    ctx: *typecheck_ctx,
    name: []const u8,
    arg_types: []const TypeId,
    self_name: ?[]const u8,
    generics: *string_map(TypeId),
    locals: *string_map(TypeId),
    call_id: ?uir_mod.uir_identifier,
    receiver: ?TypeId,
) TypeId {
    const group = ctx.functions.get(name) orelse {
        if (call_id) |id| {
            if (!is_qualified_name(name)) {
                report_unknown_call(ctx, name, arg_types, receiver, id);
            }
        }
        return ctx.types.new_var();
    };
    var best_idx: ?usize = null;
    var best_score: ?usize = null;
    var constraint_fail: ?constraint_failure = null;
    var arg_fail: ?arg_mismatch = null;
    var arity_options = std.array_list.Managed(usize).init(ctx.allocator);
    defer arity_options.deinit();

    for (group.items, 0..) |info, idx| {
        const checkpoint = ctx.types.checkpoint();
        var fail: call_failure = undefined;
        if (!ctx.try_unify_call(info, arg_types, self_name, generics, locals, &fail)) {
            switch (fail) {
                .constraint => |con| {
                    if (constraint_fail == null) constraint_fail = con;
                },
                .arg_mismatch => |arg| {
                    if (arg_fail == null) arg_fail = arg;
                },
                .arity => |arity| {
                    var seen = false;
                    for (arity_options.items) |count| {
                        if (count == arity.expected) {
                            seen = true;
                            break;
                        }
                    }
                    if (!seen) arity_options.append(arity.expected) catch {};
                },
            }
            ctx.types.rollback(checkpoint);
            continue;
        }
        const score = match_score(ctx, arg_types);
        if (pick_better(score, best_score)) {
            best_idx = idx;
            best_score = score;
        }
        ctx.types.rollback(checkpoint);
    }

    if (best_idx == null) {
        if (call_id) |id| {
            var fail: ?call_failure = null;
            if (constraint_fail) |con| {
                fail = .{ .constraint = con };
            } else if (arg_fail) |arg| {
                fail = .{ .arg_mismatch = arg };
            } else if (arity_options.items.len > 0) {
                fail = .{ .arity = .{ .expected = arity_options.items[0], .found = arg_types.len } };
            }
            report_call_failure(ctx, name, arg_types, receiver, id, fail, arity_options.items);
        }
        return ctx.types.new_var();
    }
    _ = ctx.try_unify_call(group.items[best_idx.?], arg_types, self_name, generics, locals, null);
    return ctx.return_type_for(group.items[best_idx.?], self_name, generics);
}

fn report_ambiguous_method(
    ctx: *typecheck_ctx,
    name: []const u8,
    recv_type: TypeId,
    call_id: uir_mod.uir_identifier,
) void {
    var buf = array_list(u8).init(ctx.allocator);
    defer buf.deinit();
    _ = buf.appendSlice("ambiguous method ") catch return;
    _ = buf.appendSlice(name) catch return;
    _ = buf.appendSlice(" for ") catch return;
    append_type_key(ctx, &buf, ctx.to_type_key(recv_type));
    const full = buf.toOwnedSlice() catch return;
    ctx.diags.append(.{
        .danger = .@"error",
        .message = full,
        .span = ctx.span_for_node(call_id),
        .source_id = ctx.source_for_node(call_id),
    }) catch {};
}

fn trait_method_return_type(
    ctx: *typecheck_ctx,
    trait_name: []const u8,
    method_name: []const u8,
    recv_type: TypeId,
    generics: *string_map(TypeId),
    visited: *string_map(void),
) ?TypeId {
    if (visited.contains(trait_name)) return null;
    visited.put(trait_name, {}) catch return null;

    const info = ctx.traits.get(trait_name) orelse return null;
    for (info.methods) |method| {
        if (!std.mem.eql(u8, method.name, method_name)) continue;
        if (method.return_type) |ret| {
            const ret_node = ctx.nodes[@intCast(ret.idx)];
            if (ret_node == .type and ret_node.type == .self) {
                return recv_type;
            }
            var local_generics = string_map(TypeId).init(ctx.allocator);
            defer local_generics.deinit();

            var it = generics.iterator();
            while (it.next()) |entry| {
                local_generics.put(entry.key_ptr.*, entry.value_ptr.*) catch {};
            }

            var bound_recv = false;
            for (info.generics) |param| {
                if (param.kind != .type) continue;
                const param_name = ctx.string_value(param.name);
                if (local_generics.contains(param_name)) continue;
                if (!bound_recv) {
                    local_generics.put(param_name, recv_type) catch {};
                    bound_recv = true;
                } else {
                    local_generics.put(param_name, ctx.types.new_var()) catch {};
                }
            }
            return ctx.type_from_type_node(ret, trait_name, &local_generics);
        }
        return ctx.types.named("unit");
    }

    for (info.requires) |req| {
        if (req.negative) continue;
        if (trait_method_return_type(ctx, req.name, method_name, recv_type, generics, visited)) |ret| {
            return ret;
        }
    }

    return null;
}

fn resolve_method_call(
    ctx: *typecheck_ctx,
    recv_type: TypeId,
    name: []const u8,
    arg_types: []const TypeId,
    self_name: ?[]const u8,
    generics: *string_map(TypeId),
    locals: *string_map(TypeId),
    call_id: ?uir_mod.uir_identifier,
) TypeId {
    const recv_resolved = ctx.types.resolve(recv_type);
    const recv_node = ctx.types.types.items[recv_resolved];
    switch (recv_node) {
        .tvar => |tv| {
            var found: ?TypeId = null;
            var ambiguous = false;
            for (tv.constraints.items) |constraint| {
                if (constraint.negative) continue;
                var visited = string_map(void).init(ctx.allocator);
                defer visited.deinit();
                if (trait_method_return_type(ctx, constraint.name, name, recv_type, generics, &visited)) |ret| {
                    if (found == null) {
                        found = ret;
                    } else {
                        ambiguous = true;
                        break;
                    }
                }
            }
            if (ambiguous) {
                if (call_id) |id| report_ambiguous_method(ctx, name, recv_type, id);
                return ctx.types.new_var();
            }
            if (found) |ret| return ret;
            if (call_id) |id| {
                var call_args = std.array_list.Managed(TypeId).init(ctx.allocator);
                defer call_args.deinit();
                call_args.append(recv_type) catch {};
                for (arg_types) |arg| call_args.append(arg) catch {};
                report_unknown_call(ctx, name, call_args.items, recv_type, id);
            }
            return ctx.types.new_var();
        },
        .dyn_trait => |trait_name| {
            if (ctx.traits.get(trait_name)) |info| {
                for (info.methods) |method| {
                    if (!std.mem.eql(u8, method.name, name)) continue;
                    if (method.return_type) |ret| {
                        return ctx.type_from_type_node(ret, trait_name, generics);
                    }
                    break;
                }
            }
            if (call_id) |id| {
                var call_args = std.array_list.Managed(TypeId).init(ctx.allocator);
                defer call_args.deinit();
                call_args.append(recv_type) catch {};
                for (arg_types) |arg| call_args.append(arg) catch {};
                report_unknown_call(ctx, name, call_args.items, recv_type, id);
            }
            return ctx.types.new_var();
        },
        .applied => |ap| {
            if (std.mem.eql(u8, ap.base, "dyn") and ap.args.len >= 1) {
                if (ctx.type_name_from_type_id(ap.args[0])) |trait_name| {
                    if (ctx.traits.get(trait_name)) |info| {
                        for (info.methods) |method| {
                            if (!std.mem.eql(u8, method.name, name)) continue;
                            if (method.return_type) |ret| {
                                return ctx.type_from_type_node(ret, trait_name, generics);
                            }
                            break;
                        }
                    }
                    if (call_id) |id| {
                        var call_args = std.array_list.Managed(TypeId).init(ctx.allocator);
                        defer call_args.deinit();
                        call_args.append(recv_type) catch {};
                        for (arg_types) |arg| call_args.append(arg) catch {};
                        report_unknown_call(ctx, name, call_args.items, recv_type, id);
                    }
                    return ctx.types.new_var();
                }
            }
        },
        .name => |trait_name| {
            if (ctx.traits.contains(trait_name)) {
                if (ctx.traits.get(trait_name)) |info| {
                    for (info.methods) |method| {
                        if (!std.mem.eql(u8, method.name, name)) continue;
                        if (method.return_type) |ret| {
                            return ctx.type_from_type_node(ret, trait_name, generics);
                        }
                        break;
                    }
                }
                if (call_id) |id| {
                    var call_args = std.array_list.Managed(TypeId).init(ctx.allocator);
                    defer call_args.deinit();
                    call_args.append(recv_type) catch {};
                    for (arg_types) |arg| call_args.append(arg) catch {};
                    report_unknown_call(ctx, name, call_args.items, recv_type, id);
                }
                return ctx.types.new_var();
            }
        },
        else => {},
    }

    var call_args = std.array_list.Managed(TypeId).init(ctx.allocator);
    defer call_args.deinit();
    call_args.append(recv_type) catch return ctx.types.new_var();
    for (arg_types) |arg| call_args.append(arg) catch return ctx.types.new_var();
    return resolve_overload(ctx, name, call_args.items, self_name, generics, locals, call_id, recv_type);
}

fn type_name_from_type_key(key: type_key) ?[]const u8 {
    return switch (key) {
        .name => |name| name,
        .dyn_trait => |name| name,
        .applied => |ap| ap.base,
        else => null,
    };
}

fn dyn_base_from_type_key(key: type_key) ?[]const u8 {
    return switch (key) {
        .dyn_trait => |name| name,
        .applied => |ap| blk: {
            if (!std.mem.eql(u8, ap.base, "dyn")) break :blk null;
            if (ap.args.len < 1) break :blk null;
            break :blk type_name_from_type_key(ap.args[0]);
        },
        else => null,
    };
}

fn dyn_has_positive(key: type_key, name: []const u8) bool {
    if (key == .dyn_trait) return std.mem.eql(u8, key.dyn_trait, name);
    if (key != .applied) return false;
    const ap = key.applied;
    if (!std.mem.eql(u8, ap.base, "dyn")) return false;
    if (ap.args.len == 0) return false;
    if (type_name_from_type_key(ap.args[0])) |base_name| {
        if (std.mem.eql(u8, base_name, name)) return true;
    }
    for (ap.args[1..]) |arg| {
        if (arg == .applied and std.mem.eql(u8, arg.applied.base, "not")) continue;
        if (type_name_from_type_key(arg)) |arg_name| {
            if (std.mem.eql(u8, arg_name, name)) return true;
        }
    }
    return false;
}

fn dyn_has_negative(key: type_key, name: []const u8) bool {
    if (key != .applied) return false;
    const ap = key.applied;
    if (!std.mem.eql(u8, ap.base, "dyn")) return false;
    for (ap.args[1..]) |arg| {
        if (arg == .applied and std.mem.eql(u8, arg.applied.base, "not") and arg.applied.args.len >= 1) {
            if (type_name_from_type_key(arg.applied.args[0])) |inner_name| {
                if (std.mem.eql(u8, inner_name, name)) return true;
            }
        }
    }
    return false;
}

fn type_has_impl(ctx: *typecheck_ctx, trait_name: []const u8, type_name: []const u8) bool {
    const impls = ctx.trait_impls.get(trait_name) orelse return false;
    for (impls.items) |impl_name| {
        if (std.mem.eql(u8, impl_name, type_name)) return true;
    }
    return false;
}

fn type_has_neg_impl(ctx: *typecheck_ctx, trait_name: []const u8, type_name: []const u8) bool {
    const impls = ctx.trait_neg_impls.get(trait_name) orelse return false;
    for (impls.items) |impl_name| {
        if (std.mem.eql(u8, impl_name, type_name)) return true;
    }
    return false;
}

fn auto_trait_struct_fields(
    ctx: *typecheck_ctx,
    struct_name: []const u8,
    info: struct_info,
    args: ?[]const type_key,
    trait_name: []const u8,
    visited_traits: *array_list([]const u8),
    visited_types: *string_map(void),
) bool {
    if (visited_types.contains(struct_name)) return true;
    visited_types.put(struct_name, {}) catch return true;
    defer _ = visited_types.remove(struct_name);

    var local_generics = string_map(TypeId).init(ctx.allocator);
    defer local_generics.deinit();
    if (args) |arg_list| {
        var arg_idx: usize = 0;
        for (info.generics) |gen| {
            if (arg_idx >= arg_list.len) break;
            if (gen.kind == .type) {
                const name = ctx.string_value(gen.name);
                const var_id = ctx.from_type_key(arg_list[arg_idx]);
                local_generics.put(name, var_id) catch return true;
            }
            arg_idx += 1;
        }
    }

    for (info.fields) |field| {
        const field_ty_id = ctx.type_from_type_node(field.ty, struct_name, &local_generics);
        const field_key = type_key_from_type_id(ctx, field_ty_id);
        if (!type_satisfies_trait(ctx, field_key, trait_name, visited_traits, visited_types)) return false;
    }
    return true;
}

fn auto_trait_structural(
    ctx: *typecheck_ctx,
    ty: type_key,
    trait_name: []const u8,
    visited_traits: *array_list([]const u8),
    visited_types: *string_map(void),
) bool {
    if (ty == .unknown) return true;
    return switch (ty) {
        .name => |name| blk: {
            if (ctx.structs.get(name)) |info| {
                break :blk auto_trait_struct_fields(ctx, name, info, null, trait_name, visited_traits, visited_types);
            }
            break :blk true;
        },
        .applied => |ap| blk: {
            if (std.mem.eql(u8, ap.base, "array")) {
                if (ap.args.len < 2) break :blk true;
                break :blk type_satisfies_trait(ctx, ap.args[1], trait_name, visited_traits, visited_types);
            }
            if (std.mem.eql(u8, ap.base, "slice") or std.mem.eql(u8, ap.base, "optional") or std.mem.eql(u8, ap.base, "ref") or std.mem.eql(u8, ap.base, "ref_mut") or std.mem.eql(u8, ap.base, "box") or std.mem.eql(u8, ap.base, "atomic") or std.mem.eql(u8, ap.base, "task")) {
                if (ap.args.len < 1) break :blk true;
                break :blk type_satisfies_trait(ctx, ap.args[0], trait_name, visited_traits, visited_types);
            }
            if (std.mem.eql(u8, ap.base, "result")) {
                if (ap.args.len < 2) break :blk true;
                if (!type_satisfies_trait(ctx, ap.args[0], trait_name, visited_traits, visited_types)) break :blk false;
                break :blk type_satisfies_trait(ctx, ap.args[1], trait_name, visited_traits, visited_types);
            }
            if (std.mem.eql(u8, ap.base, "intersect") or std.mem.eql(u8, ap.base, "union")) {
                for (ap.args) |arg| {
                    if (!type_satisfies_trait(ctx, arg, trait_name, visited_traits, visited_types)) break :blk false;
                }
                break :blk true;
            }
            if (ctx.structs.get(ap.base)) |info| {
                break :blk auto_trait_struct_fields(ctx, ap.base, info, ap.args, trait_name, visited_traits, visited_types);
            }
            for (ap.args) |arg| {
                if (!type_satisfies_trait(ctx, arg, trait_name, visited_traits, visited_types)) break :blk false;
            }
            break :blk true;
        },
        else => true,
    };
}

fn auto_trait_send(
    ctx: *typecheck_ctx,
    ty: type_key,
    visited_traits: *array_list([]const u8),
    visited_types: *string_map(void),
) bool {
    if (ty == .unknown) return true;
    return switch (ty) {
        .name => |name| blk: {
            if (ctx.structs.get(name)) |info| {
                break :blk auto_trait_struct_fields(ctx, name, info, null, "send", visited_traits, visited_types);
            }
            break :blk true;
        },
        .applied => |ap| blk: {
            if (std.mem.eql(u8, ap.base, "ref")) {
                if (ap.args.len < 1) break :blk true;
                break :blk type_satisfies_trait(ctx, ap.args[0], "sync", visited_traits, visited_types);
            }
            if (std.mem.eql(u8, ap.base, "ref_mut")) {
                if (ap.args.len < 1) break :blk true;
                break :blk type_satisfies_trait(ctx, ap.args[0], "send", visited_traits, visited_types);
            }
            if (std.mem.eql(u8, ap.base, "slice")) {
                if (ap.args.len < 1) break :blk true;
                break :blk type_satisfies_trait(ctx, ap.args[0], "sync", visited_traits, visited_types);
            }
            if (std.mem.eql(u8, ap.base, "atomic") or std.mem.eql(u8, ap.base, "task")) {
                if (ap.args.len < 1) break :blk true;
                break :blk type_satisfies_trait(ctx, ap.args[0], "send", visited_traits, visited_types);
            }
            if (std.mem.eql(u8, ap.base, "array")) {
                if (ap.args.len < 2) break :blk true;
                break :blk type_satisfies_trait(ctx, ap.args[1], "send", visited_traits, visited_types);
            }
            break :blk auto_trait_structural(ctx, ty, "send", visited_traits, visited_types);
        },
        else => true,
    };
}

fn auto_trait_sized(
    ctx: *typecheck_ctx,
    ty: type_key,
    visited_traits: *array_list([]const u8),
    visited_types: *string_map(void),
) bool {
    if (ty == .unknown) return true;
    return switch (ty) {
        .name => |name| blk: {
            if (ctx.structs.get(name)) |info| {
                break :blk auto_trait_struct_fields(ctx, name, info, null, "sized", visited_traits, visited_types);
            }
            break :blk true;
        },
        .applied => |ap| blk: {
            if (std.mem.eql(u8, ap.base, "slice")) {
                break :blk false;
            }
            if (std.mem.eql(u8, ap.base, "ref") or std.mem.eql(u8, ap.base, "ref_mut") or std.mem.eql(u8, ap.base, "box") or std.mem.eql(u8, ap.base, "task") or std.mem.eql(u8, ap.base, "atomic")) {
                break :blk true;
            }
            if (std.mem.eql(u8, ap.base, "array")) {
                if (ap.args.len < 2) break :blk true;
                break :blk type_satisfies_trait(ctx, ap.args[1], "sized", visited_traits, visited_types);
            }
            break :blk auto_trait_structural(ctx, ty, "sized", visited_traits, visited_types);
        },
        else => true,
    };
}

fn auto_trait_sync(
    ctx: *typecheck_ctx,
    ty: type_key,
    visited_traits: *array_list([]const u8),
    visited_types: *string_map(void),
) bool {
    if (ty == .unknown) return true;
    return switch (ty) {
        .name => |name| blk: {
            if (ctx.structs.get(name)) |info| {
                break :blk auto_trait_struct_fields(ctx, name, info, null, "sync", visited_traits, visited_types);
            }
            break :blk true;
        },
        .applied => |ap| blk: {
            if (std.mem.eql(u8, ap.base, "ref")) {
                if (ap.args.len < 1) break :blk true;
                break :blk type_satisfies_trait(ctx, ap.args[0], "sync", visited_traits, visited_types);
            }
            if (std.mem.eql(u8, ap.base, "ref_mut")) {
                break :blk false;
            }
            if (std.mem.eql(u8, ap.base, "slice")) {
                if (ap.args.len < 1) break :blk true;
                break :blk type_satisfies_trait(ctx, ap.args[0], "sync", visited_traits, visited_types);
            }
            if (std.mem.eql(u8, ap.base, "atomic") or std.mem.eql(u8, ap.base, "task")) {
                if (ap.args.len < 1) break :blk true;
                break :blk type_satisfies_trait(ctx, ap.args[0], "send", visited_traits, visited_types);
            }
            if (std.mem.eql(u8, ap.base, "array")) {
                if (ap.args.len < 2) break :blk true;
                break :blk type_satisfies_trait(ctx, ap.args[1], "sync", visited_traits, visited_types);
            }
            break :blk auto_trait_structural(ctx, ty, "sync", visited_traits, visited_types);
        },
        else => true,
    };
}

fn auto_trait_satisfied(
    ctx: *typecheck_ctx,
    ty: type_key,
    trait_name: []const u8,
    visited_traits: *array_list([]const u8),
    visited_types: *string_map(void),
) bool {
    if (std.mem.eql(u8, trait_name, "sized")) return auto_trait_sized(ctx, ty, visited_traits, visited_types);
    if (std.mem.eql(u8, trait_name, "send")) return auto_trait_send(ctx, ty, visited_traits, visited_types);
    if (std.mem.eql(u8, trait_name, "sync")) return auto_trait_sync(ctx, ty, visited_traits, visited_types);
    return auto_trait_structural(ctx, ty, trait_name, visited_traits, visited_types);
}

fn type_satisfies_trait(
    ctx: *typecheck_ctx,
    ty: type_key,
    trait_name: []const u8,
    visited_traits: *array_list([]const u8),
    visited_types: *string_map(void),
) bool {
    if (ty == .unknown) return true;
    if (std.mem.eql(u8, trait_name, "print_to")) {
        if (type_key_base_name(ty)) |base_name| {
            if (is_builtin_type(base_name)) return true;
        }
    }
    if (dyn_base_from_type_key(ty) != null) {
        return dyn_has_positive(ty, trait_name);
    }
    const info = ctx.traits.get(trait_name) orelse return false;
    if (type_key_base_name(ty)) |base_name| {
        if (type_has_neg_impl(ctx, trait_name, base_name)) return false;
    }
    var has_impl = false;
    if (type_key_base_name(ty)) |base_name| {
        has_impl = type_has_impl(ctx, trait_name, base_name);
    }
    if (info.methods.len != 0 and !has_impl) return false;
    if (!info.is_auto and info.methods.len == 0 and info.requires.len == 0 and !has_impl) return false;

    if (info.requires.len > 0) {
        for (visited_traits.items) |name| {
            if (std.mem.eql(u8, name, trait_name)) return true;
        }
        const prev_len = visited_traits.items.len;
        visited_traits.append(trait_name) catch return false;
        defer visited_traits.shrinkRetainingCapacity(prev_len);

        for (info.requires) |req| {
            if (req.negative) {
                if (type_satisfies_trait(ctx, ty, req.name, visited_traits, visited_types)) return false;
            } else {
                if (!type_satisfies_trait(ctx, ty, req.name, visited_traits, visited_types)) return false;
            }
        }
    }

    if (info.is_auto and info.methods.len == 0) {
        if (has_impl) return true;
        return auto_trait_satisfied(ctx, ty, trait_name, visited_traits, visited_types);
    }
    if (!info.is_auto and info.methods.len == 0 and info.requires.len > 0) {
        return true;
    }
    return has_impl;
}

fn constraint_satisfied(ctx: *typecheck_ctx, ty: type_key, constraint: trait_constraint) bool {
    if (ty == .unknown) return true;
    if (std.mem.eql(u8, constraint.name, "type")) return !constraint.negative;

    if (dyn_base_from_type_key(ty) != null) {
        if (constraint.negative) {
            if (dyn_has_positive(ty, constraint.name)) return false;
            if (dyn_has_negative(ty, constraint.name)) return true;
            return true;
        }
        return dyn_has_positive(ty, constraint.name);
    }

    var visited = array_list([]const u8).init(ctx.allocator);
    defer visited.deinit();
    var visited_types = string_map(void).init(ctx.allocator);
    defer visited_types.deinit();
    const ok = type_satisfies_trait(ctx, ty, constraint.name, &visited, &visited_types);
    return if (constraint.negative) !ok else ok;
}

fn constraints_ok(ctx: *typecheck_ctx, generics: *string_map(TypeId), failure: ?*constraint_failure) bool {
    var it = generics.iterator();
    while (it.next()) |entry| {
        const var_id = entry.value_ptr.*;
        const node = ctx.types.types.items[var_id];
        if (node != .tvar) continue;
        if (node.tvar.constraints.items.len == 0) continue;
        const resolved = ctx.types.resolve(var_id);
        if (resolved == var_id) continue;
        const ty_key = type_key_from_type_id(ctx, resolved);
        for (node.tvar.constraints.items) |constraint| {
            if (!constraint_satisfied(ctx, ty_key, constraint)) {
                if (failure) |slot| {
                    slot.* = .{
                        .generic_name = entry.key_ptr.*,
                        .constraint = constraint,
                        .ty = ty_key,
                    };
                }
                return false;
            }
        }
    }
    return true;
}

fn type_key_from_type_id(ctx: *typecheck_ctx, id: TypeId) type_key {
    const resolved = ctx.types.resolve(id);
    const node = ctx.types.types.items[resolved];
    return switch (node) {
        .name => |name| .{ .name = name },
        .dyn_trait => |name| .{ .dyn_trait = name },
        .ref => |r| blk: {
            const args = ctx.allocator.alloc(type_key, 1) catch return .unknown;
            args[0] = type_key_from_type_id(ctx, r.inner);
            ctx.owned_type_key_slices.append(args) catch {
                ctx.allocator.free(args);
                return .unknown;
            };
            break :blk .{ .applied = .{
                .base = if (r.mutable) "ref_mut" else "ref",
                .args = args,
            } };
        },
        .applied => |ap| {
            var args = ctx.allocator.alloc(type_key, ap.args.len) catch return .unknown;
            for (ap.args, 0..) |arg, idx| {
                args[idx] = type_key_from_type_id(ctx, arg);
            }
            ctx.owned_type_key_slices.append(args) catch {
                ctx.allocator.free(args);
                return .unknown;
            };
            return .{ .applied = .{ .base = ap.base, .args = args } };
        },
        .tvar => .unknown,
    };
}

fn ensure_type_key(ctx: *typecheck_ctx, id: TypeId) type_key {
    return type_key_from_type_id(ctx, id);
}
