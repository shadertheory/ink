const std = @import("std");
const ink = @import("ink");
const ir_mod = ink.ir;
const intrinsic = @import("../intrinsic.zig");
const encode = @import("../vm/encode.zig");

const max_register: u8 = 63;

pub const codegen_error = error{
    out_of_memory,
    unsupported_node,
    ambiguous_overload,
    missing_main,
    unknown_identifier,
    unknown_function,
    unknown_foreign,
    register_overflow,
    constant_index_overflow,
};

pub const program = struct {
    instructions: []const ink.exe.instruction,
    constants: []const u64,
};

const type_key = union(enum) {
    unknown,
    name: []const u8,
    dyn_trait: []const u8,
    applied: struct {
        base: []const u8,
        args: []const type_key,
    },
};

fn type_key_eq(a: type_key, b: type_key) bool {
    return switch (a) {
        .unknown => false,
        .name => |name_a| switch (b) {
            .unknown => false,
            .name => |name_b| std.mem.eql(u8, name_a, name_b),
            .dyn_trait => false,
            .applied => false,
        },
        .dyn_trait => |name_a| switch (b) {
            .dyn_trait => |name_b| std.mem.eql(u8, name_a, name_b),
            else => false,
        },
        .applied => |ap_a| switch (b) {
            .applied => |ap_b| blk: {
                if (!std.mem.eql(u8, ap_a.base, ap_b.base)) break :blk false;
                if (ap_a.args.len != ap_b.args.len) break :blk false;
                for (ap_a.args, 0..) |arg, idx| {
                    if (!type_key_eq(arg, ap_b.args[idx])) break :blk false;
                }
                break :blk true;
            },
            else => false,
        },
    };
}

fn type_key_name(key: type_key) []const u8 {
    return switch (key) {
        .unknown => "unknown",
        .name => |name| name,
        .dyn_trait => |name| name,
        .applied => |ap| ap.base,
    };
}

fn type_key_append(buf: *std.array_list.Managed(u8), key: type_key) !void {
    switch (key) {
        .unknown => try buf.appendSlice("unknown"),
        .name => |name| try buf.appendSlice(name),
        .dyn_trait => |name| {
            try buf.appendSlice("dyn ");
            try buf.appendSlice(name);
        },
        .applied => |ap| {
            try buf.appendSlice(ap.base);
            try buf.append('<');
            for (ap.args, 0..) |arg, idx| {
                if (idx > 0) try buf.append(',');
                try type_key_append(buf, arg);
            }
            try buf.append('>');
        },
    }
}

fn type_key_base_name(key: type_key) ?[]const u8 {
    return switch (key) {
        .unknown => null,
        .name => |name| name,
        .dyn_trait => |name| name,
        .applied => |ap| ap.base,
    };
}

fn is_print_name(name: []const u8) bool {
    return std.mem.eql(u8, name, "print") or std.mem.endsWith(u8, name, "::print");
}

fn is_println_name(name: []const u8) bool {
    return std.mem.eql(u8, name, "println") or std.mem.endsWith(u8, name, "::println");
}

fn is_cancel_name(name: []const u8) bool {
    return std.mem.eql(u8, name, "cancel") or std.mem.endsWith(u8, name, "::cancel");
}

fn is_builtin_type_name(name: []const u8) bool {
    return std.mem.eql(u8, name, "int") or
        std.mem.eql(u8, name, "float") or
        std.mem.eql(u8, name, "bool") or
        std.mem.eql(u8, name, "string");
}

fn is_int_type_name(name: []const u8) bool {
    return std.mem.eql(u8, name, "int");
}

fn is_float_type_name(name: []const u8) bool {
    return std.mem.eql(u8, name, "float");
}

fn is_bool_type_name(name: []const u8) bool {
    return std.mem.eql(u8, name, "bool");
}

fn unary_operator_method_name(op: ink.unary) ?[]const u8 {
    return switch (op) {
        .neg => "neg",
        .not => "not",
        .bit_not => "bit_not",
        else => null,
    };
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

fn assignment_base_op(op: ink.binary) ?ink.binary {
    return switch (op) {
        .assign_add => .add,
        .assign_sub => .sub,
        .assign_mul => .mul,
        .assign_div => .div,
        .assign_mod => .mod,
        .assign_bit_and => .bit_and,
        .assign_bit_or => .bit_or,
        .assign_bit_xor => .bit_xor,
        .assign_shl => .shl,
        .assign_shr => .shr,
        else => null,
    };
}


const function_info = struct {
    label: ink.exe.label_id,
    decl: ir_mod.ir.function_decl,
    impl_for: ?[]const u8,
};

const function_instance = struct {
    info: function_info,
    bindings: []const generic_binding,
};

const foreign_signature = struct {
    name: []const u8,
    signature: []const u8,
};

const struct_info = struct {
    fields: []const ir_mod.string_identifier,
    is_record: bool,
};

const trait_method = struct {
    name: []const u8,
    return_type: ?ir_mod.ir_identifier,
};

const trait_info = struct {
    methods: []const trait_method,
    requires: []const []const u8,
};

const generic_binding = struct {
    name: []const u8,
    ty: type_key,
};

const overload_result = struct {
    info: function_info,
    bindings: []const generic_binding,
};

const builder = struct {
    allocator: std.mem.Allocator,
    nodes: []const ir_mod.ir,
    strings: []const []const u8,
    roots: []const ir_mod.ir_identifier,
    instructions: std.array_list.Managed(ink.exe.instruction),
    constants: std.array_list.Managed(u64),
    const_map: std.AutoHashMap(u64, u32),
    functions: std.StringHashMap(std.array_list.Managed(function_info)),
    instances: std.array_list.Managed(function_instance),
    pending_instances: std.array_list.Managed(usize),
    instance_map: std.StringHashMap(ink.exe.label_id),
    foreign_overloads: std.StringHashMap(std.array_list.Managed(foreign_signature)),
    foreigns: std.StringHashMap(u32),
    structs: std.StringHashMap(struct_info),
    traits: std.StringHashMap(trait_info),
    trait_impls: std.StringHashMap(std.array_list.Managed([]const u8)),
    trait_vtables: std.StringHashMap(std.StringHashMap([]const ink.exe.label_id)),
    label_constants: std.AutoHashMap(ink.exe.label_id, u32),
    owned_slices: std.ArrayListUnmanaged([]const u8),
    owned_type_slices: std.ArrayListUnmanaged([]const type_key),
    next_label: u32,

    pub fn init(
        allocator: std.mem.Allocator,
        nodes: []const ir_mod.ir,
        strings: []const []const u8,
        roots: []const ir_mod.ir_identifier,
    ) builder {
        return .{
            .allocator = allocator,
            .nodes = nodes,
            .strings = strings,
            .roots = roots,
            .instructions = std.array_list.Managed(ink.exe.instruction).init(allocator),
            .constants = std.array_list.Managed(u64).init(allocator),
            .const_map = std.AutoHashMap(u64, u32).init(allocator),
            .functions = std.StringHashMap(std.array_list.Managed(function_info)).init(allocator),
            .instances = std.array_list.Managed(function_instance).init(allocator),
            .pending_instances = std.array_list.Managed(usize).init(allocator),
            .instance_map = std.StringHashMap(ink.exe.label_id).init(allocator),
            .foreign_overloads = std.StringHashMap(std.array_list.Managed(foreign_signature)).init(allocator),
            .foreigns = std.StringHashMap(u32).init(allocator),
            .structs = std.StringHashMap(struct_info).init(allocator),
            .traits = std.StringHashMap(trait_info).init(allocator),
            .trait_impls = std.StringHashMap(std.array_list.Managed([]const u8)).init(allocator),
            .trait_vtables = std.StringHashMap(std.StringHashMap([]const ink.exe.label_id)).init(allocator),
            .label_constants = std.AutoHashMap(ink.exe.label_id, u32).init(allocator),
            .owned_slices = std.ArrayListUnmanaged([]const u8){},
            .owned_type_slices = std.ArrayListUnmanaged([]const type_key){},
            .next_label = 1,
        };
    }

    pub fn deinit(self: *builder) void {
        self.instructions.deinit();
        self.constants.deinit();
        self.const_map.deinit();
        var func_it = self.functions.iterator();
        while (func_it.next()) |entry| {
            entry.value_ptr.*.deinit();
        }
        self.functions.deinit();
        for (self.instances.items) |instance| {
            if (instance.bindings.len > 0) {
                self.allocator.free(instance.bindings);
            }
        }
        self.instances.deinit();
        self.pending_instances.deinit();
        self.instance_map.deinit();
        var foreign_it = self.foreign_overloads.iterator();
        while (foreign_it.next()) |entry| {
            entry.value_ptr.*.deinit();
        }
        self.foreign_overloads.deinit();
        self.foreigns.deinit();
        var struct_it = self.structs.iterator();
        while (struct_it.next()) |entry| {
            self.allocator.free(entry.value_ptr.*.fields);
        }
        self.structs.deinit();
        var trait_impl_it = self.trait_impls.iterator();
        while (trait_impl_it.next()) |entry| {
            entry.value_ptr.*.deinit();
        }
        self.trait_impls.deinit();
        var trait_it = self.traits.iterator();
        while (trait_it.next()) |entry| {
            self.allocator.free(entry.value_ptr.*.methods);
            self.allocator.free(entry.value_ptr.*.requires);
        }
        self.traits.deinit();
        var vtable_it = self.trait_vtables.iterator();
        while (vtable_it.next()) |entry| {
            var inner_it = entry.value_ptr.*.iterator();
            while (inner_it.next()) |inner| {
                self.allocator.free(inner.value_ptr.*);
            }
            entry.value_ptr.*.deinit();
        }
        self.trait_vtables.deinit();
        self.label_constants.deinit();
        for (self.owned_slices.items) |slice| {
            self.allocator.free(slice);
        }
        self.owned_slices.deinit(self.allocator);
        for (self.owned_type_slices.items) |slice| {
            self.allocator.free(slice);
        }
        self.owned_type_slices.deinit(self.allocator);
    }

    pub fn emit(self: *builder, inst: ink.exe.instruction) codegen_error!void {
        self.instructions.append(inst) catch return error.out_of_memory;
    }

    pub fn new_label(self: *builder) ink.exe.label_id {
        const id = self.next_label;
        self.next_label += 1;
        return id;
    }

    pub fn intern_const(self: *builder, value: u64) codegen_error!u32 {
        if (self.const_map.get(value)) |idx| return idx;
        if (self.constants.items.len >= 8192) return error.constant_index_overflow;
        const idx: u32 = @intCast(self.constants.items.len);
        self.constants.append(value) catch return error.out_of_memory;
        self.const_map.put(value, idx) catch return error.out_of_memory;
        return idx;
    }

    fn node(self: *builder, id: ir_mod.ir_identifier) ir_mod.ir {
        return self.nodes[@intCast(id.idx)];
    }

    fn string_value(self: *builder, id: ir_mod.string_identifier) []const u8 {
        if (id.idx < self.strings.len) return self.strings[id.idx];
        return "<missing>";
    }

    fn instance_key(self: *builder, info: function_info, bindings: []const generic_binding) codegen_error![]const u8 {
        const name = self.string_value(info.decl.name);
        var buf = std.array_list.Managed(u8).init(self.allocator);
        defer buf.deinit();
        buf.appendSlice(name) catch return error.out_of_memory;
        buf.append('<') catch return error.out_of_memory;
        for (info.decl.generics, 0..) |param, idx| {
            if (idx > 0) buf.appendSlice(",") catch return error.out_of_memory;
            const param_name = self.string_value(param.name);
            const binding = lookup_generic(bindings, param_name) orelse return error.unsupported_node;
            buf.appendSlice(param_name) catch return error.out_of_memory;
            buf.append('=') catch return error.out_of_memory;
            type_key_append(&buf, binding) catch return error.out_of_memory;
        }
        buf.append('>') catch return error.out_of_memory;
        const owned = buf.toOwnedSlice() catch return error.out_of_memory;
        self.owned_slices.append(self.allocator, owned) catch return error.out_of_memory;
        return owned;
    }

    fn ensure_instance(self: *builder, info: function_info, bindings: []const generic_binding) codegen_error!ink.exe.label_id {
        if (info.decl.generics.len == 0) return info.label;
        const key = try self.instance_key(info, bindings);
        if (self.instance_map.get(key)) |label| return label;

        const label = self.new_label();
        const binding_copy = self.allocator.alloc(generic_binding, bindings.len) catch return error.out_of_memory;
        std.mem.copyForwards(generic_binding, binding_copy, bindings);

        const idx = self.instances.items.len;
        self.instances.append(.{
            .info = .{ .label = label, .decl = info.decl, .impl_for = info.impl_for },
            .bindings = binding_copy,
        }) catch return error.out_of_memory;
        self.pending_instances.append(idx) catch return error.out_of_memory;
        self.instance_map.put(key, label) catch return error.out_of_memory;
        return label;
    }

    fn const_index_for_label(self: *builder, label: ink.exe.label_id) codegen_error!u32 {
        if (self.label_constants.get(label)) |idx| return idx;
        if (self.constants.items.len >= 8192) return error.constant_index_overflow;
        const idx: u32 = @intCast(self.constants.items.len);
        self.constants.append(0) catch return error.out_of_memory;
        self.label_constants.put(label, idx) catch return error.out_of_memory;
        return idx;
    }
};

fn type_key_from_type_node_with_self(b: *builder, id: ir_mod.ir_identifier, self_name: ?[]const u8) type_key {
    const node = b.node(id);
    return switch (node) {
        .type => |ty| switch (ty) {
            .self => if (self_name) |name| .{ .name = name } else .{ .name = "self" },
            .name => |name_id| .{ .name = b.string_value(name_id) },
            .dyn => |ref| blk: {
                const trait_name = type_name_from_type_node(b, ref) orelse break :blk .unknown;
                if (!b.traits.contains(trait_name)) break :blk .unknown;
                break :blk .{ .dyn_trait = trait_name };
            },
            .optional => |ref| blk: {
                const args = b.allocator.alloc(type_key, 1) catch break :blk .unknown;
                args[0] = type_key_from_type_node_with_self(b, ref, self_name);
                b.owned_type_slices.append(b.allocator, args) catch break :blk .unknown;
                break :blk .{ .applied = .{ .base = "optional", .args = args } };
            },
            .applied => |ap| blk: {
                const args = b.allocator.alloc(type_key, ap.args.len) catch break :blk .unknown;
                for (ap.args, 0..) |arg, idx| {
                    const arg_node = b.node(arg);
                    switch (arg_node) {
                        .type => {
                            args[idx] = type_key_from_type_node_with_self(b, arg, self_name);
                        },
                        .identifier => |ident| {
                            args[idx] = .{ .name = b.string_value(ident) };
                        },
                        .integer => |value| {
                            const owned = std.fmt.allocPrint(b.allocator, "{d}", .{value}) catch break :blk .unknown;
                            b.owned_slices.append(b.allocator, owned) catch break :blk .unknown;
                            args[idx] = .{ .name = owned };
                        },
                        .float => |value| {
                            const owned = std.fmt.allocPrint(b.allocator, "{d}", .{value}) catch break :blk .unknown;
                            b.owned_slices.append(b.allocator, owned) catch break :blk .unknown;
                            args[idx] = .{ .name = owned };
                        },
                        .string => |value| {
                            args[idx] = .{ .name = b.string_value(value) };
                        },
                        else => {
                            args[idx] = .unknown;
                        },
                    }
                }
                b.owned_type_slices.append(b.allocator, args) catch break :blk .unknown;
                break :blk .{ .applied = .{ .base = b.string_value(ap.base), .args = args } };
            },
        },
        else => .unknown,
    };
}

fn type_key_from_type_node(b: *builder, id: ir_mod.ir_identifier) type_key {
    return type_key_from_type_node_with_self(b, id, null);
}

fn type_name_from_type_node(b: *builder, id: ir_mod.ir_identifier) ?[]const u8 {
    const node = b.node(id);
    if (node != .type) return null;
    return switch (node.type) {
        .name => |name_id| b.string_value(name_id),
        .applied => |ap| b.string_value(ap.base),
        .dyn => |ref| blk: {
            const inner = b.node(ref);
            if (inner != .type) break :blk null;
            switch (inner.type) {
                .name => |name_id| break :blk b.string_value(name_id),
                .applied => |ap| break :blk b.string_value(ap.base),
                else => break :blk null,
            }
        },
        .self, .optional => null,
    };
}

fn apply_bindings_to_type_key(
    b: *builder,
    key: type_key,
    bindings: []const generic_binding,
) type_key {
    return switch (key) {
        .unknown => .unknown,
        .name => |name| lookup_generic(bindings, name) orelse key,
        .dyn_trait => |name| lookup_generic(bindings, name) orelse key,
        .applied => |ap| blk: {
            const out = b.allocator.alloc(type_key, ap.args.len) catch break :blk .unknown;
            for (ap.args, 0..) |arg, idx| {
                out[idx] = apply_bindings_to_type_key(b, arg, bindings);
            }
            b.owned_type_slices.append(b.allocator, out) catch break :blk .unknown;
            break :blk .{ .applied = .{ .base = ap.base, .args = out } };
        },
    };
}

fn element_type_from_container(key: type_key) ?type_key {
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
        .dyn_trait => null,
        else => null,
    };
}

fn decl_generic_param(b: *builder, decl: ir_mod.ir.function_decl, name: []const u8) ?ir_mod.ir.generic_param {
    for (decl.generics) |param| {
        if (param.kind != .type) continue;
        const param_name = b.string_value(param.name);
        if (std.mem.eql(u8, param_name, name)) return param;
    }
    return null;
}

fn is_decl_generic(b: *builder, decl: ir_mod.ir.function_decl, name: []const u8) bool {
    return decl_generic_param(b, decl, name) != null;
}

fn type_key_has_generic(b: *builder, decl: ir_mod.ir.function_decl, key: type_key) bool {
    return switch (key) {
        .unknown => false,
        .name => |name| is_decl_generic(b, decl, name),
        .dyn_trait => |name| is_decl_generic(b, decl, name),
        .applied => |ap| blk: {
            for (ap.args) |arg| {
                if (type_key_has_generic(b, decl, arg)) break :blk true;
            }
            break :blk false;
        },
    };
}

fn signature_type_match(
    b: *builder,
    left_decl: ir_mod.ir.function_decl,
    right_decl: ir_mod.ir.function_decl,
    left_type: type_key,
    right_type: type_key,
) bool {
    return switch (left_type) {
        .unknown => right_type == .unknown,
        .name => |lname| switch (right_type) {
            .name => |rname| blk: {
                const left_generic = is_decl_generic(b, left_decl, lname);
                const right_generic = is_decl_generic(b, right_decl, rname);
                if (left_generic or right_generic) break :blk left_generic and right_generic;
                break :blk std.mem.eql(u8, lname, rname);
            },
            else => false,
        },
        .dyn_trait => |lname| switch (right_type) {
            .dyn_trait => |rname| blk: {
                const left_generic = is_decl_generic(b, left_decl, lname);
                const right_generic = is_decl_generic(b, right_decl, rname);
                if (left_generic or right_generic) break :blk left_generic and right_generic;
                break :blk std.mem.eql(u8, lname, rname);
            },
            else => false,
        },
        .applied => |ap| switch (right_type) {
            .applied => |right_ap| blk: {
                if (!std.mem.eql(u8, ap.base, right_ap.base)) break :blk false;
                if (ap.args.len != right_ap.args.len) break :blk false;
                for (ap.args, 0..) |arg, idx| {
                    if (!signature_type_match(b, left_decl, right_decl, arg, right_ap.args[idx])) break :blk false;
                }
                break :blk true;
            },
            else => false,
        },
    };
}

fn match_param_type(
    b: *builder,
    decl: ir_mod.ir.function_decl,
    param_type: type_key,
    arg_type: type_key,
    bindings: *std.ArrayListUnmanaged(generic_binding),
) bool {
    return switch (param_type) {
        .unknown => true,
        .name => |name| blk: {
            if (is_decl_generic(b, decl, name)) {
                break :blk bind_generic(b.allocator, bindings, name, arg_type);
            }
            break :blk type_key_eq(param_type, arg_type);
        },
        .dyn_trait => |name| blk: {
            if (is_decl_generic(b, decl, name)) {
                break :blk bind_generic(b.allocator, bindings, name, arg_type);
            }
            break :blk type_key_eq(param_type, arg_type);
        },
        .applied => |ap| switch (arg_type) {
            .applied => |arg_ap| blk: {
                if (!std.mem.eql(u8, ap.base, arg_ap.base)) break :blk false;
                if (ap.args.len != arg_ap.args.len) break :blk false;
                for (ap.args, 0..) |param_arg, idx| {
                    if (!match_param_type(b, decl, param_arg, arg_ap.args[idx], bindings)) break :blk false;
                }
                break :blk true;
            },
            else => false,
        },
    };
}

fn signature_matches(b: *builder, left: function_info, right: function_info) bool {
    const left_params = left.decl.params;
    const right_params = right.decl.params;
    if (left_params.len != right_params.len) return false;
    for (left_params, 0..) |param, idx| {
        const right_param = right_params[idx];
        if (param.variadic != right_param.variadic) return false;
        const left_type = type_key_from_type_node_with_self(b, param.ty, left.impl_for);
        const right_type = type_key_from_type_node_with_self(b, right_param.ty, right.impl_for);
        if (!signature_type_match(b, left.decl, right.decl, left_type, right_type)) {
            return false;
        }
    }
    return true;
}

fn foreign_overload_score(sig: foreign_signature, arg_types: []const type_key) ?usize {
    if (sig.signature.len == 0) {
        if (arg_types.len != 0) return null;
        return 0;
    }
    var it = std.mem.splitScalar(u8, sig.signature, ',');
    var idx: usize = 0;
    var score: usize = 0;
    while (it.next()) |part| {
        if (idx >= arg_types.len) return null;
        const arg_type = arg_types[idx];
        if (type_key_base_name(arg_type)) |name| {
            if (!sanitized_name_eq(name, part)) return null;
            score += 1;
        }
        idx += 1;
    }
    if (idx != arg_types.len) return null;
    return score;
}

fn sanitized_name_eq(name: []const u8, expected: []const u8) bool {
    var idx: usize = 0;
    for (name) |ch| {
        const mapped = if ((ch >= 'a' and ch <= 'z') or
            (ch >= 'A' and ch <= 'Z') or
            (ch >= '0' and ch <= '9') or
            ch == '_')
        ch else '_';
        if (idx >= expected.len or expected[idx] != mapped) return false;
        idx += 1;
    }
    return idx == expected.len;
}

fn string_list_contains(list: []const []const u8, value: []const u8) bool {
    for (list) |item| {
        if (std.mem.eql(u8, item, value)) return true;
    }
    return false;
}

fn bind_generic(
    allocator: std.mem.Allocator,
    bindings: *std.ArrayListUnmanaged(generic_binding),
    name: []const u8,
    ty: type_key,
) bool {
    for (bindings.items) |*binding| {
        if (!std.mem.eql(u8, binding.name, name)) continue;
        if (ty == .unknown) return true;
        if (binding.ty == .unknown) {
            binding.ty = ty;
            return true;
        }
        return type_key_eq(binding.ty, ty);
    }
    bindings.append(allocator, .{ .name = name, .ty = ty }) catch return false;
    return true;
}

fn lookup_generic(bindings: []const generic_binding, name: []const u8) ?type_key {
    for (bindings) |binding| {
        if (std.mem.eql(u8, binding.name, name)) return binding.ty;
    }
    return null;
}

fn type_has_impl(b: *builder, trait_name: []const u8, type_name: []const u8) bool {
    const set = b.trait_impls.get(trait_name) orelse return false;
    for (set.items) |item| {
        if (std.mem.eql(u8, item, type_name)) return true;
    }
    return false;
}

fn type_satisfies_constraint_inner(
    b: *builder,
    type_name: []const u8,
    constraint_name: []const u8,
    visited: *std.ArrayListUnmanaged([]const u8),
) bool {
    for (visited.items) |name| {
        if (std.mem.eql(u8, name, constraint_name)) return true;
    }
    visited.append(b.allocator, constraint_name) catch return false;

    if (b.traits.get(constraint_name)) |info| {
        const has_impl = type_has_impl(b, constraint_name, type_name);
        if (info.methods.len != 0 and !has_impl) return false;
        for (info.requires) |req| {
            if (!type_satisfies_constraint_inner(b, type_name, req, visited)) return false;
        }
        return has_impl or info.methods.len == 0;
    }
    return false;
}

fn type_satisfies_constraint(b: *builder, type_name: []const u8, constraint_name: []const u8) bool {
    var visited = std.ArrayListUnmanaged([]const u8){};
    defer visited.deinit(b.allocator);
    return type_satisfies_constraint_inner(b, type_name, constraint_name, &visited);
}

const function_ctx = struct {
    b: *builder,
    locals: std.StringHashMap(u8),
    local_types: std.StringHashMap(type_key),
    temp_base: u8,
    next_temp: u8,
    returned: bool,
    pinned_mask: u64,
    self_type: ?[]const u8,
    bindings: []const generic_binding,

    const binary_prep = struct {
        left: u8,
        right: u8,
        dst: u8,
        left_temp: bool,
        right_temp: bool,
    };

    const unary_prep = struct {
        src: u8,
        dst: u8,
        src_temp: bool,
    };

    pub fn init(
        b: *builder,
        params: []const ir_mod.ir.function_decl.param,
        self_type: ?[]const u8,
        bindings: []const generic_binding,
    ) codegen_error!function_ctx {
        var locals = std.StringHashMap(u8).init(b.allocator);
        errdefer locals.deinit();
        var local_types = std.StringHashMap(type_key).init(b.allocator);
        errdefer local_types.deinit();

        var reg_index: u8 = 1;
        for (params) |param| {
            const name = b.string_value(param.name);
            locals.put(name, reg_index) catch return error.out_of_memory;
            var ty = type_key_from_type_node_with_self(b, param.ty, self_type);
            if (bindings.len > 0) {
                ty = apply_bindings_to_type_key(b, ty, bindings);
            }
            local_types.put(name, ty) catch return error.out_of_memory;
            reg_index += 1;
        }

        if (reg_index > max_register) return error.register_overflow;

        return .{
            .b = b,
            .locals = locals,
            .local_types = local_types,
            .temp_base = reg_index,
            .next_temp = reg_index,
            .returned = false,
            .pinned_mask = 0,
            .self_type = self_type,
            .bindings = bindings,
        };
    }

    pub fn deinit(self: *function_ctx) void {
        self.locals.deinit();
        self.local_types.deinit();
    }

    fn alloc_temp(self: *function_ctx) codegen_error!u8 {
        if (self.next_temp > max_register) return error.register_overflow;
        const reg = self.next_temp;
        self.next_temp += 1;
        return reg;
    }

    fn alloc_local(self: *function_ctx) codegen_error!u8 {
        if (self.next_temp > max_register) return error.register_overflow;
        const reg = self.next_temp;
        self.next_temp += 1;
        self.temp_base = self.next_temp;
        return reg;
    }

    fn free_temp(self: *function_ctx, reg: u8) void {
        if (reg >= self.temp_base and !self.is_pinned(reg) and reg + 1 == self.next_temp) {
            self.next_temp -= 1;
        }
    }

    fn is_temp(self: *function_ctx, reg: u8) bool {
        return reg >= self.temp_base and !self.is_pinned(reg);
    }

    fn pin_temp(self: *function_ctx, reg: u8) void {
        if (reg <= max_register) {
            self.pinned_mask |= @as(u64, 1) << @intCast(reg);
        }
    }

    fn unpin_temp(self: *function_ctx, reg: u8) void {
        if (reg <= max_register) {
            self.pinned_mask &= ~(@as(u64, 1) << @intCast(reg));
        }
    }

    fn is_pinned(self: *function_ctx, reg: u8) bool {
        if (reg > max_register) return false;
        return (self.pinned_mask & (@as(u64, 1) << @intCast(reg))) != 0;
    }

    fn infer_expr_type(self: *function_ctx, id: ir_mod.ir_identifier) type_key {
        const node = self.b.node(id);
        return switch (node) {
            .integer => .{ .name = "int" },
            .float => .{ .name = "float" },
            .boolean => .{ .name = "bool" },
            .string => .{ .name = "string" },
            .identifier => |ident| blk: {
                const name = self.b.string_value(ident);
                if (self.local_types.get(name)) |ty| break :blk ty;
                break :blk .unknown;
            },
            .unary => |un| switch (un.op) {
                .@"try", .unwrap_optional => .unknown,
                else => blk: {
                    const right = self.infer_expr_type(un.right);
                    if (unary_operator_method_name(un.op)) |method| {
                        const ret = self.infer_method_return_type(method, right, &.{});
                        if (ret != .unknown) break :blk ret;
                    }
                    break :blk right;
                },
            },
            .binary => |bin| switch (bin.op) {
                .call => self.infer_call_type(id),
                .@"as" => blk: {
                    const type_node = self.b.node(bin.right);
                    if (type_node == .type and type_node.type == .dyn) {
                        const name = type_name_from_type_node(self.b, bin.right) orelse break :blk .unknown;
                        if (self.b.traits.contains(name)) break :blk .{ .dyn_trait = name };
                    }
                    const name = type_name_from_type_node(self.b, bin.right) orelse break :blk .unknown;
                    break :blk .{ .name = name };
                },
                .add, .sub, .mul, .div, .mod, .bit_and, .bit_or, .bit_xor, .shl, .shr => blk: {
                    const left = self.infer_expr_type(bin.left);
                    const right = self.infer_expr_type(bin.right);
                    if (binary_operator_method_name(bin.op)) |method| {
                        const ret = self.infer_method_return_type(method, left, &.{right});
                        if (ret != .unknown) break :blk ret;
                    }
                    if (type_key_eq(left, right)) break :blk left;
                    break :blk .unknown;
                },
                .index => blk: {
                    const left = self.infer_expr_type(bin.left);
                    if (element_type_from_container(left)) |elem| break :blk elem;
                    const index_ty = self.infer_expr_type(bin.right);
                    const ret = self.infer_method_return_type("index", left, &.{index_ty});
                    if (ret != .unknown) break :blk ret;
                    break :blk .unknown;
                },
                .less_than,
                .less_or_equal,
                .greater_than,
                .greater_or_equal,
                .equal,
                .not_equal,
                .logical_and,
                .logical_or,
                .logical_xor,
                => .{ .name = "bool" },
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
                => self.infer_expr_type(bin.left),
                else => .unknown,
            },
            .if_expr => |ife| blk: {
                const then_ty = self.infer_expr_type(ife.then_branch);
                const else_ty = if (ife.else_branch) |ref| self.infer_expr_type(ref) else .unknown;
                if (type_key_eq(then_ty, else_ty)) break :blk then_ty;
                break :blk .unknown;
            },
            .intrinsic => |call| blk: {
                const name = self.b.string_value(call.name);
                const def = intrinsic.lookup(name) orelse break :blk .unknown;
                break :blk intrinsic_return_type(def);
            },
            .record_literal => |rec| .{ .name = self.b.string_value(rec.type_name) },
            .decl => |decl| switch (decl) {
                .@"const" => |c| self.infer_expr_type(c.value),
                .@"var" => |v| self.infer_expr_type(v.value),
                else => .unknown,
            },
            else => .unknown,
        };
    }

    fn try_compile_method_call(
        self: *function_ctx,
        name: []const u8,
        recv: ir_mod.ir_identifier,
        args: []const ir_mod.ir_identifier,
    ) codegen_error!?u8 {
        const recv_type = self.infer_expr_type(recv);
        if (self.trait_name_from_type(recv_type)) |trait_name| {
            if (self.trait_method_index(trait_name, name) == null) {
                return error.unknown_function;
            }
            const reg = try self.compile_trait_method_call(trait_name, name, recv, args);
            return reg;
        }

        const group = self.b.functions.get(name) orelse return null;
        if (args.len + 1 > 7) return error.register_overflow;

        var arg_ids_buf: [8]ir_mod.ir_identifier = undefined;
        arg_ids_buf[0] = recv;
        std.mem.copyForwards(ir_mod.ir_identifier, arg_ids_buf[1 .. args.len + 1], args);
        const arg_ids = arg_ids_buf[0 .. args.len + 1];

        var arg_types: [7]type_key = undefined;
        for (arg_ids, 0..) |arg_id, idx| {
            arg_types[idx] = self.infer_expr_type(arg_id);
        }
        const arg_type_slice = arg_types[0..arg_ids.len];

        const resolved = self.resolve_function_overload(group.items, arg_type_slice) catch |err| switch (err) {
            error.unknown_function => return null,
            else => return err,
        };
        defer if (resolved.bindings.len > 0) self.b.allocator.free(resolved.bindings);

        for (arg_ids, 0..) |arg_id, idx| {
            const arg_reg = try self.compile_expr(arg_id);
            const dst: u8 = @intCast(idx + 1);
            try self.emit(.{ .argument_set = .{ .dst = dst, .src = arg_reg } });
            if (self.is_temp(arg_reg)) self.free_temp(arg_reg);
        }

        const target = try self.b.ensure_instance(resolved.info, resolved.bindings);
        try self.emit(.{ .call = .{ .target = target } });
        const reg = try self.save_result_reg(0);
        return @as(?u8, reg);
    }

    fn try_compile_method_call_regs(
        self: *function_ctx,
        name: []const u8,
        recv_reg: u8,
        recv_type: type_key,
        arg_regs: []const u8,
        arg_types: []const type_key,
    ) codegen_error!?u8 {
        if (self.trait_name_from_type(recv_type)) |trait_name| {
            if (self.trait_method_index(trait_name, name) == null) {
                return error.unknown_function;
            }
            const reg = try self.compile_trait_method_call_regs(trait_name, name, recv_reg, arg_regs);
            return reg;
        }

        const group = self.b.functions.get(name) orelse return null;
        if (arg_regs.len + 1 > 7) return error.register_overflow;

        var call_types_buf: [8]type_key = undefined;
        call_types_buf[0] = recv_type;
        std.mem.copyForwards(type_key, call_types_buf[1 .. arg_types.len + 1], arg_types);
        const call_types = call_types_buf[0 .. arg_types.len + 1];

        const resolved = self.resolve_function_overload(group.items, call_types) catch |err| switch (err) {
            error.unknown_function => return null,
            else => return err,
        };
        defer if (resolved.bindings.len > 0) self.b.allocator.free(resolved.bindings);

        try self.emit(.{ .argument_set = .{ .dst = 1, .src = recv_reg } });
        for (arg_regs, 0..) |arg_reg, idx| {
            const dst: u8 = @intCast(idx + 2);
            try self.emit(.{ .argument_set = .{ .dst = dst, .src = arg_reg } });
        }

        const target = try self.b.ensure_instance(resolved.info, resolved.bindings);
        try self.emit(.{ .call = .{ .target = target } });
        const reg = try self.save_result_reg(0);
        return @as(?u8, reg);
    }

    fn compile_trait_method_call_regs(
        self: *function_ctx,
        trait_name: []const u8,
        method_name: []const u8,
        receiver_reg: u8,
        args: []const u8,
    ) codegen_error!u8 {
        const method_idx = self.trait_method_index(trait_name, method_name) orelse return error.unknown_function;
        if (args.len + 1 > 7) return error.register_overflow;

        var recv_reg = receiver_reg;
        var recv_temp: ?u8 = null;
        if (recv_reg == 0) {
            const tmp = try self.alloc_temp();
            try self.emit(.{ .move = .{ .dst = tmp, .src = recv_reg } });
            recv_reg = tmp;
            recv_temp = tmp;
        }

        const vtable_reg = try self.deref_to_temp(recv_reg);
        const data_reg = blk: {
            const offset_idx = try self.b.intern_const(1);
            const offset_reg = try self.alloc_temp();
            try self.emit(.{ .load_const = .{ .dst = offset_reg, .const_index = offset_idx } });
            try self.emit(.{ .add = .{ .dst = offset_reg, .src_a = recv_reg, .src_b = offset_reg } });
            const data_reg = try self.deref_to_temp(offset_reg);
            self.free_temp(offset_reg);
            break :blk data_reg;
        };

        const fn_reg = try self.alloc_temp();
        if (method_idx == 0) {
            try self.deref_into(vtable_reg, fn_reg);
        } else {
            const method_idx_const = try self.b.intern_const(@intCast(method_idx));
            const method_reg = try self.alloc_temp();
            try self.emit(.{ .load_const = .{ .dst = method_reg, .const_index = method_idx_const } });
            try self.emit(.{ .add = .{ .dst = method_reg, .src_a = vtable_reg, .src_b = method_reg } });
            try self.deref_into(method_reg, fn_reg);
            self.free_temp(method_reg);
        }
        if (self.is_temp(vtable_reg)) self.free_temp(vtable_reg);

        if (self.is_temp(data_reg)) self.pin_temp(data_reg);
        if (self.is_temp(fn_reg)) self.pin_temp(fn_reg);

        try self.emit(.{ .argument_set = .{ .dst = 1, .src = data_reg } });
        for (args, 0..) |arg_reg, idx| {
            const dst: u8 = @intCast(idx + 2);
            try self.emit(.{ .argument_set = .{ .dst = dst, .src = arg_reg } });
        }

        if (self.is_temp(data_reg)) self.unpin_temp(data_reg);
        if (self.is_temp(fn_reg)) self.unpin_temp(fn_reg);

        try self.emit(.{ .call_register = .{ .src = fn_reg, .dst = 0 } });

        if (self.is_temp(fn_reg)) self.free_temp(fn_reg);
        if (self.is_temp(data_reg)) self.free_temp(data_reg);
        if (recv_temp) |tmp| self.free_temp(tmp);

        return self.save_result_reg(0);
    }

    fn compile_overloadable_unary(
        self: *function_ctx,
        op: ink.unary,
        right_id: ir_mod.ir_identifier,
    ) codegen_error!u8 {
        const method = unary_operator_method_name(op) orelse return error.unsupported_node;
        const right_type = self.infer_expr_type(right_id);
        if (self.trait_name_from_type(right_type) != null or self.is_known_non_builtin(right_type)) {
            if (try self.try_compile_method_call(method, right_id, &.{}) ) |reg| {
                return reg;
            }
            if (self.is_known_non_builtin(right_type)) return error.unknown_function;
        }

        switch (op) {
            .neg => {
                if (self.is_float_type_key(right_type)) {
                    return self.emit_float_unary(right_id, .fneg);
                }
                if (self.is_int_type_key(right_type) or right_type == .unknown) {
                    return self.emit_int_unary(right_id, .int_neg);
                }
            },
            .not => {
                if (self.is_bool_type_key(right_type) or right_type == .unknown) {
                    return self.emit_bool_not(right_id);
                }
            },
            .bit_not => {
                if (self.is_int_type_key(right_type) or right_type == .unknown) {
                    return self.emit_int_unary(right_id, .bit_not);
                }
            },
            else => {},
        }

        return error.unsupported_node;
    }

    fn compile_overloadable_binary(
        self: *function_ctx,
        op: ink.binary,
        left_id: ir_mod.ir_identifier,
        right_id: ir_mod.ir_identifier,
    ) codegen_error!u8 {
        const method = binary_operator_method_name(op) orelse return error.unsupported_node;
        const left_type = self.infer_expr_type(left_id);
        const right_type = self.infer_expr_type(right_id);

        if (self.trait_name_from_type(left_type) != null or self.is_known_non_builtin(left_type)) {
            if (try self.try_compile_method_call(method, left_id, &.{right_id}) ) |reg| {
                return reg;
            }
            if (self.is_known_non_builtin(left_type)) return error.unknown_function;
        }

        return self.compile_builtin_binary(op, left_id, right_id, left_type, right_type);
    }

    fn compile_builtin_binary(
        self: *function_ctx,
        op: ink.binary,
        left_id: ir_mod.ir_identifier,
        right_id: ir_mod.ir_identifier,
        left_type: type_key,
        right_type: type_key,
    ) codegen_error!u8 {
        const same_int = self.is_int_type_key(left_type) and self.is_int_type_key(right_type);
        const same_float = self.is_float_type_key(left_type) and self.is_float_type_key(right_type);
        const same_bool = self.is_bool_type_key(left_type) and self.is_bool_type_key(right_type);

        switch (op) {
            .add => {
                if (same_float) return self.emit_float_binary(left_id, right_id, .fadd);
                if (same_int or left_type == .unknown or right_type == .unknown) return self.emit_int_binary(left_id, right_id, .add);
            },
            .sub => {
                if (same_float) return self.emit_float_binary(left_id, right_id, .fsub);
                if (same_int or left_type == .unknown or right_type == .unknown) return self.emit_int_binary(left_id, right_id, .sub);
            },
            .mul => {
                if (same_float) return self.emit_float_binary(left_id, right_id, .fmul);
                if (same_int or left_type == .unknown or right_type == .unknown) return self.emit_int_binary(left_id, right_id, .mul);
            },
            .div => {
                if (same_float) return self.emit_float_binary(left_id, right_id, .fdiv);
                if (same_int or left_type == .unknown or right_type == .unknown) return self.emit_int_binary(left_id, right_id, .div);
            },
            .mod => {
                if (same_float) return self.emit_float_binary(left_id, right_id, .frem);
                if (same_int or left_type == .unknown or right_type == .unknown) return self.emit_int_binary(left_id, right_id, .rem);
            },
            .bit_and => {
                if (same_int or left_type == .unknown or right_type == .unknown) return self.emit_int_binary(left_id, right_id, .bit_and);
            },
            .bit_or => {
                if (same_int or left_type == .unknown or right_type == .unknown) return self.emit_int_binary(left_id, right_id, .bit_or);
            },
            .bit_xor => {
                if (same_int or left_type == .unknown or right_type == .unknown) return self.emit_int_binary(left_id, right_id, .bit_xor);
            },
            .shl => {
                if (same_int or left_type == .unknown or right_type == .unknown) return self.emit_int_binary(left_id, right_id, .bit_shl);
            },
            .shr => {
                if (same_int or left_type == .unknown or right_type == .unknown) return self.emit_int_binary(left_id, right_id, .bit_shr);
            },
            .equal => {
                if (same_float) return self.emit_float_binary(left_id, right_id, .fcompare_eq);
                if (same_int or same_bool or left_type == .unknown or right_type == .unknown) return self.emit_int_binary(left_id, right_id, .compare_eq);
            },
            .not_equal => {
                if (same_float) return self.emit_float_not_equal(left_id, right_id);
                if (same_int or same_bool or left_type == .unknown or right_type == .unknown) return self.emit_int_not_equal(left_id, right_id);
            },
            .less_than => {
                if (same_float) return self.emit_float_binary(left_id, right_id, .fcompare_lt);
                if (same_int or left_type == .unknown or right_type == .unknown) return self.emit_int_binary(left_id, right_id, .compare_lt);
            },
            .less_or_equal => {
                if (same_float) return self.emit_float_invert_compare(left_id, right_id, .fcompare_gt);
                if (same_int or left_type == .unknown or right_type == .unknown) return self.emit_int_binary(left_id, right_id, .compare_le);
            },
            .greater_than => {
                if (same_float) return self.emit_float_binary(left_id, right_id, .fcompare_gt);
                if (same_int or left_type == .unknown or right_type == .unknown) return self.emit_int_binary(left_id, right_id, .compare_gt);
            },
            .greater_or_equal => {
                if (same_float) return self.emit_float_invert_compare(left_id, right_id, .fcompare_lt);
                if (same_int or left_type == .unknown or right_type == .unknown) return self.emit_int_binary(left_id, right_id, .compare_ge);
            },
            else => {},
        }

        return error.unsupported_node;
    }

    fn emit_bool_not(self: *function_ctx, value_id: ir_mod.ir_identifier) codegen_error!u8 {
        const prep = try self.prep_unary(value_id);
        const zero_idx = try self.b.intern_const(0);
        const zero_reg = try self.alloc_temp();
        try self.emit(.{ .load_const = .{ .dst = zero_reg, .const_index = zero_idx } });
        try self.emit(.{ .compare_eq = .{ .dst = prep.dst, .src_a = prep.src, .src_b = zero_reg } });
        self.free_temp(zero_reg);
        self.finish_unary(prep);
        return prep.dst;
    }

    fn compile_logical_and(
        self: *function_ctx,
        left_id: ir_mod.ir_identifier,
        right_id: ir_mod.ir_identifier,
    ) codegen_error!u8 {
        const left_reg = try self.compile_expr(left_id);
        const dst = if (self.is_temp(left_reg)) left_reg else try self.alloc_temp();
        if (dst != left_reg) {
            try self.emit(.{ .move = .{ .dst = dst, .src = left_reg } });
            if (self.is_temp(left_reg)) self.free_temp(left_reg);
        }

        const end_label = self.b.new_label();
        try self.emit(.{ .jump_if_false = .{ .condition = dst, .target = end_label } });

        const right_reg = try self.compile_expr(right_id);
        if (right_reg != dst) {
            try self.emit(.{ .move = .{ .dst = dst, .src = right_reg } });
        }
        if (self.is_temp(right_reg) and right_reg != dst) self.free_temp(right_reg);

        try self.emit(.{ .label = .{ .id = end_label } });
        return dst;
    }

    fn compile_logical_or(
        self: *function_ctx,
        left_id: ir_mod.ir_identifier,
        right_id: ir_mod.ir_identifier,
    ) codegen_error!u8 {
        const left_reg = try self.compile_expr(left_id);
        const dst = if (self.is_temp(left_reg)) left_reg else try self.alloc_temp();
        if (dst != left_reg) {
            try self.emit(.{ .move = .{ .dst = dst, .src = left_reg } });
            if (self.is_temp(left_reg)) self.free_temp(left_reg);
        }

        const eval_right = self.b.new_label();
        const end_label = self.b.new_label();
        try self.emit(.{ .jump_if_false = .{ .condition = dst, .target = eval_right } });
        try self.emit(.{ .jump = .{ .target = end_label } });

        try self.emit(.{ .label = .{ .id = eval_right } });
        const right_reg = try self.compile_expr(right_id);
        if (right_reg != dst) {
            try self.emit(.{ .move = .{ .dst = dst, .src = right_reg } });
        }
        if (self.is_temp(right_reg) and right_reg != dst) self.free_temp(right_reg);

        try self.emit(.{ .label = .{ .id = end_label } });
        return dst;
    }

    fn compile_logical_xor(
        self: *function_ctx,
        left_id: ir_mod.ir_identifier,
        right_id: ir_mod.ir_identifier,
    ) codegen_error!u8 {
        return self.emit_int_binary(left_id, right_id, .bit_xor);
    }

    fn compile_assign_expr(
        self: *function_ctx,
        op: ink.binary,
        left_id: ir_mod.ir_identifier,
        right_id: ir_mod.ir_identifier,
    ) codegen_error!u8 {
        if (op == .assign) {
            return self.compile_simple_assign(left_id, right_id);
        }
        const base_op = assignment_base_op(op) orelse return error.unsupported_node;
        const value_reg = try self.compile_overloadable_binary(base_op, left_id, right_id);
        const value_type = self.infer_expr_type(left_id);
        return self.store_assignment_value(left_id, value_reg, value_type);
    }

    fn compile_simple_assign(
        self: *function_ctx,
        left_id: ir_mod.ir_identifier,
        right_id: ir_mod.ir_identifier,
    ) codegen_error!u8 {
        const value_reg = try self.compile_expr(right_id);
        const value_type = self.infer_expr_type(right_id);
        return self.store_assignment_value(left_id, value_reg, value_type);
    }

    fn store_assignment_value(
        self: *function_ctx,
        left_id: ir_mod.ir_identifier,
        value_reg: u8,
        value_type: type_key,
    ) codegen_error!u8 {
        const left_node = self.b.node(left_id);
        switch (left_node) {
            .identifier => |ident| {
                const name = self.b.string_value(ident);
                const local_reg = self.locals.get(name) orelse return error.unknown_identifier;
                if (local_reg != value_reg) {
                    try self.emit(.{ .move = .{ .dst = local_reg, .src = value_reg } });
                }
                if (self.is_temp(value_reg) and local_reg != value_reg) {
                    self.free_temp(value_reg);
                }
                return local_reg;
            },
            .binary => |bin| switch (bin.op) {
                .access => {
                    const ptr_reg = try self.compile_access_ptr(bin.left, bin.right);
                    try self.store_value(ptr_reg, value_reg);
                    if (self.is_temp(ptr_reg)) self.free_temp(ptr_reg);
                    return value_reg;
                },
                .index => {
                    const base_type = self.infer_expr_type(bin.left);
                    if (type_key_base_name(base_type)) |name| {
                        if (!std.mem.eql(u8, name, "slice")) {
                            if (try self.store_index_set_value(bin.left, bin.right, value_reg, base_type, value_type)) |reg| {
                                return reg;
                            }
                            if (self.is_known_non_builtin(base_type)) return error.unknown_function;
                            return error.unsupported_node;
                        }
                    } else if (self.trait_name_from_type(base_type) != null or self.is_known_non_builtin(base_type)) {
                        if (try self.store_index_set_value(bin.left, bin.right, value_reg, base_type, value_type)) |reg| {
                            return reg;
                        }
                        if (self.is_known_non_builtin(base_type)) return error.unknown_function;
                        return error.unsupported_node;
                    }

                    const ptr_reg = try self.compile_index_ptr(bin.left, bin.right);
                    try self.store_value(ptr_reg, value_reg);
                    if (self.is_temp(ptr_reg)) self.free_temp(ptr_reg);
                    return value_reg;
                },
                else => return error.unsupported_node,
            },
            else => return error.unsupported_node,
        }
    }

    fn store_index_set_value(
        self: *function_ctx,
        base_id: ir_mod.ir_identifier,
        index_id: ir_mod.ir_identifier,
        value_reg: u8,
        base_type: type_key,
        value_type: type_key,
    ) codegen_error!?u8 {
        var base_reg = try self.compile_expr(base_id);
        if (base_reg == 0) base_reg = try self.save_result_reg(base_reg);
        var index_reg = try self.compile_expr(index_id);
        if (index_reg == 0) index_reg = try self.save_result_reg(index_reg);
        var val_reg = value_reg;
        if (val_reg == 0) val_reg = try self.save_result_reg(val_reg);

        const index_type = self.infer_expr_type(index_id);
        const arg_regs = [_]u8{ index_reg, val_reg };
        const arg_types = [_]type_key{ index_type, value_type };
        const result = try self.try_compile_method_call_regs(
            "index_set",
            base_reg,
            base_type,
            arg_regs[0..],
            arg_types[0..],
        );

        if (result) |reg| {
            if (self.is_temp(reg)) self.free_temp(reg);
        }

        if (self.is_temp(base_reg) and base_reg != val_reg and base_reg != index_reg) self.free_temp(base_reg);
        if (self.is_temp(index_reg) and index_reg != val_reg and index_reg != base_reg) self.free_temp(index_reg);
        if (self.is_temp(val_reg) and val_reg != value_reg) self.free_temp(val_reg);

        if (result != null) return value_reg;
        return null;
    }

    fn compile_local_decl(
        self: *function_ctx,
        name_id: ir_mod.string_identifier,
        ty: ?ir_mod.ir_identifier,
        value_id: ir_mod.ir_identifier,
    ) codegen_error!u8 {
        const name = self.b.string_value(name_id);
        const existing = self.locals.get(name);
        const local_reg = if (existing) |reg| reg else blk: {
            const reg = try self.alloc_local();
            self.locals.put(name, reg) catch return error.out_of_memory;
            break :blk reg;
        };

        const value_reg = try self.compile_expr(value_id);
        if (local_reg != value_reg) {
            try self.emit(.{ .move = .{ .dst = local_reg, .src = value_reg } });
        }
        if (self.is_temp(value_reg) and value_reg != local_reg) self.free_temp(value_reg);

        const value_type = if (ty) |ty_id| type_key_from_type_node(self.b, ty_id) else self.infer_expr_type(value_id);
        if (value_type != .unknown) {
            self.local_types.put(name, value_type) catch return error.out_of_memory;
        }
        return local_reg;
    }

    fn infer_call_type(self: *function_ctx, id: ir_mod.ir_identifier) type_key {
        var base_id = id;
        var args_buf: [7]ir_mod.ir_identifier = undefined;
        var arg_count: usize = 0;

        while (true) {
            const node = self.b.node(base_id);
            switch (node) {
                .binary => |bin| {
                    if (bin.op == .call) {
                        if (arg_count >= args_buf.len) return .unknown;
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

        if (arg_count == 1 and self.is_unit(args_buf[0])) {
            arg_count = 0;
        }

        if (arg_count == 1 and self.is_unit(args_buf[0])) {
            arg_count = 0;
        }

        const base = self.resolve_call_base(base_id) catch return .unknown;

        var arg_ids = args_buf[0..arg_count];
        var arg_ids_buf: [8]ir_mod.ir_identifier = undefined;
        if (base.receiver) |recv| {
            if (arg_count + 1 > arg_ids_buf.len) return .unknown;
            arg_ids_buf[0] = recv;
            std.mem.copyForwards(ir_mod.ir_identifier, arg_ids_buf[1 .. arg_count + 1], arg_ids);
            arg_ids = arg_ids_buf[0 .. arg_count + 1];
            arg_count += 1;
        }

        const name = base.name;
        if (is_print_name(name)) return .{ .name = "unit" };
        if (is_cancel_name(name)) return .{ .name = "unit" };

        if (base.receiver) |recv| {
            const recv_type = self.infer_expr_type(recv);
            if (self.trait_name_from_type(recv_type)) |type_name| {
                return self.trait_method_return_type(type_name, name);
            }
        }

        if (self.b.foreigns.contains(name)) return .unknown;

        var arg_types: [7]type_key = undefined;
        for (arg_ids, 0..) |arg_id, idx| {
            arg_types[idx] = self.infer_expr_type(arg_id);
        }
        const arg_type_slice = arg_types[0..arg_ids.len];

        const group = self.b.functions.get(name) orelse return .unknown;
        const resolved = self.resolve_function_overload(group.items, arg_type_slice) catch return .unknown;
        const info = resolved.info;
        const return_type = info.decl.return_type orelse return .unknown;
        const base_return = type_key_from_type_node_with_self(self.b, return_type, info.impl_for);
        var resolved_return = base_return;
        if (resolved.bindings.len > 0) {
            resolved_return = apply_bindings_to_type_key(self.b, base_return, resolved.bindings);
        }
        if (resolved.bindings.len > 0) self.b.allocator.free(resolved.bindings);
        return resolved_return;
    }

    fn match_overload(
        self: *function_ctx,
        info: function_info,
        arg_types: []const type_key,
        bindings: *std.ArrayListUnmanaged(generic_binding),
    ) ?usize {
        bindings.clearRetainingCapacity();
        const decl = info.decl;
        const params = decl.params;
        if (params.len == 0) return if (arg_types.len == 0) 0 else null;

        const is_variadic = params[params.len - 1].variadic;
        if (!is_variadic) {
            if (params.len != arg_types.len) return null;
        } else if (arg_types.len < params.len - 1) {
            return null;
        }

        const fixed_len = if (is_variadic) params.len - 1 else params.len;
        var score: usize = 0;
        for (params[0..fixed_len], 0..) |param, idx| {
            const arg_type = arg_types[idx];
            const param_type = type_key_from_type_node_with_self(self.b, param.ty, info.impl_for);
            if (param_type == .unknown) return null;
            if (!match_param_type(self.b, decl, param_type, arg_type, bindings)) return null;
            if (!type_key_has_generic(self.b, decl, param_type) and arg_type != .unknown) {
                score += 1;
            }
        }

        if (is_variadic) {
            const pack_param = params[fixed_len];
            const pack_type = type_key_from_type_node_with_self(self.b, pack_param.ty, info.impl_for);
            if (pack_type == .unknown) return null;
            const pack_name = type_key_base_name(pack_type);
            const pack_is_generic = pack_name != null and is_decl_generic(self.b, decl, pack_name.?);
            const pack_has_generic = type_key_has_generic(self.b, decl, pack_type);
            if (!pack_is_generic and !pack_has_generic) {
                for (arg_types[fixed_len..]) |arg_type| {
                    if (arg_type == .unknown) continue;
                    if (!type_key_eq(pack_type, arg_type)) return null;
                    score += 1;
                }
            }
            if (pack_is_generic) {
                if (pack_name) |name| {
                    if (decl_generic_param(self.b, decl, name)) |param| {
                        if (param.constraint) |constraint| {
                            for (arg_types[fixed_len..]) |arg_type| {
                                if (!self.constraint_satisfied(arg_type, constraint)) return null;
                            }
                        }
                    }
                }
            }
        }

        if (!self.check_constraints(info, bindings.items, arg_types, fixed_len, is_variadic)) return null;
        return score;
    }

    fn overload_score(
        self: *function_ctx,
        info: function_info,
        arg_types: []const type_key,
    ) ?usize {
        var bindings = std.ArrayListUnmanaged(generic_binding){};
        defer bindings.deinit(self.b.allocator);
        return self.match_overload(info, arg_types, &bindings);
    }

    fn resolve_function_overload(
        self: *function_ctx,
        overloads: []const function_info,
        arg_types: []const type_key,
    ) codegen_error!overload_result {
        var best_idx: ?usize = null;
        var best_score: usize = 0;
        var ambiguous = false;

        var best_bindings = std.ArrayListUnmanaged(generic_binding){};
        errdefer best_bindings.deinit(self.b.allocator);
        var candidate = std.ArrayListUnmanaged(generic_binding){};
        defer candidate.deinit(self.b.allocator);

        for (overloads, 0..) |info, idx| {
            const score = self.match_overload(info, arg_types, &candidate) orelse continue;
            if (best_idx == null or score > best_score) {
                best_idx = idx;
                best_score = score;
                best_bindings.deinit(self.b.allocator);
                best_bindings = candidate;
                candidate = std.ArrayListUnmanaged(generic_binding){};
                continue;
            }
            if (score == best_score) {
                ambiguous = true;
            }
        }

        if (best_idx == null) return error.unknown_function;
        if (ambiguous) return error.ambiguous_overload;

        const info = overloads[best_idx.?];
        const owned = best_bindings.toOwnedSlice(self.b.allocator) catch return error.out_of_memory;
        return .{ .info = info, .bindings = owned };
    }

    fn is_pack_generic(self: *function_ctx, decl: ir_mod.ir.function_decl, name: []const u8) bool {
        for (decl.generics) |param| {
            if (!param.is_pack) continue;
            if (std.mem.eql(u8, self.b.string_value(param.name), name)) return true;
        }
        return false;
    }

    fn constraint_satisfied(self: *function_ctx, ty: type_key, constraint_id: ir_mod.ir_identifier) bool {
        switch (ty) {
            .unknown => return true,
            .name => |type_name| {
                const constraint_name = type_name_from_type_node(self.b, constraint_id) orelse return true;
                if (!self.b.traits.contains(constraint_name)) return true;
                return type_satisfies_constraint(self.b, type_name, constraint_name);
            },
            .dyn_trait => |type_name| {
                const constraint_name = type_name_from_type_node(self.b, constraint_id) orelse return true;
                if (!self.b.traits.contains(constraint_name)) return true;
                return std.mem.eql(u8, type_name, constraint_name);
            },
            .applied => |ap| {
                const constraint_name = type_name_from_type_node(self.b, constraint_id) orelse return true;
                if (!self.b.traits.contains(constraint_name)) return true;
                return type_satisfies_constraint(self.b, ap.base, constraint_name);
            },
        }
    }

    fn check_constraints(
        self: *function_ctx,
        info: function_info,
        bindings: []const generic_binding,
        arg_types: []const type_key,
        fixed_len: usize,
        is_variadic: bool,
    ) bool {
        const decl = info.decl;
        for (decl.generics) |param| {
            if (param.kind != .type) continue;
            if (param.constraint == null) continue;
            if (param.is_pack) {
                if (!is_variadic) continue;
                const pack_param = decl.params[fixed_len];
                const pack_type = type_key_from_type_node_with_self(self.b, pack_param.ty, info.impl_for);
                if (pack_type == .unknown) continue;
                const pack_name = type_key_base_name(pack_type) orelse continue;
                if (!std.mem.eql(u8, pack_name, self.b.string_value(param.name))) continue;
                for (arg_types[fixed_len..]) |arg_type| {
                    if (!self.constraint_satisfied(arg_type, param.constraint.?)) return false;
                }
                continue;
            }
            const param_name = self.b.string_value(param.name);
            const binding = lookup_generic(bindings, param_name) orelse continue;
            if (!self.constraint_satisfied(binding, param.constraint.?)) return false;
        }

        for (decl.where_clause) |req| {
            const req_name = self.b.string_value(req.name);
            const binding = lookup_generic(bindings, req_name) orelse continue;
            if (!self.constraint_satisfied(binding, req.constraint)) return false;
        }

        return true;
    }

    fn resolve_function_overload_index(
        self: *function_ctx,
        overloads: []const function_info,
        arg_types: []const type_key,
    ) codegen_error!usize {
        var best_idx: ?usize = null;
        var best_score: usize = 0;
        var ambiguous = false;
        for (overloads, 0..) |info, idx| {
            const score = self.overload_score(info, arg_types) orelse continue;
            if (best_idx == null or score > best_score) {
                best_idx = idx;
                best_score = score;
                ambiguous = false;
            } else if (score == best_score) {
                ambiguous = true;
            }
        }
        if (best_idx == null) return error.unknown_function;
        if (ambiguous) return error.ambiguous_overload;
        return best_idx.?;
    }

    fn resolve_foreign_overload_index(
        self: *function_ctx,
        overloads: []const foreign_signature,
        arg_types: []const type_key,
    ) codegen_error!usize {
        _ = self;
        var best_idx: ?usize = null;
        var best_score: usize = 0;
        var ambiguous = false;
        for (overloads, 0..) |sig, idx| {
            const score = foreign_overload_score(sig, arg_types) orelse continue;
            if (best_idx == null or score > best_score) {
                best_idx = idx;
                best_score = score;
                ambiguous = false;
            } else if (score == best_score) {
                ambiguous = true;
            }
        }
        if (best_idx == null) return error.unknown_foreign;
        if (ambiguous) return error.ambiguous_overload;
        return best_idx.?;
    }

    fn emit(self: *function_ctx, inst: ink.exe.instruction) codegen_error!void {
        try self.b.emit(inst);
    }

    fn prep_binary(self: *function_ctx, left_id: ir_mod.ir_identifier, right_id: ir_mod.ir_identifier) codegen_error!binary_prep {
        var left_reg = try self.compile_expr(left_id);
        if (left_reg == 0) {
            const tmp = try self.alloc_temp();
            try self.emit(.{ .move = .{ .dst = tmp, .src = left_reg } });
            left_reg = tmp;
        }
        const right_reg = try self.compile_expr(right_id);
        var dst: u8 = undefined;
        if (self.is_temp(left_reg)) {
            dst = left_reg;
        } else if (self.is_temp(right_reg)) {
            dst = right_reg;
        } else {
            dst = try self.alloc_temp();
        }
        return .{
            .left = left_reg,
            .right = right_reg,
            .dst = dst,
            .left_temp = self.is_temp(left_reg),
            .right_temp = self.is_temp(right_reg),
        };
    }

    fn finish_binary(self: *function_ctx, prep: binary_prep) void {
        if (prep.right_temp and prep.dst != prep.right) self.free_temp(prep.right);
        if (prep.left_temp and prep.dst != prep.left) self.free_temp(prep.left);
    }

    fn prep_unary(self: *function_ctx, arg_id: ir_mod.ir_identifier) codegen_error!unary_prep {
        const src = try self.compile_expr(arg_id);
        const dst = if (self.is_temp(src)) src else try self.alloc_temp();
        return .{ .src = src, .dst = dst, .src_temp = self.is_temp(src) };
    }

    fn finish_unary(self: *function_ctx, prep: unary_prep) void {
        if (prep.src_temp and prep.dst != prep.src) self.free_temp(prep.src);
    }

    const call_base = struct {
        name: []const u8,
        receiver: ?ir_mod.ir_identifier,
    };

    fn resolve_call_base(self: *function_ctx, base_id: ir_mod.ir_identifier) codegen_error!call_base {
        const base_node = self.b.node(base_id);
        return switch (base_node) {
            .identifier => |ident| .{ .name = self.b.string_value(ident), .receiver = null },
            .binary => |bin| blk: {
                if (bin.op != .access) return error.unsupported_node;
                const field_node = self.b.node(bin.right);
                if (field_node != .identifier) return error.unsupported_node;
                break :blk .{ .name = self.b.string_value(field_node.identifier), .receiver = bin.left };
            },
            else => error.unsupported_node,
        };
    }

    fn foreign_index(self: *function_ctx, name: []const u8) codegen_error!u32 {
        return self.b.foreigns.get(name) orelse error.unknown_foreign;
    }

    fn emit_foreign_call(self: *function_ctx, foreign_idx: u32, args: []const u8) codegen_error!void {
        if (args.len > 7) return error.register_overflow;
        for (args, 0..) |arg_reg, idx| {
            const dst: u8 = @intCast(idx + 1);
            try self.emit(.{ .argument_set = .{ .dst = dst, .src = arg_reg } });
        }
        try self.emit(.{ .call_foreign = .{ .index = foreign_idx } });
    }

    fn is_unit(self: *function_ctx, id: ir_mod.ir_identifier) bool {
        const node = self.b.node(id);
        return switch (node) {
            .identifier => |ident| std.mem.eql(u8, self.b.string_value(ident), "unit"),
            else => false,
        };
    }

    fn trait_name_from_type(self: *function_ctx, ty: type_key) ?[]const u8 {
        return switch (ty) {
            .dyn_trait => |name| name,
            .name => |name| if (self.b.traits.contains(name)) name else null,
            .applied => |ap| if (self.b.traits.contains(ap.base)) ap.base else null,
            .unknown => null,
        };
    }

    fn is_builtin_type_key(self: *function_ctx, ty: type_key) bool {
        _ = self;
        const name = type_key_base_name(ty) orelse return false;
        return is_builtin_type_name(name);
    }

    fn is_int_type_key(self: *function_ctx, ty: type_key) bool {
        _ = self;
        const name = type_key_base_name(ty) orelse return false;
        return is_int_type_name(name);
    }

    fn is_float_type_key(self: *function_ctx, ty: type_key) bool {
        _ = self;
        const name = type_key_base_name(ty) orelse return false;
        return is_float_type_name(name);
    }

    fn is_bool_type_key(self: *function_ctx, ty: type_key) bool {
        _ = self;
        const name = type_key_base_name(ty) orelse return false;
        return is_bool_type_name(name);
    }

    fn is_known_non_builtin(self: *function_ctx, ty: type_key) bool {
        const name = type_key_base_name(ty) orelse return false;
        if (is_builtin_type_name(name)) return false;
        if (self.b.traits.contains(name)) return false;
        return true;
    }

    fn compile_expr(self: *function_ctx, id: ir_mod.ir_identifier) codegen_error!u8 {
        const node = self.b.node(id);
        return switch (node) {
            .integer => |value| blk: {
                const idx = try self.b.intern_const(@bitCast(value));
                const reg = try self.alloc_temp();
                try self.emit(.{ .load_const = .{ .dst = reg, .const_index = idx } });
                break :blk reg;
            },
            .float => |value| blk: {
                const idx = try self.b.intern_const(@bitCast(value));
                const reg = try self.alloc_temp();
                try self.emit(.{ .load_const = .{ .dst = reg, .const_index = idx } });
                break :blk reg;
            },
            .string => |value| blk: {
                const idx = try self.b.intern_const(@as(u64, @intCast(value.idx)));
                const reg = try self.alloc_temp();
                try self.emit(.{ .load_const = .{ .dst = reg, .const_index = idx } });
                break :blk reg;
            },
            .boolean => |value| blk: {
                const idx = try self.b.intern_const(@intFromBool(value));
                const reg = try self.alloc_temp();
                try self.emit(.{ .load_const = .{ .dst = reg, .const_index = idx } });
                break :blk reg;
            },
            .identifier => |ident| blk: {
                const name = self.b.string_value(ident);
                if (self.locals.get(name)) |reg| break :blk reg;
                return error.unknown_identifier;
            },
            .unary => |un| switch (un.op) {
                .ret => {
                    const reg = try self.compile_expr(un.right);
                    try self.emit(.{ .ret_value = .{ .src = reg } });
                    self.returned = true;
                    return reg;
                },
                .dynamic, .@"comptime" => return self.compile_expr(un.right),
                .spawn => return self.compile_spawn_expr(un.right),
                .await => return self.compile_await_expr(un.right),
                .@"try" => return self.compile_try_expr(un.right),
                .unwrap_optional => return self.compile_optional_unwrap(un.right),
                .neg, .not, .bit_not => return self.compile_overloadable_unary(un.op, un.right),
                else => return error.unsupported_node,
            },
            .binary => |bin| switch (bin.op) {
                .call => try self.compile_call(id),
                .access => try self.compile_access(bin.left, bin.right),
                .index => try self.compile_index_expr(bin.left, bin.right),
                .@"as" => try self.compile_as_expr(bin.left, bin.right),
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
                => try self.compile_assign_expr(bin.op, bin.left, bin.right),
                .logical_and => try self.compile_logical_and(bin.left, bin.right),
                .logical_or => try self.compile_logical_or(bin.left, bin.right),
                .logical_xor => try self.compile_logical_xor(bin.left, bin.right),
                .add,
                .sub,
                .mul,
                .div,
                .mod,
                .bit_and,
                .bit_or,
                .bit_xor,
                .shl,
                .shr,
                .less_than,
                .less_or_equal,
                .greater_than,
                .greater_or_equal,
                .equal,
                .not_equal,
                => try self.compile_overloadable_binary(bin.op, bin.left, bin.right),
                else => return error.unsupported_node,
            },
            .intrinsic => |call| return self.compile_intrinsic(call),
            .record_literal => |rec| return self.compile_record_literal(rec),
            .block => |items| {
                const count = items.len;
                var idx: usize = 0;
                var last_reg: u8 = 0;
                while (idx < count) : (idx += 1) {
                    const reg = try self.compile_expr(items[idx]);
                    if (idx + 1 == count) {
                        last_reg = reg;
                    } else if (self.is_temp(reg)) {
                        self.free_temp(reg);
                    }
                    if (self.returned) return reg;
                }
                return last_reg;
            },
            .decl => |decl| switch (decl) {
                .@"const" => |c| try self.compile_local_decl(c.name, c.ty, c.value),
                .@"var" => |v| try self.compile_local_decl(v.name, v.ty, v.value),
                else => return error.unsupported_node,
            },
            .if_expr => |ife| blk: {
                const cond_reg = try self.compile_expr(ife.condition);
                const else_label = self.b.new_label();
                const end_label = self.b.new_label();
                try self.emit(.{ .jump_if_false = .{ .condition = cond_reg, .target = else_label } });
                if (self.is_temp(cond_reg)) self.free_temp(cond_reg);

                const dst = try self.alloc_temp();
                const then_reg = try self.compile_expr(ife.then_branch);
                if (then_reg != dst) {
                    try self.emit(.{ .move = .{ .dst = dst, .src = then_reg } });
                }
                if (self.is_temp(then_reg)) self.free_temp(then_reg);
                try self.emit(.{ .jump = .{ .target = end_label } });

                try self.emit(.{ .label = .{ .id = else_label } });
                if (ife.else_branch) |else_ref| {
                    const else_reg = try self.compile_expr(else_ref);
                    if (else_reg != dst) {
                        try self.emit(.{ .move = .{ .dst = dst, .src = else_reg } });
                    }
                    if (self.is_temp(else_reg)) self.free_temp(else_reg);
                } else {
                    return error.unsupported_node;
                }

                try self.emit(.{ .label = .{ .id = end_label } });
                break :blk dst;
            },
            .select_expr => |se| return self.compile_select_expr(se.arms),
            else => return error.unsupported_node,
        };
    }

    fn compile_spawn_expr(self: *function_ctx, id: ir_mod.ir_identifier) codegen_error!u8 {
        const node = self.b.node(id);
        return switch (node) {
            .binary => |bin| if (bin.op == .call) try self.compile_spawn_call(id) else error.unsupported_node,
            else => error.unsupported_node,
        };
    }

    fn compile_spawn_call(self: *function_ctx, id: ir_mod.ir_identifier) codegen_error!u8 {
        var base_id = id;
        var args = std.array_list.Managed(ir_mod.ir_identifier).init(self.b.allocator);
        defer args.deinit();

        while (true) {
            const node = self.b.node(base_id);
            switch (node) {
                .binary => |bin| {
                    if (bin.op == .call) {
                        args.append(bin.right) catch return error.out_of_memory;
                        base_id = bin.left;
                        continue;
                    }
                },
                else => {},
            }
            break;
        }

        std.mem.reverse(ir_mod.ir_identifier, args.items);

        const base_node = self.b.node(base_id);
        const name = switch (base_node) {
            .identifier => |ident| self.b.string_value(ident),
            else => return error.unsupported_node,
        };

        if (args.items.len == 1 and self.is_unit(args.items[0])) {
            args.clearRetainingCapacity();
        }

        if (self.b.foreigns.contains(name)) return error.unsupported_node;
        if (args.items.len > 7) return error.register_overflow;

        var arg_types: [7]type_key = undefined;
        for (args.items, 0..) |arg_id, idx| {
            arg_types[idx] = self.infer_expr_type(arg_id);
        }
        const arg_type_slice = arg_types[0..args.items.len];

        const group = self.b.functions.get(name) orelse return error.unknown_function;
        const resolved = try self.resolve_function_overload(group.items, arg_type_slice);
        defer if (resolved.bindings.len > 0) self.b.allocator.free(resolved.bindings);
        const info = resolved.info;

        for (args.items, 0..) |arg_id, idx| {
            const arg_reg = try self.compile_expr(arg_id);
            const dst: u8 = @intCast(idx + 1);
            try self.emit(.{ .argument_set = .{ .dst = dst, .src = arg_reg } });
            if (self.is_temp(arg_reg)) self.free_temp(arg_reg);
        }

        const dst_reg = try self.alloc_temp();
        try self.emit(.{ .task_spawn = .{ .dst = dst_reg, .target = info.label, .argc = @intCast(args.items.len) } });
        return dst_reg;
    }

    fn compile_await_expr(self: *function_ctx, id: ir_mod.ir_identifier) codegen_error!u8 {
        const src_reg = try self.compile_expr(id);
        const dst_reg = if (self.is_temp(src_reg)) src_reg else try self.alloc_temp();
        try self.emit(.{ .task_await = .{ .dst = dst_reg, .src = src_reg } });
        if (self.is_temp(src_reg) and src_reg != dst_reg) self.free_temp(src_reg);
        return dst_reg;
    }

    fn compile_try_expr(self: *function_ctx, arg_id: ir_mod.ir_identifier) codegen_error!u8 {
        var result_reg = try self.compile_expr(arg_id);
        if (result_reg == 0) {
            const tmp = try self.alloc_temp();
            try self.emit(.{ .move = .{ .dst = tmp, .src = result_reg } });
            result_reg = tmp;
        }

        const err_label = self.b.new_label();
        const end_label = self.b.new_label();

        const ok_reg = try self.emit_foreign_call_reg("std::result_is_ok", result_reg);
        try self.emit(.{ .jump_if_false = .{ .condition = ok_reg, .target = err_label } });
        if (self.is_temp(ok_reg)) self.free_temp(ok_reg);

        const value_reg = try self.emit_foreign_call_reg("std::result_unwrap", result_reg);
        const dst: u8 = if (self.is_temp(result_reg))
            result_reg
        else if (self.is_temp(value_reg))
            value_reg
        else
            try self.alloc_temp();

        if (value_reg != dst) {
            try self.emit(.{ .move = .{ .dst = dst, .src = value_reg } });
        }
        if (self.is_temp(value_reg) and dst != value_reg) self.free_temp(value_reg);
        const free_result = self.is_temp(result_reg) and dst != result_reg;

        try self.emit(.{ .jump = .{ .target = end_label } });

        try self.emit(.{ .label = .{ .id = err_label } });
        try self.emit(.{ .ret_value = .{ .src = result_reg } });
        self.returned = true;

        try self.emit(.{ .label = .{ .id = end_label } });
        if (free_result) self.free_temp(result_reg);
        return dst;
    }

    fn compile_optional_unwrap(self: *function_ctx, arg_id: ir_mod.ir_identifier) codegen_error!u8 {
        const value_reg = try self.compile_expr(arg_id);
        const zero_idx = try self.b.intern_const(0);
        const zero_reg = try self.alloc_temp();
        try self.emit(.{ .load_const = .{ .dst = zero_reg, .const_index = zero_idx } });

        const cond_reg = try self.alloc_temp();
        try self.emit(.{ .compare_eq = .{ .dst = cond_reg, .src_a = value_reg, .src_b = zero_reg } });
        const ok_label = self.b.new_label();
        try self.emit(.{ .jump_if_false = .{ .condition = cond_reg, .target = ok_label } });

        if (self.is_temp(cond_reg)) self.free_temp(cond_reg);
        if (self.is_temp(zero_reg)) self.free_temp(zero_reg);

        try self.emit(.{ .ret_value = .{ .src = value_reg } });
        self.returned = true;

        try self.emit(.{ .label = .{ .id = ok_label } });
        return value_reg;
    }

    fn load_const_reg(self: *function_ctx, value: u64) codegen_error!u8 {
        const idx = try self.b.intern_const(value);
        const reg = try self.alloc_temp();
        try self.emit(.{ .load_const = .{ .dst = reg, .const_index = idx } });
        return reg;
    }

    fn alloc_words(self: *function_ctx, count: u64) codegen_error!u8 {
        const size_reg = try self.load_const_reg(count);
        const alloc_idx = try self.foreign_index("std::alloc");
        try self.emit_foreign_call(alloc_idx, &[_]u8{size_reg});
        self.free_temp(size_reg);

        const ptr_reg = try self.alloc_temp();
        try self.emit(.{ .move = .{ .dst = ptr_reg, .src = 0 } });
        return ptr_reg;
    }

    fn deref_to_temp(self: *function_ctx, ptr_reg: u8) codegen_error!u8 {
        const deref_idx = try self.foreign_index("std::deref");
        try self.emit_foreign_call(deref_idx, &[_]u8{ptr_reg});
        const dst = try self.alloc_temp();
        try self.emit(.{ .move = .{ .dst = dst, .src = 0 } });
        return dst;
    }

    fn deref_into(self: *function_ctx, ptr_reg: u8, dst_reg: u8) codegen_error!void {
        const deref_idx = try self.foreign_index("std::deref");
        try self.emit_foreign_call(deref_idx, &[_]u8{ptr_reg});
        try self.emit(.{ .move = .{ .dst = dst_reg, .src = 0 } });
    }

    fn store_value(self: *function_ctx, ptr_reg: u8, value_reg: u8) codegen_error!void {
        const store_idx = try self.foreign_index("std::store");
        try self.emit_foreign_call(store_idx, &[_]u8{ptr_reg, value_reg});
    }

    fn load_label_const_reg(self: *function_ctx, label: ink.exe.label_id) codegen_error!u8 {
        const idx = try self.b.const_index_for_label(label);
        const reg = try self.alloc_temp();
        try self.emit(.{ .load_const = .{ .dst = reg, .const_index = idx } });
        return reg;
    }

    fn compile_as_expr(self: *function_ctx, value_id: ir_mod.ir_identifier, trait_id: ir_mod.ir_identifier) codegen_error!u8 {
        const type_node = self.b.node(trait_id);
        if (type_node == .type and type_node.type == .dyn) {
            const trait_name = type_name_from_type_node(self.b, trait_id) orelse return error.unsupported_node;
            if (!self.b.traits.contains(trait_name)) {
                return error.unsupported_node;
            }

            const value_type = self.infer_expr_type(value_id);
            const type_name = switch (value_type) {
                .name => |name| name,
                .dyn_trait => return error.unsupported_node,
                .applied => |ap| ap.base,
                .unknown => return error.unsupported_node,
            };

            const vtable_map = self.b.trait_vtables.get(trait_name) orelse return error.unsupported_node;
            const labels = vtable_map.get(type_name) orelse return error.unsupported_node;

            const obj_reg = try self.alloc_words(2);
            var value_reg = try self.compile_expr(value_id);
            var value_temp: ?u8 = null;
            if (value_reg == 0) {
                const tmp = try self.alloc_temp();
                try self.emit(.{ .move = .{ .dst = tmp, .src = value_reg } });
                value_reg = tmp;
                value_temp = tmp;
            }

            const vtable_reg = try self.alloc_words(labels.len);
            for (labels, 0..) |label, idx| {
                const ptr_reg = if (idx == 0) vtable_reg else blk: {
                    const offset_idx = try self.b.intern_const(@intCast(idx));
                    const ptr_reg = try self.alloc_temp();
                    try self.emit(.{ .load_const = .{ .dst = ptr_reg, .const_index = offset_idx } });
                    try self.emit(.{ .add = .{ .dst = ptr_reg, .src_a = vtable_reg, .src_b = ptr_reg } });
                    break :blk ptr_reg;
                };

                const label_idx = try self.b.const_index_for_label(label);
                const label_reg = try self.alloc_temp();
                try self.emit(.{ .load_const = .{ .dst = label_reg, .const_index = label_idx } });
                try self.store_value(ptr_reg, label_reg);
                self.free_temp(label_reg);
                if (idx != 0) self.free_temp(ptr_reg);
            }

            try self.store_value(obj_reg, vtable_reg);

            const offset_idx = try self.b.intern_const(1);
            const offset_reg = try self.alloc_temp();
            try self.emit(.{ .load_const = .{ .dst = offset_reg, .const_index = offset_idx } });
            try self.emit(.{ .add = .{ .dst = offset_reg, .src_a = obj_reg, .src_b = offset_reg } });
            try self.store_value(offset_reg, value_reg);
            self.free_temp(offset_reg);

            if (self.is_temp(vtable_reg)) self.free_temp(vtable_reg);
            if (value_temp) |tmp| {
                self.free_temp(tmp);
            } else if (self.is_temp(value_reg)) {
                self.free_temp(value_reg);
            }
            return obj_reg;
        }

        if (type_name_from_type_node(self.b, trait_id)) |trait_name| {
            if (self.b.traits.contains(trait_name)) return error.unsupported_node;
        }
        return self.compile_expr(value_id);
    }

    fn compile_select_expr(self: *function_ctx, arms: []const ir_mod.ir.select_arm) codegen_error!u8 {
        if (arms.len == 0) return error.unsupported_node;
        if (arms.len > 7) return error.register_overflow;

        const result_reg = try self.alloc_temp();
        const base_reg = try self.alloc_temp();
        var i: usize = 0;
        while (i < arms.len) : (i += 1) {
            if (i > 0) _ = try self.alloc_temp();
            const target_reg: u8 = @intCast(base_reg + i);
            const task_reg = try self.compile_expr(arms[i].task);
            if (task_reg != target_reg) {
                try self.emit(.{ .move = .{ .dst = target_reg, .src = task_reg } });
                if (self.is_temp(task_reg)) self.free_temp(task_reg);
            }
        }

        const selected_reg = try self.alloc_temp();
        try self.emit(.{ .task_await_any = .{ .dst = selected_reg, .src = base_reg, .count = @intCast(arms.len) } });

        var arm_labels = std.array_list.Managed(ink.exe.label_id).init(self.b.allocator);
        defer arm_labels.deinit();
        for (arms) |_| {
            arm_labels.append(self.b.new_label()) catch return error.out_of_memory;
        }
        const end_label = self.b.new_label();

        i = 0;
        while (i < arms.len) : (i += 1) {
            const task_reg: u8 = @intCast(base_reg + i);
            const cond_reg = try self.alloc_temp();
            try self.emit(.{ .compare_eq = .{ .dst = cond_reg, .src_a = selected_reg, .src_b = task_reg } });
            try self.emit(.{ .jump_if_true = .{ .condition = cond_reg, .target = arm_labels.items[i] } });
            if (self.is_temp(cond_reg)) self.free_temp(cond_reg);
        }
        try self.emit(.{ .jump = .{ .target = end_label } });

        i = 0;
        while (i < arms.len) : (i += 1) {
            try self.emit(.{ .label = .{ .id = arm_labels.items[i] } });

            var j: usize = 0;
            while (j < arms.len) : (j += 1) {
                if (j == i) continue;
                if (arms[j].detached) continue;
                const cancel_reg: u8 = @intCast(base_reg + j);
                try self.emit(.{ .task_cancel = .{ .src = cancel_reg } });
            }

            const task_reg: u8 = @intCast(base_reg + i);
            const await_reg = try self.alloc_temp();
            try self.emit(.{ .task_await = .{ .dst = await_reg, .src = task_reg } });

            var name_restore: ?u8 = null;
            var type_restore: ?type_key = null;
            if (arms[i].name) |name_id| {
                const name = self.b.string_value(name_id);
                name_restore = self.locals.get(name);
                type_restore = self.local_types.get(name);
                self.locals.put(name, await_reg) catch return error.out_of_memory;
                self.local_types.put(name, .unknown) catch return error.out_of_memory;
                self.pin_temp(await_reg);
            }

            const body_reg = try self.compile_expr(arms[i].body);
            if (body_reg != result_reg) {
                try self.emit(.{ .move = .{ .dst = result_reg, .src = body_reg } });
            }
            if (self.is_temp(body_reg) and body_reg != result_reg) self.free_temp(body_reg);

            if (arms[i].name) |name_id| {
                const name = self.b.string_value(name_id);
                _ = self.locals.remove(name);
                _ = self.local_types.remove(name);
                if (name_restore) |prev| {
                    self.locals.put(name, prev) catch return error.out_of_memory;
                }
                if (type_restore) |prev_ty| {
                    self.local_types.put(name, prev_ty) catch return error.out_of_memory;
                }
                self.unpin_temp(await_reg);
            }

            if (self.is_temp(await_reg)) self.free_temp(await_reg);
            try self.emit(.{ .jump = .{ .target = end_label } });
        }

        try self.emit(.{ .label = .{ .id = end_label } });

        var reg = selected_reg;
        while (true) {
            if (reg < base_reg) break;
            self.free_temp(reg);
            if (reg == base_reg) break;
            reg -= 1;
        }
        return result_reg;
    }

    fn emit_ptr_of(self: *function_ctx, reg_index: u8) codegen_error!u8 {
        const idx_const = try self.b.intern_const(@intCast(reg_index));
        const tmp = try self.alloc_temp();
        try self.emit(.{ .load_const = .{ .dst = tmp, .const_index = idx_const } });
        const foreign_idx = try self.foreign_index("std::ptr_of");
        try self.emit_foreign_call(foreign_idx, &[_]u8{tmp});
        self.free_temp(tmp);
        return 0;
    }

    fn compile_borrow_value(self: *function_ctx, arg_id: ir_mod.ir_identifier) codegen_error!u8 {
        var value_reg = try self.compile_expr(arg_id);
        var value_temp: ?u8 = null;
        if (value_reg == 0) {
            const tmp = try self.alloc_temp();
            try self.emit(.{ .move = .{ .dst = tmp, .src = value_reg } });
            value_reg = tmp;
            value_temp = tmp;
        }

        const size_const = try self.b.intern_const(1);
        const size_reg = try self.alloc_temp();
        try self.emit(.{ .load_const = .{ .dst = size_reg, .const_index = size_const } });
        const alloc_idx = try self.foreign_index("std::alloc");
        try self.emit_foreign_call(alloc_idx, &[_]u8{size_reg});
        self.free_temp(size_reg);

        const store_idx = try self.foreign_index("std::store");
        try self.emit_foreign_call(store_idx, &[_]u8{@as(u8, 0), value_reg});
        if (value_temp) |tmp| self.free_temp(tmp);

        return 0;
    }

    fn compile_borrow_call(self: *function_ctx, args: []const ir_mod.ir_identifier) codegen_error!u8 {
        if (args.len != 1) return error.unsupported_node;
        const arg_id = args[0];
        const arg_node = self.b.node(arg_id);
        switch (arg_node) {
            .identifier => |ident| {
                const name = self.b.string_value(ident);
                if (self.locals.get(name)) |reg| {
                    return self.emit_ptr_of(reg);
                }
            },
            else => {},
        }
        return self.compile_borrow_value(arg_id);
    }

    const foreign_name = struct {
        name: []const u8,
        owned: bool,
    };

    fn foreign_name_from_expr(self: *function_ctx, id: ir_mod.ir_identifier) codegen_error!foreign_name {
        const node = self.b.node(id);
        switch (node) {
            .unary => |un| {
                if (un.op == .dynamic or un.op == .@"comptime") return self.foreign_name_from_expr(un.right);
            },
            .identifier => |ident| return .{ .name = self.b.string_value(ident), .owned = false },
            .binary => |bin| {
                if (bin.op == .scope_access) {
                    const left = try self.foreign_name_from_expr(bin.left);
                    const right = try self.foreign_name_from_expr(bin.right);
                    defer {
                        if (left.owned) self.b.allocator.free(left.name);
                        if (right.owned) self.b.allocator.free(right.name);
                    }

                    const sep = "::";
                    const buf = self.b.allocator.alloc(u8, left.name.len + sep.len + right.name.len) catch {
                        return error.out_of_memory;
                    };
                    std.mem.copyForwards(u8, buf[0..left.name.len], left.name);
                    std.mem.copyForwards(u8, buf[left.name.len .. left.name.len + sep.len], sep);
                    std.mem.copyForwards(u8, buf[left.name.len + sep.len ..], right.name);
                    return .{ .name = buf, .owned = true };
                }
            },
            else => {},
        }
        return error.unsupported_node;
    }

    fn compile_foreign_call(self: *function_ctx, id: ir_mod.ir_identifier) codegen_error!u8 {
        var base_id = id;
        var args = std.array_list.Managed(ir_mod.ir_identifier).init(self.b.allocator);
        defer args.deinit();

        while (true) {
            const node = self.b.node(base_id);
            switch (node) {
                .unary => |un| {
                    if (un.op == .dynamic or un.op == .@"comptime") {
                        base_id = un.right;
                        continue;
                    }
                },
                .binary => |bin| {
                    if (bin.op == .call) {
                        args.append(bin.right) catch return error.out_of_memory;
                        base_id = bin.left;
                        continue;
                    }
                },
                else => {},
            }
            break;
        }

        std.mem.reverse(ir_mod.ir_identifier, args.items);

        const name_info = try self.foreign_name_from_expr(base_id);
        defer if (name_info.owned) self.b.allocator.free(name_info.name);

        if (args.items.len == 1 and self.is_unit(args.items[0])) {
            args.clearRetainingCapacity();
        }

        if (args.items.len > 7) return error.register_overflow;

        const foreign_idx = try self.foreign_index(name_info.name);
        for (args.items, 0..) |arg_id, idx| {
            const arg_reg = try self.compile_expr(arg_id);
            const dst: u8 = @intCast(idx + 1);
            try self.emit(.{ .argument_set = .{ .dst = dst, .src = arg_reg } });
            if (self.is_temp(arg_reg)) self.free_temp(arg_reg);
        }
        try self.emit(.{ .call_foreign = .{ .index = foreign_idx } });
        return 0;
    }

    fn emit_foreign_call_named(self: *function_ctx, name: []const u8, args: []const ir_mod.ir_identifier) codegen_error!u8 {
        if (args.len > 7) return error.register_overflow;
        for (args, 0..) |arg_id, idx| {
            const arg_reg = try self.compile_expr(arg_id);
            const dst: u8 = @intCast(idx + 1);
            try self.emit(.{ .argument_set = .{ .dst = dst, .src = arg_reg } });
            if (self.is_temp(arg_reg)) self.free_temp(arg_reg);
        }
        const foreign_idx = try self.foreign_index(name);
        try self.emit(.{ .call_foreign = .{ .index = foreign_idx } });
        return 0;
    }

    fn emit_foreign_call_reg(self: *function_ctx, name: []const u8, arg_reg: u8) codegen_error!u8 {
        try self.emit(.{ .argument_set = .{ .dst = 1, .src = arg_reg } });
        const foreign_idx = try self.foreign_index(name);
        try self.emit(.{ .call_foreign = .{ .index = foreign_idx } });
        return 0;
    }

    fn save_result_reg(self: *function_ctx, reg: u8) codegen_error!u8 {
        if (reg != 0) return reg;
        const tmp = try self.alloc_temp();
        try self.emit(.{ .move = .{ .dst = tmp, .src = reg } });
        return tmp;
    }

    fn coerce_string(self: *function_ctx, arg_reg: u8, arg_type: type_key) codegen_error!u8 {
        const name = type_key_base_name(arg_type) orelse return error.unsupported_node;
        if (std.mem.eql(u8, name, "string")) return arg_reg;

        const foreign_fn = if (std.mem.eql(u8, name, "int"))
            "std::string_from_int"
        else if (std.mem.eql(u8, name, "float"))
            "std::string_from_float"
        else if (std.mem.eql(u8, name, "bool"))
            "std::string_from_bool"
        else
            return error.unsupported_node;

        _ = try self.emit_foreign_call_reg(foreign_fn, arg_reg);
        if (self.is_temp(arg_reg)) self.free_temp(arg_reg);
        return 0;
    }

    fn compile_interpolate(self: *function_ctx, args: []const ir_mod.ir_identifier) codegen_error!u8 {
        if (args.len == 0) {
            const zero_idx = try self.b.intern_const(0);
            const zero_reg = try self.alloc_temp();
            try self.emit(.{ .load_const = .{ .dst = zero_reg, .const_index = zero_idx } });
            _ = try self.emit_foreign_call_reg("std::string_new", zero_reg);
            self.free_temp(zero_reg);
            return self.save_result_reg(0);
        }

        var current_reg: ?u8 = null;
        for (args) |arg_id| {
            const arg_reg = try self.compile_expr(arg_id);
            const arg_type = self.infer_expr_type(arg_id);
            const value_reg = try self.coerce_string(arg_reg, arg_type);
            const saved_reg = try self.save_result_reg(value_reg);

            if (current_reg == null) {
                current_reg = saved_reg;
                continue;
            }

            try self.emit(.{ .argument_set = .{ .dst = 1, .src = current_reg.? } });
            try self.emit(.{ .argument_set = .{ .dst = 2, .src = saved_reg } });
            const concat_idx = try self.foreign_index("std::string_concat");
            try self.emit(.{ .call_foreign = .{ .index = concat_idx } });

            const result_reg = try self.save_result_reg(0);
            if (self.is_temp(current_reg.?)) self.free_temp(current_reg.?);
            if (self.is_temp(saved_reg) and saved_reg != result_reg and saved_reg != current_reg.?) {
                self.free_temp(saved_reg);
            }
            current_reg = result_reg;
        }

        return current_reg.?;
    }

    fn compile_intrinsic(self: *function_ctx, call: ir_mod.intrinsic) codegen_error!u8 {
        const name = self.b.string_value(call.name);
        const def = intrinsic.lookup(name) orelse return error.unsupported_node;
        const args = call.args;
        if (!def.variadic and args.len != @as(usize, def.arity)) return error.unsupported_node;

        switch (def.id) {
            .interpolate => return self.compile_interpolate(args),
            .iadd => return self.emit_int_binary(args[0], args[1], .add),
            .isub => return self.emit_int_binary(args[0], args[1], .sub),
            .imul => return self.emit_int_binary(args[0], args[1], .mul),
            .idiv => return self.emit_int_binary(args[0], args[1], .div),
            .irem => return self.emit_int_binary(args[0], args[1], .rem),
            .imin => return self.emit_int_binary(args[0], args[1], .min),
            .imax => return self.emit_int_binary(args[0], args[1], .max),
            .ieq => return self.emit_int_binary(args[0], args[1], .compare_eq),
            .ilt => return self.emit_int_binary(args[0], args[1], .compare_lt),
            .ile => return self.emit_int_binary(args[0], args[1], .compare_le),
            .igt => return self.emit_int_binary(args[0], args[1], .compare_gt),
            .ige => return self.emit_int_binary(args[0], args[1], .compare_ge),
            .ine => return self.emit_int_not_equal(args[0], args[1]),
            .ineg => return self.emit_int_unary(args[0], .int_neg),
            .iabs => return self.emit_int_unary(args[0], .int_abs),
            .bnot => return self.emit_int_unary(args[0], .bit_not),
            .band => return self.emit_int_binary(args[0], args[1], .bit_and),
            .bor => return self.emit_int_binary(args[0], args[1], .bit_or),
            .bxor => return self.emit_int_binary(args[0], args[1], .bit_xor),
            .shl => return self.emit_int_binary(args[0], args[1], .bit_shl),
            .shr => return self.emit_int_binary(args[0], args[1], .bit_shr),
            .sar => return self.emit_int_binary(args[0], args[1], .bit_sar),
            .rol => return self.emit_int_binary(args[0], args[1], .bit_rol),
            .ror => return self.emit_int_binary(args[0], args[1], .bit_ror),

            .fadd => return self.emit_float_binary(args[0], args[1], .fadd),
            .fsub => return self.emit_float_binary(args[0], args[1], .fsub),
            .fmul => return self.emit_float_binary(args[0], args[1], .fmul),
            .fdiv => return self.emit_float_binary(args[0], args[1], .fdiv),
            .frem => return self.emit_float_binary(args[0], args[1], .frem),
            .fmin => return self.emit_float_binary(args[0], args[1], .fmin),
            .fmax => return self.emit_float_binary(args[0], args[1], .fmax),
            .feq => return self.emit_float_binary(args[0], args[1], .fcompare_eq),
            .flt => return self.emit_float_binary(args[0], args[1], .fcompare_lt),
            .fgt => return self.emit_float_binary(args[0], args[1], .fcompare_gt),
            .fne => return self.emit_float_not_equal(args[0], args[1]),
            .fle => return self.emit_float_invert_compare(args[0], args[1], .fcompare_gt),
            .fge => return self.emit_float_invert_compare(args[0], args[1], .fcompare_lt),
            .fneg => return self.emit_float_unary(args[0], .fneg),
            .fabs => return self.emit_float_unary(args[0], .fabs),
            .sqrt => return self.emit_float_unary(args[0], .fsqrt),
            .sin => return self.emit_float_unary(args[0], .fsin),
            .cos => return self.emit_float_unary(args[0], .fcos),
            .tan => return self.emit_float_unary(args[0], .ftan),
            .asin => return self.emit_float_unary(args[0], .fasin),
            .acos => return self.emit_float_unary(args[0], .facos),
            .atan => return self.emit_float_unary(args[0], .fatan),
            .floor => return self.emit_float_unary(args[0], .ffloor),
            .ceil => return self.emit_float_unary(args[0], .fceil),
            .round => return self.emit_float_unary(args[0], .fround),
            .trunc => return self.emit_float_unary(args[0], .ftrunc),
            .alloc => return self.emit_foreign_call_named("std::alloc", args),
            .free => return self.emit_foreign_call_named("std::free", args),
            .deref => return self.emit_foreign_call_named("std::deref", args),
            .result_ok => return self.emit_foreign_call_named("std::result_ok", args),
            .result_err => return self.emit_foreign_call_named("std::result_err", args),
            .result_is_ok => return self.emit_foreign_call_named("std::result_is_ok", args),
            .result_unwrap => return self.emit_foreign_call_named("std::result_unwrap", args),
            .result_unwrap_err => return self.emit_foreign_call_named("std::result_unwrap_err", args),
            .borrow, .borrow_mut => return self.compile_borrow_call(args),
        }
    }

    fn emit_int_unary(self: *function_ctx, arg_id: ir_mod.ir_identifier, op: enum { bit_not, int_neg, int_abs }) codegen_error!u8 {
        const prep = try self.prep_unary(arg_id);
        switch (op) {
            .bit_not => try self.emit(.{ .bit_not = .{ .dst = prep.dst, .src_a = prep.src, .src_b = 0 } }),
            .int_neg => try self.emit(.{ .int_neg = .{ .dst = prep.dst, .src_a = prep.src, .src_b = 0 } }),
            .int_abs => try self.emit(.{ .int_abs = .{ .dst = prep.dst, .src_a = prep.src, .src_b = 0 } }),
        }
        self.finish_unary(prep);
        return prep.dst;
    }

    fn emit_float_unary(self: *function_ctx, arg_id: ir_mod.ir_identifier, op: enum {
        fneg,
        fabs,
        fsqrt,
        fsin,
        fcos,
        ftan,
        fasin,
        facos,
        fatan,
        ffloor,
        fceil,
        fround,
        ftrunc,
    }) codegen_error!u8 {
        const prep = try self.prep_unary(arg_id);
        switch (op) {
            .fneg => try self.emit(.{ .fneg = .{ .dst = prep.dst, .src_a = prep.src, .src_b = 0 } }),
            .fabs => try self.emit(.{ .fabs = .{ .dst = prep.dst, .src_a = prep.src, .src_b = 0 } }),
            .fsqrt => try self.emit(.{ .fsqrt = .{ .dst = prep.dst, .src_a = prep.src, .src_b = 0 } }),
            .fsin => try self.emit(.{ .fsin = .{ .dst = prep.dst, .src_a = prep.src, .src_b = 0 } }),
            .fcos => try self.emit(.{ .fcos = .{ .dst = prep.dst, .src_a = prep.src, .src_b = 0 } }),
            .ftan => try self.emit(.{ .ftan = .{ .dst = prep.dst, .src_a = prep.src, .src_b = 0 } }),
            .fasin => try self.emit(.{ .fasin = .{ .dst = prep.dst, .src_a = prep.src, .src_b = 0 } }),
            .facos => try self.emit(.{ .facos = .{ .dst = prep.dst, .src_a = prep.src, .src_b = 0 } }),
            .fatan => try self.emit(.{ .fatan = .{ .dst = prep.dst, .src_a = prep.src, .src_b = 0 } }),
            .ffloor => try self.emit(.{ .ffloor = .{ .dst = prep.dst, .src_a = prep.src, .src_b = 0 } }),
            .fceil => try self.emit(.{ .fceil = .{ .dst = prep.dst, .src_a = prep.src, .src_b = 0 } }),
            .fround => try self.emit(.{ .fround = .{ .dst = prep.dst, .src_a = prep.src, .src_b = 0 } }),
            .ftrunc => try self.emit(.{ .ftrunc = .{ .dst = prep.dst, .src_a = prep.src, .src_b = 0 } }),
        }
        self.finish_unary(prep);
        return prep.dst;
    }

    fn emit_int_binary(
        self: *function_ctx,
        left_id: ir_mod.ir_identifier,
        right_id: ir_mod.ir_identifier,
        op: enum {
            add,
            sub,
            mul,
            div,
            rem,
            min,
            max,
            bit_and,
            bit_or,
            bit_xor,
            bit_shl,
            bit_shr,
            bit_sar,
            bit_rol,
            bit_ror,
            compare_eq,
            compare_lt,
            compare_le,
            compare_gt,
            compare_ge,
        },
    ) codegen_error!u8 {
        const prep = try self.prep_binary(left_id, right_id);
        switch (op) {
            .add => try self.emit(.{ .add = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .sub => try self.emit(.{ .sub = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .mul => try self.emit(.{ .mul = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .div => try self.emit(.{ .div = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .rem => try self.emit(.{ .rem = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .min => try self.emit(.{ .min = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .max => try self.emit(.{ .max = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .bit_and => try self.emit(.{ .bit_and = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .bit_or => try self.emit(.{ .bit_or = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .bit_xor => try self.emit(.{ .bit_xor = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .bit_shl => try self.emit(.{ .bit_shl = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .bit_shr => try self.emit(.{ .bit_shr = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .bit_sar => try self.emit(.{ .bit_sar = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .bit_rol => try self.emit(.{ .bit_rol = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .bit_ror => try self.emit(.{ .bit_ror = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .compare_eq => try self.emit(.{ .compare_eq = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .compare_lt => try self.emit(.{ .compare_lt = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .compare_le => try self.emit(.{ .compare_le = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .compare_gt => try self.emit(.{ .compare_gt = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .compare_ge => try self.emit(.{ .compare_ge = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
        }
        self.finish_binary(prep);
        return prep.dst;
    }

    fn emit_float_binary(
        self: *function_ctx,
        left_id: ir_mod.ir_identifier,
        right_id: ir_mod.ir_identifier,
        op: enum { fadd, fsub, fmul, fdiv, frem, fmin, fmax, fcompare_eq, fcompare_lt, fcompare_gt },
    ) codegen_error!u8 {
        const prep = try self.prep_binary(left_id, right_id);
        switch (op) {
            .fadd => try self.emit(.{ .fadd = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .fsub => try self.emit(.{ .fsub = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .fmul => try self.emit(.{ .fmul = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .fdiv => try self.emit(.{ .fdiv = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .frem => try self.emit(.{ .frem = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .fmin => try self.emit(.{ .fmin = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .fmax => try self.emit(.{ .fmax = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .fcompare_eq => try self.emit(.{ .fcompare_eq = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .fcompare_lt => try self.emit(.{ .fcompare_lt = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
            .fcompare_gt => try self.emit(.{ .fcompare_gt = .{ .dst = prep.dst, .src_a = prep.left, .src_b = prep.right } }),
        }
        self.finish_binary(prep);
        return prep.dst;
    }

    fn emit_int_not_equal(self: *function_ctx, left_id: ir_mod.ir_identifier, right_id: ir_mod.ir_identifier) codegen_error!u8 {
        const prep = try self.prep_binary(left_id, right_id);
        const dst = prep.dst;
        try self.emit(.{ .compare_eq = .{ .dst = dst, .src_a = prep.left, .src_b = prep.right } });
        const zero_idx = try self.b.intern_const(0);
        const zero_reg = try self.alloc_temp();
        try self.emit(.{ .load_const = .{ .dst = zero_reg, .const_index = zero_idx } });
        try self.emit(.{ .compare_eq = .{ .dst = dst, .src_a = dst, .src_b = zero_reg } });
        self.free_temp(zero_reg);
        self.finish_binary(prep);
        return dst;
    }

    fn emit_float_not_equal(self: *function_ctx, left_id: ir_mod.ir_identifier, right_id: ir_mod.ir_identifier) codegen_error!u8 {
        const prep = try self.prep_binary(left_id, right_id);
        const dst = prep.dst;
        try self.emit(.{ .fcompare_eq = .{ .dst = dst, .src_a = prep.left, .src_b = prep.right } });
        const zero_idx = try self.b.intern_const(0);
        const zero_reg = try self.alloc_temp();
        try self.emit(.{ .load_const = .{ .dst = zero_reg, .const_index = zero_idx } });
        try self.emit(.{ .compare_eq = .{ .dst = dst, .src_a = dst, .src_b = zero_reg } });
        self.free_temp(zero_reg);
        self.finish_binary(prep);
        return dst;
    }

    fn emit_float_invert_compare(
        self: *function_ctx,
        left_id: ir_mod.ir_identifier,
        right_id: ir_mod.ir_identifier,
        op: enum { fcompare_lt, fcompare_gt },
    ) codegen_error!u8 {
        const prep = try self.prep_binary(left_id, right_id);
        const dst = prep.dst;
        switch (op) {
            .fcompare_lt => try self.emit(.{ .fcompare_lt = .{ .dst = dst, .src_a = prep.left, .src_b = prep.right } }),
            .fcompare_gt => try self.emit(.{ .fcompare_gt = .{ .dst = dst, .src_a = prep.left, .src_b = prep.right } }),
        }
        const zero_idx = try self.b.intern_const(0);
        const zero_reg = try self.alloc_temp();
        try self.emit(.{ .load_const = .{ .dst = zero_reg, .const_index = zero_idx } });
        try self.emit(.{ .compare_eq = .{ .dst = dst, .src_a = dst, .src_b = zero_reg } });
        self.free_temp(zero_reg);
        self.finish_binary(prep);
        return dst;
    }

    const print_foreign_name = struct {
        name: []const u8,
        owned: bool,
    };

    fn print_prefix(name: []const u8, suffix: []const u8) []const u8 {
        if (std.mem.endsWith(u8, name, suffix)) {
            return name[0 .. name.len - suffix.len];
        }
        return "";
    }

    fn print_foreign(self: *function_ctx, prefix: []const u8, suffix: []const u8) codegen_error!print_foreign_name {
        if (prefix.len == 0) return .{ .name = suffix, .owned = false };
        const sep = "::";
        const buf = self.b.allocator.alloc(u8, prefix.len + sep.len + suffix.len) catch return error.out_of_memory;
        std.mem.copyForwards(u8, buf[0..prefix.len], prefix);
        std.mem.copyForwards(u8, buf[prefix.len .. prefix.len + sep.len], sep);
        std.mem.copyForwards(u8, buf[prefix.len + sep.len ..], suffix);
        return .{ .name = buf, .owned = true };
    }

    fn emit_print_foreign_reg(
        self: *function_ctx,
        prefix: []const u8,
        suffix: []const u8,
        arg_reg: ?u8,
    ) codegen_error!void {
        const full = try self.print_foreign(prefix, suffix);
        defer if (full.owned) self.b.allocator.free(full.name);
        if (arg_reg) |reg| {
            try self.emit(.{ .argument_set = .{ .dst = 1, .src = reg } });
        }
        const foreign_idx = try self.foreign_index(full.name);
        try self.emit(.{ .call_foreign = .{ .index = foreign_idx } });
    }

    fn resolve_print_to(self: *function_ctx, arg_type: type_key) codegen_error!?function_info {
        const group = self.b.functions.get("print_to") orelse return null;
        var call_types = [_]type_key{ .{ .name = "int" }, arg_type };
        const selected_idx = self.resolve_function_overload_index(group.items, call_types[0..]) catch |err| switch (err) {
            error.unknown_function => return null,
            else => return err,
        };
        return group.items[selected_idx];
    }

    fn emit_print_to_call(self: *function_ctx, info: function_info, arg_reg: u8) codegen_error!void {
        const writer_idx = try self.b.intern_const(0);
        const writer_reg = try self.alloc_temp();
        try self.emit(.{ .load_const = .{ .dst = writer_reg, .const_index = writer_idx } });

        try self.emit(.{ .argument_set = .{ .dst = 1, .src = writer_reg } });
        try self.emit(.{ .argument_set = .{ .dst = 2, .src = arg_reg } });
        try self.emit(.{ .call = .{ .target = info.label } });

        if (self.is_temp(writer_reg)) self.free_temp(writer_reg);
    }

    const print_separator = enum {
        space,
        newline,
    };

    fn compile_print_call(
        self: *function_ctx,
        name: []const u8,
        args: []const ir_mod.ir_identifier,
        suffix: []const u8,
        separator: print_separator,
    ) codegen_error!u8 {
        const prefix = print_prefix(name, suffix);
        const separator_suffix = switch (separator) {
            .space => "print_sep",
            .newline => "print_line",
        };
        if (args.len == 0) {
            try self.emit_print_foreign_reg(prefix, "print_line", null);
            return 0;
        }

        var printed_any = false;

        for (args) |arg_id| {
            if (self.interpolation_args(arg_id)) |parts| {
                if (parts.len == 0) continue;
                if (printed_any) {
                    try self.emit_print_foreign_reg(prefix, separator_suffix, null);
                }
                for (parts) |part_id| {
                    try self.emit_print_value(prefix, part_id);
                }
                printed_any = true;
                continue;
            }

            if (printed_any) {
                try self.emit_print_foreign_reg(prefix, separator_suffix, null);
            }
            try self.emit_print_value(prefix, arg_id);
            printed_any = true;
        }

        try self.emit_print_foreign_reg(prefix, "print_line", null);
        return 0;
    }

    fn emit_print_value(self: *function_ctx, prefix: []const u8, arg_id: ir_mod.ir_identifier) codegen_error!void {
        const arg_type = self.infer_expr_type(arg_id);
        const info = try self.resolve_print_to(arg_type);
        const arg_reg = try self.compile_expr(arg_id);

        if (info) |print_info| {
            try self.emit_print_to_call(print_info, arg_reg);
        } else {
            const print_suffix = switch (arg_type) {
                .name => |type_name| blk: {
                    if (std.mem.eql(u8, type_name, "int")) break :blk "print_int";
                    if (std.mem.eql(u8, type_name, "float")) break :blk "print_float";
                    if (std.mem.eql(u8, type_name, "bool")) break :blk "print_bool";
                    if (std.mem.eql(u8, type_name, "string")) break :blk "print_string";
                    return error.unknown_function;
                },
                .dyn_trait, .applied, .unknown => return error.unknown_function,
            };
            try self.emit_print_foreign_reg(prefix, print_suffix, arg_reg);
        }

        if (self.is_temp(arg_reg)) self.free_temp(arg_reg);
    }

    fn interpolation_args(self: *function_ctx, arg_id: ir_mod.ir_identifier) ?[]const ir_mod.ir_identifier {
        const node = self.b.node(arg_id);
        return switch (node) {
            .intrinsic => |call| blk: {
                const name = self.b.string_value(call.name);
                if (!std.mem.eql(u8, name, "interpolate")) break :blk null;
                break :blk call.args;
            },
            else => null,
        };
    }

    fn trait_method_index(self: *function_ctx, trait_name: []const u8, method_name: []const u8) ?usize {
        const info = self.b.traits.get(trait_name) orelse return null;
        for (info.methods, 0..) |method, idx| {
            if (std.mem.eql(u8, method.name, method_name)) return idx;
        }
        return null;
    }

    fn trait_method_return_type(self: *function_ctx, trait_name: []const u8, method_name: []const u8) type_key {
        const info = self.b.traits.get(trait_name) orelse return .unknown;
        for (info.methods) |method| {
            if (!std.mem.eql(u8, method.name, method_name)) continue;
            const ret = method.return_type orelse return .unknown;
            return type_key_from_type_node_with_self(self.b, ret, trait_name);
        }
        return .unknown;
    }

    fn infer_method_return_type(
        self: *function_ctx,
        name: []const u8,
        recv_type: type_key,
        arg_types: []const type_key,
    ) type_key {
        if (self.trait_name_from_type(recv_type)) |trait_name| {
            return self.trait_method_return_type(trait_name, name);
        }

        const group = self.b.functions.get(name) orelse return .unknown;
        if (arg_types.len + 1 > 7) return .unknown;

        var call_types_buf: [8]type_key = undefined;
        call_types_buf[0] = recv_type;
        std.mem.copyForwards(type_key, call_types_buf[1 .. arg_types.len + 1], arg_types);
        const call_types = call_types_buf[0 .. arg_types.len + 1];

        const resolved = self.resolve_function_overload(group.items, call_types) catch return .unknown;
        defer if (resolved.bindings.len > 0) self.b.allocator.free(resolved.bindings);

        const return_type = resolved.info.decl.return_type orelse return .unknown;
        const base_return = type_key_from_type_node_with_self(self.b, return_type, resolved.info.impl_for);
        if (resolved.bindings.len == 0) return base_return;
        return apply_bindings_to_type_key(self.b, base_return, resolved.bindings);
    }

    fn compile_trait_method_call(
        self: *function_ctx,
        trait_name: []const u8,
        method_name: []const u8,
        receiver_id: ir_mod.ir_identifier,
        args: []const ir_mod.ir_identifier,
    ) codegen_error!u8 {
        const method_idx = self.trait_method_index(trait_name, method_name) orelse return error.unknown_function;
        if (args.len + 1 > 7) return error.register_overflow;

        var recv_reg = try self.compile_expr(receiver_id);
        var recv_temp: ?u8 = null;
        if (recv_reg == 0) {
            const tmp = try self.alloc_temp();
            try self.emit(.{ .move = .{ .dst = tmp, .src = recv_reg } });
            recv_reg = tmp;
            recv_temp = tmp;
        }

        const vtable_reg = try self.deref_to_temp(recv_reg);
        const data_reg = blk: {
            const offset_idx = try self.b.intern_const(1);
            const offset_reg = try self.alloc_temp();
            try self.emit(.{ .load_const = .{ .dst = offset_reg, .const_index = offset_idx } });
            try self.emit(.{ .add = .{ .dst = offset_reg, .src_a = recv_reg, .src_b = offset_reg } });
            const data_reg = try self.deref_to_temp(offset_reg);
            self.free_temp(offset_reg);
            break :blk data_reg;
        };

        const fn_reg = try self.alloc_temp();
        if (method_idx == 0) {
            try self.deref_into(vtable_reg, fn_reg);
        } else {
            const method_idx_const = try self.b.intern_const(@intCast(method_idx));
            const method_reg = try self.alloc_temp();
            try self.emit(.{ .load_const = .{ .dst = method_reg, .const_index = method_idx_const } });
            try self.emit(.{ .add = .{ .dst = method_reg, .src_a = vtable_reg, .src_b = method_reg } });
            try self.deref_into(method_reg, fn_reg);
            self.free_temp(method_reg);
        }
        if (self.is_temp(vtable_reg)) self.free_temp(vtable_reg);

        if (self.is_temp(data_reg)) self.pin_temp(data_reg);
        if (self.is_temp(fn_reg)) self.pin_temp(fn_reg);

        try self.emit(.{ .argument_set = .{ .dst = 1, .src = data_reg } });
        for (args, 0..) |arg_id, idx| {
            const arg_reg = try self.compile_expr(arg_id);
            const dst: u8 = @intCast(idx + 2);
            try self.emit(.{ .argument_set = .{ .dst = dst, .src = arg_reg } });
            if (self.is_temp(arg_reg)) self.free_temp(arg_reg);
        }

        if (self.is_temp(data_reg)) self.unpin_temp(data_reg);
        if (self.is_temp(fn_reg)) self.unpin_temp(fn_reg);

        try self.emit(.{ .call_register = .{ .src = fn_reg, .dst = 0 } });

        if (self.is_temp(fn_reg)) self.free_temp(fn_reg);
        if (self.is_temp(data_reg)) self.free_temp(data_reg);
        if (recv_temp) |tmp| self.free_temp(tmp);

        return self.save_result_reg(0);
    }

    fn compile_call(self: *function_ctx, id: ir_mod.ir_identifier) codegen_error!u8 {
        var base_id = id;
        var args = std.array_list.Managed(ir_mod.ir_identifier).init(self.b.allocator);
        defer args.deinit();

        while (true) {
            const node = self.b.node(base_id);
            switch (node) {
                .binary => |bin| {
                    if (bin.op == .call) {
                        args.append(bin.right) catch return error.out_of_memory;
                        base_id = bin.left;
                        continue;
                    }
                },
                else => {},
            }
            break;
        }

        std.mem.reverse(ir_mod.ir_identifier, args.items);

        if (args.items.len == 1 and self.is_unit(args.items[0])) {
            args.clearRetainingCapacity();
        }

        const base = try self.resolve_call_base(base_id);
        var arg_ids = args.items;
        var arg_ids_buf: [8]ir_mod.ir_identifier = undefined;
        if (base.receiver) |recv| {
            if (arg_ids.len + 1 > arg_ids_buf.len) return error.register_overflow;
            arg_ids_buf[0] = recv;
            std.mem.copyForwards(ir_mod.ir_identifier, arg_ids_buf[1 .. arg_ids.len + 1], arg_ids);
            arg_ids = arg_ids_buf[0 .. arg_ids.len + 1];
        }

        const name = base.name;

        if (base.receiver) |recv| {
            const recv_type = self.infer_expr_type(recv);
            if (self.trait_name_from_type(recv_type)) |type_name| {
                const other_args = arg_ids[1..];
                return self.compile_trait_method_call(type_name, name, recv, other_args);
            }
        }

        if (std.mem.endsWith(u8, name, "::borrow") or std.mem.endsWith(u8, name, "::borrow_mut")) {
            return self.compile_borrow_call(arg_ids);
        }

        if (is_print_name(name)) {
            return self.compile_print_call(name, arg_ids, "::print", .space);
        }

        if (is_println_name(name)) {
            return self.compile_print_call(name, arg_ids, "::println", .newline);
        }

        if (is_cancel_name(name)) {
            return self.compile_cancel_call(arg_ids);
        }

        if (arg_ids.len > 7) return error.register_overflow;

        var arg_types: [7]type_key = undefined;
        for (arg_ids, 0..) |arg_id, idx| {
            arg_types[idx] = self.infer_expr_type(arg_id);
        }
        const arg_type_slice = arg_types[0..arg_ids.len];

        if (self.b.foreigns.contains(name)) {
            if (self.b.foreign_overloads.get(name)) |overloads| {
                if (overloads.items.len > 1) {
                    const selected_idx = try self.resolve_foreign_overload_index(overloads.items, arg_type_slice);
                    const foreign_idx = try self.foreign_index(overloads.items[selected_idx].name);
                    for (arg_ids, 0..) |arg_id, idx| {
                        const arg_reg = try self.compile_expr(arg_id);
                        const dst: u8 = @intCast(idx + 1);
                        try self.emit(.{ .argument_set = .{ .dst = dst, .src = arg_reg } });
                        if (self.is_temp(arg_reg)) self.free_temp(arg_reg);
                    }
                    try self.emit(.{ .call_foreign = .{ .index = foreign_idx } });
                    return self.save_result_reg(0);
                }
            }

            const foreign_idx = try self.foreign_index(name);
            for (arg_ids, 0..) |arg_id, idx| {
                const arg_reg = try self.compile_expr(arg_id);
                const dst: u8 = @intCast(idx + 1);
                try self.emit(.{ .argument_set = .{ .dst = dst, .src = arg_reg } });
                if (self.is_temp(arg_reg)) self.free_temp(arg_reg);
            }
            try self.emit(.{ .call_foreign = .{ .index = foreign_idx } });
            return self.save_result_reg(0);
        }

        const group = self.b.functions.get(name) orelse return error.unknown_function;
        const resolved = try self.resolve_function_overload(group.items, arg_type_slice);
        defer if (resolved.bindings.len > 0) self.b.allocator.free(resolved.bindings);
        const info = resolved.info;

        for (arg_ids, 0..) |arg_id, idx| {
            const arg_reg = try self.compile_expr(arg_id);
            const dst: u8 = @intCast(idx + 1);
            try self.emit(.{ .argument_set = .{ .dst = dst, .src = arg_reg } });
            if (self.is_temp(arg_reg)) self.free_temp(arg_reg);
        }

        const target = try self.b.ensure_instance(info, resolved.bindings);
        try self.emit(.{ .call = .{ .target = target } });
        return self.save_result_reg(0);
    }

    fn struct_field_index(info: struct_info, name: ir_mod.string_identifier) ?usize {
        for (info.fields, 0..) |field_id, idx| {
            if (field_id.idx == name.idx) return idx;
        }
        return null;
    }

    fn compile_record_literal(self: *function_ctx, rec: ir_mod.record_literal) codegen_error!u8 {
        const type_name = self.b.string_value(rec.type_name);
        const info = self.b.structs.get(type_name) orelse return error.unsupported_node;
        if (!info.is_record) return error.unsupported_node;
        const field_count = info.fields.len;
        const alloc_word_count: u64 = if (field_count == 0) 1 else @intCast(field_count);
        const base_reg = try self.alloc_words(alloc_word_count);

        var seen = self.b.allocator.alloc(bool, field_count) catch return error.out_of_memory;
        defer self.b.allocator.free(seen);
        @memset(seen, false);

        for (rec.fields) |field| {
            const field_idx = struct_field_index(info, field.name) orelse return error.unsupported_node;
            if (seen[field_idx]) return error.unsupported_node;
            seen[field_idx] = true;

            const value_reg = try self.compile_expr(field.value);
            var ptr_reg = base_reg;
            var ptr_temp = false;
            var offset_reg: ?u8 = null;
            if (field_idx != 0) {
                const offset_idx = try self.b.intern_const(@intCast(field_idx));
                const off_reg = try self.alloc_temp();
                offset_reg = off_reg;
                ptr_reg = try self.alloc_temp();
                ptr_temp = true;
                try self.emit(.{ .load_const = .{ .dst = off_reg, .const_index = offset_idx } });
                try self.emit(.{ .add = .{ .dst = ptr_reg, .src_a = base_reg, .src_b = off_reg } });
            }
            try self.store_value(ptr_reg, value_reg);

            if (self.is_temp(value_reg)) self.free_temp(value_reg);
            if (ptr_temp) self.free_temp(ptr_reg);
            if (offset_reg) |off_reg| self.free_temp(off_reg);
        }

        for (seen) |hit| {
            if (!hit and field_count > 0) return error.unsupported_node;
        }

        return base_reg;
    }

    fn compile_access_ptr(
        self: *function_ctx,
        left: ir_mod.ir_identifier,
        right: ir_mod.ir_identifier,
    ) codegen_error!u8 {
        const field_node = self.b.node(right);
        if (field_node != .identifier) return error.unsupported_node;
        const field_id = field_node.identifier;
        const base_type = self.infer_expr_type(left);
        const struct_name = switch (base_type) {
            .name => |name| name,
            else => return error.unsupported_node,
        };
        const info = self.b.structs.get(struct_name) orelse return error.unsupported_node;
        const field_idx = struct_field_index(info, field_id) orelse return error.unsupported_node;

        const base_reg = try self.compile_expr(left);
        var ptr_reg = base_reg;
        if (field_idx != 0) {
            const offset_idx = try self.b.intern_const(@intCast(field_idx));
            const off_reg = try self.alloc_temp();
            try self.emit(.{ .load_const = .{ .dst = off_reg, .const_index = offset_idx } });
            try self.emit(.{ .add = .{ .dst = off_reg, .src_a = base_reg, .src_b = off_reg } });
            ptr_reg = off_reg;
            if (self.is_temp(base_reg)) self.free_temp(base_reg);
        }
        return ptr_reg;
    }

    fn compile_access(self: *function_ctx, left: ir_mod.ir_identifier, right: ir_mod.ir_identifier) codegen_error!u8 {
        const field_node = self.b.node(right);
        if (field_node != .identifier) return error.unsupported_node;
        const field_id = field_node.identifier;
        const base_type = self.infer_expr_type(left);
        const struct_name = switch (base_type) {
            .name => |name| name,
            else => return error.unsupported_node,
        };
        const info = self.b.structs.get(struct_name) orelse return error.unsupported_node;
        const field_idx = struct_field_index(info, field_id) orelse return error.unsupported_node;

        const base_reg = try self.compile_expr(left);
        var ptr_reg = base_reg;
        var offset_reg: ?u8 = null;
        if (field_idx != 0) {
            const offset_idx = try self.b.intern_const(@intCast(field_idx));
            const off_reg = try self.alloc_temp();
            offset_reg = off_reg;
            try self.emit(.{ .load_const = .{ .dst = off_reg, .const_index = offset_idx } });
            try self.emit(.{ .add = .{ .dst = off_reg, .src_a = base_reg, .src_b = off_reg } });
            ptr_reg = off_reg;
        }

        const deref_idx = try self.foreign_index("std::deref");
        var args = [_]u8{ptr_reg};
        try self.emit_foreign_call(deref_idx, args[0..]);

        if (offset_reg) |off_reg| self.free_temp(off_reg);
        if (self.is_temp(base_reg)) self.free_temp(base_reg);
        return self.save_result_reg(0);
    }

    fn compile_index_expr(self: *function_ctx, left: ir_mod.ir_identifier, right: ir_mod.ir_identifier) codegen_error!u8 {
        var base_reg = try self.compile_expr(left);
        if (base_reg == 0) base_reg = try self.save_result_reg(base_reg);

        const base_type = self.infer_expr_type(left);
        if (type_key_base_name(base_type)) |name| {
            if (std.mem.eql(u8, name, "slice")) {
                _ = try self.emit_foreign_call_reg("std::slice_ptr", base_reg);
                if (self.is_temp(base_reg)) self.free_temp(base_reg);
                base_reg = try self.save_result_reg(0);
            } else if (self.trait_name_from_type(base_type) != null or self.is_known_non_builtin(base_type)) {
                if (try self.try_compile_method_call("index", left, &.{right})) |reg| {
                    return reg;
                }
                if (self.is_known_non_builtin(base_type)) return error.unknown_function;
                return error.unsupported_node;
            } else {
                return error.unsupported_node;
            }
        } else if (self.trait_name_from_type(base_type) != null or self.is_known_non_builtin(base_type)) {
            if (try self.try_compile_method_call("index", left, &.{right})) |reg| {
                return reg;
            }
            if (self.is_known_non_builtin(base_type)) return error.unknown_function;
            return error.unsupported_node;
        } else {
            return error.unsupported_node;
        }

        var index_reg = try self.compile_expr(right);
        if (index_reg == 0) index_reg = try self.save_result_reg(index_reg);

        const ptr_reg = if (self.is_temp(base_reg))
            base_reg
        else if (self.is_temp(index_reg))
            index_reg
        else
            try self.alloc_temp();
        try self.emit(.{ .add = .{ .dst = ptr_reg, .src_a = base_reg, .src_b = index_reg } });

        if (self.is_temp(base_reg) and ptr_reg != base_reg) self.free_temp(base_reg);
        if (self.is_temp(index_reg) and ptr_reg != index_reg) self.free_temp(index_reg);

        _ = try self.emit_foreign_call_reg("std::deref", ptr_reg);
        if (self.is_temp(ptr_reg)) self.free_temp(ptr_reg);
        return self.save_result_reg(0);
    }

    fn compile_index_ptr(
        self: *function_ctx,
        left: ir_mod.ir_identifier,
        right: ir_mod.ir_identifier,
    ) codegen_error!u8 {
        var base_reg = try self.compile_expr(left);
        if (base_reg == 0) base_reg = try self.save_result_reg(base_reg);

        const base_type = self.infer_expr_type(left);
        if (type_key_base_name(base_type)) |name| {
            if (std.mem.eql(u8, name, "slice")) {
                _ = try self.emit_foreign_call_reg("std::slice_ptr", base_reg);
                if (self.is_temp(base_reg)) self.free_temp(base_reg);
                base_reg = try self.save_result_reg(0);
            } else {
                return error.unsupported_node;
            }
        } else {
            return error.unsupported_node;
        }

        var index_reg = try self.compile_expr(right);
        if (index_reg == 0) index_reg = try self.save_result_reg(index_reg);

        const ptr_reg = if (self.is_temp(base_reg))
            base_reg
        else if (self.is_temp(index_reg))
            index_reg
        else
            try self.alloc_temp();
        try self.emit(.{ .add = .{ .dst = ptr_reg, .src_a = base_reg, .src_b = index_reg } });

        if (self.is_temp(base_reg) and ptr_reg != base_reg) self.free_temp(base_reg);
        if (self.is_temp(index_reg) and ptr_reg != index_reg) self.free_temp(index_reg);

        return ptr_reg;
    }

    fn compile_cancel_call(self: *function_ctx, args: []const ir_mod.ir_identifier) codegen_error!u8 {
        if (args.len != 1) return error.unsupported_node;
        const arg_reg = try self.compile_expr(args[0]);
        try self.emit(.{ .task_cancel = .{ .src = arg_reg } });
        if (self.is_temp(arg_reg)) self.free_temp(arg_reg);
        return 0;
    }
};

fn intrinsic_return_type(def: intrinsic.intrinsic_def) type_key {
    return switch (def.id) {
        .iadd, .isub, .imul, .idiv, .irem, .imin, .imax, .ineg, .iabs,
        .bnot, .band, .bor, .bxor, .shl, .shr, .sar, .rol, .ror,
        .alloc, .deref, .borrow, .borrow_mut => .{ .name = "int" },
        .ieq, .ine, .ilt, .ile, .igt, .ige => .{ .name = "bool" },
        .fadd, .fsub, .fmul, .fdiv, .frem, .fmin, .fmax, .fneg, .fabs,
        .sqrt, .sin, .cos, .tan, .asin, .acos, .atan, .floor, .ceil, .round, .trunc => .{ .name = "float" },
        .feq, .fne, .flt, .fle, .fgt, .fge => .{ .name = "bool" },
        .free => .{ .name = "unit" },
        .result_ok, .result_err => .{ .name = "result" },
        .result_is_ok => .{ .name = "bool" },
        .result_unwrap, .result_unwrap_err => .unknown,
        .interpolate => .{ .name = "string" },
    };
}

pub fn generate(
    allocator: std.mem.Allocator,
    nodes: []const ir_mod.ir,
    strings: []const []const u8,
    roots: []const ir_mod.ir_identifier,
    foreigns: []const []const u8,
) codegen_error!program {
    var b = builder.init(allocator, nodes, strings, roots);
    defer b.deinit();

    var functions = std.array_list.Managed(function_info).init(allocator);
    defer functions.deinit();

    const impl_record = struct {
        trait_name: []const u8,
        type_name: []const u8,
        methods: std.StringHashMap(ink.exe.label_id),
    };
    var impl_records = std.array_list.Managed(impl_record).init(allocator);
    defer {
        for (impl_records.items) |*record| {
            record.methods.deinit();
        }
        impl_records.deinit();
    }

    for (foreigns, 0..) |name, idx| {
        if (!b.foreigns.contains(name)) {
            b.foreigns.put(name, @intCast(idx)) catch return error.out_of_memory;
        }
    }

    for (foreigns) |name| {
        const sep = std.mem.indexOfScalar(u8, name, '$') orelse continue;
        const base = name[0..sep];
        const signature = if (sep + 1 < name.len) name[sep + 1 ..] else "";
        if (b.foreign_overloads.getPtr(base)) |group| {
            group.append(.{ .name = name, .signature = signature }) catch return error.out_of_memory;
        } else {
            var group = std.array_list.Managed(foreign_signature).init(allocator);
            group.append(.{ .name = name, .signature = signature }) catch return error.out_of_memory;
            b.foreign_overloads.put(base, group) catch return error.out_of_memory;
        }
    }

    for (roots) |root| {
        const node = nodes[@intCast(root.idx)];
        switch (node) {
                .decl => |decl| switch (decl) {
                .@"struct" => |st| {
                    const name = b.string_value(st.name);
                    if (!b.structs.contains(name)) {
                        const field_ids = b.allocator.alloc(ir_mod.string_identifier, st.fields.len) catch return error.out_of_memory;
                        for (st.fields, 0..) |field, idx| {
                            field_ids[idx] = field.name;
                        }
                        b.structs.put(name, .{
                            .fields = field_ids,
                            .is_record = st.is_record,
                        }) catch return error.out_of_memory;
                    }
                },
                .trait => |tr| {
                    var methods = std.array_list.Managed(trait_method).init(allocator);
                    for (tr.items) |item| {
                        switch (item) {
                            .function => |func| methods.append(.{
                                .name = b.string_value(func.name),
                                .return_type = func.return_type,
                            }) catch return error.out_of_memory,
                            .assoc_type => {},
                        }
                    }
                    const method_slice = methods.toOwnedSlice() catch return error.out_of_memory;
                    var reqs = std.array_list.Managed([]const u8).init(allocator);
                    for (tr.requires) |req_id| {
                        const name = type_name_from_type_node(&b, req_id) orelse continue;
                        reqs.append(name) catch return error.out_of_memory;
                    }
                    const req_slice = reqs.toOwnedSlice() catch return error.out_of_memory;
                    b.traits.put(b.string_value(tr.name), .{ .methods = method_slice, .requires = req_slice }) catch return error.out_of_memory;
                },
                else => {},
            },
            else => {},
        }
    }

    for (roots) |root| {
        const node = nodes[@intCast(root.idx)];
        switch (node) {
            .decl => |decl| switch (decl) {
                .function => |func| {
                    const name = b.string_value(func.name);
                    if (b.foreigns.contains(name)) continue;

                    const label = b.new_label();
                    const info = function_info{
                        .label = label,
                        .decl = func,
                        .impl_for = null,
                    };

                    if (b.functions.getPtr(name)) |group| {
                        for (group.items) |existing| {
                            if (signature_matches(&b, existing, info)) {
                                return error.unsupported_node;
                            }
                        }
                        group.append(info) catch return error.out_of_memory;
                    } else {
                        var group = std.array_list.Managed(function_info).init(allocator);
                        group.append(info) catch return error.out_of_memory;
                        b.functions.put(name, group) catch return error.out_of_memory;
                    }

                    functions.append(info) catch return error.out_of_memory;
                },
                .@"impl" => |impl| {
                    const trait_name = b.string_value(impl.by_trait);
                    const type_name = b.string_value(impl.for_struct);
                    const is_runtime_trait = b.traits.contains(trait_name);
                    var method_map = std.StringHashMap(ink.exe.label_id).init(allocator);

                    for (impl.functions) |func| {
                        const name = b.string_value(func.name);
                        if (b.foreigns.contains(name)) continue;

                        const label = b.new_label();
                        const info = function_info{
                            .label = label,
                            .decl = func,
                            .impl_for = b.string_value(impl.for_struct),
                        };

                        if (b.functions.getPtr(name)) |group| {
                            for (group.items) |existing| {
                                if (signature_matches(&b, existing, info)) {
                                    return error.unsupported_node;
                                }
                            }
                            group.append(info) catch return error.out_of_memory;
                        } else {
                            var group = std.array_list.Managed(function_info).init(allocator);
                            group.append(info) catch return error.out_of_memory;
                            b.functions.put(name, group) catch return error.out_of_memory;
                        }

                        functions.append(info) catch return error.out_of_memory;
                        if (is_runtime_trait) {
                            method_map.put(name, label) catch return error.out_of_memory;
                        }
                    }

                    var impl_list = b.trait_impls.getPtr(trait_name);
                    if (impl_list == null) {
                        var list = std.array_list.Managed([]const u8).init(allocator);
                        list.append(type_name) catch return error.out_of_memory;
                        b.trait_impls.put(trait_name, list) catch return error.out_of_memory;
                    } else if (!string_list_contains(impl_list.?.items, type_name)) {
                        impl_list.?.append(type_name) catch return error.out_of_memory;
                    }

                    if (is_runtime_trait) {
                        impl_records.append(.{
                            .trait_name = trait_name,
                            .type_name = type_name,
                            .methods = method_map,
                        }) catch return error.out_of_memory;
                    } else {
                        method_map.deinit();
                    }
                },
                else => {},
            },
            else => {},
        }
    }

    for (impl_records.items) |*record| {
        const trait_meta = b.traits.get(record.trait_name) orelse continue;
        const method_count = trait_meta.methods.len;
        const labels = b.allocator.alloc(ink.exe.label_id, method_count) catch return error.out_of_memory;
        for (trait_meta.methods, 0..) |method, idx| {
            const label = record.methods.get(method.name) orelse return error.unsupported_node;
            labels[idx] = label;
        }
        var inner = b.trait_vtables.getPtr(record.trait_name);
        if (inner == null) {
            var map = std.StringHashMap([]const ink.exe.label_id).init(allocator);
            map.put(record.type_name, labels) catch return error.out_of_memory;
            b.trait_vtables.put(record.trait_name, map) catch return error.out_of_memory;
        } else {
            inner.?.put(record.type_name, labels) catch return error.out_of_memory;
        }
    }

    const main_group = b.functions.get("main") orelse return error.missing_main;
    var main_info: ?function_info = null;
    for (main_group.items) |info| {
        if (info.decl.params.len == 0) {
            if (main_info != null) return error.ambiguous_overload;
            main_info = info;
        }
    }
    if (main_info == null) return error.missing_main;

    try b.emit(.{ .call = .{ .target = main_info.?.label } });
    try b.emit(.{ .halt = {} });

    for (functions.items) |info| {
        if (info.decl.generics.len != 0) continue;
        const idx = b.instances.items.len;
        b.instances.append(.{ .info = info, .bindings = &.{} }) catch return error.out_of_memory;
        b.pending_instances.append(idx) catch return error.out_of_memory;
    }

    var queue_index: usize = 0;
    while (queue_index < b.pending_instances.items.len) : (queue_index += 1) {
        const idx = b.pending_instances.items[queue_index];
        const instance = b.instances.items[idx];
        const info = instance.info;
        try b.emit(.{ .label = .{ .id = info.label } });
        var ctx = try function_ctx.init(&b, info.decl.params, info.impl_for, instance.bindings);
        defer ctx.deinit();

        if (info.decl.body) |body_ref| {
            const result_reg = try ctx.compile_expr(body_ref);
            if (!ctx.returned) {
                try ctx.emit(.{ .ret_value = .{ .src = result_reg } });
            }
        } else {
            try ctx.emit(.{ .ret = {} });
        }
    }

    if (b.label_constants.count() > 0) {
        var label_offsets = encode.compute_label_offsets(allocator, b.instructions.items) catch |err| switch (err) {
            error.OutOfMemory => return error.out_of_memory,
            else => return error.unsupported_node,
        };
        defer label_offsets.deinit();

        var it = b.label_constants.iterator();
        while (it.next()) |entry| {
            const offset = label_offsets.get(entry.key_ptr.*) orelse return error.unsupported_node;
            if (entry.value_ptr.* >= b.constants.items.len) return error.unsupported_node;
            b.constants.items[entry.value_ptr.*] = @intCast(offset);
        }
    }

    return .{
        .instructions = b.instructions.toOwnedSlice() catch return error.out_of_memory,
        .constants = b.constants.toOwnedSlice() catch return error.out_of_memory,
    };
}
