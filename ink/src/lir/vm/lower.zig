const std = @import("std");
const ink = @import("ink");
const mir_mod = ink.mir;
const core = @import("core.zig");
const intrinsic = @import("../../intrinsic.zig");
const encode = @import("../../vm/encode.zig");
const type_key_mod = @import("../../type_key.zig");

const max_register: u8 = 63;

const macro_token_kind = enum(u8) {
    new_line,
    identifier,
    label,
    string,
    number,
    comma,
    colon,
    dot,
    ellipsis,
    hash,
    at_sign,
    function,
    constant,
    variable,
    expr_if,
    expr_else,
    expr_match,
    expr_select,
    case,
    detached,
    stmt_return,
    stmt_break,
    stmt_continue,
    yield,
    loop,
    @"while",
    until,
    repeat,
    @"for",
    each,
    sleep,
    timeout,
    deadline,
    spawn,
    await,
    @"try",
    atomic,
    auto,
    box,
    logical_or,
    logical_and,
    logical_xor,
    logical_not,
    logical_false,
    logical_true,
    assign,
    plus_assign,
    minus_assign,
    asterisk_assign,
    slash_assign,
    percent_assign,
    ampersand_assign,
    bar_assign,
    caret_assign,
    shift_left_assign,
    shift_right_assign,
    plus,
    minus,
    asterisk,
    slash,
    percent,
    ampersand,
    bar,
    caret,
    shift_left,
    shift_right,
    bang,
    tilde,
    less_than,
    greater_than,
    less_or_equal,
    greater_or_equal,
    equal,
    not_equal,
    @"enum",
    type,
    arrow,
    question,
    question_dot,
    coalesce,
    range,
    range_inclusive,
    double_colon,
    pipe,
    in,
    trait,
    impl,
    as,
    import,
    from,
    with,
    dynamic,
    @"comptime",
    @"struct",
    where,
    self,
    this,
    mut,
    ref,
    requires,
    dyn,
};

const macro_token_tree_kind = enum(u8) { token, group };
const macro_delimiter = enum(u8) { paren, bracket, block };

pub const lower_error = error{
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

pub const error_info = struct {
    message: ?[]const u8 = null,
    node: ?mir_mod.mir_identifier = null,
    owns_message: bool = false,

    pub fn deinit(self: *error_info, allocator: std.mem.Allocator) void {
        if (self.owns_message) {
            if (self.message) |msg| allocator.free(msg);
        }
        self.* = .{};
    }
};

const type_key = type_key_mod.type_key;
const type_key_eq = type_key_mod.type_key_eq;
const type_key_base_name = type_key_mod.type_key_base_name;

fn macro_enum_value(full_name: []const u8) ?u64 {
    const split = std.mem.lastIndexOf(u8, full_name, "::") orelse return null;
    const base = full_name[0..split];
    const variant = full_name[split + 2 ..];
    if (std.mem.endsWith(u8, base, "token_kind")) {
        const value = std.meta.stringToEnum(macro_token_kind, variant) orelse return null;
        return @intFromEnum(value);
    }
    if (std.mem.endsWith(u8, base, "token_tree_kind")) {
        const value = std.meta.stringToEnum(macro_token_tree_kind, variant) orelse return null;
        return @intFromEnum(value);
    }
    if (std.mem.endsWith(u8, base, "delimiter")) {
        const value = std.meta.stringToEnum(macro_delimiter, variant) orelse return null;
        return @intFromEnum(value);
    }
    return null;
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

fn append_call_signature(
    buf: *std.array_list.Managed(u8),
    name: []const u8,
    arg_types: []const type_key,
    receiver: bool,
) !void {
    if (receiver and arg_types.len > 0) {
        try type_key_append(buf, arg_types[0]);
        try buf.append('.');
    }
    try buf.appendSlice(name);
    try buf.append('(');
    const skip: usize = if (receiver and arg_types.len > 0) 1 else 0;
    if (arg_types.len > skip) {
        for (arg_types[skip..], 0..) |arg_type, idx| {
            if (idx > 0) try buf.appendSlice(", ");
            try type_key_append(buf, arg_type);
        }
    }
    try buf.append(')');
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
        std.mem.eql(u8, name, "uint") or
        std.mem.eql(u8, name, "float") or
        std.mem.eql(u8, name, "bool") or
        std.mem.eql(u8, name, "string");
}

fn is_int_type_name(name: []const u8) bool {
    return std.mem.eql(u8, name, "int") or std.mem.eql(u8, name, "uint");
}

fn is_float_type_name(name: []const u8) bool {
    return std.mem.eql(u8, name, "float");
}

fn is_bool_type_name(name: []const u8) bool {
    return std.mem.eql(u8, name, "bool");
}

fn atomic_inner_type_key(ty: type_key) ?type_key {
    return switch (ty) {
        .applied => |ap| blk: {
            if (!std.mem.eql(u8, ap.base, "atomic")) break :blk null;
            if (ap.args.len != 1) break :blk null;
            break :blk ap.args[0];
        },
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

fn unary_operator_symbol(op: ink.unary) []const u8 {
    return switch (op) {
        .neg => "-",
        .not => "not",
        .bit_not => "~",
        else => "?",
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

fn binary_operator_symbol(op: ink.binary) []const u8 {
    return switch (op) {
        .add => "+",
        .sub => "-",
        .mul => "*",
        .div => "/",
        .mod => "%",
        .bit_and => "&",
        .bit_or => "|",
        .bit_xor, .logical_xor => "^",
        .shl => "<<",
        .shr => ">>",
        .equal => "==",
        .not_equal => "!=",
        .less_than => "<",
        .less_or_equal => "<=",
        .greater_than => ">",
        .greater_or_equal => ">=",
        .logical_and => "and",
        .logical_or => "or",
        else => "?",
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
    decl: mir_mod.mir.function_decl,
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

pub const function_signature = struct {
    name: []const u8,
    label: ink.exe.label_id,
    param_count: usize,
    is_method: bool,
};

pub const lower_options = struct {
    require_main: bool = true,
    signatures: ?*std.array_list.Managed(function_signature) = null,
};

const struct_info = struct {
    fields: []const mir_mod.mir.struct_decl.field,
    is_record: bool,
    generics: []const mir_mod.mir.generic_param,
};

const trait_method = struct {
    name: []const u8,
    return_type: ?mir_mod.mir_identifier,
};

const trait_constraint = struct {
    name: []const u8,
    negative: bool,
};

const trait_info = struct {
    methods: []const trait_method,
    requires: []const trait_constraint,
    is_auto: bool,
};

const generic_binding = struct {
    name: []const u8,
    ty: type_key,
};

const overload_result = struct {
    info: function_info,
    bindings: []const generic_binding,
};

const global_const = struct {
    value: mir_mod.mir_identifier,
    ty: ?mir_mod.mir_identifier,
};

const builder = struct {
    allocator: std.mem.Allocator,
    nodes: []const mir_mod.mir,
    strings: []const []const u8,
    roots: []const mir_mod.mir_identifier,
    node_types: ?[]const type_key,
    error_info: ?*error_info,
    instructions: std.array_list.Managed(ink.exe.instruction),
    constants: std.array_list.Managed(u64),
    const_map: std.AutoHashMap(u64, u32),
    functions: std.StringHashMap(std.array_list.Managed(function_info)),
    instances: std.array_list.Managed(function_instance),
    pending_instances: std.array_list.Managed(usize),
    instance_map: std.StringHashMap(ink.exe.label_id),
    foreign_overloads: std.StringHashMap(std.array_list.Managed(foreign_signature)),
    foreigns: std.StringHashMap(u32),
    global_consts: std.StringHashMap(global_const),
    structs: std.StringHashMap(struct_info),
    traits: std.StringHashMap(trait_info),
    trait_impls: std.StringHashMap(std.array_list.Managed([]const u8)),
    trait_neg_impls: std.StringHashMap(std.array_list.Managed([]const u8)),
    trait_vtables: std.StringHashMap(std.StringHashMap([]const ink.exe.label_id)),
    label_constants: std.AutoHashMap(ink.exe.label_id, u32),
    owned_slices: std.ArrayListUnmanaged([]const u8),
    owned_type_slices: std.ArrayListUnmanaged([]const type_key),
    next_label: u32,

    pub fn init(
        allocator: std.mem.Allocator,
        nodes: []const mir_mod.mir,
        strings: []const []const u8,
        roots: []const mir_mod.mir_identifier,
        node_types: ?[]const type_key,
        error_state: ?*error_info,
    ) builder {
        return .{
            .allocator = allocator,
            .nodes = nodes,
            .strings = strings,
            .roots = roots,
            .node_types = node_types,
            .error_info = error_state,
            .instructions = std.array_list.Managed(ink.exe.instruction).init(allocator),
            .constants = std.array_list.Managed(u64).init(allocator),
            .const_map = std.AutoHashMap(u64, u32).init(allocator),
            .functions = std.StringHashMap(std.array_list.Managed(function_info)).init(allocator),
            .instances = std.array_list.Managed(function_instance).init(allocator),
            .pending_instances = std.array_list.Managed(usize).init(allocator),
            .instance_map = std.StringHashMap(ink.exe.label_id).init(allocator),
            .foreign_overloads = std.StringHashMap(std.array_list.Managed(foreign_signature)).init(allocator),
            .foreigns = std.StringHashMap(u32).init(allocator),
            .global_consts = std.StringHashMap(global_const).init(allocator),
            .structs = std.StringHashMap(struct_info).init(allocator),
            .traits = std.StringHashMap(trait_info).init(allocator),
            .trait_impls = std.StringHashMap(std.array_list.Managed([]const u8)).init(allocator),
            .trait_neg_impls = std.StringHashMap(std.array_list.Managed([]const u8)).init(allocator),
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
        self.global_consts.deinit();
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
        var trait_neg_impl_it = self.trait_neg_impls.iterator();
        while (trait_neg_impl_it.next()) |entry| {
            entry.value_ptr.*.deinit();
        }
        self.trait_neg_impls.deinit();
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

    pub fn emit(self: *builder, inst: ink.exe.instruction) lower_error!void {
        self.instructions.append(inst) catch return error.out_of_memory;
    }

    pub fn new_label(self: *builder) ink.exe.label_id {
        const id = self.next_label;
        self.next_label += 1;
        return id;
    }

    pub fn intern_const(self: *builder, value: u64) lower_error!u32 {
        if (self.const_map.get(value)) |idx| return idx;
        if (self.constants.items.len >= 8192) return error.constant_index_overflow;
        const idx: u32 = @intCast(self.constants.items.len);
        self.constants.append(value) catch return error.out_of_memory;
        self.const_map.put(value, idx) catch return error.out_of_memory;
        return idx;
    }

    fn node(self: *builder, id: mir_mod.mir_identifier) mir_mod.mir {
        return self.nodes[@intCast(id.idx)];
    }

    fn string_value(self: *builder, id: mir_mod.string_identifier) []const u8 {
        if (id.idx < self.strings.len) return self.strings[id.idx];
        return "<missing>";
    }

    fn set_error_message(self: *builder, node_id: ?mir_mod.mir_identifier, msg: []const u8) void {
        if (self.error_info == null) return;
        const info = self.error_info.?;
        if (info.message != null) return;
        const owned = self.allocator.dupe(u8, msg) catch return;
        info.message = owned;
        info.node = node_id;
        info.owns_message = true;
    }

    fn set_error_fmt(self: *builder, node_id: ?mir_mod.mir_identifier, comptime fmt: []const u8, args: anytype) void {
        if (self.error_info == null) return;
        const info = self.error_info.?;
        if (info.message != null) return;
        var buf = std.array_list.Managed(u8).init(self.allocator);
        defer buf.deinit();
        buf.writer().print(fmt, args) catch return;
        const owned = buf.toOwnedSlice() catch return;
        info.message = owned;
        info.node = node_id;
        info.owns_message = true;
    }

    fn instance_key(self: *builder, info: function_info, bindings: []const generic_binding) lower_error![]const u8 {
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

    fn ensure_instance(self: *builder, info: function_info, bindings: []const generic_binding) lower_error!ink.exe.label_id {
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

    fn const_index_for_label(self: *builder, label: ink.exe.label_id) lower_error!u32 {
        if (self.label_constants.get(label)) |idx| return idx;
        if (self.constants.items.len >= 8192) return error.constant_index_overflow;
        const idx: u32 = @intCast(self.constants.items.len);
        self.constants.append(0) catch return error.out_of_memory;
        self.label_constants.put(label, idx) catch return error.out_of_memory;
        return idx;
    }
};

fn type_key_from_type_node_with_self(b: *builder, id: mir_mod.mir_identifier, self_name: ?[]const u8) type_key {
    const node = b.node(id);
    return switch (node) {
        .identifier => |ident| .{ .name = b.string_value(ident) },
        .type => |ty| switch (ty) {
            .self => if (self_name) |name| .{ .name = name } else .{ .name = "self" },
            .name => |name_id| .{ .name = b.string_value(name_id) },
            .dyn => |ref| blk: {
                var pos = std.array_list.Managed([]const u8).init(b.allocator);
                defer pos.deinit();
                var neg = std.array_list.Managed([]const u8).init(b.allocator);
                defer neg.deinit();
                var base: ?[]const u8 = null;
                collect_dyn_trait_names(b, ref, &base, &pos, &neg);
                const base_name = base orelse break :blk .unknown;
                if (!b.traits.contains(base_name)) break :blk .unknown;
                if (pos.items.len == 0 and neg.items.len == 0) {
                    break :blk .{ .dyn_trait = base_name };
                }
                const arg_count = 1 + pos.items.len + neg.items.len;
                const args = b.allocator.alloc(type_key, arg_count) catch break :blk .unknown;
                args[0] = .{ .name = base_name };
                var idx: usize = 1;
                for (pos.items) |name| {
                    args[idx] = .{ .name = name };
                    idx += 1;
                }
                for (neg.items) |name| {
                    const inner_args = b.allocator.alloc(type_key, 1) catch break :blk .unknown;
                    inner_args[0] = .{ .name = name };
                    b.owned_type_slices.append(b.allocator, inner_args) catch break :blk .unknown;
                    args[idx] = .{ .applied = .{ .base = "not", .args = inner_args } };
                    idx += 1;
                }
                b.owned_type_slices.append(b.allocator, args) catch break :blk .unknown;
                break :blk .{ .applied = .{ .base = "dyn", .args = args } };
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

fn type_key_from_type_node(b: *builder, id: mir_mod.mir_identifier) type_key {
    return type_key_from_type_node_with_self(b, id, null);
}

fn type_name_from_type_node(b: *builder, id: mir_mod.mir_identifier) ?[]const u8 {
    const node = b.node(id);
    if (node == .identifier) return b.string_value(node.identifier);
    if (node != .type) return null;
    return switch (node.type) {
        .name => |name_id| b.string_value(name_id),
        .applied => |ap| b.string_value(ap.base),
        .dyn => |ref| blk: {
            const inner = b.node(ref);
            if (inner == .identifier) break :blk b.string_value(inner.identifier);
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

fn constraint_from_type_node(b: *builder, id: mir_mod.mir_identifier) ?trait_constraint {
    const node = b.node(id);
    if (node == .type and node.type == .applied) {
        const ap = node.type.applied;
        if (std.mem.eql(u8, b.string_value(ap.base), "not") and ap.args.len >= 1) {
            const inner_name = type_name_from_type_node(b, ap.args[0]) orelse return null;
            return .{ .name = inner_name, .negative = true };
        }
    }
    const name = type_name_from_type_node(b, id) orelse return null;
    return .{ .name = name, .negative = false };
}

fn add_builtin_traits(b: *builder) void {
    const builtin_names = [_][]const u8{ "send", "sync", "sized" };
    for (builtin_names) |name| {
        if (b.traits.contains(name)) continue;
        const methods = b.allocator.alloc(trait_method, 0) catch return;
        const reqs = b.allocator.alloc(trait_constraint, 0) catch return;
        b.traits.put(name, .{
            .methods = methods,
            .requires = reqs,
            .is_auto = true,
        }) catch return;
    }
}

fn is_unsized_marker(b: *builder, id: mir_mod.mir_identifier) bool {
    const node = b.node(id);
    if (node != .type) return false;
    if (node.type != .optional) return false;
    const inner_name = type_name_from_type_node(b, node.type.optional) orelse return false;
    return std.mem.eql(u8, inner_name, "sized");
}

fn append_dyn_trait_name(base: *?[]const u8, list: *std.array_list.Managed([]const u8), name: []const u8) void {
    if (base.* == null) {
        base.* = name;
        return;
    }
    for (list.items) |item| {
        if (std.mem.eql(u8, item, name)) return;
    }
    list.append(name) catch {};
}

fn collect_dyn_trait_names(
    b: *builder,
    id: mir_mod.mir_identifier,
    base: *?[]const u8,
    pos: *std.array_list.Managed([]const u8),
    neg: *std.array_list.Managed([]const u8),
) void {
    const node = b.node(id);
    switch (node) {
        .type => |ty| switch (ty) {
            .name => |name_id| append_dyn_trait_name(base, pos, b.string_value(name_id)),
            .applied => |ap| {
                const base_name = b.string_value(ap.base);
                if (std.mem.eql(u8, base_name, "intersect")) {
                    for (ap.args) |arg| {
                        collect_dyn_trait_names(b, arg, base, pos, neg);
                    }
                    return;
                }
                if (std.mem.eql(u8, base_name, "not") and ap.args.len >= 1) {
                    if (type_name_from_type_node(b, ap.args[0])) |inner_name| {
                        for (neg.items) |item| {
                            if (std.mem.eql(u8, item, inner_name)) return;
                        }
                        neg.append(inner_name) catch {};
                    }
                    return;
                }
                append_dyn_trait_name(base, pos, base_name);
            },
            else => {},
        },
        .identifier => |ident| append_dyn_trait_name(base, pos, b.string_value(ident)),
        else => {},
    }
}

fn struct_word_count(
    b: *builder,
    name: []const u8,
    visited: *std.StringHashMapUnmanaged(void),
) u8 {
    if (visited.contains(name)) return 1;
    const info = b.structs.get(name) orelse return 1;
    visited.put(b.allocator, name, {}) catch return 1;
    var count: u8 = 0;
    for (info.fields) |field| {
        const field_ty = type_key_from_type_node_with_self(b, field.ty, name);
        count +|= word_count_for_type(b, field_ty);
    }
    if (count == 0) count = 1;
    return count;
}

fn array_length_from_type_key(key: type_key) ?u64 {
    return switch (key) {
        .name => |name| std.fmt.parseInt(u64, name, 10) catch null,
        else => null,
    };
}

fn word_count_for_type(b: *builder, ty: type_key) u8 {
    switch (ty) {
        .name => |name| {
            if (!b.structs.contains(name)) return 1;
            var visited = std.StringHashMapUnmanaged(void){};
            defer visited.deinit(b.allocator);
            return struct_word_count(b, name, &visited);
        },
        .dyn_trait => return 2,
        .applied => |ap| {
            if (std.mem.eql(u8, ap.base, "atomic")) return 2;
            if (std.mem.eql(u8, ap.base, "dyn")) return 2;
            if (std.mem.eql(u8, ap.base, "array")) {
                if (ap.args.len < 2) return 1;
                const len = array_length_from_type_key(ap.args[0]) orelse return 1;
                const elem_words = word_count_for_type(b, ap.args[1]);
                const total = len * @as(u64, elem_words);
                if (total == 0) return 1;
                if (total > std.math.maxInt(u8)) return std.math.maxInt(u8);
                return @intCast(total);
            }
            if (!b.structs.contains(ap.base)) return 1;
            var visited = std.StringHashMapUnmanaged(void){};
            defer visited.deinit(b.allocator);
            return struct_word_count(b, ap.base, &visited);
        },
        else => return 1,
    }
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

fn decl_generic_param(b: *builder, decl: mir_mod.mir.function_decl, name: []const u8) ?mir_mod.mir.generic_param {
    for (decl.generics) |param| {
        if (param.kind != .type) continue;
        const param_name = b.string_value(param.name);
        if (std.mem.eql(u8, param_name, name)) return param;
    }
    return null;
}

fn is_decl_generic(b: *builder, decl: mir_mod.mir.function_decl, name: []const u8) bool {
    return decl_generic_param(b, decl, name) != null;
}

fn type_key_has_generic(b: *builder, decl: mir_mod.mir.function_decl, key: type_key) bool {
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
    left_decl: mir_mod.mir.function_decl,
    right_decl: mir_mod.mir.function_decl,
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
    decl: mir_mod.mir.function_decl,
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

fn type_has_neg_impl(b: *builder, trait_name: []const u8, type_name: []const u8) bool {
    const set = b.trait_neg_impls.get(trait_name) orelse return false;
    for (set.items) |item| {
        if (std.mem.eql(u8, item, type_name)) return true;
    }
    return false;
}

fn auto_trait_struct_fields(
    b: *builder,
    struct_name: []const u8,
    info: struct_info,
    args: ?[]const type_key,
    trait_name: []const u8,
    visited_traits: *std.ArrayListUnmanaged([]const u8),
    visited_types: *std.StringHashMapUnmanaged(void),
) bool {
    if (visited_types.contains(struct_name)) return true;
    visited_types.put(b.allocator, struct_name, {}) catch return true;
    defer _ = visited_types.remove(struct_name);

    var bindings = std.ArrayListUnmanaged(generic_binding){};
    defer bindings.deinit(b.allocator);
    if (args) |arg_list| {
        var arg_idx: usize = 0;
        for (info.generics) |gen| {
            if (arg_idx >= arg_list.len) break;
            if (gen.kind == .type) {
                const name = b.string_value(gen.name);
                bindings.append(b.allocator, .{ .name = name, .ty = arg_list[arg_idx] }) catch return true;
            }
            arg_idx += 1;
        }
    }

    for (info.fields) |field| {
        var field_key = type_key_from_type_node_with_self(b, field.ty, struct_name);
        if (bindings.items.len > 0) {
            field_key = apply_bindings_to_type_key(b, field_key, bindings.items);
        }
        if (!type_satisfies_trait(b, field_key, trait_name, visited_traits, visited_types)) return false;
    }
    return true;
}

fn auto_trait_structural(
    b: *builder,
    ty: type_key,
    trait_name: []const u8,
    visited_traits: *std.ArrayListUnmanaged([]const u8),
    visited_types: *std.StringHashMapUnmanaged(void),
) bool {
    if (ty == .unknown) return true;
    return switch (ty) {
        .name => |name| blk: {
            if (b.structs.get(name)) |info| {
                break :blk auto_trait_struct_fields(b, name, info, null, trait_name, visited_traits, visited_types);
            }
            break :blk true;
        },
        .applied => |ap| blk: {
            if (std.mem.eql(u8, ap.base, "array")) {
                if (ap.args.len < 2) break :blk true;
                break :blk type_satisfies_trait(b, ap.args[1], trait_name, visited_traits, visited_types);
            }
            if (std.mem.eql(u8, ap.base, "slice") or std.mem.eql(u8, ap.base, "optional") or std.mem.eql(u8, ap.base, "ref") or std.mem.eql(u8, ap.base, "ref_mut") or std.mem.eql(u8, ap.base, "box") or std.mem.eql(u8, ap.base, "atomic") or std.mem.eql(u8, ap.base, "task")) {
                if (ap.args.len < 1) break :blk true;
                break :blk type_satisfies_trait(b, ap.args[0], trait_name, visited_traits, visited_types);
            }
            if (std.mem.eql(u8, ap.base, "result")) {
                if (ap.args.len < 2) break :blk true;
                if (!type_satisfies_trait(b, ap.args[0], trait_name, visited_traits, visited_types)) break :blk false;
                break :blk type_satisfies_trait(b, ap.args[1], trait_name, visited_traits, visited_types);
            }
            if (std.mem.eql(u8, ap.base, "intersect") or std.mem.eql(u8, ap.base, "union")) {
                for (ap.args) |arg| {
                    if (!type_satisfies_trait(b, arg, trait_name, visited_traits, visited_types)) break :blk false;
                }
                break :blk true;
            }
            if (b.structs.get(ap.base)) |info| {
                break :blk auto_trait_struct_fields(b, ap.base, info, ap.args, trait_name, visited_traits, visited_types);
            }
            for (ap.args) |arg| {
                if (!type_satisfies_trait(b, arg, trait_name, visited_traits, visited_types)) break :blk false;
            }
            break :blk true;
        },
        else => true,
    };
}

fn auto_trait_send(
    b: *builder,
    ty: type_key,
    visited_traits: *std.ArrayListUnmanaged([]const u8),
    visited_types: *std.StringHashMapUnmanaged(void),
) bool {
    if (ty == .unknown) return true;
    return switch (ty) {
        .name => |name| blk: {
            if (b.structs.get(name)) |info| {
                break :blk auto_trait_struct_fields(b, name, info, null, "send", visited_traits, visited_types);
            }
            break :blk true;
        },
        .applied => |ap| blk: {
            if (std.mem.eql(u8, ap.base, "ref")) {
                if (ap.args.len < 1) break :blk true;
                break :blk type_satisfies_trait(b, ap.args[0], "sync", visited_traits, visited_types);
            }
            if (std.mem.eql(u8, ap.base, "ref_mut")) {
                if (ap.args.len < 1) break :blk true;
                break :blk type_satisfies_trait(b, ap.args[0], "send", visited_traits, visited_types);
            }
            if (std.mem.eql(u8, ap.base, "slice")) {
                if (ap.args.len < 1) break :blk true;
                break :blk type_satisfies_trait(b, ap.args[0], "sync", visited_traits, visited_types);
            }
            if (std.mem.eql(u8, ap.base, "atomic") or std.mem.eql(u8, ap.base, "task")) {
                if (ap.args.len < 1) break :blk true;
                break :blk type_satisfies_trait(b, ap.args[0], "send", visited_traits, visited_types);
            }
            if (std.mem.eql(u8, ap.base, "array")) {
                if (ap.args.len < 2) break :blk true;
                break :blk type_satisfies_trait(b, ap.args[1], "send", visited_traits, visited_types);
            }
            break :blk auto_trait_structural(b, ty, "send", visited_traits, visited_types);
        },
        else => true,
    };
}

fn auto_trait_sized(
    b: *builder,
    ty: type_key,
    visited_traits: *std.ArrayListUnmanaged([]const u8),
    visited_types: *std.StringHashMapUnmanaged(void),
) bool {
    if (ty == .unknown) return true;
    return switch (ty) {
        .name => |name| blk: {
            if (b.structs.get(name)) |info| {
                break :blk auto_trait_struct_fields(b, name, info, null, "sized", visited_traits, visited_types);
            }
            break :blk true;
        },
        .applied => |ap| blk: {
            if (std.mem.eql(u8, ap.base, "ref") or std.mem.eql(u8, ap.base, "ref_mut") or std.mem.eql(u8, ap.base, "box") or std.mem.eql(u8, ap.base, "slice") or std.mem.eql(u8, ap.base, "task") or std.mem.eql(u8, ap.base, "atomic")) {
                break :blk true;
            }
            if (std.mem.eql(u8, ap.base, "array")) {
                if (ap.args.len < 2) break :blk true;
                break :blk type_satisfies_trait(b, ap.args[1], "sized", visited_traits, visited_types);
            }
            break :blk auto_trait_structural(b, ty, "sized", visited_traits, visited_types);
        },
        else => true,
    };
}

fn auto_trait_sync(
    b: *builder,
    ty: type_key,
    visited_traits: *std.ArrayListUnmanaged([]const u8),
    visited_types: *std.StringHashMapUnmanaged(void),
) bool {
    if (ty == .unknown) return true;
    return switch (ty) {
        .name => |name| blk: {
            if (b.structs.get(name)) |info| {
                break :blk auto_trait_struct_fields(b, name, info, null, "sync", visited_traits, visited_types);
            }
            break :blk true;
        },
        .applied => |ap| blk: {
            if (std.mem.eql(u8, ap.base, "ref")) {
                if (ap.args.len < 1) break :blk true;
                break :blk type_satisfies_trait(b, ap.args[0], "sync", visited_traits, visited_types);
            }
            if (std.mem.eql(u8, ap.base, "ref_mut")) {
                break :blk false;
            }
            if (std.mem.eql(u8, ap.base, "slice")) {
                if (ap.args.len < 1) break :blk true;
                break :blk type_satisfies_trait(b, ap.args[0], "sync", visited_traits, visited_types);
            }
            if (std.mem.eql(u8, ap.base, "atomic") or std.mem.eql(u8, ap.base, "task")) {
                if (ap.args.len < 1) break :blk true;
                break :blk type_satisfies_trait(b, ap.args[0], "send", visited_traits, visited_types);
            }
            if (std.mem.eql(u8, ap.base, "array")) {
                if (ap.args.len < 2) break :blk true;
                break :blk type_satisfies_trait(b, ap.args[1], "sync", visited_traits, visited_types);
            }
            break :blk auto_trait_structural(b, ty, "sync", visited_traits, visited_types);
        },
        else => true,
    };
}

fn auto_trait_satisfied(
    b: *builder,
    ty: type_key,
    trait_name: []const u8,
    visited_traits: *std.ArrayListUnmanaged([]const u8),
    visited_types: *std.StringHashMapUnmanaged(void),
) bool {
    if (std.mem.eql(u8, trait_name, "sized")) return auto_trait_sized(b, ty, visited_traits, visited_types);
    if (std.mem.eql(u8, trait_name, "send")) return auto_trait_send(b, ty, visited_traits, visited_types);
    if (std.mem.eql(u8, trait_name, "sync")) return auto_trait_sync(b, ty, visited_traits, visited_types);
    return auto_trait_structural(b, ty, trait_name, visited_traits, visited_types);
}

fn type_satisfies_trait(
    b: *builder,
    ty: type_key,
    trait_name: []const u8,
    visited_traits: *std.ArrayListUnmanaged([]const u8),
    visited_types: *std.StringHashMapUnmanaged(void),
) bool {
    if (ty == .unknown) return true;
    if (std.mem.eql(u8, trait_name, "print_to")) {
        if (type_key_base_name(ty)) |base_name| {
            if (is_builtin_type_name(base_name)) return true;
        }
    }
    if (dyn_base_from_type_key(ty) != null) {
        return dyn_has_positive(ty, trait_name);
    }
    const info = b.traits.get(trait_name) orelse return false;
    if (type_key_base_name(ty)) |base_name| {
        if (type_has_neg_impl(b, trait_name, base_name)) return false;
    }
    var has_impl = false;
    if (type_key_base_name(ty)) |base_name| {
        has_impl = type_has_impl(b, trait_name, base_name);
    }
    if (info.methods.len != 0 and !has_impl) return false;
    if (!info.is_auto and info.methods.len == 0 and info.requires.len == 0 and !has_impl) return false;

    if (info.requires.len > 0) {
        for (visited_traits.items) |name| {
            if (std.mem.eql(u8, name, trait_name)) return true;
        }
        const prev_len = visited_traits.items.len;
        visited_traits.append(b.allocator, trait_name) catch return false;
        defer visited_traits.shrinkRetainingCapacity(prev_len);

        for (info.requires) |req| {
            if (req.negative) {
                if (type_satisfies_trait(b, ty, req.name, visited_traits, visited_types)) return false;
            } else {
                if (!type_satisfies_trait(b, ty, req.name, visited_traits, visited_types)) return false;
            }
        }
    }

    if (info.is_auto and info.methods.len == 0) {
        if (has_impl) return true;
        return auto_trait_satisfied(b, ty, trait_name, visited_traits, visited_types);
    }
    if (!info.is_auto and info.methods.len == 0 and info.requires.len > 0) {
        return true;
    }
    return has_impl;
}

fn type_satisfies_trait_root(b: *builder, ty: type_key, trait_name: []const u8) bool {
    var visited = std.ArrayListUnmanaged([]const u8){};
    defer visited.deinit(b.allocator);
    var visited_types = std.StringHashMapUnmanaged(void){};
    defer visited_types.deinit(b.allocator);
    return type_satisfies_trait(b, ty, trait_name, &visited, &visited_types);
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

const function_ctx = struct {
    b: *builder,
    locals: std.StringHashMap(u8),
    local_types: std.StringHashMap(type_key),
    local_dyn_types: std.StringHashMap(type_key),
    global_const_stack: std.StringHashMap(u8),
    temp_base: u8,
    next_temp: u8,
    returned: bool,
    pinned_mask: u64,
    self_type: ?[]const u8,
    bindings: []const generic_binding,
    return_type: type_key,
    return_words: u8,
    sret_reg: ?u8,
    loop_stack: std.ArrayListUnmanaged(loop_frame),

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

    const loop_frame = struct {
        label: ?[]const u8,
        break_label: ink.exe.label_id,
        continue_label: ?ink.exe.label_id,
        result_reg: u8,
        result_words: u8,
    };

    pub fn init(
        b: *builder,
        params: []const mir_mod.mir.function_decl.param,
        self_type: ?[]const u8,
        bindings: []const generic_binding,
        return_type: ?mir_mod.mir_identifier,
    ) lower_error!function_ctx {
        var locals = std.StringHashMap(u8).init(b.allocator);
        errdefer locals.deinit();
        var local_types = std.StringHashMap(type_key).init(b.allocator);
        errdefer local_types.deinit();
        var local_dyn_types = std.StringHashMap(type_key).init(b.allocator);
        errdefer local_dyn_types.deinit();
        var global_const_stack = std.StringHashMap(u8).init(b.allocator);
        errdefer global_const_stack.deinit();

        var resolved_return = if (return_type) |ret_id|
            type_key_from_type_node_with_self(b, ret_id, self_type)
        else
            type_key{ .name = "unit" };
        if (bindings.len > 0) {
            resolved_return = apply_bindings_to_type_key(b, resolved_return, bindings);
        }
        const return_words = word_count_for_type(b, resolved_return);
        const sret_reg: ?u8 = if (return_words > 1) 1 else null;
        var reg_index: u8 = if (sret_reg != null) 2 else 1;
        for (params) |param| {
            const name = b.string_value(param.name);
            locals.put(name, reg_index) catch return error.out_of_memory;
            var ty = type_key_from_type_node_with_self(b, param.ty, self_type);
            if (bindings.len > 0) {
                ty = apply_bindings_to_type_key(b, ty, bindings);
            }
            local_types.put(name, ty) catch return error.out_of_memory;
            reg_index += word_count_for_type(b, ty);
        }

        if (reg_index > max_register) return error.register_overflow;

        return .{
            .b = b,
            .locals = locals,
            .local_types = local_types,
            .local_dyn_types = local_dyn_types,
            .global_const_stack = global_const_stack,
            .temp_base = reg_index,
            .next_temp = reg_index,
            .returned = false,
            .pinned_mask = 0,
            .self_type = self_type,
            .bindings = bindings,
            .return_type = resolved_return,
            .return_words = return_words,
            .sret_reg = sret_reg,
            .loop_stack = .{},
        };
    }

    pub fn deinit(self: *function_ctx) void {
        self.locals.deinit();
        self.local_types.deinit();
        self.local_dyn_types.deinit();
        self.global_const_stack.deinit();
        self.loop_stack.deinit(self.b.allocator);
    }

    fn alloc_temp(self: *function_ctx) lower_error!u8 {
        if (self.next_temp > max_register) return error.register_overflow;
        const reg = self.next_temp;
        self.next_temp += 1;
        return reg;
    }

    fn alloc_temp_words(self: *function_ctx, count: u8) lower_error!u8 {
        if (count == 0) return self.alloc_temp();
        const end = @as(u16, self.next_temp) + count;
        if (end > max_register + 1) return error.register_overflow;
        const reg = self.next_temp;
        self.next_temp = @intCast(end);
        return reg;
    }

    fn alloc_local(self: *function_ctx) lower_error!u8 {
        if (self.next_temp > max_register) return error.register_overflow;
        const reg = self.next_temp;
        self.next_temp += 1;
        self.temp_base = self.next_temp;
        return reg;
    }

    fn alloc_local_words(self: *function_ctx, count: u8) lower_error!u8 {
        if (count == 0) return self.alloc_local();
        const end = @as(u16, self.next_temp) + count;
        if (end > max_register + 1) return error.register_overflow;
        const reg = self.next_temp;
        self.next_temp = @intCast(end);
        self.temp_base = self.next_temp;
        return reg;
    }

    fn free_temp(self: *function_ctx, reg: u8) void {
        if (reg >= self.temp_base and !self.is_pinned(reg) and reg + 1 == self.next_temp) {
            self.next_temp -= 1;
        }
    }

    fn free_temp_words(self: *function_ctx, base: u8, count: u8) void {
        if (count == 0) return;
        if (base < self.temp_base) return;
        if (@as(u16, base) + count != self.next_temp) return;
        var idx: u8 = 0;
        while (idx < count) : (idx += 1) {
            if (self.is_pinned(base + idx)) return;
        }
        self.next_temp = base;
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

    fn push_loop(
        self: *function_ctx,
        label: ?[]const u8,
        break_label: ink.exe.label_id,
        continue_label: ?ink.exe.label_id,
        result_reg: u8,
        result_words: u8,
    ) lower_error!void {
        self.loop_stack.append(self.b.allocator, .{
            .label = label,
            .break_label = break_label,
            .continue_label = continue_label,
            .result_reg = result_reg,
            .result_words = result_words,
        }) catch return error.out_of_memory;
    }

    fn pop_loop(self: *function_ctx) void {
        if (self.loop_stack.items.len == 0) return;
        self.loop_stack.items.len -= 1;
    }

    fn find_break_frame(self: *function_ctx, label: ?[]const u8) ?*loop_frame {
        var idx = self.loop_stack.items.len;
        while (idx > 0) {
            idx -= 1;
            const frame = &self.loop_stack.items[idx];
            if (label) |lab| {
                if (frame.label != null and std.mem.eql(u8, frame.label.?, lab)) return frame;
            } else if (frame.continue_label != null) {
                return frame;
            }
        }
        return null;
    }

    fn find_continue_frame(self: *function_ctx, label: ?[]const u8) ?*loop_frame {
        var idx = self.loop_stack.items.len;
        while (idx > 0) {
            idx -= 1;
            const frame = &self.loop_stack.items[idx];
            if (frame.continue_label == null) continue;
            if (label) |lab| {
                if (frame.label != null and std.mem.eql(u8, frame.label.?, lab)) return frame;
            } else {
                return frame;
            }
        }
        return null;
    }

    fn is_unbound_type_name(self: *function_ctx, name: []const u8) bool {
        if (is_builtin_type_name(name)) return false;
        if (self.b.structs.contains(name)) return false;
        if (self.b.traits.contains(name)) return false;
        return true;
    }

    fn dyn_concrete_type(self: *function_ctx, id: mir_mod.mir_identifier) ?type_key {
        const node = self.b.node(id);
        switch (node) {
            .binary => |bin| {
                if (bin.op != .@"as") return null;
                const type_node = self.b.node(bin.right);
                if (type_node == .type and type_node.type == .dyn) {
                    const inferred = self.infer_expr_type(bin.left);
                    switch (inferred) {
                        .unknown => {
                            if (self.type_name_from_value(bin.left)) |name| {
                                return .{ .name = name };
                            }
                            return null;
                        },
                        .name => |name| {
                            if (self.is_unbound_type_name(name)) {
                                if (self.type_name_from_value(bin.left)) |resolved| {
                                    return .{ .name = resolved };
                                }
                                return null;
                            }
                        },
                        else => {},
                    }
                    return inferred;
                }
            },
            .identifier => |ident| {
                const name = self.b.string_value(ident);
                if (self.local_dyn_types.get(name)) |ty| {
                    if (ty == .unknown) return null;
                    return ty;
                }
            },
            else => {},
        }
        return null;
    }

    fn type_word_count(self: *function_ctx, ty: type_key) u8 {
        return word_count_for_type(self.b, ty);
    }

    fn expr_word_count(self: *function_ctx, id: mir_mod.mir_identifier) u8 {
        return self.type_word_count(self.infer_expr_type(id));
    }

    const field_layout = struct {
        offset: u8,
        ty: type_key,
        words: u8,
    };

    fn struct_field_layout(
        self: *function_ctx,
        struct_name: []const u8,
        field_id: mir_mod.string_identifier,
    ) ?field_layout {
        const info = self.b.structs.get(struct_name) orelse return null;
        var offset: u8 = 0;
        for (info.fields) |field| {
            const field_ty = type_key_from_type_node_with_self(self.b, field.ty, struct_name);
            const field_words = self.type_word_count(field_ty);
            if (field.name.idx == field_id.idx) {
                return .{
                    .offset = offset,
                    .ty = field_ty,
                    .words = field_words,
                };
            }
            offset +|= field_words;
        }
        return null;
    }

    fn copy_words(self: *function_ctx, dst_base: u8, src_base: u8, count: u8) lower_error!void {
        var idx: u8 = 0;
        while (idx < count) : (idx += 1) {
            const dst = dst_base + idx;
            const src = src_base + idx;
            try self.emit(.{ .move = .{ .dst = dst, .src = src } });
        }
    }

    fn zero_words(self: *function_ctx, base: u8, count: u8) lower_error!void {
        if (count == 0) return;
        const zero_idx = try self.b.intern_const(0);
        const zero_reg = try self.alloc_temp();
        try self.emit(.{ .load_const = .{ .dst = zero_reg, .const_index = zero_idx } });
        var idx: u8 = 0;
        while (idx < count) : (idx += 1) {
            try self.emit(.{ .move = .{ .dst = base + idx, .src = zero_reg } });
        }
        self.free_temp(zero_reg);
    }

    fn load_words_from_ptr(self: *function_ctx, ptr_reg: u8, dst_base: u8, count: u8) lower_error!void {
        if (count == 0) return;
        var idx: u8 = 0;
        while (idx < count) : (idx += 1) {
            if (idx == 0) {
                try self.deref_into(ptr_reg, dst_base);
                continue;
            }
            const offset_idx = try self.b.intern_const(@intCast(idx));
            const off_reg = try self.alloc_temp();
            try self.emit(.{ .load_const = .{ .dst = off_reg, .const_index = offset_idx } });
            try self.emit(.{ .add = .{ .dst = off_reg, .src_a = ptr_reg, .src_b = off_reg } });
            try self.deref_into(off_reg, dst_base + idx);
            if (self.is_temp(off_reg)) self.free_temp(off_reg);
        }
    }

    fn ptr_of_reg(self: *function_ctx, reg: u8) lower_error!u8 {
        const reg_idx = try self.b.intern_const(@intCast(reg));
        const reg_val = try self.alloc_temp();
        try self.emit(.{ .load_const = .{ .dst = reg_val, .const_index = reg_idx } });
        const foreign_idx = try self.foreign_index("std::ptr_of");
        var args = [_]u8{reg_val};
        try self.emit_foreign_call(foreign_idx, args[0..]);
        if (self.is_temp(reg_val)) self.free_temp(reg_val);
        return self.save_result_reg(0);
    }

    fn store_words_to_ptr(self: *function_ctx, ptr_reg: u8, src_base: u8, count: u8) lower_error!void {
        if (count == 0) return;
        var idx: u8 = 0;
        while (idx < count) : (idx += 1) {
            if (idx == 0) {
                try self.store_value(ptr_reg, src_base);
                continue;
            }
            const offset_idx = try self.b.intern_const(@intCast(idx));
            const off_reg = try self.alloc_temp();
            try self.emit(.{ .load_const = .{ .dst = off_reg, .const_index = offset_idx } });
            try self.emit(.{ .add = .{ .dst = off_reg, .src_a = ptr_reg, .src_b = off_reg } });
            try self.store_value(off_reg, src_base + idx);
            if (self.is_temp(off_reg)) self.free_temp(off_reg);
        }
    }

    fn compile_atomic_load_reg(
        self: *function_ctx,
        ptr_reg: u8,
        inner_type: type_key,
    ) lower_error!u8 {
        const words = self.type_word_count(inner_type);
        try self.emit_atomic_lock();
        const dst_reg = try self.alloc_temp_words(words);
        try self.load_words_from_ptr(ptr_reg, dst_reg, words);
        try self.emit_atomic_unlock();
        return dst_reg;
    }

    fn free_temp_value(self: *function_ctx, reg: u8, ty: type_key) void {
        if (!self.is_temp(reg)) return;
        const words = self.type_word_count(ty);
        if (words <= 1) {
            self.free_temp(reg);
        } else {
            self.free_temp_words(reg, words);
        }
    }

    fn emit_return_value(self: *function_ctx, value_reg: u8, value_type: type_key) lower_error!void {
        _ = value_type;
        if (self.return_words > 1) {
            const sret = self.sret_reg orelse return error.unsupported_node;
            try self.store_words_to_ptr(sret, value_reg, self.return_words);
            try self.emit(.{ .ret = {} });
        } else {
            try self.emit(.{ .ret_value = .{ .src = value_reg } });
        }
        self.returned = true;
    }

    fn infer_expr_type(self: *function_ctx, id: mir_mod.mir_identifier) type_key {
        if (self.b.node_types) |types| {
            const idx: usize = @intCast(id.idx);
            if (idx < types.len) {
                const mapped = types[idx];
                if (mapped != .unknown) {
                    if (mapped == .name and self.is_unbound_type_name(mapped.name)) {
                        // Fall back to local inference to refine unbound type names.
                    } else {
                        return mapped;
                    }
                }
            }
        }
        const node = self.b.node(id);
        return switch (node) {
            .integer => .{ .name = "int" },
            .float => .{ .name = "float" },
            .boolean => .{ .name = "bool" },
            .string => .{ .name = "string" },
            .identifier => |ident| blk: {
                const name = self.b.string_value(ident);
                if (self.local_types.get(name)) |ty| break :blk ty;
                if (self.b.global_consts.get(name)) |info| {
                    break :blk self.infer_global_const_type(name, info);
                }
                break :blk .unknown;
            },
        .unary => |un| switch (un.op) {
            .@"try", .unwrap_optional => .unknown,
            else => blk: {
                const right = self.infer_expr_type(un.right);
                if (un.op == .borrow or un.op == .ref) {
                    const args = self.b.allocator.alloc(type_key, 1) catch break :blk .unknown;
                    args[0] = right;
                    self.b.owned_type_slices.append(self.b.allocator, args) catch break :blk .unknown;
                    break :blk .{ .applied = .{ .base = "ref", .args = args } };
                }
                if (un.op == .borrow_mut or un.op == .ref_mut) {
                    const args = self.b.allocator.alloc(type_key, 1) catch break :blk .unknown;
                    args[0] = right;
                    self.b.owned_type_slices.append(self.b.allocator, args) catch break :blk .unknown;
                    break :blk .{ .applied = .{ .base = "ref_mut", .args = args } };
                }
                if (un.op == .deref) {
                    if (right == .applied and right.applied.args.len == 1) {
                        if (std.mem.eql(u8, right.applied.base, "ref") or std.mem.eql(u8, right.applied.base, "ref_mut")) {
                            break :blk right.applied.args[0];
                        }
                    }
                    break :blk .{ .name = "int" };
                }
                if (un.op == .box) {
                    const args = self.b.allocator.alloc(type_key, 1) catch break :blk .unknown;
                    args[0] = right;
                    self.b.owned_type_slices.append(self.b.allocator, args) catch break :blk .unknown;
                    break :blk .{ .applied = .{ .base = "box", .args = args } };
                }
                if (unary_operator_method_name(un.op)) |method| {
                    const ret = self.infer_method_return_type(method, right, &.{});
                    if (ret != .unknown) break :blk ret;
                }
                break :blk right;
                },
            },
            .binary => |bin| switch (bin.op) {
                .call => self.infer_call_type(id),
                .access => blk: {
                    const left = self.infer_expr_type(bin.left);
                    const struct_name = switch (left) {
                        .name => |name| name,
                        else => break :blk .unknown,
                    };
                    const info = self.b.structs.get(struct_name) orelse break :blk .unknown;
                    const field_node = self.b.node(bin.right);
                    if (field_node != .identifier) break :blk .unknown;
                    const field_idx = struct_field_index(info, field_node.identifier) orelse break :blk .unknown;
                    const field = info.fields[field_idx];
                    break :blk type_key_from_type_node_with_self(self.b, field.ty, struct_name);
                },
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
                        if (ret != .unknown) {
                            if (ret == .name and self.is_unbound_type_name(ret.name)) {
                                const left_concrete = self.dyn_concrete_type(bin.left);
                                const right_concrete = self.dyn_concrete_type(bin.right);
                                if (left_concrete != null and right_concrete != null and
                                    type_key_eq(left_concrete.?, right_concrete.?))
                                {
                                    break :blk left_concrete.?;
                                }
                            }
                            break :blk ret;
                        }
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
        recv: mir_mod.mir_identifier,
        args: []const mir_mod.mir_identifier,
    ) lower_error!?u8 {
        const recv_type = self.infer_expr_type(recv);
        if (self.trait_name_from_type(recv_type)) |trait_name| {
            if (self.trait_method_index(trait_name, name) == null) {
                return error.unknown_function;
            }
            const recv_concrete = self.dyn_concrete_type(recv);
            const recv_reg = try self.compile_expr(recv);
            var recv_pinned = false;
            if (self.is_temp(recv_reg)) {
                self.pin_temp(recv_reg);
                recv_pinned = true;
            }

            if (recv_concrete) |recv_ty| {
                const recv_words = self.type_word_count(recv_ty);
                const recv_data_reg = try self.dyn_data_reg(recv_reg);
                const recv_value_reg = try self.alloc_temp_words(recv_words);
                try self.load_words_from_ptr(recv_data_reg, recv_value_reg, recv_words);

                var arg_regs_buf: [8]u8 = undefined;
                var arg_types_buf: [8]type_key = undefined;
                var arg_pinned: [8]bool = undefined;
                for (args, 0..) |arg_id, idx| {
                    var arg_type = self.infer_expr_type(arg_id);
                    var arg_reg = try self.compile_expr(arg_id);
                    var pinned = false;
                    if (self.is_temp(arg_reg)) {
                        self.pin_temp(arg_reg);
                        pinned = true;
                    }
                    if (self.trait_name_from_type(self.infer_expr_type(arg_id))) |arg_trait| {
                        if (std.mem.eql(u8, arg_trait, trait_name)) {
                            if (self.dyn_concrete_type(arg_id)) |arg_ty| {
                                if (!type_key_eq(recv_ty, arg_ty)) return error.unsupported_node;
                            }
                            const data_reg = try self.dyn_data_reg(arg_reg);
                            if (pinned) {
                                self.unpin_temp(arg_reg);
                                pinned = false;
                            }
                            const value_reg = try self.alloc_temp_words(recv_words);
                            try self.load_words_from_ptr(data_reg, value_reg, recv_words);
                            self.free_temp_value(arg_reg, arg_type);
                            arg_reg = value_reg;
                            arg_type = recv_ty;
                            if (self.is_temp(arg_reg)) {
                                self.pin_temp(arg_reg);
                                pinned = true;
                            }
                        }
                    }
                    arg_regs_buf[idx] = arg_reg;
                    arg_types_buf[idx] = arg_type;
                    arg_pinned[idx] = pinned;
                }

                const arg_regs = arg_regs_buf[0..args.len];
                const arg_types = arg_types_buf[0..args.len];
                const reg = try self.try_compile_method_call_regs(name, recv_value_reg, recv_ty, null, arg_regs, arg_types) orelse return error.unknown_function;

                for (arg_regs, 0..) |arg_reg, idx| {
                    if (arg_pinned[idx]) self.unpin_temp(arg_reg);
                    if (arg_reg != reg) {
                        self.free_temp_value(arg_reg, arg_types[idx]);
                    }
                }
                if (recv_value_reg != reg) {
                    self.free_temp_value(recv_value_reg, recv_ty);
                }
                if (recv_pinned) self.unpin_temp(recv_reg);
                self.free_temp_value(recv_reg, recv_type);
                return reg;
            }

            var arg_regs_buf: [8]u8 = undefined;
            var arg_types_buf: [8]type_key = undefined;
            var arg_pinned: [8]bool = undefined;
            for (args, 0..) |arg_id, idx| {
                var arg_type = self.infer_expr_type(arg_id);
                var arg_reg = try self.compile_expr(arg_id);
                var pinned = false;
                if (self.is_temp(arg_reg)) {
                    self.pin_temp(arg_reg);
                    pinned = true;
                }
                if (self.trait_name_from_type(self.infer_expr_type(arg_id))) |arg_trait| {
                    if (std.mem.eql(u8, arg_trait, trait_name)) {
                        var concrete: ?type_key = null;
                        if (recv_concrete) |recv_ty| {
                            if (self.dyn_concrete_type(arg_id)) |arg_ty| {
                                if (!type_key_eq(recv_ty, arg_ty)) return error.unsupported_node;
                            }
                            concrete = recv_ty;
                        } else {
                            concrete = self.dyn_concrete_type(arg_id);
                        }

                        const data_reg = try self.dyn_data_reg(arg_reg);
                        if (pinned) {
                            self.unpin_temp(arg_reg);
                            pinned = false;
                        }

                        if (concrete) |concrete_ty| {
                            const words = self.type_word_count(concrete_ty);
                            const value_reg = try self.alloc_temp_words(words);
                            try self.load_words_from_ptr(data_reg, value_reg, words);
                            self.free_temp_value(arg_reg, arg_type);
                            arg_reg = value_reg;
                            arg_type = concrete_ty;
                        } else {
                            return error.unsupported_node;
                        }

                        if (self.is_temp(arg_reg)) {
                            self.pin_temp(arg_reg);
                            pinned = true;
                        }
                    }
                }
                arg_regs_buf[idx] = arg_reg;
                arg_types_buf[idx] = arg_type;
                arg_pinned[idx] = pinned;
            }
            const arg_regs = arg_regs_buf[0..args.len];
            const arg_types = arg_types_buf[0..args.len];

            const reg = try self.compile_trait_method_call_regs(trait_name, name, recv_reg, arg_regs, arg_types, recv_concrete);

            for (arg_regs, 0..) |arg_reg, idx| {
                if (arg_pinned[idx]) self.unpin_temp(arg_reg);
                self.free_temp_value(arg_reg, arg_types[idx]);
            }
            if (recv_pinned) self.unpin_temp(recv_reg);
            self.free_temp_value(recv_reg, recv_type);
            return reg;
        }

        const group = self.b.functions.get(name) orelse return null;

        var arg_ids_buf: [8]mir_mod.mir_identifier = undefined;
        arg_ids_buf[0] = recv;
        std.mem.copyForwards(mir_mod.mir_identifier, arg_ids_buf[1 .. args.len + 1], args);
        const arg_ids = arg_ids_buf[0 .. args.len + 1];

        var arg_types: [8]type_key = undefined;
        for (arg_ids, 0..) |arg_id, idx| {
            arg_types[idx] = self.infer_expr_type(arg_id);
        }
        const arg_type_slice = arg_types[0..arg_ids.len];

        const resolved = self.resolve_function_overload(group.items, arg_type_slice) catch |err| switch (err) {
            error.unknown_function => return null,
            else => return err,
        };
        defer if (resolved.bindings.len > 0) self.b.allocator.free(resolved.bindings);

        var base_return = if (resolved.info.decl.return_type) |ret_id|
            type_key_from_type_node_with_self(self.b, ret_id, resolved.info.impl_for)
        else
            type_key{ .name = "unit" };
        if (resolved.bindings.len > 0) {
            base_return = apply_bindings_to_type_key(self.b, base_return, resolved.bindings);
        }
        const return_words = self.type_word_count(base_return);

        var arg_regs = self.b.allocator.alloc(u8, arg_ids.len) catch return error.out_of_memory;
        defer self.b.allocator.free(arg_regs);
        for (arg_ids, 0..) |arg_id, idx| {
            arg_regs[idx] = try self.compile_expr(arg_id);
        }

        var result_base: ?u8 = null;
        var sret_ptr: ?u8 = null;
        var dst_start: u8 = 1;
        if (return_words > 1) {
            const base_reg = try self.alloc_temp_words(return_words);
            const ptr_reg = try self.ptr_of_reg(base_reg);
            result_base = base_reg;
            sret_ptr = ptr_reg;
            dst_start = 2;
            try self.emit(.{ .argument_set = .{ .dst = 1, .src = ptr_reg } });
        }

        _ = try self.emit_argument_values(arg_regs, arg_type_slice, dst_start);

        const target = try self.b.ensure_instance(resolved.info, resolved.bindings);
        try self.emit(.{ .call = .{ .target = target } });

        if (sret_ptr) |ptr| if (self.is_temp(ptr)) self.free_temp(ptr);
        for (arg_regs, 0..) |arg_reg, idx| {
            self.free_temp_value(arg_reg, arg_type_slice[idx]);
        }

        if (result_base) |result_reg| return result_reg;
        const reg = try self.save_result_reg(0);
        return @as(?u8, reg);
    }

    fn try_compile_method_call_regs(
        self: *function_ctx,
        name: []const u8,
        recv_reg: u8,
        recv_type: type_key,
        recv_concrete: ?type_key,
        arg_regs: []const u8,
        arg_types: []const type_key,
    ) lower_error!?u8 {
        if (self.trait_name_from_type(recv_type)) |trait_name| {
            if (self.trait_method_index(trait_name, name) == null) {
                return error.unknown_function;
            }
            const reg = try self.compile_trait_method_call_regs(trait_name, name, recv_reg, arg_regs, arg_types, recv_concrete);
            return reg;
        }

        const group = self.b.functions.get(name) orelse return null;

        var call_types_buf: [8]type_key = undefined;
        call_types_buf[0] = recv_type;
        std.mem.copyForwards(type_key, call_types_buf[1 .. arg_types.len + 1], arg_types);
        const call_types = call_types_buf[0 .. arg_types.len + 1];

        const resolved = self.resolve_function_overload(group.items, call_types) catch |err| switch (err) {
            error.unknown_function => return null,
            else => return err,
        };
        defer if (resolved.bindings.len > 0) self.b.allocator.free(resolved.bindings);

        var base_return = if (resolved.info.decl.return_type) |ret_id|
            type_key_from_type_node_with_self(self.b, ret_id, resolved.info.impl_for)
        else
            type_key{ .name = "unit" };
        if (resolved.bindings.len > 0) {
            base_return = apply_bindings_to_type_key(self.b, base_return, resolved.bindings);
        }
        const return_words = self.type_word_count(base_return);

        var result_base: ?u8 = null;
        var sret_ptr: ?u8 = null;
        var dst_start: u8 = 1;
        if (return_words > 1) {
            const base_reg = try self.alloc_temp_words(return_words);
            const ptr_reg = try self.ptr_of_reg(base_reg);
            result_base = base_reg;
            sret_ptr = ptr_reg;
            dst_start = 2;
            try self.emit(.{ .argument_set = .{ .dst = 1, .src = ptr_reg } });
        }

        try self.emit_argument_words(recv_reg, self.type_word_count(recv_type), dst_start);
        _ = try self.emit_argument_values(arg_regs, arg_types, dst_start + self.type_word_count(recv_type));

        const target = try self.b.ensure_instance(resolved.info, resolved.bindings);
        try self.emit(.{ .call = .{ .target = target } });

        if (sret_ptr) |ptr| if (self.is_temp(ptr)) self.free_temp(ptr);

        if (result_base) |result_reg| return result_reg;
        const reg = try self.save_result_reg(0);
        return @as(?u8, reg);
    }

    fn compile_trait_method_call_regs(
        self: *function_ctx,
        trait_name: []const u8,
        method_name: []const u8,
        receiver_reg: u8,
        args: []const u8,
        arg_types: []const type_key,
        recv_concrete: ?type_key,
    ) lower_error!u8 {
        const method_idx = self.trait_method_index(trait_name, method_name) orelse return error.unknown_function;
        var return_type = self.trait_method_return_type(trait_name, method_name);
        if (return_type == .name and self.is_unbound_type_name(return_type.name)) {
            if (recv_concrete) |concrete| {
                if (concrete == .unknown) return error.unsupported_node;
                return_type = concrete;
            } else {
                return error.unsupported_node;
            }
        }
        const return_words = self.type_word_count(return_type);

        var recv_reg = receiver_reg;
        var recv_copy: ?u8 = null;
        if (recv_reg == 0) {
            const tmp = try self.alloc_temp_words(2);
            try self.copy_words(tmp, recv_reg, 2);
            recv_reg = tmp;
            recv_copy = tmp;
        }

        const vtable_reg = recv_reg;
        const data_reg = recv_reg + 1;

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
        if (self.is_temp(fn_reg)) self.pin_temp(fn_reg);

        var result_base: ?u8 = null;
        var sret_ptr: ?u8 = null;
        var dst_start: u8 = 1;
        if (return_words > 1) {
            const base_reg = try self.alloc_temp_words(return_words);
            const ptr_reg = try self.ptr_of_reg(base_reg);
            result_base = base_reg;
            sret_ptr = ptr_reg;
            dst_start = 2;
            try self.emit(.{ .argument_set = .{ .dst = 1, .src = ptr_reg } });
        }

        var recv_words: u8 = 1;
        if (recv_concrete) |concrete| {
            recv_words = self.type_word_count(concrete);
        }
        const recv_arg_reg = try self.alloc_temp_words(recv_words);
        try self.load_words_from_ptr(data_reg, recv_arg_reg, recv_words);
        const recv_temp_base: ?u8 = recv_arg_reg;

        try self.emit_argument_words(recv_arg_reg, recv_words, dst_start);
        _ = try self.emit_argument_values(args, arg_types, dst_start + recv_words);

        if (self.is_temp(fn_reg)) self.unpin_temp(fn_reg);

        try self.emit(.{ .call_register = .{ .src = fn_reg, .dst = 0 } });

        if (self.is_temp(fn_reg)) self.free_temp(fn_reg);
        if (recv_copy) |tmp| self.free_temp_words(tmp, 2);
        if (recv_temp_base) |base| self.free_temp_words(base, recv_words);

        if (sret_ptr) |ptr| if (self.is_temp(ptr)) self.free_temp(ptr);

        if (result_base) |result_reg| return result_reg;
        return self.save_result_reg(0);
    }

    fn compile_overloadable_unary(
        self: *function_ctx,
        op: ink.unary,
        right_id: mir_mod.mir_identifier,
        node_id: mir_mod.mir_identifier,
    ) lower_error!u8 {
        const method = unary_operator_method_name(op) orelse return error.unsupported_node;
        const right_type = self.infer_expr_type(right_id);
        if (self.trait_name_from_type(right_type) != null or self.is_known_non_builtin(right_type)) {
            const maybe_reg = self.try_compile_method_call(method, right_id, &.{}) catch |err| switch (err) {
                error.unknown_function => {
                    self.record_unary_operator_error(node_id, op, right_type);
                    return err;
                },
                error.ambiguous_overload => {
                    self.record_unary_operator_ambiguous(node_id, op, right_type);
                    return err;
                },
                else => return err,
            };
            if (maybe_reg) |reg| {
                return reg;
            }
            if (self.is_known_non_builtin(right_type)) {
                self.record_unary_operator_error(node_id, op, right_type);
                return error.unknown_function;
            }
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
        left_id: mir_mod.mir_identifier,
        right_id: mir_mod.mir_identifier,
        node_id: mir_mod.mir_identifier,
    ) lower_error!u8 {
        const method = binary_operator_method_name(op) orelse return error.unsupported_node;
        const left_type = self.infer_expr_type(left_id);
        const right_type = self.infer_expr_type(right_id);

        if (self.trait_name_from_type(left_type) != null or self.is_known_non_builtin(left_type)) {
            const maybe_reg = self.try_compile_method_call(method, left_id, &.{right_id}) catch |err| switch (err) {
                error.unknown_function => {
                    self.record_binary_operator_error(node_id, op, left_type, right_type);
                    return err;
                },
                error.ambiguous_overload => {
                    self.record_binary_operator_ambiguous(node_id, op, left_type, right_type);
                    return err;
                },
                else => return err,
            };
            if (maybe_reg) |reg| {
                return reg;
            }
            if (self.is_known_non_builtin(left_type)) {
                self.record_binary_operator_error(node_id, op, left_type, right_type);
                return error.unknown_function;
            }
        }

        return self.compile_builtin_binary(op, left_id, right_id, left_type, right_type);
    }

    fn compile_builtin_binary(
        self: *function_ctx,
        op: ink.binary,
        left_id: mir_mod.mir_identifier,
        right_id: mir_mod.mir_identifier,
        left_type: type_key,
        right_type: type_key,
    ) lower_error!u8 {
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

    fn emit_bool_not(self: *function_ctx, value_id: mir_mod.mir_identifier) lower_error!u8 {
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
        left_id: mir_mod.mir_identifier,
        right_id: mir_mod.mir_identifier,
    ) lower_error!u8 {
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
        left_id: mir_mod.mir_identifier,
        right_id: mir_mod.mir_identifier,
    ) lower_error!u8 {
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
        left_id: mir_mod.mir_identifier,
        right_id: mir_mod.mir_identifier,
    ) lower_error!u8 {
        return self.emit_int_binary(left_id, right_id, .bit_xor);
    }

    fn compile_assign_expr(
        self: *function_ctx,
        op: ink.binary,
        left_id: mir_mod.mir_identifier,
        right_id: mir_mod.mir_identifier,
        node_id: mir_mod.mir_identifier,
    ) lower_error!u8 {
        if (op == .assign) {
            return self.compile_simple_assign(left_id, right_id);
        }
        const base_op = assignment_base_op(op) orelse return error.unsupported_node;
        const value_reg = try self.compile_overloadable_binary(base_op, left_id, right_id, node_id);
        const value_type = self.infer_expr_type(left_id);
        return self.store_assignment_value(left_id, value_reg, value_type);
    }

    fn compile_simple_assign(
        self: *function_ctx,
        left_id: mir_mod.mir_identifier,
        right_id: mir_mod.mir_identifier,
    ) lower_error!u8 {
        const value_reg = try self.compile_expr(right_id);
        const value_type = self.infer_expr_type(right_id);
        return self.store_assignment_value(left_id, value_reg, value_type);
    }

    fn store_assignment_value(
        self: *function_ctx,
        left_id: mir_mod.mir_identifier,
        value_reg: u8,
        value_type: type_key,
    ) lower_error!u8 {
        const left_type = self.infer_expr_type(left_id);
        if (atomic_inner_type_key(left_type)) |inner_type| {
            return self.store_atomic_assignment(left_id, left_type, inner_type, value_reg, value_type);
        }

        const left_node = self.b.node(left_id);
        switch (left_node) {
            .identifier => |ident| {
                const name = self.b.string_value(ident);
                const local_reg = self.locals.get(name) orelse return error.unknown_identifier;
                const local_type = self.local_types.get(name) orelse value_type;
                const value_words = self.type_word_count(local_type);
                if (value_words <= 1) {
                    if (local_reg != value_reg) {
                        try self.emit(.{ .move = .{ .dst = local_reg, .src = value_reg } });
                    }
                    if (self.is_temp(value_reg) and local_reg != value_reg) {
                        self.free_temp(value_reg);
                    }
                } else {
                    try self.copy_words(local_reg, value_reg, value_words);
                    if (self.is_temp(value_reg)) {
                        self.free_temp_words(value_reg, value_words);
                    }
                }
                return local_reg;
            },
            .binary => |bin| switch (bin.op) {
                .access => {
                    const value_words = self.type_word_count(value_type);
                    const ptr_reg = try self.compile_access_ptr(bin.left, bin.right);
                    try self.store_words_to_ptr(ptr_reg, value_reg, value_words);
                    if (self.is_temp(ptr_reg)) self.free_temp(ptr_reg);
                    return value_reg;
                },
                .index => {
                    const value_words = self.type_word_count(value_type);
                    const base_type = self.infer_expr_type(bin.left);
                    if (type_key_base_name(base_type)) |name| {
                        if (!std.mem.eql(u8, name, "slice") and !std.mem.eql(u8, name, "array")) {
                            if (try self.store_index_set_value(bin.left, bin.right, value_reg, base_type, value_type, left_id)) |reg| {
                                return reg;
                            }
                            if (self.is_known_non_builtin(base_type)) return error.unknown_function;
                            return error.unsupported_node;
                        }
                    } else if (self.trait_name_from_type(base_type) != null or self.is_known_non_builtin(base_type)) {
                        if (try self.store_index_set_value(bin.left, bin.right, value_reg, base_type, value_type, left_id)) |reg| {
                            return reg;
                        }
                        if (self.is_known_non_builtin(base_type)) return error.unknown_function;
                        return error.unsupported_node;
                    }

                    const ptr_reg = try self.compile_index_ptr(bin.left, bin.right);
                    try self.store_words_to_ptr(ptr_reg, value_reg, value_words);
                    if (self.is_temp(ptr_reg)) self.free_temp(ptr_reg);
                    return value_reg;
                },
                else => return error.unsupported_node,
            },
            .unary => |un| switch (un.op) {
                .deref => {
                    const value_words = self.type_word_count(value_type);
                    const ptr_reg = try self.compile_expr(un.right);
                    try self.store_words_to_ptr(ptr_reg, value_reg, value_words);
                    if (self.is_temp(ptr_reg)) self.free_temp(ptr_reg);
                    return value_reg;
                },
                else => return error.unsupported_node,
            },
            else => return error.unsupported_node,
        }
    }

    fn store_atomic_assignment(
        self: *function_ctx,
        left_id: mir_mod.mir_identifier,
        left_type: type_key,
        inner_type: type_key,
        value_reg: u8,
        value_type: type_key,
    ) lower_error!u8 {
        _ = left_type;
        const left_node = self.b.node(left_id);
        var atomic_reg: u8 = 0;
        var atomic_temp = false;

        switch (left_node) {
            .identifier => |ident| {
                const name = self.b.string_value(ident);
                const local_reg = self.locals.get(name) orelse return error.unknown_identifier;
                atomic_reg = local_reg;
            },
            .binary => |bin| switch (bin.op) {
                .access => {
                    const ptr_reg = try self.compile_access_ptr(bin.left, bin.right);
                    atomic_reg = try self.alloc_temp_words(2);
                    atomic_temp = true;
                    try self.load_words_from_ptr(ptr_reg, atomic_reg, 2);
                    if (self.is_temp(ptr_reg)) self.free_temp(ptr_reg);
                },
                .index => {
                    const ptr_reg = try self.compile_index_ptr(bin.left, bin.right);
                    atomic_reg = try self.alloc_temp_words(2);
                    atomic_temp = true;
                    try self.load_words_from_ptr(ptr_reg, atomic_reg, 2);
                    if (self.is_temp(ptr_reg)) self.free_temp(ptr_reg);
                },
                else => return error.unsupported_node,
            },
            .unary => |un| switch (un.op) {
                .deref => {
                    var ptr_reg = try self.compile_expr(un.right);
                    if (ptr_reg == 0) ptr_reg = try self.save_result_reg(ptr_reg);
                    atomic_reg = try self.alloc_temp_words(2);
                    atomic_temp = true;
                    try self.load_words_from_ptr(ptr_reg, atomic_reg, 2);
                    if (self.is_temp(ptr_reg)) self.free_temp(ptr_reg);
                },
                else => return error.unsupported_node,
            },
            else => return error.unsupported_node,
        }

        var inner_reg = value_reg;
        var stored_inner_type = inner_type;
        const inner_words = self.type_word_count(inner_type);
        if (atomic_inner_type_key(value_type)) |rhs_inner| {
            stored_inner_type = rhs_inner;
            inner_reg = try self.compile_atomic_load_reg(value_reg, rhs_inner);
            if (self.is_temp(value_reg)) self.free_temp_words(value_reg, 2);
        }

        try self.emit_atomic_lock();
        try self.store_words_to_ptr(atomic_reg, inner_reg, inner_words);
        try self.emit_atomic_unlock();

        if (atomic_inner_type_key(value_type) != null) {
            self.free_temp_value(inner_reg, stored_inner_type);
        } else if (self.is_temp(inner_reg)) {
            self.free_temp_value(inner_reg, value_type);
        }

        return atomic_reg;
    }

    fn store_index_set_value(
        self: *function_ctx,
        base_id: mir_mod.mir_identifier,
        index_id: mir_mod.mir_identifier,
        value_reg: u8,
        base_type: type_key,
        value_type: type_key,
        call_id: mir_mod.mir_identifier,
    ) lower_error!?u8 {
        var base_reg = try self.compile_expr(base_id);
        if (base_reg == 0) base_reg = try self.save_result_reg(base_reg);
        var index_reg = try self.compile_expr(index_id);
        if (index_reg == 0) index_reg = try self.save_result_reg(index_reg);
        var val_reg = value_reg;
        if (val_reg == 0) val_reg = try self.save_result_reg(val_reg);

        const index_type = self.infer_expr_type(index_id);
        const arg_regs = [_]u8{ index_reg, val_reg };
        const arg_types = [_]type_key{ index_type, value_type };
        const result = self.try_compile_method_call_regs(
            "index_set",
            base_reg,
            base_type,
            self.dyn_concrete_type(base_id),
            arg_regs[0..],
            arg_types[0..],
        ) catch |err| switch (err) {
            error.unknown_function => {
                self.record_call_error(call_id, "unknown method ", "index_set", &.{ base_type, index_type, value_type }, true);
                return err;
            },
            else => return err,
        };

        if (result) |reg| {
            if (self.is_temp(reg)) self.free_temp(reg);
        }

        if (base_reg != val_reg and base_reg != index_reg) {
            self.free_temp_value(base_reg, base_type);
        }
        if (index_reg != val_reg and index_reg != base_reg) {
            self.free_temp_value(index_reg, index_type);
        }
        if (val_reg != value_reg) {
            self.free_temp_value(val_reg, value_type);
        }

        if (result != null) return value_reg;
        if (self.is_known_non_builtin(base_type)) {
            self.record_call_error(call_id, "no matching overload for ", "index_set", &.{ base_type, index_type, value_type }, true);
        }
        return null;
    }

    fn compile_local_decl(
        self: *function_ctx,
        name_id: mir_mod.string_identifier,
        ty: ?mir_mod.mir_identifier,
        value_id: mir_mod.mir_identifier,
    ) lower_error!u8 {
        const name = self.b.string_value(name_id);
        const value_type = if (ty) |ty_id| type_key_from_type_node(self.b, ty_id) else self.infer_expr_type(value_id);
        const value_words = self.type_word_count(value_type);
        const existing = self.locals.get(name);
        const local_reg = if (existing) |reg| reg else blk: {
            const reg = try self.alloc_local_words(value_words);
            self.locals.put(name, reg) catch return error.out_of_memory;
            break :blk reg;
        };

        if (existing != null) {
            if (self.local_types.get(name)) |known| {
                const known_words = self.type_word_count(known);
                if (known_words != value_words) return error.unsupported_node;
            }
        }

        const value_reg = try self.compile_expr(value_id);
        if (value_words <= 1) {
            if (local_reg != value_reg) {
                try self.emit(.{ .move = .{ .dst = local_reg, .src = value_reg } });
            }
            if (self.is_temp(value_reg) and value_reg != local_reg) self.free_temp(value_reg);
        } else {
            try self.copy_words(local_reg, value_reg, value_words);
            if (self.is_temp(value_reg)) self.free_temp_words(value_reg, value_words);
        }

        if (value_type != .unknown) {
            self.local_types.put(name, value_type) catch return error.out_of_memory;
        }
        if (self.dyn_concrete_type(value_id)) |concrete| {
            self.local_dyn_types.put(name, concrete) catch return error.out_of_memory;
        } else {
            _ = self.local_dyn_types.remove(name);
        }
        return local_reg;
    }

    fn infer_call_type(self: *function_ctx, id: mir_mod.mir_identifier) type_key {
        var base_id = id;
        var args_buf: [7]mir_mod.mir_identifier = undefined;
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
        var arg_ids_buf: [8]mir_mod.mir_identifier = undefined;
        if (base.receiver) |recv| {
            if (arg_count + 1 > arg_ids_buf.len) return .unknown;
            arg_ids_buf[0] = recv;
            std.mem.copyForwards(mir_mod.mir_identifier, arg_ids_buf[1 .. arg_count + 1], arg_ids);
            arg_ids = arg_ids_buf[0 .. arg_count + 1];
            arg_count += 1;
        }

        const name = base.name;
        if (is_print_name(name)) return .{ .name = "unit" };
        if (is_cancel_name(name)) return .{ .name = "unit" };

        if (base.receiver) |recv| {
            const recv_type = self.infer_expr_type(recv);
            if (self.trait_name_from_type(recv_type)) |type_name| {
                const ret = self.trait_method_return_type(type_name, name);
                if (ret == .name and self.is_unbound_type_name(ret.name)) {
                    if (self.dyn_concrete_type(recv)) |concrete| return concrete;
                }
                return ret;
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
    ) lower_error!overload_result {
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

    fn is_pack_generic(self: *function_ctx, decl: mir_mod.mir.function_decl, name: []const u8) bool {
        for (decl.generics) |param| {
            if (!param.is_pack) continue;
            if (std.mem.eql(u8, self.b.string_value(param.name), name)) return true;
        }
        return false;
    }

    fn constraint_satisfied(self: *function_ctx, ty: type_key, constraint_id: mir_mod.mir_identifier) bool {
        if (ty == .unknown) return true;
        const constraint = constraint_from_type_node(self.b, constraint_id) orelse return true;
        if (!self.b.traits.contains(constraint.name)) return true;

        if (dyn_base_from_type_key(ty) != null) {
            if (constraint.negative) {
                if (dyn_has_positive(ty, constraint.name)) return false;
                if (dyn_has_negative(ty, constraint.name)) return true;
                return true;
            }
            return dyn_has_positive(ty, constraint.name);
        }

        const ok = type_satisfies_trait_root(self.b, ty, constraint.name);
        return if (constraint.negative) !ok else ok;
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
        var unsized_allowed = std.StringHashMapUnmanaged(void){};
        defer unsized_allowed.deinit(self.b.allocator);

        for (decl.generics) |param| {
            if (param.kind != .type) continue;
            const param_name = self.b.string_value(param.name);
            if (param.constraint == null) continue;
            const constraint_id = param.constraint.?;
            if (is_unsized_marker(self.b, constraint_id)) {
                unsized_allowed.put(self.b.allocator, param_name, {}) catch {};
                continue;
            }
            if (constraint_from_type_node(self.b, constraint_id)) |constraint| {
                if (constraint.negative and std.mem.eql(u8, constraint.name, "sized")) {
                    unsized_allowed.put(self.b.allocator, param_name, {}) catch {};
                }
            }
            if (param.is_pack) {
                if (!is_variadic) continue;
                const pack_param = decl.params[fixed_len];
                const pack_type = type_key_from_type_node_with_self(self.b, pack_param.ty, info.impl_for);
                if (pack_type == .unknown) continue;
                const pack_name = type_key_base_name(pack_type) orelse continue;
                if (!std.mem.eql(u8, pack_name, self.b.string_value(param.name))) continue;
                for (arg_types[fixed_len..]) |arg_type| {
                    const ok = self.constraint_satisfied(arg_type, constraint_id);
                    if (!ok) return false;
                }
                continue;
            }
            const binding = lookup_generic(bindings, param_name) orelse continue;
            const ok = self.constraint_satisfied(binding, constraint_id);
            if (!ok) return false;
        }

        for (decl.where_clause) |req| {
            const req_name = self.b.string_value(req.name);
            const binding = lookup_generic(bindings, req_name) orelse continue;
            if (is_unsized_marker(self.b, req.constraint)) {
                unsized_allowed.put(self.b.allocator, req_name, {}) catch {};
                continue;
            }
            if (constraint_from_type_node(self.b, req.constraint)) |constraint| {
                if (constraint.negative and std.mem.eql(u8, constraint.name, "sized")) {
                    unsized_allowed.put(self.b.allocator, req_name, {}) catch {};
                }
            }
            const ok = self.constraint_satisfied(binding, req.constraint);
            if (!ok) return false;
        }

        for (decl.generics) |param| {
            if (param.kind != .type) continue;
            const param_name = self.b.string_value(param.name);
            if (unsized_allowed.contains(param_name)) continue;
            if (param.is_pack) {
                if (!is_variadic) continue;
                for (arg_types[fixed_len..]) |arg_type| {
                    if (!type_satisfies_trait_root(self.b, arg_type, "sized")) return false;
                }
                continue;
            }
            const binding = lookup_generic(bindings, param_name) orelse continue;
            if (!type_satisfies_trait_root(self.b, binding, "sized")) return false;
        }

        return true;
    }

    fn resolve_function_overload_index(
        self: *function_ctx,
        overloads: []const function_info,
        arg_types: []const type_key,
    ) lower_error!usize {
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
    ) lower_error!usize {
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

    fn emit(self: *function_ctx, inst: ink.exe.instruction) lower_error!void {
        try self.b.emit(inst);
    }

    fn prep_binary(self: *function_ctx, left_id: mir_mod.mir_identifier, right_id: mir_mod.mir_identifier) lower_error!binary_prep {
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

    fn prep_unary(self: *function_ctx, arg_id: mir_mod.mir_identifier) lower_error!unary_prep {
        const src = try self.compile_expr(arg_id);
        const dst = if (self.is_temp(src)) src else try self.alloc_temp();
        return .{ .src = src, .dst = dst, .src_temp = self.is_temp(src) };
    }

    fn finish_unary(self: *function_ctx, prep: unary_prep) void {
        if (prep.src_temp and prep.dst != prep.src) self.free_temp(prep.src);
    }

    const call_base = struct {
        name: []const u8,
        receiver: ?mir_mod.mir_identifier,
    };

    fn resolve_call_base(self: *function_ctx, base_id: mir_mod.mir_identifier) lower_error!call_base {
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

    fn record_call_error(
        self: *function_ctx,
        node: mir_mod.mir_identifier,
        prefix: []const u8,
        name: []const u8,
        arg_types: []const type_key,
        receiver: bool,
    ) void {
        var buf = std.array_list.Managed(u8).init(self.b.allocator);
        defer buf.deinit();
        buf.appendSlice(prefix) catch return;
        append_call_signature(&buf, name, arg_types, receiver) catch return;
        self.b.set_error_message(node, buf.items);
    }

    fn record_binary_operator_error(
        self: *function_ctx,
        node: mir_mod.mir_identifier,
        op: ink.binary,
        left_type: type_key,
        right_type: type_key,
    ) void {
        var buf = std.array_list.Managed(u8).init(self.b.allocator);
        defer buf.deinit();
        buf.appendSlice("operator ") catch return;
        buf.appendSlice(binary_operator_symbol(op)) catch return;
        buf.appendSlice(" not supported for ") catch return;
        type_key_append(&buf, left_type) catch return;
        buf.appendSlice(" and ") catch return;
        type_key_append(&buf, right_type) catch return;
        self.b.set_error_message(node, buf.items);
    }

    fn record_unary_operator_error(
        self: *function_ctx,
        node: mir_mod.mir_identifier,
        op: ink.unary,
        arg_type: type_key,
    ) void {
        var buf = std.array_list.Managed(u8).init(self.b.allocator);
        defer buf.deinit();
        buf.appendSlice("operator ") catch return;
        buf.appendSlice(unary_operator_symbol(op)) catch return;
        buf.appendSlice(" not supported for ") catch return;
        type_key_append(&buf, arg_type) catch return;
        self.b.set_error_message(node, buf.items);
    }

    fn record_binary_operator_ambiguous(
        self: *function_ctx,
        node: mir_mod.mir_identifier,
        op: ink.binary,
        left_type: type_key,
        right_type: type_key,
    ) void {
        var buf = std.array_list.Managed(u8).init(self.b.allocator);
        defer buf.deinit();
        buf.appendSlice("ambiguous overload for operator ") catch return;
        buf.appendSlice(binary_operator_symbol(op)) catch return;
        buf.appendSlice(" with ") catch return;
        type_key_append(&buf, left_type) catch return;
        buf.appendSlice(" and ") catch return;
        type_key_append(&buf, right_type) catch return;
        self.b.set_error_message(node, buf.items);
    }

    fn record_unary_operator_ambiguous(
        self: *function_ctx,
        node: mir_mod.mir_identifier,
        op: ink.unary,
        arg_type: type_key,
    ) void {
        var buf = std.array_list.Managed(u8).init(self.b.allocator);
        defer buf.deinit();
        buf.appendSlice("ambiguous overload for operator ") catch return;
        buf.appendSlice(unary_operator_symbol(op)) catch return;
        buf.appendSlice(" with ") catch return;
        type_key_append(&buf, arg_type) catch return;
        self.b.set_error_message(node, buf.items);
    }

    fn foreign_index(self: *function_ctx, name: []const u8) lower_error!u32 {
        return self.b.foreigns.get(name) orelse error.unknown_foreign;
    }

    fn emit_foreign_call(self: *function_ctx, foreign_idx: u32, args: []const u8) lower_error!void {
        if (args.len > max_register) return error.register_overflow;
        for (args, 0..) |arg_reg, idx| {
            const dst: u8 = @intCast(idx + 1);
            try self.emit(.{ .argument_set = .{ .dst = dst, .src = arg_reg } });
        }
        try self.emit(.{ .call_foreign = .{ .index = foreign_idx } });
    }

    fn emit_argument_words(self: *function_ctx, src_base: u8, words: u8, dst_start: u8) lower_error!void {
        var idx: u8 = 0;
        while (idx < words) : (idx += 1) {
            const dst: u8 = dst_start + idx;
            if (dst > max_register) return error.register_overflow;
            try self.emit(.{ .argument_set = .{ .dst = dst, .src = src_base + idx } });
        }
    }

    fn emit_argument_values(
        self: *function_ctx,
        arg_regs: []const u8,
        arg_types: []const type_key,
        dst_start: u8,
    ) lower_error!u8 {
        var dst = dst_start;
        for (arg_regs, 0..) |arg_reg, idx| {
            const words = self.type_word_count(arg_types[idx]);
            try self.emit_argument_words(arg_reg, words, dst);
            dst +|= words;
        }
        return dst;
    }

    fn is_unit(self: *function_ctx, id: mir_mod.mir_identifier) bool {
        const node = self.b.node(id);
        return switch (node) {
            .identifier => |ident| std.mem.eql(u8, self.b.string_value(ident), "unit"),
            else => false,
        };
    }

    fn compile_global_const(
        self: *function_ctx,
        name: []const u8,
        info: global_const,
        node_id: mir_mod.mir_identifier,
    ) lower_error!u8 {
        if (self.global_const_stack.contains(name)) {
            self.b.set_error_fmt(node_id, "recursive const reference {s}", .{name});
            return error.unsupported_node;
        }
        self.global_const_stack.put(name, 1) catch return error.out_of_memory;
        defer _ = self.global_const_stack.remove(name);
        return self.compile_expr(info.value);
    }

    fn infer_global_const_type(self: *function_ctx, name: []const u8, info: global_const) type_key {
        if (self.global_const_stack.contains(name)) return .unknown;
        self.global_const_stack.put(name, 1) catch return .unknown;
        defer _ = self.global_const_stack.remove(name);
        if (info.ty) |ty_id| {
            return type_key_from_type_node(self.b, ty_id);
        }
        return self.infer_expr_type(info.value);
    }

    fn trait_name_from_type(self: *function_ctx, ty: type_key) ?[]const u8 {
        return switch (ty) {
            .dyn_trait => |name| name,
            .name => |name| if (self.b.traits.contains(name)) name else null,
            .applied => |ap| blk: {
                if (std.mem.eql(u8, ap.base, "dyn") and ap.args.len >= 1) {
                    const base_name = type_key_base_name(ap.args[0]) orelse break :blk null;
                    if (self.b.traits.contains(base_name)) break :blk base_name;
                }
                if (self.b.traits.contains(ap.base)) break :blk ap.base;
                break :blk null;
            },
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

    fn compile_expr(self: *function_ctx, id: mir_mod.mir_identifier) lower_error!u8 {
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
            .duration => |value| blk: {
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
                if (macro_enum_value(name)) |value| {
                    const idx = try self.b.intern_const(value);
                    const reg = try self.alloc_temp();
                    try self.emit(.{ .load_const = .{ .dst = reg, .const_index = idx } });
                    break :blk reg;
                }
                if (self.b.global_consts.get(name)) |info| {
                    break :blk try self.compile_global_const(name, info, id);
                }
                self.b.set_error_fmt(id, "unknown identifier {s}", .{name});
                return error.unknown_identifier;
            },
        .unary => |un| switch (un.op) {
            .ret => {
                const reg = try self.compile_expr(un.right);
                try self.emit_return_value(reg, self.infer_expr_type(un.right));
                return reg;
            },
            .dynamic, .@"comptime" => return self.compile_expr(un.right),
            .box => return self.compile_box_expr(un.right),
            .sleep => return self.compile_sleep_expr(un.right),
            .timeout => return self.compile_timeout_expr(un.right),
            .deadline => return self.compile_deadline_expr(un.right),
            .spawn => return self.compile_spawn_expr(un.right),
            .await => return self.compile_await_expr(un.right),
            .@"try" => return self.compile_try_expr(un.right),
            .unwrap_optional => return self.compile_optional_unwrap(un.right),
            .borrow, .borrow_mut => return self.compile_borrow_expr(un.right),
            .deref => blk: {
                if (atomic_inner_type_key(self.infer_expr_type(un.right))) |inner| {
                    break :blk self.compile_atomic_deref(un.right, inner);
                }
                break :blk self.compile_deref_expr(un.right, id);
            },
            .neg, .not, .bit_not => blk: {
                if (atomic_inner_type_key(self.infer_expr_type(un.right))) |_| {
                    break :blk self.compile_atomic_unary(un.op, un.right);
                }
                break :blk self.compile_overloadable_unary(un.op, un.right, id);
            },
            else => return error.unsupported_node,
            },
        .binary => |bin| switch (bin.op) {
            .call => try self.compile_call(id),
            .access => try self.compile_access(bin.left, bin.right),
            .index => try self.compile_index_expr(bin.left, bin.right, id),
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
            => try self.compile_assign_expr(bin.op, bin.left, bin.right, id),
            .logical_and => blk: {
                const left_type = self.infer_expr_type(bin.left);
                const right_type = self.infer_expr_type(bin.right);
                if (atomic_inner_type_key(left_type) != null or atomic_inner_type_key(right_type) != null) {
                    break :blk try self.compile_atomic_binary(bin.op, bin.left, bin.right, id);
                }
                break :blk try self.compile_logical_and(bin.left, bin.right);
            },
            .logical_or => blk: {
                const left_type = self.infer_expr_type(bin.left);
                const right_type = self.infer_expr_type(bin.right);
                if (atomic_inner_type_key(left_type) != null or atomic_inner_type_key(right_type) != null) {
                    break :blk try self.compile_atomic_binary(bin.op, bin.left, bin.right, id);
                }
                break :blk try self.compile_logical_or(bin.left, bin.right);
            },
            .logical_xor => blk: {
                const left_type = self.infer_expr_type(bin.left);
                const right_type = self.infer_expr_type(bin.right);
                if (atomic_inner_type_key(left_type) != null or atomic_inner_type_key(right_type) != null) {
                    break :blk try self.compile_atomic_binary(bin.op, bin.left, bin.right, id);
                }
                break :blk try self.compile_logical_xor(bin.left, bin.right);
            },
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
            => blk: {
                const left_type = self.infer_expr_type(bin.left);
                const right_type = self.infer_expr_type(bin.right);
                if (atomic_inner_type_key(left_type) != null or atomic_inner_type_key(right_type) != null) {
                    break :blk try self.compile_atomic_binary(bin.op, bin.left, bin.right, id);
                }
                break :blk try self.compile_overloadable_binary(bin.op, bin.left, bin.right, id);
            },
            else => return error.unsupported_node,
        },
            .intrinsic => |call| return self.compile_intrinsic(call),
            .record_literal => |rec| return self.compile_record_literal(rec),
            .label_expr => |le| return self.compile_label_expr(le.name, le.body),
            .loop_expr => |le| return self.compile_loop_expr(le.body, id, null),
            .while_expr => |we| return self.compile_while_expr(we.condition, we.body, id, null),
            .while_in_expr => |we| return self.compile_for_in_expr(we.pattern, we.iter, we.body, id, null),
            .until_expr => |ue| return self.compile_until_expr(ue.condition, ue.body, id, null),
            .repeat_expr => |re| return self.compile_repeat_expr(re.count, re.body, id, null),
            .for_expr => |fe| return self.compile_for_in_expr(fe.pattern, fe.iter, fe.body, id, null),
            .each_expr => |ee| return self.compile_for_in_expr(ee.pattern, ee.iter, ee.body, id, null),
            .break_expr => |be| return self.compile_break_expr(be.label, be.value),
            .continue_expr => |ce| return self.compile_continue_expr(ce.label),
            .yield_expr => |ye| return self.compile_yield_expr(ye.value),
            .atomic_expr => |ae| return self.compile_atomic_expr(ae.value, ae.ordering),
            .block => |items| {
                const count = items.len;
                var idx: usize = 0;
                var last_reg: u8 = 0;
                while (idx < count) : (idx += 1) {
                    const reg = try self.compile_expr(items[idx]);
                    if (idx + 1 == count) {
                        last_reg = reg;
                    } else {
                        self.free_temp_value(reg, self.infer_expr_type(items[idx]));
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

                const result_type = self.infer_expr_type(id);
                const result_words = self.type_word_count(result_type);
                const dst = try self.alloc_temp_words(result_words);
                try self.zero_words(dst, result_words);
                const then_reg = try self.compile_expr(ife.then_branch);
                if (result_words <= 1) {
                    if (then_reg != dst) {
                        try self.emit(.{ .move = .{ .dst = dst, .src = then_reg } });
                    }
                    self.free_temp_value(then_reg, self.infer_expr_type(ife.then_branch));
                } else {
                    try self.copy_words(dst, then_reg, result_words);
                    self.free_temp_value(then_reg, self.infer_expr_type(ife.then_branch));
                }
                try self.emit(.{ .jump = .{ .target = end_label } });

                try self.emit(.{ .label = .{ .id = else_label } });
                if (ife.else_branch) |else_ref| {
                    const else_reg = try self.compile_expr(else_ref);
                    if (result_words <= 1) {
                        if (else_reg != dst) {
                            try self.emit(.{ .move = .{ .dst = dst, .src = else_reg } });
                        }
                        self.free_temp_value(else_reg, self.infer_expr_type(else_ref));
                    } else {
                        try self.copy_words(dst, else_reg, result_words);
                        self.free_temp_value(else_reg, self.infer_expr_type(else_ref));
                    }
                }

                try self.emit(.{ .label = .{ .id = end_label } });
                break :blk dst;
            },
            .select_expr => |se| return self.compile_select_expr(id, se.arms),
            else => return error.unsupported_node,
        };
    }

    fn compile_sleep_expr(self: *function_ctx, arg_id: mir_mod.mir_identifier) lower_error!u8 {
        const arg_type = self.infer_expr_type(arg_id);
        if (type_key_base_name(arg_type)) |name| {
            if (std.mem.eql(u8, name, "deadline") or std.mem.eql(u8, name, "instant")) {
                _ = try self.emit_foreign_call_named("std::sleep_until", &[_]mir_mod.mir_identifier{arg_id});
                return self.save_result_reg(0);
            }
        }
        _ = try self.emit_foreign_call_named("std::sleep", &[_]mir_mod.mir_identifier{arg_id});
        return self.save_result_reg(0);
    }

    fn compile_timeout_expr(self: *function_ctx, arg_id: mir_mod.mir_identifier) lower_error!u8 {
        _ = try self.emit_foreign_call_named("std::timeout", &[_]mir_mod.mir_identifier{arg_id});
        return self.save_result_reg(0);
    }

    fn compile_deadline_expr(self: *function_ctx, arg_id: mir_mod.mir_identifier) lower_error!u8 {
        const arg_type = self.infer_expr_type(arg_id);
        if (type_key_base_name(arg_type)) |name| {
            if (std.mem.eql(u8, name, "deadline")) {
                return self.compile_expr(arg_id);
            }
            if (std.mem.eql(u8, name, "duration")) {
                _ = try self.emit_foreign_call_named("std::timeout", &[_]mir_mod.mir_identifier{arg_id});
                return self.save_result_reg(0);
            }
            if (std.mem.eql(u8, name, "instant")) {
                _ = try self.emit_foreign_call_named("std::deadline", &[_]mir_mod.mir_identifier{arg_id});
                return self.save_result_reg(0);
            }
        }
        _ = try self.emit_foreign_call_named("std::deadline", &[_]mir_mod.mir_identifier{arg_id});
        return self.save_result_reg(0);
    }

    fn compile_yield_expr(self: *function_ctx, value: ?mir_mod.mir_identifier) lower_error!u8 {
        if (value) |ref| {
            const reg = try self.compile_expr(ref);
            self.free_temp_value(reg, self.infer_expr_type(ref));
        }
        try self.emit_foreign_call_void("std::yield");
        return try self.load_const_reg(0);
    }

    fn compile_label_expr(self: *function_ctx, name_id: mir_mod.string_identifier, body_id: mir_mod.mir_identifier) lower_error!u8 {
        const label_name = self.b.string_value(name_id);
        const body_node = self.b.node(body_id);
        return switch (body_node) {
            .loop_expr => |le| self.compile_loop_expr(le.body, body_id, label_name),
            .while_expr => |we| self.compile_while_expr(we.condition, we.body, body_id, label_name),
            .while_in_expr => |we| self.compile_for_in_expr(we.pattern, we.iter, we.body, body_id, label_name),
            .until_expr => |ue| self.compile_until_expr(ue.condition, ue.body, body_id, label_name),
            .repeat_expr => |re| self.compile_repeat_expr(re.count, re.body, body_id, label_name),
            .for_expr => |fe| self.compile_for_in_expr(fe.pattern, fe.iter, fe.body, body_id, label_name),
            .each_expr => |ee| self.compile_for_in_expr(ee.pattern, ee.iter, ee.body, body_id, label_name),
            else => self.compile_label_block(label_name, body_id),
        };
    }

    fn compile_label_block(self: *function_ctx, label_name: []const u8, body_id: mir_mod.mir_identifier) lower_error!u8 {
        const result_type = self.infer_expr_type(body_id);
        const result_words = self.type_word_count(result_type);
        const result_reg = try self.alloc_temp_words(result_words);
        var i: u8 = 0;
        while (i < result_words) : (i += 1) self.pin_temp(result_reg + i);
        try self.zero_words(result_reg, result_words);

        const break_label = self.b.new_label();
        try self.push_loop(label_name, break_label, null, result_reg, result_words);

        const body_reg = try self.compile_expr(body_id);
        if (result_words <= 1) {
            if (body_reg != result_reg) {
                try self.emit(.{ .move = .{ .dst = result_reg, .src = body_reg } });
            }
        } else {
            try self.copy_words(result_reg, body_reg, result_words);
        }
        self.free_temp_value(body_reg, result_type);

        try self.emit(.{ .label = .{ .id = break_label } });
        self.pop_loop();
        i = 0;
        while (i < result_words) : (i += 1) self.unpin_temp(result_reg + i);
        return result_reg;
    }

    fn compile_loop_expr(
        self: *function_ctx,
        body_id: mir_mod.mir_identifier,
        loop_id: mir_mod.mir_identifier,
        label_name: ?[]const u8,
    ) lower_error!u8 {
        const result_type = self.infer_expr_type(loop_id);
        const result_words = self.type_word_count(result_type);
        const result_reg = try self.alloc_temp_words(result_words);
        var i: u8 = 0;
        while (i < result_words) : (i += 1) self.pin_temp(result_reg + i);
        try self.zero_words(result_reg, result_words);

        const start_label = self.b.new_label();
        const break_label = self.b.new_label();
        try self.push_loop(label_name, break_label, start_label, result_reg, result_words);

        try self.emit(.{ .label = .{ .id = start_label } });
        const body_reg = try self.compile_expr(body_id);
        self.free_temp_value(body_reg, self.infer_expr_type(body_id));
        if (!self.returned) {
            try self.emit(.{ .jump = .{ .target = start_label } });
        }
        try self.emit(.{ .label = .{ .id = break_label } });
        self.pop_loop();
        i = 0;
        while (i < result_words) : (i += 1) self.unpin_temp(result_reg + i);
        return result_reg;
    }

    fn compile_while_expr(
        self: *function_ctx,
        condition_id: mir_mod.mir_identifier,
        body_id: mir_mod.mir_identifier,
        loop_id: mir_mod.mir_identifier,
        label_name: ?[]const u8,
    ) lower_error!u8 {
        const result_type = self.infer_expr_type(loop_id);
        const result_words = self.type_word_count(result_type);
        const result_reg = try self.alloc_temp_words(result_words);
        var i: u8 = 0;
        while (i < result_words) : (i += 1) self.pin_temp(result_reg + i);
        try self.zero_words(result_reg, result_words);

        const start_label = self.b.new_label();
        const break_label = self.b.new_label();
        try self.push_loop(label_name, break_label, start_label, result_reg, result_words);

        try self.emit(.{ .label = .{ .id = start_label } });
        const cond_reg = try self.compile_expr(condition_id);
        try self.emit(.{ .jump_if_false = .{ .condition = cond_reg, .target = break_label } });
        if (self.is_temp(cond_reg)) self.free_temp(cond_reg);

        const body_reg = try self.compile_expr(body_id);
        self.free_temp_value(body_reg, self.infer_expr_type(body_id));
        if (!self.returned) {
            try self.emit(.{ .jump = .{ .target = start_label } });
        }
        try self.emit(.{ .label = .{ .id = break_label } });
        self.pop_loop();
        i = 0;
        while (i < result_words) : (i += 1) self.unpin_temp(result_reg + i);
        return result_reg;
    }

    fn compile_until_expr(
        self: *function_ctx,
        condition_id: mir_mod.mir_identifier,
        body_id: mir_mod.mir_identifier,
        loop_id: mir_mod.mir_identifier,
        label_name: ?[]const u8,
    ) lower_error!u8 {
        const result_type = self.infer_expr_type(loop_id);
        const result_words = self.type_word_count(result_type);
        const result_reg = try self.alloc_temp_words(result_words);
        var i: u8 = 0;
        while (i < result_words) : (i += 1) self.pin_temp(result_reg + i);
        try self.zero_words(result_reg, result_words);

        const start_label = self.b.new_label();
        const break_label = self.b.new_label();
        try self.push_loop(label_name, break_label, start_label, result_reg, result_words);

        try self.emit(.{ .label = .{ .id = start_label } });
        const cond_reg = try self.compile_expr(condition_id);
        try self.emit(.{ .jump_if_true = .{ .condition = cond_reg, .target = break_label } });
        if (self.is_temp(cond_reg)) self.free_temp(cond_reg);

        const body_reg = try self.compile_expr(body_id);
        self.free_temp_value(body_reg, self.infer_expr_type(body_id));
        if (!self.returned) {
            try self.emit(.{ .jump = .{ .target = start_label } });
        }
        try self.emit(.{ .label = .{ .id = break_label } });
        self.pop_loop();
        i = 0;
        while (i < result_words) : (i += 1) self.unpin_temp(result_reg + i);
        return result_reg;
    }

    fn compile_repeat_expr(
        self: *function_ctx,
        count_id: mir_mod.mir_identifier,
        body_id: mir_mod.mir_identifier,
        loop_id: mir_mod.mir_identifier,
        label_name: ?[]const u8,
    ) lower_error!u8 {
        const result_type = self.infer_expr_type(loop_id);
        const result_words = self.type_word_count(result_type);
        const result_reg = try self.alloc_temp_words(result_words);
        var i: u8 = 0;
        while (i < result_words) : (i += 1) self.pin_temp(result_reg + i);
        try self.zero_words(result_reg, result_words);

        var count_reg = try self.compile_expr(count_id);
        if (count_reg == 0) count_reg = try self.save_result_reg(count_reg);
        var count_pinned = false;
        if (self.is_temp(count_reg)) {
            self.pin_temp(count_reg);
            count_pinned = true;
        }

        const zero_reg = try self.load_const_reg(0);
        const one_reg = try self.load_const_reg(1);
        self.pin_temp(zero_reg);
        self.pin_temp(one_reg);

        const start_label = self.b.new_label();
        const break_label = self.b.new_label();
        try self.push_loop(label_name, break_label, start_label, result_reg, result_words);

        try self.emit(.{ .label = .{ .id = start_label } });
        const cond_reg = try self.alloc_temp();
        try self.emit(.{ .compare_le = .{ .dst = cond_reg, .src_a = count_reg, .src_b = zero_reg } });
        try self.emit(.{ .jump_if_true = .{ .condition = cond_reg, .target = break_label } });
        if (self.is_temp(cond_reg)) self.free_temp(cond_reg);

        const body_reg = try self.compile_expr(body_id);
        self.free_temp_value(body_reg, self.infer_expr_type(body_id));

        try self.emit(.{ .sub = .{ .dst = count_reg, .src_a = count_reg, .src_b = one_reg } });
        if (!self.returned) {
            try self.emit(.{ .jump = .{ .target = start_label } });
        }
        try self.emit(.{ .label = .{ .id = break_label } });

        self.pop_loop();
        if (count_pinned) self.unpin_temp(count_reg);
        if (self.is_temp(count_reg)) self.free_temp(count_reg);
        self.unpin_temp(zero_reg);
        self.unpin_temp(one_reg);
        if (self.is_temp(zero_reg)) self.free_temp(zero_reg);
        if (self.is_temp(one_reg)) self.free_temp(one_reg);
        i = 0;
        while (i < result_words) : (i += 1) self.unpin_temp(result_reg + i);
        return result_reg;
    }

    fn compile_for_in_expr(
        self: *function_ctx,
        pattern_id: mir_mod.mir_identifier,
        iter_id: mir_mod.mir_identifier,
        body_id: mir_mod.mir_identifier,
        loop_id: mir_mod.mir_identifier,
        label_name: ?[]const u8,
    ) lower_error!u8 {
        const iter_type = self.infer_expr_type(iter_id);
        const result_type = self.infer_expr_type(loop_id);
        const result_words = self.type_word_count(result_type);
        const result_reg = try self.alloc_temp_words(result_words);
        var i: u8 = 0;
        while (i < result_words) : (i += 1) self.pin_temp(result_reg + i);
        try self.zero_words(result_reg, result_words);

        const elem_type = element_type_from_container(iter_type) orelse return error.unsupported_node;
        const elem_words = self.type_word_count(elem_type);

        var iter_reg = try self.compile_expr(iter_id);
        if (iter_reg == 0) iter_reg = try self.save_result_reg(iter_reg);
        const iter_words = self.type_word_count(iter_type);
        var iter_pinned = false;
        if (self.is_temp(iter_reg)) {
            i = 0;
            while (i < iter_words) : (i += 1) self.pin_temp(iter_reg + i);
            iter_pinned = true;
        }

        var base_ptr: u8 = 0;
        var len_reg: u8 = 0;
        switch (iter_type) {
            .applied => |ap| {
                if (std.mem.eql(u8, ap.base, "array") and ap.args.len >= 2) {
                    const length = array_length_from_type_key(ap.args[0]) orelse return error.unsupported_node;
                    base_ptr = try self.ptr_of_reg(iter_reg);
                    len_reg = try self.load_const_reg(length);
                } else if (std.mem.eql(u8, ap.base, "slice") and ap.args.len >= 1) {
                    base_ptr = try self.deref_to_temp(iter_reg);
                    const offset_reg = try self.load_const_reg(1);
                    const len_ptr = try self.alloc_temp();
                    try self.emit(.{ .add = .{ .dst = len_ptr, .src_a = iter_reg, .src_b = offset_reg } });
                    len_reg = try self.deref_to_temp(len_ptr);
                    if (self.is_temp(offset_reg)) self.free_temp(offset_reg);
                    if (self.is_temp(len_ptr)) self.free_temp(len_ptr);
                } else {
                    return error.unsupported_node;
                }
            },
            else => return error.unsupported_node,
        }

        var base_pinned = false;
        if (self.is_temp(base_ptr)) {
            self.pin_temp(base_ptr);
            base_pinned = true;
        }
        var len_pinned = false;
        if (self.is_temp(len_reg)) {
            self.pin_temp(len_reg);
            len_pinned = true;
        }

        const idx_reg = try self.load_const_reg(0);
        self.pin_temp(idx_reg);

        const start_label = self.b.new_label();
        const break_label = self.b.new_label();
        try self.push_loop(label_name, break_label, start_label, result_reg, result_words);

        try self.emit(.{ .label = .{ .id = start_label } });
        const cond_reg = try self.alloc_temp();
        try self.emit(.{ .compare_lt = .{ .dst = cond_reg, .src_a = idx_reg, .src_b = len_reg } });
        try self.emit(.{ .jump_if_false = .{ .condition = cond_reg, .target = break_label } });
        if (self.is_temp(cond_reg)) self.free_temp(cond_reg);

        const ptr_reg = blk: {
            if (elem_words > 1) {
                const scale_idx = try self.b.intern_const(elem_words);
                const scale_reg = try self.alloc_temp();
                try self.emit(.{ .load_const = .{ .dst = scale_reg, .const_index = scale_idx } });
                const offset_reg = try self.alloc_temp();
                try self.emit(.{ .mul = .{ .dst = offset_reg, .src_a = idx_reg, .src_b = scale_reg } });
                try self.emit(.{ .add = .{ .dst = offset_reg, .src_a = base_ptr, .src_b = offset_reg } });
                if (self.is_temp(scale_reg)) self.free_temp(scale_reg);
                break :blk offset_reg;
            }
            const offset_reg = try self.alloc_temp();
            try self.emit(.{ .add = .{ .dst = offset_reg, .src_a = base_ptr, .src_b = idx_reg } });
            break :blk offset_reg;
        };

        const elem_reg = blk: {
            if (elem_words > 1) {
                const dst_reg = try self.alloc_temp_words(elem_words);
                try self.load_words_from_ptr(ptr_reg, dst_reg, elem_words);
                break :blk dst_reg;
            }
            _ = try self.emit_foreign_call_reg("std::deref", ptr_reg);
            break :blk try self.save_result_reg(0);
        };

        if (self.is_temp(ptr_reg)) self.free_temp(ptr_reg);
        try self.bind_pattern_value(pattern_id, elem_reg, elem_type);
        self.free_temp_value(elem_reg, elem_type);

        const body_reg = try self.compile_expr(body_id);
        self.free_temp_value(body_reg, self.infer_expr_type(body_id));

        const one_reg = try self.load_const_reg(1);
        try self.emit(.{ .add = .{ .dst = idx_reg, .src_a = idx_reg, .src_b = one_reg } });
        if (self.is_temp(one_reg)) self.free_temp(one_reg);

        if (!self.returned) {
            try self.emit(.{ .jump = .{ .target = start_label } });
        }
        try self.emit(.{ .label = .{ .id = break_label } });

        self.pop_loop();
        self.unpin_temp(idx_reg);
        if (len_pinned) self.unpin_temp(len_reg);
        if (self.is_temp(idx_reg)) self.free_temp(idx_reg);
        if (self.is_temp(len_reg)) self.free_temp(len_reg);
        if (base_pinned) self.unpin_temp(base_ptr);
        if (self.is_temp(base_ptr)) self.free_temp(base_ptr);
        if (iter_pinned) {
            i = 0;
            while (i < iter_words) : (i += 1) self.unpin_temp(iter_reg + i);
        }
        if (self.is_temp(iter_reg)) self.free_temp_words(iter_reg, iter_words);
        i = 0;
        while (i < result_words) : (i += 1) self.unpin_temp(result_reg + i);
        return result_reg;
    }

    fn bind_pattern_value(
        self: *function_ctx,
        pattern_id: mir_mod.mir_identifier,
        value_reg: u8,
        value_type: type_key,
    ) lower_error!void {
        const node = self.b.node(pattern_id);
        if (node != .identifier) return error.unsupported_node;
        const name = self.b.string_value(node.identifier);
        if (std.mem.eql(u8, name, "_") or std.mem.eql(u8, name, "*")) return;
        const words = self.type_word_count(value_type);
        if (self.locals.get(name)) |local_reg| {
            if (words <= 1) {
                if (local_reg != value_reg) {
                    try self.emit(.{ .move = .{ .dst = local_reg, .src = value_reg } });
                }
            } else {
                try self.copy_words(local_reg, value_reg, words);
            }
            return;
        }
        const local_reg = try self.alloc_local_words(words);
        self.locals.put(name, local_reg) catch return error.out_of_memory;
        self.local_types.put(name, value_type) catch return error.out_of_memory;
        if (words <= 1) {
            try self.emit(.{ .move = .{ .dst = local_reg, .src = value_reg } });
        } else {
            try self.copy_words(local_reg, value_reg, words);
        }
    }

    fn compile_break_expr(
        self: *function_ctx,
        label: ?mir_mod.string_identifier,
        value: ?mir_mod.mir_identifier,
    ) lower_error!u8 {
        const label_name = if (label) |lab| self.b.string_value(lab) else null;
        const frame = self.find_break_frame(label_name) orelse return error.unsupported_node;
        if (value) |ref| {
            const value_reg = try self.compile_expr(ref);
            const value_type = self.infer_expr_type(ref);
            if (frame.result_words <= 1) {
                if (frame.result_reg != value_reg) {
                    try self.emit(.{ .move = .{ .dst = frame.result_reg, .src = value_reg } });
                }
            } else {
                try self.copy_words(frame.result_reg, value_reg, frame.result_words);
            }
            self.free_temp_value(value_reg, value_type);
        } else {
            try self.zero_words(frame.result_reg, frame.result_words);
        }
        try self.emit(.{ .jump = .{ .target = frame.break_label } });
        return frame.result_reg;
    }

    fn compile_continue_expr(self: *function_ctx, label: ?mir_mod.string_identifier) lower_error!u8 {
        const label_name = if (label) |lab| self.b.string_value(lab) else null;
        const frame = self.find_continue_frame(label_name) orelse return error.unsupported_node;
        const target = frame.continue_label orelse return error.unsupported_node;
        try self.emit(.{ .jump = .{ .target = target } });
        return frame.result_reg;
    }

    fn atomic_ordering_value(name: []const u8) ?u64 {
        const short = if (std.mem.lastIndexOf(u8, name, "::")) |idx| name[idx + 2 ..] else name;
        if (std.mem.eql(u8, short, "relaxed")) return 0;
        if (std.mem.eql(u8, short, "acquire")) return 1;
        if (std.mem.eql(u8, short, "release")) return 2;
        if (std.mem.eql(u8, short, "acq_rel")) return 3;
        if (std.mem.eql(u8, short, "seq_cst")) return 4;
        return null;
    }

    fn compile_atomic_expr(
        self: *function_ctx,
        value_id: mir_mod.mir_identifier,
        ordering: mir_mod.string_identifier,
    ) lower_error!u8 {
        var value_reg = try self.compile_expr(value_id);
        if (value_reg == 0) value_reg = try self.save_result_reg(value_reg);
        const value_type = self.infer_expr_type(value_id);
        const words = self.type_word_count(value_type);
        const ptr_reg = try self.alloc_words(words);
        try self.store_words_to_ptr(ptr_reg, value_reg, words);
        if (self.is_temp(value_reg)) self.free_temp_words(value_reg, words);

        const order_name = self.b.string_value(ordering);
        const order_val = atomic_ordering_value(order_name) orelse return error.unsupported_node;
        const order_reg = try self.load_const_reg(order_val);
        const atomic_reg = try self.alloc_temp_words(2);
        try self.emit(.{ .move = .{ .dst = atomic_reg, .src = ptr_reg } });
        try self.emit(.{ .move = .{ .dst = atomic_reg + 1, .src = order_reg } });
        if (self.is_temp(ptr_reg)) self.free_temp(ptr_reg);
        if (self.is_temp(order_reg)) self.free_temp(order_reg);
        return atomic_reg;
    }

    fn compile_atomic_deref(
        self: *function_ctx,
        target_id: mir_mod.mir_identifier,
        inner_type: type_key,
    ) lower_error!u8 {
        var atomic_reg = try self.compile_expr(target_id);
        if (atomic_reg == 0) atomic_reg = try self.save_result_reg(atomic_reg);
        const value_reg = try self.compile_atomic_load_reg(atomic_reg, inner_type);
        if (self.is_temp(atomic_reg)) self.free_temp_words(atomic_reg, 2);
        return value_reg;
    }

    fn compile_atomic_unary(
        self: *function_ctx,
        op: ink.unary,
        target_id: mir_mod.mir_identifier,
    ) lower_error!u8 {
        const atomic_type = self.infer_expr_type(target_id);
        const inner_type = atomic_inner_type_key(atomic_type) orelse return error.unsupported_node;
        var atomic_reg = try self.compile_expr(target_id);
        if (atomic_reg == 0) atomic_reg = try self.save_result_reg(atomic_reg);
        var pinned = false;
        if (self.is_temp(atomic_reg)) {
            self.pin_temp(atomic_reg);
            self.pin_temp(atomic_reg + 1);
            pinned = true;
        }
        const value_reg = try self.compile_atomic_load_reg(atomic_reg, inner_type);
        const result_reg = try self.emit_unary_reg(op, value_reg, inner_type);
        self.free_temp_value(value_reg, inner_type);
        const atomic_result = try self.wrap_atomic_value(result_reg, inner_type, atomic_reg + 1);
        if (pinned) {
            self.unpin_temp(atomic_reg);
            self.unpin_temp(atomic_reg + 1);
            self.free_temp_words(atomic_reg, 2);
        }
        return atomic_result;
    }

    fn compile_atomic_binary(
        self: *function_ctx,
        op: ink.binary,
        left_id: mir_mod.mir_identifier,
        right_id: mir_mod.mir_identifier,
        node_id: mir_mod.mir_identifier,
    ) lower_error!u8 {
        const left_type = self.infer_expr_type(left_id);
        const right_type = self.infer_expr_type(right_id);
        const left_inner = atomic_inner_type_key(left_type) orelse left_type;
        const right_inner = atomic_inner_type_key(right_type) orelse right_type;

        var order_reg: ?u8 = null;
        var order_pinned = false;
        var order_atomic_reg: ?u8 = null;

        var left_reg: u8 = 0;
        if (atomic_inner_type_key(left_type)) |inner| {
            var atomic_reg = try self.compile_expr(left_id);
            if (atomic_reg == 0) atomic_reg = try self.save_result_reg(atomic_reg);
            if (order_reg == null) {
                order_reg = atomic_reg + 1;
                order_atomic_reg = atomic_reg;
                if (self.is_temp(atomic_reg)) {
                    self.pin_temp(atomic_reg);
                    self.pin_temp(atomic_reg + 1);
                    order_pinned = true;
                }
            }
            left_reg = try self.compile_atomic_load_reg(atomic_reg, inner);
            if (self.is_temp(atomic_reg) and atomic_reg != order_atomic_reg) {
                self.free_temp_words(atomic_reg, 2);
            }
        } else {
            left_reg = try self.compile_expr(left_id);
        }

        var right_reg: u8 = 0;
        if (atomic_inner_type_key(right_type)) |inner| {
            var atomic_reg = try self.compile_expr(right_id);
            if (atomic_reg == 0) atomic_reg = try self.save_result_reg(atomic_reg);
            if (order_reg == null) {
                order_reg = atomic_reg + 1;
                order_atomic_reg = atomic_reg;
                if (self.is_temp(atomic_reg)) {
                    self.pin_temp(atomic_reg);
                    self.pin_temp(atomic_reg + 1);
                    order_pinned = true;
                }
            }
            right_reg = try self.compile_atomic_load_reg(atomic_reg, inner);
            if (self.is_temp(atomic_reg) and atomic_reg != order_atomic_reg) {
                self.free_temp_words(atomic_reg, 2);
            }
        } else {
            right_reg = try self.compile_expr(right_id);
        }

        const inner_result = self.emit_binary_reg(op, left_reg, right_reg, left_inner, right_inner) catch |err| switch (err) {
            error.unknown_function => {
                self.record_binary_operator_error(node_id, op, left_inner, right_inner);
                return err;
            },
            else => return err,
        };
        self.free_temp_value(left_reg, left_inner);
        self.free_temp_value(right_reg, right_inner);

        const order_src = order_reg orelse blk: {
            const reg = try self.load_const_reg(4);
            break :blk reg;
        };
        const result_type = switch (op) {
            .equal,
            .not_equal,
            .less_than,
            .less_or_equal,
            .greater_than,
            .greater_or_equal,
            .logical_and,
            .logical_or,
            .logical_xor,
            => type_key{ .name = "bool" },
            else => left_inner,
        };
        const atomic_result = try self.wrap_atomic_value(inner_result, result_type, order_src);
        if (order_pinned) {
            const reg = order_atomic_reg.?;
            self.unpin_temp(reg);
            self.unpin_temp(reg + 1);
            self.free_temp_words(reg, 2);
        } else if (order_reg != null and self.is_temp(order_reg.?)) {
            self.free_temp(order_reg.?);
        }
        return atomic_result;
    }

    fn emit_unary_reg(self: *function_ctx, op: ink.unary, src: u8, ty: type_key) lower_error!u8 {
        const dst = try self.alloc_temp();
        if (self.is_float_type_key(ty)) {
            if (op == .neg) {
                try self.emit(.{ .fneg = .{ .dst = dst, .src_a = src, .src_b = src } });
                return dst;
            }
        }
        switch (op) {
            .neg => try self.emit(.{ .int_neg = .{ .dst = dst, .src_a = src, .src_b = src } }),
            .bit_not => try self.emit(.{ .bit_not = .{ .dst = dst, .src_a = src, .src_b = src } }),
            .not => {
                const zero_idx = try self.b.intern_const(0);
                const zero_reg = try self.alloc_temp();
                try self.emit(.{ .load_const = .{ .dst = zero_reg, .const_index = zero_idx } });
                try self.emit(.{ .compare_eq = .{ .dst = dst, .src_a = src, .src_b = zero_reg } });
                self.free_temp(zero_reg);
            },
            else => return error.unsupported_node,
        }
        return dst;
    }

    fn emit_binary_reg(
        self: *function_ctx,
        op: ink.binary,
        left: u8,
        right: u8,
        left_type: type_key,
        right_type: type_key,
    ) lower_error!u8 {
        const same_int = self.is_int_type_key(left_type) and self.is_int_type_key(right_type);
        const same_float = self.is_float_type_key(left_type) and self.is_float_type_key(right_type);
        const same_bool = self.is_bool_type_key(left_type) and self.is_bool_type_key(right_type);

        if (binary_operator_method_name(op)) |method| {
            if (self.trait_name_from_type(left_type) != null or self.is_known_non_builtin(left_type)) {
                if (try self.try_compile_method_call_regs(method, left, left_type, null, &.{right}, &.{right_type})) |reg| {
                    return reg;
                }
                if (self.is_known_non_builtin(left_type)) return error.unknown_function;
            }
        }

        const dst = try self.alloc_temp();
        switch (op) {
            .add => {
                if (same_float) {
                    try self.emit(.{ .fadd = .{ .dst = dst, .src_a = left, .src_b = right } });
                } else {
                    try self.emit(.{ .add = .{ .dst = dst, .src_a = left, .src_b = right } });
                }
            },
            .sub => {
                if (same_float) {
                    try self.emit(.{ .fsub = .{ .dst = dst, .src_a = left, .src_b = right } });
                } else {
                    try self.emit(.{ .sub = .{ .dst = dst, .src_a = left, .src_b = right } });
                }
            },
            .mul => {
                if (same_float) {
                    try self.emit(.{ .fmul = .{ .dst = dst, .src_a = left, .src_b = right } });
                } else {
                    try self.emit(.{ .mul = .{ .dst = dst, .src_a = left, .src_b = right } });
                }
            },
            .div => {
                if (same_float) {
                    try self.emit(.{ .fdiv = .{ .dst = dst, .src_a = left, .src_b = right } });
                } else {
                    try self.emit(.{ .div = .{ .dst = dst, .src_a = left, .src_b = right } });
                }
            },
            .mod => {
                if (same_float) {
                    try self.emit(.{ .frem = .{ .dst = dst, .src_a = left, .src_b = right } });
                } else {
                    try self.emit(.{ .rem = .{ .dst = dst, .src_a = left, .src_b = right } });
                }
            },
            .bit_and => try self.emit(.{ .bit_and = .{ .dst = dst, .src_a = left, .src_b = right } }),
            .bit_or => try self.emit(.{ .bit_or = .{ .dst = dst, .src_a = left, .src_b = right } }),
            .bit_xor, .logical_xor => try self.emit(.{ .bit_xor = .{ .dst = dst, .src_a = left, .src_b = right } }),
            .shl => try self.emit(.{ .bit_shl = .{ .dst = dst, .src_a = left, .src_b = right } }),
            .shr => try self.emit(.{ .bit_shr = .{ .dst = dst, .src_a = left, .src_b = right } }),
            .logical_and => try self.emit(.{ .bit_and = .{ .dst = dst, .src_a = left, .src_b = right } }),
            .logical_or => try self.emit(.{ .bit_or = .{ .dst = dst, .src_a = left, .src_b = right } }),
            .equal => {
                if (same_float) {
                    try self.emit(.{ .fcompare_eq = .{ .dst = dst, .src_a = left, .src_b = right } });
                } else {
                    try self.emit(.{ .compare_eq = .{ .dst = dst, .src_a = left, .src_b = right } });
                }
            },
            .not_equal => {
                if (same_float) {
                    try self.emit(.{ .fcompare_eq = .{ .dst = dst, .src_a = left, .src_b = right } });
                } else {
                    try self.emit(.{ .compare_eq = .{ .dst = dst, .src_a = left, .src_b = right } });
                }
                const zero_idx = try self.b.intern_const(0);
                const zero_reg = try self.alloc_temp();
                try self.emit(.{ .load_const = .{ .dst = zero_reg, .const_index = zero_idx } });
                try self.emit(.{ .compare_eq = .{ .dst = dst, .src_a = dst, .src_b = zero_reg } });
                self.free_temp(zero_reg);
            },
            .less_than => {
                if (same_float) {
                    try self.emit(.{ .fcompare_lt = .{ .dst = dst, .src_a = left, .src_b = right } });
                } else {
                    try self.emit(.{ .compare_lt = .{ .dst = dst, .src_a = left, .src_b = right } });
                }
            },
            .greater_than => {
                if (same_float) {
                    try self.emit(.{ .fcompare_gt = .{ .dst = dst, .src_a = left, .src_b = right } });
                } else {
                    try self.emit(.{ .compare_gt = .{ .dst = dst, .src_a = left, .src_b = right } });
                }
            },
            .less_or_equal => {
                if (same_float) {
                    try self.emit(.{ .fcompare_gt = .{ .dst = dst, .src_a = left, .src_b = right } });
                } else {
                    try self.emit(.{ .compare_le = .{ .dst = dst, .src_a = left, .src_b = right } });
                    return dst;
                }
                const zero_idx = try self.b.intern_const(0);
                const zero_reg = try self.alloc_temp();
                try self.emit(.{ .load_const = .{ .dst = zero_reg, .const_index = zero_idx } });
                try self.emit(.{ .compare_eq = .{ .dst = dst, .src_a = dst, .src_b = zero_reg } });
                self.free_temp(zero_reg);
            },
            .greater_or_equal => {
                if (same_float) {
                    try self.emit(.{ .fcompare_lt = .{ .dst = dst, .src_a = left, .src_b = right } });
                } else {
                    try self.emit(.{ .compare_ge = .{ .dst = dst, .src_a = left, .src_b = right } });
                    return dst;
                }
                const zero_idx = try self.b.intern_const(0);
                const zero_reg = try self.alloc_temp();
                try self.emit(.{ .load_const = .{ .dst = zero_reg, .const_index = zero_idx } });
                try self.emit(.{ .compare_eq = .{ .dst = dst, .src_a = dst, .src_b = zero_reg } });
                self.free_temp(zero_reg);
            },
            else => return error.unsupported_node,
        }

        if (!same_int and !same_float and !same_bool) return error.unsupported_node;
        return dst;
    }

    fn wrap_atomic_value(
        self: *function_ctx,
        value_reg: u8,
        value_type: type_key,
        order_reg: u8,
    ) lower_error!u8 {
        const words = self.type_word_count(value_type);
        const ptr_reg = try self.alloc_words(words);
        try self.store_words_to_ptr(ptr_reg, value_reg, words);
        if (self.is_temp(value_reg)) self.free_temp_words(value_reg, words);

        const atomic_reg = try self.alloc_temp_words(2);
        try self.emit(.{ .move = .{ .dst = atomic_reg, .src = ptr_reg } });
        try self.emit(.{ .move = .{ .dst = atomic_reg + 1, .src = order_reg } });
        if (self.is_temp(ptr_reg)) self.free_temp(ptr_reg);
        return atomic_reg;
    }

    fn compile_spawn_expr(self: *function_ctx, id: mir_mod.mir_identifier) lower_error!u8 {
        const node = self.b.node(id);
        return switch (node) {
            .binary => |bin| if (bin.op == .call) try self.compile_spawn_call(id) else error.unsupported_node,
            else => error.unsupported_node,
        };
    }

    fn compile_spawn_call(self: *function_ctx, id: mir_mod.mir_identifier) lower_error!u8 {
        var base_id = id;
        var args = std.array_list.Managed(mir_mod.mir_identifier).init(self.b.allocator);
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

        std.mem.reverse(mir_mod.mir_identifier, args.items);

        const base_node = self.b.node(base_id);
        const name = switch (base_node) {
            .identifier => |ident| self.b.string_value(ident),
            else => return error.unsupported_node,
        };

        if (args.items.len == 1 and self.is_unit(args.items[0])) {
            args.clearRetainingCapacity();
        }

        if (self.b.foreigns.contains(name)) return error.unsupported_node;

        const arg_count = args.items.len;
        var arg_types = self.b.allocator.alloc(type_key, arg_count) catch return error.out_of_memory;
        defer self.b.allocator.free(arg_types);
        for (args.items, 0..) |arg_id, idx| {
            arg_types[idx] = self.infer_expr_type(arg_id);
        }
        const arg_type_slice = arg_types[0..arg_count];

        const group = self.b.functions.get(name) orelse {
            self.record_call_error(id, "unknown function ", name, arg_type_slice, false);
            return error.unknown_function;
        };
        const resolved = self.resolve_function_overload(group.items, arg_type_slice) catch |err| switch (err) {
            error.unknown_function => {
                self.record_call_error(id, "no matching overload for ", name, arg_type_slice, false);
                return err;
            },
            error.ambiguous_overload => {
                self.record_call_error(id, "ambiguous overload for ", name, arg_type_slice, false);
                return err;
            },
            else => return err,
        };
        defer if (resolved.bindings.len > 0) self.b.allocator.free(resolved.bindings);
        const info = resolved.info;

        const return_type = info.decl.return_type orelse return error.unsupported_node;
        const base_return = type_key_from_type_node_with_self(self.b, return_type, info.impl_for);
        const resolved_return = if (resolved.bindings.len > 0)
            apply_bindings_to_type_key(self.b, base_return, resolved.bindings)
        else
            base_return;
        if (self.type_word_count(resolved_return) > 1) return error.unsupported_node;

        var arg_regs = self.b.allocator.alloc(u8, arg_count) catch return error.out_of_memory;
        defer self.b.allocator.free(arg_regs);
        for (args.items, 0..) |arg_id, idx| {
            arg_regs[idx] = try self.compile_expr(arg_id);
        }
        const total_words = try self.emit_argument_values(arg_regs, arg_type_slice, 1);
        if (total_words > max_register + 1) return error.register_overflow;
        for (arg_regs, 0..) |arg_reg, idx| {
            self.free_temp_value(arg_reg, arg_types[idx]);
        }

        const dst_reg = try self.alloc_temp();
        try self.emit(.{ .task_spawn = .{ .dst = dst_reg, .target = info.label, .argc = total_words - 1 } });
        return dst_reg;
    }

    fn compile_await_expr(self: *function_ctx, id: mir_mod.mir_identifier) lower_error!u8 {
        const src_reg = try self.compile_expr(id);
        const dst_reg = if (self.is_temp(src_reg)) src_reg else try self.alloc_temp();
        try self.emit(.{ .task_await = .{ .dst = dst_reg, .src = src_reg } });
        if (self.is_temp(src_reg) and src_reg != dst_reg) self.free_temp(src_reg);
        return dst_reg;
    }

    fn compile_try_expr(self: *function_ctx, arg_id: mir_mod.mir_identifier) lower_error!u8 {
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
        try self.emit_return_value(result_reg, self.infer_expr_type(arg_id));

        try self.emit(.{ .label = .{ .id = end_label } });
        if (free_result) self.free_temp(result_reg);
        return dst;
    }

    fn compile_optional_unwrap(self: *function_ctx, arg_id: mir_mod.mir_identifier) lower_error!u8 {
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

        try self.emit_return_value(value_reg, self.infer_expr_type(arg_id));

        try self.emit(.{ .label = .{ .id = ok_label } });
        return value_reg;
    }

    fn load_const_reg(self: *function_ctx, value: u64) lower_error!u8 {
        const idx = try self.b.intern_const(value);
        const reg = try self.alloc_temp();
        try self.emit(.{ .load_const = .{ .dst = reg, .const_index = idx } });
        return reg;
    }

    fn alloc_words(self: *function_ctx, count: u64) lower_error!u8 {
        const size_reg = try self.load_const_reg(count);
        const alloc_idx = try self.foreign_index("std::alloc");
        try self.emit_foreign_call(alloc_idx, &[_]u8{size_reg});
        self.free_temp(size_reg);

        const ptr_reg = try self.alloc_temp();
        try self.emit(.{ .move = .{ .dst = ptr_reg, .src = 0 } });
        return ptr_reg;
    }

    fn deref_to_temp(self: *function_ctx, ptr_reg: u8) lower_error!u8 {
        const deref_idx = try self.foreign_index("std::deref");
        try self.emit_foreign_call(deref_idx, &[_]u8{ptr_reg});
        const dst = try self.alloc_temp();
        try self.emit(.{ .move = .{ .dst = dst, .src = 0 } });
        return dst;
    }

    fn deref_into(self: *function_ctx, ptr_reg: u8, dst_reg: u8) lower_error!void {
        const deref_idx = try self.foreign_index("std::deref");
        try self.emit_foreign_call(deref_idx, &[_]u8{ptr_reg});
        try self.emit(.{ .move = .{ .dst = dst_reg, .src = 0 } });
    }

    fn store_value(self: *function_ctx, ptr_reg: u8, value_reg: u8) lower_error!void {
        const store_idx = try self.foreign_index("std::store");
        try self.emit_foreign_call(store_idx, &[_]u8{ptr_reg, value_reg});
    }

    fn load_label_const_reg(self: *function_ctx, label: ink.exe.label_id) lower_error!u8 {
        const idx = try self.b.const_index_for_label(label);
        const reg = try self.alloc_temp();
        try self.emit(.{ .load_const = .{ .dst = reg, .const_index = idx } });
        return reg;
    }

    fn dyn_data_reg(self: *function_ctx, obj_reg: u8) lower_error!u8 {
        _ = self;
        return obj_reg + 1;
    }

    fn type_name_from_value(self: *function_ctx, value_id: mir_mod.mir_identifier) ?[]const u8 {
        const node = self.b.node(value_id);
        return switch (node) {
            .record_literal => |rec| self.b.string_value(rec.type_name),
            .integer => "int",
            .float => "float",
            .boolean => "bool",
            .string => "string",
            .identifier => |ident| blk: {
                const name = self.b.string_value(ident);
                const ty = self.local_types.get(name) orelse break :blk null;
                break :blk switch (ty) {
                    .name => |type_name| type_name,
                    .dyn_trait => |type_name| type_name,
                    .applied => |ap| ap.base,
                    .unknown => null,
                };
            },
            .binary => |bin| blk: {
                if (bin.op != .@"as") break :blk null;
                const type_name = type_name_from_type_node(self.b, bin.right) orelse break :blk null;
                break :blk type_name;
            },
            else => null,
        };
    }

    fn compile_as_expr(self: *function_ctx, value_id: mir_mod.mir_identifier, trait_id: mir_mod.mir_identifier) lower_error!u8 {
        const type_node = self.b.node(trait_id);
        if (type_node == .type and type_node.type == .dyn) {
            const trait_name = type_name_from_type_node(self.b, trait_id) orelse return error.unsupported_node;
            if (!self.b.traits.contains(trait_name)) {
                return error.unsupported_node;
            }

            const value_type = self.infer_expr_type(value_id);
            var type_name: ?[]const u8 = switch (value_type) {
                .name => |name| name,
                .dyn_trait => |name| {
                    if (std.mem.eql(u8, name, trait_name)) {
                        return self.compile_expr(value_id);
                    }
                    return error.unsupported_node;
                },
                .applied => |ap| ap.base,
                .unknown => null,
            };
            if (type_name == null) {
                type_name = self.type_name_from_value(value_id);
            }
            if (type_name == null) return error.unsupported_node;

            const vtable_map = self.b.trait_vtables.get(trait_name) orelse return error.unsupported_node;
            const labels = vtable_map.get(type_name.?) orelse return error.unsupported_node;

            const obj_reg = try self.alloc_temp_words(2);
            var value_reg = try self.compile_expr(value_id);
            var value_temp: ?u8 = null;
            if (value_reg == 0) {
                const tmp = try self.alloc_temp();
                try self.emit(.{ .move = .{ .dst = tmp, .src = value_reg } });
                value_reg = tmp;
                value_temp = tmp;
            }
            const value_words = self.type_word_count(value_type);
            const data_ptr = if (self.is_temp(value_reg)) blk: {
                const heap_ptr = try self.alloc_words(value_words);
                try self.store_words_to_ptr(heap_ptr, value_reg, value_words);
                self.free_temp_value(value_reg, value_type);
                break :blk heap_ptr;
            } else blk: {
                const ptr = try self.ptr_of_reg(value_reg);
                if (value_temp) |tmp| self.free_temp(tmp);
                break :blk ptr;
            };
            value_reg = data_ptr;
            value_temp = null;

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

            try self.emit(.{ .move = .{ .dst = obj_reg, .src = vtable_reg } });
            try self.emit(.{ .move = .{ .dst = obj_reg + 1, .src = value_reg } });

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

    fn compile_select_expr(
        self: *function_ctx,
        id: mir_mod.mir_identifier,
        arms: []const mir_mod.mir.select_arm,
    ) lower_error!u8 {
        if (arms.len == 0) return error.unsupported_node;
        if (arms.len > 7) return error.register_overflow;

        const result_type = self.infer_expr_type(id);
        const result_words = self.type_word_count(result_type);
        const result_reg = try self.alloc_temp_words(result_words);
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
            if (result_words <= 1) {
                if (body_reg != result_reg) {
                    try self.emit(.{ .move = .{ .dst = result_reg, .src = body_reg } });
                }
                self.free_temp_value(body_reg, self.infer_expr_type(arms[i].body));
            } else {
                try self.copy_words(result_reg, body_reg, result_words);
                self.free_temp_value(body_reg, self.infer_expr_type(arms[i].body));
            }

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

    const foreign_name = struct {
        name: []const u8,
        owned: bool,
    };

    fn foreign_name_from_expr(self: *function_ctx, id: mir_mod.mir_identifier) lower_error!foreign_name {
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

    fn compile_foreign_call(self: *function_ctx, id: mir_mod.mir_identifier) lower_error!u8 {
        var base_id = id;
        var args = std.array_list.Managed(mir_mod.mir_identifier).init(self.b.allocator);
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

        std.mem.reverse(mir_mod.mir_identifier, args.items);

        const name_info = try self.foreign_name_from_expr(base_id);
        defer if (name_info.owned) self.b.allocator.free(name_info.name);

        if (args.items.len == 1 and self.is_unit(args.items[0])) {
            args.clearRetainingCapacity();
        }

        const foreign_idx = try self.foreign_index(name_info.name);
        const arg_count = args.items.len;
        var arg_types = self.b.allocator.alloc(type_key, arg_count) catch return error.out_of_memory;
        defer self.b.allocator.free(arg_types);
        var arg_regs = self.b.allocator.alloc(u8, arg_count) catch return error.out_of_memory;
        defer self.b.allocator.free(arg_regs);
        for (args.items, 0..) |arg_id, idx| {
            arg_types[idx] = self.infer_expr_type(arg_id);
            arg_regs[idx] = try self.compile_expr(arg_id);
        }
        _ = try self.emit_argument_values(arg_regs, arg_types, 1);
        try self.emit(.{ .call_foreign = .{ .index = foreign_idx } });
        for (arg_regs, 0..) |arg_reg, idx| {
            self.free_temp_value(arg_reg, arg_types[idx]);
        }
        return 0;
    }

    fn emit_foreign_call_named(self: *function_ctx, name: []const u8, args: []const mir_mod.mir_identifier) lower_error!u8 {
        const arg_count = args.len;
        var arg_types = self.b.allocator.alloc(type_key, arg_count) catch return error.out_of_memory;
        defer self.b.allocator.free(arg_types);
        var arg_regs = self.b.allocator.alloc(u8, arg_count) catch return error.out_of_memory;
        defer self.b.allocator.free(arg_regs);
        for (args, 0..) |arg_id, idx| {
            arg_types[idx] = self.infer_expr_type(arg_id);
            arg_regs[idx] = try self.compile_expr(arg_id);
        }
        _ = try self.emit_argument_values(arg_regs, arg_types, 1);
        const foreign_idx = try self.foreign_index(name);
        try self.emit(.{ .call_foreign = .{ .index = foreign_idx } });
        for (arg_regs, 0..) |arg_reg, idx| {
            self.free_temp_value(arg_reg, arg_types[idx]);
        }
        return 0;
    }

    fn emit_foreign_call_reg(self: *function_ctx, name: []const u8, arg_reg: u8) lower_error!u8 {
        try self.emit(.{ .argument_set = .{ .dst = 1, .src = arg_reg } });
        const foreign_idx = try self.foreign_index(name);
        try self.emit(.{ .call_foreign = .{ .index = foreign_idx } });
        return 0;
    }

    fn emit_foreign_call_void(self: *function_ctx, name: []const u8) lower_error!void {
        const foreign_idx = try self.foreign_index(name);
        try self.emit(.{ .call_foreign = .{ .index = foreign_idx } });
    }

    fn emit_atomic_lock(self: *function_ctx) lower_error!void {
        try self.emit_foreign_call_void("std::atomic_lock");
    }

    fn emit_atomic_unlock(self: *function_ctx) lower_error!void {
        try self.emit_foreign_call_void("std::atomic_unlock");
    }

    fn save_result_reg(self: *function_ctx, reg: u8) lower_error!u8 {
        if (reg != 0) return reg;
        const tmp = try self.alloc_temp();
        try self.emit(.{ .move = .{ .dst = tmp, .src = reg } });
        return tmp;
    }

    fn coerce_string(self: *function_ctx, arg_reg: u8, arg_type: type_key) lower_error!u8 {
        const name = type_key_base_name(arg_type) orelse return error.unsupported_node;
        if (std.mem.eql(u8, name, "string")) return arg_reg;

        const foreign_fn = if (std.mem.eql(u8, name, "int") or std.mem.eql(u8, name, "uint"))
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

    fn compile_interpolate(self: *function_ctx, args: []const mir_mod.mir_identifier) lower_error!u8 {
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

    fn compile_intrinsic(self: *function_ctx, call: mir_mod.intrinsic) lower_error!u8 {
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
        }
    }

    fn emit_int_unary(self: *function_ctx, arg_id: mir_mod.mir_identifier, op: enum { bit_not, int_neg, int_abs }) lower_error!u8 {
        const prep = try self.prep_unary(arg_id);
        switch (op) {
            .bit_not => try self.emit(.{ .bit_not = .{ .dst = prep.dst, .src_a = prep.src, .src_b = 0 } }),
            .int_neg => try self.emit(.{ .int_neg = .{ .dst = prep.dst, .src_a = prep.src, .src_b = 0 } }),
            .int_abs => try self.emit(.{ .int_abs = .{ .dst = prep.dst, .src_a = prep.src, .src_b = 0 } }),
        }
        self.finish_unary(prep);
        return prep.dst;
    }

    fn emit_float_unary(self: *function_ctx, arg_id: mir_mod.mir_identifier, op: enum {
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
    }) lower_error!u8 {
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
        left_id: mir_mod.mir_identifier,
        right_id: mir_mod.mir_identifier,
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
    ) lower_error!u8 {
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
        left_id: mir_mod.mir_identifier,
        right_id: mir_mod.mir_identifier,
        op: enum { fadd, fsub, fmul, fdiv, frem, fmin, fmax, fcompare_eq, fcompare_lt, fcompare_gt },
    ) lower_error!u8 {
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

    fn emit_int_not_equal(self: *function_ctx, left_id: mir_mod.mir_identifier, right_id: mir_mod.mir_identifier) lower_error!u8 {
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

    fn emit_float_not_equal(self: *function_ctx, left_id: mir_mod.mir_identifier, right_id: mir_mod.mir_identifier) lower_error!u8 {
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
        left_id: mir_mod.mir_identifier,
        right_id: mir_mod.mir_identifier,
        op: enum { fcompare_lt, fcompare_gt },
    ) lower_error!u8 {
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

    fn print_foreign(self: *function_ctx, prefix: []const u8, suffix: []const u8) lower_error!print_foreign_name {
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
    ) lower_error!void {
        const full = try self.print_foreign(prefix, suffix);
        defer if (full.owned) self.b.allocator.free(full.name);
        if (arg_reg) |reg| {
            try self.emit(.{ .argument_set = .{ .dst = 1, .src = reg } });
        }
        const foreign_idx = try self.foreign_index(full.name);
        try self.emit(.{ .call_foreign = .{ .index = foreign_idx } });
    }

    fn resolve_print_to(self: *function_ctx, arg_type: type_key) lower_error!?function_info {
        const group = self.b.functions.get("print_to") orelse return null;
        var call_types = [_]type_key{ .{ .name = "int" }, arg_type };
        const selected_idx = self.resolve_function_overload_index(group.items, call_types[0..]) catch |err| switch (err) {
            error.unknown_function => return null,
            else => return err,
        };
        return group.items[selected_idx];
    }

    fn emit_print_to_call(
        self: *function_ctx,
        info: function_info,
        arg_reg: u8,
        arg_type: type_key,
    ) lower_error!void {
        if (info.decl.return_type) |ret_id| {
            const ret_type = type_key_from_type_node_with_self(self.b, ret_id, info.impl_for);
            if (self.type_word_count(ret_type) > 1) return error.unsupported_node;
        }
        const writer_idx = try self.b.intern_const(0);
        const writer_reg = try self.alloc_temp();
        try self.emit(.{ .load_const = .{ .dst = writer_reg, .const_index = writer_idx } });

        try self.emit(.{ .argument_set = .{ .dst = 1, .src = writer_reg } });
        try self.emit_argument_words(arg_reg, self.type_word_count(arg_type), 2);
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
        args: []const mir_mod.mir_identifier,
        suffix: []const u8,
        separator: print_separator,
    ) lower_error!u8 {
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

    fn emit_print_value(self: *function_ctx, prefix: []const u8, arg_id: mir_mod.mir_identifier) lower_error!void {
        var arg_type = self.infer_expr_type(arg_id);
        if (arg_type == .unknown) {
            if (self.type_name_from_value(arg_id)) |name| {
                arg_type = .{ .name = name };
            }
        }
        const info = try self.resolve_print_to(arg_type);
        const arg_reg = try self.compile_expr(arg_id);

        if (info) |print_info| {
            try self.emit_print_to_call(print_info, arg_reg, arg_type);
        } else {
            const print_suffix = switch (arg_type) {
                .name => |type_name| blk: {
                    if (std.mem.eql(u8, type_name, "int") or std.mem.eql(u8, type_name, "uint")) break :blk "print_int";
                    if (std.mem.eql(u8, type_name, "float")) break :blk "print_float";
                    if (std.mem.eql(u8, type_name, "bool")) break :blk "print_bool";
                    if (std.mem.eql(u8, type_name, "string")) break :blk "print_string";
                    self.b.set_error_fmt(arg_id, "cannot print value of type {s}", .{type_name});
                    return error.unknown_function;
                },
                .dyn_trait => |type_name| {
                    self.b.set_error_fmt(arg_id, "cannot print value of dyn {s}", .{type_name});
                    return error.unknown_function;
                },
                .applied => |ap| {
                    self.b.set_error_fmt(arg_id, "cannot print value of type {s}", .{ap.base});
                    return error.unknown_function;
                },
                .unknown => {
                    self.b.set_error_message(arg_id, "cannot print value of unknown type");
                    return error.unknown_function;
                },
            };
            try self.emit_print_foreign_reg(prefix, print_suffix, arg_reg);
        }

        self.free_temp_value(arg_reg, arg_type);
    }

    fn interpolation_args(self: *function_ctx, arg_id: mir_mod.mir_identifier) ?[]const mir_mod.mir_identifier {
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
        if (arg_types.len + 1 > max_register) return .unknown;

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
        receiver_id: mir_mod.mir_identifier,
        args: []const mir_mod.mir_identifier,
    ) lower_error!u8 {
        const recv_type = type_key{ .dyn_trait = trait_name };
        const recv_concrete = self.dyn_concrete_type(receiver_id);
        const recv_reg = try self.compile_expr(receiver_id);
        var recv_pinned = false;
        if (self.is_temp(recv_reg)) {
            self.pin_temp(recv_reg);
            recv_pinned = true;
        }

        var arg_regs_buf: [8]u8 = undefined;
        var arg_types_buf: [8]type_key = undefined;
        var arg_pinned: [8]bool = undefined;
        for (args, 0..) |arg_id, idx| {
            var arg_type = self.infer_expr_type(arg_id);
            var arg_reg = try self.compile_expr(arg_id);
            var pinned = false;
            if (self.is_temp(arg_reg)) {
                self.pin_temp(arg_reg);
                pinned = true;
            }
            if (self.trait_name_from_type(self.infer_expr_type(arg_id))) |arg_trait| {
                if (std.mem.eql(u8, arg_trait, trait_name)) {
                    var concrete: ?type_key = null;
                    if (recv_concrete) |recv_ty| {
                        if (self.dyn_concrete_type(arg_id)) |arg_ty| {
                            if (!type_key_eq(recv_ty, arg_ty)) return error.unsupported_node;
                        }
                        concrete = recv_ty;
                    } else {
                        concrete = self.dyn_concrete_type(arg_id);
                    }

                    if (pinned) {
                        self.unpin_temp(arg_reg);
                        pinned = false;
                    }

                    if (concrete) |concrete_ty| {
                        const words = self.type_word_count(concrete_ty);
                        const value_reg = try self.alloc_temp_words(words);
                        const data_reg = try self.dyn_data_reg(arg_reg);
                        try self.load_words_from_ptr(data_reg, value_reg, words);
                        self.free_temp_value(arg_reg, arg_type);
                        arg_reg = value_reg;
                        arg_type = concrete_ty;
                    } else {
                        return error.unsupported_node;
                    }

                    if (self.is_temp(arg_reg)) {
                        self.pin_temp(arg_reg);
                        pinned = true;
                    }
                }
            }
            arg_regs_buf[idx] = arg_reg;
            arg_types_buf[idx] = arg_type;
            arg_pinned[idx] = pinned;
        }
        const arg_regs = arg_regs_buf[0..args.len];
        const arg_types = arg_types_buf[0..args.len];

        const reg = try self.compile_trait_method_call_regs(trait_name, method_name, recv_reg, arg_regs, arg_types, recv_concrete);

        for (arg_regs, 0..) |arg_reg, idx| {
            if (arg_pinned[idx]) self.unpin_temp(arg_reg);
            self.free_temp_value(arg_reg, arg_types[idx]);
        }
        if (recv_pinned) self.unpin_temp(recv_reg);
        self.free_temp_value(recv_reg, recv_type);
        return reg;
    }

    fn compile_call(self: *function_ctx, id: mir_mod.mir_identifier) lower_error!u8 {
        var base_id = id;
        var args = std.array_list.Managed(mir_mod.mir_identifier).init(self.b.allocator);
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

        std.mem.reverse(mir_mod.mir_identifier, args.items);

        if (args.items.len == 1 and self.is_unit(args.items[0])) {
            args.clearRetainingCapacity();
        }

        const base = try self.resolve_call_base(base_id);
        var arg_ids = args.items;
        var arg_ids_buf: [8]mir_mod.mir_identifier = undefined;
        if (base.receiver) |recv| {
            if (arg_ids.len + 1 > arg_ids_buf.len) return error.register_overflow;
            arg_ids_buf[0] = recv;
            std.mem.copyForwards(mir_mod.mir_identifier, arg_ids_buf[1 .. arg_ids.len + 1], arg_ids);
            arg_ids = arg_ids_buf[0 .. arg_ids.len + 1];
        }

        const name = base.name;

        var arg_types: [8]type_key = undefined;
        for (arg_ids, 0..) |arg_id, idx| {
            arg_types[idx] = self.infer_expr_type(arg_id);
        }
        const arg_type_slice = arg_types[0..arg_ids.len];

        if (base.receiver) |recv| {
            const recv_type = self.infer_expr_type(recv);
            if (self.trait_name_from_type(recv_type)) |type_name| {
                const other_args = arg_ids[1..];
                return self.compile_trait_method_call(type_name, name, recv, other_args) catch |err| switch (err) {
                    error.unknown_function => {
                        self.record_call_error(id, "unknown method ", name, arg_type_slice, true);
                        return err;
                    },
                    error.ambiguous_overload => {
                        self.record_call_error(id, "ambiguous overload for ", name, arg_type_slice, true);
                        return err;
                    },
                    else => return err,
                };
            }
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

        if (self.b.foreigns.contains(name)) {
            if (self.b.foreign_overloads.get(name)) |overloads| {
                if (overloads.items.len > 1) {
                    const selected_idx = self.resolve_foreign_overload_index(overloads.items, arg_type_slice) catch |err| switch (err) {
                        error.unknown_function, error.unknown_foreign => {
                            self.record_call_error(id, "no matching overload for ", name, arg_type_slice, base.receiver != null);
                            return err;
                        },
                        error.ambiguous_overload => {
                            self.record_call_error(id, "ambiguous overload for ", name, arg_type_slice, base.receiver != null);
                            return err;
                        },
                        else => return err,
                    };
                    const foreign_idx = try self.foreign_index(overloads.items[selected_idx].name);
                    var arg_regs: [8]u8 = undefined;
                    for (arg_ids, 0..) |arg_id, idx| {
                        arg_regs[idx] = try self.compile_expr(arg_id);
                    }
                    _ = try self.emit_argument_values(arg_regs[0..arg_ids.len], arg_type_slice, 1);
                    try self.emit(.{ .call_foreign = .{ .index = foreign_idx } });
                    for (arg_regs[0..arg_ids.len], 0..) |arg_reg, idx| {
                        self.free_temp_value(arg_reg, arg_type_slice[idx]);
                    }
                    return self.save_result_reg(0);
                }
            }

            const foreign_idx = try self.foreign_index(name);
            var arg_regs: [8]u8 = undefined;
            for (arg_ids, 0..) |arg_id, idx| {
                arg_regs[idx] = try self.compile_expr(arg_id);
            }
            _ = try self.emit_argument_values(arg_regs[0..arg_ids.len], arg_type_slice, 1);
            try self.emit(.{ .call_foreign = .{ .index = foreign_idx } });
            for (arg_regs[0..arg_ids.len], 0..) |arg_reg, idx| {
                self.free_temp_value(arg_reg, arg_type_slice[idx]);
            }
            return self.save_result_reg(0);
        }

        const group = self.b.functions.get(name) orelse {
            const prefix = if (base.receiver != null) "unknown method " else "unknown function ";
            self.record_call_error(id, prefix, name, arg_type_slice, base.receiver != null);
            return error.unknown_function;
        };
        const resolved = self.resolve_function_overload(group.items, arg_type_slice) catch |err| switch (err) {
            error.unknown_function => {
                self.record_call_error(id, "no matching overload for ", name, arg_type_slice, base.receiver != null);
                return err;
            },
            error.ambiguous_overload => {
                self.record_call_error(id, "ambiguous overload for ", name, arg_type_slice, base.receiver != null);
                return err;
            },
            else => return err,
        };
        defer if (resolved.bindings.len > 0) self.b.allocator.free(resolved.bindings);
        const info = resolved.info;

        var arg_regs: [8]u8 = undefined;
        for (arg_ids, 0..) |arg_id, idx| {
            arg_regs[idx] = try self.compile_expr(arg_id);
        }

        var base_return = if (info.decl.return_type) |ret_id|
            type_key_from_type_node_with_self(self.b, ret_id, info.impl_for)
        else
            type_key{ .name = "unit" };
        if (resolved.bindings.len > 0) {
            base_return = apply_bindings_to_type_key(self.b, base_return, resolved.bindings);
        }
        const return_words = self.type_word_count(base_return);

        var result_base: ?u8 = null;
        var sret_ptr: ?u8 = null;
        var dst_start: u8 = 1;
        if (return_words > 1) {
            const base_reg = try self.alloc_temp_words(return_words);
            const ptr_reg = try self.ptr_of_reg(base_reg);
            result_base = base_reg;
            sret_ptr = ptr_reg;
            dst_start = 2;
            try self.emit(.{ .argument_set = .{ .dst = 1, .src = ptr_reg } });
        }

        _ = try self.emit_argument_values(arg_regs[0..arg_ids.len], arg_type_slice, dst_start);

        const target = try self.b.ensure_instance(info, resolved.bindings);
        try self.emit(.{ .call = .{ .target = target } });

        if (sret_ptr) |ptr| if (self.is_temp(ptr)) self.free_temp(ptr);
        for (arg_regs[0..arg_ids.len], 0..) |arg_reg, idx| {
            self.free_temp_value(arg_reg, arg_type_slice[idx]);
        }

        if (result_base) |result_reg| return result_reg;
        return self.save_result_reg(0);
    }

    fn struct_field_index(info: struct_info, name: mir_mod.string_identifier) ?usize {
        for (info.fields, 0..) |field, idx| {
            if (field.name.idx == name.idx) return idx;
        }
        return null;
    }

    fn compile_record_literal(self: *function_ctx, rec: mir_mod.record_literal) lower_error!u8 {
        const type_name = self.b.string_value(rec.type_name);
        const info = self.b.structs.get(type_name) orelse return error.unsupported_node;
        const field_count = info.fields.len;
        const alloc_word_count = word_count_for_type(self.b, .{ .name = type_name });
        const base_reg = try self.alloc_temp_words(alloc_word_count);

        var seen = self.b.allocator.alloc(bool, field_count) catch return error.out_of_memory;
        defer self.b.allocator.free(seen);
        @memset(seen, false);

        for (rec.fields) |field| {
            const field_idx = struct_field_index(info, field.name) orelse return error.unsupported_node;
            if (seen[field_idx]) return error.unsupported_node;
            seen[field_idx] = true;

            const layout = self.struct_field_layout(type_name, field.name) orelse return error.unsupported_node;
            const field_base = base_reg + layout.offset;

            const value_reg = try self.compile_expr(field.value);
            if (layout.words <= 1) {
                if (value_reg != field_base) {
                    try self.emit(.{ .move = .{ .dst = field_base, .src = value_reg } });
                }
                if (self.is_temp(value_reg) and value_reg != field_base) self.free_temp(value_reg);
            } else {
                try self.copy_words(field_base, value_reg, layout.words);
                if (self.is_temp(value_reg)) self.free_temp_words(value_reg, layout.words);
            }
        }

        for (seen) |hit| {
            if (!hit and field_count > 0) return error.unsupported_node;
        }

        return base_reg;
    }

    fn compile_access_ptr(
        self: *function_ctx,
        left: mir_mod.mir_identifier,
        right: mir_mod.mir_identifier,
    ) lower_error!u8 {
        const field_node = self.b.node(right);
        if (field_node != .identifier) return error.unsupported_node;
        const field_id = field_node.identifier;
        const base_type = self.infer_expr_type(left);
        const struct_name = switch (base_type) {
            .name => |name| name,
            .applied => |ap| ap.base,
            else => return error.unsupported_node,
        };
        const layout = self.struct_field_layout(struct_name, field_id) orelse return error.unsupported_node;

        const base_reg = try self.compile_expr(left);
        const field_reg = base_reg + layout.offset;
        const ptr_reg = try self.ptr_of_reg(field_reg);
        if (self.is_temp(base_reg)) {
            const base_words = self.type_word_count(base_type);
            self.free_temp_words(base_reg, base_words);
        }
        return ptr_reg;
    }

    fn compile_borrow_expr(self: *function_ctx, target: mir_mod.mir_identifier) lower_error!u8 {
        const node = self.b.node(target);
        switch (node) {
            .identifier => |ident| {
                const name = self.b.string_value(ident);
                const reg = self.locals.get(name) orelse return error.unknown_identifier;
                const reg_idx = try self.b.intern_const(@intCast(reg));
                const reg_val = try self.alloc_temp();
                try self.emit(.{ .load_const = .{ .dst = reg_val, .const_index = reg_idx } });
                const foreign_idx = try self.foreign_index("std::ptr_of");
                var args = [_]u8{reg_val};
                try self.emit_foreign_call(foreign_idx, args[0..]);
                if (self.is_temp(reg_val)) self.free_temp(reg_val);
                return self.save_result_reg(0);
            },
            .binary => |bin| switch (bin.op) {
                .access => return self.compile_access_ptr(bin.left, bin.right),
                .index => return self.compile_index_ptr(bin.left, bin.right),
                else => return error.unsupported_node,
            },
            else => return error.unsupported_node,
        }
    }

    fn compile_deref_expr(
        self: *function_ctx,
        target: mir_mod.mir_identifier,
        result_id: mir_mod.mir_identifier,
    ) lower_error!u8 {
        var ptr_reg = try self.compile_expr(target);
        if (ptr_reg == 0) ptr_reg = try self.save_result_reg(ptr_reg);

        const result_type = self.infer_expr_type(result_id);
        const result_words = self.type_word_count(result_type);
        if (result_words > 1) {
            const dst_reg = try self.alloc_temp_words(result_words);
            try self.load_words_from_ptr(ptr_reg, dst_reg, result_words);
            if (self.is_temp(ptr_reg)) self.free_temp(ptr_reg);
            return dst_reg;
        }

        const reg = try self.deref_to_temp(ptr_reg);
        if (self.is_temp(ptr_reg)) self.free_temp(ptr_reg);
        return reg;
    }

    fn compile_box_expr(self: *function_ctx, value_id: mir_mod.mir_identifier) lower_error!u8 {
        var value_reg = try self.compile_expr(value_id);
        if (value_reg == 0) value_reg = try self.save_result_reg(value_reg);
        const value_type = self.infer_expr_type(value_id);
        const words = self.type_word_count(value_type);
        const ptr_reg = try self.alloc_words(words);
        try self.store_words_to_ptr(ptr_reg, value_reg, words);
        if (self.is_temp(value_reg)) self.free_temp_words(value_reg, words);
        return ptr_reg;
    }

    fn compile_access(self: *function_ctx, left: mir_mod.mir_identifier, right: mir_mod.mir_identifier) lower_error!u8 {
        const field_node = self.b.node(right);
        if (field_node != .identifier) return error.unsupported_node;
        const field_id = field_node.identifier;
        const base_type = self.infer_expr_type(left);
        const struct_name = switch (base_type) {
            .name => |name| name,
            .applied => |ap| ap.base,
            else => return error.unsupported_node,
        };
        const layout = self.struct_field_layout(struct_name, field_id) orelse return error.unsupported_node;

        const base_reg = try self.compile_expr(left);
        const base_words = self.type_word_count(base_type);
        const field_reg = base_reg + layout.offset;

        if (!self.is_temp(base_reg)) {
            return field_reg;
        }

        if (layout.words <= 1) {
            const dst = try self.alloc_temp();
            try self.emit(.{ .move = .{ .dst = dst, .src = field_reg } });
            self.free_temp_words(base_reg, base_words);
            return dst;
        }

        const dst = try self.alloc_temp_words(layout.words);
        try self.copy_words(dst, field_reg, layout.words);
        self.free_temp_words(base_reg, base_words);
        return dst;
    }

    fn compile_index_expr(
        self: *function_ctx,
        left: mir_mod.mir_identifier,
        right: mir_mod.mir_identifier,
        node_id: mir_mod.mir_identifier,
    ) lower_error!u8 {
        var base_reg = try self.compile_expr(left);
        if (base_reg == 0) base_reg = try self.save_result_reg(base_reg);

        const base_type = self.infer_expr_type(left);
        const elem_type = element_type_from_container(base_type) orelse .unknown;
        const elem_words = self.type_word_count(elem_type);
        var data_reg = base_reg;
        var base_owner_reg: ?u8 = null;
        if (type_key_base_name(base_type)) |name| {
            if (std.mem.eql(u8, name, "slice")) {
                data_reg = try self.deref_to_temp(base_reg);
                if (self.is_temp(base_reg)) self.free_temp(base_reg);
            } else if (std.mem.eql(u8, name, "array")) {
                data_reg = try self.ptr_of_reg(base_reg);
                if (self.is_temp(base_reg)) base_owner_reg = base_reg;
            } else if (self.trait_name_from_type(base_type) != null or self.is_known_non_builtin(base_type)) {
                const maybe_reg = self.try_compile_method_call("index", left, &.{right}) catch |err| switch (err) {
                    error.unknown_function => {
                        self.record_call_error(node_id, "unknown method ", "index", &.{ base_type, self.infer_expr_type(right) }, true);
                        return err;
                    },
                    else => return err,
                };
                if (maybe_reg) |reg| {
                    return reg;
                }
                if (self.is_known_non_builtin(base_type)) {
                    self.record_call_error(node_id, "no matching overload for ", "index", &.{ base_type, self.infer_expr_type(right) }, true);
                    return error.unknown_function;
                }
                return error.unsupported_node;
            } else {
                return error.unsupported_node;
            }
        } else if (self.trait_name_from_type(base_type) != null or self.is_known_non_builtin(base_type)) {
            const maybe_reg = self.try_compile_method_call("index", left, &.{right}) catch |err| switch (err) {
                error.unknown_function => {
                    self.record_call_error(node_id, "unknown method ", "index", &.{ base_type, self.infer_expr_type(right) }, true);
                    return err;
                },
                else => return err,
            };
            if (maybe_reg) |reg| {
                return reg;
            }
            if (self.is_known_non_builtin(base_type)) {
                self.record_call_error(node_id, "no matching overload for ", "index", &.{ base_type, self.infer_expr_type(right) }, true);
                return error.unknown_function;
            }
            return error.unsupported_node;
        } else {
            return error.unsupported_node;
        }

        var index_reg = try self.compile_expr(right);
        if (index_reg == 0) index_reg = try self.save_result_reg(index_reg);

        const ptr_reg = if (elem_words > 1)
            if (self.is_temp(index_reg)) index_reg else try self.alloc_temp()
        else if (self.is_temp(data_reg))
            data_reg
        else if (self.is_temp(index_reg))
            index_reg
        else
            try self.alloc_temp();
        if (elem_words > 1) {
            const scale_idx = try self.b.intern_const(elem_words);
            const scale_reg = try self.alloc_temp();
            try self.emit(.{ .load_const = .{ .dst = scale_reg, .const_index = scale_idx } });
            try self.emit(.{ .mul = .{ .dst = ptr_reg, .src_a = index_reg, .src_b = scale_reg } });
            if (self.is_temp(scale_reg)) self.free_temp(scale_reg);
            try self.emit(.{ .add = .{ .dst = ptr_reg, .src_a = data_reg, .src_b = ptr_reg } });
        } else {
            try self.emit(.{ .add = .{ .dst = ptr_reg, .src_a = data_reg, .src_b = index_reg } });
        }

        if (self.is_temp(data_reg) and ptr_reg != data_reg) {
            self.free_temp(data_reg);
        }
        if (base_owner_reg) |owner| {
            if (owner != ptr_reg and owner != data_reg) {
                self.free_temp_value(owner, base_type);
            }
        }
        if (self.is_temp(index_reg) and ptr_reg != index_reg) {
            self.free_temp_value(index_reg, self.infer_expr_type(right));
        }

        if (elem_words > 1) {
            const dst_reg = try self.alloc_temp_words(elem_words);
            try self.load_words_from_ptr(ptr_reg, dst_reg, elem_words);
            if (self.is_temp(ptr_reg)) self.free_temp(ptr_reg);
            return dst_reg;
        }

        _ = try self.emit_foreign_call_reg("std::deref", ptr_reg);
        if (self.is_temp(ptr_reg)) self.free_temp(ptr_reg);
        return self.save_result_reg(0);
    }

    fn compile_index_ptr(
        self: *function_ctx,
        left: mir_mod.mir_identifier,
        right: mir_mod.mir_identifier,
    ) lower_error!u8 {
        var base_reg = try self.compile_expr(left);
        if (base_reg == 0) base_reg = try self.save_result_reg(base_reg);

        const base_type = self.infer_expr_type(left);
        const elem_type = element_type_from_container(base_type) orelse .unknown;
        const elem_words = self.type_word_count(elem_type);
        var data_reg = base_reg;
        var base_owner_reg: ?u8 = null;
        if (type_key_base_name(base_type)) |name| {
            if (std.mem.eql(u8, name, "slice")) {
                data_reg = try self.deref_to_temp(base_reg);
                if (self.is_temp(base_reg)) self.free_temp(base_reg);
            } else if (std.mem.eql(u8, name, "array")) {
                data_reg = try self.ptr_of_reg(base_reg);
                if (self.is_temp(base_reg)) base_owner_reg = base_reg;
            } else {
                return error.unsupported_node;
            }
        } else {
            return error.unsupported_node;
        }

        var index_reg = try self.compile_expr(right);
        if (index_reg == 0) index_reg = try self.save_result_reg(index_reg);

        const ptr_reg = if (elem_words > 1)
            if (self.is_temp(index_reg)) index_reg else try self.alloc_temp()
        else if (self.is_temp(data_reg))
            data_reg
        else if (self.is_temp(index_reg))
            index_reg
        else
            try self.alloc_temp();
        if (elem_words > 1) {
            const scale_idx = try self.b.intern_const(elem_words);
            const scale_reg = try self.alloc_temp();
            try self.emit(.{ .load_const = .{ .dst = scale_reg, .const_index = scale_idx } });
            try self.emit(.{ .mul = .{ .dst = ptr_reg, .src_a = index_reg, .src_b = scale_reg } });
            if (self.is_temp(scale_reg)) self.free_temp(scale_reg);
            try self.emit(.{ .add = .{ .dst = ptr_reg, .src_a = data_reg, .src_b = ptr_reg } });
        } else {
            try self.emit(.{ .add = .{ .dst = ptr_reg, .src_a = data_reg, .src_b = index_reg } });
        }

        if (self.is_temp(data_reg) and ptr_reg != data_reg) {
            self.free_temp(data_reg);
        }
        if (base_owner_reg) |owner| {
            if (owner != ptr_reg and owner != data_reg) {
                self.free_temp_value(owner, base_type);
            }
        }
        if (self.is_temp(index_reg) and ptr_reg != index_reg) {
            self.free_temp_value(index_reg, self.infer_expr_type(right));
        }

        return ptr_reg;
    }

    fn compile_cancel_call(self: *function_ctx, args: []const mir_mod.mir_identifier) lower_error!u8 {
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
        .alloc, .deref => .{ .name = "int" },
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

pub fn lower(
    allocator: std.mem.Allocator,
    nodes: []const mir_mod.mir,
    strings: []const []const u8,
    roots: []const mir_mod.mir_identifier,
    foreigns: []const []const u8,
    node_types: ?[]const type_key,
    error_state: ?*error_info,
) lower_error!core.program {
    return lower_with_options(allocator, nodes, strings, roots, foreigns, node_types, error_state, .{});
}

pub fn lower_with_options(
    allocator: std.mem.Allocator,
    nodes: []const mir_mod.mir,
    strings: []const []const u8,
    roots: []const mir_mod.mir_identifier,
    foreigns: []const []const u8,
    node_types: ?[]const type_key,
    error_state: ?*error_info,
    options: lower_options,
) lower_error!core.program {
    var b = builder.init(allocator, nodes, strings, roots, node_types, error_state);
    defer b.deinit();
    add_builtin_traits(&b);
    for (roots) |root| {
        const node = nodes[@intCast(root.idx)];
        switch (node) {
            .decl => |decl| switch (decl) {
                .@"const" => |c| {
                    const name = b.string_value(c.name);
                    if (!b.global_consts.contains(name)) {
                        b.global_consts.put(name, .{ .value = c.value, .ty = c.ty }) catch return error.out_of_memory;
                    }
                },
                else => {},
            },
            else => {},
        }
    }

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
                        const fields = b.allocator.alloc(mir_mod.mir.struct_decl.field, st.fields.len) catch return error.out_of_memory;
                        std.mem.copyForwards(mir_mod.mir.struct_decl.field, fields, st.fields);
                        b.structs.put(name, .{
                            .fields = fields,
                            .is_record = st.is_record,
                            .generics = st.generics,
                        }) catch return error.out_of_memory;
                    }
                },
                .trait => |tr| {
                    const trait_name = b.string_value(tr.name);
                    if (!b.traits.contains(trait_name)) {
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
                        var reqs = std.array_list.Managed(trait_constraint).init(allocator);
                        for (tr.requires) |req_id| {
                            const constraint = constraint_from_type_node(&b, req_id) orelse continue;
                            reqs.append(constraint) catch return error.out_of_memory;
                        }
                        const req_slice = reqs.toOwnedSlice() catch return error.out_of_memory;
                        b.traits.put(trait_name, .{
                            .methods = method_slice,
                            .requires = req_slice,
                            .is_auto = tr.is_auto,
                        }) catch return error.out_of_memory;
                    }
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
                    if (impl.negative) {
                        if (impl.functions.len != 0) return error.unsupported_node;
                        var neg_list = b.trait_neg_impls.getPtr(trait_name);
                        if (neg_list == null) {
                            var list = std.array_list.Managed([]const u8).init(allocator);
                            list.append(type_name) catch return error.out_of_memory;
                            b.trait_neg_impls.put(trait_name, list) catch return error.out_of_memory;
                        } else if (!string_list_contains(neg_list.?.items, type_name)) {
                            neg_list.?.append(type_name) catch return error.out_of_memory;
                        }
                    } else {
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

    if (options.signatures) |sigs| {
        for (functions.items) |info| {
            const name = b.string_value(info.decl.name);
            const duped = b.allocator.dupe(u8, name) catch return error.out_of_memory;
            sigs.append(.{
                .name = duped,
                .label = info.label,
                .param_count = info.decl.params.len,
                .is_method = info.impl_for != null,
            }) catch return error.out_of_memory;
        }
    }

    if (options.require_main) {
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
    }

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
        var ctx = try function_ctx.init(&b, info.decl.params, info.impl_for, instance.bindings, info.decl.return_type);
        defer ctx.deinit();

        if (info.decl.body) |body_ref| {
            const result_reg = try ctx.compile_expr(body_ref);
            if (!ctx.returned) {
                try ctx.emit_return_value(result_reg, ctx.return_type);
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
