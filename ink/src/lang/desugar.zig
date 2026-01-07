const std = @import("std");
const ink = @import("ink");
const source = @import("../source.zig");

pub const mem_allocator = std.mem.Allocator;
const array_list = std.array_list.Managed;
const string_map = std.hash_map.StringHashMap;
const diagnostic = ink.diagnostic;
const desugar_error = error{OutOfMemory};

pub const import_decl = struct {
    module: ink.identifier,
    item: ?ink.identifier = null,
    alias: ?ink.identifier = null,
};

const symbol_import = struct {
    module: []const u8,
    item: []const u8,
};

pub const origin_map = std.AutoHashMap(*const ink.node, *const ink.node);

pub const result = struct {
    nodes: []const *ink.node,
    imports: []const import_decl,
    origin: origin_map,
};

const desugarer = struct {
    const bit_alias_info = struct {
        base: []const u8,
        bits: i64,
        where: ink.location,
    };
    const operator_target = struct {
        name: []const u8,
        where: ink.location,
    };

    node_allocator: mem_allocator,
    allocator: mem_allocator,
    diags: *array_list(diagnostic),
    imports: array_list(import_decl),
    import_map: string_map([]const u8),
    module_map: string_map([]const u8),
    symbol_imports: string_map(symbol_import),
    origin: origin_map,
    operator_unary: std.AutoHashMap(ink.unary, operator_target),
    operator_binary: std.AutoHashMap(ink.binary, operator_target),
    with_counter: usize,

    fn init(node_allocator: mem_allocator, allocator: mem_allocator, diags: *array_list(diagnostic)) desugarer {
        return .{
            .node_allocator = node_allocator,
            .allocator = allocator,
            .diags = diags,
            .imports = array_list(import_decl).init(allocator),
            .import_map = string_map([]const u8).init(allocator),
            .module_map = string_map([]const u8).init(allocator),
            .symbol_imports = string_map(symbol_import).init(allocator),
            .origin = origin_map.init(allocator),
            .operator_unary = std.AutoHashMap(ink.unary, operator_target).init(allocator),
            .operator_binary = std.AutoHashMap(ink.binary, operator_target).init(allocator),
            .with_counter = 0,
        };
    }

    fn deinit(self: *desugarer) void {
        self.imports.deinit();
        self.import_map.deinit();
        self.module_map.deinit();
        self.symbol_imports.deinit();
        self.operator_unary.deinit();
        self.operator_binary.deinit();
    }

    fn add_error(self: *desugarer, message: []const u8, where: ?ink.location) desugar_error!void {
        const span = if (where) |loc| source.span{ .start = loc.start, .end = loc.end } else null;
        try self.diags.append(.{ .danger = .@"error", .message = message, .span = span });
    }

    fn add_import(self: *desugarer, imp: ink.ast.import_decl) desugar_error!void {
        if (imp.item) |item| {
            const alias = imp.alias orelse item;
            if (self.import_map.contains(alias.string) or self.symbol_imports.contains(alias.string)) {
                try self.add_error("duplicate import alias", alias.where);
                return;
            }
            try self.symbol_imports.put(alias.string, .{ .module = imp.module.string, .item = item.string });
        } else {
            const alias = imp.alias orelse imp.module;
            if (self.import_map.contains(alias.string) or self.symbol_imports.contains(alias.string)) {
                try self.add_error("duplicate import alias", alias.where);
                return;
            }
            if (self.module_map.contains(imp.module.string)) {
                try self.add_error("duplicate import", imp.module.where);
                return;
            }
            try self.import_map.put(alias.string, imp.module.string);
            try self.module_map.put(imp.module.string, alias.string);
        }
        try self.imports.append(.{ .module = imp.module, .item = imp.item, .alias = imp.alias });
    }

    fn collect_imports(self: *desugarer, nodes: []const *ink.node) desugar_error!void {
        for (nodes) |node| {
            if (import_decl_from(node)) |imp| {
                try self.add_import(imp);
            }
        }
    }

    fn collect_operator_overloads(self: *desugarer, nodes: []const *ink.node) desugar_error!void {
        for (nodes) |node| {
            if (node.* != .decl) continue;
            if (node.decl == .function) {
                try self.collect_operator_overload(node.decl.function);
            }
        }
    }

    fn collect_operator_overload(self: *desugarer, func: ink.ast.function_decl) desugar_error!void {
        if (func.attributes.len == 0) return;
        for (func.attributes) |attr| {
            if (!std.mem.eql(u8, attr.name.string, "operator")) continue;
            try self.register_operator_overload(attr, func);
        }
    }

    fn register_operator_overload(
        self: *desugarer,
        attr: ink.ast.attribute,
        func: ink.ast.function_decl,
    ) desugar_error!void {
        if (attr.args.len == 0) {
            try self.add_error("operator attribute requires a symbol", attr.name.where);
            return;
        }

        const symbol = self.operator_symbol(attr.args[0]) orelse {
            try self.add_error("operator attribute expects a string or identifier", attr.name.where);
            return;
        };

        if (func.params.len == 1) {
            const op = unary_from_symbol(symbol) orelse {
                try self.add_error("unsupported unary operator overload", attr.name.where);
                return;
            };
            if (self.operator_unary.contains(op)) {
                try self.add_error("duplicate unary operator overload", attr.name.where);
                return;
            }
            try self.operator_unary.put(op, .{ .name = func.name.string, .where = attr.name.where });
            return;
        }

        if (func.params.len == 2) {
            const op = binary_from_symbol(symbol) orelse {
                try self.add_error("unsupported binary operator overload", attr.name.where);
                return;
            };
            if (self.operator_binary.contains(op)) {
                try self.add_error("duplicate binary operator overload", attr.name.where);
                return;
            }
            try self.operator_binary.put(op, .{ .name = func.name.string, .where = attr.name.where });
            return;
        }

        try self.add_error("operator overload functions must take 1 or 2 params", func.name.where);
    }

    fn operator_symbol(self: *desugarer, ref: ink.ast.node_ref) ?[]const u8 {
        _ = self;
        const node = ink.ast.deref(ref);
        return switch (node.*) {
            .identifier => |id| id.string,
            .string => |id| id.string,
            else => null,
        };
    }

    fn unary_from_symbol(symbol: []const u8) ?ink.unary {
        if (std.mem.eql(u8, symbol, "-")) return .neg;
        if (std.mem.eql(u8, symbol, "!")) return .not;
        if (std.mem.eql(u8, symbol, "not")) return .not;
        if (std.mem.eql(u8, symbol, "~")) return .bit_not;
        return null;
    }

    fn binary_from_symbol(symbol: []const u8) ?ink.binary {
        if (std.mem.eql(u8, symbol, "+")) return .add;
        if (std.mem.eql(u8, symbol, "-")) return .sub;
        if (std.mem.eql(u8, symbol, "*")) return .mul;
        if (std.mem.eql(u8, symbol, "/")) return .div;
        if (std.mem.eql(u8, symbol, "%")) return .mod;
        if (std.mem.eql(u8, symbol, "&")) return .bit_and;
        if (std.mem.eql(u8, symbol, "|")) return .bit_or;
        if (std.mem.eql(u8, symbol, "^")) return .bit_xor;
        if (std.mem.eql(u8, symbol, "<<")) return .shl;
        if (std.mem.eql(u8, symbol, ">>")) return .shr;
        if (std.mem.eql(u8, symbol, "==")) return .equal;
        if (std.mem.eql(u8, symbol, "!=")) return .not_equal;
        if (std.mem.eql(u8, symbol, "<")) return .less_than;
        if (std.mem.eql(u8, symbol, "<=")) return .less_or_equal;
        if (std.mem.eql(u8, symbol, ">")) return .greater_than;
        if (std.mem.eql(u8, symbol, ">=")) return .greater_or_equal;
        if (std.mem.eql(u8, symbol, "|>")) return .pipe;
        if (std.mem.eql(u8, symbol, "??")) return .coalesce;
        if (std.mem.eql(u8, symbol, "and")) return .logical_and;
        if (std.mem.eql(u8, symbol, "or")) return .logical_or;
        if (std.mem.eql(u8, symbol, "xor")) return .logical_xor;
        return null;
    }

    fn desugar_nodes(self: *desugarer, nodes: []const *ink.node) desugar_error![]const *ink.node {
        var out = array_list(*ink.node).init(self.allocator);
        for (nodes) |node| {
            if (import_decl_from(node)) |_| {
                continue;
            }
            try out.append(try self.desugar_node(node));
        }
        return out.toOwnedSlice();
    }

    fn desugar_node(self: *desugarer, node: *ink.node) desugar_error!*ink.node {
        switch (node.*) {
            .integer, .float, .duration, .string => return node,
            .identifier => |id| {
                if (id.owner == .ref) {
                    if (self.symbol_imports.get(id.string)) |sym| {
                        const qualified = try self.qualify_name(sym.module, sym.item);
                        node.* = .{ .identifier = .{
                            .string = qualified,
                            .owner = id.owner,
                            .where = id.where,
                        } };
                    }
                }
                return node;
            },
            .unary => |un| {
                const right = try self.desugar_node(ink.ast.deref(un.right));
                if (self.operator_unary.get(un.op)) |target| {
                    const loc = node_location(right);
                    const base = try self.new_identifier(node, target.name, loc);
                    var args = [_]*ink.node{right};
                    return self.call_with_args(node, base, args[0..]);
                }
                node.* = .{ .unary = .{
                    .op = un.op,
                    .right = ink.ast.ref(right),
                } };
                return node;
            },
            .binary => |bin| {
                const left = try self.desugar_node(ink.ast.deref(bin.left));
                const right = try self.desugar_node(ink.ast.deref(bin.right));
                if (bin.op == .scope_access) {
                    return self.desugar_scope_access(node, left, right);
                }
                if (self.operator_binary.get(bin.op)) |target| {
                    const loc = node_location(left);
                    const base = try self.new_identifier(node, target.name, loc);
                    var args = [_]*ink.node{ left, right };
                    return self.call_with_args(node, base, args[0..]);
                }
                node.* = .{ .binary = .{
                    .left = ink.ast.ref(left),
                    .op = bin.op,
                    .right = ink.ast.ref(right),
                } };
                return node;
            },
            .if_expr => |ife| {
                const condition = try self.desugar_node(ink.ast.deref(ife.condition));
                const then_branch = try self.desugar_node(ink.ast.deref(ife.then_branch));
                const else_branch = if (ife.else_branch) |ref| try self.desugar_node(ink.ast.deref(ref)) else null;
                node.* = .{ .if_expr = .{
                    .condition = ink.ast.ref(condition),
                    .then_branch = ink.ast.ref(then_branch),
                    .else_branch = ink.ast.ref_opt(else_branch),
                } };
                return node;
            },
            .label_expr => |le| {
                const body = try self.desugar_node(ink.ast.deref(le.body));
                node.* = .{ .label_expr = .{
                    .name = le.name,
                    .body = ink.ast.ref(body),
                } };
                return node;
            },
            .loop_expr => |le| {
                const body = try self.desugar_node(ink.ast.deref(le.body));
                node.* = .{ .loop_expr = .{ .body = ink.ast.ref(body) } };
                return node;
            },
            .while_expr => |we| {
                const condition = try self.desugar_node(ink.ast.deref(we.condition));
                const body = try self.desugar_node(ink.ast.deref(we.body));
                node.* = .{ .while_expr = .{
                    .condition = ink.ast.ref(condition),
                    .body = ink.ast.ref(body),
                } };
                return node;
            },
            .while_in_expr => |we| {
                const pattern = try self.desugar_node(ink.ast.deref(we.pattern));
                const iter = try self.desugar_node(ink.ast.deref(we.iter));
                const body = try self.desugar_node(ink.ast.deref(we.body));
                node.* = .{ .while_in_expr = .{
                    .pattern = ink.ast.ref(pattern),
                    .iter = ink.ast.ref(iter),
                    .body = ink.ast.ref(body),
                } };
                return node;
            },
            .until_expr => |ue| {
                const condition = try self.desugar_node(ink.ast.deref(ue.condition));
                const body = try self.desugar_node(ink.ast.deref(ue.body));
                node.* = .{ .until_expr = .{
                    .condition = ink.ast.ref(condition),
                    .body = ink.ast.ref(body),
                } };
                return node;
            },
            .repeat_expr => |re| {
                const count = try self.desugar_node(ink.ast.deref(re.count));
                const body = try self.desugar_node(ink.ast.deref(re.body));
                node.* = .{ .repeat_expr = .{
                    .count = ink.ast.ref(count),
                    .body = ink.ast.ref(body),
                } };
                return node;
            },
            .for_expr => |fe| {
                const pattern = try self.desugar_node(ink.ast.deref(fe.pattern));
                const iter = try self.desugar_node(ink.ast.deref(fe.iter));
                const body = try self.desugar_node(ink.ast.deref(fe.body));
                node.* = .{ .for_expr = .{
                    .pattern = ink.ast.ref(pattern),
                    .iter = ink.ast.ref(iter),
                    .body = ink.ast.ref(body),
                } };
                return node;
            },
            .each_expr => |ee| {
                const pattern = try self.desugar_node(ink.ast.deref(ee.pattern));
                const iter = try self.desugar_node(ink.ast.deref(ee.iter));
                const body = try self.desugar_node(ink.ast.deref(ee.body));
                node.* = .{ .each_expr = .{
                    .pattern = ink.ast.ref(pattern),
                    .iter = ink.ast.ref(iter),
                    .body = ink.ast.ref(body),
                } };
                return node;
            },
            .break_expr => |be| {
                const value = if (be.value) |ref| try self.desugar_node(ink.ast.deref(ref)) else null;
                node.* = .{ .break_expr = .{
                    .label = be.label,
                    .value = ink.ast.ref_opt(value),
                } };
                return node;
            },
            .continue_expr => |ce| {
                node.* = .{ .continue_expr = .{ .label = ce.label } };
                return node;
            },
            .yield_expr => |ye| {
                const value = if (ye.value) |ref| try self.desugar_node(ink.ast.deref(ref)) else null;
                node.* = .{ .yield_expr = .{ .value = ink.ast.ref_opt(value) } };
                return node;
            },
            .match_expr => |me| {
                const target = try self.desugar_node(ink.ast.deref(me.target));
                const arms = @constCast(me.arms);
                for (arms) |*arm| {
                    const pattern = ink.ast.deref(arm.pattern);
                    const body = try self.desugar_node(ink.ast.deref(arm.body));
                    arm.* = .{ .pattern = ink.ast.ref(pattern), .body = ink.ast.ref(body) };
                }
                node.* = .{ .match_expr = .{
                    .target = ink.ast.ref(target),
                    .arms = arms,
                } };
                return node;
            },
            .select_expr => |sel| {
                const arms = @constCast(sel.arms);
                for (arms) |*arm| {
                    const task = try self.desugar_node(ink.ast.deref(arm.task));
                    const body = try self.desugar_node(ink.ast.deref(arm.body));
                    arm.* = .{
                        .name = arm.name,
                        .task = ink.ast.ref(task),
                        .body = ink.ast.ref(body),
                        .detached = arm.detached,
                    };
                }
                node.* = .{ .select_expr = .{ .arms = arms } };
                return node;
            },
            .with_expr => |we| {
                return self.desugar_with_expr(node, we);
            },
            .block => |blk| {
                const rebuilt = try self.desugar_block(blk);
                node.* = .{ .block = rebuilt };
                return node;
            },
            .record => |rec| {
                return self.desugar_record(node, rec);
            },
            .intrinsic => |call| {
                const args = try self.desugar_node_refs(call.args);
                node.* = .{ .intrinsic = .{
                    .name = call.name,
                    .args = args,
                } };
                return node;
            },
            .atomic_expr => |ae| {
                const value = try self.desugar_node(ink.ast.deref(ae.value));
                node.* = .{ .atomic_expr = .{
                    .value = ink.ast.ref(value),
                    .ordering = ae.ordering,
                } };
                return node;
            },
            .associate => |assoc| {
                const value = if (assoc.value) |ref| try self.desugar_node(ink.ast.deref(ref)) else null;
                node.* = .{ .associate = .{
                    .name = assoc.name,
                    .value = ink.ast.ref_opt(value),
                } };
                return node;
            },
            .type => |ty| {
                node.* = .{ .type = try self.desugar_type_expr(ty) };
                return node;
            },
            .decl => |decl| {
                node.* = .{ .decl = try self.desugar_decl(decl) };
                return node;
            },
        }
    }

    fn desugar_node_refs(
        self: *desugarer,
        refs: []const ink.ast.node_ref,
    ) desugar_error![]const ink.ast.node_ref {
        var out = array_list(ink.ast.node_ref).init(self.node_allocator);
        for (refs) |ref| {
            const node = try self.desugar_node(ink.ast.deref(ref));
            try out.append(ink.ast.ref(node));
        }
        return out.toOwnedSlice();
    }

    fn desugar_attributes(
        self: *desugarer,
        attrs: []const ink.ast.attribute,
    ) desugar_error![]const ink.ast.attribute {
        if (attrs.len == 0) return &[_]ink.ast.attribute{};
        var out = array_list(ink.ast.attribute).init(self.node_allocator);
        for (attrs) |attr| {
            const args = try self.desugar_node_refs(attr.args);
            try out.append(.{ .name = attr.name, .args = args });
        }
        return out.toOwnedSlice();
    }

    fn desugar_block(self: *desugarer, blk: ink.ast.block_expr) desugar_error!ink.ast.block_expr {
        var items = array_list(*ink.node).init(self.node_allocator);
        for (blk.items) |item_ref| {
            const item = ink.ast.deref(item_ref);
            if (import_decl_from(item)) |imp| {
                try self.add_error("import only allowed at top level", imp.module.where);
                continue;
            }
            try items.append(try self.desugar_node(item));
        }
        const slice = try items.toOwnedSlice();
        return .{ .items = ink.ast.ref_slice(slice) };
    }

    fn desugar_with_expr(self: *desugarer, origin_node: *ink.node, we: ink.ast.with_expr) desugar_error!*ink.node {
        const body = try self.desugar_node(ink.ast.deref(we.body));
        const std_alias = self.module_map.get("std") orelse blk: {
            try self.add_error("with arena requires import std", we.name.where);
            break :blk "std";
        };
        const arena_new = try self.new_qualified_identifier(origin_node, std_alias, "arena_new", we.name.where);
        const arena_reset = try self.new_qualified_identifier(origin_node, std_alias, "arena_reset", we.name.where);
        const arena_deinit = try self.new_qualified_identifier(origin_node, std_alias, "arena_deinit", we.name.where);
        const zero = try self.new_integer(origin_node, 0, we.name.where);

        var arena_args = [_]*ink.node{zero};
        const arena_call = try self.call_with_args(origin_node, arena_new, arena_args[0..]);
        const arena_decl = try self.new_const_decl(origin_node, we.name, arena_call);

        const result_name = try self.new_temp_name("__with_result");
        const result_ident = ink.identifier{ .string = result_name, .owner = .ref, .where = we.name.where };
        const result_decl = try self.new_const_decl(origin_node, result_ident, body);

        const arena_ident = try self.new_identifier(origin_node, we.name.string, we.name.where);
        var reset_args = [_]*ink.node{arena_ident};
        const reset_call = try self.call_with_args(origin_node, arena_reset, reset_args[0..]);
        const arena_ident_again = try self.new_identifier(origin_node, we.name.string, we.name.where);
        var deinit_args = [_]*ink.node{arena_ident_again};
        const deinit_call = try self.call_with_args(origin_node, arena_deinit, deinit_args[0..]);
        const result_ref = try self.new_identifier(origin_node, result_name, we.name.where);

        var items = [_]*ink.node{ arena_decl, result_decl, reset_call, deinit_call, result_ref };
        const slice = try self.node_allocator.dupe(*ink.node, items[0..]);
        const block = ink.ast.block_expr{ .items = ink.ast.ref_slice(slice) };
        const node = try self.node_allocator.create(ink.node);
        node.* = .{ .block = block };
        try self.origin.put(node, origin_node);
        return node;
    }

    fn desugar_type_expr(self: *desugarer, ty: ink.ast.type_expr) desugar_error!ink.ast.type_expr {
        return switch (ty) {
            .self => ty,
            .name => |id| {
                if (id.owner == .ref) {
                    if (self.bit_alias(id)) |alias| {
                        return try self.desugar_bit_alias(id, alias);
                    }
                }
                if (id.owner == .ref) {
                    if (self.symbol_imports.get(id.string)) |sym| {
                        const qualified = try self.qualify_name(sym.module, sym.item);
                        return .{ .name = .{
                            .string = qualified,
                            .owner = id.owner,
                            .where = id.where,
                        } };
                    }
                }
                return ty;
            },
            .optional => |ref| {
                const inner = try self.desugar_node(ink.ast.deref(ref));
                return .{ .optional = ink.ast.ref(inner) };
            },
            .dyn => |ref| {
                const inner = try self.desugar_node(ink.ast.deref(ref));
                return .{ .dyn = ink.ast.ref(inner) };
            },
            .applied => |ap| {
                const args = @constCast(ap.args);
                for (args) |*arg_ref| {
                    const arg = try self.desugar_node(ink.ast.deref(arg_ref.*));
                    arg_ref.* = ink.ast.ref(arg);
                }
                return .{ .applied = .{
                    .base = ap.base,
                    .args = args,
                } };
            },
        };
    }

    fn bit_alias(self: *desugarer, id: ink.identifier) ?bit_alias_info {
        _ = self;
        if (id.string.len < 2) return null;
        const prefix = id.string[0];
        if (prefix != 'i' and prefix != 'u' and prefix != 'b') return null;
        const digits = id.string[1..];
        var i: usize = 0;
        while (i < digits.len) : (i += 1) {
            if (digits[i] < '0' or digits[i] > '9') return null;
        }
        const bits = std.fmt.parseInt(i64, digits, 10) catch return null;
        if (bits <= 0) return null;
        const base = switch (prefix) {
            'i' => "int",
            'u' => "uint",
            'b' => "uint",
            else => return null,
        };
        return .{ .base = base, .bits = bits, .where = id.where };
    }

    fn desugar_bit_alias(
        self: *desugarer,
        id: ink.identifier,
        alias: bit_alias_info,
    ) desugar_error!ink.ast.type_expr {
        const int_node = try self.node_allocator.create(ink.node);
        int_node.* = .{ .integer = .{ .value = alias.bits, .where = alias.where } };

        const args = try self.node_allocator.alloc(*ink.node, 1);
        args[0] = int_node;

        const base = ink.identifier{
            .string = alias.base,
            .owner = .ref,
            .where = id.where,
        };

        return .{ .applied = .{
            .base = base,
            .args = ink.ast.ref_slice(args),
        } };
    }

    fn desugar_decl(self: *desugarer, decl: ink.ast.decl) desugar_error!ink.ast.decl {
        return switch (decl) {
            .function => |func| .{ .function = try self.desugar_function_decl(func) },
            .@"struct" => |st| .{ .@"struct" = try self.desugar_struct_decl(st) },
            .trait => |tr| .{ .trait = try self.desugar_trait_decl(tr) },
            .@"enum" => |e| .{ .@"enum" = try self.desugar_enum_decl(e) },
            .impl => |im| .{ .impl = try self.desugar_impl_decl(im) },
            .import => |imp| .{ .import = try self.desugar_import_decl(imp) },
            .type_alias => |t| .{ .type_alias = try self.desugar_type_alias_decl(t) },
            .@"const" => |c| .{ .@"const" = try self.desugar_const_decl(c) },
            .@"var" => |v| .{ .@"var" = try self.desugar_var_decl(v) },
        };
    }

    fn desugar_function_decl(self: *desugarer, func: ink.ast.function_decl) desugar_error!ink.ast.function_decl {
        const attributes = try self.desugar_attributes(func.attributes);
        const generics = @constCast(func.generics);
        for (generics) |*param| {
            if (param.constraint) |ref| {
                const constraint = try self.desugar_node(ink.ast.deref(ref));
                param.constraint = ink.ast.ref(constraint);
            }
            if (param.default) |ref| {
                const def = try self.desugar_node(ink.ast.deref(ref));
                param.default = ink.ast.ref(def);
            }
        }

        const params = @constCast(func.params);
        for (params) |*param| {
            const ty = try self.desugar_node(ink.ast.deref(param.ty));
            param.ty = ink.ast.ref(ty);
        }

        const where_clause = @constCast(func.where_clause);
        for (where_clause) |*req| {
            const constraint = try self.desugar_node(ink.ast.deref(req.constraint));
            req.constraint = ink.ast.ref(constraint);
        }

        const return_type = if (func.return_type) |ref| try self.desugar_node(ink.ast.deref(ref)) else null;
        const body = if (func.body) |ref| try self.desugar_node(ink.ast.deref(ref)) else null;

        return .{
            .attributes = attributes,
            .name = func.name,
            .generics = generics,
            .params = params,
            .return_type = ink.ast.ref_opt(return_type),
            .where_clause = where_clause,
            .body = ink.ast.ref_opt(body),
        };
    }

    fn desugar_struct_decl(self: *desugarer, st: ink.ast.struct_decl) desugar_error!ink.ast.struct_decl {
        const attributes = try self.desugar_attributes(st.attributes);
        const generics = @constCast(st.generics);
        for (generics) |*param| {
            if (param.constraint) |ref| {
                const constraint = try self.desugar_node(ink.ast.deref(ref));
                param.constraint = ink.ast.ref(constraint);
            }
            if (param.default) |ref| {
                const def = try self.desugar_node(ink.ast.deref(ref));
                param.default = ink.ast.ref(def);
            }
        }

        const fields = @constCast(st.fields);
        for (fields) |*field| {
            const ty = try self.desugar_node(ink.ast.deref(field.ty));
            field.ty = ink.ast.ref(ty);
        }

        return .{
            .attributes = attributes,
            .name = st.name,
            .generics = generics,
            .fields = fields,
        };
    }

    fn desugar_type_alias_decl(self: *desugarer, t: ink.ast.type_decl) desugar_error!ink.ast.type_decl {
        const attributes = try self.desugar_attributes(t.attributes);
        const generics = @constCast(t.generics);
        for (generics) |*param| {
            if (param.constraint) |ref| {
                const constraint = try self.desugar_node(ink.ast.deref(ref));
                param.constraint = ink.ast.ref(constraint);
            }
            if (param.default) |ref| {
                const def = try self.desugar_node(ink.ast.deref(ref));
                param.default = ink.ast.ref(def);
            }
        }

        const value = try self.desugar_node(ink.ast.deref(t.value));
        return .{
            .attributes = attributes,
            .name = t.name,
            .generics = generics,
            .value = ink.ast.ref(value),
        };
    }

    fn desugar_import_decl(self: *desugarer, imp: ink.ast.import_decl) desugar_error!ink.ast.import_decl {
        const attributes = try self.desugar_attributes(imp.attributes);
        return .{
            .attributes = attributes,
            .module = imp.module,
            .item = imp.item,
            .alias = imp.alias,
        };
    }

    fn desugar_trait_decl(self: *desugarer, tr: ink.ast.trait_decl) desugar_error!ink.ast.trait_decl {
        const attributes = try self.desugar_attributes(tr.attributes);
        const generics = @constCast(tr.generics);
        for (generics) |*param| {
            if (param.constraint) |ref| {
                const constraint = try self.desugar_node(ink.ast.deref(ref));
                param.constraint = ink.ast.ref(constraint);
            }
            if (param.default) |ref| {
                const def = try self.desugar_node(ink.ast.deref(ref));
                param.default = ink.ast.ref(def);
            }
        }

        const items = @constCast(tr.items);
        for (items) |*item| {
            switch (item.*) {
                .function => |func| {
                    item.* = .{ .function = try self.desugar_function_decl(func) };
                },
                .assoc_type => |assoc| {
                    item.* = .{ .assoc_type = try self.desugar_assoc_type_decl(assoc) };
                },
            }
        }

        const requires = @constCast(tr.requires);
        for (requires) |*req_ref| {
            const req_node = try self.desugar_node(ink.ast.deref(req_ref.*));
            req_ref.* = ink.ast.ref(req_node);
        }

        return .{
            .attributes = attributes,
            .is_auto = tr.is_auto,
            .name = tr.name,
            .generics = generics,
            .items = items,
            .requires = requires,
        };
    }

    fn desugar_assoc_type_decl(
        self: *desugarer,
        assoc: ink.ast.associated_type_decl,
    ) desugar_error!ink.ast.associated_type_decl {
        const attributes = try self.desugar_attributes(assoc.attributes);
        const value = if (assoc.value) |ref| try self.desugar_node(ink.ast.deref(ref)) else null;
        return .{
            .attributes = attributes,
            .name = assoc.name,
            .value = ink.ast.ref_opt(value),
        };
    }

    fn desugar_enum_decl(self: *desugarer, e: ink.ast.enum_decl) desugar_error!ink.ast.enum_decl {
        const attributes = try self.desugar_attributes(e.attributes);
        const generics = @constCast(e.generics);
        for (generics) |*param| {
            if (param.constraint) |ref| {
                const constraint = try self.desugar_node(ink.ast.deref(ref));
                param.constraint = ink.ast.ref(constraint);
            }
            if (param.default) |ref| {
                const def = try self.desugar_node(ink.ast.deref(ref));
                param.default = ink.ast.ref(def);
            }
        }

        const variants = @constCast(e.variants);
        for (variants) |*variant| {
            if (variant.payload) |ref| {
                const payload = try self.desugar_node(ink.ast.deref(ref));
                variant.payload = ink.ast.ref_opt(payload);
            }
        }
        return .{
            .attributes = attributes,
            .name = e.name,
            .generics = generics,
            .variants = variants,
        };
    }

    fn desugar_impl_decl(self: *desugarer, im: ink.ast.impl_decl) desugar_error!ink.ast.impl_decl {
        const attributes = try self.desugar_attributes(im.attributes);
        const functions = @constCast(im.functions);
        for (functions) |*func| {
            func.* = try self.desugar_function_decl(func.*);
        }

        return .{
            .attributes = attributes,
            .negative = im.negative,
            .by_trait = im.by_trait,
            .for_struct = im.for_struct,
            .functions = functions,
        };
    }

    fn desugar_const_decl(self: *desugarer, c: ink.ast.const_decl) desugar_error!ink.ast.const_decl {
        const attributes = try self.desugar_attributes(c.attributes);
        const ty = if (c.ty) |ref| try self.desugar_node(ink.ast.deref(ref)) else null;
        const value = try self.desugar_node(ink.ast.deref(c.value));
        return .{
            .attributes = attributes,
            .name = c.name,
            .ty = ink.ast.ref_opt(ty),
            .value = ink.ast.ref(value),
        };
    }

    fn desugar_var_decl(self: *desugarer, v: ink.ast.var_decl) desugar_error!ink.ast.var_decl {
        const attributes = try self.desugar_attributes(v.attributes);
        const ty = if (v.ty) |ref| try self.desugar_node(ink.ast.deref(ref)) else null;
        const value = try self.desugar_node(ink.ast.deref(v.value));
        return .{
            .attributes = attributes,
            .name = v.name,
            .ty = ink.ast.ref_opt(ty),
            .value = ink.ast.ref(value),
        };
    }

    fn desugar_record(self: *desugarer, origin_node: *ink.node, rec: ink.ast.record_expr) desugar_error!*ink.node {
        const items = @constCast(rec.items);
        for (items) |*assoc| {
            if (assoc.value) |ref| {
                const value = try self.desugar_node(ink.ast.deref(ref));
                assoc.value = ink.ast.ref_opt(value);
            }
        }
        return self.new_node(origin_node, .{ .record = .{ .items = items } });
    }

    fn desugar_scope_access(self: *desugarer, origin_node: *ink.node, left: *ink.node, right: *ink.node) desugar_error!*ink.node {
        if (left.* != .identifier or right.* != .identifier) {
            try self.add_error("scope access requires identifiers", null);
            return origin_node;
        }

        const left_id = left.identifier;
        const right_id = right.identifier;
        const is_builtin_scope = std.mem.eql(u8, left_id.string, "error");
        if (std.mem.indexOf(u8, left_id.string, "::") == null and !self.import_map.contains(left_id.string) and !is_builtin_scope) {
            try self.add_error("unknown import", left_id.where);
        }

        const qualified = try self.qualify_name(left_id.string, right_id.string);
        const where = ink.location{ .start = left_id.where.start, .end = right_id.where.end };
        return self.new_identifier(origin_node, qualified, where);
    }

    fn qualify_name(self: *desugarer, alias: []const u8, name: []const u8) desugar_error![]const u8 {
        const sep = "::";
        var buf = try self.node_allocator.alloc(u8, alias.len + sep.len + name.len);
        std.mem.copyForwards(u8, buf[0..alias.len], alias);
        std.mem.copyForwards(u8, buf[alias.len .. alias.len + sep.len], sep);
        std.mem.copyForwards(u8, buf[alias.len + sep.len ..], name);
        return buf;
    }

    fn new_node(self: *desugarer, origin_node: *ink.node, value: ink.node) desugar_error!*ink.node {
        const node = try self.node_allocator.create(ink.node);
        node.* = value;
        try self.origin.put(node, origin_node);
        return node;
    }

    fn new_identifier(self: *desugarer, origin_node: *ink.node, text: []const u8, where: ink.location) desugar_error!*ink.node {
        const node = try self.node_allocator.create(ink.node);
        node.* = .{ .identifier = .{
            .string = text,
            .owner = .ref,
            .where = where,
        } };
        try self.origin.put(node, origin_node);
        return node;
    }

    fn new_qualified_identifier(
        self: *desugarer,
        origin_node: *ink.node,
        module_name: []const u8,
        name: []const u8,
        where: ink.location,
    ) desugar_error!*ink.node {
        const qualified = try self.qualify_name(module_name, name);
        return self.new_identifier(origin_node, qualified, where);
    }

    fn new_integer(self: *desugarer, origin_node: *ink.node, value: i64, where: ink.location) desugar_error!*ink.node {
        const node = try self.node_allocator.create(ink.node);
        node.* = .{ .integer = .{ .value = value, .where = where } };
        try self.origin.put(node, origin_node);
        return node;
    }

    fn new_temp_name(self: *desugarer, prefix: []const u8) desugar_error![]const u8 {
        const name = try std.fmt.allocPrint(self.node_allocator, "{s}{d}", .{ prefix, self.with_counter });
        self.with_counter += 1;
        return name;
    }

    fn new_const_decl(
        self: *desugarer,
        origin_node: *ink.node,
        name: ink.identifier,
        value: *ink.node,
    ) desugar_error!*ink.node {
        const node = try self.node_allocator.create(ink.node);
        node.* = .{ .decl = .{ .@"const" = .{
            .attributes = &[_]ink.ast.attribute{},
            .name = name,
            .ty = ink.ast.ref_opt(null),
            .value = ink.ast.ref(value),
        } } };
        try self.origin.put(node, origin_node);
        return node;
    }

    fn new_string(self: *desugarer, origin_node: *ink.node, text: []const u8, where: ink.location) desugar_error!*ink.node {
        const node = try self.node_allocator.create(ink.node);
        node.* = .{ .string = .{
            .string = text,
            .owner = .ref,
            .where = where,
        } };
        try self.origin.put(node, origin_node);
        return node;
    }

    fn call_with_args(self: *desugarer, origin_node: *ink.node, base: *ink.node, args: []const *ink.node) desugar_error!*ink.node {
        var expr = base;
        for (args) |arg| {
            const node = try self.node_allocator.create(ink.node);
            node.* = .{ .binary = .{
                .left = ink.ast.ref(expr),
                .op = .call,
                .right = ink.ast.ref(arg),
            } };
            try self.origin.put(node, origin_node);
            expr = node;
        }
        return expr;
    }
};

fn record_location(rec: ink.ast.record_expr) ink.location {
    if (rec.items.len == 0) {
        return .{ .start = 0, .end = 0 };
    }
    const start = rec.items[0].name.where.start;
    const end = rec.items[rec.items.len - 1].name.where.end;
    return .{ .start = start, .end = end };
}

fn node_location(node: *const ink.node) ink.location {
    return switch (node.*) {
        .identifier => |id| id.where,
        .string => |str| str.where,
        else => .{ .start = 0, .end = 0 },
    };
}

fn import_decl_from(node: *const ink.node) ?ink.ast.import_decl {
    if (node.* != .decl) return null;
    return switch (node.decl) {
        .import => |imp| imp,
        else => null,
    };
}

pub fn desugar(
    node_allocator: mem_allocator,
    allocator: mem_allocator,
    nodes: []const *ink.node,
    diags: *array_list(diagnostic),
) desugar_error!result {
    var d = desugarer.init(node_allocator, allocator, diags);
    defer d.deinit();

    try d.collect_imports(nodes);
    try d.collect_operator_overloads(nodes);
    const out_nodes = try d.desugar_nodes(nodes);
    return .{
        .nodes = out_nodes,
        .imports = try d.imports.toOwnedSlice(),
        .origin = d.origin,
    };
}
