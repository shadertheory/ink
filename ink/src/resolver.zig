const std = @import("std");
const source = @import("source.zig");
const diag = @import("diagnostic.zig");
const ink = @import("root.zig");
const intrinsic = @import("intrinsic.zig");
const lang_spec = @import("lang/spec.zig");
const token = ink.token;
const mem_allocator = std.mem.Allocator;
const alloc_error = mem_allocator.Error;
const arena_allocator = std.heap.ArenaAllocator;
const array_list = std.array_list.Managed;
const string_map = std.hash_map.StringHashMap;
const hash_map = std.hash_map.HashMap;
const auto_map = std.hash_map.AutoHashMap;
const span = source.span;
const diagnostic = diag.diagnostic;

const type_flag_trait: u8 = 0b001;
const type_flag_other: u8 = 0b010;

pub const resolver = struct {
    pub const module_id = source.module_id;
    pub const module_import = struct { id: module_id, alias: []const u8 };

    const module_state = struct {
        imports: []const module_import,
        values: string_map(void),
        types: string_map(u8),
        allocated_names: std.ArrayListUnmanaged([]const u8) = .{},
        allocator: mem_allocator,

        fn deinit(self: *module_state) void {
            self.values.deinit();
            self.types.deinit();
            for (self.allocated_names.items) |name| {
                self.allocator.free(name);
            }
            self.allocated_names.deinit(self.allocator);
            self.allocator.free(self.imports);
        }
    };

    const scope = struct {
        values: string_map(void),
        types: string_map(u8),

        fn init(allocator: mem_allocator) scope {
            return .{
                .values = string_map(void).init(allocator),
                .types = string_map(u8).init(allocator),
            };
        }

        fn deinit(self: *scope) void {
            self.values.deinit();
            self.types.deinit();
        }
    };

    const context = struct {
        allocator: mem_allocator,
        base_values: string_map(void),
        base_types: string_map(u8),
        scopes: std.ArrayListUnmanaged(scope) = .{},
        allocated_names: std.ArrayListUnmanaged([]const u8) = .{},
        ctx_span_of: ?*const fn (*const ink.node) ?span = null,
        diag_messages: ?*array_list([]const u8) = null,
        current_source_id: ?source.source_id = null,

        fn deinit(self: *context) void {
            self.base_values.deinit();
            self.base_types.deinit();
            for (self.scopes.items) |*s| s.deinit();
            self.scopes.deinit(self.allocator);
            for (self.allocated_names.items) |name| {
                self.allocator.free(name);
            }
            self.allocated_names.deinit(self.allocator);
        }

        fn push(self: *context) alloc_error!void {
            try self.scopes.append(self.allocator, scope.init(self.allocator));
        }

        fn pop(self: *context) void {
            if (self.scopes.pop()) |s| {
                var scope_val = s;
                scope_val.deinit();
            }
        }

        fn declare_value(self: *context, name: []const u8, diags: *array_list(diagnostic), node: ?*const ink.node) alloc_error!void {
            var s = &self.scopes.items[self.scopes.items.len - 1];
            if (s.values.contains(name)) {
                try diags.append(.{
                    .danger = .@"error",
                    .message = "duplicate  symbol",
                    .span = span_of(self, node),
                    .source_id = self.current_source_id,
                });
                return;
            }
            try s.values.put(name, {});
        }

        fn declare_overloadable_value(
            self: *context,
            name: []const u8,
            diags: *array_list(diagnostic),
            node: ?*const ink.node,
        ) alloc_error!void {
            var s = &self.scopes.items[self.scopes.items.len - 1];
            if (s.values.contains(name)) {
                _ = diags;
                _ = node;
                return;
            }
            try s.values.put(name, {});
        }

        fn declare_type(
            self: *context,
            name: []const u8,
            flag: u8,
            diags: *array_list(diagnostic),
            node: ?*const ink.node,
        ) alloc_error!void {
            var s = &self.scopes.items[self.scopes.items.len - 1];
            if (s.types.get(name)) |existing| {
                const has_other = (existing & type_flag_other) != 0 or (flag & type_flag_other) != 0;
                const duplicate = (existing & flag) != 0;
                if (has_other or duplicate) {
                    try diags.append(.{
                        .danger = .@"error",
                        .message = "duplicate symbol",
                        .span = span_of(self, node),
                        .source_id = self.current_source_id,
                    });
                    return;
                }
                try s.types.put(name, existing | flag);
                return;
            }
            try s.types.put(name, flag);
        }

        fn resolve_value(self: *context, name: []const u8) bool {
            var i: usize = self.scopes.items.len;
            while (i > 0) : (i -= 1) {
                if (self.scopes.items[i - 1].values.contains(name)) return true;
            }
            return self.base_values.contains(name);
        }

        fn resolve_local_value(self: *context, name: []const u8) bool {
            var i: usize = self.scopes.items.len;
            while (i > 1) : (i -= 1) {
                if (self.scopes.items[i - 1].values.contains(name)) return true;
            }
            return false;
        }

        fn resolve_type(self: *context, name: []const u8) bool {
            var i: usize = self.scopes.items.len;
            while (i > 0) : (i -= 1) {
                if (self.scopes.items[i - 1].types.contains(name)) return true;
            }
            return self.base_types.contains(name);
        }

        fn lookup_type_flags(self: *context, name: []const u8) ?u8 {
            var i: usize = self.scopes.items.len;
            while (i > 0) : (i -= 1) {
                if (self.scopes.items[i - 1].types.get(name)) |flags| return flags;
            }
            return self.base_types.get(name);
        }

        fn resolve_trait(self: *context, name: []const u8) bool {
            if (self.lookup_type_flags(name)) |flags| {
                return (flags & type_flag_trait) != 0;
            }
            return false;
        }

        fn span_of(self: *context, node: ?*const ink.node) ?span {
            if (node == null) return null;
            if (self.ctx_span_of) |f| return f(node.?);
            return null;
        }
    };

    allocator: mem_allocator,
    span_of: ?*const fn (*const ink.node) ?span,
    modules: auto_map(module_id, module_state),
    diag_messages: ?*array_list([]const u8),

    pub fn init(
        allocator: mem_allocator,
        span_of_fn: ?*const fn (*const ink.node) ?span,
        diag_messages: ?*array_list([]const u8),
    ) resolver {
        return .{
            .allocator = allocator,
            .span_of = span_of_fn,
            .modules = auto_map(module_id, module_state).init(allocator),
            .diag_messages = diag_messages,
        };
    }

    pub fn deinit(self: *resolver) void {
        var it = self.modules.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.*.deinit();
        }
        self.modules.deinit();
    }

    pub fn add_module(
        self: *resolver,
        id: module_id,
        imports: []const module_import,
        items: []const *ink.node,
        diags: *array_list(diagnostic),
    ) alloc_error!void {
        var state = module_state{
            .imports = try self.allocator.dupe(module_import, imports),
            .values = string_map(void).init(self.allocator),
            .types = string_map(u8).init(self.allocator),
            .allocated_names = .{},
            .allocator = self.allocator,
        };
        try scan_exports(&state, items, diags);
        try self.modules.put(id, state);
    }

    pub fn resolve_module(
        self: *resolver,
        id: module_id,
        items: []const *ink.node,
        item_sources: []const source.source_id,
        diags: *array_list(diagnostic),
    ) alloc_error!void {
        const state = self.modules.get(id) orelse return;
        var ctx = context{
            .allocator = self.allocator,
            .base_values = string_map(void).init(self.allocator),
            .base_types = string_map(u8).init(self.allocator),
            .ctx_span_of = self.span_of,
            .diag_messages = self.diag_messages,
        };
        defer ctx.deinit();
        try ctx.push();

        try ctx.base_values.put("unit", {});
        try ctx.base_types.put("unit", type_flag_other);
        try ctx.base_values.put("true", {});
        try ctx.base_values.put("false", {});
        try ctx.base_values.put("none", {});
        try ctx.base_values.put("cancel", {});
        try ctx.base_values.put("error::cancelled", {});
        try ctx.base_values.put("error::timeout", {});
        try ctx.base_types.put("int", type_flag_other);
        try ctx.base_types.put("uint", type_flag_other);
        try ctx.base_types.put("float", type_flag_other);
        try ctx.base_types.put("bool", type_flag_other);
        try ctx.base_types.put("string", type_flag_other);
        try ctx.base_types.put("token_stream", type_flag_other);
        try ctx.base_types.put("token_tree", type_flag_other);
        try ctx.base_types.put("token", type_flag_other);
        try ctx.base_types.put("token_group", type_flag_other);
        try ctx.base_types.put("token_kind", type_flag_other);
        try ctx.base_types.put("token_tree_kind", type_flag_other);
        try ctx.base_types.put("delimiter", type_flag_other);
        try ctx.base_types.put("span", type_flag_other);
        try ctx.base_types.put("symbol", type_flag_other);
        try ctx.base_types.put("type", type_flag_other);
        try ctx.base_types.put("none", type_flag_other);
        try ctx.base_types.put("result", type_flag_other);
        try ctx.base_types.put("error", type_flag_other);
        try ctx.base_types.put("task", type_flag_other);
        try ctx.base_types.put("buf", type_flag_other);
        try ctx.base_types.put("arena", type_flag_other);
        try ctx.base_types.put("union", type_flag_other);
        try ctx.base_types.put("intersect", type_flag_other);
        try ctx.base_types.put("tuple", type_flag_other);
        try ctx.base_types.put("fn", type_flag_other);
        try ctx.base_types.put("slice", type_flag_other);
        try ctx.base_types.put("array", type_flag_other);
        try ctx.base_types.put("box", type_flag_other);
        try ctx.base_types.put("atomic", type_flag_other);
        try ctx.base_types.put("duration", type_flag_other);
        try ctx.base_types.put("instant", type_flag_other);
        try ctx.base_types.put("deadline", type_flag_other);
        try ctx.base_types.put("not", type_flag_other);

        try merge_symbols(&ctx.base_values, &ctx.base_types, state);
        for (state.imports) |imported| {
            if (self.modules.get(imported.id)) |dep_state| {
                try merge_imported_symbols(&ctx, &ctx.base_values, &ctx.base_types, dep_state, imported.alias);
            }
        }

        for (items, 0..) |item, idx| {
            if (idx < item_sources.len) {
                ctx.current_source_id = item_sources[idx];
            } else {
                ctx.current_source_id = null;
            }
            try resolve_node(&ctx, item, diags);
        }
    }

    fn scan_exports(state: *module_state, items: []const *ink.node, diags: *array_list(diagnostic)) alloc_error!void {
        for (items) |item| {
            const node = item;
            if (node.* != .decl) continue;
            switch (node.decl) {
                .function => |f| try state.values.put(f.name.string, {}),
                .@"const" => |c| {
                    try state.values.put(c.name.string, {});
                },
                .@"var" => |v| try state.values.put(v.name.string, {}),
                .type_alias => |t| try add_export_type(&state.types, t.name.string, type_flag_other),
                .@"struct" => |s| {
                    try state.values.put(s.name.string, {});
                    try add_export_type(&state.types, s.name.string, type_flag_other);
                },
                .trait => |t| try add_export_type(&state.types, t.name.string, type_flag_trait),
                .@"enum" => |e| {
                    try add_export_type(&state.types, e.name.string, type_flag_other);
                    for (e.variants) |variant| {
                        const qualified = try qualify_name_owned(state.allocator, e.name.string, variant.name.string);
                        try state.values.put(qualified, {});
                        try state.allocated_names.append(state.allocator, qualified);
                    }
                },
                .import => |_| {},
                .impl => |_| {},
            }
        }
        _ = diags;
    }

    fn qualify_name_owned(allocator: mem_allocator, left: []const u8, right: []const u8) alloc_error![]const u8 {
        const sep = "::";
        var buf = try allocator.alloc(u8, left.len + sep.len + right.len);
        std.mem.copyForwards(u8, buf[0..left.len], left);
        std.mem.copyForwards(u8, buf[left.len .. left.len + sep.len], sep);
        std.mem.copyForwards(u8, buf[left.len + sep.len ..], right);
        return buf;
    }

    fn add_export_type(types: *string_map(u8), name: []const u8, flag: u8) alloc_error!void {
        if (types.get(name)) |existing| {
            const merged = merge_type_flags(existing, flag);
            if (merged != existing) {
                try types.put(name, merged);
            }
            return;
        }
        try types.put(name, flag);
    }

    fn merge_type_flags(existing: u8, incoming: u8) u8 {
        if ((existing & type_flag_other) != 0 or (incoming & type_flag_other) != 0) {
            return existing;
        }
        return existing | incoming;
    }

    fn merge_symbols(
        values: *string_map(void),
        types: *string_map(u8),
        state: module_state,
    ) alloc_error!void {
        var itv = state.values.iterator();
        while (itv.next()) |entry| {
            if (!values.contains(entry.key_ptr.*)) {
                try values.put(entry.key_ptr.*, {});
            }
        }

        var itt = state.types.iterator();
        while (itt.next()) |entry| {
            const incoming = entry.value_ptr.*;
            if (types.get(entry.key_ptr.*)) |existing| {
                const merged = merge_type_flags(existing, incoming);
                if (merged != existing) {
                    try types.put(entry.key_ptr.*, merged);
                }
            } else {
                try types.put(entry.key_ptr.*, incoming);
            }
        }
    }

    fn merge_imported_symbols(
        ctx: *context,
        values: *string_map(void),
        types: *string_map(u8),
        state: module_state,
        alias: []const u8,
    ) alloc_error!void {
        var itv = state.values.iterator();
        while (itv.next()) |entry| {
            const qualified = try qualify_name(ctx, alias, entry.key_ptr.*);
            if (!values.contains(qualified)) {
                try values.put(qualified, {});
            }
        }

        var itt = state.types.iterator();
        while (itt.next()) |entry| {
            const qualified = try qualify_name(ctx, alias, entry.key_ptr.*);
            const incoming = entry.value_ptr.*;
            if (types.get(qualified)) |existing| {
                const merged = merge_type_flags(existing, incoming);
                if (merged != existing) {
                    try types.put(qualified, merged);
                }
            } else {
                try types.put(qualified, incoming);
            }
        }
    }

    fn qualify_name(ctx: *context, alias: []const u8, name: []const u8) alloc_error![]const u8 {
        const sep = "::";
        var buf = try ctx.allocator.alloc(u8, alias.len + sep.len + name.len);
        std.mem.copyForwards(u8, buf[0..alias.len], alias);
        std.mem.copyForwards(u8, buf[alias.len .. alias.len + sep.len], sep);
        std.mem.copyForwards(u8, buf[alias.len + sep.len ..], name);
        try ctx.allocated_names.append(ctx.allocator, buf);
        return buf;
    }

    fn resolve_node(ctx: *context, node: *const ink.node, diags: *array_list(diagnostic)) alloc_error!void {
        switch (node.*) {
            .integer, .float, .duration, .string => {},
            .identifier => |id| {
                const ok = ctx.resolve_value(id.string);
                if (!ok) {
                    const hint = try suggest_value(ctx, id.string);
                    try report_unknown(
                        ctx,
                        diags,
                        "unknown identifier",
                        id.string,
                        span{ .start = id.where.start, .end = id.where.end },
                        "E2001",
                        hint,
                    );
                }
            },
            .type => |ty| try resolve_type_expr(ctx, ty, diags),
            .unary => |un| {
                if (un.op == .ret) {
                }
                try resolve_node(ctx, ink.ast.deref(un.right), diags);
            },
            .binary => |bin| try resolve_binary(ctx, bin, diags),
            .macro_call => |mc| {
                try diags.append(.{
                    .danger = .@"error",
                    .message = "macro call was not expanded",
                    .span = span{ .start = mc.where.start, .end = mc.where.end },
                    .source_id = ctx.current_source_id,
                });
            },
            .label_expr => |le| try resolve_node(ctx, ink.ast.deref(le.body), diags),
            .loop_expr => |le| try resolve_node(ctx, ink.ast.deref(le.body), diags),
            .while_expr => |we| {
                try resolve_node(ctx, ink.ast.deref(we.condition), diags);
                try resolve_node(ctx, ink.ast.deref(we.body), diags);
            },
            .while_in_expr => |we| {
                try resolve_node(ctx, ink.ast.deref(we.iter), diags);
                try ctx.push();
                defer ctx.pop();
                try bind_pattern(ctx, ink.ast.deref(we.pattern), diags);
                try resolve_node(ctx, ink.ast.deref(we.body), diags);
            },
            .until_expr => |ue| {
                try resolve_node(ctx, ink.ast.deref(ue.condition), diags);
                try resolve_node(ctx, ink.ast.deref(ue.body), diags);
            },
            .repeat_expr => |re| {
                try resolve_node(ctx, ink.ast.deref(re.count), diags);
                try resolve_node(ctx, ink.ast.deref(re.body), diags);
            },
            .for_expr => |fe| {
                try resolve_node(ctx, ink.ast.deref(fe.iter), diags);
                try ctx.push();
                defer ctx.pop();
                try bind_pattern(ctx, ink.ast.deref(fe.pattern), diags);
                try resolve_node(ctx, ink.ast.deref(fe.body), diags);
            },
            .each_expr => |ee| {
                try resolve_node(ctx, ink.ast.deref(ee.iter), diags);
                try ctx.push();
                defer ctx.pop();
                try bind_pattern(ctx, ink.ast.deref(ee.pattern), diags);
                try resolve_node(ctx, ink.ast.deref(ee.body), diags);
            },
            .break_expr => |be| if (be.value) |ref| try resolve_node(ctx, ink.ast.deref(ref), diags),
            .continue_expr => |_| {},
            .yield_expr => |ye| if (ye.value) |ref| try resolve_node(ctx, ink.ast.deref(ref), diags),
            .atomic_expr => |ae| try resolve_node(ctx, ink.ast.deref(ae.value), diags),
            .block => |blk| {
                try ctx.push();
                defer ctx.pop();

                for (blk.items) |ref| {
                    const item = ink.ast.deref(ref);
                    if (item.* == .decl) try declare_decl(ctx, item.decl, diags, item);
                    try resolve_node(ctx, item, diags);
                }
            },
            .if_expr => |ife| {
                try resolve_node(ctx, ink.ast.deref(ife.condition), diags);
                try resolve_node(ctx, ink.ast.deref(ife.then_branch), diags);
                if (ife.else_branch) |ref| try resolve_node(ctx, ink.ast.deref(ref), diags);
            },
            .match_expr => |me| {
                try resolve_node(ctx, ink.ast.deref(me.target), diags);
                for (me.arms) |arm| {
                    try ctx.push();
                    defer ctx.pop();
                    try bind_pattern(ctx, ink.ast.deref(arm.pattern), diags);
                    try resolve_node(ctx, ink.ast.deref(arm.body), diags);
                }
            },
            .select_expr => |sel| {
                for (sel.arms) |arm| {
                    try resolve_node(ctx, ink.ast.deref(arm.task), diags);
                    try ctx.push();
                    defer ctx.pop();
                    if (arm.name) |name| {
                        if (!std.mem.eql(u8, name.string, "_")) {
                            try ctx.declare_value(name.string, diags, node);
                        }
                    }
                    try resolve_node(ctx, ink.ast.deref(arm.body), diags);
                }
            },
            .with_expr => |we| {
                try ctx.push();
                defer ctx.pop();
                try ctx.declare_value(we.name.string, diags, node);
                try resolve_node(ctx, ink.ast.deref(we.body), diags);
            },
            .record => |rec| {
                for (rec.items) |assoc| {
                    if (assoc.value) |ref| try resolve_node(ctx, ink.ast.deref(ref), diags);
                }
            },
            .intrinsic => |call| {
                const def = intrinsic.lookup(call.name.string);
                if (def == null) {
                    const hint = try suggest_intrinsic(ctx, call.name.string);
                    try report_unknown(
                        ctx,
                        diags,
                        "unknown intrinsic",
                        call.name.string,
                        span{ .start = call.name.where.start, .end = call.name.where.end },
                        "E2003",
                        hint,
                    );
                } else if (!def.?.variadic and call.args.len != @as(usize, def.?.arity)) {
                    const msg = try std.fmt.allocPrint(
                        ctx.allocator,
                        "intrinsic '{s}' expects {d} argument(s)",
                        .{ call.name.string, def.?.arity },
                    );
                    if (ctx.diag_messages != null) {
                        try ctx.diag_messages.?.append(msg);
                    }
                    try diags.append(.{
                        .danger = .@"error",
                        .message = msg,
                        .span = span{ .start = call.name.where.start, .end = call.name.where.end },
                        .code = "E2004",
                        .source_id = ctx.current_source_id,
                    });
                }
                for (call.args) |arg_ref| {
                    try resolve_node(ctx, ink.ast.deref(arg_ref), diags);
                }
            },
            .associate => |assoc| {
                if (assoc.value) |ref| try resolve_node(ctx, ink.ast.deref(ref), diags);
            },
            .decl => |decl| try resolve_decl(ctx, decl, diags),
        }
    }

    fn bind_pattern(ctx: *context, node: *const ink.node, diags: *array_list(diagnostic)) alloc_error!void {
        switch (node.*) {
            .identifier => |id| {
                if (std.mem.eql(u8, id.string, "*")) return;
                try ctx.declare_value(id.string, diags, node);
            },
            .record => |rec| {
                for (rec.items) |assoc| {
                    if (assoc.value) |ref| try bind_pattern(ctx, ink.ast.deref(ref), diags);
                }
            },
            .binary => |bin| switch (bin.op) {
                .call => try bind_pattern_call(ctx, node, diags),
                .access, .scope_access => {},
                else => {
                    try bind_pattern(ctx, ink.ast.deref(bin.left), diags);
                    try bind_pattern(ctx, ink.ast.deref(bin.right), diags);
                },
            },
            .unary => |un| try bind_pattern(ctx, ink.ast.deref(un.right), diags),
            else => {},
        }
    }

    fn bind_pattern_call(ctx: *context, node: *const ink.node, diags: *array_list(diagnostic)) alloc_error!void {
        var current = node;
        while (current.* == .binary and current.binary.op == .call) {
            const call = current.binary;
            try bind_pattern(ctx, ink.ast.deref(call.right), diags);
            current = ink.ast.deref(call.left);
        }
    }

    fn resolve_binary(ctx: *context, bin: ink.ast.binary_expr, diags: *array_list(diagnostic)) alloc_error!void {
        switch (bin.op) {
            .access => {
                try resolve_node(ctx, ink.ast.deref(bin.left), diags);
            },
            else => {
                try resolve_node(ctx, ink.ast.deref(bin.left), diags);
                try resolve_node(ctx, ink.ast.deref(bin.right), diags);
            },
        }
    }

    fn declare_decl(ctx: *context, decl: ink.ast.decl, diags: *array_list(diagnostic), node: *const ink.node) alloc_error!void {
        switch (decl) {
            .function => |func| try ctx.declare_overloadable_value(func.name.string, diags, node),
            .@"const" => |c| try ctx.declare_value(c.name.string, diags, node),
            .@"var" => |v| try ctx.declare_value(v.name.string, diags, node),
            .type_alias => |t| try ctx.declare_type(t.name.string, type_flag_other, diags, node),
            .@"struct" => |s| {
                try ctx.declare_type(s.name.string, type_flag_other, diags, node);
                try ctx.declare_value(s.name.string, diags, node);
            },
            .trait => |t| try ctx.declare_type(t.name.string, type_flag_trait, diags, node),
            .@"enum" => |e| try ctx.declare_type(e.name.string, type_flag_other, diags, node),
            .import => |_| {},
            .impl => {},
        }
    }

    fn resolve_decl(ctx: *context, decl: ink.ast.decl, diags: *array_list(diagnostic)) alloc_error!void {
        switch (decl) {
            .function => |f| try resolve_function_decl(ctx, f, diags),
            .@"struct" => |s| try resolve_struct_decl(ctx, s, diags),
            .trait => |t| try resolve_trait_decl(ctx, t, diags),
            .@"enum" => |e| try resolve_enum_decl(ctx, e, diags),
            .impl => |i| try resolve_impl_decl(ctx, i, diags),
            .import => |imp| try resolve_import_decl(ctx, imp, diags),
            .type_alias => |t| try resolve_type_alias_decl(ctx, t, diags),
            .@"const" => |c| try resolve_const_decl(ctx, c, diags),
            .@"var" => |v| try resolve_var_decl(ctx, v, diags),
        }
    }

    fn resolve_attributes(
        ctx: *context,
        attrs: []const ink.ast.attribute,
        diags: *array_list(diagnostic),
    ) alloc_error!void {
        for (attrs) |attr| {
            if (std.mem.eql(u8, attr.name.string, "repr")) {
                if (attr.args) |arg_stream| {
                    try resolve_repr_args(ctx, arg_stream, diags);
                }
                continue;
            }
        }
    }

    fn resolve_repr_args(
        ctx: *context,
        args: ink.identifier,
        diags: *array_list(diagnostic),
    ) alloc_error!void {
        var lexer = ink.lexer.init(args.string) catch return;
        while (true) {
            const maybe_tok = lexer.next() catch return;
            if (maybe_tok == null) break;
            const tok = maybe_tok.?;
            switch (tok.which) {
                .end_of_file => break,
                .comma, .new_line, .indent, .dedent => continue,
                else => {},
            }
            if (!is_name_token(tok.which)) continue;
            if (!ctx.resolve_type(tok.what.string)) {
                const hint = try suggest_type(ctx, tok.what.string);
                try report_unknown(
                    ctx,
                    diags,
                    "unknown type",
                    tok.what.string,
                    span{ .start = tok.what.where.start, .end = tok.what.where.end },
                    "E2002",
                    hint,
                );
            }
        }
    }

    fn is_name_token(kind: token.kind) bool {
        if (kind == .identifier) return true;
        inline for (lang_spec.keyword_lexemes) |lex| {
            if (kind == @field(token.kind, lex.kind)) return true;
        }
        return false;
    }

    fn has_attribute(attrs: []const ink.ast.attribute, name: []const u8) bool {
        for (attrs) |attr| {
            if (std.mem.eql(u8, attr.name.string, name)) return true;
        }
        return false;
    }

    fn resolve_function_decl(ctx: *context, func: ink.ast.function_decl, diags: *array_list(diagnostic)) alloc_error!void {
        const is_foreign = has_attribute(func.attributes, "foreign");
        try resolve_attributes(ctx, func.attributes, diags);
        try ctx.push();
        defer ctx.pop();

        var pack_params = string_map(void).init(ctx.allocator);
        defer pack_params.deinit();
        var type_params = string_map(void).init(ctx.allocator);
        defer type_params.deinit();

        for (func.generics) |param| {
            if (param.kind == .type) {
                try ctx.declare_type(param.name.string, type_flag_other, diags, null);
                type_params.put(param.name.string, {}) catch return error.OutOfMemory;
            } else {
                try ctx.declare_value(param.name.string, diags, null);
            }
            if (param.constraint) |ref| try resolve_type_node(ctx, ink.ast.deref(ref), diags);
            if (param.default) |ref| try resolve_type_node(ctx, ink.ast.deref(ref), diags);
            if (param.is_pack) {
                if (param.kind != .type) {
                    try diags.append(.{
                        .danger = .@"error",
                        .message = "type packs must be declared as type generics",
                        .span = span{ .start = param.name.where.start, .end = param.name.where.end },
                        .source_id = ctx.current_source_id,
                    });
                }
                pack_params.put(param.name.string, {}) catch return error.OutOfMemory;
            }
        }

        var seen_variadic = false;
        for (func.params, 0..) |param, idx| {
            if (!param.variadic) continue;
            if (seen_variadic) {
                try diags.append(.{
                    .danger = .@"error",
                    .message = "only one variadic param is allowed",
                    .span = span{ .start = param.name.where.start, .end = param.name.where.end },
                    .source_id = ctx.current_source_id,
                });
            }
            if (idx + 1 != func.params.len) {
                try diags.append(.{
                    .danger = .@"error",
                    .message = "variadic param must be the last parameter",
                    .span = span{ .start = param.name.where.start, .end = param.name.where.end },
                    .source_id = ctx.current_source_id,
                });
            }
            seen_variadic = true;

            const ty_node = ink.ast.deref(param.ty);
            if (ty_node.* != .type or ty_node.type != .name) {
                try diags.append(.{
                    .danger = .@"error",
                    .message = "variadic params must use a named type",
                    .span = span{ .start = param.name.where.start, .end = param.name.where.end },
                    .source_id = ctx.current_source_id,
                });
            } else if (type_params.contains(ty_node.type.name.string) and !pack_params.contains(ty_node.type.name.string)) {
                try diags.append(.{
                    .danger = .@"error",
                    .message = "variadic param must reference a type pack generic",
                    .span = span{ .start = param.name.where.start, .end = param.name.where.end },
                    .source_id = ctx.current_source_id,
                });
            }
        }

        for (func.params) |param| {
            try resolve_type_node(ctx, ink.ast.deref(param.ty), diags);
        }
        for (func.params) |param| {
            try ctx.declare_value(param.name.string, diags, null);
        }

        for (func.where_clause) |req| {
            try resolve_type_node(ctx, ink.ast.deref(req.constraint), diags);
        }
        if (func.return_type) |ref| try resolve_type_node(ctx, ink.ast.deref(ref), diags);

        if (is_foreign and func.body != null) {
            try diags.append(.{
                .danger = .@"error",
                .message = "foreign functions cannot have bodies",
                .span = .{ .start = func.name.where.start, .end = func.name.where.end },
                .source_id = ctx.current_source_id,
            });
        }
        if (!is_foreign) {
            if (func.body) |ref| try resolve_node(ctx, ink.ast.deref(ref), diags);
        }
    }

    fn resolve_struct_decl(ctx: *context, st: ink.ast.struct_decl, diags: *array_list(diagnostic)) alloc_error!void {
        try resolve_attributes(ctx, st.attributes, diags);
        try ctx.push();
        defer ctx.pop();

        for (st.generics) |param| {
            if (param.kind == .type) {
                try ctx.declare_type(param.name.string, type_flag_other, diags, null);
            } else {
                try ctx.declare_value(param.name.string, diags, null);
            }
            if (param.constraint) |ref| try resolve_type_node(ctx, ink.ast.deref(ref), diags);
            if (param.default) |ref| try resolve_type_node(ctx, ink.ast.deref(ref), diags);
        }

        for (st.fields) |field| {
            try resolve_type_node(ctx, ink.ast.deref(field.ty), diags);
        }
    }

    fn resolve_trait_decl(ctx: *context, tr: ink.ast.trait_decl, diags: *array_list(diagnostic)) alloc_error!void {
        try resolve_attributes(ctx, tr.attributes, diags);
        try ctx.push();
        defer ctx.pop();

        for (tr.generics) |param| {
            if (param.kind == .type) {
                try ctx.declare_type(param.name.string, type_flag_other, diags, null);
            } else {
                try ctx.declare_value(param.name.string, diags, null);
            }
            if (param.constraint) |ref| try resolve_type_node(ctx, ink.ast.deref(ref), diags);
            if (param.default) |ref| try resolve_type_node(ctx, ink.ast.deref(ref), diags);
        }

        for (tr.items) |item| {
            switch (item) {
                .function => |func| try resolve_function_decl(ctx, func, diags),
                .assoc_type => |assoc| {
                    try resolve_attributes(ctx, assoc.attributes, diags);
                    try ctx.declare_type(assoc.name.string, type_flag_other, diags, null);
                    if (assoc.value) |ref| try resolve_type_node(ctx, ink.ast.deref(ref), diags);
                },
            }
        }

        for (tr.requires) |req_ref| {
            try resolve_type_node(ctx, ink.ast.deref(req_ref), diags);
        }
    }


    fn resolve_enum_decl(ctx: *context, e: ink.ast.enum_decl, diags: *array_list(diagnostic)) alloc_error!void {
        try resolve_attributes(ctx, e.attributes, diags);
        try ctx.push();
        defer ctx.pop();

        for (e.generics) |param| {
            if (param.kind == .type) {
                try ctx.declare_type(param.name.string, type_flag_other, diags, null);
            } else {
                try ctx.declare_value(param.name.string, diags, null);
            }
            if (param.constraint) |ref| try resolve_type_node(ctx, ink.ast.deref(ref), diags);
            if (param.default) |ref| try resolve_type_node(ctx, ink.ast.deref(ref), diags);
        }

        for (e.variants) |variant| {
            if (variant.payload) |ref| try resolve_type_node(ctx, ink.ast.deref(ref), diags);
        }
    }

    fn resolve_type_alias_decl(ctx: *context, t: ink.ast.type_decl, diags: *array_list(diagnostic)) alloc_error!void {
        try resolve_attributes(ctx, t.attributes, diags);
        try ctx.push();
        defer ctx.pop();

        for (t.generics) |param| {
            if (param.kind == .type) {
                try ctx.declare_type(param.name.string, type_flag_other, diags, null);
            } else {
                try ctx.declare_value(param.name.string, diags, null);
            }
            if (param.constraint) |ref| try resolve_type_node(ctx, ink.ast.deref(ref), diags);
            if (param.default) |ref| try resolve_type_node(ctx, ink.ast.deref(ref), diags);
        }

        try resolve_type_node(ctx, ink.ast.deref(t.value), diags);
    }

    fn resolve_impl_decl(ctx: *context, im: ink.ast.impl_decl, diags: *array_list(diagnostic)) alloc_error!void {
        try resolve_attributes(ctx, im.attributes, diags);
        if (!ctx.resolve_type(im.by_trait.string)) {
            const hint = try suggest_type(ctx, im.by_trait.string);
            try report_unknown(
                ctx,
                diags,
                "unknown trait",
                im.by_trait.string,
                span{ .start = im.by_trait.where.start, .end = im.by_trait.where.end },
                "E2003",
                hint,
            );
        }
        if (!ctx.resolve_type(im.for_struct.string)) {
            const hint = try suggest_type(ctx, im.for_struct.string);
            try report_unknown(
                ctx,
                diags,
                "unknown type",
                im.for_struct.string,
                span{ .start = im.for_struct.where.start, .end = im.for_struct.where.end },
                "E2002",
                hint,
            );
        }

        if (im.negative and im.functions.len != 0) {
            try diags.append(.{
                .danger = .@"error",
                .message = "negative impls cannot define functions",
                .span = span{ .start = im.by_trait.where.start, .end = im.by_trait.where.end },
                .source_id = ctx.current_source_id,
            });
            return;
        }

        if (im.negative) return;

        for (im.functions) |func| {
            try resolve_function_decl(ctx, func, diags);
        }
    }

    fn resolve_const_decl(ctx: *context, c: ink.ast.const_decl, diags: *array_list(diagnostic)) alloc_error!void {
        try resolve_attributes(ctx, c.attributes, diags);
        if (c.ty) |ref| try resolve_type_node(ctx, ink.ast.deref(ref), diags);
        try resolve_node(ctx, ink.ast.deref(c.value), diags);
    }

    fn resolve_var_decl(ctx: *context, v: ink.ast.var_decl, diags: *array_list(diagnostic)) alloc_error!void {
        try resolve_attributes(ctx, v.attributes, diags);
        if (v.ty) |ref| try resolve_type_node(ctx, ink.ast.deref(ref), diags);
        try resolve_node(ctx, ink.ast.deref(v.value), diags);
    }

    fn resolve_import_decl(ctx: *context, imp: ink.ast.import_decl, diags: *array_list(diagnostic)) alloc_error!void {
        try resolve_attributes(ctx, imp.attributes, diags);
    }

    fn resolve_type_node(ctx: *context, node: *const ink.node, diags: *array_list(diagnostic)) alloc_error!void {
        switch (node.*) {
            .type => |ty| try resolve_type_expr(ctx, ty, diags),
            else => try resolve_node(ctx, node, diags),
        }
    }

    fn resolve_type_expr(ctx: *context, ty: ink.ast.type_expr, diags: *array_list(diagnostic)) alloc_error!void {
        switch (ty) {
            .self => {},
            .name => |id| {
                if (!ctx.resolve_type(id.string)) {
                    const hint = try suggest_type(ctx, id.string);
                    try report_unknown(
                        ctx,
                        diags,
                        "unknown type",
                        id.string,
                        span{ .start = id.where.start, .end = id.where.end },
                        "E2002",
                        hint,
                    );
                }
            },
            .optional => |ref| try resolve_type_node(ctx, ink.ast.deref(ref), diags),
            .dyn => |ref| {
                const inner = ink.ast.deref(ref);
                try resolve_type_node(ctx, inner, diags);
                var pos = std.array_list.Managed([]const u8).init(ctx.allocator);
                defer pos.deinit();
                var neg = std.array_list.Managed([]const u8).init(ctx.allocator);
                defer neg.deinit();
                var base: ?[]const u8 = null;
                collect_dyn_trait_names(ctx, inner, &base, &pos, &neg);

                if (base == null) {
                    const where = ctx.span_of(inner) orelse span{ .start = 0, .end = 0 };
                    try diags.append(.{
                        .danger = .@"error",
                        .message = "dyn types require at least one positive trait",
                        .span = where,
                        .code = "E2003",
                        .source_id = ctx.current_source_id,
                    });
                    return;
                }

                for (pos.items) |name| {
                    if (!ctx.resolve_trait(name)) {
                        const hint = try suggest_type(ctx, name);
                        const where = ctx.span_of(inner) orelse span{ .start = 0, .end = 0 };
                        try report_unknown(ctx, diags, "unknown trait", name, where, "E2002", hint);
                    }
                }
                for (neg.items) |name| {
                    if (!ctx.resolve_trait(name)) {
                        const hint = try suggest_type(ctx, name);
                        const where = ctx.span_of(inner) orelse span{ .start = 0, .end = 0 };
                        try report_unknown(ctx, diags, "unknown trait", name, where, "E2002", hint);
                    }
                }
            },
            .applied => |ap| {
                if (!ctx.resolve_type(ap.base.string)) {
                    const hint = try suggest_type(ctx, ap.base.string);
                    try report_unknown(
                        ctx,
                        diags,
                        "unknown type",
                        ap.base.string,
                        span{ .start = ap.base.where.start, .end = ap.base.where.end },
                        "E2002",
                        hint,
                    );
                }
                if (std.mem.eql(u8, ap.base.string, "slice") and ap.args.len != 1) {
                    try diags.append(.{
                        .danger = .@"error",
                        .message = "slice type expects 1 argument",
                        .span = span{ .start = ap.base.where.start, .end = ap.base.where.end },
                        .code = "E2003",
                        .source_id = ctx.current_source_id,
                    });
                }
                if (std.mem.eql(u8, ap.base.string, "array") and ap.args.len != 2) {
                    try diags.append(.{
                        .danger = .@"error",
                        .message = "array type expects length and element type",
                        .span = span{ .start = ap.base.where.start, .end = ap.base.where.end },
                        .code = "E2003",
                        .source_id = ctx.current_source_id,
                    });
                }
                if (std.mem.eql(u8, ap.base.string, "int") or std.mem.eql(u8, ap.base.string, "uint")) {
                    try validate_int_bits(ctx, ap, diags);
                }
                for (ap.args) |arg_ref| {
                    const arg_node = ink.ast.deref(arg_ref);
                    if (arg_node.* == .identifier and ctx.resolve_type(arg_node.identifier.string)) {
                        continue;
                    }
                    try resolve_type_node(ctx, arg_node, diags);
                }
            },
        }
    }

    fn append_dyn_trait_name(base: *?[]const u8, list: *array_list([]const u8), name: []const u8) void {
        if (base.* == null) {
            base.* = name;
            return;
        }
        for (list.items) |item| {
            if (std.mem.eql(u8, item, name)) return;
        }
        list.append(name) catch {};
    }

    fn type_name_from_node(node: *const ink.node) ?[]const u8 {
        return switch (node.*) {
            .type => |ty| switch (ty) {
                .name => |name_id| name_id.string,
                .applied => |ap| ap.base.string,
                else => null,
            },
            .identifier => |ident| ident.string,
            else => null,
        };
    }

    fn collect_dyn_trait_names(
        ctx: *context,
        node: *const ink.node,
        base: *?[]const u8,
        pos: *array_list([]const u8),
        neg: *array_list([]const u8),
    ) void {
        switch (node.*) {
            .type => |ty| switch (ty) {
                .name => |name_id| append_dyn_trait_name(base, pos, name_id.string),
                .applied => |ap| {
                    if (std.mem.eql(u8, ap.base.string, "intersect")) {
                        for (ap.args) |arg_ref| {
                            collect_dyn_trait_names(ctx, ink.ast.deref(arg_ref), base, pos, neg);
                        }
                        return;
                    }
                    if (std.mem.eql(u8, ap.base.string, "not") and ap.args.len >= 1) {
                        const inner_node = ink.ast.deref(ap.args[0]);
                        if (type_name_from_node(inner_node)) |inner_name| {
                            for (neg.items) |item| {
                                if (std.mem.eql(u8, item, inner_name)) return;
                            }
                            neg.append(inner_name) catch {};
                        }
                        return;
                    }
                    append_dyn_trait_name(base, pos, ap.base.string);
                },
                else => {},
            },
            .identifier => |ident| append_dyn_trait_name(base, pos, ident.string),
            else => {},
        }
    }

    const suggestion_info = struct {
        name: []const u8,
        distance: usize,
    };

    fn report_unknown(
        ctx: *context,
        diags: *array_list(diagnostic),
        base_message: []const u8,
        name: []const u8,
        where: span,
        code: []const u8,
        suggestion_name: ?[]const u8,
    ) alloc_error!void {
        if (suggestion_name != null and ctx.diag_messages != null) {
            const msg = try std.fmt.allocPrint(
                ctx.allocator,
                "{s} '{s}'; did you mean '{s}'?",
                .{ base_message, name, suggestion_name.? },
            );
            try ctx.diag_messages.?.append(msg);
            try diags.append(.{
                .danger = .@"error",
                .message = msg,
                .span = where,
                .code = code,
                .source_id = ctx.current_source_id,
            });
            return;
        }
        try diags.append(.{
            .danger = .@"error",
            .message = base_message,
            .span = where,
            .code = code,
            .source_id = ctx.current_source_id,
        });
    }

    fn suggest_value(ctx: *context, name: []const u8) alloc_error!?[]const u8 {
        return suggest_from_maps(ctx, name, .value);
    }

    fn suggest_type(ctx: *context, name: []const u8) alloc_error!?[]const u8 {
        return suggest_from_maps(ctx, name, .type);
    }

    fn suggest_intrinsic(ctx: *context, name: []const u8) alloc_error!?[]const u8 {
        const max_dist = max_suggestion_distance(name);
        if (max_dist == 0) return null;
        var best: ?suggestion_info = null;
        for (intrinsic.names()) |item| {
            try consider_candidate(ctx, name, item.name, max_dist, &best);
        }
        return if (best) |hit| hit.name else null;
    }

    const suggestion_kind = enum { value, type };

    fn suggest_from_maps(ctx: *context, name: []const u8, kind: suggestion_kind) alloc_error!?[]const u8 {
        const max_dist = max_suggestion_distance(name);
        if (max_dist == 0) return null;

        var best: ?suggestion_info = null;

        var i: usize = ctx.scopes.items.len;
        while (i > 0) : (i -= 1) {
            const scope_ref = &ctx.scopes.items[i - 1];
            if (kind == .value) {
                try consider_map(ctx, name, max_dist, &scope_ref.values, &best);
            } else {
                try consider_map(ctx, name, max_dist, &scope_ref.types, &best);
            }
        }

        if (kind == .value) {
            try consider_map(ctx, name, max_dist, &ctx.base_values, &best);
        } else {
            try consider_map(ctx, name, max_dist, &ctx.base_types, &best);
        }

        return if (best) |hit| hit.name else null;
    }

    fn consider_map(
        ctx: *context,
        name: []const u8,
        max_dist: usize,
        map: anytype,
        best: *?suggestion_info,
    ) alloc_error!void {
        var it = map.iterator();
        while (it.next()) |entry| {
            try consider_candidate(ctx, name, entry.key_ptr.*, max_dist, best);
        }
    }

    fn consider_candidate(
        ctx: *context,
        name: []const u8,
        candidate: []const u8,
        max_dist: usize,
        best: *?suggestion_info,
    ) alloc_error!void {
        const distance = try levenshtein_limit(ctx.allocator, name, candidate, max_dist) orelse return;
        if (best.* == null or distance < best.*.?.distance or
            (distance == best.*.?.distance and candidate.len < best.*.?.name.len))
        {
            best.* = .{ .name = candidate, .distance = distance };
        }
    }

    fn max_suggestion_distance(name: []const u8) usize {
        if (name.len <= 2) return 0;
        if (name.len <= 4) return 1;
        if (name.len <= 7) return 2;
        return 3;
    }

    fn levenshtein_limit(
        allocator: mem_allocator,
        a: []const u8,
        b: []const u8,
        max_dist: usize,
    ) alloc_error!?usize {
        if (a.len == 0) return if (b.len <= max_dist) b.len else null;
        if (b.len == 0) return if (a.len <= max_dist) a.len else null;
        const diff = if (a.len > b.len) a.len - b.len else b.len - a.len;
        if (diff > max_dist) return null;

        var prev = try allocator.alloc(usize, b.len + 1);
        defer allocator.free(prev);
        var cur = try allocator.alloc(usize, b.len + 1);
        defer allocator.free(cur);

        var j: usize = 0;
        while (j <= b.len) : (j += 1) {
            prev[j] = j;
        }

        var i: usize = 0;
        while (i < a.len) : (i += 1) {
            cur[0] = i + 1;
            var row_min = cur[0];
            j = 0;
            while (j < b.len) : (j += 1) {
                const cost: usize = if (a[i] == b[j]) 0 else 1;
                const del = prev[j + 1] + 1;
                const ins = cur[j] + 1;
                const sub = prev[j] + cost;
                const value = @min(del, @min(ins, sub));
                cur[j + 1] = value;
                if (value < row_min) row_min = value;
            }
            if (row_min > max_dist) return null;
            std.mem.swap([]usize, &prev, &cur);
        }

        const dist = prev[b.len];
        return if (dist <= max_dist) dist else null;
    }

    fn validate_int_bits(ctx: *context, ap: ink.ast.type_applied, diags: *array_list(diagnostic)) alloc_error!void {
        if (ap.args.len != 1) {
            try diags.append(.{
                .danger = .@"error",
                .message = "int/uint expects a single bit-width argument",
                .span = span{ .start = ap.base.where.start, .end = ap.base.where.end },
                .source_id = ctx.current_source_id,
            });
            return;
        }
        const bits_node = ink.ast.deref(ap.args[0]);
        const bits = eval_const_int(bits_node) orelse {
            try diags.append(.{
                .danger = .@"error",
                .message = "int/uint bit width must be a constant integer expression",
                .span = span_of_node(bits_node),
                .source_id = ctx.current_source_id,
            });
            return;
        };
        if (bits <= 0) {
            try diags.append(.{
                .danger = .@"error",
                .message = "int/uint bit width must be > 0",
                .span = span_of_node(bits_node),
                .source_id = ctx.current_source_id,
            });
        }
    }

    fn eval_const_int(node: *const ink.node) ?i64 {
        return switch (node.*) {
            .integer => |value| value.value,
            .unary => |un| switch (un.op) {
                .neg => blk: {
                    const inner = eval_const_int(ink.ast.deref(un.right)) orelse break :blk null;
                    break :blk -inner;
                },
                else => null,
            },
            .binary => |bin| blk: {
                const left = eval_const_int(ink.ast.deref(bin.left)) orelse break :blk null;
                const right = eval_const_int(ink.ast.deref(bin.right)) orelse break :blk null;
                break :blk switch (bin.op) {
                    .add => left + right,
                    .sub => left - right,
                    .mul => left * right,
                    .div => if (right == 0) null else @divTrunc(left, right),
                    .mod => if (right == 0) null else @mod(left, right),
                    else => null,
                };
            },
            else => null,
        };
    }

    fn span_of_node(node: *const ink.node) ?span {
        return switch (node.*) {
            .identifier => |id| span{ .start = id.where.start, .end = id.where.end },
            .integer => |value| span{ .start = value.where.start, .end = value.where.end },
            .float => |value| span{ .start = value.where.start, .end = value.where.end },
            .duration => |value| span{ .start = value.where.start, .end = value.where.end },
            .string => |str| span{ .start = str.where.start, .end = str.where.end },
            .unary => |un| span_of_node(ink.ast.deref(un.right)),
            .binary => |bin| span_of_node(ink.ast.deref(bin.left)) orelse span_of_node(ink.ast.deref(bin.right)),
            .intrinsic => |call| span{ .start = call.name.where.start, .end = call.name.where.end },
            .macro_call => |mc| span{ .start = mc.where.start, .end = mc.where.end },
            .type => |_| null,
            .label_expr => |le| span{ .start = le.name.where.start, .end = le.name.where.end },
            .loop_expr => |le| span_of_node(ink.ast.deref(le.body)),
            .while_expr => |we| span_of_node(ink.ast.deref(we.condition)) orelse span_of_node(ink.ast.deref(we.body)),
            .while_in_expr => |we| span_of_node(ink.ast.deref(we.pattern)) orelse span_of_node(ink.ast.deref(we.iter)),
            .until_expr => |ue| span_of_node(ink.ast.deref(ue.condition)) orelse span_of_node(ink.ast.deref(ue.body)),
            .repeat_expr => |re| span_of_node(ink.ast.deref(re.count)) orelse span_of_node(ink.ast.deref(re.body)),
            .for_expr => |fe| span_of_node(ink.ast.deref(fe.pattern)) orelse span_of_node(ink.ast.deref(fe.iter)),
            .each_expr => |ee| span_of_node(ink.ast.deref(ee.pattern)) orelse span_of_node(ink.ast.deref(ee.iter)),
            .break_expr => |be| blk: {
                if (be.value) |ref| break :blk span_of_node(ink.ast.deref(ref));
                if (be.label) |lab| break :blk span{ .start = lab.where.start, .end = lab.where.end };
                break :blk null;
            },
            .continue_expr => |ce| if (ce.label) |lab| span{ .start = lab.where.start, .end = lab.where.end } else null,
            .yield_expr => |ye| if (ye.value) |ref| span_of_node(ink.ast.deref(ref)) else null,
            .atomic_expr => |ae| span{ .start = ae.ordering.where.start, .end = ae.ordering.where.end },
            .decl, .if_expr, .match_expr, .select_expr, .with_expr, .block, .record, .associate => null,
        };
    }
};
