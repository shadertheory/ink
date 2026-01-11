const std = @import("std");
const src = @import("source.zig");
const diag = @import("diagnostic.zig");
const resol = @import("resolver.zig");
const ink = @import("root.zig");
const lang_spec = @import("lang/spec.zig");
const target_mod = @import("target.zig");
const backend = @import("backend/backend.zig");
const vm_backend = @import("backend/vm.zig");
const typecheck = @import("typecheck.zig");
const mir_lower = ink.mir_lower;
const lir_lower = ink.lir_lower;
const macro_ctx_mod = @import("macro_context.zig");
const mem_allocator = std.mem.Allocator;
const arena_allocator = std.heap.ArenaAllocator;
const array_list = std.array_list.Managed;
const string_map = std.hash_map.StringHashMap;
const hash_map = std.hash_map.AutoHashMap;
const source_file = src.source_file;
const span = src.span;
const diagnostic = diag.diagnostic;
const resolver = resol.resolver;
const desugar = ink.desugar;

pub const compiler = struct {
    pub const source_id = src.source_id;
    pub const module_id = src.module_id;
    pub const source = src.source_file;

    pub const module_spec = struct {
        name: []const u8,
        sources: []const source_id,
        deps: []const []const u8,
    };

    pub const compile_request = struct {
        sources: []const source,
        modules: []const module_spec,
        root_module: []const u8,
        target: target_mod.target_spec = .{ .kind = .vm },
        prelude: desugar.prelude_spec = desugar.default_prelude,
    };

    pub const compile_result = struct {
        ok: bool,
        diagnostics: []const diagnostic,
        diag_messages: ?[]const []const u8 = null,
        instructions: ?[]const ink.exe.instruction = null,
        constants: ?[]const u64 = null,
        data: ?[]const ink.vm.inkb.data_entry = null,
        bytecode: ?[]u8 = null,
        foreigns: ?[]const []const u8 = null,

        pub fn deinit(self: *compile_result, allocator: mem_allocator) void {
            allocator.free(self.diagnostics);
            if (self.diag_messages) |msgs| {
                for (msgs) |msg| allocator.free(msg);
                allocator.free(msgs);
            }
            if (self.instructions) |insts| allocator.free(insts);
            if (self.constants) |consts| allocator.free(consts);
            if (self.data) |data| {
                for (data) |entry| allocator.free(entry.bytes);
                allocator.free(data);
            }
            if (self.bytecode) |bytes| allocator.free(bytes);
            if (self.foreigns) |foreigns| {
                for (foreigns) |name| allocator.free(name);
                allocator.free(foreigns);
            }
        }
    };

    const module = struct {
        id: module_id,
        name: []const u8,
        sources: []const source_id,
        deps: []const module_id,
    };

    const ast_file = struct {
        source_id: source_id,
        arena: std.heap.ArenaAllocator,
        tokens: []const ink.token,
        nodes: []const *ink.node,

        fn deinit(self: *ast_file, allocator: mem_allocator) void {
            allocator.free(self.tokens);
            self.arena.deinit();
        }
    };

    const macro_overload = struct {
        param_count: usize,
        is_attribute: bool,
        label_offset: usize,
    };

    const macro_runtime = struct {
        bytecode: []u8,
        constants: []const u64,
        data: []const ink.vm.inkb.data_entry,
        foreigns: []const []const u8,
        label_offsets: std.AutoHashMap(u32, usize),
        signatures: []const ink.lir_vm_lower.function_signature,

        fn deinit(self: *macro_runtime, allocator: mem_allocator) void {
            allocator.free(self.bytecode);
            allocator.free(self.constants);
            for (self.data) |entry| allocator.free(entry.bytes);
            allocator.free(self.data);
            for (self.foreigns) |name| allocator.free(name);
            allocator.free(self.foreigns);
            for (self.signatures) |sig| allocator.free(sig.name);
            allocator.free(self.signatures);
            self.label_offsets.deinit();
        }
    };

    const macro_module = struct {
        runtime: macro_runtime,
        macros: string_map(std.array_list.Managed(macro_overload)),

        fn deinit(self: *macro_module, allocator: mem_allocator) void {
            var it = self.macros.iterator();
            while (it.next()) |entry| {
                entry.value_ptr.*.deinit();
            }
            self.macros.deinit();
            self.runtime.deinit(allocator);
        }
    };

    const symbol_import = struct {
        module_id: module_id,
        item: []const u8,
    };

    const macro_imports = struct {
        module_map: string_map(module_id),
        symbol_map: string_map(symbol_import),

        fn init(allocator: mem_allocator) macro_imports {
            return .{
                .module_map = string_map(module_id).init(allocator),
                .symbol_map = string_map(symbol_import).init(allocator),
            };
        }

        fn deinit(self: *macro_imports) void {
            self.module_map.deinit();
            self.symbol_map.deinit();
        }
    };

    const macro_info = struct {
        name: []const u8,
        param_count: usize,
        is_attribute: bool,
        where: ink.location,
    };

    const macro_recursion_limit: usize = 128;
    const macro_step_budget: usize = 1_000_000;

    const macro_target = struct {
        module_id: module_id,
        name: []const u8,
    };

    const macro_expander = struct {
        const macro_error = error{OutOfMemory};

        allocator: mem_allocator,
        macro_ctx: *macro_ctx_mod.macro_context,
        macro_modules: *hash_map(module_id, macro_module),
        imports: *const macro_imports,
        module_id: module_id,
        diags: *array_list(diagnostic),
        diag_messages: *array_list([]const u8),
        macro_arenas: *array_list(arena_allocator),

        fn expand_nodes(
            self: *macro_expander,
            nodes: []const *ink.node,
            tokens: []const ink.token,
            source_id_value: src.source_id,
            node_allocator: mem_allocator,
            out_allocator: mem_allocator,
            depth: usize,
        ) macro_error![]const *ink.node {
            var out = array_list(*ink.node).init(out_allocator);
            for (nodes) |node| {
                try self.expand_node_into(&out, node, tokens, source_id_value, node_allocator, depth);
            }
            return out.toOwnedSlice();
        }

        fn expand_node_into(
            self: *macro_expander,
            out: *array_list(*ink.node),
            node: *ink.node,
            tokens: []const ink.token,
            source_id_value: src.source_id,
            node_allocator: mem_allocator,
            depth: usize,
        ) macro_error!void {
            if (node.* == .decl) {
                return self.expand_decl_into(out, node, tokens, source_id_value, node_allocator, depth);
            }
            const expr = try self.expand_expr(node, tokens, source_id_value, node_allocator, depth);
            try out.append(expr);
        }

        fn expand_decl_into(
            self: *macro_expander,
            out: *array_list(*ink.node),
            node: *ink.node,
            tokens: []const ink.token,
            source_id_value: src.source_id,
            node_allocator: mem_allocator,
            depth: usize,
        ) macro_error!void {
            const decl = node.decl;
            const attrs = decl_attributes(decl);
            if (attrs.len > 0) {
                if (try self.expand_attribute_macro(node, attrs, tokens, source_id_value, node_allocator, depth)) |expanded| {
                    defer self.allocator.free(expanded);
                    try out.appendSlice(expanded);
                    return;
                }
            }

            try self.expand_decl_fields(node, tokens, source_id_value, node_allocator, depth);
            try out.append(node);
        }

        fn expand_attribute_macro(
            self: *macro_expander,
            node: *ink.node,
            attrs: []const ink.ast.attribute,
            tokens: []const ink.token,
            source_id_value: src.source_id,
            node_allocator: mem_allocator,
            depth: usize,
        ) macro_error!?[]const *ink.node {
            _ = node_allocator;
            if (depth >= macro_recursion_limit) {
                try self.diags.append(.{
                    .danger = .@"error",
                    .message = "macro expansion depth exceeded",
                    .span = span{ .start = decl_where(node.decl).start, .end = decl_where(node.decl).end },
                    .source_id = source_id_value,
                });
                return null;
            }

            for (attrs) |attr| {
                if (std.mem.eql(u8, attr.name.string, "attribute")) continue;
                if (std.mem.eql(u8, attr.name.string, "foreign")) continue;

                const target = self.resolve_attribute_macro(attr.name.string) orelse continue;
                const module_entry = self.macro_modules.getPtr(target.module_id) orelse continue;
                const group = module_entry.macros.getPtr(target.name) orelse continue;

                var overload: ?macro_overload = null;
                var arg_streams_buf: [2]u32 = .{ 0, 0 };
                var arg_count: usize = 0;

                self.macro_ctx.reset_errors();
                const args_stream = self.attribute_args_stream(tokens, source_id_value, attr) orelse {
                    try self.diags.append(.{
                        .danger = .@"error",
                        .message = "attribute macro args failed",
                        .span = span{ .start = attr.where.start, .end = attr.where.end },
                        .source_id = source_id_value,
                    });
                    return null;
                };
                const item_stream = self.attribute_item_stream(tokens, source_id_value, decl_where(node.decl), attr) orelse 0;
                if (item_stream == 0) {
                    try self.diags.append(.{
                        .danger = .@"error",
                        .message = "attribute macro input failed",
                        .span = span{ .start = attr.where.start, .end = attr.where.end },
                        .source_id = source_id_value,
                    });
                    return null;
                }
                if (self.macro_ctx.failed) return null;

                const args_empty = self.macro_ctx.token_stream_len(args_stream) == 0;
                const has_one = self.find_macro_overload(group.items, 1, true) != null;
                const has_two = self.find_macro_overload(group.items, 2, true) != null;
                if (args_empty and has_one and has_two) {
                    try self.diags.append(.{
                        .danger = .@"error",
                        .message = "ambiguous attribute macro overload",
                        .span = span{ .start = attr.where.start, .end = attr.where.end },
                        .source_id = source_id_value,
                    });
                    return null;
                }

                if (args_empty and has_one) {
                    overload = self.find_macro_overload(group.items, 1, true);
                    arg_streams_buf[0] = item_stream;
                    arg_count = 1;
                } else {
                    overload = self.find_macro_overload(group.items, 2, true);
                    arg_streams_buf[0] = if (args_stream != 0) args_stream else self.macro_ctx.token_stream_empty();
                    arg_streams_buf[1] = item_stream;
                    arg_count = 2;
                }

                if (overload == null) {
                    try self.diags.append(.{
                        .danger = .@"error",
                        .message = "no matching attribute macro overload",
                        .span = span{ .start = attr.where.start, .end = attr.where.end },
                        .source_id = source_id_value,
                    });
                    return null;
                }

                const call_span = self.macro_ctx.span_from_location(source_id_value, attr.where);
                const out_stream = try self.run_macro(&module_entry.runtime, overload.?, arg_streams_buf[0..arg_count], call_span, attr.where, source_id_value);
                if (out_stream == null) return null;

                const parsed = try self.parse_macro_program(out_stream.?, attr.where, source_id_value);
                if (parsed == null) return null;
                defer self.allocator.free(parsed.?.tokens);
                const expanded = try self.expand_nodes(
                    parsed.?.nodes,
                    parsed.?.tokens,
                    source_id_value,
                    parsed.?.node_allocator,
                    self.allocator,
                    depth + 1,
                );
                return expanded;
            }

            return null;
        }

        fn expand_decl_fields(
            self: *macro_expander,
            node: *ink.node,
            tokens: []const ink.token,
            source_id_value: src.source_id,
            node_allocator: mem_allocator,
            depth: usize,
        ) macro_error!void {
            switch (node.decl) {
                .function => |*func| {
                    if (func.body) |body_ref| {
                        const body = ink.ast.deref(body_ref);
                        const expanded = try self.expand_expr(body, tokens, source_id_value, node_allocator, depth);
                        func.body = ink.ast.ref_opt(expanded);
                    }
                },
                .@"const" => |*c| {
                    const expanded = try self.expand_expr(ink.ast.deref(c.value), tokens, source_id_value, node_allocator, depth);
                    c.value = ink.ast.ref(expanded);
                },
                .@"var" => |*v| {
                    const expanded = try self.expand_expr(ink.ast.deref(v.value), tokens, source_id_value, node_allocator, depth);
                    v.value = ink.ast.ref(expanded);
                },
                .impl => |*impl_decl| {
                    const funcs = @constCast(impl_decl.functions);
                    for (funcs) |*func| {
                        if (func.body) |body_ref| {
                            const body = ink.ast.deref(body_ref);
                            const expanded = try self.expand_expr(body, tokens, source_id_value, node_allocator, depth);
                            func.body = ink.ast.ref_opt(expanded);
                        }
                    }
                },
                .trait => |*trait_decl| {
                    const items = @constCast(trait_decl.items);
                    for (items) |*item| {
                        switch (item.*) {
                            .function => |*func| {
                                if (func.body) |body_ref| {
                                    const body = ink.ast.deref(body_ref);
                                    const expanded = try self.expand_expr(body, tokens, source_id_value, node_allocator, depth);
                                    func.body = ink.ast.ref_opt(expanded);
                                }
                            },
                            .assoc_type => |*assoc| {
                                if (assoc.value) |value_ref| {
                                    const expanded = try self.expand_expr(ink.ast.deref(value_ref), tokens, source_id_value, node_allocator, depth);
                                    assoc.value = ink.ast.ref_opt(expanded);
                                }
                            },
                        }
                    }
                    const requires = @constCast(trait_decl.requires);
                    for (requires) |*req_ref| {
                        const expanded = try self.expand_expr(ink.ast.deref(req_ref.*), tokens, source_id_value, node_allocator, depth);
                        req_ref.* = ink.ast.ref(expanded);
                    }
                },
                .type_alias => |*ty| {
                    const expanded = try self.expand_expr(ink.ast.deref(ty.value), tokens, source_id_value, node_allocator, depth);
                    ty.value = ink.ast.ref(expanded);
                },
                else => {},
            }
        }

        fn expand_expr(
            self: *macro_expander,
            node: *ink.node,
            tokens: []const ink.token,
            source_id_value: src.source_id,
            node_allocator: mem_allocator,
            depth: usize,
        ) macro_error!*ink.node {
            switch (node.*) {
                .macro_call => |mc| {
                    if (depth >= macro_recursion_limit) {
                        try self.diags.append(.{
                            .danger = .@"error",
                            .message = "macro expansion depth exceeded",
                            .span = span{ .start = mc.where.start, .end = mc.where.end },
                            .source_id = source_id_value,
                        });
                        return node;
                    }
                    const expanded = try self.expand_macro_call(node, mc, tokens, source_id_value, node_allocator, depth);
                    return expanded;
                },
                .unary => |*un| {
                    const expanded = try self.expand_expr(ink.ast.deref(un.right), tokens, source_id_value, node_allocator, depth);
                    un.right = ink.ast.ref(expanded);
                },
                .binary => |*bin| {
                    const left = try self.expand_expr(ink.ast.deref(bin.left), tokens, source_id_value, node_allocator, depth);
                    const right = try self.expand_expr(ink.ast.deref(bin.right), tokens, source_id_value, node_allocator, depth);
                    bin.left = ink.ast.ref(left);
                    bin.right = ink.ast.ref(right);
                },
                .if_expr => |*ife| {
                    const cond = try self.expand_expr(ink.ast.deref(ife.condition), tokens, source_id_value, node_allocator, depth);
                    const then_branch = try self.expand_expr(ink.ast.deref(ife.then_branch), tokens, source_id_value, node_allocator, depth);
                    ife.condition = ink.ast.ref(cond);
                    ife.then_branch = ink.ast.ref(then_branch);
                    if (ife.else_branch) |else_ref| {
                        const else_node = try self.expand_expr(ink.ast.deref(else_ref), tokens, source_id_value, node_allocator, depth);
                        ife.else_branch = ink.ast.ref_opt(else_node);
                    }
                },
                .match_expr => |*me| {
                    const target = try self.expand_expr(ink.ast.deref(me.target), tokens, source_id_value, node_allocator, depth);
                    me.target = ink.ast.ref(target);
                    const arms = @constCast(me.arms);
                    for (arms) |*arm| {
                        const pattern = try self.expand_expr(ink.ast.deref(arm.pattern), tokens, source_id_value, node_allocator, depth);
                        const body = try self.expand_expr(ink.ast.deref(arm.body), tokens, source_id_value, node_allocator, depth);
                        arm.pattern = ink.ast.ref(pattern);
                        arm.body = ink.ast.ref(body);
                    }
                },
                .select_expr => |*se| {
                    const arms = @constCast(se.arms);
                    for (arms) |*arm| {
                        const task = try self.expand_expr(ink.ast.deref(arm.task), tokens, source_id_value, node_allocator, depth);
                        const body = try self.expand_expr(ink.ast.deref(arm.body), tokens, source_id_value, node_allocator, depth);
                        arm.task = ink.ast.ref(task);
                        arm.body = ink.ast.ref(body);
                    }
                },
                .with_expr => |*we| {
                    const body = try self.expand_expr(ink.ast.deref(we.body), tokens, source_id_value, node_allocator, depth);
                    we.body = ink.ast.ref(body);
                },
                .label_expr => |*le| {
                    const body = try self.expand_expr(ink.ast.deref(le.body), tokens, source_id_value, node_allocator, depth);
                    le.body = ink.ast.ref(body);
                },
                .loop_expr => |*le| {
                    const body = try self.expand_expr(ink.ast.deref(le.body), tokens, source_id_value, node_allocator, depth);
                    le.body = ink.ast.ref(body);
                },
                .while_expr => |*we| {
                    const cond = try self.expand_expr(ink.ast.deref(we.condition), tokens, source_id_value, node_allocator, depth);
                    const body = try self.expand_expr(ink.ast.deref(we.body), tokens, source_id_value, node_allocator, depth);
                    we.condition = ink.ast.ref(cond);
                    we.body = ink.ast.ref(body);
                },
                .while_in_expr => |*we| {
                    const iter = try self.expand_expr(ink.ast.deref(we.iter), tokens, source_id_value, node_allocator, depth);
                    const body = try self.expand_expr(ink.ast.deref(we.body), tokens, source_id_value, node_allocator, depth);
                    const pattern = try self.expand_expr(ink.ast.deref(we.pattern), tokens, source_id_value, node_allocator, depth);
                    we.iter = ink.ast.ref(iter);
                    we.body = ink.ast.ref(body);
                    we.pattern = ink.ast.ref(pattern);
                },
                .until_expr => |*ue| {
                    const cond = try self.expand_expr(ink.ast.deref(ue.condition), tokens, source_id_value, node_allocator, depth);
                    const body = try self.expand_expr(ink.ast.deref(ue.body), tokens, source_id_value, node_allocator, depth);
                    ue.condition = ink.ast.ref(cond);
                    ue.body = ink.ast.ref(body);
                },
                .repeat_expr => |*re| {
                    const count = try self.expand_expr(ink.ast.deref(re.count), tokens, source_id_value, node_allocator, depth);
                    const body = try self.expand_expr(ink.ast.deref(re.body), tokens, source_id_value, node_allocator, depth);
                    re.count = ink.ast.ref(count);
                    re.body = ink.ast.ref(body);
                },
                .for_expr => |*fe| {
                    const iter = try self.expand_expr(ink.ast.deref(fe.iter), tokens, source_id_value, node_allocator, depth);
                    const body = try self.expand_expr(ink.ast.deref(fe.body), tokens, source_id_value, node_allocator, depth);
                    const pattern = try self.expand_expr(ink.ast.deref(fe.pattern), tokens, source_id_value, node_allocator, depth);
                    fe.iter = ink.ast.ref(iter);
                    fe.body = ink.ast.ref(body);
                    fe.pattern = ink.ast.ref(pattern);
                },
                .each_expr => |*ee| {
                    const iter = try self.expand_expr(ink.ast.deref(ee.iter), tokens, source_id_value, node_allocator, depth);
                    const body = try self.expand_expr(ink.ast.deref(ee.body), tokens, source_id_value, node_allocator, depth);
                    const pattern = try self.expand_expr(ink.ast.deref(ee.pattern), tokens, source_id_value, node_allocator, depth);
                    ee.iter = ink.ast.ref(iter);
                    ee.body = ink.ast.ref(body);
                    ee.pattern = ink.ast.ref(pattern);
                },
                .break_expr => |*be| {
                    if (be.value) |val_ref| {
                        const value = try self.expand_expr(ink.ast.deref(val_ref), tokens, source_id_value, node_allocator, depth);
                        be.value = ink.ast.ref_opt(value);
                    }
                },
                .yield_expr => |*ye| {
                    if (ye.value) |val_ref| {
                        const value = try self.expand_expr(ink.ast.deref(val_ref), tokens, source_id_value, node_allocator, depth);
                        ye.value = ink.ast.ref_opt(value);
                    }
                },
                .atomic_expr => |*ae| {
                    const value = try self.expand_expr(ink.ast.deref(ae.value), tokens, source_id_value, node_allocator, depth);
                    ae.value = ink.ast.ref(value);
                },
                .block => |*block| {
                    const items = @constCast(block.items);
                    var item_nodes = try self.allocator.alloc(*ink.node, items.len);
                    defer self.allocator.free(item_nodes);
                    for (items, 0..) |item_ref, i| {
                        item_nodes[i] = ink.ast.deref(item_ref);
                    }
                    const expanded_items = try self.expand_nodes(
                        item_nodes,
                        tokens,
                        source_id_value,
                        node_allocator,
                        node_allocator,
                        depth,
                    );
                    block.items = ink.ast.ref_slice(expanded_items);
                },
                .record => |*rec| {
                    const items = @constCast(rec.items);
                    for (items) |*assoc| {
                        if (assoc.value) |val_ref| {
                            const value = try self.expand_expr(ink.ast.deref(val_ref), tokens, source_id_value, node_allocator, depth);
                            assoc.value = ink.ast.ref_opt(value);
                        }
                    }
                },
                .intrinsic => |*call| {
                    const args = @constCast(call.args);
                    for (args) |*arg_ref| {
                        const value = try self.expand_expr(ink.ast.deref(arg_ref.*), tokens, source_id_value, node_allocator, depth);
                        arg_ref.* = ink.ast.ref(value);
                    }
                },
                .associate => |*assoc| {
                    if (assoc.value) |val_ref| {
                        const value = try self.expand_expr(ink.ast.deref(val_ref), tokens, source_id_value, node_allocator, depth);
                        assoc.value = ink.ast.ref_opt(value);
                    }
                },
                else => {},
            }

            return node;
        }

        fn expand_macro_call(
            self: *macro_expander,
            node: *ink.node,
            mc: ink.ast.macro_call,
            tokens: []const ink.token,
            source_id_value: src.source_id,
            node_allocator: mem_allocator,
            depth: usize,
        ) macro_error!*ink.node {
            _ = node_allocator;
            self.macro_ctx.reset_errors();
            const target_node = ink.ast.deref(mc.target);
            const target = self.resolve_macro_target(target_node) orelse {
                try self.diags.append(.{
                    .danger = .@"error",
                    .message = "unknown macro",
                    .span = span{ .start = mc.where.start, .end = mc.where.end },
                    .source_id = source_id_value,
                });
                return node;
            };

            const module_entry = self.macro_modules.getPtr(target.module_id) orelse {
                try self.diags.append(.{
                    .danger = .@"error",
                    .message = "unknown macro",
                    .span = span{ .start = mc.where.start, .end = mc.where.end },
                    .source_id = source_id_value,
                });
                return node;
            };
            const group = module_entry.macros.getPtr(target.name) orelse {
                try self.diags.append(.{
                    .danger = .@"error",
                    .message = "unknown macro",
                    .span = span{ .start = mc.where.start, .end = mc.where.end },
                    .source_id = source_id_value,
                });
                return node;
            };

            const body_stream = self.macro_ctx.token_stream_from_source(tokens, source_id_value, mc.body.start, mc.body.end);
            if (body_stream == 0) {
                try self.diags.append(.{
                    .danger = .@"error",
                    .message = "macro input failed",
                    .span = span{ .start = mc.where.start, .end = mc.where.end },
                    .source_id = source_id_value,
                });
                return node;
            }
            if (self.macro_ctx.failed) return node;

            var arg_streams = try split_stream_on_newlines(self.macro_ctx, body_stream, self.allocator);
            defer self.allocator.free(arg_streams);

            var overload = self.find_macro_overload(group.items, arg_streams.len, false);
            if (arg_streams.len == 0) {
                const has_zero = self.find_macro_overload(group.items, 0, false) != null;
                const has_one = self.find_macro_overload(group.items, 1, false) != null;
                if (has_zero and has_one) {
                    try self.diags.append(.{
                        .danger = .@"error",
                        .message = "ambiguous macro overload",
                        .span = span{ .start = mc.where.start, .end = mc.where.end },
                        .source_id = source_id_value,
                    });
                    return node;
                }
                if (has_one) {
                    const empty = self.macro_ctx.token_stream_empty();
                    if (empty == 0) return node;
                    self.allocator.free(arg_streams);
                    arg_streams = try self.allocator.alloc(u32, 1);
                    arg_streams[0] = empty;
                    overload = self.find_macro_overload(group.items, 1, false);
                } else {
                    overload = self.find_macro_overload(group.items, 0, false);
                }
            }

            if (overload == null) {
                try self.diags.append(.{
                    .danger = .@"error",
                    .message = "no matching macro overload",
                    .span = span{ .start = mc.where.start, .end = mc.where.end },
                    .source_id = source_id_value,
                });
                return node;
            }

            const call_span = self.macro_ctx.span_from_location(source_id_value, mc.where);
            const out_stream = try self.run_macro(&module_entry.runtime, overload.?, arg_streams, call_span, mc.where, source_id_value);
            if (out_stream == null) return node;

            const parsed = try self.parse_macro_expr(out_stream.?, mc.where, source_id_value);
            if (parsed == null) return node;
            defer self.allocator.free(parsed.?.tokens);
            const expanded = try self.expand_expr(parsed.?.node, parsed.?.tokens, source_id_value, parsed.?.node_allocator, depth + 1);
            return expanded;
        }

        fn resolve_macro_target(self: *macro_expander, node: *ink.node) ?macro_target {
            switch (node.*) {
                .identifier => |id| {
                    if (self.macro_modules.getPtr(self.module_id)) |mod| {
                        if (mod.macros.contains(id.string)) {
                            return .{ .module_id = self.module_id, .name = id.string };
                        }
                    }
                    if (self.imports.symbol_map.get(id.string)) |sym| {
                        return .{ .module_id = sym.module_id, .name = sym.item };
                    }
                },
                .binary => |bin| {
                    if (bin.op != .scope_access) return null;
                    const left = ink.ast.deref(bin.left);
                    const right = ink.ast.deref(bin.right);
                    if (left.* != .identifier or right.* != .identifier) return null;
                    const left_id = left.identifier;
                    const right_id = right.identifier;
                    if (self.imports.module_map.get(left_id.string)) |module_id_val| {
                        return .{ .module_id = module_id_val, .name = right_id.string };
                    }
                },
                else => {},
            }
            return null;
        }

        fn resolve_attribute_macro(self: *macro_expander, name: []const u8) ?macro_target {
            if (self.macro_modules.getPtr(self.module_id)) |mod| {
                if (mod.macros.contains(name)) {
                    return .{ .module_id = self.module_id, .name = name };
                }
            }
            if (self.imports.symbol_map.get(name)) |sym| {
                return .{ .module_id = sym.module_id, .name = sym.item };
            }
            return null;
        }

        fn find_macro_overload(
            self: *macro_expander,
            group: []const macro_overload,
            param_count: usize,
            is_attribute: bool,
        ) ?macro_overload {
            _ = self;
            for (group) |entry| {
                if (entry.param_count == param_count and entry.is_attribute == is_attribute) return entry;
            }
            return null;
        }

        fn attribute_args_stream(
            self: *macro_expander,
            tokens: []const ink.token,
            source_id_value: src.source_id,
            attr: ink.ast.attribute,
        ) ?u32 {
            var left: ?ink.token = null;
            var right: ?ink.token = null;
            var depth: usize = 0;
            for (tokens) |tok| {
                if (tok.where.start < attr.where.start or tok.where.end > attr.where.end) continue;
                switch (tok.which) {
                    .paren_left => {
                        if (depth == 0) left = tok;
                        depth += 1;
                    },
                    .paren_right => {
                        if (depth > 0) depth -= 1;
                        if (depth == 0) {
                            right = tok;
                            break;
                        }
                    },
                    else => {},
                }
            }
            if (left == null or right == null) {
                const empty = self.macro_ctx.token_stream_empty();
                if (empty == 0) return null;
                return empty;
            }
            if (right.?.where.start <= left.?.where.end) {
                const empty = self.macro_ctx.token_stream_empty();
                if (empty == 0) return null;
                return empty;
            }
            const stream = self.macro_ctx.token_stream_from_source(tokens, source_id_value, left.?.where.end, right.?.where.start);
            if (stream == 0) return null;
            return stream;
        }

        fn attribute_item_stream(
            self: *macro_expander,
            tokens: []const ink.token,
            source_id_value: src.source_id,
            item_loc: ink.location,
            attr: ink.ast.attribute,
        ) ?u32 {
            var filtered = array_list(ink.token).init(self.allocator);
            defer filtered.deinit();
            for (tokens) |tok| {
                if (tok.where.start < item_loc.start or tok.where.end > item_loc.end) continue;
                if (tok.where.start >= attr.where.start and tok.where.end <= attr.where.end) continue;
                filtered.append(tok) catch return null;
            }
            return self.macro_ctx.token_stream_from_source(filtered.items, source_id_value, item_loc.start, item_loc.end);
        }

        fn run_macro(
            self: *macro_expander,
            runtime: *macro_runtime,
            overload: macro_overload,
            args: []const u32,
            call_span: u32,
            call_loc: ink.location,
            source_id_value: src.source_id,
        ) macro_error!?u32 {
            const max_regs: usize = @intCast(ink.vm.register_count);
            if (args.len + 1 > max_regs) {
                try self.diags.append(.{
                    .danger = .@"error",
                    .message = "macro argument overflow",
                    .span = span{ .start = call_loc.start, .end = call_loc.end },
                    .source_id = source_id_value,
                });
                return null;
            }
            self.macro_ctx.reset_errors();
            self.macro_ctx.push_call_site(call_span);
            defer self.macro_ctx.pop_call_site();

            var machine = ink.vm.vm.init(
                self.allocator,
                runtime.bytecode,
                runtime.constants,
                runtime.data,
                runtime.foreigns,
                null,
                null,
            );
            defer machine.deinit();

            machine.processor.current.pc = overload.label_offset;
            machine.processor.current.fp = 2;
            machine.processor.current.sp = 2 + max_regs;
            machine.processor.halted = false;
            machine.processor.suspended = false;
            machine.memory.write(0, 0);
            machine.memory.write(1, 0);

            var i: usize = 0;
            while (i < args.len and i + 1 < max_regs) : (i += 1) {
                machine.memory.write(machine.processor.current.fp + 1 + i, @intCast(args[i]));
            }

            _ = machine.step(macro_step_budget);

            if (machine.processor.suspended) {
                try self.diags.append(.{
                    .danger = .@"error",
                    .message = "macro suspended during expansion",
                    .span = span{ .start = call_loc.start, .end = call_loc.end },
                    .source_id = source_id_value,
                });
                return null;
            }
            if (!machine.processor.halted) {
                try self.diags.append(.{
                    .danger = .@"error",
                    .message = "macro execution limit exceeded",
                    .span = span{ .start = call_loc.start, .end = call_loc.end },
                    .source_id = source_id_value,
                });
                return null;
            }

            if (self.macro_ctx.failed) return null;

            const raw = machine.processor.return_value();
            if (raw == 0) {
                try self.diags.append(.{
                    .danger = .@"error",
                    .message = "macro returned invalid token_stream",
                    .span = span{ .start = call_loc.start, .end = call_loc.end },
                    .source_id = source_id_value,
                });
                return null;
            }
            return @intCast(raw);
        }

        const parsed_expr = struct {
            node: *ink.node,
            tokens: []const ink.token,
            node_allocator: mem_allocator,
        };

        const parsed_program = struct {
            nodes: []const *ink.node,
            tokens: []const ink.token,
            node_allocator: mem_allocator,
        };

        fn parse_macro_expr(
            self: *macro_expander,
            stream_id: u32,
            call_loc: ink.location,
            source_id_value: src.source_id,
        ) macro_error!?parsed_expr {
            const tokens = self.macro_ctx.token_stream_to_tokens(stream_id, call_loc, self.allocator) catch {
                try self.diags.append(.{
                    .danger = .@"error",
                    .message = "macro output tokenization failed",
                    .span = span{ .start = call_loc.start, .end = call_loc.end },
                    .source_id = source_id_value,
                });
                return null;
            };
            const tokens_no_eof = if (tokens.len > 0 and tokens[tokens.len - 1].which == .end_of_file)
                tokens[0 .. tokens.len - 1]
            else
                tokens;

            var parse = ink.peg_parser.parse_from(self.allocator, tokens_no_eof, ink.peg.nonterminal_kind.expr) catch {
                self.allocator.free(tokens);
                try self.diags.append(.{
                    .danger = .@"error",
                    .message = "macro output parse failed",
                    .span = span{ .start = call_loc.start, .end = call_loc.end },
                    .source_id = source_id_value,
                });
                return null;
            };
            if (!parse.ok) {
                if (parse.@"error") |info| {
                    const msg = try format_parse_error(self.allocator, info, tokens);
                    errdefer self.allocator.free(msg);
                    try self.diag_messages.append(msg);
                    try self.diags.append(.{
                        .danger = .@"error",
                        .message = msg,
                        .span = span_from_token_index(tokens, info.position),
                        .source_id = source_id_value,
                    });
                } else {
                    try self.diags.append(.{
                        .danger = .@"error",
                        .message = "macro output parse error",
                        .span = span{ .start = call_loc.start, .end = call_loc.end },
                        .source_id = source_id_value,
                    });
                }
                parse.deinit();
                self.allocator.free(tokens);
                return null;
            }

            var builder = ink.peg_ast.builder.init(parse.arena.allocator(), tokens, &parse.tree, "");
            const expr = builder.build_expr_root(parse.root.?) catch |err| {
                switch (err) {
                    else => {},
                }
                if (builder.last_error) |info| {
                    const msg = try format_ast_error(self.allocator, info, tokens);
                    errdefer self.allocator.free(msg);
                    try self.diag_messages.append(msg);
                    try self.diags.append(.{
                        .danger = .@"error",
                        .message = msg,
                        .span = span_from_token_index(tokens, info.position),
                        .source_id = source_id_value,
                    });
                } else {
                    try self.diags.append(.{
                        .danger = .@"error",
                        .message = "macro output ast error",
                        .span = span{ .start = call_loc.start, .end = call_loc.end },
                        .source_id = source_id_value,
                    });
                }
                parse.deinit();
                self.allocator.free(tokens);
                return null;
            };

            parse.tree.deinit(parse.arena.allocator());
            try self.macro_arenas.append(parse.arena);
            return .{ .node = expr, .tokens = tokens, .node_allocator = parse.arena.allocator() };
        }

        fn parse_macro_program(
            self: *macro_expander,
            stream_id: u32,
            call_loc: ink.location,
            source_id_value: src.source_id,
        ) macro_error!?parsed_program {
            const tokens = self.macro_ctx.token_stream_to_tokens(stream_id, call_loc, self.allocator) catch {
                try self.diags.append(.{
                    .danger = .@"error",
                    .message = "macro output tokenization failed",
                    .span = span{ .start = call_loc.start, .end = call_loc.end },
                    .source_id = source_id_value,
                });
                return null;
            };

            var parse = ink.peg_parser.parse_from(self.allocator, tokens, ink.peg.nonterminal_kind.program) catch {
                self.allocator.free(tokens);
                try self.diags.append(.{
                    .danger = .@"error",
                    .message = "macro output parse failed",
                    .span = span{ .start = call_loc.start, .end = call_loc.end },
                    .source_id = source_id_value,
                });
                return null;
            };
            if (!parse.ok) {
                if (parse.@"error") |info| {
                    const msg = try format_parse_error(self.allocator, info, tokens);
                    errdefer self.allocator.free(msg);
                    try self.diag_messages.append(msg);
                    try self.diags.append(.{
                        .danger = .@"error",
                        .message = msg,
                        .span = span_from_token_index(tokens, info.position),
                        .source_id = source_id_value,
                    });
                } else {
                    try self.diags.append(.{
                        .danger = .@"error",
                        .message = "macro output parse error",
                        .span = span{ .start = call_loc.start, .end = call_loc.end },
                        .source_id = source_id_value,
                    });
                }
                parse.deinit();
                self.allocator.free(tokens);
                return null;
            }

            var builder = ink.peg_ast.builder.init(parse.arena.allocator(), tokens, &parse.tree, "");
            const nodes = builder.build_program(parse.root.?) catch |err| {
                switch (err) {
                    else => {},
                }
                if (builder.last_error) |info| {
                    const msg = try format_ast_error(self.allocator, info, tokens);
                    errdefer self.allocator.free(msg);
                    try self.diag_messages.append(msg);
                    try self.diags.append(.{
                        .danger = .@"error",
                        .message = msg,
                        .span = span_from_token_index(tokens, info.position),
                        .source_id = source_id_value,
                    });
                } else {
                    try self.diags.append(.{
                        .danger = .@"error",
                        .message = "macro output ast error",
                        .span = span{ .start = call_loc.start, .end = call_loc.end },
                        .source_id = source_id_value,
                    });
                }
                parse.deinit();
                self.allocator.free(tokens);
                return null;
            };

            parse.tree.deinit(parse.arena.allocator());
            try self.macro_arenas.append(parse.arena);
            return .{ .nodes = nodes, .tokens = tokens, .node_allocator = parse.arena.allocator() };
        }
    };

    fn decl_attributes(decl: ink.ast.decl) []const ink.ast.attribute {
        return switch (decl) {
            .function => |f| f.attributes,
            .@"struct" => |s| s.attributes,
            .trait => |t| t.attributes,
            .@"enum" => |e| e.attributes,
            .impl => |i| i.attributes,
            .import => |i| i.attributes,
            .type_alias => |t| t.attributes,
            .@"const" => |c| c.attributes,
            .@"var" => |v| v.attributes,
        };
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

    fn tree_is_newline(ctx: *macro_ctx_mod.macro_context, tree_id: u32) bool {
        const kind = ctx.tree_kind(tree_id) orelse return false;
        if (kind != .token) return false;
        const tok_id = ctx.tree_token(tree_id);
        const tok_kind = ctx.token_kind_of(tok_id) orelse return false;
        return tok_kind == .new_line;
    }

    fn split_stream_on_newlines(
        ctx: *macro_ctx_mod.macro_context,
        stream_id: u32,
        allocator: mem_allocator,
    ) ![]u32 {
        var out = array_list(u32).init(allocator);
        const len = ctx.token_stream_len(stream_id);
        var start: usize = 0;
        var idx: usize = 0;
        while (idx < len) : (idx += 1) {
            const tree_id = ctx.token_stream_get(stream_id, idx);
            if (!tree_is_newline(ctx, tree_id)) continue;
            if (idx > start) {
                const slice_id = ctx.token_stream_slice(stream_id, start, idx);
                if (slice_id == 0) return error.OutOfMemory;
                try out.append(slice_id);
            }
            start = idx + 1;
        }
        if (len > start) {
            const slice_id = ctx.token_stream_slice(stream_id, start, len);
            if (slice_id == 0) return error.OutOfMemory;
            try out.append(slice_id);
        }
        return out.toOwnedSlice();
    }

    pub fn compile(allocator: mem_allocator, req: compile_request) !compile_result {
        var diags = array_list(diagnostic).init(allocator);
        errdefer diags.deinit();
        var diag_messages = array_list([]const u8).init(allocator);
        errdefer {
            for (diag_messages.items) |msg| allocator.free(msg);
            diag_messages.deinit();
        }

        // 1) source store
        var sources_by_id = hash_map(source_id, source_file).init(allocator);
        defer sources_by_id.deinit();
        var max_source_id: src.source_id = 0;

        for (req.sources) |compsrc| {
            if (sources_by_id.contains(compsrc.id)) {
                try diags.append(.{ .danger = .@"error", .message = "duplicate source id", .span = null });
            } else {
                try sources_by_id.put(compsrc.id, compsrc);
            }
            if (compsrc.id > max_source_id) {
                max_source_id = compsrc.id;
            }
        }

        // 2) module graph
        var module_names = string_map(module_id).init(allocator);
        defer module_names.deinit();

        var modules = array_list(module).init(allocator);
        defer {
            for (modules.items) |mod| allocator.free(mod.deps);
            modules.deinit();
        }

        for (req.modules, 0..) |spec, idx| {
            if (module_names.contains(spec.name)) {
                try diags.append(.{ .danger = .@"error", .message = "duplicate module name", .span = null });
                continue;
            }
            const id: module_id = @intCast(idx);
            try module_names.put(spec.name, id);
            try modules.append(.{
                .id = id,
                .name = spec.name,
                .sources = spec.sources,
                .deps = &[_]module_id{},
            });
        }

        // resolve deps + validate sources
        for (req.modules, 0..) |spec, idx| {
            var deps = try allocator.alloc(module_id, spec.deps.len);
            for (spec.deps, 0..) |dep_name, j| {
                if (module_names.get(dep_name)) |dep_id| {
                    deps[j] = dep_id;
                } else {
                    try diags.append(.{ .danger = .@"error", .message = "unknown module dependency", .span = null });
                    deps[j] = 0;
                }
            }
            for (spec.sources) |sid| {
                if (!sources_by_id.contains(sid)) {
                    try diags.append(.{ .danger = .@"error", .message = "unknown source id", .span = null });
                }
            }
            modules.items[idx].deps = deps;
        }

        if (has_error(diags.items)) {
            return finish(&diags, &diag_messages, null, null, null, null, null, false);
        }

        // 3) parse per file + 4) build per-module node list
        var module_nodes = try allocator.alloc([]const *ink.node, modules.items.len);
        var module_node_sources = try allocator.alloc([]const src.source_id, modules.items.len);
        var module_raw_nodes = try allocator.alloc([]const *ink.node, modules.items.len);
        var module_raw_sources = try allocator.alloc([]const src.source_id, modules.items.len);
        var module_files = try allocator.alloc([]ast_file, modules.items.len);
        var module_imports = try allocator.alloc([]const resolver.module_import, modules.items.len);
        var module_import_specs = try allocator.alloc(?[]const desugar.import_decl, modules.items.len);
        var module_foreigns = try allocator.alloc([]const []const u8, modules.items.len);
        var module_arenas = try allocator.alloc(arena_allocator, modules.items.len);
        var module_exports = try allocator.alloc(string_map(void), modules.items.len);
        defer {
            var i: usize = 0;
            while (i < modules.items.len) : (i += 1) {
                for (module_files[i]) |*file| file.deinit(allocator);
                allocator.free(module_files[i]);
                allocator.free(module_nodes[i]);
                allocator.free(module_node_sources[i]);
                allocator.free(module_raw_nodes[i]);
                allocator.free(module_raw_sources[i]);
                allocator.free(module_imports[i]);
                if (module_import_specs[i]) |imports| allocator.free(imports);
                free_foreign_list(allocator, module_foreigns[i]);
                module_arenas[i].deinit();
                module_exports[i].deinit();
            }
            allocator.free(module_files);
            allocator.free(module_nodes);
            allocator.free(module_node_sources);
            allocator.free(module_raw_nodes);
            allocator.free(module_raw_sources);
            allocator.free(module_imports);
            allocator.free(module_import_specs);
            allocator.free(module_foreigns);
            allocator.free(module_arenas);
            allocator.free(module_exports);
        }

        for (module_import_specs) |*slot| slot.* = null;
        for (module_exports) |*exports| exports.* = string_map(void).init(allocator);

        for (module_arenas) |*arena| {
            arena.* = arena_allocator.init(allocator);
        }

        for (module_nodes) |*slot| slot.* = try allocator.alloc(*ink.node, 0);
        for (module_node_sources) |*slot| slot.* = try allocator.alloc(src.source_id, 0);
        for (module_raw_nodes) |*slot| slot.* = try allocator.alloc(*ink.node, 0);
        for (module_raw_sources) |*slot| slot.* = try allocator.alloc(src.source_id, 0);
        for (module_imports) |*slot| slot.* = try allocator.alloc(resolver.module_import, 0);
        for (module_foreigns) |*slot| slot.* = try allocator.alloc([]const u8, 0);

        for (modules.items, 0..) |mod, mi| {
            var files = array_list(ast_file).init(allocator);
            errdefer files.deinit();

            var nodes = array_list(*ink.node).init(allocator);
            errdefer nodes.deinit();

            var node_sources = array_list(src.source_id).init(allocator);
            errdefer node_sources.deinit();

            for (mod.sources) |sid| {
                const compsrc = sources_by_id.get(sid).?;

                const tokens = try lex_all(allocator, compsrc.text, &diags, compsrc.id);
                var parse = try ink.peg_parser.parse(allocator, tokens);
                if (!parse.ok) {
                    if (parse.@"error") |info| {
                        const msg = try format_parse_error(allocator, info, tokens);
                        errdefer allocator.free(msg);
                        try diag_messages.append(msg);
                        errdefer diag_messages.items.len -= 1;
                        try diags.append(.{
                            .danger = .@"error",
                            .message = msg,
                            .span = span_from_token_index(tokens, info.position),
                            .source_id = compsrc.id,
                            .code = "E1001",
                        });
                    } else {
                        try diags.append(.{ .danger = .@"error", .message = "parse error", .span = null, .source_id = compsrc.id, .code = "E1001" });
                    }
                    allocator.free(tokens);
                    parse.deinit();
                    continue;
                }

                var builder = ink.peg_ast.builder.init(parse.arena.allocator(), tokens, &parse.tree, compsrc.text);
                const file_nodes = builder.build_program(parse.root.?) catch |err| {
                    std.debug.print("ast error in {s}: {s}\n", .{ compsrc.path, @errorName(err) });
                    if (builder.last_error) |info| {
                        if (info.position < tokens.len) {
                            const tok = tokens[info.position];
                            std.debug.print("ast error token {s} at {d}: {s}\n", .{ @tagName(tok.which), info.position, tok.what.string });
                        }
                        const msg = try format_ast_error(allocator, info, tokens);
                        errdefer allocator.free(msg);
                        try diag_messages.append(msg);
                        errdefer diag_messages.items.len -= 1;
                        try diags.append(.{
                            .danger = .@"error",
                            .message = msg,
                            .span = span_from_token_index(tokens, info.position),
                            .source_id = compsrc.id,
                            .code = "E1002",
                        });
                        allocator.free(tokens);
                        parse.deinit();
                        continue;
                    }
                    try diags.append(.{ .danger = .@"error", .message = "ast error", .span = null, .source_id = compsrc.id, .code = "E1002" });
                    allocator.free(tokens);
                    parse.deinit();
                    continue;
                };

                try files.append(.{
                    .source_id = sid,
                    .arena = parse.arena,
                    .tokens = tokens,
                    .nodes = file_nodes,
                });

                for (file_nodes) |n| {
                    try nodes.append(n);
                    try node_sources.append(sid);
                }
            }

            module_files[mi] = try files.toOwnedSlice();

            const raw_nodes = try nodes.toOwnedSlice();
            const raw_sources = try node_sources.toOwnedSlice();
            allocator.free(module_raw_nodes[mi]);
            allocator.free(module_raw_sources[mi]);
            module_raw_nodes[mi] = raw_nodes;
            module_raw_sources[mi] = raw_sources;
        }

        if (has_error(diags.items)) {
            return finish(&diags, &diag_messages, null, null, null, null, null, false);
        }

        // 4) macro expansion + final desugar per module
        var macro_ctx = macro_ctx_mod.macro_context.init(allocator, &diags, &diag_messages);
        defer macro_ctx.deinit();
        macro_ctx.next_virtual_source = max_source_id + 1;

        var macro_modules = hash_map(module_id, macro_module).init(allocator);
        defer {
            var it = macro_modules.iterator();
            while (it.next()) |entry| {
                entry.value_ptr.*.deinit(allocator);
            }
            macro_modules.deinit();
        }

        var module_macro_imports = try allocator.alloc(macro_imports, modules.items.len);
        defer {
            for (module_macro_imports) |*imports| imports.deinit();
            allocator.free(module_macro_imports);
        }

        var module_macro_infos = try allocator.alloc([]const macro_info, modules.items.len);
        defer {
            for (module_macro_infos) |infos| allocator.free(infos);
            allocator.free(module_macro_infos);
        }

        var macro_arenas = array_list(arena_allocator).init(allocator);
        defer {
            for (macro_arenas.items) |*arena| arena.deinit();
            macro_arenas.deinit();
        }

        for (module_macro_imports) |*imports| {
            imports.* = macro_imports.init(allocator);
        }
        for (module_macro_infos) |*slot| {
            slot.* = try allocator.alloc(macro_info, 0);
        }

        for (modules.items, 0..) |mod, mi| {
            allocator.free(module_macro_infos[mi]);
            try collect_macro_imports(&module_macro_imports[mi], &module_names, module_raw_nodes[mi], module_raw_sources[mi], &diags);
            module_macro_infos[mi] = try collect_macro_infos(allocator, module_raw_nodes[mi], module_raw_sources[mi], &diags);

            free_foreign_list(allocator, module_foreigns[mi]);
            module_foreigns[mi] = try collect_foreigns_from_nodes(allocator, module_raw_nodes[mi]);
            _ = mod;
        }

        if (has_error(diags.items)) {
            return finish(&diags, &diag_messages, null, null, null, null, null, false);
        }

        for (modules.items, 0..) |mod, mi| {
            const infos = module_macro_infos[mi];
            if (infos.len == 0) continue;

            var compile_nodes = try collect_macro_compile_nodes(allocator, module_raw_nodes[mi], module_raw_sources[mi]);
            defer compile_nodes.deinit(allocator);

            const runtime_opt = try compile_macro_runtime(
                allocator,
                compile_nodes.nodes,
                compile_nodes.sources,
                mod.id,
                module_foreigns,
                &module_macro_imports[mi],
                req.prelude,
                &diags,
                &diag_messages,
            );
            if (runtime_opt) |runtime| {
                var runtime_mut = runtime;
                var macro_map = try build_macro_map(allocator, infos, &runtime_mut, &diags);
                if (has_error(diags.items)) {
                    var it = macro_map.iterator();
                    while (it.next()) |entry| entry.value_ptr.*.deinit();
                    macro_map.deinit();
                    runtime_mut.deinit(allocator);
                    continue;
                }
                try macro_modules.put(mod.id, .{ .runtime = runtime_mut, .macros = macro_map });
            }
        }

        if (has_error(diags.items)) {
            return finish(&diags, &diag_messages, null, null, null, null, null, false);
        }

        ink.vm.foreign.set_macro_context(&macro_ctx);
        defer ink.vm.foreign.set_macro_context(null);

        for (modules.items, 0..) |mod, mi| {
            var expander = macro_expander{
                .allocator = allocator,
                .macro_ctx = &macro_ctx,
                .macro_modules = &macro_modules,
                .imports = &module_macro_imports[mi],
                .module_id = mod.id,
                .diags = &diags,
                .diag_messages = &diag_messages,
                .macro_arenas = &macro_arenas,
            };

            var expanded_nodes = array_list(*ink.node).init(allocator);
            errdefer expanded_nodes.deinit();
            var expanded_sources = array_list(src.source_id).init(allocator);
            errdefer expanded_sources.deinit();

            for (module_files[mi]) |*file| {
                const file_nodes = try expander.expand_nodes(
                    file.nodes,
                    file.tokens,
                    file.source_id,
                    file.arena.allocator(),
                    allocator,
                    0,
                );
                defer allocator.free(file_nodes);
                try expanded_nodes.appendSlice(file_nodes);
                for (file_nodes) |_| {
                    try expanded_sources.append(file.source_id);
                }
            }

            const new_nodes = try expanded_nodes.toOwnedSlice();
            const new_sources = try expanded_sources.toOwnedSlice();
            allocator.free(module_raw_nodes[mi]);
            allocator.free(module_raw_sources[mi]);
            module_raw_nodes[mi] = new_nodes;
            module_raw_sources[mi] = new_sources;
        }

        if (has_error(diags.items)) {
            return finish(&diags, &diag_messages, null, null, null, null, null, false);
        }

        for (modules.items, 0..) |mod, mi| {
            _ = mod;
            var desugared = desugar.desugar(
                module_arenas[mi].allocator(),
                allocator,
                module_raw_nodes[mi],
                &diags,
                req.prelude,
            ) catch {
                try diags.append(.{ .danger = .@"error", .message = "desugar error", .span = null });
                continue;
            };
            defer desugared.origin.deinit();

            if (module_import_specs[mi]) |imports| {
                allocator.free(imports);
            }
            module_import_specs[mi] = desugared.imports;

            const filtered_sources = try sources_without_imports(allocator, module_raw_nodes[mi], module_raw_sources[mi]);
            allocator.free(module_nodes[mi]);
            allocator.free(module_node_sources[mi]);
            allocator.free(module_imports[mi]);
            free_foreign_list(allocator, module_foreigns[mi]);

            module_nodes[mi] = desugared.nodes;
            module_node_sources[mi] = filtered_sources;
            module_imports[mi] = try build_module_imports(allocator, &module_names, desugared.imports, &diags);
            module_foreigns[mi] = try collect_foreigns_from_nodes(allocator, desugared.nodes);
        }

        if (has_error(diags.items)) {
            return finish(&diags, &diag_messages, null, null, null, null, null, false);
        }

        // 5) resolver
        var res = resolver.init(allocator, null, &diag_messages);
        defer res.deinit();

        for (modules.items, 0..) |mod, mi| {
            try res.add_module(mod.id, module_imports[mi], module_nodes[mi], &diags);
        }
        for (modules.items, 0..) |mod, mi| {
            try res.resolve_module(mod.id, module_nodes[mi], module_node_sources[mi], &diags);
        }

        if (has_error(diags.items)) {
            return finish(&diags, &diag_messages, null, null, null, null, null, false);
        }

        // 6) UIR build (root module only)
        const root_id = module_names.get(req.root_module) orelse {
            try diags.append(.{ .danger = .@"error", .message = "unknown root module", .span = null });
            return finish(&diags, &diag_messages, null, null, null, null, null, false);
        };
        const root_nodes = module_nodes[@intCast(root_id)];
        const root_sources = module_node_sources[@intCast(root_id)];

        var uir_arena = arena_allocator.init(allocator);
        defer uir_arena.deinit();
        var uir_builder = ink.uir_build.builder.init(uir_arena.allocator());
        defer uir_builder.deinit();
        uir_builder.build_nodes_with_sources(root_nodes, root_sources) catch {
            var msg: []const u8 = "uir build error";
            if (uir_builder.last_error_node) |name| {
                const owned = try std.fmt.allocPrint(allocator, "uir build error: unsupported node {s}", .{name});
                try diag_messages.append(owned);
                msg = owned;
            }
            try diags.append(.{
                .danger = .@"error",
                .message = msg,
                .span = uir_builder.last_error_span,
                .source_id = uir_builder.current_source_id,
            });
            return finish(&diags, &diag_messages, null, null, null, null, null, false);
        };
        const uir_result = uir_builder.finish() catch {
            try diags.append(.{ .danger = .@"error", .message = "uir build error", .span = null });
            return finish(&diags, &diag_messages, null, null, null, null, null, false);
        };

        // 7) typecheck (root module only)
        var type_result = typecheck.check(
            allocator,
            uir_result.nodes,
            uir_result.strings,
            uir_result.roots,
            uir_result.spans,
            uir_result.sources,
            &diags,
        ) catch {
            try diags.append(.{ .danger = .@"error", .message = "typecheck error", .span = null });
            return finish(&diags, &diag_messages, null, null, null, null, null, false);
        };
        defer type_result.deinit(allocator);

        if (has_error(diags.items)) {
            return finish(&diags, &diag_messages, null, null, null, null, null, false);
        }

        // 8) backend emit
        var foreign_names = array_list([]const u8).init(allocator);
        var foreign_allocated = array_list([]const u8).init(allocator);
        defer {
            for (foreign_allocated.items) |name| allocator.free(name);
            foreign_allocated.deinit();
            foreign_names.deinit();
        }
        var foreign_set = string_map(void).init(allocator);
        defer foreign_set.deinit();

        for (module_foreigns[@intCast(root_id)]) |name| {
            if (!foreign_set.contains(name)) {
                try foreign_set.put(name, {});
                try foreign_names.append(name);
            }
        }

        for (module_imports[@intCast(root_id)]) |imp| {
            const foreigns = module_foreigns[@intCast(imp.id)];
            for (foreigns) |fname| {
                const qualified = try qualify_name(allocator, imp.alias, fname);
                if (!foreign_set.contains(qualified)) {
                    try foreign_set.put(qualified, {});
                    try foreign_names.append(qualified);
                    try foreign_allocated.append(qualified);
                } else {
                    allocator.free(qualified);
                }
            }
        }

        try add_intrinsic_foreigns(&foreign_set, &foreign_names, uir_result.nodes, uir_result.strings);

        var foreigns = try allocator.alloc([]const u8, foreign_names.items.len);
        errdefer {
            for (foreigns) |name| allocator.free(name);
            allocator.free(foreigns);
        }
        for (foreign_names.items, 0..) |name, idx| {
            foreigns[idx] = try allocator.dupe(u8, name);
        }

        if (req.target.kind != .vm) {
            try diags.append(.{
                .danger = .@"error",
                .message = "unsupported target backend",
                .span = null,
            });
            return finish(&diags, &diag_messages, null, null, null, null, foreigns, false);
        }

        var mir_arena = arena_allocator.init(allocator);
        defer mir_arena.deinit();
        const mir_result = mir_lower.lower(mir_arena.allocator(), uir_result) catch {
            try diags.append(.{ .danger = .@"error", .message = "mir lowering error", .span = null });
            return finish(&diags, &diag_messages, null, null, null, null, foreigns, false);
        };

        var lower_info = lir_lower.error_info{};
        defer lower_info.deinit(allocator);

        const lir_result = lir_lower.lower(
            allocator,
            req.target,
            mir_result.nodes,
            mir_result.strings,
            mir_result.roots,
            foreigns,
            type_result.types,
            &lower_info,
        ) catch |err| {
            if (err == error.unsupported_target) {
                try diags.append(.{
                    .danger = .@"error",
                    .message = "unsupported target backend",
                    .span = null,
                });
                return finish(&diags, &diag_messages, null, null, null, null, foreigns, false);
            }
            const default_msg: []const u8 = switch (err) {
                error.out_of_memory => "lir error: out_of_memory",
                error.unsupported_node => "lir error: unsupported_node",
                error.ambiguous_overload => "lir error: ambiguous_overload",
                error.missing_main => "lir error: missing_main",
                error.unknown_identifier => "lir error: unknown_identifier",
                error.unknown_function => "lir error: unknown_function",
                error.unknown_foreign => "lir error: unknown_foreign",
                error.register_overflow => "lir error: register_overflow",
                error.constant_index_overflow => "lir error: constant_index_overflow",
                else => return err,
            };
            var msg = default_msg;
            if (lower_info.message) |owned| {
                msg = owned;
                if (lower_info.owns_message) {
                    try diag_messages.append(owned);
                    lower_info.owns_message = false;
                }
            }
            var diag_span: ?span = null;
            if (lower_info.node) |node_id| {
                const idx: usize = @intCast(node_id.idx);
                if (idx < mir_result.spans.len) {
                    diag_span = mir_result.spans[idx];
                }
            }
            var diag_source_id: ?src.source_id = null;
            if (lower_info.node) |node_id| {
                const idx: usize = @intCast(node_id.idx);
                if (idx < mir_result.sources.len) {
                    diag_source_id = mir_result.sources[idx];
                }
            }
            try diags.append(.{
                .danger = .@"error",
                .message = msg,
                .span = diag_span,
                .source_id = diag_source_id,
            });
            return finish(&diags, &diag_messages, null, null, null, null, foreigns, false);
        };

        const backend_req = backend.backend_request{
            .allocator = allocator,
            .target = req.target,
            .input = switch (lir_result) {
                .vm => |bundle| .{ .vm = bundle },
            },
            .foreigns = foreigns,
        };

        const backend_result = backend.emit(backend_req) catch |err| {
            if (err == error.unsupported_target) {
                try diags.append(.{
                    .danger = .@"error",
                    .message = "unsupported target backend",
                    .span = null,
                });
                return finish(&diags, &diag_messages, null, null, null, null, foreigns, false);
            }
            const default_msg: []const u8 = switch (err) {
                error.OutOfMemory => "backend error: out_of_memory",
                error.label_not_found => "backend error: label_not_found",
                error.const_index_too_large => "backend error: const_index_too_large",
                error.register_too_large => "backend error: register_too_large",
                error.unsupported_instruction => "backend error: unsupported_instruction",
                error.instruction_invalid => "backend error: instruction_invalid",
                else => return err,
            };
            try diags.append(.{
                .danger = .@"error",
                .message = default_msg,
                .span = null,
            });
            return finish(&diags, &diag_messages, null, null, null, null, foreigns, false);
        };

        const result_foreigns = backend_result.foreigns orelse foreigns;
        return finish(
            &diags,
            &diag_messages,
            backend_result.instructions,
            backend_result.constants,
            backend_result.data,
            backend_result.bytecode,
            result_foreigns,
            true,
        );
    }

    fn lex_all(
        allocator: mem_allocator,
        source_text: []const u8,
        diags: *array_list(diagnostic),
        src_id: src.source_id,
    ) ![]const ink.token {
        var lexer = try ink.lexer.init(source_text);
        var tokens = array_list(ink.token).init(allocator);
        errdefer tokens.deinit();

        while (true) {
            const maybe_tok = lexer.next() catch {
                try diags.append(.{ .danger = .@"error", .message = "lexer error", .span = null, .source_id = src_id, .code = "E1000" });
                break;
            };
            if (maybe_tok) |tok| {
                try tokens.append(tok);
                if (tok.which == .end_of_file) break;
            } else break;
        }

        return tokens.toOwnedSlice();
    }

    fn collect_exports(exports: *string_map(void), nodes: []const *ink.node) !void {
        for (nodes) |node| {
            if (node.* != .decl) continue;
            switch (node.decl) {
                .function => |f| try add_export(exports, f.name.string),
                .@"const" => |c| try add_export(exports, c.name.string),
                .@"var" => |v| try add_export(exports, v.name.string),
                .type_alias => |t| try add_export(exports, t.name.string),
                .@"struct" => |s| try add_export(exports, s.name.string),
                .trait => |t| try add_export(exports, t.name.string),
                .@"enum" => |e| try add_export(exports, e.name.string),
                .import => |_| {},
                .impl => |_| {},
            }
        }
    }

    fn add_export(exports: *string_map(void), name: []const u8) !void {
        if (!exports.contains(name)) {
            try exports.put(name, {});
        }
    }

    fn has_attribute(attrs: []const ink.ast.attribute, name: []const u8) bool {
        for (attrs) |attr| {
            if (std.mem.eql(u8, attr.name.string, name)) return true;
        }
        return false;
    }

    fn has_error(diags: []const diagnostic) bool {
        for (diags) |d| if (d.danger == .@"error") return true;
        return false;
    }

    fn qualify_name(allocator: mem_allocator, alias: []const u8, name: []const u8) ![]const u8 {
        const sep = "::";
        var buf = try allocator.alloc(u8, alias.len + sep.len + name.len);
        std.mem.copyForwards(u8, buf[0..alias.len], alias);
        std.mem.copyForwards(u8, buf[alias.len .. alias.len + sep.len], sep);
        std.mem.copyForwards(u8, buf[alias.len + sep.len ..], name);
        return buf;
    }

    fn add_foreign_name(
        foreign_set: *string_map(void),
        foreign_names: *array_list([]const u8),
        name: []const u8,
    ) !void {
        if (!foreign_set.contains(name)) {
            try foreign_set.put(name, {});
            try foreign_names.append(name);
        }
    }

    fn mangle_foreign_name(
        allocator: mem_allocator,
        base: []const u8,
        params: []const ink.ast.param,
    ) ![]const u8 {
        var out = array_list(u8).init(allocator);
        errdefer out.deinit();
        try out.appendSlice(base);
        try out.append('$');
        for (params, 0..) |param, idx| {
            if (idx != 0) try out.append(',');
            const name = type_key_from_node(ink.ast.deref(param.ty));
            try append_sanitized_name(&out, name);
        }
        return out.toOwnedSlice();
    }

    fn append_sanitized_name(out: *array_list(u8), name: []const u8) !void {
        for (name) |ch| {
            if ((ch >= 'a' and ch <= 'z') or
                (ch >= 'A' and ch <= 'Z') or
                (ch >= '0' and ch <= '9') or
                ch == '_')
            {
                try out.append(ch);
            } else {
                try out.append('_');
            }
        }
    }

    fn type_key_from_node(node: *const ink.node) []const u8 {
        switch (node.*) {
            .type => |ty| switch (ty) {
                .self => return "self",
                .name => |id| return id.string,
                else => return "unknown",
            },
            .identifier => |id| return id.string,
            .string => |id| return id.string,
            else => return "unknown",
        }
    }

    fn add_intrinsic_foreigns(
        foreign_set: *string_map(void),
        foreign_names: *array_list([]const u8),
        nodes: []const ink.uir.uir,
        strings: []const []const u8,
    ) !void {
        var need_alloc = false;
        var need_free = false;
        var need_deref = false;
        var need_store = false;
        var need_result_ok = false;
        var need_result_err = false;
        var need_result_is_ok = false;
        var need_result_unwrap = false;
        var need_result_unwrap_err = false;
        var need_try = false;
        var need_ptr_of = false;
        var need_string_new = false;
        var need_string_concat = false;
        var need_string_from_int = false;
        var need_string_from_float = false;
        var need_string_from_bool = false;
        var need_sleep = false;
        var need_sleep_until = false;
        var need_timeout = false;
        var need_deadline = false;
        var need_yield = false;

        for (nodes) |node| {
            if (node == .record_literal) {
                need_alloc = true;
                need_store = true;
            } else if (node == .binary and node.binary.op == .access) {
                need_deref = true;
            } else if (node == .binary and node.binary.op == .index) {
                need_deref = true;
            } else if (node == .unary and node.unary.op == .deref) {
                need_deref = true;
            } else if (node == .intrinsic) {
                const call = node.intrinsic;
                const name_idx: usize = @intCast(call.name.idx);
                if (name_idx >= strings.len) continue;
                const name = strings[name_idx];
                if (std.mem.eql(u8, name, "alloc")) {
                    need_alloc = true;
                } else if (std.mem.eql(u8, name, "free")) {
                    need_free = true;
                } else if (std.mem.eql(u8, name, "deref")) {
                    need_deref = true;
                } else if (std.mem.eql(u8, name, "result_ok")) {
                    need_result_ok = true;
                } else if (std.mem.eql(u8, name, "result_err")) {
                    need_result_err = true;
                } else if (std.mem.eql(u8, name, "result_is_ok")) {
                    need_result_is_ok = true;
                } else if (std.mem.eql(u8, name, "result_unwrap")) {
                    need_result_unwrap = true;
                } else if (std.mem.eql(u8, name, "result_unwrap_err")) {
                    need_result_unwrap_err = true;
                } else if (std.mem.eql(u8, name, "interpolate")) {
                    need_string_new = true;
                    need_string_concat = true;
                    need_string_from_int = true;
                    need_string_from_float = true;
                    need_string_from_bool = true;
                }
            } else if (node == .binary and node.binary.op == .@"as") {
                need_alloc = true;
                need_deref = true;
                need_store = true;
            } else if (node == .unary and (node.unary.op == .borrow or node.unary.op == .borrow_mut)) {
                need_ptr_of = true;
            } else if (node == .unary and node.unary.op == .@"try") {
                need_try = true;
            } else if (node == .unary) {
                switch (node.unary.op) {
                    .sleep => {
                        need_sleep = true;
                        need_sleep_until = true;
                    },
                    .timeout => {
                        need_timeout = true;
                    },
                    .deadline => {
                        need_deadline = true;
                        need_timeout = true;
                    },
                    else => {},
                }
            } else if (node == .yield_expr) {
                need_yield = true;
            }
        }

        if (need_alloc) try add_foreign_name(foreign_set, foreign_names, "std::alloc");
        if (need_free) try add_foreign_name(foreign_set, foreign_names, "std::free");
        if (need_deref) try add_foreign_name(foreign_set, foreign_names, "std::deref");
        if (need_store) try add_foreign_name(foreign_set, foreign_names, "std::store");
        if (need_ptr_of) try add_foreign_name(foreign_set, foreign_names, "std::ptr_of");
        if (need_result_ok) try add_foreign_name(foreign_set, foreign_names, "std::result_ok");
        if (need_result_err) try add_foreign_name(foreign_set, foreign_names, "std::result_err");
        if (need_result_is_ok) try add_foreign_name(foreign_set, foreign_names, "std::result_is_ok");
        if (need_result_unwrap) try add_foreign_name(foreign_set, foreign_names, "std::result_unwrap");
        if (need_result_unwrap_err) try add_foreign_name(foreign_set, foreign_names, "std::result_unwrap_err");
        if (need_string_new) try add_foreign_name(foreign_set, foreign_names, "std::string_new");
        if (need_string_concat) try add_foreign_name(foreign_set, foreign_names, "std::string_concat");
        if (need_string_from_int) try add_foreign_name(foreign_set, foreign_names, "std::string_from_int");
        if (need_string_from_float) try add_foreign_name(foreign_set, foreign_names, "std::string_from_float");
        if (need_string_from_bool) try add_foreign_name(foreign_set, foreign_names, "std::string_from_bool");
        if (need_try) {
            try add_foreign_name(foreign_set, foreign_names, "std::result_is_ok");
            try add_foreign_name(foreign_set, foreign_names, "std::result_unwrap");
        }
        if (need_sleep) try add_foreign_name(foreign_set, foreign_names, "std::sleep");
        if (need_sleep_until) try add_foreign_name(foreign_set, foreign_names, "std::sleep_until");
        if (need_timeout) try add_foreign_name(foreign_set, foreign_names, "std::timeout");
        if (need_deadline) try add_foreign_name(foreign_set, foreign_names, "std::deadline");
        if (need_yield) try add_foreign_name(foreign_set, foreign_names, "std::yield");
        try add_foreign_name(foreign_set, foreign_names, "std::atomic_lock");
        try add_foreign_name(foreign_set, foreign_names, "std::atomic_unlock");
    }

    fn finish(
        diags: *array_list(diagnostic),
        diag_messages: *array_list([]const u8),
        instructions: ?[]const ink.exe.instruction,
        constants: ?[]const u64,
        data: ?[]const ink.vm.inkb.data_entry,
        bytecode: ?[]u8,
        foreigns: ?[]const []const u8,
        ok: bool,
    ) !compile_result {
        const diag_slice = try diags.toOwnedSlice();
        const msg_slice = try diag_messages.toOwnedSlice();
        return .{
            .ok = ok,
            .diagnostics = diag_slice,
            .diag_messages = msg_slice,
            .instructions = instructions,
            .constants = constants,
            .data = data,
            .bytecode = bytecode,
            .foreigns = foreigns,
        };
    }

    const macro_compile_nodes = struct {
        nodes: []const *ink.node,
        sources: []const src.source_id,

        fn deinit(self: *macro_compile_nodes, allocator: mem_allocator) void {
            allocator.free(self.nodes);
            allocator.free(self.sources);
        }
    };

    fn collect_macro_imports(
        imports: *macro_imports,
        module_names: *string_map(module_id),
        nodes: []const *ink.node,
        sources: []const src.source_id,
        diags: *array_list(diagnostic),
    ) !void {
        for (nodes, 0..) |node, idx| {
            if (node.* != .decl) continue;
            if (node.decl != .import) continue;
            const imp = node.decl.import;
            const module_name = imp.module.string;
            const maybe_module_id = module_names.get(module_name);
            if (maybe_module_id == null) {
                try diags.append(.{
                    .danger = .@"error",
                    .message = "unknown module import",
                    .span = span{ .start = imp.module.where.start, .end = imp.module.where.end },
                    .source_id = if (idx < sources.len) sources[idx] else null,
                });
                continue;
            }
            const module_id_actual = maybe_module_id.?;

            if (imp.item) |item| {
                const alias = imp.alias orelse item;
                if (imports.module_map.contains(alias.string) or imports.symbol_map.contains(alias.string)) {
                    try diags.append(.{
                        .danger = .@"error",
                        .message = "duplicate import alias",
                        .span = span{ .start = alias.where.start, .end = alias.where.end },
                        .source_id = if (idx < sources.len) sources[idx] else null,
                    });
                    continue;
                }
                try imports.symbol_map.put(alias.string, .{ .module_id = module_id_actual, .item = item.string });
            } else {
                const alias = imp.alias orelse imp.module;
                if (imports.module_map.contains(alias.string) or imports.symbol_map.contains(alias.string)) {
                    try diags.append(.{
                        .danger = .@"error",
                        .message = "duplicate import alias",
                        .span = span{ .start = alias.where.start, .end = alias.where.end },
                        .source_id = if (idx < sources.len) sources[idx] else null,
                    });
                    continue;
                }
                try imports.module_map.put(alias.string, module_id_actual);
            }
        }
    }

    fn type_is_token_stream(node: *const ink.node) bool {
        if (node.* != .type) return false;
        return switch (node.type) {
            .name => |id| std.mem.eql(u8, id.string, "token_stream"),
            else => false,
        };
    }

    fn collect_macro_infos(
        allocator: mem_allocator,
        nodes: []const *ink.node,
        sources: []const src.source_id,
        diags: *array_list(diagnostic),
    ) ![]const macro_info {
        var out = array_list(macro_info).init(allocator);
        for (nodes, 0..) |node, idx| {
            if (node.* != .decl) continue;
            if (node.decl != .function) continue;
            const func = node.decl.function;
            const has_attr = has_attribute(func.attributes, "attribute");
            if (!func.is_comptime) {
                if (has_attr) {
                    try diags.append(.{
                        .danger = .@"error",
                        .message = "attribute macros must be comptime functions",
                        .span = span{ .start = func.name.where.start, .end = func.name.where.end },
                        .source_id = if (idx < sources.len) sources[idx] else null,
                    });
                }
                continue;
            }
            if (func.generics.len != 0) {
                if (has_attr) {
                    try diags.append(.{
                        .danger = .@"error",
                        .message = "macro functions cannot be generic",
                        .span = span{ .start = func.name.where.start, .end = func.name.where.end },
                        .source_id = if (idx < sources.len) sources[idx] else null,
                    });
                }
                continue;
            }
            if (func.return_type == null) {
                if (has_attr) {
                    try diags.append(.{
                        .danger = .@"error",
                        .message = "macro functions must return token_stream",
                        .span = span{ .start = func.name.where.start, .end = func.name.where.end },
                        .source_id = if (idx < sources.len) sources[idx] else null,
                    });
                }
                continue;
            }
            if (!type_is_token_stream(ink.ast.deref(func.return_type.?))) {
                if (has_attr) {
                    try diags.append(.{
                        .danger = .@"error",
                        .message = "macro functions must return token_stream",
                        .span = span{ .start = func.name.where.start, .end = func.name.where.end },
                        .source_id = if (idx < sources.len) sources[idx] else null,
                    });
                }
                continue;
            }
            var all_params_stream = true;
            for (func.params) |param| {
                if (!type_is_token_stream(ink.ast.deref(param.ty))) {
                    all_params_stream = false;
                    break;
                }
            }
            if (!all_params_stream) {
                if (has_attr) {
                    try diags.append(.{
                        .danger = .@"error",
                        .message = "macro functions must take token_stream parameters",
                        .span = span{ .start = func.name.where.start, .end = func.name.where.end },
                        .source_id = if (idx < sources.len) sources[idx] else null,
                    });
                }
                continue;
            }
            if (func.body == null) {
                try diags.append(.{
                    .danger = .@"error",
                    .message = "macro functions must have a body",
                    .span = span{ .start = func.name.where.start, .end = func.name.where.end },
                    .source_id = if (idx < sources.len) sources[idx] else null,
                });
                continue;
            }
            try out.append(.{
                .name = func.name.string,
                .param_count = func.params.len,
                .is_attribute = has_attr,
                .where = func.name.where,
            });
        }
        return out.toOwnedSlice();
    }

    fn collect_macro_compile_nodes(
        allocator: mem_allocator,
        nodes: []const *ink.node,
        sources: []const src.source_id,
    ) !macro_compile_nodes {
        var out_nodes = array_list(*ink.node).init(allocator);
        var out_sources = array_list(src.source_id).init(allocator);
        for (nodes, 0..) |node, idx| {
            if (node.* != .decl) continue;
            switch (node.decl) {
                .import => {
                    try out_nodes.append(node);
                    try out_sources.append(sources[idx]);
                },
                .function => |func| {
                    if (!func.is_comptime) continue;
                    try out_nodes.append(node);
                    try out_sources.append(sources[idx]);
                },
                .@"struct", .@"enum", .trait, .type_alias, .@"const" => {
                    try out_nodes.append(node);
                    try out_sources.append(sources[idx]);
                },
                else => {},
            }
        }
        return .{
            .nodes = try out_nodes.toOwnedSlice(),
            .sources = try out_sources.toOwnedSlice(),
        };
    }

    fn collect_foreigns_from_nodes(
        allocator: mem_allocator,
        nodes: []const *ink.node,
    ) ![]const []const u8 {
        var counts = string_map(usize).init(allocator);
        defer counts.deinit();

        for (nodes) |node| {
            if (node.* != .decl) continue;
            if (node.decl != .function) continue;
            const func = node.decl.function;
            if (!has_attribute(func.attributes, "foreign")) continue;
            const name = func.name.string;
            if (counts.getPtr(name)) |count| {
                count.* += 1;
            } else {
                try counts.put(name, 1);
            }
        }

        var out = array_list([]const u8).init(allocator);
        errdefer {
            for (out.items) |name| allocator.free(name);
            out.deinit();
        }

        for (nodes) |node| {
            if (node.* != .decl) continue;
            if (node.decl != .function) continue;
            const func = node.decl.function;
            if (!has_attribute(func.attributes, "foreign")) continue;
            const name = func.name.string;
            const count = counts.get(name) orelse 0;
            if (count > 1) {
                const mangled = try mangle_foreign_name(allocator, name, func.params);
                try out.append(mangled);
            } else {
                try out.append(try allocator.dupe(u8, name));
            }
        }

        return out.toOwnedSlice();
    }

    fn free_foreign_list(allocator: mem_allocator, list: []const []const u8) void {
        for (list) |name| allocator.free(name);
        allocator.free(list);
    }

    fn sources_without_imports(
        allocator: mem_allocator,
        nodes: []const *ink.node,
        sources: []const src.source_id,
    ) ![]const src.source_id {
        var out = array_list(src.source_id).init(allocator);
        for (nodes, 0..) |node, idx| {
            if (node.* == .decl and node.decl == .import) continue;
            if (idx < sources.len) {
                try out.append(sources[idx]);
            }
        }
        return out.toOwnedSlice();
    }

    fn build_module_imports(
        allocator: mem_allocator,
        module_names: *string_map(module_id),
        imports: []const desugar.import_decl,
        diags: *array_list(diagnostic),
    ) ![]const resolver.module_import {
        var out = array_list(resolver.module_import).init(allocator);
        var seen_modules = string_map(void).init(allocator);
        defer seen_modules.deinit();

        for (imports) |imp| {
            if (imp.item != null) continue;
            if (seen_modules.contains(imp.module.string)) continue;
            const alias = imp.alias orelse imp.module;
            const maybe_module_id = module_names.get(imp.module.string);
            if (maybe_module_id == null) {
                try diags.append(.{
                    .danger = .@"error",
                    .message = "unknown module dependency",
                    .span = span{ .start = imp.module.where.start, .end = imp.module.where.end },
                });
                continue;
            }
            try out.append(.{ .id = maybe_module_id.?, .alias = alias.string });
            try seen_modules.put(imp.module.string, {});
        }

        for (imports) |imp| {
            if (imp.item == null) continue;
            if (seen_modules.contains(imp.module.string)) continue;
            const maybe_module_id = module_names.get(imp.module.string);
            if (maybe_module_id == null) {
                try diags.append(.{
                    .danger = .@"error",
                    .message = "unknown module dependency",
                    .span = span{ .start = imp.module.where.start, .end = imp.module.where.end },
                });
                continue;
            }
            try out.append(.{ .id = maybe_module_id.?, .alias = imp.module.string });
            try seen_modules.put(imp.module.string, {});
        }
        return out.toOwnedSlice();
    }

    fn deinit_mir_result(allocator: mem_allocator, result: *mir_lower.build_result) void {
        allocator.free(result.nodes);
        allocator.free(result.strings);
        allocator.free(result.roots);
        allocator.free(result.spans);
        allocator.free(result.sources);
    }

    fn compile_macro_runtime(
        allocator: mem_allocator,
        nodes: []const *ink.node,
        sources: []const src.source_id,
        module_id_value: module_id,
        module_foreigns: []const []const []const u8,
        imports: *const macro_imports,
        prelude: desugar.prelude_spec,
        diags: *array_list(diagnostic),
        diag_messages: *array_list([]const u8),
    ) !?macro_runtime {
        var foreign_names = array_list([]const u8).init(allocator);
        var foreign_allocated = array_list([]const u8).init(allocator);
        var foreign_set = string_map(void).init(allocator);
        defer {
            for (foreign_allocated.items) |name| allocator.free(name);
            foreign_allocated.deinit();
            foreign_names.deinit();
            foreign_set.deinit();
        }

        const module_index: usize = @intCast(module_id_value);
        if (module_index < module_foreigns.len) {
            for (module_foreigns[module_index]) |name| {
                if (!foreign_set.contains(name)) {
                    try foreign_set.put(name, {});
                    try foreign_names.append(name);
                }
            }
        }

        var it = imports.module_map.iterator();
        while (it.next()) |entry| {
            const alias = entry.key_ptr.*;
            const dep_id = entry.value_ptr.*;
            const dep_index: usize = @intCast(dep_id);
            if (dep_index >= module_foreigns.len) continue;
            const foreigns = module_foreigns[dep_index];
            for (foreigns) |fname| {
                const qualified = try qualify_name(allocator, alias, fname);
                if (!foreign_set.contains(qualified)) {
                    try foreign_set.put(qualified, {});
                    try foreign_names.append(qualified);
                    try foreign_allocated.append(qualified);
                } else {
                    allocator.free(qualified);
                }
            }
        }

        var node_arena = arena_allocator.init(allocator);
        defer node_arena.deinit();

        var desugared = desugar.desugar(
            node_arena.allocator(),
            allocator,
            nodes,
            diags,
            prelude,
        ) catch {
            return error.OutOfMemory;
        };
        defer desugared.origin.deinit();
        defer allocator.free(desugared.nodes);
        if (desugared.imports.len > 0) {
            allocator.free(desugared.imports);
        }
        if (has_error(diags.items)) return null;

        const filtered_sources = try sources_without_imports(allocator, nodes, sources);
        defer allocator.free(filtered_sources);

        var uir_arena = arena_allocator.init(allocator);
        defer uir_arena.deinit();
        var uir_builder = ink.uir_build.builder.init(uir_arena.allocator());
        defer uir_builder.deinit();
        uir_builder.build_nodes_with_sources(desugared.nodes, filtered_sources) catch {
            var msg: []const u8 = "uir build error";
            if (uir_builder.last_error_node) |name| {
                const owned = try std.fmt.allocPrint(allocator, "uir build error: unsupported node {s}", .{name});
                try diag_messages.append(owned);
                msg = owned;
            }
            try diags.append(.{
                .danger = .@"error",
                .message = msg,
                .span = uir_builder.last_error_span,
                .source_id = uir_builder.current_source_id,
            });
            return null;
        };
        const uir_result = uir_builder.finish() catch {
            try diags.append(.{ .danger = .@"error", .message = "uir build error", .span = null });
            return null;
        };

        var type_result = typecheck.check(
            allocator,
            uir_result.nodes,
            uir_result.strings,
            uir_result.roots,
            uir_result.spans,
            uir_result.sources,
            diags,
        ) catch {
            try diags.append(.{ .danger = .@"error", .message = "typecheck error", .span = null });
            return null;
        };
        defer type_result.deinit(allocator);

        if (has_error(diags.items)) return null;

        try add_intrinsic_foreigns(&foreign_set, &foreign_names, uir_result.nodes, uir_result.strings);

        var foreigns = try allocator.alloc([]const u8, foreign_names.items.len);
        errdefer {
            for (foreigns) |name| allocator.free(name);
            allocator.free(foreigns);
        }
        for (foreign_names.items, 0..) |name, idx| {
            foreigns[idx] = try allocator.dupe(u8, name);
        }

        var mir_arena = arena_allocator.init(allocator);
        defer mir_arena.deinit();
        const mir_result = mir_lower.lower(mir_arena.allocator(), uir_result) catch {
            try diags.append(.{ .danger = .@"error", .message = "mir lowering error", .span = null });
            for (foreigns) |name| allocator.free(name);
            allocator.free(foreigns);
            return null;
        };

        var lower_info = lir_lower.error_info{};
        defer lower_info.deinit(allocator);

        var signatures = array_list(ink.lir_vm_lower.function_signature).init(allocator);
        errdefer signatures.deinit();

        const program = ink.lir_vm_lower.lower_with_options(
            allocator,
            mir_result.nodes,
            mir_result.strings,
            mir_result.roots,
            foreigns,
            type_result.types,
            &lower_info,
            .{ .require_main = false, .signatures = &signatures },
        ) catch |err| {
            const default_msg: []const u8 = switch (err) {
                error.out_of_memory => "lir error: out_of_memory",
                error.unsupported_node => "lir error: unsupported_node",
                error.ambiguous_overload => "lir error: ambiguous_overload",
                error.unknown_identifier => "lir error: unknown_identifier",
                error.unknown_function => "lir error: unknown_function",
                error.unknown_foreign => "lir error: unknown_foreign",
                error.register_overflow => "lir error: register_overflow",
                error.constant_index_overflow => "lir error: constant_index_overflow",
                else => return err,
            };
            var msg = default_msg;
            if (lower_info.message) |owned| {
                msg = owned;
                if (lower_info.owns_message) {
                    try diag_messages.append(owned);
                    lower_info.owns_message = false;
                }
            }
            var diag_span: ?span = null;
            if (lower_info.node) |node_id| {
                const idx: usize = @intCast(node_id.idx);
                if (idx < mir_result.spans.len) {
                    diag_span = mir_result.spans[idx];
                }
            }
            var diag_source_id: ?src.source_id = null;
            if (lower_info.node) |node_id| {
                const idx: usize = @intCast(node_id.idx);
                if (idx < mir_result.sources.len) {
                    diag_source_id = mir_result.sources[idx];
                }
            }
            try diags.append(.{
                .danger = .@"error",
                .message = msg,
                .span = diag_span,
                .source_id = diag_source_id,
            });
            for (foreigns) |name| allocator.free(name);
            allocator.free(foreigns);
            return null;
        };

        var label_offsets = ink.vm.encode.compute_label_offsets(allocator, program.instructions) catch {
            try diags.append(.{ .danger = .@"error", .message = "macro compile error", .span = null });
            for (foreigns) |name| allocator.free(name);
            allocator.free(foreigns);
            return null;
        };

        const bundle = ink.lir_vm.bundle{ .program = program, .strings = mir_result.strings };
        const backend_result = vm_backend.emit(allocator, bundle, foreigns) catch |err| {
            const default_msg: []const u8 = switch (err) {
                error.OutOfMemory => "backend error: out_of_memory",
                error.label_not_found => "backend error: label_not_found",
                error.const_index_too_large => "backend error: const_index_too_large",
                error.register_too_large => "backend error: register_too_large",
                error.unsupported_instruction => "backend error: unsupported_instruction",
                error.instruction_invalid => "backend error: instruction_invalid",
                else => return err,
            };
            try diags.append(.{
                .danger = .@"error",
                .message = default_msg,
                .span = null,
            });
            for (foreigns) |name| allocator.free(name);
            allocator.free(foreigns);
            label_offsets.deinit();
            return null;
        };

        if (backend_result.instructions) |insts| allocator.free(insts);

        const sig_slice = try signatures.toOwnedSlice();
        return .{
            .bytecode = backend_result.bytecode.?,
            .constants = backend_result.constants.?,
            .data = backend_result.data.?,
            .foreigns = foreigns,
            .label_offsets = label_offsets,
            .signatures = sig_slice,
        };
    }

    fn build_macro_map(
        allocator: mem_allocator,
        macro_infos: []const macro_info,
        runtime: *macro_runtime,
        diags: *array_list(diagnostic),
    ) !string_map(std.array_list.Managed(macro_overload)) {
        var macros = string_map(std.array_list.Managed(macro_overload)).init(allocator);
        for (macro_infos) |info| {
            var matched: ?ink.lir_vm_lower.function_signature = null;
            for (runtime.signatures) |sig| {
                if (sig.is_method) continue;
                if (!std.mem.eql(u8, sig.name, info.name)) continue;
                if (sig.param_count != info.param_count) continue;
                matched = sig;
                break;
            }
            if (matched == null) {
                try diags.append(.{
                    .danger = .@"error",
                    .message = "macro function not compiled",
                    .span = span{ .start = info.where.start, .end = info.where.end },
                });
                continue;
            }
            const sig = matched.?;
            const offset = runtime.label_offsets.get(sig.label) orelse {
                try diags.append(.{
                    .danger = .@"error",
                    .message = "macro label not found",
                    .span = span{ .start = info.where.start, .end = info.where.end },
                });
                continue;
            };
            const overload = macro_overload{
                .param_count = info.param_count,
                .is_attribute = info.is_attribute,
                .label_offset = offset,
            };
            if (macros.getPtr(info.name)) |group| {
                for (group.items) |existing| {
                    if (existing.param_count == overload.param_count and existing.is_attribute == overload.is_attribute) {
                        try diags.append(.{
                            .danger = .@"error",
                            .message = "duplicate macro overload",
                            .span = span{ .start = info.where.start, .end = info.where.end },
                        });
                        break;
                    }
                }
                try group.append(overload);
            } else {
                var group = std.array_list.Managed(macro_overload).init(allocator);
                try group.append(overload);
                try macros.put(info.name, group);
            }
        }
        return macros;
    }

    fn span_from_token_index(tokens: []const ink.token, pos: usize) ?span {
        if (tokens.len == 0) return null;
        if (pos >= tokens.len) {
            const end = tokens[tokens.len - 1].where.end;
            return span{ .start = end, .end = end };
        }
        const tok = tokens[pos];
        return span{ .start = tok.where.start, .end = tok.where.end };
    }

    fn token_lexeme(kind: ink.token.kind) ?[]const u8 {
        inline for (lang_spec.keyword_lexemes) |lex| {
            if (kind == @field(ink.token.kind, lex.kind)) return lex.text;
        }
        inline for (lang_spec.symbol_lexemes) |lex| {
            if (kind == @field(ink.token.kind, lex.kind)) return lex.text;
        }
        return null;
    }

    fn write_token_label(writer: anytype, kind: ink.token.kind) !void {
        switch (kind) {
            .end_of_file => return writer.writeAll("end of file"),
            .new_line => return writer.writeAll("newline"),
            .indent => return writer.writeAll("indent"),
            .dedent => return writer.writeAll("dedent"),
            else => {},
        }
        if (token_lexeme(kind)) |lex| {
            try writer.print("'{s}'", .{lex});
            return;
        }
        try writer.writeAll(@tagName(kind));
    }

    fn format_expected_list(writer: anytype, expected: []const ink.token.kind) !void {
        const max_expected: usize = 6;
        var shown: usize = 0;
        for (expected) |kind| {
            if (shown >= max_expected) break;
            if (shown != 0) try writer.writeAll(", ");
            try write_token_label(writer, kind);
            shown += 1;
        }
        if (expected.len > max_expected) {
            try writer.writeAll(", ...");
        }
    }

    fn format_parse_error(
        allocator: mem_allocator,
        info: ink.peg_parser.parse_error_info,
        tokens: []const ink.token,
    ) ![]const u8 {
        var buf = std.ArrayList(u8).empty;
        errdefer buf.deinit(allocator);
        const writer = buf.writer(allocator);

        if (looks_like_match_arm_error(tokens, info)) {
            try writer.writeAll("parse error: expected '=>' in match arm; use 'pattern => expr' (the '=>' can be on the next line after a multiline pattern)");
            return buf.toOwnedSlice(allocator);
        }

        try writer.writeAll("parse error: expected ");
        if (info.expected.len == 0) {
            try writer.writeAll("token");
        } else {
            try format_expected_list(writer, info.expected);
        }
        try writer.writeAll(", found ");
        if (info.found) |found| {
            try write_token_label(writer, found);
        } else {
            try writer.writeAll("end of file");
        }
        return buf.toOwnedSlice(allocator);
    }


    fn looks_like_match_arm_error(tokens: []const ink.token, info: ink.peg_parser.parse_error_info) bool {
        if (!expected_contains(info.expected, .arrow)) return false;
        var steps: usize = 0;
        var i: isize = @as(isize, @intCast(info.position)) - 1;
        while (i >= 0 and steps < 12) : (steps += 1) {
            const tok = tokens[@intCast(i)].which;
            switch (tok) {
                .expr_match => return true,
                .new_line, .indent, .dedent => {},
                else => {},
            }
            i -= 1;
        }
        return false;
    }

    fn expected_contains(expected: []const ink.token.kind, kind: ink.token.kind) bool {
        for (expected) |item| {
            if (item == kind) return true;
        }
        return false;
    }

    fn format_ast_error(
        allocator: mem_allocator,
        info: ink.peg_ast.error_info,
        tokens: []const ink.token,
    ) ![]const u8 {
        var buf = std.ArrayList(u8).empty;
        errdefer buf.deinit(allocator);
        const writer = buf.writer(allocator);

        switch (info.kind) {
            .unexpected_node => try writer.writeAll("syntax error"),
            .unexpected_token => {
                try writer.writeAll("unexpected token");
                if (info.position < tokens.len) {
                    try writer.writeAll(": ");
                    try write_token_label(writer, tokens[info.position].which);
                }
            },
            .unsupported_construct => try writer.writeAll("unsupported construct"),
            .unsupported_operator => try writer.writeAll("unsupported operator"),
            .unsupported_generic_target => try writer.writeAll("generic arguments must follow a type name"),
            .empty_block => try writer.writeAll("empty block"),
            .multiple_statements => try writer.writeAll("expected a single statement"),
            .string_literal => try writer.writeAll("invalid string interpolation"),
            .invalid_duration_literal => try writer.writeAll("invalid duration literal"),
        }

        return buf.toOwnedSlice(allocator);
    }
};
