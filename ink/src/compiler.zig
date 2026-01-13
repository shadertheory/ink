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
const validate_mod = @import("validate.zig");
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
const sandbox_mod = ink.sandbox;

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
        sandbox: ?sandbox_mod.Config = null,
        debug_info: bool = false,
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
        debug: ?ink.vm.inkb.debug_info = null,

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
            if (self.debug) |*dbg| {
                for (dbg.functions) |func| allocator.free(func.name);
                allocator.free(dbg.functions);
                self.debug = null;
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
        registry: []const *ink.node,

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
            for (self.signatures) |sig| if (sig.impl_for) |impl_name| allocator.free(impl_name);
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
        const arena_buf_node = struct {
            data: usize,
            node: std.SinglyLinkedList.Node = .{},
        };

        allocator: mem_allocator,
        macro_ctx: *macro_ctx_mod.macro_context,
        macro_modules: *hash_map(module_id, macro_module),
        imports: *const macro_imports,
        module_id: module_id,
        diags: *array_list(diagnostic),
        diag_messages: *array_list([]const u8),
        macro_arenas: *array_list(*arena_allocator),
        checks: ?sandbox_mod.Checks = null,
        node_set: ?*std.AutoHashMap(usize, void) = null,
        files: []const ast_file,

        fn slice_error(
            self: *macro_expander,
            base: []const u8,
            context: []const u8,
            source_id_value: src.source_id,
            err_span: ?span,
        ) macro_error!void {
            const msg = try std.fmt.allocPrint(self.allocator, "{s} ({s})", .{ base, context });
            errdefer self.allocator.free(msg);
            try self.diag_messages.append(msg);
            try self.diags.append(.{
                .danger = .@"error",
                .message = msg,
                .span = err_span,
                .source_id = source_id_value,
            });
        }

        fn arena_contains(arena: *const arena_allocator, start_addr: usize, end_addr: usize) bool {
            var it = arena.state.buffer_list.first;
            while (it) |node| : (it = node.next) {
                const buf_node: *arena_buf_node = @fieldParentPtr("node", node);
                const base = @intFromPtr(buf_node);
                const buf_start = base + @sizeOf(arena_buf_node);
                const buf_end = base + buf_node.data;
                _ = end_addr;
                if (start_addr >= buf_start and start_addr < buf_end) return true;
            }
            return false;
        }

        fn slice_in_known_arenas(
            self: *macro_expander,
            ptr_addr: usize,
            len: usize,
            elem_size: usize,
        ) bool {
            if (len == 0) return true;
            const total_size = std.math.mul(usize, len, elem_size) catch return false;
            const end_addr = std.math.add(usize, ptr_addr, total_size) catch return false;
            for (self.files) |file| {
                if (arena_contains(&file.arena, ptr_addr, end_addr)) return true;
            }
            for (self.macro_arenas.items) |arena| {
                if (arena_contains(arena, ptr_addr, end_addr)) return true;
            }
            return false;
        }

        fn contains_node_ref(comptime T: type) bool {
            if (T == ink.ast.node_ref) return true;
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

        fn validate_node_ref_in_arenas(
            self: *macro_expander,
            ref: ink.ast.node_ref,
            source_id_value: src.source_id,
            span_value: ?span,
        ) bool {
            const addr = @intFromPtr(ref);
            if (addr == 0) {
                self.diags.append(.{
                    .danger = .@"error",
                    .message = "macro expansion encountered null node reference",
                    .span = span_value,
                    .source_id = source_id_value,
                }) catch {};
                return false;
            }
            if (@mod(addr, @alignOf(ink.node)) != 0) {
                self.diags.append(.{
                    .danger = .@"error",
                    .message = "macro expansion encountered misaligned node reference",
                    .span = span_value,
                    .source_id = source_id_value,
                }) catch {};
                return false;
            }
            if (!self.slice_in_known_arenas(addr, 1, @sizeOf(ink.node))) {
                self.diags.append(.{
                    .danger = .@"error",
                    .message = "macro expansion encountered node reference outside arena",
                    .span = span_value,
                    .source_id = source_id_value,
                }) catch {};
                return false;
            }
            if (self.node_set) |set| {
                if (!set.contains(addr)) {
                    _ = set.put(addr, {}) catch {};
                }
            }
            return true;
        }

        fn validate_node_refs_in_arenas(
            self: *macro_expander,
            value: anytype,
            source_id_value: src.source_id,
            span_value: ?span,
        ) bool {
            const T = @TypeOf(value);
            if (!contains_node_ref(T)) return true;
            if (T == ink.ast.node_ref) {
                return self.validate_node_ref_in_arenas(value, source_id_value, span_value);
            }
            switch (@typeInfo(T)) {
                .optional => |_| {
                    if (value) |payload| {
                        return self.validate_node_refs_in_arenas(payload, source_id_value, span_value);
                    }
                    return true;
                },
                .pointer => |ptr| {
                    if (ptr.size == .slice) {
                        if (!contains_node_ref(ptr.child)) return true;
                        if (value.len > 0) {
                            const ptr_addr = @intFromPtr(value.ptr);
                            if (ptr_addr == 0) {
                                self.diags.append(.{
                                    .danger = .@"error",
                                    .message = "macro expansion encountered null slice pointer",
                                    .span = span_value,
                                    .source_id = source_id_value,
                                }) catch {};
                                return false;
                            }
                            if (!self.slice_in_known_arenas(ptr_addr, value.len, @sizeOf(ptr.child))) {
                                self.diags.append(.{
                                    .danger = .@"error",
                                    .message = "macro expansion encountered slice outside arena",
                                    .span = span_value,
                                    .source_id = source_id_value,
                                }) catch {};
                                return false;
                            }
                        }
                        for (value) |item| {
                            if (!self.validate_node_refs_in_arenas(item, source_id_value, span_value)) return false;
                        }
                        return true;
                    }
                    return true;
                },
                .@"struct" => |info| {
                    inline for (info.fields) |field| {
                        const field_value = @field(value, field.name);
                        if (!self.validate_node_refs_in_arenas(field_value, source_id_value, span_value)) return false;
                    }
                    return true;
                },
                .@"union" => |info| {
                    if (info.tag_type == null) return true;
                    switch (value) {
                        inline else => |payload| {
                            return self.validate_node_refs_in_arenas(payload, source_id_value, span_value);
                        },
                    }
                },
                .array => |info| {
                    if (contains_node_ref(info.child)) {
                        for (value) |item| {
                            if (!self.validate_node_refs_in_arenas(item, source_id_value, span_value)) return false;
                        }
                    }
                    return true;
                },
                else => return true,
            }
        }

        fn ensure_node_ptr(
            self: *macro_expander,
            node: *ink.node,
            source_id_value: src.source_id,
        ) macro_error!bool {
            const addr = @intFromPtr(node);
            if (addr == 0) {
                try self.diags.append(.{
                    .danger = .@"error",
                    .message = "macro expansion encountered null ast node",
                    .span = null,
                    .source_id = source_id_value,
                });
                return false;
            }
            if (@mod(addr, @alignOf(ink.node)) != 0) {
                try self.diags.append(.{
                    .danger = .@"error",
                    .message = "macro expansion encountered misaligned ast node",
                    .span = null,
                    .source_id = source_id_value,
                });
                return false;
            }
            if (!self.slice_in_known_arenas(addr, 1, @sizeOf(ink.node))) {
                try self.diags.append(.{
                    .danger = .@"error",
                    .message = "macro expansion encountered node outside arena",
                    .span = null,
                    .source_id = source_id_value,
                });
                return false;
            }
            if (self.node_set) |set| {
                if (!set.contains(addr)) {
                    _ = set.put(addr, {}) catch {};
                }
            }
            return true;
        }

        fn deref_node(
            self: *macro_expander,
            ref: ink.ast.node_ref,
            source_id_value: src.source_id,
        ) ?*ink.node {
            const addr = @intFromPtr(ref);
            if (addr == 0) {
                self.diags.append(.{
                    .danger = .@"error",
                    .message = "macro expansion encountered null node reference",
                    .span = null,
                    .source_id = source_id_value,
                }) catch {};
                return null;
            }
            if (@mod(addr, @alignOf(ink.node)) != 0) {
                self.diags.append(.{
                    .danger = .@"error",
                    .message = "macro expansion encountered misaligned node reference",
                    .span = null,
                    .source_id = source_id_value,
                }) catch {};
                return null;
            }
            if (!self.slice_in_known_arenas(addr, 1, @sizeOf(ink.node))) {
                self.diags.append(.{
                    .danger = .@"error",
                    .message = "macro expansion encountered node reference outside arena",
                    .span = null,
                    .source_id = source_id_value,
                }) catch {};
                return null;
            }
            if (self.node_set) |set| {
                if (!set.contains(addr)) {
                    _ = set.put(addr, {}) catch {};
                }
            }
            return ink.ast.deref(ref);
        }

        fn ensure_slice(
            self: *macro_expander,
            ptr_addr: usize,
            len: usize,
            source_id_value: src.source_id,
            elem_size: usize,
            context: []const u8,
            err_span: ?span,
        ) macro_error!bool {
            if (len == 0) return true;
            if (ptr_addr == 0) {
                try self.slice_error("macro expansion encountered null slice pointer", context, source_id_value, err_span);
                return false;
            }
            if (!self.slice_in_known_arenas(ptr_addr, len, elem_size)) {
                try self.slice_error("macro expansion encountered slice outside arena", context, source_id_value, err_span);
                return false;
            }
            return true;
        }

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
            if (!try self.ensure_node_ptr(node, source_id_value)) return;
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
                const decl_span = span{ .start = decl_where(decl).start, .end = decl_where(decl).end };
                if (!try self.ensure_slice(@intFromPtr(attrs.ptr), attrs.len, source_id_value, @sizeOf(ink.ast.attribute), "decl.attributes", decl_span)) return;
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

                if (self.checks) |checks| {
                    if (checks.macro_streams) {
                        if (self.macro_ctx.validate_stream(self.allocator, out_stream.?)) |err| {
                            const msg = switch (err) {
                                .invalid_stream => "macro output stream invalid",
                                .invalid_tree => "macro output tree invalid",
                                .invalid_token => "macro output token invalid",
                                .invalid_group => "macro output group invalid",
                                .cycle => "macro output stream cycle detected",
                            };
                            try self.diags.append(.{
                                .danger = .@"error",
                                .message = msg,
                                .span = span{ .start = attr.where.start, .end = attr.where.end },
                                .source_id = source_id_value,
                            });
                            return null;
                        }
                    }
                }

                const parsed = try self.parse_macro_program(out_stream.?, attr.where, source_id_value);
                if (parsed == null) return null;
                defer self.allocator.free(parsed.?.tokens);
                if (self.checks) |checks| {
                    if (checks.macro_ast) {
                        const ok = try validate_mod.validate_macro_ast(
                            self.allocator,
                            parsed.?.nodes,
                            parsed.?.registry,
                            self.diags,
                            source_id_value,
                            span{ .start = attr.where.start, .end = attr.where.end },
                        );
                        if (!ok) return null;
                    }
                }
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
                        const body = self.deref_node(body_ref, source_id_value) orelse return;
                        const expanded = try self.expand_expr(body, tokens, source_id_value, node_allocator, depth);
                        func.body = ink.ast.ref_opt(expanded);
                    }
                },
                .@"const" => |*c| {
                    const value_node = self.deref_node(c.value, source_id_value) orelse return;
                    const expanded = try self.expand_expr(value_node, tokens, source_id_value, node_allocator, depth);
                    c.value = ink.ast.ref(expanded);
                },
                .@"var" => |*v| {
                    const value_node = self.deref_node(v.value, source_id_value) orelse return;
                    const expanded = try self.expand_expr(value_node, tokens, source_id_value, node_allocator, depth);
                    v.value = ink.ast.ref(expanded);
                },
                .impl => |*impl_decl| {
                    const funcs = @constCast(impl_decl.functions);
                    if (funcs.len > 0) {
                        const decl_span = span{ .start = decl_where(node.decl).start, .end = decl_where(node.decl).end };
                        if (!try self.ensure_slice(@intFromPtr(funcs.ptr), funcs.len, source_id_value, @sizeOf(ink.ast.function_decl), "impl_decl.functions", decl_span)) return;
                    }
                    for (funcs) |*func| {
                        if (func.body) |body_ref| {
                            const body = self.deref_node(body_ref, source_id_value) orelse return;
                            const expanded = try self.expand_expr(body, tokens, source_id_value, node_allocator, depth);
                            func.body = ink.ast.ref_opt(expanded);
                        }
                    }
                },
                .trait => |*trait_decl| {
                    const items = @constCast(trait_decl.items);
                    if (items.len > 0) {
                        const decl_span = span{ .start = decl_where(node.decl).start, .end = decl_where(node.decl).end };
                        if (!try self.ensure_slice(@intFromPtr(items.ptr), items.len, source_id_value, @sizeOf(ink.ast.trait_item), "trait_decl.items", decl_span)) return;
                    }
                    for (items) |*item| {
                        switch (item.*) {
                            .function => |*func| {
                                if (func.body) |body_ref| {
                                    const body = self.deref_node(body_ref, source_id_value) orelse return;
                                    const expanded = try self.expand_expr(body, tokens, source_id_value, node_allocator, depth);
                                    func.body = ink.ast.ref_opt(expanded);
                                }
                            },
                            .assoc_type => |*assoc| {
                                if (assoc.value) |value_ref| {
                                    const value_node = self.deref_node(value_ref, source_id_value) orelse return;
                                    const expanded = try self.expand_expr(value_node, tokens, source_id_value, node_allocator, depth);
                                    assoc.value = ink.ast.ref_opt(expanded);
                                }
                            },
                        }
                    }
                    const requires = @constCast(trait_decl.requires);
                    if (requires.len > 0) {
                        const decl_span = span{ .start = decl_where(node.decl).start, .end = decl_where(node.decl).end };
                        if (!try self.ensure_slice(@intFromPtr(requires.ptr), requires.len, source_id_value, @sizeOf(ink.ast.node_ref), "trait_decl.requires", decl_span)) return;
                    }
                    for (requires) |*req_ref| {
                        const req_node = self.deref_node(req_ref.*, source_id_value) orelse return;
                        const expanded = try self.expand_expr(req_node, tokens, source_id_value, node_allocator, depth);
                        req_ref.* = ink.ast.ref(expanded);
                    }
                },
                .type_alias => |*ty| {
                    const value_node = self.deref_node(ty.value, source_id_value) orelse return;
                    const expanded = try self.expand_expr(value_node, tokens, source_id_value, node_allocator, depth);
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
            if (!try self.ensure_node_ptr(node, source_id_value)) return node;
            if (!self.validate_node_refs_in_arenas(node.*, source_id_value, null)) return node;
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
                    const right = self.deref_node(un.right, source_id_value) orelse return node;
                    const expanded = try self.expand_expr(right, tokens, source_id_value, node_allocator, depth);
                    un.right = ink.ast.ref(expanded);
                },
                .binary => |*bin| {
                    const left_node = self.deref_node(bin.left, source_id_value) orelse return node;
                    const right_node = self.deref_node(bin.right, source_id_value) orelse return node;
                    const left = try self.expand_expr(left_node, tokens, source_id_value, node_allocator, depth);
                    const right = try self.expand_expr(right_node, tokens, source_id_value, node_allocator, depth);
                    bin.left = ink.ast.ref(left);
                    bin.right = ink.ast.ref(right);
                },
                .if_expr => |*ife| {
                    const cond_node = self.deref_node(ife.condition, source_id_value) orelse return node;
                    const then_node = self.deref_node(ife.then_branch, source_id_value) orelse return node;
                    const cond = try self.expand_expr(cond_node, tokens, source_id_value, node_allocator, depth);
                    const then_branch = try self.expand_expr(then_node, tokens, source_id_value, node_allocator, depth);
                    ife.condition = ink.ast.ref(cond);
                    ife.then_branch = ink.ast.ref(then_branch);
                    if (ife.else_branch) |else_ref| {
                        const else_ptr = self.deref_node(else_ref, source_id_value) orelse return node;
                        const else_node = try self.expand_expr(else_ptr, tokens, source_id_value, node_allocator, depth);
                        ife.else_branch = ink.ast.ref_opt(else_node);
                    }
                },
                .match_expr => |*me| {
                    const target_node = self.deref_node(me.target, source_id_value) orelse return node;
                    const target = try self.expand_expr(target_node, tokens, source_id_value, node_allocator, depth);
                    me.target = ink.ast.ref(target);
                    const arms = @constCast(me.arms);
                    if (arms.len > 0) {
                        if (!try self.ensure_slice(@intFromPtr(arms.ptr), arms.len, source_id_value, @sizeOf(ink.ast.match_arm), "match_expr.arms", null)) return node;
                    }
                    for (arms) |*arm| {
                        const pattern_node = self.deref_node(arm.pattern, source_id_value) orelse return node;
                        const body_node = self.deref_node(arm.body, source_id_value) orelse return node;
                        const pattern = try self.expand_expr(pattern_node, tokens, source_id_value, node_allocator, depth);
                        const body = try self.expand_expr(body_node, tokens, source_id_value, node_allocator, depth);
                        arm.pattern = ink.ast.ref(pattern);
                        arm.body = ink.ast.ref(body);
                    }
                },
                .select_expr => |*se| {
                    const arms = @constCast(se.arms);
                    if (arms.len > 0) {
                        if (!try self.ensure_slice(@intFromPtr(arms.ptr), arms.len, source_id_value, @sizeOf(ink.ast.select_arm), "select_expr.arms", null)) return node;
                    }
                    for (arms) |*arm| {
                        const task_node = self.deref_node(arm.task, source_id_value) orelse return node;
                        const body_node = self.deref_node(arm.body, source_id_value) orelse return node;
                        const task = try self.expand_expr(task_node, tokens, source_id_value, node_allocator, depth);
                        const body = try self.expand_expr(body_node, tokens, source_id_value, node_allocator, depth);
                        arm.task = ink.ast.ref(task);
                        arm.body = ink.ast.ref(body);
                    }
                },
                .with_expr => |*we| {
                    const body_node = self.deref_node(we.body, source_id_value) orelse return node;
                    const body = try self.expand_expr(body_node, tokens, source_id_value, node_allocator, depth);
                    we.body = ink.ast.ref(body);
                },
                .label_expr => |*le| {
                    const body_node = self.deref_node(le.body, source_id_value) orelse return node;
                    const body = try self.expand_expr(body_node, tokens, source_id_value, node_allocator, depth);
                    le.body = ink.ast.ref(body);
                },
                .loop_expr => |*le| {
                    const body_node = self.deref_node(le.body, source_id_value) orelse return node;
                    const body = try self.expand_expr(body_node, tokens, source_id_value, node_allocator, depth);
                    le.body = ink.ast.ref(body);
                },
                .while_expr => |*we| {
                    const cond_node = self.deref_node(we.condition, source_id_value) orelse return node;
                    const body_node = self.deref_node(we.body, source_id_value) orelse return node;
                    const cond = try self.expand_expr(cond_node, tokens, source_id_value, node_allocator, depth);
                    const body = try self.expand_expr(body_node, tokens, source_id_value, node_allocator, depth);
                    we.condition = ink.ast.ref(cond);
                    we.body = ink.ast.ref(body);
                },
                .while_in_expr => |*we| {
                    const iter_node = self.deref_node(we.iter, source_id_value) orelse return node;
                    const body_node = self.deref_node(we.body, source_id_value) orelse return node;
                    const pattern_node = self.deref_node(we.pattern, source_id_value) orelse return node;
                    const iter = try self.expand_expr(iter_node, tokens, source_id_value, node_allocator, depth);
                    const body = try self.expand_expr(body_node, tokens, source_id_value, node_allocator, depth);
                    const pattern = try self.expand_expr(pattern_node, tokens, source_id_value, node_allocator, depth);
                    we.iter = ink.ast.ref(iter);
                    we.body = ink.ast.ref(body);
                    we.pattern = ink.ast.ref(pattern);
                },
                .until_expr => |*ue| {
                    const cond_node = self.deref_node(ue.condition, source_id_value) orelse return node;
                    const body_node = self.deref_node(ue.body, source_id_value) orelse return node;
                    const cond = try self.expand_expr(cond_node, tokens, source_id_value, node_allocator, depth);
                    const body = try self.expand_expr(body_node, tokens, source_id_value, node_allocator, depth);
                    ue.condition = ink.ast.ref(cond);
                    ue.body = ink.ast.ref(body);
                },
                .repeat_expr => |*re| {
                    const count_node = self.deref_node(re.count, source_id_value) orelse return node;
                    const body_node = self.deref_node(re.body, source_id_value) orelse return node;
                    const count = try self.expand_expr(count_node, tokens, source_id_value, node_allocator, depth);
                    const body = try self.expand_expr(body_node, tokens, source_id_value, node_allocator, depth);
                    re.count = ink.ast.ref(count);
                    re.body = ink.ast.ref(body);
                },
                .for_expr => |*fe| {
                    const iter_node = self.deref_node(fe.iter, source_id_value) orelse return node;
                    const body_node = self.deref_node(fe.body, source_id_value) orelse return node;
                    const pattern_node = self.deref_node(fe.pattern, source_id_value) orelse return node;
                    const iter = try self.expand_expr(iter_node, tokens, source_id_value, node_allocator, depth);
                    const body = try self.expand_expr(body_node, tokens, source_id_value, node_allocator, depth);
                    const pattern = try self.expand_expr(pattern_node, tokens, source_id_value, node_allocator, depth);
                    fe.iter = ink.ast.ref(iter);
                    fe.body = ink.ast.ref(body);
                    fe.pattern = ink.ast.ref(pattern);
                },
                .each_expr => |*ee| {
                    const iter_node = self.deref_node(ee.iter, source_id_value) orelse return node;
                    const body_node = self.deref_node(ee.body, source_id_value) orelse return node;
                    const pattern_node = self.deref_node(ee.pattern, source_id_value) orelse return node;
                    const iter = try self.expand_expr(iter_node, tokens, source_id_value, node_allocator, depth);
                    const body = try self.expand_expr(body_node, tokens, source_id_value, node_allocator, depth);
                    const pattern = try self.expand_expr(pattern_node, tokens, source_id_value, node_allocator, depth);
                    ee.iter = ink.ast.ref(iter);
                    ee.body = ink.ast.ref(body);
                    ee.pattern = ink.ast.ref(pattern);
                },
                .break_expr => |*be| {
                    if (be.value) |val_ref| {
                        const value_node = self.deref_node(val_ref, source_id_value) orelse return node;
                        const value = try self.expand_expr(value_node, tokens, source_id_value, node_allocator, depth);
                        be.value = ink.ast.ref_opt(value);
                    }
                },
                .yield_expr => |*ye| {
                    if (ye.value) |val_ref| {
                        const value_node = self.deref_node(val_ref, source_id_value) orelse return node;
                        const value = try self.expand_expr(value_node, tokens, source_id_value, node_allocator, depth);
                        ye.value = ink.ast.ref_opt(value);
                    }
                },
                .atomic_expr => |*ae| {
                    const value_node = self.deref_node(ae.value, source_id_value) orelse return node;
                    const value = try self.expand_expr(value_node, tokens, source_id_value, node_allocator, depth);
                    ae.value = ink.ast.ref(value);
                },
                .block => |*block| {
                    const items = @constCast(block.items);
                    if (items.len > 0) {
                        if (!try self.ensure_slice(@intFromPtr(items.ptr), items.len, source_id_value, @sizeOf(ink.ast.node_ref), "block.items", null)) return node;
                    }
                    var item_nodes = try self.allocator.alloc(*ink.node, items.len);
                    defer self.allocator.free(item_nodes);
                    for (items, 0..) |item_ref, i| {
                        item_nodes[i] = self.deref_node(item_ref, source_id_value) orelse return node;
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
                    if (items.len > 0) {
                        if (!try self.ensure_slice(@intFromPtr(items.ptr), items.len, source_id_value, @sizeOf(ink.ast.associate), "record.items", null)) return node;
                    }
                    for (items) |*assoc| {
                        if (assoc.value) |val_ref| {
                            const value_node = self.deref_node(val_ref, source_id_value) orelse return node;
                            const value = try self.expand_expr(value_node, tokens, source_id_value, node_allocator, depth);
                            assoc.value = ink.ast.ref_opt(value);
                        }
                    }
                },
                .intrinsic => |*call| {
                    const args = @constCast(call.args);
                    if (args.len > 0) {
                        if (!try self.ensure_slice(@intFromPtr(args.ptr), args.len, source_id_value, @sizeOf(ink.ast.node_ref), "intrinsic.args", null)) return node;
                    }
                    for (args) |*arg_ref| {
                        const arg_node = self.deref_node(arg_ref.*, source_id_value) orelse return node;
                        const value = try self.expand_expr(arg_node, tokens, source_id_value, node_allocator, depth);
                        arg_ref.* = ink.ast.ref(value);
                    }
                },
                .associate => |*assoc| {
                    if (assoc.value) |val_ref| {
                        const value_node = self.deref_node(val_ref, source_id_value) orelse return node;
                        const value = try self.expand_expr(value_node, tokens, source_id_value, node_allocator, depth);
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
            const target_node = self.deref_node(mc.target, source_id_value) orelse return node;
            const target = self.resolve_macro_target(target_node, source_id_value) orelse {
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

            if (self.checks) |checks| {
                if (checks.macro_streams) {
                    if (self.macro_ctx.validate_stream(self.allocator, out_stream.?)) |err| {
                        const msg = switch (err) {
                            .invalid_stream => "macro output stream invalid",
                            .invalid_tree => "macro output tree invalid",
                            .invalid_token => "macro output token invalid",
                            .invalid_group => "macro output group invalid",
                            .cycle => "macro output stream cycle detected",
                        };
                        try self.diags.append(.{
                            .danger = .@"error",
                            .message = msg,
                            .span = span{ .start = mc.where.start, .end = mc.where.end },
                            .source_id = source_id_value,
                        });
                        return node;
                    }
                }
            }

            const parsed = try self.parse_macro_expr(out_stream.?, mc.where, source_id_value);
            if (parsed == null) return node;
            defer self.allocator.free(parsed.?.tokens);
            if (self.checks) |checks| {
                if (checks.macro_ast) {
                    const ok = try validate_mod.validate_macro_ast(
                        self.allocator,
                        &[_]*ink.node{parsed.?.node},
                        parsed.?.registry,
                        self.diags,
                        source_id_value,
                        span{ .start = mc.where.start, .end = mc.where.end },
                    );
                    if (!ok) return node;
                }
            }
            const expanded = try self.expand_expr(parsed.?.node, parsed.?.tokens, source_id_value, parsed.?.node_allocator, depth + 1);
            return expanded;
        }

        fn resolve_macro_target(self: *macro_expander, node: *ink.node, source_id_value: src.source_id) ?macro_target {
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
                    const left = self.deref_node(bin.left, source_id_value) orelse return null;
                    const right = self.deref_node(bin.right, source_id_value) orelse return null;
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
                true,
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
            registry: []const *ink.node,
        };

        const parsed_program = struct {
            nodes: []const *ink.node,
            tokens: []const ink.token,
            node_allocator: mem_allocator,
            registry: []const *ink.node,
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

            var registry = std.array_list.Managed(*ink.node).init(parse.arena.allocator());
            var builder = ink.peg_ast.builder.init_with_registry(parse.arena.allocator(), tokens, &parse.tree, "", &registry);
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

            const registry_nodes = try registry.toOwnedSlice();
            if (self.node_set) |set| {
                for (registry_nodes) |reg_node| {
                    _ = set.put(@intFromPtr(reg_node), {}) catch {};
                }
            }
            parse.tree.deinit(parse.arena.allocator());
            const arena_ptr = try self.allocator.create(arena_allocator);
            errdefer self.allocator.destroy(arena_ptr);
            arena_ptr.* = parse.arena;
            try self.macro_arenas.append(arena_ptr);
            return .{
                .node = expr,
                .tokens = tokens,
                .node_allocator = arena_ptr.allocator(),
                .registry = registry_nodes,
            };
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

            var registry = std.array_list.Managed(*ink.node).init(parse.arena.allocator());
            var builder = ink.peg_ast.builder.init_with_registry(parse.arena.allocator(), tokens, &parse.tree, "", &registry);
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

            const registry_nodes = try registry.toOwnedSlice();
            if (self.node_set) |set| {
                for (registry_nodes) |reg_node| {
                    _ = set.put(@intFromPtr(reg_node), {}) catch {};
                }
            }
            parse.tree.deinit(parse.arena.allocator());
            const arena_ptr = try self.allocator.create(arena_allocator);
            errdefer self.allocator.destroy(arena_ptr);
            arena_ptr.* = parse.arena;
            try self.macro_arenas.append(arena_ptr);
            return .{
                .nodes = nodes,
                .tokens = tokens,
                .node_allocator = arena_ptr.allocator(),
                .registry = registry_nodes,
            };
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

        const validation_checks: ?sandbox_mod.Checks = if (req.sandbox) |cfg|
            sandbox_mod.resolve_checks(cfg)
        else
            null;

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
            return finish(&diags, &diag_messages, req.sources, null, null, null, null, null, false);
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
        var module_value_exports = try allocator.alloc(string_map(void), modules.items.len);
        var module_type_exports = try allocator.alloc(string_map(void), modules.items.len);
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
                module_value_exports[i].deinit();
                module_type_exports[i].deinit();
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
            allocator.free(module_value_exports);
            allocator.free(module_type_exports);
        }

        for (module_import_specs) |*slot| slot.* = null;
        for (module_value_exports) |*exports| exports.* = string_map(void).init(allocator);
        for (module_type_exports) |*exports| exports.* = string_map(void).init(allocator);

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

                var registry = std.array_list.Managed(*ink.node).init(parse.arena.allocator());
                var builder = ink.peg_ast.builder.init_with_registry(parse.arena.allocator(), tokens, &parse.tree, compsrc.text, &registry);
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
                if (validation_checks) |checks| {
                    if (checks.ast) {
                        const ok = try validate_mod.validate_ast_with_registry(
                            allocator,
                            file_nodes,
                            registry.items,
                            &diags,
                            compsrc.id,
                            null,
                        );
                        if (!ok) {
                            allocator.free(tokens);
                            parse.deinit();
                            continue;
                        }
                    }
                }

                const registry_nodes = try registry.toOwnedSlice();

                try files.append(.{
                    .source_id = sid,
                    .arena = parse.arena,
                    .tokens = tokens,
                    .nodes = file_nodes,
                    .registry = registry_nodes,
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
            return finish(&diags, &diag_messages, req.sources, null, null, null, null, null, false);
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

        var macro_arenas = array_list(*arena_allocator).init(allocator);
        defer {
            for (macro_arenas.items) |arena| {
                arena.deinit();
                allocator.destroy(arena);
            }
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
            return finish(&diags, &diag_messages, req.sources, null, null, null, null, null, false);
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
                validation_checks,
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
            return finish(&diags, &diag_messages, req.sources, null, null, null, null, null, false);
        }

        ink.vm.foreign.set_macro_context(&macro_ctx);
        defer ink.vm.foreign.set_macro_context(null);

        for (modules.items, 0..) |mod, mi| {
            var node_set = std.AutoHashMap(usize, void).init(allocator);
            defer node_set.deinit();
            for (module_files[mi]) |file| {
                for (file.registry) |reg_node| {
                    _ = try node_set.put(@intFromPtr(reg_node), {});
                }
            }
            var expander = macro_expander{
                .allocator = allocator,
                .macro_ctx = &macro_ctx,
                .macro_modules = &macro_modules,
                .imports = &module_macro_imports[mi],
                .module_id = mod.id,
                .diags = &diags,
                .diag_messages = &diag_messages,
                .macro_arenas = &macro_arenas,
                .checks = validation_checks,
                .node_set = &node_set,
                .files = module_files[mi],
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

            if (req.sandbox) |cfg| {
                try sandbox_mod.validate_foreigns(module_raw_nodes[mi], module_raw_sources[mi], &diags, cfg);
            }
            if (validation_checks) |checks| {
                if (checks.ast) {
                    _ = try validate_mod.validate_ast(
                        allocator,
                        module_raw_nodes[mi],
                        &diags,
                        null,
                        null,
                    );
                }
            }
        }

        if (has_error(diags.items)) {
            return finish(&diags, &diag_messages, req.sources, null, null, null, null, null, false);
        }

        for (modules.items, 0..) |mod, mi| {
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
            try collect_exports(&module_value_exports[mi], &module_type_exports[mi], module_nodes[mi]);
            if (!std.mem.eql(u8, mod.name, req.root_module)) {
                try qualify_module_nodes(
                    allocator,
                    module_arenas[mi].allocator(),
                    module_nodes[mi],
                    mod.name,
                    &module_value_exports[mi],
                    &module_type_exports[mi],
                );
            }
            module_imports[mi] = try build_module_imports(allocator, &module_names, desugared.imports, &diags);
            module_foreigns[mi] = try collect_foreigns_from_nodes(allocator, module_nodes[mi]);
        }

        if (has_error(diags.items)) {
            return finish(&diags, &diag_messages, req.sources, null, null, null, null, null, false);
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
            return finish(&diags, &diag_messages, req.sources, null, null, null, null, null, false);
        }

        // 6) UIR build (all modules)
        const root_id = module_names.get(req.root_module) orelse {
            try diags.append(.{ .danger = .@"error", .message = "unknown root module", .span = null });
            return finish(&diags, &diag_messages, req.sources, null, null, null, null, null, false);
        };
        const combined = try combine_module_nodes(allocator, module_nodes, module_node_sources);
        defer allocator.free(combined.nodes);
        defer allocator.free(combined.sources);

        var uir_arena = arena_allocator.init(allocator);
        defer uir_arena.deinit();
        var uir_builder = ink.uir_build.builder.init(uir_arena.allocator());
        defer uir_builder.deinit();
        uir_builder.build_nodes_with_sources(combined.nodes, combined.sources) catch {
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
            return finish(&diags, &diag_messages, req.sources, null, null, null, null, null, false);
        };
        const uir_result = uir_builder.finish() catch {
            try diags.append(.{ .danger = .@"error", .message = "uir build error", .span = null });
            return finish(&diags, &diag_messages, req.sources, null, null, null, null, null, false);
        };
        if (validation_checks) |checks| {
            if (checks.uir) {
                const ok = try validate_mod.validate_uir(uir_result, &diags);
                if (!ok) {
                    return finish(&diags, &diag_messages, req.sources, null, null, null, null, null, false);
                }
            }
        }

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
            return finish(&diags, &diag_messages, req.sources, null, null, null, null, null, false);
        };
        defer type_result.deinit(allocator);

        if (has_error(diags.items)) {
            return finish(&diags, &diag_messages, req.sources, null, null, null, null, null, false);
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
                var qualified = fname;
                var allocated = false;
                if (std.mem.indexOf(u8, fname, "::") == null) {
                    qualified = try qualify_name(allocator, imp.alias, fname);
                    allocated = true;
                }
                if (!foreign_set.contains(qualified)) {
                    try foreign_set.put(qualified, {});
                    try foreign_names.append(qualified);
                    if (allocated) try foreign_allocated.append(qualified);
                } else if (allocated) {
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
            return finish(&diags, &diag_messages, req.sources, null, null, null, null, foreigns, false);
        }

        var mir_arena = arena_allocator.init(allocator);
        defer mir_arena.deinit();
        const mir_result = mir_lower.lower(mir_arena.allocator(), uir_result) catch {
            try diags.append(.{ .danger = .@"error", .message = "mir lowering error", .span = null });
            return finish(&diags, &diag_messages, req.sources, null, null, null, null, foreigns, false);
        };
        if (validation_checks) |checks| {
            if (checks.mir) {
                const ok = try validate_mod.validate_mir(mir_result, &diags);
                if (!ok) {
                    return finish(&diags, &diag_messages, req.sources, null, null, null, null, foreigns, false);
                }
            }
        }

        var lower_info = lir_lower.error_info{};
        defer lower_info.deinit(allocator);

        const collect_signatures = req.debug_info and req.target.kind == .vm;
        var signatures = array_list(ink.lir_vm_lower.function_signature).init(allocator);
        defer deinit_signatures(allocator, &signatures);

        const lower_opts: ?ink.lir_vm_lower.lower_options = if (collect_signatures)
            .{ .signatures = &signatures }
        else
            null;

        const lir_result = lir_lower.lower_with_options(
            allocator,
            req.target,
            mir_result.nodes,
            mir_result.strings,
            mir_result.roots,
            foreigns,
            type_result.types,
            &lower_info,
            lower_opts,
        ) catch |err| {
            if (err == error.unsupported_target) {
                try diags.append(.{
                    .danger = .@"error",
                    .message = "unsupported target backend",
                    .span = null,
                });
                return finish(&diags, &diag_messages, req.sources, null, null, null, null, foreigns, false);
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
            return finish(&diags, &diag_messages, req.sources, null, null, null, null, foreigns, false);
        };
        if (validation_checks) |checks| {
            if (checks.lir) {
                const ok = switch (lir_result) {
                    .vm => |bundle| try validate_mod.validate_vm_program(allocator, bundle.program, foreigns.len, &diags),
                };
                if (!ok) {
                    return finish(&diags, &diag_messages, req.sources, null, null, null, null, foreigns, false);
                }
            }
        }

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
                return finish(&diags, &diag_messages, req.sources, null, null, null, null, foreigns, false);
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
            return finish(&diags, &diag_messages, req.sources, null, null, null, null, foreigns, false);
        };

        const result_foreigns = backend_result.foreigns orelse foreigns;
        var debug_info: ?ink.vm.inkb.debug_info = null;
        if (collect_signatures) {
            const instructions = switch (lir_result) {
                .vm => |bundle| bundle.program.instructions,
            };
            debug_info = build_debug_info(allocator, instructions, signatures.items) catch |err| {
                const msg = switch (err) {
                    error.OutOfMemory => "debug info error: out_of_memory",
                    error.label_not_found => "debug info error: label_not_found",
                    else => "debug info error",
                };
                try diags.append(.{
                    .danger = .@"error",
                    .message = msg,
                    .span = null,
                });
                return finish(&diags, &diag_messages, req.sources, null, null, null, null, result_foreigns, false);
            };
        }

        var result = try finish(
            &diags,
            &diag_messages,
            req.sources,
            backend_result.instructions,
            backend_result.constants,
            backend_result.data,
            backend_result.bytecode,
            result_foreigns,
            true,
        );
        result.debug = debug_info;
        return result;
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
                if (tok.which == .illegal) {
                    const msg = if (is_all_spaces(tok.what.string))
                        "leading spaces are not allowed; use tabs for indentation"
                    else
                        "illegal token";
                    try diags.append(.{
                        .danger = .@"error",
                        .message = msg,
                        .span = span{ .start = tok.where.start, .end = tok.where.end },
                        .source_id = src_id,
                        .code = "E1000",
                    });
                }
                try tokens.append(tok);
                if (tok.which == .end_of_file) break;
            } else break;
        }

        return tokens.toOwnedSlice();
    }

    fn is_all_spaces(text: []const u8) bool {
        if (text.len == 0) return false;
        for (text) |ch| {
            if (ch != ' ') return false;
        }
        return true;
    }

    fn collect_exports(
        value_exports: *string_map(void),
        type_exports: *string_map(void),
        nodes: []const *ink.node,
    ) !void {
        for (nodes) |node| {
            if (node.* != .decl) continue;
            switch (node.decl) {
                .function => |f| try add_export(value_exports, f.name.string),
                .@"const" => |c| try add_export(value_exports, c.name.string),
                .@"var" => |v| try add_export(value_exports, v.name.string),
                .type_alias => |t| try add_export(type_exports, t.name.string),
                .@"struct" => |s| {
                    try add_export(value_exports, s.name.string);
                    try add_export(type_exports, s.name.string);
                },
                .trait => |t| try add_export(type_exports, t.name.string),
                .@"enum" => |e| {
                    try add_export(type_exports, e.name.string);
                    try add_export(value_exports, e.name.string);
                },
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

    const module_qualifier = struct {
        const qualify_error = error{OutOfMemory};

        const scope = struct {
            values: string_map(void),
            types: string_map(void),

            fn init(allocator: mem_allocator) scope {
                return .{
                    .values = string_map(void).init(allocator),
                    .types = string_map(void).init(allocator),
                };
            }

            fn deinit(self: *scope) void {
                self.values.deinit();
                self.types.deinit();
            }
        };

        allocator: mem_allocator,
        node_allocator: mem_allocator,
        module_name: []const u8,
        value_exports: *const string_map(void),
        type_exports: *const string_map(void),
        prefixed: string_map([]const u8),
        scopes: std.ArrayListUnmanaged(scope) = .{},

        fn init(
            allocator: mem_allocator,
            node_allocator: mem_allocator,
            module_name: []const u8,
            value_exports: *const string_map(void),
            type_exports: *const string_map(void),
        ) module_qualifier {
            return .{
                .allocator = allocator,
                .node_allocator = node_allocator,
                .module_name = module_name,
                .value_exports = value_exports,
                .type_exports = type_exports,
                .prefixed = string_map([]const u8).init(allocator),
                .scopes = .{},
            };
        }

        fn deinit(self: *module_qualifier) void {
            var i: usize = self.scopes.items.len;
            while (i > 0) : (i -= 1) {
                self.scopes.items[i - 1].deinit();
            }
            self.scopes.deinit(self.allocator);
            self.prefixed.deinit();
        }

        fn push_scope(self: *module_qualifier) qualify_error!void {
            try self.scopes.append(self.allocator, scope.init(self.allocator));
        }

        fn pop_scope(self: *module_qualifier) void {
            if (self.scopes.pop()) |s| {
                var scope_val = s;
                scope_val.deinit();
            }
        }

        fn add_local_value(self: *module_qualifier, name: []const u8) qualify_error!void {
            if (self.scopes.items.len == 0) try self.push_scope();
            const idx = self.scopes.items.len - 1;
            try self.scopes.items[idx].values.put(name, {});
        }

        fn add_local_type(self: *module_qualifier, name: []const u8) qualify_error!void {
            if (self.scopes.items.len == 0) try self.push_scope();
            const idx = self.scopes.items.len - 1;
            try self.scopes.items[idx].types.put(name, {});
        }

        fn is_local_value(self: *module_qualifier, name: []const u8) bool {
            var i: usize = self.scopes.items.len;
            while (i > 0) : (i -= 1) {
                if (self.scopes.items[i - 1].values.contains(name)) return true;
            }
            return false;
        }

        fn is_local_type(self: *module_qualifier, name: []const u8) bool {
            var i: usize = self.scopes.items.len;
            while (i > 0) : (i -= 1) {
                if (self.scopes.items[i - 1].types.contains(name)) return true;
            }
            return false;
        }

        fn has_module_prefix(self: *module_qualifier, name: []const u8) bool {
            if (!std.mem.startsWith(u8, name, self.module_name)) return false;
            if (name.len < self.module_name.len + 2) return false;
            return name[self.module_name.len] == ':' and name[self.module_name.len + 1] == ':';
        }

        fn base_name(name: []const u8) []const u8 {
            if (std.mem.indexOf(u8, name, "::")) |idx| {
                return name[0..idx];
            }
            return name;
        }

        fn is_builtin_value_name(name: []const u8) bool {
            if (std.mem.eql(u8, name, "_") or std.mem.eql(u8, name, "*")) return true;
            if (std.mem.eql(u8, name, "unit")) return true;
            if (std.mem.eql(u8, name, "true")) return true;
            if (std.mem.eql(u8, name, "false")) return true;
            if (std.mem.eql(u8, name, "none")) return true;
            if (std.mem.eql(u8, name, "cancel")) return true;
            if (std.mem.eql(u8, name, "error")) return true;
            if (std.mem.eql(u8, name, "token_tree_kind")) return true;
            if (std.mem.eql(u8, name, "token_kind")) return true;
            if (std.mem.eql(u8, name, "delimiter")) return true;
            if (std.mem.startsWith(u8, name, "error::")) return true;
            return false;
        }

        fn is_builtin_type_name(name: []const u8) bool {
            return std.mem.eql(u8, name, "int") or std.mem.eql(u8, name, "uint") or
                std.mem.eql(u8, name, "u8") or std.mem.eql(u8, name, "float") or std.mem.eql(u8, name, "bool") or
                std.mem.eql(u8, name, "string") or std.mem.eql(u8, name, "token_stream") or
                std.mem.eql(u8, name, "token_tree") or std.mem.eql(u8, name, "token") or
                std.mem.eql(u8, name, "token_group") or std.mem.eql(u8, name, "token_kind") or
                std.mem.eql(u8, name, "token_tree_kind") or std.mem.eql(u8, name, "delimiter") or
                std.mem.eql(u8, name, "span") or std.mem.eql(u8, name, "symbol") or
                std.mem.eql(u8, name, "type") or std.mem.eql(u8, name, "none") or
                std.mem.eql(u8, name, "result") or std.mem.eql(u8, name, "error") or
                std.mem.eql(u8, name, "task") or std.mem.eql(u8, name, "buf") or
                std.mem.eql(u8, name, "arena") or std.mem.eql(u8, name, "union") or
                std.mem.eql(u8, name, "intersect") or std.mem.eql(u8, name, "tuple") or
                std.mem.eql(u8, name, "fn") or std.mem.eql(u8, name, "slice") or
                std.mem.eql(u8, name, "array") or std.mem.eql(u8, name, "list") or
                std.mem.eql(u8, name, "box") or std.mem.eql(u8, name, "atomic") or
                std.mem.eql(u8, name, "duration") or std.mem.eql(u8, name, "instant") or
                std.mem.eql(u8, name, "deadline") or std.mem.eql(u8, name, "fd") or
                std.mem.eql(u8, name, "not") or std.mem.eql(u8, name, "send") or
                std.mem.eql(u8, name, "sync") or std.mem.eql(u8, name, "sized");
        }

        fn prefixed_name(self: *module_qualifier, name: []const u8) qualify_error![]const u8 {
            if (self.prefixed.get(name)) |cached| return cached;
            const sep = "::";
            var buf = try self.node_allocator.alloc(u8, self.module_name.len + sep.len + name.len);
            std.mem.copyForwards(u8, buf[0..self.module_name.len], self.module_name);
            std.mem.copyForwards(u8, buf[self.module_name.len .. self.module_name.len + sep.len], sep);
            std.mem.copyForwards(u8, buf[self.module_name.len + sep.len ..], name);
            try self.prefixed.put(name, buf);
            return buf;
        }

        fn qualify_value_ident(self: *module_qualifier, id: *ink.identifier) qualify_error!void {
            const name = id.string;
            if (self.has_module_prefix(name)) return;
            const base = base_name(name);
            if (is_builtin_value_name(base)) return;
            if (self.is_local_value(base)) return;
            if (!self.value_exports.contains(base)) return;
            id.string = try self.prefixed_name(name);
        }

        fn qualify_type_ident(self: *module_qualifier, id: *ink.identifier) qualify_error!void {
            const name = id.string;
            if (self.has_module_prefix(name)) return;
            const base = base_name(name);
            if (is_builtin_type_name(base)) return;
            if (self.is_local_type(base)) return;
            if (!self.type_exports.contains(base)) return;
            id.string = try self.prefixed_name(name);
        }

        fn qualify_type_expr(self: *module_qualifier, ty: *ink.ast.type_expr) qualify_error!void {
            switch (ty.*) {
                .self => {},
                .name => |*id| try self.qualify_type_ident(id),
                .optional => |ref| try self.qualify_type_node(ink.ast.deref(ref)),
                .dyn => |ref| try self.qualify_type_node(ink.ast.deref(ref)),
                .applied => |*ap| {
                    try self.qualify_type_ident(&ap.base);
                    for (ap.args) |arg_ref| {
                        try self.qualify_type_arg(ink.ast.deref(arg_ref));
                    }
                },
            }
        }

        fn qualify_type_arg(self: *module_qualifier, node: *ink.node) qualify_error!void {
            switch (node.*) {
                .identifier => |*id| try self.qualify_type_ident(id),
                .type => |*ty| try self.qualify_type_expr(ty),
                else => try self.qualify_node(node, false),
            }
        }

        fn qualify_type_node(self: *module_qualifier, node: *ink.node) qualify_error!void {
            switch (node.*) {
                .type => |*ty| try self.qualify_type_expr(ty),
                else => try self.qualify_node(node, false),
            }
        }

        fn qualify_generic_params(self: *module_qualifier, params: []const ink.ast.generic_param) qualify_error!void {
            for (params) |param| {
                switch (param.kind) {
                    .type => try self.add_local_type(param.name.string),
                    .value => try self.add_local_value(param.name.string),
                }
            }
            for (params) |param| {
                if (param.constraint) |ref| {
                    const node = ink.ast.deref(ref);
                    if (param.kind == .type) {
                        try self.qualify_type_node(node);
                    } else {
                        try self.qualify_node(node, false);
                    }
                }
                if (param.default) |ref| {
                    const node = ink.ast.deref(ref);
                    if (param.kind == .type) {
                        try self.qualify_type_node(node);
                    } else {
                        try self.qualify_node(node, false);
                    }
                }
            }
        }

        fn qualify_where_clause(self: *module_qualifier, reqs: []const ink.ast.where_req) qualify_error!void {
            for (reqs) |req| {
                try self.qualify_type_node(ink.ast.deref(req.constraint));
            }
        }

        fn qualify_pattern(self: *module_qualifier, node: *ink.node) qualify_error!void {
            switch (node.*) {
                .identifier => |*id| {
                    if (std.mem.eql(u8, id.string, "*") or std.mem.eql(u8, id.string, "_")) return;
                    if (std.mem.indexOf(u8, id.string, "::") != null) {
                        try self.qualify_value_ident(id);
                        return;
                    }
                    try self.add_local_value(id.string);
                },
                .record => |rec| {
                    for (rec.items) |assoc| {
                        if (assoc.value) |ref| try self.qualify_pattern(ink.ast.deref(ref));
                    }
                },
                .binary => |bin| switch (bin.op) {
                    .call => {
                        var current = node;
                        var args = std.array_list.Managed(*ink.node).init(self.allocator);
                        defer args.deinit();
                        while (current.* == .binary and current.binary.op == .call) {
                            const call = current.binary;
                            args.append(ink.ast.deref(call.right)) catch return error.OutOfMemory;
                            current = ink.ast.deref(call.left);
                        }
                        std.mem.reverse(*ink.node, args.items);
                        try self.qualify_node(current, false);
                        for (args.items) |arg| try self.qualify_pattern(arg);
                    },
                    .access, .scope_access => try self.qualify_node(ink.ast.deref(bin.left), false),
                    else => {
                        try self.qualify_pattern(ink.ast.deref(bin.left));
                        try self.qualify_pattern(ink.ast.deref(bin.right));
                    },
                },
                .unary => |un| try self.qualify_pattern(ink.ast.deref(un.right)),
                else => try self.qualify_node(node, false),
            }
        }

        fn qualify_function_decl(
            self: *module_qualifier,
            func: *ink.ast.function_decl,
            top_level: bool,
        ) qualify_error!void {
            if (top_level) {
                var name = func.name;
                try self.qualify_value_ident(&name);
                func.name = name;
            }
            try self.push_scope();
            defer self.pop_scope();

            try self.qualify_generic_params(func.generics);
            for (func.params) |param| {
                try self.add_local_value(param.name.string);
            }
            for (func.params) |param| {
                try self.qualify_type_node(ink.ast.deref(param.ty));
            }
            if (func.return_type) |ref| {
                try self.qualify_type_node(ink.ast.deref(ref));
            }
            try self.qualify_where_clause(func.where_clause);
            if (func.body) |ref| {
                try self.qualify_node(ink.ast.deref(ref), false);
            }
        }

        fn qualify_decl(self: *module_qualifier, decl: *ink.ast.decl, top_level: bool) qualify_error!void {
            switch (decl.*) {
                .function => |*func| try self.qualify_function_decl(func, top_level),
                .@"const" => |*c| {
                    if (top_level) {
                        var name = c.name;
                        try self.qualify_value_ident(&name);
                        c.name = name;
                    } else {
                        try self.add_local_value(c.name.string);
                    }
                    if (c.ty) |ref| try self.qualify_type_node(ink.ast.deref(ref));
                    try self.qualify_node(ink.ast.deref(c.value), false);
                },
                .@"var" => |*v| {
                    if (top_level) {
                        var name = v.name;
                        try self.qualify_value_ident(&name);
                        v.name = name;
                    } else {
                        try self.add_local_value(v.name.string);
                    }
                    if (v.ty) |ref| try self.qualify_type_node(ink.ast.deref(ref));
                    try self.qualify_node(ink.ast.deref(v.value), false);
                },
                .type_alias => |*t| {
                    if (top_level) {
                        var name = t.name;
                        try self.qualify_type_ident(&name);
                        t.name = name;
                    } else {
                        try self.add_local_type(t.name.string);
                    }
                    try self.push_scope();
                    defer self.pop_scope();
                    try self.qualify_generic_params(t.generics);
                    try self.qualify_type_node(ink.ast.deref(t.value));
                },
                .@"struct" => |*s| {
                    if (top_level) {
                        var name = s.name;
                        try self.qualify_type_ident(&name);
                        s.name = name;
                    } else {
                        try self.add_local_type(s.name.string);
                        try self.add_local_value(s.name.string);
                    }
                    try self.push_scope();
                    defer self.pop_scope();
                    try self.qualify_generic_params(s.generics);
                    for (s.fields) |field| {
                        try self.qualify_type_node(ink.ast.deref(field.ty));
                    }
                },
                .@"enum" => |*e| {
                    if (top_level) {
                        var name = e.name;
                        try self.qualify_type_ident(&name);
                        e.name = name;
                    } else {
                        try self.add_local_type(e.name.string);
                    }
                    try self.push_scope();
                    defer self.pop_scope();
                    try self.qualify_generic_params(e.generics);
                    for (e.variants) |variant| {
                        if (variant.payload) |ref| {
                            try self.qualify_type_node(ink.ast.deref(ref));
                        }
                    }
                },
                .trait => |*t| {
                    if (top_level) {
                        var name = t.name;
                        try self.qualify_type_ident(&name);
                        t.name = name;
                    } else {
                        try self.add_local_type(t.name.string);
                    }
                    try self.push_scope();
                    defer self.pop_scope();
                    try self.qualify_generic_params(t.generics);
                    for (t.requires) |ref| {
                        try self.qualify_type_node(ink.ast.deref(ref));
                    }
                    const items = @constCast(t.items);
                    for (items) |*item| {
                        switch (item.*) {
                            .function => |*func| try self.qualify_function_decl(func, false),
                            .assoc_type => |*assoc| {
                                if (assoc.value) |ref| {
                                    try self.qualify_type_node(ink.ast.deref(ref));
                                }
                            },
                        }
                    }
                },
                .@"impl" => |*im| {
                    try self.qualify_type_ident(&im.by_trait);
                    try self.qualify_type_ident(&im.for_struct);
                    const functions = @constCast(im.functions);
                    for (functions) |*func| {
                        try self.qualify_function_decl(func, false);
                    }
                },
                .import => |_| {},
            }
        }

        fn qualify_node(self: *module_qualifier, node: *ink.node, top_level: bool) qualify_error!void {
            switch (node.*) {
                .integer, .float, .duration, .string => {},
                .identifier => |*id| {
                    if (id.owner == .ref) {
                        try self.qualify_value_ident(id);
                    }
                },
                .type => |*ty| try self.qualify_type_expr(ty),
                .unary => |un| try self.qualify_node(ink.ast.deref(un.right), false),
                .binary => |bin| {
                    switch (bin.op) {
                        .access, .scope_access => try self.qualify_node(ink.ast.deref(bin.left), false),
                        else => {
                            try self.qualify_node(ink.ast.deref(bin.left), false);
                            try self.qualify_node(ink.ast.deref(bin.right), false);
                        },
                    }
                },
                .macro_call => |mc| try self.qualify_node(ink.ast.deref(mc.target), false),
                .if_expr => |ife| {
                    try self.qualify_node(ink.ast.deref(ife.condition), false);
                    try self.qualify_node(ink.ast.deref(ife.then_branch), false);
                    if (ife.else_branch) |ref| try self.qualify_node(ink.ast.deref(ref), false);
                },
                .match_expr => |me| {
                    try self.qualify_node(ink.ast.deref(me.target), false);
                    for (me.arms) |arm| {
                        try self.push_scope();
                        try self.qualify_pattern(ink.ast.deref(arm.pattern));
                        try self.qualify_node(ink.ast.deref(arm.body), false);
                        self.pop_scope();
                    }
                },
                .select_expr => |se| {
                    for (se.arms) |arm| {
                        try self.push_scope();
                        if (arm.name) |id| try self.add_local_value(id.string);
                        try self.qualify_node(ink.ast.deref(arm.task), false);
                        try self.qualify_node(ink.ast.deref(arm.body), false);
                        self.pop_scope();
                    }
                },
                .with_expr => |we| {
                    try self.push_scope();
                    try self.add_local_value(we.name.string);
                    try self.qualify_node(ink.ast.deref(we.body), false);
                    self.pop_scope();
                },
                .label_expr => |le| try self.qualify_node(ink.ast.deref(le.body), false),
                .loop_expr => |le| try self.qualify_node(ink.ast.deref(le.body), false),
                .while_expr => |we| {
                    try self.qualify_node(ink.ast.deref(we.condition), false);
                    try self.qualify_node(ink.ast.deref(we.body), false);
                },
                .while_in_expr => |we| {
                    try self.qualify_node(ink.ast.deref(we.iter), false);
                    try self.push_scope();
                    try self.qualify_pattern(ink.ast.deref(we.pattern));
                    try self.qualify_node(ink.ast.deref(we.body), false);
                    self.pop_scope();
                },
                .until_expr => |ue| {
                    try self.qualify_node(ink.ast.deref(ue.condition), false);
                    try self.qualify_node(ink.ast.deref(ue.body), false);
                },
                .repeat_expr => |re| {
                    try self.qualify_node(ink.ast.deref(re.count), false);
                    try self.qualify_node(ink.ast.deref(re.body), false);
                },
                .for_expr => |fe| {
                    try self.qualify_node(ink.ast.deref(fe.iter), false);
                    try self.push_scope();
                    try self.qualify_pattern(ink.ast.deref(fe.pattern));
                    try self.qualify_node(ink.ast.deref(fe.body), false);
                    self.pop_scope();
                },
                .each_expr => |ee| {
                    try self.qualify_node(ink.ast.deref(ee.iter), false);
                    try self.push_scope();
                    try self.qualify_pattern(ink.ast.deref(ee.pattern));
                    try self.qualify_node(ink.ast.deref(ee.body), false);
                    self.pop_scope();
                },
                .break_expr => |be| if (be.value) |ref| try self.qualify_node(ink.ast.deref(ref), false),
                .continue_expr => {},
                .yield_expr => |ye| if (ye.value) |ref| try self.qualify_node(ink.ast.deref(ref), false),
                .atomic_expr => |ae| try self.qualify_node(ink.ast.deref(ae.value), false),
                .block => |blk| {
                    try self.push_scope();
                    defer self.pop_scope();
                    for (blk.items) |ref| {
                        const item = ink.ast.deref(ref);
                        try self.qualify_node(item, false);
                    }
                },
                .record => |rec| {
                    for (rec.items) |assoc| {
                        if (assoc.value) |ref| try self.qualify_node(ink.ast.deref(ref), false);
                    }
                },
                .associate => |assoc| if (assoc.value) |ref| try self.qualify_node(ink.ast.deref(ref), false),
                .intrinsic => |call| {
                    for (call.args) |arg_ref| try self.qualify_node(ink.ast.deref(arg_ref), false);
                },
                .decl => |*decl| try self.qualify_decl(decl, top_level),
            }
        }

        fn qualify_nodes(self: *module_qualifier, nodes: []const *ink.node) qualify_error!void {
            for (nodes) |node| {
                try self.qualify_node(node, true);
            }
        }
    };

    fn qualify_module_nodes(
        allocator: mem_allocator,
        node_allocator: mem_allocator,
        nodes: []const *ink.node,
        module_name: []const u8,
        value_exports: *const string_map(void),
        type_exports: *const string_map(void),
    ) module_qualifier.qualify_error!void {
        var qualifier = module_qualifier.init(allocator, node_allocator, module_name, value_exports, type_exports);
        defer qualifier.deinit();
        try qualifier.qualify_nodes(nodes);
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
        sources: []const source_file,
        instructions: ?[]const ink.exe.instruction,
        constants: ?[]const u64,
        data: ?[]const ink.vm.inkb.data_entry,
        bytecode: ?[]u8,
        foreigns: ?[]const []const u8,
        ok: bool,
    ) !compile_result {
        clamp_diagnostics(diags.items, sources);
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
            .debug = null,
        };
    }

    fn deinit_signatures(
        allocator: mem_allocator,
        signatures: *array_list(ink.lir_vm_lower.function_signature),
    ) void {
        for (signatures.items) |sig| {
            allocator.free(sig.name);
            if (sig.impl_for) |impl_name| allocator.free(impl_name);
        }
        signatures.deinit();
    }

    fn build_debug_info(
        allocator: mem_allocator,
        instructions: []const ink.exe.instruction,
        signatures: []const ink.lir_vm_lower.function_signature,
    ) !ink.vm.inkb.debug_info {
        var label_offsets = try ink.vm.encode.compute_label_offsets(allocator, instructions);
        defer label_offsets.deinit();

        var list = array_list(ink.vm.inkb.debug_function).init(allocator);
        errdefer {
            for (list.items) |func| allocator.free(func.name);
            list.deinit();
        }

        for (signatures) |sig| {
            const offset = label_offsets.get(sig.label) orelse continue;
            const name = try format_debug_name(allocator, sig);
            try list.append(.{ .entry_pc = @intCast(offset), .name = name });
        }

        const slice = try list.toOwnedSlice();
        std.mem.sort(ink.vm.inkb.debug_function, slice, {}, debug_func_less);
        return .{ .functions = slice };
    }

    fn format_debug_name(
        allocator: mem_allocator,
        sig: ink.lir_vm_lower.function_signature,
    ) ![]const u8 {
        if (sig.impl_for) |impl_name| {
            return std.fmt.allocPrint(allocator, "{s}::{s}", .{ impl_name, sig.name });
        }
        return allocator.dupe(u8, sig.name);
    }

    fn debug_func_less(_: void, lhs: ink.vm.inkb.debug_function, rhs: ink.vm.inkb.debug_function) bool {
        if (lhs.entry_pc == rhs.entry_pc) {
            return std.mem.lessThan(u8, lhs.name, rhs.name);
        }
        return lhs.entry_pc < rhs.entry_pc;
    }

    fn clamp_diagnostics(diags: []diagnostic, sources: []const source_file) void {
        for (diags) |*entry| {
            const span_opt = entry.span orelse continue;
            const src_id = entry.source_id orelse continue;
            const len = source_length(sources, src_id) orelse continue;
            var start = span_opt.start;
            var end = span_opt.end;
            if (start > len) start = len;
            if (end > len) end = len;
            if (start > end) start = end;
            entry.span = span{ .start = start, .end = end };
        }
    }

    fn source_length(sources: []const source_file, id: src.source_id) ?usize {
        for (sources) |file| {
            if (file.id == id) return file.text.len;
        }
        return null;
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

    fn combine_module_nodes(
        allocator: mem_allocator,
        module_nodes: []const []const *ink.node,
        module_sources: []const []const src.source_id,
    ) !struct { nodes: []const *ink.node, sources: []const src.source_id } {
        var total: usize = 0;
        for (module_nodes) |nodes| {
            total += nodes.len;
        }
        var nodes_out = try allocator.alloc(*ink.node, total);
        var sources_out = try allocator.alloc(src.source_id, total);
        var offset: usize = 0;
        for (module_nodes, 0..) |nodes, idx| {
            const sources = module_sources[idx];
            if (nodes.len > 0) {
                std.mem.copyForwards(*ink.node, nodes_out[offset .. offset + nodes.len], nodes);
                std.mem.copyForwards(src.source_id, sources_out[offset .. offset + nodes.len], sources);
            }
            offset += nodes.len;
        }
        return .{ .nodes = nodes_out, .sources = sources_out };
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

    fn clone_ast_nodes(
        node_allocator: mem_allocator,
        temp_allocator: mem_allocator,
        nodes: []const *ink.node,
    ) ![]const *ink.node {
        var map = std.AutoHashMap(usize, *ink.node).init(temp_allocator);
        defer map.deinit();

        const cloner = struct {
            const clone_error = error{OutOfMemory};
            node_allocator: mem_allocator,
            map: *std.AutoHashMap(usize, *ink.node),

            fn clone_node(self: *@This(), node: *const ink.node) clone_error!*ink.node {
                const addr = @intFromPtr(node);
                if (self.map.get(addr)) |existing| return existing;
                const out = try self.node_allocator.create(ink.node);
                try self.map.put(addr, out);
                out.* = try self.clone_any(node.*);
                return out;
            }

            fn clone_any(self: *@This(), value: anytype) clone_error!@TypeOf(value) {
                const T = @TypeOf(value);
                if (T == ink.ast.node_ref) {
                    const cloned = try self.clone_node(ink.ast.deref(value));
                    return ink.ast.ref(cloned);
                }
                switch (@typeInfo(T)) {
                    .optional => |_| {
                        if (value) |payload| {
                            return try self.clone_any(payload);
                        }
                        return null;
                    },
                    .pointer => |ptr| {
                        if (ptr.size == .slice) {
                            if (ptr.child == u8) {
                                return value;
                            }
                            const out = try self.node_allocator.alloc(ptr.child, value.len);
                            for (value, 0..) |item, idx| {
                                out[idx] = try self.clone_any(item);
                            }
                            return out;
                        }
                        return value;
                    },
                    .@"struct" => |info| {
                        var out: T = undefined;
                        inline for (info.fields) |field| {
                            const field_value = @field(value, field.name);
                            @field(out, field.name) = try self.clone_any(field_value);
                        }
                        return out;
                    },
                    .@"union" => |info| {
                        if (info.tag_type == null) return value;
                        switch (value) {
                            inline else => |payload, tag| {
                                return @unionInit(T, @tagName(tag), try self.clone_any(payload));
                            },
                        }
                    },
                    .array => |info| {
                        var out: T = undefined;
                        for (value, 0..) |item, idx| {
                            out[idx] = try self.clone_any(item);
                        }
                        _ = info;
                        return out;
                    },
                    else => return value,
                }
            }
        };

        var out_nodes = array_list(*ink.node).init(node_allocator);
        var ctx = cloner{ .node_allocator = node_allocator, .map = &map };
        for (nodes) |node| {
            try out_nodes.append(try ctx.clone_node(node));
        }
        return out_nodes.toOwnedSlice();
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
        checks: ?sandbox_mod.Checks,
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
                var qualified = fname;
                var allocated = false;
                if (std.mem.indexOf(u8, fname, "::") == null) {
                    qualified = try qualify_name(allocator, alias, fname);
                    allocated = true;
                }
                if (!foreign_set.contains(qualified)) {
                    try foreign_set.put(qualified, {});
                    try foreign_names.append(qualified);
                    if (allocated) try foreign_allocated.append(qualified);
                } else if (allocated) {
                    allocator.free(qualified);
                }
            }
        }

        var node_arena = arena_allocator.init(allocator);
        defer node_arena.deinit();

        const cloned_nodes = try clone_ast_nodes(node_arena.allocator(), allocator, nodes);
        var desugared = desugar.desugar(
            node_arena.allocator(),
            allocator,
            cloned_nodes,
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
        if (checks) |active| {
            if (active.uir) {
                const ok = try validate_mod.validate_uir(uir_result, diags);
                if (!ok) return null;
            }
        }

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
        if (checks) |active| {
            if (active.mir) {
                const ok = try validate_mod.validate_mir(mir_result, diags);
                if (!ok) {
                    for (foreigns) |name| allocator.free(name);
                    allocator.free(foreigns);
                    return null;
                }
            }
        }

        var lower_info = lir_lower.error_info{};
        defer lower_info.deinit(allocator);

        var signatures = array_list(ink.lir_vm_lower.function_signature).init(allocator);
        errdefer deinit_signatures(allocator, &signatures);

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
        if (checks) |active| {
            if (active.lir) {
                const ok = try validate_mod.validate_vm_program(allocator, program, foreigns.len, diags);
                if (!ok) {
                    allocator.free(program.instructions);
                    allocator.free(program.constants);
                    for (foreigns) |name| allocator.free(name);
                    allocator.free(foreigns);
                    deinit_signatures(allocator, &signatures);
                    return null;
                }
            }
        }

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
