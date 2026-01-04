const std = @import("std");
const src = @import("source.zig");
const diag = @import("diagnostic.zig");
const resol = @import("resolver.zig");
const ink = @import("root.zig");
const lang_spec = @import("lang/spec.zig");
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

        for (req.sources) |compsrc| {
            if (sources_by_id.contains(compsrc.id)) {
                try diags.append(.{ .danger = .@"error", .message = "duplicate source id", .span = null });
            } else {
                try sources_by_id.put(compsrc.id, compsrc);
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
                allocator.free(module_imports[i]);
                if (module_import_specs[i]) |imports| allocator.free(imports);
                allocator.free(module_foreigns[i]);
                module_arenas[i].deinit();
                module_exports[i].deinit();
            }
            allocator.free(module_files);
            allocator.free(module_nodes);
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

        for (modules.items, 0..) |mod, mi| {
            var files = array_list(ast_file).init(allocator);
            errdefer files.deinit();

            var nodes = array_list(*ink.node).init(allocator);
            errdefer nodes.deinit();

            for (mod.sources) |sid| {
                const compsrc = sources_by_id.get(sid).?;

                const tokens = try lex_all(allocator, compsrc.text, &diags);
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
                            .code = "E1001",
                        });
                    } else {
                        try diags.append(.{ .danger = .@"error", .message = "parse error", .span = null, .code = "E1001" });
                    }
                    allocator.free(tokens);
                    parse.deinit();
                    continue;
                }

                var builder = ink.peg_ast.builder.init(parse.arena.allocator(), tokens, &parse.tree);
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
                            .code = "E1002",
                        });
                        allocator.free(tokens);
                        parse.deinit();
                        continue;
                    }
                    try diags.append(.{ .danger = .@"error", .message = "ast error", .span = null, .code = "E1002" });
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

                for (file_nodes) |n| try nodes.append(n);
            }

            module_files[mi] = try files.toOwnedSlice();

            const raw_nodes = try nodes.toOwnedSlice();
            var desugar_result = try desugar.desugar(module_arenas[mi].allocator(), allocator, raw_nodes, &diags);
            allocator.free(raw_nodes);

            var imports = array_list(resolver.module_import).init(allocator);
            var import_aliases = string_map(void).init(allocator);
            defer import_aliases.deinit();
            for (desugar_result.imports) |imp| {
                const module_name = imp.module.string;
                const alias = if (imp.item != null) module_name else if (imp.alias) |a| a.string else module_name;
                if (module_names.get(module_name)) |dep_id| {
                    if (!import_aliases.contains(alias)) {
                        try import_aliases.put(alias, {});
                        try imports.append(.{ .id = dep_id, .alias = alias });
                    }
                } else {
                    try diags.append(.{
                        .danger = .@"error",
                        .message = "unknown module import",
                        .span = .{ .start = imp.module.where.start, .end = imp.module.where.end },
                    });
                }
            }

            module_imports[mi] = try imports.toOwnedSlice();
            module_import_specs[mi] = desugar_result.imports;
            desugar_result.origin.deinit();
            module_nodes[mi] = desugar_result.nodes;
            try collect_exports(&module_exports[mi], module_nodes[mi]);

            var foreign_counts = string_map(usize).init(allocator);
            defer foreign_counts.deinit();
            for (module_nodes[mi]) |node| {
                if (node.* != .decl) continue;
                switch (node.decl) {
                    .function => |f| {
                        if (!has_attribute(f.attributes, "foreign")) continue;
                        const entry = try foreign_counts.getOrPut(f.name.string);
                        if (!entry.found_existing) entry.value_ptr.* = 0;
                        entry.value_ptr.* += 1;
                    },
                    else => {},
                }
            }

            var foreign_seen = string_map(void).init(allocator);
            defer foreign_seen.deinit();
            var foreigns = array_list([]const u8).init(allocator);
            for (module_nodes[mi]) |node| {
                if (node.* != .decl) continue;
                switch (node.decl) {
                    .function => |f| {
                        if (!has_attribute(f.attributes, "foreign")) continue;
                        const base = f.name.string;
                        if (!foreign_seen.contains(base)) {
                            try foreign_seen.put(base, {});
                            try foreigns.append(base);
                        }
                        if (foreign_counts.get(base).? > 1) {
                            const mangled = try mangle_foreign_name(module_arenas[mi].allocator(), base, f.params);
                            try foreigns.append(mangled);
                        }
                    },
                    else => {},
                }
            }
            module_foreigns[mi] = try foreigns.toOwnedSlice();
            foreigns.deinit();
        }

        for (module_import_specs) |maybe_imports| {
            if (maybe_imports) |imports| {
                for (imports) |imp| {
                    if (imp.item) |item| {
                        if (module_names.get(imp.module.string)) |dep_id| {
                            const dep_idx: usize = @intCast(dep_id);
                            if (!module_exports[dep_idx].contains(item.string)) {
                                try diags.append(.{
                                    .danger = .@"error",
                                    .message = "unknown import item",
                                    .span = .{ .start = item.where.start, .end = item.where.end },
                                });
                            }
                        }
                    }
                }
            }
        }

        if (has_error(diags.items)) {
            return finish(&diags, &diag_messages, null, null, null, null, null, false);
        }

        // 5) name resolution
        var res = resolver.init(allocator, null, &diag_messages);
        defer res.deinit();

        for (modules.items, 0..) |mod, mi| {
            try res.add_module(mod.id, module_imports[mi], module_nodes[mi], &diags);
        }
        for (modules.items, 0..) |mod, mi| {
            try res.resolve_module(mod.id, module_nodes[mi], &diags);
        }

        if (has_error(diags.items)) {
            return finish(&diags, &diag_messages, null, null, null, null, null, false);
        }

        // 6) IR build (root module only)
        const root_id = module_names.get(req.root_module) orelse {
            try diags.append(.{ .danger = .@"error", .message = "unknown root module", .span = null });
            return finish(&diags, &diag_messages, null, null, null, null, null, false);
        };
        const root_nodes = module_nodes[@intCast(root_id)];

        var ir_arena = arena_allocator.init(allocator);
        defer ir_arena.deinit();
        var ir_builder = ink.ir_build.builder.init(ir_arena.allocator());
        defer ir_builder.deinit();
        const ir_result = ir_builder.build(root_nodes) catch {
            try diags.append(.{ .danger = .@"error", .message = "ir build error", .span = null });
            return finish(&diags, &diag_messages, null, null, null, null, null, false);
        };

        // 7) codegen + encode
        const data_entries = try build_data_entries(allocator, ir_result.strings);
        errdefer {
            for (data_entries) |entry| allocator.free(entry.bytes);
            allocator.free(data_entries);
        }
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

        try add_intrinsic_foreigns(&foreign_set, &foreign_names, ir_result.nodes, ir_result.strings);

        var foreigns = try allocator.alloc([]const u8, foreign_names.items.len);
        errdefer {
            for (foreigns) |name| allocator.free(name);
            allocator.free(foreigns);
        }
        for (foreign_names.items, 0..) |name, idx| {
            foreigns[idx] = try allocator.dupe(u8, name);
        }

        const program = try ink.ir_codegen.generate(
            allocator,
            ir_result.nodes,
            ir_result.strings,
            ir_result.roots,
            foreigns,
        );
        const bytecode = try ink.vm.encode.encode(allocator, program.instructions);

        return finish(
            &diags,
            &diag_messages,
            program.instructions,
            program.constants,
            data_entries,
            bytecode,
            foreigns,
            true,
        );
    }

    fn lex_all(allocator: mem_allocator, source_text: []const u8, diags: *array_list(diagnostic)) ![]const ink.token {
        var lexer = try ink.lexer.init(source_text);
        var tokens = array_list(ink.token).init(allocator);
        errdefer tokens.deinit();

        while (true) {
            const maybe_tok = lexer.next() catch {
                try diags.append(.{ .danger = .@"error", .message = "lexer error", .span = null, .code = "E1000" });
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
        nodes: []const ink.ir.ir,
        strings: []const []const u8,
    ) !void {
        var need_alloc = false;
        var need_free = false;
        var need_deref = false;
        var need_store = false;
        var need_borrow = false;
        var need_result_ok = false;
        var need_result_err = false;
        var need_result_is_ok = false;
        var need_result_unwrap = false;
        var need_result_unwrap_err = false;
        var need_try = false;
        var need_slice_ptr = false;
        var need_string_new = false;
        var need_string_concat = false;
        var need_string_from_int = false;
        var need_string_from_float = false;
        var need_string_from_bool = false;

        for (nodes) |node| {
            if (node == .record_literal) {
                need_alloc = true;
                need_store = true;
            } else if (node == .binary and node.binary.op == .access) {
                need_deref = true;
            } else if (node == .binary and node.binary.op == .index) {
                need_deref = true;
                need_slice_ptr = true;
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
                } else if (std.mem.eql(u8, name, "borrow") or std.mem.eql(u8, name, "borrow_mut")) {
                    need_borrow = true;
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
            } else if (node == .unary and node.unary.op == .@"try") {
                need_try = true;
            }
        }

        if (need_alloc) try add_foreign_name(foreign_set, foreign_names, "std::alloc");
        if (need_free) try add_foreign_name(foreign_set, foreign_names, "std::free");
        if (need_deref) try add_foreign_name(foreign_set, foreign_names, "std::deref");
        if (need_store) try add_foreign_name(foreign_set, foreign_names, "std::store");
        if (need_borrow) {
            try add_foreign_name(foreign_set, foreign_names, "std::alloc");
            try add_foreign_name(foreign_set, foreign_names, "std::store");
            try add_foreign_name(foreign_set, foreign_names, "std::ptr_of");
        }
        if (need_result_ok) try add_foreign_name(foreign_set, foreign_names, "std::result_ok");
        if (need_result_err) try add_foreign_name(foreign_set, foreign_names, "std::result_err");
        if (need_result_is_ok) try add_foreign_name(foreign_set, foreign_names, "std::result_is_ok");
        if (need_result_unwrap) try add_foreign_name(foreign_set, foreign_names, "std::result_unwrap");
        if (need_result_unwrap_err) try add_foreign_name(foreign_set, foreign_names, "std::result_unwrap_err");
        if (need_slice_ptr) try add_foreign_name(foreign_set, foreign_names, "std::slice_ptr");
        if (need_string_new) try add_foreign_name(foreign_set, foreign_names, "std::string_new");
        if (need_string_concat) try add_foreign_name(foreign_set, foreign_names, "std::string_concat");
        if (need_string_from_int) try add_foreign_name(foreign_set, foreign_names, "std::string_from_int");
        if (need_string_from_float) try add_foreign_name(foreign_set, foreign_names, "std::string_from_float");
        if (need_string_from_bool) try add_foreign_name(foreign_set, foreign_names, "std::string_from_bool");
        if (need_try) {
            try add_foreign_name(foreign_set, foreign_names, "std::result_is_ok");
            try add_foreign_name(foreign_set, foreign_names, "std::result_unwrap");
        }
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

    fn build_data_entries(
        allocator: mem_allocator,
        strings: []const []const u8,
    ) ![]const ink.vm.inkb.data_entry {
        const entries = try allocator.alloc(ink.vm.inkb.data_entry, strings.len);
        errdefer allocator.free(entries);
        var idx: usize = 0;
        errdefer {
            var i: usize = 0;
            while (i < idx) : (i += 1) {
                allocator.free(entries[i].bytes);
            }
        }
        for (strings, 0..) |value, i| {
            const duped = try allocator.dupe(u8, value);
            entries[i] = .{ .kind = .string, .bytes = duped };
            idx = i + 1;
        }
        return entries;
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
        }

        return buf.toOwnedSlice(allocator);
    }
};
