const std = @import("std");
const ink = @import("ink");
const lang_spec = ink.lang_spec;
const source = ink.source;
const unicode = std.unicode;

const mem_allocator = std.mem.Allocator;
const string_map = std.StringArrayHashMap;

const direction = enum { client_to_server, server_to_client };

const position = struct {
    line: usize,
    character: usize,
};

const semantic_token_type = enum(u32) {
    namespace,
    type,
    function,
    variable,
    property,
    keyword,
    number,
    string,
    operator,
    boolean,
};

const semantic_tokens_legend_json =
    "{\"tokenTypes\":[\"namespace\",\"type\",\"function\",\"variable\",\"property\",\"keyword\",\"number\",\"string\",\"operator\",\"boolean\"],\"tokenModifiers\":[]}";

const completion_item = struct {
    label: []const u8,
    kind: u8,
};

const inlay_hint = struct {
    position: position,
    label: []const u8,
    owned: bool,
    kind: u8,
    padding_left: bool = false,
    padding_right: bool = true,
};

const completion_kind_keyword: u8 = 14;
const completion_kind_function: u8 = 3;
const completion_kind_variable: u8 = 6;
const completion_kind_type: u8 = 7;
const completion_kind_constant: u8 = 21;
const completion_kind_field: u8 = 5;
const completion_kind_module: u8 = 9;
const completion_kind_enum_member: u8 = 20;
const completion_kind_type_param: u8 = 25;
const completion_kind_value: u8 = 12;
const completion_kind_unit: u8 = 11;

const inlay_hint_kind_type: u8 = 1;
const inlay_hint_kind_parameter: u8 = 2;

const semantic_mode = enum { full, augment };

const server_state = struct {
    semantic_mode: semantic_mode = .full,
    target: ink.target.target_spec = .{ .kind = .vm },
};

const ast_cache = struct {
    tokens: []const ink.token,
    parse: ink.peg_parser.parse_result,
    nodes: ?[]const *ink.node = null,
    ast_error: ?ink.peg_ast.error_info = null,

    pub fn deinit(self: *ast_cache, allocator: mem_allocator) void {
        allocator.free(self.tokens);
        self.parse.deinit();
    }
};

const document_entry = struct {
    text: []const u8,
    ast: ?ast_cache = null,

    pub fn deinit(self: *document_entry, allocator: mem_allocator) void {
        allocator.free(self.text);
        if (self.ast) |*cache| cache.deinit(allocator);
    }
};

const document_store = struct {
    allocator: mem_allocator,
    docs: string_map(document_entry),

    pub fn init(allocator: mem_allocator) document_store {
        return .{ .allocator = allocator, .docs = string_map(document_entry).init(allocator) };
    }

    pub fn deinit(self: *document_store) void {
        var it = self.docs.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            entry.value_ptr.*.deinit(self.allocator);
        }
        self.docs.deinit();
    }

    pub fn put(self: *document_store, uri: []const u8, text: []const u8) !void {
        if (self.docs.fetchSwapRemove(uri)) |kv| {
            self.allocator.free(kv.key);
            var existing = kv.value;
            existing.deinit(self.allocator);
        }

        const uri_copy = try self.allocator.dupe(u8, uri);
        errdefer self.allocator.free(uri_copy);
        const text_copy = try self.allocator.dupe(u8, text);
        errdefer self.allocator.free(text_copy);

        try self.docs.put(uri_copy, .{ .text = text_copy });
    }

    pub fn get_text(self: *document_store, uri: []const u8) ?[]const u8 {
        if (self.docs.get(uri)) |entry| return entry.text;
        return null;
    }

    pub fn get_entry(self: *document_store, uri: []const u8) ?*document_entry {
        return self.docs.getPtr(uri);
    }

    pub fn set_ast(self: *document_store, uri: []const u8, cache: ast_cache) void {
        if (self.docs.getPtr(uri)) |entry| {
            if (entry.ast) |*old| old.deinit(self.allocator);
            entry.ast = cache;
        } else {
            var orphan = cache;
            orphan.deinit(self.allocator);
        }
    }

    pub fn remove(self: *document_store, uri: []const u8) void {
        if (self.docs.fetchSwapRemove(uri)) |kv| {
            self.allocator.free(kv.key);
            var existing = kv.value;
            existing.deinit(self.allocator);
        }
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const cli_opts = parse_cli_options(args);
    const log_path = cli_opts.log_path orelse "/tmp/inkd.lsp.log";

    var log_file = try std.fs.cwd().createFile(log_path, .{ .truncate = false, .read = true });
    defer log_file.close();
    try log_file.seekFromEnd(0);

    var docs = document_store.init(allocator);
    defer docs.deinit();

    var readerbuf = [_]u8{0} ** 8192;
    var writerbuf = [_]u8{0} ** 8192;
    var logbuf = [_]u8{0} ** 8192;
    var stdin_reader = std.fs.File.stdin().reader(&readerbuf);
    var stdout_writer = std.fs.File.stdout().writer(&writerbuf);
    var log_writer = log_file.writer(&logbuf);
    const io_reader = &stdin_reader.interface;
    const io_writer = &stdout_writer.interface;
    const io_log_writer = &log_writer.interface;

    var state = server_state{ .target = cli_opts.target };

    while (true) {
        const body_opt = try read_message(allocator, io_reader);
        if (body_opt == null) break;

        const body = body_opt.?;
        defer allocator.free(body);

        try log_message(&log_file, io_log_writer, .client_to_server, body);

        const keep_running = try handle_message(allocator, &docs, &state, &log_file, io_log_writer, io_writer, body);
        if (!keep_running) break;
    }
}

fn handle_message(
    allocator: mem_allocator,
    docs: *document_store,
    state: *server_state,
    log_file: *std.fs.File,
    log_writer: *std.Io.Writer,
    writer: *std.Io.Writer,
    body: []const u8,
) !bool {
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, body, .{}) catch return true;
    defer parsed.deinit();

    const root = parsed.value;
    const method = get_string_field(root, "method");

    if (method) |name| {
        if (std.mem.eql(u8, name, "initialize")) {
            if (get_field(root, "params")) |params| {
                state.semantic_mode = detect_semantic_mode(params);
            }
            const id_value = get_field(root, "id") orelse return true;
            const response_body = try build_initialize_response(allocator, id_value);
            defer allocator.free(response_body);

            try log_message(log_file, log_writer, .server_to_client, response_body);
            try write_message(writer, response_body);
            try writer.flush();
            return true;
        }
        if (std.mem.eql(u8, name, "exit")) {
            return false;
        } else if (std.mem.eql(u8, name, "textDocument/didOpen")) {
            if (get_field(root, "params")) |params| {
                try handle_did_open(allocator, docs, state, log_file, log_writer, writer, params);
            }
            return true;
        } else if (std.mem.eql(u8, name, "textDocument/didChange")) {
            if (get_field(root, "params")) |params| {
                try handle_did_change(allocator, docs, state, log_file, log_writer, writer, params);
            }
            return true;
        } else if (std.mem.eql(u8, name, "textDocument/didClose")) {
            if (get_field(root, "params")) |params| {
                try handle_did_close(allocator, docs, log_file, log_writer, writer, params);
            }
            return true;
        }
    }

    const id_value = get_field(root, "id") orelse return true;
    switch (id_value) {
        .null => return true,
        else => {},
    }

    if (method) |name| {
        if (std.mem.eql(u8, name, "textDocument/semanticTokens/full")) {
            if (get_field(root, "params")) |params| {
                const response_body = try build_semantic_tokens_response(allocator, docs, id_value, params, state.semantic_mode);
                defer allocator.free(response_body);

                try log_message(log_file, log_writer, .server_to_client, response_body);
                try write_message(writer, response_body);
                try writer.flush();
            }
            return true;
        } else if (std.mem.eql(u8, name, "textDocument/completion")) {
            if (get_field(root, "params")) |params| {
                const response_body = try build_completion_response(allocator, docs, id_value, params);
                defer allocator.free(response_body);

                try log_message(log_file, log_writer, .server_to_client, response_body);
                try write_message(writer, response_body);
                try writer.flush();
            }
            return true;
        } else if (std.mem.eql(u8, name, "textDocument/hover")) {
            if (get_field(root, "params")) |params| {
                const response_body = try build_hover_response(allocator, docs, id_value, params);
                defer allocator.free(response_body);

                try log_message(log_file, log_writer, .server_to_client, response_body);
                try write_message(writer, response_body);
                try writer.flush();
            }
            return true;
        } else if (std.mem.eql(u8, name, "textDocument/inlayHint")) {
            if (get_field(root, "params")) |params| {
                const response_body = try build_inlay_hint_response(allocator, docs, id_value, params);
                defer allocator.free(response_body);

                try log_message(log_file, log_writer, .server_to_client, response_body);
                try write_message(writer, response_body);
                try writer.flush();
            }
            return true;
        } else if (std.mem.eql(u8, name, "textDocument/signatureHelp")) {
            if (get_field(root, "params")) |params| {
                const response_body = try build_signature_help_response(allocator, docs, id_value, params);
                defer allocator.free(response_body);

                try log_message(log_file, log_writer, .server_to_client, response_body);
                try write_message(writer, response_body);
                try writer.flush();
            }
            return true;
        }
    }

    const response_body = try build_response_body(allocator, id_value, method);
    defer allocator.free(response_body);

    try log_message(log_file, log_writer, .server_to_client, response_body);
    try write_message(writer, response_body);
    try writer.flush();

    return true;
}

fn build_response_body(
    allocator: mem_allocator,
    id: std.json.Value,
    method: ?[]const u8,
) ![]u8 {
    var allocating = std.Io.Writer.Allocating.init(allocator);
    errdefer allocating.deinit();

    const writer = &allocating.writer;
    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try std.json.Stringify.value(id, .{}, writer);
    try writer.writeAll(",\"result\":");

    if (method) |name| {
        if (std.mem.eql(u8, name, "initialize")) {
            try writer.writeAll("{\"capabilities\":{\"textDocumentSync\":1,\"semanticTokensProvider\":{\"legend\":");
            try writer.writeAll(semantic_tokens_legend_json);
            try writer.writeAll(",\"full\":true},\"completionProvider\":{\"triggerCharacters\":[\".\",\":\"]},\"hoverProvider\":true,\"inlayHintProvider\":true,\"signatureHelpProvider\":{\"triggerCharacters\":[\"(\",\",\"]}}}}");
        } else if (std.mem.eql(u8, name, "workspace/configuration")) {
            try writer.writeAll("[]");
        } else {
            try writer.writeAll("null");
        }
    } else {
        try writer.writeAll("null");
    }

    try writer.writeAll("}");
    const result = try allocating.toOwnedSlice();
    allocating.deinit();
    return result;
}

fn build_initialize_response(
    allocator: mem_allocator,
    id: std.json.Value,
) ![]u8 {
    var allocating = std.Io.Writer.Allocating.init(allocator);
    errdefer allocating.deinit();

    const writer = &allocating.writer;
    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try std.json.Stringify.value(id, .{}, writer);
    try writer.writeAll(",\"result\":{\"capabilities\":{\"textDocumentSync\":1,");
    try writer.writeAll("\"semanticTokensProvider\":{\"legend\":");
    try writer.writeAll(semantic_tokens_legend_json);
    try writer.writeAll(",\"full\":true},");
    try writer.writeAll("\"completionProvider\":{\"triggerCharacters\":[\".\",\":\"]},\"hoverProvider\":true,\"inlayHintProvider\":true,\"signatureHelpProvider\":{\"triggerCharacters\":[\"(\",\",\"]}}}}");

    const result = try allocating.toOwnedSlice();
    allocating.deinit();
    return result;
}

fn detect_semantic_mode(params: std.json.Value) semantic_mode {
    const caps = get_field(params, "capabilities") orelse return .full;
    const text_doc = get_field(caps, "textDocument") orelse return .full;
    const semantic = get_field(text_doc, "semanticTokens") orelse return .full;
    if (get_bool_field(semantic, "augmentsSyntaxTokens") orelse false) return .augment;
    return .full;
}

fn handle_did_open(
    allocator: mem_allocator,
    docs: *document_store,
    state: *server_state,
    log_file: *std.fs.File,
    log_writer: *std.Io.Writer,
    writer: *std.Io.Writer,
    params: std.json.Value,
) !void {
    const text_doc = get_field(params, "textDocument") orelse return;
    const uri = get_string_field(text_doc, "uri") orelse return;
    const text = get_string_field(text_doc, "text") orelse return;

    try docs.put(uri, text);
    try compile_and_publish(allocator, docs, state, log_file, log_writer, writer, uri);
}

fn handle_did_change(
    allocator: mem_allocator,
    docs: *document_store,
    state: *server_state,
    log_file: *std.fs.File,
    log_writer: *std.Io.Writer,
    writer: *std.Io.Writer,
    params: std.json.Value,
) !void {
    const text_doc = get_field(params, "textDocument") orelse return;
    const uri = get_string_field(text_doc, "uri") orelse return;

    const changes_val = get_field(params, "contentChanges") orelse return;
    const changes = switch (changes_val) {
        .array => |arr| arr,
        else => return,
    };
    if (changes.items.len == 0) return;

    const change = changes.items[0];
    const text = get_string_field(change, "text") orelse return;

    try docs.put(uri, text);
    try compile_and_publish(allocator, docs, state, log_file, log_writer, writer, uri);
}

fn handle_did_close(
    allocator: mem_allocator,
    docs: *document_store,
    log_file: *std.fs.File,
    log_writer: *std.Io.Writer,
    writer: *std.Io.Writer,
    params: std.json.Value,
) !void {
    const text_doc = get_field(params, "textDocument") orelse return;
    const uri = get_string_field(text_doc, "uri") orelse return;

    docs.remove(uri);

    const empty_diags: []const ink.diagnostic = &[_]ink.diagnostic{};
    const body = try build_publish_diagnostics(allocator, uri, "", null, empty_diags);
    defer allocator.free(body);

    try log_message(log_file, log_writer, .server_to_client, body);
    try write_message(writer, body);
    try writer.flush();
}

fn compile_and_publish(
    allocator: mem_allocator,
    docs: *document_store,
    state: *server_state,
    log_file: *std.fs.File,
    log_writer: *std.Io.Writer,
    writer: *std.Io.Writer,
    uri: []const u8,
) !void {
    const entry = docs.get_entry(uri) orelse return;
    const text = entry.text;

    const cache = try build_ast_cache(allocator, text);
    docs.set_ast(uri, cache);

    if (!cache.parse.ok or cache.ast_error != null or cache.nodes == null) {
        var diags = std.array_list.Managed(ink.diagnostic).init(allocator);
        defer diags.deinit();

        var messages = std.array_list.Managed([]const u8).init(allocator);
        defer {
            for (messages.items) |msg| allocator.free(msg);
            messages.deinit();
        }

        if (!cache.parse.ok) {
            if (cache.parse.@"error") |info| {
                const msg = try format_parse_error(allocator, info, cache.tokens);
                try messages.append(msg);
                try diags.append(.{
                    .danger = .@"error",
                    .message = msg,
                    .span = span_from_token_index(cache.tokens, info.position),
                    .code = "E1001",
                });
            } else {
                try diags.append(.{ .danger = .@"error", .message = "parse error", .span = null, .code = "E1001" });
            }
        } else if (cache.ast_error) |info| {
            const msg = try format_ast_error(allocator, info, cache.tokens);
            try messages.append(msg);
            try diags.append(.{
                .danger = .@"error",
                .message = msg,
                .span = span_from_token_index(cache.tokens, info.position),
                .code = "E1002",
            });
        } else {
            try diags.append(.{ .danger = .@"error", .message = "ast error", .span = null, .code = "E1002" });
        }

        const body = try build_publish_diagnostics(allocator, uri, text, null, diags.items);
        defer allocator.free(body);

        try log_message(log_file, log_writer, .server_to_client, body);
        try write_message(writer, body);
        try writer.flush();
        return;
    }

    var sources = std.array_list.Managed(ink.compiler.source).init(allocator);
    defer {
        for (sources.items) |src| {
            if (src.id != 0) allocator.free(src.text);
        }
        sources.deinit();
    }

    var allocated_paths = std.array_list.Managed([]const u8).init(allocator);
    defer {
        for (allocated_paths.items) |path| {
            allocator.free(path);
        }
        allocated_paths.deinit();
    }

    var module_source_slices = std.array_list.Managed([]ink.compiler.source_id).init(allocator);
    defer {
        for (module_source_slices.items) |slice| {
            allocator.free(slice);
        }
        module_source_slices.deinit();
    }

    var next_source_id: ink.compiler.source_id = 0;
    const main_id = next_source_id;
    next_source_id += 1;

    const file_path = try path_from_uri(allocator, uri);
    defer if (file_path != null) allocator.free(file_path.?);
    const main_path = file_path orelse uri;
    try sources.append(.{ .id = main_id, .path = main_path, .text = text });

    const main_sources = try allocator.alloc(ink.compiler.source_id, 1);
    main_sources[0] = main_id;
    try module_source_slices.append(main_sources);

    var std_sources: []ink.compiler.source_id = &.{};
    if (file_path) |path| {
        if (try find_std_dir(allocator, path)) |std_dir| {
            defer allocator.free(std_dir);
            std_sources = try load_module_sources(allocator, std_dir, &next_source_id, &sources, &allocated_paths);
            if (std_sources.len != 0) {
                try module_source_slices.append(std_sources);
            } else {
                allocator.free(std_sources);
                std_sources = &.{};
            }
        }
    }

    const empty_deps = &[_][]const u8{};
    var modules = std.array_list.Managed(ink.compiler.module_spec).init(allocator);
    defer modules.deinit();
    try modules.append(.{ .name = "main", .sources = main_sources, .deps = empty_deps });
    if (std_sources.len != 0) {
        try modules.append(.{ .name = "std", .sources = std_sources, .deps = empty_deps });
    }

    const req = ink.compiler.compile_request{
        .sources = sources.items,
        .modules = modules.items,
        .root_module = "main",
        .target = state.target,
    };

    var result = ink.compiler.compile(allocator, req) catch |err| {
        var diags = std.array_list.Managed(ink.diagnostic).init(allocator);
        defer diags.deinit();

        var msg_buf: [128]u8 = undefined;
        const msg = std.fmt.bufPrint(&msg_buf, "compiler error: {s}", .{@errorName(err)}) catch "compiler error";
        try diags.append(.{
            .danger = .@"error",
            .message = msg,
            .span = null,
            .code = "E0000",
            .source_id = main_id,
        });

        const body = try build_publish_diagnostics(allocator, uri, text, main_id, diags.items);
        defer allocator.free(body);

        try log_message(log_file, log_writer, .server_to_client, body);
        try write_message(writer, body);
        try writer.flush();
        return;
    };
    defer result.deinit(allocator);

    const body = try build_publish_diagnostics(allocator, uri, text, main_id, result.diagnostics);
    defer allocator.free(body);

    try log_message(log_file, log_writer, .server_to_client, body);
    try write_message(writer, body);
    try writer.flush();
}

const token_buffer = struct {
    tokens: []const ink.token,
    owned: bool,
    nodes: ?[]const *ink.node = null,
};

fn build_semantic_tokens_response(
    allocator: mem_allocator,
    docs: *document_store,
    id: std.json.Value,
    params: std.json.Value,
    mode: semantic_mode,
) ![]u8 {
    const text_doc = get_field(params, "textDocument") orelse return build_semantic_tokens_empty(allocator, id);
    const uri = get_string_field(text_doc, "uri") orelse return build_semantic_tokens_empty(allocator, id);
    const entry = docs.get_entry(uri) orelse return build_semantic_tokens_empty(allocator, id);
    const text = entry.text;

    const tokens_info = try tokens_for_semantic(allocator, entry, text);
    defer if (tokens_info.owned) allocator.free(tokens_info.tokens);

    const data = try build_semantic_tokens_data(allocator, text, tokens_info.tokens, tokens_info.nodes, mode == .augment);
    defer allocator.free(data);

    var allocating = std.Io.Writer.Allocating.init(allocator);
    errdefer allocating.deinit();
    const writer = &allocating.writer;

    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try std.json.Stringify.value(id, .{}, writer);
    try writer.writeAll(",\"result\":{\"data\":[");
    for (data, 0..) |value, i| {
        if (i != 0) try writer.writeAll(",");
        try writer.print("{d}", .{value});
    }
    try writer.writeAll("]}}");

    const result = try allocating.toOwnedSlice();
    allocating.deinit();
    return result;
}

fn build_semantic_tokens_empty(allocator: mem_allocator, id: std.json.Value) ![]u8 {
    var allocating = std.Io.Writer.Allocating.init(allocator);
    errdefer allocating.deinit();
    const writer = &allocating.writer;

    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try std.json.Stringify.value(id, .{}, writer);
    try writer.writeAll(",\"result\":{\"data\":[]}}");

    const result = try allocating.toOwnedSlice();
    allocating.deinit();
    return result;
}

fn build_completion_response(
    allocator: mem_allocator,
    docs: *document_store,
    id: std.json.Value,
    params: std.json.Value,
) ![]u8 {
    const text_doc = get_field(params, "textDocument") orelse return build_completion_empty(allocator, id);
    const uri = get_string_field(text_doc, "uri") orelse return build_completion_empty(allocator, id);
    const position_value = get_field(params, "position") orelse return build_completion_empty(allocator, id);
    const pos = parse_position(position_value) orelse return build_completion_empty(allocator, id);
    const entry = docs.get_entry(uri) orelse return build_completion_empty(allocator, id);
    const text = entry.text;

    const line_offsets = try build_line_offsets(allocator, text);
    defer allocator.free(line_offsets);
    const offset = offset_from_position(text, line_offsets, pos);
    const prefix = identifier_prefix(text, offset) orelse "";

    var items = std.array_list.Managed(completion_item).init(allocator);
    defer items.deinit();

    try collect_keyword_completions(&items, prefix);
    try collect_builtin_completions(&items, prefix);
    if (entry.ast) |cache| {
        if (cache.nodes) |nodes| {
            for (nodes) |node| {
                try collect_completion_node(&items, node, prefix);
            }
        }
    }

    var allocating = std.Io.Writer.Allocating.init(allocator);
    errdefer allocating.deinit();
    const writer = &allocating.writer;

    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try std.json.Stringify.value(id, .{}, writer);
    try writer.writeAll(",\"result\":{\"isIncomplete\":false,\"items\":[");
    for (items.items, 0..) |item, i| {
        if (i != 0) try writer.writeAll(",");
        try writer.writeAll("{\"label\":");
        try std.json.Stringify.value(item.label, .{}, writer);
        try writer.writeAll(",\"kind\":");
        try writer.print("{d}", .{item.kind});
        try writer.writeAll("}");
    }
    try writer.writeAll("]}}");

    const result = try allocating.toOwnedSlice();
    allocating.deinit();
    return result;
}

fn build_completion_empty(allocator: mem_allocator, id: std.json.Value) ![]u8 {
    var allocating = std.Io.Writer.Allocating.init(allocator);
    errdefer allocating.deinit();
    const writer = &allocating.writer;

    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try std.json.Stringify.value(id, .{}, writer);
    try writer.writeAll(",\"result\":{\"isIncomplete\":false,\"items\":[]}}");

    const result = try allocating.toOwnedSlice();
    allocating.deinit();
    return result;
}

fn build_hover_response(
    allocator: mem_allocator,
    docs: *document_store,
    id: std.json.Value,
    params: std.json.Value,
) ![]u8 {
    const text_doc = get_field(params, "textDocument") orelse return build_hover_empty(allocator, id);
    const uri = get_string_field(text_doc, "uri") orelse return build_hover_empty(allocator, id);
    const position_value = get_field(params, "position") orelse return build_hover_empty(allocator, id);
    const pos = parse_position(position_value) orelse return build_hover_empty(allocator, id);
    const entry = docs.get_entry(uri) orelse return build_hover_empty(allocator, id);
    const text = entry.text;

    const line_offsets = try build_line_offsets(allocator, text);
    defer allocator.free(line_offsets);
    const offset = offset_from_position(text, line_offsets, pos);
    const span = identifier_span_at(text, offset) orelse return build_hover_empty(allocator, id);
    const name = text[span.start..span.end];

    var hover_buf = std.array_list.Managed(u8).init(allocator);
    errdefer hover_buf.deinit();
    const hover_writer = hover_buf.writer();
    try hover_writer.writeAll("```ink\n");

    const info = if (entry.ast) |cache|
        if (cache.nodes) |nodes|
            find_hover_info(nodes, name)
        else
            null
    else
        null;

    if (info) |found| {
        try write_hover_info(hover_writer, found);
    } else if (builtin_hover_label(name)) |label| {
        try hover_writer.writeAll(label);
    } else {
        hover_buf.deinit();
        return build_hover_empty(allocator, id);
    }

    try hover_writer.writeAll("\n```");
    const hover_text = try hover_buf.toOwnedSlice();
    hover_buf.deinit();
    defer allocator.free(hover_text);

    var allocating = std.Io.Writer.Allocating.init(allocator);
    errdefer allocating.deinit();
    const writer = &allocating.writer;

    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try std.json.Stringify.value(id, .{}, writer);
    try writer.writeAll(",\"result\":{\"contents\":{\"kind\":\"markdown\",\"value\":");
    try std.json.Stringify.value(hover_text, .{}, writer);
    try writer.writeAll("}}}");

    const result = try allocating.toOwnedSlice();
    allocating.deinit();
    return result;
}

fn build_hover_empty(allocator: mem_allocator, id: std.json.Value) ![]u8 {
    var allocating = std.Io.Writer.Allocating.init(allocator);
    errdefer allocating.deinit();
    const writer = &allocating.writer;

    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try std.json.Stringify.value(id, .{}, writer);
    try writer.writeAll(",\"result\":null}");

    const result = try allocating.toOwnedSlice();
    allocating.deinit();
    return result;
}

fn build_inlay_hint_response(
    allocator: mem_allocator,
    docs: *document_store,
    id: std.json.Value,
    params: std.json.Value,
) ![]u8 {
    const text_doc = get_field(params, "textDocument") orelse return build_inlay_hint_empty(allocator, id);
    const uri = get_string_field(text_doc, "uri") orelse return build_inlay_hint_empty(allocator, id);
    const entry = docs.get_entry(uri) orelse return build_inlay_hint_empty(allocator, id);
    const text = entry.text;

    const range_value = get_field(params, "range");
    var range_start: ?position = null;
    var range_end: ?position = null;
    if (range_value) |val| {
        if (parse_range(val)) |req_range| {
            range_start = req_range.start;
            range_end = req_range.end;
        }
    }

    var hints = std.array_list.Managed(inlay_hint).init(allocator);
    defer {
        for (hints.items) |hint| {
            if (hint.owned) allocator.free(hint.label);
        }
        hints.deinit();
    }

    if (entry.ast) |cache| {
        if (cache.nodes) |nodes| {
            try collect_inlay_hints(allocator, &hints, nodes, text, range_start, range_end);
        }
    }

    var allocating = std.Io.Writer.Allocating.init(allocator);
    errdefer allocating.deinit();
    const writer = &allocating.writer;

    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try std.json.Stringify.value(id, .{}, writer);
    try writer.writeAll(",\"result\":[");
    for (hints.items, 0..) |hint, i| {
        if (i != 0) try writer.writeAll(",");
        try writer.writeAll("{\"position\":{\"line\":");
        try writer.print("{d}", .{hint.position.line});
        try writer.writeAll(",\"character\":");
        try writer.print("{d}", .{hint.position.character});
        try writer.writeAll("},\"label\":");
        try std.json.Stringify.value(hint.label, .{}, writer);
        try writer.writeAll(",\"kind\":");
        try writer.print("{d}", .{hint.kind});
        if (hint.padding_left) {
            try writer.writeAll(",\"paddingLeft\":true");
        }
        if (hint.padding_right) {
            try writer.writeAll(",\"paddingRight\":true");
        }
        try writer.writeAll("}");
    }
    try writer.writeAll("]}");

    const result = try allocating.toOwnedSlice();
    allocating.deinit();
    return result;
}

fn build_inlay_hint_empty(allocator: mem_allocator, id: std.json.Value) ![]u8 {
    var allocating = std.Io.Writer.Allocating.init(allocator);
    errdefer allocating.deinit();
    const writer = &allocating.writer;

    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try std.json.Stringify.value(id, .{}, writer);
    try writer.writeAll(",\"result\":[]}");

    const result = try allocating.toOwnedSlice();
    allocating.deinit();
    return result;
}

fn build_signature_help_response(
    allocator: mem_allocator,
    docs: *document_store,
    id: std.json.Value,
    params: std.json.Value,
) ![]u8 {
    const text_doc = get_field(params, "textDocument") orelse return build_signature_help_empty(allocator, id);
    const uri = get_string_field(text_doc, "uri") orelse return build_signature_help_empty(allocator, id);
    const position_value = get_field(params, "position") orelse return build_signature_help_empty(allocator, id);
    const pos = parse_position(position_value) orelse return build_signature_help_empty(allocator, id);
    const entry = docs.get_entry(uri) orelse return build_signature_help_empty(allocator, id);
    const text = entry.text;

    const cache = entry.ast orelse return build_signature_help_empty(allocator, id);
    const line_offsets = try build_line_offsets(allocator, text);
    defer allocator.free(line_offsets);
    const offset = offset_from_position(text, line_offsets, pos);
    const context = find_signature_context(cache.tokens, offset) orelse return build_signature_help_empty(allocator, id);

    var sigs = std.array_list.Managed(function_sig).init(allocator);
    defer sigs.deinit();
    if (cache.nodes) |nodes| {
        for (nodes) |node| try collect_function_sigs(&sigs, node);
    }
    const sig = find_function_sig(sigs.items, context.name) orelse return build_signature_help_empty(allocator, id);

    var label_buf = std.array_list.Managed(u8).init(allocator);
    defer label_buf.deinit();
    const skip_self = context.is_method and sig.params.len > 0 and is_receiver_param_name(sig.params[0].name.string);
    try write_signature_label(&label_buf, sig, skip_self);
    const label = try label_buf.toOwnedSlice();
    defer allocator.free(label);

    const param_start: usize = if (skip_self) 1 else 0;
    const param_count = if (sig.params.len >= param_start) sig.params.len - param_start else 0;
    var active_param: ?usize = null;
    if (param_count > 0) {
        active_param = if (context.arg_index >= param_count) param_count - 1 else context.arg_index;
    }

    var allocating = std.Io.Writer.Allocating.init(allocator);
    errdefer allocating.deinit();
    const writer = &allocating.writer;

    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try std.json.Stringify.value(id, .{}, writer);
    try writer.writeAll(",\"result\":{\"signatures\":[{\"label\":");
    try std.json.Stringify.value(label, .{}, writer);
    try writer.writeAll(",\"parameters\":[");
    if (param_count > 0) {
        for (sig.params[param_start..], 0..) |param, idx| {
            if (idx != 0) try writer.writeAll(",");
            try writer.writeAll("{\"label\":");
            try std.json.Stringify.value(param.name.string, .{}, writer);
            try writer.writeAll("}");
        }
    }
    try writer.writeAll("]}],\"activeSignature\":0");
    if (active_param) |idx| {
        try writer.writeAll(",\"activeParameter\":");
        try writer.print("{d}", .{idx});
    }
    try writer.writeAll("}}");

    const result = try allocating.toOwnedSlice();
    allocating.deinit();
    return result;
}

fn build_signature_help_empty(allocator: mem_allocator, id: std.json.Value) ![]u8 {
    var allocating = std.Io.Writer.Allocating.init(allocator);
    errdefer allocating.deinit();
    const writer = &allocating.writer;

    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    try std.json.Stringify.value(id, .{}, writer);
    try writer.writeAll(",\"result\":null}");

    const result = try allocating.toOwnedSlice();
    allocating.deinit();
    return result;
}

fn tokens_for_semantic(
    allocator: mem_allocator,
    entry: *document_entry,
    text: []const u8,
) !token_buffer {
    if (entry.ast) |cache| {
        return .{ .tokens = cache.tokens, .owned = false, .nodes = cache.nodes };
    }
    const tokens = try lex_all(allocator, text);
    return .{ .tokens = tokens, .owned = true, .nodes = null };
}

fn build_semantic_tokens_data(
    allocator: mem_allocator,
    text: []const u8,
    tokens: []const ink.token,
    nodes: ?[]const *ink.node,
    augment_only: bool,
) ![]u32 {
    var data = std.array_list.Managed(u32).init(allocator);
    errdefer data.deinit();

    const line_offsets = try build_line_offsets(allocator, text);
    defer allocator.free(line_offsets);

    var spans = std.array_list.Managed(semantic_emit_span).init(allocator);
    defer spans.deinit();

    var collector = semantic_collector{
        .spans = &spans,
        .text_len = text.len,
    };

    var semantic_map: ?std.AutoHashMap(usize, semantic_span) = null;
    if (nodes) |ast_nodes| {
        var map = std.AutoHashMap(usize, semantic_span).init(allocator);
        errdefer map.deinit();
        try collect_semantic_spans(&map, ast_nodes);
        semantic_map = map;
    }
    defer if (semantic_map) |*map| map.deinit();
    const semantic_map_ptr = if (semantic_map) |*map| map else null;
    const has_semantic_map = (semantic_map_ptr != null);

    var prev_kind: ?ink.token.kind = null;

    for (tokens, 0..) |tok, idx| {
        if (tok.which == .end_of_file or tok.which == .new_line or tok.which == .indent or tok.which == .dedent) {
            continue;
        }

        if (tok.which == .string) {
            if (try emit_interpolated_string_tokens(&collector, tok, augment_only)) {
                if (is_significant_token(tok.which)) prev_kind = tok.which;
                continue;
            }
        }

        const next_kind = next_significant_kind(tokens, idx + 1);
        var used_semantic_map = false;
        const type_index = blk: {
            if (tok.which == .identifier and semantic_map_ptr != null) {
                if (semantic_map_ptr.?.get(tok.where.start)) |entry| {
                    if (entry.end == tok.where.end) {
                        used_semantic_map = true;
                        break :blk entry.kind;
                    }
                }
            }
            break :blk semantic_token_type_for(tok.which, prev_kind, next_kind) orelse {
                if (is_significant_token(tok.which)) prev_kind = tok.which;
                continue;
            };
        };

        if (is_significant_token(tok.which)) prev_kind = tok.which;

        if (augment_only and has_semantic_map and tok.which == .identifier and type_index == .variable and !used_semantic_map) {
            continue;
        }

        try collector.emit(tok.where.start, tok.where.end, type_index);
    }

    std.sort.heap(semantic_emit_span, spans.items, {}, semantic_emit_span_less_than);

    var emitter = semantic_emitter{
        .data = &data,
        .text = text,
        .line_offsets = line_offsets,
    };
    var prev_end: usize = 0;
    var have_prev = false;
    for (spans.items) |span| {
        if (have_prev and span.start < prev_end) continue;
        try emitter.emit(span.start, span.end, span.kind);
        prev_end = span.end;
        have_prev = true;
    }

    return data.toOwnedSlice();
}

const semantic_emit_span = struct {
    start: usize,
    end: usize,
    kind: semantic_token_type,
};

fn semantic_emit_span_less_than(_: void, lhs: semantic_emit_span, rhs: semantic_emit_span) bool {
    if (lhs.start != rhs.start) return lhs.start < rhs.start;
    if (lhs.end != rhs.end) return lhs.end < rhs.end;
    return @intFromEnum(lhs.kind) < @intFromEnum(rhs.kind);
}

const semantic_collector = struct {
    spans: *std.array_list.Managed(semantic_emit_span),
    text_len: usize,

    fn emit(self: *semantic_collector, start_raw: usize, end_raw: usize, kind: semantic_token_type) semantic_error!void {
        const start = @min(start_raw, self.text_len);
        const end = @min(end_raw, self.text_len);
        if (end <= start) return;
        try self.spans.append(.{ .start = start, .end = end, .kind = kind });
    }
};

const semantic_emitter = struct {
    data: *std.array_list.Managed(u32),
    text: []const u8,
    line_offsets: []const usize,
    line_idx: usize = 0,
    prev_line: usize = 0,
    prev_char: usize = 0,
    first: bool = true,

    fn emit(self: *semantic_emitter, start_raw: usize, end_raw: usize, kind: semantic_token_type) semantic_error!void {
        const start = @min(start_raw, self.text.len);
        const end = @min(end_raw, self.text.len);
        if (end <= start) return;

        while (self.line_idx + 1 < self.line_offsets.len and start >= self.line_offsets[self.line_idx + 1]) {
            self.line_idx += 1;
        }

        const line_start = self.line_offsets[self.line_idx];
        const line_end = if (self.line_idx + 1 < self.line_offsets.len) self.line_offsets[self.line_idx + 1] else self.text.len;
        const line = self.line_idx;
        const token_end = @min(end, line_end);
        if (token_end <= start) return;

        const col = utf16_len(self.text[line_start..start]);
        const length = utf16_len(self.text[start..token_end]);

        const delta_line = if (self.first) line else line - self.prev_line;
        const delta_start = if (self.first or delta_line != 0) col else col - self.prev_char;

        try self.data.append(@intCast(delta_line));
        try self.data.append(@intCast(delta_start));
        try self.data.append(@intCast(length));
        try self.data.append(@intCast(@intFromEnum(kind)));
        try self.data.append(0);

        self.prev_line = line;
        self.prev_char = col;
        self.first = false;
    }
};

fn emit_interpolated_string_tokens(
    emitter: anytype,
    tok: ink.token,
    augment_only: bool,
) semantic_error!bool {
    _ = augment_only;
    const content = tok.what.string;
    if (!has_interpolation_marker(content)) return false;

    var i: usize = 0;
    var literal_start: usize = 0;
    while (i < content.len) {
        const ch = content[i];
        if (ch == '{') {
            if (i + 1 < content.len and content[i + 1] == '{') {
                i += 2;
                continue;
            }

            try emit_string_segment(emitter, tok.where.start, literal_start, i);
            try emitter.emit(tok.where.start + i, tok.where.start + i + 1, .operator);

            const end = find_interpolation_end(content, i + 1) orelse {
                try emit_string_segment(emitter, tok.where.start, i, content.len);
                return true;
            };

            try emit_inline_expr_tokens(emitter, tok.where.start + i + 1, content[i + 1 .. end]);
            try emitter.emit(tok.where.start + end, tok.where.start + end + 1, .operator);

            i = end + 1;
            literal_start = i;
            continue;
        }
        if (ch == '}' and i + 1 < content.len and content[i + 1] == '}') {
            i += 2;
            continue;
        }
        i += 1;
    }

    try emit_string_segment(emitter, tok.where.start, literal_start, content.len);
    return true;
}

fn emit_string_segment(
    emitter: anytype,
    base: usize,
    start: usize,
    end: usize,
) semantic_error!void {
    if (end <= start) return;
    try emitter.emit(base + start, base + end, .string);
}

fn emit_inline_expr_tokens(
    emitter: anytype,
    base: usize,
    expr: []const u8,
) semantic_error!void {
    var lexer = ink.lexer.init(expr) catch return;
    var prev_kind: ?ink.token.kind = null;
    var current = next_significant_token(&lexer) orelse return;
    var next = next_significant_token(&lexer);

    while (true) {
        const next_kind = if (next) |next_tok| next_tok.which else null;
        const kind = semantic_token_type_for(current.which, prev_kind, next_kind) orelse {
            if (is_significant_token(current.which)) prev_kind = current.which;
            if (next) |next_tok| {
                current = next_tok;
                next = next_significant_token(&lexer);
                continue;
            }
            break;
        };
        if (is_significant_token(current.which)) prev_kind = current.which;
        try emitter.emit(base + current.where.start, base + current.where.end, kind);

        if (next) |next_tok| {
            current = next_tok;
            next = next_significant_token(&lexer);
        } else {
            break;
        }
    }
}

fn has_interpolation_marker(text: []const u8) bool {
    var i: usize = 0;
    while (i < text.len) : (i += 1) {
        if (text[i] == '{') {
            if (i + 1 < text.len and text[i + 1] == '{') {
                i += 1;
                continue;
            }
            return true;
        }
    }
    return false;
}

fn find_interpolation_end(text: []const u8, start: usize) ?usize {
    var i = start;
    var in_string = false;
    var escaped = false;
    var depth: usize = 0;
    while (i < text.len) : (i += 1) {
        const ch = text[i];
        if (escaped) {
            escaped = false;
            continue;
        }
        if (ch == '\\') {
            escaped = true;
            continue;
        }
        if (ch == '"') {
            in_string = !in_string;
            continue;
        }
        if (!in_string) {
            if (ch == '{') {
                depth += 1;
                continue;
            }
            if (ch == '}') {
                if (depth == 0) return i;
                depth -= 1;
            }
        }
    }
    return null;
}

const semantic_span = struct {
    start: usize,
    end: usize,
    kind: semantic_token_type,
    priority: u8,
};

const semantic_error = mem_allocator.Error;

fn semantic_priority(kind: semantic_token_type) u8 {
    return switch (kind) {
        .function => 5,
        .type => 4,
        .namespace => 3,
        .property => 2,
        .variable => 1,
        else => 0,
    };
}

fn record_ident(map: *std.AutoHashMap(usize, semantic_span), ident: ink.identifier, kind: semantic_token_type) semantic_error!void {
    if (ident.where.end <= ident.where.start) return;
    const priority = semantic_priority(kind);
    if (map.get(ident.where.start)) |existing| {
        if (existing.priority >= priority) return;
    }
    try map.put(ident.where.start, .{
        .start = ident.where.start,
        .end = ident.where.end,
        .kind = kind,
        .priority = priority,
    });
}

fn collect_semantic_spans(map: *std.AutoHashMap(usize, semantic_span), nodes: []const *ink.node) semantic_error!void {
    for (nodes) |node| try collect_node_spans(map, node);
}

fn collect_node_spans(map: *std.AutoHashMap(usize, semantic_span), node: *const ink.node) semantic_error!void {
    switch (node.*) {
        .integer, .float, .duration, .string => {},
        .identifier => {},
        .unary => |un| try collect_node_spans(map, ink.ast.deref(un.right)),
        .binary => |bin| {
            if (bin.op == .call) {
                try mark_call_target(map, ink.ast.deref(bin.left));
            } else if (bin.op == .access) {
                const right = ink.ast.deref(bin.right);
                if (right.* == .identifier) try record_ident(map, right.identifier, .property);
            } else if (bin.op == .scope_access) {
                const left = ink.ast.deref(bin.left);
                const right = ink.ast.deref(bin.right);
                if (left.* == .identifier) try record_ident(map, left.identifier, .namespace);
                if (right.* == .identifier) try record_ident(map, right.identifier, .namespace);
            }
            try collect_node_spans(map, ink.ast.deref(bin.left));
            try collect_node_spans(map, ink.ast.deref(bin.right));
        },
        .block => |blk| {
            for (blk.items) |ref| try collect_node_spans(map, ink.ast.deref(ref));
        },
        .if_expr => |ife| {
            try collect_node_spans(map, ink.ast.deref(ife.condition));
            try collect_node_spans(map, ink.ast.deref(ife.then_branch));
            if (ife.else_branch) |ref| try collect_node_spans(map, ink.ast.deref(ref));
        },
        .match_expr => |me| {
            try collect_node_spans(map, ink.ast.deref(me.target));
            for (me.arms) |arm| {
                try collect_node_spans(map, ink.ast.deref(arm.pattern));
                try collect_node_spans(map, ink.ast.deref(arm.body));
            }
        },
        .select_expr => |se| {
            for (se.arms) |arm| {
                if (arm.name) |name| try record_ident(map, name, .variable);
                try collect_node_spans(map, ink.ast.deref(arm.task));
                try collect_node_spans(map, ink.ast.deref(arm.body));
            }
        },
        .with_expr => |we| {
            try record_ident(map, we.name, .variable);
            try collect_node_spans(map, ink.ast.deref(we.body));
        },
        .label_expr => |label_expr| {
            try record_ident(map, label_expr.name, .variable);
            try collect_node_spans(map, ink.ast.deref(label_expr.body));
        },
        .loop_expr => |loop_expr| {
            try collect_node_spans(map, ink.ast.deref(loop_expr.body));
        },
        .while_expr => |while_expr| {
            try collect_node_spans(map, ink.ast.deref(while_expr.condition));
            try collect_node_spans(map, ink.ast.deref(while_expr.body));
        },
        .while_in_expr => |while_in_expr| {
            try collect_node_spans(map, ink.ast.deref(while_in_expr.pattern));
            try collect_node_spans(map, ink.ast.deref(while_in_expr.iter));
            try collect_node_spans(map, ink.ast.deref(while_in_expr.body));
        },
        .until_expr => |until_expr| {
            try collect_node_spans(map, ink.ast.deref(until_expr.condition));
            try collect_node_spans(map, ink.ast.deref(until_expr.body));
        },
        .repeat_expr => |repeat_expr| {
            try collect_node_spans(map, ink.ast.deref(repeat_expr.count));
            try collect_node_spans(map, ink.ast.deref(repeat_expr.body));
        },
        .for_expr => |for_expr| {
            try collect_node_spans(map, ink.ast.deref(for_expr.pattern));
            try collect_node_spans(map, ink.ast.deref(for_expr.iter));
            try collect_node_spans(map, ink.ast.deref(for_expr.body));
        },
        .each_expr => |each_expr| {
            try collect_node_spans(map, ink.ast.deref(each_expr.pattern));
            try collect_node_spans(map, ink.ast.deref(each_expr.iter));
            try collect_node_spans(map, ink.ast.deref(each_expr.body));
        },
        .break_expr => |break_expr| {
            if (break_expr.label) |label| try record_ident(map, label, .variable);
            if (break_expr.value) |ref| try collect_node_spans(map, ink.ast.deref(ref));
        },
        .continue_expr => |continue_expr| {
            if (continue_expr.label) |label| try record_ident(map, label, .variable);
        },
        .yield_expr => |yield_expr| {
            if (yield_expr.value) |ref| try collect_node_spans(map, ink.ast.deref(ref));
        },
        .atomic_expr => |atomic_expr| {
            try collect_node_spans(map, ink.ast.deref(atomic_expr.value));
            try record_ident(map, atomic_expr.ordering, .variable);
        },
        .record => |rec| {
            for (rec.items) |assoc| {
                try record_ident(map, assoc.name, .property);
                if (assoc.value) |ref| try collect_node_spans(map, ink.ast.deref(ref));
            }
        },
        .intrinsic => |call| {
            try record_ident(map, call.name, .operator);
            for (call.args) |ref| try collect_node_spans(map, ink.ast.deref(ref));
        },
        .associate => |assoc| {
            try record_ident(map, assoc.name, .property);
            if (assoc.value) |ref| try collect_node_spans(map, ink.ast.deref(ref));
        },
        .type => |ty| try collect_type_spans(map, ty),
        .decl => |decl| try collect_decl_spans(map, decl),
    }
}

fn collect_decl_spans(map: *std.AutoHashMap(usize, semantic_span), decl: ink.ast.decl) semantic_error!void {
    switch (decl) {
        .function => |func| {
            try record_ident(map, func.name, .function);
            for (func.generics) |param| {
                if (param.constraint) |ref| try collect_node_spans(map, ink.ast.deref(ref));
                if (param.default) |ref| try collect_node_spans(map, ink.ast.deref(ref));
            }
            for (func.params) |param| try collect_node_spans(map, ink.ast.deref(param.ty));
            for (func.where_clause) |req| try collect_node_spans(map, ink.ast.deref(req.constraint));
            if (func.return_type) |ref| try collect_node_spans(map, ink.ast.deref(ref));
            if (func.body) |ref| try collect_node_spans(map, ink.ast.deref(ref));
        },
        .@"struct" => |st| {
            try record_ident(map, st.name, .type);
            for (st.generics) |param| {
                if (param.constraint) |ref| try collect_node_spans(map, ink.ast.deref(ref));
                if (param.default) |ref| try collect_node_spans(map, ink.ast.deref(ref));
            }
            for (st.fields) |field| try collect_node_spans(map, ink.ast.deref(field.ty));
        },
        .trait => |tr| {
            try record_ident(map, tr.name, .type);
            for (tr.generics) |param| {
                if (param.constraint) |ref| try collect_node_spans(map, ink.ast.deref(ref));
                if (param.default) |ref| try collect_node_spans(map, ink.ast.deref(ref));
            }
            for (tr.items) |item| {
                switch (item) {
                    .function => |func| try collect_decl_spans(map, .{ .function = func }),
                    .assoc_type => |assoc| {
                        try record_ident(map, assoc.name, .type);
                        if (assoc.value) |ref| try collect_node_spans(map, ink.ast.deref(ref));
                    },
                }
            }
            for (tr.requires) |ref| try collect_node_spans(map, ink.ast.deref(ref));
        },
        .@"enum" => |e| {
            try record_ident(map, e.name, .type);
            for (e.generics) |param| {
                if (param.constraint) |ref| try collect_node_spans(map, ink.ast.deref(ref));
                if (param.default) |ref| try collect_node_spans(map, ink.ast.deref(ref));
            }
            for (e.variants) |variant| {
                try record_ident(map, variant.name, .type);
                if (variant.payload) |ref| try collect_node_spans(map, ink.ast.deref(ref));
            }
        },
        .impl => |im| {
            try record_ident(map, im.by_trait, .type);
            try record_ident(map, im.for_struct, .type);
            for (im.functions) |func| try collect_decl_spans(map, .{ .function = func });
        },
        .import => |imp| {
            try record_ident(map, imp.module, .namespace);
            if (imp.item) |item| try record_ident(map, item, .namespace);
            if (imp.alias) |alias| try record_ident(map, alias, .namespace);
        },
        .type_alias => |t| {
            try record_ident(map, t.name, .type);
            for (t.generics) |param| {
                if (param.constraint) |ref| try collect_node_spans(map, ink.ast.deref(ref));
                if (param.default) |ref| try collect_node_spans(map, ink.ast.deref(ref));
            }
            try collect_node_spans(map, ink.ast.deref(t.value));
        },
        .@"const" => |c| {
            try record_ident(map, c.name, .variable);
            if (c.ty) |ref| try collect_node_spans(map, ink.ast.deref(ref));
            try collect_node_spans(map, ink.ast.deref(c.value));
        },
        .@"var" => |v| {
            try record_ident(map, v.name, .variable);
            if (v.ty) |ref| try collect_node_spans(map, ink.ast.deref(ref));
            try collect_node_spans(map, ink.ast.deref(v.value));
        },
    }
}

fn collect_type_spans(map: *std.AutoHashMap(usize, semantic_span), ty: ink.ast.type_expr) semantic_error!void {
    switch (ty) {
        .self => {},
        .name => |id| try record_ident(map, id, .type),
        .optional => |ref| try collect_node_spans(map, ink.ast.deref(ref)),
        .dyn => |ref| try collect_node_spans(map, ink.ast.deref(ref)),
        .applied => |ap| {
            try record_ident(map, ap.base, .type);
            for (ap.args) |arg_ref| try collect_node_spans(map, ink.ast.deref(arg_ref));
        },
    }
}

fn mark_call_target(map: *std.AutoHashMap(usize, semantic_span), node: *const ink.node) semantic_error!void {
    switch (node.*) {
        .identifier => |id| try record_ident(map, id, .function),
        .binary => |bin| {
            if (bin.op == .scope_access) {
                const left = ink.ast.deref(bin.left);
                const right = ink.ast.deref(bin.right);
                if (left.* == .identifier) try record_ident(map, left.identifier, .namespace);
                if (right.* == .identifier) try record_ident(map, right.identifier, .function);
            } else if (bin.op == .access) {
                const right = ink.ast.deref(bin.right);
                if (right.* == .identifier) try record_ident(map, right.identifier, .function);
            }
        },
        else => {},
    }
}

fn build_line_offsets(allocator: mem_allocator, text: []const u8) ![]usize {
    var offsets = std.array_list.Managed(usize).init(allocator);
    errdefer offsets.deinit();
    try offsets.append(0);
    for (text, 0..) |ch, idx| {
        if (ch == '\n') {
            if (idx + 1 <= text.len) {
                try offsets.append(idx + 1);
            }
        }
    }
    return offsets.toOwnedSlice();
}

fn is_significant_token(kind: ink.token.kind) bool {
    return switch (kind) {
        .end_of_file, .new_line, .indent, .dedent => false,
        else => true,
    };
}

fn semantic_token_type_for(
    kind: ink.token.kind,
    prev: ?ink.token.kind,
    next: ?ink.token.kind,
) ?semantic_token_type {
    switch (kind) {
        .identifier => {
            if (next) |next_kind| {
                if (next_kind == .paren_left) return .function;
            }
            if (prev) |prev_kind| {
                return switch (prev_kind) {
                    .function => .function,
                    .@"struct", .trait, .@"enum", .type, .dyn => .type,
                    .import, .from => .namespace,
                    .dot, .question_dot => .property,
                    .double_colon => .namespace,
                    .colon, .arrow, .as => .type,
                    .at_sign => .operator,
                    else => .variable,
                };
            }
            return .variable;
        },
        .number => return .number,
        .string => return .string,
        .logical_true, .logical_false => return .boolean,
        .function, .constant, .variable, .mut, .expr_if, .expr_else, .expr_match, .expr_select, .case, .detached, .stmt_return, .spawn, .await, .@"try", .logical_or, .logical_and, .logical_xor, .logical_not, .in, .trait, .impl, .as, .import, .from, .dynamic, .@"for", .@"struct", .where, .@"comptime", .self, .this, .type, .@"enum", .requires, .dyn => return .keyword,
        .plus, .minus, .asterisk, .slash, .assign, .pipe, .coalesce, .double_colon, .dot, .question_dot, .less_than, .greater_than, .less_or_equal, .greater_or_equal, .equal, .not_equal, .arrow, .paren_left, .paren_right, .bracket_left, .bracket_right, .comma, .colon, .bar, .ampersand, .question, .range, .range_inclusive, .ellipsis, .at_sign, .hash => return .operator,
        else => return null,
    }
}

fn next_significant_kind(tokens: []const ink.token, start: usize) ?ink.token.kind {
    var i = start;
    while (i < tokens.len) : (i += 1) {
        const kind = tokens[i].which;
        if (kind == .end_of_file) return null;
        if (!is_significant_token(kind)) continue;
        return kind;
    }
    return null;
}

fn next_significant_token(lexer: *ink.lexer) ?ink.token {
    while (true) {
        const maybe_tok = lexer.next() catch return null;
        if (maybe_tok) |tok| {
            if (!is_significant_token(tok.which)) {
                if (tok.which == .end_of_file) return null;
                continue;
            }
            return tok;
        }
        return null;
    }
}

const hover_info = union(enum) {
    function: ink.ast.function_decl,
    @"const": ink.ast.const_decl,
    @"var": ink.ast.var_decl,
    variable: ink.identifier,
    @"struct": ink.ast.struct_decl,
    trait: ink.ast.trait_decl,
    enum_variant: enum_variant_info,
    @"enum": ink.ast.enum_decl,
    type_alias: ink.ast.type_decl,
    assoc_type: ink.ast.associated_type_decl,
    param: param_info,
    field: field_info,
    generic_param: ink.ast.generic_param,
    import: ink.ast.import_decl,
};

const enum_variant_info = struct {
    parent: []const u8,
    variant: ink.ast.sum_variant,
};

const param_info = struct {
    name: []const u8,
    ty: ink.ast.node_ref,
};

const field_info = struct {
    name: []const u8,
    ty: ink.ast.node_ref,
};

const function_sig = struct {
    name: []const u8,
    params: []const ink.ast.param,
    return_type: ?ink.ast.node_ref,
    is_foreign: bool,
};

const call_info = struct {
    callee: *const ink.node,
    args: []const *ink.node,
};

const signature_context = struct {
    name: []const u8,
    arg_index: usize,
    is_method: bool,
};

const type_label = struct {
    text: []const u8,
    owned: bool,
};

const variant_info = struct {
    name: []const u8,
    parent: []const u8,
    kind: variant_kind,
};

const variant_kind = enum {
    @"enum",
};

fn collect_keyword_completions(items: *std.array_list.Managed(completion_item), prefix: []const u8) !void {
    inline for (lang_spec.keyword_lexemes) |lex| {
        try push_completion(items, lex.text, completion_kind_keyword, prefix);
    }
}

fn collect_builtin_completions(items: *std.array_list.Managed(completion_item), prefix: []const u8) !void {
    const builtin_types = [_][]const u8{
        "int",
        "uint",
        "float",
        "bool",
        "string",
        "none",
        "union",
        "intersect",
        "tuple",
        "fn",
        "slice",
        "array",
    };
    const builtin_values = [_][]const u8{
        "unit",
        "true",
        "false",
    };

    for (builtin_types) |name| {
        try push_completion(items, name, completion_kind_type, prefix);
    }
    for (builtin_values) |name| {
        const kind = if (std.mem.eql(u8, name, "unit")) completion_kind_unit else completion_kind_value;
        try push_completion(items, name, kind, prefix);
    }
}

fn collect_completion_node(
    items: *std.array_list.Managed(completion_item),
    node: *const ink.node,
    prefix: []const u8,
) !void {
    switch (node.*) {
        .decl => |decl| switch (decl) {
            .function => |func| {
                try push_completion(items, func.name.string, completion_kind_function, prefix);
                for (func.generics) |param| {
                    try push_completion(items, param.name.string, completion_kind_type_param, prefix);
                }
                for (func.params) |param| {
                    try push_completion(items, param.name.string, completion_kind_variable, prefix);
                }
                if (func.body) |ref| try collect_completion_node(items, ink.ast.deref(ref), prefix);
            },
            .@"struct" => |st| {
                try push_completion(items, st.name.string, completion_kind_type, prefix);
                for (st.generics) |param| {
                    try push_completion(items, param.name.string, completion_kind_type_param, prefix);
                }
                for (st.fields) |field| {
                    try push_completion(items, field.name.string, completion_kind_field, prefix);
                }
            },
            .trait => |tr| {
                try push_completion(items, tr.name.string, completion_kind_type, prefix);
                for (tr.generics) |param| {
                    try push_completion(items, param.name.string, completion_kind_type_param, prefix);
                }
                for (tr.items) |item| {
                    switch (item) {
                        .function => |func| try push_completion(items, func.name.string, completion_kind_function, prefix),
                        .assoc_type => |assoc| try push_completion(items, assoc.name.string, completion_kind_type, prefix),
                    }
                }
                for (tr.requires) |ref| try collect_completion_node(items, ink.ast.deref(ref), prefix);
            },
            .@"enum" => |e| {
                try push_completion(items, e.name.string, completion_kind_type, prefix);
                for (e.generics) |param| {
                    try push_completion(items, param.name.string, completion_kind_type_param, prefix);
                }
                for (e.variants) |variant| {
                    try push_completion(items, variant.name.string, completion_kind_enum_member, prefix);
                }
            },
            .impl => |im| {
                for (im.functions) |func| {
                    try push_completion(items, func.name.string, completion_kind_function, prefix);
                }
            },
            .import => |imp| {
                try push_completion(items, imp.module.string, completion_kind_module, prefix);
                if (imp.item) |item| try push_completion(items, item.string, completion_kind_module, prefix);
                if (imp.alias) |alias| try push_completion(items, alias.string, completion_kind_module, prefix);
            },
            .type_alias => |t| {
                try push_completion(items, t.name.string, completion_kind_type, prefix);
                for (t.generics) |param| {
                    try push_completion(items, param.name.string, completion_kind_type_param, prefix);
                }
                try collect_completion_node(items, ink.ast.deref(t.value), prefix);
            },
            .@"const" => |c| {
                try push_completion(items, c.name.string, completion_kind_constant, prefix);
                try collect_completion_node(items, ink.ast.deref(c.value), prefix);
            },
            .@"var" => |v| {
                try push_completion(items, v.name.string, completion_kind_variable, prefix);
                try collect_completion_node(items, ink.ast.deref(v.value), prefix);
            },
        },
        .block => |blk| {
            for (blk.items) |ref| try collect_completion_node(items, ink.ast.deref(ref), prefix);
        },
        .if_expr => |ife| {
            try collect_completion_node(items, ink.ast.deref(ife.condition), prefix);
            try collect_completion_node(items, ink.ast.deref(ife.then_branch), prefix);
            if (ife.else_branch) |ref| try collect_completion_node(items, ink.ast.deref(ref), prefix);
        },
        .match_expr => |me| {
            try collect_completion_node(items, ink.ast.deref(me.target), prefix);
            for (me.arms) |arm| {
                try collect_completion_node(items, ink.ast.deref(arm.pattern), prefix);
                try collect_completion_node(items, ink.ast.deref(arm.body), prefix);
            }
        },
        .select_expr => |se| {
            for (se.arms) |arm| {
                if (arm.name) |name| try push_completion(items, name.string, completion_kind_variable, prefix);
                try collect_completion_node(items, ink.ast.deref(arm.task), prefix);
                try collect_completion_node(items, ink.ast.deref(arm.body), prefix);
            }
        },
        .label_expr => |label_expr| {
            try push_completion(items, label_expr.name.string, completion_kind_variable, prefix);
            try collect_completion_node(items, ink.ast.deref(label_expr.body), prefix);
        },
        .loop_expr => |loop_expr| {
            try collect_completion_node(items, ink.ast.deref(loop_expr.body), prefix);
        },
        .while_expr => |while_expr| {
            try collect_completion_node(items, ink.ast.deref(while_expr.condition), prefix);
            try collect_completion_node(items, ink.ast.deref(while_expr.body), prefix);
        },
        .while_in_expr => |while_in_expr| {
            try collect_completion_node(items, ink.ast.deref(while_in_expr.pattern), prefix);
            try collect_completion_node(items, ink.ast.deref(while_in_expr.iter), prefix);
            try collect_completion_node(items, ink.ast.deref(while_in_expr.body), prefix);
        },
        .until_expr => |until_expr| {
            try collect_completion_node(items, ink.ast.deref(until_expr.condition), prefix);
            try collect_completion_node(items, ink.ast.deref(until_expr.body), prefix);
        },
        .repeat_expr => |repeat_expr| {
            try collect_completion_node(items, ink.ast.deref(repeat_expr.count), prefix);
            try collect_completion_node(items, ink.ast.deref(repeat_expr.body), prefix);
        },
        .for_expr => |for_expr| {
            try collect_completion_node(items, ink.ast.deref(for_expr.pattern), prefix);
            try collect_completion_node(items, ink.ast.deref(for_expr.iter), prefix);
            try collect_completion_node(items, ink.ast.deref(for_expr.body), prefix);
        },
        .each_expr => |each_expr| {
            try collect_completion_node(items, ink.ast.deref(each_expr.pattern), prefix);
            try collect_completion_node(items, ink.ast.deref(each_expr.iter), prefix);
            try collect_completion_node(items, ink.ast.deref(each_expr.body), prefix);
        },
        .break_expr => |break_expr| {
            if (break_expr.value) |ref| try collect_completion_node(items, ink.ast.deref(ref), prefix);
        },
        .continue_expr => {},
        .yield_expr => |yield_expr| {
            if (yield_expr.value) |ref| try collect_completion_node(items, ink.ast.deref(ref), prefix);
        },
        .atomic_expr => |atomic_expr| {
            try collect_completion_node(items, ink.ast.deref(atomic_expr.value), prefix);
        },
        .record => |rec| {
            for (rec.items) |assoc| {
                try push_completion(items, assoc.name.string, completion_kind_field, prefix);
                if (assoc.value) |ref| try collect_completion_node(items, ink.ast.deref(ref), prefix);
            }
        },
        .associate => |assoc| {
            try push_completion(items, assoc.name.string, completion_kind_field, prefix);
            if (assoc.value) |ref| try collect_completion_node(items, ink.ast.deref(ref), prefix);
        },
        .binary => |bin| {
            try collect_completion_node(items, ink.ast.deref(bin.left), prefix);
            try collect_completion_node(items, ink.ast.deref(bin.right), prefix);
        },
        .unary => |un| try collect_completion_node(items, ink.ast.deref(un.right), prefix),
        .type => |ty| switch (ty) {
            .optional => |ref| try collect_completion_node(items, ink.ast.deref(ref), prefix),
            .dyn => |ref| try collect_completion_node(items, ink.ast.deref(ref), prefix),
            .applied => |ap| for (ap.args) |arg_ref| try collect_completion_node(items, ink.ast.deref(arg_ref), prefix),
            else => {},
        },
        else => {},
    }
}

fn push_completion(
    items: *std.array_list.Managed(completion_item),
    label: []const u8,
    kind: u8,
    prefix: []const u8,
) !void {
    if (prefix.len != 0 and !std.mem.startsWith(u8, label, prefix)) return;
    for (items.items) |item| {
        if (std.mem.eql(u8, item.label, label)) return;
    }
    try items.append(.{ .label = label, .kind = kind });
}

fn find_hover_info(nodes: []const *ink.node, name: []const u8) ?hover_info {
    for (nodes) |node| {
        if (find_hover_info_node(node, name)) |info| return info;
    }
    return null;
}

fn find_hover_info_node(node: *const ink.node, name: []const u8) ?hover_info {
    switch (node.*) {
        .decl => |decl| switch (decl) {
            .function => |func| {
                if (std.mem.eql(u8, func.name.string, name)) return .{ .function = func };
                for (func.generics) |param| {
                    if (std.mem.eql(u8, param.name.string, name)) return .{ .generic_param = param };
                }
                for (func.params) |param| {
                    if (std.mem.eql(u8, param.name.string, name)) {
                        return .{ .param = .{ .name = param.name.string, .ty = param.ty } };
                    }
                }
                if (func.body) |ref| {
                    if (find_hover_info_node(ink.ast.deref(ref), name)) |info| return info;
                }
            },
            .@"struct" => |st| {
                if (std.mem.eql(u8, st.name.string, name)) return .{ .@"struct" = st };
                for (st.generics) |param| {
                    if (std.mem.eql(u8, param.name.string, name)) return .{ .generic_param = param };
                }
                for (st.fields) |field| {
                    if (std.mem.eql(u8, field.name.string, name)) {
                        return .{ .field = .{ .name = field.name.string, .ty = field.ty } };
                    }
                }
            },
            .trait => |tr| {
                if (std.mem.eql(u8, tr.name.string, name)) return .{ .trait = tr };
                for (tr.generics) |param| {
                    if (std.mem.eql(u8, param.name.string, name)) return .{ .generic_param = param };
                }
                for (tr.items) |item| {
                    switch (item) {
                        .function => |func| {
                            if (std.mem.eql(u8, func.name.string, name)) return .{ .function = func };
                        },
                        .assoc_type => |assoc| {
                            if (std.mem.eql(u8, assoc.name.string, name)) return .{ .assoc_type = assoc };
                        },
                    }
                }
            },
            .@"enum" => |e| {
                if (std.mem.eql(u8, e.name.string, name)) return .{ .@"enum" = e };
                for (e.variants) |variant| {
                    if (std.mem.eql(u8, variant.name.string, name)) {
                        return .{ .enum_variant = .{ .parent = e.name.string, .variant = variant } };
                    }
                }
            },
            .impl => |im| {
                for (im.functions) |func| {
                    if (std.mem.eql(u8, func.name.string, name)) return .{ .function = func };
                }
            },
            .import => |imp| {
                if (std.mem.eql(u8, imp.module.string, name)) return .{ .import = imp };
                if (imp.item) |item| {
                    if (std.mem.eql(u8, item.string, name)) return .{ .import = imp };
                }
                if (imp.alias) |alias| {
                    if (std.mem.eql(u8, alias.string, name)) return .{ .import = imp };
                }
            },
            .type_alias => |t| {
                if (std.mem.eql(u8, t.name.string, name)) return .{ .type_alias = t };
                for (t.generics) |param| {
                    if (std.mem.eql(u8, param.name.string, name)) return .{ .generic_param = param };
                }
            },
            .@"const" => |c| {
                if (std.mem.eql(u8, c.name.string, name)) return .{ .@"const" = c };
            },
            .@"var" => |v| {
                if (std.mem.eql(u8, v.name.string, name)) return .{ .@"var" = v };
            },
        },
        .block => |blk| {
            for (blk.items) |ref| {
                if (find_hover_info_node(ink.ast.deref(ref), name)) |info| return info;
            }
        },
        .if_expr => |ife| {
            if (find_hover_info_node(ink.ast.deref(ife.condition), name)) |info| return info;
            if (find_hover_info_node(ink.ast.deref(ife.then_branch), name)) |info| return info;
            if (ife.else_branch) |ref| {
                if (find_hover_info_node(ink.ast.deref(ref), name)) |info| return info;
            }
        },
        .match_expr => |me| {
            if (find_hover_info_node(ink.ast.deref(me.target), name)) |info| return info;
            for (me.arms) |arm| {
                if (find_hover_info_node(ink.ast.deref(arm.pattern), name)) |info| return info;
                if (find_hover_info_node(ink.ast.deref(arm.body), name)) |info| return info;
            }
        },
        .select_expr => |se| {
            for (se.arms) |arm| {
                if (arm.name) |arm_name| {
                    if (std.mem.eql(u8, arm_name.string, name)) {
                        return .{ .variable = arm_name };
                    }
                }
                if (find_hover_info_node(ink.ast.deref(arm.task), name)) |info| return info;
                if (find_hover_info_node(ink.ast.deref(arm.body), name)) |info| return info;
            }
        },
        .label_expr => |label_expr| {
            if (std.mem.eql(u8, label_expr.name.string, name)) {
                return .{ .variable = label_expr.name };
            }
            if (find_hover_info_node(ink.ast.deref(label_expr.body), name)) |info| return info;
        },
        .loop_expr => |loop_expr| {
            if (find_hover_info_node(ink.ast.deref(loop_expr.body), name)) |info| return info;
        },
        .while_expr => |while_expr| {
            if (find_hover_info_node(ink.ast.deref(while_expr.condition), name)) |info| return info;
            if (find_hover_info_node(ink.ast.deref(while_expr.body), name)) |info| return info;
        },
        .while_in_expr => |while_in_expr| {
            if (find_hover_info_node(ink.ast.deref(while_in_expr.pattern), name)) |info| return info;
            if (find_hover_info_node(ink.ast.deref(while_in_expr.iter), name)) |info| return info;
            if (find_hover_info_node(ink.ast.deref(while_in_expr.body), name)) |info| return info;
        },
        .until_expr => |until_expr| {
            if (find_hover_info_node(ink.ast.deref(until_expr.condition), name)) |info| return info;
            if (find_hover_info_node(ink.ast.deref(until_expr.body), name)) |info| return info;
        },
        .repeat_expr => |repeat_expr| {
            if (find_hover_info_node(ink.ast.deref(repeat_expr.count), name)) |info| return info;
            if (find_hover_info_node(ink.ast.deref(repeat_expr.body), name)) |info| return info;
        },
        .for_expr => |for_expr| {
            if (find_hover_info_node(ink.ast.deref(for_expr.pattern), name)) |info| return info;
            if (find_hover_info_node(ink.ast.deref(for_expr.iter), name)) |info| return info;
            if (find_hover_info_node(ink.ast.deref(for_expr.body), name)) |info| return info;
        },
        .each_expr => |each_expr| {
            if (find_hover_info_node(ink.ast.deref(each_expr.pattern), name)) |info| return info;
            if (find_hover_info_node(ink.ast.deref(each_expr.iter), name)) |info| return info;
            if (find_hover_info_node(ink.ast.deref(each_expr.body), name)) |info| return info;
        },
        .break_expr => |break_expr| {
            if (break_expr.label) |label| {
                if (std.mem.eql(u8, label.string, name)) return .{ .variable = label };
            }
            if (break_expr.value) |ref| {
                if (find_hover_info_node(ink.ast.deref(ref), name)) |info| return info;
            }
        },
        .continue_expr => |continue_expr| {
            if (continue_expr.label) |label| {
                if (std.mem.eql(u8, label.string, name)) return .{ .variable = label };
            }
        },
        .yield_expr => |yield_expr| {
            if (yield_expr.value) |ref| {
                if (find_hover_info_node(ink.ast.deref(ref), name)) |info| return info;
            }
        },
        .atomic_expr => |atomic_expr| {
            if (find_hover_info_node(ink.ast.deref(atomic_expr.value), name)) |info| return info;
        },
        .record => |rec| {
            for (rec.items) |assoc| {
                if (std.mem.eql(u8, assoc.name.string, name)) {
                    if (assoc.value) |ref| {
                        return .{ .field = .{ .name = assoc.name.string, .ty = ref } };
                    }
                }
                if (assoc.value) |ref| {
                    if (find_hover_info_node(ink.ast.deref(ref), name)) |info| return info;
                }
            }
        },
        .associate => |assoc| {
            if (assoc.value) |ref| {
                if (find_hover_info_node(ink.ast.deref(ref), name)) |info| return info;
            }
        },
        .binary => |bin| {
            if (find_hover_info_node(ink.ast.deref(bin.left), name)) |info| return info;
            if (find_hover_info_node(ink.ast.deref(bin.right), name)) |info| return info;
        },
        .unary => |un| return find_hover_info_node(ink.ast.deref(un.right), name),
        .type => |ty| switch (ty) {
            .optional => |ref| return find_hover_info_node(ink.ast.deref(ref), name),
            .applied => |ap| {
                for (ap.args) |arg_ref| {
                    if (find_hover_info_node(ink.ast.deref(arg_ref), name)) |info| return info;
                }
            },
            else => {},
        },
        else => {},
    }
    return null;
}

fn has_attribute(attrs: []const ink.ast.attribute, name: []const u8) bool {
    for (attrs) |attr| {
        if (std.mem.eql(u8, attr.name.string, name)) return true;
    }
    return false;
}

fn write_hover_info(writer: anytype, info: hover_info) anyerror!void {
    switch (info) {
        .function => |func| try write_function_signature(writer, func, has_attribute(func.attributes, "foreign")),
        .@"const" => |c| {
            try writer.writeAll("const ");
            try writer.writeAll(c.name.string);
            if (c.ty) |ref| {
                try writer.writeAll(": ");
                try write_type_node(writer, ink.ast.deref(ref));
            }
        },
        .@"var" => |v| {
            try writer.writeAll("var ");
            try writer.writeAll(v.name.string);
            if (v.ty) |ref| {
                try writer.writeAll(": ");
                try write_type_node(writer, ink.ast.deref(ref));
            }
        },
        .variable => |ident| {
            try writer.writeAll("var ");
            try writer.writeAll(ident.string);
        },
        .@"struct" => |st| {
            try writer.writeAll("struct ");
            try writer.writeAll(st.name.string);
        },
        .trait => |tr| {
            try writer.writeAll("trait ");
            try writer.writeAll(tr.name.string);
        },
        .enum_variant => |info_variant| {
            try writer.writeAll("variant ");
            try writer.writeAll(info_variant.variant.name.string);
            if (info_variant.variant.payload) |ref| {
                try writer.writeAll(": ");
                try write_type_node(writer, ink.ast.deref(ref));
            }
            try writer.writeAll(" (");
            try writer.writeAll(info_variant.parent);
            try writer.writeAll(")");
        },
        .@"enum" => |e| {
            try writer.writeAll("enum ");
            try writer.writeAll(e.name.string);
        },
        .type_alias => |t| {
            try writer.writeAll("type ");
            try writer.writeAll(t.name.string);
            try writer.writeAll(" = ");
            try write_type_node(writer, ink.ast.deref(t.value));
        },
        .assoc_type => |assoc| {
            try writer.writeAll("type ");
            try writer.writeAll(assoc.name.string);
            if (assoc.value) |ref| {
                try writer.writeAll(" = ");
                try write_type_node(writer, ink.ast.deref(ref));
            }
        },
        .param => |param| {
            try writer.writeAll("param ");
            try writer.writeAll(param.name);
            try writer.writeAll(": ");
            try write_type_node(writer, ink.ast.deref(param.ty));
        },
        .field => |field| {
            try writer.writeAll("field ");
            try writer.writeAll(field.name);
            try writer.writeAll(": ");
            try write_type_node(writer, ink.ast.deref(field.ty));
        },
        .generic_param => |param| {
            try writer.writeAll("generic ");
            try writer.writeAll(param.name.string);
            if (param.constraint) |ref| {
                try writer.writeAll(": ");
                try write_type_node(writer, ink.ast.deref(ref));
            }
        },
        .import => |imp| {
            try writer.writeAll("import ");
            try writer.writeAll(imp.module.string);
            if (imp.item) |item| {
                try writer.writeAll("::");
                try writer.writeAll(item.string);
            }
            if (imp.alias) |alias| {
                try writer.writeAll(" as ");
                try writer.writeAll(alias.string);
            }
        },
    }
}

fn write_function_signature(writer: anytype, func: ink.ast.function_decl, is_foreign: bool) anyerror!void {
    if (is_foreign) try writer.writeAll("#[foreign] ");
    try writer.writeAll("fn ");
    try writer.writeAll(func.name.string);
    try writer.writeAll("(");
    for (func.params, 0..) |param, idx| {
        if (idx != 0) try writer.writeAll(", ");
        try writer.writeAll(param.name.string);
        try writer.writeAll(": ");
        try write_type_node(writer, ink.ast.deref(param.ty));
    }
    try writer.writeAll(")");
    if (func.return_type) |ref| {
        try writer.writeAll(" -> ");
        try write_type_node(writer, ink.ast.deref(ref));
    }
}

fn write_signature_label(buf: *std.array_list.Managed(u8), sig: function_sig, skip_self: bool) !void {
    const writer = buf.writer();
    try writer.writeAll("fn ");
    try writer.writeAll(sig.name);
    try writer.writeAll("(");
    const param_start: usize = if (skip_self and sig.params.len > 0) 1 else 0;
    for (sig.params[param_start..], 0..) |param, idx| {
        if (idx != 0) try writer.writeAll(", ");
        try writer.writeAll(param.name.string);
        try writer.writeAll(": ");
        try write_type_node(writer, ink.ast.deref(param.ty));
    }
    try writer.writeAll(")");
    if (sig.return_type) |ref| {
        try writer.writeAll(" -> ");
        try write_type_node(writer, ink.ast.deref(ref));
    }
}

fn write_type_node(writer: anytype, node: *const ink.node) anyerror!void {
    switch (node.*) {
        .type => |ty| try write_type_expr(writer, ty),
        .identifier => |id| try writer.writeAll(id.string),
        .string => |id| try writer.writeAll(id.string),
        else => try writer.writeAll("unknown"),
    }
}

fn write_type_expr(writer: anytype, ty: ink.ast.type_expr) anyerror!void {
    switch (ty) {
        .self => try writer.writeAll("self"),
        .name => |id| try writer.writeAll(id.string),
        .optional => |ref| {
            try writer.writeAll("?");
            try write_type_node(writer, ink.ast.deref(ref));
        },
        .dyn => |ref| {
            try writer.writeAll("dyn ");
            try write_type_node(writer, ink.ast.deref(ref));
        },
        .applied => |ap| {
            const base = ap.base.string;
            if (std.mem.eql(u8, base, "fn") and ap.args.len == 2) {
                try write_type_node(writer, ink.ast.deref(ap.args[0]));
                try writer.writeAll(" -> ");
                try write_type_node(writer, ink.ast.deref(ap.args[1]));
                return;
            }
            if (std.mem.eql(u8, base, "union")) {
                try write_type_list(writer, ap.args, " | ");
                return;
            }
            if (std.mem.eql(u8, base, "intersect")) {
                try write_type_list(writer, ap.args, " & ");
                return;
            }
            if (std.mem.eql(u8, base, "tuple")) {
                try writer.writeAll("(");
                try write_type_list(writer, ap.args, ", ");
                try writer.writeAll(")");
                return;
            }
            if (std.mem.eql(u8, base, "slice") and ap.args.len == 1) {
                try writer.writeAll("[]");
                try write_type_node(writer, ink.ast.deref(ap.args[0]));
                return;
            }
            if (std.mem.eql(u8, base, "array") and ap.args.len == 2) {
                try writer.writeAll("[");
                try write_expr_brief(writer, ink.ast.deref(ap.args[0]));
                try writer.writeAll("]");
                try write_type_node(writer, ink.ast.deref(ap.args[1]));
                return;
            }
            if (std.mem.eql(u8, base, "field") and ap.args.len == 2) {
                try write_record_field(writer, ink.ast.deref(ap.args[0]));
                try writer.writeAll(": ");
                try write_type_node(writer, ink.ast.deref(ap.args[1]));
                return;
            }
            try writer.writeAll(base);
            if (ap.args.len != 0) {
                try writer.writeAll("<");
                try write_type_list(writer, ap.args, ", ");
                try writer.writeAll(">");
            }
        },
    }
}

fn write_record_field(writer: anytype, node: *const ink.node) anyerror!void {
    switch (node.*) {
        .string => |id| try writer.writeAll(id.string),
        .identifier => |id| try writer.writeAll(id.string),
        .type => |ty| switch (ty) {
            .applied => |ap| {
                if (std.mem.eql(u8, ap.base.string, "field") and ap.args.len >= 1) {
                    try write_record_field(writer, ink.ast.deref(ap.args[0]));
                    if (ap.args.len >= 2) {
                        try writer.writeAll(": ");
                        try write_type_node(writer, ink.ast.deref(ap.args[1]));
                    }
                    return;
                }
            },
            else => {},
        },
        else => try writer.writeAll("field"),
    }
}

fn write_type_list(writer: anytype, args: []const ink.ast.node_ref, sep: []const u8) anyerror!void {
    for (args, 0..) |arg_ref, idx| {
        if (idx != 0) try writer.writeAll(sep);
        try write_type_node(writer, ink.ast.deref(arg_ref));
    }
}

fn write_expr_brief(writer: anytype, node: *const ink.node) anyerror!void {
    switch (node.*) {
        .identifier => |id| try writer.writeAll(id.string),
        .integer => |val| try writer.print("{d}", .{val.value}),
        .float => |val| try writer.print("{d}", .{val.value}),
        .duration => |val| try writer.print("{d}", .{val.value}),
        .string => |id| {
            try writer.writeAll("\"");
            try writer.writeAll(id.string);
            try writer.writeAll("\"");
        },
        .type => |ty| try write_type_expr(writer, ty),
        else => try writer.writeAll("expr"),
    }
}

const numeric_hint_kind = enum { integer, float, duration };

fn is_digit_char(ch: u8) bool {
    return ch >= '0' and ch <= '9';
}

fn add_digit_group_hints(
    hints: *std.array_list.Managed(inlay_hint),
    line_offsets: []const usize,
    text: []const u8,
    digits: []const u8,
    base_offset: usize,
    start_offset: usize,
    end_offset: usize,
) !void {
    if (digits.len <= 3) return;
    var first_group = digits.len % 3;
    if (first_group == 0) first_group = 3;
    var idx = first_group;
    while (idx < digits.len) : (idx += 3) {
        const hint_offset = base_offset + idx;
        if (hint_offset < start_offset or hint_offset > end_offset) continue;
        const pos = position_from_offset_with_lines(text, line_offsets, hint_offset);
        try hints.append(.{
            .position = pos,
            .label = "_",
            .owned = false,
            .kind = inlay_hint_kind_type,
            .padding_left = false,
            .padding_right = false,
        });
    }
}

fn maybe_add_numeric_group_hints(
    hints: *std.array_list.Managed(inlay_hint),
    line_offsets: []const usize,
    text: []const u8,
    span: source.span,
    kind: numeric_hint_kind,
    start_offset: usize,
    end_offset: usize,
) !void {
    if (span.start >= text.len or span.end > text.len or span.start >= span.end) return;
    const literal = text[span.start..span.end];
    switch (kind) {
        .integer => try add_digit_group_hints(hints, line_offsets, text, literal, span.start, start_offset, end_offset),
        .float => {
            var cut = literal.len;
            if (std.mem.indexOfScalar(u8, literal, '.')) |idx| {
                if (idx < cut) cut = idx;
            }
            if (std.mem.indexOfScalar(u8, literal, 'e')) |idx| {
                if (idx < cut) cut = idx;
            }
            if (std.mem.indexOfScalar(u8, literal, 'E')) |idx| {
                if (idx < cut) cut = idx;
            }
            if (cut == 0) return;
            try add_digit_group_hints(hints, line_offsets, text, literal[0..cut], span.start, start_offset, end_offset);
        },
        .duration => {
            var idx: usize = 0;
            while (idx < literal.len) {
                if (!is_digit_char(literal[idx])) {
                    idx += 1;
                    continue;
                }
                const start = idx;
                idx += 1;
                while (idx < literal.len and is_digit_char(literal[idx])) : (idx += 1) {}
                try add_digit_group_hints(hints, line_offsets, text, literal[start..idx], span.start + start, start_offset, end_offset);
            }
        },
    }
}

fn collect_inlay_hints(
    allocator: mem_allocator,
    hints: *std.array_list.Managed(inlay_hint),
    nodes: []const *ink.node,
    text: []const u8,
    range_start: ?position,
    range_end: ?position,
) !void {
    const line_offsets = try build_line_offsets(allocator, text);
    defer allocator.free(line_offsets);

    const start_offset = if (range_start) |pos|
        offset_from_position(text, line_offsets, pos)
    else
        0;
    const end_offset = if (range_end) |pos|
        offset_from_position(text, line_offsets, pos)
    else
        text.len;

    var sigs = std.array_list.Managed(function_sig).init(allocator);
    defer sigs.deinit();
    for (nodes) |node| try collect_function_sigs(&sigs, node);

    var variants = std.array_list.Managed(variant_info).init(allocator);
    defer variants.deinit();
    for (nodes) |node| try collect_variant_infos(&variants, node);

    for (nodes) |node| {
        try collect_inlay_hints_node(
            allocator,
            hints,
            sigs.items,
            variants.items,
            line_offsets,
            text,
            node,
            start_offset,
            end_offset,
        );
    }
}

fn collect_inlay_hints_node(
    allocator: mem_allocator,
    hints: *std.array_list.Managed(inlay_hint),
    sigs: []const function_sig,
    variants: []const variant_info,
    line_offsets: []const usize,
    text: []const u8,
    node: *const ink.node,
    start_offset: usize,
    end_offset: usize,
) !void {
    switch (node.*) {
        .decl => |decl| switch (decl) {
            .function => |func| {
                if (func.return_type == null and func.body != null) {
                    if (try infer_return_type_from_body(allocator, sigs, ink.ast.deref(func.body.?))) |label| {
                        defer free_type_label(allocator, label);
                        const hint_label = try std.fmt.allocPrint(allocator, " -> {s}", .{label.text});
                        const hint_offset = function_hint_offset(func);
                        if (hint_offset >= start_offset and hint_offset <= end_offset) {
                            const pos = position_from_offset_with_lines(text, line_offsets, hint_offset);
                            try hints.append(.{
                                .position = pos,
                                .label = hint_label,
                                .owned = true,
                                .kind = inlay_hint_kind_type,
                                .padding_left = true,
                                .padding_right = true,
                            });
                        } else {
                            allocator.free(hint_label);
                        }
                    }
                }
                if (func.body) |ref| try collect_inlay_hints_node(
                    allocator,
                    hints,
                    sigs,
                    variants,
                    line_offsets,
                    text,
                    ink.ast.deref(ref),
                    start_offset,
                    end_offset,
                );
            },
            .@"const" => |c| {
                if (c.ty == null) {
                    try maybe_add_type_hint(
                        allocator,
                        hints,
                        sigs,
                        line_offsets,
                        text,
                        c.name.where.end,
                        ink.ast.deref(c.value),
                        start_offset,
                        end_offset,
                    );
                }
                try collect_inlay_hints_node(
                    allocator,
                    hints,
                    sigs,
                    variants,
                    line_offsets,
                    text,
                    ink.ast.deref(c.value),
                    start_offset,
                    end_offset,
                );
            },
            .@"var" => |v| {
                if (v.ty == null) {
                    try maybe_add_type_hint(
                        allocator,
                        hints,
                        sigs,
                        line_offsets,
                        text,
                        v.name.where.end,
                        ink.ast.deref(v.value),
                        start_offset,
                        end_offset,
                    );
                }
                try collect_inlay_hints_node(
                    allocator,
                    hints,
                    sigs,
                    variants,
                    line_offsets,
                    text,
                    ink.ast.deref(v.value),
                    start_offset,
                    end_offset,
                );
            },
            .type_alias => |t| try collect_inlay_hints_node(
                allocator,
                hints,
                sigs,
                variants,
                line_offsets,
                text,
                ink.ast.deref(t.value),
                start_offset,
                end_offset,
            ),
            .impl => |im| {
                for (im.functions) |func| {
                    if (func.body) |ref| try collect_inlay_hints_node(
                        allocator,
                        hints,
                        sigs,
                        variants,
                        line_offsets,
                        text,
                        ink.ast.deref(ref),
                        start_offset,
                        end_offset,
                    );
                }
            },
            else => {},
        },
        .block => |blk| {
            for (blk.items) |ref| try collect_inlay_hints_node(
                allocator,
                hints,
                sigs,
                variants,
                line_offsets,
                text,
                ink.ast.deref(ref),
                start_offset,
                end_offset,
            );
        },
        .if_expr => |ife| {
            try maybe_add_expr_result_hint(allocator, hints, sigs, line_offsets, text, node, start_offset, end_offset);
            try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(ife.condition), start_offset, end_offset);
            try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(ife.then_branch), start_offset, end_offset);
            if (ife.else_branch) |ref| {
                try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(ref), start_offset, end_offset);
            }
        },
        .match_expr => |me| {
            try maybe_add_expr_result_hint(allocator, hints, sigs, line_offsets, text, node, start_offset, end_offset);
            try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(me.target), start_offset, end_offset);
            for (me.arms) |arm| {
                try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(arm.pattern), start_offset, end_offset);
                try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(arm.body), start_offset, end_offset);
            }
        },
        .select_expr => |se| {
            try maybe_add_expr_result_hint(allocator, hints, sigs, line_offsets, text, node, start_offset, end_offset);
            for (se.arms) |arm| {
                try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(arm.task), start_offset, end_offset);
                try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(arm.body), start_offset, end_offset);
            }
        },
        .label_expr => |label_expr| {
            try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(label_expr.body), start_offset, end_offset);
        },
        .loop_expr => |loop_expr| {
            try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(loop_expr.body), start_offset, end_offset);
        },
        .while_expr => |while_expr| {
            try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(while_expr.condition), start_offset, end_offset);
            try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(while_expr.body), start_offset, end_offset);
        },
        .while_in_expr => |while_in_expr| {
            try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(while_in_expr.pattern), start_offset, end_offset);
            try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(while_in_expr.iter), start_offset, end_offset);
            try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(while_in_expr.body), start_offset, end_offset);
        },
        .until_expr => |until_expr| {
            try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(until_expr.condition), start_offset, end_offset);
            try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(until_expr.body), start_offset, end_offset);
        },
        .repeat_expr => |repeat_expr| {
            try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(repeat_expr.count), start_offset, end_offset);
            try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(repeat_expr.body), start_offset, end_offset);
        },
        .for_expr => |for_expr| {
            try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(for_expr.pattern), start_offset, end_offset);
            try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(for_expr.iter), start_offset, end_offset);
            try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(for_expr.body), start_offset, end_offset);
        },
        .each_expr => |each_expr| {
            try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(each_expr.pattern), start_offset, end_offset);
            try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(each_expr.iter), start_offset, end_offset);
            try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(each_expr.body), start_offset, end_offset);
        },
        .break_expr => |break_expr| {
            if (break_expr.value) |ref| {
                try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(ref), start_offset, end_offset);
            }
        },
        .continue_expr => {},
        .yield_expr => |yield_expr| {
            if (yield_expr.value) |ref| {
                try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(ref), start_offset, end_offset);
            }
        },
        .atomic_expr => |atomic_expr| {
            try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(atomic_expr.value), start_offset, end_offset);
        },
        .record => |rec| {
            for (rec.items) |assoc| {
                if (assoc.value) |ref| try collect_inlay_hints_node(
                    allocator,
                    hints,
                    sigs,
                    variants,
                    line_offsets,
                    text,
                    ink.ast.deref(ref),
                    start_offset,
                    end_offset,
                );
                if (assoc.value) |ref| {
                    try maybe_add_type_hint(
                        allocator,
                        hints,
                        sigs,
                        line_offsets,
                        text,
                        assoc.name.where.end,
                        ink.ast.deref(ref),
                        start_offset,
                        end_offset,
                    );
                }
            }
        },
        .associate => |assoc| {
            if (assoc.value) |ref| try collect_inlay_hints_node(
                allocator,
                hints,
                sigs,
                variants,
                line_offsets,
                text,
                ink.ast.deref(ref),
                start_offset,
                end_offset,
            );
        },
        .binary => |bin| {
            if (bin.op == .call) {
                const call = try flatten_call(allocator, node);
                defer allocator.free(call.args);
                const sig = if (callee_name(call.callee)) |name|
                    find_function_sig(sigs, name)
                else
                    null;
                var param_offset: usize = 0;
                if (sig) |found_sig| {
                    if (call.callee.* == .binary and call.callee.binary.op == .access) {
                        if (found_sig.params.len > 0 and is_receiver_param_name(found_sig.params[0].name.string)) {
                            param_offset = 1;
                        }
                    }
                }
                for (call.args, 0..) |arg, idx| {
                    if (sig) |found_sig| {
                        const param_idx = idx + param_offset;
                        if (param_idx < found_sig.params.len) {
                            if (node_start_span(arg)) |span| {
                                if (span.start >= start_offset and span.start <= end_offset) {
                                    const label = try std.fmt.allocPrint(allocator, "{s}:", .{found_sig.params[param_idx].name.string});
                                    const pos = position_from_offset_with_lines(text, line_offsets, span.start);
                                    try hints.append(.{
                                        .position = pos,
                                        .label = label,
                                        .owned = true,
                                        .kind = inlay_hint_kind_parameter,
                                        .padding_right = true,
                                    });
                                }
                            }
                        }
                    }
                    try collect_inlay_hints_node(
                        allocator,
                        hints,
                        sigs,
                        variants,
                        line_offsets,
                        text,
                        arg,
                        start_offset,
                        end_offset,
                    );
                }
                return;
            }
            if (bin.op == .access or bin.op == .scope_access) {
                const right = ink.ast.deref(bin.right);
                if (right.* == .identifier) {
                    try maybe_add_variant_hint(
                        allocator,
                        hints,
                        variants,
                        line_offsets,
                        text,
                        right.identifier,
                        start_offset,
                        end_offset,
                    );
                }
            }
            try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(bin.left), start_offset, end_offset);
            try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(bin.right), start_offset, end_offset);
        },
        .unary => |un| try collect_inlay_hints_node(
            allocator,
            hints,
            sigs,
            variants,
            line_offsets,
            text,
            ink.ast.deref(un.right),
            start_offset,
            end_offset,
        ),
        .type => |ty| switch (ty) {
            .optional => |ref| try collect_inlay_hints_node(
                allocator,
                hints,
                sigs,
                variants,
                line_offsets,
                text,
                ink.ast.deref(ref),
                start_offset,
                end_offset,
            ),
            .applied => |ap| {
                for (ap.args) |arg_ref| {
                    try collect_inlay_hints_node(allocator, hints, sigs, variants, line_offsets, text, ink.ast.deref(arg_ref), start_offset, end_offset);
                }
            },
            else => {},
        },
        .identifier => |id| {
            try maybe_add_variant_hint(allocator, hints, variants, line_offsets, text, id, start_offset, end_offset);
        },
        else => {},
    }
}

fn collect_function_sigs(sigs: *std.array_list.Managed(function_sig), node: *const ink.node) !void {
    switch (node.*) {
        .decl => |decl| switch (decl) {
            .function => |func| {
                try sigs.append(.{
                    .name = func.name.string,
                    .params = func.params,
                    .return_type = func.return_type,
                    .is_foreign = has_attribute(func.attributes, "foreign"),
                });
                if (func.body) |ref| try collect_function_sigs(sigs, ink.ast.deref(ref));
            },
            .impl => |im| {
                for (im.functions) |func| {
                    try sigs.append(.{
                        .name = func.name.string,
                        .params = func.params,
                        .return_type = func.return_type,
                        .is_foreign = has_attribute(func.attributes, "foreign"),
                    });
                    if (func.body) |ref| try collect_function_sigs(sigs, ink.ast.deref(ref));
                }
            },
            .trait => |tr| {
                for (tr.items) |item| {
                    switch (item) {
                        .function => |func| try sigs.append(.{
                            .name = func.name.string,
                            .params = func.params,
                            .return_type = func.return_type,
                            .is_foreign = has_attribute(func.attributes, "foreign"),
                        }),
                        else => {},
                    }
                }
            },
            .@"const" => |c| try collect_function_sigs(sigs, ink.ast.deref(c.value)),
            .@"var" => |v| try collect_function_sigs(sigs, ink.ast.deref(v.value)),
            .type_alias => |t| try collect_function_sigs(sigs, ink.ast.deref(t.value)),
            else => {},
        },
        .block => |blk| {
            for (blk.items) |ref| try collect_function_sigs(sigs, ink.ast.deref(ref));
        },
        .if_expr => |ife| {
            try collect_function_sigs(sigs, ink.ast.deref(ife.condition));
            try collect_function_sigs(sigs, ink.ast.deref(ife.then_branch));
            if (ife.else_branch) |ref| try collect_function_sigs(sigs, ink.ast.deref(ref));
        },
        .match_expr => |me| {
            try collect_function_sigs(sigs, ink.ast.deref(me.target));
            for (me.arms) |arm| {
                try collect_function_sigs(sigs, ink.ast.deref(arm.pattern));
                try collect_function_sigs(sigs, ink.ast.deref(arm.body));
            }
        },
        .select_expr => |se| {
            for (se.arms) |arm| {
                try collect_function_sigs(sigs, ink.ast.deref(arm.task));
                try collect_function_sigs(sigs, ink.ast.deref(arm.body));
            }
        },
        .label_expr => |label_expr| {
            try collect_function_sigs(sigs, ink.ast.deref(label_expr.body));
        },
        .loop_expr => |loop_expr| {
            try collect_function_sigs(sigs, ink.ast.deref(loop_expr.body));
        },
        .while_expr => |while_expr| {
            try collect_function_sigs(sigs, ink.ast.deref(while_expr.condition));
            try collect_function_sigs(sigs, ink.ast.deref(while_expr.body));
        },
        .while_in_expr => |while_in_expr| {
            try collect_function_sigs(sigs, ink.ast.deref(while_in_expr.pattern));
            try collect_function_sigs(sigs, ink.ast.deref(while_in_expr.iter));
            try collect_function_sigs(sigs, ink.ast.deref(while_in_expr.body));
        },
        .until_expr => |until_expr| {
            try collect_function_sigs(sigs, ink.ast.deref(until_expr.condition));
            try collect_function_sigs(sigs, ink.ast.deref(until_expr.body));
        },
        .repeat_expr => |repeat_expr| {
            try collect_function_sigs(sigs, ink.ast.deref(repeat_expr.count));
            try collect_function_sigs(sigs, ink.ast.deref(repeat_expr.body));
        },
        .for_expr => |for_expr| {
            try collect_function_sigs(sigs, ink.ast.deref(for_expr.pattern));
            try collect_function_sigs(sigs, ink.ast.deref(for_expr.iter));
            try collect_function_sigs(sigs, ink.ast.deref(for_expr.body));
        },
        .each_expr => |each_expr| {
            try collect_function_sigs(sigs, ink.ast.deref(each_expr.pattern));
            try collect_function_sigs(sigs, ink.ast.deref(each_expr.iter));
            try collect_function_sigs(sigs, ink.ast.deref(each_expr.body));
        },
        .break_expr => |break_expr| {
            if (break_expr.value) |ref| try collect_function_sigs(sigs, ink.ast.deref(ref));
        },
        .continue_expr => {},
        .yield_expr => |yield_expr| {
            if (yield_expr.value) |ref| try collect_function_sigs(sigs, ink.ast.deref(ref));
        },
        .atomic_expr => |atomic_expr| {
            try collect_function_sigs(sigs, ink.ast.deref(atomic_expr.value));
        },
        .record => |rec| {
            for (rec.items) |assoc| {
                if (assoc.value) |ref| try collect_function_sigs(sigs, ink.ast.deref(ref));
            }
        },
        .associate => |assoc| {
            if (assoc.value) |ref| try collect_function_sigs(sigs, ink.ast.deref(ref));
        },
        .binary => |bin| {
            try collect_function_sigs(sigs, ink.ast.deref(bin.left));
            try collect_function_sigs(sigs, ink.ast.deref(bin.right));
        },
        .unary => |un| try collect_function_sigs(sigs, ink.ast.deref(un.right)),
        .type => |ty| switch (ty) {
            .optional => |ref| try collect_function_sigs(sigs, ink.ast.deref(ref)),
            .dyn => |ref| try collect_function_sigs(sigs, ink.ast.deref(ref)),
            .applied => |ap| for (ap.args) |arg_ref| try collect_function_sigs(sigs, ink.ast.deref(arg_ref)),
            else => {},
        },
        else => {},
    }
}

fn collect_variant_infos(variants: *std.array_list.Managed(variant_info), node: *const ink.node) !void {
    switch (node.*) {
        .decl => |decl| switch (decl) {
            .@"enum" => |e| {
                for (e.variants) |variant| {
                    try variants.append(.{
                        .name = variant.name.string,
                        .parent = e.name.string,
                        .kind = .@"enum",
                    });
                    if (variant.payload) |ref| try collect_variant_infos(variants, ink.ast.deref(ref));
                }
            },
            .function => |func| if (func.body) |ref| try collect_variant_infos(variants, ink.ast.deref(ref)),
            .impl => |im| {
                for (im.functions) |func| {
                    if (func.body) |ref| try collect_variant_infos(variants, ink.ast.deref(ref));
                }
            },
            .@"const" => |c| try collect_variant_infos(variants, ink.ast.deref(c.value)),
            .@"var" => |v| try collect_variant_infos(variants, ink.ast.deref(v.value)),
            .type_alias => |t| try collect_variant_infos(variants, ink.ast.deref(t.value)),
            else => {},
        },
        .block => |blk| for (blk.items) |ref| try collect_variant_infos(variants, ink.ast.deref(ref)),
        .if_expr => |ife| {
            try collect_variant_infos(variants, ink.ast.deref(ife.condition));
            try collect_variant_infos(variants, ink.ast.deref(ife.then_branch));
            if (ife.else_branch) |ref| try collect_variant_infos(variants, ink.ast.deref(ref));
        },
        .match_expr => |me| {
            try collect_variant_infos(variants, ink.ast.deref(me.target));
            for (me.arms) |arm| {
                try collect_variant_infos(variants, ink.ast.deref(arm.pattern));
                try collect_variant_infos(variants, ink.ast.deref(arm.body));
            }
        },
        .select_expr => |se| {
            for (se.arms) |arm| {
                try collect_variant_infos(variants, ink.ast.deref(arm.task));
                try collect_variant_infos(variants, ink.ast.deref(arm.body));
            }
        },
        .label_expr => |label_expr| {
            try collect_variant_infos(variants, ink.ast.deref(label_expr.body));
        },
        .loop_expr => |loop_expr| {
            try collect_variant_infos(variants, ink.ast.deref(loop_expr.body));
        },
        .while_expr => |while_expr| {
            try collect_variant_infos(variants, ink.ast.deref(while_expr.condition));
            try collect_variant_infos(variants, ink.ast.deref(while_expr.body));
        },
        .while_in_expr => |while_in_expr| {
            try collect_variant_infos(variants, ink.ast.deref(while_in_expr.pattern));
            try collect_variant_infos(variants, ink.ast.deref(while_in_expr.iter));
            try collect_variant_infos(variants, ink.ast.deref(while_in_expr.body));
        },
        .until_expr => |until_expr| {
            try collect_variant_infos(variants, ink.ast.deref(until_expr.condition));
            try collect_variant_infos(variants, ink.ast.deref(until_expr.body));
        },
        .repeat_expr => |repeat_expr| {
            try collect_variant_infos(variants, ink.ast.deref(repeat_expr.count));
            try collect_variant_infos(variants, ink.ast.deref(repeat_expr.body));
        },
        .for_expr => |for_expr| {
            try collect_variant_infos(variants, ink.ast.deref(for_expr.pattern));
            try collect_variant_infos(variants, ink.ast.deref(for_expr.iter));
            try collect_variant_infos(variants, ink.ast.deref(for_expr.body));
        },
        .each_expr => |each_expr| {
            try collect_variant_infos(variants, ink.ast.deref(each_expr.pattern));
            try collect_variant_infos(variants, ink.ast.deref(each_expr.iter));
            try collect_variant_infos(variants, ink.ast.deref(each_expr.body));
        },
        .break_expr => |break_expr| {
            if (break_expr.value) |ref| try collect_variant_infos(variants, ink.ast.deref(ref));
        },
        .continue_expr => {},
        .yield_expr => |yield_expr| {
            if (yield_expr.value) |ref| try collect_variant_infos(variants, ink.ast.deref(ref));
        },
        .atomic_expr => |atomic_expr| {
            try collect_variant_infos(variants, ink.ast.deref(atomic_expr.value));
        },
        .record => |rec| {
            for (rec.items) |assoc| {
                if (assoc.value) |ref| try collect_variant_infos(variants, ink.ast.deref(ref));
            }
        },
        .associate => |assoc| {
            if (assoc.value) |ref| try collect_variant_infos(variants, ink.ast.deref(ref));
        },
        .binary => |bin| {
            try collect_variant_infos(variants, ink.ast.deref(bin.left));
            try collect_variant_infos(variants, ink.ast.deref(bin.right));
        },
        .unary => |un| try collect_variant_infos(variants, ink.ast.deref(un.right)),
        .type => |ty| switch (ty) {
            .optional => |ref| try collect_variant_infos(variants, ink.ast.deref(ref)),
            .applied => |ap| for (ap.args) |arg_ref| try collect_variant_infos(variants, ink.ast.deref(arg_ref)),
            else => {},
        },
        else => {},
    }
}

fn find_function_sig(sigs: []const function_sig, name: []const u8) ?function_sig {
    for (sigs) |sig| {
        if (std.mem.eql(u8, sig.name, name)) return sig;
    }
    return null;
}

fn flatten_call(allocator: mem_allocator, node: *const ink.node) !call_info {
    var args = std.array_list.Managed(*ink.node).init(allocator);
    errdefer args.deinit();

    var current: *const ink.node = node;
    while (current.* == .binary and current.binary.op == .call) {
        const bin = current.binary;
        try args.append(ink.ast.deref(bin.right));
        current = ink.ast.deref(bin.left);
    }

    const slice = try args.toOwnedSlice();
    std.mem.reverse(*ink.node, slice);
    return .{ .callee = current, .args = slice };
}

fn callee_name(node: *const ink.node) ?[]const u8 {
    switch (node.*) {
        .identifier => |id| return id.string,
        .binary => |bin| {
            if (bin.op == .access or bin.op == .scope_access) {
                const right = ink.ast.deref(bin.right);
                if (right.* == .identifier) return right.identifier.string;
            }
        },
        .type => |ty| switch (ty) {
            .name => |id| return id.string,
            else => {},
        },
        else => {},
    }
    return null;
}

fn is_receiver_param_name(name: []const u8) bool {
    return std.mem.eql(u8, name, "self") or std.mem.eql(u8, name, "this");
}

fn maybe_add_type_hint(
    allocator: mem_allocator,
    hints: *std.array_list.Managed(inlay_hint),
    sigs: []const function_sig,
    line_offsets: []const usize,
    text: []const u8,
    name_end: usize,
    value_node: *const ink.node,
    start_offset: usize,
    end_offset: usize,
) !void {
    if (name_end < start_offset or name_end > end_offset) return;
    if (try infer_expr_type_label(allocator, sigs, value_node)) |label| {
        defer free_type_label(allocator, label);
        const hint_label = try std.fmt.allocPrint(allocator, ": {s}", .{label.text});
        const pos = position_from_offset_with_lines(text, line_offsets, name_end);
        try hints.append(.{
            .position = pos,
            .label = hint_label,
            .owned = true,
            .kind = inlay_hint_kind_type,
            .padding_left = true,
            .padding_right = true,
        });
    }
}

fn function_hint_offset(func: ink.ast.function_decl) usize {
    if (func.params.len != 0) {
        const last = func.params[func.params.len - 1].name;
        return last.where.end;
    }
    return func.name.where.end;
}

fn maybe_add_expr_result_hint(
    allocator: mem_allocator,
    hints: *std.array_list.Managed(inlay_hint),
    sigs: []const function_sig,
    line_offsets: []const usize,
    text: []const u8,
    node: *const ink.node,
    start_offset: usize,
    end_offset: usize,
) !void {
    const span = node_start_span(node) orelse return;
    const pos_offset = span.end;
    if (pos_offset < start_offset or pos_offset > end_offset) return;
    if (try infer_expr_type_label(allocator, sigs, node)) |label| {
        defer free_type_label(allocator, label);
        const hint_label = try std.fmt.allocPrint(allocator, ": {s}", .{label.text});
        const pos = position_from_offset_with_lines(text, line_offsets, pos_offset);
        try hints.append(.{
            .position = pos,
            .label = hint_label,
            .owned = true,
            .kind = inlay_hint_kind_type,
            .padding_left = true,
            .padding_right = true,
        });
    }
}

fn maybe_add_variant_hint(
    allocator: mem_allocator,
    hints: *std.array_list.Managed(inlay_hint),
    variants: []const variant_info,
    line_offsets: []const usize,
    text: []const u8,
    ident: ink.identifier,
    start_offset: usize,
    end_offset: usize,
) !void {
    const end = ident.where.end;
    if (end < start_offset or end > end_offset) return;
    const parent = find_variant_parent(variants, ident.string) orelse return;
    const hint_label = try std.fmt.allocPrint(allocator, ": {s}", .{parent});
    const pos = position_from_offset_with_lines(text, line_offsets, end);
    try hints.append(.{
        .position = pos,
        .label = hint_label,
        .owned = true,
        .kind = inlay_hint_kind_type,
        .padding_left = true,
        .padding_right = true,
    });
}

fn find_variant_parent(variants: []const variant_info, name: []const u8) ?[]const u8 {
    var found: ?[]const u8 = null;
    for (variants) |variant| {
        if (!std.mem.eql(u8, variant.name, name)) continue;
        if (found == null) {
            found = variant.parent;
        } else if (!std.mem.eql(u8, found.?, variant.parent)) {
            return null;
        }
    }
    return found;
}

fn infer_return_type_from_body(
    allocator: mem_allocator,
    sigs: []const function_sig,
    node: *const ink.node,
) !?type_label {
    var state = return_state{};
    try collect_return_labels(allocator, sigs, node, &state);
    if (!state.valid) return null;
    if (state.saw_return) return state.label;
    return infer_expr_type_label(allocator, sigs, node);
}

const return_state = struct {
    label: ?type_label = null,
    saw_return: bool = false,
    valid: bool = true,
};

fn collect_return_labels(
    allocator: mem_allocator,
    sigs: []const function_sig,
    node: *const ink.node,
    state: *return_state,
) !void {
    if (!state.valid) return;
    switch (node.*) {
        .unary => |un| {
            if (un.op == .ret) {
                state.saw_return = true;
                const label = try infer_expr_type_label(allocator, sigs, ink.ast.deref(un.right));
                if (label == null) {
                    if (state.label) |held| free_type_label(allocator, held);
                    state.label = null;
                    state.valid = false;
                    return;
                }
                if (state.label == null) {
                    state.label = label;
                    return;
                }
                const merged = merge_type_labels(allocator, state.label, label);
                if (merged == null) {
                    state.label = null;
                    state.valid = false;
                } else {
                    state.label = merged;
                }
                return;
            }
            try collect_return_labels(allocator, sigs, ink.ast.deref(un.right), state);
        },
        .block => |blk| {
            for (blk.items) |ref| try collect_return_labels(allocator, sigs, ink.ast.deref(ref), state);
        },
        .if_expr => |ife| {
            try collect_return_labels(allocator, sigs, ink.ast.deref(ife.then_branch), state);
            if (ife.else_branch) |ref| try collect_return_labels(allocator, sigs, ink.ast.deref(ref), state);
        },
        .match_expr => |me| {
            for (me.arms) |arm| try collect_return_labels(allocator, sigs, ink.ast.deref(arm.body), state);
        },
        .select_expr => |se| {
            for (se.arms) |arm| try collect_return_labels(allocator, sigs, ink.ast.deref(arm.body), state);
        },
        .label_expr => |label_expr| {
            try collect_return_labels(allocator, sigs, ink.ast.deref(label_expr.body), state);
        },
        .loop_expr => |loop_expr| {
            try collect_return_labels(allocator, sigs, ink.ast.deref(loop_expr.body), state);
        },
        .while_expr => |while_expr| {
            try collect_return_labels(allocator, sigs, ink.ast.deref(while_expr.condition), state);
            try collect_return_labels(allocator, sigs, ink.ast.deref(while_expr.body), state);
        },
        .while_in_expr => |while_in_expr| {
            try collect_return_labels(allocator, sigs, ink.ast.deref(while_in_expr.pattern), state);
            try collect_return_labels(allocator, sigs, ink.ast.deref(while_in_expr.iter), state);
            try collect_return_labels(allocator, sigs, ink.ast.deref(while_in_expr.body), state);
        },
        .until_expr => |until_expr| {
            try collect_return_labels(allocator, sigs, ink.ast.deref(until_expr.condition), state);
            try collect_return_labels(allocator, sigs, ink.ast.deref(until_expr.body), state);
        },
        .repeat_expr => |repeat_expr| {
            try collect_return_labels(allocator, sigs, ink.ast.deref(repeat_expr.count), state);
            try collect_return_labels(allocator, sigs, ink.ast.deref(repeat_expr.body), state);
        },
        .for_expr => |for_expr| {
            try collect_return_labels(allocator, sigs, ink.ast.deref(for_expr.pattern), state);
            try collect_return_labels(allocator, sigs, ink.ast.deref(for_expr.iter), state);
            try collect_return_labels(allocator, sigs, ink.ast.deref(for_expr.body), state);
        },
        .each_expr => |each_expr| {
            try collect_return_labels(allocator, sigs, ink.ast.deref(each_expr.pattern), state);
            try collect_return_labels(allocator, sigs, ink.ast.deref(each_expr.iter), state);
            try collect_return_labels(allocator, sigs, ink.ast.deref(each_expr.body), state);
        },
        .break_expr => |break_expr| {
            if (break_expr.value) |ref| try collect_return_labels(allocator, sigs, ink.ast.deref(ref), state);
        },
        .continue_expr => {},
        .yield_expr => |yield_expr| {
            if (yield_expr.value) |ref| try collect_return_labels(allocator, sigs, ink.ast.deref(ref), state);
        },
        .atomic_expr => |atomic_expr| {
            try collect_return_labels(allocator, sigs, ink.ast.deref(atomic_expr.value), state);
        },
        .record => |rec| {
            for (rec.items) |assoc| {
                if (assoc.value) |ref| try collect_return_labels(allocator, sigs, ink.ast.deref(ref), state);
            }
        },
        .associate => |assoc| {
            if (assoc.value) |ref| try collect_return_labels(allocator, sigs, ink.ast.deref(ref), state);
        },
        .binary => |bin| {
            try collect_return_labels(allocator, sigs, ink.ast.deref(bin.left), state);
            try collect_return_labels(allocator, sigs, ink.ast.deref(bin.right), state);
        },
        .type => |ty| switch (ty) {
            .optional => |ref| try collect_return_labels(allocator, sigs, ink.ast.deref(ref), state),
            .applied => |ap| for (ap.args) |arg_ref| try collect_return_labels(allocator, sigs, ink.ast.deref(arg_ref), state),
            else => {},
        },
        else => {},
    }
}

fn infer_expr_type_label(
    allocator: mem_allocator,
    sigs: []const function_sig,
    node: *const ink.node,
) !?type_label {
    switch (node.*) {
        .integer => return .{ .text = "int", .owned = false },
        .float => return .{ .text = "float", .owned = false },
        .duration => return .{ .text = "duration", .owned = false },
        .string => return .{ .text = "string", .owned = false },
        .identifier => |id| {
            if (std.mem.eql(u8, id.string, "true") or std.mem.eql(u8, id.string, "false")) {
                return .{ .text = "bool", .owned = false };
            }
            if (std.mem.eql(u8, id.string, "unit")) {
                return .{ .text = "unit", .owned = false };
            }
            return null;
        },
        .record => return null,
        .label_expr => |label_expr| return infer_expr_type_label(allocator, sigs, ink.ast.deref(label_expr.body)),
        .unary => |un| return infer_expr_type_label(allocator, sigs, ink.ast.deref(un.right)),
        .binary => |bin| switch (bin.op) {
            .call => {
                const target = call_callee(node);
                if (callee_name(target)) |name| {
                    if (find_function_sig(sigs, name)) |sig| {
                        if (sig.return_type) |ref| {
                            return try label_from_type_node(allocator, ink.ast.deref(ref));
                        }
                    }
                }
                return null;
            },
            .assign, .pipe => return infer_expr_type_label(allocator, sigs, ink.ast.deref(bin.right)),
            .coalesce => return merge_type_labels(
                allocator,
                try infer_expr_type_label(allocator, sigs, ink.ast.deref(bin.left)),
                try infer_expr_type_label(allocator, sigs, ink.ast.deref(bin.right)),
            ),
            .logical_and, .logical_or, .logical_xor, .less_than, .less_or_equal, .greater_than, .greater_or_equal, .equal, .not_equal => return .{ .text = "bool", .owned = false },
            .add, .sub, .mul, .div, .mod, .min, .max => {
                const left = try infer_expr_type_label(allocator, sigs, ink.ast.deref(bin.left));
                const right = try infer_expr_type_label(allocator, sigs, ink.ast.deref(bin.right));
                return merge_numeric_labels(allocator, left, right);
            },
            else => return null,
        },
        .if_expr => |ife| {
            if (ife.else_branch == null) return null;
            const then_label = try infer_expr_type_label(allocator, sigs, ink.ast.deref(ife.then_branch));
            const else_label = try infer_expr_type_label(allocator, sigs, ink.ast.deref(ife.else_branch.?));
            return merge_type_labels(allocator, then_label, else_label);
        },
        .match_expr => |me| {
            var base: ?type_label = null;
            for (me.arms) |arm| {
                const label = try infer_expr_type_label(allocator, sigs, ink.ast.deref(arm.body));
                if (label == null) {
                    if (base) |held| free_type_label(allocator, held);
                    return null;
                }
                if (base == null) {
                    base = label;
                } else if (!labels_equal(base.?, label.?)) {
                    free_type_label(allocator, base.?);
                    free_type_label(allocator, label.?);
                    return null;
                } else {
                    free_type_label(allocator, label.?);
                }
            }
            return base;
        },
        .select_expr => |se| {
            var base: ?type_label = null;
            for (se.arms) |arm| {
                const label = try infer_expr_type_label(allocator, sigs, ink.ast.deref(arm.body));
                if (label == null) {
                    if (base) |held| free_type_label(allocator, held);
                    return null;
                }
                if (base == null) {
                    base = label;
                } else if (!labels_equal(base.?, label.?)) {
                    free_type_label(allocator, base.?);
                    free_type_label(allocator, label.?);
                    return null;
                } else {
                    free_type_label(allocator, label.?);
                }
            }
            return base;
        },
        .block => |blk| {
            if (blk.items.len == 0) return null;
            const last = ink.ast.deref(blk.items[blk.items.len - 1]);
            return infer_expr_type_label(allocator, sigs, last);
        },
        else => return null,
    }
}

fn call_callee(node: *const ink.node) *const ink.node {
    var current = node;
    while (current.* == .binary and current.binary.op == .call) {
        current = ink.ast.deref(current.binary.left);
    }
    return current;
}

fn label_from_type_node(allocator: mem_allocator, node: *const ink.node) !type_label {
    const text = try format_type_node(allocator, node);
    return .{ .text = text, .owned = true };
}

fn format_type_node(allocator: mem_allocator, node: *const ink.node) ![]const u8 {
    var buf = std.array_list.Managed(u8).init(allocator);
    errdefer buf.deinit();
    try write_type_node(buf.writer(), node);
    return buf.toOwnedSlice();
}

fn merge_numeric_labels(
    allocator: mem_allocator,
    left: ?type_label,
    right: ?type_label,
) ?type_label {
    if (left == null or right == null) {
        if (left) |lbl| free_type_label(allocator, lbl);
        if (right) |lbl| free_type_label(allocator, lbl);
        return null;
    }
    if (!is_numeric_label(left.?) or !is_numeric_label(right.?)) {
        free_type_label(allocator, left.?);
        free_type_label(allocator, right.?);
        return null;
    }
    const result = if (is_float_label(left.?) or is_float_label(right.?))
        type_label{ .text = "float", .owned = false }
    else
        type_label{ .text = "int", .owned = false };
    free_type_label(allocator, left.?);
    free_type_label(allocator, right.?);
    return result;
}

fn merge_type_labels(
    allocator: mem_allocator,
    left: ?type_label,
    right: ?type_label,
) ?type_label {
    if (left == null or right == null) {
        if (left) |lbl| free_type_label(allocator, lbl);
        if (right) |lbl| free_type_label(allocator, lbl);
        return null;
    }
    if (!labels_equal(left.?, right.?)) {
        free_type_label(allocator, left.?);
        free_type_label(allocator, right.?);
        return null;
    }
    free_type_label(allocator, right.?);
    return left;
}

fn labels_equal(left: type_label, right: type_label) bool {
    return std.mem.eql(u8, left.text, right.text);
}

fn is_numeric_label(label: type_label) bool {
    return is_float_label(label) or std.mem.eql(u8, label.text, "int") or std.mem.eql(u8, label.text, "uint");
}

fn is_float_label(label: type_label) bool {
    return std.mem.eql(u8, label.text, "float");
}

fn free_type_label(allocator: mem_allocator, label: type_label) void {
    if (label.owned) allocator.free(label.text);
}

fn node_start_span(node: *const ink.node) ?source.span {
    switch (node.*) {
        .integer => |val| return .{ .start = val.where.start, .end = val.where.end },
        .float => |val| return .{ .start = val.where.start, .end = val.where.end },
        .duration => |val| return .{ .start = val.where.start, .end = val.where.end },
        .identifier => |id| return .{ .start = id.where.start, .end = id.where.end },
        .string => |id| return .{ .start = id.where.start, .end = id.where.end },
        .type => |ty| switch (ty) {
            .name => |id| return .{ .start = id.where.start, .end = id.where.end },
            .optional => |ref| return node_start_span(ink.ast.deref(ref)),
            .applied => |ap| {
                if (ap.base.where.end > ap.base.where.start) {
                    return .{ .start = ap.base.where.start, .end = ap.base.where.end };
                }
                if (ap.args.len != 0) {
                    return node_start_span(ink.ast.deref(ap.args[0]));
                }
            },
            else => {},
        },
        .binary => |bin| {
            return node_start_span(ink.ast.deref(bin.left)) orelse node_start_span(ink.ast.deref(bin.right));
        },
        .unary => |un| return node_start_span(ink.ast.deref(un.right)),
        .if_expr => |ife| return node_start_span(ink.ast.deref(ife.condition)),
        .match_expr => |me| return node_start_span(ink.ast.deref(me.target)),
        .select_expr => |se| {
            if (se.arms.len != 0) return node_start_span(ink.ast.deref(se.arms[0].task));
        },
        .block => |blk| {
            if (blk.items.len != 0) return node_start_span(ink.ast.deref(blk.items[0]));
        },
        .record => |rec| {
            if (rec.items.len != 0) {
                return .{
                    .start = rec.items[0].name.where.start,
                    .end = rec.items[0].name.where.end,
                };
            }
        },
        .associate => |assoc| return .{ .start = assoc.name.where.start, .end = assoc.name.where.end },
        .decl => |decl| switch (decl) {
            .function => |func| return .{ .start = func.name.where.start, .end = func.name.where.end },
            .@"struct" => |st| return .{ .start = st.name.where.start, .end = st.name.where.end },
            .trait => |tr| return .{ .start = tr.name.where.start, .end = tr.name.where.end },
            .@"enum" => |e| return .{ .start = e.name.where.start, .end = e.name.where.end },
            .type_alias => |t| return .{ .start = t.name.where.start, .end = t.name.where.end },
            .@"const" => |c| return .{ .start = c.name.where.start, .end = c.name.where.end },
            .@"var" => |v| return .{ .start = v.name.where.start, .end = v.name.where.end },
            else => {},
        },
        else => {},
    }
    return null;
}

fn builtin_hover_label(name: []const u8) ?[]const u8 {
    if (std.mem.eql(u8, name, "unit")) return "unit";
    if (std.mem.eql(u8, name, "true") or std.mem.eql(u8, name, "false")) return "bool";
    if (std.mem.eql(u8, name, "int")) return "int";
    if (std.mem.eql(u8, name, "uint")) return "uint";
    if (std.mem.eql(u8, name, "float")) return "float";
    if (std.mem.eql(u8, name, "bool")) return "bool";
    if (std.mem.eql(u8, name, "string")) return "string";
    if (std.mem.eql(u8, name, "none")) return "none";
    return null;
}

fn is_ident_byte(ch: u8) bool {
    return std.ascii.isAlphabetic(ch) or std.ascii.isDigit(ch) or ch == '_';
}

fn identifier_prefix(text: []const u8, offset: usize) ?[]const u8 {
    const limit = @min(offset, text.len);
    var start = limit;
    while (start > 0 and is_ident_byte(text[start - 1])) : (start -= 1) {}
    if (start == limit) return null;
    return text[start..limit];
}

fn identifier_span_at(text: []const u8, offset: usize) ?source.span {
    const limit = @min(offset, text.len);
    var start = limit;
    while (start > 0 and is_ident_byte(text[start - 1])) : (start -= 1) {}
    var end = limit;
    while (end < text.len and is_ident_byte(text[end])) : (end += 1) {}
    if (start == end) return null;
    return .{ .start = start, .end = end };
}

fn token_index_at_offset(tokens: []const ink.token, offset: usize) ?usize {
    var idx: ?usize = null;
    for (tokens, 0..) |tok, i| {
        if (tok.where.start <= offset) {
            idx = i;
            continue;
        }
        break;
    }
    return idx;
}

fn token_is_layout(kind: ink.token.kind) bool {
    return kind == .new_line or kind == .indent or kind == .dedent;
}

fn find_call_callee(tokens: []const ink.token, paren_idx: usize) ?signature_context {
    if (paren_idx == 0) return null;
    var idx = paren_idx;
    while (idx > 0) {
        idx -= 1;
        const tok = tokens[idx];
        if (token_is_layout(tok.which)) continue;
        if (tok.which == .identifier) {
            var is_method = false;
            if (idx > 0 and tokens[idx - 1].which == .dot) {
                is_method = true;
            }
            return .{ .name = tok.what.string, .arg_index = 0, .is_method = is_method };
        }
    }
    return null;
}

fn count_call_commas(tokens: []const ink.token, start_idx: usize, end_idx: usize) usize {
    if (start_idx > end_idx) return 0;
    var depth: i32 = 0;
    var count: usize = 0;
    var idx = start_idx;
    while (idx <= end_idx) : (idx += 1) {
        const tok = tokens[idx];
        switch (tok.which) {
            .paren_left, .bracket_left => depth += 1,
            .paren_right, .bracket_right => {
                if (depth > 0) depth -= 1;
            },
            .comma => {
                if (depth == 0) count += 1;
            },
            else => {},
        }
    }
    return count;
}

fn find_signature_context(tokens: []const ink.token, offset: usize) ?signature_context {
    const idx = token_index_at_offset(tokens, offset) orelse return null;
    var depth: i32 = 0;
    var i = idx;
    while (true) {
        const tok = tokens[i];
        switch (tok.which) {
            .paren_right => depth += 1,
            .paren_left => {
                if (depth == 0) {
                    const callee = find_call_callee(tokens, i) orelse return null;
                    const arg_index = count_call_commas(tokens, i + 1, idx);
                    return .{ .name = callee.name, .arg_index = arg_index, .is_method = callee.is_method };
                }
                depth -= 1;
            },
            else => {},
        }
        if (i == 0) break;
        i -= 1;
    }
    return null;
}

fn build_ast_cache(allocator: mem_allocator, text: []const u8) !ast_cache {
    const tokens = try lex_all(allocator, text);
    errdefer allocator.free(tokens);
    var parse = try ink.peg_parser.parse(allocator, tokens);
    var nodes: ?[]const *ink.node = null;
    var ast_error: ?ink.peg_ast.error_info = null;

    if (parse.ok) {
        var builder = ink.peg_ast.builder.init(parse.arena.allocator(), tokens, &parse.tree);
        const built = builder.build_program(parse.root.?) catch blk: {
            ast_error = builder.last_error;
            break :blk null;
        };
        nodes = built;
    }

    return .{
        .tokens = tokens,
        .parse = parse,
        .nodes = nodes,
        .ast_error = ast_error,
    };
}

fn lex_all(allocator: mem_allocator, source_text: []const u8) ![]const ink.token {
    var lexer = try ink.lexer.init(source_text);
    var tokens = std.array_list.Managed(ink.token).init(allocator);
    errdefer tokens.deinit();

    while (true) {
        const maybe_tok = lexer.next() catch break;
        if (maybe_tok) |tok| {
            try tokens.append(tok);
            if (tok.which == .end_of_file) break;
        } else break;
    }

    return tokens.toOwnedSlice();
}

fn span_from_token_index(tokens: []const ink.token, pos: usize) ?source.span {
    if (tokens.len == 0) return null;
    if (pos >= tokens.len) {
        const end = tokens[tokens.len - 1].where.end;
        return .{ .start = end, .end = end };
    }
    const tok = tokens[pos];
    return .{ .start = tok.where.start, .end = tok.where.end };
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
    var buf = std.array_list.Managed(u8).init(allocator);
    errdefer buf.deinit();
    const writer = buf.writer();

    if (looks_like_match_arm_error(tokens, info)) {
        try writer.writeAll("parse error: expected '=>' in match arm; use 'pattern => expr' (the '=>' can be on the next line after a multiline pattern)");
        return buf.toOwnedSlice();
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
    return buf.toOwnedSlice();
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
    var buf = std.array_list.Managed(u8).init(allocator);
    errdefer buf.deinit();
    const writer = buf.writer();

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
        .string_literal => try writer.writeAll("string literals are not supported here"),
        .invalid_duration_literal => try writer.writeAll("invalid duration literal"),
    }

    return buf.toOwnedSlice();
}

fn load_module_sources(
    allocator: mem_allocator,
    dir_path: []const u8,
    next_id: *ink.compiler.source_id,
    sources: *std.array_list.Managed(ink.compiler.source),
    allocated_paths: *std.array_list.Managed([]const u8),
) ![]ink.compiler.source_id {
    var ids = std.array_list.Managed(ink.compiler.source_id).init(allocator);
    errdefer ids.deinit();

    var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch |err| {
        if (err == error.FileNotFound) {
            return ids.toOwnedSlice();
        }
        return err;
    };
    defer dir.close();

    var it = dir.iterate();
    while (try it.next()) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".ink")) continue;
        const path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ dir_path, entry.name });
        try allocated_paths.append(path);
        const text = try std.fs.cwd().readFileAlloc(allocator, path, 1_000_000);
        const id = next_id.*;
        next_id.* += 1;
        try sources.append(.{ .id = id, .path = path, .text = text });
        try ids.append(id);
    }

    return ids.toOwnedSlice();
}

fn find_std_dir(allocator: mem_allocator, input_path: []const u8) !?[]const u8 {
    const abs_path = try std.fs.cwd().realpathAlloc(allocator, input_path);
    defer allocator.free(abs_path);

    var dir = std.fs.path.dirname(abs_path) orelse return null;
    while (true) {
        const candidate = try std.fmt.allocPrint(allocator, "{s}/std/src", .{dir});
        if (std.fs.cwd().openDir(candidate, .{})) |found_dir| {
            var dir_handle = found_dir;
            dir_handle.close();
            return candidate;
        } else |err| {
            if (err != error.FileNotFound and err != error.NotDir) {
                allocator.free(candidate);
                return err;
            }
        }
        allocator.free(candidate);
        const parent = std.fs.path.dirname(dir) orelse break;
        if (std.mem.eql(u8, parent, dir)) break;
        dir = parent;
    }

    return null;
}

fn path_from_uri(allocator: mem_allocator, uri: []const u8) !?[]const u8 {
    const prefix = "file://";
    if (!std.mem.startsWith(u8, uri, prefix)) return null;

    var path = uri[prefix.len..];
    if (std.mem.startsWith(u8, path, "localhost/")) {
        path = path["localhost".len..];
    }

    const decoded = try decode_percent(allocator, path);
    return @as(?[]const u8, decoded);
}

fn decode_percent(allocator: mem_allocator, input: []const u8) ![]const u8 {
    var out = std.array_list.Managed(u8).init(allocator);
    errdefer out.deinit();

    var i: usize = 0;
    while (i < input.len) {
        if (input[i] == '%' and i + 2 < input.len) {
            if (hex_value(input[i + 1])) |hi| {
                if (hex_value(input[i + 2])) |lo| {
                    try out.append((hi << 4) | lo);
                    i += 3;
                    continue;
                }
            }
        }
        try out.append(input[i]);
        i += 1;
    }

    return out.toOwnedSlice();
}

fn hex_value(c: u8) ?u8 {
    return switch (c) {
        '0'...'9' => c - '0',
        'a'...'f' => c - 'a' + 10,
        'A'...'F' => c - 'A' + 10,
        else => null,
    };
}

fn build_publish_diagnostics(
    allocator: mem_allocator,
    uri: []const u8,
    text: []const u8,
    source_id_filter: ?ink.compiler.source_id,
    diags: []const ink.diagnostic,
) ![]u8 {
    var allocating = std.Io.Writer.Allocating.init(allocator);
    errdefer allocating.deinit();

    const writer = &allocating.writer;
    try writer.writeAll("{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\",\"params\":{\"uri\":");
    try std.json.Stringify.value(uri, .{}, writer);
    try writer.writeAll(",\"diagnostics\":[");

    var wrote_any = false;
    for (diags) |diag| {
        if (source_id_filter != null and diag.source_id != null and diag.source_id.? != source_id_filter.?) {
            continue;
        }
        if (wrote_any) try writer.writeAll(",");
        try write_diag(writer, diag, text);
        wrote_any = true;
    }

    try writer.writeAll("]}}");
    const result = try allocating.toOwnedSlice();
    allocating.deinit();
    return result;
}

fn write_diag(writer: *std.Io.Writer, diag: ink.diagnostic, text: []const u8) !void {
    var start = position{ .line = 0, .character = 0 };
    var end = position{ .line = 0, .character = 0 };

    if (diag.span) |span| {
        const start_offset = @min(span.start, text.len);
        var end_offset = @min(span.end, text.len);
        if (end_offset < start_offset) end_offset = start_offset;

        start = position_from_offset(text, start_offset);
        end = position_from_offset(text, end_offset);
    }

    try writer.writeAll("{\"range\":{\"start\":{\"line\":");
    try writer.print("{d}", .{start.line});
    try writer.writeAll(",\"character\":");
    try writer.print("{d}", .{start.character});
    try writer.writeAll("},\"end\":{\"line\":");
    try writer.print("{d}", .{end.line});
    try writer.writeAll(",\"character\":");
    try writer.print("{d}", .{end.character});
    try writer.writeAll("}},\"severity\":");
    try writer.print("{d}", .{severity_to_lsp(diag.danger)});
    if (diag.code) |code| {
        try writer.writeAll(",\"code\":");
        try std.json.Stringify.value(code, .{}, writer);
    }
    try writer.writeAll(",\"message\":");
    try std.json.Stringify.value(diag.message, .{}, writer);
    try writer.writeAll("}");
}

fn severity_to_lsp(sev: ink.severity) u8 {
    return switch (sev) {
        .note => 3,
        .warn => 2,
        .@"error" => 1,
    };
}

fn position_from_offset(text: []const u8, offset: usize) position {
    var line: usize = 0;
    var line_start: usize = 0;
    var i: usize = 0;
    const limit = @min(offset, text.len);

    while (i < limit) : (i += 1) {
        if (text[i] == '\n') {
            line += 1;
            line_start = i + 1;
        }
    }

    const col = utf16_len(text[line_start..limit]);
    return .{ .line = line, .character = col };
}

fn utf16_len(slice: []const u8) usize {
    var count: usize = 0;
    var i: usize = 0;
    while (i < slice.len) {
        const first = slice[i];
        const seq_len = unicode.utf8ByteSequenceLength(first) catch {
            count += 1;
            i += 1;
            continue;
        };
        if (i + seq_len > slice.len) {
            count += 1;
            break;
        }
        const codepoint = unicode.utf8Decode(slice[i .. i + seq_len]) catch {
            count += 1;
            i += 1;
            continue;
        };
        if (codepoint >= 0x10000) {
            count += 2;
        } else {
            count += 1;
        }
        i += seq_len;
    }
    return count;
}

fn line_index_for_offset(line_offsets: []const usize, offset: usize) usize {
    if (line_offsets.len == 0) return 0;
    var lo: usize = 0;
    var hi: usize = line_offsets.len;
    while (lo + 1 < hi) {
        const mid = (lo + hi) / 2;
        if (line_offsets[mid] <= offset) {
            lo = mid;
        } else {
            hi = mid;
        }
    }
    return lo;
}

fn position_from_offset_with_lines(text: []const u8, line_offsets: []const usize, offset: usize) position {
    const clamped = @min(offset, text.len);
    const line_idx = line_index_for_offset(line_offsets, clamped);
    const line_start = if (line_idx < line_offsets.len) line_offsets[line_idx] else 0;
    const col = utf16_len(text[line_start..clamped]);
    return .{ .line = line_idx, .character = col };
}

fn offset_from_position(text: []const u8, line_offsets: []const usize, pos: position) usize {
    if (line_offsets.len == 0) return 0;
    var line = pos.line;
    if (line >= line_offsets.len) line = line_offsets.len - 1;
    const line_start = line_offsets[line];
    const line_end = if (line + 1 < line_offsets.len) line_offsets[line + 1] else text.len;
    const col_offset = byte_offset_from_utf16(text[line_start..line_end], pos.character);
    return line_start + col_offset;
}

fn byte_offset_from_utf16(slice: []const u8, utf16_col: usize) usize {
    var count: usize = 0;
    var i: usize = 0;
    while (i < slice.len and count < utf16_col) {
        const byte = slice[i];
        if (byte == '\n' or byte == '\r') break;
        const seq_len = unicode.utf8ByteSequenceLength(byte) catch {
            count += 1;
            i += 1;
            continue;
        };
        if (i + seq_len > slice.len) break;
        const codepoint = unicode.utf8Decode(slice[i .. i + seq_len]) catch {
            count += 1;
            i += 1;
            continue;
        };
        const step: usize = if (codepoint >= 0x10000) 2 else 1;
        if (count + step > utf16_col) break;
        count += step;
        i += seq_len;
    }
    return i;
}

fn get_field(value: std.json.Value, key: []const u8) ?std.json.Value {
    switch (value) {
        .object => |obj| return obj.get(key),
        else => return null,
    }
}

fn get_string_field(value: std.json.Value, key: []const u8) ?[]const u8 {
    const field = get_field(value, key) orelse return null;
    switch (field) {
        .string => |s| return s,
        else => return null,
    }
}

fn get_int_field(value: std.json.Value, key: []const u8) ?usize {
    const field = get_field(value, key) orelse return null;
    return switch (field) {
        .integer => |i| if (i < 0) null else @intCast(i),
        .float => |f| if (f < 0) null else @intFromFloat(f),
        else => null,
    };
}

fn get_bool_field(value: std.json.Value, key: []const u8) ?bool {
    const field = get_field(value, key) orelse return null;
    return switch (field) {
        .bool => |b| b,
        else => null,
    };
}

const range = struct {
    start: position,
    end: position,
};

fn parse_position(value: std.json.Value) ?position {
    const line = get_int_field(value, "line") orelse return null;
    const character = get_int_field(value, "character") orelse return null;
    return .{ .line = line, .character = character };
}

fn parse_range(value: std.json.Value) ?range {
    const start_val = get_field(value, "start") orelse return null;
    const end_val = get_field(value, "end") orelse return null;
    const start = parse_position(start_val) orelse return null;
    const end = parse_position(end_val) orelse return null;
    return .{ .start = start, .end = end };
}

fn log_message(
    log_file: *std.fs.File,
    log_writer: *std.Io.Writer,
    dir: direction,
    body: []const u8,
) !void {
    const prefix = if (dir == .client_to_server) "C->S" else "S->C";
    try log_writer.print("{s} {d}\n", .{ prefix, body.len });
    try log_writer.writeAll(body);
    try log_writer.writeAll("\n\n");
    try log_writer.flush();
    try log_file.sync();
}

fn read_message(allocator: mem_allocator, reader: *std.Io.Reader) !?[]u8 {
    var saw_any = false;
    var content_length: ?usize = null;

    while (true) {
        const line_opt = try reader.takeDelimiter('\n');
        if (line_opt == null) {
            if (!saw_any) return null;
            return error.unexpected_eof;
        }
        saw_any = true;

        const line = std.mem.trimRight(u8, line_opt.?, "\r");
        if (line.len == 0) break;

        if (std.ascii.startsWithIgnoreCase(line, "Content-Length:")) {
            const value = std.mem.trim(u8, line["Content-Length:".len..], " \t");
            content_length = std.fmt.parseInt(usize, value, 10) catch return error.invalid_content_length;
        }
    }

    if (content_length == null) return error.missing_content_length;

    const len = content_length.?;
    const body = try allocator.alloc(u8, len);
    errdefer allocator.free(body);

    try reader.readSliceAll(body);
    return body;
}

fn write_message(writer: *std.Io.Writer, body: []const u8) !void {
    try writer.print("Content-Length: {d}\r\n\r\n", .{body.len});
    try writer.writeAll(body);
}

const cli_options = struct {
    log_path: ?[]const u8 = null,
    target: ink.target.target_spec = .{ .kind = .vm },
};

fn parse_cli_options(args: []const []const u8) cli_options {
    var opts: cli_options = .{};
    var i: usize = 0;
    while (i + 1 < args.len) : (i += 1) {
        if (std.mem.eql(u8, args[i], "--log")) {
            opts.log_path = args[i + 1];
            i += 1;
            continue;
        }
        if (std.mem.eql(u8, args[i], "--target") or std.mem.eql(u8, args[i], "-t")) {
            opts.target = ink.target.parse_target(args[i + 1]) catch {
                var err_buf: [256]u8 = undefined;
                var err_writer = std.fs.File.stderr().writer(&err_buf);
                err_writer.interface.print("error: invalid target: {s}\n", .{args[i + 1]}) catch {};
                err_writer.interface.flush() catch {};
                std.process.exit(1);
            };
            i += 1;
            continue;
        }
    }
    return opts;
}
