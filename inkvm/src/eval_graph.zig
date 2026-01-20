const std = @import("std");
const ink = @import("ink");
const manifest = @import("eval_manifest.zig");

const mem_allocator = std.mem.Allocator;
const array_list = std.array_list.Managed;

pub const Graph = struct {
    allocator: mem_allocator,
    sources: array_list(ink.compiler.source),
    modules: array_list(ink.compiler.module_spec),
    source_slices: array_list([]ink.compiler.source_id),
    string_slices: array_list([]const []const u8),
    owned_strings: array_list([]const u8),
    owned_paths: array_list([]const u8),
    root_module_indices: array_list(usize),
    root_module: []const u8,
    prelude: ?ink.desugar.prelude_spec,
    default_registry: []const u8,
    registries: array_list(manifest.Registry),
    profiles: array_list(manifest.Profile),
    next_source_id: ink.compiler.source_id,

    pub fn init(allocator: mem_allocator) Graph {
        return .{
            .allocator = allocator,
            .sources = array_list(ink.compiler.source).init(allocator),
            .modules = array_list(ink.compiler.module_spec).init(allocator),
            .source_slices = array_list([]ink.compiler.source_id).init(allocator),
            .string_slices = array_list([]const []const u8).init(allocator),
            .owned_strings = array_list([]const u8).init(allocator),
            .owned_paths = array_list([]const u8).init(allocator),
            .root_module_indices = array_list(usize).init(allocator),
            .root_module = "",
            .prelude = null,
            .default_registry = "",
            .registries = array_list(manifest.Registry).init(allocator),
            .profiles = array_list(manifest.Profile).init(allocator),
            .next_source_id = 0,
        };
    }

    pub fn deinit(self: *Graph) void {
        for (self.sources.items) |src| {
            self.allocator.free(src.text);
        }
        for (self.source_slices.items) |slice| self.allocator.free(slice);
        for (self.string_slices.items) |slice| self.allocator.free(slice);
        for (self.owned_strings.items) |s| self.allocator.free(s);
        for (self.owned_paths.items) |p| self.allocator.free(p);
        self.sources.deinit();
        self.modules.deinit();
        self.source_slices.deinit();
        self.string_slices.deinit();
        self.owned_strings.deinit();
        self.owned_paths.deinit();
        self.root_module_indices.deinit();
        self.registries.deinit();
        self.profiles.deinit();
    }
};

pub fn build(allocator: mem_allocator, root_dir: []const u8) !Graph {
    var graph = Graph.init(allocator);
    errdefer graph.deinit();

    var module_names = std.StringHashMap(void).init(allocator);
    defer module_names.deinit();
    var package_modules = std.StringHashMap([]const []const u8).init(allocator);
    defer package_modules.deinit();

    const abs_root = try std.fs.cwd().realpathAlloc(allocator, root_dir);
    try graph.owned_paths.append(abs_root);

    _ = try load_package(&graph, abs_root, &package_modules, &module_names, true);

    if (!module_names.contains("std")) {
        if (try find_std_dir(allocator, abs_root)) |std_dir| {
            defer allocator.free(std_dir);
            try add_std_module(&graph, std_dir, &module_names);
            try add_std_dep(&graph);
        }
    }

    return graph;
}

fn load_package(
    graph: *Graph,
    abs_dir: []const u8,
    package_modules: *std.StringHashMap([]const []const u8),
    module_names: *std.StringHashMap(void),
    is_root: bool,
) ![]const []const u8 {
    if (package_modules.get(abs_dir)) |existing| return existing;

    const manifest_path = try alloc_path(graph, &.{ abs_dir, "package.ink" });
    var man = try manifest.parse(graph.allocator, manifest_path);
    defer man.deinit();

    if (is_root) {
        graph.default_registry = try dupe_string(graph, man.registry);
        for (man.registries) |reg| {
            try graph.registries.append(.{
                .name = try dupe_string(graph, reg.name),
                .url = try dupe_string(graph, reg.url),
            });
        }
        for (man.profiles) |profile| {
            try graph.profiles.append(.{
                .name = try dupe_string(graph, profile.name),
                .target = try dupe_string(graph, profile.target),
                .opt = try dupe_string(graph, profile.opt),
                .debug_info = try dupe_string(graph, profile.debug_info),
                .prelude = try dupe_string(graph, profile.prelude),
                .sandbox = profile.sandbox,
            });
        }
        if (man.prelude) |prelude| {
            const std_items = try dupe_string_list(graph, prelude.std_items);
            const std_scopes = try dupe_string_list(graph, prelude.std_scopes);
            graph.prelude = .{
                .std_items = std_items,
                .std_scopes = std_scopes,
            };
        }
    }

    var dep_names = std.StringHashMap(void).init(graph.allocator);
    defer dep_names.deinit();

    for (man.deps) |dep| {
        if (dep.path.len != 0) {
            const dep_dir = try resolve_path(graph, abs_dir, dep.path);
            const dep_module_names = try load_package(graph, dep_dir, package_modules, module_names, false);
            for (dep_module_names) |mod_name| {
                _ = try dep_names.put(mod_name, {});
            }
            continue;
        }

        const registry_name = resolve_dep_registry(dep, man.registry);
        if (registry_name.len == 0) return error.MissingDepPath;
        return error.RegistryUnavailable;
    }

    var deps_list = array_list([]const u8).init(graph.allocator);
    defer deps_list.deinit();
    var dep_it = dep_names.iterator();
    while (dep_it.next()) |entry| {
        try deps_list.append(entry.key_ptr.*);
    }
    std.mem.sort([]const u8, deps_list.items, {}, string_less_than);
    const deps_slice = try deps_list.toOwnedSlice();
    try graph.string_slices.append(deps_slice);

    var pkg_module_names = array_list([]const u8).init(graph.allocator);
    defer pkg_module_names.deinit();

    for (man.modules) |mod| {
        const mod_name = try dupe_string(graph, mod.name);
        try pkg_module_names.append(mod_name);
        if (module_names.contains(mod_name)) return error.DuplicateModuleName;
        try module_names.put(mod_name, {});

        const mod_sources_path = try resolve_path(graph, abs_dir, mod.sources);
        const sources = try load_module_sources(graph, mod_sources_path);
        const module_spec = ink.compiler.module_spec{
            .name = mod_name,
            .sources = sources,
            .deps = deps_slice,
        };
        try graph.modules.append(module_spec);
        if (is_root) {
            if (graph.root_module.len == 0) {
                graph.root_module = mod_name;
            }
            try graph.root_module_indices.append(graph.modules.items.len - 1);
        }
    }

    const pkg_module_slice = try pkg_module_names.toOwnedSlice();
    try graph.string_slices.append(pkg_module_slice);
    try package_modules.put(abs_dir, pkg_module_slice);
    return pkg_module_slice;
}

fn resolve_dep_registry(dep: manifest.Dep, default_registry: []const u8) []const u8 {
    if (dep.registry.len != 0) return dep.registry;
    if (std.mem.indexOf(u8, dep.name, "::")) |idx| {
        return dep.name[0..idx];
    }
    return default_registry;
}

fn add_std_module(
    graph: *Graph,
    std_dir: []const u8,
    module_names: *std.StringHashMap(void),
) !void {
    const name = try dupe_string(graph, "std");
    if (module_names.contains(name)) return;
    try module_names.put(name, {});

    const sources = try load_module_sources(graph, std_dir);
    const deps = &[_][]const u8{};
    const module_spec = ink.compiler.module_spec{
        .name = name,
        .sources = sources,
        .deps = deps,
    };
    try graph.modules.append(module_spec);
}

fn add_std_dep(graph: *Graph) !void {
    for (graph.root_module_indices.items) |idx| {
        const current = graph.modules.items[idx].deps;
        if (contains_name(current, "std")) continue;
        const expanded = try graph.allocator.alloc([]const u8, current.len + 1);
        std.mem.copyForwards([]const u8, expanded[0..current.len], current);
        expanded[current.len] = "std";
        graph.modules.items[idx].deps = expanded;
        try graph.string_slices.append(expanded);
    }
}

fn load_module_sources(graph: *Graph, path: []const u8) ![]ink.compiler.source_id {
    const stat = std.fs.cwd().statFile(path) catch |err| {
        if (err == error.FileNotFound or err == error.NotDir) return error.MissingSources;
        return err;
    };

    var ids = array_list(ink.compiler.source_id).init(graph.allocator);
    errdefer ids.deinit();

    if (stat.kind == .file) {
        if (!std.mem.endsWith(u8, path, ".ink")) return error.InvalidSources;
        try load_source_file(graph, path, &ids);
        const slice = try ids.toOwnedSlice();
        try graph.source_slices.append(slice);
        return slice;
    }

    if (stat.kind != .directory) return error.InvalidSources;
    var dir = try std.fs.cwd().openDir(path, .{ .iterate = true });
    defer dir.close();
    var it = dir.iterate();
    while (try it.next()) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".ink")) continue;
        const file_path = try alloc_path(graph, &.{ path, entry.name });
        try load_source_file(graph, file_path, &ids);
    }
    const slice = try ids.toOwnedSlice();
    try graph.source_slices.append(slice);
    return slice;
}

fn load_source_file(graph: *Graph, path: []const u8, ids: *array_list(ink.compiler.source_id)) !void {
    const text = try std.fs.cwd().readFileAlloc(graph.allocator, path, 1_000_000);
    const source_id = graph.next_source_id;
    graph.next_source_id +%= 1;
    try ids.append(source_id);
    try graph.sources.append(.{ .id = source_id, .path = path, .text = text });
}

fn find_std_dir(allocator: mem_allocator, root_dir: []const u8) !?[]const u8 {
    var dir = root_dir;
    while (true) {
        const candidate = try std.fs.path.join(allocator, &.{ dir, "std", "src" });
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

fn contains_name(list: []const []const u8, name: []const u8) bool {
    for (list) |item| {
        if (std.mem.eql(u8, item, name)) return true;
    }
    return false;
}

fn alloc_path(graph: *Graph, parts: []const []const u8) ![]const u8 {
    const full = try std.fs.path.join(graph.allocator, parts);
    try graph.owned_paths.append(full);
    return full;
}

fn resolve_path(graph: *Graph, base: []const u8, rel: []const u8) ![]const u8 {
    if (std.fs.path.isAbsolute(rel)) return alloc_path(graph, &.{rel});
    return alloc_path(graph, &.{ base, rel });
}

fn dupe_string(graph: *Graph, text: []const u8) ![]const u8 {
    const duped = try graph.allocator.dupe(u8, text);
    try graph.owned_strings.append(duped);
    return duped;
}

fn dupe_string_list(graph: *Graph, list: []const []const u8) ![]const []const u8 {
    const slice = try graph.allocator.alloc([]const u8, list.len);
    for (list, 0..) |item, idx| {
        slice[idx] = try dupe_string(graph, item);
    }
    try graph.string_slices.append(slice);
    return slice;
}

fn string_less_than(_: void, lhs: []const u8, rhs: []const u8) bool {
    return std.mem.lessThan(u8, lhs, rhs);
}
