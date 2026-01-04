const std = @import("std");
const array_list = std.array_list.Managed;

pub const Module = struct {
    name: []const u8,
    sources: []const u8,
};

pub const Registry = struct {
    name: []const u8,
    url: []const u8,
};

pub const Dep = struct {
    name: []const u8,
    registry: []const u8,
    path: []const u8,
};

pub const Manifest = struct {
    name: []const u8,
    version: []const u8,
    registry: []const u8,
    root_dir: []const u8,
    modules: []const Module,
    registries: []const Registry,
    deps: []const Dep,
    arena: std.heap.ArenaAllocator,

    pub fn deinit(self: *Manifest) void {
        self.arena.deinit();
    }
};

pub const ParseError = error{
    MissingName,
    MissingVersion,
    MissingModuleSources,
    MissingDepPath,
    MissingRegistryName,
    MissingRegistryUrl,
    InvalidManifest,
};

const ContextKind = enum { root, package, module, registries, registry, deps, dep };

const Context = struct {
    kind: ContextKind,
    indent: usize,
    index: usize,
};

pub fn parse(allocator: std.mem.Allocator, manifest_path: []const u8) !Manifest {
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();
    const a = arena.allocator();

    const abs_manifest_path = try std.fs.cwd().realpathAlloc(a, manifest_path);
    const root_dir = std.fs.path.dirname(abs_manifest_path) orelse ".";

    const text = try std.fs.cwd().readFileAlloc(a, abs_manifest_path, 1_000_000);

    var modules = array_list(Module).init(a);
    var registries = array_list(Registry).init(a);
    var deps = array_list(Dep).init(a);

    var name: ?[]const u8 = null;
    var version: ?[]const u8 = null;
    var default_registry: ?[]const u8 = null;

    var stack = array_list(Context).init(a);
    try stack.append(.{ .kind = .root, .indent = 0, .index = 0 });

    var line_iter = std.mem.splitScalar(u8, text, '\n');
    while (line_iter.next()) |raw_line| {
        const line = std.mem.trimRight(u8, raw_line, "\r");
        const trimmed = std.mem.trimLeft(u8, line, " \t");
        if (trimmed.len == 0) continue;
        if (std.mem.startsWith(u8, trimmed, "//") or std.mem.startsWith(u8, trimmed, "#")) continue;

        const indent = count_indent(line);
        while (stack.items.len > 1 and indent <= stack.items[stack.items.len - 1].indent) {
            _ = stack.pop();
        }
        const ctx = stack.items[stack.items.len - 1];

        if (std.mem.eql(u8, trimmed, "build::package")) {
            try stack.append(.{ .kind = .package, .indent = indent, .index = 0 });
            continue;
        }

        if (parse_assignment(trimmed)) |assign| {
            if (std.mem.eql(u8, assign.value, "build::module")) {
                try modules.append(.{ .name = "", .sources = "" });
                try stack.append(.{ .kind = .module, .indent = indent, .index = modules.items.len - 1 });
                continue;
            }
            if (std.mem.eql(u8, assign.value, "build::registries")) {
                try stack.append(.{ .kind = .registries, .indent = indent, .index = 0 });
                continue;
            }
            if (std.mem.eql(u8, assign.value, "build::registry")) {
                try registries.append(.{ .name = "", .url = "" });
                try stack.append(.{ .kind = .registry, .indent = indent, .index = registries.items.len - 1 });
                continue;
            }
            if (std.mem.eql(u8, assign.value, "build::deps")) {
                try stack.append(.{ .kind = .deps, .indent = indent, .index = 0 });
                continue;
            }
            if (std.mem.eql(u8, assign.value, "build::dep")) {
                try deps.append(.{ .name = "", .registry = "", .path = "" });
                try stack.append(.{ .kind = .dep, .indent = indent, .index = deps.items.len - 1 });
                continue;
            }

            switch (ctx.kind) {
                .package => {
                    if (std.mem.eql(u8, assign.key, "name")) {
                        name = assign.value;
                    } else if (std.mem.eql(u8, assign.key, "version")) {
                        version = assign.value;
                    } else if (std.mem.eql(u8, assign.key, "registry")) {
                        default_registry = assign.value;
                    }
                },
                .module => {
                    if (ctx.index < modules.items.len) {
                        if (std.mem.eql(u8, assign.key, "name")) {
                            modules.items[ctx.index].name = assign.value;
                        } else if (std.mem.eql(u8, assign.key, "sources")) {
                            modules.items[ctx.index].sources = assign.value;
                        }
                    }
                },
                .registry => {
                    if (ctx.index < registries.items.len) {
                        if (std.mem.eql(u8, assign.key, "name")) {
                            registries.items[ctx.index].name = assign.value;
                        } else if (std.mem.eql(u8, assign.key, "url")) {
                            registries.items[ctx.index].url = assign.value;
                        }
                    }
                },
                .dep => {
                    if (ctx.index < deps.items.len) {
                        if (std.mem.eql(u8, assign.key, "name")) {
                            deps.items[ctx.index].name = assign.value;
                        } else if (std.mem.eql(u8, assign.key, "registry")) {
                            deps.items[ctx.index].registry = assign.value;
                        } else if (std.mem.eql(u8, assign.key, "path")) {
                            deps.items[ctx.index].path = assign.value;
                        }
                    }
                },
                else => {},
            }
        }
    }

    if (name == null) return error.MissingName;
    if (version == null) return error.MissingVersion;

    if (modules.items.len == 0) {
        try modules.append(.{ .name = name.?, .sources = "src" });
    } else {
        for (modules.items) |m| {
            if (m.sources.len == 0) return error.MissingModuleSources;
            if (m.name.len == 0) return error.InvalidManifest;
        }
    }

    for (registries.items) |r| {
        if (r.name.len == 0) return error.MissingRegistryName;
        if (r.url.len == 0) return error.MissingRegistryUrl;
    }

    for (deps.items) |d| {
        if (d.name.len == 0) return error.InvalidManifest;
        if (d.path.len == 0 and d.registry.len == 0 and std.mem.indexOf(u8, d.name, "::") == null and default_registry == null) {
            return error.MissingDepPath;
        }
    }

    return .{
        .name = name.?,
        .version = version.?,
        .registry = default_registry orelse "ink",
        .root_dir = root_dir,
        .modules = try modules.toOwnedSlice(),
        .registries = try registries.toOwnedSlice(),
        .deps = try deps.toOwnedSlice(),
        .arena = arena,
    };
}

const Assignment = struct {
    key: []const u8,
    value: []const u8,
};

fn parse_assignment(line: []const u8) ?Assignment {
    const eq = std.mem.indexOfScalar(u8, line, '=') orelse return null;
    const key = std.mem.trim(u8, line[0..eq], " \t");
    const raw_val = std.mem.trim(u8, line[eq + 1 ..], " \t");
    if (key.len == 0 or raw_val.len == 0) return null;
    return .{ .key = key, .value = parse_value(raw_val) };
}

fn parse_value(raw_val: []const u8) []const u8 {
    if (raw_val.len >= 2 and raw_val[0] == '"' and raw_val[raw_val.len - 1] == '"') {
        return raw_val[1 .. raw_val.len - 1];
    }
    return raw_val;
}

fn count_indent(line: []const u8) usize {
    var count: usize = 0;
    while (count < line.len) : (count += 1) {
        const ch = line[count];
        if (ch != ' ' and ch != '\t') break;
    }
    return count;
}
