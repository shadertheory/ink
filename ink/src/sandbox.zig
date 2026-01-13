const std = @import("std");
const ast = @import("lang/ast.zig");
const diag_mod = @import("diagnostic.zig");
const source = @import("source.zig");

pub const Level = enum {
    basic,
    strict,
    paranoid,
};

pub const Checks = struct {
    macro_streams: bool = false,
    macro_ast: bool = false,
    ast: bool = false,
    uir: bool = false,
    mir: bool = false,
    lir: bool = false,
};

pub const Mode = enum {
    sandbox,
    sim,
};

pub const Config = struct {
    level: Level = .basic,
    checks: ?Checks = null,
    mode: Mode = .sandbox,
    allow_categories: ?[]const []const u8 = null,
    deny_categories: ?[]const []const u8 = null,
    allow_foreigns: ?[]const []const u8 = null,
    deny_foreigns: ?[]const []const u8 = null,
};

pub const SandboxAttr = struct {
    allowed: bool,
    category: ?[]const u8,
};

pub fn checks_for_level(level: Level) Checks {
    return switch (level) {
        .basic => .{
            .macro_streams = true,
            .macro_ast = true,
            .ast = true,
        },
        .strict => .{
            .macro_streams = true,
            .macro_ast = true,
            .ast = true,
            .uir = true,
            .mir = true,
        },
        .paranoid => .{
            .macro_streams = true,
            .macro_ast = true,
            .ast = true,
            .uir = true,
            .mir = true,
            .lir = true,
        },
    };
}

pub fn resolve_checks(config: Config) Checks {
    if (config.checks) |checks| return checks;
    return checks_for_level(config.level);
}

pub fn find_attr_named(attrs: []const ast.attribute, name: []const u8) ?SandboxAttr {
    for (attrs) |attr| {
        if (!std.mem.eql(u8, attr.name.string, name)) continue;
        return parse_attr(attr);
    }
    return null;
}

pub fn find_attr(attrs: []const ast.attribute) ?SandboxAttr {
    return find_attr_named(attrs, "sandbox");
}

pub fn validate_foreigns(
    nodes: []const *ast.node,
    node_sources: []const source.source_id,
    diags: *std.array_list.Managed(diag_mod.diagnostic),
    config: Config,
) !void {
    for (nodes, 0..) |node, idx| {
        if (node.* != .decl) continue;
        if (node.decl != .function) continue;
        const func = node.decl.function;
        if (!has_attribute(func.attributes, "foreign")) continue;

        const span = source.span{ .start = func.name.where.start, .end = func.name.where.end };
        const source_id = if (idx < node_sources.len) node_sources[idx] else null;
        const attr_name = if (config.mode == .sim) "sim" else "sandbox";
        const sandbox_attr = find_attr_named(func.attributes, attr_name);
        if (sandbox_attr == null) {
            try diags.append(.{
                .danger = .@"error",
                .message = if (config.mode == .sim)
                    "foreign function missing sim attribute"
                else
                    "foreign function missing sandbox attribute",
                .span = span,
                .source_id = source_id,
            });
            continue;
        }
        const attr = sandbox_attr.?;
        if (!attr.allowed) {
            try diags.append(.{
                .danger = .@"error",
                .message = if (config.mode == .sim)
                    "foreign function not allowed in sim"
                else
                    "foreign function not allowed in sandbox",
                .span = span,
                .source_id = source_id,
            });
            continue;
        }
        if (attr.category == null or attr.category.?.len == 0) {
            try diags.append(.{
                .danger = .@"error",
                .message = if (config.mode == .sim)
                    "sim attribute missing category"
                else
                    "sandbox attribute missing category",
                .span = span,
                .source_id = source_id,
            });
            continue;
        }
        if (config.allow_categories != null or config.allow_foreigns != null) {
            var allowed = false;
            if (config.allow_categories) |cats| {
                if (in_list(cats, attr.category.?)) allowed = true;
            }
            if (config.allow_foreigns) |names| {
                if (in_list(names, func.name.string)) allowed = true;
            }
            if (!allowed) {
                try diags.append(.{
                    .danger = .@"error",
                    .message = if (config.mode == .sim)
                        "foreign function not in sim allowlist"
                    else
                        "foreign function not in sandbox allowlist",
                    .span = span,
                    .source_id = source_id,
                });
                continue;
            }
        }
        if (config.deny_categories) |denied| {
            if (in_list(denied, attr.category.?)) {
                try diags.append(.{
                    .danger = .@"error",
                    .message = if (config.mode == .sim)
                        "foreign function category denied in sim"
                    else
                        "foreign function category denied in sandbox",
                    .span = span,
                    .source_id = source_id,
                });
                continue;
            }
        }
        if (config.deny_foreigns) |denied| {
            if (in_list(denied, func.name.string)) {
                try diags.append(.{
                    .danger = .@"error",
                    .message = "foreign function in denylist",
                    .span = span,
                    .source_id = source_id,
                });
                continue;
            }
        }
    }
}

fn has_attribute(attrs: []const ast.attribute, name: []const u8) bool {
    for (attrs) |attr| {
        if (std.mem.eql(u8, attr.name.string, name)) return true;
    }
    return false;
}

fn in_list(list: []const []const u8, value: []const u8) bool {
    for (list) |item| {
        if (std.mem.eql(u8, item, value)) return true;
    }
    return false;
}

fn parse_attr(attr: ast.attribute) SandboxAttr {
    var allowed = false;
    var category: ?[]const u8 = null;
    if (attr.args) |args| {
        var iter = std.mem.tokenizeAny(u8, args.string, " \t,");
        while (iter.next()) |raw| {
            const token = std.mem.trim(u8, raw, " \t\r\n\"");
            if (token.len == 0) continue;
            if (std.mem.eql(u8, token, "allowed") or std.mem.eql(u8, token, "allow")) {
                allowed = true;
                continue;
            }
            if (std.mem.eql(u8, token, "deny") or std.mem.eql(u8, token, "denied")) {
                allowed = false;
                continue;
            }
            if (std.mem.startsWith(u8, token, "category=")) {
                category = token["category=".len..];
                continue;
            }
            if (std.mem.startsWith(u8, token, "category:")) {
                category = token["category:".len..];
                continue;
            }
            if (category == null) category = token;
        }
    }
    return .{ .allowed = allowed, .category = category };
}
