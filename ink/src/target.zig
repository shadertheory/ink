const std = @import("std");

const mem_allocator = std.mem.Allocator;

pub const target_kind = enum {
    vm,
    native,
};

pub const target_triple = struct {
    arch: []const u8,
    vendor: []const u8,
    os: []const u8,
    abi: ?[]const u8 = null,
};

pub const target_spec = struct {
    kind: target_kind,
    triple: ?target_triple = null,
    cpu: ?[]const u8 = null,
    features: ?[]const u8 = null,
};

pub const parse_error = error{
    invalid_target,
};

pub fn default_vm() target_spec {
    return .{ .kind = .vm };
}

pub fn parse_target(text: []const u8) parse_error!target_spec {
    if (text.len == 0) return error.invalid_target;
    if (std.mem.eql(u8, text, "vm") or std.mem.eql(u8, text, "inkvm") or std.mem.eql(u8, text, "inkb")) {
        return default_vm();
    }

    var parts: [4][]const u8 = undefined;
    var count: usize = 0;
    var it = std.mem.splitScalar(u8, text, '-');
    while (it.next()) |part| {
        if (part.len == 0) return error.invalid_target;
        if (count >= parts.len) return error.invalid_target;
        parts[count] = part;
        count += 1;
    }

    if (count < 3) return error.invalid_target;

    for (parts[0..count]) |part| {
        if (std.mem.startsWith(u8, part, "inkvm") or std.mem.eql(u8, part, "inkb")) {
            return error.invalid_target;
        }
    }

    return .{
        .kind = .native,
        .triple = .{
            .arch = parts[0],
            .vendor = parts[1],
            .os = parts[2],
            .abi = if (count > 3) parts[3] else null,
        },
    };
}

pub fn format_target(allocator: mem_allocator, target: target_spec) ![]const u8 {
    if (target.kind == .vm) {
        return allocator.dupe(u8, "inkvm");
    }
    const triple = target.triple orelse return allocator.dupe(u8, "unknown");
    if (triple.abi) |abi| {
        return std.fmt.allocPrint(allocator, "{s}-{s}-{s}-{s}", .{ triple.arch, triple.vendor, triple.os, abi });
    }
    return std.fmt.allocPrint(allocator, "{s}-{s}-{s}", .{ triple.arch, triple.vendor, triple.os });
}
