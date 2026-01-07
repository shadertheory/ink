const std = @import("std");

pub const type_key = union(enum) {
    unknown: void,
    name: []const u8,
    dyn_trait: []const u8,
    applied: struct {
        base: []const u8,
        args: []const type_key,
    },
};

pub fn type_key_eq(a: type_key, b: type_key) bool {
    return switch (a) {
        .unknown => false,
        .name => |name_a| switch (b) {
            .unknown => false,
            .name => |name_b| std.mem.eql(u8, name_a, name_b),
            .dyn_trait => false,
            .applied => false,
        },
        .dyn_trait => |name_a| switch (b) {
            .dyn_trait => |name_b| std.mem.eql(u8, name_a, name_b),
            else => false,
        },
        .applied => |ap_a| switch (b) {
            .applied => |ap_b| blk: {
                if (!std.mem.eql(u8, ap_a.base, ap_b.base)) break :blk false;
                if (ap_a.args.len != ap_b.args.len) break :blk false;
                for (ap_a.args, 0..) |arg, idx| {
                    if (!type_key_eq(arg, ap_b.args[idx])) break :blk false;
                }
                break :blk true;
            },
            else => false,
        },
    };
}

pub fn type_key_base_name(key: type_key) ?[]const u8 {
    return switch (key) {
        .unknown => null,
        .name => |name| name,
        .dyn_trait => |name| name,
        .applied => |ap| ap.base,
    };
}
