const std = @import("std");
const source = @import("source.zig");
const array_list = std.array_list.Managed;
const mem_allocator = std.mem.Allocator;

pub const severity = enum { note, warn, @"error" };

pub const diagnostic = struct {
    danger: severity,
    message: []const u8,
    span: ?source.span = null,
    code: ?[]const u8 = null,
    source_id: ?source.source_id = null,
};

pub const bag = struct {
    allocator: mem_allocator,
    items: array_list(diagnostic),

    pub fn init(allocator: std.mem.Allocator) bag {
        return .{ .allocator = allocator, .items = std.array_list.Managed(diagnostic).init(allocator) };
    }

    pub fn deinit(self: *bag) void {
        self.items.deinit();
    }

    pub fn add(self: *bag, diag: diagnostic) !void {
        try self.items.append(diag);
    }
};
