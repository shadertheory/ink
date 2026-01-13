const std = @import("std");
const target_mod = @import("../target.zig");
const mir_core = @import("../mir/core.zig");
const type_key_mod = @import("../type_key.zig");
const vm_core = @import("vm/core.zig");
const vm_lower = @import("vm/lower.zig");

pub const lower_error = vm_lower.lower_error || error{unsupported_target};
pub const error_info = vm_lower.error_info;

pub const lower_result = union(enum) {
    vm: vm_core.bundle,
};

pub fn lower(
    allocator: std.mem.Allocator,
    target: target_mod.target_spec,
    nodes: []const mir_core.mir,
    strings: []const []const u8,
    roots: []const mir_core.mir_identifier,
    foreigns: []const []const u8,
    node_types: ?[]const type_key_mod.type_key,
    info: ?*error_info,
) lower_error!lower_result {
    return lower_with_options(allocator, target, nodes, strings, roots, foreigns, node_types, info, null);
}

pub fn lower_with_options(
    allocator: std.mem.Allocator,
    target: target_mod.target_spec,
    nodes: []const mir_core.mir,
    strings: []const []const u8,
    roots: []const mir_core.mir_identifier,
    foreigns: []const []const u8,
    node_types: ?[]const type_key_mod.type_key,
    info: ?*error_info,
    options: ?vm_lower.lower_options,
) lower_error!lower_result {
    return switch (target.kind) {
        .vm => blk: {
            const program = if (options) |opts|
                try vm_lower.lower_with_options(allocator, nodes, strings, roots, foreigns, node_types, info, opts)
            else
                try vm_lower.lower(allocator, nodes, strings, roots, foreigns, node_types, info);
            break :blk .{ .vm = .{ .program = program, .strings = strings } };
        },
        else => error.unsupported_target,
    };
}
