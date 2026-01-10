const std = @import("std");
const backend = @import("backend.zig");
const lir_vm = @import("../lir/vm/core.zig");
const encode = @import("../vm/encode.zig");
const inkb = @import("../vm/inkb.zig");

const mem_allocator = std.mem.Allocator;

pub const emit_error = encode.encode_error;

pub fn emit(
    allocator: mem_allocator,
    bundle: lir_vm.bundle,
    foreigns: []const []const u8,
) emit_error!backend.backend_result {
    const data_entries = try build_data_entries(allocator, bundle.strings);
    errdefer free_data_entries(allocator, data_entries);

    const bytecode = try encode.encode(allocator, bundle.program.instructions);

    return .{
        .instructions = bundle.program.instructions,
        .constants = bundle.program.constants,
        .data = data_entries,
        .bytecode = bytecode,
        .foreigns = foreigns,
    };
}

fn free_data_entries(allocator: mem_allocator, data: []const inkb.data_entry) void {
    for (data) |entry| allocator.free(entry.bytes);
    allocator.free(data);
}

fn build_data_entries(
    allocator: mem_allocator,
    strings: []const []const u8,
) ![]const inkb.data_entry {
    const entries = try allocator.alloc(inkb.data_entry, strings.len);
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
