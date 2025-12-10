const ink = @import("ink");
const std = @import("std");

pub fn print_identifier_string(id: ink.identifier) void {
    std.debug.print("Identifier: {s}\\n", .{id.string});
}

