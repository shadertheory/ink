const std = @import("std");
const mem_allocator = std.mem.Allocator;
const string_map = std.hash_map.StringHashMap;

const lsp = struct {
    allocator: mem_allocator,
    documents: string_map([]const u8) = .{},
    project_root: []const u8 = "",

    pub fn init(allocator: mem_allocator) lsp {
        return .{ .allocator = allocator };
    }

    pub fn deinit(this: *lsp) void {
        var it = this.documents.iterator();
        while (it.next()) |entry| this.allocator.free(entry.value_ptr.*);
        this.documents.deinit(this.allocator);
        if (this.project_root.len != 0) this.allocator.free(this.project_root);
    }
};
