pub const string = struct { id: usize };

pub const pool = struct {
    index: array_list([]const u8),
    lookup: string_hash_map(string),
    allocator: std.mem.Allocator,

    pub fn intern(self: *pool, str: []const u8) string {
        if (self.lookup.get(str)) |id| return id;

        const dup = self.allocator.dupe(u8,  str) catch unreachable;

        const id = string { .id: @as(usize, @intCast(self.strings.items.len)) };

        self.strings.append(dup) catch unreachable;
        self.lookup.put(dup, id) catch unreachable;

        return id;
    }
}; 
