pub const def_spec = struct {
    pub const entry = struct {
        name: []const u8,
        payload: type,
        named: bool,
        overload: bool,
        @"export": bool,
        entry_point: bool,
    };

    pub const entries = comptime .{
        .{ .name = }
    };
};

pub const program = struct {};
