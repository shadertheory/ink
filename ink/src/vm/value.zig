pub const value = union(enum) {
    unit: void,
    bang: void,
    string: []const u8,
    boolean: bool,
    number: *tensor,
    heap: *object,
};
