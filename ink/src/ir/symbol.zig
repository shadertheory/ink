pub const string_hash_map = @import("std").StringHashMap;
pub const array_list = @import("std").array_list.Managed;
pub const mem_allocator = @import("std").mem.Allocator;
pub const machine = @import("./vm/core.zig").machine;
pub const root = @import("root.zig");
pub const table = struct {
    pub const depth = struct { value: u64 };
    pub const symbol = struct {
        name: root.identifier,
        what: kind,
        mutable: bool,

        pub const kind = union(enum) { local: u64, global: u64, upvalue: u64, function: struct {
            id: u64,
            arity: u8,
        } };
    };
    pub const scope = struct {
        symbols: string_hash_map(symbol),
        start: machine.register,
    };
    pub const function_state = struct {
        active_count: machine.register,
        peak_count: machine.register,
        global_count: machine.index,
    };

    current: function_state,
    scopes: array_list(scope),
    allocator: mem_allocator,

    pub fn init(allocator: mem_allocator) table {
        const zero = .{ .value = 0 };
        var t = table{ .scopes = array_list(scope).init(allocator), .allocator = allocator, .current = .{ .active_count = .{ .value = zero }, .peak_count = .{ .value = zero }, .global_count = zero } };
        t.enter_scope() catch unreachable;
        return t;
    }
};
