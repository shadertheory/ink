pub const op = @import("op.zig");

pub const trait_identifier = struct { id: u64 };

pub const type_identifier = struct { id: u64 };

pub const function_identifier = struct { id: u64 };

pub const number = union(enum) {
    integer,
    float
};

pub const comparison = enum(u8) {
    equal,
    not_equal,
    less_than,
    less_or_equal,
    greater_than,
    greater_or_equal
};


fn compare(cmp: comparison, lhs: anytype, rhs: @TypeOf(lhs)) bool {
    // Determine the ordering using your original logic
    // 1: greater, -1: less, 0: equal
    const order: i8 = if (lhs > rhs) 1 else if (lhs < rhs) -1 else 0;

    return switch (cmp) {
        .equal            => order == 0,
        .not_equal        => order != 0,
        .less_than        => order == -1,
        .less_or_equal    => order <= 0,
        .greater_than     => order == 1,
        .greater_or_equal => order >= 0,
    };
}

pub const binary = enum(u8) {
    add, sub, mul, div, mod,
    min, max,
};

pub const unary = enum(u8) {
    neg, abs, sqrt,
    sin, cos, tan,
    asin, acos, atan,
    floor, ceil, round, trunc,
};

pub const bitwise = enum(u8) {
    band, bor, xor, not,
    shl, shr, sar,
    rol, ror,
};
