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
