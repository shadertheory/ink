
pub const control = enum(u8) {
    no_operation,
    halt,
    
    //control
    jump_always,
    };


    //TODO async

pub const float_math = enum(u8) {
    compare_eq,
    compare_lt,
    compare_gt,

    unary_negate,
    unary_absolute,
    unary_sqrt,
    unary_sine,
    unary_cosine,
    unary_tangent,
    unary_arcsine,
    unary_arccosine,
    unary_arctangent,
    unary_floor,
    unary_ceil,
    unary_round,
    unary_truncate,

    binary_add,
    binary_sub,
    binary_multiply,
    binary_divide,
    binary_remainder,
    binary_minimum,
    binary_maximum,
};
    //integer mathematics
pub const int_math = enum(u8) {
    compare_equal,
    compare_less_than,
    compare_less_than_or_equal,
    compare_greater_than,
    compare_greater_than_or_equal,

    unary_negate,
    unary_absolute,
    unary_bitwise_not,

    binary_add,
    binary_sub,
    binary_multiply,
    binary_divide,
    binary_remainder,
    binary_minimum,
    binary_maximum,

    bitwise_and,
    bitwise_or,
    bitwise_xor,
    bitwise_shift_left,
    bitwise_shift_right,
    bitwise_shift_arithmetic,
    bitwise_rotate_left,
    bitwise_rotate_right,
};
pub const memory = enum(u8) {
    copy,
    swap,
    allocate,
    free,
    load,
    read,
    write,
};


