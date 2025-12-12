pub const root = @import("ink");
pub const value = root.vm.value; 
pub const register = root.vm.register;

pub const operation_code = struct { code: u64 }; 

pub const instruction = struct {
    operation: operation_code,
    destination: register,
    source_a: register,
    source_b: register,
};

pub const instruction = enum(u16) {
    no_operation,
    halt,
    
    //control
    jump_always,
    jump_if_true,
    jump_if_false,

    conditional_move,
    conditional_move_value,

    argument_set,

    call,
    call_register,
    call_foreign,
    call_tail,

    ret,
    ret_value,

    method_virtual_get,

    //TODO async

    //float mathematics
    float_cmp_eq,
    float_cmp_lt,
    float_cmp_gt,

    float_unary_negate,
    float_unary_absolute,
    float_unary_sqrt,
    float_unary_sine,
    float_unary_cosine,
    float_unary_tangent,
    float_unary_arcsine,
    float_unary_arccosine,
    float_unary_arctangent,
    float_unary_floor,
    float_unary_ceil,
    float_unary_round,
    float_unary_truncate,

    float_binary_add,
    float_binary_sub,
    float_binary_multiply,
    float_binary_divide,
    float_binary_remainder,
    float_binary_minimum,
    float_binary_maximum,

    //integer mathematics
    int_compare_equal,
    int_compare_less_than,
    int_compare_less_than_or_equal,
    int_compare_greater_than,
    int_compare_greater_than_or_equal,

    int_unary_negate,
    int_unary_absolute,
    int_unary_bitwise_not,

    int_binary_add,
    int_binary_sub,
    int_binary_multiply,
    int_binary_divide,
    int_binary_remainder,
    int_binary_minimum,
    int_binary_maximum,

    int_bitwise_and,
    int_bitwise_or,
    int_bitwise_xor,
    int_bitwise_shift_left,
    int_bitwise_shift_right,
    int_bitwise_shift_arithmetic,
    int_bitwise_rotate_left,
    int_bitwise_rotate_right,

    memory_copy,
    memory_swap,
    memory_allocate,
    memory_free,
    memory_load,
    memory_read,
    memory_write,

    pub const format = enum(u8) {
        reg_left_right,
        reg_source,
        reg_immediete,
        reg_jump_offset
    };
};
**/

pub const executable = struct {
    constants: []value,
    instructions: []instruction,
};

