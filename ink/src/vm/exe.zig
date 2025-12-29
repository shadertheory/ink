pub const root = @import("ink");
pub const value = root.vm.value;
pub const register = root.vm.register;

pub const label_id = u32;

pub const instruction = union(enum) {
    label: struct { id: label_id },

    load_const: struct { dst: u8, const_index: u32 },
    move: struct { dst: u8, src: u8 },
    argument_set: struct { dst: u8, src: u8 },

    add: struct { dst: u8, src_a: u8, src_b: u8 },
    sub: struct { dst: u8, src_a: u8, src_b: u8 },
    mul: struct { dst: u8, src_a: u8, src_b: u8 },
    div: struct { dst: u8, src_a: u8, src_b: u8 },

    compare_eq: struct { dst: u8, src_a: u8, src_b: u8 },
    compare_ne: struct { dst: u8, src_a: u8, src_b: u8 },
    compare_lt: struct { dst: u8, src_a: u8, src_b: u8 },
    compare_le: struct { dst: u8, src_a: u8, src_b: u8 },
    compare_gt: struct { dst: u8, src_a: u8, src_b: u8 },
    compare_ge: struct { dst: u8, src_a: u8, src_b: u8 },

    jump: struct { target: label_id },
    jump_if_true: struct { condition: u8, target: label_id },
    jump_if_false: struct { condition: u8, target: label_id },

    call: struct { target: label_id },
    call_foreign: struct { index: u32 },

    ret: void,
    ret_value: struct { src: u8 },
    halt: void,
};
