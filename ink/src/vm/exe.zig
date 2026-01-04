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
    rem: struct { dst: u8, src_a: u8, src_b: u8 },
    min: struct { dst: u8, src_a: u8, src_b: u8 },
    max: struct { dst: u8, src_a: u8, src_b: u8 },

    bit_and: struct { dst: u8, src_a: u8, src_b: u8 },
    bit_or: struct { dst: u8, src_a: u8, src_b: u8 },
    bit_xor: struct { dst: u8, src_a: u8, src_b: u8 },
    bit_shl: struct { dst: u8, src_a: u8, src_b: u8 },
    bit_shr: struct { dst: u8, src_a: u8, src_b: u8 },
    bit_sar: struct { dst: u8, src_a: u8, src_b: u8 },
    bit_rol: struct { dst: u8, src_a: u8, src_b: u8 },
    bit_ror: struct { dst: u8, src_a: u8, src_b: u8 },
    bit_not: struct { dst: u8, src_a: u8, src_b: u8 },
    int_neg: struct { dst: u8, src_a: u8, src_b: u8 },
    int_abs: struct { dst: u8, src_a: u8, src_b: u8 },

    compare_eq: struct { dst: u8, src_a: u8, src_b: u8 },
    compare_ne: struct { dst: u8, src_a: u8, src_b: u8 },
    compare_lt: struct { dst: u8, src_a: u8, src_b: u8 },
    compare_le: struct { dst: u8, src_a: u8, src_b: u8 },
    compare_gt: struct { dst: u8, src_a: u8, src_b: u8 },
    compare_ge: struct { dst: u8, src_a: u8, src_b: u8 },

    fadd: struct { dst: u8, src_a: u8, src_b: u8 },
    fsub: struct { dst: u8, src_a: u8, src_b: u8 },
    fmul: struct { dst: u8, src_a: u8, src_b: u8 },
    fdiv: struct { dst: u8, src_a: u8, src_b: u8 },
    frem: struct { dst: u8, src_a: u8, src_b: u8 },
    fmin: struct { dst: u8, src_a: u8, src_b: u8 },
    fmax: struct { dst: u8, src_a: u8, src_b: u8 },
    fcompare_eq: struct { dst: u8, src_a: u8, src_b: u8 },
    fcompare_lt: struct { dst: u8, src_a: u8, src_b: u8 },
    fcompare_gt: struct { dst: u8, src_a: u8, src_b: u8 },
    fneg: struct { dst: u8, src_a: u8, src_b: u8 },
    fabs: struct { dst: u8, src_a: u8, src_b: u8 },
    fsqrt: struct { dst: u8, src_a: u8, src_b: u8 },
    fsin: struct { dst: u8, src_a: u8, src_b: u8 },
    fcos: struct { dst: u8, src_a: u8, src_b: u8 },
    ftan: struct { dst: u8, src_a: u8, src_b: u8 },
    fasin: struct { dst: u8, src_a: u8, src_b: u8 },
    facos: struct { dst: u8, src_a: u8, src_b: u8 },
    fatan: struct { dst: u8, src_a: u8, src_b: u8 },
    ffloor: struct { dst: u8, src_a: u8, src_b: u8 },
    fceil: struct { dst: u8, src_a: u8, src_b: u8 },
    fround: struct { dst: u8, src_a: u8, src_b: u8 },
    ftrunc: struct { dst: u8, src_a: u8, src_b: u8 },

    jump: struct { target: label_id },
    jump_if_true: struct { condition: u8, target: label_id },
    jump_if_false: struct { condition: u8, target: label_id },

    call: struct { target: label_id },
    call_register: struct { src: u8, dst: u8 },
    call_foreign: struct { index: u32 },

    task_spawn: struct { dst: u8, target: label_id, argc: u8 },
    task_await: struct { dst: u8, src: u8 },
    task_await_any: struct { dst: u8, src: u8, count: u8 },
    task_cancel: struct { src: u8 },

    ret: void,
    ret_value: struct { src: u8 },
    halt: void,
};
