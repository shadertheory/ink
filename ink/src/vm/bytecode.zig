pub const operation = union(enum) {
    state: control,
    arithmetic: math, 
    block: memory,

    pub const control = union(enum) {
        //no operation
        unit: void,
        //halt operation
        bang: void,
        jump: struct { to: address, condition: jump_condition },
        call: struct { argc: u8, argp: address, ret: address, location: call_location },
        ret: struct { value: ?address },
        match: struct {
            dst: address,
            condition: jump_condition,
            true_if: address,
            false_if: address,
        },

        pub const jump_condition = union(enum) {
            always,
            check_if: struct { cmp: bool, subject: address },
            check_comparison: struct { cmp: comparison, lhs: address, rhs: address, ty: number },
        };

        pub const call_location = union(enum) {
            direct: struct { where: address, },
            native: struct { id: foreign_function },
            indirect: struct { which: address },
            virtual: struct { vtable: virtual_table, offset: u8 }
        };

    };
    pub const math = union(enum) {
        compute_binary: struct {
            dst: address,
            lhs: address,
            rhs: address,
            op: binary,
        },
        compute_unary: struct {
            dst: address,
            src: address,
            op: unary,
        },
        compare: struct {
            dst: address, // Stores a boolean result (0 or 1)
            lhs: address,
            rhs: address,
            op: comparison,
        },
    };
    pub const memory = union(enum) {
        copy: struct { dst: address, src: address },
        clone: struct { dst: address, src: address },
        swap: struct { lhs: address, rhs: address },
        load_const: struct { dst: address, pool_index: u16 },
        alloc: struct { dst: address, size_bytes: u32, align_bytes: u8 },
        alloc_array: struct { dst: address, element_type_id: u16, count: address },
        free: struct { ptr: address },
        read_ptr: struct { dst: address, ptr: address, offset: u16 },
        read_global: struct { dst: address, global_id: u16 },
        read_closure_upvalue: struct { dst: address, index: u8 },
        write_ptr: struct { ptr: address, src: address, offset: u16 },
        write_global: struct { global_id: u16, src: address },
        write_closure_upvalue: struct { index: u8, src: address },
        get_field: struct { dst: address, object: address, field_id: u16 },
        set_field: struct { object: address, field_id: u16, src: address },
        get_element: struct { dst: address, array: address, index: address },
        set_element: struct { array: address, index: address, src: address },
    };
};

pub const location = struct {
    start: usize,
    end: usize
};

pub const identifier = struct {
    string: []const u8,
    owner: ownership
};

pub const ownership = enum(u8) {
    ref,
    mut,
    owned
};
