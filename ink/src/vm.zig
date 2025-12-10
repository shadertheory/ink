pub const root = @import("root.zig");
pub const std = @import("std");

pub const hash_map = std.AutoHashMap;
pub const array_list = std.array_list.Managed;
pub const trait_identifier = root.trait_identifier;
pub const type_identifier = root.type_identifier;
pub const identifier = root.op.identifier;
pub const stack = array_list(value);
pub const call_stack = array_list(call_frame);

pub const bytes = struct { count: usize };

pub const trait_object = struct {
    data: usize,
    vtable: *const virtual_table,
};

pub const struct_reference = struct { ptr: usize };

pub const function_index = struct { index: usize };

pub const value = union(enum) {
    unit: void,
    integer: u64,
    float: f64,
    boolean: bool,
    ptr: struct_reference,
    dyn: trait_object,
};

pub const virtual_table = struct {
    owner: type_identifier,
    implemented: trait_identifier,
    methods: []const function_index,
    destructor: function_index,
};

pub const runtime_type = struct {
    id: type_identifier,
    size: bytes,
    name: identifier,
};


pub const executable = struct {
    code: []instruction,
    constant: []value,
};

pub const address = struct { where: usize };

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
            ty: number,
        },
        compute_unary: struct {
            dst: address,
            src: address,
            op: unary,
            ty: number,
        },
        compare: struct {
            dst: address, // Stores a boolean result (0 or 1)
            lhs: address,
            rhs: address,
            op: comparison,
            ty: number,
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

pub const machine = struct { 
    frames: call_stack,
    data: []u64,
    ip: address, 
    fp: address,
    executable: address,
    stack: address,
    heap: address,

    pub fn step(self: *machine) !bool {
        const instruction: *operation = @ptrCast(self.ip.read(operation));
        self.ip.next();
        
        switch(instruction) {
            .state => |control_op| switch (control_op) {
                .unit => {},
                .bang => return true,
                .jump { to, condition } => switch (condition) {
                    .always => self.ip.jump(to),
                    .check_if { cmp, subject } => {
                        const value: *bool = subject.read(bool);
                        if (value.* == cmp) {
                            self.ip.jump(to);
                        } 
                    }
                    ,
                    .check_comparison { cmp, lhs, rhs, ty } => {
                        const num = if (ty == .number) u64 else f32; 
                        const should_jump = ink.compare(
                            cmp,
                            lhs.read(num),
                            rhs.read(num)
                        );
                        if (should_jump) {
                            self.ip.jump(to);
                        }
                     }
                },
                .call { argc, argp, ret, location } => {
                    var frame: call_frame = undefined;
                    frame.dest = ret.read(machine.register); 
                    frame.ret = address { .where = self.ip.where + 1 };

                    try self.frames.push(frame);

                    switch (location) {
                        .direct { where } => {
                            self.ip.jump(where);
                        },
                        .indirect { where } => {
                            self.ip.jump(where.read(address).*);
                        },
                        else => unreachable
                    }
                },
                .ret { value, size } => {
                    const frame = try self.frames.pop();
                    self.ip.jump(frame.ret);

                    if (value) |value| {
                        frame.dest.write_bytes(value, size);
                    }
                }
            },
            .math => |math_op| switch(math_op) {
                .compute_binary { dst, lhs, rhs, op } => {},
                .compute_unary { dst, src, op } => {},
                .compare { dst, lhs, rhs, op, ty } => {}
            },
            .block => |memory_op| switch(memory_op) {
                else => {}
            }
        }
        return false;
    }

    pub const register = struct {
        which: u64,
    };
};

pub const call_frame = struct {
    ret: instruction_pointer,
    prev: frame_pointer,
    dest: machine.register,
};
