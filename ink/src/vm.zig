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

pub const machine = struct { 
    registers: stack,
    function: register,
    frames: call_stack,
    executable: *const executable, 
    ip: address, 
    mem: memory,

    pub fn step(self: *machine) !bool {
        const code = self.executable.code; 
        
        const instruction = code[self.current.ip];
        self.current.ip += 1;
        
        switch(instruction) {
            .unit => {},
            .bang => return true,
            .jump { to, condition } => switch (condition) {
                .always => self.ip.where = to,
                .check_if { cmp, subject } => if (@as(*bool, memory.load(subject)).* == cmp) {
                },
                .check_comparison { cmp, lhs, rhs, ty } => {
                    
                },
            } 
        }
        return false;
    }

    pub const register = struct {
        which: u64,
    };
};

pub const call_frame = struct {
    return_address: instruction_pointer,
    previous_function: frame_pointer,
    destination: machine.register,
};
