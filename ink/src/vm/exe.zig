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

