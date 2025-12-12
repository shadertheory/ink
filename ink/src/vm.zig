const core = @import("vm/core.zig");
const exe = @import("vm/exe.zig");
const obj = @import("vm/obj.zig");
pub const vm = core.machine;
pub const object = obj.object;
pub const value = core.value;  
pub const instruction = exe.instruction; 
pub const register = vm.register;
