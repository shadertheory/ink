const std = @import("std");
const ink = @import("ink");

const mem_allocator = std.mem.Allocator;
const compiler = ink.compiler;
const inkb = ink.vm.inkb;

pub const compile_request = compiler.compile_request;
pub const compile_result = compiler.compile_result;

pub fn compile(allocator: mem_allocator, req: compile_request) !compile_result {
    return compiler.compile(allocator, req);
}

pub fn compile_to_inkb(allocator: mem_allocator, req: compile_request, output_path: []const u8) !compile_result {
    const result = try compiler.compile(allocator, req);
    if (result.ok) {
        const bytecode = result.bytecode orelse return error.MissingBytecode;
        const constants = result.constants orelse return error.MissingConstants;
        const data = result.data orelse return error.MissingData;
        const foreigns = result.foreigns orelse return error.MissingForeigns;
        try inkb.write_file(output_path, bytecode, constants, data, foreigns);
    }
    return result;
}
