const std = @import("std");
const target_mod = @import("../target.zig");
const lir_vm = @import("../lir/vm/core.zig");
const vm_backend = @import("vm.zig");

const mem_allocator = std.mem.Allocator;

pub const backend_error = vm_backend.emit_error || error{unsupported_target};

pub const backend_input = union(enum) {
    vm: lir_vm.bundle,
};

pub const backend_request = struct {
    allocator: mem_allocator,
    target: target_mod.target_spec,
    input: backend_input,
    foreigns: []const []const u8,
};

pub const backend_result = struct {
    instructions: ?[]const @import("../vm/exe.zig").instruction = null,
    constants: ?[]const u64 = null,
    data: ?[]const @import("../vm/inkb.zig").data_entry = null,
    bytecode: ?[]u8 = null,
    foreigns: ?[]const []const u8 = null,
};

pub fn emit(req: backend_request) backend_error!backend_result {
    return switch (req.target.kind) {
        .vm => switch (req.input) {
            .vm => |bundle| vm_backend.emit(req.allocator, bundle, req.foreigns),
        },
        else => error.unsupported_target,
    };
}
