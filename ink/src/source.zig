const std = @import("std");

pub const source_id = u64;
pub const module_id = u64;

pub const span = struct { start: usize, end: usize };

pub const source_file = struct {
    id: source_id,
    path: []const u8,
    text: []const u8,
};

pub const module_spec = struct {
    name: []const u8,
    sources: []const source_id,
    deps: []const []const u8,
};

pub const compile_request = struct {
    sources: []const source_file,
    modules: []const module_spec,
    root_module: []const u8,
};
