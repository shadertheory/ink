const ink = @import("ink");

pub const program = struct {
    instructions: []const ink.exe.instruction,
    constants: []const u64,
};

pub const bundle = struct {
    program: program,
    strings: []const []const u8,
};
