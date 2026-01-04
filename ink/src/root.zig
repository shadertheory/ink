pub const exe = @import("vm/exe.zig");
pub const ast = @import("lang/ast.zig");
pub const node = @import("lang/ast.zig").node;
pub const lexer = @import("lang/lexer.zig").lexer;
pub const peg = @import("lang/peg.zig");
pub const peg_parser = @import("lang/peg_parser.zig");
pub const peg_ast = @import("lang/peg_ast.zig");
pub const lang_spec = @import("lang/spec.zig");
pub const desugar = @import("lang/desugar.zig");
pub const ir = @import("ir/core.zig");
pub const ir_build = @import("ir/build.zig");
pub const ir_codegen = @import("ir/codegen.zig");
pub const token = @import("lang/token.zig").token;
pub const precedence = @import("lang/token.zig").precedence;
pub const vm = @import("vm.zig");
pub const compiler = @import("compiler.zig").compiler;
pub const diagnostic = @import("diagnostic.zig").diagnostic;
pub const severity = @import("diagnostic.zig").severity;
pub const source = @import("source.zig");
pub const runtime = @import("runtime.zig");

pub const location = struct {
    start: usize,
    end: usize,
};

pub const identifier_owner = enum { ref, def };

pub const identifier = struct {
    string: []const u8,
    owner: identifier_owner,
    where: location = .{ .start = 0, .end = 0 },
};

pub const binary = enum(u8) {
    add,
    sub,
    mul,
    div,
    mod,
    bit_and,
    bit_or,
    bit_xor,
    shl,
    shr,
    min,
    max,
    equal,
    not_equal,
    less_than,
    less_or_equal,
    greater_than,
    greater_or_equal,
    call,
    pipe,
    access,
    scope_access,
    coalesce,
    logical_or,
    logical_and,
    logical_xor,
    assign,
    assign_add,
    assign_sub,
    assign_mul,
    assign_div,
    assign_mod,
    assign_bit_and,
    assign_bit_or,
    assign_bit_xor,
    assign_shl,
    assign_shr,
    @"as",
    index,
};

pub const unary = enum(u8) {
    neg,
    not,
    bit_not,
    abs,
    sqrt,
    sin,
    cos,
    tan,
    asin,
    acos,
    atan,
    floor,
    ceil,
    round,
    trunc,
    dynamic,
    @"comptime",
    ret,
    spawn,
    await,
    @"try",
    unwrap_optional,
};
