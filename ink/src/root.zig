pub const exe = @import("vm/exe.zig");
pub const strings = @import("common/string.zig");
pub const ast = @import("lang/ast.zig");
pub const node = @import("lang/ast.zig").node;
pub const lexer = @import("lang/lexer.zig").lexer;
pub const peg = @import("lang/peg.zig");
pub const peg_parser = @import("lang/peg_parser.zig");
pub const peg_ast = @import("lang/peg_ast.zig");
pub const ir = @import("ir/core.zig");
pub const ir_build = @import("ir/build.zig");
pub const ir_codegen = @import("ir/codegen.zig");
pub const ir_print = @import("ir/print.zig");
pub const token = @import("lang/token.zig").token;
pub const precedence = @import("lang/token.zig").precedence;
pub const vm = @import("vm.zig");

pub const identifier_owner = enum { ref, def };

pub const identifier = struct {
    string: []const u8,
    owner: identifier_owner,
};

pub const location = struct {
    start: usize,
    end: usize,
};

pub const binary = enum(u8) {
    add,
    sub,
    mul,
    div,
    mod,
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
    coalesce,
    logical_or,
    logical_and,
    logical_xor,
    assign,
    index,
};

pub const unary = enum(u8) {
    neg,
    not,
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
    ret,
};
