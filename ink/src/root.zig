pub const exe = @import("vm/exe.zig");
pub const ast = @import("lang/ast.zig");
pub const node = @import("lang/ast.zig").node;
pub const lexer = @import("lang/lexer.zig").lexer;
pub const peg = @import("lang/peg.zig");
pub const peg_parser = @import("lang/peg_parser.zig");
pub const peg_ast = @import("lang/peg_ast.zig");
pub const lang_spec = @import("lang/spec.zig");
pub const desugar = @import("lang/desugar.zig");
pub const sim = @import("sim.zig");
pub const uir = @import("uir/core.zig");
pub const uir_build = @import("uir/build.zig");
pub const mir = @import("mir/core.zig");
pub const mir_lower = @import("mir/lower.zig");
pub const lir_lower = @import("lir/lower.zig");
pub const lir_vm = @import("lir/vm/core.zig");
pub const lir_vm_lower = @import("lir/vm/lower.zig");
pub const backend = @import("backend/backend.zig");
pub const target = @import("target.zig");
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
    deref,
    borrow,
    borrow_mut,
    ref,
    ref_mut,
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
    box,
    sleep,
    timeout,
    deadline,
    ret,
    spawn,
    await,
    @"try",
    unwrap_optional,
};
