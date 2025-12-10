const std = @import("std");
pub const exe = @import("vm/exe.zig");
pub const value = @import("vm/value.zig");
pub const strings = @import("common/string.zig");
pub const ast = @import("lang/ast.zig");
pub const lexer = @import("lang/lexer.zig");
pub const parser = @import("lang/parser.zig");
pub const token = @import("lang/token.zig");
const tuple = std.meta.Tuple;

pub const identifier = exe.identifier;

pub const literal = exe.literal; 

pub const trait_identifier = struct { id: u64 };

pub const type_identifier = struct { id: u64 };

pub const function_identifier = struct { id: u64 };

pub const number = union(enum) {
    integer,
    float
};

pub const comparison = enum(u8) {
    equal,
    not_equal,
    less_than,
    less_or_equal,
    greater_than,
    greater_or_equal
};


fn compare(cmp: comparison, lhs: anytype, rhs: @TypeOf(lhs)) bool {
    // Determine the ordering using your original logic
    // 1: greater, -1: less, 0: equal
    const order: i8 = if (lhs > rhs) 1 else if (lhs < rhs) -1 else 0;

    return switch (cmp) {
        .equal            => order == 0,
        .not_equal        => order != 0,
        .less_than        => order == -1,
        .less_or_equal    => order <= 0,
        .greater_than     => order == 1,
        .greater_or_equal => order >= 0,
    };
}

pub const binary = enum(u8) {
    add, sub, mul, div, mod,
    min, max,
};

pub const unary = enum(u8) {
    neg, abs, sqrt,
    sin, cos, tan,
    asin, acos, atan,
    floor, ceil, round, trunc,
};

pub const bitwise = enum(u8) {
    band, bor, xor, not,
    shl, shr, sar,
    rol, ror,
};


pub const type_reference = struct {
    id: trait_identifier,
    params: []const type_identifier,
};

pub const trait_reference = struct {
    id: u64,
};

pub const struct_decl = struct {
    name: identifier,
    data: []const field,

    pub const field = struct {
        name: identifier,
        what: type_reference,
    };
};

pub const generic_parameter = struct {
    name: identifier,
    contraints: []const trait_reference,
};

pub const argument = struct {
    name: identifier,
    what: type_reference,
    mutable: bool,
};

pub const type_kind = union(enum) {
    name: identifier,
    generic: identifier,
    reference: *type_reference,
    parameter: struct { base: identifier, params: []const type_reference },
    unit,
    bang,
    self,
    any,
};

pub const function_decl = struct {
    name: identifier,
    generics: []const generic_parameter,
    params: []const argument,
    ret: type_reference,
    body: []const statement,
};

pub const block = struct {
    []const statement,
};

pub const statement = union(enum) {
    declare: struct {
        name: identifier,
        explicit: ?type_reference,
        mutable: bool,
        value: expression,
    },
    assignment: struct {
        target: identifier,
        value: expression,
    },
    flow_return: ?expression,
    flow_if: struct {
        condition: expression,
        then_block: block,
        else_block: ?block,
    },
    flow_loop: struct {
        condition: expression,
        do: block,
    },
    match: struct {
        subject: expression,
        cases: []const match_case,
    },
    standalone: expression,

    pub const match_case = struct {
        against: pattern,
        body: block,

        pub const pattern = union(enum) {
            wildcard,           // `_` (Matches anything)
            lit: literal,
            variant: struct {   
                name: []identifier,   // "Some"
                capture: ?[]identifier,   // "val" (Variable to bind data to)
                },
            };
        };
    };


pub const expression = union(enum) {
    lit: literal,
    ident: identifier,
    binary: struct {
        lhs: *const expression,
        op: binary,
        rhs: *const expression,
    },
    call: struct {
        name: identifier,
        args: []const expression
    },
    init: struct {
        name: identifier,
        assign: []const struct { name: identifier, value: expression },
    },
    access: struct {
        target: *const expression,
        field: identifier,
    }
};
