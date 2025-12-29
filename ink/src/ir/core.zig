pub const std = @import("std");
pub const ink = @import("ink");

pub const ir_identifier = struct { idx: u32 };
pub const string_identifier = struct { idx: u32 };

pub const ir = union(enum) {
    integer: i64,
    float: f64,
    string: string_identifier,
    boolean: bool,
    identifier: string_identifier,

    unary: struct { op: ink.unary, right: ir_identifier },
    binary: struct { left: ir_identifier, op: ink.binary, right: ir_identifier },
    block: []const ir_identifier,

    if_expr: struct { condition: ir_identifier, then_branch: ir_identifier, else_branch: ?ir_identifier },
    match_expr: struct { target: ir_identifier, arms: []const match_arm },

    associate: struct { name: string_identifier, value: ?ir_identifier },

    type: union(enum) {
        self: void,
        name: string_identifier,
        optional: ir_identifier,
        applied: struct { base: string_identifier, args: []const ir_identifier },
    },

    decl: union(enum) {
        @"struct": struct_decl,
        function: function_decl,
        trait: trait_decl,
        concept: concept_decl,
        sum: sum_decl,
        @"enum": enum_decl,
        impl: impl_decl,
        @"const": const_decl,
        @"var": var_decl,
    },

    pub const match_arm = struct { pattern: ir_identifier, body: ir_identifier };

    pub const function_decl = struct {
        name: string_identifier,
        generics: []const generic_param,
        params: []const param,
        return_type: ?ir_identifier,
        where_clause: []const where_req,
        body: ?ir_identifier,

        pub const param = struct { name: string_identifier, ty: ir_identifier };
        pub const where_req = struct { name: string_identifier, constraint: ir_identifier };
    };

    pub const struct_decl = struct {
        name: string_identifier,
        generics: []const generic_param,
        fields: []const field,

        pub const field = struct { name: string_identifier, ty: ir_identifier };
    };

    pub const impl_decl = struct {
        for_struct: string_identifier,
        by_trait: string_identifier,
        functions: []const function_decl,
    };

    pub const const_decl = struct {
        name: string_identifier,
        ty: ?ir_identifier,
        value: ir_identifier,
    };

    pub const var_decl = struct {
        name: string_identifier,
        ty: ?ir_identifier,
        value: ir_identifier,
    };

    pub const trait_decl = struct {
        name: string_identifier,
        generics: []const generic_param,
        items: []const trait_item,

        pub const trait_item = union(enum) {
            function: function_decl,
            assoc_type: associated_type_decl,
        };
    };

    pub const associated_type_decl = struct {
        name: string_identifier,
        value: ?ir_identifier,
    };

    pub const concept_decl = struct {
        name: string_identifier,
        generics: []const generic_param,
        requires: []const ir_identifier,
    };

    pub const sum_decl = struct {
        name: string_identifier,
        generics: []const generic_param,
        variants: []const sum_variant,

        pub const sum_variant = struct {
            name: string_identifier,
            payload: ?ir_identifier,
        };
    };

    pub const enum_decl = struct {
        name: string_identifier,
        cases: []const string_identifier,
    };

    pub const generic_param = struct {
        name: string_identifier,
        kind: generic_kind,
        constraint: ?ir_identifier,
        default: ?ir_identifier,

        pub const generic_kind = enum { type, value };
    };
};
