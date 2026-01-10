pub const std = @import("std");
pub const ink = @import("ink");

pub const mir_identifier = struct { idx: u32 };
pub const string_identifier = struct { idx: u32 };

pub const intrinsic = struct { name: string_identifier, args: []const mir_identifier };

pub const record_literal = struct {
    type_name: string_identifier,
    fields: []const record_field,

    pub const record_field = struct { name: string_identifier, value: mir_identifier };
};

pub const mir = union(enum) {
    integer: i64,
    float: f64,
    duration: i64,
    string: string_identifier,
    boolean: bool,
    identifier: string_identifier,

    unary: struct { op: ink.unary, right: mir_identifier },
    binary: struct { left: mir_identifier, op: ink.binary, right: mir_identifier },
    block: []const mir_identifier,
    intrinsic: intrinsic,

    if_expr: struct { condition: mir_identifier, then_branch: mir_identifier, else_branch: ?mir_identifier },
    match_expr: struct { target: mir_identifier, arms: []const match_arm },
    select_expr: struct { arms: []const select_arm },
    label_expr: struct { name: string_identifier, body: mir_identifier },
    loop_expr: struct { body: mir_identifier },
    while_expr: struct { condition: mir_identifier, body: mir_identifier },
    while_in_expr: struct { pattern: mir_identifier, iter: mir_identifier, body: mir_identifier },
    until_expr: struct { condition: mir_identifier, body: mir_identifier },
    repeat_expr: struct { count: mir_identifier, body: mir_identifier },
    for_expr: struct { pattern: mir_identifier, iter: mir_identifier, body: mir_identifier },
    each_expr: struct { pattern: mir_identifier, iter: mir_identifier, body: mir_identifier },
    break_expr: struct { label: ?string_identifier, value: ?mir_identifier },
    continue_expr: struct { label: ?string_identifier },
    yield_expr: struct { value: ?mir_identifier },
    atomic_expr: struct { value: mir_identifier, ordering: string_identifier },

    associate: struct { name: string_identifier, value: ?mir_identifier },
    record_literal: record_literal,

    type: union(enum) {
        self: void,
        name: string_identifier,
        optional: mir_identifier,
        dyn: mir_identifier,
        applied: struct { base: string_identifier, args: []const mir_identifier },
    },

    decl: union(enum) {
        @"struct": struct_decl,
        function: function_decl,
        trait: trait_decl,
        @"enum": enum_decl,
        impl: impl_decl,
        type_alias: type_decl,
        @"const": const_decl,
        @"var": var_decl,
    },

    pub const match_arm = struct { pattern: mir_identifier, body: mir_identifier };
    pub const select_arm = struct {
        name: ?string_identifier,
        task: mir_identifier,
        body: mir_identifier,
        detached: bool,
    };

    pub const function_decl = struct {
        name: string_identifier,
        generics: []const generic_param,
        params: []const param,
        return_type: ?mir_identifier,
        where_clause: []const where_req,
        body: ?mir_identifier,
        span: ?ink.source.span = null,
        source_id: ink.source.source_id = 0,

        pub const param = struct { name: string_identifier, ty: mir_identifier, variadic: bool };
        pub const where_req = struct { name: string_identifier, constraint: mir_identifier };
    };

    pub const type_decl = struct {
        name: string_identifier,
        generics: []const generic_param,
        value: mir_identifier,
    };

    pub const struct_decl = struct {
        name: string_identifier,
        generics: []const generic_param,
        fields: []const field,
        is_record: bool,

        pub const field = struct { name: string_identifier, ty: mir_identifier };
    };

    pub const impl_decl = struct {
        negative: bool,
        for_struct: string_identifier,
        by_trait: string_identifier,
        functions: []const function_decl,
    };

    pub const const_decl = struct {
        name: string_identifier,
        ty: ?mir_identifier,
        value: mir_identifier,
    };

    pub const var_decl = struct {
        name: string_identifier,
        ty: ?mir_identifier,
        value: mir_identifier,
    };

    pub const trait_decl = struct {
        is_auto: bool,
        name: string_identifier,
        generics: []const generic_param,
        items: []const trait_item,
        requires: []const mir_identifier,

        pub const trait_item = union(enum) {
            function: function_decl,
            assoc_type: associated_type_decl,
        };
    };

    pub const associated_type_decl = struct {
        name: string_identifier,
        value: ?mir_identifier,
    };

    pub const enum_variant = struct {
        name: string_identifier,
        payload: ?mir_identifier,
    };

    pub const enum_decl = struct {
        name: string_identifier,
        generics: []const generic_param,
        variants: []const enum_variant,
    };

    pub const generic_param = struct {
        name: string_identifier,
        kind: generic_kind,
        constraint: ?mir_identifier,
        default: ?mir_identifier,
        is_pack: bool,

        pub const generic_kind = enum { type, value };
    };
};
