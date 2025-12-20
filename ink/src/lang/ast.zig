const ink = @import("ink");
const identifier = ink.identifier;
pub const node = union(enum) {
    integer: i64,
    float: f32,
    identifier: identifier,

    decl: union(enum) {
        function: function_decl,
        trait: trait_decl,
        concept: concept_decl,
        sum: sum_decl,
        @"enum": enum_decl,
    },

    unary: struct { op: ink.unary, right: *node },
    binary: struct { left: *node, op: ink.binary, right: *node },

    if_expr: struct { condition: *node, then_branch: *node, else_branch: ?*node },
    match_expr: struct { target: *node, arms: []match_arm },

    associate: struct { name: identifier, value: ?*node },
    type: union(enum) { self: void, name: identifier, optional: *node, applied: struct { base: identifier, args: []const *node } },

    pub const match_arm = struct { pattern: *node, body: *node };
    pub const function_decl = struct {
        name: identifier,
        generics: []const generic_param, // <T, U, ...>
        params: []const param,
        return_type: ?*node, // type node
        where_clause: []const where_req,
        body: ?*node, // null for trait signatures

        pub const param = struct { name: identifier, ty: *node };
        pub const where_req = struct { name: identifier, constraint: *node };
    };

    pub const trait_decl = struct {
        name: identifier,
        generics: []const generic_param,
        items: []const trait_item,

        pub const trait_item = union(enum) {
            function: function_decl, // signature-only (body = null)
            assoc_type: associated_type_decl,
        };
    };

    pub const associated_type_decl = struct {
        name: identifier,
        value: ?*node,
    };

    pub const concept_decl = struct {
        name: identifier,
        generics: []const generic_param,
        requires: []const *node, // type node references to traits
    };

    pub const sum_decl = struct {
        name: identifier,
        generics: []const generic_param,
        variants: []const sum_variant,

        pub const sum_variant = struct {
            name: identifier,
            payload: ?*node, // type node
        };
    };
    pub const enum_decl = struct {
        name: identifier,
        cases: []const identifier,
    };

    pub const generic_param = struct {
        name: identifier,
        kind: generic_kind,
        constraint: ?*node,
        default: ?*node,

        pub const generic_kind = enum { type, value };
    };
};
