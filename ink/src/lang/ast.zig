const ink = @import("ink");
const identifier = ink.op.identifier;
pub const node = union(enum) {
    integer: i64,
    float: f32,
    identifier: identifier,
    decl: function,
    unary: struct { op: ink.unary, right: *node },
    binary: struct { left: *node, op: ink.binary, right: *node },
    if_expr: struct { condition: *node, then_branch: *node, else_branch: ?*node },
    match_expr: struct { target: *node, arms: []match_arm },

    pub const match_arm = struct { pattern: *node, body: *node };
    pub const function = struct {
        name: identifier,
        params: []const param,
        return_type: ?identifier,
        body: *node,
        pub const param = struct { name: identifier, ty: identifier };
    };
};
