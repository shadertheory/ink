const ink = @import("ink");

pub const token = struct {
    which: kind,
    where: ink.location,
    what: ink.identifier,

    pub const kind = enum {
        end_of_file,
        illegal,

        indent,
        dedent,
        new_line,

        identifier,
        string,
        number,
        comma,
        colon,
        dot,

        function,
        constant,
        variable,
        expr_if,
        expr_else,
        expr_match,
        stmt_return,
        logical_or,
        logical_and,
        logical_xor,
        logical_not,
        logical_false,
        logical_true,

        paren_left,
        paren_right,
        bracket_left,
        bracket_right,

        assign,
        plus,
        minus,
        asterisk,
        slash,
        bang,

        less_than,
        greater_than,

        arrow
    };

    pub fn precedence_of(this: kind) precedence {
        return switch (this) {
            .assign => .assignment,
            .logical_or, .logical_xor => .logical_or,
            .logical_and => .logical_and, 
            .less_than, .greater_than => .comparison,
            .plus, .minus => .sum,
            .asterisk, .slash => .product,
            .bang, .logical_not => .unary,
            .paren_left, .bracket_left, .dot => .call,
            else => .none,
        };
    } 
};

pub const precedence = enum(u8) {
    none = 0,
    assignment,
    logical_or,
    logical_and,
    comparison,
    sum,
    product,
    unary,
    call,
    primary,

    pub fn value(self: precedence)  usize {
        return @as(usize, @intFromEnum(self));
    }

    pub fn cmp(lhs: precedence, rhs: precedence)  bool {
        return lhs.value() < rhs.value();
    }
};
