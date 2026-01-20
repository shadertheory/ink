const ink = @import("ink");
const spec = @import("spec.zig");

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
        label,
        string,
        character,
        number,
        comma,
        colon,
        dot,
        ellipsis,
        hash,
        at_sign,
        function,
        constant,
        variable,
        expr_if,
        expr_else,
        expr_match,
        expr_select,
        case,
        detached,
        stmt_return,
        stmt_break,
        stmt_continue,
        yield,
        loop,
        @"while",
        until,
        repeat,
        @"for",
        each,
        sleep,
        timeout,
        deadline,
        spawn,
        await,
        @"try",
        atomic,
        auto,
        box,
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
        plus_assign,
        minus_assign,
        asterisk_assign,
        slash_assign,
        percent_assign,
        ampersand_assign,
        bar_assign,
        caret_assign,
        shift_left_assign,
        shift_right_assign,
        plus,
        minus,
        asterisk,
        slash,
        percent,
        ampersand,
        bar,
        caret,
        shift_left,
        shift_right,
        bang,
        tilde,
        less_than,
        greater_than,
        less_or_equal,
        greater_or_equal,
        equal,
        not_equal,
        @"enum",
        flag,
        type,
        arrow,
        question,
        question_dot,
        coalesce,
        range,
        range_inclusive,
        double_colon,
        pipe,
        in,
        trait,
        impl,
        @"pub",
        as,
        import,
        mod,
        from,
        with,
        dynamic,
        @"comptime",
        @"struct",
        where,
        self,
        this,
        mut,
        ref,
        requires,
        dyn,
    };

    pub fn precedence_of(this: kind) precedence {
        inline for (precedence_entries) |entry| {
            if (this == entry.kind) return entry.prec;
        }
        return .none;
    }
};

pub const precedence = enum(u8) {
    none = 0,
    assignment,
    pipe,
    logical_or,
    logical_and,
    bit_or,
    bit_xor,
    bit_and,
    comparison,
    range,
    shift,
    sum,
    product,
    unary,
    call,
    primary,

    pub fn value(self: precedence) usize {
        return @as(usize, @intFromEnum(self));
    }

    pub fn cmp(lhs: precedence, rhs: precedence) bool {
        return lhs.value() < rhs.value();
    }
};

const precedence_entry = struct { kind: token.kind, prec: precedence };
const precedence_entries = blk: {
    var entries: [spec.parser_precedence.len]precedence_entry = undefined;
    for (spec.parser_precedence, 0..) |entry, i| {
        entries[i] = .{
            .kind = @field(token.kind, entry.kind),
            .prec = @field(precedence, entry.precedence),
        };
    }
    break :blk entries;
};
