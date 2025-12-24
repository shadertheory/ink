pub const fundamental = enum { i64, f32, unit, identifier, unary, binary };

pub const type_ref = union(enum) {
    builtin: fundamental,
    named: []const u8,
    ptr: *const type_ref,
    opt: *const type_ref,
    slice: slice_ref,
};

pub const slice_ref = struct {
    child: *const type_ref,
    @"const": bool,
};

pub const struct_spec = struct {
    name: []const u8,
    fields: []const field,
};

pub const union_field = struct {
    name: []const u8,
    ty: type_ref,
};

pub const union_spec = struct { name: []const u8, fields: []const union_field };

pub const enum_spec = struct { name: []const u8, tags: []const []const u8 };

pub const type_spec = union(enum) {
    @"struct": struct_spec,
    @"union": union_spec,
    @"enum": enum_spec,
};

const ref_node = type_ref{ .named = "node" };
const ref_node_ptr = type_ref{ .ptr = &ref_node };
const ref_node_ptr_opt = type_ref{ .opt = &ref_node_ptr };
const ref_node_ptr_slice = type_ref{ .slice = .{ .child = &ref_node_ptr, .@"const" = true } };

const ref_identifier = type_ref{ .builtin = .identifier };
const ref_binary = type_ref{ .builtin = .binary };
const ref_unary = type_ref{ .builtin = .unary };

const ref_match_arm = type_ref{ .named = "match_arm" };
const ref_match_arm_slice = type_ref{ .slice = .{ .child = &ref_match_arm, .@"const" = true } };

const ref_generic_param = type_ref{ .named = "generic_param" };
const ref_generic_param_slice = type_ref{ .slice = .{ .child = &ref_generic_param, .@"const" = true } };

const ref_param = type_ref{ .named = "param" };
const ref_param_slice = type_ref{ .slice = .{ .child = &ref_param, .@"const" = true } };

const ref_where_req = type_ref{ .named = "where_req" };
const ref_where_req_slice = type_ref{ .slice = .{ .child = &ref_where_req, .@"const" = true } };

const ref_trait_item = type_ref{ .named = "trait_item" };
const ref_trait_item_slice = type_ref{ .slice = .{ .child = &ref_trait_item, .@"const" = true } };

const ref_sum_variant = type_ref{ .named = "sum_variant" };
const ref_sum_variant_slice = type_ref{ .slice = .{ .child = &ref_sum_variant, .@"const" = true } };

const ref_struct_field = type_ref{ .named = "struct_field" };
const ref_struct_field_slice = type_ref{ .slice = .{ .child = &ref_struct_field, .@"const" = true } };

const ref_function_decl = type_ref{ .named = "function_decl" };
const ref_function_decl_slice = type_ref{ .slice = .{ .child = &ref_function_decl, .@"const" = true } };

const ref_identifier_slice = type_ref{ .slice = .{ .child = &ref_identifier, .@"const" = true } };

pub const field = struct {
    name: []const u8,
    ty: type_ref,
};

pub const ast_node = struct {
    name: []const u8,
    fields: []const field,
};

pub const specification = [_]type_spec{
    .{ .@"enum" = .{
        .name = "generic_kind",
        .tags = &.{ "type", "value" },
    } },
    .{ .@"struct" = .{ .name = "match_arm", .fields = &.{
        .{ .name = "pattern", .ty = ref_node_ptr },
        .{ .name = "body", .ty = ref_node_ptr },
    } } },
    .{ .@"struct" = .{ .name = "param", .fields = &.{
        .{ .name = "name", .ty = ref_identifier },
        .{ .name = "ty", .ty = ref_node_ptr },
    } } },
    .{ .@"struct" = .{ .name = "where_req", .fields = &.{
        .{ .name = "name", .ty = ref_identifier },
        .{ .name = "constraint", .ty = ref_node_ptr },
    } } },
    .{ .@"struct" = .{ .name = "function_decl", .fields = &.{
        .{ .name = "name", .ty = ref_identifier },
        .{ .name = "generics", .ty = ref_generic_param_slice },
        .{ .name = "params", .ty = ref_param_slice },
        .{ .name = "return_type", .ty = ref_node_ptr_opt },
        .{ .name = "where_clause", .ty = ref_where_req_slice },
        .{ .name = "body", .ty = ref_node_ptr_opt },
    } } },

    .{ .@"struct" = .{
        .name = "struct_field",
        .fields = &.{
            .{ .name = "name", .ty = ref_identifier },
            .{ .name = "ty", .ty = ref_node_ptr },
        },
    } },

    .{ .@"struct" = .{
        .name = "struct_decl",
        .fields = &.{
            .{ .name = "name", .ty = ref_identifier },
            .{ .name = "generics", .ty = ref_generic_param_slice },
            .{ .name = "fields", .ty = ref_struct_field_slice },
        },
    } },

    .{ .@"struct" = .{
        .name = "impl_decl",
        .fields = &.{
            .{ .name = "by_trait", .ty = ref_identifier },
            .{ .name = "for_struct", .ty = ref_identifier },
            .{ .name = "functions", .ty = ref_function_decl_slice },
        },
    } },

    .{ .@"union" = .{
        .name = "trait_item",
        .fields = &.{
            .{ .name = "function", .ty = type_ref{ .named = "function_decl" } },
            .{ .name = "assoc_type", .ty = type_ref{ .named = "associated_type_decl" } },
        },
    } },

    .{ .@"struct" = .{
        .name = "associated_type_decl",
        .fields = &.{
            .{ .name = "name", .ty = ref_identifier },
            .{ .name = "value", .ty = ref_node_ptr_opt },
        },
    } },

    .{ .@"struct" = .{
        .name = "trait_decl",
        .fields = &.{
            .{ .name = "name", .ty = ref_identifier },
            .{ .name = "generics", .ty = ref_generic_param_slice },
            .{ .name = "items", .ty = ref_trait_item_slice },
        },
    } },

    .{ .@"struct" = .{
        .name = "concept_decl",
        .fields = &.{
            .{ .name = "name", .ty = ref_identifier },
            .{ .name = "generics", .ty = ref_generic_param_slice },
            .{ .name = "requires", .ty = ref_node_ptr_slice },
        },
    } },

    .{ .@"struct" = .{
        .name = "sum_decl",
        .fields = &.{
            .{ .name = "name", .ty = ref_identifier },
            .{ .name = "generics", .ty = ref_generic_param_slice },
            .{ .name = "variants", .ty = ref_sum_variant_slice },
        },
    } },

    .{ .@"struct" = .{
        .name = "sum_variant",
        .fields = &.{
            .{ .name = "name", .ty = ref_identifier },
            .{ .name = "payload", .ty = ref_node_ptr_opt },
        },
    } },

    .{ .@"struct" = .{
        .name = "enum_decl",
        .fields = &.{
            .{ .name = "name", .ty = ref_identifier },
            .{ .name = "cases", .ty = ref_identifier_slice },
        },
    } },

    .{ .@"struct" = .{
        .name = "generic_param",
        .fields = &.{
            .{ .name = "name", .ty = ref_identifier },
            .{ .name = "kind", .ty = type_ref{ .named = "generic_kind" } },
            .{ .name = "constraint", .ty = ref_node_ptr_opt },
            .{ .name = "default", .ty = ref_node_ptr_opt },
        },
    } },

    .{ .@"struct" = .{
        .name = "unary_expr",
        .fields = &.{
            .{ .name = "op", .ty = ref_unary },
            .{ .name = "right", .ty = ref_node_ptr },
        },
    } },

    .{ .@"struct" = .{
        .name = "binary_expr",
        .fields = &.{
            .{ .name = "left", .ty = ref_node_ptr },
            .{ .name = "op", .ty = ref_binary },
            .{ .name = "right", .ty = ref_node_ptr },
        },
    } },

    .{ .@"struct" = .{
        .name = "if_expr",
        .fields = &.{
            .{ .name = "condition", .ty = ref_node_ptr },
            .{ .name = "then_branch", .ty = ref_node_ptr },
            .{ .name = "else_branch", .ty = ref_node_ptr_opt },
        },
    } },

    .{ .@"struct" = .{
        .name = "match_expr",
        .fields = &.{
            .{ .name = "target", .ty = ref_node_ptr },
            .{ .name = "arms", .ty = ref_match_arm_slice },
        },
    } },

    .{ .@"struct" = .{
        .name = "block_expr",
        .fields = &.{
            .{ .name = "items", .ty = ref_node_ptr_slice },
        },
    } },

    .{ .@"struct" = .{
        .name = "associate",
        .fields = &.{
            .{ .name = "name", .ty = ref_identifier },
            .{ .name = "value", .ty = ref_node_ptr_opt },
        },
    } },

    .{ .@"struct" = .{
        .name = "type_applied",
        .fields = &.{
            .{ .name = "base", .ty = ref_identifier },
            .{ .name = "args", .ty = ref_node_ptr_slice },
        },
    } },

    .{ .@"union" = .{
        .name = "type_expr",
        .fields = &.{
            .{ .name = "self", .ty = type_ref{ .builtin = .unit } },
            .{ .name = "name", .ty = ref_identifier },
            .{ .name = "optional", .ty = ref_node_ptr },
            .{ .name = "applied", .ty = type_ref{ .named = "type_applied" } },
        },
    } },

    .{ .@"union" = .{
        .name = "decl",
        .fields = &.{
            .{ .name = "function", .ty = type_ref{ .named = "function_decl" } },
            .{ .name = "struct", .ty = type_ref{ .named = "struct_decl" } },
            .{ .name = "trait", .ty = type_ref{ .named = "trait_decl" } },
            .{ .name = "concept", .ty = type_ref{ .named = "concept_decl" } },
            .{ .name = "sum", .ty = type_ref{ .named = "sum_decl" } },
            .{ .name = "enum", .ty = type_ref{ .named = "enum_decl" } },
            .{ .name = "impl", .ty = type_ref{ .named = "impl_decl" } },
        },
    } },
};

pub const node_union = union_spec{
    .name = "node",
    .fields = &.{
        .{ .name = "integer", .ty = type_ref{ .builtin = .i64 } },
        .{ .name = "float", .ty = type_ref{ .builtin = .f32 } },
        .{ .name = "identifier", .ty = ref_identifier },
        .{ .name = "decl", .ty = type_ref{ .named = "decl" } },
        .{ .name = "unary", .ty = type_ref{ .named = "unary_expr" } },
        .{ .name = "binary", .ty = type_ref{ .named = "binary_expr" } },
        .{ .name = "if_expr", .ty = type_ref{ .named = "if_expr" } },
        .{ .name = "match_expr", .ty = type_ref{ .named = "match_expr" } },
        .{ .name = "block", .ty = type_ref{ .named = "block_expr" } },
        .{ .name = "associate", .ty = type_ref{ .named = "associate" } },
        .{ .name = "type", .ty = type_ref{ .named = "type_expr" } },
    },
};

pub const binary = struct { name: []const u8, ir_name: []const u8 };
pub const unary = struct { name: []const u8, ir_name: []const u8 };
pub const lexeme = struct { text: []const u8, kind: []const u8 };

pub const binarys = [_]binary{
    .{ .name = "add", .ir_name = "add" },
    .{ .name = "sub", .ir_name = "sub" },
    .{ .name = "mul", .ir_name = "mul" },
    .{ .name = "div", .ir_name = "div" },
    .{ .name = "mod", .ir_name = "mod" },
    .{ .name = "min", .ir_name = "min" },
    .{ .name = "max", .ir_name = "max" },
    .{ .name = "equal", .ir_name = "equal" },
    .{ .name = "not_equal", .ir_name = "not_equal" },
    .{ .name = "less_than", .ir_name = "less_than" },
    .{ .name = "less_or_equal", .ir_name = "less_or_equal" },
    .{ .name = "greater_than", .ir_name = "greater_than" },
    .{ .name = "greater_or_equal", .ir_name = "greater_or_equal" },
    .{ .name = "call", .ir_name = "call" },
    .{ .name = "pipe", .ir_name = "pipe" },
};

pub const unarys = [_]unary{
    .{ .name = "neg", .ir_name = "neg" },
    .{ .name = "not", .ir_name = "not" },
    .{ .name = "abs", .ir_name = "abs" },
    .{ .name = "sqrt", .ir_name = "sqrt" },
    .{ .name = "sin", .ir_name = "sin" },
    .{ .name = "cos", .ir_name = "cos" },
    .{ .name = "tan", .ir_name = "tan" },
    .{ .name = "asin", .ir_name = "asin" },
    .{ .name = "acos", .ir_name = "acos" },
    .{ .name = "atan", .ir_name = "atan" },
    .{ .name = "floor", .ir_name = "floor" },
    .{ .name = "ceil", .ir_name = "ceil" },
    .{ .name = "round", .ir_name = "round" },
    .{ .name = "trunc", .ir_name = "trunc" },
    .{ .name = "ret", .ir_name = "ret" },
};

pub const keyword_lexemes = [_]lexeme{
    .{ .text = "fn", .kind = "function" },
    .{ .text = "const", .kind = "constant" },
    .{ .text = "var", .kind = "variable" },
    .{ .text = "if", .kind = "expr_if" },
    .{ .text = "else", .kind = "expr_else" },
    .{ .text = "match", .kind = "expr_match" },
    .{ .text = "return", .kind = "stmt_return" },
    .{ .text = "or", .kind = "logical_or" },
    .{ .text = "and", .kind = "logical_and" },
    .{ .text = "xor", .kind = "logical_xor" },
    .{ .text = "not", .kind = "logical_not" },
    .{ .text = "false", .kind = "logical_false" },
    .{ .text = "true", .kind = "logical_true" },
    .{ .text = "trait", .kind = "trait" },
    .{ .text = "concept", .kind = "concept" },
    .{ .text = "in", .kind = "in" },
    .{ .text = "impl", .kind = "impl" },
    .{ .text = "for", .kind = "for" },
    .{ .text = "struct", .kind = "struct" },
    .{ .text = "self", .kind = "self" },
    .{ .text = "this", .kind = "this" },
    .{ .text = "type", .kind = "type" },
    .{ .text = "enum", .kind = "enum" },
    .{ .text = "where", .kind = "where" },
    .{ .text = "sum", .kind = "sum" },
    .{ .text = "requires", .kind = "requires" },
};

pub const symbol_lexemes = [_]lexeme{
    .{ .text = "..=", .kind = "range_inclusive" },
    .{ .text = "->", .kind = "arrow" },
    .{ .text = "=>", .kind = "arrow" },
    .{ .text = "==", .kind = "equal" },
    .{ .text = "!=", .kind = "not_equal" },
    .{ .text = ">=", .kind = "greater_or_equal" },
    .{ .text = "<=", .kind = "less_or_equal" },
    .{ .text = "?.", .kind = "question_dot" },
    .{ .text = "??", .kind = "coalesce" },
    .{ .text = "::", .kind = "double_colon" },
    .{ .text = "..", .kind = "range" },
    .{ .text = "|>", .kind = "pipe" },
    .{ .text = "?", .kind = "question" },
    .{ .text = "]", .kind = "bracket_right" },
    .{ .text = "[", .kind = "bracket_left" },
    .{ .text = ")", .kind = "paren_right" },
    .{ .text = "(", .kind = "paren_left" },
    .{ .text = "+", .kind = "plus" },
    .{ .text = "*", .kind = "asterisk" },
    .{ .text = "/", .kind = "slash" },
    .{ .text = "=", .kind = "assign" },
    .{ .text = ":", .kind = "colon" },
    .{ .text = "!", .kind = "bang" },
    .{ .text = "<", .kind = "less_than" },
    .{ .text = ">", .kind = "greater_than" },
    .{ .text = ",", .kind = "comma" },
    .{ .text = ".", .kind = "dot" },
    .{ .text = "-", .kind = "minus" },
};

pub const alpha_chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";
pub const digit_chars = "0123456789";
pub const identifier_start = alpha_chars;
pub const identifier_continue = alpha_chars ++ digit_chars;

pub const whitespace = struct {
    pub const space: u8 = ' ';
    pub const tab: u8 = '\t';
    pub const newline: u8 = '\n';
};

pub const eof_char: u8 = 0;

pub const number = struct {
    pub const decimal_separator: u8 = '.';
    pub const exponent_markers = "eE";
    pub const exponent_signs = "+-";
};

pub const generic_value_kinds = [_][]const u8{
    "int",
    "bool",
    "float",
};

pub const parser_precedence = [_]struct { kind: []const u8, precedence: []const u8 }{
    .{ .kind = "assign", .precedence = "assignment" },
    .{ .kind = "pipe", .precedence = "pipe" },
    .{ .kind = "logical_or", .precedence = "logical_or" },
    .{ .kind = "logical_xor", .precedence = "logical_or" },
    .{ .kind = "logical_and", .precedence = "logical_and" },
    .{ .kind = "less_than", .precedence = "comparison" },
    .{ .kind = "greater_than", .precedence = "comparison" },
    .{ .kind = "less_or_equal", .precedence = "comparison" },
    .{ .kind = "greater_or_equal", .precedence = "comparison" },
    .{ .kind = "equal", .precedence = "comparison" },
    .{ .kind = "not_equal", .precedence = "comparison" },
    .{ .kind = "plus", .precedence = "sum" },
    .{ .kind = "minus", .precedence = "sum" },
    .{ .kind = "asterisk", .precedence = "product" },
    .{ .kind = "slash", .precedence = "product" },
    .{ .kind = "bang", .precedence = "unary" },
    .{ .kind = "logical_not", .precedence = "unary" },
    .{ .kind = "paren_left", .precedence = "call" },
    .{ .kind = "bracket_left", .precedence = "call" },
    .{ .kind = "dot", .precedence = "call" },
};

pub const parser_unary_ops = [_]struct { kind: []const u8, op: []const u8 }{
    .{ .kind = "minus", .op = "neg" },
    .{ .kind = "bang", .op = "not" },
    .{ .kind = "logical_not", .op = "not" },
    .{ .kind = "stmt_return", .op = "ret" },
};

pub const parser_binary_ops = [_]struct { kind: []const u8, op: []const u8 }{
    .{ .kind = "plus", .op = "add" },
    .{ .kind = "minus", .op = "sub" },
    .{ .kind = "asterisk", .op = "mul" },
    .{ .kind = "slash", .op = "div" },
    .{ .kind = "pipe", .op = "pipe" },
    .{ .kind = "less_than", .op = "less_than" },
    .{ .kind = "greater_than", .op = "greater_than" },
    .{ .kind = "less_or_equal", .op = "less_or_equal" },
    .{ .kind = "greater_or_equal", .op = "greater_or_equal" },
    .{ .kind = "equal", .op = "equal" },
    .{ .kind = "not_equal", .op = "not_equal" },
    .{ .kind = "paren_left", .op = "call" },
};

pub const parser_prefix_kinds = [_][]const u8{
    "number",
    "identifier",
    "expr_if",
    "expr_match",
    "function",
    "struct",
    "trait",
    "concept",
    "sum",
    "enum",
    "impl",
    "stmt_return",
    "paren_left",
    "bracket_left",
    "minus",
    "bang",
    "logical_not",
    "logical_true",
    "logical_false",
    "this",
};
