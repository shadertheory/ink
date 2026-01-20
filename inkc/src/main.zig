const std = @import("std");
const ink = @import("ink");
const inkc = @import("inkc");
const graph = @import("graph.zig");
const manifest = @import("manifest.zig");
const target_mod = ink.target;

const mem_allocator = std.mem.Allocator;

const Cli = struct {
    const Options = struct {
        input_path: ?[]const u8 = null,
        output_path: ?[]const u8 = null,
        manifest_path: ?[]const u8 = null,
        profile: ?[]const u8 = null,
        target: ?[]const u8 = null,
    };

    const ParseError = error{InvalidArgs};

    fn parse(args: []const []const u8, p: *std.Io.Writer) ParseError!Options {
        var input_path: ?[]const u8 = null;
        var output_path: ?[]const u8 = null;
        var manifest_path: ?[]const u8 = null;
        var profile: ?[]const u8 = null;
        var target: ?[]const u8 = null;

        var i: usize = 1;
        while (i < args.len) : (i += 1) {
            const arg = args[i];
            if (std.mem.eql(u8, arg, "--output") or std.mem.eql(u8, arg, "-o")) {
                if (i + 1 >= args.len) {
                    p.print("error: missing value for {s}\n", .{arg}) catch {};
                    print_usage(p, args[0]) catch {};
                    return error.InvalidArgs;
                }
                output_path = args[i + 1];
                i += 1;
                continue;
            }
            if (std.mem.eql(u8, arg, "--manifest") or std.mem.eql(u8, arg, "-m")) {
                if (i + 1 >= args.len) {
                    p.print("error: missing value for {s}\n", .{arg}) catch {};
                    print_usage(p, args[0]) catch {};
                    return error.InvalidArgs;
                }
                manifest_path = args[i + 1];
                i += 1;
                continue;
            }
            if (std.mem.eql(u8, arg, "--profile")) {
                if (i + 1 >= args.len) {
                    p.print("error: missing value for {s}\n", .{arg}) catch {};
                    print_usage(p, args[0]) catch {};
                    return error.InvalidArgs;
                }
                profile = args[i + 1];
                i += 1;
                continue;
            }
            if (std.mem.eql(u8, arg, "--target") or std.mem.eql(u8, arg, "-t")) {
                if (i + 1 >= args.len) {
                    p.print("error: missing value for {s}\n", .{arg}) catch {};
                    print_usage(p, args[0]) catch {};
                    return error.InvalidArgs;
                }
                target = args[i + 1];
                i += 1;
                continue;
            }
            if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
                print_usage(p, args[0]) catch {};
                return error.InvalidArgs;
            }
            if (input_path == null) {
                input_path = arg;
            } else {
                p.print("error: unexpected argument: {s}\n", .{arg}) catch {};
                print_usage(p, args[0]) catch {};
                return error.InvalidArgs;
            }
        }

        if (manifest_path != null and input_path != null) {
            p.print("error: cannot combine --manifest with a source file\n", .{}) catch {};
            print_usage(p, args[0]) catch {};
            return error.InvalidArgs;
        }

        if (manifest_path == null and input_path == null) {
            print_usage(p, args[0]) catch {};
            return error.InvalidArgs;
        }

        return .{
            .input_path = input_path,
            .output_path = output_path,
            .manifest_path = manifest_path,
            .profile = profile,
            .target = target,
        };
    }

    fn print_usage(p: *std.Io.Writer, exe_name: []const u8) !void {
        try p.print("Usage: {s} [-o <path>] [-t <target>] <source_file>\n", .{exe_name});
        try p.print("       {s} --manifest <path> [--profile <name>] [-o <path>] [-t <target>]\n", .{exe_name});
    }
};

pub fn printer(writer_type: type) type {
    return struct {
        const Self = @This();
        const WriterError = switch (@typeInfo(writer_type)) {
            .pointer => |ptr| ptr.child.Error,
            else => writer_type.Error,
        };
        writer: writer_type,

        fn deref_node(ptr: ink.ast.node_ref) *const ink.node {
            return ink.ast.deref(ptr);
        }

        fn deref_node_opt(ptr: ?ink.ast.node_ref) ?*const ink.node {
            return if (ptr) |p| deref_node(p) else null;
        }

        fn print_indent(self: *Self, depth: usize) WriterError!void {
            var i: usize = 0;
            while (i < depth) : (i += 1) {
                try self.writer.writeAll("  ");
            }
        }

        fn print_line(self: *Self, depth: usize, comptime fmt: []const u8, args: anytype) WriterError!void {
            try self.print_indent(depth);
            try self.writer.print(fmt, args);
            try self.writer.writeByte('\n');
        }

        fn print_node_field(self: *Self, label: []const u8, node_ref: ink.ast.node_ref, depth: usize) WriterError!void {
            try self.print_indent(depth);
            try self.writer.print("({s}\n", .{label});
            try self.print_node_tree(deref_node(node_ref), depth + 1);
            try self.print_line(depth, ")", .{});
        }

        fn print_node_opt_field(self: *Self, label: []const u8, node_ref: ?ink.ast.node_ref, depth: usize) WriterError!void {
            if (deref_node_opt(node_ref)) |node| {
                try self.print_indent(depth);
                try self.writer.print("({s}\n", .{label});
                try self.print_node_tree(node, depth + 1);
                try self.print_line(depth, ")", .{});
            } else {
                try self.print_line(depth, "({s} nil)", .{label});
            }
        }

        fn print_node_list(self: *Self, label: []const u8, nodes: []const ink.ast.node_ref, depth: usize) WriterError!void {
            if (nodes.len == 0) {
                try self.print_line(depth, "({s})", .{label});
                return;
            }

            try self.print_indent(depth);
            try self.writer.print("({s}\n", .{label});
            for (nodes) |node_ref| {
                try self.print_node_tree(deref_node(node_ref), depth + 1);
            }
            try self.print_line(depth, ")", .{});
        }

        fn print_identifier_field(self: *Self, label: []const u8, ident: ink.identifier, depth: usize) WriterError!void {
            try self.print_line(depth, "({s} \"{s}\")", .{ label, ident.string });
        }

        fn print_identifier_list(
            self: *Self,
            list_label: []const u8,
            item_label: []const u8,
            items: []const ink.identifier,
            depth: usize,
        ) WriterError!void {
            if (items.len == 0) {
                try self.print_line(depth, "({s})", .{list_label});
                return;
            }

            try self.print_indent(depth);
            try self.writer.print("({s}\n", .{list_label});
            for (items) |item| {
                try self.print_line(depth + 1, "({s} \"{s}\")", .{ item_label, item.string });
            }
            try self.print_line(depth, ")", .{});
        }

        fn print_generic_param_list(
            self: *Self,
            label: []const u8,
            params: []const ink.ast.generic_param,
            depth: usize,
        ) WriterError!void {
            if (params.len == 0) {
                try self.print_line(depth, "({s})", .{label});
                return;
            }

            try self.print_indent(depth);
            try self.writer.print("({s}\n", .{label});
            for (params) |param| {
                try self.print_indent(depth + 1);
                try self.writer.print("(param\n", .{});
                try self.print_identifier_field("name", param.name, depth + 2);
                try self.print_line(depth + 2, "(kind {s})", .{@tagName(param.kind)});
                try self.print_node_opt_field("constraint", param.constraint, depth + 2);
                try self.print_node_opt_field("default", param.default, depth + 2);
                try self.print_line(depth + 1, ")", .{});
            }
            try self.print_line(depth, ")", .{});
        }

        fn print_param_list(self: *Self, label: []const u8, params: []const ink.ast.param, depth: usize) WriterError!void {
            if (params.len == 0) {
                try self.print_line(depth, "({s})", .{label});
                return;
            }

            try self.print_indent(depth);
            try self.writer.print("({s}\n", .{label});
            for (params) |param| {
                try self.print_indent(depth + 1);
                try self.writer.print("(param\n", .{});
                try self.print_identifier_field("name", param.name, depth + 2);
                try self.print_node_field("ty", param.ty, depth + 2);
                try self.print_line(depth + 1, ")", .{});
            }
            try self.print_line(depth, ")", .{});
        }

        fn print_struct_field_list(self: *Self, label: []const u8, fields: []const ink.ast.struct_field, depth: usize) WriterError!void {
            if (fields.len == 0) {
                try self.print_line(depth, "({s})", .{label});
                return;
            }

            try self.print_indent(depth);
            try self.writer.print("({s}\n", .{label});
            for (fields) |field| {
                try self.print_indent(depth + 1);
                try self.writer.print("(field\n", .{});
                try self.print_identifier_field("name", field.name, depth + 2);
                try self.print_node_field("ty", field.ty, depth + 2);
                try self.print_line(depth + 1, ")", .{});
            }
            try self.print_line(depth, ")", .{});
        }

        fn print_associate_list(self: *Self, label: []const u8, items: []const ink.ast.associate, depth: usize) WriterError!void {
            if (items.len == 0) {
                try self.print_line(depth, "({s})", .{label});
                return;
            }

            try self.print_indent(depth);
            try self.writer.print("({s}\n", .{label});
            for (items) |item| {
                try self.print_indent(depth + 1);
                try self.writer.print("(field\n", .{});
                try self.print_identifier_field("name", item.name, depth + 2);
                if (item.value) |ref| {
                    try self.print_node_field("value", ref, depth + 2);
                } else {
                    try self.print_line(depth + 2, "(value nil)", .{});
                }
                try self.print_line(depth + 1, ")", .{});
            }
            try self.print_line(depth, ")", .{});
        }

        fn print_function_decl_list(self: *Self, label: []const u8, functions: []const ink.ast.function_decl, depth: usize) WriterError!void {
            if (functions.len == 0) {
                try self.print_line(depth, "({s})", .{label});
                return;
            }

            try self.print_indent(depth);
            try self.writer.print("({s}\n", .{label});
            for (functions) |func| {
                try self.print_function_decl(func, depth + 1);
            }
            try self.print_line(depth, ")", .{});
        }

        fn print_where_clause(self: *Self, label: []const u8, clause: []const ink.ast.where_req, depth: usize) WriterError!void {
            if (clause.len == 0) {
                try self.print_line(depth, "({s})", .{label});
                return;
            }

            try self.print_indent(depth);
            try self.writer.print("({s}\n", .{label});
            for (clause) |req| {
                try self.print_indent(depth + 1);
                try self.writer.print("(where_req\n", .{});
                try self.print_identifier_field("name", req.name, depth + 2);
                try self.print_node_field("constraint", req.constraint, depth + 2);
                try self.print_line(depth + 1, ")", .{});
            }
            try self.print_line(depth, ")", .{});
        }

        fn print_function_decl(self: *Self, func: ink.ast.function_decl, depth: usize) WriterError!void {
            try self.print_indent(depth);
            try self.writer.print("(function_decl\n", .{});
            try self.print_identifier_field("name", func.name, depth + 1);
            try self.print_generic_param_list("generics", func.generics, depth + 1);
            try self.print_param_list("params", func.params, depth + 1);
            try self.print_node_opt_field("return_type", func.return_type, depth + 1);
            try self.print_where_clause("where_clause", func.where_clause, depth + 1);
            try self.print_node_opt_field("body", func.body, depth + 1);
            try self.print_line(depth, ")", .{});
        }

        fn print_associated_type_decl(self: *Self, assoc: ink.ast.associated_type_decl, depth: usize) WriterError!void {
            try self.print_indent(depth);
            try self.writer.print("(assoc_type\n", .{});
            try self.print_identifier_field("name", assoc.name, depth + 1);
            try self.print_node_opt_field("value", assoc.value, depth + 1);
            try self.print_line(depth, ")", .{});
        }

        fn print_trait_items(self: *Self, label: []const u8, items: []const ink.ast.trait_item, depth: usize) WriterError!void {
            if (items.len == 0) {
                try self.print_line(depth, "({s})", .{label});
                return;
            }

            try self.print_indent(depth);
            try self.writer.print("({s}\n", .{label});
            for (items) |item| {
                switch (item) {
                    .function => |func| try self.print_function_decl(func, depth + 1),
                    .assoc_type => |assoc| try self.print_associated_type_decl(assoc, depth + 1),
                }
            }
            try self.print_line(depth, ")", .{});
        }

        fn print_match_arm_list(self: *Self, label: []const u8, arms: []const ink.ast.match_arm, depth: usize) WriterError!void {
            if (arms.len == 0) {
                try self.print_line(depth, "({s})", .{label});
                return;
            }

            try self.print_indent(depth);
            try self.writer.print("({s}\n", .{label});
            for (arms) |arm| {
                try self.print_indent(depth + 1);
                try self.writer.print("(match_arm\n", .{});
                try self.print_node_field("pattern", arm.pattern, depth + 2);
                try self.print_node_field("body", arm.body, depth + 2);
                try self.print_line(depth + 1, ")", .{});
            }
            try self.print_line(depth, ")", .{});
        }

        fn print_sum_variant_list(self: *Self, label: []const u8, variants: []const ink.ast.sum_variant, depth: usize) WriterError!void {
            if (variants.len == 0) {
                try self.print_line(depth, "({s})", .{label});
                return;
            }

            try self.print_indent(depth);
            try self.writer.print("({s}\n", .{label});
            for (variants) |variant| {
                try self.print_indent(depth + 1);
                try self.writer.print("(sum_variant\n", .{});
                try self.print_identifier_field("name", variant.name, depth + 2);
                try self.print_node_opt_field("payload", variant.payload, depth + 2);
                try self.print_line(depth + 1, ")", .{});
            }
            try self.print_line(depth, ")", .{});
        }

        fn print_type_expr(self: *Self, ty: ink.ast.type_expr, depth: usize) WriterError!void {
            try self.print_indent(depth);
            try self.writer.print("(type\n", .{});
            switch (ty) {
                .self => {
                    try self.print_line(depth + 1, "(self)", .{});
                },
                .name => |name| {
                    try self.print_identifier_field("name", name, depth + 1);
                },
                .optional => |opt| {
                    try self.print_indent(depth + 1);
                    try self.writer.print("(optional\n", .{});
                    try self.print_node_tree(deref_node(opt), depth + 2);
                    try self.print_line(depth + 1, ")", .{});
                },
                .dyn => |inner| {
                    try self.print_indent(depth + 1);
                    try self.writer.print("(dyn\n", .{});
                    try self.print_node_tree(deref_node(inner), depth + 2);
                    try self.print_line(depth + 1, ")", .{});
                },
                .applied => |ap| {
                    try self.print_indent(depth + 1);
                    try self.writer.print("(applied\n", .{});
                    try self.print_identifier_field("base", ap.base, depth + 2);
                    try self.print_node_list("args", ap.args, depth + 2);
                    try self.print_line(depth + 1, ")", .{});
                },
            }
            try self.print_line(depth, ")", .{});
        }

        fn print_node_tree(self: *Self, node: *const ink.node, depth: usize) WriterError!void {
            switch (node.*) {
                .integer => |val| {
                    try self.print_line(depth, "(integer {d})", .{val});
                },
                .float => |val| {
                    try self.print_line(depth, "(float {d})", .{val});
                },
                .string => |val| {
                    try self.print_line(depth, "(string \"{s}\")", .{val.string});
                },
                .identifier => |name| {
                    try self.print_line(depth, "(identifier \"{s}\")", .{name.string});
                },

                .decl => |decl| switch (decl) {
                    .function => |func| try self.print_function_decl(func, depth),
                    .@"struct" => |struct_decl| {
                        try self.print_indent(depth);
                        try self.writer.print("(struct_decl\n", .{});
                        try self.print_identifier_field("name", struct_decl.name, depth + 1);
                        try self.print_generic_param_list("generics", struct_decl.generics, depth + 1);
                        try self.print_struct_field_list("fields", struct_decl.fields, depth + 1);
                        try self.print_line(depth, ")", .{});
                    },
                    .trait => |trait_decl| {
                        try self.print_indent(depth);
                        try self.writer.print("(trait_decl\n", .{});
                        try self.print_identifier_field("name", trait_decl.name, depth + 1);
                        try self.print_generic_param_list("generics", trait_decl.generics, depth + 1);
                        try self.print_trait_items("items", trait_decl.items, depth + 1);
                        try self.print_node_list("requires", trait_decl.requires, depth + 1);
                        try self.print_line(depth, ")", .{});
                    },
                    .@"enum" => |enum_decl| {
                        try self.print_indent(depth);
                        try self.writer.print("(enum_decl\n", .{});
                        try self.print_identifier_field("name", enum_decl.name, depth + 1);
                        try self.print_generic_param_list("generics", enum_decl.generics, depth + 1);
                        try self.print_sum_variant_list("variants", enum_decl.variants, depth + 1);
                        try self.print_line(depth, ")", .{});
                    },
                    .impl => |impl_decl| {
                        try self.print_indent(depth);
                        try self.writer.print("(impl_decl\n", .{});
                        try self.print_identifier_field("by_trait", impl_decl.by_trait, depth + 1);
                        try self.print_identifier_field("for_struct", impl_decl.for_struct, depth + 1);
                        try self.print_function_decl_list("functions", impl_decl.functions, depth + 1);
                        try self.print_line(depth, ")", .{});
                    },
                    .import => |import_decl| {
                        try self.print_indent(depth);
                        try self.writer.print("(import_decl\n", .{});
                        try self.print_identifier_field("module", import_decl.module, depth + 1);
                        if (import_decl.item) |item| {
                            try self.print_identifier_field("item", item, depth + 1);
                        } else {
                            try self.print_line(depth + 1, "(item nil)", .{});
                        }
                        if (import_decl.alias) |alias| {
                            try self.print_identifier_field("alias", alias, depth + 1);
                        } else {
                            try self.print_line(depth + 1, "(alias nil)", .{});
                        }
                        try self.print_line(depth, ")", .{});
                    },
                    .@"const" => |const_decl| {
                        try self.print_indent(depth);
                        try self.writer.print("(const_decl\n", .{});
                        try self.print_identifier_field("name", const_decl.name, depth + 1);
                        try self.print_node_opt_field("ty", const_decl.ty, depth + 1);
                        try self.print_node_field("value", const_decl.value, depth + 1);
                        try self.print_line(depth, ")", .{});
                    },
                    .@"var" => |var_decl| {
                        try self.print_indent(depth);
                        try self.writer.print("(var_decl\n", .{});
                        try self.print_identifier_field("name", var_decl.name, depth + 1);
                        try self.print_node_opt_field("ty", var_decl.ty, depth + 1);
                        try self.print_node_field("value", var_decl.value, depth + 1);
                        try self.print_line(depth, ")", .{});
                    },
                },

                .binary => |bin| {
                    try self.print_indent(depth);
                    try self.writer.print("(binary\n", .{});
                    try self.print_line(depth + 1, "(op {s})", .{@tagName(bin.op)});
                    try self.print_node_field("left", bin.left, depth + 1);
                    try self.print_node_field("right", bin.right, depth + 1);
                    try self.print_line(depth, ")", .{});
                },

                .unary => |un| {
                    try self.print_indent(depth);
                    try self.writer.print("(unary\n", .{});
                    try self.print_line(depth + 1, "(op {s})", .{@tagName(un.op)});
                    try self.print_node_field("right", un.right, depth + 1);
                    try self.print_line(depth, ")", .{});
                },

                .if_expr => |ife| {
                    try self.print_indent(depth);
                    try self.writer.print("(if_expr\n", .{});
                    try self.print_node_field("condition", ife.condition, depth + 1);
                    try self.print_node_field("then_branch", ife.then_branch, depth + 1);
                    try self.print_node_opt_field("else_branch", ife.else_branch, depth + 1);
                    try self.print_line(depth, ")", .{});
                },

                .match_expr => |me| {
                    try self.print_indent(depth);
                    try self.writer.print("(match_expr\n", .{});
                    try self.print_node_field("target", me.target, depth + 1);
                    try self.print_match_arm_list("arms", me.arms, depth + 1);
                    try self.print_line(depth, ")", .{});
                },
                .block => |blk| {
                    try self.print_indent(depth);
                    try self.writer.print("(block\n", .{});
                    try self.print_node_list("items", blk.items, depth + 1);
                    try self.print_line(depth, ")", .{});
                },

                .record => |rec| {
                    try self.print_indent(depth);
                    try self.writer.print("(record\n", .{});
                    try self.print_associate_list("items", rec.items, depth + 1);
                    try self.print_line(depth, ")", .{});
                },

                .associate => |assoc| {
                    try self.print_indent(depth);
                    try self.writer.print("(associate\n", .{});
                    try self.print_identifier_field("name", assoc.name, depth + 1);
                    try self.print_node_opt_field("value", assoc.value, depth + 1);
                    try self.print_line(depth, ")", .{});
                },

                .type => |ty| try self.print_type_expr(ty, depth),
            }
        }
    };
}

const line_info = struct {
    line: usize,
    column: usize,
    line_start: usize,
    line_end: usize,
};

fn lineInfo(source: []const u8, pos: usize) line_info {
    var line: usize = 1;
    var column: usize = 1;
    var line_start: usize = 0;
    var i: usize = 0;
    while (i < pos and i < source.len) : (i += 1) {
        if (source[i] == '\n') {
            line += 1;
            column = 1;
            line_start = i + 1;
        } else {
            column += 1;
        }
    }
    var line_end = line_start;
    while (line_end < source.len and source[line_end] != '\n') : (line_end += 1) {}
    return .{ .line = line, .column = column, .line_start = line_start, .line_end = line_end };
}

fn digits(value: usize) usize {
    var v = value;
    var count: usize = 1;
    while (v >= 10) : (v /= 10) {
        count += 1;
    }
    return count;
}

fn severity_label(danger: ink.severity) []const u8 {
    return switch (danger) {
        .note => "note",
        .warn => "warn",
        .@"error" => "error",
    };
}

fn visualColumn(line: []const u8, offset: usize, tab_width: usize) usize {
    var col: usize = 1;
    var i: usize = 0;
    while (i < offset and i < line.len) : (i += 1) {
        if (line[i] == '\t') {
            const pad = tab_width - ((col - 1) % tab_width);
            col += pad;
        } else {
            col += 1;
        }
    }
    return col;
}

fn expandTabs(allocator: mem_allocator, line: []const u8, tab_width: usize) ?[]u8 {
    var out = std.array_list.Managed(u8).init(allocator);
    var col: usize = 1;
    for (line) |ch| {
        if (ch == '\t') {
            const pad = tab_width - ((col - 1) % tab_width);
            out.appendNTimes(' ', pad) catch {
                out.deinit();
                return null;
            };
            col += pad;
        } else {
            out.append(ch) catch {
                out.deinit();
                return null;
            };
            col += 1;
        }
    }
    return out.toOwnedSlice() catch {
        out.deinit();
        return null;
    };
}

fn find_source_by_id(sources: []const ink.compiler.source, id: ink.compiler.source_id) ?*const ink.compiler.source {
    for (sources) |*src| {
        if (src.id == id) return src;
    }
    return null;
}

fn find_source_for_diag(
    sources: []const ink.compiler.source,
    fallback_id: ?ink.compiler.source_id,
    diag: ink.diagnostic,
) ?*const ink.compiler.source {
    if (diag.source_id) |sid| {
        if (find_source_by_id(sources, sid)) |src| return src;
    }
    if (fallback_id) |sid| {
        if (find_source_by_id(sources, sid)) |src| return src;
    }
    if (sources.len > 0) return &sources[0];
    return null;
}

fn build_marker(
    allocator: mem_allocator,
    start_col: usize,
    end_col: usize,
    line_len: usize,
    same_line: bool,
) ?[]u8 {
    const start_col_safe = if (start_col == 0) 1 else start_col;
    var mark_len: usize = 1;
    if (same_line) {
        if (end_col > start_col_safe) {
            mark_len = end_col - start_col_safe;
        }
    } else {
        if (line_len > start_col_safe - 1) {
            mark_len = line_len - (start_col_safe - 1);
        }
    }
    if (mark_len == 0) mark_len = 1;
    const space_len = start_col_safe - 1;
    var buf = allocator.alloc(u8, space_len + mark_len) catch return null;
    @memset(buf[0..space_len], ' ');
    buf[space_len] = '^';
    if (mark_len > 1) {
        @memset(buf[space_len + 1 ..], '-');
    }
    return buf;
}

fn print_diagnostics(
    writer: *std.Io.Writer,
    allocator: mem_allocator,
    sources: []const ink.compiler.source,
    fallback_id: ?ink.compiler.source_id,
    diags: []const ink.diagnostic,
) void {
    const tab_width: usize = 4;
    for (diags) |diag| {
        const label = severity_label(diag.danger);
        if (diag.code) |code| {
            writer.print("{s}[{s}]: {s}\n", .{ label, code, diag.message }) catch {};
        } else {
            writer.print("{s}: {s}\n", .{ label, diag.message }) catch {};
        }

        const src = find_source_for_diag(sources, fallback_id, diag) orelse continue;
        const span = diag.span orelse continue;

        const start_pos = @min(span.start, src.text.len);
        const end_pos = @min(span.end, src.text.len);
        const norm_start = @min(start_pos, end_pos);
        const norm_end = @max(start_pos, end_pos);
        const start_info = lineInfo(src.text, norm_start);
        const end_info = lineInfo(src.text, norm_end);
        const line_slice = src.text[start_info.line_start..start_info.line_end];
        const start_offset = if (norm_start >= start_info.line_start) norm_start - start_info.line_start else 0;
        const end_offset = if (norm_end >= start_info.line_start) norm_end - start_info.line_start else 0;
        const start_col = visualColumn(line_slice, start_offset, tab_width);
        const end_col = if (start_info.line == end_info.line)
            visualColumn(line_slice, end_offset, tab_width)
        else
            start_col;
        var display_line = line_slice;
        var display_owned = false;
        if (expandTabs(allocator, line_slice, tab_width)) |expanded| {
            display_line = expanded;
            display_owned = true;
        }
        defer if (display_owned) allocator.free(display_line);
        const line_digits = digits(start_info.line);
        const pad = allocator.alloc(u8, line_digits) catch {
            writer.print("\n", .{}) catch {};
            continue;
        };
        defer allocator.free(pad);
        @memset(pad, ' ');

        writer.print("  --> {s}:{d}:{d}\n", .{ src.path, start_info.line, start_col }) catch {};
        writer.print("  {s} |\n", .{pad}) catch {};
        writer.print("  {d} | {s}\n", .{ start_info.line, display_line }) catch {};

        if (build_marker(allocator, start_col, end_col, display_line.len, start_info.line == end_info.line)) |marker| {
            defer allocator.free(marker);
            writer.print("  {s} | {s}\n", .{ pad, marker }) catch {};
        }
        writer.print("\n", .{}) catch {};
    }
}

fn default_output_path(allocator: mem_allocator, input_path: []const u8) ![]const u8 {
    const ext = std.fs.path.extension(input_path);
    if (ext.len == 0) {
        return std.fmt.allocPrint(allocator, "{s}.inkb", .{input_path});
    }
    const stem = input_path[0 .. input_path.len - ext.len];
    return std.fmt.allocPrint(allocator, "{s}.inkb", .{stem});
}

fn resolve_manifest_root(allocator: mem_allocator, path: []const u8) ![]const u8 {
    const abs = try std.fs.cwd().realpathAlloc(allocator, path);
    errdefer allocator.free(abs);
    const stat = try std.fs.cwd().statFile(abs);
    if (stat.kind == .directory) {
        return abs;
    }
    const dir = std.fs.path.dirname(abs) orelse return error.InvalidArgs;
    const duped = try allocator.dupe(u8, dir);
    allocator.free(abs);
    return duped;
}

fn resolve_simulator_manifest(allocator: mem_allocator, root_dir: []const u8) ![]const u8 {
    const sim_path = try std.fs.path.join(allocator, &.{ root_dir, "simulator.ink" });
    if (std.fs.cwd().access(sim_path, .{})) {
        return sim_path;
    } else |err| switch (err) {
        error.FileNotFound => {
            allocator.free(sim_path);
            return try std.fs.path.join(allocator, &.{ root_dir, "package.ink" });
        },
        else => return err,
    }
}

fn resolve_output_path(
    allocator: mem_allocator,
    root_dir: []const u8,
    name: []const u8,
    provided: ?[]const u8,
) ![]const u8 {
    if (provided) |path| return try allocator.dupe(u8, path);
    const out_dir = try std.fs.path.join(allocator, &.{ root_dir, ".quill", "lib" });
    defer allocator.free(out_dir);
    return std.fmt.allocPrint(allocator, "{s}/{s}.inkb", .{ out_dir, name });
}


fn load_module_sources(
    allocator: mem_allocator,
    dir_path: []const u8,
    next_id: *ink.compiler.source_id,
    sources: *std.array_list.Managed(ink.compiler.source),
    allocated_paths: *std.array_list.Managed([]const u8),
) ![]ink.compiler.source_id {
    var ids = std.array_list.Managed(ink.compiler.source_id).init(allocator);
    errdefer ids.deinit();

    var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch |err| {
        if (err == error.FileNotFound) {
            return ids.toOwnedSlice();
        }
        return err;
    };
    defer dir.close();

    var it = dir.iterate();
    while (try it.next()) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".ink")) continue;
        const path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ dir_path, entry.name });
        try allocated_paths.append(path);
        const text = try std.fs.cwd().readFileAlloc(allocator, path, 1_000_000);
        const id = next_id.*;
        next_id.* += 1;
        try sources.append(.{ .id = id, .path = path, .text = text });
        try ids.append(id);
    }

    return ids.toOwnedSlice();
}

fn find_std_dir(allocator: mem_allocator, input_path: []const u8) !?[]const u8 {
    const abs_path = try std.fs.cwd().realpathAlloc(allocator, input_path);
    defer allocator.free(abs_path);

    var dir = std.fs.path.dirname(abs_path) orelse return null;
    while (true) {
        const candidate = try std.fmt.allocPrint(allocator, "{s}/std/src", .{dir});
        if (std.fs.cwd().openDir(candidate, .{})) |found| {
            var found_dir = found;
            found_dir.close();
            return candidate;
        } else |err| {
            if (err != error.FileNotFound and err != error.NotDir) {
                allocator.free(candidate);
                return err;
            }
        }
        allocator.free(candidate);
        const parent = std.fs.path.dirname(dir) orelse break;
        if (std.mem.eql(u8, parent, dir)) break;
        dir = parent;
    }

    return null;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator: mem_allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var err_memory = [_]u8{0} ** 8192;
    var err_file_writer = std.fs.File.stderr().writer(err_memory[0..]);
    var err_writer = &err_file_writer.interface;
    const options = Cli.parse(args, err_writer) catch |err| {
        if (err == error.InvalidArgs) {
            err_writer.flush() catch {};
            std.process.exit(1);
        }
        return err;
    };

    if (options.profile != null and options.manifest_path == null) {
        err_writer.writeAll("error: --profile requires --manifest\n") catch {};
        err_writer.flush() catch {};
        std.process.exit(1);
    }

    var target_spec: target_mod.target_spec = .{ .kind = .vm };
    if (options.target) |target_text| {
        target_spec = target_mod.parse_target(target_text) catch {
            err_writer.print("error: invalid target: {s}\n", .{target_text}) catch {};
            err_writer.flush() catch {};
            std.process.exit(1);
        };
    }

    if (options.manifest_path) |manifest_path| {
        const root_dir = try resolve_manifest_root(allocator, manifest_path);
        defer allocator.free(root_dir);

        var dep_graph = graph.build(allocator, root_dir) catch |err| {
            switch (err) {
                error.FileNotFound => err_writer.print("error: package.ink not found under {s}\n", .{root_dir}) catch {},
                error.MissingName => err_writer.writeAll("error: package.ink missing package name\n") catch {},
                error.MissingVersion => err_writer.writeAll("error: package.ink missing package version\n") catch {},
                error.MissingModuleSources => err_writer.writeAll("error: module missing sources\n") catch {},
                error.MissingDepPath => err_writer.writeAll("error: dependency missing path\n") catch {},
                error.MissingRegistryName => err_writer.writeAll("error: registry missing name\n") catch {},
                error.MissingRegistryUrl => err_writer.writeAll("error: registry missing url\n") catch {},
                error.InvalidManifest => err_writer.writeAll("error: invalid package.ink format\n") catch {},
                error.MissingSources => err_writer.writeAll("error: module sources not found\n") catch {},
                error.InvalidSources => err_writer.writeAll("error: invalid module sources path\n") catch {},
                error.DuplicateModuleName => err_writer.writeAll("error: duplicate module name\n") catch {},
                error.RegistryUnavailable => err_writer.writeAll("error: registry dependencies are not supported yet\n") catch {},
                else => return err,
            }
            err_writer.flush() catch {};
            return err;
        };
        defer dep_graph.deinit();

        const output_path = try resolve_output_path(allocator, root_dir, dep_graph.root_module, options.output_path);
        defer allocator.free(output_path);

        try std.fs.cwd().makePath(std.fs.path.dirname(output_path) orelse ".");

        var sandbox_cfg: ?ink.sandbox.Config = null;
        var sim_cfg_hold: ?ink.sim.Simulator = null;
        var debug_info_enabled = false;
        defer if (sim_cfg_hold) |*cfg| cfg.deinit();
        if (options.profile) |profile_name| {
            const profile = find_profile(dep_graph.profiles.items, profile_name) orelse {
                err_writer.print("error: unknown profile: {s}\n", .{profile_name}) catch {};
                err_writer.flush() catch {};
                std.process.exit(1);
            };
            if (options.target == null and profile.target.len != 0) {
                target_spec = target_mod.parse_target(profile.target) catch {
                    err_writer.print("error: invalid target: {s}\n", .{profile.target}) catch {};
                    err_writer.flush() catch {};
                    std.process.exit(1);
                };
            }
            if (profile.sandbox) |enabled| {
                if (enabled) sandbox_cfg = .{ .level = .strict };
            } else if (std.mem.eql(u8, profile.name, "debug")) {
                sandbox_cfg = .{ .level = .strict };
            }
            if (profile.debug_info.len != 0) {
                const enabled = parse_debug_info(profile.debug_info) orelse {
                    err_writer.writeAll("error: invalid profile debug_info\n") catch {};
                    err_writer.flush() catch {};
                    std.process.exit(1);
                };
                debug_info_enabled = enabled;
            } else if (std.mem.eql(u8, profile.name, "debug") or std.mem.eql(u8, profile.name, "sim")) {
                debug_info_enabled = true;
            }
            if (std.mem.eql(u8, profile.name, "sim")) {
                const sim_manifest_path = try resolve_simulator_manifest(allocator, root_dir);
                defer allocator.free(sim_manifest_path);
                const sim_manifest_name = std.fs.path.basename(sim_manifest_path);
                sim_cfg_hold = ink.sim.parse(allocator, sim_manifest_path) catch |err| {
                    switch (err) {
                        error.FileNotFound => err_writer.writeAll("error: package.ink not found\n") catch {},
                        error.MissingSimulator => err_writer.print("error: {s} missing top-level simulator config\n", .{sim_manifest_name}) catch {},
                        error.MissingSimImport => err_writer.print("error: {s} missing import sim for simulator config\n", .{sim_manifest_name}) catch {},
                        error.MissingScenarioName => err_writer.print("error: {s} missing simulator scenario name\n", .{sim_manifest_name}) catch {},
                        error.InvalidSimulator => err_writer.print("error: invalid simulator config format in {s}\n", .{sim_manifest_name}) catch {},
                        else => return err,
                    }
                    err_writer.flush() catch {};
                    std.process.exit(1);
                };
                const sim_cfg = &sim_cfg_hold.?;
                const checks = parse_validation_checks(sim_cfg.validation) catch {
                    err_writer.writeAll("error: invalid simulator validation override\n") catch {};
                    err_writer.flush() catch {};
                    std.process.exit(1);
                };
                const foreigns = sim_cfg.foreigns;
                var allow_categories: ?[]const []const u8 = null;
                var deny_categories: ?[]const []const u8 = null;
                var allow_foreigns: ?[]const []const u8 = null;
                var deny_foreigns: ?[]const []const u8 = null;
                var allow_defined = false;
                if (foreigns) |f| {
                    if (f.allow_categories.len != 0) {
                        allow_categories = f.allow_categories;
                        allow_defined = true;
                    }
                    if (f.allow.len != 0) {
                        allow_foreigns = f.allow;
                        allow_defined = true;
                    }
                    if (f.deny_categories.len != 0) {
                        deny_categories = f.deny_categories;
                    }
                    if (f.deny.len != 0) {
                        deny_foreigns = f.deny;
                    }
                    if (!allow_defined) {
                        allow_foreigns = &.{};
                    }
                } else {
                    allow_foreigns = &.{};
                }
                sandbox_cfg = .{
                    .level = parse_sandbox_level(sim_cfg.validation),
                    .checks = checks,
                    .mode = .sim,
                    .allow_categories = allow_categories,
                    .deny_categories = deny_categories,
                    .allow_foreigns = allow_foreigns,
                    .deny_foreigns = deny_foreigns,
                };
            }
        }

        var request = ink.compiler.compile_request{
            .sources = dep_graph.sources.items,
            .modules = dep_graph.modules.items,
            .root_module = dep_graph.root_module,
            .target = target_spec,
            .debug_info = debug_info_enabled,
        };
        if (dep_graph.prelude) |prelude| {
            request.prelude = prelude;
        }
        if (sandbox_cfg) |cfg| {
            request.sandbox = cfg;
        }

        var result = try inkc.compile_to_inkb(allocator, request, output_path);
        defer result.deinit(allocator);

        if (result.diagnostics.len != 0) {
            print_diagnostics(err_writer, allocator, dep_graph.sources.items, null, result.diagnostics);
            err_writer.flush() catch {};
        }

        if (!result.ok) {
            std.process.exit(1);
        }
        return;
    }

    const input_path = options.input_path.?;
    const output_path = if (options.output_path) |path| path else try default_output_path(allocator, input_path);
    defer if (options.output_path == null) allocator.free(output_path);

    var sources = std.array_list.Managed(ink.compiler.source).init(allocator);
    defer {
        for (sources.items) |src| {
            allocator.free(src.text);
        }
        sources.deinit();
    }

    var allocated_paths = std.array_list.Managed([]const u8).init(allocator);
    defer {
        for (allocated_paths.items) |path| {
            allocator.free(path);
        }
        allocated_paths.deinit();
    }

    var module_source_slices = std.array_list.Managed([]ink.compiler.source_id).init(allocator);
    defer {
        for (module_source_slices.items) |slice| {
            allocator.free(slice);
        }
        module_source_slices.deinit();
    }

    var next_source_id: ink.compiler.source_id = 0;
    const source_text = try std.fs.cwd().readFileAlloc(allocator, input_path, 1_000_000);
    const main_id = next_source_id;
    next_source_id += 1;
    try sources.append(.{ .id = main_id, .path = input_path, .text = source_text });

    const main_sources = try allocator.alloc(ink.compiler.source_id, 1);
    main_sources[0] = main_id;
    try module_source_slices.append(main_sources);

    var lib_sources: []ink.compiler.source_id = &.{};
    if (std.mem.eql(u8, std.fs.path.basename(input_path), "main.ink")) {
        const input_dir = std.fs.path.dirname(input_path) orelse ".";
        const lib_path = try std.fs.path.join(allocator, &.{ input_dir, "lib.ink" });
        if (std.fs.cwd().access(lib_path, .{})) |_| {
            try allocated_paths.append(lib_path);
            const lib_text = try std.fs.cwd().readFileAlloc(allocator, lib_path, 1_000_000);
            const lib_id = next_source_id;
            next_source_id += 1;
            try sources.append(.{ .id = lib_id, .path = lib_path, .text = lib_text });
            lib_sources = try allocator.alloc(ink.compiler.source_id, 1);
            lib_sources[0] = lib_id;
            try module_source_slices.append(lib_sources);
        } else |err| switch (err) {
            error.FileNotFound => allocator.free(lib_path),
            else => return err,
        }
    }

    var std_sources: []ink.compiler.source_id = &.{};
    if (try find_std_dir(allocator, input_path)) |std_dir| {
        defer allocator.free(std_dir);
        std_sources = try load_module_sources(allocator, std_dir, &next_source_id, &sources, &allocated_paths);
        if (std_sources.len != 0) {
            try module_source_slices.append(std_sources);
        } else {
            allocator.free(std_sources);
            std_sources = &.{};
        }
    }

    const empty_deps = &[_][]const u8{};

    var modules = std.array_list.Managed(ink.compiler.module_spec).init(allocator);
    defer modules.deinit();
    try modules.append(.{ .name = "main", .sources = main_sources, .deps = empty_deps });
    if (lib_sources.len != 0) {
        try modules.append(.{ .name = "lib", .sources = lib_sources, .deps = empty_deps });
    }
    if (std_sources.len != 0) {
        try modules.append(.{ .name = "std", .sources = std_sources, .deps = empty_deps });
    }

    const request = ink.compiler.compile_request{
        .sources = sources.items,
        .modules = modules.items,
        .root_module = "main",
        .target = target_spec,
    };

    var result = try inkc.compile_to_inkb(allocator, request, output_path);
    defer result.deinit(allocator);

    if (result.diagnostics.len != 0) {
        print_diagnostics(err_writer, allocator, sources.items, main_id, result.diagnostics);
        err_writer.flush() catch {};
    }

    if (!result.ok) {
        std.process.exit(1);
    }
}

fn find_profile(profiles: []const manifest.Profile, name: []const u8) ?manifest.Profile {
    for (profiles) |profile| {
        if (std.mem.eql(u8, profile.name, name)) return profile;
    }
    return null;
}

fn parse_sandbox_level(validation: ?ink.sim.Validation) ink.sandbox.Level {
    if (validation) |val| {
        if (std.mem.eql(u8, val.level, "paranoid")) return .paranoid;
        if (std.mem.eql(u8, val.level, "strict")) return .strict;
        if (std.mem.eql(u8, val.level, "basic")) return .basic;
    }
    return .strict;
}

fn parse_validation_checks(validation: ?ink.sim.Validation) !?ink.sandbox.Checks {
    const val = validation orelse return null;
    if (val.overrides.len == 0) return null;

    var checks = ink.sandbox.checks_for_level(parse_sandbox_level(validation));
    for (val.overrides) |entry| {
        const enabled = parse_bool_text(entry.value) orelse return error.InvalidValidationOverride;
        if (std.mem.eql(u8, entry.key, "macro_streams")) {
            checks.macro_streams = enabled;
        } else if (std.mem.eql(u8, entry.key, "macro_ast")) {
            checks.macro_ast = enabled;
        } else if (std.mem.eql(u8, entry.key, "ast")) {
            checks.ast = enabled;
        } else if (std.mem.eql(u8, entry.key, "uir")) {
            checks.uir = enabled;
        } else if (std.mem.eql(u8, entry.key, "mir")) {
            checks.mir = enabled;
        } else if (std.mem.eql(u8, entry.key, "lir")) {
            checks.lir = enabled;
        } else {
            return error.InvalidValidationOverride;
        }
    }

    return checks;
}

fn parse_debug_info(raw: []const u8) ?bool {
    if (std.mem.eql(u8, raw, "full")) return true;
    if (std.mem.eql(u8, raw, "none")) return false;
    return parse_bool_text(raw);
}

fn parse_bool_text(raw: []const u8) ?bool {
    if (std.mem.eql(u8, raw, "true") or std.mem.eql(u8, raw, "1")) return true;
    if (std.mem.eql(u8, raw, "false") or std.mem.eql(u8, raw, "0")) return false;
    return null;
}
