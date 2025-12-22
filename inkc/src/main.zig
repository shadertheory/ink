const std = @import("std");
const ink = @import("ink");

const Cli = struct {
    const Options = struct {
        input_path: []const u8,
        output_path: ?[]const u8 = null,
    };

    const ParseError = error{InvalidArgs};

    fn parse(args: []const []const u8, p: *std.Io.Writer) ParseError!Options {
        var input_path: ?[]const u8 = null;
        var output_path: ?[]const u8 = null;

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

        if (input_path == null) {
            print_usage(p, args[0]) catch {};
            return error.InvalidArgs;
        }

        return .{ .input_path = input_path.?, .output_path = output_path };
    }

    fn print_usage(p: *std.Io.Writer, exe_name: []const u8) !void {
        try p.print("Usage: {s} [-o <path>] <source_file>\n", .{exe_name});
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
                        try self.print_line(depth, ")", .{});
                    },
                    .concept => |concept_decl| {
                        try self.print_indent(depth);
                        try self.writer.print("(concept_decl\n", .{});
                        try self.print_identifier_field("name", concept_decl.name, depth + 1);
                        try self.print_generic_param_list("generics", concept_decl.generics, depth + 1);
                        try self.print_node_list("requires", concept_decl.requires, depth + 1);
                        try self.print_line(depth, ")", .{});
                    },
                    .sum => |sum_decl| {
                        try self.print_indent(depth);
                        try self.writer.print("(sum_decl\n", .{});
                        try self.print_identifier_field("name", sum_decl.name, depth + 1);
                        try self.print_generic_param_list("generics", sum_decl.generics, depth + 1);
                        try self.print_sum_variant_list("variants", sum_decl.variants, depth + 1);
                        try self.print_line(depth, ")", .{});
                    },
                    .@"enum" => |enum_decl| {
                        try self.print_indent(depth);
                        try self.writer.print("(enum_decl\n", .{});
                        try self.print_identifier_field("name", enum_decl.name, depth + 1);
                        try self.print_identifier_list("cases", "case", enum_decl.cases, depth + 1);
                        try self.print_line(depth, ")", .{});
                    },
                    .@"impl" => |impl_decl| {
                        try self.print_indent(depth);
                        try self.writer.print("(impl_decl\n", .{});
                        try self.print_identifier_field("by_trait", impl_decl.by_trait, depth + 1);
                        try self.print_identifier_field("for_struct", impl_decl.for_struct, depth + 1);
                        try self.print_function_decl_list("functions", impl_decl.functions, depth + 1);
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

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var err_memory = [_]u8{0} ** 8192;
    var err_file_writer = std.fs.File.stderr().writer(err_memory[0..]);
    var err_writer = &err_file_writer.interface;
    const options = Cli.parse(args, err_writer) catch |err| {
        if (err == error.InvalidArgs) {
            err_writer.flush() catch {};
            return;
        }
        return err;
    };

    const source = try std.fs.cwd().readFileAlloc(allocator, options.input_path, 1_000_000);
    defer allocator.free(source);

    var out_file_buffer: [1_000_000]u8 = undefined;
    var out_file: ?std.fs.File = null;
    defer if (out_file) |file| file.close();

    var out_file_writer = std.fs.File.stdout().writer(out_file_buffer[0..]);
    var out_writer = &out_file_writer.interface;
    if (options.output_path) |path| {
        const file = if (std.fs.path.isAbsolute(path))
            try std.fs.createFileAbsolute(path, .{ .truncate = true })
        else
            try std.fs.cwd().createFile(path, .{ .truncate = true });
        out_file = file;
        out_file_writer = file.writer(out_file_buffer[0..]);
        out_writer = &out_file_writer.interface;
    }

    var p = printer(@TypeOf(out_writer)){ .writer = out_writer };

    var lexer = try ink.lexer.init(source);
    var tokens: [1024]ink.token = undefined;
    var i: usize = 0;

    while (true) {
        if (i >= tokens.len) break;

        if (try lexer.next()) |tok| {
            tokens[i] = tok;
            i += 1;
            if (tok.which == .end_of_file) break;
        } else {
            break;
        }
    }

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var parser = try ink.parser.parse(source, tokens[0..i], arena.allocator());

    while (!parser.at_end()) {
        const node = try parser.process();
        try p.print_node_tree(node, 0);
        try p.writer.writeByte('\n');
    }

    try p.writer.flush();
}
