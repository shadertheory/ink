const std = @import("std");
const ink = @import("ink");
const core = @import("core.zig");
const source = @import("../source.zig");

pub const ir = core.ir;
pub const ir_identifier = core.ir_identifier;
pub const string_identifier = core.string_identifier;
pub const mem_allocator = std.mem.Allocator;

pub const build_error = error{
    build_failed,
    out_of_memory,
};

fn merge_span(left: ?source.span, right: ?source.span) ?source.span {
    if (left == null) return right;
    if (right == null) return left;
    const l = left.?;
    const r = right.?;
    return .{
        .start = if (l.start < r.start) l.start else r.start,
        .end = if (l.end > r.end) l.end else r.end,
    };
}

fn span_from_location(loc: ink.location) source.span {
    return .{ .start = loc.start, .end = loc.end };
}

fn span_for_node(node: *const ink.node) ?source.span {
    return switch (node.*) {
        .identifier => |id| span_from_location(id.where),
        .string => |str| span_from_location(str.where),
        .integer => |value| span_from_location(value.where),
        .float => |value| span_from_location(value.where),
        .duration => |value| span_from_location(value.where),
        .unary => |un| span_for_node(ink.ast.deref(un.right)),
        .binary => |bin| merge_span(span_for_node(ink.ast.deref(bin.left)), span_for_node(ink.ast.deref(bin.right))),
        .block => |blk| if (blk.items.len == 0) null else merge_span(
            span_for_node(ink.ast.deref(blk.items[0])),
            span_for_node(ink.ast.deref(blk.items[blk.items.len - 1])),
        ),
        .record => |rec| if (rec.items.len == 0) null else merge_span(
            span_from_location(rec.items[0].name.where),
            span_from_location(rec.items[rec.items.len - 1].name.where),
        ),
        .associate => |assoc| span_from_location(assoc.name.where),
        .intrinsic => |call| span_from_location(call.name.where),
        .if_expr => |ife| merge_span(
            span_for_node(ink.ast.deref(ife.condition)),
            span_for_node(if (ife.else_branch) |ref| ink.ast.deref(ref) else ink.ast.deref(ife.then_branch)),
        ),
        .match_expr => |me| merge_span(
            span_for_node(ink.ast.deref(me.target)),
            if (me.arms.len == 0) null else span_for_node(ink.ast.deref(me.arms[me.arms.len - 1].body)),
        ),
        .select_expr => |se| if (se.arms.len == 0) null else merge_span(
            span_for_node(ink.ast.deref(se.arms[0].task)),
            span_for_node(ink.ast.deref(se.arms[se.arms.len - 1].body)),
        ),
        .with_expr => |we| merge_span(span_from_location(we.name.where), span_for_node(ink.ast.deref(we.body))),
        .label_expr => |le| merge_span(span_from_location(le.name.where), span_for_node(ink.ast.deref(le.body))),
        .loop_expr => |le| span_for_node(ink.ast.deref(le.body)),
        .while_expr => |we| merge_span(span_for_node(ink.ast.deref(we.condition)), span_for_node(ink.ast.deref(we.body))),
        .while_in_expr => |we| blk: {
            const head = merge_span(span_for_node(ink.ast.deref(we.pattern)), span_for_node(ink.ast.deref(we.iter)));
            break :blk merge_span(head, span_for_node(ink.ast.deref(we.body)));
        },
        .until_expr => |ue| merge_span(span_for_node(ink.ast.deref(ue.condition)), span_for_node(ink.ast.deref(ue.body))),
        .repeat_expr => |re| merge_span(span_for_node(ink.ast.deref(re.count)), span_for_node(ink.ast.deref(re.body))),
        .for_expr => |fe| blk: {
            const head = merge_span(span_for_node(ink.ast.deref(fe.pattern)), span_for_node(ink.ast.deref(fe.iter)));
            break :blk merge_span(head, span_for_node(ink.ast.deref(fe.body)));
        },
        .each_expr => |ee| blk: {
            const head = merge_span(span_for_node(ink.ast.deref(ee.pattern)), span_for_node(ink.ast.deref(ee.iter)));
            break :blk merge_span(head, span_for_node(ink.ast.deref(ee.body)));
        },
        .break_expr => |be| blk: {
            if (be.value) |ref| break :blk span_for_node(ink.ast.deref(ref));
            if (be.label) |lab| break :blk span_from_location(lab.where);
            break :blk null;
        },
        .continue_expr => |ce| if (ce.label) |lab| span_from_location(lab.where) else null,
        .yield_expr => |ye| if (ye.value) |ref| span_for_node(ink.ast.deref(ref)) else null,
        .atomic_expr => |ae| merge_span(span_for_node(ink.ast.deref(ae.value)), span_from_location(ae.ordering.where)),
        .type => |ty| switch (ty) {
            .name => |id| span_from_location(id.where),
            .applied => |ap| span_from_location(ap.base.where),
            .optional => |ref| span_for_node(ink.ast.deref(ref)),
            .dyn => |ref| span_for_node(ink.ast.deref(ref)),
            .self => null,
        },
        .decl => |decl| switch (decl) {
            .function => |func| span_from_location(func.name.where),
            .@"struct" => |st| span_from_location(st.name.where),
            .trait => |tr| span_from_location(tr.name.where),
            .@"enum" => |en| span_from_location(en.name.where),
            .impl => |im| span_from_location(im.for_struct.where),
            .type_alias => |tyd| span_from_location(tyd.name.where),
            .@"const" => |c| span_from_location(c.name.where),
            .@"var" => |v| span_from_location(v.name.where),
            .import => |imp| span_from_location(imp.module.where),
        },
    };
}

fn span_for_function_decl(func: ink.ast.function_decl) ?source.span {
    const base: ?source.span = span_from_location(func.name.where);
    if (func.body) |ref| return merge_span(base, span_for_node(ink.ast.deref(ref)));
    if (func.return_type) |ref| return merge_span(base, span_for_node(ink.ast.deref(ref)));
    return base;
}

pub const error_kind = enum {
    unsupported_node,
};

pub const build_result = struct {
    nodes: []const ir,
    strings: []const []const u8,
    roots: []const ir_identifier,
    spans: []const ?source.span,
    sources: []const source.source_id,
};

pub const builder = struct {
    allocator: mem_allocator,
    nodes: std.array_list.Managed(ir),
    spans: std.array_list.Managed(?source.span),
    span_stack: std.array_list.Managed(?source.span),
    sources: std.array_list.Managed(source.source_id),
    strings: std.array_list.Managed([]const u8),
    roots: std.array_list.Managed(ir_identifier),
    string_map: std.StringHashMapUnmanaged(string_identifier) = .{},
    last_error: ?error_kind = null,
    current_source_id: source.source_id = 0,

    pub fn init(allocator: mem_allocator) builder {
        return .{
            .allocator = allocator,
            .nodes = std.array_list.Managed(ir).init(allocator),
            .spans = std.array_list.Managed(?source.span).init(allocator),
            .span_stack = std.array_list.Managed(?source.span).init(allocator),
            .sources = std.array_list.Managed(source.source_id).init(allocator),
            .strings = std.array_list.Managed([]const u8).init(allocator),
            .roots = std.array_list.Managed(ir_identifier).init(allocator),
            .string_map = .{},
            .last_error = null,
            .current_source_id = 0,
        };
    }

    pub fn deinit(self: *builder) void {
        self.string_map.deinit(self.allocator);
        self.nodes.deinit();
        self.spans.deinit();
        self.span_stack.deinit();
        self.sources.deinit();
        self.strings.deinit();
        self.roots.deinit();
    }

    pub fn build(self: *builder, ast_nodes: []const *ink.node) build_error!build_result {
        try self.build_file(ast_nodes, 0);
        return self.finish();
    }

    pub fn build_file(self: *builder, ast_nodes: []const *ink.node, source_id: source.source_id) build_error!void {
        self.current_source_id = source_id;
        for (ast_nodes) |node| {
            const id = try self.build_node(node);
            self.roots.append(id) catch return error.out_of_memory;
        }
    }

    pub fn finish(self: *builder) build_error!build_result {
        return .{
            .nodes = self.nodes.toOwnedSlice() catch return error.out_of_memory,
            .strings = self.strings.toOwnedSlice() catch return error.out_of_memory,
            .roots = self.roots.toOwnedSlice() catch return error.out_of_memory,
            .spans = self.spans.toOwnedSlice() catch return error.out_of_memory,
            .sources = self.sources.toOwnedSlice() catch return error.out_of_memory,
        };
    }

    fn build_node(self: *builder, node: *const ink.node) build_error!ir_identifier {
        self.span_stack.append(span_for_node(node)) catch return error.out_of_memory;
        defer _ = self.span_stack.pop();
        return switch (node.*) {
            .integer => |value| self.emit(.{ .integer = value.value }),
            .float => |value| self.emit(.{ .float = @as(f64, value.value) }),
            .duration => |value| self.emit(.{ .duration = value.value }),
            .string => |value| self.emit(.{ .string = try self.intern_string(value.string) }),
            .identifier => |id| self.build_identifier(id),
            .unary => |un| self.build_unary(un),
            .binary => |bin| self.build_binary(bin),
            .block => |blk| self.build_block(blk),
            .intrinsic => |call| self.build_intrinsic(call),
            .if_expr => |ife| self.build_if_expr(ife),
            .match_expr => |me| self.build_match_expr(me),
            .select_expr => |se| self.build_select_expr(se),
            .label_expr => |le| self.build_label_expr(le),
            .loop_expr => |le| self.build_loop_expr(le),
            .while_expr => |we| self.build_while_expr(we),
            .while_in_expr => |we| self.build_while_in_expr(we),
            .until_expr => |ue| self.build_until_expr(ue),
            .repeat_expr => |re| self.build_repeat_expr(re),
            .for_expr => |fe| self.build_for_expr(fe),
            .each_expr => |ee| self.build_each_expr(ee),
            .break_expr => |be| self.build_break_expr(be),
            .continue_expr => |ce| self.build_continue_expr(ce),
            .yield_expr => |ye| self.build_yield_expr(ye),
            .atomic_expr => |ae| self.build_atomic_expr(ae),
            .with_expr => |_| self.fail(.unsupported_node),
            .record => |_| self.fail(.unsupported_node),
            .associate => |assoc| self.build_associate(assoc),
            .type => |ty| self.build_type_expr(ty),
            .decl => |decl| self.build_decl(decl),
        };
    }

    fn build_identifier(self: *builder, id: ink.identifier) build_error!ir_identifier {
        if (std.mem.eql(u8, id.string, "true")) return self.emit(.{ .boolean = true });
        if (std.mem.eql(u8, id.string, "false")) return self.emit(.{ .boolean = false });
        if (std.mem.eql(u8, id.string, "none")) return self.emit(.{ .integer = 0 });
        if (std.mem.eql(u8, id.string, "error::cancelled")) return self.emit(.{ .integer = 1 });
        if (std.mem.eql(u8, id.string, "error::timeout")) return self.emit(.{ .integer = 2 });
        const sid = try self.intern_string(id.string);
        return self.emit(.{ .identifier = sid });
    }

    fn build_unary(self: *builder, un: ink.ast.unary_expr) build_error!ir_identifier {
        const right = try self.build_node(ink.ast.deref(un.right));
        return self.emit(.{ .unary = .{ .op = un.op, .right = right } });
    }

    fn build_binary(self: *builder, bin: ink.ast.binary_expr) build_error!ir_identifier {
        if (bin.op == .call) {
            if (try self.try_build_record_literal(bin)) |literal| return literal;
            if (self.is_record_call(ink.ast.deref(bin.left))) return self.fail(.unsupported_node);
        }
        const left = try self.build_node(ink.ast.deref(bin.left));
        const right = try self.build_node(ink.ast.deref(bin.right));
        return self.emit(.{ .binary = .{ .left = left, .op = bin.op, .right = right } });
    }

    const record_field_source = struct {
        name: []const u8,
        value: *const ink.node,
    };

    fn try_build_record_literal(self: *builder, bin: ink.ast.binary_expr) build_error!?ir_identifier {
        const ctor_name = self.constructor_name(ink.ast.deref(bin.left)) orelse return null;
        if (is_record_name(ctor_name)) return null;
        const fields = try self.collect_record_fields(ink.ast.deref(bin.right)) orelse return null;

        const type_name = try self.intern_string(ctor_name);
        const out_fields = try self.alloc(core.record_literal.record_field, fields.len);
        for (fields, 0..) |field, idx| {
            out_fields[idx] = .{
                .name = try self.intern_string(field.name),
                .value = try self.build_node(field.value),
            };
        }

        const id = try self.emit(.{ .record_literal = .{ .type_name = type_name, .fields = out_fields } });
        return @as(?ir_identifier, id);
    }

    fn constructor_name(self: *builder, node: *const ink.node) ?[]const u8 {
        _ = self;
        return switch (node.*) {
            .identifier => |id| id.string,
            else => null,
        };
    }

    fn collect_record_fields(self: *builder, node: *const ink.node) build_error!?[]const record_field_source {
        return switch (node.*) {
            .record => |rec| try self.collect_record_fields_from_record(rec),
            else => try self.collect_record_fields_from_call(node),
        };
    }

    fn collect_record_fields_from_record(self: *builder, rec: ink.ast.record_expr) build_error!?[]const record_field_source {
        var fields = std.array_list.Managed(record_field_source).init(self.allocator);
        for (rec.items) |assoc| {
            const value = if (assoc.value) |ref| ink.ast.deref(ref) else return self.fail(.unsupported_node);
            fields.append(.{ .name = assoc.name.string, .value = value }) catch return error.out_of_memory;
        }
        return fields.toOwnedSlice() catch return error.out_of_memory;
    }

    fn collect_record_fields_from_call(self: *builder, node: *const ink.node) build_error!?[]const record_field_source {
        var args = std.array_list.Managed(*const ink.node).init(self.allocator);
        defer args.deinit();

        const base = try self.collect_call_chain(node, &args) orelse return null;
        if (base.* != .identifier) return null;
        const base_name = base.identifier.string;
        if (!is_record_name(base_name)) return null;

        if (args.items.len == 1 and is_unit_identifier(args.items[0])) {
            const empty = try self.alloc(record_field_source, 0);
            return @as(?[]const record_field_source, empty);
        }

        var fields = std.array_list.Managed(record_field_source).init(self.allocator);
        for (args.items) |arg| {
            const field = try self.parse_record_field_call(arg) orelse return self.fail(.unsupported_node);
            fields.append(field) catch return error.out_of_memory;
        }
        return fields.toOwnedSlice() catch return error.out_of_memory;
    }

    fn parse_record_field_call(self: *builder, node: *const ink.node) build_error!?record_field_source {
        var args = std.array_list.Managed(*const ink.node).init(self.allocator);
        defer args.deinit();

        const base = try self.collect_call_chain(node, &args) orelse return null;
        if (base.* != .identifier) return null;
        const base_name = base.identifier.string;
        if (!is_field_name(base_name)) return null;
        if (args.items.len != 2) return self.fail(.unsupported_node);
        const name_node = args.items[0];
        if (name_node.* != .string) return self.fail(.unsupported_node);
        return .{ .name = name_node.string.string, .value = args.items[1] };
    }

    fn collect_call_chain(
        self: *builder,
        node: *const ink.node,
        args: *std.array_list.Managed(*const ink.node),
    ) build_error!?*const ink.node {
        _ = self;
        var current = node;
        while (current.* == .binary and current.binary.op == .call) {
            args.append(ink.ast.deref(current.binary.right)) catch return error.out_of_memory;
            current = ink.ast.deref(current.binary.left);
        }
        std.mem.reverse(*const ink.node, args.items);
        return current;
    }

    fn is_record_call(self: *builder, node: *const ink.node) bool {
        _ = self;
        var current = node;
        while (current.* == .binary and current.binary.op == .call) {
            current = ink.ast.deref(current.binary.left);
        }
        if (current.* != .identifier) return false;
        return is_record_name(current.identifier.string);
    }

    fn is_record_name(name: []const u8) bool {
        return std.mem.eql(u8, name, "record") or std.mem.endsWith(u8, name, "::record");
    }

    fn is_field_name(name: []const u8) bool {
        return std.mem.eql(u8, name, "field") or std.mem.endsWith(u8, name, "::field");
    }

    fn is_unit_identifier(node: *const ink.node) bool {
        return node.* == .identifier and std.mem.eql(u8, node.identifier.string, "unit");
    }

    fn build_block(self: *builder, blk: ink.ast.block_expr) build_error!ir_identifier {
        const items = try self.build_node_refs(blk.items);
        return self.emit(.{ .block = items });
    }

    fn build_intrinsic(self: *builder, call: ink.ast.intrinsic_call) build_error!ir_identifier {
        const name = try self.intern_string(call.name.string);
        const args = try self.build_node_refs(call.args);
        return self.emit(.{ .intrinsic = .{ .name = name, .args = args } });
    }

    fn build_if_expr(self: *builder, ife: ink.ast.if_expr) build_error!ir_identifier {
        const condition = try self.build_node(ink.ast.deref(ife.condition));
        const then_branch = try self.build_node(ink.ast.deref(ife.then_branch));
        const else_branch = if (ife.else_branch) |ref| try self.build_node(ink.ast.deref(ref)) else null;
        return self.emit(.{ .if_expr = .{
            .condition = condition,
            .then_branch = then_branch,
            .else_branch = else_branch,
        } });
    }

    fn build_label_expr(self: *builder, le: ink.ast.label_expr) build_error!ir_identifier {
        return self.emit(.{ .label_expr = .{
            .name = try self.intern_string(le.name.string),
            .body = try self.build_node(ink.ast.deref(le.body)),
        } });
    }

    fn build_loop_expr(self: *builder, le: ink.ast.loop_expr) build_error!ir_identifier {
        return self.emit(.{ .loop_expr = .{
            .body = try self.build_node(ink.ast.deref(le.body)),
        } });
    }

    fn build_while_expr(self: *builder, we: ink.ast.while_expr) build_error!ir_identifier {
        return self.emit(.{ .while_expr = .{
            .condition = try self.build_node(ink.ast.deref(we.condition)),
            .body = try self.build_node(ink.ast.deref(we.body)),
        } });
    }

    fn build_while_in_expr(self: *builder, we: ink.ast.while_in_expr) build_error!ir_identifier {
        return self.emit(.{ .while_in_expr = .{
            .pattern = try self.build_node(ink.ast.deref(we.pattern)),
            .iter = try self.build_node(ink.ast.deref(we.iter)),
            .body = try self.build_node(ink.ast.deref(we.body)),
        } });
    }

    fn build_until_expr(self: *builder, ue: ink.ast.until_expr) build_error!ir_identifier {
        return self.emit(.{ .until_expr = .{
            .condition = try self.build_node(ink.ast.deref(ue.condition)),
            .body = try self.build_node(ink.ast.deref(ue.body)),
        } });
    }

    fn build_repeat_expr(self: *builder, re: ink.ast.repeat_expr) build_error!ir_identifier {
        return self.emit(.{ .repeat_expr = .{
            .count = try self.build_node(ink.ast.deref(re.count)),
            .body = try self.build_node(ink.ast.deref(re.body)),
        } });
    }

    fn build_for_expr(self: *builder, fe: ink.ast.for_expr) build_error!ir_identifier {
        return self.emit(.{ .for_expr = .{
            .pattern = try self.build_node(ink.ast.deref(fe.pattern)),
            .iter = try self.build_node(ink.ast.deref(fe.iter)),
            .body = try self.build_node(ink.ast.deref(fe.body)),
        } });
    }

    fn build_each_expr(self: *builder, ee: ink.ast.each_expr) build_error!ir_identifier {
        return self.emit(.{ .each_expr = .{
            .pattern = try self.build_node(ink.ast.deref(ee.pattern)),
            .iter = try self.build_node(ink.ast.deref(ee.iter)),
            .body = try self.build_node(ink.ast.deref(ee.body)),
        } });
    }

    fn build_break_expr(self: *builder, be: ink.ast.break_expr) build_error!ir_identifier {
        const label = if (be.label) |lab| try self.intern_string(lab.string) else null;
        const value = if (be.value) |ref| try self.build_node(ink.ast.deref(ref)) else null;
        return self.emit(.{ .break_expr = .{ .label = label, .value = value } });
    }

    fn build_continue_expr(self: *builder, ce: ink.ast.continue_expr) build_error!ir_identifier {
        const label = if (ce.label) |lab| try self.intern_string(lab.string) else null;
        return self.emit(.{ .continue_expr = .{ .label = label } });
    }

    fn build_yield_expr(self: *builder, ye: ink.ast.yield_expr) build_error!ir_identifier {
        const value = if (ye.value) |ref| try self.build_node(ink.ast.deref(ref)) else null;
        return self.emit(.{ .yield_expr = .{ .value = value } });
    }

    fn build_atomic_expr(self: *builder, ae: ink.ast.atomic_expr) build_error!ir_identifier {
        return self.emit(.{ .atomic_expr = .{
            .value = try self.build_node(ink.ast.deref(ae.value)),
            .ordering = try self.intern_string(ae.ordering.string),
        } });
    }

    fn build_match_expr(self: *builder, me: ink.ast.match_expr) build_error!ir_identifier {
        const target = try self.build_node(ink.ast.deref(me.target));
        const arms = try self.build_match_arms(me.arms);
        return self.emit(.{ .match_expr = .{ .target = target, .arms = arms } });
    }

    fn build_select_expr(self: *builder, se: ink.ast.select_expr) build_error!ir_identifier {
        const arms = try self.build_select_arms(se.arms);
        return self.emit(.{ .select_expr = .{ .arms = arms } });
    }

    fn build_match_arms(self: *builder, arms: []const ink.ast.match_arm) build_error![]const ir.match_arm {
        if (arms.len == 0) return &[_]ir.match_arm{};
        var out = try self.alloc(ir.match_arm, arms.len);
        for (arms, 0..) |arm, i| {
            out[i] = .{
                .pattern = try self.build_node(ink.ast.deref(arm.pattern)),
                .body = try self.build_node(ink.ast.deref(arm.body)),
            };
        }
        return out;
    }

    fn build_select_arms(self: *builder, arms: []const ink.ast.select_arm) build_error![]const ir.select_arm {
        if (arms.len == 0) return &[_]ir.select_arm{};
        var out = try self.alloc(ir.select_arm, arms.len);
        for (arms, 0..) |arm, i| {
            const name = if (arm.name) |ident| try self.intern_string(ident.string) else null;
            out[i] = .{
                .name = name,
                .task = try self.build_node(ink.ast.deref(arm.task)),
                .body = try self.build_node(ink.ast.deref(arm.body)),
                .detached = arm.detached,
            };
        }
        return out;
    }

    fn build_associate(self: *builder, assoc: ink.ast.associate) build_error!ir_identifier {
        const name = try self.intern_string(assoc.name.string);
        const value = if (assoc.value) |ref| try self.build_node(ink.ast.deref(ref)) else null;
        return self.emit(.{ .associate = .{ .name = name, .value = value } });
    }

    fn build_type_expr(self: *builder, ty: ink.ast.type_expr) build_error!ir_identifier {
        return switch (ty) {
            .self => self.emit(.{ .type = .self }),
            .name => |id| self.emit(.{ .type = .{ .name = try self.intern_string(id.string) } }),
            .optional => |ref| {
                const inner = try self.build_node(ink.ast.deref(ref));
                return self.emit(.{ .type = .{ .optional = inner } });
            },
            .dyn => |ref| {
                const inner = try self.build_node(ink.ast.deref(ref));
                return self.emit(.{ .type = .{ .dyn = inner } });
            },
            .applied => |ap| {
                const base = try self.intern_string(ap.base.string);
                const args = try self.build_node_refs(ap.args);
                return self.emit(.{ .type = .{ .applied = .{ .base = base, .args = args } } });
            },
        };
    }

    fn build_node_with_self(self: *builder, node: *const ink.node, self_name: []const u8) build_error!ir_identifier {
        return switch (node.*) {
            .type => |ty| self.build_type_expr_with_self(ty, self_name),
            else => self.build_node(node),
        };
    }

    fn build_node_refs_with_self(
        self: *builder,
        refs: []const ink.ast.node_ref,
        self_name: []const u8,
    ) build_error![]const ir_identifier {
        if (refs.len == 0) return &[_]ir_identifier{};
        var out = try self.alloc(ir_identifier, refs.len);
        for (refs, 0..) |ref, i| {
            out[i] = try self.build_node_with_self(ink.ast.deref(ref), self_name);
        }
        return out;
    }

    fn build_type_expr_with_self(self: *builder, ty: ink.ast.type_expr, self_name: []const u8) build_error!ir_identifier {
        return switch (ty) {
            .self => self.emit(.{ .type = .{ .name = try self.intern_string(self_name) } }),
            .name => |id| self.emit(.{ .type = .{ .name = try self.intern_string(id.string) } }),
            .optional => |ref| {
                const inner = try self.build_node_with_self(ink.ast.deref(ref), self_name);
                return self.emit(.{ .type = .{ .optional = inner } });
            },
            .dyn => |ref| {
                const inner = try self.build_node_with_self(ink.ast.deref(ref), self_name);
                return self.emit(.{ .type = .{ .dyn = inner } });
            },
            .applied => |ap| {
                const base = try self.intern_string(ap.base.string);
                const args = try self.build_node_refs_with_self(ap.args, self_name);
                return self.emit(.{ .type = .{ .applied = .{ .base = base, .args = args } } });
            },
        };
    }

    fn build_decl(self: *builder, decl: ink.ast.decl) build_error!ir_identifier {
        return self.emit(.{ .decl = switch (decl) {
            .function => |func| .{ .function = try self.build_function_decl(func) },
            .@"struct" => |s| .{ .@"struct" = try self.build_struct_decl(s) },
            .trait => |t| .{ .trait = try self.build_trait_decl(t) },
            .@"enum" => |e| .{ .@"enum" = try self.build_enum_decl(e) },
            .@"impl" => |i| .{ .@"impl" = try self.build_impl_decl(i) },
            .import => return self.fail(.unsupported_node),
            .type_alias => |t| .{ .type_alias = try self.build_type_decl(t) },
            .@"const" => |c| .{ .@"const" = try self.build_const_decl(c) },
            .@"var" => |v| .{ .@"var" = try self.build_var_decl(v) },
        } });
    }

    fn build_function_decl(self: *builder, func: ink.ast.function_decl) build_error!ir.function_decl {
        return .{
            .name = try self.intern_string(func.name.string),
            .generics = try self.build_generic_params(func.generics),
            .params = try self.build_params(func.params),
            .return_type = if (func.return_type) |ref| try self.build_node(ink.ast.deref(ref)) else null,
            .where_clause = try self.build_where_clause(func.where_clause),
            .body = if (func.body) |ref| try self.build_node(ink.ast.deref(ref)) else null,
            .span = span_for_function_decl(func),
            .source_id = self.current_source_id,
        };
    }

    fn build_function_decl_with_self(
        self: *builder,
        func: ink.ast.function_decl,
        self_name: []const u8,
    ) build_error!ir.function_decl {
        return .{
            .name = try self.intern_string(func.name.string),
            .generics = try self.build_generic_params(func.generics),
            .params = try self.build_params_with_self(func.params, self_name),
            .return_type = if (func.return_type) |ref| try self.build_node_with_self(ink.ast.deref(ref), self_name) else null,
            .where_clause = try self.build_where_clause_with_self(func.where_clause, self_name),
            .body = if (func.body) |ref| try self.build_node(ink.ast.deref(ref)) else null,
            .span = span_for_function_decl(func),
            .source_id = self.current_source_id,
        };
    }

    fn build_type_decl(self: *builder, decl: ink.ast.type_decl) build_error!ir.type_decl {
        return .{
            .name = try self.intern_string(decl.name.string),
            .generics = try self.build_generic_params(decl.generics),
            .value = try self.build_node(ink.ast.deref(decl.value)),
        };
    }

    fn has_attribute(self: *builder, attrs: []const ink.ast.attribute, name: []const u8) bool {
        _ = self;
        for (attrs) |attr| {
            if (std.mem.eql(u8, attr.name.string, name)) return true;
        }
        return false;
    }

    fn build_struct_decl(self: *builder, s: ink.ast.struct_decl) build_error!ir.struct_decl {
        return .{
            .name = try self.intern_string(s.name.string),
            .generics = try self.build_generic_params(s.generics),
            .fields = try self.build_struct_fields(s.fields),
            .is_record = self.has_attribute(s.attributes, "record"),
        };
    }

    fn build_trait_decl(self: *builder, t: ink.ast.trait_decl) build_error!ir.trait_decl {
        return .{
            .is_auto = t.is_auto,
            .name = try self.intern_string(t.name.string),
            .generics = try self.build_generic_params(t.generics),
            .items = try self.build_trait_items(t.items),
            .requires = try self.build_node_refs(t.requires),
        };
    }

    fn build_enum_decl(self: *builder, e: ink.ast.enum_decl) build_error!ir.enum_decl {
        return .{
            .name = try self.intern_string(e.name.string),
            .generics = try self.build_generic_params(e.generics),
            .variants = try self.build_enum_variants(e.variants),
        };
    }

    fn build_impl_decl(self: *builder, i: ink.ast.impl_decl) build_error!ir.impl_decl {
        return .{
            .negative = i.negative,
            .for_struct = try self.intern_string(i.for_struct.string),
            .by_trait = try self.intern_string(i.by_trait.string),
            .functions = try self.build_function_list_with_self(i.functions, i.for_struct.string),
        };
    }

    fn build_const_decl(self: *builder, c: ink.ast.const_decl) build_error!ir.const_decl {
        return .{
            .name = try self.intern_string(c.name.string),
            .ty = if (c.ty) |ref| try self.build_node(ink.ast.deref(ref)) else null,
            .value = try self.build_node(ink.ast.deref(c.value)),
        };
    }

    fn build_var_decl(self: *builder, v: ink.ast.var_decl) build_error!ir.var_decl {
        return .{
            .name = try self.intern_string(v.name.string),
            .ty = if (v.ty) |ref| try self.build_node(ink.ast.deref(ref)) else null,
            .value = try self.build_node(ink.ast.deref(v.value)),
        };
    }

    fn build_trait_items(self: *builder, items: []const ink.ast.trait_item) build_error![]const ir.trait_decl.trait_item {
        if (items.len == 0) return &[_]ir.trait_decl.trait_item{};
        var out = try self.alloc(ir.trait_decl.trait_item, items.len);
        for (items, 0..) |item, i| {
            out[i] = switch (item) {
                .function => |func| .{ .function = try self.build_function_decl(func) },
                .assoc_type => |assoc| .{ .assoc_type = try self.build_assoc_type_decl(assoc) },
            };
        }
        return out;
    }

    fn build_assoc_type_decl(self: *builder, assoc: ink.ast.associated_type_decl) build_error!ir.associated_type_decl {
        return .{
            .name = try self.intern_string(assoc.name.string),
            .value = if (assoc.value) |ref| try self.build_node(ink.ast.deref(ref)) else null,
        };
    }

    fn build_enum_variants(self: *builder, variants: []const ink.ast.sum_variant) build_error![]const ir.enum_variant {
        if (variants.len == 0) return &[_]ir.enum_variant{};
        var out = try self.alloc(ir.enum_variant, variants.len);
        for (variants, 0..) |variant, i| {
            out[i] = .{
                .name = try self.intern_string(variant.name.string),
                .payload = if (variant.payload) |ref| try self.build_node(ink.ast.deref(ref)) else null,
            };
        }
        return out;
    }

    fn build_struct_fields(self: *builder, fields: []const ink.ast.struct_field) build_error![]const ir.struct_decl.field {
        if (fields.len == 0) return &[_]ir.struct_decl.field{};
        var out = try self.alloc(ir.struct_decl.field, fields.len);
        for (fields, 0..) |field, i| {
            out[i] = .{
                .name = try self.intern_string(field.name.string),
                .ty = try self.build_node(ink.ast.deref(field.ty)),
            };
        }
        return out;
    }

    fn build_generic_params(self: *builder, params: []const ink.ast.generic_param) build_error![]const ir.generic_param {
        if (params.len == 0) return &[_]ir.generic_param{};
        var out = try self.alloc(ir.generic_param, params.len);
        for (params, 0..) |param, i| {
            out[i] = .{
                .name = try self.intern_string(param.name.string),
                .kind = map_generic_kind(param.kind),
                .constraint = if (param.constraint) |ref| try self.build_node(ink.ast.deref(ref)) else null,
                .default = if (param.default) |ref| try self.build_node(ink.ast.deref(ref)) else null,
                .is_pack = param.is_pack,
            };
        }
        return out;
    }

    fn build_params(self: *builder, params: []const ink.ast.param) build_error![]const ir.function_decl.param {
        if (params.len == 0) return &[_]ir.function_decl.param{};
        var out = try self.alloc(ir.function_decl.param, params.len);
        for (params, 0..) |param, i| {
            out[i] = .{
                .name = try self.intern_string(param.name.string),
                .ty = try self.build_node(ink.ast.deref(param.ty)),
                .variadic = param.variadic,
            };
        }
        return out;
    }

    fn build_params_with_self(
        self: *builder,
        params: []const ink.ast.param,
        self_name: []const u8,
    ) build_error![]const ir.function_decl.param {
        if (params.len == 0) return &[_]ir.function_decl.param{};
        var out = try self.alloc(ir.function_decl.param, params.len);
        for (params, 0..) |param, i| {
            out[i] = .{
                .name = try self.intern_string(param.name.string),
                .ty = try self.build_node_with_self(ink.ast.deref(param.ty), self_name),
                .variadic = param.variadic,
            };
        }
        return out;
    }

    fn build_where_clause(self: *builder, clauses: []const ink.ast.where_req) build_error![]const ir.function_decl.where_req {
        if (clauses.len == 0) return &[_]ir.function_decl.where_req{};
        var out = try self.alloc(ir.function_decl.where_req, clauses.len);
        for (clauses, 0..) |req, i| {
            out[i] = .{
                .name = try self.intern_string(req.name.string),
                .constraint = try self.build_node(ink.ast.deref(req.constraint)),
            };
        }
        return out;
    }

    fn build_where_clause_with_self(
        self: *builder,
        clauses: []const ink.ast.where_req,
        self_name: []const u8,
    ) build_error![]const ir.function_decl.where_req {
        if (clauses.len == 0) return &[_]ir.function_decl.where_req{};
        var out = try self.alloc(ir.function_decl.where_req, clauses.len);
        for (clauses, 0..) |req, i| {
            out[i] = .{
                .name = try self.intern_string(req.name.string),
                .constraint = try self.build_node_with_self(ink.ast.deref(req.constraint), self_name),
            };
        }
        return out;
    }

    fn build_function_list(self: *builder, funcs: []const ink.ast.function_decl) build_error![]const ir.function_decl {
        if (funcs.len == 0) return &[_]ir.function_decl{};
        var out = try self.alloc(ir.function_decl, funcs.len);
        for (funcs, 0..) |func, i| {
            out[i] = try self.build_function_decl(func);
        }
        return out;
    }

    fn build_function_list_with_self(
        self: *builder,
        funcs: []const ink.ast.function_decl,
        self_name: []const u8,
    ) build_error![]const ir.function_decl {
        if (funcs.len == 0) return &[_]ir.function_decl{};
        var out = try self.alloc(ir.function_decl, funcs.len);
        for (funcs, 0..) |func, i| {
            out[i] = try self.build_function_decl_with_self(func, self_name);
        }
        return out;
    }

    fn build_identifier_list(self: *builder, items: []const ink.identifier) build_error![]const string_identifier {
        if (items.len == 0) return &[_]string_identifier{};
        var out = try self.alloc(string_identifier, items.len);
        for (items, 0..) |item, i| {
            out[i] = try self.intern_string(item.string);
        }
        return out;
    }

    fn build_node_refs(self: *builder, refs: []const ink.ast.node_ref) build_error![]const ir_identifier {
        if (refs.len == 0) return &[_]ir_identifier{};
        var out = try self.alloc(ir_identifier, refs.len);
        for (refs, 0..) |ref, i| {
            out[i] = try self.build_node(ink.ast.deref(ref));
        }
        return out;
    }

    fn emit(self: *builder, value: ir) build_error!ir_identifier {
        const idx = self.nodes.items.len;
        self.nodes.append(value) catch return error.out_of_memory;
        const current_span = if (self.span_stack.items.len == 0)
            null
        else
            self.span_stack.items[self.span_stack.items.len - 1];
        self.spans.append(current_span) catch return error.out_of_memory;
        self.sources.append(self.current_source_id) catch return error.out_of_memory;
        return .{ .idx = @intCast(idx) };
    }

    fn intern_string(self: *builder, value: []const u8) build_error!string_identifier {
        if (self.string_map.get(value)) |id| return id;
        const idx: u32 = @intCast(self.strings.items.len);
        self.strings.append(value) catch return error.out_of_memory;
        const id = string_identifier{ .idx = idx };
        self.string_map.put(self.allocator, value, id) catch return error.out_of_memory;
        return id;
    }

    fn alloc(self: *builder, comptime T: type, len: usize) build_error![]T {
        return self.allocator.alloc(T, len) catch return error.out_of_memory;
    }

    fn map_generic_kind(kind: ink.ast.generic_kind) ir.generic_param.generic_kind {
        return @enumFromInt(@intFromEnum(kind));
    }

    fn fail(self: *builder, kind: error_kind) build_error {
        self.last_error = kind;
        return error.build_failed;
    }
};
