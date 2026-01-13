const std = @import("std");
const sim = @import("sim.zig");
const sim_runtime = @import("sim_runtime.zig");
const async_sim = @import("runtime/async_sim.zig");
const async_common = @import("runtime/async_common.zig");
const trace = @import("runtime/trace.zig");

const array_list = std.array_list.Managed;
const FaultKind = async_sim.FaultKind;

pub const ReportOptions = struct {
    root_dir: []const u8,
    manifest_path: []const u8,
};

const ScenarioSummary = struct {
    name: []const u8,
    runs: u64,
    events: u64,
    fault_counts: []u64,
};

const fault_kind_names = blk: {
    const fields = @typeInfo(FaultKind).@"enum".fields;
    var names: [fields.len][]const u8 = undefined;
    for (fields, 0..) |field, idx| {
        names[idx] = field.name;
    }
    break :blk names;
};

const op_kind = async_common.op_kind;
const op_kind_names = blk: {
    const fields = @typeInfo(op_kind).@"enum".fields;
    var names: [fields.len][]const u8 = undefined;
    for (fields, 0..) |field, idx| {
        names[idx] = field.name;
    }
    break :blk names;
};

pub fn emit_reports(
    allocator: std.mem.Allocator,
    reports: []const async_sim.Report,
    runs: []const sim_runtime.Run,
    cfg: sim.Report,
    options: ReportOptions,
) !void {
    if (reports.len == 0) return;

    var run_map = std.StringHashMap(*const sim_runtime.Run).init(allocator);
    defer run_map.deinit();
    for (runs) |*run| {
        _ = try run_map.put(run.name, run);
    }

    const report_indices = try build_report_indices(allocator, reports);
    defer allocator.free(report_indices);

    var stdout_buf: [4096]u8 = undefined;
    var file_buf: [4096]u8 = undefined;
    var text_writer = std.fs.File.stdout().writer(&stdout_buf);
    var text_out = &text_writer.interface;
    var text_file: ?std.fs.File = null;
    defer if (text_file) |file| file.close();
    if (cfg.text.len != 0) {
        text_file = try std.fs.cwd().createFile(cfg.text, .{});
        text_writer = text_file.?.writer(&file_buf);
        text_out = &text_writer.interface;
    }

    const wants_text = cfg.text.len != 0 or cfg.per_scenario or cfg.aggregate or cfg.tree;
    if (wants_text) {
        try write_text_report(text_out, allocator, reports, report_indices, &run_map, cfg, options);
        try text_out.flush();
    }

    if (cfg.json.len != 0) {
        var file = try std.fs.cwd().createFile(cfg.json, .{});
        defer file.close();
        var json_buf: [4096]u8 = undefined;
        var json_writer = file.writer(&json_buf);
        var json_out = &json_writer.interface;
        try write_json_report(json_out, allocator, reports, report_indices, &run_map, cfg, options);
        try json_out.flush();
    }
}

fn build_report_indices(
    allocator: std.mem.Allocator,
    reports: []const async_sim.Report,
) ![]usize {
    const indices = try allocator.alloc(usize, reports.len);
    for (reports, 0..) |_, idx| indices[idx] = idx;
    std.mem.sort(usize, indices, reports, report_index_less);
    return indices;
}

fn report_index_less(reports: []const async_sim.Report, lhs: usize, rhs: usize) bool {
    const left = reports[lhs];
    const right = reports[rhs];
    const order = std.mem.order(u8, left.scenario_name, right.scenario_name);
    if (order != .eq) return order == .lt;
    return std.mem.lessThan(u8, left.run_name, right.run_name);
}

fn write_text_report(
    writer: *std.Io.Writer,
    allocator: std.mem.Allocator,
    reports: []const async_sim.Report,
    report_indices: []const usize,
    run_map: *const std.StringHashMap(*const sim_runtime.Run),
    cfg: sim.Report,
    options: ReportOptions,
) !void {
    const fault_kind_count: usize = fault_kind_names.len;
    var scenario_map = std.StringHashMap(usize).init(allocator);
    defer scenario_map.deinit();
    var summaries = array_list(ScenarioSummary).init(allocator);
    defer {
        for (summaries.items) |summary| allocator.free(summary.fault_counts);
        summaries.deinit();
    }

    var aggregate_counts = try allocator.alloc(u64, fault_kind_count);
    defer allocator.free(aggregate_counts);
    @memset(aggregate_counts, 0);

    var aggregate_events: u64 = 0;

    for (reports) |report| {
        const key = report.scenario_name;
        const index = scenario_map.get(key) orelse blk: {
            const counts = try allocator.alloc(u64, fault_kind_count);
            @memset(counts, 0);
            const idx = summaries.items.len;
            try summaries.append(.{
                .name = key,
                .runs = 0,
                .events = 0,
                .fault_counts = counts,
            });
            try scenario_map.put(key, idx);
            break :blk idx;
        };
        summaries.items[index].runs += 1;
        summaries.items[index].events += report.event_count;
        aggregate_events += report.event_count;

        for (report.fault_counts, 0..) |count, idx| {
            summaries.items[index].fault_counts[idx] += count;
            aggregate_counts[idx] += count;
        }
    }

    std.mem.sort(ScenarioSummary, summaries.items, {}, scenario_less_than);

    try writer.print("sim report: {d} runs\n", .{reports.len});
    try write_run_details(writer, reports, report_indices, run_map, options);

    if (cfg.per_scenario) {
        for (summaries.items) |summary| {
            try writer.print("scenario {s}: runs={d} events={d}\n", .{ summary.name, summary.runs, summary.events });
            try write_fault_counts(writer, summary.fault_counts, 1);
        }
    }

    if (cfg.aggregate) {
        try writer.print("aggregate: events={d}\n", .{aggregate_events});
        try write_fault_counts(writer, aggregate_counts, 1);
    }

    if (cfg.tree) {
        const event_root = try build_event_tree(allocator, reports);
        defer free_event_tree(allocator, event_root);
        try writer.writeAll("event tree:\n");
        try write_event_tree(writer, allocator, event_root);
        try writer.writeAll("event path leaves:\n");
        try write_event_paths(writer, allocator, event_root);

        const program_root = try build_program_tree(allocator, reports);
        if (program_root) |root_node| {
            var program_tree = trace.ProgramTree{ .root = root_node };
            defer program_tree.deinit(allocator);
            try writer.writeAll("program tree:\n");
            try write_program_tree(writer, allocator, root_node);
            try writer.writeAll("program path leaves:\n");
            try write_program_paths(writer, allocator, root_node);
        }
    }
}

fn write_run_details(
    writer: *std.Io.Writer,
    reports: []const async_sim.Report,
    report_indices: []const usize,
    run_map: *const std.StringHashMap(*const sim_runtime.Run),
    options: ReportOptions,
) !void {
    if (reports.len == 0) return;
    try writer.writeAll("runs:\n");
    for (report_indices) |idx| {
        const report = reports[idx];
        const run = run_map.get(report.run_name);
        try writer.print(
            "run {s}: scenario={s} seed={d} events={d}\n",
            .{ report.run_name, report.scenario_name, report.seed, report.event_count },
        );
        if (run) |info| {
            if (info.params.len != 0) {
                try writer.writeAll("  params:");
                for (info.params) |param| {
                    try writer.print(" {s}={s}", .{ param.key, param.value });
                }
                try writer.writeAll("\n");
            }
            if (info.config.snapshot_dir) |dir| {
                const rel = relative_path(dir, options.root_dir);
                try writer.print("  snapshot dir: {s}\n", .{rel});
            }
        }
        if (has_faults(report.fault_counts)) {
            try writer.writeAll("  faults:\n");
            try write_fault_counts(writer, report.fault_counts, 2);
        } else {
            try writer.writeAll("  faults: none\n");
        }
        try write_fault_events(writer, report.fault_sequence);
        try write_snapshots(writer, report.snapshot_paths, options);
        try write_events(writer, report.events);
        if (report.snapshot_paths.len != 0) {
            const last = report.snapshot_paths[report.snapshot_paths.len - 1];
            try writer.print(
                "  replay: quill sim --manifest {s} --scenario {s} --replay {s}\n",
                .{ options.manifest_path, report.scenario_name, last },
            );
        }
    }
}

fn write_fault_counts(writer: *std.Io.Writer, counts: []const u64, indent: usize) !void {
    var idx: usize = 0;
    while (idx < counts.len and idx < fault_kind_names.len) : (idx += 1) {
        if (counts[idx] == 0) continue;
        try write_indent(writer, indent);
        try writer.print("{s}: {d}\n", .{ fault_kind_names[idx], counts[idx] });
    }
}

fn has_faults(counts: []const u64) bool {
    for (counts) |count| {
        if (count != 0) return true;
    }
    return false;
}

fn write_fault_events(writer: *std.Io.Writer, sequence: []const FaultKind) !void {
    var count: usize = 0;
    for (sequence) |kind| {
        if (kind != .none) count += 1;
    }
    if (count == 0) return;
    try writer.print("  fault events ({d}): ", .{count});
    var emitted: usize = 0;
    for (sequence, 0..) |kind, idx| {
        if (kind == .none) continue;
        if (emitted != 0) try writer.writeAll(", ");
        const name = fault_kind_names[@intFromEnum(kind)];
        try writer.print("#{d} {s}", .{ idx + 1, name });
        emitted += 1;
    }
    try writer.writeAll("\n");
}

fn write_events(writer: *std.Io.Writer, events: []const async_sim.ReportEvent) !void {
    try writer.print("  events: {d}\n", .{events.len});
    for (events, 0..) |event, idx| {
        try writer.writeAll("    #");
        try writer.print("{d} ", .{idx + 1});
        const op_name = op_kind_names[@intFromEnum(event.kind)];
        const fault_name = fault_kind_names[@intFromEnum(event.fault_kind)];
        try writer.print(
            "{s} fault={s} result={d} err={d} task={d} op={d} time={d}\n",
            .{
                op_name,
                fault_name,
                event.result,
                event.err_code,
                event.user_data,
                event.op_id,
                event.time_ns,
            },
        );
    }
}

fn write_snapshots(
    writer: *std.Io.Writer,
    paths: []const []const u8,
    options: ReportOptions,
) !void {
    try writer.print("  snapshots: {d}\n", .{paths.len});
    if (paths.len == 0) return;
    if (paths.len <= 5) {
        for (paths) |path| {
            const rel = relative_path(path, options.root_dir);
            try writer.print("    - {s}\n", .{rel});
        }
        return;
    }
    const first = relative_path(paths[0], options.root_dir);
    const last = relative_path(paths[paths.len - 1], options.root_dir);
    try writer.print("    first: {s}\n", .{first});
    try writer.print("    last:  {s}\n", .{last});
}

fn scenario_less_than(_: void, lhs: ScenarioSummary, rhs: ScenarioSummary) bool {
    return std.mem.lessThan(u8, lhs.name, rhs.name);
}

const EventKey = struct {
    kind: op_kind,
    fault_kind: FaultKind,
};

const EventPath = struct {
    count: u64,
    steps: []EventKey,
};

const ProgramStepKind = enum {
    task,
    call,
    io,
};

const ProgramStep = struct {
    kind: ProgramStepKind,
    label: []const u8,
};

const ProgramPath = struct {
    count: u64,
    steps: []ProgramStep,
};

const EventTree = struct {
    count: u64,
    children: std.AutoHashMap(EventKey, *EventTree),
};

const EventChild = struct {
    key: EventKey,
    node: *EventTree,
};

fn build_event_tree(allocator: std.mem.Allocator, reports: []const async_sim.Report) !*EventTree {
    const root = try allocator.create(EventTree);
    root.* = .{ .count = 0, .children = std.AutoHashMap(EventKey, *EventTree).init(allocator) };
    for (reports) |report| {
        if (report.events.len == 0) continue;
        var node = root;
        node.count += 1;
        for (report.events) |event| {
            const key = EventKey{ .kind = event.kind, .fault_kind = event.fault_kind };
            const entry = try node.children.getOrPut(key);
            if (!entry.found_existing) {
                const child = try allocator.create(EventTree);
                child.* = .{ .count = 0, .children = std.AutoHashMap(EventKey, *EventTree).init(allocator) };
                entry.value_ptr.* = child;
            }
            node = entry.value_ptr.*;
            node.count += 1;
        }
    }
    return root;
}

fn collect_event_children(
    allocator: std.mem.Allocator,
    node: *EventTree,
) ![]EventChild {
    const op_fields = @typeInfo(op_kind).@"enum".fields;
    const fault_fields = @typeInfo(FaultKind).@"enum".fields;
    var list = array_list(EventChild).init(allocator);
    errdefer list.deinit();
    inline for (op_fields) |op_field| {
        const kind: op_kind = @enumFromInt(op_field.value);
        inline for (fault_fields) |fault_field| {
            const fault_kind: FaultKind = @enumFromInt(fault_field.value);
            const key = EventKey{ .kind = kind, .fault_kind = fault_kind };
            if (node.children.get(key)) |child| {
                try list.append(.{ .key = key, .node = child });
            }
        }
    }
    return try list.toOwnedSlice();
}

fn write_event_tree(writer: *std.Io.Writer, allocator: std.mem.Allocator, root: *EventTree) !void {
    var stack = array_list(bool).init(allocator);
    defer stack.deinit();
    const children = try collect_event_children(allocator, root);
    defer allocator.free(children);
    for (children, 0..) |child, idx| {
        const is_last = idx + 1 == children.len;
        try write_event_tree_node(writer, allocator, child, &stack, is_last);
    }
}

fn write_event_tree_node(
    writer: *std.Io.Writer,
    allocator: std.mem.Allocator,
    child: EventChild,
    stack: *array_list(bool),
    is_last: bool,
) !void {
    try write_tree_prefix(writer, "  ", stack.items, is_last);
    const op_name = op_kind_names[@intFromEnum(child.key.kind)];
    if (child.key.fault_kind == .none) {
        try writer.print("{s} (count={d})\n", .{ op_name, child.node.count });
    } else {
        const fault_name = fault_kind_names[@intFromEnum(child.key.fault_kind)];
        try writer.print("{s}/{s} (count={d})\n", .{ op_name, fault_name, child.node.count });
    }
    const children = try collect_event_children(allocator, child.node);
    defer allocator.free(children);
    if (children.len == 0) return;
    try stack.append(is_last);
    defer _ = stack.pop();
    for (children, 0..) |grand, idx| {
        const is_last_child = idx + 1 == children.len;
        try write_event_tree_node(writer, allocator, grand, stack, is_last_child);
    }
}

fn write_indent(writer: *std.Io.Writer, depth: usize) !void {
    var idx: usize = 0;
    while (idx < depth) : (idx += 1) {
        try writer.writeAll("  ");
    }
}

fn write_tree_prefix(
    writer: *std.Io.Writer,
    base: []const u8,
    stack: []const bool,
    is_last: bool,
) !void {
    if (base.len != 0) {
        try writer.writeAll(base);
    }
    for (stack) |ancestor_last| {
        try writer.writeAll(if (ancestor_last) "   " else "│  ");
    }
    try writer.writeAll(if (is_last) "└─ " else "├─ ");
}

fn free_event_tree(allocator: std.mem.Allocator, node: *EventTree) void {
    var it = node.children.iterator();
    while (it.next()) |entry| {
        free_event_tree(allocator, entry.value_ptr.*);
    }
    node.children.deinit();
    allocator.destroy(node);
}

fn write_event_paths(
    writer: *std.Io.Writer,
    allocator: std.mem.Allocator,
    root: *EventTree,
) !void {
    const paths = try build_event_paths(allocator, root);
    defer free_event_paths(allocator, paths);

    if (paths.len == 0) {
        try writer.writeAll("  <none>\n");
        return;
    }

    for (paths, 0..) |path, idx| {
        const is_last = idx + 1 == paths.len;
        try write_tree_prefix(writer, "  ", &.{}, is_last);
        try write_event_path(writer, path.steps);
        try writer.print(" (count={d})\n", .{path.count});
    }
}

fn write_event_path(writer: *std.Io.Writer, steps: []const EventKey) !void {
    if (steps.len == 0) {
        try writer.writeAll("<none>");
        return;
    }
    var idx: usize = 0;
    while (idx < steps.len) {
        const key = steps[idx];
        var run_len: usize = 1;
        while (idx + run_len < steps.len and event_key_eq(key, steps[idx + run_len])) {
            run_len += 1;
        }
        if (idx != 0) try writer.writeAll(" -> ");
        try write_event_key(writer, key);
        if (run_len > 1) {
            try writer.print(" x{d}", .{run_len});
        }
        idx += run_len;
    }
}

fn write_event_key(writer: *std.Io.Writer, key: EventKey) !void {
    const op_name = op_kind_names[@intFromEnum(key.kind)];
    if (key.fault_kind == .none) {
        try writer.print("{s}", .{op_name});
    } else {
        const fault_name = fault_kind_names[@intFromEnum(key.fault_kind)];
        try writer.print("{s}/{s}", .{ op_name, fault_name });
    }
}

fn event_key_eq(lhs: EventKey, rhs: EventKey) bool {
    return lhs.kind == rhs.kind and lhs.fault_kind == rhs.fault_kind;
}

fn build_event_paths(
    allocator: std.mem.Allocator,
    root: *EventTree,
) ![]EventPath {
    var paths = array_list(EventPath).init(allocator);
    errdefer {
        for (paths.items) |path| allocator.free(path.steps);
        paths.deinit();
    }
    var stack = array_list(EventKey).init(allocator);
    defer stack.deinit();

    try collect_event_paths(allocator, root, &stack, &paths);
    return try paths.toOwnedSlice();
}

fn collect_event_paths(
    allocator: std.mem.Allocator,
    node: *EventTree,
    stack: *array_list(EventKey),
    paths: *array_list(EventPath),
) !void {
    if (node.children.count() == 0) {
        const steps = try allocator.dupe(EventKey, stack.items);
        try paths.append(.{ .count = node.count, .steps = steps });
        return;
    }

    const op_fields = @typeInfo(op_kind).@"enum".fields;
    const fault_fields = @typeInfo(FaultKind).@"enum".fields;
    inline for (op_fields) |op_field| {
        const kind: op_kind = @enumFromInt(op_field.value);
        inline for (fault_fields) |fault_field| {
            const fault_kind: FaultKind = @enumFromInt(fault_field.value);
            const key = EventKey{ .kind = kind, .fault_kind = fault_kind };
            if (node.children.get(key)) |child| {
                try stack.append(key);
                try collect_event_paths(allocator, child, stack, paths);
                _ = stack.pop();
            }
        }
    }
}

fn free_event_paths(allocator: std.mem.Allocator, paths: []EventPath) void {
    for (paths) |path| allocator.free(path.steps);
    allocator.free(paths);
}

fn build_program_tree(
    allocator: std.mem.Allocator,
    reports: []const async_sim.Report,
) !?*trace.TaskNode {
    var root: ?*trace.TaskNode = null;
    errdefer {
        if (root) |node| {
            var tree = trace.ProgramTree{ .root = node };
            tree.deinit(allocator);
        }
    }

    for (reports) |report| {
        const tree = report.program_tree orelse continue;
        const tree_root = tree.root orelse continue;
        if (root == null) {
            root = try clone_task_node(allocator, tree_root);
        } else {
            try merge_task_node(allocator, root.?, tree_root);
        }
    }

    return root;
}

fn merge_task_node(
    allocator: std.mem.Allocator,
    dst: *trace.TaskNode,
    src: *trace.TaskNode,
) !void {
    dst.count += src.count;
    if (src.call_root) |src_call| {
        if (dst.call_root) |dst_call| {
            try merge_call_node(allocator, dst_call, src_call);
        } else {
            dst.call_root = try clone_call_node(allocator, src_call);
        }
    }
    var it = src.children.iterator();
    while (it.next()) |entry| {
        const child = entry.value_ptr.*;
        if (dst.children.get(child.label)) |existing| {
            try merge_task_node(allocator, existing, child);
        } else {
            const clone = try clone_task_node(allocator, child);
            try dst.children.put(clone.label, clone);
        }
    }
}

fn merge_call_node(
    allocator: std.mem.Allocator,
    dst: *trace.CallNode,
    src: *trace.CallNode,
) !void {
    dst.count += src.count;
    var idx: usize = 0;
    while (idx < dst.io_counts.len) : (idx += 1) {
        dst.io_counts[idx] += src.io_counts[idx];
    }
    var it = src.children.iterator();
    while (it.next()) |entry| {
        const child = entry.value_ptr.*;
        if (dst.children.get(child.label)) |existing| {
            try merge_call_node(allocator, existing, child);
        } else {
            const clone = try clone_call_node(allocator, child);
            try dst.children.put(clone.label, clone);
        }
    }
}

fn clone_task_node(allocator: std.mem.Allocator, src: *trace.TaskNode) !*trace.TaskNode {
    const node = try trace.make_task_node(allocator, src.label, false);
    node.count = src.count;
    if (src.call_root) |call_root| {
        node.call_root = try clone_call_node(allocator, call_root);
    }
    var it = src.children.iterator();
    while (it.next()) |entry| {
        const child = entry.value_ptr.*;
        const clone = try clone_task_node(allocator, child);
        try node.children.put(clone.label, clone);
    }
    return node;
}

fn clone_call_node(allocator: std.mem.Allocator, src: *trace.CallNode) !*trace.CallNode {
    const node = try trace.make_call_node(allocator, src.label);
    node.count = src.count;
    node.io_counts = src.io_counts;
    var it = src.children.iterator();
    while (it.next()) |entry| {
        const child = entry.value_ptr.*;
        const clone = try clone_call_node(allocator, child);
        try node.children.put(clone.label, clone);
    }
    return node;
}

fn write_program_tree(
    writer: *std.Io.Writer,
    allocator: std.mem.Allocator,
    root: *trace.TaskNode,
) !void {
    var stack = array_list(bool).init(allocator);
    defer stack.deinit();
    const children = try collect_task_children(allocator, root);
    defer allocator.free(children);
    for (children, 0..) |child, idx| {
        const is_last = idx + 1 == children.len;
        try write_task_node(writer, allocator, child, &stack, is_last);
    }
}

fn write_task_node(
    writer: *std.Io.Writer,
    allocator: std.mem.Allocator,
    node: *trace.TaskNode,
    stack: *array_list(bool),
    is_last: bool,
) !void {
    try write_tree_prefix(writer, "  ", stack.items, is_last);
    try writer.print("task {s} (count={d})\n", .{ node.label, node.count });

    const children = try collect_task_children(allocator, node);
    defer allocator.free(children);
    const has_call_root = node.call_root != null;
    const total = children.len + @intFromBool(has_call_root);
    if (total == 0) return;

    try stack.append(is_last);
    defer _ = stack.pop();

    var idx: usize = 0;
    if (node.call_root) |call_root| {
        const is_last_child = idx + 1 == total;
        try write_call_node(writer, allocator, call_root, stack, is_last_child);
        idx += 1;
    }
    for (children) |child| {
        const is_last_child = idx + 1 == total;
        try write_task_node(writer, allocator, child, stack, is_last_child);
        idx += 1;
    }
}

fn write_call_node(
    writer: *std.Io.Writer,
    allocator: std.mem.Allocator,
    node: *trace.CallNode,
    stack: *array_list(bool),
    is_last: bool,
) !void {
    try write_tree_prefix(writer, "  ", stack.items, is_last);
    try writer.print("fn {s} (count={d})\n", .{ node.label, node.count });

    var io_total: usize = 0;
    var io_idx: usize = 0;
    while (io_idx < node.io_counts.len and io_idx < op_kind_names.len) : (io_idx += 1) {
        if (node.io_counts[io_idx] != 0) io_total += 1;
    }

    const children = try collect_call_children(allocator, node);
    defer allocator.free(children);
    const total = io_total + children.len;
    if (total == 0) return;

    try stack.append(is_last);
    defer _ = stack.pop();

    var idx: usize = 0;
    var op_idx: usize = 0;
    while (op_idx < node.io_counts.len and op_idx < op_kind_names.len) : (op_idx += 1) {
        const count = node.io_counts[op_idx];
        if (count == 0) continue;
        const is_last_child = idx + 1 == total;
        try write_tree_prefix(writer, "  ", stack.items, is_last_child);
        try writer.print("io.{s} (count={d})\n", .{ op_kind_names[op_idx], count });
        idx += 1;
    }

    for (children) |child| {
        const is_last_child = idx + 1 == total;
        try write_call_node(writer, allocator, child, stack, is_last_child);
        idx += 1;
    }
}

fn collect_task_children(
    allocator: std.mem.Allocator,
    node: *trace.TaskNode,
) ![]*trace.TaskNode {
    var list = try allocator.alloc(*trace.TaskNode, node.children.count());
    var idx: usize = 0;
    var it = node.children.iterator();
    while (it.next()) |entry| {
        list[idx] = entry.value_ptr.*;
        idx += 1;
    }
    std.mem.sort(*trace.TaskNode, list, {}, task_node_less_than);
    return list;
}

fn collect_call_children(
    allocator: std.mem.Allocator,
    node: *trace.CallNode,
) ![]*trace.CallNode {
    var list = try allocator.alloc(*trace.CallNode, node.children.count());
    var idx: usize = 0;
    var it = node.children.iterator();
    while (it.next()) |entry| {
        list[idx] = entry.value_ptr.*;
        idx += 1;
    }
    std.mem.sort(*trace.CallNode, list, {}, call_node_less_than);
    return list;
}

fn task_node_less_than(_: void, lhs: *trace.TaskNode, rhs: *trace.TaskNode) bool {
    return std.mem.lessThan(u8, lhs.label, rhs.label);
}

fn call_node_less_than(_: void, lhs: *trace.CallNode, rhs: *trace.CallNode) bool {
    return std.mem.lessThan(u8, lhs.label, rhs.label);
}

fn build_program_paths(
    allocator: std.mem.Allocator,
    root: *trace.TaskNode,
) ![]ProgramPath {
    var paths = array_list(ProgramPath).init(allocator);
    errdefer {
        for (paths.items) |path| allocator.free(path.steps);
        paths.deinit();
    }
    var stack = array_list(ProgramStep).init(allocator);
    defer stack.deinit();

    const children = try collect_task_children(allocator, root);
    defer allocator.free(children);
    for (children) |child| {
        try collect_program_paths_task(allocator, child, &stack, &paths);
    }
    return try paths.toOwnedSlice();
}

fn collect_program_paths_task(
    allocator: std.mem.Allocator,
    node: *trace.TaskNode,
    stack: *array_list(ProgramStep),
    paths: *array_list(ProgramPath),
) !void {
    try stack.append(.{ .kind = .task, .label = node.label });

    var emitted = false;
    if (node.call_root) |call_root| {
        try collect_program_paths_call(allocator, call_root, stack, paths);
        emitted = true;
    }

    const children = try collect_task_children(allocator, node);
    defer allocator.free(children);
    for (children) |child| {
        try collect_program_paths_task(allocator, child, stack, paths);
        emitted = true;
    }

    if (!emitted) {
        const steps = try allocator.dupe(ProgramStep, stack.items);
        try paths.append(.{ .count = node.count, .steps = steps });
    }

    _ = stack.pop();
}

fn collect_program_paths_call(
    allocator: std.mem.Allocator,
    node: *trace.CallNode,
    stack: *array_list(ProgramStep),
    paths: *array_list(ProgramPath),
) !void {
    try stack.append(.{ .kind = .call, .label = node.label });

    var emitted = false;
    var op_idx: usize = 0;
    while (op_idx < node.io_counts.len and op_idx < op_kind_names.len) : (op_idx += 1) {
        const count = node.io_counts[op_idx];
        if (count == 0) continue;
        emitted = true;
        try stack.append(.{ .kind = .io, .label = op_kind_names[op_idx] });
        const steps = try allocator.dupe(ProgramStep, stack.items);
        try paths.append(.{ .count = count, .steps = steps });
        _ = stack.pop();
    }

    const children = try collect_call_children(allocator, node);
    defer allocator.free(children);
    for (children) |child| {
        emitted = true;
        try collect_program_paths_call(allocator, child, stack, paths);
    }

    if (!emitted) {
        const steps = try allocator.dupe(ProgramStep, stack.items);
        try paths.append(.{ .count = node.count, .steps = steps });
    }

    _ = stack.pop();
}

fn write_program_paths(
    writer: *std.Io.Writer,
    allocator: std.mem.Allocator,
    root: *trace.TaskNode,
) !void {
    const paths = try build_program_paths(allocator, root);
    defer free_program_paths(allocator, paths);

    if (paths.len == 0) {
        try writer.writeAll("  <none>\n");
        return;
    }

    for (paths, 0..) |path, idx| {
        const is_last = idx + 1 == paths.len;
        try write_tree_prefix(writer, "  ", &.{}, is_last);
        try write_program_path(writer, path.steps);
        try writer.print(" (count={d})\n", .{path.count});
    }
}

fn write_program_path(writer: *std.Io.Writer, steps: []const ProgramStep) !void {
    for (steps, 0..) |step, idx| {
        if (idx != 0) try writer.writeAll(" -> ");
        switch (step.kind) {
            .task => try writer.print("task {s}", .{step.label}),
            .call => try writer.print("fn {s}", .{step.label}),
            .io => try writer.print("io.{s}", .{step.label}),
        }
    }
}

fn free_program_paths(allocator: std.mem.Allocator, paths: []ProgramPath) void {
    for (paths) |path| allocator.free(path.steps);
    allocator.free(paths);
}

fn write_json_report(
    writer: *std.Io.Writer,
    allocator: std.mem.Allocator,
    reports: []const async_sim.Report,
    report_indices: []const usize,
    run_map: *const std.StringHashMap(*const sim_runtime.Run),
    cfg: sim.Report,
    options: ReportOptions,
) !void {
    var scenario_map = std.StringHashMap(usize).init(allocator);
    defer scenario_map.deinit();
    var summaries = array_list(ScenarioSummary).init(allocator);
    defer {
        for (summaries.items) |summary| allocator.free(summary.fault_counts);
        summaries.deinit();
    }

    const fault_kind_count: usize = fault_kind_names.len;
    var aggregate_counts = try allocator.alloc(u64, fault_kind_count);
    defer allocator.free(aggregate_counts);
    @memset(aggregate_counts, 0);
    var aggregate_events: u64 = 0;

    for (reports) |report| {
        const key = report.scenario_name;
        const index = scenario_map.get(key) orelse blk: {
            const counts = try allocator.alloc(u64, fault_kind_count);
            @memset(counts, 0);
            const idx = summaries.items.len;
            try summaries.append(.{
                .name = key,
                .runs = 0,
                .events = 0,
                .fault_counts = counts,
            });
            try scenario_map.put(key, idx);
            break :blk idx;
        };
        summaries.items[index].runs += 1;
        summaries.items[index].events += report.event_count;
        aggregate_events += report.event_count;
        for (report.fault_counts, 0..) |count, idx| {
            summaries.items[index].fault_counts[idx] += count;
            aggregate_counts[idx] += count;
        }
    }

    std.mem.sort(ScenarioSummary, summaries.items, {}, scenario_less_than);

    try writer.writeAll("{\n");
    try writer.print("  \"runs\": {d},\n", .{reports.len});

    try writer.writeAll("  \"run_details\": [\n");
    for (report_indices, 0..) |idx, report_idx| {
        const report = reports[idx];
        if (report_idx != 0) try writer.writeAll(",\n");
        try writer.writeAll("    {\"name\": ");
        try write_json_string(writer, report.run_name);
        try writer.writeAll(", \"scenario\": ");
        try write_json_string(writer, report.scenario_name);
        try writer.print(", \"seed\": {d}, \"events\": {d}, \"faults\": {{", .{ report.seed, report.event_count });
        var fi: usize = 0;
        while (fi < report.fault_counts.len and fi < fault_kind_names.len) : (fi += 1) {
            if (fi != 0) try writer.writeAll(", ");
            try writer.print("\"{s}\": {d}", .{ fault_kind_names[fi], report.fault_counts[fi] });
        }
        try writer.writeAll("}, \"fault_events\": [");
        try write_fault_events_json(writer, report.fault_sequence);
        try writer.writeAll("], \"events\": [");
        try write_events_json(writer, report.events);
        try writer.writeAll("], \"snapshots\": [");
        try write_snapshot_list_json(writer, report.snapshot_paths, options);
        try writer.writeAll("]");
        if (run_map.get(report.run_name)) |info| {
            if (info.params.len != 0) {
                try writer.writeAll(", \"params\": [");
                for (info.params, 0..) |param, pidx| {
                    if (pidx != 0) try writer.writeAll(", ");
                    try writer.writeAll("{\"key\": ");
                    try write_json_string(writer, param.key);
                    try writer.writeAll(", \"value\": ");
                    try write_json_string(writer, param.value);
                    try writer.writeAll("}");
                }
                try writer.writeAll("]");
            }
            if (info.config.snapshot_dir) |dir| {
                const rel = relative_path(dir, options.root_dir);
                try writer.writeAll(", \"snapshot_dir\": ");
                try write_json_string(writer, rel);
            }
        }
        if (report.snapshot_paths.len != 0) {
            const last = report.snapshot_paths[report.snapshot_paths.len - 1];
            try writer.writeAll(", \"replay\": {\"manifest\": ");
            try write_json_string(writer, options.manifest_path);
            try writer.writeAll(", \"scenario\": ");
            try write_json_string(writer, report.scenario_name);
            try writer.writeAll(", \"snapshot\": ");
            try write_json_string(writer, last);
            try writer.writeAll("}");
        }
        try writer.writeAll("}");
    }
    try writer.writeAll("\n  ],\n");

    if (cfg.per_scenario) {
        try writer.writeAll("  \"scenarios\": [\n");
        for (summaries.items, 0..) |summary, idx| {
            if (idx != 0) try writer.writeAll(",\n");
            try writer.writeAll("    {\"name\": ");
            try write_json_string(writer, summary.name);
            try writer.print(", \"runs\": {d}, \"events\": {d}, \"faults\": {{", .{ summary.runs, summary.events });
            var fi: usize = 0;
            while (fi < summary.fault_counts.len and fi < fault_kind_names.len) : (fi += 1) {
                if (fi != 0) try writer.writeAll(", ");
                try writer.print("\"{s}\": {d}", .{ fault_kind_names[fi], summary.fault_counts[fi] });
            }
            try writer.writeAll("} }");
        }
        try writer.writeAll("\n  ],\n");
    }

    if (cfg.aggregate) {
        try writer.writeAll("  \"aggregate\": {\"events\": ");
        try writer.print("{d}", .{aggregate_events});
        try writer.writeAll(", \"faults\": {");
        var fi: usize = 0;
        while (fi < aggregate_counts.len and fi < fault_kind_names.len) : (fi += 1) {
            if (fi != 0) try writer.writeAll(", ");
            try writer.print("\"{s}\": {d}", .{ fault_kind_names[fi], aggregate_counts[fi] });
        }
        try writer.writeAll("} },\n");
    }

    if (cfg.tree) {
        const root = try build_event_tree(allocator, reports);
        defer free_event_tree(allocator, root);
        try writer.writeAll("  \"tree_mode\": \"events\",\n");
        try writer.writeAll("  \"tree\": ");
        try write_event_tree_json(writer, root);
        try writer.writeAll(",\n");
        const paths = try build_event_paths(allocator, root);
        defer free_event_paths(allocator, paths);
        try writer.writeAll("  \"event_paths\": ");
        try write_event_paths_json(writer, paths);
        try writer.writeAll(",\n");

        const program_root = try build_program_tree(allocator, reports);
        try writer.writeAll("  \"program_tree\": ");
        if (program_root) |node| {
            var program_tree = trace.ProgramTree{ .root = node };
            defer program_tree.deinit(allocator);
            try write_program_tree_json(writer, allocator, node);
            const program_paths = try build_program_paths(allocator, node);
            defer free_program_paths(allocator, program_paths);
            try writer.writeAll(",\n  \"program_paths\": ");
            try write_program_paths_json(writer, program_paths);
        } else {
            try writer.writeAll("null");
            try writer.writeAll(",\n  \"program_paths\": null");
        }
        try writer.writeAll("\n");
    } else {
        try writer.writeAll("  \"tree\": null,\n");
        try writer.writeAll("  \"event_paths\": null,\n");
        try writer.writeAll("  \"program_tree\": null,\n");
        try writer.writeAll("  \"program_paths\": null\n");
    }

    try writer.writeAll("}\n");
}

fn write_fault_events_json(writer: *std.Io.Writer, sequence: []const FaultKind) !void {
    var first = true;
    for (sequence, 0..) |kind, idx| {
        if (kind == .none) continue;
        if (!first) try writer.writeAll(", ");
        first = false;
        try writer.writeAll("{\"index\": ");
        try writer.print("{d}", .{idx + 1});
        try writer.writeAll(", \"kind\": ");
        try write_json_string(writer, fault_kind_names[@intFromEnum(kind)]);
        try writer.writeAll("}");
    }
}

fn write_events_json(writer: *std.Io.Writer, events: []const async_sim.ReportEvent) !void {
    for (events, 0..) |event, idx| {
        if (idx != 0) try writer.writeAll(", ");
        try writer.writeAll("{\"index\": ");
        try writer.print("{d}", .{idx + 1});
        try writer.writeAll(", \"time_ns\": ");
        try writer.print("{d}", .{event.time_ns});
        try writer.writeAll(", \"op_id\": ");
        try writer.print("{d}", .{event.op_id});
        try writer.writeAll(", \"op\": ");
        try write_json_string(writer, op_kind_names[@intFromEnum(event.kind)]);
        try writer.writeAll(", \"fault\": ");
        try write_json_string(writer, fault_kind_names[@intFromEnum(event.fault_kind)]);
        try writer.writeAll(", \"result\": ");
        try writer.print("{d}", .{event.result});
        try writer.writeAll(", \"err_code\": ");
        try writer.print("{d}", .{event.err_code});
        try writer.writeAll(", \"user_data\": ");
        try writer.print("{d}", .{event.user_data});
        try writer.writeAll("}");
    }
}

fn write_snapshot_list_json(
    writer: *std.Io.Writer,
    paths: []const []const u8,
    options: ReportOptions,
) !void {
    for (paths, 0..) |path, idx| {
        if (idx != 0) try writer.writeAll(", ");
        const rel = relative_path(path, options.root_dir);
        try write_json_string(writer, rel);
    }
}

fn write_program_tree_json(
    writer: *std.Io.Writer,
    allocator: std.mem.Allocator,
    root: *trace.TaskNode,
) !void {
    try writer.writeAll("[");
    const children = try collect_task_children(allocator, root);
    defer allocator.free(children);
    for (children, 0..) |child, idx| {
        if (idx != 0) try writer.writeAll(", ");
        try write_task_node_json(writer, allocator, child);
    }
    try writer.writeAll("]");
}

fn write_task_node_json(
    writer: *std.Io.Writer,
    allocator: std.mem.Allocator,
    node: *trace.TaskNode,
) !void {
    try writer.writeAll("{\"label\": ");
    try write_json_string(writer, node.label);
    try writer.writeAll(", \"count\": ");
    try writer.print("{d}", .{node.count});
    try writer.writeAll(", \"calls\": [");
    if (node.call_root) |call_root| {
        try write_call_node_json(writer, allocator, call_root);
    }
    try writer.writeAll("], \"tasks\": [");
    const children = try collect_task_children(allocator, node);
    defer allocator.free(children);
    for (children, 0..) |child, idx| {
        if (idx != 0) try writer.writeAll(", ");
        try write_task_node_json(writer, allocator, child);
    }
    try writer.writeAll("]}");
}

fn write_call_node_json(
    writer: *std.Io.Writer,
    allocator: std.mem.Allocator,
    node: *trace.CallNode,
) !void {
    try writer.writeAll("{\"label\": ");
    try write_json_string(writer, node.label);
    try writer.writeAll(", \"count\": ");
    try writer.print("{d}", .{node.count});
    try writer.writeAll(", \"io\": [");
    var wrote_io = false;
    var io_idx: usize = 0;
    while (io_idx < node.io_counts.len and io_idx < op_kind_names.len) : (io_idx += 1) {
        const count = node.io_counts[io_idx];
        if (count == 0) continue;
        if (wrote_io) try writer.writeAll(", ");
        wrote_io = true;
        try writer.writeAll("{\"kind\": ");
        try write_json_string(writer, op_kind_names[io_idx]);
        try writer.writeAll(", \"count\": ");
        try writer.print("{d}", .{count});
        try writer.writeAll("}");
    }
    try writer.writeAll("], \"calls\": [");
    const children = try collect_call_children(allocator, node);
    defer allocator.free(children);
    for (children, 0..) |child, idx| {
        if (idx != 0) try writer.writeAll(", ");
        try write_call_node_json(writer, allocator, child);
    }
    try writer.writeAll("]}");
}

fn write_event_tree_json(writer: *std.Io.Writer, node: *EventTree) !void {
    try writer.writeAll("{\"count\": ");
    try writer.print("{d}", .{node.count});
    try writer.writeAll(", \"children\": [");
    var first = true;
    const op_fields = @typeInfo(op_kind).@"enum".fields;
    const fault_fields = @typeInfo(FaultKind).@"enum".fields;
    inline for (op_fields) |op_field| {
        const kind: op_kind = @enumFromInt(op_field.value);
        inline for (fault_fields) |fault_field| {
            const fault_kind: FaultKind = @enumFromInt(fault_field.value);
            const key = EventKey{ .kind = kind, .fault_kind = fault_kind };
            if (node.children.get(key)) |child| {
                if (!first) try writer.writeAll(", ");
                first = false;
                try writer.writeAll("{\"op\": ");
                try write_json_string(writer, op_field.name);
                try writer.writeAll(", \"fault\": ");
                try write_json_string(writer, fault_field.name);
                try writer.writeAll(", \"node\": ");
                try write_event_tree_json(writer, child);
                try writer.writeAll("}");
            }
        }
    }
    try writer.writeAll("]}");
}

fn write_event_paths_json(writer: *std.Io.Writer, paths: []const EventPath) !void {
    try writer.writeAll("[");
    for (paths, 0..) |path, idx| {
        if (idx != 0) try writer.writeAll(", ");
        try write_event_path_json(writer, path);
    }
    try writer.writeAll("]");
}

fn write_event_path_json(writer: *std.Io.Writer, path: EventPath) !void {
    try writer.writeAll("{\"count\": ");
    try writer.print("{d}", .{path.count});
    try writer.writeAll(", \"steps\": [");
    var idx: usize = 0;
    var first = true;
    while (idx < path.steps.len) {
        const key = path.steps[idx];
        var run_len: usize = 1;
        while (idx + run_len < path.steps.len and event_key_eq(key, path.steps[idx + run_len])) {
            run_len += 1;
        }
        if (!first) try writer.writeAll(", ");
        first = false;
        try writer.writeAll("{\"op\": ");
        try write_json_string(writer, op_kind_names[@intFromEnum(key.kind)]);
        try writer.writeAll(", \"fault\": ");
        try write_json_string(writer, fault_kind_names[@intFromEnum(key.fault_kind)]);
        try writer.writeAll(", \"repeat\": ");
        try writer.print("{d}", .{run_len});
        try writer.writeAll("}");
        idx += run_len;
    }
    try writer.writeAll("]}");
}

fn write_program_paths_json(writer: *std.Io.Writer, paths: []const ProgramPath) !void {
    try writer.writeAll("[");
    for (paths, 0..) |path, idx| {
        if (idx != 0) try writer.writeAll(", ");
        try write_program_path_json(writer, path);
    }
    try writer.writeAll("]");
}

fn write_program_path_json(writer: *std.Io.Writer, path: ProgramPath) !void {
    try writer.writeAll("{\"count\": ");
    try writer.print("{d}", .{path.count});
    try writer.writeAll(", \"steps\": [");
    for (path.steps, 0..) |step, idx| {
        if (idx != 0) try writer.writeAll(", ");
        try writer.writeAll("{\"kind\": ");
        switch (step.kind) {
            .task => try write_json_string(writer, "task"),
            .call => try write_json_string(writer, "fn"),
            .io => try write_json_string(writer, "io"),
        }
        try writer.writeAll(", \"label\": ");
        try write_json_string(writer, step.label);
        try writer.writeAll("}");
    }
    try writer.writeAll("]}");
}

fn relative_path(path: []const u8, root_dir: []const u8) []const u8 {
    if (root_dir.len == 0) return path;
    if (!std.mem.startsWith(u8, path, root_dir)) return path;
    var rel = path[root_dir.len..];
    if (rel.len != 0 and rel[0] == std.fs.path.sep) rel = rel[1..];
    if (rel.len == 0) return ".";
    return rel;
}

fn write_json_string(writer: *std.Io.Writer, text: []const u8) !void {
    try writer.writeAll("\"");
    for (text) |ch| {
        switch (ch) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            else => try writer.writeAll(&[_]u8{ch}),
        }
    }
    try writer.writeAll("\"");
}
