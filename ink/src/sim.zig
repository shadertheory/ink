const std = @import("std");

const array_list = std.array_list.Managed;

pub const KeyValue = struct {
    key: []const u8,
    value: []const u8,
};

pub const Scenario = struct {
    name: []const u8,
    components: []const KeyValue,
    faults: []const KeyValue,
};

pub const Snapshots = struct {
    steps: ?u64,
    mode: []const u8,
    compress: []const u8,
    retention: ?u64,
};

pub const Validation = struct {
    level: []const u8,
    overrides: []const KeyValue,
};

pub const Simulator = struct {
    seed: ?u64,
    concurrency: []const u8,
    snapshots: ?Snapshots,
    validation: ?Validation,
    scenarios: []const Scenario,
    arena: std.heap.ArenaAllocator,

    pub fn deinit(self: *Simulator) void {
        self.arena.deinit();
    }
};

pub const ParseError = error{
    MissingSimulator,
    MissingScenarioName,
    InvalidSimulator,
};

const ContextKind = enum { root, simulator, scenario, components, faults, snapshots, validation };

const Context = struct {
    kind: ContextKind,
    indent: usize,
    index: usize,
};

const ScenarioBuilder = struct {
    name: []const u8 = "",
    components: array_list(KeyValue),
    faults: array_list(KeyValue),
};

pub fn parse(allocator: std.mem.Allocator, path: []const u8) !Simulator {
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();
    const a = arena.allocator();

    const abs_path = try std.fs.cwd().realpathAlloc(a, path);
    const text = try std.fs.cwd().readFileAlloc(a, abs_path, 1_000_000);

    var scenarios = array_list(ScenarioBuilder).init(a);
    var validation_overrides = array_list(KeyValue).init(a);

    var seed: ?u64 = null;
    var concurrency: []const u8 = "";
    var snapshots_defined = false;
    var validation_defined = false;
    var snapshots = Snapshots{
        .steps = null,
        .mode = "",
        .compress = "",
        .retention = null,
    };
    var validation = Validation{
        .level = "",
        .overrides = &.{},
    };

    var stack = array_list(Context).init(a);
    try stack.append(.{ .kind = .root, .indent = 0, .index = 0 });

    var line_iter = std.mem.splitScalar(u8, text, '\n');
    var saw_simulator = false;
    while (line_iter.next()) |raw_line| {
        const line = std.mem.trimRight(u8, raw_line, "\r");
        const trimmed = std.mem.trimLeft(u8, line, " \t");
        if (trimmed.len == 0) continue;
        if (std.mem.startsWith(u8, trimmed, "//") or std.mem.startsWith(u8, trimmed, "#")) continue;

        const indent = count_indent(line);
        while (stack.items.len > 1 and indent <= stack.items[stack.items.len - 1].indent) {
            _ = stack.pop();
        }
        const ctx = stack.items[stack.items.len - 1];

        if (std.mem.eql(u8, trimmed, "sim::simulator")) {
            saw_simulator = true;
            try stack.append(.{ .kind = .simulator, .indent = indent, .index = 0 });
            continue;
        }

        if (parse_assignment(trimmed)) |assign| {
            if (std.mem.eql(u8, assign.value, "sim::scenario")) {
                var builder = ScenarioBuilder{
                    .components = array_list(KeyValue).init(a),
                    .faults = array_list(KeyValue).init(a),
                };
                try scenarios.append(builder);
                try stack.append(.{ .kind = .scenario, .indent = indent, .index = scenarios.items.len - 1 });
                continue;
            }
            if (std.mem.eql(u8, assign.value, "sim::components")) {
                try stack.append(.{ .kind = .components, .indent = indent, .index = ctx.index });
                continue;
            }
            if (std.mem.eql(u8, assign.value, "sim::faults")) {
                try stack.append(.{ .kind = .faults, .indent = indent, .index = ctx.index });
                continue;
            }
            if (std.mem.eql(u8, assign.value, "sim::snapshots")) {
                snapshots_defined = true;
                try stack.append(.{ .kind = .snapshots, .indent = indent, .index = 0 });
                continue;
            }
            if (std.mem.eql(u8, assign.value, "sim::validation")) {
                validation_defined = true;
                try stack.append(.{ .kind = .validation, .indent = indent, .index = 0 });
                continue;
            }

            switch (ctx.kind) {
                .simulator => {
                    if (std.mem.eql(u8, assign.key, "seed")) {
                        seed = parse_u64(assign.value) orelse return error.InvalidSimulator;
                    } else if (std.mem.eql(u8, assign.key, "concurrency")) {
                        concurrency = assign.value;
                    }
                },
                .scenario => {
                    if (ctx.index < scenarios.items.len and std.mem.eql(u8, assign.key, "name")) {
                        scenarios.items[ctx.index].name = assign.value;
                    }
                },
                .components => {
                    if (ctx.index < scenarios.items.len) {
                        try scenarios.items[ctx.index].components.append(.{ .key = assign.key, .value = assign.value });
                    }
                },
                .faults => {
                    if (ctx.index < scenarios.items.len) {
                        try scenarios.items[ctx.index].faults.append(.{ .key = assign.key, .value = assign.value });
                    }
                },
                .snapshots => {
                    if (std.mem.eql(u8, assign.key, "steps")) {
                        snapshots.steps = parse_u64(assign.value) orelse return error.InvalidSimulator;
                    } else if (std.mem.eql(u8, assign.key, "mode")) {
                        snapshots.mode = assign.value;
                    } else if (std.mem.eql(u8, assign.key, "compress")) {
                        snapshots.compress = assign.value;
                    } else if (std.mem.eql(u8, assign.key, "retention")) {
                        snapshots.retention = parse_u64(assign.value) orelse return error.InvalidSimulator;
                    }
                },
                .validation => {
                    if (std.mem.eql(u8, assign.key, "level")) {
                        validation.level = assign.value;
                    } else {
                        try validation_overrides.append(.{ .key = assign.key, .value = assign.value });
                    }
                },
                else => {},
            }
        }
    }

    if (!saw_simulator) return error.MissingSimulator;

    var scenario_list = array_list(Scenario).init(a);
    for (scenarios.items) |builder| {
        if (builder.name.len == 0) return error.MissingScenarioName;
        const components = try builder.components.toOwnedSlice();
        const faults = try builder.faults.toOwnedSlice();
        try scenario_list.append(.{
            .name = builder.name,
            .components = components,
            .faults = faults,
        });
    }

    if (validation_defined) {
        validation.overrides = try validation_overrides.toOwnedSlice();
    }

    return .{
        .seed = seed,
        .concurrency = concurrency,
        .snapshots = if (snapshots_defined) snapshots else null,
        .validation = if (validation_defined) validation else null,
        .scenarios = try scenario_list.toOwnedSlice(),
        .arena = arena,
    };
}

const Assignment = struct {
    key: []const u8,
    value: []const u8,
};

fn parse_assignment(line: []const u8) ?Assignment {
    const eq = std.mem.indexOfScalar(u8, line, '=') orelse return null;
    const key = std.mem.trim(u8, line[0..eq], " \t");
    const raw_val = std.mem.trim(u8, line[eq + 1 ..], " \t");
    if (key.len == 0 or raw_val.len == 0) return null;
    return .{ .key = key, .value = parse_value(raw_val) };
}

fn parse_value(raw_val: []const u8) []const u8 {
    if (raw_val.len >= 2 and raw_val[0] == '"' and raw_val[raw_val.len - 1] == '"') {
        return raw_val[1 .. raw_val.len - 1];
    }
    return raw_val;
}

fn parse_u64(raw_val: []const u8) ?u64 {
    return std.fmt.parseInt(u64, raw_val, 10) catch null;
}

fn count_indent(line: []const u8) usize {
    var count: usize = 0;
    while (count < line.len) : (count += 1) {
        const ch = line[count];
        if (ch != ' ' and ch != '\t') break;
    }
    return count;
}
