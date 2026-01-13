const std = @import("std");

const array_list = std.array_list.Managed;

pub const KeyValue = struct {
    key: []const u8,
    value: []const u8,
};

pub const FaultRule = struct {
    kind: []const u8,
    component: []const u8,
    op: []const u8,
    probability: ?f64,
    delay_ns: ?u64,
    jitter_ns: ?u64,
    partial_min: ?u64,
    partial_max: ?u64,
    timeout_ns: ?u64,
    err: []const u8,
    addr: []const u8,
    path: []const u8,
};

pub const Sweep = struct {
    mode: []const u8,
    seed: ?u64,
    samples: ?u64,
    entries: []const KeyValue,
};

pub const Scenario = struct {
    name: []const u8,
    compose: []const []const u8,
    components: []const KeyValue,
    faults: []const KeyValue,
    fault_rules: []const FaultRule,
    sweeps: []const Sweep,
};

pub const Snapshots = struct {
    steps: ?u64,
    mode: []const u8,
    compress: []const u8,
    retention: ?u64,
    level: []const u8,
};

pub const Validation = struct {
    level: []const u8,
    overrides: []const KeyValue,
};

pub const Foreigns = struct {
    allow: []const []const u8,
    deny: []const []const u8,
    allow_categories: []const []const u8,
    deny_categories: []const []const u8,
};

pub const Report = struct {
    text: []const u8,
    json: []const u8,
    tree: bool,
    per_scenario: bool,
    aggregate: bool,
};

pub const Simulator = struct {
    seed: ?u64,
    concurrency: []const u8,
    snapshots: ?Snapshots,
    validation: ?Validation,
    foreigns: ?Foreigns,
    report: ?Report,
    scenarios: []const Scenario,
    arena: std.heap.ArenaAllocator,

    pub fn deinit(self: *Simulator) void {
        self.arena.deinit();
    }
};

pub const ParseError = error{
    MissingSimulator,
    MissingScenarioName,
    MissingSimImport,
    InvalidSimulator,
};

const ContextKind = enum {
    root,
    simulator,
    scenario,
    components,
    faults,
    fault,
    sweep,
    snapshots,
    validation,
    foreigns,
    report,
};

const Context = struct {
    kind: ContextKind,
    indent: usize,
    scenario_index: usize,
    sub_index: usize,
};

const FaultRuleBuilder = struct {
    kind: []const u8 = "",
    component: []const u8 = "",
    op: []const u8 = "",
    probability: ?f64 = null,
    delay_ns: ?u64 = null,
    jitter_ns: ?u64 = null,
    partial_min: ?u64 = null,
    partial_max: ?u64 = null,
    timeout_ns: ?u64 = null,
    err: []const u8 = "",
    addr: []const u8 = "",
    path: []const u8 = "",
};

const SweepBuilder = struct {
    mode: []const u8 = "",
    seed: ?u64 = null,
    samples: ?u64 = null,
    entries: array_list(KeyValue),
};

const ScenarioBuilder = struct {
    name: []const u8 = "",
    compose: array_list([]const u8),
    components: array_list(KeyValue),
    faults: array_list(KeyValue),
    fault_rules: array_list(FaultRuleBuilder),
    sweeps: array_list(SweepBuilder),
};

pub fn parse(allocator: std.mem.Allocator, path: []const u8) !Simulator {
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();
    const a = arena.allocator();

    const abs_path = try std.fs.cwd().realpathAlloc(a, path);
    const text = try std.fs.cwd().readFileAlloc(a, abs_path, 1_000_000);

    var scenarios = array_list(ScenarioBuilder).init(a);
    var validation_overrides = array_list(KeyValue).init(a);
    var foreign_allow = array_list([]const u8).init(a);
    var foreign_deny = array_list([]const u8).init(a);
    var foreign_allow_categories = array_list([]const u8).init(a);
    var foreign_deny_categories = array_list([]const u8).init(a);

    var seed: ?u64 = null;
    var concurrency: []const u8 = "";
    var snapshots_defined = false;
    var validation_defined = false;
    var foreigns_defined = false;
    var report_defined = false;
    var snapshots = Snapshots{
        .steps = null,
        .mode = "",
        .compress = "",
        .retention = null,
        .level = "",
    };
    var validation = Validation{
        .level = "",
        .overrides = &.{},
    };
    var report = Report{
        .text = "",
        .json = "",
        .tree = true,
        .per_scenario = true,
        .aggregate = true,
    };

    var stack = array_list(Context).init(a);
    try stack.append(.{ .kind = .root, .indent = 0, .scenario_index = 0, .sub_index = 0 });

    var line_iter = std.mem.splitScalar(u8, text, '\n');
    var saw_sim_import = false;
    var saw_simulator_fn = false;
    var in_simulator_fn = false;
    var simulator_indent: usize = 0;
    var saw_simulator = false;
    while (line_iter.next()) |raw_line| {
        const line = std.mem.trimRight(u8, raw_line, "\r");
        const indent = count_indent(line);
        if (indent != 0 and std.mem.indexOfScalar(u8, line[0..indent], ' ') != null) {
            return error.InvalidSimulator;
        }
        const trimmed = std.mem.trimLeft(u8, line, " \t");
        if (trimmed.len == 0) continue;
        if (std.mem.startsWith(u8, trimmed, "//") or std.mem.startsWith(u8, trimmed, "#")) continue;
        if (std.mem.eql(u8, trimmed, "import sim")) {
            saw_sim_import = true;
        }
        if (indent == 0 and is_simulator_fn_line(trimmed)) {
            saw_simulator_fn = true;
            in_simulator_fn = true;
            simulator_indent = indent;
            if (stack.items.len > 1) stack.shrinkRetainingCapacity(1);
            continue;
        }
        if (in_simulator_fn and indent <= simulator_indent) {
            in_simulator_fn = false;
            if (stack.items.len > 1) stack.shrinkRetainingCapacity(1);
            continue;
        }
        if (!in_simulator_fn) continue;

        while (stack.items.len > 1 and indent <= stack.items[stack.items.len - 1].indent) {
            _ = stack.pop();
        }
        const ctx = stack.items[stack.items.len - 1];

        if (std.mem.eql(u8, trimmed, "sim::simulator")) {
            saw_simulator = true;
            try stack.append(.{ .kind = .simulator, .indent = indent, .scenario_index = 0, .sub_index = 0 });
            continue;
        }

        if (parse_assignment(trimmed)) |assign| {
            if (std.mem.eql(u8, assign.value, "sim::scenario")) {
                const builder = ScenarioBuilder{
                    .compose = array_list([]const u8).init(a),
                    .components = array_list(KeyValue).init(a),
                    .faults = array_list(KeyValue).init(a),
                    .fault_rules = array_list(FaultRuleBuilder).init(a),
                    .sweeps = array_list(SweepBuilder).init(a),
                };
                try scenarios.append(builder);
                try stack.append(.{
                    .kind = .scenario,
                    .indent = indent,
                    .scenario_index = scenarios.items.len - 1,
                    .sub_index = 0,
                });
                continue;
            }
            if (std.mem.eql(u8, assign.value, "sim::components")) {
                try stack.append(.{
                    .kind = .components,
                    .indent = indent,
                    .scenario_index = ctx.scenario_index,
                    .sub_index = 0,
                });
                continue;
            }
            if (std.mem.eql(u8, assign.value, "sim::faults")) {
                try stack.append(.{
                    .kind = .faults,
                    .indent = indent,
                    .scenario_index = ctx.scenario_index,
                    .sub_index = 0,
                });
                continue;
            }
            if (std.mem.eql(u8, assign.value, "sim::fault")) {
                if (ctx.scenario_index < scenarios.items.len) {
                    try scenarios.items[ctx.scenario_index].fault_rules.append(.{});
                    const index = scenarios.items[ctx.scenario_index].fault_rules.items.len - 1;
                    try stack.append(.{
                        .kind = .fault,
                        .indent = indent,
                        .scenario_index = ctx.scenario_index,
                        .sub_index = index,
                    });
                }
                continue;
            }
            if (std.mem.eql(u8, assign.value, "sim::sweep")) {
                if (ctx.scenario_index < scenarios.items.len) {
                    const sweep_builder = SweepBuilder{
                        .entries = array_list(KeyValue).init(a),
                    };
                    try scenarios.items[ctx.scenario_index].sweeps.append(sweep_builder);
                    const index = scenarios.items[ctx.scenario_index].sweeps.items.len - 1;
                    try stack.append(.{
                        .kind = .sweep,
                        .indent = indent,
                        .scenario_index = ctx.scenario_index,
                        .sub_index = index,
                    });
                }
                continue;
            }
            if (std.mem.eql(u8, assign.value, "sim::snapshots")) {
                snapshots_defined = true;
                try stack.append(.{
                    .kind = .snapshots,
                    .indent = indent,
                    .scenario_index = 0,
                    .sub_index = 0,
                });
                continue;
            }
            if (std.mem.eql(u8, assign.value, "sim::validation")) {
                validation_defined = true;
                try stack.append(.{
                    .kind = .validation,
                    .indent = indent,
                    .scenario_index = 0,
                    .sub_index = 0,
                });
                continue;
            }
            if (std.mem.eql(u8, assign.value, "sim::foreigns")) {
                foreigns_defined = true;
                try stack.append(.{
                    .kind = .foreigns,
                    .indent = indent,
                    .scenario_index = 0,
                    .sub_index = 0,
                });
                continue;
            }
            if (std.mem.eql(u8, assign.value, "sim::report")) {
                report_defined = true;
                try stack.append(.{
                    .kind = .report,
                    .indent = indent,
                    .scenario_index = 0,
                    .sub_index = 0,
                });
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
                    if (ctx.scenario_index < scenarios.items.len and std.mem.eql(u8, assign.key, "name")) {
                        scenarios.items[ctx.scenario_index].name = assign.value;
                    } else if (ctx.scenario_index < scenarios.items.len and std.mem.eql(u8, assign.key, "compose")) {
                        try append_list(&scenarios.items[ctx.scenario_index].compose, assign.value);
                    }
                },
                .components => {
                    if (ctx.scenario_index < scenarios.items.len) {
                        try scenarios.items[ctx.scenario_index].components.append(.{ .key = assign.key, .value = assign.value });
                    }
                },
                .faults => {
                    if (ctx.scenario_index < scenarios.items.len) {
                        try scenarios.items[ctx.scenario_index].faults.append(.{ .key = assign.key, .value = assign.value });
                    }
                },
                .fault => {
                    if (ctx.scenario_index < scenarios.items.len) {
                        const rules = &scenarios.items[ctx.scenario_index].fault_rules;
                        if (ctx.sub_index < rules.items.len) {
                            var rule = &rules.items[ctx.sub_index];
                            if (std.mem.eql(u8, assign.key, "kind")) {
                                rule.kind = assign.value;
                            } else if (std.mem.eql(u8, assign.key, "component")) {
                                rule.component = assign.value;
                            } else if (std.mem.eql(u8, assign.key, "op")) {
                                rule.op = assign.value;
                            } else if (std.mem.eql(u8, assign.key, "probability") or std.mem.eql(u8, assign.key, "p")) {
                                rule.probability = parse_f64(assign.value) orelse return error.InvalidSimulator;
                            } else if (std.mem.eql(u8, assign.key, "delay_ns")) {
                                rule.delay_ns = parse_u64(assign.value) orelse return error.InvalidSimulator;
                            } else if (std.mem.eql(u8, assign.key, "jitter_ns")) {
                                rule.jitter_ns = parse_u64(assign.value) orelse return error.InvalidSimulator;
                            } else if (std.mem.eql(u8, assign.key, "partial_min")) {
                                rule.partial_min = parse_u64(assign.value) orelse return error.InvalidSimulator;
                            } else if (std.mem.eql(u8, assign.key, "partial_max")) {
                                rule.partial_max = parse_u64(assign.value) orelse return error.InvalidSimulator;
                            } else if (std.mem.eql(u8, assign.key, "timeout_ns")) {
                                rule.timeout_ns = parse_u64(assign.value) orelse return error.InvalidSimulator;
                            } else if (std.mem.eql(u8, assign.key, "error")) {
                                rule.err = assign.value;
                            } else if (std.mem.eql(u8, assign.key, "addr")) {
                                rule.addr = assign.value;
                            } else if (std.mem.eql(u8, assign.key, "path")) {
                                rule.path = assign.value;
                            } else {
                                return error.InvalidSimulator;
                            }
                        }
                    }
                },
                .sweep => {
                    if (ctx.scenario_index < scenarios.items.len) {
                        const sweeps = &scenarios.items[ctx.scenario_index].sweeps;
                        if (ctx.sub_index < sweeps.items.len) {
                            var sweep = &sweeps.items[ctx.sub_index];
                            if (std.mem.eql(u8, assign.key, "mode")) {
                                sweep.mode = assign.value;
                            } else if (std.mem.eql(u8, assign.key, "seed")) {
                                sweep.seed = parse_u64(assign.value) orelse return error.InvalidSimulator;
                            } else if (std.mem.eql(u8, assign.key, "samples")) {
                                sweep.samples = parse_u64(assign.value) orelse return error.InvalidSimulator;
                            } else {
                                try sweep.entries.append(.{ .key = assign.key, .value = assign.value });
                            }
                        }
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
                    } else if (std.mem.eql(u8, assign.key, "level")) {
                        snapshots.level = assign.value;
                    }
                },
                .validation => {
                    if (std.mem.eql(u8, assign.key, "level")) {
                        validation.level = assign.value;
                    } else {
                        try validation_overrides.append(.{ .key = assign.key, .value = assign.value });
                    }
                },
                .foreigns => {
                    if (std.mem.eql(u8, assign.key, "allow")) {
                        try append_list(&foreign_allow, assign.value);
                    } else if (std.mem.eql(u8, assign.key, "deny")) {
                        try append_list(&foreign_deny, assign.value);
                    } else if (std.mem.eql(u8, assign.key, "allow_categories") or std.mem.eql(u8, assign.key, "allow_category")) {
                        try append_list(&foreign_allow_categories, assign.value);
                    } else if (std.mem.eql(u8, assign.key, "deny_categories") or std.mem.eql(u8, assign.key, "deny_category")) {
                        try append_list(&foreign_deny_categories, assign.value);
                    } else {
                        return error.InvalidSimulator;
                    }
                },
                .report => {
                    if (std.mem.eql(u8, assign.key, "text")) {
                        report.text = assign.value;
                    } else if (std.mem.eql(u8, assign.key, "json")) {
                        report.json = assign.value;
                    } else if (std.mem.eql(u8, assign.key, "tree")) {
                        report.tree = parse_bool(assign.value) orelse return error.InvalidSimulator;
                    } else if (std.mem.eql(u8, assign.key, "per_scenario")) {
                        report.per_scenario = parse_bool(assign.value) orelse return error.InvalidSimulator;
                    } else if (std.mem.eql(u8, assign.key, "aggregate")) {
                        report.aggregate = parse_bool(assign.value) orelse return error.InvalidSimulator;
                    } else {
                        return error.InvalidSimulator;
                    }
                },
                else => {},
            }
        }
    }

    if (!saw_simulator_fn or !saw_simulator) return error.MissingSimulator;
    if (!saw_sim_import) return error.MissingSimImport;

    var scenario_list = array_list(Scenario).init(a);
    for (scenarios.items) |*builder| {
        if (builder.name.len == 0) return error.MissingScenarioName;
        const compose = try builder.compose.toOwnedSlice();
        const components = try builder.components.toOwnedSlice();
        const faults = try builder.faults.toOwnedSlice();
        var fault_rules = array_list(FaultRule).init(a);
        for (builder.fault_rules.items) |rule| {
            if (rule.kind.len == 0) return error.InvalidSimulator;
            try fault_rules.append(.{
                .kind = rule.kind,
                .component = rule.component,
                .op = rule.op,
                .probability = rule.probability,
                .delay_ns = rule.delay_ns,
                .jitter_ns = rule.jitter_ns,
                .partial_min = rule.partial_min,
                .partial_max = rule.partial_max,
                .timeout_ns = rule.timeout_ns,
                .err = rule.err,
                .addr = rule.addr,
                .path = rule.path,
            });
        }
        var sweeps = array_list(Sweep).init(a);
        for (builder.sweeps.items) |*sweep| {
            const entries = try sweep.entries.toOwnedSlice();
            try sweeps.append(.{
                .mode = sweep.mode,
                .seed = sweep.seed,
                .samples = sweep.samples,
                .entries = entries,
            });
        }
        try scenario_list.append(.{
            .name = builder.name,
            .compose = compose,
            .components = components,
            .faults = faults,
            .fault_rules = try fault_rules.toOwnedSlice(),
            .sweeps = try sweeps.toOwnedSlice(),
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
        .foreigns = if (foreigns_defined) .{
            .allow = try foreign_allow.toOwnedSlice(),
            .deny = try foreign_deny.toOwnedSlice(),
            .allow_categories = try foreign_allow_categories.toOwnedSlice(),
            .deny_categories = try foreign_deny_categories.toOwnedSlice(),
        } else null,
        .report = if (report_defined) report else null,
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

fn parse_f64(raw_val: []const u8) ?f64 {
    return std.fmt.parseFloat(f64, raw_val) catch null;
}

fn parse_bool(raw_val: []const u8) ?bool {
    if (std.mem.eql(u8, raw_val, "true") or std.mem.eql(u8, raw_val, "1")) return true;
    if (std.mem.eql(u8, raw_val, "false") or std.mem.eql(u8, raw_val, "0")) return false;
    return null;
}

fn count_indent(line: []const u8) usize {
    var count: usize = 0;
    while (count < line.len) : (count += 1) {
        const ch = line[count];
        if (ch != ' ' and ch != '\t') break;
    }
    return count;
}

fn append_list(list: *array_list([]const u8), raw_val: []const u8) !void {
    var iter = std.mem.tokenizeAny(u8, raw_val, " ,\t");
    while (iter.next()) |token| {
        if (token.len == 0) continue;
        try list.append(token);
    }
}

fn is_simulator_fn_line(trimmed: []const u8) bool {
    if (!std.mem.startsWith(u8, trimmed, "fn ")) return false;
    const rest = trimmed["fn ".len..];
    if (!std.mem.startsWith(u8, rest, "simulator")) return false;
    const tail = rest["simulator".len..];
    if (tail.len == 0) return true;
    const ch = tail[0];
    return ch == '(' or ch == ' ' or ch == '\t' or ch == '\r';
}
