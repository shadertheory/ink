const std = @import("std");
const sim = @import("sim.zig");
const async_sim = @import("runtime/async_sim.zig");
const fd = @import("runtime/fd.zig");

const arena_allocator = std.heap.ArenaAllocator;
const array_list = std.array_list.Managed;

pub const Run = struct {
    name: []const u8,
    scenario: []const u8,
    seed: u64,
    config: async_sim.Config,
    params: []const sim.KeyValue,
};

pub const Runs = struct {
    runs: []Run,
    arena: arena_allocator,

    pub fn deinit(self: *Runs) void {
        self.arena.deinit();
    }
};

pub const BuildOptions = struct {
    scenario_names: ?[]const []const u8 = null,
    base_seed: ?u64 = null,
    sample_override: ?u64 = null,
    capture_report: bool = false,
};

const ScenarioConfig = struct {
    name: []const u8,
    components: []const sim.KeyValue,
    faults: []const sim.KeyValue,
    fault_rules: []const sim.FaultRule,
    sweeps: []const sim.Sweep,
};

const SweepEntry = struct {
    key: []const u8,
    values: []const []const u8,
};

pub fn find_scenario(config: sim.Simulator, name: []const u8) ?sim.Scenario {
    for (config.scenarios) |scenario| {
        if (std.mem.eql(u8, scenario.name, name)) return scenario;
    }
    return null;
}

pub fn build_runs(
    allocator: std.mem.Allocator,
    root_dir: []const u8,
    config: sim.Simulator,
    options: BuildOptions,
) !Runs {
    var arena = arena_allocator.init(allocator);
    errdefer arena.deinit();
    const a = arena.allocator();

    if (options.scenario_names) |names| {
        for (names) |name| {
            if (std.mem.eql(u8, name, "all")) continue;
            if (find_scenario(config, name) == null) return error.UnknownScenario;
        }
    }

    const base_seed = options.base_seed orelse config.seed orelse random_seed();
    var runs = array_list(Run).init(a);
    var run_id: u64 = 0;

    for (config.scenarios) |scenario| {
        if (!scenario_selected(options.scenario_names, scenario.name)) continue;

        var stack = array_list([]const u8).init(a);
        const composed = try compose_scenario(a, config, scenario, &stack);

        const capture_report = options.capture_report or config.report != null;
        var scenario_run_index: usize = 0;

        if (composed.sweeps.len == 0) {
            const overrides: []const sim.KeyValue = &.{};
            try append_run(
                a,
                root_dir,
                config,
                composed,
                overrides,
                null,
                base_seed,
                0,
                scenario_run_index,
                capture_report,
                &runs,
                &run_id,
            );
            scenario_run_index += 1;
            continue;
        }

        for (composed.sweeps, 0..) |sweep, sweep_index| {
            const entries = try parse_sweep_entries(a, sweep.entries);
            const mode = if (sweep.mode.len == 0) "grid" else sweep.mode;
            const use_samples = is_sample_mode(mode);
            const samples = options.sample_override orelse sweep.samples orelse 16;
            const sweep_seed = sweep.seed orelse derive_seed(base_seed, composed.name, sweep_index, 0);
            const override_sets = if (use_samples)
                try build_sample_overrides(a, entries, samples, sweep_seed)
            else
                try build_grid_overrides(a, entries);

            for (override_sets) |overrides| {
                try append_run(
                    a,
                    root_dir,
                    config,
                    composed,
                    overrides,
                    null,
                    base_seed,
                    sweep_index,
                    scenario_run_index,
                    capture_report,
                    &runs,
                    &run_id,
                );
                scenario_run_index += 1;
            }
        }
    }

    return .{ .runs = try runs.toOwnedSlice(), .arena = arena };
}

fn scenario_selected(filter: ?[]const []const u8, name: []const u8) bool {
    if (filter == null) return true;
    for (filter.?) |item| {
        if (std.mem.eql(u8, item, "all")) return true;
        if (std.mem.eql(u8, item, name)) return true;
    }
    return false;
}

fn compose_scenario(
    allocator: std.mem.Allocator,
    config: sim.Simulator,
    scenario: sim.Scenario,
    stack: *array_list([]const u8),
) !ScenarioConfig {
    for (stack.items) |name| {
        if (std.mem.eql(u8, name, scenario.name)) return error.ScenarioCycle;
    }
    try stack.append(scenario.name);

    var components = array_list(sim.KeyValue).init(allocator);
    var faults = array_list(sim.KeyValue).init(allocator);
    var fault_rules = array_list(sim.FaultRule).init(allocator);
    var sweeps = array_list(sim.Sweep).init(allocator);

    for (scenario.compose) |base_name| {
        const base = find_scenario(config, base_name) orelse return error.UnknownScenario;
        const base_cfg = try compose_scenario(allocator, config, base, stack);
        try merge_key_values_into(&components, base_cfg.components);
        try merge_key_values_into(&faults, base_cfg.faults);
        try fault_rules.appendSlice(base_cfg.fault_rules);
        try sweeps.appendSlice(base_cfg.sweeps);
    }

    try merge_key_values_into(&components, scenario.components);
    try merge_key_values_into(&faults, scenario.faults);
    try fault_rules.appendSlice(scenario.fault_rules);
    try sweeps.appendSlice(scenario.sweeps);

    _ = stack.pop();

    return .{
        .name = scenario.name,
        .components = try components.toOwnedSlice(),
        .faults = try faults.toOwnedSlice(),
        .fault_rules = try fault_rules.toOwnedSlice(),
        .sweeps = try sweeps.toOwnedSlice(),
    };
}

fn merge_key_values_into(list: *array_list(sim.KeyValue), overlay: []const sim.KeyValue) !void {
    for (overlay) |item| {
        var replaced = false;
        for (list.items) |*existing| {
            if (std.mem.eql(u8, existing.key, item.key)) {
                existing.value = item.value;
                replaced = true;
                break;
            }
        }
        if (!replaced) try list.append(item);
    }
}

fn parse_sweep_entries(allocator: std.mem.Allocator, entries: []const sim.KeyValue) ![]const SweepEntry {
    var list = array_list(SweepEntry).init(allocator);
    for (entries) |entry| {
        var values = array_list([]const u8).init(allocator);
        var iter = std.mem.tokenizeAny(u8, entry.value, " ,\t");
        while (iter.next()) |token| {
            if (token.len == 0) continue;
            try values.append(token);
        }
        if (values.items.len == 0) return error.InvalidSweep;
        try list.append(.{ .key = entry.key, .values = try values.toOwnedSlice() });
    }
    return try list.toOwnedSlice();
}

fn build_grid_overrides(allocator: std.mem.Allocator, entries: []const SweepEntry) ![]const []const sim.KeyValue {
    var output = array_list([]const sim.KeyValue).init(allocator);
    var current = array_list(sim.KeyValue).init(allocator);
    try expand_grid_overrides(allocator, entries, 0, &current, &output);
    return try output.toOwnedSlice();
}

fn expand_grid_overrides(
    allocator: std.mem.Allocator,
    entries: []const SweepEntry,
    index: usize,
    current: *array_list(sim.KeyValue),
    output: *array_list([]const sim.KeyValue),
) !void {
    if (index >= entries.len) {
        const slice = try allocator.alloc(sim.KeyValue, current.items.len);
        std.mem.copyForwards(sim.KeyValue, slice, current.items);
        try output.append(slice);
        return;
    }

    const entry = entries[index];
    for (entry.values) |value| {
        try current.append(.{ .key = entry.key, .value = value });
        try expand_grid_overrides(allocator, entries, index + 1, current, output);
        _ = current.pop();
    }
}

fn build_sample_overrides(
    allocator: std.mem.Allocator,
    entries: []const SweepEntry,
    samples: u64,
    seed: u64,
) ![]const []const sim.KeyValue {
    var output = array_list([]const sim.KeyValue).init(allocator);
    var rng = std.Random.DefaultPrng.init(seed);
    var idx: u64 = 0;
    while (idx < samples) : (idx += 1) {
        const slice = try allocator.alloc(sim.KeyValue, entries.len);
        for (entries, 0..) |entry, entry_idx| {
            const pick = rng.random().uintLessThan(usize, entry.values.len);
            slice[entry_idx] = .{ .key = entry.key, .value = entry.values[pick] };
        }
        try output.append(slice);
    }
    return try output.toOwnedSlice();
}

fn append_run(
    allocator: std.mem.Allocator,
    root_dir: []const u8,
    config: sim.Simulator,
    scenario: ScenarioConfig,
    overrides: []const sim.KeyValue,
    seed_override: ?u64,
    base_seed: u64,
    sweep_index: usize,
    run_index: usize,
    capture_report: bool,
    runs: *array_list(Run),
    run_id: *u64,
) !void {
    const applied = try apply_overrides(allocator, scenario.components, scenario.faults, overrides);
    const run_seed = seed_override orelse applied.seed_override orelse derive_seed(base_seed, scenario.name, sweep_index, run_index);
    const run_name = if (scenario.sweeps.len == 0)
        try std.fmt.allocPrint(allocator, "{s}", .{scenario.name})
    else
        try std.fmt.allocPrint(allocator, "{s}-s{d}-r{d}", .{ scenario.name, sweep_index, run_index });

    const components = build_components(applied.components);
    const faults = try build_faults(allocator, applied.faults, scenario.fault_rules);

    var snapshots: ?async_sim.Snapshots = null;
    if (config.snapshots) |snap| {
        snapshots = .{
            .steps = snap.steps,
            .mode = snap.mode,
            .compress = snap.compress,
            .retention = snap.retention,
            .level = snap.level,
        };
    }

    const snapshot_dir = try std.fs.path.join(allocator, &.{ root_dir, ".ink", "snapshots" });

    try runs.append(.{
        .name = run_name,
        .scenario = scenario.name,
        .seed = run_seed,
        .params = overrides,
        .config = .{
            .seed = run_seed,
            .scenario_name = scenario.name,
            .run_name = run_name,
            .snapshot_dir = snapshot_dir,
            .components = components,
            .faults = faults,
            .snapshots = snapshots,
            .capture_report = capture_report,
        },
    });
    run_id.* += 1;
}

const AppliedOverrides = struct {
    components: []const sim.KeyValue,
    faults: []const sim.KeyValue,
    seed_override: ?u64,
};

fn apply_overrides(
    allocator: std.mem.Allocator,
    base_components: []const sim.KeyValue,
    base_faults: []const sim.KeyValue,
    overrides: []const sim.KeyValue,
) !AppliedOverrides {
    var components = array_list(sim.KeyValue).init(allocator);
    var faults = array_list(sim.KeyValue).init(allocator);
    try components.appendSlice(base_components);
    try faults.appendSlice(base_faults);

    var seed_override: ?u64 = null;
    for (overrides) |item| {
        if (std.mem.startsWith(u8, item.key, "components.")) {
            const key = item.key["components.".len..];
            try upsert_key_value(&components, key, item.value);
        } else if (std.mem.startsWith(u8, item.key, "faults.")) {
            const key = item.key["faults.".len..];
            try upsert_key_value(&faults, key, item.value);
        } else if (std.mem.eql(u8, item.key, "seed")) {
            seed_override = parse_u64(item.value) orelse return error.InvalidSweep;
        }
    }

    return .{
        .components = try components.toOwnedSlice(),
        .faults = try faults.toOwnedSlice(),
        .seed_override = seed_override,
    };
}

fn upsert_key_value(list: *array_list(sim.KeyValue), key: []const u8, value: []const u8) !void {
    for (list.items) |*existing| {
        if (std.mem.eql(u8, existing.key, key)) {
            existing.value = value;
            return;
        }
    }
    try list.append(.{ .key = key, .value = value });
}

fn build_components(entries: []const sim.KeyValue) async_sim.Components {
    var components = async_sim.Components{};
    for (entries) |item| {
        const mode = parse_mode(item.value);
        if (std.mem.eql(u8, item.key, "tcp")) components.tcp = mode;
        if (std.mem.eql(u8, item.key, "udp")) components.udp = mode;
        if (std.mem.eql(u8, item.key, "fs")) components.fs = mode;
        if (std.mem.eql(u8, item.key, "clock")) components.clock = mode;
        if (std.mem.eql(u8, item.key, "rng")) components.rng = mode;
        if (std.mem.eql(u8, item.key, "alloc")) components.alloc = mode;
        if (std.mem.eql(u8, item.key, "scheduler")) components.scheduler = mode;
    }
    return components;
}

fn build_faults(
    allocator: std.mem.Allocator,
    entries: []const sim.KeyValue,
    explicit_rules: []const sim.FaultRule,
) !async_sim.Faults {
    var rules = array_list(async_sim.FaultRule).init(allocator);
    var oom: f64 = 0.0;

    for (entries) |item| {
        if (std.mem.eql(u8, item.key, "oom")) {
            oom = parse_f64(item.value) orelse return error.InvalidSimulator;
            continue;
        }
        const prob = parse_f64(item.value) orelse return error.InvalidSimulator;
        if (parse_fault_shorthand(item.key, prob)) |rule| {
            try rules.append(rule);
        }
    }

    for (explicit_rules) |rule| {
        const parsed = parse_explicit_rule(rule) orelse continue;
        try rules.append(parsed);
    }

    return .{ .rules = try rules.toOwnedSlice(), .oom = oom };
}

fn parse_fault_shorthand(key: []const u8, prob: f64) ?async_sim.FaultRule {
    const parsed = parse_fault_key(key) orelse return null;
    return .{
        .kind = parsed.kind,
        .component = parsed.component,
        .probability = prob,
    };
}

const FaultKey = struct {
    kind: async_sim.FaultKind,
    component: ?fd.fd_kind,
};

fn parse_fault_key(key: []const u8) ?FaultKey {
    if (std.mem.eql(u8, key, "io_error")) return .{ .kind = .@"error", .component = null };
    if (std.mem.eql(u8, key, "fs_error")) return .{ .kind = .@"error", .component = .fs };
    if (std.mem.eql(u8, key, "drop")) return .{ .kind = .drop, .component = null };
    if (std.mem.eql(u8, key, "delay")) return .{ .kind = .delay, .component = null };
    if (std.mem.eql(u8, key, "reorder")) return .{ .kind = .reorder, .component = null };
    if (std.mem.eql(u8, key, "corrupt")) return .{ .kind = .corrupt, .component = null };
    if (std.mem.eql(u8, key, "partial")) return .{ .kind = .partial, .component = null };
    if (std.mem.eql(u8, key, "disconnect")) return .{ .kind = .disconnect, .component = null };
    if (std.mem.eql(u8, key, "timeout")) return .{ .kind = .timeout, .component = null };

    const underscore = std.mem.indexOfScalar(u8, key, '_') orelse return null;
    const comp_str = key[0..underscore];
    const kind_str = key[underscore + 1 ..];
    const component = parse_component(comp_str) orelse return null;
    const kind = parse_fault_kind(kind_str) orelse return null;
    return .{ .kind = kind, .component = component };
}

fn parse_explicit_rule(rule: sim.FaultRule) ?async_sim.FaultRule {
    const kind = parse_fault_kind(rule.kind) orelse return null;
    const component = if (rule.component.len == 0) null else (parse_component(rule.component) orelse return null);
    const op = if (rule.op.len == 0) null else (parse_op(rule.op) orelse return null);
    const probability = rule.probability orelse 1.0;
    return .{
        .kind = kind,
        .component = component,
        .op = op,
        .probability = probability,
        .delay_ns = rule.delay_ns orelse 0,
        .jitter_ns = rule.jitter_ns orelse 0,
        .partial_min = rule.partial_min orelse 0,
        .partial_max = rule.partial_max orelse 0,
        .timeout_ns = rule.timeout_ns orelse 0,
        .addr = rule.addr,
        .path = rule.path,
    };
}

fn parse_fault_kind(value: []const u8) ?async_sim.FaultKind {
    if (std.mem.eql(u8, value, "error")) return .@"error";
    if (std.mem.eql(u8, value, "drop")) return .drop;
    if (std.mem.eql(u8, value, "delay")) return .delay;
    if (std.mem.eql(u8, value, "reorder")) return .reorder;
    if (std.mem.eql(u8, value, "corrupt")) return .corrupt;
    if (std.mem.eql(u8, value, "partial")) return .partial;
    if (std.mem.eql(u8, value, "disconnect")) return .disconnect;
    if (std.mem.eql(u8, value, "timeout")) return .timeout;
    if (std.mem.eql(u8, value, "io_error")) return .@"error";
    return null;
}

fn parse_component(value: []const u8) ?fd.fd_kind {
    if (std.mem.eql(u8, value, "tcp")) return .tcp;
    if (std.mem.eql(u8, value, "udp")) return .udp;
    if (std.mem.eql(u8, value, "fs")) return .fs;
    return null;
}

fn parse_op(value: []const u8) ?async_sim.op_kind {
    if (std.mem.eql(u8, value, "read")) return .read;
    if (std.mem.eql(u8, value, "write")) return .write;
    if (std.mem.eql(u8, value, "accept")) return .accept;
    if (std.mem.eql(u8, value, "timer")) return .timer;
    return null;
}

fn parse_mode(value: []const u8) async_sim.ComponentMode {
    if (std.mem.eql(u8, value, "real")) return .real;
    if (std.mem.eql(u8, value, "mock")) return .mock;
    if (std.mem.eql(u8, value, "sim")) return .sim;
    return .mock;
}

fn parse_f64(value: []const u8) ?f64 {
    return std.fmt.parseFloat(f64, value) catch null;
}

fn parse_u64(value: []const u8) ?u64 {
    return std.fmt.parseInt(u64, value, 10) catch null;
}

fn is_sample_mode(value: []const u8) bool {
    return std.mem.eql(u8, value, "sample") or std.mem.eql(u8, value, "samples") or std.mem.eql(u8, value, "random");
}

fn derive_seed(base: u64, scenario: []const u8, sweep_index: usize, run_index: usize) u64 {
    var hasher = std.hash.Wyhash.init(base);
    hasher.update(scenario);
    hasher.update(std.mem.asBytes(&sweep_index));
    hasher.update(std.mem.asBytes(&run_index));
    return hasher.final();
}

fn random_seed() u64 {
    var buf: [8]u8 = undefined;
    std.crypto.random.bytes(&buf);
    return std.mem.readInt(u64, &buf, .little);
}
