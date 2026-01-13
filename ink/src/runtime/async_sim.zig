const std = @import("std");
const builtin = @import("builtin");
const common = @import("async_common.zig");
const fd = @import("fd.zig");
const trace = @import("trace.zig");

const real_backend = switch (builtin.os.tag) {
    .macos, .ios, .tvos, .watchos, .visionos => @import("async_kqueue.zig"),
    .linux => @import("async_uring.zig"),
    else => @import("async_stub.zig"),
};

pub const op_kind = common.op_kind;
pub const completion = common.completion;

pub const ComponentMode = enum {
    real,
    mock,
    sim,
};

pub const Components = struct {
    fs: ComponentMode = .real,
    tcp: ComponentMode = .mock,
    udp: ComponentMode = .mock,
    clock: ComponentMode = .sim,
    rng: ComponentMode = .sim,
    alloc: ComponentMode = .sim,
    scheduler: ComponentMode = .sim,
};

pub const FaultKind = enum(u8) {
    none,
    @"error",
    drop,
    delay,
    reorder,
    corrupt,
    partial,
    disconnect,
    timeout,
};

const fault_kind_count: usize = @typeInfo(FaultKind).@"enum".fields.len;

pub const FaultRule = struct {
    kind: FaultKind = .none,
    component: ?fd.fd_kind = null,
    op: ?op_kind = null,
    probability: f64 = 0.0,
    delay_ns: u64 = 0,
    jitter_ns: u64 = 0,
    partial_min: u64 = 0,
    partial_max: u64 = 0,
    timeout_ns: u64 = 0,
    addr: []const u8 = "",
    path: []const u8 = "",
};

pub const Faults = struct {
    rules: []const FaultRule = &.{},
    oom: f64 = 0.0,
};

pub const Snapshots = struct {
    steps: ?u64 = null,
    mode: []const u8 = "",
    compress: []const u8 = "",
    retention: ?u64 = null,
    level: []const u8 = "",
};

const SnapshotKind = enum(u8) {
    full = 0,
    delta = 1,
};

const SnapshotInfo = struct {
    path: []const u8,
    kind: SnapshotKind,
};

pub const Config = struct {
    seed: u64 = 0,
    scenario_name: []const u8 = "",
    run_name: []const u8 = "",
    snapshot_dir: ?[]const u8 = null,
    replay_path: ?[]const u8 = null,
    components: Components = .{},
    faults: Faults = .{},
    snapshots: ?Snapshots = null,
    capture_report: bool = false,
};

pub const Report = struct {
    scenario_name: []const u8,
    run_name: []const u8,
    seed: u64,
    event_count: u64,
    fault_counts: []const u64,
    fault_sequence: []const FaultKind,
    snapshot_paths: []const []const u8,
    events: []const ReportEvent,
    program_tree: ?trace.ProgramTree = null,

    pub fn deinit(self: *Report, allocator: std.mem.Allocator) void {
        allocator.free(self.fault_counts);
        allocator.free(self.fault_sequence);
        for (self.snapshot_paths) |path| allocator.free(path);
        allocator.free(self.snapshot_paths);
        allocator.free(self.events);
        if (self.program_tree) |*tree| {
            tree.deinit(allocator);
            self.program_tree = null;
        }
    }
};

pub const ReportEvent = struct {
    time_ns: u64,
    op_id: u32,
    kind: op_kind,
    result: i64,
    err_code: u32,
    user_data: u64,
    fault_kind: FaultKind,
};

const SimError = error{
    Injected,
    Dropped,
    Oom,
    Timeout,
    Disconnect,
};

const Pending = struct {
    kind: op_kind,
    user_data: u64,
    fd_kind: fd.fd_kind,
    mode: ComponentMode,
    real_id: ?u32,
    buf_ptr: ?[*]u8,
    buf_len: u32,
};

const Event = struct {
    at_ns: u64,
    completion: completion,
    fault_kind: FaultKind,
};

const EventLogEntry = struct {
    time_ns: u64,
    op_id: u32,
    kind: op_kind,
    result: i64,
    err_code: u32,
    user_data: u64,
    fault_kind: FaultKind,
    data: []const u8,
};

const SubmitLogEntry = struct {
    op_id: u32,
    kind: op_kind,
    user_data: u64,
    fd_kind: fd.fd_kind,
    buf_len: u32,
    err_code: u32,
};

const ReplayLog = struct {
    submits: []SubmitLogEntry,
    events: []EventLogEntry,
};

const FaultDecision = struct {
    kind: FaultKind = .none,
    delay_ns: u64 = 0,
    result: ?i64 = null,
    err: ?anyerror = null,
    corrupt: bool = false,
};

pub const reactor = struct {
    allocator: std.mem.Allocator,
    config: Config,
    now: u64,
    next_id: u32,
    ready: std.ArrayListUnmanaged(completion),
    timeline: std.ArrayListUnmanaged(Event),
    pending: std.AutoHashMap(u32, Pending),
    real: ?real_backend.reactor,
    real_to_sim: std.AutoHashMap(u32, u32),
    rng: std.Random.DefaultPrng,
    name_rng: std.Random.DefaultPrng,
    mock_fd: u64,
    event_log: std.ArrayListUnmanaged(EventLogEntry),
    submit_log: std.ArrayListUnmanaged(SubmitLogEntry),
    fault_log: std.ArrayListUnmanaged(FaultKind),
    fault_counts: [fault_kind_count]u64,
    snapshot_paths: std.ArrayListUnmanaged([]const u8),
    snapshot_meta: std.ArrayListUnmanaged(SnapshotInfo),
    snapshot_index: u64,
    last_snapshot_submit: u64,
    last_snapshot_event: u64,
    event_count: u64,
    replay_log: ?ReplayLog,
    replay_index: usize,
    replay_submit_index: usize,

    pub fn init(allocator: std.mem.Allocator, config: Config) !reactor {
        const replay_log = if (config.replay_path) |path| try read_snapshot(allocator, path) else null;
        const rng_seed = select_rng_seed(config);
        const rng = std.Random.DefaultPrng.init(rng_seed);
        const name_rng = std.Random.DefaultPrng.init(rng_seed ^ 0x9e3779b97f4a7c15);
        const start_now = if (replay_log == null and config.components.clock == .real)
            monotonic_now_ns()
        else
            0;
        return .{
            .allocator = allocator,
            .config = config,
            .now = start_now,
            .next_id = 1,
            .ready = .{},
            .timeline = .{},
            .pending = std.AutoHashMap(u32, Pending).init(allocator),
            .real = if (replay_log == null and needs_real(config.components)) try real_backend.reactor.init(allocator) else null,
            .real_to_sim = std.AutoHashMap(u32, u32).init(allocator),
            .rng = rng,
            .name_rng = name_rng,
            .mock_fd = 1_000_000,
            .event_log = .{},
            .submit_log = .{},
            .fault_log = .{},
            .fault_counts = [_]u64{0} ** fault_kind_count,
            .snapshot_paths = .{},
            .snapshot_meta = .{},
            .snapshot_index = 0,
            .last_snapshot_submit = 0,
            .last_snapshot_event = 0,
            .event_count = 0,
            .replay_log = replay_log,
            .replay_index = 0,
            .replay_submit_index = 0,
        };
    }

    pub fn deinit(self: *reactor) void {
        self.flush_snapshot() catch {};
        if (self.real) |*real_rt| real_rt.deinit();
        self.pending.deinit();
        self.real_to_sim.deinit();
        self.ready.deinit(self.allocator);
        self.timeline.deinit(self.allocator);
        for (self.snapshot_paths.items) |path| self.allocator.free(path);
        self.snapshot_paths.deinit(self.allocator);
        self.snapshot_meta.deinit(self.allocator);
        for (self.event_log.items) |entry| {
            if (entry.data.len != 0) self.allocator.free(entry.data);
        }
        self.event_log.deinit(self.allocator);
        self.submit_log.deinit(self.allocator);
        self.fault_log.deinit(self.allocator);
        if (self.replay_log) |log| {
            var owned = log;
            free_replay_log(self.allocator, &owned);
        }
    }

    pub fn report(self: *reactor, allocator: std.mem.Allocator) !Report {
        const counts = try allocator.alloc(u64, fault_kind_count);
        std.mem.copyForwards(u64, counts, self.fault_counts[0..]);
        const sequence = try allocator.alloc(FaultKind, self.fault_log.items.len);
        std.mem.copyForwards(FaultKind, sequence, self.fault_log.items);
        var paths = try allocator.alloc([]const u8, self.snapshot_paths.items.len);
        for (self.snapshot_paths.items, 0..) |path, idx| {
            paths[idx] = try allocator.dupe(u8, path);
        }
        const events = try allocator.alloc(ReportEvent, self.event_log.items.len);
        for (self.event_log.items, 0..) |entry, idx| {
            events[idx] = .{
                .time_ns = entry.time_ns,
                .op_id = entry.op_id,
                .kind = entry.kind,
                .result = entry.result,
                .err_code = entry.err_code,
                .user_data = entry.user_data,
                .fault_kind = entry.fault_kind,
            };
        }
        return .{
            .scenario_name = self.config.scenario_name,
            .run_name = self.config.run_name,
            .seed = self.config.seed,
            .event_count = self.event_count,
            .fault_counts = counts,
            .fault_sequence = sequence,
            .snapshot_paths = paths,
            .events = events,
            .program_tree = null,
        };
    }

    pub fn submit_read(self: *reactor, fd_value: u64, buf: []u8, user_data: u64) !u32 {
        const info = fd.decode(fd_value);
        const mode = io_mode(component_mode(self.config.components, info.kind));
        if (self.should_oom()) {
            const err_code: u32 = @intCast(@intFromError(error.OutOfMemory));
            try self.record_submit(0, .read, user_data, info.kind, @intCast(buf.len), err_code);
            return error.OutOfMemory;
        }
        const sim_id = try self.alloc_op_id(.read, user_data, info.kind, @intCast(buf.len));
        try self.pending.put(sim_id, .{
            .kind = .read,
            .user_data = user_data,
            .fd_kind = info.kind,
            .mode = mode,
            .real_id = null,
            .buf_ptr = if (buf.len == 0) null else buf.ptr,
            .buf_len = @intCast(buf.len),
        });
        try self.record_submit(sim_id, .read, user_data, info.kind, @intCast(buf.len), 0);
        if (self.replay_log != null) return sim_id;
        switch (mode) {
            .real => {
                const real_rt = try self.ensure_real();
                const real_id = try real_rt.submit_read(info.raw, buf, user_data);
                if (self.pending.getPtr(sim_id)) |pending| pending.real_id = real_id;
                try self.real_to_sim.put(real_id, sim_id);
            },
            else => {
                try self.schedule_mock(sim_id, .read, info.kind, @intCast(buf.len), user_data, buf);
            },
        }
        return sim_id;
    }

    pub fn submit_write(self: *reactor, fd_value: u64, buf: []u8, user_data: u64) !u32 {
        const info = fd.decode(fd_value);
        const mode = io_mode(component_mode(self.config.components, info.kind));
        if (self.should_oom()) {
            const err_code: u32 = @intCast(@intFromError(error.OutOfMemory));
            try self.record_submit(0, .write, user_data, info.kind, @intCast(buf.len), err_code);
            return error.OutOfMemory;
        }
        const sim_id = try self.alloc_op_id(.write, user_data, info.kind, @intCast(buf.len));
        try self.pending.put(sim_id, .{
            .kind = .write,
            .user_data = user_data,
            .fd_kind = info.kind,
            .mode = mode,
            .real_id = null,
            .buf_ptr = null,
            .buf_len = @intCast(buf.len),
        });
        try self.record_submit(sim_id, .write, user_data, info.kind, @intCast(buf.len), 0);
        if (self.replay_log != null) return sim_id;
        switch (mode) {
            .real => {
                const real_rt = try self.ensure_real();
                const real_id = try real_rt.submit_write(info.raw, buf, user_data);
                if (self.pending.getPtr(sim_id)) |pending| pending.real_id = real_id;
                try self.real_to_sim.put(real_id, sim_id);
            },
            else => {
                try self.schedule_mock(sim_id, .write, info.kind, @intCast(buf.len), user_data, null);
            },
        }
        return sim_id;
    }

    pub fn submit_accept(self: *reactor, fd_value: u64, user_data: u64) !u32 {
        const info = fd.decode(fd_value);
        const mode = io_mode(component_mode(self.config.components, info.kind));
        if (self.should_oom()) {
            const err_code: u32 = @intCast(@intFromError(error.OutOfMemory));
            try self.record_submit(0, .accept, user_data, info.kind, 0, err_code);
            return error.OutOfMemory;
        }
        const sim_id = try self.alloc_op_id(.accept, user_data, info.kind, 0);
        try self.pending.put(sim_id, .{
            .kind = .accept,
            .user_data = user_data,
            .fd_kind = info.kind,
            .mode = mode,
            .real_id = null,
            .buf_ptr = null,
            .buf_len = 0,
        });
        try self.record_submit(sim_id, .accept, user_data, info.kind, 0, 0);
        if (self.replay_log != null) return sim_id;
        switch (mode) {
            .real => {
                const real_rt = try self.ensure_real();
                const real_id = try real_rt.submit_accept(info.raw, user_data);
                if (self.pending.getPtr(sim_id)) |pending| pending.real_id = real_id;
                try self.real_to_sim.put(real_id, sim_id);
            },
            else => {
                const mock_fd = self.next_mock_fd();
                const encoded = fd.encode(@intCast(mock_fd), fallback_kind(info.kind));
                try self.schedule_mock(sim_id, .accept, info.kind, @intCast(encoded), user_data, null);
            },
        }
        return sim_id;
    }

    pub fn submit_timer(self: *reactor, timeout_ns: u64, user_data: u64) !u32 {
        self.sync_clock();
        if (self.should_oom()) {
            const err_code: u32 = @intCast(@intFromError(error.OutOfMemory));
            try self.record_submit(0, .timer, user_data, .unknown, 0, err_code);
            return error.OutOfMemory;
        }
        const sim_id = try self.alloc_op_id(.timer, user_data, .unknown, 0);
        try self.pending.put(sim_id, .{
            .kind = .timer,
            .user_data = user_data,
            .fd_kind = .unknown,
            .mode = .sim,
            .real_id = null,
            .buf_ptr = null,
            .buf_len = 0,
        });
        try self.record_submit(sim_id, .timer, user_data, .unknown, 0, 0);
        if (self.replay_log != null) return sim_id;
        const at_ns = self.now + timeout_ns;
        try self.timeline.append(self.allocator, .{
            .at_ns = at_ns,
            .completion = .{
                .id = sim_id,
                .kind = .timer,
                .result = 0,
                .err = null,
                .user_data = user_data,
            },
            .fault_kind = .none,
        });
        return sim_id;
    }

    pub fn cancel(self: *reactor, op_id: u32) bool {
        const removed = self.pending.fetchRemove(op_id) orelse return false;
        if (removed.value.real_id) |real_id| {
            if (self.real) |*real_rt| {
                _ = real_rt.cancel(real_id);
            }
            _ = self.real_to_sim.remove(real_id);
        }
        var idx: usize = 0;
        while (idx < self.timeline.items.len) : (idx += 1) {
            if (self.timeline.items[idx].completion.id == op_id) {
                _ = self.timeline.swapRemove(idx);
                break;
            }
        }
        return true;
    }

    pub fn poll(self: *reactor, timeout_ns: ?u64) ![]const completion {
        self.ready.clearRetainingCapacity();
        self.sync_clock();

        if (self.replay_log != null) {
            self.emit_replay();
            if (self.ready.items.len > 0) return self.ready.items;
        }

        if (self.real) |*real_rt| {
            const wait_ns = self.real_poll_timeout(timeout_ns);
            const completed = try real_rt.poll(wait_ns);
            for (completed) |item| {
                const sim_id = self.real_to_sim.get(item.id) orelse continue;
                const pending = self.pending.get(sim_id) orelse continue;
                _ = self.real_to_sim.remove(item.id);
                var out = item;
                out.id = sim_id;
                if (out.kind == .accept and out.err == null and out.result >= 0) {
                    const raw_fd: u64 = @intCast(out.result);
                    out.result = @intCast(fd.encode(@intCast(raw_fd), fallback_kind(pending.fd_kind)));
                }
                const decision = self.decide_fault(pending.fd_kind, out.kind, completion_result(out));
                if (decision.result) |override| {
                    out.result = override;
                }
                if (decision.err) |err| {
                    out.err = err;
                    out.result = -1;
                }
                if (decision.corrupt and out.err == null and out.kind == .read and pending.buf_ptr != null) {
                    const count = completion_result(out);
                    const max_len = @min(@as(usize, pending.buf_len), @as(usize, @intCast(count)));
                    if (max_len > 0) {
                        const buf = pending.buf_ptr.?[0..max_len];
                        self.corrupt_buffer(buf);
                    }
                }
                if (decision.delay_ns > 0) {
                    try self.timeline.append(self.allocator, .{
                        .at_ns = self.now + decision.delay_ns,
                        .completion = out,
                        .fault_kind = decision.kind,
                    });
                    continue;
                }
                var removed: ?Pending = null;
                if (self.pending.fetchRemove(sim_id)) |entry| {
                    removed = entry.value;
                }
                self.push_ready(out, removed, decision.kind);
            }
        }

        self.emit_due_events();

        if (self.ready.items.len > 0) return self.ready.items;

        if (self.replay_log != null) {
            self.emit_replay();
            if (self.ready.items.len > 0) return self.ready.items;
            if (self.replay_stalled()) return error.ReplayMismatch;
            return self.ready.items;
        }

        if (self.config.components.clock != .real) {
            if (self.next_event_time()) |next_time| {
                if (timeout_ns) |timeout| {
                    const target = self.now + timeout;
                    if (next_time > target) {
                        self.now = target;
                        return self.ready.items;
                    }
                }
                self.now = next_time;
                self.emit_due_events();
            }
        }

        return self.ready.items;
    }

    pub fn now_ns(self: *reactor) u64 {
        self.sync_clock();
        return self.now;
    }

    fn sync_clock(self: *reactor) void {
        if (self.replay_log != null) return;
        if (self.config.components.clock == .real) {
            self.now = monotonic_now_ns();
        }
    }

    fn alloc_id(self: *reactor) u32 {
        const id = self.next_id;
        self.next_id += 1;
        return id;
    }

    fn alloc_op_id(
        self: *reactor,
        kind: op_kind,
        user_data: u64,
        fd_kind: fd.fd_kind,
        buf_len: u32,
    ) !u32 {
        if (self.replay_log) |log| {
            if (log.submits.len != 0) {
                if (self.replay_submit_index >= log.submits.len) {
                    self.report_replay_submit_mismatch(kind, user_data, fd_kind, buf_len, null);
                    return error.ReplayMismatch;
                }
                const entry = log.submits[self.replay_submit_index];
                self.replay_submit_index += 1;
                if (entry.kind != kind or entry.user_data != user_data or entry.fd_kind != fd_kind or entry.buf_len != buf_len) {
                    self.report_replay_submit_mismatch(kind, user_data, fd_kind, buf_len, entry);
                    return error.ReplayMismatch;
                }
                if (entry.err_code != 0) {
                    return @errorFromInt(@as(u16, @intCast(entry.err_code)));
                }
                return entry.op_id;
            }
        }
        return self.alloc_id();
    }

    fn record_submit(
        self: *reactor,
        op_id: u32,
        kind: op_kind,
        user_data: u64,
        fd_kind: fd.fd_kind,
        buf_len: u32,
        err_code: u32,
    ) !void {
        if (self.replay_log != null) return;
        if (self.config.snapshots == null) return;
        try self.submit_log.append(self.allocator, .{
            .op_id = op_id,
            .kind = kind,
            .user_data = user_data,
            .fd_kind = fd_kind,
            .buf_len = buf_len,
            .err_code = err_code,
        });
    }

    fn report_replay_submit_mismatch(
        self: *reactor,
        kind: op_kind,
        user_data: u64,
        fd_kind: fd.fd_kind,
        buf_len: u32,
        expected: ?SubmitLogEntry,
    ) void {
        const idx = if (self.replay_submit_index == 0) 0 else self.replay_submit_index - 1;
        if (expected) |entry| {
            std.log.err(
                "sim replay mismatch: submit #{d} expected {s} fd={s} user={d} len={d}, got {s} fd={s} user={d} len={d}",
                .{
                    idx,
                    @tagName(entry.kind),
                    @tagName(entry.fd_kind),
                    entry.user_data,
                    entry.buf_len,
                    @tagName(kind),
                    @tagName(fd_kind),
                    user_data,
                    buf_len,
                },
            );
        } else {
            std.log.err(
                "sim replay mismatch: submit #{d} expected no more entries, got {s} fd={s} user={d} len={d}",
                .{ idx, @tagName(kind), @tagName(fd_kind), user_data, buf_len },
            );
        }
    }

    fn ensure_real(self: *reactor) !*real_backend.reactor {
        if (self.real) |*real_rt| return real_rt;
        self.real = try real_backend.reactor.init(self.allocator);
        return &self.real.?;
    }

    fn emit_due_events(self: *reactor) void {
        var idx: usize = 0;
        while (idx < self.timeline.items.len) {
            if (self.timeline.items[idx].at_ns > self.now) {
                idx += 1;
                continue;
            }
            const event = self.timeline.items[idx];
            _ = self.timeline.swapRemove(idx);
            var pending: ?Pending = null;
            if (self.pending.fetchRemove(event.completion.id)) |removed| {
                pending = removed.value;
            }
            self.push_ready(event.completion, pending, event.fault_kind);
        }
    }

    fn next_event_time(self: *reactor) ?u64 {
        if (self.timeline.items.len == 0) return null;
        var min_time = self.timeline.items[0].at_ns;
        for (self.timeline.items[1..]) |event| {
            if (event.at_ns < min_time) min_time = event.at_ns;
        }
        return min_time;
    }

    fn emit_replay(self: *reactor) void {
        const log = self.replay_log orelse return;
        while (self.replay_index < log.events.len) {
            const entry = log.events[self.replay_index];
            const match_id = if (log.submits.len != 0)
                (if (self.pending.contains(entry.op_id)) entry.op_id else break)
            else
                (self.match_replay_pending(entry) orelse break);
            self.replay_index += 1;
            self.now = entry.time_ns;
            var err: ?anyerror = null;
            if (entry.err_code != 0) {
                err = @errorFromInt(@as(u16, @intCast(entry.err_code)));
            }
            var pending: ?Pending = null;
            if (self.pending.fetchRemove(match_id)) |removed| {
                pending = removed.value;
                if (entry.kind == .read and entry.data.len != 0 and removed.value.buf_ptr != null) {
                    const max_len = @min(@as(usize, removed.value.buf_len), entry.data.len);
                    const dst = removed.value.buf_ptr.?[0..max_len];
                    std.mem.copyForwards(u8, dst, entry.data[0..max_len]);
                }
            }
            self.push_ready(.{
                .id = match_id,
                .kind = entry.kind,
                .result = @intCast(entry.result),
                .err = err,
                .user_data = entry.user_data,
            }, pending, entry.fault_kind);
        }
    }

    fn match_replay_pending(self: *reactor, entry: EventLogEntry) ?u32 {
        if (self.pending.contains(entry.op_id)) return entry.op_id;
        var fallback: ?u32 = null;
        var any: ?u32 = null;
        var it = self.pending.iterator();
        while (it.next()) |item| {
            const pending = item.value_ptr.*;
            if (any == null) any = item.key_ptr.*;
            if (pending.kind != entry.kind) continue;
            if (pending.user_data == entry.user_data) return item.key_ptr.*;
            if (fallback == null) fallback = item.key_ptr.*;
        }
        if (fallback) |match_id| return match_id;
        if (self.pending.count() == 1) return any;
        return null;
    }

    fn replay_stalled(self: *reactor) bool {
        const log = self.replay_log orelse return false;
        if (self.replay_index >= log.events.len) {
            if (self.pending.count() != 0) {
                std.log.err("sim replay mismatch: log exhausted with {d} pending ops", .{self.pending.count()});
                return true;
            }
            return false;
        }
        const entry = log.events[self.replay_index];
        if (log.submits.len != 0) {
            if (!self.pending.contains(entry.op_id)) {
                std.log.err(
                    "sim replay mismatch: next event op_id={d} kind={s} user={d} not pending",
                    .{ entry.op_id, @tagName(entry.kind), entry.user_data },
                );
                return true;
            }
            return false;
        }
        if (self.match_replay_pending(entry) == null) {
            std.log.err(
                "sim replay mismatch: next event kind={s} user={d} has no pending match",
                .{ @tagName(entry.kind), entry.user_data },
            );
            return true;
        }
        return false;
    }

    fn schedule_mock(
        self: *reactor,
        sim_id: u32,
        kind: op_kind,
        fd_kind: fd.fd_kind,
        result: u64,
        user_data: u64,
        buf: ?[]u8,
    ) !void {
        const decision = self.decide_fault(fd_kind, kind, result);
        const err: ?anyerror = decision.err;
        var final_result: i64 = @intCast(result);
        if (decision.result) |override| {
            final_result = override;
        }
        if (err == null and kind == .read and buf != null) {
            self.fill_read_buffer(buf.?);
            if (decision.corrupt) {
                self.corrupt_buffer(buf.?);
            }
        }
        const at_ns = self.now + decision.delay_ns;
        try self.timeline.append(self.allocator, .{
            .at_ns = at_ns,
            .completion = .{
                .id = sim_id,
                .kind = kind,
                .result = if (err == null) final_result else -1,
                .err = err,
                .user_data = user_data,
            },
            .fault_kind = decision.kind,
        });
    }

    fn push_ready(self: *reactor, item: completion, pending: ?Pending, fault_kind: FaultKind) void {
        var out = item;
        if (self.replay_log == null) {
            self.clamp_completion(&out, pending);
        }
        self.ready.append(self.allocator, out) catch return;
        self.record_event(out, pending, fault_kind) catch {};
    }

    fn clamp_completion(self: *reactor, item: *completion, pending: ?Pending) void {
        _ = self;
        const info = pending orelse return;
        if (item.err != null) return;
        if (item.kind != .read and item.kind != .write) return;
        if (item.result < 0) return;
        const max_len: isize = @intCast(info.buf_len);
        if (item.result > max_len) item.result = max_len;
    }

    fn record_event(self: *reactor, item: completion, pending: ?Pending, fault_kind: FaultKind) !void {
        const capture_events = self.config.capture_report or self.config.snapshots != null;
        if (capture_events) {
            try self.fault_log.append(self.allocator, fault_kind);
            self.fault_counts[@intFromEnum(fault_kind)] += 1;
            self.event_count += 1;
        }

        if (!capture_events) return;
        const err_code: u32 = if (item.err) |err| @intCast(@intFromError(err)) else 0;
        var data: []u8 = &[_]u8{};
        if (self.replay_log == null and self.config.snapshots != null and self.should_capture_data() and pending != null and item.kind == .read and item.err == null and item.result > 0) {
            const count: usize = @intCast(item.result);
            const src_info = pending.?;
            if (src_info.buf_ptr != null) {
                const max_len = @min(@as(usize, src_info.buf_len), count);
                if (max_len > 0) {
                    data = try self.allocator.alloc(u8, max_len);
                    std.mem.copyForwards(u8, data, src_info.buf_ptr.?[0..max_len]);
                }
            }
        }
        try self.event_log.append(self.allocator, .{
            .time_ns = self.now,
            .op_id = item.id,
            .kind = item.kind,
            .result = @intCast(item.result),
            .err_code = err_code,
            .user_data = item.user_data,
            .fault_kind = fault_kind,
            .data = data,
        });
        if (self.replay_log == null) {
            if (self.config.snapshots) |snap_cfg| {
                if (snap_cfg.steps) |steps| {
                    if (steps != 0 and (self.event_count % steps) == 0) {
                        try self.write_snapshot();
                    }
                }
            }
        }
    }

    fn flush_snapshot(self: *reactor) !void {
        if (self.replay_log != null) return;
        if (self.config.snapshots == null) return;
        if (self.event_log.items.len == 0 and self.submit_log.items.len == 0) return;
        try self.write_snapshot();
    }

    fn write_snapshot(self: *reactor) !void {
        if (self.replay_log != null) return;
        const config = self.config.snapshots orelse return;
        const dir = self.config.snapshot_dir orelse ".";
        const mode = snapshot_mode(config.mode);
        var kind: SnapshotKind = .full;
        switch (mode) {
            .full => kind = .full,
            .delta => {
                if (self.snapshot_paths.items.len != 0) kind = .delta;
            },
            .full_delta => {
                const interval = snapshot_full_interval(config);
                if (self.snapshot_paths.items.len != 0 and interval > 1 and (self.snapshot_index % interval) != 0) {
                    kind = .delta;
                }
            },
        }

        var base_name: []const u8 = "";
        if (kind == .delta) {
            if (self.snapshot_paths.items.len == 0) {
                kind = .full;
            } else {
                base_name = std.fs.path.basename(self.snapshot_paths.items[self.snapshot_paths.items.len - 1]);
            }
        }

        const submit_start: u64 = if (kind == .full) 0 else self.last_snapshot_submit;
        const event_start: u64 = if (kind == .full) 0 else self.last_snapshot_event;
        const submit_start_idx: usize = @intCast(submit_start);
        const event_start_idx: usize = @intCast(event_start);
        if (submit_start_idx > self.submit_log.items.len or event_start_idx > self.event_log.items.len) return;

        const submit_slice = self.submit_log.items[submit_start_idx..];
        const event_slice = self.event_log.items[event_start_idx..];

        const name = try self.random_snapshot_name();
        defer self.allocator.free(name);
        try std.fs.cwd().makePath(dir);
        const path = try std.fs.path.join(self.allocator, &.{ dir, name });
        var path_owned = true;
        errdefer if (path_owned) self.allocator.free(path);
        var file = try std.fs.cwd().createFile(path, .{});
        defer file.close();
        try file.writeAll("INKSNAP5");
        try write_u32(&file, 5);
        try write_string_file(&file, self.config.scenario_name);
        try write_string_file(&file, self.config.run_name);
        try write_string_file(&file, config.mode);
        try write_string_file(&file, config.level);
        try write_string_file(&file, config.compress);
        try write_u64(&file, self.config.seed);
        try write_u8(&file, @intFromEnum(kind));
        try write_string_file(&file, base_name);
        try write_u64(&file, submit_start);
        try write_u64(&file, event_start);
        try write_u64(&file, @intCast(submit_slice.len));
        try write_u64(&file, @intCast(event_slice.len));

        if (is_compact_compress(config.compress)) {
            try write_snapshot_compact(&file, submit_slice, event_slice);
        } else {
            try write_snapshot_raw(&file, submit_slice, event_slice);
        }

        try self.snapshot_paths.append(self.allocator, path);
        errdefer _ = self.snapshot_paths.pop();
        try self.snapshot_meta.append(self.allocator, .{ .path = path, .kind = kind });
        path_owned = false;
        self.last_snapshot_submit = @intCast(self.submit_log.items.len);
        self.last_snapshot_event = @intCast(self.event_log.items.len);
        self.snapshot_index += 1;

        if (config.retention) |keep| {
            if (keep != 0) self.trim_snapshots(@intCast(keep));
        }
    }

    fn trim_snapshots(self: *reactor, keep: usize) void {
        while (self.snapshot_meta.items.len > keep) {
            if (self.snapshot_meta.items.len == 0) break;
            if (self.snapshot_meta.items.len > 1 and self.snapshot_meta.items[0].kind == .full and self.snapshot_meta.items[1].kind == .delta) {
                self.remove_snapshot_chain();
                continue;
            }
            self.remove_snapshot_at(0);
        }
        while (self.snapshot_meta.items.len > 0 and self.snapshot_meta.items[0].kind == .delta) {
            self.remove_snapshot_at(0);
        }
    }

    fn remove_snapshot_chain(self: *reactor) void {
        while (self.snapshot_meta.items.len > 0) {
            const kind = self.snapshot_meta.items[0].kind;
            self.remove_snapshot_at(0);
            if (kind == .full) {
                if (self.snapshot_meta.items.len == 0) break;
                if (self.snapshot_meta.items[0].kind == .full) break;
            }
        }
    }

    fn remove_snapshot_at(self: *reactor, index: usize) void {
        if (index >= self.snapshot_meta.items.len or index >= self.snapshot_paths.items.len) return;
        _ = self.snapshot_meta.orderedRemove(index);
        const old = self.snapshot_paths.orderedRemove(index);
        std.fs.cwd().deleteFile(old) catch {};
        self.allocator.free(old);
    }

    fn random_snapshot_name(self: *reactor) ![]const u8 {
        const alphabet = "abcdefghijklmnopqrstuvwxyz0123456789";
        const buf = try self.allocator.alloc(u8, 16);
        for (buf) |*ch| {
            const idx = self.name_rng.random().uintLessThan(usize, alphabet.len);
            ch.* = alphabet[idx];
        }
        const base = if (self.config.run_name.len != 0) self.config.run_name else self.config.scenario_name;
        const prefix = try sanitize_label(self.allocator, base);
        defer self.allocator.free(prefix);
        const name = if (prefix.len != 0)
            try std.fmt.allocPrint(self.allocator, "{s}-{s}.inksnapshot", .{ prefix, buf })
        else
            try std.fmt.allocPrint(self.allocator, "{s}.inksnapshot", .{buf});
        self.allocator.free(buf);
        return name;
    }

    fn decide_fault(self: *reactor, kind: fd.fd_kind, op: op_kind, max_result: u64) FaultDecision {
        if (self.replay_log != null) return .{};
        for (self.config.faults.rules) |rule| {
            if (rule.probability <= 0) continue;
            if (rule.component) |comp| {
                if (comp != kind) continue;
            }
            if (rule.op) |op_filter| {
                if (op_filter != op) continue;
            }
            if (rule.addr.len != 0 or rule.path.len != 0) {
                continue;
            }
            if (self.rng.random().float(f64) >= rule.probability) continue;
            var decision = FaultDecision{ .kind = rule.kind };
            switch (rule.kind) {
                .none => {},
                .@"error" => decision.err = SimError.Injected,
                .drop => decision.err = SimError.Dropped,
                .disconnect => decision.err = SimError.Disconnect,
                .timeout => {
                    decision.err = SimError.Timeout;
                    if (rule.timeout_ns != 0) decision.delay_ns = rule.timeout_ns;
                },
                .delay, .reorder => {
                    const base_delay: u64 = if (rule.delay_ns != 0) rule.delay_ns else 1_000_000;
                    var delay = base_delay;
                    if (rule.jitter_ns != 0) {
                        delay += self.rng.random().uintLessThan(u64, rule.jitter_ns + 1);
                    }
                    decision.delay_ns = delay;
                },
                .corrupt => {
                    if (op == .read) {
                        decision.corrupt = true;
                    }
                },
                .partial => {
                    if (op == .read or op == .write) {
                        const max_len = max_result;
                        if (max_len == 0) return decision;
                        var min_len: u64 = if (rule.partial_min != 0) rule.partial_min else 1;
                        var max_allowed: u64 = if (rule.partial_max != 0) rule.partial_max else max_len;
                        if (max_allowed > max_len) max_allowed = max_len;
                        if (min_len > max_allowed) min_len = max_allowed;
                        const span = max_allowed - min_len + 1;
                        const pick = if (span == 0) 0 else self.rng.random().uintLessThan(u64, span);
                        decision.result = @intCast(min_len + pick);
                    }
                },
            }
            return decision;
        }
        return .{};
    }

    fn should_oom(self: *reactor) bool {
        if (self.replay_log != null) return false;
        if (self.config.components.alloc == .real) return false;
        if (self.config.faults.oom <= 0) return false;
        return self.rng.random().float(f64) < self.config.faults.oom;
    }

    fn fill_read_buffer(self: *reactor, buf: []u8) void {
        for (buf) |*byte| {
            byte.* = @intCast(self.rng.random().uintLessThan(u16, 256));
        }
    }

    fn corrupt_buffer(self: *reactor, buf: []u8) void {
        if (buf.len == 0) return;
        const flips = @min(@as(usize, 4), buf.len);
        var i: usize = 0;
        while (i < flips) : (i += 1) {
            const idx = self.rng.random().uintLessThan(usize, buf.len);
            buf[idx] = ~buf[idx];
        }
    }

    fn should_capture_data(self: *reactor) bool {
        const snap = self.config.snapshots orelse return false;
        if (snap.level.len == 0) return true;
        if (std.mem.eql(u8, snap.level, "full")) return true;
        if (std.mem.eql(u8, snap.level, "events")) return false;
        if (std.mem.eql(u8, snap.level, "meta")) return false;
        if (std.mem.eql(u8, snap.level, "none")) return false;
        return true;
    }

    fn completion_result(item: completion) u64 {
        if (item.result < 0) return 0;
        return @intCast(item.result);
    }

    fn next_mock_fd(self: *reactor) u64 {
        const val = self.mock_fd;
        self.mock_fd += 1;
        return val;
    }

    fn real_poll_timeout(self: *reactor, timeout_ns: ?u64) ?u64 {
        if (timeout_ns) |timeout| return timeout;
        if (self.next_event_time()) |next_time| {
            if (next_time > self.now) return next_time - self.now;
            return 0;
        }
        return null;
    }
};

const SnapshotMode = enum {
    full,
    delta,
    full_delta,
};

fn snapshot_mode(value: []const u8) SnapshotMode {
    if (std.mem.eql(u8, value, "delta")) return .delta;
    if (std.mem.eql(u8, value, "full+delta") or std.mem.eql(u8, value, "full_delta")) return .full_delta;
    return .full;
}

fn snapshot_full_interval(snap: Snapshots) u64 {
    if (snap.retention) |keep| {
        if (keep > 0) return keep;
    }
    return 16;
}

fn is_compact_compress(value: []const u8) bool {
    if (value.len == 0) return false;
    if (std.mem.eql(u8, value, "none")) return false;
    if (std.mem.eql(u8, value, "compact")) return true;
    if (std.mem.eql(u8, value, "zstd")) return true;
    if (std.mem.eql(u8, value, "rle")) return true;
    return false;
}

fn needs_real(components: Components) bool {
    return components.fs == .real or components.tcp == .real or components.udp == .real;
}

fn io_mode(mode: ComponentMode) ComponentMode {
    return switch (mode) {
        .real => .real,
        else => .mock,
    };
}

fn component_mode(components: Components, kind: fd.fd_kind) ComponentMode {
    return switch (kind) {
        .fs => components.fs,
        .tcp => components.tcp,
        .udp => components.udp,
        .unknown => components.fs,
    };
}

fn fallback_kind(kind: fd.fd_kind) fd.fd_kind {
    return if (kind == .unknown) .tcp else kind;
}

fn free_replay_log(allocator: std.mem.Allocator, log: *ReplayLog) void {
    for (log.events) |entry| {
        if (entry.data.len != 0) allocator.free(entry.data);
    }
    allocator.free(log.events);
    allocator.free(log.submits);
}

fn resolve_snapshot_base(
    allocator: std.mem.Allocator,
    path: []const u8,
    base: []const u8,
) ![]const u8 {
    if (base.len == 0) return error.InvalidSnapshot;
    if (std.fs.path.isAbsolute(base)) {
        return allocator.dupe(u8, base);
    }
    const dir = std.fs.path.dirname(path) orelse ".";
    return try std.fs.path.join(allocator, &.{ dir, base });
}

fn read_snapshot(allocator: std.mem.Allocator, path: []const u8) !ReplayLog {
    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const stat = try file.stat();
    const data = try file.readToEndAlloc(allocator, stat.size);
    defer allocator.free(data);

    var reader = SliceReader{ .data = data };
    const magic = try reader.take(8);
    if (!(std.mem.eql(u8, magic, "INKSNAP1") or std.mem.eql(u8, magic, "INKSNAP2") or std.mem.eql(u8, magic, "INKSNAP3") or std.mem.eql(u8, magic, "INKSNAP4") or std.mem.eql(u8, magic, "INKSNAP5"))) {
        return error.InvalidSnapshot;
    }
    const version = try reader.take_u32();
    if (version == 1) {
        {
            const scenario = try read_string(allocator, &reader);
            if (scenario.len != 0) allocator.free(scenario);
        }
        {
            const mode = try read_string(allocator, &reader);
            if (mode.len != 0) allocator.free(mode);
        }
        {
            const compress = try read_string(allocator, &reader);
            if (compress.len != 0) allocator.free(compress);
        }
        const count = try reader.take_u64();
        var events = try allocator.alloc(EventLogEntry, @intCast(count));
        var idx: usize = 0;
        while (idx < events.len) : (idx += 1) {
            const time_ns = try reader.take_u64();
            const op_id = try reader.take_u32();
            const kind_val = try reader.take_u8();
            const result = try reader.take_i64();
            const err_code = try reader.take_u32();
            const user_data = try reader.take_u64();
            events[idx] = .{
                .time_ns = time_ns,
                .op_id = op_id,
                .kind = @enumFromInt(kind_val),
                .result = result,
                .err_code = err_code,
                .user_data = user_data,
                .fault_kind = .none,
                .data = "",
            };
        }
        const submits = try allocator.alloc(SubmitLogEntry, 0);
        return .{ .submits = submits, .events = events };
    }
    if (version == 2) {
        {
            const scenario = try read_string(allocator, &reader);
            if (scenario.len != 0) allocator.free(scenario);
        }
        {
            const run_name = try read_string(allocator, &reader);
            if (run_name.len != 0) allocator.free(run_name);
        }
        {
            const mode = try read_string(allocator, &reader);
            if (mode.len != 0) allocator.free(mode);
        }
        {
            const level = try read_string(allocator, &reader);
            if (level.len != 0) allocator.free(level);
        }
        {
            const compress = try read_string(allocator, &reader);
            if (compress.len != 0) allocator.free(compress);
        }
        _ = try reader.take_u64();
        const count = try reader.take_u64();
        var events = try allocator.alloc(EventLogEntry, @intCast(count));
        var idx: usize = 0;
        while (idx < events.len) : (idx += 1) {
            const time_ns = try reader.take_u64();
            const op_id = try reader.take_u32();
            const kind_val = try reader.take_u8();
            const result = try reader.take_i64();
            const err_code = try reader.take_u32();
            const user_data = try reader.take_u64();
            const fault_val = try reader.take_u8();
            const data_len = try reader.take_u32();
            var data_slice: []const u8 = "";
            if (data_len != 0) {
                const bytes = try reader.take(@intCast(data_len));
                data_slice = try allocator.dupe(u8, bytes);
            }
            events[idx] = .{
                .time_ns = time_ns,
                .op_id = op_id,
                .kind = @enumFromInt(kind_val),
                .result = result,
                .err_code = err_code,
                .user_data = user_data,
                .fault_kind = @enumFromInt(fault_val),
                .data = data_slice,
            };
        }
        const submits = try allocator.alloc(SubmitLogEntry, 0);
        return .{ .submits = submits, .events = events };
    }
    if (version == 3) {
        {
            const scenario = try read_string(allocator, &reader);
            if (scenario.len != 0) allocator.free(scenario);
        }
        {
            const run_name = try read_string(allocator, &reader);
            if (run_name.len != 0) allocator.free(run_name);
        }
        {
            const mode = try read_string(allocator, &reader);
            if (mode.len != 0) allocator.free(mode);
        }
        {
            const level = try read_string(allocator, &reader);
            if (level.len != 0) allocator.free(level);
        }
        {
            const compress = try read_string(allocator, &reader);
            if (compress.len != 0) allocator.free(compress);
        }
        _ = try reader.take_u64();
        const submit_count = try reader.take_u64();
        var submits = try allocator.alloc(SubmitLogEntry, @intCast(submit_count));
        var submit_idx: usize = 0;
        while (submit_idx < submits.len) : (submit_idx += 1) {
            const op_id = try reader.take_u32();
            const kind_val = try reader.take_u8();
            const user_data = try reader.take_u64();
            const fd_kind_val = try reader.take_u8();
            const buf_len = try reader.take_u32();
            submits[submit_idx] = .{
                .op_id = op_id,
                .kind = @enumFromInt(kind_val),
                .user_data = user_data,
                .fd_kind = @enumFromInt(fd_kind_val),
                .buf_len = buf_len,
                .err_code = 0,
            };
        }
        const count = try reader.take_u64();
        var events = try allocator.alloc(EventLogEntry, @intCast(count));
        var idx: usize = 0;
        while (idx < events.len) : (idx += 1) {
            const time_ns = try reader.take_u64();
            const op_id = try reader.take_u32();
            const kind_val = try reader.take_u8();
            const result = try reader.take_i64();
            const err_code = try reader.take_u32();
            const user_data = try reader.take_u64();
            const fault_val = try reader.take_u8();
            const data_len = try reader.take_u32();
            var data_slice: []const u8 = "";
            if (data_len != 0) {
                const bytes = try reader.take(@intCast(data_len));
                data_slice = try allocator.dupe(u8, bytes);
            }
            events[idx] = .{
                .time_ns = time_ns,
                .op_id = op_id,
                .kind = @enumFromInt(kind_val),
                .result = result,
                .err_code = err_code,
                .user_data = user_data,
                .fault_kind = @enumFromInt(fault_val),
                .data = data_slice,
            };
        }
        return .{ .submits = submits, .events = events };
    }
    if (version == 4) {
        {
            const scenario = try read_string(allocator, &reader);
            if (scenario.len != 0) allocator.free(scenario);
        }
        {
            const run_name = try read_string(allocator, &reader);
            if (run_name.len != 0) allocator.free(run_name);
        }
        {
            const mode = try read_string(allocator, &reader);
            if (mode.len != 0) allocator.free(mode);
        }
        {
            const level = try read_string(allocator, &reader);
            if (level.len != 0) allocator.free(level);
        }
        {
            const compress = try read_string(allocator, &reader);
            if (compress.len != 0) allocator.free(compress);
        }
        _ = try reader.take_u64();
        const submit_count = try reader.take_u64();
        var submits = try allocator.alloc(SubmitLogEntry, @intCast(submit_count));
        var submit_idx: usize = 0;
        while (submit_idx < submits.len) : (submit_idx += 1) {
            const op_id = try reader.take_u32();
            const kind_val = try reader.take_u8();
            const user_data = try reader.take_u64();
            const fd_kind_val = try reader.take_u8();
            const buf_len = try reader.take_u32();
            const err_code = try reader.take_u32();
            submits[submit_idx] = .{
                .op_id = op_id,
                .kind = @enumFromInt(kind_val),
                .user_data = user_data,
                .fd_kind = @enumFromInt(fd_kind_val),
                .buf_len = buf_len,
                .err_code = err_code,
            };
        }
        const count = try reader.take_u64();
        var events = try allocator.alloc(EventLogEntry, @intCast(count));
        var idx: usize = 0;
        while (idx < events.len) : (idx += 1) {
            const time_ns = try reader.take_u64();
            const op_id = try reader.take_u32();
            const kind_val = try reader.take_u8();
            const result = try reader.take_i64();
            const err_code = try reader.take_u32();
            const user_data = try reader.take_u64();
            const fault_val = try reader.take_u8();
            const data_len = try reader.take_u32();
            var data_slice: []const u8 = "";
            if (data_len != 0) {
                const bytes = try reader.take(@intCast(data_len));
                data_slice = try allocator.dupe(u8, bytes);
            }
            events[idx] = .{
                .time_ns = time_ns,
                .op_id = op_id,
                .kind = @enumFromInt(kind_val),
                .result = result,
                .err_code = err_code,
                .user_data = user_data,
                .fault_kind = @enumFromInt(fault_val),
                .data = data_slice,
            };
        }
        return .{ .submits = submits, .events = events };
    }
    if (version != 5) return error.InvalidSnapshot;
    {
        const scenario = try read_string(allocator, &reader);
        if (scenario.len != 0) allocator.free(scenario);
    }
    {
        const run_name = try read_string(allocator, &reader);
        if (run_name.len != 0) allocator.free(run_name);
    }
    {
        const mode = try read_string(allocator, &reader);
        if (mode.len != 0) allocator.free(mode);
    }
    {
        const level = try read_string(allocator, &reader);
        if (level.len != 0) allocator.free(level);
    }
    const compress = try read_string(allocator, &reader);
    const compact = is_compact_compress(compress);
    defer if (compress.len != 0) allocator.free(compress);
    _ = try reader.take_u64();
    const kind_val = try reader.take_u8();
    if (kind_val > @intFromEnum(SnapshotKind.delta)) return error.InvalidSnapshot;
    const kind: SnapshotKind = @enumFromInt(kind_val);
    const base = try read_string(allocator, &reader);
    defer if (base.len != 0) allocator.free(base);
    const submit_start = try reader.take_u64();
    const event_start = try reader.take_u64();
    const submit_count = try reader.take_u64();
    const event_count = try reader.take_u64();

    var submits = try allocator.alloc(SubmitLogEntry, @intCast(submit_count));
    var submit_idx: usize = 0;
    while (submit_idx < submits.len) : (submit_idx += 1) {
        if (compact) {
            const op_id_raw = try reader.take_var_u64();
            if (op_id_raw > std.math.maxInt(u32)) return error.InvalidSnapshot;
            const op_id: u32 = @intCast(op_id_raw);
            const kind_raw = try reader.take_u8();
            const user_data = try reader.take_var_u64();
            const fd_kind_raw = try reader.take_u8();
            const buf_len = try reader.take_var_u64();
            const err_code = try reader.take_var_u64();
            if (buf_len > std.math.maxInt(u32) or err_code > std.math.maxInt(u32)) return error.InvalidSnapshot;
            submits[submit_idx] = .{
                .op_id = op_id,
                .kind = @enumFromInt(kind_raw),
                .user_data = user_data,
                .fd_kind = @enumFromInt(fd_kind_raw),
                .buf_len = @intCast(buf_len),
                .err_code = @intCast(err_code),
            };
        } else {
            const op_id = try reader.take_u32();
            const kind_raw = try reader.take_u8();
            const user_data = try reader.take_u64();
            const fd_kind_raw = try reader.take_u8();
            const buf_len = try reader.take_u32();
            const err_code = try reader.take_u32();
            submits[submit_idx] = .{
                .op_id = op_id,
                .kind = @enumFromInt(kind_raw),
                .user_data = user_data,
                .fd_kind = @enumFromInt(fd_kind_raw),
                .buf_len = buf_len,
                .err_code = err_code,
            };
        }
    }

    var events = try allocator.alloc(EventLogEntry, @intCast(event_count));
    var idx: usize = 0;
    while (idx < events.len) : (idx += 1) {
        if (compact) {
            const time_ns = try reader.take_var_u64();
            const op_id_raw = try reader.take_var_u64();
            if (op_id_raw > std.math.maxInt(u32)) return error.InvalidSnapshot;
            const op_id: u32 = @intCast(op_id_raw);
            const kind_raw = try reader.take_u8();
            const result = try reader.take_var_i64();
            const err_code = try reader.take_var_u64();
            const user_data = try reader.take_var_u64();
            const fault_raw = try reader.take_u8();
            const data_len = try reader.take_var_u64();
            if (err_code > std.math.maxInt(u32) or data_len > std.math.maxInt(usize)) return error.InvalidSnapshot;
            var data_slice: []const u8 = "";
            if (data_len != 0) {
                const bytes = try reader.take(@intCast(data_len));
                data_slice = try allocator.dupe(u8, bytes);
            }
            events[idx] = .{
                .time_ns = time_ns,
                .op_id = op_id,
                .kind = @enumFromInt(kind_raw),
                .result = result,
                .err_code = @intCast(err_code),
                .user_data = user_data,
                .fault_kind = @enumFromInt(fault_raw),
                .data = data_slice,
            };
        } else {
            const time_ns = try reader.take_u64();
            const op_id = try reader.take_u32();
            const kind_raw = try reader.take_u8();
            const result = try reader.take_i64();
            const err_code = try reader.take_u32();
            const user_data = try reader.take_u64();
            const fault_raw = try reader.take_u8();
            const data_len = try reader.take_u32();
            var data_slice: []const u8 = "";
            if (data_len != 0) {
                const bytes = try reader.take(@intCast(data_len));
                data_slice = try allocator.dupe(u8, bytes);
            }
            events[idx] = .{
                .time_ns = time_ns,
                .op_id = op_id,
                .kind = @enumFromInt(kind_raw),
                .result = result,
                .err_code = err_code,
                .user_data = user_data,
                .fault_kind = @enumFromInt(fault_raw),
                .data = data_slice,
            };
        }
    }

    if (kind == .delta) {
        const base_path = try resolve_snapshot_base(allocator, path, base);
        defer allocator.free(base_path);
        var base_log = try read_snapshot(allocator, base_path);
        defer free_replay_log(allocator, &base_log);
        if (@as(u64, @intCast(base_log.submits.len)) != submit_start or @as(u64, @intCast(base_log.events.len)) != event_start) {
            return error.InvalidSnapshot;
        }

        const combined_submits = try allocator.alloc(SubmitLogEntry, base_log.submits.len + submits.len);
        std.mem.copyForwards(SubmitLogEntry, combined_submits[0..base_log.submits.len], base_log.submits);
        std.mem.copyForwards(SubmitLogEntry, combined_submits[base_log.submits.len..], submits);

        const combined_events = try allocator.alloc(EventLogEntry, base_log.events.len + events.len);
        var out_idx: usize = 0;
        for (base_log.events) |entry| {
            var data_slice: []const u8 = "";
            if (entry.data.len != 0) data_slice = try allocator.dupe(u8, entry.data);
            combined_events[out_idx] = entry;
            combined_events[out_idx].data = data_slice;
            out_idx += 1;
        }
        for (events) |entry| {
            var data_slice: []const u8 = "";
            if (entry.data.len != 0) data_slice = try allocator.dupe(u8, entry.data);
            combined_events[out_idx] = entry;
            combined_events[out_idx].data = data_slice;
            out_idx += 1;
        }

        var delta_log = ReplayLog{ .submits = submits, .events = events };
        free_replay_log(allocator, &delta_log);
        return .{ .submits = combined_submits, .events = combined_events };
    }

    return .{ .submits = submits, .events = events };
}

fn write_snapshot_raw(
    file: *std.fs.File,
    submits: []const SubmitLogEntry,
    events: []const EventLogEntry,
) !void {
    for (submits) |entry| {
        try write_u32(file, entry.op_id);
        try write_u8(file, @intFromEnum(entry.kind));
        try write_u64(file, entry.user_data);
        try write_u8(file, @intFromEnum(entry.fd_kind));
        try write_u32(file, entry.buf_len);
        try write_u32(file, entry.err_code);
    }
    for (events) |entry| {
        try write_u64(file, entry.time_ns);
        try write_u32(file, entry.op_id);
        try write_u8(file, @intFromEnum(entry.kind));
        try write_i64(file, entry.result);
        try write_u32(file, entry.err_code);
        try write_u64(file, entry.user_data);
        try write_u8(file, @intFromEnum(entry.fault_kind));
        try write_u32(file, @intCast(entry.data.len));
        if (entry.data.len != 0) {
            try file.writeAll(entry.data);
        }
    }
}

fn write_snapshot_compact(
    file: *std.fs.File,
    submits: []const SubmitLogEntry,
    events: []const EventLogEntry,
) !void {
    for (submits) |entry| {
        try write_var_u64(file, entry.op_id);
        try write_u8(file, @intFromEnum(entry.kind));
        try write_var_u64(file, entry.user_data);
        try write_u8(file, @intFromEnum(entry.fd_kind));
        try write_var_u64(file, @intCast(entry.buf_len));
        try write_var_u64(file, @intCast(entry.err_code));
    }

    for (events) |entry| {
        try write_var_u64(file, entry.time_ns);
        try write_var_u64(file, entry.op_id);
        try write_u8(file, @intFromEnum(entry.kind));
        try write_var_i64(file, entry.result);
        try write_var_u64(file, @intCast(entry.err_code));
        try write_var_u64(file, entry.user_data);
        try write_u8(file, @intFromEnum(entry.fault_kind));
        try write_var_u64(file, @intCast(entry.data.len));
        if (entry.data.len != 0) {
            try file.writeAll(entry.data);
        }
    }
}

const SliceReader = struct {
    data: []const u8,
    offset: usize = 0,

    fn take(self: *SliceReader, len: usize) ![]const u8 {
        if (self.offset + len > self.data.len) return error.InvalidSnapshot;
        const slice = self.data[self.offset .. self.offset + len];
        self.offset += len;
        return slice;
    }

    fn take_u8(self: *SliceReader) !u8 {
        const slice = try self.take(1);
        return slice[0];
    }

    fn take_u32(self: *SliceReader) !u32 {
        const slice = try self.take(4);
        const ptr: *const [4]u8 = @ptrCast(slice.ptr);
        return std.mem.readInt(u32, ptr, .little);
    }

    fn take_u64(self: *SliceReader) !u64 {
        const slice = try self.take(8);
        const ptr: *const [8]u8 = @ptrCast(slice.ptr);
        return std.mem.readInt(u64, ptr, .little);
    }

    fn take_i64(self: *SliceReader) !i64 {
        const slice = try self.take(8);
        const ptr: *const [8]u8 = @ptrCast(slice.ptr);
        return std.mem.readInt(i64, ptr, .little);
    }

    fn take_var_u64(self: *SliceReader) !u64 {
        var value: u64 = 0;
        var shift: u6 = 0;
        while (true) {
            const byte = try self.take_u8();
            value |= (@as(u64, byte & 0x7f) << shift);
            if ((byte & 0x80) == 0) break;
            shift += 7;
            if (shift >= 63) return error.InvalidSnapshot;
        }
        return value;
    }

    fn take_var_i64(self: *SliceReader) !i64 {
        const raw = try self.take_var_u64();
        const sign: i64 = @intCast(raw & 1);
        return @as(i64, @intCast(raw >> 1)) ^ -sign;
    }
};

fn read_string(allocator: std.mem.Allocator, reader: *SliceReader) ![]const u8 {
    const len = try reader.take_u32();
    if (len == 0) return "";
    const bytes = try reader.take(len);
    return try allocator.dupe(u8, bytes);
}

fn write_u8(file: *std.fs.File, value: u8) !void {
    try file.writeAll(&.{value});
}

fn write_u32(file: *std.fs.File, value: u32) !void {
    var buf: [4]u8 = undefined;
    std.mem.writeInt(u32, &buf, value, .little);
    try file.writeAll(&buf);
}

fn write_u64(file: *std.fs.File, value: u64) !void {
    var buf: [8]u8 = undefined;
    std.mem.writeInt(u64, &buf, value, .little);
    try file.writeAll(&buf);
}

fn write_i64(file: *std.fs.File, value: i64) !void {
    var buf: [8]u8 = undefined;
    std.mem.writeInt(i64, &buf, value, .little);
    try file.writeAll(&buf);
}

fn write_string_file(file: *std.fs.File, value: []const u8) !void {
    try write_u32(file, @intCast(value.len));
    if (value.len != 0) try file.writeAll(value);
}

fn write_var_u64(file: *std.fs.File, value: u64) !void {
    var buf: [10]u8 = undefined;
    var idx: usize = 0;
    var remaining = value;
    while (remaining >= 0x80) {
        buf[idx] = @intCast((remaining & 0x7f) | 0x80);
        remaining >>= 7;
        idx += 1;
    }
    buf[idx] = @intCast(remaining & 0x7f);
    idx += 1;
    try file.writeAll(buf[0..idx]);
}

fn write_var_i64(file: *std.fs.File, value: i64) !void {
    const bits: u64 = @bitCast(value);
    const sign_mask: u64 = @bitCast(value >> 63);
    const zigzag = (bits << 1) ^ sign_mask;
    try write_var_u64(file, zigzag);
}

fn sanitize_label(allocator: std.mem.Allocator, value: []const u8) ![]const u8 {
    if (value.len == 0) return try allocator.dupe(u8, "");
    var out = try allocator.alloc(u8, value.len);
    for (value, 0..) |ch, idx| {
        if ((ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z') or (ch >= '0' and ch <= '9') or ch == '-' or ch == '_') {
            out[idx] = ch;
        } else {
            out[idx] = '-';
        }
    }
    return out;
}

fn select_rng_seed(config: Config) u64 {
    if (config.components.rng == .real) return random_seed();
    return config.seed;
}

fn random_seed() u64 {
    var buf: [8]u8 = undefined;
    std.crypto.random.bytes(&buf);
    return std.mem.readInt(u64, &buf, .little);
}

fn monotonic_now_ns() u64 {
    const now: i128 = std.time.nanoTimestamp();
    if (now <= 0) return 0;
    const max_u64: i128 = @intCast(std.math.maxInt(u64));
    if (now > max_u64) return std.math.maxInt(u64);
    return @intCast(now);
}
