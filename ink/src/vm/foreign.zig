const std = @import("std");
const builtin = @import("builtin");
const inkb = @import("inkb.zig");
const thread_mutex = std.Thread.Mutex;

var atomic_mutex = thread_mutex{};

pub const foreign_id = u32;

pub const InkRuntime = extern struct {
    memory: [*]u64,
    memory_len: usize,
    fp: usize,
    sp: usize,
    arg_base: usize,
    arg_base_valid: bool,
};

pub const ForeignFn = *const fn (*InkRuntime) callconv(.c) void;

pub const Resolver = struct {
    allocator: std.mem.Allocator,
    cache: std.StringHashMap(ForeignFn),
    owned_names: std.ArrayListUnmanaged([]const u8) = .{},
    libs: std.ArrayListUnmanaged(std.DynLib) = .{},
    lib_dir: ?[]const u8,

    pub fn init(allocator: std.mem.Allocator, lib_dir: ?[]const u8) Resolver {
        return .{
            .allocator = allocator,
            .cache = std.StringHashMap(ForeignFn).init(allocator),
            .lib_dir = lib_dir,
        };
    }

    pub fn deinit(self: *Resolver) void {
        for (self.owned_names.items) |name| {
            self.allocator.free(name);
        }
        self.owned_names.deinit(self.allocator);
        for (self.libs.items) |*lib| {
            lib.close();
        }
        self.libs.deinit(self.allocator);
        self.cache.deinit();
    }

    pub fn resolve(self: *Resolver, name: []const u8) ?ForeignFn {
        if (self.cache.get(name)) |func| return func;
        const symbol = mangle_symbol(self.allocator, name) catch return null;
        defer self.allocator.free(symbol);

        if (self.resolve_from_lib_dir(symbol)) |func| {
            self.cache_insert(name, func) catch return null;
            return func;
        }

        if (self.resolve_from_env(symbol)) |func| {
            self.cache_insert(name, func) catch return null;
            return func;
        }

        if (self.resolve_from_loader(symbol)) |func| {
            self.cache_insert(name, func) catch return null;
            return func;
        }

        return null;
    }

    fn cache_insert(self: *Resolver, name: []const u8, func: ForeignFn) !void {
        const duped = try self.allocator.dupe(u8, name);
        try self.owned_names.append(self.allocator, duped);
        try self.cache.put(duped, func);
    }

    fn resolve_from_lib_dir(self: *Resolver, symbol: []const u8) ?ForeignFn {
        const dir = self.lib_dir orelse return null;
        return self.resolve_in_dir(dir, symbol);
    }

    fn resolve_from_env(self: *Resolver, symbol: []const u8) ?ForeignFn {
        const env_name = "INK_FOREIGN_PATH";
        const raw = std.process.getEnvVarOwned(self.allocator, env_name) catch return null;
        defer self.allocator.free(raw);

        const sep = if (builtin.os.tag == .windows) ';' else ':';
        var it = std.mem.splitScalar(u8, raw, sep);
        while (it.next()) |dir| {
            const trimmed = std.mem.trim(u8, dir, " \t");
            if (trimmed.len == 0) continue;
            if (self.resolve_in_dir(trimmed, symbol)) |func| return func;
        }
        return null;
    }

    fn resolve_from_loader(self: *Resolver, symbol: []const u8) ?ForeignFn {
        const ext = lib_extension();
        const base = std.fmt.allocPrint(self.allocator, "{s}{s}", .{ symbol, ext }) catch return null;
        defer self.allocator.free(base);
        return self.resolve_in_path(base, symbol);
    }

    fn resolve_in_dir(self: *Resolver, dir: []const u8, symbol: []const u8) ?ForeignFn {
        const ext = lib_extension();
        const base_names = [_][]const u8{ symbol, "lib" };
        for (base_names) |prefix| {
            const stem = if (std.mem.eql(u8, prefix, "lib"))
                std.fmt.allocPrint(self.allocator, "lib{s}", .{symbol}) catch continue
            else
                std.fmt.allocPrint(self.allocator, "{s}", .{symbol}) catch continue;
            defer self.allocator.free(stem);
            const file_name = std.fmt.allocPrint(self.allocator, "{s}{s}", .{ stem, ext }) catch continue;
            defer self.allocator.free(file_name);
            const path = std.fs.path.join(self.allocator, &.{ dir, file_name }) catch continue;
            defer self.allocator.free(path);
            if (self.resolve_in_path(path, symbol)) |func| return func;
        }
        return null;
    }

    fn resolve_in_path(self: *Resolver, path: []const u8, symbol: []const u8) ?ForeignFn {
        var lib = std.DynLib.open(path) catch |err| {
            if (err == error.FileNotFound or err == error.NotDir) return null;
            return null;
        };
        const symbol_z = self.allocator.alloc(u8, symbol.len + 1) catch {
            lib.close();
            return null;
        };
        defer self.allocator.free(symbol_z);
        std.mem.copyForwards(u8, symbol_z[0..symbol.len], symbol);
        symbol_z[symbol.len] = 0;
        const func = lib.lookup(ForeignFn, symbol_z[0..symbol.len :0]) orelse {
            lib.close();
            return null;
        };
        self.libs.append(self.allocator, lib) catch {
            lib.close();
            return null;
        };
        return func;
    }
};

pub fn dispatch(machine: anytype, id: foreign_id) void {
    const debug_checks = builtin.mode != .ReleaseFast;
    if (id >= machine.foreign_names.len) {
        fail(machine, "unknown foreign id");
        return;
    }
    const name = machine.foreign_names[id];
    if (dispatch_builtin(machine, name, debug_checks)) return;

    const func = machine.foreign_resolver.resolve(name) orelse {
        fail(machine, "foreign not found");
        return;
    };

    var runtime = InkRuntime{
        .memory = machine.memory.data.ptr,
        .memory_len = machine.memory.data.len,
        .fp = machine.current.fp,
        .sp = machine.current.sp,
        .arg_base = machine.arg_base,
        .arg_base_valid = machine.arg_base_valid,
    };
    func(&runtime);
    machine.current.fp = runtime.fp;
    machine.current.sp = runtime.sp;
    machine.arg_base = runtime.arg_base;
    machine.arg_base_valid = runtime.arg_base_valid;
}

fn dispatch_builtin(machine: anytype, name: []const u8, debug_checks: bool) bool {
    if (std.mem.eql(u8, name, "std::print_int")) {
        builtin_print_int(machine);
        return true;
    }
    if (std.mem.eql(u8, name, "std::print_float")) {
        builtin_print_float(machine);
        return true;
    }
    if (std.mem.eql(u8, name, "std::print_bool")) {
        builtin_print_bool(machine);
        return true;
    }
    if (std.mem.eql(u8, name, "std::print_string")) {
        builtin_print_string(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::print_sep")) {
        builtin_print_sep(machine);
        return true;
    }
    if (std.mem.eql(u8, name, "std::print_line")) {
        builtin_print_line(machine);
        return true;
    }
    if (std.mem.eql(u8, name, "std::alloc")) {
        builtin_alloc(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::free")) {
        builtin_free(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::deref")) {
        builtin_deref(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::store")) {
        builtin_store(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::atomic_lock")) {
        builtin_atomic_lock(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::atomic_unlock")) {
        builtin_atomic_unlock(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::ptr_of")) {
        builtin_ptr_of(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::result_ok")) {
        builtin_result_ok(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::result_err")) {
        builtin_result_err(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::result_is_ok")) {
        builtin_result_is_ok(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::result_unwrap")) {
        builtin_result_unwrap(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::result_unwrap_err")) {
        builtin_result_unwrap_err(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::cancelled")) {
        builtin_cancelled(machine);
        return true;
    }
    if (std.mem.eql(u8, name, "std::sleep")) {
        builtin_sleep(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::sleep_until")) {
        builtin_sleep_until(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::timeout")) {
        builtin_timeout(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::deadline")) {
        builtin_deadline(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::yield")) {
        builtin_yield(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::read")) {
        builtin_io_read(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::write")) {
        builtin_io_write(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::accept")) {
        builtin_io_accept(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::bytes_new")) {
        builtin_bytes_new(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::bytes_from_string")) {
        builtin_bytes_from_string(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::bytes_free")) {
        builtin_bytes_free(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::bytes_len")) {
        builtin_bytes_len(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::bytes_cap")) {
        builtin_bytes_cap(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::bytes_ptr")) {
        builtin_bytes_ptr(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::bytes_set_len")) {
        builtin_bytes_set_len(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::string_new")) {
        builtin_string_new(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::string_free")) {
        builtin_string_free(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::string_len")) {
        builtin_string_len(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::string_cap")) {
        builtin_string_cap(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::string_ptr")) {
        builtin_string_ptr(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::string_set_len")) {
        builtin_string_set_len(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::string_from_int")) {
        builtin_string_from_int(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::string_from_float")) {
        builtin_string_from_float(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::string_from_bool")) {
        builtin_string_from_bool(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::string_concat")) {
        builtin_string_concat(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::buf_new")) {
        builtin_buf_new(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::buf_free")) {
        builtin_buf_free(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::buf_len")) {
        builtin_buf_len(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::buf_capacity")) {
        builtin_buf_capacity(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::buf_read_ptr")) {
        builtin_buf_read_ptr(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::buf_write_ptr")) {
        builtin_buf_write_ptr(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::buf_read_advance")) {
        builtin_buf_read_advance(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::buf_write_advance")) {
        builtin_buf_write_advance(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::buf_write_bytes")) {
        builtin_buf_write_bytes(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::buf_read_bytes")) {
        builtin_buf_read_bytes(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::buf_reset")) {
        builtin_buf_reset(machine);
        return true;
    }
    if (std.mem.eql(u8, name, "std::slice_new")) {
        builtin_slice_new(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::slice_free")) {
        builtin_slice_free(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::slice_ptr")) {
        builtin_slice_ptr(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::slice_len")) {
        builtin_slice_len(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::arena_new")) {
        builtin_arena_new(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::arena_alloc")) {
        builtin_arena_alloc(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::arena_reset")) {
        builtin_arena_reset(machine, debug_checks);
        return true;
    }
    if (std.mem.eql(u8, name, "std::arena_deinit")) {
        builtin_arena_deinit(machine, debug_checks);
        return true;
    }
    return false;
}

fn builtin_print_int(machine: anytype) void {
    var buffer: [256]u8 = undefined;
    var out_file = std.fs.File.stdout().writer(buffer[0..]);
    var out = &out_file.interface;
    const raw = read_arg(machine, 1);
    const value: i64 = @bitCast(raw);
    out.print("{d}", .{value}) catch {};
    out.flush() catch {};
}

fn builtin_print_float(machine: anytype) void {
    var buffer: [256]u8 = undefined;
    var out_file = std.fs.File.stdout().writer(buffer[0..]);
    var out = &out_file.interface;
    const raw = read_arg(machine, 1);
    const value: f64 = @bitCast(raw);
    out.print("{d}", .{value}) catch {};
    out.flush() catch {};
}

fn builtin_print_bool(machine: anytype) void {
    var buffer: [256]u8 = undefined;
    var out_file = std.fs.File.stdout().writer(buffer[0..]);
    var out = &out_file.interface;
    const value = read_arg(machine, 1) != 0;
    const text = if (value) "true" else "false";
    out.print("{s}", .{text}) catch {};
    out.flush() catch {};
}

fn builtin_print_string(machine: anytype, debug_checks: bool) void {
    const value = read_arg(machine, 1);
    if (value < @as(u64, @intCast(machine.data.len))) {
        if (data_entry(machine, value, debug_checks)) |entry| {
            if (entry.kind == .string) {
                var buffer: [256]u8 = undefined;
                var out_file = std.fs.File.stdout().writer(buffer[0..]);
                var out = &out_file.interface;
                out.print("{s}", .{entry.bytes}) catch {};
                out.flush() catch {};
                return;
            }
            if (debug_checks) {
                fail(machine, "print_string data kind mismatch");
                return;
            }
        }
    }

    const info = string_info(machine, value, debug_checks) orelse return;
    var buffer: [256]u8 = undefined;
    var out_file = std.fs.File.stdout().writer(buffer[0..]);
    var out = &out_file.interface;
    out.print("{s}", .{info.payload[0..info.len]}) catch {};
    out.flush() catch {};
}

fn builtin_print_sep(machine: anytype) void {
    _ = machine;
    var buffer: [256]u8 = undefined;
    var out_file = std.fs.File.stdout().writer(buffer[0..]);
    var out = &out_file.interface;
    out.print(" ", .{}) catch {};
    out.flush() catch {};
}

fn builtin_print_line(machine: anytype) void {
    _ = machine;
    var buffer: [256]u8 = undefined;
    var out_file = std.fs.File.stdout().writer(buffer[0..]);
    var out = &out_file.interface;
    out.print("\n", .{}) catch {};
    out.flush() catch {};
}

fn builtin_alloc(machine: anytype, debug_checks: bool) void {
    const size_val = read_arg(machine, 1);
    const size = to_usize(size_val, debug_checks, machine) orelse return;
    const alloc_words = if (size == 0) 1 else size;
    const ptr = alloc_block(machine, alloc_words, debug_checks) orelse return;
    set_ret(machine, @intCast(ptr));
}

fn builtin_free(machine: anytype, debug_checks: bool) void {
    const ptr_val = read_arg(machine, 1);
    const ptr = to_usize(ptr_val, debug_checks, machine) orelse return;
    free_block(machine, ptr, debug_checks);
}

fn builtin_deref(machine: anytype, debug_checks: bool) void {
    const ptr_val = read_arg(machine, 1);
    const ptr = to_usize(ptr_val, debug_checks, machine) orelse return;
    const value = deref_ptr(machine, ptr, debug_checks) orelse return;
    set_ret(machine, value);
}

fn builtin_store(machine: anytype, debug_checks: bool) void {
    const ptr_val = read_arg(machine, 1);
    const value = read_arg(machine, 2);
    const ptr = to_usize(ptr_val, debug_checks, machine) orelse return;
    store_ptr(machine, ptr, value, debug_checks);
}

fn builtin_atomic_lock(machine: anytype, debug_checks: bool) void {
    _ = machine;
    _ = debug_checks;
    atomic_mutex.lock();
}

fn builtin_atomic_unlock(machine: anytype, debug_checks: bool) void {
    _ = machine;
    _ = debug_checks;
    atomic_mutex.unlock();
}

fn builtin_ptr_of(machine: anytype, debug_checks: bool) void {
    const reg_val = read_arg(machine, 1);
    const reg = to_usize(reg_val, debug_checks, machine) orelse return;
    if (debug_checks and reg > 63) {
        fail(machine, "ptr_of register out of range");
        return;
    }
    set_ret(machine, @intCast(machine.current.fp + reg));
}

fn builtin_result_ok(machine: anytype, debug_checks: bool) void {
    const value = read_arg(machine, 1);
    set_result(machine, true, value, debug_checks);
}

fn builtin_result_err(machine: anytype, debug_checks: bool) void {
    const value = read_arg(machine, 1);
    set_result(machine, false, value, debug_checks);
}

fn builtin_result_is_ok(machine: anytype, debug_checks: bool) void {
    const handle_val = read_arg(machine, 1);
    if (handle_val == 0) {
        set_ret(machine, 0);
        return;
    }
    const ptr = to_usize(handle_val, debug_checks, machine) orelse return;
    const tag = deref_ptr(machine, ptr, debug_checks) orelse return;
    set_ret(machine, if (tag == 0) 1 else 0);
}

fn builtin_result_unwrap(machine: anytype, debug_checks: bool) void {
    const handle_val = read_arg(machine, 1);
    if (handle_val == 0) {
        set_ret(machine, 0);
        return;
    }
    const ptr = to_usize(handle_val, debug_checks, machine) orelse return;
    const value = deref_ptr(machine, ptr + 1, debug_checks) orelse return;
    set_ret(machine, value);
}

fn builtin_result_unwrap_err(machine: anytype, debug_checks: bool) void {
    const handle_val = read_arg(machine, 1);
    if (handle_val == 0) {
        set_ret(machine, 0);
        return;
    }
    const ptr = to_usize(handle_val, debug_checks, machine) orelse return;
    const value = deref_ptr(machine, ptr + 1, debug_checks) orelse return;
    set_ret(machine, value);
}

fn builtin_cancelled(machine: anytype) void {
    const sched = machine.scheduler orelse {
        set_ret(machine, 0);
        return;
    };
    const is_set = sched.is_cancelled(machine.current_task_id);
    set_ret(machine, if (is_set) 1 else 0);
}

fn builtin_sleep(machine: anytype, debug_checks: bool) void {
    const sched = machine.scheduler orelse {
        set_result(machine, false, 0, debug_checks);
        return;
    };
    if (pending_match(machine)) {
        const op_id = machine.pending_op_id;
        if (sched.take_completion(op_id)) |completion| {
            clear_pending(machine);
            if (completion.err) |err| {
                set_result(machine, false, err_code(err), debug_checks);
                return;
            }
            set_result(machine, true, 0, debug_checks);
            return;
        }
        machine.suspend_op(op_id);
        return;
    }

    const timeout_ns = read_arg(machine, 1);
    const op_id = sched.reactor.submit_timer(timeout_ns, @intCast(machine.current_task_id)) catch {
        set_result(machine, false, 0, debug_checks);
        return;
    };
    set_pending(machine, op_id);
    machine.suspend_op(op_id);
}

fn builtin_sleep_until(machine: anytype, debug_checks: bool) void {
    const sched = machine.scheduler orelse {
        set_result(machine, false, 0, debug_checks);
        return;
    };
    if (pending_match(machine)) {
        const op_id = machine.pending_op_id;
        if (sched.take_completion(op_id)) |completion| {
            clear_pending(machine);
            if (completion.err) |err| {
                set_result(machine, false, err_code(err), debug_checks);
                return;
            }
            set_result(machine, true, 0, debug_checks);
            return;
        }
        machine.suspend_op(op_id);
        return;
    }

    const deadline_ns = read_arg(machine, 1);
    const now_ns = monotonic_now_ns();
    const sub = @subWithOverflow(deadline_ns, now_ns);
    const timeout_ns = if (sub[1] != 0) 0 else sub[0];
    const op_id = sched.reactor.submit_timer(timeout_ns, @intCast(machine.current_task_id)) catch {
        set_result(machine, false, 0, debug_checks);
        return;
    };
    set_pending(machine, op_id);
    machine.suspend_op(op_id);
}

fn monotonic_now_ns() u64 {
    const now: i128 = std.time.nanoTimestamp();
    if (now <= 0) return 0;
    const max_u64: i128 = @intCast(std.math.maxInt(u64));
    if (now > max_u64) return std.math.maxInt(u64);
    return @intCast(now);
}

fn builtin_timeout(machine: anytype, debug_checks: bool) void {
    _ = debug_checks;
    const duration_ns = read_arg(machine, 1);
    const now_ns = monotonic_now_ns();
    const add = @addWithOverflow(now_ns, duration_ns);
    const deadline_ns = if (add[1] != 0) std.math.maxInt(u64) else add[0];
    set_ret(machine, deadline_ns);
}

fn builtin_deadline(machine: anytype, debug_checks: bool) void {
    _ = debug_checks;
    set_ret(machine, read_arg(machine, 1));
}

fn builtin_yield(machine: anytype, debug_checks: bool) void {
    _ = debug_checks;
    const sched = machine.scheduler orelse return;
    sched.ready.append(sched.allocator, machine.current_task_id) catch {};
    machine.suspend_manual();
}

fn builtin_io_read(machine: anytype, debug_checks: bool) void {
    const sched = machine.scheduler orelse {
        set_result(machine, false, 0, debug_checks);
        return;
    };
    if (pending_match(machine)) {
        const op_id = machine.pending_op_id;
        if (sched.take_completion(op_id)) |completion| {
            clear_pending(machine);
            if (completion.err) |err| {
                set_result(machine, false, err_code(err), debug_checks);
                return;
            }
            const bytes_read = result_count(completion.result);
            if (bytes_read) |count| {
                if (buf_write_advance_raw(machine, read_arg(machine, 2), count, debug_checks)) {
                    set_result(machine, true, count, debug_checks);
                    return;
                }
            }
            set_result(machine, false, 0, debug_checks);
            return;
        }
        machine.suspend_op(op_id);
        return;
    }

    const fd = to_fd(read_arg(machine, 1), debug_checks, machine) orelse return;
    const buf_handle = read_arg(machine, 2);
    const slice = buf_write_slice(machine, buf_handle, debug_checks) orelse return;
    if (slice.len == 0) {
        set_result(machine, true, 0, debug_checks);
        return;
    }
    const op_id = sched.reactor.submit_read(fd, slice, @intCast(machine.current_task_id)) catch {
        set_result(machine, false, 0, debug_checks);
        return;
    };
    set_pending(machine, op_id);
    machine.suspend_op(op_id);
}

fn builtin_io_write(machine: anytype, debug_checks: bool) void {
    const sched = machine.scheduler orelse {
        set_result(machine, false, 0, debug_checks);
        return;
    };
    if (pending_match(machine)) {
        const op_id = machine.pending_op_id;
        if (sched.take_completion(op_id)) |completion| {
            clear_pending(machine);
            if (completion.err) |err| {
                set_result(machine, false, err_code(err), debug_checks);
                return;
            }
            const bytes_written = result_count(completion.result);
            if (bytes_written) |count| {
                if (buf_read_advance_raw(machine, read_arg(machine, 2), count, debug_checks)) {
                    set_result(machine, true, count, debug_checks);
                    return;
                }
            }
            set_result(machine, false, 0, debug_checks);
            return;
        }
        machine.suspend_op(op_id);
        return;
    }

    const fd = to_fd(read_arg(machine, 1), debug_checks, machine) orelse return;
    const buf_handle = read_arg(machine, 2);
    const slice = buf_read_slice(machine, buf_handle, debug_checks) orelse return;
    if (slice.len == 0) {
        set_result(machine, true, 0, debug_checks);
        return;
    }
    const op_id = sched.reactor.submit_write(fd, slice, @intCast(machine.current_task_id)) catch {
        set_result(machine, false, 0, debug_checks);
        return;
    };
    set_pending(machine, op_id);
    machine.suspend_op(op_id);
}

fn builtin_io_accept(machine: anytype, debug_checks: bool) void {
    const sched = machine.scheduler orelse {
        set_result(machine, false, 0, debug_checks);
        return;
    };
    if (pending_match(machine)) {
        const op_id = machine.pending_op_id;
        if (sched.take_completion(op_id)) |completion| {
            clear_pending(machine);
            if (completion.err) |err| {
                set_result(machine, false, err_code(err), debug_checks);
                return;
            }
            const fd_val = result_count(completion.result) orelse 0;
            set_result(machine, true, fd_val, debug_checks);
            return;
        }
        machine.suspend_op(op_id);
        return;
    }

    const fd = to_fd(read_arg(machine, 1), debug_checks, machine) orelse return;
    const op_id = sched.reactor.submit_accept(fd, @intCast(machine.current_task_id)) catch {
        set_result(machine, false, 0, debug_checks);
        return;
    };
    set_pending(machine, op_id);
    machine.suspend_op(op_id);
}

fn builtin_bytes_new(machine: anytype, debug_checks: bool) void {
    const cap_val = read_arg(machine, 1);
    const cap = to_usize(cap_val, debug_checks, machine) orelse return;
    const ptr = bytes_alloc(machine, cap, debug_checks) orelse return;
    set_ret(machine, @intCast(ptr));
}

fn builtin_bytes_from_string(machine: anytype, debug_checks: bool) void {
    const value = read_arg(machine, 1);
    var source: ?[]const u8 = null;

    if (value < @as(u64, @intCast(machine.data.len))) {
        if (data_entry(machine, value, debug_checks)) |entry| {
            if (entry.kind == .string) {
                source = entry.bytes;
            } else if (debug_checks) {
                fail(machine, "bytes_from_string data kind mismatch");
                return;
            }
        }
    }

    if (source == null) {
        const info = string_info(machine, value, debug_checks) orelse return;
        source = info.payload[0..info.len];
    }

    const bytes = source.?;
    const cap = bytes.len;
    const ptr = bytes_alloc(machine, cap, debug_checks) orelse return;
    const info = bytes_info(machine, @intCast(ptr), debug_checks) orelse return;
    var idx: usize = 0;
    while (idx < cap) : (idx += 1) {
        machine.memory.write(info.data_ptr + idx, bytes[idx]);
    }
    set_ret(machine, @intCast(ptr));
}

fn builtin_bytes_free(machine: anytype, debug_checks: bool) void {
    const info = bytes_info(machine, read_arg(machine, 1), debug_checks) orelse return;
    free_block(machine, info.data_ptr, debug_checks);
    free_block(machine, info.ptr, debug_checks);
}

fn builtin_bytes_len(machine: anytype, debug_checks: bool) void {
    const info = bytes_info(machine, read_arg(machine, 1), debug_checks) orelse return;
    set_ret(machine, @intCast(info.len));
}

fn builtin_bytes_cap(machine: anytype, debug_checks: bool) void {
    const info = bytes_info(machine, read_arg(machine, 1), debug_checks) orelse return;
    set_ret(machine, @intCast(info.len));
}

fn builtin_bytes_ptr(machine: anytype, debug_checks: bool) void {
    const info = bytes_info(machine, read_arg(machine, 1), debug_checks) orelse return;
    set_ret(machine, @intCast(info.data_ptr));
}

fn builtin_bytes_set_len(machine: anytype, debug_checks: bool) void {
    const info = bytes_info(machine, read_arg(machine, 1), debug_checks) orelse return;
    const new_len = to_usize(read_arg(machine, 2), debug_checks, machine) orelse return;
    if (new_len > info.len) {
        if (debug_checks) fail(machine, "bytes len out of range");
        return;
    }
    machine.memory.write(info.ptr + 1, @intCast(new_len));
}

fn builtin_string_new(machine: anytype, debug_checks: bool) void {
    const cap_val = read_arg(machine, 1);
    const cap = to_usize(cap_val, debug_checks, machine) orelse return;
    const ptr = string_alloc(machine, cap, debug_checks) orelse return;
    set_ret(machine, @intCast(ptr));
}

fn builtin_string_free(machine: anytype, debug_checks: bool) void {
    const handle = read_arg(machine, 1);
    const ptr = to_usize(handle, debug_checks, machine) orelse return;
    free_block(machine, ptr, debug_checks);
}

fn builtin_string_len(machine: anytype, debug_checks: bool) void {
    const info = string_info(machine, read_arg(machine, 1), debug_checks) orelse return;
    set_ret(machine, @intCast(info.len));
}

fn builtin_string_cap(machine: anytype, debug_checks: bool) void {
    const info = string_info(machine, read_arg(machine, 1), debug_checks) orelse return;
    set_ret(machine, @intCast(info.cap));
}

fn builtin_string_ptr(machine: anytype, debug_checks: bool) void {
    const info = string_info(machine, read_arg(machine, 1), debug_checks) orelse return;
    set_ret(machine, @intCast(info.payload_ptr));
}

fn builtin_string_set_len(machine: anytype, debug_checks: bool) void {
    const info = string_info(machine, read_arg(machine, 1), debug_checks) orelse return;
    const new_len = to_usize(read_arg(machine, 2), debug_checks, machine) orelse return;
    if (new_len > info.cap) {
        if (debug_checks) fail(machine, "string len out of range");
        return;
    }
    machine.memory.write(info.ptr, @intCast(new_len));
}

fn builtin_string_from_int(machine: anytype, debug_checks: bool) void {
    var buf: [64]u8 = undefined;
    const text = std.fmt.bufPrint(&buf, "{d}", .{read_arg(machine, 1)}) catch return;
    const ptr = string_alloc(machine, text.len, debug_checks) orelse return;
    const info = string_info(machine, @intCast(ptr), debug_checks) orelse return;
    std.mem.copyForwards(u8, info.payload[0..text.len], text);
    machine.memory.write(info.ptr, @intCast(text.len));
    set_ret(machine, @intCast(ptr));
}

fn builtin_string_from_float(machine: anytype, debug_checks: bool) void {
    var buf: [64]u8 = undefined;
    const raw = read_arg(machine, 1);
    const value: f64 = @bitCast(raw);
    const text = std.fmt.bufPrint(&buf, "{d}", .{value}) catch return;
    const ptr = string_alloc(machine, text.len, debug_checks) orelse return;
    const info = string_info(machine, @intCast(ptr), debug_checks) orelse return;
    std.mem.copyForwards(u8, info.payload[0..text.len], text);
    machine.memory.write(info.ptr, @intCast(text.len));
    set_ret(machine, @intCast(ptr));
}

fn builtin_string_from_bool(machine: anytype, debug_checks: bool) void {
    const value = read_arg(machine, 1) != 0;
    const text = if (value) "true" else "false";
    const ptr = string_alloc(machine, text.len, debug_checks) orelse return;
    const info = string_info(machine, @intCast(ptr), debug_checks) orelse return;
    std.mem.copyForwards(u8, info.payload[0..text.len], text);
    machine.memory.write(info.ptr, @intCast(text.len));
    set_ret(machine, @intCast(ptr));
}

fn builtin_string_concat(machine: anytype, debug_checks: bool) void {
    const left = read_arg(machine, 1);
    const right = read_arg(machine, 2);
    const left_bytes = string_bytes(machine, left, debug_checks) orelse return;
    const right_bytes = string_bytes(machine, right, debug_checks) orelse return;
    const total = left_bytes.len + right_bytes.len;
    const ptr = string_alloc(machine, total, debug_checks) orelse return;
    const info = string_info(machine, @intCast(ptr), debug_checks) orelse return;
    std.mem.copyForwards(u8, info.payload[0..left_bytes.len], left_bytes);
    std.mem.copyForwards(u8, info.payload[left_bytes.len .. total], right_bytes);
    machine.memory.write(info.ptr, @intCast(total));
    set_ret(machine, @intCast(ptr));
}

fn builtin_buf_new(machine: anytype, debug_checks: bool) void {
    const cap_val = read_arg(machine, 1);
    const cap = to_usize(cap_val, debug_checks, machine) orelse return;
    const ptr = buf_alloc(machine, cap, debug_checks) orelse return;
    set_ret(machine, @intCast(ptr));
}

fn builtin_buf_free(machine: anytype, debug_checks: bool) void {
    const handle = read_arg(machine, 1);
    const ptr = to_usize(handle, debug_checks, machine) orelse return;
    free_block(machine, ptr, debug_checks);
}

fn builtin_buf_len(machine: anytype, debug_checks: bool) void {
    const info = buf_info(machine, read_arg(machine, 1), debug_checks) orelse return;
    set_ret(machine, @intCast(info.write - info.read));
}

fn builtin_buf_capacity(machine: anytype, debug_checks: bool) void {
    const info = buf_info(machine, read_arg(machine, 1), debug_checks) orelse return;
    set_ret(machine, @intCast(info.cap));
}

fn builtin_buf_read_ptr(machine: anytype, debug_checks: bool) void {
    const info = buf_info(machine, read_arg(machine, 1), debug_checks) orelse return;
    set_ret(machine, @intCast(info.payload_ptr + bytes_to_words(info.read)));
}

fn builtin_buf_write_ptr(machine: anytype, debug_checks: bool) void {
    const info = buf_info(machine, read_arg(machine, 1), debug_checks) orelse return;
    set_ret(machine, @intCast(info.payload_ptr + bytes_to_words(info.write)));
}

fn builtin_buf_read_advance(machine: anytype, debug_checks: bool) void {
    _ = buf_read_advance_raw(machine, read_arg(machine, 1), read_arg(machine, 2), debug_checks);
}

fn builtin_buf_write_advance(machine: anytype, debug_checks: bool) void {
    _ = buf_write_advance_raw(machine, read_arg(machine, 1), read_arg(machine, 2), debug_checks);
}

fn builtin_buf_write_bytes(machine: anytype, debug_checks: bool) void {
    const buf_handle = read_arg(machine, 1);
    const bytes_handle = read_arg(machine, 2);
    const buf_data = buf_info(machine, buf_handle, debug_checks) orelse return;
    const bytes_data = bytes_info(machine, bytes_handle, debug_checks) orelse return;
    const available = buf_data.cap - buf_data.write;
    const count = @min(available, bytes_data.len);
    if (count == 0) {
        set_ret(machine, 0);
        return;
    }
    var idx: usize = 0;
    while (idx < count) : (idx += 1) {
        const word = machine.memory.read(bytes_data.data_ptr + idx);
        buf_data.payload[buf_data.write + idx] = @intCast(word);
    }
    machine.memory.write(buf_data.ptr + 1, @intCast(buf_data.write + count));
    set_ret(machine, @intCast(count));
}

fn builtin_buf_read_bytes(machine: anytype, debug_checks: bool) void {
    const buf_handle = read_arg(machine, 1);
    const bytes_handle = read_arg(machine, 2);
    const buf_data = buf_info(machine, buf_handle, debug_checks) orelse return;
    const bytes_data = bytes_info(machine, bytes_handle, debug_checks) orelse return;
    const available = buf_data.write - buf_data.read;
    const count = @min(available, bytes_data.len);
    if (count == 0) {
        machine.memory.write(bytes_data.ptr + 1, 0);
        set_ret(machine, 0);
        return;
    }
    var idx: usize = 0;
    while (idx < count) : (idx += 1) {
        const value = buf_data.payload[buf_data.read + idx];
        machine.memory.write(bytes_data.data_ptr + idx, value);
    }
    machine.memory.write(bytes_data.ptr + 1, @intCast(count));
    const next_read = buf_data.read + count;
    if (next_read == buf_data.write) {
        machine.memory.write(buf_data.ptr, 0);
        machine.memory.write(buf_data.ptr + 1, 0);
    } else {
        machine.memory.write(buf_data.ptr, @intCast(next_read));
    }
    set_ret(machine, @intCast(count));
}

fn builtin_buf_reset(machine: anytype) void {
    const handle = read_arg(machine, 1);
    const ptr = to_usize(handle, false, machine) orelse return;
    machine.memory.write(ptr, 0);
    machine.memory.write(ptr + 1, 0);
}

fn builtin_slice_new(machine: anytype, debug_checks: bool) void {
    const data_ptr = read_arg(machine, 1);
    const len = to_usize(read_arg(machine, 2), debug_checks, machine) orelse return;
    const ptr = slice_alloc(machine, data_ptr, len, debug_checks) orelse return;
    set_ret(machine, @intCast(ptr));
}

fn builtin_slice_free(machine: anytype, debug_checks: bool) void {
    const handle = read_arg(machine, 1);
    const ptr = to_usize(handle, debug_checks, machine) orelse return;
    free_block(machine, ptr, debug_checks);
}

fn builtin_slice_ptr(machine: anytype, debug_checks: bool) void {
    const info = slice_info(machine, read_arg(machine, 1), debug_checks) orelse return;
    set_ret(machine, info.data_ptr);
}

fn builtin_slice_len(machine: anytype, debug_checks: bool) void {
    const info = slice_info(machine, read_arg(machine, 1), debug_checks) orelse return;
    set_ret(machine, @intCast(info.len));
}

fn builtin_arena_new(machine: anytype, debug_checks: bool) void {
    const size_val = read_arg(machine, 1);
    const size = to_usize(size_val, debug_checks, machine) orelse return;
    const cap = if (size == 0) default_arena_bytes else size;
    const ptr = arena_alloc(machine, cap, debug_checks) orelse return;
    set_ret(machine, @intCast(ptr));
}

fn builtin_arena_alloc(machine: anytype, debug_checks: bool) void {
    const handle = read_arg(machine, 1);
    const arena = arena_info(machine, handle, debug_checks) orelse return;
    const size_val = read_arg(machine, 2);
    const size = to_usize(size_val, debug_checks, machine) orelse return;
    const aligned = align8(size);
    if (arena.offset + aligned > arena.cap) {
        set_ret(machine, 0);
        return;
    }
    const offset_words = arena.offset / 8;
    const addr = arena.payload_ptr + offset_words;
    machine.memory.write(arena.ptr + 1, @intCast(arena.offset + aligned));
    set_ret(machine, @intCast(addr));
}

fn builtin_arena_reset(machine: anytype, debug_checks: bool) void {
    const handle = read_arg(machine, 1);
    const arena = arena_info(machine, handle, debug_checks) orelse return;
    machine.memory.write(arena.ptr + 1, 0);
}

fn builtin_arena_deinit(machine: anytype, debug_checks: bool) void {
    const handle = read_arg(machine, 1);
    const ptr = to_usize(handle, debug_checks, machine) orelse return;
    free_block(machine, ptr, debug_checks);
}

fn lib_extension() []const u8 {
    return switch (builtin.os.tag) {
        .windows => ".dll",
        .macos => ".dylib",
        else => ".so",
    };
}

fn mangle_symbol(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
    var out = std.array_list.Managed(u8).init(allocator);
    errdefer out.deinit();
    try out.appendSlice("ink_foreign_");
    var i: usize = 0;
    while (i < name.len) : (i += 1) {
        if (name[i] == ':' and i + 1 < name.len and name[i + 1] == ':') {
            try out.append('_');
            i += 1;
            continue;
        }
        try out.append(name[i]);
    }
    return out.toOwnedSlice();
}

fn read_arg(machine: anytype, index: usize) u64 {
    if (machine.arg_base_valid) {
        return machine.memory.read(machine.arg_base + 2 + index);
    }
    return machine.memory.read(machine.current.fp + index);
}

fn data_entry(machine: anytype, idx: u64, debug_checks: bool) ?inkb.data_entry {
    if (idx > std.math.maxInt(usize)) {
        if (debug_checks) fail(machine, "data index out of range");
        return null;
    }
    const slot: usize = @intCast(idx);
    if (slot >= machine.data.len) {
        if (debug_checks) fail(machine, "data index out of range");
        return null;
    }
    return machine.data[slot];
}

fn set_ret(machine: anytype, value: u64) void {
    machine.memory.write(machine.current.fp + 0, value);
}

const bytes_header_words: usize = 2;
const string_header_words: usize = 2;
const slice_header_words: usize = 2;
const buf_header_words: usize = 3;
const arena_header_words: usize = 2;
const default_arena_bytes: usize = 64 * 1024;

const bytes_view = struct {
    ptr: usize,
    data_ptr: usize,
    len: usize,
};

const string_view = struct {
    ptr: usize,
    len: usize,
    cap: usize,
    payload_ptr: usize,
    payload: []u8,
};

const slice_view = struct {
    ptr: usize,
    data_ptr: u64,
    len: usize,
};

const buf_view = struct {
    ptr: usize,
    read: usize,
    write: usize,
    cap: usize,
    payload_ptr: usize,
    payload: []u8,
};

const arena_view = struct {
    ptr: usize,
    cap: usize,
    offset: usize,
    payload_ptr: usize,
};

fn set_result(machine: anytype, ok: bool, value: u64, debug_checks: bool) void {
    const ptr = alloc_block(machine, 2, debug_checks) orelse return;
    store_ptr(machine, ptr, if (ok) 0 else 1, debug_checks);
    store_ptr(machine, ptr + 1, value, debug_checks);
    set_ret(machine, @intCast(ptr));
}

fn pending_match(machine: anytype) bool {
    return machine.pending_op_id != 0 and machine.pending_op_pc == machine.current.pc;
}

fn set_pending(machine: anytype, op_id: u32) void {
    machine.pending_op_id = op_id;
    machine.pending_op_pc = machine.current.pc;
}

fn clear_pending(machine: anytype) void {
    machine.pending_op_id = 0;
    machine.pending_op_pc = 0;
}

fn bytes_to_words(count: usize) usize {
    return if (count == 0) 0 else (count + 7) / 8;
}

fn bytes_payload(machine: anytype, base: usize, cap: usize) []u8 {
    const words = bytes_to_words(cap);
    const raw = machine.memory.data[base .. base + words];
    return std.mem.sliceAsBytes(raw)[0..cap];
}

fn bytes_info(machine: anytype, handle: u64, debug_checks: bool) ?bytes_view {
    const ptr = to_usize(handle, debug_checks, machine) orelse return null;
    if (debug_checks and ptr + bytes_header_words > machine.memory.data.len) {
        fail(machine, "bytes handle out of range");
        return null;
    }
    const data_ptr = @as(usize, @intCast(machine.memory.read(ptr)));
    const len = @as(usize, @intCast(machine.memory.read(ptr + 1)));
    if (debug_checks and data_ptr + len > machine.memory.data.len) {
        fail(machine, "bytes payload out of range");
        return null;
    }
    return .{
        .ptr = ptr,
        .data_ptr = data_ptr,
        .len = len,
    };
}

fn bytes_alloc(machine: anytype, cap: usize, debug_checks: bool) ?usize {
    const data_words = if (cap == 0) 1 else cap;
    const data_ptr = alloc_block(machine, data_words, debug_checks) orelse return null;
    const ptr = alloc_block(machine, bytes_header_words, debug_checks) orelse return null;
    machine.memory.write(ptr, @intCast(data_ptr));
    machine.memory.write(ptr + 1, @intCast(cap));
    return ptr;
}

fn string_info(machine: anytype, handle: u64, debug_checks: bool) ?string_view {
    const ptr = to_usize(handle, debug_checks, machine) orelse return null;
    if (debug_checks and ptr + string_header_words > machine.memory.data.len) {
        std.debug.print("string handle out of range: {d} mem={d} data={d}\n", .{ ptr, machine.memory.data.len, machine.data.len });
        fail(machine, "string handle out of range");
        return null;
    }
    const len = @as(usize, @intCast(machine.memory.read(ptr)));
    const cap = @as(usize, @intCast(machine.memory.read(ptr + 1)));
    const payload_ptr = ptr + string_header_words;
    if (debug_checks and payload_ptr + bytes_to_words(cap) > machine.memory.data.len) {
        fail(machine, "string payload out of range");
        return null;
    }
    const payload = bytes_payload(machine, payload_ptr, cap);
    return .{
        .ptr = ptr,
        .len = len,
        .cap = cap,
        .payload_ptr = payload_ptr,
        .payload = payload,
    };
}

fn string_bytes(machine: anytype, value: u64, debug_checks: bool) ?[]const u8 {
    if (value < @as(u64, @intCast(machine.data.len))) {
        if (data_entry(machine, value, debug_checks)) |entry| {
            if (entry.kind == .string) return entry.bytes;
            if (debug_checks) fail(machine, "string data kind mismatch");
            return null;
        }
    }
    const info = string_info(machine, value, debug_checks) orelse return null;
    return info.payload[0..info.len];
}

fn string_alloc(machine: anytype, cap: usize, debug_checks: bool) ?usize {
    const words = string_header_words + bytes_to_words(cap);
    const ptr = alloc_block(machine, if (words == 0) 1 else words, debug_checks) orelse return null;
    machine.memory.write(ptr, 0);
    machine.memory.write(ptr + 1, @intCast(cap));
    return ptr;
}

fn slice_info(machine: anytype, handle: u64, debug_checks: bool) ?slice_view {
    const ptr = to_usize(handle, debug_checks, machine) orelse return null;
    if (debug_checks and ptr + slice_header_words > machine.memory.data.len) {
        fail(machine, "slice handle out of range");
        return null;
    }
    const data_ptr = machine.memory.read(ptr);
    const len = @as(usize, @intCast(machine.memory.read(ptr + 1)));
    return .{
        .ptr = ptr,
        .data_ptr = data_ptr,
        .len = len,
    };
}

fn slice_alloc(machine: anytype, data_ptr: u64, len: usize, debug_checks: bool) ?usize {
    const ptr = alloc_block(machine, slice_header_words, debug_checks) orelse return null;
    machine.memory.write(ptr, data_ptr);
    machine.memory.write(ptr + 1, @intCast(len));
    return ptr;
}

fn buf_info(machine: anytype, handle: u64, debug_checks: bool) ?buf_view {
    const ptr = to_usize(handle, debug_checks, machine) orelse return null;
    if (debug_checks and ptr + buf_header_words > machine.memory.data.len) {
        fail(machine, "buf handle out of range");
        return null;
    }
    const read = @as(usize, @intCast(machine.memory.read(ptr)));
    const write = @as(usize, @intCast(machine.memory.read(ptr + 1)));
    const cap = @as(usize, @intCast(machine.memory.read(ptr + 2)));
    const payload_ptr = ptr + buf_header_words;
    if (debug_checks and payload_ptr + bytes_to_words(cap) > machine.memory.data.len) {
        fail(machine, "buf payload out of range");
        return null;
    }
    const payload = bytes_payload(machine, payload_ptr, cap);
    return .{
        .ptr = ptr,
        .read = read,
        .write = write,
        .cap = cap,
        .payload_ptr = payload_ptr,
        .payload = payload,
    };
}

fn buf_alloc(machine: anytype, cap: usize, debug_checks: bool) ?usize {
    const words = buf_header_words + bytes_to_words(cap);
    const ptr = alloc_block(machine, if (words == 0) 1 else words, debug_checks) orelse return null;
    machine.memory.write(ptr, 0);
    machine.memory.write(ptr + 1, 0);
    machine.memory.write(ptr + 2, @intCast(cap));
    return ptr;
}

fn arena_info(machine: anytype, handle: u64, debug_checks: bool) ?arena_view {
    const ptr = to_usize(handle, debug_checks, machine) orelse return null;
    if (debug_checks and ptr + arena_header_words > machine.memory.data.len) {
        fail(machine, "arena handle out of range");
        return null;
    }
    const cap = @as(usize, @intCast(machine.memory.read(ptr)));
    const offset = @as(usize, @intCast(machine.memory.read(ptr + 1)));
    const payload_ptr = ptr + arena_header_words;
    if (debug_checks and payload_ptr + bytes_to_words(cap) > machine.memory.data.len) {
        fail(machine, "arena payload out of range");
        return null;
    }
    return .{
        .ptr = ptr,
        .cap = cap,
        .offset = offset,
        .payload_ptr = payload_ptr,
    };
}

fn arena_alloc(machine: anytype, cap: usize, debug_checks: bool) ?usize {
    const words = arena_header_words + bytes_to_words(cap);
    const ptr = alloc_block(machine, if (words == 0) 1 else words, debug_checks) orelse return null;
    machine.memory.write(ptr, @intCast(cap));
    machine.memory.write(ptr + 1, 0);
    return ptr;
}

fn align8(size: usize) usize {
    return (size + 7) & ~@as(usize, 7);
}

fn buf_write_slice(machine: anytype, handle: u64, debug_checks: bool) ?[]u8 {
    const info = buf_info(machine, handle, debug_checks) orelse return null;
    if (info.write >= info.cap) return info.payload[info.cap..info.cap];
    return info.payload[info.write..info.cap];
}

fn buf_read_slice(machine: anytype, handle: u64, debug_checks: bool) ?[]u8 {
    const info = buf_info(machine, handle, debug_checks) orelse return null;
    if (info.read >= info.write) return info.payload[info.write..info.write];
    return info.payload[info.read..info.write];
}

fn buf_write_advance_raw(machine: anytype, handle: u64, count_val: u64, debug_checks: bool) bool {
    const info = buf_info(machine, handle, debug_checks) orelse return false;
    const count = to_usize(count_val, debug_checks, machine) orelse return false;
    if (info.write + count > info.cap) {
        if (debug_checks) fail(machine, "buf write overflow");
        return false;
    }
    machine.memory.write(info.ptr + 1, @intCast(info.write + count));
    return true;
}

fn buf_read_advance_raw(machine: anytype, handle: u64, count_val: u64, debug_checks: bool) bool {
    const info = buf_info(machine, handle, debug_checks) orelse return false;
    const count = to_usize(count_val, debug_checks, machine) orelse return false;
    if (info.read + count > info.write) {
        if (debug_checks) fail(machine, "buf read overflow");
        return false;
    }
    const next_read = info.read + count;
    if (next_read == info.write) {
        machine.memory.write(info.ptr, 0);
        machine.memory.write(info.ptr + 1, 0);
        return true;
    }
    machine.memory.write(info.ptr, @intCast(next_read));
    return true;
}

fn err_code(err: anyerror) u64 {
    return @intCast(@intFromError(err));
}

fn result_count(value: isize) ?u64 {
    if (value < 0) return null;
    return @intCast(value);
}

fn to_fd(value: u64, debug_checks: bool, machine: anytype) ?std.posix.fd_t {
    if (value > std.math.maxInt(std.posix.fd_t)) {
        if (debug_checks) fail(machine, "fd out of range");
        return null;
    }
    return @intCast(value);
}

fn to_usize(value: u64, debug_checks: bool, machine: anytype) ?usize {
    if (value > std.math.maxInt(usize)) {
        if (debug_checks) fail(machine, "pointer out of range");
        return null;
    }
    return @intCast(value);
}

fn fail(machine: anytype, message: []const u8) void {
    std.debug.print("vm error: {s}\n", .{message});
    machine.halted = true;
}

fn alloc_block(machine: anytype, words: usize, debug_checks: bool) ?usize {
    const header_words: usize = 3;
    const null_ptr = std.math.maxInt(usize);

    if (words == 0) {
        if (debug_checks) fail(machine, "alloc size must be > 0");
        return null;
    }

    var prev: ?usize = null;
    var cursor = machine.free_head;
    while (cursor != null_ptr) {
        const size = @as(usize, @intCast(machine.memory.read(cursor)));
        const flags = machine.memory.read(cursor + 1);
        const next = @as(usize, @intCast(machine.memory.read(cursor + 2)));
        if (debug_checks and flags != 1) {
            fail(machine, "corrupt free list");
            return null;
        }
        if (size >= words) {
            if (prev) |prev_idx| {
                machine.memory.write(prev_idx + 2, @intCast(next));
            } else {
                machine.free_head = next;
            }
            machine.memory.write(cursor + 1, 0);
            machine.memory.write(cursor + 2, @intCast(null_ptr));
            return cursor + header_words;
        }
        prev = cursor;
        cursor = next;
    }

    const total = header_words + words;
    if (machine.heap_top < total or machine.heap_top - total <= machine.current.sp) {
        if (debug_checks) fail(machine, "out of memory");
        return null;
    }

    const header = machine.heap_top - total;
    machine.heap_top = header;
    machine.memory.write(header + 0, @intCast(words));
    machine.memory.write(header + 1, 0);
    machine.memory.write(header + 2, @intCast(null_ptr));
    return header + header_words;
}

fn free_block(machine: anytype, ptr: usize, debug_checks: bool) void {
    const header_words: usize = 3;

    if (ptr < header_words or ptr >= machine.memory.data.len) {
        if (debug_checks) fail(machine, "free invalid pointer");
        return;
    }

    if (ptr < machine.heap_top) {
        if (debug_checks) fail(machine, "free of stack pointer");
        return;
    }

    const header = ptr - header_words;
    const flags = machine.memory.read(header + 1);
    if (debug_checks and flags != 0) {
        fail(machine, "double free");
        return;
    }
    machine.memory.write(header + 1, 1);
    machine.memory.write(header + 2, @intCast(machine.free_head));
    machine.free_head = header;
}

fn heap_header_for_ptr(machine: anytype, ptr: usize) ?usize {
    const header_words: usize = 3;
    if (ptr < header_words) return null;
    var header = ptr - header_words;
    while (true) {
        if (header < machine.heap_top) return null;
        const size = @as(usize, @intCast(machine.memory.read(header)));
        if (size > 0) {
            const start = header + header_words;
            const end_info = @addWithOverflow(start, size);
            if (end_info[1] != 0) {
                if (header == 0) break;
                header -= 1;
                continue;
            }
            const end = end_info[0];
            if (end < start) {
                if (header == 0) break;
                header -= 1;
                continue;
            }
            if (ptr >= start and ptr < end) return header;
        }
        if (header == 0) break;
        header -= 1;
    }
    return null;
}

fn deref_ptr(machine: anytype, ptr: usize, debug_checks: bool) ?u64 {
    if (ptr >= machine.memory.data.len) {
        if (debug_checks) fail(machine, "deref out of bounds");
        return null;
    }
    if (ptr < machine.heap_top) {
        if (debug_checks and ptr >= machine.current.sp) {
            fail(machine, "deref invalid stack pointer");
            return null;
        }
        return machine.memory.read(ptr);
    }

    const header = heap_header_for_ptr(machine, ptr) orelse {
        if (debug_checks) fail(machine, "deref invalid heap pointer");
        return null;
    };
    const flags = machine.memory.read(header + 1);
    if (debug_checks and flags != 0) {
        fail(machine, "use after free");
        return null;
    }
    return machine.memory.read(ptr);
}

fn store_ptr(machine: anytype, ptr: usize, value: u64, debug_checks: bool) void {
    if (ptr >= machine.memory.data.len) {
        if (debug_checks) fail(machine, "store out of bounds");
        return;
    }
    if (ptr < machine.heap_top) {
        if (debug_checks and ptr >= machine.current.sp) {
            fail(machine, "store invalid stack pointer");
            return;
        }
        machine.memory.write(ptr, value);
        return;
    }

    const header = heap_header_for_ptr(machine, ptr) orelse {
        if (debug_checks) fail(machine, "store invalid heap pointer");
        return;
    };
    const flags = machine.memory.read(header + 1);
    if (debug_checks and flags != 0) {
        fail(machine, "use after free");
        return;
    }
    machine.memory.write(ptr, value);
}
