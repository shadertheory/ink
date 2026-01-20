const std = @import("std");

fn fail(machine: anytype, message: []const u8) void {
    std.debug.print("vm error: {s}\n", .{message});
    machine.halted = true;
}

pub fn bytes_to_words(count: usize) usize {
    return if (count == 0) 0 else (count + 7) / 8;
}

pub fn alloc_block(machine: anytype, words: usize, debug_checks: bool) ?usize {
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

pub fn free_block(machine: anytype, ptr: usize, debug_checks: bool) void {
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
    const null_ptr = std.math.maxInt(usize);
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
            if (ptr >= start and ptr < end) {
                const flags = machine.memory.read(header + 1);
                if (flags == 0) {
                    const next = @as(usize, @intCast(machine.memory.read(header + 2)));
                    if (next != null_ptr) {
                        if (header == 0) break;
                        header -= 1;
                        continue;
                    }
                } else if (flags != 1) {
                    if (header == 0) break;
                    header -= 1;
                    continue;
                }
                return header;
            }
        }
        if (header == 0) break;
        header -= 1;
    }
    return null;
}

pub fn deref_ptr(machine: anytype, ptr: usize, debug_checks: bool) ?u64 {
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

pub fn store_ptr(machine: anytype, ptr: usize, value: u64, debug_checks: bool) void {
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
