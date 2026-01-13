const std = @import("std");

const mem_allocator = std.mem.Allocator;

pub const format_version: u32 = 3;
pub const magic = "INKB";

pub const data_kind = enum(u8) {
    string = 1,
};

pub const data_entry = struct {
    kind: data_kind,
    bytes: []const u8,
};

pub const debug_function = struct {
    entry_pc: u64,
    name: []const u8,
};

pub const debug_info = struct {
    functions: []debug_function,
};

pub const program = struct {
    bytecode: []u8,
    constants: []u64,
    data: []data_entry,
    foreigns: []const []const u8,
    debug: ?debug_info = null,

    pub fn deinit(self: *program, allocator: mem_allocator) void {
        allocator.free(self.bytecode);
        allocator.free(self.constants);
        for (self.data) |entry| {
            allocator.free(entry.bytes);
        }
        allocator.free(self.data);
        for (self.foreigns) |name| {
            allocator.free(name);
        }
        allocator.free(self.foreigns);
        if (self.debug) |*dbg| {
            for (dbg.functions) |func| allocator.free(func.name);
            allocator.free(dbg.functions);
            self.debug = null;
        }
    }
};

pub const read_error = error{
    InvalidMagic,
    InvalidVersion,
    UnexpectedEof,
    LengthOverflow,
};

pub fn write_file(
    path: []const u8,
    bytecode: []const u8,
    constants: []const u64,
    data: []const data_entry,
    foreigns: []const []const u8,
    debug: ?debug_info,
) !void {
    var file = try std.fs.cwd().createFile(path, .{ .truncate = true });
    defer file.close();
    var buffer = [_]u8{0} ** 8192;
    var file_writer = file.writer(buffer[0..]);
    const writer = &file_writer.interface;
    try write(writer, bytecode, constants, data, foreigns, debug);
    try writer.flush();
}

pub fn write(
    writer: anytype,
    bytecode: []const u8,
    constants: []const u64,
    data: []const data_entry,
    foreigns: []const []const u8,
    debug: ?debug_info,
) !void {
    try writer.writeAll(magic);
    try write_int(writer, u32, format_version);
    try write_int(writer, u64, @as(u64, @intCast(constants.len)));
    try write_int(writer, u64, @as(u64, @intCast(data.len)));
    try write_int(writer, u64, @as(u64, @intCast(foreigns.len)));
    try write_int(writer, u64, @as(u64, @intCast(bytecode.len)));
    const debug_functions = if (debug) |info| info.functions else &[_]debug_function{};
    try write_int(writer, u64, @as(u64, @intCast(debug_functions.len)));
    for (constants) |value| {
        try write_int(writer, u64, value);
    }
    for (data) |entry| {
        try write_int(writer, u8, @intFromEnum(entry.kind));
        try write_int(writer, u64, @as(u64, @intCast(entry.bytes.len)));
        try writer.writeAll(entry.bytes);
    }
    for (foreigns) |name| {
        try write_int(writer, u64, @as(u64, @intCast(name.len)));
        try writer.writeAll(name);
    }
    try writer.writeAll(bytecode);
    for (debug_functions) |func| {
        try write_int(writer, u64, func.entry_pc);
        try write_int(writer, u64, @as(u64, @intCast(func.name.len)));
        try writer.writeAll(func.name);
    }
}

pub fn read_file(allocator: mem_allocator, path: []const u8) !program {
    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    var buffer = [_]u8{0} ** 8192;
    var file_reader = file.reader(buffer[0..]);
    const reader = &file_reader.interface;
    return read(allocator, reader);
}

pub fn read(allocator: mem_allocator, reader: anytype) !program {
    var magic_buf: [magic.len]u8 = undefined;
    try read_exact(reader, magic_buf[0..]);
    if (!std.mem.eql(u8, magic_buf[0..], magic)) return error.InvalidMagic;

    const version = try read_int(reader, u32);
    switch (version) {
        1 => {
            const constants_len_v1 = try read_int(reader, u64);
            const bytecode_len_v1 = try read_int(reader, u64);
            if (constants_len_v1 > std.math.maxInt(usize)) return error.LengthOverflow;
            if (bytecode_len_v1 > std.math.maxInt(usize)) return error.LengthOverflow;

            const constants_v1 = try allocator.alloc(u64, @intCast(constants_len_v1));
            errdefer allocator.free(constants_v1);
            for (constants_v1) |*value| {
                value.* = try read_int(reader, u64);
            }

            const bytecode_v1 = try allocator.alloc(u8, @intCast(bytecode_len_v1));
            errdefer allocator.free(bytecode_v1);
            try read_exact(reader, bytecode_v1);

            const empty_data = try allocator.alloc(data_entry, 0);
            errdefer allocator.free(empty_data);
            const empty_foreigns = try allocator.alloc([]const u8, 0);
            errdefer allocator.free(empty_foreigns);
            return .{
                .bytecode = bytecode_v1,
                .constants = constants_v1,
                .data = empty_data,
                .foreigns = empty_foreigns,
                .debug = null,
            };
        },
        2, 3 => {
            const constants_len = try read_int(reader, u64);
            const data_len = try read_int(reader, u64);
            const foreigns_len = try read_int(reader, u64);
            const bytecode_len = try read_int(reader, u64);
            const debug_len = if (version >= 3) try read_int(reader, u64) else 0;

            if (constants_len > std.math.maxInt(usize)) return error.LengthOverflow;
            if (data_len > std.math.maxInt(usize)) return error.LengthOverflow;
            if (foreigns_len > std.math.maxInt(usize)) return error.LengthOverflow;
            if (bytecode_len > std.math.maxInt(usize)) return error.LengthOverflow;
            if (debug_len > std.math.maxInt(usize)) return error.LengthOverflow;

            const constants = try allocator.alloc(u64, @intCast(constants_len));
            errdefer allocator.free(constants);
            for (constants) |*value| {
                value.* = try read_int(reader, u64);
            }

            var data_list = std.array_list.Managed(data_entry).init(allocator);
            errdefer {
                for (data_list.items) |entry| allocator.free(entry.bytes);
                data_list.deinit();
            }
            var data_idx: u64 = 0;
            while (data_idx < data_len) : (data_idx += 1) {
                const kind_raw = try read_int(reader, u8);
                const kind: data_kind = @enumFromInt(kind_raw);
                const len = try read_int(reader, u64);
                if (len > std.math.maxInt(usize)) return error.LengthOverflow;
                const bytes = try allocator.alloc(u8, @intCast(len));
                try read_exact(reader, bytes);
                try data_list.append(.{ .kind = kind, .bytes = bytes });
            }

            var foreign_list = std.array_list.Managed([]const u8).init(allocator);
            errdefer {
                for (foreign_list.items) |name| allocator.free(name);
                foreign_list.deinit();
            }
            var foreign_idx: u64 = 0;
            while (foreign_idx < foreigns_len) : (foreign_idx += 1) {
                const len = try read_int(reader, u64);
                if (len > std.math.maxInt(usize)) return error.LengthOverflow;
                const buf = try allocator.alloc(u8, @intCast(len));
                try read_exact(reader, buf);
                try foreign_list.append(buf);
            }

            const bytecode = try allocator.alloc(u8, @intCast(bytecode_len));
            errdefer allocator.free(bytecode);
            try read_exact(reader, bytecode);

            var debug_info_opt: ?debug_info = null;
            if (debug_len != 0) {
                var debug_list = std.array_list.Managed(debug_function).init(allocator);
                errdefer {
                    for (debug_list.items) |func| allocator.free(func.name);
                    debug_list.deinit();
                }
                var dbg_idx: u64 = 0;
                while (dbg_idx < debug_len) : (dbg_idx += 1) {
                    const entry_pc = try read_int(reader, u64);
                    const name_len = try read_int(reader, u64);
                    if (name_len > std.math.maxInt(usize)) return error.LengthOverflow;
                    const name_buf = try allocator.alloc(u8, @intCast(name_len));
                    try read_exact(reader, name_buf);
                    try debug_list.append(.{ .entry_pc = entry_pc, .name = name_buf });
                }
                debug_info_opt = .{ .functions = try debug_list.toOwnedSlice() };
            }

            const data = try data_list.toOwnedSlice();
            const foreigns = try foreign_list.toOwnedSlice();
            return .{
                .bytecode = bytecode,
                .constants = constants,
                .data = data,
                .foreigns = foreigns,
                .debug = debug_info_opt,
            };
        },
        else => return error.InvalidVersion,
    }
}

fn write_int(writer: anytype, comptime T: type, value: T) !void {
    var buf: [@sizeOf(T)]u8 = undefined;
    std.mem.writeInt(T, &buf, value, .little);
    try writer.writeAll(buf[0..]);
}

fn read_int(reader: anytype, comptime T: type) !T {
    var buf: [@sizeOf(T)]u8 = undefined;
    try read_exact(reader, buf[0..]);
    return std.mem.readInt(T, &buf, .little);
}

fn read_exact(reader: anytype, buf: []u8) !void {
    try reader.readSliceAll(buf);
}
