const std = @import("std");
const builtin = @import("builtin");
const posix = std.posix;

const mem_allocator = std.mem.Allocator;
const memory_bytes_per_line: usize = 16;
const default_memory_read_bytes: i64 = 128;

pub const Options = struct {
    program: ?[]const u8 = null,
    sim: bool = false,
    scenario: ?[]const u8 = null,
    replay: ?[]const u8 = null,
    attach: ?[]const u8 = null,
};

pub fn run(allocator: mem_allocator, options: Options) !void {
    var term = try Terminal.init();
    defer term.deinit();

    var app = try App.init(allocator, &term, options);
    defer app.deinit();

    try app.loop();
}

pub const Terminal = struct {
    fd: posix.fd_t,
    out_file: std.fs.File,
    out_buf: [16384]u8 = undefined,
    out_writer: std.fs.File.Writer,
    orig: posix.termios,
    raw: bool = false,
    alt: bool = false,

    pub const InitOptions = struct {
        raw: bool = true,
        alt: bool = true,
        wrap: bool = false,
        cursor: bool = false,
        mouse: bool = true,
        clear: bool = true,
        isig: bool = false,
    };

    pub fn init() !Terminal {
        return initWithOptions(.{});
    }

    pub fn initWithOptions(opts: InitOptions) !Terminal {
        const stdin_file = std.fs.File.stdin();
        const stdout_file = std.fs.File.stdout();
        var term = Terminal{
            .fd = stdin_file.handle,
            .out_file = stdout_file,
            .out_writer = stdout_file.writer(&[_]u8{}),
            .orig = try posix.tcgetattr(stdin_file.handle),
        };
        term.out_writer = stdout_file.writer(term.out_buf[0..]);
        if (opts.raw) {
            try term.enterRawWithOptions(.{ .isig = opts.isig });
        }
        if (opts.alt) try term.enterAlt();
        if (opts.wrap) {
            try term.enableWrap();
        } else {
            try term.disableWrap();
        }
        if (opts.cursor) {
            try term.showCursor();
        } else {
            try term.hideCursor();
        }
        if (opts.mouse) {
            try term.enableMouse();
        } else {
            try term.disableMouse();
        }
        if (opts.clear) try term.clear();
        return term;
    }

    pub fn deinit(self: *Terminal) void {
        self.disableMouse() catch {};
        self.showCursor() catch {};
        self.enableWrap() catch {};
        self.exitAlt() catch {};
        self.leaveRaw();
    }

    pub fn writer(self: *Terminal) *std.Io.Writer {
        return &self.out_writer.interface;
    }

    const RawOptions = struct {
        isig: bool = false,
    };

    fn enterRawWithOptions(self: *Terminal, opts: RawOptions) !void {
        if (self.raw) return;
        var raw = self.orig;
        clearIf(&raw.iflag, "IGNBRK");
        clearIf(&raw.iflag, "BRKINT");
        clearIf(&raw.iflag, "PARMRK");
        clearIf(&raw.iflag, "ISTRIP");
        clearIf(&raw.iflag, "INLCR");
        clearIf(&raw.iflag, "IGNCR");
        clearIf(&raw.iflag, "ICRNL");
        clearIf(&raw.iflag, "IXON");
        clearIf(&raw.iflag, "IXOFF");
        clearIf(&raw.iflag, "IXANY");

        clearIf(&raw.oflag, "OPOST");

        clearIf(&raw.lflag, "ECHO");
        clearIf(&raw.lflag, "ECHONL");
        clearIf(&raw.lflag, "ICANON");
        if (!opts.isig) {
            clearIf(&raw.lflag, "ISIG");
        }
        clearIf(&raw.lflag, "IEXTEN");

        clearIf(&raw.cflag, "PARENB");
        setEnumIf(&raw.cflag, "CSIZE", "CS8");
        setBoolIf(&raw.cflag, "CREAD", true);
        raw.cc[@intFromEnum(posix.V.MIN)] = 0;
        raw.cc[@intFromEnum(posix.V.TIME)] = 0;
        try posix.tcsetattr(self.fd, .FLUSH, raw);
        self.raw = true;
    }

    fn enterRaw(self: *Terminal) !void {
        return self.enterRawWithOptions(.{});
    }

    fn leaveRaw(self: *Terminal) void {
        if (!self.raw) return;
        posix.tcsetattr(self.fd, .FLUSH, self.orig) catch {};
        self.raw = false;
    }

    pub fn enterAlt(self: *Terminal) !void {
        try self.writer().writeAll("\x1b[?1049h\x1b[?47h\x1b[?1047h");
        try self.writer().flush();
        self.alt = true;
    }

    pub fn exitAlt(self: *Terminal) !void {
        if (!self.alt) return;
        try self.writer().writeAll("\x1b[?1049l\x1b[?47l\x1b[?1047l");
        try self.writer().flush();
        self.alt = false;
    }

    pub fn hideCursor(self: *Terminal) !void {
        try self.writer().writeAll("\x1b[?25l");
        try self.writer().flush();
    }

    pub fn showCursor(self: *Terminal) !void {
        try self.writer().writeAll("\x1b[?25h");
        try self.writer().flush();
    }

    pub fn clear(self: *Terminal) !void {
        try self.writer().writeAll("\x1b[2J\x1b[H");
        try self.writer().flush();
    }

    pub fn enableMouse(self: *Terminal) !void {
        try self.writer().writeAll("\x1b[?1000h\x1b[?1002h\x1b[?1003h\x1b[?1006h\x1b[?1007h");
        try self.writer().flush();
    }

    pub fn disableMouse(self: *Terminal) !void {
        try self.writer().writeAll("\x1b[?1000l\x1b[?1002l\x1b[?1003l\x1b[?1006l\x1b[?1007l");
        try self.writer().flush();
    }

    pub fn disableWrap(self: *Terminal) !void {
        try self.writer().writeAll("\x1b[?7l");
        try self.writer().flush();
    }

    pub fn enableWrap(self: *Terminal) !void {
        try self.writer().writeAll("\x1b[?7h");
        try self.writer().flush();
    }

    pub fn size(self: *Terminal) Size {
        var winsize: posix.winsize = .{
            .row = 0,
            .col = 0,
            .xpixel = 0,
            .ypixel = 0,
        };
        const err = posix.system.ioctl(self.fd, posix.T.IOCGWINSZ, @intFromPtr(&winsize));
        if (posix.errno(err) != .SUCCESS or winsize.col == 0 or winsize.row == 0) {
            return .{ .cols = 80, .rows = 25 };
        }
        return .{ .cols = winsize.col, .rows = winsize.row };
    }
};

pub const Size = struct {
    cols: u16,
    rows: u16,
};

pub const Point = struct {
    x: i32,
    y: i32,
};

pub const Rect = struct {
    x: i32,
    y: i32,
    w: i32,
    h: i32,

    pub fn contains(self: Rect, p: Point) bool {
        return p.x >= self.x and p.y >= self.y and p.x < self.x + self.w and p.y < self.y + self.h;
    }
};

pub const Color = struct {
    r: u8,
    g: u8,
    b: u8,
};

pub const Style = struct {
    fg: Color,
    bg: Color,
    bold: bool = false,
    dim: bool = false,
    underline: bool = false,

    pub fn eq(a: Style, b: Style) bool {
        return a.fg.r == b.fg.r and a.fg.g == b.fg.g and a.fg.b == b.fg.b and
            a.bg.r == b.bg.r and a.bg.g == b.bg.g and a.bg.b == b.bg.b and
            a.bold == b.bold and a.dim == b.dim and a.underline == b.underline;
    }
};

pub const Cell = struct {
    ch: u21,
    style: Style,
    skip: bool = false,
};

pub const Border = struct {
    pub const h = '─';
    pub const v = '│';
    pub const tl = '┌';
    pub const tr = '┐';
    pub const bl = '└';
    pub const br = '┘';
    pub const tee_up = '┴';
    pub const tee_down = '┬';
    pub const tee_left = '┤';
    pub const tee_right = '├';
    pub const cross = '┼';
};

const BorderBits = struct {
    const up: u4 = 1;
    const right: u4 = 2;
    const down: u4 = 4;
    const left: u4 = 8;
};

fn borderMask(ch: u21) u4 {
    return switch (ch) {
        Border.h => BorderBits.left | BorderBits.right,
        Border.v => BorderBits.up | BorderBits.down,
        Border.tl => BorderBits.right | BorderBits.down,
        Border.tr => BorderBits.left | BorderBits.down,
        Border.bl => BorderBits.right | BorderBits.up,
        Border.br => BorderBits.left | BorderBits.up,
        Border.tee_right => BorderBits.up | BorderBits.down | BorderBits.right,
        Border.tee_left => BorderBits.up | BorderBits.down | BorderBits.left,
        Border.tee_down => BorderBits.left | BorderBits.right | BorderBits.down,
        Border.tee_up => BorderBits.left | BorderBits.right | BorderBits.up,
        Border.cross => BorderBits.up | BorderBits.right | BorderBits.down | BorderBits.left,
        else => 0,
    };
}

fn borderRune(mask: u4) u21 {
    return switch (mask) {
        BorderBits.left | BorderBits.right => Border.h,
        BorderBits.up | BorderBits.down => Border.v,
        BorderBits.right | BorderBits.down => Border.tl,
        BorderBits.left | BorderBits.down => Border.tr,
        BorderBits.right | BorderBits.up => Border.bl,
        BorderBits.left | BorderBits.up => Border.br,
        BorderBits.up | BorderBits.down | BorderBits.right => Border.tee_right,
        BorderBits.up | BorderBits.down | BorderBits.left => Border.tee_left,
        BorderBits.left | BorderBits.right | BorderBits.down => Border.tee_down,
        BorderBits.left | BorderBits.right | BorderBits.up => Border.tee_up,
        BorderBits.up | BorderBits.right | BorderBits.down | BorderBits.left => Border.cross,
        else => Border.h,
    };
}

fn chooseBorderStyle(existing: Style, incoming: Style) Style {
    if (Style.eq(existing, incoming)) return existing;
    if (existing.bold and !incoming.bold) return existing;
    if (!existing.bold and incoming.bold) return incoming;
    return incoming;
}

pub const Canvas = struct {
    allocator: mem_allocator,
    width: usize,
    height: usize,
    cells: []Cell,
    clear_style: Style,

    pub fn init(allocator: mem_allocator, width: usize, height: usize, clear_style: Style) !Canvas {
        const cells = try allocator.alloc(Cell, width * height);
        for (cells) |*cell| {
            cell.* = .{ .ch = ' ', .style = clear_style, .skip = false };
        }
        return .{ .allocator = allocator, .width = width, .height = height, .cells = cells, .clear_style = clear_style };
    }

    pub fn deinit(self: *Canvas) void {
        self.allocator.free(self.cells);
    }

    pub fn resize(self: *Canvas, width: usize, height: usize) !void {
        if (self.width == width and self.height == height) return;
        self.allocator.free(self.cells);
        self.width = width;
        self.height = height;
        self.cells = try self.allocator.alloc(Cell, width * height);
        self.clear();
    }

    pub fn clear(self: *Canvas) void {
        for (self.cells) |*cell| {
            cell.* = .{ .ch = ' ', .style = self.clear_style, .skip = false };
        }
    }

    pub fn put(self: *Canvas, x: i32, y: i32, ch: u21, style: Style) void {
        if (x < 0 or y < 0) return;
        const ux: usize = @intCast(x);
        const uy: usize = @intCast(y);
        if (ux >= self.width or uy >= self.height) return;
        self.cells[uy * self.width + ux] = .{ .ch = ch, .style = style, .skip = false };
        const width = cellWidth(ch);
        if (width > 1 and ux + 1 < self.width) {
            self.cells[uy * self.width + ux + 1] = .{ .ch = ' ', .style = style, .skip = true };
        }
    }

    pub fn putBorder(self: *Canvas, x: i32, y: i32, ch: u21, style: Style) void {
        if (x < 0 or y < 0) return;
        const ux: usize = @intCast(x);
        const uy: usize = @intCast(y);
        if (ux >= self.width or uy >= self.height) return;

        const idx = uy * self.width + ux;
        const incoming_mask = borderMask(ch);
        if (incoming_mask == 0) {
            self.cells[idx] = .{ .ch = ch, .style = style, .skip = false };
            return;
        }

        const existing = self.cells[idx];
        const existing_mask = borderMask(existing.ch);
        const merged_mask = if (existing_mask != 0) existing_mask | incoming_mask else incoming_mask;
        const merged_ch = borderRune(merged_mask);
        const merged_style = if (existing_mask != 0)
            chooseBorderStyle(existing.style, style)
        else
            style;
        self.cells[idx] = .{ .ch = merged_ch, .style = merged_style, .skip = false };
    }

    pub fn write(self: *Canvas, x: i32, y: i32, text: []const u8, style: Style) void {
        var col = x;
        var idx: usize = 0;
        while (nextCodepointLossy(text, &idx)) |cp| {
            self.put(col, y, cp, style);
            col += cellWidth(cp);
        }
    }

    pub fn writeClipped(self: *Canvas, x: i32, y: i32, max_w: i32, text: []const u8, style: Style) void {
        if (max_w <= 0) return;
        var col = x;
        var remaining = max_w;
        var idx: usize = 0;
        while (remaining > 0) {
            const cp = nextCodepointLossy(text, &idx) orelse break;
            const width = cellWidth(cp);
            if (width <= 0) continue;
            if (width > remaining) break;
            self.put(col, y, cp, style);
            col += width;
            remaining -= width;
        }
    }

    pub fn writeClippedExpanded(self: *Canvas, x: i32, y: i32, max_w: i32, text: []const u8, style: Style) void {
        if (max_w <= 0) return;
        const tab_width: i32 = 4;
        var col = x;
        var remaining = max_w;
        var offset: i32 = 0;
        var idx: usize = 0;
        while (remaining > 0) {
            var cp = nextCodepointLossy(text, &idx) orelse break;
            if (cp == '\t') {
                const advance = tab_width - @mod(offset, tab_width);
                var step: i32 = 0;
                while (step < advance and remaining > 0) : (step += 1) {
                    self.put(col, y, ' ', style);
                    col += 1;
                    remaining -= 1;
                    offset += 1;
                }
                continue;
            }
            if (cp < 0x20) cp = ' ';
            const width = cellWidth(cp);
            if (width <= 0) continue;
            if (width > remaining) break;
            self.put(col, y, cp, style);
            col += width;
            remaining -= width;
            offset += width;
        }
    }

    pub fn fill(self: *Canvas, rect: Rect, ch: u21, style: Style) void {
        var y = rect.y;
        while (y < rect.y + rect.h) : (y += 1) {
            var x = rect.x;
            while (x < rect.x + rect.w) : (x += 1) {
                if (x < 0 or y < 0) continue;
                const ux: usize = @intCast(x);
                const uy: usize = @intCast(y);
                if (ux >= self.width or uy >= self.height) continue;
                self.cells[uy * self.width + ux] = .{ .ch = ch, .style = style, .skip = false };
            }
        }
    }
};

pub const Renderer = struct {
    writer: *std.Io.Writer,

    pub fn render(self: *Renderer, canvas: *Canvas) !void {
        var out = std.Io.Writer.Allocating.init(std.heap.page_allocator);
        defer out.deinit();
        try out.writer.writeAll("\x1b[H");

        var last_style: ?Style = null;
        var y: usize = 0;
        while (y < canvas.height) : (y += 1) {
            var x: usize = 0;
            while (x < canvas.width) : (x += 1) {
                const cell = canvas.cells[y * canvas.width + x];
                if (cell.skip) continue;
                if (last_style == null or !Style.eq(last_style.?, cell.style)) {
                    last_style = cell.style;
                    try emitStyle(&out.writer, cell.style);
                }
                try emitRune(&out.writer, cell.ch);
            }
            if (y + 1 < canvas.height) {
                try out.writer.writeAll("\r\n");
            }
        }
        try out.writer.writeAll("\x1b[0m");
        try self.writer.writeAll(out.writer.buffered());
        try self.writer.flush();
    }
};

fn emitStyle(writer: *std.Io.Writer, style: Style) !void {
    try writer.writeAll("\x1b[0m");
    if (style.bold) try writer.writeAll("\x1b[1m");
    if (style.dim) try writer.writeAll("\x1b[2m");
    if (style.underline) try writer.writeAll("\x1b[4m");
    try writer.print("\x1b[38;2;{d};{d};{d}m", .{ style.fg.r, style.fg.g, style.fg.b });
    try writer.print("\x1b[48;2;{d};{d};{d}m", .{ style.bg.r, style.bg.g, style.bg.b });
}

fn emitRune(writer: *std.Io.Writer, cp: u21) !void {
    var buf: [4]u8 = undefined;
    const len = std.unicode.utf8Encode(cp, &buf) catch return;
    try writer.writeAll(buf[0..len]);
}

fn clearIf(flags: anytype, comptime field: []const u8) void {
    if (@hasField(@TypeOf(flags.*), field)) {
        @field(flags.*, field) = false;
    }
}

fn setBoolIf(flags: anytype, comptime field: []const u8, value: bool) void {
    if (@hasField(@TypeOf(flags.*), field)) {
        @field(flags.*, field) = value;
    }
}

fn setEnumIf(flags: anytype, comptime field: []const u8, comptime tag: []const u8) void {
    if (@hasField(@TypeOf(flags.*), field)) {
        const FieldType = @TypeOf(@field(flags.*, field));
        @field(flags.*, field) = @field(FieldType, tag);
    }
}

const Theme = struct {
    bg0: Color,
    bg1: Color,
    bg2: Color,
    bg3: Color,
    fg0: Color,
    fg1: Color,
    fg2: Color,
    red: Color,
    green: Color,
    yellow: Color,
    blue: Color,
    purple: Color,
    aqua: Color,
    orange: Color,
    gray: Color,

    panel_border: Style,
    panel_header: Style,
    panel_header_active: Style,
    panel_text: Style,
    panel_dim: Style,
    status_left: Style,
    status_mid: Style,
    status_right: Style,
    accent: Style,
    accent_alt: Style,

    fn gruvboxHard() Theme {
        const bg0 = Color{ .r = 0x1d, .g = 0x20, .b = 0x21 };
        const bg1 = Color{ .r = 0x28, .g = 0x28, .b = 0x28 };
        const bg2 = Color{ .r = 0x3c, .g = 0x38, .b = 0x36 };
        const bg3 = Color{ .r = 0x50, .g = 0x49, .b = 0x45 };
        const fg0 = Color{ .r = 0xfb, .g = 0xf1, .b = 0xc7 };
        const fg1 = Color{ .r = 0xeb, .g = 0xdb, .b = 0xb2 };
        const fg2 = Color{ .r = 0xd5, .g = 0xc4, .b = 0xa1 };
        const red = Color{ .r = 0xcc, .g = 0x24, .b = 0x1d };
        const green = Color{ .r = 0x98, .g = 0x97, .b = 0x1a };
        const yellow = Color{ .r = 0xd7, .g = 0x99, .b = 0x21 };
        const blue = Color{ .r = 0x45, .g = 0x85, .b = 0x88 };
        const purple = Color{ .r = 0xb1, .g = 0x62, .b = 0x86 };
        const aqua = Color{ .r = 0x68, .g = 0x9d, .b = 0x6a };
        const orange = Color{ .r = 0xd6, .g = 0x5d, .b = 0x0e };
        const gray = Color{ .r = 0x92, .g = 0x83, .b = 0x74 };

        return .{
            .bg0 = bg0,
            .bg1 = bg1,
            .bg2 = bg2,
            .bg3 = bg3,
            .fg0 = fg0,
            .fg1 = fg1,
            .fg2 = fg2,
            .red = red,
            .green = green,
            .yellow = yellow,
            .blue = blue,
            .purple = purple,
            .aqua = aqua,
            .orange = orange,
            .gray = gray,
            .panel_border = .{ .fg = bg3, .bg = bg1 },
            .panel_header = .{ .fg = fg2, .bg = bg2, .bold = true },
            .panel_header_active = .{ .fg = fg0, .bg = blue, .bold = true },
            .panel_text = .{ .fg = fg1, .bg = bg1 },
            .panel_dim = .{ .fg = gray, .bg = bg1, .dim = true },
            .status_left = .{ .fg = bg0, .bg = yellow, .bold = true },
            .status_mid = .{ .fg = fg1, .bg = bg2 },
            .status_right = .{ .fg = bg0, .bg = aqua, .bold = true },
            .accent = .{ .fg = fg0, .bg = purple, .bold = true },
            .accent_alt = .{ .fg = fg0, .bg = orange, .bold = true },
        };
    }
};

pub const Icons = struct {
    pub const file = "󰈙";
    pub const code = "󰅨";
    pub const stack = "󰜎";
    pub const tasks = "󰄬";
    pub const threads = "󰐌";
    pub const breakpoint = "";
    pub const watch = "󰥔";
    pub const scope = "󰂡";
    pub const repl = "";
    pub const output = "󰆍";
    pub const events = "󰋽";
    pub const memory = "󰍛";
    pub const wrap = "󰌑";
    pub const wrap_back = "󰌐";
    pub const run = "";
    pub const pause = "";
    pub const step_over = "";
    pub const step_in = "";
    pub const step_out = "";
    pub const command = "";
    pub const quit = "󰗼";
    pub const mouse = "󰍽";
    pub const status_ok = "󰗠";
    pub const status_warn = "󰀦";
};

const ToolbarAction = enum {
    run,
    pause,
    step_over,
    step_in,
    step_out,
    breakpoint,
    watch,
    repl,
    command,
    quit,
};

const ToolbarButton = struct {
    rect: Rect,
    action: ToolbarAction,
    label: []const u8,
};

const ToolbarPress = struct {
    rect: Rect,
    action: ToolbarAction,
};

const InputEvent = union(enum) {
    key: KeyEvent,
    mouse: MouseEvent,
};

const KeyEvent = union(enum) {
    char: u21,
    ctrl: u8,
    special: SpecialKey,
    alt: u21,
};

const SpecialKey = enum {
    enter,
    backspace,
    tab,
    esc,
    up,
    down,
    left,
    right,
    home,
    end,
    page_up,
    page_down,
    delete,
    insert,
};

const MouseEvent = struct {
    x: i32,
    y: i32,
    button: MouseButton,
    kind: MouseKind,
    mods: KeyMods,
};

const MouseButton = enum {
    left,
    middle,
    right,
    wheel_up,
    wheel_down,
    none,
};

const MouseKind = enum {
    down,
    up,
    drag,
    move,
    scroll,
};

const KeyMods = struct {
    shift: bool = false,
    alt: bool = false,
    ctrl: bool = false,
};

const Input = struct {
    fd: posix.fd_t,
    buf: [4096]u8 = undefined,
    len: usize = 0,
    idx: usize = 0,

    fn init(fd: posix.fd_t) Input {
        return .{ .fd = fd };
    }

    fn nextEvent(self: *Input) !?InputEvent {
        const b = try self.readByte() orelse return null;
        if (b == 0x1b) {
            return self.parseEscape();
        }
        if (b == '\r' or b == '\n') return .{ .key = .{ .special = .enter } };
        if (b == 0x7f) return .{ .key = .{ .special = .backspace } };
        if (b == '\t') return .{ .key = .{ .special = .tab } };
        if (b < 0x20) return .{ .key = .{ .ctrl = b } };
        return .{ .key = .{ .char = b } };
    }

    fn readByte(self: *Input) !?u8 {
        if (self.idx < self.len) {
            const out = self.buf[self.idx];
            self.idx += 1;
            return out;
        }
        self.idx = 0;
        self.len = 0;
        const amt = posix.read(self.fd, self.buf[0..]) catch return null;
        if (amt == 0) return null;
        self.len = amt;
        const out = self.buf[0];
        self.idx = 1;
        return out;
    }

    fn readByteTimed(self: *Input, timeout_ms: i32) !?u8 {
        if (self.idx < self.len) return self.readByte();
        var fds = [_]posix.pollfd{.{ .fd = self.fd, .events = posix.POLL.IN, .revents = 0 }};
        const ready = posix.poll(&fds, timeout_ms) catch return null;
        if (ready == 0) return null;
        return self.readByte();
    }

    fn peekByte(self: *Input) !?u8 {
        if (self.idx < self.len) return self.buf[self.idx];
        return null;
    }

    fn parseEscape(self: *Input) !?InputEvent {
        const next = try self.readByteTimed(10) orelse return .{ .key = .{ .special = .esc } };
        if (next == '[') return self.parseCsi();
        if (next == 'O') return self.parseSs3();
        if (next >= 0x20) return .{ .key = .{ .alt = next } };
        return .{ .key = .{ .special = .esc } };
    }

    fn parseSs3(self: *Input) !?InputEvent {
        const next = try self.readByte() orelse return null;
        return switch (next) {
            'A' => .{ .key = .{ .special = .up } },
            'B' => .{ .key = .{ .special = .down } },
            'C' => .{ .key = .{ .special = .right } },
            'D' => .{ .key = .{ .special = .left } },
            'H' => .{ .key = .{ .special = .home } },
            'F' => .{ .key = .{ .special = .end } },
            else => null,
        };
    }

    fn parseCsi(self: *Input) !?InputEvent {
        const next = try self.readByte() orelse return null;
        if (next == '<') return self.parseMouse();
        if (next == 'M') return self.parseMouseX10();
        if (next >= '0' and next <= '9') {
            var nums: [4]i32 = .{0} ** 4;
            var count: usize = 0;
            var current = @as(i32, next - '0');
            while (true) {
                const b = try self.readByte() orelse return null;
                if (b >= '0' and b <= '9') {
                    current = current * 10 + @as(i32, b - '0');
                    continue;
                }
                nums[count] = current;
                count += 1;
                if (b == ';') {
                    current = 0;
                    continue;
                }
                return self.mapCsiNumbers(nums, count, b);
            }
        }
        return switch (next) {
            'A' => .{ .key = .{ .special = .up } },
            'B' => .{ .key = .{ .special = .down } },
            'C' => .{ .key = .{ .special = .right } },
            'D' => .{ .key = .{ .special = .left } },
            'H' => .{ .key = .{ .special = .home } },
            'F' => .{ .key = .{ .special = .end } },
            else => null,
        };
    }

    fn mapCsiNumbers(self: *Input, nums: [4]i32, count: usize, final: u8) ?InputEvent {
        _ = self;
        if (final == '~') {
            const code = nums[0];
            return switch (code) {
                1, 7 => .{ .key = .{ .special = .home } },
                4, 8 => .{ .key = .{ .special = .end } },
                5 => .{ .key = .{ .special = .page_up } },
                6 => .{ .key = .{ .special = .page_down } },
                2 => .{ .key = .{ .special = .insert } },
                3 => .{ .key = .{ .special = .delete } },
                else => null,
            };
        }
        if (final == 'A' or final == 'B' or final == 'C' or final == 'D') {
            return switch (final) {
                'A' => .{ .key = .{ .special = .up } },
                'B' => .{ .key = .{ .special = .down } },
                'C' => .{ .key = .{ .special = .right } },
                'D' => .{ .key = .{ .special = .left } },
                else => null,
            };
        }
        if (count >= 2 and final == 'm') return null;
        return null;
    }

    fn parseMouse(self: *Input) !?InputEvent {
        var nums: [3]i32 = .{0} ** 3;
        var count: usize = 0;
        var current: i32 = 0;
        while (true) {
            const b = try self.readByte() orelse return null;
            if (b >= '0' and b <= '9') {
                current = current * 10 + @as(i32, b - '0');
                continue;
            }
            nums[count] = current;
            count += 1;
            if (b == ';') {
                current = 0;
                continue;
            }
            const btn_code = nums[0];
            const x = nums[1] - 1;
            const y = nums[2] - 1;
            const is_release = b == 'm';
            const mods = KeyMods{
                .shift = (btn_code & 4) != 0,
                .alt = (btn_code & 8) != 0,
                .ctrl = (btn_code & 16) != 0,
            };
            if ((btn_code & 64) != 0) {
                const wheel = if ((btn_code & 1) != 0) MouseButton.wheel_down else MouseButton.wheel_up;
                return .{ .mouse = .{ .x = x, .y = y, .button = wheel, .kind = .scroll, .mods = mods } };
            }
            const button = switch (btn_code & 3) {
                0 => MouseButton.left,
                1 => MouseButton.middle,
                2 => MouseButton.right,
                else => MouseButton.none,
            };
            const motion = (btn_code & 32) != 0;
            const kind: MouseKind = if (is_release)
                .up
            else if (motion and button == .none)
                .move
            else if (motion)
                .drag
            else
                .down;
            return .{ .mouse = .{ .x = x, .y = y, .button = button, .kind = kind, .mods = mods } };
        }
    }

    fn parseMouseX10(self: *Input) !?InputEvent {
        const btn_raw = try self.readByte() orelse return null;
        const x_raw = try self.readByte() orelse return null;
        const y_raw = try self.readByte() orelse return null;
        const btn_code = @as(i32, @intCast(btn_raw)) - 32;
        const x = @as(i32, @intCast(x_raw)) - 33;
        const y = @as(i32, @intCast(y_raw)) - 33;
        const mods = KeyMods{
            .shift = (btn_code & 4) != 0,
            .alt = (btn_code & 8) != 0,
            .ctrl = (btn_code & 16) != 0,
        };
        if ((btn_code & 64) != 0) {
            const wheel = if ((btn_code & 1) != 0) MouseButton.wheel_down else MouseButton.wheel_up;
            return .{ .mouse = .{ .x = x, .y = y, .button = wheel, .kind = .scroll, .mods = mods } };
        }
        const button_bits = btn_code & 3;
        const button = switch (button_bits) {
            0 => MouseButton.left,
            1 => MouseButton.middle,
            2 => MouseButton.right,
            else => MouseButton.none,
        };
        const motion = (btn_code & 32) != 0;
        const kind: MouseKind = if (button_bits == 3)
            .up
        else if (motion and button == .none)
            .move
        else if (motion)
            .drag
        else
            .down;
        return .{ .mouse = .{ .x = x, .y = y, .button = button, .kind = kind, .mods = mods } };
    }
};

const Axis = enum {
    horizontal,
    vertical,
};

const TabKind = enum {
    source,
    disasm,
    stack,
    tasks,
    threads,
    breakpoints,
    watches,
    scopes,
    repl,
    output,
    events,
    memory,
};

const TabMeta = struct {
    kind: TabKind,
    title: []const u8,
};

const Group = struct {
    tabs: std.array_list.Managed(TabKind),
    active: usize,

    fn init(allocator: mem_allocator, tabs: []const TabKind, active: usize) !Group {
        var list = std.array_list.Managed(TabKind).init(allocator);
        try list.appendSlice(tabs);
        return .{ .tabs = list, .active = active };
    }
};

const Split = struct {
    axis: Axis,
    ratio: f32,
    first: usize,
    second: usize,
};

const NodeData = union(enum) {
    group: Group,
    split: Split,
};

const Node = struct {
    parent: ?usize,
    data: NodeData,
};

const DockTree = struct {
    allocator: mem_allocator,
    nodes: std.array_list.Managed(Node),
    root: usize,

    fn init(allocator: mem_allocator) DockTree {
        return .{ .allocator = allocator, .nodes = std.array_list.Managed(Node).init(allocator), .root = 0 };
    }

    fn deinit(self: *DockTree) void {
        for (self.nodes.items) |*node| {
            switch (node.data) {
                .group => |*grp| grp.tabs.deinit(),
                .split => {},
            }
        }
        self.nodes.deinit();
    }

    fn addGroup(self: *DockTree, tabs: []const TabKind, active: usize) !usize {
        const group = try Group.init(self.allocator, tabs, active);
        const id = self.nodes.items.len;
        try self.nodes.append(.{ .parent = null, .data = .{ .group = group } });
        return id;
    }

    fn addSplit(self: *DockTree, axis: Axis, ratio: f32, first: usize, second: usize) !usize {
        const id = self.nodes.items.len;
        try self.nodes.append(.{ .parent = null, .data = .{ .split = .{ .axis = axis, .ratio = ratio, .first = first, .second = second } } });
        self.nodes.items[first].parent = id;
        self.nodes.items[second].parent = id;
        return id;
    }

    fn replaceChild(self: *DockTree, parent_id: usize, old_id: usize, new_id: usize) void {
        var node = &self.nodes.items[parent_id];
        if (node.data != .split) return;
        if (node.data.split.first == old_id) {
            node.data.split.first = new_id;
        } else if (node.data.split.second == old_id) {
            node.data.split.second = new_id;
        }
        self.nodes.items[new_id].parent = parent_id;
    }

    fn removeGroup(self: *DockTree, group_id: usize) void {
        const parent_id = self.nodes.items[group_id].parent orelse {
            return;
        };
        const parent_node = self.nodes.items[parent_id];
        if (parent_node.data != .split) return;
        const sibling_id = if (parent_node.data.split.first == group_id)
            parent_node.data.split.second
        else
            parent_node.data.split.first;
        const grand = parent_node.parent;
        self.nodes.items[sibling_id].parent = grand;
        if (grand) |gid| {
            self.replaceChild(gid, parent_id, sibling_id);
        } else {
            self.root = sibling_id;
        }
    }
};

const GroupLayout = struct {
    id: usize,
    rect: Rect,
    header: Rect,
    body: Rect,
};

const SplitLayout = struct {
    node_id: usize,
    rect: Rect,
    axis: Axis,
    bounds: Rect,
};

const LayoutCache = struct {
    groups: std.array_list.Managed(GroupLayout),
    splitters: std.array_list.Managed(SplitLayout),

    fn init(allocator: mem_allocator) LayoutCache {
        return .{
            .groups = std.array_list.Managed(GroupLayout).init(allocator),
            .splitters = std.array_list.Managed(SplitLayout).init(allocator),
        };
    }

    fn deinit(self: *LayoutCache) void {
        self.groups.deinit();
        self.splitters.deinit();
    }

    fn clear(self: *LayoutCache) void {
        self.groups.clearRetainingCapacity();
        self.splitters.clearRetainingCapacity();
    }
};

const DropZone = enum {
    center,
    left,
    right,
    top,
    bottom,
};

const DragState = union(enum) {
    none,
    splitter: SplitDrag,
    tab: TabDrag,
};

const SplitDrag = struct {
    node_id: usize,
    axis: Axis,
    start: Point,
    start_ratio: f32,
    bounds: Rect,
};

const TabDrag = struct {
    from_group: usize,
    tab_index: usize,
    start: Point,
    pos: Point,
    moved: bool = false,
    target_group: ?usize = null,
    target_zone: ?DropZone = null,
};

const InputMode = enum {
    normal,
    command,
    repl,
};

const CommandPalette = struct {
    input: std.array_list.Managed(u8),
    cursor: usize = 0,

    fn init(allocator: mem_allocator) CommandPalette {
        return .{ .input = std.array_list.Managed(u8).init(allocator) };
    }

    fn deinit(self: *CommandPalette) void {
        self.input.deinit();
    }

    fn reset(self: *CommandPalette) void {
        self.input.clearRetainingCapacity();
        self.cursor = 0;
    }

    fn setText(self: *CommandPalette, value: []const u8) !void {
        self.input.clearRetainingCapacity();
        try self.input.appendSlice(value);
        self.cursor = self.input.items.len;
    }

    fn push(self: *CommandPalette, ch: u8) !void {
        try self.input.insert(self.cursor, ch);
        self.cursor += 1;
    }

    fn pop(self: *CommandPalette) void {
        if (self.cursor == 0) return;
        self.cursor -= 1;
        _ = self.input.orderedRemove(self.cursor);
    }

    fn text(self: *CommandPalette) []const u8 {
        return self.input.items;
    }
};

pub const LogBuffer = struct {
    allocator: mem_allocator,
    lines: std.array_list.Managed([]u8),
    max_lines: usize,
    scroll_offset: i32 = 0,

    pub fn init(allocator: mem_allocator, max_lines: usize) LogBuffer {
        return .{
            .allocator = allocator,
            .lines = std.array_list.Managed([]u8).init(allocator),
            .max_lines = max_lines,
            .scroll_offset = 0,
        };
    }

    pub fn deinit(self: *LogBuffer) void {
        for (self.lines.items) |line| self.allocator.free(line);
        self.lines.deinit();
    }

    pub fn add(self: *LogBuffer, text: []const u8) !void {
        const duped = try self.allocator.dupe(u8, text);
        try self.lines.append(duped);
        if (self.scroll_offset > 0) self.scroll_offset += 1;
        if (self.max_lines > 0) {
            while (self.lines.items.len > self.max_lines) {
                const line = self.lines.orderedRemove(0);
                self.allocator.free(line);
            }
        }
        self.clampScrollOffset(0);
    }

    pub fn clear(self: *LogBuffer) void {
        for (self.lines.items) |line| self.allocator.free(line);
        self.lines.clearRetainingCapacity();
        self.scroll_offset = 0;
    }

    pub fn clampScrollOffset(self: *LogBuffer, visible: usize) void {
        const max_offset = if (self.lines.items.len > visible)
            @as(i32, @intCast(self.lines.items.len - visible))
        else
            0;
        if (self.scroll_offset < 0) self.scroll_offset = 0;
        if (self.scroll_offset > max_offset) self.scroll_offset = max_offset;
    }

    pub fn scroll(self: *LogBuffer, delta: i32, visible: usize) void {
        if (visible == 0) return;
        self.clampScrollOffset(visible);
        const max_offset = if (self.lines.items.len > visible)
            @as(i32, @intCast(self.lines.items.len - visible))
        else
            0;
        var next = @as(i64, @intCast(self.scroll_offset)) - delta;
        if (next < 0) next = 0;
        if (next > max_offset) next = max_offset;
        self.scroll_offset = @intCast(next);
    }

    pub fn startIndex(self: *LogBuffer, visible: usize) usize {
        if (visible == 0) return 0;
        self.clampScrollOffset(visible);
        const max_offset = if (self.lines.items.len > visible)
            self.lines.items.len - visible
        else
            0;
        return max_offset - @as(usize, @intCast(self.scroll_offset));
    }
};

const ReplContext = enum {
    frame,
    repl,
};

const ScopeKind = enum {
    locals,
    registers,
};

const PendingRequest = union(enum) {
    evaluate_repl,
    evaluate_watch: usize,
    stack_trace,
    threads,
    tasks,
    scopes: i64,
    variables: ScopeKind,
    disassemble,
    read_memory,
    set_breakpoints: []u8,
};

const ThreadInfo = struct {
    id: i64,
    name: []u8,
};

const TaskInfo = struct {
    id: i64,
    parent: ?i64,
    state: []u8,
    name: []u8,
    pc: ?i64,
};

const Watch = struct {
    expr: []u8,
    value: ?[]u8 = null,
    err: ?[]u8 = null,

    fn deinit(self: *Watch, allocator: mem_allocator) void {
        allocator.free(self.expr);
        if (self.value) |val| allocator.free(val);
        if (self.err) |err| allocator.free(err);
    }
};

const Breakpoint = struct {
    path: []u8,
    line: i64,
    verified: bool,
};

const DisasmLine = struct {
    addr: usize,
    text: []u8,
    bytes: []u8,
    symbol: ?[]u8 = null,
    source_path: ?[]u8 = null,
    line: ?i64 = null,
};

const MemoryView = struct {
    base: usize,
    bytes: []u8,
};

const VarEntry = struct {
    name: []u8,
    value: []u8,
    type_name: ?[]u8,
};

const ScopeState = struct {
    locals: std.array_list.Managed(VarEntry),
    registers: std.array_list.Managed(VarEntry),
    locals_ref: ?i64 = null,
    registers_ref: ?i64 = null,

    fn init(allocator: mem_allocator) ScopeState {
        return .{
            .locals = std.array_list.Managed(VarEntry).init(allocator),
            .registers = std.array_list.Managed(VarEntry).init(allocator),
        };
    }

    fn deinit(self: *ScopeState, allocator: mem_allocator) void {
        for (self.locals.items) |entry| {
            allocator.free(entry.name);
            allocator.free(entry.value);
            if (entry.type_name) |ty| allocator.free(ty);
        }
        self.locals.deinit();
        for (self.registers.items) |entry| {
            allocator.free(entry.name);
            allocator.free(entry.value);
            if (entry.type_name) |ty| allocator.free(ty);
        }
        self.registers.deinit();
    }

    fn reset(self: *ScopeState, allocator: mem_allocator) void {
        for (self.locals.items) |entry| {
            allocator.free(entry.name);
            allocator.free(entry.value);
            if (entry.type_name) |ty| allocator.free(ty);
        }
        self.locals.clearRetainingCapacity();
        for (self.registers.items) |entry| {
            allocator.free(entry.name);
            allocator.free(entry.value);
            if (entry.type_name) |ty| allocator.free(ty);
        }
        self.registers.clearRetainingCapacity();
        self.locals_ref = null;
        self.registers_ref = null;
    }
};

pub const SourceLine = struct {
    start: usize,
    len: usize,
};

pub const SourceFile = struct {
    path: []u8,
    text: []u8,
    lines: []SourceLine,
};

pub const SourceCache = struct {
    allocator: mem_allocator,
    files: std.StringHashMap(SourceFile),

    pub fn init(allocator: mem_allocator) SourceCache {
        return .{
            .allocator = allocator,
            .files = std.StringHashMap(SourceFile).init(allocator),
        };
    }

    pub fn deinit(self: *SourceCache) void {
        var it = self.files.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.path);
            self.allocator.free(entry.value_ptr.text);
            self.allocator.free(entry.value_ptr.lines);
        }
        self.files.deinit();
    }

    pub fn get(self: *SourceCache, path: []const u8) !?*SourceFile {
        if (self.files.getPtr(path)) |file| return file;
        const text = std.fs.cwd().readFileAlloc(self.allocator, path, 8 * 1024 * 1024) catch return null;
        errdefer self.allocator.free(text);
        const owned_path = try self.allocator.dupe(u8, path);
        errdefer self.allocator.free(owned_path);
        var lines = std.array_list.Managed(SourceLine).init(self.allocator);
        defer lines.deinit();
        var iter = std.mem.splitScalar(u8, text, '\n');
        var offset: usize = 0;
        while (iter.next()) |line| {
            const trimmed = std.mem.trimRight(u8, line, "\r");
            try lines.append(.{ .start = offset, .len = trimmed.len });
            offset += line.len + 1;
        }
        const line_slice = try lines.toOwnedSlice();
        const key = try self.allocator.dupe(u8, path);
        try self.files.put(key, .{
            .path = owned_path,
            .text = text,
            .lines = line_slice,
        });
        return self.files.getPtr(key);
    }

    pub fn put(self: *SourceCache, path: []const u8, text: []const u8) !*SourceFile {
        if (self.files.getPtr(path)) |file| return file;
        const owned_text = try self.allocator.dupe(u8, text);
        errdefer self.allocator.free(owned_text);
        const owned_path = try self.allocator.dupe(u8, path);
        errdefer self.allocator.free(owned_path);
        var lines = std.array_list.Managed(SourceLine).init(self.allocator);
        defer lines.deinit();
        var iter = std.mem.splitScalar(u8, owned_text, '\n');
        var offset: usize = 0;
        while (iter.next()) |line| {
            const trimmed = std.mem.trimRight(u8, line, "\r");
            try lines.append(.{ .start = offset, .len = trimmed.len });
            offset += line.len + 1;
        }
        const line_slice = try lines.toOwnedSlice();
        const key = try self.allocator.dupe(u8, path);
        try self.files.put(key, .{
            .path = owned_path,
            .text = owned_text,
            .lines = line_slice,
        });
        return self.files.getPtr(key).?;
    }
};

pub const SourceView = struct {
    path: ?[]u8 = null,
    line: usize = 0,
    scroll: usize = 0,
};

const DebugFrame = struct {
    id: i64,
    name: []u8,
    source_path: ?[]u8,
    line: i64,
    ip: ?usize,
    sp: ?usize,
    fp: ?usize,
};

const DebugSession = struct {
    allocator: mem_allocator,
    child: ?std.process.Child,
    stream: ?std.net.Stream,
    read_fd: posix.fd_t,
    write_fd: posix.fd_t,
    stderr_fd: posix.fd_t,
    in_buf: std.array_list.Managed(u8),
    messages: std.array_list.Managed([]u8),
    stderr_messages: std.array_list.Managed([]u8),
    pending: std.AutoHashMap(i64, PendingRequest),
    frames: std.array_list.Managed(DebugFrame),
    seq: i64 = 1,
    paused: bool = false,
    connected: bool = false,
    last_thread: ?i64 = null,
    selected_frame: usize = 0,

    fn init(allocator: mem_allocator, options: Options) !?DebugSession {
        if (options.attach == null and options.program == null) return null;

        var session = DebugSession{
            .allocator = allocator,
            .child = null,
            .stream = null,
            .read_fd = -1,
            .write_fd = -1,
            .stderr_fd = -1,
            .in_buf = std.array_list.Managed(u8).init(allocator),
            .messages = std.array_list.Managed([]u8).init(allocator),
            .stderr_messages = std.array_list.Managed([]u8).init(allocator),
            .pending = std.AutoHashMap(i64, PendingRequest).init(allocator),
            .frames = std.array_list.Managed(DebugFrame).init(allocator),
        };
        errdefer session.deinit();

        if (options.attach) |addr| {
            session.stream = try connect_stream(addr);
            session.read_fd = session.stream.?.handle;
            session.write_fd = session.stream.?.handle;
        } else {
            const program = options.program.?;
            var argv = std.array_list.Managed([]const u8).init(allocator);
            defer argv.deinit();
            const inkvm_path = try resolveInkvmPath(allocator);
            defer allocator.free(inkvm_path);
            try argv.append(inkvm_path);
            try argv.append("--debug");
            if (options.sim) try argv.append("--sim");
            if (options.scenario) |name| {
                try argv.append("--scenario");
                try argv.append(name);
            }
            if (options.replay) |path| {
                try argv.append("--replay");
                try argv.append(path);
            }
            try argv.append(program);

            var child = std.process.Child.init(argv.items, allocator);
            child.stdin_behavior = .Pipe;
            child.stdout_behavior = .Pipe;
            child.stderr_behavior = .Pipe;
            try child.spawn();
            session.child = child;
            session.read_fd = child.stdout.?.handle;
            session.write_fd = child.stdin.?.handle;
            if (child.stderr) |stderr| {
                session.stderr_fd = stderr.handle;
                try set_nonblocking(stderr.handle);
            }
        }

        try set_nonblocking(session.read_fd);
        session.connected = true;

        try session.sendInitialize();
        if (options.attach != null) {
            try session.sendAttach();
        } else {
            try session.sendLaunch(options);
        }
        try session.sendConfigurationDone();
        return session;
    }

    fn deinit(self: *DebugSession) void {
        self.clearFrames();
        self.frames.deinit();
        for (self.messages.items) |msg| self.allocator.free(msg);
        self.messages.deinit();
        for (self.stderr_messages.items) |msg| self.allocator.free(msg);
        self.stderr_messages.deinit();
        self.in_buf.deinit();
        self.pending.deinit();
        if (self.stream) |*stream| stream.close();
        if (self.child) |*child| {
            if (child.kill()) |_| {} else |_| {}
            _ = child.wait() catch {};
        }
    }

    fn popMessage(self: *DebugSession) ?[]u8 {
        if (self.messages.items.len == 0) return null;
        return self.messages.orderedRemove(0);
    }

    fn popStderr(self: *DebugSession) ?[]u8 {
        if (self.stderr_messages.items.len == 0) return null;
        return self.stderr_messages.orderedRemove(0);
    }

    fn readIncoming(self: *DebugSession) !void {
        if (!self.connected) return;
        var buf: [4096]u8 = undefined;
        while (true) {
            const amt = posix.read(self.read_fd, &buf) catch |err| switch (err) {
                error.WouldBlock => break,
                else => return err,
            };
            if (amt == 0) {
                self.connected = false;
                break;
            }
            try self.in_buf.appendSlice(buf[0..amt]);
        }

        while (true) {
            const frame = parse_frame(self.in_buf.items) orelse break;
            const payload = self.in_buf.items[frame.start .. frame.start + frame.len];
            const owned = try self.allocator.dupe(u8, payload);
            try self.messages.append(owned);
            self.removePrefix(frame.consumed);
        }
    }

    fn readStderr(self: *DebugSession) !void {
        if (self.stderr_fd < 0) return;
        var buf: [4096]u8 = undefined;
        while (true) {
            const amt = posix.read(self.stderr_fd, &buf) catch |err| switch (err) {
                error.WouldBlock => break,
                else => return err,
            };
            if (amt == 0) {
                self.stderr_fd = -1;
                break;
            }
            const owned = try self.allocator.dupe(u8, buf[0..amt]);
            try self.stderr_messages.append(owned);
        }
    }

    fn sendEvaluate(self: *DebugSession, expr: []const u8, frame_id: ?i64, context: ?[]const u8) !i64 {
        const args = .{
            .expression = expr,
            .frameId = frame_id,
            .context = context,
        };
        return self.sendRequestBody("evaluate", args);
    }

    fn sendStackTrace(self: *DebugSession, thread_id: i64) !i64 {
        const args = .{
            .threadId = thread_id,
            .startFrame = @as(i64, 0),
            .levels = @as(i64, 64),
        };
        return self.sendRequestBody("stackTrace", args);
    }

    fn sendSimple(self: *DebugSession, command: []const u8) !i64 {
        return self.sendRequest(command);
    }

    fn sendThreads(self: *DebugSession) !i64 {
        return self.sendRequest("threads");
    }

    fn sendScopes(self: *DebugSession, frame_id: i64) !i64 {
        const args = .{ .frameId = frame_id };
        return self.sendRequestBody("scopes", args);
    }

    fn sendVariables(self: *DebugSession, reference: i64) !i64 {
        const args = .{ .variablesReference = reference };
        return self.sendRequestBody("variables", args);
    }

    fn sendDisassemble(self: *DebugSession, base: usize, count: i64) !i64 {
        var buf: [32]u8 = undefined;
        const addr = std.fmt.bufPrint(&buf, "0x{x}", .{base}) catch "0x0";
        const args = .{
            .memoryReference = addr,
            .instructionCount = count,
        };
        return self.sendRequestBody("disassemble", args);
    }

    fn sendReadMemory(self: *DebugSession, base: usize, count: i64, frame_id: ?i64) !i64 {
        var buf: [32]u8 = undefined;
        const addr = std.fmt.bufPrint(&buf, "0x{x}", .{base}) catch "0x0";
        const args = .{
            .memoryReference = addr,
            .offset = @as(i64, 0),
            .count = count,
            .frameId = frame_id,
        };
        return self.sendRequestBody("readMemory", args);
    }

    fn sendInkxTasks(self: *DebugSession) !i64 {
        return self.sendRequest("inkxTasks");
    }

    fn sendSetBreakpoints(self: *DebugSession, path: []const u8, lines: []const i64) !i64 {
        const BreakpointSpec = struct { line: i64 };
        var list = std.array_list.Managed(BreakpointSpec).init(self.allocator);
        defer list.deinit();
        for (lines) |line| try list.append(.{ .line = line });
        const args = .{
            .source = .{
                .path = path,
            },
            .breakpoints = list.items,
        };
        return self.sendRequestBody("setBreakpoints", args);
    }

    fn takePending(self: *DebugSession, request_seq: i64) ?PendingRequest {
        if (self.pending.fetchRemove(request_seq)) |entry| return entry.value;
        return null;
    }

    fn clearFrames(self: *DebugSession) void {
        for (self.frames.items) |frame| {
            self.allocator.free(frame.name);
            if (frame.source_path) |path| self.allocator.free(path);
        }
        self.frames.clearRetainingCapacity();
        self.selected_frame = 0;
    }

    fn updateFrames(self: *DebugSession, frames_val: std.json.Value) !void {
        self.clearFrames();
        if (frames_val != .array) return;
        for (frames_val.array.items) |item| {
            if (item != .object) continue;
            const obj = item.object;
            const id_val = obj.get("id") orelse continue;
            if (id_val != .integer) continue;
            const name_val = obj.get("name") orelse continue;
            if (name_val != .string) continue;
            const line_val = obj.get("line") orelse continue;
            if (line_val != .integer) continue;
            var ip: ?usize = null;
            if (obj.get("instructionPointerReference")) |ip_val| {
                if (ip_val == .string) {
                    ip = parse_address(ip_val.string);
                }
            }
            var sp: ?usize = null;
            if (obj.get("inkxSp")) |sp_val| {
                if (sp_val == .integer and sp_val.integer >= 0) {
                    sp = @intCast(sp_val.integer);
                }
            }
            var fp: ?usize = null;
            if (obj.get("inkxFp")) |fp_val| {
                if (fp_val == .integer and fp_val.integer >= 0) {
                    fp = @intCast(fp_val.integer);
                }
            }
            var source_path: ?[]u8 = null;
            if (obj.get("source")) |src_val| {
                if (src_val == .object) {
                    if (src_val.object.get("path")) |path_val| {
                        if (path_val == .string) {
                            source_path = try self.allocator.dupe(u8, path_val.string);
                        }
                    }
                }
            }
            const name = try self.allocator.dupe(u8, name_val.string);
            try self.frames.append(.{
                .id = @intCast(id_val.integer),
                .name = name,
                .source_path = source_path,
                .line = @intCast(line_val.integer),
                .ip = ip,
                .sp = sp,
                .fp = fp,
            });
        }
    }

    fn selectedFrameId(self: *DebugSession) ?i64 {
        if (self.frames.items.len == 0) return null;
        if (self.selected_frame >= self.frames.items.len) return null;
        return self.frames.items[self.selected_frame].id;
    }

    fn sendInitialize(self: *DebugSession) !void {
        const args = .{
            .adapterID = "inkx",
            .clientID = "inkx",
            .linesStartAt1 = true,
            .columnsStartAt1 = true,
            .pathFormat = "path",
            .supportsRunInTerminalRequest = false,
        };
        _ = try self.sendRequestBody("initialize", args);
    }

    fn sendLaunch(self: *DebugSession, options: Options) !void {
        const args = .{
            .program = options.program.?,
            .sim = options.sim,
            .scenario = options.scenario,
            .replay = options.replay,
        };
        _ = try self.sendRequestBody("launch", args);
    }

    fn sendAttach(self: *DebugSession) !void {
        _ = try self.sendRequest("attach");
    }

    fn sendConfigurationDone(self: *DebugSession) !void {
        _ = try self.sendRequest("configurationDone");
    }

    fn sendRequest(self: *DebugSession, command: []const u8) !i64 {
        const seq = self.seq;
        self.seq += 1;
        var out = std.Io.Writer.Allocating.init(self.allocator);
        defer out.deinit();
        var stream = std.json.Stringify{ .writer = &out.writer };
        try stream.beginObject();
        try stream.objectField("type");
        try stream.write("request");
        try stream.objectField("seq");
        try stream.write(seq);
        try stream.objectField("command");
        try stream.write(command);
        try stream.endObject();
        try self.writeFrame(out.writer.buffered());
        return seq;
    }

    fn sendRequestBody(self: *DebugSession, command: []const u8, body: anytype) !i64 {
        const seq = self.seq;
        self.seq += 1;
        var out = std.Io.Writer.Allocating.init(self.allocator);
        defer out.deinit();
        var stream = std.json.Stringify{ .writer = &out.writer };
        try stream.beginObject();
        try stream.objectField("type");
        try stream.write("request");
        try stream.objectField("seq");
        try stream.write(seq);
        try stream.objectField("command");
        try stream.write(command);
        try stream.objectField("arguments");
        try stream.write(body);
        try stream.endObject();
        try self.writeFrame(out.writer.buffered());
        return seq;
    }

    fn writeFrame(self: *DebugSession, payload: []const u8) !void {
        var header_buf: [64]u8 = undefined;
        const header = try std.fmt.bufPrint(&header_buf, "Content-Length: {d}\r\n\r\n", .{payload.len});
        try write_all(self.write_fd, header);
        try write_all(self.write_fd, payload);
    }

    fn removePrefix(self: *DebugSession, count: usize) void {
        if (count == 0) return;
        if (count >= self.in_buf.items.len) {
            self.in_buf.items.len = 0;
            return;
        }
        const remaining = self.in_buf.items.len - count;
        std.mem.copyForwards(u8, self.in_buf.items[0..remaining], self.in_buf.items[count..]);
        self.in_buf.items.len = remaining;
    }
};

const FrameInfo = struct {
    start: usize,
    len: usize,
    consumed: usize,
};

fn parse_frame(buf: []const u8) ?FrameInfo {
    const sep = std.mem.indexOf(u8, buf, "\r\n\r\n") orelse return null;
    const header = buf[0..sep];
    var content_length: ?usize = null;
    var lines = std.mem.splitSequence(u8, header, "\r\n");
    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t");
        if (std.mem.startsWith(u8, trimmed, "Content-Length:")) {
            const value = std.mem.trim(u8, trimmed["Content-Length:".len..], " \t");
            content_length = std.fmt.parseInt(usize, value, 10) catch null;
        }
    }
    if (content_length == null) return null;
    const total = sep + 4 + content_length.?;
    if (buf.len < total) return null;
    return .{ .start = sep + 4, .len = content_length.?, .consumed = total };
}

fn write_all(fd: posix.fd_t, data: []const u8) !void {
    var offset: usize = 0;
    while (offset < data.len) {
        const amt = posix.write(fd, data[offset..]) catch |err| switch (err) {
            error.WouldBlock => continue,
            else => return err,
        };
        if (amt == 0) return error.EndOfStream;
        offset += amt;
    }
}

fn set_nonblocking(fd: posix.fd_t) !void {
    const flags = try posix.fcntl(fd, posix.F.GETFL, 0);
    const nonblock = @as(@TypeOf(flags), 1) << @bitOffsetOf(posix.O, "NONBLOCK");
        _ = try posix.fcntl(fd, posix.F.SETFL, flags | nonblock);
    }

    fn connect_stream(addr: []const u8) !std.net.Stream {
    const colon = std.mem.lastIndexOfScalar(u8, addr, ':') orelse return error.InvalidAddress;
    const host = if (colon == 0) "127.0.0.1" else addr[0..colon];
    const port_str = addr[colon + 1 ..];
    const port = std.fmt.parseInt(u16, port_str, 10) catch return error.InvalidAddress;
    const address = try std.net.Address.parseIp(host, port);
    return std.net.tcpConnectToAddress(address);
}

const App = struct {
    allocator: mem_allocator,
    term: *Terminal,
    input: Input,
    theme: Theme,
    canvas: Canvas,
    renderer: Renderer,
    dock: DockTree,
    layout: LayoutCache,
    toolbar_rect: Rect,
    toolbar_buttons: std.array_list.Managed(ToolbarButton),
    toolbar_press: ?ToolbarPress = null,
    drag: DragState,
    mouse_down_active: bool = false,
    mouse_release_supported: bool = false,
    active_group: usize,
    mode: InputMode,
    command: CommandPalette,
    log: LogBuffer,
    events: LogBuffer,
    output: LogBuffer,
    debug: ?DebugSession,
    repl_input: std.array_list.Managed(u8),
    repl_cursor: usize,
    repl_context: ReplContext,
    source_cache: SourceCache,
    source_view: SourceView,
    disasm_lines: std.array_list.Managed(DisasmLine),
    disasm_scroll: usize,
    disasm_cursor: ?usize,
    memory_view: ?MemoryView,
    threads: std.array_list.Managed(ThreadInfo),
    tasks: std.array_list.Managed(TaskInfo),
    selected_thread: ?i64,
    selected_task: ?i64,
    watches: std.array_list.Managed(Watch),
    breakpoints: std.array_list.Managed(Breakpoint),
    scopes: ScopeState,
    should_quit: bool = false,
    status_line: []const u8,

    fn init(allocator: mem_allocator, term: *Terminal, options: Options) !App {
        const size = term.size();
        const theme = Theme.gruvboxHard();
        const canvas = try Canvas.init(
            allocator,
            @intCast(size.cols),
            @intCast(size.rows),
            .{ .fg = theme.fg1, .bg = theme.bg1 },
        );

        var dock = DockTree.init(allocator);
        const left = try dock.addGroup(&.{ .breakpoints, .watches, .scopes }, 0);
        const center = try dock.addGroup(&.{ .source, .disasm }, 0);
        const right = try dock.addGroup(&.{ .stack, .tasks, .threads }, 0);
        const bottom = try dock.addGroup(&.{ .repl, .events, .memory }, 0);

        const mid = try dock.addSplit(.vertical, 0.68, center, right);
        const top = try dock.addSplit(.vertical, 0.22, left, mid);
        const root = try dock.addSplit(.horizontal, 0.72, top, bottom);
        dock.root = root;

        const layout = LayoutCache.init(allocator);
        const log = LogBuffer.init(allocator, 200);
        const events = LogBuffer.init(allocator, 200);
        const output = LogBuffer.init(allocator, 200);
        const toolbar_buttons = std.array_list.Managed(ToolbarButton).init(allocator);

        const repl_input = std.array_list.Managed(u8).init(allocator);
        const source_cache = SourceCache.init(allocator);
        const disasm_lines = std.array_list.Managed(DisasmLine).init(allocator);
        const threads = std.array_list.Managed(ThreadInfo).init(allocator);
        const tasks = std.array_list.Managed(TaskInfo).init(allocator);
        const watches = std.array_list.Managed(Watch).init(allocator);
        const breakpoints = std.array_list.Managed(Breakpoint).init(allocator);
        const scopes = ScopeState.init(allocator);
        var debug: ?DebugSession = null;
        var status_line: []const u8 = "inkx ready";
        if (try DebugSession.init(allocator, options)) |session| {
            debug = session;
            status_line = "debug starting";
        } else {
            status_line = "no debug target";
        }

        return .{
            .allocator = allocator,
            .term = term,
            .input = Input.init(term.fd),
            .theme = theme,
            .canvas = canvas,
            .renderer = .{ .writer = term.writer() },
            .dock = dock,
            .layout = layout,
            .toolbar_rect = Rect{ .x = 0, .y = 0, .w = 0, .h = 0 },
            .toolbar_buttons = toolbar_buttons,
            .toolbar_press = null,
            .drag = .none,
            .mouse_release_supported = false,
            .active_group = center,
            .mode = .normal,
            .command = CommandPalette.init(allocator),
            .log = log,
            .events = events,
            .output = output,
            .debug = debug,
            .repl_input = repl_input,
            .repl_cursor = 0,
            .repl_context = .frame,
            .source_cache = source_cache,
            .source_view = .{},
            .disasm_lines = disasm_lines,
            .disasm_scroll = 0,
            .disasm_cursor = null,
            .memory_view = null,
            .threads = threads,
            .tasks = tasks,
            .selected_thread = null,
            .selected_task = null,
            .watches = watches,
            .breakpoints = breakpoints,
            .scopes = scopes,
            .status_line = status_line,
        };
    }

    fn deinit(self: *App) void {
        self.canvas.deinit();
        self.dock.deinit();
        self.layout.deinit();
        self.toolbar_buttons.deinit();
        self.command.deinit();
        self.log.deinit();
        self.events.deinit();
        self.output.deinit();
        if (self.debug) |*debug| debug.deinit();
        self.repl_input.deinit();
        self.source_cache.deinit();
        if (self.source_view.path) |path| self.allocator.free(path);
        for (self.disasm_lines.items) |line| {
            self.allocator.free(line.text);
            self.allocator.free(line.bytes);
            if (line.symbol) |sym| self.allocator.free(sym);
            if (line.source_path) |path| self.allocator.free(path);
        }
        self.disasm_lines.deinit();
        if (self.memory_view) |view| self.allocator.free(view.bytes);
        for (self.threads.items) |thread| self.allocator.free(thread.name);
        self.threads.deinit();
        for (self.tasks.items) |task| {
            self.allocator.free(task.state);
            self.allocator.free(task.name);
        }
        self.tasks.deinit();
        for (self.watches.items) |*watch| watch.deinit(self.allocator);
        self.watches.deinit();
        for (self.breakpoints.items) |bp| self.allocator.free(bp.path);
        self.breakpoints.deinit();
        self.scopes.deinit(self.allocator);
    }

    fn loop(self: *App) !void {
        const poll_timeout_ms: i32 = 8;
        var last_size = self.term.size();
        while (!self.should_quit) {
            const now_size = self.term.size();
            if (now_size.cols != last_size.cols or now_size.rows != last_size.rows) {
                last_size = now_size;
                try self.canvas.resize(now_size.cols, now_size.rows);
            }

            var poll_fds = [_]posix.pollfd{.{ .fd = self.term.fd, .events = posix.POLL.IN, .revents = 0 }};
            _ = posix.poll(&poll_fds, poll_timeout_ms) catch {};
            while (true) {
                const ev = try self.input.nextEvent() orelse break;
                self.handleEvent(ev) catch {};
            }
            self.pollDebug() catch {};
            try self.draw();
        }
    }

    fn handleEvent(self: *App, ev: InputEvent) !void {
        switch (ev) {
            .key => |key| try self.handleKey(key),
            .mouse => |mouse| try self.handleMouse(mouse),
        }
    }

    fn pollDebug(self: *App) !void {
        if (self.debug) |*debug| {
            try debug.readIncoming();
            try debug.readStderr();
            while (debug.popMessage()) |payload| {
                defer self.allocator.free(payload);
                try self.handleDapMessage(payload);
            }
            while (debug.popStderr()) |payload| {
                defer self.allocator.free(payload);
                try self.appendOutputLines(payload, Icons.status_warn ++ " stderr: ");
            }
        }
    }

    fn handleDapMessage(self: *App, payload: []const u8) !void {
        var parsed = try std.json.parseFromSlice(std.json.Value, self.allocator, payload, .{});
        defer parsed.deinit();
        if (parsed.value != .object) return;
        const obj = parsed.value.object;
        const type_val = obj.get("type") orelse return;
        if (type_val != .string) return;
        if (std.mem.eql(u8, type_val.string, "event")) {
            try self.handleDapEvent(obj);
        } else if (std.mem.eql(u8, type_val.string, "response")) {
            try self.handleDapResponse(obj);
        }
    }

    fn handleDapEvent(self: *App, obj: std.json.ObjectMap) !void {
        const event_val = obj.get("event") orelse return;
        if (event_val != .string) return;
        const name = event_val.string;
        if (std.mem.eql(u8, name, "stopped")) {
            self.status_line = "paused";
            if (self.debug) |*debug| {
                debug.paused = true;
                var thread_id: i64 = 0;
                var reason: []const u8 = "stopped";
                if (obj.get("body")) |body_val| {
                    if (body_val == .object) {
                        if (body_val.object.get("threadId")) |tid| {
                            if (tid == .integer) thread_id = @intCast(tid.integer);
                        }
                        if (body_val.object.get("reason")) |reason_val| {
                            if (reason_val == .string) reason = reason_val.string;
                        }
                    }
                }
                debug.last_thread = if (thread_id != 0) thread_id else debug.last_thread;
                self.selected_thread = debug.last_thread;
                const seq = try debug.sendStackTrace(thread_id);
                try debug.pending.put(seq, .stack_trace);
                const thread_seq = try debug.sendThreads();
                try debug.pending.put(thread_seq, .threads);
                const task_seq = try debug.sendInkxTasks();
                try debug.pending.put(task_seq, .tasks);
                const event_line = try std.fmt.allocPrint(self.allocator, "stop: {s} (task {d})", .{ reason, thread_id });
                defer self.allocator.free(event_line);
                try self.events.add(event_line);
            }
            return;
        }
        if (std.mem.eql(u8, name, "continued")) {
            self.status_line = "running";
            if (self.debug) |*debug| debug.paused = false;
            try self.events.add("continue");
            return;
        }
        if (std.mem.eql(u8, name, "terminated")) {
            self.status_line = "terminated";
            if (self.debug) |*debug| debug.paused = false;
            try self.events.add("terminated");
            return;
        }
        if (std.mem.eql(u8, name, "output")) {
            if (obj.get("body")) |body_val| {
                if (body_val == .object) {
                    if (body_val.object.get("output")) |out_val| {
                        if (out_val == .string) {
                            try self.appendOutputLines(out_val.string, null);
                        }
                    }
                }
            }
            return;
        }
        var buf: [128]u8 = undefined;
        const line = std.fmt.bufPrint(&buf, "event: {s}", .{name}) catch name;
        try self.events.add(line);
    }

    fn handleDapResponse(self: *App, obj: std.json.ObjectMap) !void {
        const seq_val = obj.get("request_seq") orelse return;
        if (seq_val != .integer) return;
        if (self.debug == null) return;
        const request_seq: i64 = @intCast(seq_val.integer);
        const pending = self.debug.?.takePending(request_seq) orelse return;

        const success_val = obj.get("success") orelse std.json.Value{ .bool = true };
        const success = success_val == .bool and success_val.bool;
        const body_val = obj.get("body");
        const body = if (body_val != null and body_val.? == .object) body_val.?.object else null;
        const message_val = obj.get("message");
        const message = if (message_val != null and message_val.? == .string) message_val.?.string else null;
        switch (pending) {
            .evaluate_repl => {
                if (!success) {
                    if (message) |msg| {
                        try self.logError(msg);
                    } else {
                        try self.log.add(Icons.status_warn ++ " eval failed");
                    }
                    return;
                }
                if (body) |b| {
                    if (b.get("result")) |result_val| {
                        if (result_val == .string) {
                            try self.logResult(result_val.string);
                            return;
                        }
                    }
                }
                try self.log.add("=> (no output)");
            },
            .evaluate_watch => |index| {
                if (index >= self.watches.items.len) return;
                var watch = &self.watches.items[index];
                if (!success) {
                    if (watch.value) |val| {
                        self.allocator.free(val);
                        watch.value = null;
                    }
                    if (watch.err) |err| self.allocator.free(err);
                    if (message) |msg| {
                        watch.err = try self.allocator.dupe(u8, msg);
                    } else {
                        watch.err = try self.allocator.dupe(u8, "watch failed");
                    }
                    return;
                }
                if (watch.err) |err| {
                    self.allocator.free(err);
                    watch.err = null;
                }
                if (watch.value) |val| {
                    self.allocator.free(val);
                    watch.value = null;
                }
                if (body) |b| {
                    if (b.get("result")) |result_val| {
                        if (result_val == .string) {
                            watch.value = try self.allocator.dupe(u8, result_val.string);
                        }
                    }
                }
            },
            .stack_trace => {
                if (!success) {
                    if (message) |msg| {
                        const line = try std.fmt.allocPrint(self.allocator, "! {s}", .{msg});
                        defer self.allocator.free(line);
                        try self.log.add(line);
                    }
                    return;
                }
                if (body) |b| {
                    if (b.get("stackFrames")) |frames_val| {
                        try self.debug.?.updateFrames(frames_val);
                        try self.onFramesUpdated();
                    }
                }
            },
            .threads => {
                if (!success or body == null) return;
                if (body.?.get("threads")) |threads_val| {
                    try self.updateThreads(threads_val);
                }
            },
            .tasks => {
                if (!success or body == null) return;
                if (body.?.get("tasks")) |tasks_val| {
                    try self.updateTasks(tasks_val);
                }
            },
            .scopes => |frame_id| {
                if (!success or body == null) return;
                if (body.?.get("scopes")) |scopes_val| {
                    try self.updateScopes(frame_id, scopes_val);
                }
            },
            .variables => |kind| {
                if (!success or body == null) return;
                if (body.?.get("variables")) |vars_val| {
                    try self.updateVariables(kind, vars_val);
                }
            },
            .disassemble => {
                if (!success or body == null) return;
                if (body.?.get("instructions")) |inst_val| {
                    try self.updateDisasm(inst_val);
                }
            },
            .read_memory => {
                if (!success or body == null) return;
                try self.updateMemory(body.?);
            },
            .set_breakpoints => |path| {
                defer self.allocator.free(path);
                if (!success or body == null) return;
                if (body.?.get("breakpoints")) |bps_val| {
                    try self.updateBreakpoints(path, bps_val);
                }
            },
        }
    }

    fn logLinesWithPrefix(self: *App, text: []const u8, prefix: []const u8) !void {
        var iter = std.mem.splitScalar(u8, text, '\n');
        var any = false;
        while (iter.next()) |raw| {
            const line = std.mem.trimRight(u8, raw, "\r");
            if (line.len == 0) continue;
            any = true;
            if (prefix.len == 0) {
                try self.log.add(line);
            } else {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "{s}{s}", .{ prefix, line }) catch {
                    const owned = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ prefix, line });
                    defer self.allocator.free(owned);
                    try self.log.add(owned);
                    continue;
                };
                try self.log.add(msg);
            }
        }
        if (!any and prefix.len > 0) {
            try self.log.add(prefix);
        }
    }

    fn logResult(self: *App, text: []const u8) !void {
        if (text.len == 0) {
            try self.log.add("=> (no output)");
            return;
        }
        var iter = std.mem.splitScalar(u8, text, '\n');
        var first = true;
        while (iter.next()) |raw| {
            const line = std.mem.trimRight(u8, raw, "\r");
            const prefix = if (first) "=> " else "   ";
            first = false;
            if (line.len == 0) {
                try self.log.add(prefix);
                continue;
            }
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "{s}{s}", .{ prefix, line }) catch {
                const owned = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ prefix, line });
                defer self.allocator.free(owned);
                try self.log.add(owned);
                continue;
            };
            try self.log.add(msg);
        }
    }

    fn logError(self: *App, message: []const u8) !void {
        try self.logLinesWithPrefix(message, Icons.status_warn ++ " ");
    }

    fn appendOutputLines(self: *App, text: []const u8, prefix: ?[]const u8) !void {
        var iter = std.mem.splitScalar(u8, text, '\n');
        var wrote = false;
        while (iter.next()) |raw| {
            const line = std.mem.trimRight(u8, raw, "\r");
            if (line.len == 0) continue;
            wrote = true;
            const use_prefix = prefix orelse "";
            if (use_prefix.len == 0) {
                try self.output.add(line);
                try self.log.add(line);
            } else {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "{s}{s}", .{ use_prefix, line }) catch {
                    const owned = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ use_prefix, line });
                    defer self.allocator.free(owned);
                    try self.output.add(owned);
                    try self.log.add(owned);
                    continue;
                };
                try self.output.add(msg);
                try self.log.add(msg);
            }
        }
        if (!wrote and prefix != null) {
            try self.output.add(prefix.?);
            try self.log.add(prefix.?);
        }
    }

    fn onFramesUpdated(self: *App) !void {
        if (self.debug == null) return;
        const debug = &self.debug.?;
        if (debug.frames.items.len == 0) return;
        if (debug.selected_frame >= debug.frames.items.len) debug.selected_frame = 0;
        const frame = debug.frames.items[debug.selected_frame];
        if (frame.source_path) |path| {
            try self.setSourcePath(path);
            if (frame.line > 0) self.source_view.line = @intCast(frame.line);
            self.adjustSourceScroll();
        }
        if (debug.selectedFrameId()) |frame_id| {
            const seq = try debug.sendScopes(frame_id);
            try debug.pending.put(seq, .{ .scopes = frame_id });
            if (frame.sp) |sp| {
                const base = sp * @sizeOf(u64);
                const mem_seq = try debug.sendReadMemory(base, self.memoryReadCount(), frame_id);
                try debug.pending.put(mem_seq, .read_memory);
            }
        }
        if (frame.ip) |ip| {
            const dis_seq = try debug.sendDisassemble(ip, 48);
            try debug.pending.put(dis_seq, .disassemble);
            self.disasm_scroll = 0;
        }
        try self.refreshWatches();
    }

    fn setSourcePath(self: *App, path: []const u8) !void {
        if (self.source_view.path) |current| {
            if (std.mem.eql(u8, current, path)) return;
            self.allocator.free(current);
        }
        self.source_view.path = try self.allocator.dupe(u8, path);
        self.source_view.scroll = 0;
    }

    fn adjustSourceScroll(self: *App) void {
        if (self.source_view.line == 0) return;
        const line_index = self.source_view.line - 1;
        const margin: usize = 3;
        if (line_index <= margin) {
            self.source_view.scroll = 0;
        } else {
            self.source_view.scroll = line_index - margin;
        }
    }

    fn updateThreads(self: *App, threads_val: std.json.Value) !void {
        for (self.threads.items) |thread| self.allocator.free(thread.name);
        self.threads.clearRetainingCapacity();
        if (threads_val != .array) return;
        for (threads_val.array.items) |item| {
            if (item != .object) continue;
            const obj = item.object;
            const id_val = obj.get("id") orelse continue;
            const name_val = obj.get("name") orelse continue;
            if (id_val != .integer or name_val != .string) continue;
            try self.threads.append(.{
                .id = @intCast(id_val.integer),
                .name = try self.allocator.dupe(u8, name_val.string),
            });
        }
        var selected = self.selected_thread;
        if (selected != null) {
            var found = false;
            for (self.threads.items) |thread| {
                if (thread.id == selected.?) {
                    found = true;
                    break;
                }
            }
            if (!found) selected = null;
        }
        if (selected == null) {
            if (self.debug) |debug| {
                if (debug.last_thread) |tid| selected = tid;
            }
        }
        if (selected == null and self.threads.items.len > 0) {
            selected = self.threads.items[0].id;
        }
        self.selected_thread = selected;
    }

    fn updateTasks(self: *App, tasks_val: std.json.Value) !void {
        for (self.tasks.items) |task| {
            self.allocator.free(task.state);
            self.allocator.free(task.name);
        }
        self.tasks.clearRetainingCapacity();
        if (tasks_val != .array) return;
        for (tasks_val.array.items) |item| {
            if (item != .object) continue;
            const obj = item.object;
            const id_val = obj.get("id") orelse continue;
            const state_val = obj.get("state") orelse continue;
            const name_val = obj.get("name") orelse continue;
            if (id_val != .integer or state_val != .string or name_val != .string) continue;
            var parent: ?i64 = null;
            if (obj.get("parent")) |parent_val| {
                if (parent_val == .integer) parent = @intCast(parent_val.integer);
            }
            var pc: ?i64 = null;
            if (obj.get("pc")) |pc_val| {
                if (pc_val == .integer) pc = @intCast(pc_val.integer);
            }
            try self.tasks.append(.{
                .id = @intCast(id_val.integer),
                .parent = parent,
                .state = try self.allocator.dupe(u8, state_val.string),
                .name = try self.allocator.dupe(u8, name_val.string),
                .pc = pc,
            });
        }
        var selected = self.selected_task;
        if (selected != null) {
            var found = false;
            for (self.tasks.items) |task| {
                if (task.id == selected.?) {
                    found = true;
                    break;
                }
            }
            if (!found) selected = null;
        }
        if (selected == null and self.tasks.items.len > 0) {
            selected = self.tasks.items[0].id;
        }
        self.selected_task = selected;
    }

    fn updateScopes(self: *App, frame_id: i64, scopes_val: std.json.Value) !void {
        if (self.debug == null) return;
        self.scopes.reset(self.allocator);
        if (scopes_val != .array) return;
        for (scopes_val.array.items) |item| {
            if (item != .object) continue;
            const obj = item.object;
            const name_val = obj.get("name") orelse continue;
            const ref_val = obj.get("variablesReference") orelse continue;
            if (name_val != .string or ref_val != .integer) continue;
            const name = name_val.string;
            const ref_id: i64 = @intCast(ref_val.integer);
            if (std.mem.eql(u8, name, "Locals") or std.mem.eql(u8, name, "locals")) {
                self.scopes.locals_ref = ref_id;
            } else if (std.mem.eql(u8, name, "Registers") or std.mem.eql(u8, name, "registers")) {
                self.scopes.registers_ref = ref_id;
            }
        }
        if (self.debug) |*debug| {
            if (self.scopes.locals_ref) |locals_ref| {
                const seq = try debug.sendVariables(locals_ref);
                try debug.pending.put(seq, .{ .variables = .locals });
            }
            if (self.scopes.registers_ref) |regs_ref| {
                const seq = try debug.sendVariables(regs_ref);
                try debug.pending.put(seq, .{ .variables = .registers });
            }
        }
        _ = frame_id;
    }

    fn updateVariables(self: *App, kind: ScopeKind, vars_val: std.json.Value) !void {
        var target = if (kind == .locals) &self.scopes.locals else &self.scopes.registers;
        for (target.items) |entry| {
            self.allocator.free(entry.name);
            self.allocator.free(entry.value);
            if (entry.type_name) |ty| self.allocator.free(ty);
        }
        target.clearRetainingCapacity();
        if (vars_val != .array) return;
        for (vars_val.array.items) |item| {
            if (item != .object) continue;
            const obj = item.object;
            const name_val = obj.get("name") orelse continue;
            const value_val = obj.get("value") orelse continue;
            if (name_val != .string or value_val != .string) continue;
            var type_name: ?[]u8 = null;
            if (obj.get("type")) |type_val| {
                if (type_val == .string) {
                    type_name = try self.allocator.dupe(u8, type_val.string);
                }
            }
            try target.append(.{
                .name = try self.allocator.dupe(u8, name_val.string),
                .value = try self.allocator.dupe(u8, value_val.string),
                .type_name = type_name,
            });
        }
    }

    fn updateDisasm(self: *App, inst_val: std.json.Value) !void {
        for (self.disasm_lines.items) |line| {
            self.allocator.free(line.text);
            self.allocator.free(line.bytes);
            if (line.symbol) |sym| self.allocator.free(sym);
            if (line.source_path) |path| self.allocator.free(path);
        }
        self.disasm_lines.clearRetainingCapacity();
        if (inst_val != .array) return;
        for (inst_val.array.items) |item| {
            if (item != .object) continue;
            const obj = item.object;
            const addr_val = obj.get("address") orelse continue;
            const inst_val_str = obj.get("instruction") orelse continue;
            const bytes_val = obj.get("instructionBytes") orelse continue;
            if (addr_val != .string or inst_val_str != .string or bytes_val != .string) continue;
            const addr = parse_address(addr_val.string) orelse 0;
            var symbol: ?[]u8 = null;
            if (obj.get("symbol")) |sym_val| {
                if (sym_val == .string) symbol = try self.allocator.dupe(u8, sym_val.string);
            }
            var source_path: ?[]u8 = null;
            if (obj.get("location")) |loc_val| {
                if (loc_val == .object) {
                    if (loc_val.object.get("path")) |path_val| {
                        if (path_val == .string) {
                            source_path = try self.allocator.dupe(u8, path_val.string);
                        }
                    }
                }
            }
            var line_no: ?i64 = null;
            if (obj.get("line")) |line_val| {
                if (line_val == .integer) line_no = @intCast(line_val.integer);
            }
            try self.disasm_lines.append(.{
                .addr = addr,
                .text = try self.allocator.dupe(u8, inst_val_str.string),
                .bytes = try self.allocator.dupe(u8, bytes_val.string),
                .symbol = symbol,
                .source_path = source_path,
                .line = line_no,
            });
        }
    }

    fn updateMemory(self: *App, body: std.json.ObjectMap) !void {
        const data_val = body.get("data") orelse return;
        const addr_val = body.get("address") orelse std.json.Value{ .string = "0x0" };
        if (data_val != .string) return;
        const data_str = data_val.string;
        const decoded_len = std.base64.standard.Decoder.calcSizeForSlice(data_str) catch return;
        const decoded = try self.allocator.alloc(u8, decoded_len);
        errdefer self.allocator.free(decoded);
        std.base64.standard.Decoder.decode(decoded, data_str) catch return;
        if (self.memory_view) |view| self.allocator.free(view.bytes);
        const base = if (addr_val == .string) parse_address(addr_val.string) orelse 0 else 0;
        self.memory_view = .{ .base = base, .bytes = decoded };
    }

    fn updateBreakpoints(self: *App, path: []const u8, bp_val: std.json.Value) !void {
        var idx: usize = 0;
        while (idx < self.breakpoints.items.len) {
            const bp = self.breakpoints.items[idx];
            if (std.mem.eql(u8, bp.path, path)) {
                self.allocator.free(bp.path);
                _ = self.breakpoints.orderedRemove(idx);
                continue;
            }
            idx += 1;
        }
        if (bp_val != .array) return;
        for (bp_val.array.items) |item| {
            if (item != .object) continue;
            const obj = item.object;
            const line_val = obj.get("line") orelse continue;
            if (line_val != .integer) continue;
            const verified = if (obj.get("verified")) |ver_val|
                (ver_val == .bool and ver_val.bool)
            else
                false;
            try self.breakpoints.append(.{
                .path = try self.allocator.dupe(u8, path),
                .line = @intCast(line_val.integer),
                .verified = verified,
            });
        }
    }

    fn refreshWatches(self: *App) !void {
        if (self.debug == null) return;
        if (self.watches.items.len == 0) return;
        const debug = &self.debug.?;
        const frame_id = debug.selectedFrameId();
        const context: ?[]const u8 = if (frame_id == null) @as([]const u8, "repl") else null;
        for (self.watches.items, 0..) |watch, index| {
            const seq = try debug.sendEvaluate(watch.expr, frame_id, context);
            try debug.pending.put(seq, .{ .evaluate_watch = index });
        }
    }

    fn requestSetBreakpoints(self: *App, path: []const u8) !void {
        if (self.debug == null) return;
        var lines = std.array_list.Managed(i64).init(self.allocator);
        defer lines.deinit();
        for (self.breakpoints.items) |bp| {
            if (std.mem.eql(u8, bp.path, path)) {
                try lines.append(bp.line);
            }
        }
        const seq = try self.debug.?.sendSetBreakpoints(path, lines.items);
        const owned_path = try self.allocator.dupe(u8, path);
        try self.debug.?.pending.put(seq, .{ .set_breakpoints = owned_path });
    }

    fn toggleBreakpointAtCursor(self: *App) !void {
        const path = self.source_view.path orelse return;
        if (self.source_view.line == 0) return;
        const line: i64 = @intCast(self.source_view.line);
        try self.toggleBreakpointAtLine(path, line);
    }

    fn toggleBreakpointAtLine(self: *App, path: []const u8, line: i64) !void {
        if (line <= 0) return;
        var idx: usize = 0;
        while (idx < self.breakpoints.items.len) {
            const bp = self.breakpoints.items[idx];
            if (bp.line == line and std.mem.eql(u8, bp.path, path)) {
                self.allocator.free(bp.path);
                _ = self.breakpoints.orderedRemove(idx);
                try self.requestSetBreakpoints(path);
                return;
            }
            idx += 1;
        }
        try self.breakpoints.append(.{
            .path = try self.allocator.dupe(u8, path),
            .line = line,
            .verified = false,
        });
        try self.requestSetBreakpoints(path);
    }

    fn removeBreakpointAtLine(self: *App, path: []const u8, line: i64) !bool {
        var idx: usize = 0;
        while (idx < self.breakpoints.items.len) {
            const bp = self.breakpoints.items[idx];
            if (bp.line == line and std.mem.eql(u8, bp.path, path)) {
                self.allocator.free(bp.path);
                _ = self.breakpoints.orderedRemove(idx);
                try self.requestSetBreakpoints(path);
                return true;
            }
            idx += 1;
        }
        return false;
    }

    fn removeBreakpointAtIndex(self: *App, index: usize) !void {
        if (index >= self.breakpoints.items.len) return;
        const bp = self.breakpoints.items[index];
        const path = try self.allocator.dupe(u8, bp.path);
        defer self.allocator.free(path);
        self.allocator.free(bp.path);
        _ = self.breakpoints.orderedRemove(index);
        try self.requestSetBreakpoints(path);
    }

    fn addBreakpoint(self: *App, path: []const u8, line: i64) !void {
        if (line <= 0) return;
        for (self.breakpoints.items) |bp| {
            if (bp.line == line and std.mem.eql(u8, bp.path, path)) {
                return;
            }
        }
        try self.breakpoints.append(.{
            .path = try self.allocator.dupe(u8, path),
            .line = line,
            .verified = false,
        });
        try self.requestSetBreakpoints(path);
    }

    fn selectFrame(self: *App, index: usize) !void {
        if (self.debug == null) return;
        if (index >= self.debug.?.frames.items.len) return;
        self.debug.?.selected_frame = index;
        try self.onFramesUpdated();
    }

    fn selectThreadById(self: *App, thread_id: i64) !void {
        self.selected_thread = thread_id;
        if (self.debug) |*debug| {
            debug.last_thread = thread_id;
            const seq = try debug.sendStackTrace(thread_id);
            try debug.pending.put(seq, .stack_trace);
            const thread_seq = try debug.sendThreads();
            try debug.pending.put(thread_seq, .threads);
            const task_seq = try debug.sendInkxTasks();
            try debug.pending.put(task_seq, .tasks);
        }
    }

    fn jumpToSource(self: *App, path: []const u8, line: i64) !void {
        try self.setSourcePath(path);
        if (line > 0) self.source_view.line = @intCast(line);
        self.adjustSourceScroll();
    }

    fn addWatch(self: *App, expr: []const u8) !void {
        const watch = Watch{ .expr = try self.allocator.dupe(u8, expr) };
        try self.watches.append(watch);
        try self.refreshWatches();
    }

    fn removeWatch(self: *App, index: usize) void {
        if (index >= self.watches.items.len) return;
        var watch = self.watches.items[index];
        watch.deinit(self.allocator);
        _ = self.watches.orderedRemove(index);
    }

    fn hasBreakpoint(self: *App, path: []const u8, line: i64) bool {
        for (self.breakpoints.items) |bp| {
            if (bp.line == line and std.mem.eql(u8, bp.path, path)) return true;
        }
        return false;
    }

    fn taskById(self: *App, id: i64) ?*TaskInfo {
        for (self.tasks.items) |*task| {
            if (task.id == id) return task;
        }
        return null;
    }

    fn taskDepth(self: *App, task: TaskInfo) usize {
        var depth: usize = 0;
        var cursor = task.parent;
        var guard: usize = 0;
        while (cursor != null and guard < self.tasks.items.len) : (guard += 1) {
            depth += 1;
            const parent_task = self.taskById(cursor.?) orelse break;
            cursor = parent_task.parent;
        }
        return depth;
    }

    fn scrollActive(self: *App, delta: i32) !void {
        const active = self.activeTabKind();
        switch (active) {
            .repl => {
                const rect = self.activeBodyRect() orelse return;
                if (rect.h <= 1) return;
                const visible = @as(usize, @intCast(rect.h - 1));
                self.log.scroll(delta, visible);
            },
            .output => {
                const rect = self.activeBodyRect() orelse return;
                if (rect.h <= 1) return;
                const visible = @as(usize, @intCast(rect.h - 1));
                self.output.scroll(delta, visible);
            },
            .events => {
                const rect = self.activeBodyRect() orelse return;
                if (rect.h <= 0) return;
                const visible = @as(usize, @intCast(rect.h));
                self.events.scroll(delta, visible);
            },
            .source => {
                if (self.source_view.path == null) return;
                const file = (try self.source_cache.get(self.source_view.path.?)) orelse return;
                const max_scroll = if (file.lines.len > 0) file.lines.len - 1 else 0;
                const current = @as(i64, @intCast(self.source_view.scroll));
                var next = current + delta;
                if (next < 0) next = 0;
                if (@as(usize, @intCast(next)) > max_scroll) {
                    next = @intCast(max_scroll);
                }
                self.source_view.scroll = @intCast(next);
            },
            .disasm => {
                const max_scroll = if (self.disasm_lines.items.len > 0) self.disasm_lines.items.len - 1 else 0;
                const current = @as(i64, @intCast(self.disasm_scroll));
                var next = current + delta;
                if (next < 0) next = 0;
                if (@as(usize, @intCast(next)) > max_scroll) {
                    next = @intCast(max_scroll);
                }
                self.disasm_scroll = @intCast(next);
            },
            .stack => {
                if (self.debug == null) return;
                const debug = &self.debug.?;
                if (debug.frames.items.len == 0) return;
                const current = @as(i64, @intCast(debug.selected_frame));
                var next = current + delta;
                if (next < 0) next = 0;
                if (@as(usize, @intCast(next)) >= debug.frames.items.len) {
                    next = @intCast(debug.frames.items.len - 1);
                }
                debug.selected_frame = @intCast(next);
                try self.onFramesUpdated();
            },
            .memory => {
                if (self.memory_view == null) return;
                if (self.debug == null) return;
                const base = @as(i64, @intCast(self.memory_view.?.base));
                const step: i64 = @intCast(memory_bytes_per_line);
                var next = base + delta * step;
                if (next < 0) next = 0;
                const frame_id = self.debug.?.selectedFrameId();
                const seq = try self.debug.?.sendReadMemory(@intCast(next), self.memoryReadCount(), frame_id);
                try self.debug.?.pending.put(seq, .read_memory);
            },
            else => {},
        }
    }

    fn handleKey(self: *App, key: KeyEvent) !void {
        switch (self.mode) {
            .command => try self.handleCommandKey(key),
            .repl => try self.handleReplKey(key),
            .normal => try self.handleNormalKey(key),
        }
    }

    fn handleCommandKey(self: *App, key: KeyEvent) !void {
        switch (key) {
            .char => |cp| {
                if (cp <= 0x7f) try self.command.push(@intCast(cp));
            },
            .special => |sp| {
                switch (sp) {
                    .enter => {
                        try self.execCommand(self.command.text());
                        self.exitToNormal();
                        self.command.reset();
                    },
                    .esc => {
                        self.exitToNormal();
                        self.command.reset();
                    },
                    .backspace => self.command.pop(),
                    else => {},
                }
            },
            .ctrl => |c| {
                if (c == 3) {
                    self.exitToNormal();
                    self.command.reset();
                }
            },
            else => {},
        }
    }

    fn handleReplKey(self: *App, key: KeyEvent) !void {
        switch (key) {
            .char => |cp| {
                if (cp <= 0x7f) try self.replInsertByte(@intCast(cp));
            },
            .special => |sp| {
                switch (sp) {
                    .enter => try self.submitRepl(),
                    .esc => self.exitToNormal(),
                    .backspace => self.replBackspace(),
                    .delete => self.replDelete(),
                    .left => {
                        if (self.repl_cursor > 0) self.repl_cursor -= 1;
                    },
                    .right => {
                        if (self.repl_cursor < self.repl_input.items.len) self.repl_cursor += 1;
                    },
                    .home => self.repl_cursor = 0,
                    .end => self.repl_cursor = self.repl_input.items.len,
                    else => {},
                }
            },
            .ctrl => |c| {
                if (c == 3) self.exitToNormal();
            },
            else => {},
        }
    }

    fn handleNormalKey(self: *App, key: KeyEvent) !void {
        switch (key) {
            .char => |cp| {
                switch (cp) {
                    'q' => self.should_quit = true,
                    ':' => self.enterCommandMode(),
                    'c' => try self.sendDebugCommand("continue", "running"),
                    'p' => try self.sendDebugCommand("pause", "paused"),
                    'n' => try self.sendDebugCommand("next", "stepping"),
                    'i' => {
                        if (self.activeTabKind() == .repl) {
                            self.enterReplMode();
                        } else {
                            try self.sendDebugCommand("stepIn", "stepping");
                        }
                    },
                    'o' => try self.sendDebugCommand("stepOut", "stepping"),
                    'r' => try self.log.add(Icons.status_warn ++ " reverse step not available"),
                    'b' => try self.toggleBreakpointAtCursor(),
                    'B' => try self.enterCommandModeWith("break "),
                    'w' => try self.enterCommandModeWith("watch "),
                    'W' => try self.enterCommandModeWith("watch "),
                    'x' => try self.enterCommandModeWith("watch del "),
                    '/' => try self.log.add("search"),
                    '?' => try self.log.add("help"),
                    else => {},
                }
            },
            .ctrl => |c| {
                if (c == 3) self.should_quit = true;
            },
            .special => |sp| {
                switch (sp) {
                    .tab => self.cycleTab(1),
                    .page_up => try self.scrollActive(-8),
                    .page_down => try self.scrollActive(8),
                    .up => try self.scrollActive(-1),
                    .down => try self.scrollActive(1),
                    .enter => if (self.activeTabKind() == .repl) self.enterReplMode(),
                    else => {},
                }
            },
            else => {},
        }
    }

    fn enterCommandMode(self: *App) void {
        self.mode = .command;
        self.command.reset();
    }

    fn enterCommandModeWith(self: *App, text: []const u8) !void {
        self.mode = .command;
        try self.command.setText(text);
    }

    fn enterReplMode(self: *App) void {
        self.mode = .repl;
        self.repl_cursor = self.repl_input.items.len;
    }

    fn exitToNormal(self: *App) void {
        self.mode = .normal;
    }

    fn replInsertByte(self: *App, byte: u8) !void {
        try self.repl_input.insert(self.repl_cursor, byte);
        self.repl_cursor += 1;
    }

    fn replBackspace(self: *App) void {
        if (self.repl_cursor == 0) return;
        self.repl_cursor -= 1;
        _ = self.repl_input.orderedRemove(self.repl_cursor);
    }

    fn replDelete(self: *App) void {
        if (self.repl_cursor >= self.repl_input.items.len) return;
        _ = self.repl_input.orderedRemove(self.repl_cursor);
    }

    fn submitRepl(self: *App) !void {
        const trimmed = std.mem.trim(u8, self.repl_input.items, " \t");
        if (trimmed.len == 0) {
            self.repl_input.clearRetainingCapacity();
            self.repl_cursor = 0;
            return;
        }
        if (trimmed[0] == ':') {
            const cmd = std.mem.trimLeft(u8, trimmed[1..], " ");
            try self.execCommand(cmd);
            self.repl_input.clearRetainingCapacity();
            self.repl_cursor = 0;
            return;
        }
        var expr_buf: ?[]u8 = null;
        defer if (expr_buf) |buf| self.allocator.free(buf);
        var expr = trimmed;
        if (try normalizeReplExpr(self.allocator, trimmed)) |normalized| {
            expr_buf = normalized;
            expr = normalized;
        }
        const line = try std.fmt.allocPrint(self.allocator, ">> {s}", .{trimmed});
        defer self.allocator.free(line);
        try self.log.add(line);
        if (self.debug) |*debug| {
            const frame_id = if (self.repl_context == .frame) debug.selectedFrameId() else null;
            const context: ?[]const u8 = if (self.repl_context == .repl or frame_id == null) @as([]const u8, "repl") else null;
            const seq = try debug.sendEvaluate(expr, frame_id, context);
            try debug.pending.put(seq, .evaluate_repl);
        } else {
            try self.log.add(Icons.status_warn ++ " no debug session");
        }
        self.repl_input.clearRetainingCapacity();
        self.repl_cursor = 0;
    }

    fn setReplCursorFromPoint(self: *App, rect: Rect, p: Point) void {
        const prompt_cols: i32 = @intCast(textWidth(replPrompt(self.repl_context)));
        const inner_w = rect.w - 2;
        const text_len = self.repl_input.items.len;
        if (inner_w <= 0 or inner_w <= prompt_cols) {
            self.repl_cursor = text_len;
            return;
        }
        const available = @as(usize, @intCast(inner_w - prompt_cols));
        const start = if (text_len > available) text_len - available else 0;
        const prompt_y = rect.y + rect.h - 1;
        if (p.y != prompt_y) {
            self.repl_cursor = text_len;
            return;
        }
        var col = p.x - (rect.x + 1 + prompt_cols);
        if (col < 0) {
            self.repl_cursor = start;
            return;
        }
        const max_col = @as(i32, @intCast(available));
        if (col > max_col) col = max_col;
        var cursor = start + @as(usize, @intCast(col));
        if (cursor > text_len) cursor = text_len;
        self.repl_cursor = cursor;
    }

    fn setCommandCursorFromPoint(self: *App, rect: Rect, p: Point) void {
        const prompt_cols: i32 = @intCast(textWidth(commandPrompt()));
        const inner_w = rect.w - 2;
        const text_len = self.command.input.items.len;
        if (inner_w <= 0 or inner_w <= prompt_cols) {
            self.command.cursor = text_len;
            return;
        }
        const available = @as(usize, @intCast(inner_w - prompt_cols));
        const start = if (text_len > available) text_len - available else 0;
        if (p.y != rect.y) {
            self.command.cursor = text_len;
            return;
        }
        var col = p.x - (rect.x + 1 + prompt_cols);
        if (col < 0) {
            self.command.cursor = start;
            return;
        }
        const max_col = @as(i32, @intCast(available));
        if (col > max_col) col = max_col;
        var cursor = start + @as(usize, @intCast(col));
        if (cursor > text_len) cursor = text_len;
        self.command.cursor = cursor;
    }

    fn handleMouse(self: *App, mouse: MouseEvent) !void {
        const p = Point{ .x = mouse.x, .y = mouse.y };
        switch (mouse.kind) {
            .down => {
                self.mouse_down_active = true;
                if (mouse.button == .left or mouse.button == .right) {
                    const cmd_rect = self.commandRect();
                    const in_command = if (cmd_rect) |rect| rect.contains(p) else false;
                    if (!in_command and !(self.toolbar_rect.h > 0 and self.toolbar_rect.contains(p))) {
                        self.activateGroupAt(p);
                    }
                }
                if (mouse.button == .left) {
                    if (self.tryFocusCommand(p)) return;
                    if (self.handleToolbarPress(p)) {
                        if (!self.mouse_release_supported) {
                            if (self.toolbar_press) |press| {
                                self.toolbar_press = null;
                                try self.handleToolbarAction(press.action);
                            }
                        }
                        return;
                    }
                    if (self.tryStartSplitterDrag(p)) {
                        self.exitToNormal();
                        return;
                    }
                    if (self.tryStartTabDrag(p)) {
                        self.exitToNormal();
                        return;
                    }
                    if (self.tryFocusInput(p)) return;
                    if (try self.handlePanelClick(p, mouse.button)) {
                        self.exitToNormal();
                        return;
                    }
                    self.exitToNormal();
                } else if (mouse.button == .right) {
                    if (try self.handlePanelClick(p, mouse.button)) {
                        self.exitToNormal();
                        return;
                    }
                }
            },
            .drag => {
                if (self.toolbar_press) |press| {
                    if (!press.rect.contains(p)) {
                        self.toolbar_press = null;
                    }
                }
                switch (self.drag) {
                    .splitter => |*drag| {
                        self.updateSplitter(drag, p);
                    },
                    .tab => |*drag| {
                        if (!drag.moved) {
                            const dx = @abs(p.x - drag.start.x);
                            const dy = @abs(p.y - drag.start.y);
                            if (dx + dy < 2) return;
                            drag.moved = true;
                        }
                        drag.pos = p;
                        if (drag.moved) self.updateTabTarget(drag);
                    },
                    else => {},
                }
            },
            .up => {
                const had_down = self.mouse_down_active;
                self.mouse_down_active = false;
                self.mouse_release_supported = true;
                switch (self.drag) {
                    .splitter => self.drag = .none,
                    .tab => |drag| {
                        if (drag.moved) {
                            try self.applyTabDrop(drag);
                        } else {
                            self.setActiveTab(drag.from_group, drag.tab_index);
                        }
                        self.drag = .none;
                    },
                    else => {},
                }
                if (mouse.button == .left) {
                    if (try self.handleToolbarRelease(p)) return;
                }
                if (self.drag == .none and !had_down) {
                    if (mouse.button == .left) {
                        if (self.tryFocusCommand(p)) return;
                        if (self.tryFocusInput(p)) return;
                        if (try self.handlePanelClick(p, mouse.button)) {
                            self.exitToNormal();
                            return;
                        }
                        self.exitToNormal();
                    } else if (mouse.button == .right) {
                        if (try self.handlePanelClick(p, mouse.button)) {
                            self.exitToNormal();
                            return;
                        }
                    }
                }
            },
            .scroll => {
                if (self.toolbar_rect.h > 0 and self.toolbar_rect.contains(p)) return;
                if (self.commandRect()) |rect| {
                    if (rect.contains(p)) return;
                }
                self.activateGroupAt(p);
                const step = self.mouseScrollStep(mouse.mods);
                if (mouse.button == .wheel_up) {
                    try self.scrollActive(-step);
                } else if (mouse.button == .wheel_down) {
                    try self.scrollActive(step);
                }
            },
            else => {},
        }
    }

    fn commandRect(self: *App) ?Rect {
        if (self.mode != .command) return null;
        if (self.canvas.height < 2) return null;
        const y = @as(i32, @intCast(self.canvas.height - 2));
        return Rect{ .x = 0, .y = y, .w = @intCast(self.canvas.width), .h = 1 };
    }

    fn tryFocusCommand(self: *App, p: Point) bool {
        const rect = self.commandRect() orelse return false;
        if (!rect.contains(p)) return false;
        self.setCommandCursorFromPoint(rect, p);
        return true;
    }

    fn tryFocusInput(self: *App, p: Point) bool {
        for (self.layout.groups.items) |group| {
            if (!group.rect.contains(p)) continue;
            self.active_group = group.id;
            const node = &self.dock.nodes.items[group.id];
            if (node.data != .group) return false;
            const active = if (node.data.group.tabs.items.len == 0)
                .source
            else
                node.data.group.tabs.items[node.data.group.active];
            if (active == .repl and group.body.contains(p)) {
                self.enterReplMode();
                self.setReplCursorFromPoint(group.body, p);
                return true;
            }
            return false;
        }
        return false;
    }

    fn execCommand(self: *App, text: []const u8) !void {
        if (text.len == 0) return;
        if (std.mem.eql(u8, text, "q") or std.mem.eql(u8, text, "quit")) {
            self.should_quit = true;
            return;
        }
        if (std.mem.eql(u8, text, "repl") or std.mem.eql(u8, text, "repl global")) {
            self.repl_context = .repl;
            try self.log.add("repl context: global");
            return;
        }
        if (std.mem.eql(u8, text, "frame") or std.mem.eql(u8, text, "repl frame")) {
            self.repl_context = .frame;
            try self.log.add("repl context: frame");
            return;
        }
        if (std.mem.eql(u8, text, "layout reset")) {
            try self.log.add("layout reset" );
            return;
        }
        if (std.mem.startsWith(u8, text, "break ")) {
            const spec = std.mem.trimLeft(u8, text["break ".len..], " ");
            try self.parseBreakpointSpec(spec);
            return;
        }
        if (std.mem.startsWith(u8, text, "b ")) {
            const spec = std.mem.trimLeft(u8, text["b ".len..], " ");
            try self.parseBreakpointSpec(spec);
            return;
        }
        if (std.mem.startsWith(u8, text, "watch ")) {
            const spec = std.mem.trimLeft(u8, text["watch ".len..], " ");
            try self.parseWatchSpec(spec);
            return;
        }
        if (std.mem.startsWith(u8, text, "w ")) {
            const spec = std.mem.trimLeft(u8, text["w ".len..], " ");
            try self.parseWatchSpec(spec);
            return;
        }
        if (std.mem.startsWith(u8, text, "frame ")) {
            const spec = std.mem.trimLeft(u8, text["frame ".len..], " ");
            const idx = std.fmt.parseInt(usize, spec, 10) catch {
                try self.log.add("! invalid frame index");
                return;
            };
            if (idx == 0) {
                try self.selectFrame(0);
            } else {
                try self.selectFrame(idx - 1);
            }
            return;
        }
        if (std.mem.startsWith(u8, text, "mem ")) {
            const spec = std.mem.trimLeft(u8, text["mem ".len..], " ");
            if (parse_address(spec)) |addr| {
                if (self.debug) |*debug| {
                    const frame_id = debug.selectedFrameId();
                    const seq = try debug.sendReadMemory(addr, self.memoryReadCount(), frame_id);
                    try debug.pending.put(seq, .read_memory);
                }
            } else {
                try self.log.add("! invalid memory address");
            }
            return;
        }
        if (std.mem.startsWith(u8, text, "tab ")) {
            const name = std.mem.trimLeft(u8, text[4..], " ");
            self.switchToTabByName(name);
            return;
        }
        try self.log.add(text);
    }

    fn parseBreakpointSpec(self: *App, spec: []const u8) !void {
        if (spec.len == 0) return;
        const colon = std.mem.lastIndexOfScalar(u8, spec, ':');
        if (colon) |idx| {
            const path = spec[0..idx];
            const line_text = std.mem.trimLeft(u8, spec[idx + 1 ..], " ");
            const line = std.fmt.parseInt(i64, line_text, 10) catch {
                try self.log.add("! invalid breakpoint line");
                return;
            };
            try self.addBreakpoint(path, line);
            return;
        }
        const path = self.source_view.path orelse {
            try self.log.add(Icons.file ++ " no source file selected");
            return;
        };
        const line = std.fmt.parseInt(i64, spec, 10) catch {
            try self.log.add("! invalid breakpoint line");
            return;
        };
        try self.addBreakpoint(path, line);
    }

    fn parseWatchSpec(self: *App, spec: []const u8) !void {
        if (spec.len == 0) return;
        if (std.mem.startsWith(u8, spec, "del ")) {
            const idx_text = std.mem.trimLeft(u8, spec["del ".len..], " ");
            const idx = std.fmt.parseInt(usize, idx_text, 10) catch {
                try self.log.add("! invalid watch index");
                return;
            };
            if (idx == 0) return;
            self.removeWatch(idx - 1);
            return;
        }
        if (std.mem.startsWith(u8, spec, "rm ")) {
            const idx_text = std.mem.trimLeft(u8, spec["rm ".len..], " ");
            const idx = std.fmt.parseInt(usize, idx_text, 10) catch {
                try self.log.add("! invalid watch index");
                return;
            };
            if (idx == 0) return;
            self.removeWatch(idx - 1);
            return;
        }
        try self.addWatch(spec);
    }

    fn sendDebugCommand(self: *App, command: []const u8, status: []const u8) !void {
        if (self.debug) |*debug| {
            _ = try debug.sendSimple(command);
            self.status_line = status;
            return;
        }
        try self.log.add(Icons.status_warn ++ " no debug session");
    }

    fn switchToTabByName(self: *App, name: []const u8) void {
        const group_idx = self.active_group;
        var node = &self.dock.nodes.items[group_idx];
        if (node.data != .group) return;
        for (node.data.group.tabs.items, 0..) |tab, idx| {
            if (std.mem.eql(u8, tabName(tab), name) or std.mem.eql(u8, tabTitle(tab), name)) {
                node.data.group.active = idx;
                return;
            }
        }
    }

    fn cycleTab(self: *App, delta: i32) void {
        var node = &self.dock.nodes.items[self.active_group];
        if (node.data != .group) return;
        const count = node.data.group.tabs.items.len;
        if (count == 0) return;
        const current = @as(i32, @intCast(node.data.group.active));
        var next = current + delta;
        if (next < 0) next = @as(i32, @intCast(count)) - 1;
        if (next >= @as(i32, @intCast(count))) next = 0;
        node.data.group.active = @intCast(next);
    }

    fn activeTabKind(self: *App) TabKind {
        const node = &self.dock.nodes.items[self.active_group];
        if (node.data != .group or node.data.group.tabs.items.len == 0) return .source;
        return node.data.group.tabs.items[node.data.group.active];
    }

    fn activeBodyRect(self: *App) ?Rect {
        for (self.layout.groups.items) |group| {
            if (group.id == self.active_group) return group.body;
        }
        return null;
    }

    fn activeVisibleRows(self: *App) ?usize {
        const rect = self.activeBodyRect() orelse return null;
        if (rect.h <= 0) return null;
        var rows = rect.h;
        switch (self.activeTabKind()) {
            .repl, .output => {
                if (rows <= 1) return null;
                rows -= 1;
            },
            else => {},
        }
        return @intCast(rows);
    }

    fn mouseScrollStep(self: *App, mods: KeyMods) i32 {
        var step: i32 = 3;
        if (mods.shift or mods.alt) {
            if (self.activeVisibleRows()) |rows| {
                const page = if (rows > 1) rows - 1 else 1;
                step = @intCast(page);
            }
        } else if (mods.ctrl) {
            step = 1;
        }
        return step;
    }

    fn memoryBodyRect(self: *App) ?Rect {
        for (self.layout.groups.items) |group| {
            const node = &self.dock.nodes.items[group.id];
            if (node.data != .group) continue;
            if (node.data.group.tabs.items.len == 0) continue;
            if (node.data.group.tabs.items[node.data.group.active] == .memory) {
                return group.body;
            }
        }
        return null;
    }

    fn memoryReadCount(self: *App) i64 {
        const rect = self.memoryBodyRect() orelse return default_memory_read_bytes;
        if (rect.h <= 0) return default_memory_read_bytes;
        const rows: i64 = @intCast(rect.h);
        const count = rows * @as(i64, @intCast(memory_bytes_per_line));
        if (count < default_memory_read_bytes) return default_memory_read_bytes;
        return count;
    }

    fn activateGroupAt(self: *App, p: Point) void {
        for (self.layout.groups.items) |group| {
            if (group.rect.contains(p)) {
                self.active_group = group.id;
                return;
            }
        }
    }

    fn setActiveTab(self: *App, group_id: usize, tab_idx: usize) void {
        if (group_id >= self.dock.nodes.items.len) return;
        var node = &self.dock.nodes.items[group_id];
        if (node.data != .group) return;
        if (tab_idx >= node.data.group.tabs.items.len) return;
        self.active_group = group_id;
        node.data.group.active = tab_idx;
    }

    fn focusTab(self: *App, kind: TabKind) void {
        for (self.layout.groups.items) |group| {
            const node = &self.dock.nodes.items[group.id];
            if (node.data != .group) continue;
            for (node.data.group.tabs.items, 0..) |tab, idx| {
                if (tab == kind) {
                    self.active_group = group.id;
                    node.data.group.active = idx;
                    return;
                }
            }
        }
    }

    fn handleToolbarClick(self: *App, p: Point) !bool {
        if (self.toolbar_rect.h == 0) return false;
        if (!self.toolbar_rect.contains(p)) return false;
        for (self.toolbar_buttons.items) |btn| {
            if (btn.rect.contains(p)) {
                try self.handleToolbarAction(btn.action);
                return true;
            }
        }
        return true;
    }

    fn handleToolbarPress(self: *App, p: Point) bool {
        if (self.toolbar_rect.h == 0) return false;
        if (!self.toolbar_rect.contains(p)) return false;
        self.toolbar_press = null;
        for (self.toolbar_buttons.items) |btn| {
            if (btn.rect.contains(p)) {
                self.toolbar_press = .{ .rect = btn.rect, .action = btn.action };
                return true;
            }
        }
        return true;
    }

    fn handleToolbarRelease(self: *App, p: Point) !bool {
        if (self.toolbar_press) |press| {
            self.toolbar_press = null;
            if (press.rect.contains(p)) {
                try self.handleToolbarAction(press.action);
            }
            return true;
        }
        return try self.handleToolbarClick(p);
    }

    fn handleToolbarAction(self: *App, action: ToolbarAction) !void {
        switch (action) {
            .run => try self.sendDebugCommand("continue", "running"),
            .pause => try self.sendDebugCommand("pause", "paused"),
            .step_over => try self.sendDebugCommand("next", "stepping"),
            .step_in => try self.sendDebugCommand("stepIn", "stepping"),
            .step_out => try self.sendDebugCommand("stepOut", "stepping"),
            .breakpoint => {
                if (self.activeTabKind() == .source and self.source_view.line > 0) {
                    try self.toggleBreakpointAtCursor();
                } else {
                    self.focusTab(.source);
                    try self.enterCommandModeWith("break ");
                }
            },
            .watch => try self.enterCommandModeWith("watch "),
            .repl => {
                self.focusTab(.repl);
                self.enterReplMode();
            },
            .command => self.enterCommandMode(),
            .quit => self.should_quit = true,
        }
        switch (action) {
            .repl, .command => {},
            else => self.exitToNormal(),
        }
    }

    fn handlePanelClick(self: *App, p: Point, button: MouseButton) !bool {
        for (self.layout.groups.items) |group| {
            if (!group.body.contains(p)) continue;
            self.active_group = group.id;
            const node = &self.dock.nodes.items[group.id];
            if (node.data != .group) return false;
            const active = if (node.data.group.tabs.items.len == 0)
                .source
            else
                node.data.group.tabs.items[node.data.group.active];
            return switch (active) {
                .source => self.handleSourceClick(group.body, p, button),
                .disasm => self.handleDisasmClick(group.body, p, button),
                .stack => self.handleStackClick(group.body, p),
                .tasks => self.handleTasksClick(group.body, p),
                .threads => self.handleThreadsClick(group.body, p),
                .breakpoints => self.handleBreakpointsClick(group.body, p, button),
                .watches => self.handleWatchesClick(group.body, p, button),
                .scopes => self.handleScopesClick(group.body, p, button),
                .repl => false,
                .output => self.handleOutputClick(button),
                .events => self.handleEventsClick(button),
                .memory => self.handleMemoryClick(group.body, p),
            };
        }
        return false;
    }

    fn handleSourceClick(self: *App, rect: Rect, p: Point, button: MouseButton) !bool {
        const path = self.source_view.path orelse return false;
        const file = (try self.source_cache.get(path)) orelse return false;
        if (!rect.contains(p)) return false;
        const row = p.y - rect.y;
        if (row < 0) return false;
        const line_index = self.source_view.scroll + @as(usize, @intCast(row));
        if (line_index >= file.lines.len) return false;
        self.source_view.line = line_index + 1;
        const line_no: i64 = @intCast(line_index + 1);

        const line_digits = countDigits(file.lines.len);
        const num_w: i32 = @intCast(@max(@as(usize, 2), line_digits));
        const mark_w: i32 = 2;
        const gutter_w = mark_w + num_w + 1;
        const inner_x = rect.x + 1;
        if (p.x < inner_x + gutter_w) {
            switch (button) {
                .left => try self.toggleBreakpointAtLine(path, line_no),
                .right => _ = try self.removeBreakpointAtLine(path, line_no),
                else => {},
            }
        }
        return true;
    }

    fn handleDisasmClick(self: *App, rect: Rect, p: Point, _: MouseButton) !bool {
        if (self.disasm_lines.items.len == 0) return false;
        const row = p.y - rect.y;
        if (row < 0) return false;
        const idx = self.disasm_scroll + @as(usize, @intCast(row));
        if (idx >= self.disasm_lines.items.len) return false;
        self.disasm_cursor = idx;
        const line = self.disasm_lines.items[idx];
        if (line.source_path) |path| {
            try self.setSourcePath(path);
            if (line.line) |line_no| self.source_view.line = @intCast(line_no);
            self.adjustSourceScroll();
        }
        return true;
    }

    fn handleStackClick(self: *App, rect: Rect, p: Point) !bool {
        if (self.debug == null) return false;
        const row = p.y - rect.y;
        if (row < 0) return false;
        const idx = @as(usize, @intCast(row));
        if (idx >= self.debug.?.frames.items.len) return false;
        try self.selectFrame(idx);
        return true;
    }

    fn handleTasksClick(self: *App, rect: Rect, p: Point) !bool {
        if (self.tasks.items.len == 0) return false;
        const row = p.y - rect.y;
        if (row < 0) return false;
        const idx = @as(usize, @intCast(row));
        if (idx >= self.tasks.items.len) return false;
        const task = self.tasks.items[idx];
        self.selected_task = task.id;
        try self.selectThreadById(task.id);
        return true;
    }

    fn handleThreadsClick(self: *App, rect: Rect, p: Point) !bool {
        if (self.threads.items.len == 0) return false;
        const row = p.y - rect.y;
        if (row < 0) return false;
        const idx = @as(usize, @intCast(row));
        if (idx >= self.threads.items.len) return false;
        const thread = self.threads.items[idx];
        try self.selectThreadById(thread.id);
        return true;
    }

    fn handleBreakpointsClick(self: *App, rect: Rect, p: Point, button: MouseButton) !bool {
        if (self.breakpoints.items.len == 0) return false;
        const row = p.y - rect.y;
        if (row < 0) return false;
        const idx = @as(usize, @intCast(row));
        if (idx >= self.breakpoints.items.len) return false;
        const bp = self.breakpoints.items[idx];
        switch (button) {
            .left => {
                try self.jumpToSource(bp.path, bp.line);
                self.focusTab(.source);
            },
            .right => {
                try self.removeBreakpointAtIndex(idx);
            },
            else => {},
        }
        return true;
    }

    fn handleWatchesClick(self: *App, rect: Rect, p: Point, button: MouseButton) !bool {
        if (self.watches.items.len == 0) return false;
        const row = p.y - rect.y;
        if (row < 0) return false;
        const idx = @as(usize, @intCast(row));
        if (idx >= self.watches.items.len) return false;
        const watch = self.watches.items[idx];
        switch (button) {
            .left => {
                const text = try std.fmt.allocPrint(self.allocator, "watch {s}", .{watch.expr});
                defer self.allocator.free(text);
                try self.enterCommandModeWith(text);
            },
            .right => self.removeWatch(idx),
            else => {},
        }
        return true;
    }

    fn handleScopesClick(self: *App, rect: Rect, p: Point, button: MouseButton) !bool {
        const row = p.y - rect.y;
        if (row < 0) return false;
        var cursor: i32 = 0;
        if (row == cursor) return false; // locals header
        cursor += 1;
        if (self.scopes.locals.items.len == 0) {
            if (row == cursor) return false;
            cursor += 1;
        } else {
            const locals_len = @as(i32, @intCast(self.scopes.locals.items.len));
            if (row < cursor + locals_len) {
                const idx = @as(usize, @intCast(row - cursor));
                const entry = self.scopes.locals.items[idx];
                return try self.handleScopeEntryClick(entry.name, button);
            }
            cursor += locals_len;
        }
        if (row == cursor) return false; // registers header
        cursor += 1;
        if (self.scopes.registers.items.len == 0) return false;
        const regs_len = @as(i32, @intCast(self.scopes.registers.items.len));
        if (row < cursor + regs_len) {
            const idx = @as(usize, @intCast(row - cursor));
            const entry = self.scopes.registers.items[idx];
            return try self.handleScopeEntryClick(entry.name, button);
        }
        return false;
    }

    fn handleScopeEntryClick(self: *App, name: []const u8, button: MouseButton) !bool {
        switch (button) {
            .left => {
                self.repl_input.clearRetainingCapacity();
                try self.repl_input.appendSlice(name);
                self.repl_cursor = self.repl_input.items.len;
                self.repl_context = .frame;
                self.focusTab(.repl);
                self.enterReplMode();
            },
            .right => try self.addWatch(name),
            else => {},
        }
        return true;
    }

    fn handleMemoryClick(self: *App, rect: Rect, p: Point) !bool {
        if (self.memory_view == null or self.debug == null) return false;
        const row = p.y - rect.y;
        if (row < 0) return false;
        const base = self.memory_view.?.base + @as(usize, @intCast(row)) * memory_bytes_per_line;
        const frame_id = self.debug.?.selectedFrameId();
        const seq = try self.debug.?.sendReadMemory(base, self.memoryReadCount(), frame_id);
        try self.debug.?.pending.put(seq, .read_memory);
        return true;
    }

    fn handleOutputClick(self: *App, button: MouseButton) bool {
        if (button == .right) {
            self.output.clear();
            return true;
        }
        return true;
    }

    fn handleEventsClick(self: *App, button: MouseButton) bool {
        if (button == .right) {
            self.events.clear();
            return true;
        }
        return true;
    }

    fn tryStartSplitterDrag(self: *App, p: Point) bool {
        for (self.layout.splitters.items) |split| {
            if (split.rect.contains(p)) {
                const node = self.dock.nodes.items[split.node_id];
                if (node.data != .split) continue;
                self.drag = .{ .splitter = .{
                    .node_id = split.node_id,
                    .axis = split.axis,
                    .start = p,
                    .start_ratio = node.data.split.ratio,
                    .bounds = split.bounds,
                } };
                return true;
            }
        }
        return false;
    }

    fn tryStartTabDrag(self: *App, p: Point) bool {
        for (self.layout.groups.items) |layout| {
            if (!layout.header.contains(p)) continue;
            const node = &self.dock.nodes.items[layout.id];
            if (node.data != .group) continue;
            const tab = self.hitTestTab(layout, node.data.group, p) orelse continue;
            node.data.group.active = tab;
            self.drag = .{ .tab = .{ .from_group = layout.id, .tab_index = tab, .start = p, .pos = p, .moved = false } };
            return true;
        }
        return false;
    }

    fn hitTestTab(self: *App, layout: GroupLayout, group: Group, p: Point) ?usize {
        _ = self;
        if (layout.header.h == 0) return null;
        const bar = layout.header;
        const tab_count = group.tabs.items.len;
        if (tab_count == 0) return null;
        const slot = @divTrunc(bar.w, @as(i32, @intCast(tab_count)));
        if (slot < 4) {
            if (bar.contains(p)) return group.active;
            return null;
        }

        var x = bar.x;
        for (group.tabs.items, 0..) |_, idx| {
            const slot_w = if (idx + 1 == tab_count)
                bar.x + bar.w - x
            else
                slot;
            const rect = Rect{ .x = x, .y = bar.y, .w = slot_w, .h = 1 };
            if (rect.contains(p)) return idx;
            x += slot_w;
        }
        return null;
    }

    fn updateSplitter(self: *App, drag: *SplitDrag, p: Point) void {
        var node = &self.dock.nodes.items[drag.node_id];
        if (node.data != .split) return;
        const delta = if (drag.axis == .vertical) p.x - drag.start.x else p.y - drag.start.y;
        const base = if (drag.axis == .vertical) drag.bounds.w - 1 else drag.bounds.h - 1;
        if (base <= 0) return;
        const ratio_delta = @as(f32, @floatFromInt(delta)) / @as(f32, @floatFromInt(base));
        var next = drag.start_ratio + ratio_delta;
        if (next < 0.1) next = 0.1;
        if (next > 0.9) next = 0.9;
        node.data.split.ratio = next;
    }

    fn updateTabTarget(self: *App, drag: *TabDrag) void {
        drag.target_group = null;
        drag.target_zone = null;
        for (self.layout.groups.items) |group| {
            if (!group.rect.contains(drag.pos)) continue;
            drag.target_group = group.id;
            drag.target_zone = self.pickZone(group.rect, drag.pos);
            return;
        }
    }

    fn pickZone(self: *App, rect: Rect, p: Point) DropZone {
        _ = self;
        const left = rect.x + @divTrunc(rect.w, 5);
        const right = rect.x + rect.w - @divTrunc(rect.w, 5);
        const top = rect.y + @divTrunc(rect.h, 5);
        const bottom = rect.y + rect.h - @divTrunc(rect.h, 5);
        if (p.x < left) return .left;
        if (p.x > right) return .right;
        if (p.y < top) return .top;
        if (p.y > bottom) return .bottom;
        return .center;
    }

    fn applyTabDrop(self: *App, drag: TabDrag) !void {
        const from = drag.from_group;
        const target_id = drag.target_group orelse return;
        const zone = drag.target_zone orelse return;
        const from_node = &self.dock.nodes.items[from];
        if (from_node.data != .group) return;
        if (target_id == from and zone == .center) return;
        if (target_id == from and zone != .center and from_node.data.group.tabs.items.len <= 1) {
            return;
        }
        const tab = from_node.data.group.tabs.items[drag.tab_index];
        _ = from_node.data.group.tabs.orderedRemove(drag.tab_index);
        if (from_node.data.group.tabs.items.len == 0) {
            self.dock.removeGroup(from);
        } else if (from_node.data.group.active >= from_node.data.group.tabs.items.len) {
            from_node.data.group.active = from_node.data.group.tabs.items.len - 1;
        }

        if (zone == .center) {
            const target = &self.dock.nodes.items[target_id];
            if (target.data != .group) return;
            try target.data.group.tabs.append(tab);
            target.data.group.active = target.data.group.tabs.items.len - 1;
            self.active_group = target_id;
            return;
        }

        const new_group_id = try self.dock.addGroup(&.{tab}, 0);
        var axis: Axis = .vertical;
        var ratio: f32 = 0.5;
        var first: usize = target_id;
        var second: usize = new_group_id;
        switch (zone) {
            .left => {
                axis = .vertical;
                ratio = 0.5;
                first = new_group_id;
                second = target_id;
            },
            .right => {
                axis = .vertical;
                ratio = 0.5;
                first = target_id;
                second = new_group_id;
            },
            .top => {
                axis = .horizontal;
                ratio = 0.5;
                first = new_group_id;
                second = target_id;
            },
            .bottom => {
                axis = .horizontal;
                ratio = 0.5;
                first = target_id;
                second = new_group_id;
            },
            else => {},
        }
        const split_id = try self.dock.addSplit(axis, ratio, first, second);
        const parent_id = self.dock.nodes.items[target_id].parent;
        if (parent_id) |pid| {
            self.dock.replaceChild(pid, target_id, split_id);
        } else {
            self.dock.root = split_id;
        }
        self.active_group = new_group_id;
    }

    fn draw(self: *App) !void {
        self.layout.clear();
        const size = self.term.size();
        const toolbar_h: i32 = if (size.rows > 2) 1 else 0;
        self.toolbar_rect = Rect{ .x = 0, .y = 0, .w = @intCast(size.cols), .h = toolbar_h };
        const full_h = @as(i32, @intCast(size.rows - 1)) - toolbar_h;
        const full = Rect{ .x = 0, .y = toolbar_h, .w = @intCast(size.cols), .h = if (full_h > 0) full_h else 0 };
        self.computeLayout(self.dock.root, full);

        self.canvas.clear();
        self.drawBackground();
        self.drawToolbar();
        for (self.layout.groups.items) |group| {
            try self.drawGroup(group);
        }
        for (self.layout.splitters.items) |split| {
            self.drawSplitter(split);
        }
        if (self.drag == .tab) {
            self.drawDropHint();
        }
        self.drawStatusBar();
        if (self.mode == .command) {
            self.drawCommandPalette();
        }
        try self.renderer.render(&self.canvas);
    }

    fn drawBackground(self: *App) void {
        const rect = Rect{ .x = 0, .y = 0, .w = @intCast(self.canvas.width), .h = @intCast(self.canvas.height) };
        self.canvas.fill(rect, ' ', self.theme.panel_text);
    }

    fn drawToolbar(self: *App) void {
        if (self.toolbar_rect.h == 0) return;
        const rect = self.toolbar_rect;
        self.toolbar_buttons.clearRetainingCapacity();
        self.canvas.fill(rect, ' ', self.theme.panel_header);

        const paused = if (self.debug) |debug| debug.paused else false;
        const run_action: ToolbarAction = if (paused) .run else .pause;
        const run_label: []const u8 = if (paused) Icons.run ++ " Run" else Icons.pause ++ " Pause";
        const run_style = if (paused) self.theme.accent_alt else self.theme.panel_header_active;

        const ToolbarItem = struct {
            action: ToolbarAction,
            label: []const u8,
            style: Style,
        };
        const items = [_]ToolbarItem{
            .{ .action = run_action, .label = run_label, .style = run_style },
            .{ .action = .step_over, .label = Icons.step_over ++ " Step", .style = self.theme.panel_header },
            .{ .action = .step_in, .label = Icons.step_in ++ " Into", .style = self.theme.panel_header },
            .{ .action = .step_out, .label = Icons.step_out ++ " Out", .style = self.theme.panel_header },
            .{ .action = .breakpoint, .label = Icons.breakpoint ++ " Break", .style = self.theme.panel_header },
            .{ .action = .watch, .label = Icons.watch ++ " Watch", .style = self.theme.panel_header },
            .{ .action = .repl, .label = Icons.repl ++ " Repl", .style = self.theme.panel_header },
            .{ .action = .command, .label = Icons.command ++ " Cmd", .style = self.theme.panel_header },
            .{ .action = .quit, .label = Icons.quit ++ " Quit", .style = self.theme.panel_header },
        };

        var x = rect.x + 1;
        const right_edge = rect.x + rect.w - 1;
        for (items) |item| {
            const label_cols = textWidth(item.label);
            const width = @as(i32, @intCast(label_cols + 2));
            if (x + width >= right_edge) break;
            const btn_rect = Rect{ .x = x, .y = rect.y, .w = width, .h = 1 };
            self.canvas.fill(btn_rect, ' ', item.style);
            self.canvas.writeClipped(x + 1, rect.y, width - 2, item.label, item.style);
            self.toolbar_buttons.append(.{ .rect = btn_rect, .action = item.action, .label = item.label }) catch {};
            x += width + 1;
        }

        const hint = Icons.mouse ++ " drag tabs · scroll panes (shift=page) · right click clears logs";
        const hint_cols = textWidth(hint);
        const hint_x = rect.x + rect.w - @as(i32, @intCast(hint_cols)) - 1;
        if (hint_x > x) {
            self.canvas.writeClipped(hint_x, rect.y, rect.w - hint_x - 1, hint, self.theme.panel_dim);
        }
    }

    fn computeLayout(self: *App, node_id: usize, rect: Rect) void {
        const node = self.dock.nodes.items[node_id];
        switch (node.data) {
            .group => {
                const inner_w: i32 = if (rect.w > 2) rect.w - 2 else 0;
                const inner_h: i32 = if (rect.h > 2) rect.h - 2 else 0;
                const inner = Rect{ .x = rect.x + 1, .y = rect.y + 1, .w = inner_w, .h = inner_h };
                const header_h: i32 = if (inner_h > 0) 1 else 0;
                const header = Rect{ .x = inner.x, .y = inner.y, .w = inner.w, .h = header_h };
                const body = Rect{ .x = inner.x, .y = inner.y + header_h, .w = inner.w, .h = inner_h - header_h };
                self.layout.groups.append(.{ .id = node_id, .rect = rect, .header = header, .body = body }) catch {};
            },
            .split => |split| {
                const overlap: i32 = 1;
                if (split.axis == .vertical) {
                    const available = rect.w - overlap;
                    if (available <= 0) return;
                    var first_span = @as(i32, @intFromFloat(@as(f32, @floatFromInt(available)) * split.ratio));
                    var min_span: i32 = 12 - overlap;
                    if (available < 12 * 2 - overlap) {
                        min_span = @divTrunc(available, 2);
                    }
                    if (min_span < 0) min_span = 0;
                    if (first_span < min_span) first_span = min_span;
                    if (first_span > available - min_span) first_span = available - min_span;
                    if (first_span < 0) first_span = 0;
                    if (first_span > available) first_span = available;
                    const first_rect = Rect{ .x = rect.x, .y = rect.y, .w = first_span + overlap, .h = rect.h };
                    const split_rect = Rect{ .x = rect.x + first_span, .y = rect.y, .w = overlap, .h = rect.h };
                    const second_rect = Rect{ .x = rect.x + first_span, .y = rect.y, .w = rect.w - first_span, .h = rect.h };
                    self.layout.splitters.append(.{
                        .node_id = node_id,
                        .rect = split_rect,
                        .axis = split.axis,
                        .bounds = rect,
                    }) catch {};
                    self.computeLayout(split.first, first_rect);
                    self.computeLayout(split.second, second_rect);
                } else {
                    const available = rect.h - overlap;
                    if (available <= 0) return;
                    var first_span = @as(i32, @intFromFloat(@as(f32, @floatFromInt(available)) * split.ratio));
                    var min_span: i32 = 6 - overlap;
                    if (available < 6 * 2 - overlap) {
                        min_span = @divTrunc(available, 2);
                    }
                    if (min_span < 0) min_span = 0;
                    if (first_span < min_span) first_span = min_span;
                    if (first_span > available - min_span) first_span = available - min_span;
                    if (first_span < 0) first_span = 0;
                    if (first_span > available) first_span = available;
                    const first_rect = Rect{ .x = rect.x, .y = rect.y, .w = rect.w, .h = first_span + overlap };
                    const split_rect = Rect{ .x = rect.x, .y = rect.y + first_span, .w = rect.w, .h = overlap };
                    const second_rect = Rect{ .x = rect.x, .y = rect.y + first_span, .w = rect.w, .h = rect.h - first_span };
                    self.layout.splitters.append(.{
                        .node_id = node_id,
                        .rect = split_rect,
                        .axis = split.axis,
                        .bounds = rect,
                    }) catch {};
                    self.computeLayout(split.first, first_rect);
                    self.computeLayout(split.second, second_rect);
                }
            },
        }
    }

    fn drawSplitter(self: *App, split: SplitLayout) void {
        const style = self.theme.panel_border;
        if (split.axis == .vertical) {
            if (split.rect.h <= 0) return;
            var y = split.rect.y;
            const end = split.rect.y + split.rect.h;
            while (y < end) : (y += 1) {
                self.canvas.putBorder(split.rect.x, y, Border.v, style);
            }
        } else {
            if (split.rect.w <= 0) return;
            var x = split.rect.x;
            const end = split.rect.x + split.rect.w;
            while (x < end) : (x += 1) {
                self.canvas.putBorder(x, split.rect.y, Border.h, style);
            }
        }
    }

    fn drawGroup(self: *App, layout: GroupLayout) !void {
        const node = &self.dock.nodes.items[layout.id];
        if (node.data != .group) return;
        self.drawPanelFrame(layout.rect, layout.id == self.active_group);
        self.drawTabs(layout, node.data.group);
        self.drawView(layout.body, node.data.group);
    }

    fn drawPanelFrame(self: *App, rect: Rect, active: bool) void {
        if (rect.w <= 0 or rect.h <= 0) return;
        const border = if (active) self.theme.accent else self.theme.panel_border;
        const right = rect.x + rect.w - 1;
        const bottom = rect.y + rect.h - 1;
        self.canvas.putBorder(rect.x, rect.y, Border.tl, border);
        if (rect.w > 1) self.canvas.putBorder(right, rect.y, Border.tr, border);
        if (rect.h > 1) self.canvas.putBorder(rect.x, bottom, Border.bl, border);
        if (rect.w > 1 and rect.h > 1) self.canvas.putBorder(right, bottom, Border.br, border);
        var x = rect.x + 1;
        while (x < right) : (x += 1) {
            self.canvas.putBorder(x, rect.y, Border.h, border);
            if (rect.h > 1) self.canvas.putBorder(x, bottom, Border.h, border);
        }
        var y = rect.y + 1;
        while (y < bottom) : (y += 1) {
            self.canvas.putBorder(rect.x, y, Border.v, border);
            if (rect.w > 1) self.canvas.putBorder(right, y, Border.v, border);
        }
    }

    fn drawTabs(self: *App, layout: GroupLayout, group: Group) void {
        if (layout.header.h == 0 or layout.header.w < 4) return;
        const bar = layout.header;
        self.canvas.fill(bar, ' ', self.theme.panel_header);
        const tab_count = group.tabs.items.len;
        if (tab_count == 0) return;
        const slot = @divTrunc(bar.w, @as(i32, @intCast(tab_count)));
        if (slot < 4) {
            const active = group.tabs.items[group.active];
            const label = tabTitle(active);
            var label_buf: [64]u8 = undefined;
            const label_fit = truncateLabel(label, @as(usize, @intCast(bar.w - 2)), &label_buf);
            const style = self.theme.panel_header_active;
            self.canvas.fill(bar, ' ', style);
            self.canvas.writeClipped(bar.x + 1, bar.y, bar.w - 2, label_fit, style);
            return;
        }

        var x = bar.x;
        for (group.tabs.items, 0..) |tab, idx| {
            const is_active = idx == group.active;
            const style = if (is_active) self.theme.panel_header_active else self.theme.panel_header;
            const slot_w = if (idx + 1 == tab_count)
                bar.x + bar.w - x
            else
                slot;
            self.canvas.fill(Rect{ .x = x, .y = bar.y, .w = slot_w, .h = 1 }, ' ', style);
            const label = tabTitle(tab);
            var label_buf: [64]u8 = undefined;
            const label_fit = truncateLabel(label, @as(usize, @intCast(slot_w - 2)), &label_buf);
            const label_w = slot_w - 2;
            if (label_w > 0) {
                self.canvas.writeClipped(x + 1, bar.y, label_w, label_fit, style);
            }
            x += slot_w;
        }
    }

    fn drawView(self: *App, rect: Rect, group: Group) void {
        if (group.tabs.items.len == 0) return;
        const active = group.tabs.items[group.active];
        switch (active) {
            .source => self.drawSource(rect),
            .disasm => self.drawDisasm(rect),
            .stack => self.drawStack(rect),
            .tasks => self.drawTasks(rect),
            .threads => self.drawThreads(rect),
            .breakpoints => self.drawBreakpoints(rect),
            .watches => self.drawWatches(rect),
            .scopes => self.drawScopes(rect),
            .repl => self.drawRepl(rect),
            .output => self.drawOutput(rect),
            .events => self.drawEvents(rect),
            .memory => self.drawMemory(rect),
        }
    }

    fn drawTextBlock(self: *App, rect: Rect, lines: []const []const u8, style: Style) void {
        var y = rect.y;
        var idx: usize = 0;
        while (y < rect.y + rect.h and idx < lines.len) : (y += 1) {
            const line = lines[idx];
            self.canvas.writeClipped(rect.x + 1, y, rect.w - 2, line, style);
            idx += 1;
        }
    }

    fn drawSource(self: *App, rect: Rect) void {
        if (rect.w <= 2 or rect.h <= 0) return;
        const path = self.source_view.path orelse {
            self.canvas.writeClipped(rect.x + 1, rect.y, rect.w - 2, Icons.file ++ " no source", self.theme.panel_dim);
            return;
        };
        const file = self.source_cache.get(path) catch null orelse {
            self.canvas.writeClipped(rect.x + 1, rect.y, rect.w - 2, Icons.file ++ " source not found", self.theme.panel_dim);
            return;
        };
        const line_count = file.lines.len;
        if (line_count == 0) {
            self.canvas.writeClipped(rect.x + 1, rect.y, rect.w - 2, Icons.file ++ " empty source", self.theme.panel_dim);
            return;
        }
        if (self.source_view.scroll >= line_count) {
            self.source_view.scroll = line_count - 1;
        }
        const inner_x = rect.x + 1;
        const inner_w = rect.w - 2;
        const line_digits = countDigits(line_count);
        const num_w: i32 = @intCast(@max(@as(usize, 2), line_digits));
        const mark_w: i32 = 2;
        const gutter_w = mark_w + num_w + 1;
        const text_x = inner_x + gutter_w;
        const text_w = inner_w - gutter_w;
        if (text_w <= 0) return;

        var row: i32 = 0;
        while (row < rect.h) : (row += 1) {
            const line_index = self.source_view.scroll + @as(usize, @intCast(row));
            if (line_index >= line_count) break;
            const line_no: i64 = @intCast(line_index + 1);
            const line_meta = file.lines[line_index];
            const slice = file.text[line_meta.start .. line_meta.start + line_meta.len];
            const has_bp = self.hasBreakpoint(path, line_no);
            const is_current = self.source_view.line == line_index + 1;
            const mark_char: u21 = if (has_bp) '' else if (is_current) '' else ' ';
            const line_style = if (is_current)
                Style{ .fg = self.theme.bg0, .bg = self.theme.orange, .bold = true }
            else
                self.theme.panel_text;
            const gutter_style = if (has_bp) Style{ .fg = self.theme.red, .bg = self.theme.bg1, .bold = true } else self.theme.panel_dim;
            self.canvas.put(inner_x, rect.y + row, mark_char, gutter_style);
            var num_buf: [16]u8 = undefined;
            const num_text = std.fmt.bufPrint(&num_buf, "{d}", .{line_no}) catch "";
            const pad = num_w - @as(i32, @intCast(num_text.len));
            const pad_x = inner_x + 1;
            var p: i32 = 0;
            while (p < pad) : (p += 1) {
                self.canvas.put(pad_x + p, rect.y + row, ' ', gutter_style);
            }
            self.canvas.writeClipped(pad_x + pad, rect.y + row, num_w, num_text, gutter_style);
            self.canvas.put(inner_x + 1 + num_w, rect.y + row, ' ', gutter_style);
            self.canvas.writeClippedExpanded(text_x, rect.y + row, text_w, slice, line_style);
        }
    }

    fn drawDisasm(self: *App, rect: Rect) void {
        if (rect.w <= 2 or rect.h <= 0) return;
        if (self.disasm_lines.items.len == 0) {
            self.canvas.writeClipped(rect.x + 1, rect.y, rect.w - 2, Icons.code ++ " no disassembly", self.theme.panel_dim);
            return;
        }
        var current_ip: ?usize = null;
        if (self.debug) |debug| {
            if (debug.selected_frame < debug.frames.items.len) {
                current_ip = debug.frames.items[debug.selected_frame].ip;
            }
        }
        var row: i32 = 0;
        var idx: usize = self.disasm_scroll;
        while (row < rect.h and idx < self.disasm_lines.items.len) : ({
            row += 1;
            idx += 1;
        }) {
            const line = self.disasm_lines.items[idx];
            var buf: [512]u8 = undefined;
            var stream = std.io.fixedBufferStream(&buf);
            const writer = stream.writer();
            _ = writer.print("0x{x:0>8}  ", .{line.addr}) catch {};
            _ = writer.print("{s}", .{line.bytes}) catch {};
            if (line.bytes.len < 24) {
                const pad = 24 - line.bytes.len;
                _ = writer.writeByteNTimes(' ', pad) catch {};
            } else {
                _ = writer.writeAll(" ") catch {};
            }
            _ = writer.print(" {s}", .{line.text}) catch {};
            if (line.symbol) |sym| {
                _ = writer.print(" <{s}>", .{sym}) catch {};
            }
            if (line.source_path) |path| {
                if (line.line) |line_no| {
                    const base = std.fs.path.basename(path);
                    _ = writer.print(" @{s}:{d}", .{ base, line_no }) catch {};
                }
            }
            const line_text = stream.getWritten();
            const is_selected = self.disasm_cursor != null and idx == self.disasm_cursor.?;
            const style = if (is_selected)
                Style{ .fg = self.theme.bg0, .bg = self.theme.orange, .bold = true }
            else if (current_ip != null and line.addr == current_ip.?)
                Style{ .fg = self.theme.bg0, .bg = self.theme.blue, .bold = true }
            else
                self.theme.panel_text;
            self.canvas.writeClipped(rect.x + 1, rect.y + row, rect.w - 2, line_text, style);
        }
    }

    fn drawStack(self: *App, rect: Rect) void {
        if (rect.w <= 2 or rect.h <= 0) return;
        if (self.debug == null) {
            self.canvas.writeClipped(rect.x + 1, rect.y, rect.w - 2, Icons.status_warn ++ " no debug session", self.theme.panel_dim);
            return;
        }
        const debug = &self.debug.?;
        if (debug.frames.items.len == 0) {
            const label = if (debug.paused) Icons.stack ++ " no frames" else Icons.run ++ " running";
            self.canvas.writeClipped(rect.x + 1, rect.y, rect.w - 2, label, self.theme.panel_dim);
            return;
        }
        var y = rect.y;
        for (debug.frames.items, 0..) |frame, idx| {
            if (y >= rect.y + rect.h) break;
            var buf: [256]u8 = undefined;
            const file = if (frame.source_path) |path| std.fs.path.basename(path) else "unknown";
            const line = std.fmt.bufPrint(&buf, "{s} {s} ({s}:{d})", .{ Icons.stack, frame.name, file, frame.line }) catch "frame";
            const style = if (idx == debug.selected_frame)
                Style{ .fg = self.theme.bg0, .bg = self.theme.orange, .bold = true }
            else
                self.theme.panel_text;
            self.canvas.writeClipped(rect.x + 1, y, rect.w - 2, line, style);
            y += 1;
        }
    }

    fn drawTasks(self: *App, rect: Rect) void {
        if (rect.w <= 2 or rect.h <= 0) return;
        if (self.tasks.items.len == 0) {
            self.canvas.writeClipped(rect.x + 1, rect.y, rect.w - 2, Icons.tasks ++ " no tasks", self.theme.panel_dim);
            return;
        }
        var y = rect.y;
        for (self.tasks.items) |task| {
            if (y >= rect.y + rect.h) break;
            const depth = self.taskDepth(task);
            var buf: [256]u8 = undefined;
            var stream = std.io.fixedBufferStream(&buf);
            const writer = stream.writer();
            var i: usize = 0;
            while (i < depth) : (i += 1) {
                _ = writer.writeAll("│ ") catch {};
            }
            if (depth > 0) {
                _ = writer.writeAll("├─ ") catch {};
            }
            _ = writer.print("{s} {s} ({s})", .{ Icons.tasks, task.name, task.state }) catch {};
            if (task.pc) |pc| {
                _ = writer.print(" pc=0x{x}", .{@as(u64, @intCast(pc))}) catch {};
            }
            const line_text = stream.getWritten();
            const is_selected = self.selected_task != null and task.id == self.selected_task.?;
            const style = if (is_selected)
                Style{ .fg = self.theme.bg0, .bg = self.theme.orange, .bold = true }
            else if (std.mem.eql(u8, task.state, "running"))
                Style{ .fg = self.theme.green, .bg = self.theme.bg1, .bold = true }
            else if (std.mem.eql(u8, task.state, "waiting"))
                Style{ .fg = self.theme.yellow, .bg = self.theme.bg1, .bold = true }
            else if (std.mem.eql(u8, task.state, "cancelled"))
                Style{ .fg = self.theme.red, .bg = self.theme.bg1, .bold = true }
            else
                self.theme.panel_text;
            self.canvas.writeClipped(rect.x + 1, y, rect.w - 2, line_text, style);
            y += 1;
        }
    }

    fn drawThreads(self: *App, rect: Rect) void {
        if (rect.w <= 2 or rect.h <= 0) return;
        if (self.threads.items.len == 0) {
            self.canvas.writeClipped(rect.x + 1, rect.y, rect.w - 2, Icons.threads ++ " no threads", self.theme.panel_dim);
            return;
        }
        var y = rect.y;
        for (self.threads.items) |thread| {
            if (y >= rect.y + rect.h) break;
            var buf: [128]u8 = undefined;
            const line = std.fmt.bufPrint(&buf, "{s} {s} (id {d})", .{ Icons.threads, thread.name, thread.id }) catch "thread";
            const is_selected = self.selected_thread != null and thread.id == self.selected_thread.?;
            const style = if (is_selected)
                Style{ .fg = self.theme.bg0, .bg = self.theme.orange, .bold = true }
            else
                self.theme.panel_text;
            self.canvas.writeClipped(rect.x + 1, y, rect.w - 2, line, style);
            y += 1;
        }
    }

    fn drawBreakpoints(self: *App, rect: Rect) void {
        if (rect.w <= 2 or rect.h <= 0) return;
        if (self.breakpoints.items.len == 0) {
            self.canvas.writeClipped(rect.x + 1, rect.y, rect.w - 2, Icons.breakpoint ++ " no breakpoints", self.theme.panel_dim);
            return;
        }
        var y = rect.y;
        for (self.breakpoints.items) |bp| {
            if (y >= rect.y + rect.h) break;
            const mark: u21 = '';
            const style = if (bp.verified) self.theme.panel_text else self.theme.panel_dim;
            var buf: [256]u8 = undefined;
            const line = std.fmt.bufPrint(&buf, "{s}:{d}", .{ std.fs.path.basename(bp.path), bp.line }) catch "bp";
            self.canvas.put(rect.x + 1, y, mark, style);
            self.canvas.writeClipped(rect.x + 3, y, rect.w - 4, line, style);
            y += 1;
        }
    }

    fn drawWatches(self: *App, rect: Rect) void {
        if (rect.w <= 2 or rect.h <= 0) return;
        if (self.watches.items.len == 0) {
            self.canvas.writeClipped(rect.x + 1, rect.y, rect.w - 2, Icons.watch ++ " no watches", self.theme.panel_dim);
            return;
        }
        var y = rect.y;
        for (self.watches.items, 0..) |watch, idx| {
            if (y >= rect.y + rect.h) break;
            var buf: [256]u8 = undefined;
            const label = if (watch.err) |err|
                std.fmt.bufPrint(&buf, "{s} #{d} {s} ! {s}", .{ Icons.watch, idx + 1, watch.expr, err }) catch watch.expr
            else if (watch.value) |val|
                std.fmt.bufPrint(&buf, "{s} #{d} {s} = {s}", .{ Icons.watch, idx + 1, watch.expr, val }) catch watch.expr
            else
                std.fmt.bufPrint(&buf, "{s} #{d} {s}", .{ Icons.watch, idx + 1, watch.expr }) catch watch.expr;
            const style = if (watch.err != null)
                Style{ .fg = self.theme.red, .bg = self.theme.bg1, .bold = true }
            else
                self.theme.panel_text;
            self.canvas.writeClipped(rect.x + 1, y, rect.w - 2, label, style);
            y += 1;
        }
    }

    fn drawScopes(self: *App, rect: Rect) void {
        if (rect.w <= 2 or rect.h <= 0) return;
        var y = rect.y;
        self.canvas.writeClipped(rect.x + 1, y, rect.w - 2, Icons.scope ++ " Locals", self.theme.panel_dim);
        y += 1;
        if (self.scopes.locals.items.len == 0) {
            if (y < rect.y + rect.h) {
                self.canvas.writeClipped(rect.x + 1, y, rect.w - 2, "  " ++ Icons.scope ++ " empty", self.theme.panel_dim);
                y += 1;
            }
        } else {
            for (self.scopes.locals.items) |entry| {
                if (y >= rect.y + rect.h) break;
                var buf: [256]u8 = undefined;
                const line = if (entry.type_name) |ty|
                    std.fmt.bufPrint(&buf, "  {s} {s}: {s} = {s}", .{ Icons.scope, entry.name, ty, entry.value }) catch entry.name
                else
                    std.fmt.bufPrint(&buf, "  {s} {s} = {s}", .{ Icons.scope, entry.name, entry.value }) catch entry.name;
                self.canvas.writeClipped(rect.x + 1, y, rect.w - 2, line, self.theme.panel_text);
                y += 1;
            }
        }
        if (y < rect.y + rect.h) {
            self.canvas.writeClipped(rect.x + 1, y, rect.w - 2, Icons.scope ++ " Registers", self.theme.panel_dim);
            y += 1;
        }
        if (self.scopes.registers.items.len == 0) {
            if (y < rect.y + rect.h) {
                self.canvas.writeClipped(rect.x + 1, y, rect.w - 2, "  " ++ Icons.scope ++ " empty", self.theme.panel_dim);
                y += 1;
            }
        } else {
            for (self.scopes.registers.items) |entry| {
                if (y >= rect.y + rect.h) break;
                var buf: [256]u8 = undefined;
                const line = if (entry.type_name) |ty|
                    std.fmt.bufPrint(&buf, "  {s} {s}: {s} = {s}", .{ Icons.scope, entry.name, ty, entry.value }) catch entry.name
                else
                    std.fmt.bufPrint(&buf, "  {s} {s} = {s}", .{ Icons.scope, entry.name, entry.value }) catch entry.name;
                self.canvas.writeClipped(rect.x + 1, y, rect.w - 2, line, self.theme.panel_text);
                y += 1;
            }
        }
    }

    fn drawRepl(self: *App, rect: Rect) void {
        if (rect.w <= 2 or rect.h <= 0) return;
        const prompt = replPrompt(self.repl_context);
        const prompt_cols_u = textWidth(prompt);
        const prompt_cols: i32 = @intCast(prompt_cols_u);
        const inner_x = rect.x + 1;
        const inner_w = rect.w - 2;
        const prompt_y = rect.y + rect.h - 1;

        if (rect.h > 1) {
            const log_h = rect.h - 1;
            const visible = @as(usize, @intCast(log_h));
            const total = self.log.lines.items.len;
            const start = self.log.startIndex(visible);
            var y = rect.y;
            var idx = start;
            while (y < rect.y + log_h and idx < total) : (y += 1) {
                const line = self.log.lines.items[idx];
                self.canvas.writeClipped(inner_x, y, inner_w, line, self.theme.panel_text);
                idx += 1;
            }
        }

        self.canvas.writeClipped(inner_x, prompt_y, inner_w, prompt, self.theme.panel_text);
        const text_len = self.repl_input.items.len;
        var start: usize = 0;
        if (inner_w > prompt_cols) {
            const available = @as(usize, @intCast(inner_w - prompt_cols));
            if (text_len > available) start = text_len - available;
        }
        const end = text_len;
        const visible = self.repl_input.items[start..end];
        self.canvas.writeClipped(inner_x + prompt_cols, prompt_y, inner_w - prompt_cols, visible, self.theme.panel_text);

        if (self.mode == .repl) {
            const cursor = self.repl_cursor;
            const inner_w_u = @as(usize, @intCast(inner_w));
            if (inner_w_u > prompt_cols_u) {
                const available = inner_w_u - prompt_cols_u;
                const base = if (text_len > available) text_len - available else 0;
                if (cursor >= base and cursor <= base + available) {
                    const cursor_col = inner_x + prompt_cols + @as(i32, @intCast(cursor - base));
                    self.canvas.put(cursor_col, prompt_y, '▏', self.theme.accent_alt);
                }
            }
        }
    }

    fn drawOutput(self: *App, rect: Rect) void {
        var y = rect.y;
        self.canvas.writeClipped(rect.x + 1, y, rect.w - 2, Icons.output ++ " output", self.theme.panel_dim);
        y += 1;
        if (rect.h <= 1) return;
        const available = @as(usize, @intCast(rect.h - 1));
        const total = self.output.lines.items.len;
        const start = self.output.startIndex(available);
        var idx: usize = start;
        while (y < rect.y + rect.h and idx < total) : (y += 1) {
            const line = self.output.lines.items[idx];
            self.canvas.writeClipped(rect.x + 1, y, rect.w - 2, line, self.theme.panel_text);
            idx += 1;
        }
    }

    fn drawEvents(self: *App, rect: Rect) void {
        if (rect.w <= 2 or rect.h <= 0) return;
        if (self.events.lines.items.len == 0) {
            self.canvas.writeClipped(rect.x + 1, rect.y, rect.w - 2, Icons.events ++ " no events", self.theme.panel_dim);
            return;
        }
        const total = self.events.lines.items.len;
        const available = @as(usize, @intCast(rect.h));
        const start = self.events.startIndex(available);
        var y = rect.y;
        var idx: usize = start;
        while (y < rect.y + rect.h and idx < total) : (y += 1) {
            const line = self.events.lines.items[idx];
            self.canvas.writeClipped(rect.x + 1, y, rect.w - 2, line, self.theme.panel_text);
            idx += 1;
        }
    }

    fn drawMemory(self: *App, rect: Rect) void {
        if (rect.w <= 2 or rect.h <= 0) return;
        const view = self.memory_view orelse {
            self.canvas.writeClipped(rect.x + 1, rect.y, rect.w - 2, Icons.memory ++ " no memory", self.theme.panel_dim);
            return;
        };
        const bytes = view.bytes;
        var row: i32 = 0;
        while (row < rect.h) : (row += 1) {
            const offset = @as(usize, @intCast(row)) * memory_bytes_per_line;
            if (offset >= bytes.len) break;
            var buf: [512]u8 = undefined;
            var stream = std.io.fixedBufferStream(&buf);
            const writer = stream.writer();
            _ = writer.print("0x{x:0>8}: ", .{view.base + offset}) catch {};
            var i: usize = 0;
            while (i < memory_bytes_per_line) : (i += 1) {
                if (offset + i < bytes.len) {
                    const b = bytes[offset + i];
                    _ = writer.writeByte(hexDigit(b >> 4)) catch {};
                    _ = writer.writeByte(hexDigit(b & 0xf)) catch {};
                } else {
                    _ = writer.writeAll("  ") catch {};
                }
                if (i + 1 < memory_bytes_per_line) {
                    _ = writer.writeByte(' ') catch {};
                }
            }
            _ = writer.writeAll(" |") catch {};
            i = 0;
            while (i < memory_bytes_per_line) : (i += 1) {
                if (offset + i < bytes.len) {
                    const b = bytes[offset + i];
                    const ch: u8 = if (b >= 0x20 and b <= 0x7e) b else '.';
                    _ = writer.writeByte(ch) catch {};
                } else {
                    _ = writer.writeByte(' ') catch {};
                }
            }
            _ = writer.writeAll("|") catch {};
            const line = stream.getWritten();
            self.canvas.writeClipped(rect.x + 1, rect.y + row, rect.w - 2, line, self.theme.panel_text);
        }
    }

    fn drawDropHint(self: *App) void {
        if (self.drag != .tab) return;
        const drag = self.drag.tab;
        if (!drag.moved) return;
        if (drag.target_group == null or drag.target_zone == null) return;
        for (self.layout.groups.items) |group| {
            if (group.id != drag.target_group.?) continue;
            const zone = drag.target_zone.?;
            var rect = group.rect;
            const inset_x = @max(2, @divTrunc(rect.w, 6));
            const inset_y = @max(1, @divTrunc(rect.h, 6));
            switch (zone) {
                .left => rect = Rect{ .x = rect.x, .y = rect.y, .w = inset_x, .h = rect.h },
                .right => rect = Rect{ .x = rect.x + rect.w - inset_x, .y = rect.y, .w = inset_x, .h = rect.h },
                .top => rect = Rect{ .x = rect.x, .y = rect.y, .w = rect.w, .h = inset_y },
                .bottom => rect = Rect{ .x = rect.x, .y = rect.y + rect.h - inset_y, .w = rect.w, .h = inset_y },
                .center => rect = Rect{ .x = rect.x + inset_x, .y = rect.y + inset_y, .w = rect.w - inset_x * 2, .h = rect.h - inset_y * 2 },
            }
            self.canvas.fill(rect, ' ', self.theme.accent_alt);
            const label = Icons.mouse ++ " drop";
            self.canvas.writeClipped(rect.x + 1, rect.y + @divTrunc(rect.h, 2), rect.w - 2, label, self.theme.accent_alt);
        }
    }

    fn drawStatusBar(self: *App) void {
        const y = @as(i32, @intCast(self.canvas.height - 1));
        const w = @as(i32, @intCast(self.canvas.width));
        const os_icon = if (builtin.os.tag == .macos) "" else if (builtin.os.tag == .linux) "" else "";
        var left_buf: [64]u8 = undefined;
        const left_text = std.fmt.bufPrint(&left_buf, " {s} {s} inkx ", .{ os_icon, Icons.repl }) catch " inkx ";
        const mode_label = switch (self.mode) {
            .normal => " NORMAL ",
            .command => " CMD ",
            .repl => " INSERT ",
        };
        const mode_style: Style = switch (self.mode) {
            .normal => self.theme.status_left,
            .command => self.theme.accent,
            .repl => Style{ .fg = self.theme.bg0, .bg = self.theme.green, .bold = true },
        };
        var mid_buf: [128]u8 = undefined;
        const active_tab = tabTitle(self.activeTabKind());
        const state_icon = if (self.debug == null)
            Icons.status_warn
        else if (self.debug.?.paused)
            Icons.pause
        else
            Icons.run;
        const mid_text = std.fmt.bufPrint(&mid_buf, " {s} {s} · {s}", .{ state_icon, active_tab, self.status_line }) catch self.status_line;
        const right_text = if (self.debug == null)
            " " ++ Icons.status_warn ++ " no target "
        else
            " " ++ Icons.status_ok ++ " debug ready ";
        self.canvas.fill(Rect{ .x = 0, .y = y, .w = w, .h = 1 }, ' ', self.theme.status_mid);
        self.canvas.write(0, y, left_text, self.theme.status_left);
        const left_cols = @as(i32, @intCast(textWidth(left_text)));
        const mode_cols = @as(i32, @intCast(textWidth(mode_label)));
        self.canvas.write(left_cols, y, mode_label, mode_style);
        const mid_x = left_cols + mode_cols;
        self.canvas.write(mid_x, y, mid_text, self.theme.status_mid);
        const right_cols = @as(i32, @intCast(textWidth(right_text)));
        const right_x = w - right_cols;
        if (right_x > mid_x + 1) self.canvas.write(right_x, y, right_text, self.theme.status_right);
    }

    fn drawCommandPalette(self: *App) void {
        const y = @as(i32, @intCast(self.canvas.height - 2));
        const rect = Rect{ .x = 0, .y = y, .w = @intCast(self.canvas.width), .h = 1 };
        self.canvas.fill(rect, ' ', self.theme.accent);
        const prompt = commandPrompt();
        const prompt_cols_u = textWidth(prompt);
        const prompt_cols: i32 = @intCast(prompt_cols_u);
        const inner_x = rect.x + 1;
        const inner_w = rect.w - 2;
        self.canvas.writeClipped(inner_x, y, inner_w, prompt, self.theme.accent);
        const text = self.command.text();
        const text_len = text.len;
        var start: usize = 0;
        if (inner_w > prompt_cols) {
            const available = @as(usize, @intCast(inner_w - prompt_cols));
            if (text_len > available) start = text_len - available;
        }
        const visible = text[start..text_len];
        self.canvas.writeClipped(inner_x + prompt_cols, y, inner_w - prompt_cols, visible, self.theme.accent);
        if (self.mode == .command) {
            const cursor = self.command.cursor;
            const inner_w_u = @as(usize, @intCast(inner_w));
            if (inner_w_u > prompt_cols_u) {
                const available = inner_w_u - prompt_cols_u;
                const base = if (text_len > available) text_len - available else 0;
                if (cursor >= base and cursor <= base + available) {
                    const cursor_col = inner_x + prompt_cols + @as(i32, @intCast(cursor - base));
                    self.canvas.put(cursor_col, y, '▏', self.theme.accent_alt);
                }
            }
        }
    }
};

fn resolveInkvmPath(allocator: mem_allocator) ![]const u8 {
    if (std.process.getEnvVarOwned(allocator, "INKVM_PATH")) |env| return env else |_| {}
    const exe_path = std.fs.selfExePathAlloc(allocator) catch {
        return allocator.dupe(u8, "inkvm");
    };
    defer allocator.free(exe_path);
    if (std.fs.path.dirname(exe_path)) |dir| {
        const candidate = std.fs.path.join(allocator, &.{ dir, "inkvm" }) catch {
            return allocator.dupe(u8, "inkvm");
        };
        if (std.fs.accessAbsolute(candidate, .{})) |_| {
            return candidate;
        } else |_| {
            allocator.free(candidate);
        }
    }
    return allocator.dupe(u8, "inkvm");
}

fn tabTitle(kind: TabKind) []const u8 {
    return switch (kind) {
        .source => Icons.file ++ " source",
        .disasm => Icons.code ++ " disasm",
        .stack => Icons.stack ++ " stack",
        .tasks => Icons.tasks ++ " tasks",
        .threads => Icons.threads ++ " threads",
        .breakpoints => Icons.breakpoint ++ " breakpoints",
        .watches => Icons.watch ++ " watches",
        .scopes => Icons.scope ++ " scopes",
        .repl => Icons.repl ++ " repl",
        .output => Icons.output ++ " output",
        .events => Icons.events ++ " events",
        .memory => Icons.memory ++ " memory",
    };
}

fn tabName(kind: TabKind) []const u8 {
    return switch (kind) {
        .source => "source",
        .disasm => "disasm",
        .stack => "stack",
        .tasks => "tasks",
        .threads => "threads",
        .breakpoints => "breakpoints",
        .watches => "watches",
        .scopes => "scopes",
        .repl => "repl",
        .output => "output",
        .events => "events",
        .memory => "memory",
    };
}

fn commandPrompt() []const u8 {
    return Icons.command ++ " :";
}

fn replPrompt(context: ReplContext) []const u8 {
    return switch (context) {
        .frame => Icons.repl ++ " frame> ",
        .repl => Icons.repl ++ " repl> ",
    };
}

fn textWidth(text: []const u8) usize {
    var idx: usize = 0;
    var total: usize = 0;
    while (nextCodepointLossy(text, &idx)) |cp| {
        const width = cellWidth(cp);
        if (width > 0) total += @intCast(width);
    }
    return total;
}

fn nextCodepointLossy(text: []const u8, idx: *usize) ?u21 {
    if (idx.* >= text.len) return null;
    const first = text[idx.*];
    const seq_len = std.unicode.utf8ByteSequenceLength(first) catch {
        idx.* += 1;
        return 0xfffd;
    };
    const len: usize = @intCast(seq_len);
    if (len == 1) {
        idx.* += 1;
        return @as(u21, first);
    }
    if (idx.* + len > text.len) {
        idx.* += 1;
        return 0xfffd;
    }
    const slice = text[idx.* .. idx.* + len];
    const cp = std.unicode.utf8Decode(slice) catch {
        idx.* += 1;
        return 0xfffd;
    };
    idx.* += len;
    return cp;
}

fn cellWidth(cp: u21) i32 {
    if (cp == 0) return 0;
    if (cp < 0x20 or (cp >= 0x7f and cp < 0xa0)) return 0;
    if (isCombining(cp)) return 0;
    if (isWide(cp)) return 2;
    return 1;
}

fn isCombining(cp: u21) bool {
    return switch (cp) {
        0x0300...0x036F,
        0x1AB0...0x1AFF,
        0x1DC0...0x1DFF,
        0x20D0...0x20FF,
        0xFE20...0xFE2F,
        0xFE00...0xFE0F,
        0xE0100...0xE01EF,
        => true,
        else => false,
    };
}

fn isWide(cp: u21) bool {
    return switch (cp) {
        0x1100...0x115F,
        0x231A...0x231B,
        0x2329...0x232A,
        0x23E9...0x23EC,
        0x23F0,
        0x23F3,
        0x25FD...0x25FE,
        0x2614...0x2615,
        0x2648...0x2653,
        0x267F,
        0x2693,
        0x26A1,
        0x26AA...0x26AB,
        0x26BD...0x26BE,
        0x26C4...0x26C5,
        0x26CE,
        0x26D4,
        0x26EA,
        0x26F2...0x26F3,
        0x26F5,
        0x26FA,
        0x26FD,
        0x2705,
        0x270A...0x270B,
        0x2728,
        0x274C,
        0x274E,
        0x2753...0x2755,
        0x2757,
        0x2795...0x2797,
        0x27B0,
        0x27BF,
        0x2B1B...0x2B1C,
        0x2B50,
        0x2B55,
        0x2E80...0x2E99,
        0x2E9B...0x2EF3,
        0x2F00...0x2FD5,
        0x2FF0...0x2FFB,
        0x3000...0x303E,
        0x3041...0x3096,
        0x3099...0x30FF,
        0x3105...0x312F,
        0x3131...0x318E,
        0x3190...0x31E3,
        0x31F0...0x321E,
        0x3220...0x3247,
        0x3250...0x4DBF,
        0x4E00...0xA48C,
        0xA490...0xA4C6,
        0xA960...0xA97C,
        0xAC00...0xD7A3,
        0xF900...0xFAFF,
        0xFE10...0xFE19,
        0xFE30...0xFE6B,
        0xFF01...0xFF60,
        0xFFE0...0xFFE6,
        0x1F004,
        0x1F0CF,
        0x1F18E,
        0x1F191...0x1F19A,
        0x1F1E6...0x1F1FF,
        0x1F200...0x1F202,
        0x1F210...0x1F23B,
        0x1F240...0x1F248,
        0x1F250...0x1F251,
        0x1F300...0x1F64F,
        0x1F680...0x1F6FF,
        0x1F900...0x1F9FF,
        0x1FA70...0x1FAFF,
        0x20000...0x2FFFD,
        0x30000...0x3FFFD,
        => true,
        else => false,
    };
}

const ReplTokenInfo = struct {
    value_like: bool,
    statement_start: bool,
};

const ReplToken = struct {
    start: usize,
    end: usize,
    info: ReplTokenInfo,
};

fn normalizeReplExpr(allocator: mem_allocator, input: []const u8) !?[]u8 {
    var tmp_buf: ?[]u8 = null;
    var tmp = input;
    if (std.mem.indexOfScalar(u8, input, ';') != null) {
        tmp_buf = try splitReplSemicolons(allocator, input);
        tmp = tmp_buf.?;
    }
    if (try autoSplitReplInput(allocator, tmp)) |split| {
        if (tmp_buf) |buf| allocator.free(buf);
        return split;
    }
    return tmp_buf;
}

fn splitReplSemicolons(allocator: mem_allocator, input: []const u8) ![]u8 {
    var list = std.ArrayListUnmanaged(u8){};
    defer list.deinit(allocator);
    var iter = std.mem.splitScalar(u8, input, ';');
    while (iter.next()) |part| {
        const line = std.mem.trim(u8, part, " \t");
        if (line.len == 0) continue;
        try list.appendSlice(allocator, line);
        try list.append(allocator, '\n');
    }
    return list.toOwnedSlice(allocator);
}

fn autoSplitReplInput(allocator: mem_allocator, input: []const u8) !?[]u8 {
    var out = std.ArrayListUnmanaged(u8){};
    errdefer out.deinit(allocator);
    var changed = false;
    var i: usize = 0;
    var prev: ?ReplTokenInfo = null;
    var prev_end: usize = 0;
    while (i < input.len) {
        if (isReplWhitespace(input[i])) {
            i += 1;
            continue;
        }
        const token = parseReplToken(input, i);
        if (prev) |prev_info| {
            const ws = input[prev_end..token.start];
            if (ws.len > 0) {
                const has_newline = std.mem.indexOfScalar(u8, ws, '\n') != null;
                if (has_newline) {
                    try out.appendSlice(allocator, ws);
                } else if (shouldSplitRepl(prev_info, token.info)) {
                    try out.append(allocator, '\n');
                    changed = true;
                } else {
                    try out.appendSlice(allocator, ws);
                }
            }
        } else if (token.start > 0) {
            try out.appendSlice(allocator, input[0..token.start]);
        }
        try out.appendSlice(allocator, input[token.start..token.end]);
        prev = token.info;
        prev_end = token.end;
        i = token.end;
    }
    if (prev_end < input.len) {
        try out.appendSlice(allocator, input[prev_end..]);
    }
    if (!changed) {
        out.deinit(allocator);
        return null;
    }
    return @as(?[]u8, try out.toOwnedSlice(allocator));
}

fn shouldSplitRepl(prev: ReplTokenInfo, curr: ReplTokenInfo) bool {
    if (!prev.value_like) return false;
    if (curr.value_like) return true;
    if (curr.statement_start) return true;
    return false;
}

fn parseReplToken(input: []const u8, start: usize) ReplToken {
    var i = start;
    const c = input[i];
    if (isIdentStart(c)) {
        i += 1;
        while (i < input.len and isIdentContinue(input[i])) : (i += 1) {}
        const text = input[start..i];
        return .{ .start = start, .end = i, .info = replInfoForIdent(text) };
    }
    if (isDigit(c)) {
        i += 1;
        while (i < input.len and (isDigit(input[i]) or input[i] == '_' or input[i] == '.')) : (i += 1) {}
        return .{ .start = start, .end = i, .info = .{ .value_like = true, .statement_start = false } };
    }
    if (c == '"') {
        i += 1;
        while (i < input.len) : (i += 1) {
            if (input[i] == '\\') {
                if (i + 1 < input.len) i += 1;
                continue;
            }
            if (input[i] == '"') {
                i += 1;
                break;
            }
        }
        return .{ .start = start, .end = i, .info = .{ .value_like = true, .statement_start = false } };
    }
    if (c == '\'') {
        i += 1;
        while (i < input.len) : (i += 1) {
            if (input[i] == '\\') {
                if (i + 1 < input.len) i += 1;
                continue;
            }
            if (input[i] == '\'') {
                i += 1;
                break;
            }
        }
        return .{ .start = start, .end = i, .info = .{ .value_like = true, .statement_start = false } };
    }
    if (i + 1 < input.len) {
        const pair = input[start .. start + 2];
        if (std.mem.eql(u8, pair, "==") or std.mem.eql(u8, pair, "!=") or std.mem.eql(u8, pair, "<=") or
            std.mem.eql(u8, pair, ">=") or std.mem.eql(u8, pair, "+=") or std.mem.eql(u8, pair, "-=") or
            std.mem.eql(u8, pair, "*=") or std.mem.eql(u8, pair, "/=") or std.mem.eql(u8, pair, "&&") or
            std.mem.eql(u8, pair, "||") or std.mem.eql(u8, pair, "->") or std.mem.eql(u8, pair, "=>") or
            std.mem.eql(u8, pair, "::"))
        {
            i += 2;
            return .{ .start = start, .end = i, .info = replInfoForSymbol(pair) };
        }
    }
    i += 1;
    return .{ .start = start, .end = i, .info = replInfoForSymbol(input[start..i]) };
}

fn replInfoForIdent(text: []const u8) ReplTokenInfo {
    if (std.mem.eql(u8, text, "true") or std.mem.eql(u8, text, "false") or std.mem.eql(u8, text, "null")) {
        return .{ .value_like = true, .statement_start = false };
    }
    if (std.mem.eql(u8, text, "let") or std.mem.eql(u8, text, "if") or std.mem.eql(u8, text, "else") or
        std.mem.eql(u8, text, "for") or std.mem.eql(u8, text, "while") or std.mem.eql(u8, text, "match") or
        std.mem.eql(u8, text, "return") or std.mem.eql(u8, text, "break") or std.mem.eql(u8, text, "continue") or
        std.mem.eql(u8, text, "defer") or std.mem.eql(u8, text, "spawn") or std.mem.eql(u8, text, "await") or
        std.mem.eql(u8, text, "cancel") or std.mem.eql(u8, text, "fn"))
    {
        return .{ .value_like = false, .statement_start = true };
    }
    return .{ .value_like = true, .statement_start = false };
}

fn replInfoForSymbol(sym: []const u8) ReplTokenInfo {
    if (sym.len == 1) {
        const ch = sym[0];
        if (ch == ')' or ch == ']' or ch == '}') {
            return .{ .value_like = true, .statement_start = false };
        }
    }
    return .{ .value_like = false, .statement_start = false };
}

fn isReplWhitespace(ch: u8) bool {
    return ch == ' ' or ch == '\t' or ch == '\n' or ch == '\r';
}

fn isIdentStart(ch: u8) bool {
    return (ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z') or ch == '_';
}

fn isIdentContinue(ch: u8) bool {
    return isIdentStart(ch) or isDigit(ch);
}

fn isDigit(ch: u8) bool {
    return ch >= '0' and ch <= '9';
}

fn truncateLabel(label: []const u8, max_len: usize, buf: []u8) []const u8 {
    if (textWidth(label) <= max_len) return label;
    if (max_len == 0) return label[0..0];
    if (max_len == 1) return ".";
    if (max_len == 2) return "..";
    const ellipsis = "...";
    const ellipsis_width = textWidth(ellipsis);
    if (max_len <= ellipsis_width) return ellipsis[0..max_len];
    const max_cells = max_len - ellipsis_width;
    var view = std.unicode.Utf8View.init(label) catch {
        const take = @min(label.len, max_cells);
        if (buf.len < take + ellipsis.len) return label[0..@min(label.len, max_len)];
        std.mem.copyForwards(u8, buf[0..take], label[0..take]);
        std.mem.copyForwards(u8, buf[take .. take + ellipsis.len], ellipsis);
        return buf[0 .. take + ellipsis.len];
    };
    var iter = view.iterator();
    var cells: usize = 0;
    var bytes: usize = 0;
    while (iter.nextCodepointSlice()) |slice| {
        const cp = std.unicode.utf8Decode(slice) catch 0;
        const width = cellWidth(cp);
        if (width <= 0) continue;
        if (cells + @as(usize, @intCast(width)) > max_cells) break;
        bytes += slice.len;
        cells += @intCast(width);
    }
    if (buf.len < bytes + ellipsis.len) {
        return label[0..@min(label.len, max_len)];
    }
    std.mem.copyForwards(u8, buf[0..bytes], label[0..bytes]);
    std.mem.copyForwards(u8, buf[bytes .. bytes + ellipsis.len], ellipsis);
    return buf[0 .. bytes + ellipsis.len];
}

fn countDigits(value: usize) usize {
    var v = value;
    var count: usize = 1;
    while (v >= 10) : (v /= 10) {
        count += 1;
    }
    return count;
}

fn hexDigit(nibble: u8) u8 {
    return if (nibble < 10) @as(u8, '0') + nibble else @as(u8, 'a') + (nibble - 10);
}

fn parse_address(text: []const u8) ?usize {
    if (text.len == 0) return null;
    if (std.mem.startsWith(u8, text, "0x") or std.mem.startsWith(u8, text, "0X")) {
        return std.fmt.parseInt(usize, text[2..], 16) catch null;
    }
    return std.fmt.parseInt(usize, text, 10) catch null;
}

fn tryFormat(allocator: mem_allocator, comptime fmt: []const u8, args: anytype) ![]u8 {
    return std.fmt.allocPrint(allocator, fmt, args);
}
