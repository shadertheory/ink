pub const op_kind = enum(u8) {
    read,
    write,
    accept,
    timer,
};

pub const completion = struct {
    id: u32,
    kind: op_kind,
    result: isize,
    err: ?anyerror,
    user_data: u64,
};
