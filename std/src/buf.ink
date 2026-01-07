#[foreign] fn buf_new(capacity: int) -> buf
#[foreign] fn buf_free(value: buf)
#[foreign] fn buf_len(value: buf) -> int
#[foreign] fn buf_capacity(value: buf) -> int
#[foreign] fn buf_read_ptr(value: buf) -> int
#[foreign] fn buf_write_ptr(value: buf) -> int
#[foreign] fn buf_read_advance(value: buf, count: int)
#[foreign] fn buf_write_advance(value: buf, count: int)
#[foreign] fn buf_write_bytes(value: buf, data: []u8) -> int
#[foreign] fn buf_read_bytes(value: buf, out: []u8) -> int
#[foreign] fn buf_reset(value: buf)
