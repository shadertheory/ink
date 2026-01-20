#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn buf_new(capacity: i64) -> buf
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn buf_free(value: buf)
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn buf_len(value: buf) -> i64
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn buf_capacity(value: buf) -> i64
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn buf_read_ptr(value: buf) -> i64
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn buf_write_ptr(value: buf) -> i64
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn buf_read_advance(value: buf, count: i64)
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn buf_write_advance(value: buf, count: i64)
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn buf_write_bytes(value: buf, data: bytes) -> i64
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn buf_read_bytes(value: buf, out: bytes) -> i64
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn buf_reset(value: buf)
