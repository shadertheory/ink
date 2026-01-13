#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn buf_new(capacity: int) -> buf
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn buf_free(value: buf)
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn buf_len(value: buf) -> int
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn buf_capacity(value: buf) -> int
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn buf_read_ptr(value: buf) -> int
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn buf_write_ptr(value: buf) -> int
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn buf_read_advance(value: buf, count: int)
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn buf_write_advance(value: buf, count: int)
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn buf_write_bytes(value: buf, data: bytes) -> int
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn buf_read_bytes(value: buf, out: bytes) -> int
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn buf_reset(value: buf)
