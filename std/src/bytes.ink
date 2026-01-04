#[foreign] fn bytes_new(capacity: int) -> bytes
#[foreign] fn bytes_from_string(value: string) -> bytes
#[foreign] fn bytes_free(value: bytes)
#[foreign] fn bytes_len(value: bytes) -> int
#[foreign] fn bytes_cap(value: bytes) -> int
#[foreign] fn bytes_ptr(value: bytes) -> int
#[foreign] fn bytes_set_len(value: bytes, len: int)
