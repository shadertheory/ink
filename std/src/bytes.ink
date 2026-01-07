#[foreign] fn bytes_new(capacity: int) -> []u8
#[foreign] fn bytes_from_string(value: string) -> []u8
#[foreign] fn bytes_free(value: []u8)
#[foreign] fn bytes_len(value: []u8) -> int
#[foreign] fn bytes_cap(value: []u8) -> int
#[foreign] fn bytes_ptr(value: []u8) -> int
#[foreign] fn bytes_set_len(value: []u8, len: int)
