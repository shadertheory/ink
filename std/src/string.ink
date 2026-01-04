#[foreign] fn string_new(capacity: int) -> string
#[foreign] fn string_free(value: string)
#[foreign] fn string_len(value: string) -> int
#[foreign] fn string_cap(value: string) -> int
#[foreign] fn string_ptr(value: string) -> int
#[foreign] fn string_set_len(value: string, len: int)
#[foreign] fn string_from_int(value: int) -> string
#[foreign] fn string_from_float(value: float) -> string
#[foreign] fn string_from_bool(value: bool) -> string
#[foreign] fn string_concat(left: string, right: string) -> string
