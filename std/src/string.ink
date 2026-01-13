#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn string_new(capacity: int) -> string
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn string_free(value: string)
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn string_len(value: string) -> int
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn string_cap(value: string) -> int
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn string_ptr(value: string) -> int
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn string_set_len(value: string, len: int)
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn string_from_int(value: int) -> string
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn string_from_float(value: float) -> string
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn string_from_bool(value: bool) -> string
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn string_concat(left: string, right: string) -> string
