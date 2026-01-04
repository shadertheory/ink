#[foreign] fn slice_new(ptr: int, len: int) -> slice
#[foreign] fn slice_free(value: slice)
#[foreign] fn slice_ptr(value: slice) -> int
#[foreign] fn slice_len(value: slice) -> int
