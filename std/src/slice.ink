#[foreign] fn slice_new<T: type>(ptr: int, len: int) -> []T
#[foreign] fn slice_free<T: type>(value: []T)
#[foreign] fn slice_ptr<T: type>(value: []T) -> int
#[foreign] fn slice_len<T: type>(value: []T) -> int
