#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn slice_new<T: type>(ptr: int, len: int) -> []T
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn slice_free<T: type>(value: []T)
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn slice_ptr<T: type>(value: []T) -> int
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn slice_len<T: type>(value: []T) -> int
