#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn slice_new<T: type>(ptr: i64, len: i64) -> []T
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn slice_free<T: type>(value: []T)
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn slice_ptr<T: type>(value: []T) -> i64
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn slice_len<T: type>(value: []T) -> i64
