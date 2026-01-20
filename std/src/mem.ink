#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn alloc(size: i64) -> i64
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn free(ptr: i64)
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn deref(ptr: i64) -> i64
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn store(ptr: i64, value: i64)
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn ptr_of(reg: i64) -> i64
