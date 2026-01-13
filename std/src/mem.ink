#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn alloc(size: int) -> int
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn free(ptr: int)
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn deref(ptr: int) -> int
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn store(ptr: int, value: int)
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn ptr_of(reg: int) -> int
