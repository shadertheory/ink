#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn arena_new(size: int) -> arena
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn arena_alloc(value: arena, size: int) -> int
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn arena_reset(value: arena)
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn arena_deinit(value: arena)
