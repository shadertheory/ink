#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn arena_new(size: i64) -> arena
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn arena_alloc(value: arena, size: i64) -> i64
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn arena_reset(value: arena)
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn arena_deinit(value: arena)
