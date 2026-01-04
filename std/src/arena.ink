#[foreign] fn arena_new(size: int) -> arena
#[foreign] fn arena_alloc(value: arena, size: int) -> int
#[foreign] fn arena_reset(value: arena)
#[foreign] fn arena_deinit(value: arena)
