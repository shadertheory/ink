#[foreign] fn alloc(size: int) -> int
#[foreign] fn free(ptr: int)
#[foreign] fn deref(ptr: int) -> int
#[foreign] fn store(ptr: int, value: int)
#[foreign] fn borrow(value: int) -> int
#[foreign] fn borrow_mut(value: int) -> int
#[foreign] fn ptr_of(reg: int) -> int
