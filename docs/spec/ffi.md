# Ink Foreign ABI (Custom)

## Syntax
- Declaration: `foreign fn foo(a: T, b: U) -> R`
- Call: `foreign foo(a, b)`

The call form requires a matching `foreign fn` signature in the module.

## Mangling
- Symbol name: `ink_foreign_<name>`
- `::` in the name is replaced with `_`

Example: `foreign foo()` resolves `ink_foreign_foo`.

## ABI
Dynamic foreign functions use this C ABI:

```c
typedef struct InkRuntime {
    uint64_t *memory;
    size_t memory_len;
    size_t fp;
    size_t sp;
    size_t arg_base;
    bool arg_base_valid;
} InkRuntime;

void ink_foreign_foo(InkRuntime *rt);
```

Arguments are read from:
- `rt->memory[rt->arg_base + 2 + n]` when `arg_base_valid`
- otherwise `rt->memory[rt->fp + n]`

Return value is written to:
- `rt->memory[rt->fp + 0]`

All values are 64-bit slots. Scalars/pointers are passed as raw bits.

## Lookup Order
1) Built-ins (`std::print`, `std::alloc`, `std::free`, `std::deref`, `std::store`, `std::ptr_of`)
2) Per-VM cache
3) `.quill/lib` (package manager output directory)
4) `$INK_FOREIGN_PATH` (path list)
5) System loader

## Library Filenames
Candidates searched:
- `ink_foreign_<name><ext>`
- `libink_foreign_<name><ext>`

Where `<ext>` is `.so`, `.dylib`, or `.dll`.

## Failure
Missing foreigns halt the VM at runtime.
