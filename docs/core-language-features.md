# core language feature spec (ink)

this spec defines new core language features required for a pure-ink networking stack and web tooling.

## 1) structured concurrency

goal: make all concurrency scoped, cancelable, and leak-free.

### constructs
- `task` type: handle to a running computation.
- `spawn` expression: `let t = spawn fn() -> T { ... }`
- `await` expression: `let v = await t`
- `cancel` function: `cancel(t)` requests cancellation.
- `timeout` helper: `timeout(duration, fn() -> T) -> result<T, timeout>`
- `defer cancel` is allowed; cancellation is idempotent.

### semantics
- tasks are bound to a parent scope; when the parent ends, all child tasks are canceled and awaited.
- `await` propagates errors from the task.
- cancellation cooperates: task checks `cancelled()` or uses `await`/io points that are cancellation-aware.
- cancellation is best-effort; tasks must be written to honor it.

### errors
- `error.cancelled` is a builtin error variant.
- canceling a task causes `await` to return `error.cancelled` unless the task completes first.

## 2) async io + event loop

goal: nonblocking io without external runtimes.

### runtime
- a single event loop per thread (default), with an opt-in multi-loop api.
- io operations register with the loop; no busy waiting.
- timers are part of the loop.

### language surface
- io functions return `result<T, io_error>`.
- all async io points are `await`-able.
- `select` expression:
  ```
  select
      case x = await a => ...
      case y = await b => ...
      case _ = await timeout(100ms) => ...
  ```
- `select` cancels losing branches automatically unless they are marked `detached`.

## 3) zero-copy bytes + buffers

goal: avoid copies for high-throughput networking.

### types
- `bytes`: owned, heap-allocated contiguous buffer.
- `slice<T>`: view into another buffer, does not own.
- `buf`: mutable bytes with read/write indices, supports `split`, `compact`, `reserve`.

### rules
- `bytes` is movable, not copyable.
- `slice` is copyable but does not extend lifetime of source.
- io apis accept `slice<u8>` or `buf`.

## 4) per-scope arena allocator

goal: fast, predictable allocations per request.

### constructs
- `arena` type with `alloc`, `reset`, `deinit`.
- `with arena` expression:
  ```
  with arena a:
      let s = a.alloc(bytes, n)
  ```
- arena auto-resets at scope end.

## 5) error model

goal: explicit, typed error propagation.

### constructs
- `result<T, E>` builtin.
- `try` operator for early return.
- `?` operator for `option<T>` unwrap.
- `error` enum namespace for builtin errors.

### rules
- functions declare error types explicitly:
  ```
  fn read(...) -> result<bytes, io_error>
  ```
- `try` only works on `result`.

## 6) traits + concepts for io

goal: protocol layers share a common surface.

### builtin concepts (compile time)
- `concept read<T>`: `fn read(self: T, out: buf) -> result<int, io_error>`
- `concept write<T>`: `fn write(self: T, data: slice<u8>) -> result<int, io_error>`
- `concept stream<T>`: `requires read<T>, write<T>`

### traits (runtime dispatch)
- `trait stream`: same surface as `concept stream`, used for dynamic composition.
- `requires` on concepts can absorb traits.

## 7) cancellation and deadlines

goal: unify timeouts, cancellation, and io.

### constructs
- `deadline` type, `now()`, `sleep(duration)`.
- io functions accept optional `deadline`.
- cancellation checks happen at every `await` and io boundary.

## 8) memory safety for buffers

goal: avoid use-after-free in async code.

### rules
- any `bytes` captured by a task must be owned or cloned.
- `slice` cannot outlive source; compiler error on escape.
- `buf` is move-only.

