# async runtime design (ink)

this doc focuses on features 1 and 2: async io and structured concurrency. it targets a clean, long-term architecture with direct os calls and a platform-agnostic ink stdlib surface.

## goals
- async io without zig std net/fs wrappers.
- platform-specific backend (kqueue on mac, io_uring on linux).
- structured concurrency with explicit cancellation.
- no boxing for user data; keep ops simple and predictable.

## architecture

### reactor (backend-specific)
the reactor owns the os event loop and submits raw io operations. it uses nonblocking fds and direct syscalls:
- mac: kqueue + kevent
- linux: io_uring (planned)

the common op set is minimal and extendable:
- read(fd, buf)
- write(fd, buf)
- accept(fd)
- timer(timeout_ns)

each submission returns an op id. poll returns a list of completions with:
- op id
- kind
- result (bytes or fd)
- error (if any)
- user data (opaque u64)

### scheduler (structured concurrency)
the scheduler runs cooperative tasks and owns a reactor:
- tasks are polled; they return ready, pending, or done.
- when a task awaits io, it registers an op id and returns pending.
- the scheduler wakes tasks when completions arrive.
- cancellation is tracked per task id; tasks can check it explicitly.

this keeps the core runtime generic and lets the vm become one of the task implementations.

## ink stdlib surface (future)
the ink stdlib stays platform-agnostic. runtime-specific behavior lives in the zig backend:
- std.net, std.fs, std.io use async ops internally.
- async io methods are awaitable and cancellation-aware.
- no global executor; event loop is explicit and owned by the runtime.

## next steps
- wire the vm executor into the scheduler as a task implementation.
- add bytecode or intrinsic hooks for spawn/await/yield.
- implement io_uring backend on linux.
