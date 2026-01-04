# Ink Memory Model

Goals
- fast execution in the VM now
- direct path to native code later
- predictable layout for structs, arrays, and trait objects
- minimal runtime cost when static dispatch is used

## Core Value Model

Word size
- primary machine word is 64 bit
- all VM registers and stack slots store one word
- native backend uses the platform word size and keeps the same layout rules

Value kinds
- int and bool are immediate values in a word
- float is stored as IEEE 754 bits in a word
- pointers are word sized addresses
- string literals are indices into the constant data table
- unit is zero

Type information
- no runtime tags in release mode
- debug mode may carry a type id for diagnostics
- static dispatch uses compile time types only

## Stack Model

Frames
- each function call allocates a frame
- frame holds locals and temporaries in word slots
- arguments are passed in registers then spilled to the frame as needed

Tasks
- each task owns a stack
- stacks are not shared across tasks
- stack grows downward in VM tape and in native code

## Heap Model

Allocator
- default allocator is bump plus free list
- small allocations are fast and contiguous
- free list reuse is best effort and optional in release mode

Object header layout in words
- size
- flags
- next

Object data
- payload immediately follows the header
- alignment is word aligned
- object size is in words

Pointers
- pointer value is the first payload word index in VM tape
- pointer value is a raw address in native code

Safety checks
- debug mode validates bounds and double free
- release mode skips checks for speed

## Structs and Records

Layout
- fields are stored in declaration order
- all fields are word aligned
- field offsets are compile time constants

Construction
- record literal lowers to a constructor that allocates and stores fields in order
- missing fields are a compile error

Access
- field access lowers to a direct offset load
- in VM this is a pointer plus offset
- in native code this is a base plus offset

## Arrays and Slices

Slice
- two words, pointer and length

Array
- contiguous payload on the heap
- array value is a pointer to payload
- length is known from the array type

## Strings

Literal strings
- stored in the constant data table
- value is an index to the table
- conversion to slice yields pointer and length

Heap strings
- allocated as length plus payload
- value is a pointer to payload

## Trait Objects

Runtime trait
- trait object is two words
- vtable pointer and data pointer
- vtable is a table of function pointers

Static concept
- no runtime object
- calls are resolved at compile time

## Concurrency and Memory Order

Data races
- data races are undefined behavior
- shared mutable data requires explicit synchronization

Atomics
- atomic operations are provided by the core runtime
- memory order is explicit in the intrinsic name

## Native Backend Notes

Code generation
- struct and array layout matches the VM
- direct calls for static dispatch
- indirect calls for runtime traits using vtable

Interop
- foreign functions accept word sized values or pointers
- large values are passed by pointer

