# The Story of Building Ink (from Git Diffs)

This story is grounded in the commit-by-commit diffs in the repo history (67d0220 → 366dc4c), but told in my own voice.

## Story

I started Ink because I wanted terse, cross‑platform power: Java‑level portability, Rust and Zig‑like semantics, and a runtime strong enough to carry the language. I was tired of scratching my head every time I tried to make complex graphics and systems apps work everywhere. Zig felt like the right foundation: low‑level fundamentals without bloat, but still enough syntax to stay expressive.

The name “Ink” was a reminder that this is writing, not just programming. The mascot is a squid, and so is the language itself. I wanted the project to feel alive, and I wanted it to carry hope. I was also thinking of theprimeagen the whole time — a hero in the background keeping me pointed forward.

The first real drop was a full stack: build files, a lexer and parser, a starter AST, VM scaffolding, and a compiler driver. The lexer was the easy win — simple, clean, and fun. The first high was the AST builder; when that landed, it felt amazing.

Then I dove into the machine model. The VM and bytecode runner taught me a lot about how assembly works. The moment it clicked was the first time the VM calculated fib(10). That was the proof I needed that this wasn’t just syntax anymore. I leaned into tasteful but extensive syntax sugar, and I made an early call that still matters: keep concept/trait separate, and keep enum/sum separate. Those distinctions are a big part of Ink’s shape.

The language scope expanded: traits, generics, concepts, sums, enums, applied and optional types, where‑clauses — all of it. I also baked in async because I want concurrency to be native to the experience. The long stretch of uncertainty was the gap between having everything but the compiler and actually linking the compiler to the VM. When that finally worked, it felt like the whole system started breathing. Understanding how the compiler stages interact was the turning point.

Tooling made it feel real. The first time the LSP worked and I saw syntax highlighting, it hit me that this was becoming a real ecosystem. I love the ink/quill/squid universe I’m building. Ink shouldn’t be a general‑purpose language — it should be for terse, performance‑heavy, calculation‑centric applications. That focus is part of its identity.

So far, it’s been smooth. No painful deletions. Just steady growth. This project is hope, and I want it to keep giving that back.

## Timeline (from diffs)

- **2025‑12‑09** — Initial commit: licensing + ignore rules.
- **2025‑12‑09** — “Hello, squid!”: first full stack drop (build scripts, lexer/parser/AST/tokens, VM skeleton, `inkc` parser driver, sample `.ink`).
- **2025‑12‑09** — “Work on machine.”: typed VM operations, address model, comparison helper; instruction set gains structure.
- **2025‑12‑09** — “Redesign AST and instruction set”: AST grows to include traits/generics/types; VM math ops carry type info.
- **2025‑12‑10 / 12** — VM refactor + cleanup: VM definitions move into `vm/exe.zig`, stray artifacts removed, early IR experiment added.
- **2025‑12‑13** — “Apply changes”: assembly/encoding work begins; op/core/exe reorganized.
- **2025‑12‑14** — Bytecode redesign: comptime assembler, variable‑width instruction formats, dispatch table; string interning removed.
- **2025‑12‑14** — VM refactor: assembly/op/core cleaned; stray file removed.
- **2025‑12‑19** — Language expansion: traits/generics/concepts/sums/enums + IR/symbol scaffolding; parser/lexer updated.
- **2025‑12‑22** — Parser/IR overhaul: new IR core; AST grows to cover expanded syntax.
- **2025‑12‑24** — PEG parser + IR builder: grammar-driven parser + AST→IR pipeline.
- **2025‑12‑28** — Pipeline complete: IR codegen + VM encoding; const/var/string support; legacy parser removed.
