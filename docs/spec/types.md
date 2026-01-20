# Ink Type Operators and ADTs (Spec Draft)

Goals
- Haskell-like algebraic data types with concise syntax that lower to existing Ink constructs.
- TypeScript-style unions/intersections without runtime cost.
- Zero new runtime features in Phase I: all features are compile-time desugar only.

Current architecture (from `ink/src`)
- Type expressions (`type_expr`) support: `self`, name, `?T` optional, and `Type<Args>` applied.
- Decls already exist for `sum`, `enum`, `struct`, `trait`, `concept`, and `impl`.
- Resolver only checks names; no full type checker yet.

Intrinsic types (runtime)
- `range<T>` is a builtin generic type representing a bounded range over `T`.
- The canonical surface constructor is the range operators:
  - `a .. b` (half-open) -> `range { start = a, end = b, inclusive = false }`
  - `a ..= b` (inclusive) -> `range { start = a, end = b, inclusive = true }`
- Open-ended ranges are not in scope yet.

Phase I: Add syntax, desugar to existing AST
1) Type alias
- New decl: `type Name<...> = TypeExpr`
- Introduces a type name in the type namespace.
- If the RHS is a structural form, it desugars to existing decls:
  - Union of variants -> `sum`.
  - Union of payloadless variants -> `enum`.
  - Record literal -> `struct`.
  - Tuple literal -> `struct` with `_0`, `_1`, ... fields.
  - Otherwise -> alias (no new runtime type).

2) Type operators (syntax only in type positions)
- Union: `A | B`
- Intersection: `A & B`
- Optional: `?T`
- Reference: `&T`, `&mut T`
- Function type: `A -> B`
- Tuple type: `(A, B, C)`
- Array/slice: `[]T` and `[N]T`
- Record type: `record` block with `name: Type` fields
- Integer aliases: `iN`, `uN`, `bN` (arbitrary N)
  - `name?` is sugar for `name: ?Type`.

Operator precedence and associativity
1) Postfix: `Type<Args>`, `[]T`, `[N]T`
2) Prefix: `?T`, `&T`, `&mut T`
3) Intersection: `A & B` (left associative)
4) Union: `A | B` (left associative)
5) Arrow: `A -> B` (right associative)

Desugaring rules
- `A | B`
  - Default: `union<A, B>` (builtin type alias in std).
  - In `type Name = ...` where terms look like variants, lower to `sum Name`.
- `A & B`
  - Default: `intersect<A, B>` (builtin type alias in std).
  - If both sides are record/struct literals, merge fields (error on conflict).
  - If either side is a trait, lower to trait requirements.
- `?T`
  - Alias to `option<T>` where `option<T>` is defined as a `sum` in std.
  - Keep `?T` as canonical surface syntax for optional.
- `A -> B`
  - Lower to `fn<A, B>` (builtin alias). Right associative:
    `A -> B -> C` == `A -> (B -> C)`.
- `(A, B, C)`
  - Lower to `tuple<A, B, C>` (builtin alias) or a generated `struct`.
- `[]T` and `[N]T`
  - Lower to `slice<T>` and `array<N, T>` (builtin aliases).
- `record` block
  - Lowers to `record<...>` (builtin alias) or a generated `struct`.
- `iN` / `uN` / `bN`
  - Sugar for `int<N>`, `uint<N>`, and `uint<N>` respectively.

Examples
- Haskell-style sum:
  type maybe<T> = just(T) | none

- One-line enum:
  type color = red | green | blue

- Function type:
  type handler = event -> result

- Tuple:
  type pair<T, U> = (T, U)

- Record type:
  type user = record
      id: int
      name?: string

Simplifications (architecture)
- Treat `enum` as a `sum` where all variants are payloadless.
- Treat `?T` as sugar for `Option<T>`.
- Prefer `type` for aliasing; keep `sum`/`struct` as explicit forms.

Minimal compiler changes (Phase I)
- Parser: extend `type_expr` grammar with `|`, `&`, `->`, tuple, slice/array, and `record` block.
- AST/IR: either add new `type_expr` variants or desugar in the AST builder to existing forms.
- Desugar: convert `type` aliases and type operators into `sum`/`enum`/`struct` or `Type<Args>`.
- Resolver: walk new type nodes or resolve after desugar; no new runtime support.
