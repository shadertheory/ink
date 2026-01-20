# Ink Operator Overloading (Draft)

This spec defines the builtin operator set, method mapping, and dispatch rules.

## Operator Set

Arithmetic
- `+` `-` `*` `/` `%`

Bitwise
- `&` `|` `^` `<<` `>>` `~`

Comparison
- `==` `!=` `<` `<=` `>` `>=`

Logical
- `and` `or` `xor` `not`

Range
- `..` `..=`

Indexing
- `[]`

Assignment
- `=`
- `+=` `-=` `*=` `/=` `%=` `&=` `|=` `^=` `<<=` `>>=`

## Method Mapping

Unary
- `-x` -> `x.neg()`
- `!x` / `not x` -> `x.not()`
- `~x` -> `x.bit_not()`

Binary
- `x + y` -> `x.add(y)`
- `x - y` -> `x.sub(y)`
- `x * y` -> `x.mul(y)`
- `x / y` -> `x.div(y)`
- `x % y` -> `x.mod(y)`
- `x & y` -> `x.bit_and(y)`
- `x | y` -> `x.bit_or(y)`
- `x ^ y` -> `x.bit_xor(y)`
- `x << y` -> `x.shl(y)`
- `x >> y` -> `x.shr(y)`
- `x == y` -> `x.eq(y)`
- `x != y` -> `x.ne(y)`
- `x < y` -> `x.lt(y)`
- `x <= y` -> `x.le(y)`
- `x > y` -> `x.gt(y)`
- `x >= y` -> `x.ge(y)`
- `x[y]` -> `x.index(y)`

Range
- `x .. y` -> `range { start = x, end = y, inclusive = false }`
- `x ..= y` -> `range { start = x, end = y, inclusive = true }`

Index assignment
- `x[y] = v` -> `x.index_set(y, v)` for non-slice types
  - compound forms use `index` + `index_set` under the hood

## Dispatch Rules

- Builtin types (`int`, `float`, `bool`, `string`) use VM operators where available.
- If the left operand is `dyn Trait` or a non-builtin type with a matching method name, the operator dispatches to the method.
- For dynamic trait objects, the method is invoked through the trait vtable.
- Range operators are syntax sugar for the intrinsic `range` constructor and do not dispatch.

## Assignment Ops

Compound assignments desugar to the underlying operator and then assign:
- `x += y` -> `x = x + y`
- `x <<= y` -> `x = x << y`
