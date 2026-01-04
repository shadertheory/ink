# Editor Highlighting and Diagnostics Spec

Version: 0.1  
Status: draft  
Audience: inkd, compiler, and editor integration maintainers

## Goals
- Provide richer, stable syntax highlighting that improves as the AST/resolution becomes available.
- Provide explainable, actionable diagnostics (Rust-like) with hints and fix-its where possible.
- Keep latency low for realtime editing; avoid full recompilation for every keystroke.

## Non-Goals
- Full semantic type inference in the editor (initially).
- Perfect recovery for all invalid programs; aim for graceful best-effort.

## Architecture Overview

### Highlighting Pipeline
1) **Lexical tokens (fast, incremental)**  
   - Produced by the lexer; used immediately for basic coloring.
2) **Parsed AST (medium cost, incremental or partial)**  
   - Disambiguate identifiers into types/values/functions; enable richer classification.
3) **Resolved symbols (slower, optional)**  
   - Use resolver output to classify unknown identifiers, imports, and member access.

### Diagnostics Pipeline
1) **Lexer errors** (invalid characters, unterminated literals).  
2) **Parser errors** (unexpected token, missing token, incomplete construct).  
3) **AST build errors** (unsupported constructs or broken forms).  
4) **Resolver errors** (unknown identifiers/types, invalid imports).  
5) **IR/codegen errors** (feature gaps, internal errors).

## LSP Integration
- Support `textDocument/semanticTokens/full`.  
  Optional: `semanticTokens/delta` once diffing is reliable.
- Support `textDocument/publishDiagnostics` with rich messages and ranges.
- Use UTF-16 column conversion for LSP positions; source spans remain byte offsets.

## Highlighting Spec

### Token Types (LSP)
Use these LSP token types in `semanticTokensProvider.legend`:
- `namespace`, `type`, `function`, `variable`, `property`, `keyword`, `number`, `string`, `operator`, `boolean`

### Token Modifiers
Initially empty. Future modifiers:
- `declaration`, `readonly`, `static`, `deprecated`, `async`, `mutable`

### Lexical Highlighting Rules (Baseline)
Use token kinds from `ink/src/lang/token.zig`:
- Keywords: `fn`, `struct`, `trait`, `concept`, `impl`, `enum`, `sum`, `type`, `import`, `from`, `foreign`, `where`, `requires`, `if`, `else`, `match`, `return`, `self`, `this`, etc.
- Literals: numbers, strings, `true`/`false`.
- Operators/punct: `+ - * / = :: . ?. |> ?? < > <= >= == !=`, brackets, commas, etc.
- Identifiers default to `variable` unless overridden by AST or resolver data.

### AST-Aware Highlighting
Promote identifiers based on AST context:
- Function declarations -> `function`
- Type declarations -> `type`
- Imports -> `namespace`
- Field access (`.`) -> `property`
- Scope access (`::`) -> `namespace`

### Resolver-Aware Highlighting (Optional)
If resolver data is available:
- Unknown identifiers -> keep `variable` but add `diagnostic` with hint.
- Known types/values -> upgrade token types accordingly.
- Imported item aliases -> `namespace` or `type` as appropriate.

## Incremental Lex/Parse

### Lexing
- Re-lex from the line before the edit to preserve indent/dedent state.
- Stop when two consecutive lines produce identical token sequences.
- Cache per-line tokens and indent stack depth.

### Parsing
- Parse whole file only when:
  - lexer fails to stabilize after a small window, or
  - edit crosses block boundaries with ambiguous indent.
- Otherwise, rebuild AST for the minimal region and splice into cache.
  (If partial parsing is difficult, fall back to full parse; keep lex incremental.)

### AST Cache (inkd)
Per-document cache should store:
- Text
- Tokens
- Parse result (parse tree + arena)
- AST nodes (if built)
- Last parse error (if any)
- Version counter

On change:
- Update text.
- Rebuild token/parse/AST cache (best-effort).
- If parser/AST fails, keep old AST for highlight fallback but publish fresh errors.

## Diagnostic Spec

### Diagnostic Fields
Each diagnostic should include:
- `severity`: note/warn/error
- `message`: human-readable
- `span`: byte offsets in source text
- `code`: stable string code (e.g., `E0001`)
- `related`: optional secondary ranges (e.g., for `import` + `use`)
- `fixes`: optional suggested edits

### Message Structure
Prefer short, consistent message text:
- Primary error: what is wrong.
- If suggestion exists: `"did you mean 'X'?"`.
- Optional note: “help:”/“note:” with explanation.

### Error Recovery Strategy
- Parser should recover by:
  - skipping unexpected tokens until a safe boundary (newline, dedent, end).
  - inserting missing tokens for common constructs (e.g., missing `)`).
- AST builder should surface the earliest useful error and avoid cascades.

### Suggestion Engine
Use bounded Levenshtein distance for:
- unknown identifiers
- unknown types
- unknown imports
Distance thresholds:
- 1 for length <= 4
- 2 for length <= 7
- 3 otherwise

### Fix-Its (Future)
Emit edits for common recoverable errors:
- Missing `)` or `]`
- Missing `:` in type annotations
- `=` vs `:=` style mistakes (if applicable)

## Examples

### Unknown Identifier
Message:
```
unknown identifier 'pritn'; did you mean 'print'?
```

### Parse Error
Message:
```
parse error: expected ')' or identifier, found '->'
```

## Testing and Metrics
- Unit tests for token mapping and diagnostic formatting.
- Golden tests for parse/AST errors with sample inputs.
- Measure:
  - time to rehighlight after 1-char edit
  - max tokens emitted per request
  - diagnostics latency
