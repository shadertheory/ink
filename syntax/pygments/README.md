# Ink Pygments Lexer

This lexer mirrors Ink's keyword/operator lists by reading `ink/src/lang/spec.zig` at import time.
If the spec file moves, set `INK_SPEC_PATH` to point at it.

## Quick test

```sh
python -m pygments -x -l syntax/pygments/ink.py src/lib.ink
```

## Notes

- Lexical-only highlighting (no AST or resolver context).
- Keywords and symbols fall back to baked-in lists if the spec file isn't found.
