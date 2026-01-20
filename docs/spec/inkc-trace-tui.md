# inkc Compiler Trace TUI Spec (v1)

## Goals
- Always-on compile animation in TTY mode (no opt-out) that mirrors the real compiler.
- Full transparency: show source -> tokens -> AST -> macros -> IR -> bytecode, plus compiler call stack and code lines.
- Human-centric, teachy, and fast to scan; no interactivity required.
- Psychedelic but tasteful visuals: pulsing accents, soft motion, and bold layout without noise.
- Integrate AI narration that scales with local model speed (small models first, richer with bigger models).
- Support multiple diagnostics (errors/warnings/notes) per run and show all of them.
- JSON trace export for tooling via --trace.

## Non-Goals (v1)
- Interactive controls inside the compiler TUI (no keybindings or input handling).
- Disabling the animation in TTY mode.
- Remote or networked UI.

## Triggering + Output
- TTY detection on stdout enables the TUI (always on).
- Non-TTY: no TUI; compiler behaves normally.
- --trace <path> writes JSON line events to file.
- Diagnostics summary is printed to stdout after TUI exit.

## Trace Pipeline
- Emit trace events from the real compiler as it runs.
- Event fields include:
  - kind, phase, tag, message
  - file, fn_name, line, column
  - source_id, span (start/end)
  - index/index2 for progress and ranges
  - stack frames (from trace.scope)
- JSON trace is line-delimited JSON objects with the above fields.

## UI Layout
- Layout is fixed (no interaction):
  - Left: source view (read-only, line numbers, inlay hints)
  - Right top: phase transform log (current phase events)
  - Right middle: compiler view (compiler file+line and call stack)
  - Right bottom: IR view or byte view (IR log; hex view during encode)
  - Bottom: status bar (phase timeline, counters, AI summary snippet)

## Animations + Visual Style
- Intro: soft fade-in + light particle haze.
- Per phase: accent color shifts by phase, slow pulse on borders.
- Focus: current source line highlighted; byte focus marked in red.
- Outro: summary fades in; particles dissipate; TUI exits cleanly to original console.
- Palette is dark with bright accents; no purple bias.

## Stage Mapping (Compiler -> UI)
- source_store: source list + file discovery logs.
- module_graph: module dependency build and source mapping logs.
- lex: token stream + token count; per-token hints.
- parse: parse nodes + parse tree events.
- ast: AST node creation events; per-node hints.
- macro_discover: macro discovery events, stream sources.
- macro_compile: macro compilation + token tree events.
- macro_expand: macro expansion, registry, stream concat/slice.
- desugar: syntax rewrites and node transforms.
- resolve: name resolution, scopes, bindings, imports.
- uir: UIR node emission + spans.
- typecheck: type constraints, unification, inference steps.
- mir: MIR node conversion.
- lir: instruction emission + mapping to spans.
- backend: foreigns, exports, runtime glue.
- encode: instructions + bytecode hex view.

## Source View + Inlay Hints
- Source view uses SourceCache from inkui.
- Inlay hints combine event hint + AI hint, appended to end of line.
- Focused line is highlighted; scroll follows focus.

## Compiler View
- Shows compiler file:line (from @src) and stack frames (trace.scope).
- Helps visualize the compiler's "thought path".

## Byte View
- Reuses inkx memory-pane layout logic (hex + ASCII view).
- Byte focus range highlights current chunk in red.
- Bytes-per-line adapts to available width.

## AI Integration
- Local-first: Ollama HTTP on 127.0.0.1:11434, small model default.
- **Blocking preflight**: compiler waits for model readiness (Ollama `/api/show` then `/api/pull` if missing).
- **Download UI**: AI panel shows a progress bar + status during model pull; status updates in-place.
- **LLM handshake**: before compile, request a short “READY” response; display in AI panel as proof of life.
- **Always-on analysis**: every trace event yields a baseline analysis line; LLM responses augment with richer notes.
- LLM calls are enabled for all event kinds; analysis continues through summary/outro until shutdown.
- Prompts include phase, tag, message, source line, compiler line, stack.

## Diagnostics
- Do not stop at first error; show all diagnostics collected.
- Diagnostics appear in transform log + source inlay hints.
- Summary panel shows counts for errors/warnings/notes.

## JSON Trace (--trace)
- Line-delimited JSON, one event per line.
- Includes stack frames and spans when available.
- Designed for offline playback or alternative UI tools.

## Reuse + Refactor
- Use inkui for Terminal, Canvas, Renderer, LogBuffer, SourceCache.
- Reuse inkx memory-pane formatting logic (hex + ASCII).
- Keep SourceCache and line mapping shared in inkui.

## Visual Examples (one per stage)

Stage: source_store
+--------------------------------+--------------------------------+
| Source                         | Transform                       |
|  1 > main.ink                  | source_store.source #0          |
|  2   std/src/prelude.ink       | source_store.source #1          |
|                                |                                 |
+--------------------------------+--------------------------------+
| Compiler                        | IR/Byte                         |
| compiler.zig:1659              | no ir yet                       |
+-----------------------------------------------------------------+
| [source_store] lex parse ast ... | tok:0 ast:0 uir:0 err:0 warn:0 |
+-----------------------------------------------------------------+

Stage: module_graph
+--------------------------------+--------------------------------+
| Source                         | Transform                       |
|  1 > main.ink                  | module_graph.module main        |
|  2   std/src/prelude.ink       | module_graph.dep std            |
|                                | module_graph.source #0          |
+--------------------------------+--------------------------------+
| Compiler                        | IR/Byte                         |
| compiler.zig:1707              | no ir yet                       |
+-----------------------------------------------------------------+
| [module_graph] lex parse ast... | tok:0 ast:0 uir:0 err:0 warn:0 |
+-----------------------------------------------------------------+

Stage: lex
+--------------------------------+--------------------------------+
| Source                         | Transform                       |
| 12 > let x = 10                | lex tok#32 identifier           |
| 13   print(x)                  | lex tok#33 operator             |
|                                | lex tok#34 integer              |
+--------------------------------+--------------------------------+
| Compiler                        | IR/Byte                         |
| compiler.zig:2576              | no ir yet                       |
+-----------------------------------------------------------------+
| [lex] parse ast ...             | tok:64 ast:0 uir:0 err:0 warn:0 |
+-----------------------------------------------------------------+

Stage: parse
+--------------------------------+--------------------------------+
| Source                         | Transform                       |
| 12 > let x = 10                | parse#18 stmt                   |
| 13   print(x)                  | parse#19 expr                   |
|                                | parse#20 call                   |
+--------------------------------+--------------------------------+
| Compiler                        | IR/Byte                         |
| peg_parser.zig:301             | no ir yet                       |
+-----------------------------------------------------------------+
| [parse] ast ...                 | tok:64 ast:0 uir:0 err:0 warn:0 |
+-----------------------------------------------------------------+

Stage: ast
+--------------------------------+--------------------------------+
| Source                         | Transform                       |
| 12 > let x = 10                | ast#211 decl                    |
| 13   print(x)                  | ast#212 call                    |
|                                | ast#213 identifier              |
+--------------------------------+--------------------------------+
| Compiler                        | IR/Byte                         |
| peg_ast.zig:3356               | no ir yet                       |
+-----------------------------------------------------------------+
| [ast] macro ...                 | tok:64 ast:213 uir:0 err:0 warn:0 |
+-----------------------------------------------------------------+

Stage: macro_discover
+--------------------------------+--------------------------------+
| Source                         | Transform                       |
| 20 > @macro foo                | macro discover macro foo        |
|                                | macro discover scan registry    |
|                                | macro discover token stream     |
+--------------------------------+--------------------------------+
| Compiler                        | IR/Byte                         |
| macro_context.zig:510          | no ir yet                       |
+-----------------------------------------------------------------+
| [macro_discover] desugar ...    | tok:64 ast:213 uir:0 err:0 warn:0 |
+-----------------------------------------------------------------+

Stage: macro_compile
+--------------------------------+--------------------------------+
| Source                         | Transform                       |
| 20 > @macro foo                | macro compile stream slice      |
|                                | macro compile token tree        |
|                                | macro compile expand decl       |
+--------------------------------+--------------------------------+
| Compiler                        | IR/Byte                         |
| macro_context.zig:560          | no ir yet                       |
+-----------------------------------------------------------------+
| [macro_compile] expand ...      | tok:64 ast:213 uir:0 err:0 warn:0 |
+-----------------------------------------------------------------+

Stage: macro_expand
+--------------------------------+--------------------------------+
| Source                         | Transform                       |
| 21 > foo!(x)                   | macro expand stream concat      |
|                                | macro expand tree emit          |
|                                | macro expand registry merge     |
+--------------------------------+--------------------------------+
| Compiler                        | IR/Byte                         |
| macro_context.zig:573          | no ir yet                       |
+-----------------------------------------------------------------+
| [macro_expand] desugar ...      | tok:64 ast:213 uir:0 err:0 warn:0 |
+-----------------------------------------------------------------+

Stage: desugar
+--------------------------------+--------------------------------+
| Source                         | Transform                       |
| 12 > let x = 10                | desugar.node let -> const       |
| 13   print(x)                  | desugar.node call -> intrinsic  |
|                                | desugar.node match flatten      |
+--------------------------------+--------------------------------+
| Compiler                        | IR/Byte                         |
| desugar.zig:342                | no ir yet                       |
+-----------------------------------------------------------------+
| [desugar] resolve ...           | tok:64 ast:213 uir:0 err:0 warn:0 |
+-----------------------------------------------------------------+

Stage: resolve
+--------------------------------+--------------------------------+
| Source                         | Transform                       |
| 12 > let x = 10                | resolve.scope push              |
| 13   print(x)                  | resolve.bind x                  |
|                                | resolve.import std              |
+--------------------------------+--------------------------------+
| Compiler                        | IR/Byte                         |
| resolver.zig:590               | no ir yet                       |
+-----------------------------------------------------------------+
| [resolve] uir ...               | tok:64 ast:213 uir:0 err:0 warn:0 |
+-----------------------------------------------------------------+

Stage: uir
+--------------------------------+--------------------------------+
| Source                         | Transform                       |
| 12 > let x = 10                | uir#88 decl                     |
| 13   print(x)                  | uir#89 call                     |
|                                | uir#90 literal                  |
+--------------------------------+--------------------------------+
| Compiler                        | IR/Byte                         |
| uir/build.zig:872              | uir#88 decl                     |
+-----------------------------------------------------------------+
| [uir] typecheck ...             | tok:64 ast:213 uir:90 err:0 warn:0 |
+-----------------------------------------------------------------+

Stage: typecheck
+--------------------------------+--------------------------------+
| Source                         | Transform                       |
| 13 > print(x)                  | typecheck.infer.call_arg 1/1    |
|                                | typecheck.unify                 |
|                                | typecheck.constraint add        |
+--------------------------------+--------------------------------+
| Compiler                        | IR/Byte                         |
| typecheck.zig:2478             | uir#89 call                     |
+-----------------------------------------------------------------+
| [typecheck] mir ...             | tok:64 ast:213 uir:90 err:0 warn:0 |
+-----------------------------------------------------------------+

Stage: mir
+--------------------------------+--------------------------------+
| Source                         | Transform                       |
| 13 > print(x)                  | mir#90 call                     |
|                                | mir#91 literal                  |
|                                | mir#92 intrinsic                |
+--------------------------------+--------------------------------+
| Compiler                        | IR/Byte                         |
| mir/lower.zig:47               | mir#90 call                     |
+-----------------------------------------------------------------+
| [mir] lir ...                   | tok:64 ast:213 uir:90 err:0 warn:0 |
+-----------------------------------------------------------------+

Stage: lir
+--------------------------------+--------------------------------+
| Source                         | Transform                       |
| 13 > print(x)                  | lir inst#442 call               |
|                                | lir inst#443 load               |
|                                | lir inst#444 ret                |
+--------------------------------+--------------------------------+
| Compiler                        | IR/Byte                         |
| lir/vm/lower.zig:601           | inst#442 call                   |
+-----------------------------------------------------------------+
| [lir] backend ...               | tok:64 ast:213 uir:90 err:0 warn:0 |
+-----------------------------------------------------------------+

Stage: backend
+--------------------------------+--------------------------------+
| Source                         | Transform                       |
|  1 > module main               | backend.foreign_root 0/2        |
|                                | backend.foreign_name printf     |
|                                | backend.foreign_copy            |
+--------------------------------+--------------------------------+
| Compiler                        | IR/Byte                         |
| compiler.zig:2281              | inst#445 foreign                |
+-----------------------------------------------------------------+
| [backend] encode ...            | tok:64 ast:213 uir:90 err:0 warn:0 |
+-----------------------------------------------------------------+

Stage: encode
+--------------------------------+--------------------------------+
| Source                         | Transform                       |
| 13 > print(x)                  | encode inst#451 call            |
|                                | encode byte@0 len=16            |
|                                | encode byte@16 len=16           |
+--------------------------------+--------------------------------+
| Compiler                        | IR/Byte (hex view)              |
| compiler.zig:2530              | 0x00000000: 7f 01 2a 00 ...      |
+-----------------------------------------------------------------+
| [encode] finish                 | tok:64 ast:213 uir:90 err:0 warn:0 |
+-----------------------------------------------------------------+
