# inkx Debugger + Simulator Spec (v1)

## Goals
- Full debugger for Ink with TUI and DAP compatibility.
- Unified runtime + compiler debugging in one UI.
- Time travel in sim mode via snapshots + replay.
- Global stop semantics by default, with per-task control.

## Non-goals (v1)
- Remote debugging over network (protocol stub only).
- Reverse execution outside sim mode (forward-only default).

## Architecture
- inkx:
  - Mode detection: 200ms stdin probe for DAP `Content-Length`; otherwise TUI.
  - Launch: spawn inkvm over stdio pipes.
  - Attach: connect to inkvm debug listener over TCP.
  - TUI + DAP share the same command core (no feature gaps).
  - Headless: same DAP framing as inkvm (no feature gaps).
- inkvm:
  - Debug agent that serves DAP over TCP and stdio.
  - Writes `.ink/debug.json` and prints endpoint to stderr.
  - Simulator mode uses the same VM backend (no sim build profile).

## DAP Compatibility
- Content-Length framing only (same framing for headless inkx).
- Capabilities handshake aligned with DAP.
- Streaming events enabled (stop, output, task state, snapshot, replay).
- Watch expressions accept string form and AST form.

## Debug Info (Compiler + inkb)
- Source map + line table (line <-> op mapping).
- Function ranges + op names.
- Scopes + locals + variable locations.
- Task mapping metadata.
- Versioned inkb debug section with fallback.

## VM Debug Features
- Breakpoints: line, function, op (friendly op names).
- Watchpoints: read/write/both, memory range, expression-based.
- Execution: continue, pause, step over/in/out, reverse step (sim).
- Global stop by default, per-task control optional.
- Introspection: stack, locals, registers, memory view (hex + ASCII),
  tasks tree with colored states.
- Events: unified timeline with program tree + sim tree + snapshots.

## Snapshots + Replay
- Full VM state + sim logs in `.ink/snapshots/*.inksnapshot`.
- Retention limits:
  - 1GB global cap.
  - 128MB per scenario cap.
  - Both enforced with per-scenario trimming.
- RAM cache capped at 1/4 system RAM; stream from disk + evict on pressure.
- Compression: compact encoding by default.
- Reverse step: sim-only using snapshot + replay.

## inkx TUI
- Layout (nvim-dap-ui style, tab groups supported):
  - Left: Breakpoints / Watch / Scopes
  - Center: Source
  - Right: Stack / Tasks
  - Bottom: REPL / Output / Events
- Keybindings:
  - c continue, p pause, n step over, i step in, o step out, r reverse (sim)
  - b toggle breakpoint, B add breakpoint
  - w add watch, W edit watch, x remove watch
  - : command palette, / search, ? help

## CLI Integration
- `quill debug` launches inkx + inkvm in stdio mode by default.
- `--listen` starts inkvm in TCP mode for attach.
- `--sim` remains a runtime flag; no sim build profile.

## Compiler Debugging
- Same UI and DAP pipeline; switch via tab groups.
- Macro expansion + AST/IR pass stepping supported.

## Deliverables
- inkx binary (TUI + DAP server).
- Debug-enabled inkvm with inkdbg.
- Expanded inkb debug info + compiler emission.
- Snapshot retention manager (RAM/disk limits).
- Docs + examples.
