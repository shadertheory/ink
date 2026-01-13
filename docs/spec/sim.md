# Ink Simulator Config

Simulator configuration lives in `simulator.ink` when present, otherwise in
`package.ink`. It is active only when:
- `import sim` is present
- a top-level `fn simulator()` is defined

## Top-Level Layout

```
import sim

fn simulator()
	sim::simulator
		seed = 0
		concurrency = "half"
		foreigns = sim::foreigns
			allow_categories = "mem io time task macro"
		snapshots = sim::snapshots
			steps = 1
			mode = "full+delta"
			level = "full"
			compress = "zstd"
		validation = sim::validation
			level = "strict"
		report = sim::report
			text = "sim-report.txt"
			json = "sim-report.json"
			tree = true
			per_scenario = true
			aggregate = true
```

## Scenarios

```
	scenario = sim::scenario
		name = "default"
		components = sim::components
			tcp = "mock"
			udp = "mock"
			fs = "real"
			clock = "sim"
			rng = "sim"
			alloc = "sim"
			scheduler = "sim"
		faults = sim::faults
			tcp_drop = 0.01
			io_error = 0.001
```

Composition merges base scenarios before overrides:

```
	scenario = sim::scenario
		name = "chaos"
		compose = "default"
		faults = sim::faults
			tcp_drop = 0.05
```

## Fault Rules

Shorthand keys live in `sim::faults`:
- `io_error`, `tcp_drop`, `udp_drop`, `fs_error`, `oom`
- `delay`, `reorder`, `corrupt`, `partial`, `disconnect`, `timeout`
- `tcp_delay`, `fs_corrupt`, etc (component prefix + fault kind)

Explicit rule blocks allow filters:

```
		faults = sim::faults
			fault = sim::fault
				kind = "delay"
				component = "tcp"
				op = "read"
				probability = 0.05
				delay_ns = 1000000
```

## Sweeps

Sweeps expand scenarios into multiple runs:

```
	scenario = sim::scenario
		name = "sweep"
		compose = "default"
		sweep = sim::sweep
			mode = "grid"
			faults.tcp_drop = "0 0.02 0.1"
			components.fs = "mock real"
```

Keys inside sweeps accept:
- `faults.<name>`
- `components.<name>`
- `seed` (per-run override)

Modes:
- `grid` (cartesian product)
- `sample` (random samples)

## Snapshots

Snapshots are written under `.ink/snapshots` in the package root.

```
snapshots = sim::snapshots
    steps = 1
    mode = "full+delta"
    level = "full"
    compress = "compact"
    retention = 32
```

Modes:
- `full` (default): each snapshot contains the full submit/event log.
- `delta`: first snapshot is full, then deltas chained to the previous snapshot.
- `full+delta`: periodically writes a full snapshot (every `retention` if set, otherwise every 16), with deltas between.

Levels:
- `full`: includes read data for deterministic replay.
- `events`: includes submit/event metadata without read payloads.
- `meta`: same as `events` today (no read payloads).
- `none`: disables data capture (snapshots are still written but not replayable).

Compression:
- `compact`: varint/delta encoding for logs.
- `zstd`: alias for `compact` (no external codec needed).
- `none`: raw encoding.

## Foreign Allowlist

`sim::foreigns` controls which foreigns are allowed in simulator builds.
If omitted, sim builds default to deny-all.

Fields:
- `allow_categories` / `deny_categories`
- `allow` / `deny` (unqualified names)

## Validation Overrides

`sim::validation` can override individual validation checks on top of the
`level` default:

```
validation = sim::validation
	level = "strict"
	macro_streams = true
	macro_ast = true
	ast = true
	uir = true
	mir = true
	lir = false
```

Keys: `macro_streams`, `macro_ast`, `ast`, `uir`, `mir`, `lir`

## CLI

```
quill sim --manifest ./package.ink --scenario chaos
quill sim --manifest ./package.ink --all
quill sim --manifest ./package.ink --seed 123 --samples 50
```
