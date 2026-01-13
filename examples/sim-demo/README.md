# sim-demo

This example shows how the simulator wires mock components, fault injection, composition, and sweeps.

What it demonstrates:
- Mock TCP accept + read/write using encoded fds.
- FS writes via `std::write` (real vs mock via components).
- Fault injection counts in the "chaos" scenario.
- Composition and sweeps for scenario expansion.

Run from the repo root (sim only):
- `quill sim --manifest examples/sim-demo/package.ink --scenario default`
- `quill sim --manifest examples/sim-demo/package.ink --scenario chaos`
- `quill sim --manifest examples/sim-demo/package.ink --scenario sweep`
- Replay the last snapshot (path comes from `sim-report.txt`):
  `quill sim --manifest examples/sim-demo/package.ink --scenario chaos --replay examples/sim-demo/.ink/snapshots/<snapshot>.inksnapshot`

Notes:
- `chaos` composes `default` and adds delay + drop faults.
- `sweep` expands into multiple runs with different fault/component mixes.
- Reports are written to `examples/sim-demo/sim-report.txt` and `examples/sim-demo/sim-report.json`.
- Simulator settings live in `examples/sim-demo/simulator.ink`.
- Snapshots are written under `examples/sim-demo/.ink/snapshots`.
