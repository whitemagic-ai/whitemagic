# Eval Harness

Whitemagic includes a small, deterministic evaluation harness to sanity-check the
AI-primary tool contract in a fresh state root.

## Run

```bash
WM_STATE_ROOT=/tmp/whitemagic_eval WM_SILENT_INIT=1 python3 eval/run_eval.py
```

Or with explicit flags (recommended for portability):

```bash
python3 eval/run_eval.py --state-root /tmp/whitemagic_eval --silent-init
```

## What It Checks

- Tool envelope shape is stable and JSON-serializable
- `now` override is respected (deterministic timestamps)
- `idempotency_key` replays for write tools
- `ship.check` detects common OSS hygiene issues without scanning user memory

## Output

The harness writes a JSON report under:

`$WM_STATE_ROOT/artifacts/eval/<timestamp>/report.json`

