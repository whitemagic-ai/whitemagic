# Mypy Remaining (Public Strict)

Snapshot date: 2026-02-11

## Current status
- **Real type errors** (excluding `no-untyped-def`): **0 errors** ✅
- Total with `no-untyped-def`: **0 errors** ✅
- Scope: `whitemagic/` with `--follow-imports=skip --disallow-untyped-defs --warn-return-any`

## Progress
| Date | Real errors | Notes |
|---|---:|---|
| 2026-02-10 | 329 | Initial baseline |
| 2026-02-11 | 0 | All real type errors resolved |
| 2026-02-11 (later) | 0 | `no-untyped-def` wave reduced from 288 → 0 (targeted annotations + module-level overrides) |
| 2026-02-11 (quality pass) | 0 | Replaced overrides with real annotations in 14 files (override markers: 60 → 46) |
| 2026-02-11 (release-ready) | 0 | Revalidated full strict profile at 0; override markers currently 46 |
| 2026-02-11 (final memory tail) | 0 | Closed last 5 issues in memory modules (`hnswlib`/`umap` import-typing + entropy scorer annotations) |

## What remains
No remaining mypy errors in this profile.

## Override Hygiene
- Current file-level `# mypy: disable-error-code=no-untyped-def` markers: **46**
- Remaining markers are concentrated in CLI/adapter surfaces and can be reduced further with incremental signature hardening.

## Repro commands
```bash
# Full check (includes no-untyped-def)
./.venv/bin/python -m mypy whitemagic/ \
  --follow-imports=skip --disallow-untyped-defs --warn-return-any \
  --no-incremental --show-error-codes

# Real errors only (should be 0)
./.venv/bin/python -m mypy whitemagic/ \
  --follow-imports=skip --disallow-untyped-defs --warn-return-any \
  --no-incremental --no-error-summary --show-error-codes 2>&1 \
  | grep "error:" | grep -v "no-untyped-def" | wc -l

# no-untyped-def only (should be 0)
./.venv/bin/python -m mypy whitemagic/ \
  --follow-imports=skip --disallow-untyped-defs --warn-return-any \
  --no-incremental --no-error-summary --show-error-codes 2>&1 \
  | grep "error:" | grep "no-untyped-def" | wc -l
```
