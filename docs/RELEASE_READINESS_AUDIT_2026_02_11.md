# Release Readiness Audit (2026-02-11)

## Snapshot
- Strict mypy profile (`whitemagic/`, `--follow-imports=skip --disallow-untyped-defs --warn-return-any`): **0 errors**.
- Changed memory-focused tests pass:
  - `tests/unit/test_causal_miner.py`
  - `tests/unit/test_entropy_scorer.py`
  - `tests/unit/test_umap_projection.py`
  - Result: **46 passed**, **2 warnings**, **0 failed**.
- Full suite gate:
  - `pytest tests/ -q --tb=short`
  - Result: **1656 passed**, **2 warnings**, **0 failed** (303.06s).

## What Is Still Left Before Public Release
1. **Version metadata sync**
   - `VERSION`, `pyproject.toml`, and `whitemagic-rust/Cargo.toml` are `14.1.0`.
   - `.well-known/agent.json` is still `13.6.0` and should be updated.

2. **Packaging dry run in release-like environment**
   - `python -m build --sdist --wheel` failed in this environment due network-restricted dependency install (`wheel` unavailable in isolated build env).
   - `python -m build --sdist --wheel --no-isolation` still fails here because local env lacks `wheel`.
   - Action: run build + TestPyPI upload in a network-enabled CI/release runner.

3. **Warnings to triage (non-blocking but recommended)**
   - UMAP tests emit sandbox-related multiprocessing warnings (`joblib`/`numba` lock warnings from restricted `/dev/shm`).
   - Setuptools emits deprecation warnings around license metadata/classifier style in `pyproject.toml`.

4. **Final pre-release gate**
   - Full test suite is green in this environment.
   - Remaining gate is TestPyPI upload + smoke install in network-enabled CI/release runner.

## Files Updated in This Final Tail
- `whitemagic/core/memory/embeddings.py`
- `whitemagic/core/memory/entropy_scorer.py`
- `whitemagic/core/memory/umap_projection.py`
- `docs/MYPY_REMAINING.md`
