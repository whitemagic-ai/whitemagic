# WhiteMagic Deployment Guide

> **Version:** 15.0.0 | **Last Updated:** February 2026

---

## Pre-Flight Checklist

```bash
# 1. Run tests
python3 -m pytest tests/ -q --no-header

# 2. Lint + typecheck
ruff check whitemagic/ --select E,F,W --ignore E501
mypy whitemagic/tools/ whitemagic/interfaces/ --ignore-missing-imports

# 3. Ship check (automated hygiene)
WM_SILENT_INIT=1 python3 -c "from whitemagic.tools.unified_api import call_tool; print(call_tool('ship.check'))"

# 4. Eval harness
WM_SILENT_INIT=1 python3 -m eval.run_eval --silent-init
```

---

## Option A: GitHub Release (Recommended)

Tag-driven release via GitHub Actions. Triggers `.github/workflows/release.yml`.

```bash
# 1. Bump version
echo "15.0.0" > VERSION
# Update pyproject.toml version field to match

# 2. Commit and tag
git add -A
git commit -m "release: v15.0.0"
git tag v15.0.0

# 3. Push (triggers CI + release workflow)
git push origin main --tags
```

The release workflow will:
- Build source dist + wheel
- Verify with `twine check`
- Publish to PyPI (if `PYPI_API_TOKEN` secret is set)
- Create a GitHub Release with artifacts
- Build and push Docker image to GHCR

---

## Option B: Manual PyPI Publish

```bash
# 1. Install build tools
pip install build twine

# 2. Build
python -m build

# 3. Verify
twine check dist/*

# 4. Test on TestPyPI first (recommended)
twine upload --repository testpypi dist/*
pip install --index-url https://test.pypi.org/simple/ whitemagic

# 5. Publish to PyPI
twine upload dist/*
```

---

## Option C: Docker

### Build locally

```bash
docker build -t whitemagic:latest .
```

### Run

```bash
# MCP stdio server (PRAT mode, default)
docker run --rm -i whitemagic:latest

# MCP classic mode
docker run --rm -i -e WM_MCP_PRAT=0 whitemagic:latest

# With persistent state
docker run --rm -i -v ~/.whitemagic:/data/whitemagic whitemagic:latest

# CLI
docker run --rm whitemagic:latest wm status
```

### GHCR (after release workflow runs)

```bash
docker pull ghcr.io/whitemagic-ai/whitemagic:latest
docker run --rm -i ghcr.io/whitemagic-ai/whitemagic:latest
```

---

## Option D: Railway (Cloud API)

```bash
npm i -g @railway/cli
railway login
railway init  # > "Empty Project" > "whitemagic-core"
railway up
```

Start command: `uvicorn whitemagic.interfaces.api.app:app --host 0.0.0.0 --port $PORT`

---

## Clean Install Verification

After any release, verify in a fresh environment:

```bash
# Create clean venv
python3 -m venv /tmp/wm_verify && source /tmp/wm_verify/bin/activate

# Install from PyPI (or local wheel)
pip install whitemagic[mcp,cli]

# Verify
WM_SILENT_INIT=1 WM_STATE_ROOT=/tmp/wm_verify_state wm doctor
WM_SILENT_INIT=1 WM_STATE_ROOT=/tmp/wm_verify_state python3 -c "
from whitemagic.tools.unified_api import call_tool
out = call_tool('capabilities', include_tools=False)
assert out['status'] == 'success', f'FAIL: {out}'
print('✅ Clean install verified')
"

# Cleanup
deactivate && rm -rf /tmp/wm_verify /tmp/wm_verify_state
```

---

## Environment Variables

| Variable | Purpose | Default |
|----------|---------|---------|
| `WM_STATE_ROOT` | Runtime state directory | `~/.whitemagic` |
| `WM_DB_PATH` | SQLite database path | `$WM_STATE_ROOT/memory/whitemagic.db` |
| `WM_MCP_PRAT` | Enable PRAT mode | unset |
| `WM_MCP_LITE` | Enable lite mode | unset |
| `WM_SILENT_INIT` | Suppress init logs | unset |
| `PYPI_API_TOKEN` | GitHub secret for PyPI publish | _(set in repo settings)_ |
