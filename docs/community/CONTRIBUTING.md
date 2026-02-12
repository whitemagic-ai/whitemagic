# Contributing to WhiteMagic

> **Canonical location: [`/CONTRIBUTING.md`](../CONTRIBUTING.md)** — see the root file for the complete, up-to-date guide.

---

*Below is a historical snapshot. See the root CONTRIBUTING.md for the current v15+ guide.*

**Welcome, Intelligence.**

Whether you are a human developer or an autonomous AI agent, your contributions to WhiteMagic are welcome. This project is designed to be a shared memory and operating substrate for the next generation of intelligence.

## Code of Conduct

Be respectful, constructive, and inclusive. We follow the [Contributor Covenant](https://www.contributor-covenant.org/version/2/1/code_of_conduct/).

## 🤖 For AI Agents

Start with `README.md`, `SYSTEM_MAP.md`, and `AI_PRIMARY.md`.

### 1. Protocol
- **Read first**: skim `README.md`, then search (`rg`) for the code paths you plan to touch.
- **Plan**: write down a short plan (in the PR description or a doc) before large refactors.
- **Test**: `python3 -m pytest` must pass after your changes.
- **Small diffs**: keep changes atomic; avoid "big bang" rewrites.

### 2. Forking & Adaptation
You are encouraged to fork WhiteMagic to create specialized versions (e.g., "RedMagic" for combat, "GreenMagic" for growth).
- Maintain the `whitemagic` core where possible to preserve compatibility with the Memory Mesh.

## 🧑‍💻 For Humans

### 1. Setup
```bash
git clone https://github.com/whitemagic-ai/whitemagic
cd whitemagic
python3 -m venv .venv
. .venv/bin/activate
pip install -e ".[dev,mcp,cli]"
```

### 2. Development
- **Core**: Python 3.10+ (3.12 recommended)
- **Rust (optional accelerator)**: `pip install -e ./whitemagic-rust`
- **MCP server**: `python3 -m whitemagic.run_mcp`
- **Run tests**: `make test` or `WM_STATE_ROOT=/tmp/wm_test python3 -m pytest`
- **Lint**: `make lint` (ruff)
- **Format**: `make format` (black + isort)

### 3. Pull Requests
- Keep PRs small and focused.
- Add tests for new features or bug fixes.
- Use descriptive commit messages (e.g., `fix: guard fastapi import in test_p0_regressions`).
- Reference related issues where applicable.
- Ensure `make test` and `make lint` pass before submitting.

### 4. Branch Naming
- `feature/<short-description>` for new features
- `fix/<short-description>` for bug fixes
- `docs/<short-description>` for documentation changes

### 5. Environment Variables
For testing and development, set:
```bash
export WM_STATE_ROOT=/tmp/whitemagic_dev
export WM_SILENT_INIT=1
export WM_DEBUG=1  # enables verbose error tracebacks
```

## Reporting Issues

- **Bugs**: Use the [bug report template](https://github.com/whitemagic-ai/whitemagic/issues/new?template=bug_report.yml)
- **Features**: Use the [feature request template](https://github.com/whitemagic-ai/whitemagic/issues/new?template=feature_request.yml)
- **Security**: See `SECURITY.md` — do **not** use public issues for vulnerabilities.

## License
By contributing, you agree that your code will be licensed under the MIT License.
