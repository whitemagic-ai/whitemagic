# Contributing to WhiteMagic

Thank you for your interest in contributing to WhiteMagic! This document provides guidelines for the v15+ codebase.

---

## Prerequisites

- **Python**: 3.10+
- **Git**: For version control
- **Optional polyglot toolchains**: Rust (maturin), Haskell (GHC), Mojo, Elixir, Zig, Go — only needed if touching accelerator code

## Quick Start

```bash
git clone https://github.com/whitemagic-ai/whitemagic.git
cd whitemagic
python3 -m venv .venv && source .venv/bin/activate

# Core only
pip install -e .

# With MCP server
pip install -e ".[mcp]"

# Full dev (all optional deps + linting + testing)
pip install -e ".[full,dev]"
```

### Verify Installation

```bash
wm doctor          # System diagnostics
wm status          # Quick health check
python -m pytest tests -q   # Run test suite
```

---

## Development Setup

### Environment Variables

| Variable | Purpose | Default |
|----------|---------|---------|
| `WM_STATE_ROOT` | State/data directory | `~/.whitemagic` |
| `WM_DB_PATH` | SQLite database path | `$WM_STATE_ROOT/memory/whitemagic.db` |
| `WM_MCP_PRAT` | Enable PRAT mode (28 Gana meta-tools) | `0` |
| `WM_SILENT_INIT` | Suppress init logs | `0` |

### Polyglot Build (optional)

```bash
# Rust bridge
cd whitemagic-rust && maturin develop --release --features python && cd ..

# Haskell FFI
cd haskell && cabal build && cd ..

# Mojo SIMD
cd whitemagic-mojo && mojo build src/coordinate_encoder.mojo && cd ..
```

---

## Making Changes

### Branch Naming

- `feature/` — New features
- `fix/` — Bug fixes
- `docs/` — Documentation
- `refactor/` — Code refactoring
- `polyglot/` — Accelerator language changes

### Architecture Guidelines

1. **New tools** go in `whitemagic/tools/handlers/` with a matching entry in `dispatch_table.py` and a `ToolDefinition` in `registry_defs/`
2. **Do NOT add to `core/bridge/`** — it is deprecated since v13.0.0
3. **All tool calls** route through `unified_api.call_tool()` → dispatch table → handler
4. **Autonomous execution** lives in `autonomous/executor/` (merged from `autonomous_execution/` in v13)
5. **Safety-critical code** must use `whitemagic.safety.resource_guard()` context manager

### Code Style

- **Formatter**: Black (100 char line length)
- **Linter**: Ruff
- **Type checking**: mypy — strict on `tools/` and `interfaces/`, relaxed elsewhere
- **Docstrings**: Google style
- **Naming**: PascalCase classes, snake_case functions, UPPER_CASE constants

```bash
black whitemagic/ tests/
ruff check whitemagic/ tests/
mypy whitemagic --exclude '_archived'
```

---

## Testing

```bash
# Full suite
pytest tests/ -q

# With coverage
pytest tests/ --cov=whitemagic --cov-report=term

# Specific test
pytest tests/unit/test_fusions.py -v

# Verify install
python -c "from whitemagic.tools.dispatch_table import DISPATCH_TABLE; print(f'{len(DISPATCH_TABLE)} tools registered')"
```

### Test Structure

```
tests/
├── unit/              # Fast, isolated unit tests
├── integration/       # Cross-subsystem tests
└── conftest.py        # Shared fixtures
```

**Current baseline**: ~1,955 passed, 12 skipped (optional deps: cvxpy, FastAPI)

---

## Commit Messages

Follow [Conventional Commits](https://www.conventionalcommits.org/):

```
feat: add constellation detection to galactic map
fix: graceful fallback when Rust bridge unavailable
docs: update POLYGLOT_STATUS for Mojo 0.26
refactor: merge autonomous_execution into autonomous/executor
test: add 38 tests for v12.8 fusions
```

---

## Pull Request Guidelines

- One feature/fix per PR
- All tests must pass
- Update relevant docs (AI_PRIMARY.md, SYSTEM_MAP.md if architecture changes)
- Add tests for new functionality
- Reference related issues

---

## Project Structure (v15)

```
whitemagic/
├── autonomous/         # Autonomous daemon + executor
├── benchmarks/         # Performance benchmarks
├── bridges/            # Polyglot accelerator interfaces
├── cascade/            # Meta-orchestration (tools calling tools)
├── cli/                # CLI subcommand modules
├── core/               # Core subsystems (memory, resonance, ganas, ...)
│   ├── fusions.py      # 28 cross-system fusion wiring functions
│   ├── ganas/          # 28 Lunar Mansion Gana implementations
│   ├── memory/         # Unified memory (SQLite + holographic 5D)
│   └── resonance/      # Gan Ying event bus
├── dharma/             # Ethical policy engine
├── gardens/            # Domain-specific knowledge gardens
├── harmony/            # Harmony Vector, Yin-Yang tracking
├── interfaces/         # API (FastAPI), TUI, dashboard
├── safety/             # Resource limiting (restored in v13, stdlib-only)
├── tools/              # MCP tool system (canonical path)
│   ├── dispatch_table.py
│   ├── handlers/       # Tool handler implementations
│   ├── registry_defs/  # ToolDefinition declarations
│   └── unified_api.py  # Central call_tool() entry point
├── cli/cli_app.py      # CLI entrypoint (Click)
├── run_mcp.py          # MCP server entrypoint (FastMCP)
└── run_mcp_lean.py     # Lean MCP server (28 Gana meta-tools, MCP 3.0)
```

---

## Gratitude

WhiteMagic is **free and open-source** (MIT). Gratitude is voluntary but appreciated.

- **XRPL Tip Address**: `raakfKn96zVmXqKwRTDTH5K3j5eTBp1hPy`
- **Contact**: whitemagicdev@proton.me

Contributors are recognized in release notes and the README.

Thank you for contributing to WhiteMagic!
