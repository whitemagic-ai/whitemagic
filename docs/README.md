# WhiteMagic Documentation Index

> **Canonical system overview**: [`/SYSTEM_MAP.md`](../SYSTEM_MAP.md) · **For AI agents**: [`/AI_PRIMARY.md`](../AI_PRIMARY.md)

## Core Documentation

| Document | Description |
|----------|-------------|
| [QUICKSTART.md](./QUICKSTART.md) | Getting started guide |
| [API_REFERENCE.md](./API_REFERENCE.md) | Core engine API documentation |
| [ARCHITECTURE.md](./ARCHITECTURE.md) | System architecture overview |
| [CONTRIBUTING.md](./CONTRIBUTING.md) | Contribution guidelines |
| [CHANGELOG.md](./CHANGELOG.md) | Version history and release notes |

## MCP & Tools

| Document | Description |
|----------|-------------|
| [MCP_CONFIG_EXAMPLES.md](./MCP_CONFIG_EXAMPLES.md) | Ready-to-use MCP config templates (PRAT/classic/lite) |
| [GALAXY_PER_CLIENT_GUIDE.md](./GALAXY_PER_CLIENT_GUIDE.md) | Multi-galaxy project-scoped databases |

## System Status

| Document | Description |
|----------|-------------|
| [POLYGLOT_STATUS.md](./POLYGLOT_STATUS.md) | Polyglot accelerator status (11 languages) |
| [BENCHMARK_COMPARISON.md](./BENCHMARK_COMPARISON.md) | Performance benchmarks vs comparable tools |
| [VISION.md](./VISION.md) | Long-term vision and philosophy |

## Path Configuration

WhiteMagic uses a configurable path system. See `whitemagic/config/paths.py` for details.

- `WM_STATE_ROOT`: Root directory for state/data (default: `~/.whitemagic`)
- `WM_DB_PATH`: Path to SQLite database
- `WM_MCP_PRAT`: Set to `1` to enable PRAT mode (28 Gana meta-tools)
- `WM_SILENT_INIT`: Set to `1` for quiet initialization

## Building Components

- **Python**: `pip install -e ".[dev]"`
- **Rust**: `cd whitemagic-rust && maturin develop --release --features python`
- **Elixir**: `cd elixir && mix compile`
- **Haskell**: `cd haskell && cabal build`
- **Zig**: `cd whitemagic-zig && zig build`
