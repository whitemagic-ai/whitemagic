# WhiteMagic Documentation

> **For AI agents**: [`/AI_PRIMARY.md`](../AI_PRIMARY.md) · **Quick install**: `pip install whitemagic[mcp]`

---

## Guides

| Document | Description |
|----------|-------------|
| [Quickstart](./guides/QUICKSTART.md) | Getting started in under 5 minutes |
| [MCP Config Examples](./guides/MCP_CONFIG_EXAMPLES.md) | Ready-to-use MCP config templates (PRAT/classic/lite) |
| [Galaxy Per-Client Guide](./guides/GALAXY_PER_CLIENT_GUIDE.md) | Multi-galaxy project-scoped databases |
| [Lite vs Heavy](./guides/LITE_VS_HEAVY.md) | Choosing the right deployment tier |
| [Encryption at Rest](./guides/ENCRYPTION_AT_REST.md) | SQLCipher setup and key management |

## Reference

| Document | Description |
|----------|-------------|
| [API Reference](./reference/API_REFERENCE.md) | Core engine API documentation |
| [Architecture](./reference/ARCHITECTURE.md) | System architecture overview |
| [Polyglot Status](./reference/POLYGLOT_STATUS.md) | Accelerator status across 11 languages |

## Design & Vision

| Document | Description |
|----------|-------------|
| [Benchmarks](./design/BENCHMARK_COMPARISON.md) | Performance vs comparable tools |
| [Use Cases](./design/USE_CASES.md) | Real-world usage patterns |
| [TypeScript SDK Design](./design/TYPESCRIPT_SDK_DESIGN.md) | `@whitemagic/sdk` architecture |
| [WASM Strategy](./design/WASM_STRATEGY.md) | WebAssembly deployment path |

## Community

| Document | Description |
|----------|-------------|
| [Contributing](./community/CONTRIBUTING.md) | Contribution guidelines |
| [Changelog](./community/CHANGELOG.md) | Detailed version history |

---

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
