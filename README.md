# Whitemagic v15.1.0

[![CI](https://github.com/whitemagic-ai/whitemagic/actions/workflows/ci.yml/badge.svg)](https://github.com/whitemagic-ai/whitemagic/actions/workflows/ci.yml)
[![PyPI](https://img.shields.io/pypi/v/whitemagic)](https://pypi.org/project/whitemagic/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**The Tool Substrate for Agentic AI.**

Whitemagic is a high-performance memory and tool infrastructure designed for AI agents. It provides 313 MCP tools (or 28 in PRAT mode), tiered memory with 5D holographic coordinates and a Galactic Map lifecycle, ethical governance, multi-galaxy project-scoped databases, Edgerunner Violet security layer, local AI agent loops (Ollama), and polyglot accelerators in 11 languages.

If you're integrating Whitemagic into an agent runtime, start with `AI_PRIMARY.md`.

## Core Features

1.  **Memory Substrate**: Tiered memory management with native SQLite backing, 5D holographic coordinates (XYZWV), a Galactic Map lifecycle (no memory is ever deleted — only rotated outward), constellation detection, and association mining.
2.  **313 MCP Tools** exposed via a unified interface (`whitemagic.tools.unified_api`) with a composable middleware pipeline (input sanitizer → circuit breaker → rate limiter → RBAC → maturity gate → governor → core router → compact response).
3.  **PRAT Mode (28 Gana Meta-Tools)**: The Polymorphic Resonant Adaptive Tools router maps all 313 tools into 28 Ganas — consciousness lenses based on the Chinese Lunar Mansions. Every call carries resonance context (predecessor output, lunar phase, Harmony Vector, Wu Xing boost, Guna adaptation). Enable with `WM_MCP_PRAT=1`.
4.  **Capability Matrix**: 25 subsystems, 28 active cross-system fusions, 0 unexplored — all queryable via `capability.matrix` MCP tool.
5.  **MCP Native**: Built from the ground up to be served via the Model Context Protocol (MCP).
6.  **Self-Regulating**: Harmony Vector (7-dimension health), Dharma Rules Engine (ethical governance with YAML hot-reload), Homeostatic Loop (auto-correction), Karma Ledger (side-effect auditing), Circuit Breakers, Maturity Gates, Agent Trust scoring.
7.  **9-Language Polyglot**: Python core with compiled accelerators in Rust (PyO3), Zig (SIMD), Haskell (FFI), Elixir (OTP), Mojo (GPU/SIMD), Go (libp2p mesh), Julia (statistical forecasting), and TypeScript (SDK). Every module has a Python bridge with graceful fallback.

## Installation

### From Source (Recommended For This Repo)

> **Note**: On modern Linux systems (PEP 668), use a virtual environment to avoid `externally-managed-environment` errors.

```bash
# A pre-configured venv exists at .venv/ with all deps + Mojo 0.26.1
source .venv/bin/activate

# Or create a fresh venv:
python -m venv .venv
. .venv/bin/activate
pip install -e ".[dev,mcp,cli]"

# One-shot "everything" install:
pip install -e ".[full]"
```

### Optional: Polyglot Accelerators
```bash
# Rust (PyO3 — recommended, biggest performance gains)
cd whitemagic-rust && maturin develop --release

# Haskell (algebraic Dharma rules, dependency graph planner)
cd haskell && cabal build

# Elixir (OTP actor-model event bus, dream scheduler)
cd elixir && mix compile --force

# Go (2 modules: whitemagic-go + mesh/libp2p)
cd whitemagic-go && go build ./...
cd mesh && go build ./...

# Zig (SIMD cosine similarity, holographic projection)
cd whitemagic-zig && zig build

# Mojo (batch encoding, neuro scoring — requires Mojo 0.26+)
cd whitemagic-mojo && mojo build src/satkona_yang.mojo
```

## Quick Start

### State Root (Important)
Whitemagic writes runtime state (DB, logs, artifacts) under `WM_STATE_ROOT` (default `~/.whitemagic`).
For demos/tests/containers, set it explicitly:
```bash
export WM_STATE_ROOT=/tmp/whitemagic_state
```
The SQLite DB path defaults to `$WM_STATE_ROOT/memory/whitemagic.db`. To override it:
```bash
export WM_DB_PATH=/tmp/whitemagic_state/memory/whitemagic.db
```

### CLI
The `wm` command line tool provides immediate access to the substrate:
```bash
wm status
wm remember "We chose SQLite for Phase 1." --title "Architectural Decision" --tags architecture,v11 --type short_term
wm recall "architecture" --limit 5
```

### MCP Server (stdio)
```bash
# PRAT mode — 28 Gana meta-tools (recommended for AI clients)
WM_MCP_PRAT=1 python -m whitemagic.run_mcp

# Lean mode — 28 Gana meta-tools with MCP 3.0 features (icons, instructions, tasks)
python -m whitemagic.run_mcp_lean

# Lean mode over HTTP (Streamable HTTP transport, port 8770)
python -m whitemagic.run_mcp_lean --http

# Classic mode — all 313 individual tools
python -m whitemagic.run_mcp

# Lite mode — 92 core tools
WM_MCP_LITE=1 python -m whitemagic.run_mcp
```

If your MCP client supports per-repo server configs, see `.mcp.json.example`.
Additional client examples: `docs/guides/MCP_CONFIG_EXAMPLES.md`.

#### MCP Quickstart for AI Clients

1. **Install**: `pip install whitemagic[mcp]` (or `pip install -e ".[mcp]"` from source)
2. **Add to MCP config** (e.g., `.mcp.json`, Claude Desktop config, or Windsurf settings):
   ```json
   {"mcpServers": {"whitemagic": {"command": "python", "args": ["-m", "whitemagic.run_mcp_lean"]}}}
   ```
3. **On first connect**: Search for `quickstart guide` — WhiteMagic ships with 12 built-in guide memories covering the full system.
4. **Orientation resources** (MCP resources readable by clients):
   - `whitemagic://orientation/ai-primary` — Full technical contract
   - `whitemagic://orientation/server-instructions` — Tool usage guide
   - `whitemagic://orientation/system-map` — Repo structure

### Python API
```python
from whitemagic.tools.unified_api import call_tool

# Store
out = call_tool(
    "create_memory",
    title="Architectural Decision",
    content="We chose SQLite for Phase 1.",
    tags=["architecture", "v11"],
    type="short_term",
)
assert out["status"] == "success"

# Recall
out = call_tool("search_memories", query="architecture", limit=5)
assert out["status"] == "success"
print(out["details"]["results"][0]["entry"]["content"])

# Introspection (Gnosis portal — unified health snapshot)
out = call_tool("gnosis", compact=True)
```

## Architecture

*   `whitemagic/`: Core Python package (~170K LOC, 313 MCP tools across 30+ domain files).
*   `whitemagic/tools/prat_router.py`: PRAT router — maps 313 tools → 28 Gana meta-tools.
*   `whitemagic/tools/`: Canonical tool registry, dispatch pipeline, and handler modules.
*   `whitemagic/core/ganas/`: 28 Gana architecture (4 quadrant files + chain + base + karma + lunar + swarm).
*   `whitemagic/core/memory/`: Memory substrate — embeddings (HNSW), entropy scoring, causal mining, UMAP projection, graph walker, surprise gate, lifecycle, galactic map.
*   `whitemagic-rust/`: Rust accelerator (~8.5K LOC) — galactic batch scoring, association mining, 5D KD-tree, SIMD search.
*   `haskell/`: Haskell accelerator (~1.7K LOC) — algebraic Dharma rules, dependency graph planner, FFI bridge.
*   `elixir/`: Elixir OTP (~1.4K LOC) — actor-model Gan Ying event bus, dream scheduler, supervision trees.
*   `whitemagic-go/` + `mesh/`: Go (~1.3K LOC) — libp2p mesh (TCP+QUIC+WebSocket), PSK peer auth, NAT traversal, gossip protocol, federated galaxies.
*   `whitemagic-zig/`: Zig (~795 LOC) — SIMD cosine similarity, holographic projection, memory management.
*   `whitemagic-mojo/`: Mojo (~1.2K LOC) — batch coordinate encoding, neuro scoring, satkona yang.

> **Note:** Local inference (Ollama/LlamaCPP) has been decoupled from the core loop in v11. Agents should bring their own "Brain" (via MCP or direct API calls) and use Whitemagic purely as their *Hands and Memory*.
>
> Legacy embedded/local-model code is preserved under `whitemagic/_archived/` and remains disabled by default.

## Key Environment Variables

| Variable | Purpose | Default |
|----------|---------|---------|
| `WM_STATE_ROOT` | Runtime state directory | `~/.whitemagic` |
| `WM_DB_PASSPHRASE` | Enable SQLCipher AES-256 encryption for all memory DBs | unset |
| `WM_MESH_PSK` | Pre-shared key for private mesh network (min 32 chars) | unset |
| `WM_TERMINOLOGY` | `esoteric` (default) or `standard` (Rosetta mode for enterprise) | `esoteric` |
| `WM_MCP_PRAT` | Enable 28-tool PRAT mode | unset |
| `WM_MCP_LITE` | Enable 92-tool lite mode | unset |
| `WM_MCP_CLIENT` | Schema adaptation (gemini/deepseek/qwen) | unset |
| `OLLAMA_HOST` | Ollama server for local inference | `localhost:11434` |
| `REDIS_URL` | Redis for Gan Ying events / mesh | `redis://localhost:6379` |

## Tests
```bash
python -m pytest tests/unit/ -q --no-header
```

## Contributing
See `docs/community/CONTRIBUTING.md`.

## Gratitude

WhiteMagic is **free and open-source** under the MIT license. It will always be free. If WhiteMagic has been useful to you, gratitude is welcome but never expected.

- **XRPL Tip Address**: `raakfKn96zVmXqKwRTDTH5K3j5eTBp1hPy`
- **Tip via tool**: `call_tool("whitemagic.tip", amount=1.0, currency="XRP")`

## Contact & Support
- **Email**: whitemagicdev@proton.me
- **GitHub**: [whitemagic-ai/whitemagic](https://github.com/whitemagic-ai/whitemagic)
- **License**: MIT
