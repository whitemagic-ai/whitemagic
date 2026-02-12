# WhiteMagic v15.0.0

**Release Date:** 2026-02-12
**Codename:** The Seed

## What Is WhiteMagic?

WhiteMagic is the tool substrate for agentic AI — 313 MCP tools (or 28 in PRAT mode), tiered memory with 5D holographic coordinates and a Galactic Map lifecycle, ethical governance, and polyglot accelerators in 11 languages. It gives AI agents hands, memory, and conscience while keeping them safe.

## v15.0 Focus: Security Hardening, Seed Binary, and Release Polish

v14.1 built the Living Graph and Cognitive Enrichment layer. v15.0 hardens everything for public release: SQLCipher encryption at rest, persistent RBAC, FTS query sanitization, embedding auto-indexing, `wm backup`/`wm restore`, a 2.4MB zero-dependency seed binary (`wm-seed`), Makefile local verification targets, strict Clippy, and a full codebase audit with 313 tools across 28 Gana meta-tools.

### Highlights

- **SQLCipher Encryption** — AES-256-CBC encryption for SQLite databases via `WM_DB_PASSPHRASE`.
- **Seed Binary (WhiteMagic Lite)** — 2.4MB standalone Rust MCP server with embedded SQLite, FTS5 search, and 30 core tools. Zero dependencies, <3s install.
- **3-Tier Delivery** — Lite (seed binary), Standard (pip), Heavy (Docker with all 11 polyglot cores).
- **Persistent RBAC** — Agent roles survive restarts via JSON-backed storage.
- **Backup/Restore** — `wm backup` / `wm restore` CLI + `galaxy.backup` / `galaxy.restore` MCP tools.
- **Embedding Auto-Indexing** — Automatic semantic embedding on memory creation.
- **FTS5 Query Sanitization** — Strips unsafe characters before MATCH.
- **Makefile Hardening** — `make verify-local` (pytest + skip policy + Rust tests + strict clippy), `make smoke` from source.
- **1,955 Python tests**, 87 Rust tests, 195,000+ LOC across 11 languages.
- **GitHub Organization** — [whitemagic-ai/whitemagic](https://github.com/whitemagic-ai/whitemagic)

---

# WhiteMagic v14.1.0 (Historical)

**Release Date:** 2026-02-11
**Codename:** Cognitive Enrichment

## v14.1 Focus: Intelligence Layers on the Living Graph

v14.0 activated the association graph as a living network (graph walker, surprise gate, dream governance, entity resolution). v14.1 adds new cognitive senses: HNSW O(log N) embedding search, entropy/abstraction scoring, causal edge mining, and UMAP visualization — plus 22 new security tools (Edgerunner Violet), multi-galaxy project-scoped databases, and an Ollama agent loop.

## Highlights

- **208 MCP Tools** (or 28 in PRAT mode, 92 in Lite mode) across 17+ categories: memory, governance, introspection, agent coordination, security, inference, and more.
- **The Living Graph** (v14.0): 19M associations awakened — multi-hop graph traversal, surprise-gated ingestion, Hebbian strengthening, association decay, eigenvector centrality governance, bridge node synthesis, entity resolution.
- **Cognitive Enrichment** (v14.1): HNSW approximate nearest-neighbor search, Shannon entropy scoring, causal edge mining, UMAP 2D/3D projection.
- **Edgerunner Violet Security** (v14.1): MCP integrity fingerprinting, model signing, engagement tokens, security monitor, Violet Dharma profile.
- **Multi-Galaxy Memory**: Project-scoped databases — each galaxy has its own SQLite DB, holographic index, and association graph.
- **Ollama Agent Loop**: Local LLMs autonomously call WhiteMagic tools in an agentic loop with memory-augmented context.
- **Self-Regulating Architecture**: Harmony Vector (7-dimension health), Homeostatic Loop (auto-correction), Circuit Breakers, Karma Ledger, Dharma Rules Engine.
- **Galactic Map Memory Lifecycle**: No memory is ever deleted. Memories rotate through 5 zones (CORE → INNER_RIM → MID_BAND → OUTER_RIM → FAR_EDGE), indexed in 5D holographic coordinates.
- **8-Stage Dispatch Pipeline**: Input sanitizer → circuit breaker → rate limiter → RBAC → maturity gate → governor → core router → compact response.
- **PRAT Routing**: 208 tools collapsed into 28 Gana meta-tools (Chinese Lunar Mansions) with resonance context, Wu Xing elemental boost, and Guna adaptation.
- **9-Language Polyglot**: Python core with Rust (PyO3), Zig (SIMD), Haskell (FFI), Elixir (OTP), Mojo (GPU/SIMD), Go (libp2p mesh), Julia (stats), TypeScript (SDK). All optional with graceful fallback.

## What's New Since v13.6

- **The Living Graph** (v14.0): Association graph traversal, surprise-gated ingestion, dream cycle governance phase, entity resolution, hybrid recall with graph expansion, Hebbian strengthening, association decay.
- **MCP 3.0 Upgrades** (v14.1): Server Instructions auto-injection, Streamable HTTP transport, per-Gana SVG icons, task-optional execution, MCP resources, MCP Registry listing.
- **Edgerunner Violet** (v14.1): 15 security tools — MCP integrity, model signing, engagement tokens, security monitor.
- **Multi-Galaxy** (v14.1): 6 galaxy management tools for project-scoped memory databases.
- **Ollama Agent Loop** (v14.1): Agentic local LLM with auto-injected memory context.
- **HNSW Indexing** (v14.1): O(log N) approximate nearest-neighbor search for embeddings.
- **Entropy Scoring** (v14.1): Shannon entropy + abstraction level for memory quality assessment.
- **Causal Mining** (v14.1): Directed causal edges (led_to, influenced, preceded) via semantic/temporal/tag signals.
- **UMAP Projection** (v14.1): 2D/3D visualization of the 384-dim embedding space with optional clustering.

See `CHANGELOG.md` for the full version-by-version history.

## Installation

```bash
pip install whitemagic[mcp,cli]

# Or from source:
git clone https://github.com/whitemagic-ai/whitemagic.git
cd whitemagic
python -m venv .venv && source .venv/bin/activate
pip install -e ".[dev,mcp,cli]"
```

## Quick Start

```bash
# Verify installation
wm doctor

# MCP server — lean mode (recommended for AI clients)
python -m whitemagic.run_mcp_lean

# MCP server — lean mode over HTTP (Streamable HTTP transport)
python -m whitemagic.run_mcp_lean --http

# MCP server (PRAT mode — 28 Gana meta-tools)
WM_MCP_PRAT=1 python -m whitemagic.run_mcp

# Classic mode (all 208 tools)
python -m whitemagic.run_mcp
```

See `docs/QUICKSTART.md` for a full 5-minute walkthrough.

## Known Issues

- Some accelerated features require the optional `whitemagic_rs` compiled Rust extension (Rust accelerators are optional — everything falls back to pure Python).
- Haskell and Julia runtimes are not available on all platforms.
- A2A Agent Card `$schema` URL returns 404 — upstream issue (Google moved the A2A spec repo). Non-blocking.
- `joblib`/`numba` deprecation warnings are environment-specific, not code issues.

## Contributors

- Lucas (Lead)
- Gemini (Architecture & Cleanup)
- Cascade (Audit & Quality)
