# Whitemagic v13.6.0

**Release Date:** 2026-02-10
**Codename:** Cold Storage Awakening

## What Is WhiteMagic?

WhiteMagic is the tool substrate for agentic AI — 181 MCP tools, tiered memory with 5D holographic coordinates, ethical governance, and polyglot accelerators in 9 languages. It gives AI agents hands and memory while keeping them safe.

## v13.6 Focus: Full Corpus Searchable

The entire 110K memory corpus is now semantically searchable. Hot DB memories are searched immediately; cold storage (105K+ memories) is searched via cross-DB semantic fallback. Development history archives have been ingested as proper memories.

## Highlights

- **181 MCP Tools** (or 28 in PRAT mode, 92 in Lite mode) across 17 categories: memory, governance, introspection, agent coordination, inference, and more.
- **Semantic Memory Revolution** (v13.5): Embedding-based association mining, hybrid BM25 + embedding retrieval via Reciprocal Rank Fusion, near-duplicate detection and archival.
- **Cold Storage Awakening** (v13.6): 105K cold memories indexed with MiniLM-L6-v2 embeddings. Cross-DB search (`search_similar(include_cold=True)`). Archive ingestion of 15 development history documents.
- **Self-Regulating Architecture**: Harmony Vector (7-dimension health), Homeostatic Loop (auto-correction), Circuit Breakers, Karma Ledger, Dharma Rules Engine.
- **Galactic Map Memory Lifecycle**: No memory is ever deleted. Memories rotate through 5 zones (CORE → INNER_RIM → MID_BAND → OUTER_RIM → FAR_EDGE), indexed in 5D holographic coordinates.
- **7-Stage Dispatch Pipeline**: Input sanitizer → circuit breaker → rate limiter → RBAC → maturity gate → governor → core router → compact response.
- **PRAT Routing**: 181 tools collapsed into 28 Gana meta-tools (Chinese Lunar Mansions) with resonance context, Wu Xing elemental boost, and Guna adaptation.
- **9-Language Polyglot**: Python core with Rust (PyO3), Zig (SIMD), Haskell (FFI), Elixir (OTP), Mojo (GPU/SIMD), Go (libp2p mesh), Julia (stats), TypeScript (SDK). All optional with graceful fallback.
- **Security Hardened**: Input sanitization, per-agent RBAC, rate limiting with Rust atomic pre-check, circuit breakers, karma ledger side-effect auditing, Dharma rules ethical governance.

## What's New Since v12.8

- **Polyglot Expansion Closeout** (v13.2): 9-language stack complete. All bridges documented with benchmarks.
- **Database Unification** (v13.3): Primary + Legacy Galaxy merged into single 110K-memory DB. Intelligence shim tree removed. Python distilled from 813 → 762 files.
- **Semantic Embedding Layer** (v13.4): 5,547 memories encoded with MiniLM-L6-v2 (384 dims). 19 constellations detected. Tag and importance data quality overhaul.
- **Semantic Memory Revolution** (v13.5): Embedding-based associations replace keyword Jaccard. Hybrid BM25 + embedding retrieval. Near-duplicate detection.
- **Cold Storage Awakening** (v13.6): Full 110K corpus semantically searchable. Archive ingestion. Content mining and data quality fixes.

See `CHANGELOG.md` for the full version-by-version history.

## Installation

```bash
pip install whitemagic[mcp,cli]

# Or from source:
git clone https://github.com/lbailey94/whitemagic.git
cd whitemagic
python -m venv .venv && source .venv/bin/activate
pip install -e ".[dev,mcp,cli]"
```

## Quick Start

```bash
# Verify installation
wm doctor

# MCP server (PRAT mode — recommended)
WM_MCP_PRAT=1 python -m whitemagic.run_mcp

# Classic mode (all 181 tools)
python -m whitemagic.run_mcp
```

See `docs/QUICKSTART.md` for a full 5-minute walkthrough.

## Known Issues

- Some accelerated features require the optional `whitemagic_rs` compiled Rust extension (Rust accelerators are optional — everything falls back to pure Python).
- Haskell and Julia runtimes are not available on all platforms.
- Mypy strict mode has known warnings on internal modules (public API surface is the priority).

## Contributors

- Lucas (Lead)
- Gemini (Architecture & Cleanup)
- Cascade (Audit & Quality)
