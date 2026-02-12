# WhiteMagic v15.1.0 System Map

This file is the **canonical repo map** for humans and AIs.

Goals:
- Be **MCP-first** (external models call WhiteMagic tools/memory).
- Keep **runtime data out of git** (memories, conversations, logs, caches).
- Keep install/run steps **true** for a fresh clone.

---

## Quick Start

```bash
# Install from PyPI
pip install whitemagic[mcp]

# Or install from source
python -m venv .venv && . .venv/bin/activate && pip install -e ".[dev,mcp,cli]"

# CLI
wm status
wm remember "Hello world" --title "Test" --tags smoke --type short_term
wm recall "hello" --limit 5

# MCP server — lean mode (28 Gana meta-tools, recommended for new clients)
python -m whitemagic.run_mcp_lean

# MCP server — lean mode over HTTP (Streamable HTTP transport)
python -m whitemagic.run_mcp_lean --http

# MCP server — PRAT mode (28 Gana meta-tools via classic server)
WM_MCP_PRAT=1 python -m whitemagic.run_mcp

# MCP server — classic mode (313 individual tools)
python -m whitemagic.run_mcp

# MCP server — lite mode (92 core tools)
WM_MCP_LITE=1 python -m whitemagic.run_mcp
```

---

## Runtime State

WhiteMagic creates runtime state at startup (DB, caches, logs).

State root resolution order:
1. `$WM_STATE_ROOT` (recommended for tests/containers)
2. `$WM_CONFIG_ROOT`
3. `~/.whitemagic`
4. Fallback: `/tmp/whitemagic_state` (if the default isn't writable)
5. Last resort: `./.whitemagic` (extremely restricted environments)

Key state directories:
- `memory/` — SQLite databases (main + per-galaxy). Optional SQLCipher AES-256 encryption via `WM_DB_PASSPHRASE`.
- `karma/` — Append-only Karma Ledger (JSONL) with SHA-256 hash chain + Merkle roots.
- `vault/` — AES-256-GCM encrypted secrets vault (API keys, tokens).
- `sessions/` — Session checkpoints and scratchpads.
- `audit/` — Audit logs with Merkle root anchoring.

Vault CLI:
- `wm vault status` — Show encryption status of all data stores.
- `wm vault encrypt-db [NAME]` — Encrypt a database with SQLCipher.
- `wm vault decrypt-db [NAME]` — Decrypt back to plaintext.
- `wm vault lock` — Clear cached passphrase from OS keychain.

**Nothing under runtime state should ever be committed.**

---

## Repo Layout

```
whitemagic/                    # Main Python package (~170K LOC, 313 MCP tools)
├── cli/                       # CLI subcommands (Click)
│   └── cli_app.py             # `wm` entrypoint
├── core/
│   ├── memory/                # Memory substrate (SQLite, FTS5, holographic 5D, galactic map)
│   ├── ganas/                 # 28 Gana architecture (4 quadrant files + chain + base)
│   └── resonance/             # Gan Ying event bus
├── dharma/                    # Ethical policy engine (YAML rules, 3 profiles)
├── harmony/                   # Harmony Vector (7D health), Yin-Yang tracking
├── security/                  # Vault (AES-256-GCM), Edgerunner Violet, MCP integrity
├── tools/                     # Canonical tool system
│   ├── registry.py            # Tool registry
│   ├── unified_api.py         # call_tool() entry point
│   ├── prat_router.py         # PRAT: 313 → 28 Gana meta-tools
│   ├── middleware.py          # 8-stage dispatch pipeline
│   ├── handlers/              # Tool handler implementations
│   └── registry_defs/         # ToolDefinition declarations
├── bridges/                   # Polyglot FFI bridges (Rust, Zig, Haskell, etc.)
├── interfaces/                # REST API (FastAPI), TUI, dashboard
├── run_mcp.py                 # Classic MCP server (FastMCP stdio)
├── run_mcp_lean.py            # Lean MCP server (28 Ganas, MCP 3.0, HTTP option)
└── grimoire/                  # 7-chapter Grimoire (AI orientation spellbook)

tests/                         # Unit + integration tests (~1,955 passing)
docs/                          # Documentation
├── guides/                    # Quickstart, MCP config, encryption, galaxy guide
├── reference/                 # API reference, architecture, polyglot status
├── design/                    # Benchmarks, use cases, SDK design, WASM strategy
└── community/                 # Contributing, changelog

whitemagic-rust/               # Rust PyO3 (~12K LOC) — galactic scoring, 5D KD-tree, SIMD search, Arrow IPC
haskell/                       # Haskell FFI (~2.8K LOC) — algebraic Dharma rules, dependency planner
elixir/                        # Elixir OTP (~2.6K LOC) — Gan Ying event bus, dream scheduler
whitemagic-go/ + mesh/         # Go (~1.3K LOC) — bridge + libp2p mesh (TCP+QUIC+WS, PSK auth, NAT traversal)
whitemagic-zig/                # Zig (~2.2K LOC) — SIMD cosine similarity, holographic projection
whitemagic-mojo/               # Mojo (~1.9K LOC) — batch encoding, neuro scoring
whitemagic-julia/              # Julia (~890 LOC) — memory stats, time-series forecasting
nexus/                         # TypeScript frontend + SDK (~4.6K LOC)
sdk/                           # SDK package
examples/                      # Usage examples
scripts/                       # Build, deploy, and utility scripts
```

**Local-only (gitignored; never shipped):**
- `memory/`, `data/`, `logs/`, `reports/`, `tmp/`
- `.whitemagic/`, `.venv/`, `temp_venv/`
- `*.db`, `*.db-journal`, `*.db-shm`, `*.db-wal`

---

## Key Subsystems

| Subsystem | Entry Point | Description |
|-----------|-------------|-------------|
| **Memory Substrate** | `core/memory/` | SQLite + FTS5 + 5D holographic coordinates (XYZWV). No memory is ever deleted — rotated outward through 5 galactic zones. |
| **Galactic Map** | `core/memory/galactic_map.py` | 5-zone lifecycle: CORE → INNER_RIM → MID_BAND → OUTER_RIM → FAR_EDGE. Zone placement by 7-signal retention scoring. |
| **Living Graph** | `core/memory/association_*.py` | Weighted association graph. Multi-hop traversal, surprise gate, Hebbian strengthening, entity resolution. |
| **PRAT Router** | `tools/prat_router.py` | 313 tools → 28 Gana meta-tools based on Chinese Lunar Mansions (Xiu 宿). Resonance context per call. |
| **Dharma Rules** | `dharma/` | YAML-driven ethical governance with graduated actions (LOG → WARN → THROTTLE → BLOCK). 3 profiles: default, creative, secure. |
| **Karma Ledger** | `tools/karma_*.py` | Append-only audit trail. Tracks declared vs actual side-effects. Merkle hash chain for tamper detection. |
| **Harmony Vector** | `harmony/` | 7-dimension real-time health: balance, throughput, latency, error_rate, dharma, karma_debt, energy. Guna classification (Sattva/Rajas/Tamas). |
| **Homeostatic Loop** | `core/homeostasis.py` | Self-regulation: watches Harmony Vector, applies corrective actions when dimensions drift. |
| **Dream Cycle** | `core/dream_cycle.py` | 7-phase background processing: Consolidation → Narrative → Serendipity → Governance → Kaizen → Oracle → Decay. |
| **Circuit Breaker** | `tools/circuit_breaker.py` | Stoic resilience per tool: CLOSED → OPEN → HALF_OPEN. Configurable thresholds. |
| **28 Fusions** | `core/fusions.py` | Cross-system synthesis wiring matching the 28 Ganas. 0 unexplored. |
| **Encryption at Rest** | `core/memory/encrypted_db.py` | SQLCipher (AES-256-CBC) for all DBs. OS keychain integration. Passphrase strength validation. Migration tools. |
| **Edgerunner Violet** | `security/` | MCP integrity fingerprinting, model signing, engagement tokens, security monitor. |
| **P2P Mesh** | `mesh/` | libp2p gossip protocol (TCP+QUIC+WebSocket). PSK peer auth. NAT traversal (UPnP, relay, hole-punch). Agent coordination + distributed locks. |

---

## MCP Server Modes

| Mode | Entrypoint | Tools | Best For |
|------|-----------|-------|----------|
| **Lean** | `run_mcp_lean.py` | 28 Gana meta-tools | New clients (recommended) |
| **Lean HTTP** | `run_mcp_lean.py --http` | 28 Gana meta-tools | Streamable HTTP transport |
| **PRAT** | `WM_MCP_PRAT=1 run_mcp.py` | 28 Gana meta-tools | AI agents via stdio |
| **Classic** | `run_mcp.py` | 313 individual tools | Power users |
| **Lite** | `WM_MCP_LITE=1 run_mcp.py` | 92 core tools | Fast startup |

---

## Public Interfaces

**Primary (stable target):**
- MCP tools: served by `whitemagic/run_mcp.py` or `whitemagic/run_mcp_lean.py`
- Tool registry: `whitemagic/tools/registry.py`
- Unified API: `whitemagic/tools/unified_api.py` → `call_tool(name, **kwargs)`

**Secondary (developer UX):**
- CLI: `wm` → `whitemagic/cli/cli_app.py`
- REST API: `whitemagic/interfaces/api/` (optional; not required for MCP)

**Discovery files:**
- `AI_PRIMARY.md` — Tool contract for AI agents
- `ABOUT.md` — Project description and history
- `skill.md` — OpenClaw/MCP skill file
- `llms.txt` — LLM-readable project summary
- `.well-known/agent.json` — A2A Agent Card

---

## Dispatch Pipeline

```
Extract _agent_id / _compact
  → Input Sanitizer (validate args, strip injection)
  → Circuit Breaker (fast-fail on cooldown)
  → Rate Limiter (per-agent/tool, trust-adjusted)
  → Tool Permissions (RBAC: admin/coordinator/agent/observer/restricted)
  → Maturity Gate (SEED → BICAMERAL → REFLECTIVE → RADIANT → COLLECTIVE)
  → Governor (ethical validation via Dharma rules)
  → Core Router (Gana prefix → dispatch table → bridge fallback)
  → Breaker Feedback (success/failure updates breaker state)
  → Compact Response (optional token-efficient output)
```

---

## Key Environment Variables

| Variable | Purpose | Default |
|----------|---------|---------|
| `WM_STATE_ROOT` | Runtime state directory | `~/.whitemagic` |
| `WM_DB_PATH` | Override SQLite database path | `$WM_STATE_ROOT/memory/whitemagic.db` |
| `WM_DB_PASSPHRASE` | Enable SQLCipher encryption for all memory DBs | unset |
| `WM_MESH_PSK` | Pre-shared key for private mesh network (min 32 chars) | unset |
| `WM_TERMINOLOGY` | Terminology mode: `esoteric` (default) or `standard` (Rosetta mode) | `esoteric` |
| `WM_MCP_PRAT` | Enable 28-tool PRAT mode | unset |
| `WM_MCP_LITE` | Enable 92-tool lite mode | unset |
| `WM_MCP_CLIENT` | Schema adaptation (gemini/deepseek/qwen) | unset |
| `WM_SILENT_INIT` | Suppress startup messages | unset |
| `OLLAMA_HOST` | Ollama server for local inference | `localhost:11434` |
| `REDIS_URL` | Redis for Gan Ying events / mesh | `redis://localhost:6379` |

---

## Local Models (Status)

Embedded local-model inference is **archived/disabled by default**.
Legacy code paths require `WHITEMAGIC_ENABLE_LOCAL_MODELS=1`.
For local LLM integration, use Ollama via the `gana_roof` tools.

---

## Rosetta Mode (Standard Terminology)

Set `WM_TERMINOLOGY=standard` to alias esoteric names in CLI output and logs:

| Esoteric | Standard |
|----------|----------|
| Gana | Tool Group |
| PRAT Router | Meta-Tool Router |
| Dharma Rules | Ethics Policy |
| Karma Ledger | Side-Effect Audit |
| Harmony Vector | Health Metrics |
| Dream Cycle | Memory Consolidation |
| Galactic Map | Memory Lifecycle |
| Gan Ying | Event Bus |
| Sangha | Agent Swarm |
| Grimoire | AI Orientation Guide |
