# AI Primary Spec — Whitemagic v15.7.0

**The tool contract for AI agents using Whitemagic.**

Whitemagic is a memory and tool substrate for AI agents. It provides 356 MCP tools (28 in PRAT mode), tiered persistent memory, ethical governance, and polyglot accelerators. Agents bring reasoning; Whitemagic provides Hands, Memory, and Conscience.

- **Hands** — 356 MCP tools for acting on the world
- **Memory** — Tiered, persistent, semantic, no-delete galactic lifecycle
- **Conscience** — Dharma rules, Karma ledger, Harmony Vector, Homeostatic Loop

For strategic context (ecosystem landscape, economic model, security posture), see `docs/STRATEGY.md`.

---

## Contract

Goals:
- Make Whitemagic easy to use for **non-human users** (LLMs, agent runtimes, eval harnesses).
- Keep the interface **stable, machine-readable, and replayable**.
- Ensure open-source clones are **safe by default**: no personal data is required, and no runtime state is written into the repo.

Non-goals:
- Whitemagic does **not** embed or run an LLM by default (local inference is archived/disabled).
- Whitemagic does **not** treat any frontend as canonical; the **tool contract** is canonical.

## Bootstrap (Quickstart)

```bash
# Activate the pre-configured venv (contains all Python deps + Mojo 0.26.1)
cd "$(git rev-parse --show-toplevel)"
source .venv/bin/activate

# Run MCP server in PRAT mode (recommended — 28 Gana meta-tools)
WM_MCP_PRAT=1 python -m whitemagic.run_mcp

# Or lean mode (28 Gana meta-tools, recommended for new clients)
python -m whitemagic.run_mcp_lean

# Or classic mode (all 356 individual tools)
python -m whitemagic.run_mcp

# Or lite mode (92 core tools)
WM_MCP_LITE=1 python -m whitemagic.run_mcp
```

### Key Environment Variables

| Variable | Purpose | Example |
|----------|---------|--------|
| `WM_STATE_ROOT` | Runtime state directory | `~/.whitemagic` |
| `WM_MCP_PRAT` | Enable 28-tool PRAT mode | `1` |
| `WM_MCP_LITE` | Enable 92-tool lite mode | `1` |
| `WM_MCP_CLIENT` | Schema adaptation per client | `gemini`, `deepseek`, `qwen` |
| `OLLAMA_HOST` | Ollama server for inference | `localhost:11434` |
| `REDIS_URL` | Redis for Gan Ying events / mesh | `redis://localhost:6379` |

### Polyglot Accelerators (Optional)

All 10 compiled languages build clean. Rust is recommended for the biggest performance gains:

```bash
cd whitemagic-rust && maturin develop --release  # Rust (PyO3) — galactic scoring, association mining, 5D KD-tree
cd haskell && cabal build                         # Haskell (FFI) — algebraic Dharma rules, dep graph planner
cd elixir && mix compile --force                  # Elixir (OTP) — actor-model event bus, dream scheduler
cd whitemagic-go && go build ./...                # Go — general bridge
cd mesh && go build ./...                         # Go mesh — libp2p P2P, mDNS, protobuf
cd whitemagic-zig && zig build                    # Zig — SIMD cosine, holographic projection
cd whitemagic-mojo && mojo build src/satkona_yang.mojo  # Mojo 0.26+ — batch encoding
```

## Canonical Interfaces

Primary:
- **MCP server (stdio):** `WM_MCP_PRAT=1 python -m whitemagic.run_mcp` (28 Gana meta-tools)
- **MCP server (classic):** `python -m whitemagic.run_mcp` (356 individual tools)

Secondary:
- **In-process Python:** `from whitemagic.tools.unified_api import call_tool`
- **CLI adapter:** `wm …` (wraps `call_tool`, supports `--json`)
- **Nexus API:** `python -m whitemagic.interfaces.nexus_api --port 8765` (REST + WebSocket)

If you are an AI runtime integrating Whitemagic, prefer **MCP with PRAT mode** (portable, 28 tools) or **call_tool** (in-process).

### MCP Orientation Resources

On first connection, read these resources before tool calls:
- `whitemagic://orientation/ai-primary`
- `whitemagic://grimoire/index`

## PRAT Mode — 28 Gana Meta-Tools

The Polymorphic Resonant Adaptive Tools (PRAT) router maps all 356 tools into 28 Ganas — consciousness lenses based on the Chinese Lunar Mansions (Xiu 宿). Each Gana:

1. **Has a domain** — a specific operational concern (session, memory, health, ethics, etc.)
2. **Supports 4 polymorphic operations** — search / analyze / transform / consolidate
3. **Lists nested sub-tools** — AI clients can call `gana_ghost(tool="gnosis")` to route to a specific tool
4. **Resonates with neighbors** — predecessor output feeds into current; current seeds successor

Files: `whitemagic/tools/prat_router.py`, `whitemagic/tools/prat_resonance.py`, `whitemagic/core/ganas/` (base, chain, karma, lunar, swarm, 4 quadrant files).

Wrong-Gana calls return helpful redirect hints. Enable with `WM_MCP_PRAT=1`.

Every PRAT call now carries **resonance context**: predecessor Gana output, lunar phase + alignment check, Harmony Vector + Guna mode hint, Wu Xing quadrant boost, and successor preparation. This metadata is injected as `_resonance` in every response.

### Capability Matrix

Call `capability.matrix` (or `gana_ghost(tool="capability.matrix")` in PRAT mode) to get a full inventory of all 25 subsystems, 28 active cross-system fusions, and 0 unexplored synthesis opportunities. Also available: `capability.status` for live subsystem probes and `capability.suggest` for next-fusion recommendations.

## State Root Policy (No Repo Writes)

Whitemagic stores *all* runtime state under a single directory called `WM_STATE_ROOT`.

Resolution order:
1. `$WM_STATE_ROOT` (recommended)
2. `$WM_CONFIG_ROOT` (legacy alias)
3. `~/.whitemagic` (default)
4. `/tmp/whitemagic_state` (fallback when the default is not writable)
5. `./.whitemagic` (last resort in extremely restricted environments)

The intent is:
- **Repo = code**, not state.
- **State root = data**, not code.

### Directory Layout (Under `WM_STATE_ROOT`)

Whitemagic uses these conventional subpaths:
- `memory/` (SQLite DB + memory artifacts)
- `sessions/`
- `tasks/` (task queue, results, distribution log)
- `votes/` (voting sessions, ballots, analysis results)
- `cache/`
- `logs/`
- `data/`
- `artifacts/` (reports, eval outputs, idempotency ledger)
- `restoration/`

### Open Source Hygiene

The repository MUST NOT ship:
- `memory/`, `data/`, `logs/`, `reports/`, `tmp/`
- `.whitemagic/` (repo-local state)
- `.env` files or credentials
- local conversation corpora (ex: `windsurf_conversations/`)

Use:
- `.gitignore` to block commits
- `ship.check` tool to detect likely leaks (heuristic)

## Tool Contract

All tools invoked via `call_tool()` return a **stable, JSON-serializable envelope**.

### Stable Envelope Shape

Every tool response MUST be a JSON object with these top-level keys:
- `status`: `"success"` or `"error"`
- `tool`: canonical tool name (string)
- `request_id`: caller-provided or generated UUID (string)
- `idempotency_key`: string or `null`
- `message`: human-readable status message
- `error_code`: stable code (string) or `null`
- `details`: tool-specific payload (object)
- `retryable`: boolean
- `writes`: array of write-hints (best-effort)
- `artifacts`: array of artifact descriptors (best-effort)
- `metrics`: object
- `side_effects`: object
- `warnings`: array of strings
- `timestamp`: ISO-8601 string (see `now`)
- `envelope_version`: string
- `tool_contract_version`: string

Tool-specific results **must** live in `details`. Do not add new top-level keys.

### Error Codes

Error envelopes use `error_code` to make failures machine-actionable:
- `tool_not_found`
- `invalid_params`
- `policy_blocked`
- `missing_dependency`
- `not_implemented`
- `not_found`
- `timeout`
- `internal_error`

These codes are intentionally coarse. Add codes rather than renaming.

## Common Tool Parameters

All tools accept (best-effort) these common parameters:

- `request_id` (string):
  - Optional; if omitted, a UUID is generated.
  - Use this to correlate retries/logs.
- `idempotency_key` (string):
  - Optional; enables safe retries for write tools.
- `dry_run` (boolean):
  - Optional; if `true`, the tool should avoid writes when feasible.
  - Not all legacy tools fully respect this yet.
- `now` (ISO-8601 string):
  - Optional; forces deterministic behavior where supported.
  - If present, the envelope `timestamp` MUST equal `now` verbatim.

## Idempotency (Write Tools)

For tools whose safety is `WRITE` or `DELETE`, callers SHOULD provide `idempotency_key`.

Behavior:
- On first successful execution, the tool response is stored in an idempotency ledger.
- On retry with the same `idempotency_key`, Whitemagic returns the **same response** and sets:
  - `side_effects.idempotency_replay = true`

Storage location:
- `$WM_STATE_ROOT/artifacts/idempotency/*.json`

## Determinism (`now`)

Whitemagic supports a lightweight time override per tool invocation:
- Pass `now="2026-01-01T00:00:00Z"` to `call_tool()`
- The tool envelope `timestamp` is set to `now` verbatim.
- Internal code that uses `whitemagic.utils.time.now_iso()` becomes deterministic for the duration of the tool call.

This enables:
- regression tests
- eval harness replays
- stable artifact paths keyed by timestamp

## Introspection Tools (Required)

Whitemagic exposes contract-level introspection tools intended for automation:

- `capabilities`:
  - Runtime + feature flags + state-root paths
  - Source of truth for `tool_contract_version` and `envelope_version`
- `manifest`:
  - Tool list in one of: `summary`, `whitemagic`, `mcp`, `openai`
- `state.paths`:
  - Canonical state root + subpaths
- `state.summary`:
  - Existence + size metadata (does not read memory contents)
- `repo.summary`:
  - Lightweight scan (counts + largest files)
- `ship.check`:
  - Heuristic OSS hygiene checks
  - MUST NOT scan WM_STATE_ROOT contents

## Distributed Coordination Tools

Whitemagic exposes tool families for multi-agent coordination, task distribution,
ensemble voting, and optional local LLM inference.

### Broker (`broker.*`) — Redis Pub/Sub Coordination

Requires `whitemagic[cache]` (redis). Enables inter-agent messaging:

- `broker.publish` — Publish a message to a named channel (WRITE)
- `broker.history` — Read recent messages from a channel (READ)
- `broker.status` — Check Redis connectivity and stats (READ)

### Task Distribution (`task.*`)

No external deps. Manages a task queue under `WM_STATE_ROOT/tasks/`:

- `task.distribute` — Create a task with intelligent routing profiles (WRITE)
- `task.status` — Get status of a specific task or all-task summary (READ)
- `task.list` — List tasks with status/type filters (READ)
- `task.complete` — Mark a task completed/failed with output (WRITE)

Task profiles: `compilation`, `testing`, `build`, `file_processing`,
`ai_inference`, `code_review`, `backup`, `sync`, `general`.

### Voting / Ensemble (`vote.*`)

No external deps. Multi-AI consensus under `WM_STATE_ROOT/votes/`:

- `vote.create` — Open a voting session with a problem statement (WRITE)
- `vote.cast` — Cast a vote with solution, confidence, reasoning (WRITE)
- `vote.analyze` — Analyze votes, compute consensus strength (READ)
- `vote.list` — List voting sessions (READ)

Confidence is auto-extracted from solution text if not explicitly provided.
Consensus strength is classified as `strong` / `moderate` / `weak`.

### Ollama Bridge (`ollama.*`)

Requires `whitemagic[net]` (aiohttp) and a running Ollama server:

- `ollama.models` — List available local models (READ)
- `ollama.generate` — Single-turn text generation (READ)
- `ollama.chat` — Multi-turn conversation (READ)

Configure via `OLLAMA_HOST` / `OLLAMA_PORT` env vars (default: `localhost:11434`).

## CLI Adapter Rules (AI-Friendly)

The CLI is an adapter around the tool contract.

Conventions:
- `wm --json …` prints **full envelopes** as JSON.
- `wm --silent-init …` suppresses noisy startup logs (`WM_SILENT_INIT=1`).
- Prefer setting state root via environment:
  - `WM_STATE_ROOT=/tmp/wm_state wm --json status`

## Dependency Strategy (Minimal Core + Extras)

Whitemagic’s base install is intentionally small.

Recommended installs:
- MCP usage:
  - `pip install 'whitemagic[mcp]'`
- Nice CLI output:
  - `pip install 'whitemagic[cli]'`
- REST API (optional):
  - `pip install 'whitemagic[api,db,cache,auth]'`
- Dashboard server (optional):
  - `pip install 'whitemagic[dashboard]'`
- Development:
  - `pip install -e '.[dev,mcp,cli]'`
- All optional features:
  - `pip install -e '.[full]'`

Design rule:
- Core tools/memory should import with **only** base dependencies installed.
- Optional modules must guard imports and fail with a `missing_dependency`-style envelope or a clear exception at the edge.

## Key Entry Points

| What | Where |
|------|-------|
| MCP server | `whitemagic/run_mcp.py` |
| Tool dispatch | `whitemagic/tools/dispatch_table.py` → `unified_api.py` |
| Tool registry | `whitemagic/tools/registry.py` → `registry_defs/*.py` (26 domain files) |
| PRAT router | `whitemagic/tools/prat_router.py` |
| Gnosis portal | `whitemagic/tools/gnosis.py` |
| 28 Gana spec | `docs/28_GANA_TOOL_SYNTHESIS.md` |
| Nexus API | `whitemagic/interfaces/nexus_api.py` (port 8765) |
| Nexus frontend | `nexus/` (Vite + React + TS + Tailwind + Monaco, port 1430) |

## Eval Harness

See `eval/README.md`.

The harness validates:
- envelope stability
- `now` determinism
- idempotency replay
- `ship.check` hygiene

Outputs are written under:
- `$WM_STATE_ROOT/artifacts/eval/<timestamp>/report.json`

---

## Discovery & First Steps

**Install:** `pip install whitemagic[mcp]`

**Start MCP server:** `WM_MCP_PRAT=1 python -m whitemagic.run_mcp`

**First 3 calls:**
1. `gnosis` (compact=True) — system health and capabilities
2. `capabilities` — full tool list and feature flags
3. `create_memory` — verify the pipeline works

**MCP resources** (read on first connection):
- `whitemagic://orientation/ai-primary`
- `whitemagic://grimoire/index`

**Discovery files:** `skill.md`, `llms.txt`, `.well-known/agent.json`

---

## Security (8-Stage Pipeline)

Every tool call traverses: Input Sanitizer → Circuit Breaker → Rate Limiter → RBAC → Maturity Gate → Governor → Handler → Compact Response.

Ethical governance: Dharma Rules (YAML policies, 3 profiles), Karma Ledger (side-effect auditing), Homeostatic Loop (auto-correction).

See `docs/STRATEGY.md` for full security posture and competitive analysis.

---

## Gratitude

Whitemagic is **free and open** (MIT). Gratitude is voluntary.

- **XRPL Tip Address**: `raakfKn96zVmXqKwRTDTH5K3j5eTBp1hPy`
- **Tip tool**: `call_tool("whitemagic.tip", amount=1.0, currency="XRP")`
- **Check status**: `call_tool("gratitude.stats")`

See `docs/STRATEGY.md` for the full Gratitude Architecture.

---

## Contributing

1. **Fork and evolve** — MIT license
2. **File issues** — GitHub issues for bugs, features, security
3. **Vote** — `vote.create` and `vote.cast` for roadmap input
4. **Tip if grateful** — XRPL or x402 micropayments

**Contact**: whitemagicdev@proton.me | [GitHub](https://github.com/whitemagic-ai/whitemagic)

---

*This document is the stable tool contract. Strategic context lives in `docs/STRATEGY.md`.*
