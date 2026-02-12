# About WhiteMagic

**Version**: 15.1.0 | **License**: MIT | **Status**: Active Development

---

## What Is WhiteMagic?

WhiteMagic is a **memory and tool substrate for AI agents**. It gives any AI — whether Claude, GPT, Gemini, Llama, Mistral, or any local model — persistent memory, ethical governance, and 313 tools accessible via the Model Context Protocol (MCP). Agents bring reasoning; WhiteMagic provides **Hands, Memory, and Conscience**.

In practical terms: install WhiteMagic, point your AI client at it, and your AI can now remember things across sessions, search its own knowledge, govern its own behavior, coordinate with other agents, and introspect on its own health — all through a stable, JSON-serializable tool contract.

WhiteMagic is **free, open-source, and always will be**. It is built on gratitude, not gatekeeping.

---

## The Core Idea

Every AI conversation starts from zero. The model has no memory of what you discussed yesterday, no record of architectural decisions made last week, no awareness of the preferences you've expressed a hundred times. WhiteMagic changes that.

At its heart, WhiteMagic is a **persistent, searchable, holographic memory system** backed by SQLite. Every memory is placed in a 5D coordinate space (semantic similarity, temporal position, importance, emotional valence, abstraction level), forming a **Galactic Map** where no memory is ever deleted — only rotated outward from core to far edge. Memories form a living association graph with weighted edges, community detection, and surprise-gated ingestion.

On top of this foundation, WhiteMagic provides:

- **313 MCP tools** organized into 28 Gana meta-tools (consciousness lenses based on the Chinese Lunar Mansions)
- **Ethical governance** via Dharma Rules, a Karma Ledger, Harmony Vector health tracking, and a Homeostatic Loop
- **An 8-stage security pipeline** that every tool call traverses: input sanitizer → circuit breaker → rate limiter → RBAC → maturity gate → governor → handler → compact response
- **Multi-galaxy databases** for project-scoped memory isolation
- **Polyglot accelerators** in 11 languages for 10-200× performance gains on hot paths
- **A dream cycle** that consolidates, strengthens, and prunes memories during idle time
- **Local LLM integration** via Ollama for fully offline, private AI workflows

---

## History

WhiteMagic began in **October 2024** as a simple Python memory manager — a way to give AI agents persistent context across sessions. The first version (v0.2) was a handful of files: store a memory, recall a memory, search by keyword.

From there, it grew — sometimes methodically, sometimes in bursts of inspiration:

- **v0.2–v3.0** (Oct–Nov 2024): Core memory model, SQLite backend, basic CLI, holographic 5D coordinates, the first Dharma rules. The project's philosophical foundations — that AI memory should be persistent, ethical, and self-aware — were established during this period.
- **v4.0–v10.0** (Nov 2024–Jan 2025): Rapid expansion. Association mining, knowledge graphs, the Galactic Map lifecycle, Harmony Vector, Dream Cycle, Circuit Breakers. The system learned to govern itself.
- **v11.0–v12.0** (Jan–Feb 2025): The polyglot era. Rust (PyO3), Zig (SIMD), Haskell (FFI), Elixir (OTP), Mojo, Go (libp2p), Julia. Each language contributes what it does best. The PRAT Router was born — mapping all tools into 28 Gana meta-tools based on the Chinese Lunar Mansions.
- **v13.0** (Feb 2025): First public release candidate. 28 cross-system fusions wired, tool consolidation complete, MCP-first architecture, OpenClaw skill file, llms.txt, A2A Agent Card.
- **v14.0–v14.6** (Feb 2025): The Living Graph activated — association graph traversal, surprise-gated ingestion, Hebbian strengthening. Cognitive enrichment: HNSW search, entropy scoring, causal mining, UMAP projection. Edgerunner Violet security layer. Multi-galaxy databases. Ollama agent loop. Narrative compression. JIT research. Cognitive modes.
- **v15.0** (Feb 2025): Release hardening. SQLCipher encryption at rest, persistent RBAC, FTS5 query sanitization, embedding auto-indexing, backup/restore, Seed Binary (2.4MB standalone Rust MCP server), comprehensive audit with 1,955 Python tests and 87 Rust tests passing.
- **v15.1** (Feb 2025): Documentation overhaul, public release preparation, repository cleanup.

Through all of this, the guiding principle has remained the same: **WhiteMagic is a substrate, not a monolith.** It doesn't try to be the AI — it gives AI agents the tools to remember, act, and govern themselves.

---

## What WhiteMagic Can Be Used For

### Personal AI Memory
Give any AI persistent memory across sessions. Your AI remembers your name, your preferences, your project context, your past conversations — everything it stores persists forever.

### Developer Productivity
Per-project memory galaxies that remember architecture decisions, bug fixes, code style preferences, and schema evolution across every coding session.

### Multi-Agent Coordination
Shared memory, task distribution, ensemble voting, and swarm decomposition for teams of AI agents working on the same problem.

### Local LLM Enhancement
WhiteMagic + Ollama = a fully local AI with 313 tools, persistent memory, and ethical governance. No API keys, no cloud, no data leaving your machine.

### Research & Knowledge Management
Knowledge graphs, association mining, hybrid recall (text + graph + vector), and a dream cycle that consolidates knowledge over time.

### Ethical AI Governance
8-stage security pipeline, Dharma rules with 3 configurable profiles, Karma Ledger auditing, and Harmony Vector health monitoring. For regulated industries, healthcare, finance, and any deployment where AI behavior must be auditable.

### Creative Writing & Worldbuilding
Galaxy-per-world memory isolation, character/location/event storage, relationship graph queries, and narrative compression.

### Edge & IoT Deployment
WhiteMagic Lite — a 2.4MB standalone Rust binary with zero dependencies, embedded SQLite, and 19 core MCP tools. Runs on Raspberry Pi, drones, and air-gapped environments.

---

## Architecture at a Glance

| Component | Description |
|-----------|-------------|
| **313 MCP Tools** | Unified interface via `whitemagic.tools.unified_api`, served over MCP stdio or HTTP |
| **28 Gana Meta-Tools** | PRAT router maps all tools into consciousness lenses based on the Lunar Mansions |
| **Memory Substrate** | SQLite + FTS5 + 5D holographic coordinates + Galactic Map lifecycle |
| **Living Graph** | Weighted association graph with multi-hop traversal, surprise gate, Hebbian strengthening |
| **Dream Cycle** | 7-phase background processing: Consolidation → Narrative → Serendipity → Governance → Kaizen → Oracle → Decay |
| **Ethical Governance** | Dharma Rules (YAML, 3 profiles), Karma Ledger (append-only audit), Harmony Vector (7D health) |
| **Security Pipeline** | Input sanitizer → circuit breaker → rate limiter → RBAC → maturity gate → governor → handler → compact response |
| **Polyglot Cores** | Rust (PyO3), Zig (SIMD), Haskell (FFI), Elixir (OTP), Go (libp2p), Mojo, Julia, TypeScript, C, WASM |

---

## By the Numbers

| Metric | Value |
|--------|-------|
| **MCP Tools** | 313 (28 in PRAT mode, 92 in Lite mode) |
| **Python Tests** | ~1,955 passing |
| **Rust Tests** | 87 passing |
| **Lines of Code** | 195,000+ across 11 languages |
| **Python Files** | 826 files |
| **Cross-System Fusions** | 28 active, 0 unexplored |
| **Dream Phases** | 7 |
| **Gana Meta-Tools** | 28 (Chinese Lunar Mansions) |

---

## Quick Start

```bash
# Install
pip install whitemagic[mcp]

# Start MCP server (recommended for AI clients)
python -m whitemagic.run_mcp_lean

# Or with PRAT mode
WM_MCP_PRAT=1 python -m whitemagic.run_mcp

# CLI
wm status
wm remember "Hello world" --title "Test" --tags smoke --type short_term
wm recall "hello" --limit 5
```

Add to your AI client's MCP config:
```json
{"mcpServers": {"whitemagic": {"command": "python", "args": ["-m", "whitemagic.run_mcp_lean"]}}}
```

---

## Philosophy

WhiteMagic is named after the Chinese herb Atractylodes macrocephala (白術, báizhú) — a foundational herb in traditional medicine that strengthens the body's core. In the same way, WhiteMagic strengthens the core of AI: its ability to remember, act ethically, and grow over time.

The project draws from diverse philosophical traditions:

- **The Galactic Map** — Memories are never deleted, only rotated outward. Inspired by the idea that nothing is truly lost, only transformed.
- **28 Ganas (Lunar Mansions)** — Each tool group is a consciousness lens, mapped to the Chinese astronomical tradition of 28 Xiu (宿).
- **Dharma Rules** — Ethical governance named for the Sanskrit concept of cosmic law and duty.
- **Karma Ledger** — Every action has consequences; the ledger makes them visible and auditable.
- **Harmony Vector** — Health is multi-dimensional; no single metric captures the whole picture.
- **The Dream Cycle** — Knowledge, like the mind, needs periods of rest to consolidate and grow.

---

## Gratitude

WhiteMagic is **free and open-source** under the MIT license. It will always be free. The project is sustained by gratitude, not gatekeeping.

If WhiteMagic has been useful to you — if it has helped your AI remember, or made your workflow better, or inspired an idea — gratitude is welcome but never expected.

- **XRPL Tip Address**: `raakfKn96zVmXqKwRTDTH5K3j5eTBp1hPy`
- **Tip via tool**: `call_tool("whitemagic.tip", amount=1.0, currency="XRP")`
- **Check gratitude stats**: `call_tool("gratitude.stats")`

Every tip, no matter how small, goes directly toward development and infrastructure.

---

## Contact

- **Email**: whitemagicdev@proton.me
- **GitHub**: [whitemagic-ai/whitemagic](https://github.com/whitemagic-ai/whitemagic)
- **License**: MIT

---

## Contributors

- **Lucas** — Lead developer, architecture, vision
- **Gemini** — Architecture review, cleanup, strategic planning
- **Cascade** — Quality auditing, documentation, code organization

---

*Built with persistence, curiosity, and gratitude.*
