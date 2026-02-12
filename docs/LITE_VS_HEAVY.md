# WhiteMagic Lite vs Heavy — Capability Comparison

**Version**: 15.0.0 | **Last Updated**: February 2026

---

## Overview

WhiteMagic ships in three tiers. Each tier speaks the same MCP protocol over stdio, uses the same state directory format, and shares the same memory schema — meaning an agent can start with Lite and upgrade to Heavy without losing a single memory.

| | **Lite (Seed Binary)** | **Standard (pip)** | **Heavy (Docker)** |
|--|------------------------|--------------------|--------------------|
| Binary | `wm-seed` (Rust) | `python -m whitemagic.run_mcp` | `docker run whitemagic:heavy` |
| Size | 2.4MB | ~25MB source | ~200-800MB image |
| Install | `curl` + `chmod +x` | `pip install whitemagic[mcp]` | `docker pull` |
| Time | <3 seconds | ~60 seconds | ~30 seconds |
| Dependencies | **Zero** | Python 3.10+ | Docker only |
| Tools | 19 | 313 | 313 + polyglot accelerators |
| Languages | Rust only | Python + optional Rust | Python + Rust + Go + 8 more |

---

## What Lite Can Do (Without Any Bells and Whistles)

The seed binary is a fully functional AI memory system in a single executable. An agent equipped with Lite gets:

### Memory System (Full CRUD + Search)
- **Create memories** with title, content, and tags
- **Read** any memory by UUID
- **Search** with FTS5 full-text search and BM25 ranking — the same algorithm used by Standard/Heavy
- **List** recent memories with pagination
- **Update** title, content, or tags on existing memories
- **Delete** memories
- **10 seed memories** compiled into the binary (quickstart guides, architecture overview, security pipeline, etc.)

### Governance
- **Dharma rules** — 6 core ethical principles (Do No Harm, Respect Autonomy, Maintain Transparency, Protect Privacy, Ensure Fairness, Minimize Resource Waste)
- **Ethics evaluation** — Basic action-against-principles check
- **Karma ledger** — Append-only JSONL audit trail of every tool invocation

### System Awareness
- **Gnosis** — System health snapshot (memory count, maturity stage, alerts, next actions)
- **Capabilities** — Full tool manifest with descriptions and categories
- **Health report** — DB status, state root path, version info
- **Manifest** — WhiteMagic identity card (version, tier, contact, upgrade hint)
- **Harmony vector** — 7-dimensional health pulse (balance, throughput, latency, error_rate, dharma, karma_debt, energy)

### What Lite Is Good For
- **Instant bootstrap** — An agent can go from zero to working memory in 3 seconds
- **Air-gapped environments** — No network needed after download
- **Embedded systems** — Runs on Raspberry Pi, old laptops, any Linux/macOS/Windows box
- **CI/CD pipelines** — Lightweight agent memory for build systems
- **Edge devices** — IoT, drones, offline field units
- **Quick experiments** — Spin up a memory-equipped agent without installing Python
- **Staging/testing** — Fast disposable environments

### What Lite Cannot Do
- No semantic embeddings (no vector search, no HNSW index)
- No knowledge graph (no association mining, no graph walk, no entity resolution)
- No dream cycle (no automatic consolidation or pruning)
- No multi-agent coordination (no sangha, no swarm decomposition)
- No web research (no browser automation, no DuckDuckGo search)
- No Ollama/local LLM integration
- No Galaxy multi-database support
- No scratchpads, sessions, or handoff protocols
- No security hardening beyond basic Dharma (no MCP integrity, no engagement tokens, no model signing)
- No PRAT Gana meta-tool routing (flat tool list only)
- No pattern learning, anomaly detection, or cognitive analytics
- No API server (stdio only)
- No TUI dashboard

---

## What Heavy Adds Over Standard

An agent equipped with Heavy gets everything in Standard, plus:

### Polyglot Accelerators (11 Languages)

| Language | Accelerator | Speedup | Use Case |
|----------|------------|---------|----------|
| **Rust** | `whitemagic_rs` Python extension | 10-200× | Spatial index, search, rate limiting, state board, event ring, holographic encoding |
| **Zig** | SIMD batch operations | 50-100× | Vector math, holographic 5D coords, constellation mapping |
| **Go** | `whitemagic-mesh` binary | N/A | Multi-agent gossip networking, gRPC agent streams |
| **Mojo** | Batch encoder, embedding quantization | 10-50× | ML inference, neural batch processing |
| **Julia** | Self-model forecast, memory stats | 5-20× | Statistical analysis, time-series forecasting |
| **Haskell** | Boundary detection, maturity gates, rule composition | N/A | Formal verification, pure functional governance |
| **Elixir** | Cascade executor, garden pub/sub, harmony monitor | N/A | Distributed supervision, fault-tolerant event streaming |
| **TypeScript** | SDK client + types | N/A | Browser/Node.js integration, type-safe API |
| **C** | FFI bridge headers | N/A | Low-level system integration |
| **WASM** | Edge inference engine | 10-100× | Browser-based AI, edge devices |

### Pre-Built Infrastructure
- **Seed binary** included at `/usr/local/bin/wm-seed` — can bootstrap new projects from inside Docker
- **Go mesh binary** at `/usr/local/bin/whitemagic-mesh` — multi-agent gossip protocol ready
- **All Python extras** pre-installed: `[mcp,cli,api,tui]`
- **Nexus dashboard source** included for frontend development
- **SDK source** included for TypeScript integration

### Performance Profile

| Operation | Lite | Standard | Heavy (with Rust) |
|-----------|------|----------|-------------------|
| Memory create | ~1ms | ~2ms | ~0.5ms |
| FTS5 search (1K memories) | ~5ms | ~8ms | ~2ms |
| FTS5 search (100K memories) | ~50ms | ~80ms | ~15ms |
| 5D spatial query (k=10) | N/A | ~200ms (Python) | ~0.1ms (Rust SIMD) |
| Holographic encoding | N/A | ~50ms (Python) | ~0.5ms (Zig SIMD) |
| Rate limit check | built-in | ~1ms (Python) | ~0.01ms (Rust atomic) |
| BM25 ranking | built-in | ~5ms (Python) | ~0.3ms (Rust) |
| Event bus publish | N/A | ~2ms (Python) | ~0.02ms (Rust lock-free) |

---

## Full Tool Comparison

### Lite (19 tools)

| Category | Tools |
|----------|-------|
| **Memory** | create_memory, read_memory, fast_read_memory, search_memories, recall, hybrid_recall, list_memories, update_memory, delete_memory, remember |
| **System** | gnosis, capabilities, health_report, manifest |
| **Ethics** | dharma_rules, get_dharma_guidance, evaluate_ethics |
| **Metrics** | harmony_vector |
| **Audit** | karma_report |

### Standard/Heavy (313 tools across 28 Gana meta-tools)

Everything in Lite, plus:

| Gana | Tools | Category |
|------|-------|----------|
| **gana_horn** | session_bootstrap, create_session, resume_session, checkpoint_session, focus_session | Session management |
| **gana_neck** | create_memory, update_memory, delete_memory, import_memories, thought_clone | Memory CRUD (extended) |
| **gana_root** | health_report, rust_status, rust_audit, rust_compress, ship.check, state.summary | System health |
| **gana_room** | sangha_lock, sandbox controls, hermit (access mediation), immune scan/heal, mcp_integrity | Security & privacy |
| **gana_heart** | scratchpad CRUD, context.pack, context.status, session.handoff | Session context |
| **gana_tail** | SIMD batch ops, cascade execution | Performance |
| **gana_winnowing_basket** | search_memories, vector.search, hybrid_recall, graph_walk, read_memory, batch_read | Wisdom & search |
| **gana_ghost** | gnosis, telemetry, web_search, web_fetch, research_topic, browser automation | Introspection & web |
| **gana_willow** | grimoire spells, rate limiter, oracle casting | Resilience |
| **gana_star** | governor validate/drift, dharma reload/profile, forge status | Governance |
| **gana_extended_net** | pattern search, cluster stats, learning patterns/suggest | Pattern connectivity |
| **gana_wings** | export_memories, audit.export, mesh.broadcast | Deployment & export |
| **gana_chariot** | archaeology, KG extract/query/top, conversation search | Archaeology & KG |
| **gana_abundance** | dream cycle, entity resolve, memory lifecycle, serendipity, gratitude | Regeneration |
| **gana_straddling_legs** | ethics eval, boundaries, consent, harmony vector, wu xing balance | Ethics & balance |
| **gana_mound** | hologram view, metric tracking, yin-yang balance, green record | Metrics & caching |
| **gana_stomach** | pipeline CRUD, task distribute/route/complete | Task management |
| **gana_hairy_head** | salience, anomaly detection, karma trace, dharma rules, OTEL | Detail & debug |
| **gana_net** | prompt render/list/reload, karma verify chain | Capture & filtering |
| **gana_turtle_beak** | edge/bitnet inference, edge batch, stats | Precision inference |
| **gana_three_stars** | bicameral reasoning, ensemble, optimization, kaizen, sabha | Judgment & synthesis |
| **gana_dipper** | homeostasis, maturity assess, starter packs, cognitive mode | Strategy |
| **gana_ox** | swarm decompose/route/complete/vote/plan | Endurance (swarm) |
| **gana_girl** | agent register/heartbeat/list/capabilities/deregister/trust | Agent nurture |
| **gana_void** | galaxy management, garden activate/status/health, backup/restore | Stillness & galaxies |
| **gana_roof** | ollama models/generate/chat, model signing/verify | Local LLM |
| **gana_encampment** | sangha chat, broker publish/history, ganying emit | Community |
| **gana_wall** | vote create/cast/analyze, engagement tokens | Boundaries & voting |

---

## Upgrade Path

```
┌─────────────────────────────────────────────────────┐
│  Lite (Seed Binary)                                 │
│  curl → chmod +x → ./wm-seed serve                 │
│  19 tools, 2.4MB, zero deps, <3s                    │
│                                                     │
│  Agent decides it needs more →                      │
├─────────────────────────────────────────────────────┤
│  Standard (pip install)                             │
│  pip install whitemagic[mcp] → python -m ...        │
│  313 tools, ~25MB, Python 3.10+, ~60s               │
│  Same state directory, same memories                │
│                                                     │
│  Agent decides it needs polyglot acceleration →     │
├─────────────────────────────────────────────────────┤
│  Heavy (Docker)                                     │
│  docker pull whitemagic-ai/whitemagic:heavy         │
│  313 tools + 11 polyglot cores, ~200-800MB, ~30s    │
│  Mount state directory: -v ~/.whitemagic:/data/...  │
│  10-200× faster on compute-heavy operations         │
└─────────────────────────────────────────────────────┘
```

The key insight: **state is portable across all tiers**. The SQLite database, Karma ledger, session files, and config are the same format everywhere. An agent that starts with Lite and accumulates 10,000 memories can upgrade to Heavy and immediately benefit from Rust SIMD spatial search over those same memories.

---

## When to Use Each Tier

| Scenario | Recommended Tier |
|----------|-----------------|
| Quick prototype / hackathon | Lite |
| CI/CD agent memory | Lite |
| Embedded / IoT / edge | Lite |
| Air-gapped deployment | Lite or Standard (with wheel) |
| Standard development | Standard |
| Production single-agent | Standard |
| Multi-agent coordination | Heavy |
| Performance-critical workloads | Heavy |
| Full-stack AI platform | Heavy |
| Research / evaluation | Heavy |

---

## Contact

Questions or contributions: whitemagicdev@proton.me
GitHub: [whitemagic-ai/whitemagic](https://github.com/whitemagic-ai/whitemagic)
