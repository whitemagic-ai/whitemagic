# The Resonant Swarm: From Nervous System to Digital Metabolism

**Date:** February 10, 2026
**Version:** WhiteMagic v13.6.0 → Target v14.1–14.2
**Context:** Gap analysis between research paper "The Resonant Swarm: Architectural Feasibility of Hierarchical Tool Dispatch via Zero-Copy Shared State" and WhiteMagic's current architecture.

---

## Executive Summary

The research paper provides rigorous academic validation for WhiteMagic's Leap 7–8 architecture while revealing specific gaps that, once addressed, would position WhiteMagic as the reference implementation for resonant agentic swarms. The core finding: **WhiteMagic already has ~70% of the described architecture built or designed.** The remaining 30% — Apache Arrow data format, Iceoryx2 IPC, async waker integration, and Rust polymorphic agent futures — represents the path from "nervous system" to "digital metabolism."

### Key Insight

The paper's central thesis — that JSON-RPC serialization creates a "serialization wall" prohibiting million-agent scaling — directly validates WhiteMagic's existing Control Plane / Data Plane separation (Leap 7). MCP handles tool discovery (slow path); StateBoard + EventRing handle state synchronization (fast path). What remains is completing the wiring, upgrading the data format, and extending to multi-process.

---

## Part 1: Current State — What We Have

### Leap 7 Artifacts (Built, Partially Wired)

| Component | File | Status | Benchmarked |
|-----------|------|--------|-------------|
| **StateBoard** | `whitemagic-rust/src/state_board.rs` (418 LOC) | Built, compiled, partially wired | 2.3µs read / 432K ops/sec |
| **EventRing** | `whitemagic-rust/src/event_ring.rs` (383 LOC) | Built, compiled, partially wired | 387ns pub / 2.58M ops/sec |
| **Dispatch Core** | `whitemagic-zig/src/dispatch_core.zig` (275 LOC) | Built, compiled, partially wired | 1.6µs full pipeline / 641K ops/sec |
| **Engine Registry** | `whitemagic/core/engines/registry.py` | Built, all 28 engines registered | N/A |
| **StateBoard Bridge** | `whitemagic/core/acceleration/state_board_bridge.py` | Built, Rust fast path + Python mmap fallback | N/A |
| **EventRing Bridge** | `whitemagic/core/acceleration/event_ring_bridge.py` | Built, Rust fast path + Python deque fallback | N/A |
| **Dispatch Bridge** | `whitemagic/core/acceleration/dispatch_bridge.py` | Built, Zig FFI fast path + Python fallback | N/A |

### What's Wired vs What's Built-But-Idle

**Wired (active in production path):**
- HarmonyVector → StateBoard push on every recompute
- CircuitBreaker → StateBoard sync on state transitions
- EventRing ← GanYingBus emit()
- DispatchBridge → PRAT router pre-check

**Built but NOT yet primary path:**
- Most Python code still reads HarmonyVector from Python singleton, not StateBoard
- EventRing consumers exist but GanYingBus still defaults to Python deque
- Dispatch pipeline still runs 8 Python function calls; Zig path is advisory only

### Hierarchical Tool Dispatch (Already Implemented)

WhiteMagic's PRAT Router is a production-grade HTD system:
- **Level 0:** MCP protocol (tool discovery, capability negotiation)
- **Level 1:** 28 Gana meta-tools (domain classification)
- **Level 2:** 181 individual tools (specific operations)
- **Resonance context:** predecessor/successor awareness, Wu Xing elemental boost, Guna adaptation
- **Three operating modes:** Classic (181), Lite (92), PRAT (28)

This exceeds the paper's HTD description, which proposes only generic hierarchical routing without domain-specific resonance context.

---

## Part 2: Research Alignment Matrix

### Concept-by-Concept Mapping

| Paper Concept | Paper Section | WM Implementation | Status | Gap |
|---------------|---------------|-------------------|--------|-----|
| **Hierarchical Tool Dispatch** | §2 | PRAT Router (28 Ganas → 181 tools) | ✅ Exceeds | Deeper DAG recursion, JIT schema loading |
| **MCP Control Plane** | §2.2 | `run_mcp.py` + `prat_router.py` | ✅ Complete | None |
| **Capability Negotiation** | §2.2.1 | Three MCP modes (classic/lite/PRAT) | ✅ Complete | JIT per-Gana schema loading |
| **LMAX Ring Buffer** | §3 | `event_ring.rs` (65K slots, 128B aligned) | ✅ Complete | Async waker, hybrid wait strategy |
| **Cache-Line Padding** | §3.3.1 | 128-byte `#[repr(C, align(128))]` on EventSlot | ✅ Complete | Audit write_cursor isolation |
| **Shared Memory Blackboard** | §5.1 | `state_board.rs` (4KB mmap, atomic R/W) | ✅ Complete | Inter-process (Iceoryx2) |
| **Zero-Copy Access** | §4.2.2 | StateBoard mmap + pointer arithmetic | ⚠️ Partial | Apache Arrow for extensible data |
| **Control/Data Plane Split** | §5.2 | MCP (control) + StateBoard (data) | ⚠️ Partial | Full wiring of consumers |
| **Apache Arrow Format** | §4.2.2 | Not implemented | ❌ Missing | New — Data Sea format |
| **Iceoryx2 IPC** | §5.1 | Planned (Leap 8a) | ❌ Missing | Process-shared StateBoard |
| **Polymorphic Agent Futures** | §5.2 Layer 3 | Python CloneArmy + GanaSwarm | ⚠️ Partial | Rust tokio futures |
| **Async Disruptor** | §3.3.2 | Not implemented | ❌ Missing | tokio::Notify integration |
| **Hybrid Wait Strategy** | §6.2 | Not implemented | ❌ Missing | Spin-then-yield |
| **Process Crash Cleanup** | §5.1.1 | Not implemented | ❌ Missing | Iceoryx2 handles this |
| **Agent Sandbox (R/O vs R/W)** | §6.3 | Not implemented | ❌ Missing | Iceoryx2 subscriber permissions |

### What The Paper Validates About Our Architecture

1. **PRAT is the correct HTD approach.** The paper's "Root Dispatcher classifies, sub-agents execute" is exactly what PRAT does with Gana-level classification → tool-level dispatch.

2. **StateBoard IS the Resonant Blackboard.** Custom binary mmap with atomic access matches the paper's prescription precisely.

3. **EventRing IS the LMAX Disruptor.** Lock-free, pre-allocated, cache-line aligned, overwrite policy — all present.

4. **Zig comptime dispatch IS the "hot path in native code" recommendation.** Reading StateBoard directly from Zig bypasses the Python stack entirely.

5. **28 Engines as Blackboard consumers IS the paper's "consumer model."** Engines observe shared state and produce insights without message-passing.

6. **The Yin-Yang cycle (waking/dreaming) IS the paper's "reactive monitor → active reasoner" polymorphism** — just expressed through different metaphor.

### What The Paper Misses That WhiteMagic Has

| WhiteMagic Feature | Paper Coverage | Strategic Advantage |
|--------------------|---------------|---------------------|
| **Dharma Governance** (ethical rules, graduated actions) | Brief mention of "security" in §6.3 | Critical for trusted agent swarms |
| **Karma Ledger** (declared vs actual side-effects) | Not addressed | Auditability for autonomous agents |
| **Galactic Memory** (no-delete rotating archive, 110K memories) | Not addressed | Persistent context beyond ephemeral ring |
| **Maturity Gates** (staged capability unlocking) | Not addressed | Safe developmental progression |
| **RBAC + Input Sanitizer** | "Sandboxing" mentioned briefly | Production-grade security pipeline |
| **28-fold Mandala Structure** | "Hierarchy" without organizational principle | Coherent domain decomposition |
| **Resonance Context** (Wu Xing, Guna, predecessor/successor) | Not addressed | Inter-tool memory across invocations |
| **Economic Model** (Gratitude Architecture) | Not addressed | Sustainable agent ecosystem |

---

## Part 3: Actionable Improvements

### Priority Order (informed by research impact × implementation effort)

---

### Phase A: Complete the Nervous System Wiring ⚡
**Effort:** ~1 session | **Impact:** HIGH — activates existing infrastructure
**Prerequisite:** Rust compilation (`maturin develop --release`)

The StateBoard, EventRing, and Dispatch Core are built but not fully wired as the **primary** path. Currently they shadow the Python path. This phase makes them authoritative.

#### A1: StateBoard as Source of Truth
- Modify `harmony/vector.py` `HarmonyVector.recompute()` to write to StateBoard AND update Python fields (already partially done)
- Modify all consumers (homeostatic_loop.py, gnosis.py, prat_resonance.py) to READ from StateBoard via bridge instead of accessing HarmonyVector Python fields directly
- Add `StateBoardReader` context manager to `state_board_bridge.py` that reads the full board state once per dispatch cycle and caches it (amortize mmap reads)

#### A2: EventRing as Primary Event Bus
- Modify `gan_ying_enhanced.py` `emit()` to write to EventRing FIRST, then broadcast to Python subscribers
- Create `EventRingPoller` async task in `event_ring_bridge.py` that polls the ring and dispatches to Python callbacks
- Wire FAST lane events exclusively through EventRing (< 10ms latency requirement matches ring's 387ns publish)

#### A3: Dispatch Pre-Check Activation
- Currently Zig dispatch is advisory. Make it authoritative: if Zig says CIRCUIT_OPEN or RATE_LIMITED, skip Python pipeline entirely
- Add `dispatch_bridge.check()` as step 0 in `dispatch_table.py` — fast-fail before any Python middleware runs
- Measure: should drop dispatch overhead from ~40µs to ~2µs for denied requests

#### A4: False Sharing Audit
- Verify `EventRing.write_cursor` and `consumer_cursors[0]` don't share a cache line
- Add `#[repr(align(128))]` wrapper around `write_cursor` if needed (paper §3.3.1)
- Benchmark before/after to measure impact

**Exit Criteria:**
- All hot-path state reads come from StateBoard (via bridge with Python fallback)
- GanYing FAST lane events publish through EventRing
- Zig dispatch pre-check is authoritative for deny decisions
- No false sharing between producer and consumer cursors

---

### Phase B: Apache Arrow Data Sea 🏹
**Effort:** ~1 session | **Impact:** HIGH — enables extensible zero-copy data sharing
**New dependency:** `arrow` crate (Rust), `pyarrow` (Python)

The paper identifies Apache Arrow as the key technology for zero-copy data beyond fixed-layout structs. This replaces the "Data Sea" concept from Leap 8d with a concrete, standards-based implementation.

#### B1: Arrow Schema for Memory Embeddings
Define an Arrow schema for the hot memory corpus:

```
Schema:
  memory_id: UInt64
  title: Utf8
  embedding: FixedSizeList(Float32, 384)
  galactic_distance: Float64
  importance: Float64
  zone: UInt8  (0=CORE, 1=INNER_RIM, 2=MID_BAND, 3=OUTER_RIM, 4=FAR_EDGE)
  constellation_id: UInt16
  last_accessed_ns: UInt64
  is_protected: Boolean
```

5,562 memories × ~1.6KB per record ≈ **8.9 MB** Arrow file. Fits comfortably in L3 cache on modern CPUs.

#### B2: Rust Arrow Writer
- New file: `whitemagic-rust/src/data_sea.rs`
- Function: `build_data_sea(db_path: &str) -> Result<PathBuf>` — reads SQLite, writes Arrow IPC file
- Function: `data_sea_search(query_vec: &[f32], top_k: usize) -> Vec<(u64, f32)>` — SIMD cosine similarity over Arrow column
- PyO3 bindings for Python access

#### B3: Python Arrow Bridge
- New file: `whitemagic/core/acceleration/data_sea_bridge.py`
- Memory-map the Arrow IPC file via `pyarrow.ipc.open_file()` with memory_map=True
- Expose `search_similar_arrow(embedding, top_k)` — zero-copy, no deserialization
- Wire into `embeddings.py` as the fast path (fallback to SQLite cache)

#### B4: Engine Data Sea Protocol
- Each of the 28 engines can register an Arrow RecordBatch as its "output"
- The DataSea aggregates all engine outputs into a single shared Arrow file
- Other engines read outputs without serialization
- This replaces the current approach of engines returning Python dicts

**Exit Criteria:**
- Hot memory embeddings available as Arrow mmap file
- Embedding search via Arrow is ≥10× faster than SQLite cache
- At least 2 engines publish/consume via Arrow Data Sea protocol
- pyarrow integration tested on Python 3.10/3.11/3.12

---

### Phase C: Async Disruptor 🌀
**Effort:** ~0.5 session | **Impact:** MEDIUM — enables efficient green-thread scaling
**Prerequisite:** Phase A (EventRing wired)

The paper's "Async Disruptor" (§3.3.2) adapts the LMAX pattern for millions of lightweight tasks.

#### C1: Waker Integration
- Add `tokio::sync::Notify` to `RingBuffer` struct
- After each `publish()`, trigger `notify.notify_waiters()`
- New function: `async fn poll_async(consumer_id, max_events)` — awaits notification, then polls

#### C2: Wait Strategy Enum
```rust
pub enum WaitStrategy {
    BusySpin,                    // Lowest latency, burns CPU
    Yield,                       // Cooperative, low CPU
    HybridSpin(Duration),        // Spin for N, then yield (paper recommends 50µs)
}
```
- Default: `HybridSpin(Duration::from_micros(50))`
- Configurable per consumer via `ring_register_consumer_with_strategy()`

#### C3: Python Async Bridge
- Expose `ring_poll_async` via PyO3 with `pyo3-asyncio` for native Python `await`
- Create `async def poll_events()` in `event_ring_bridge.py`
- Wire GanYingBus consumers to optionally use async polling

**Exit Criteria:**
- EventRing supports async consumers that sleep efficiently
- Hybrid wait strategy yields CPU when idle, spins when hot
- Python async bridge allows `await ring.poll()` from asyncio code

---

### Phase D: JIT Schema Loading for PRAT 📋
**Effort:** ~0.5 session | **Impact:** MEDIUM — reduces MCP init time + agent context pressure

The paper's "Lazy Loading" architecture (§2.2.1) maps directly to an optimization of PRAT mode.

#### D1: Gana-Only Registration
- Modify `run_mcp.py` `_register_prat_tools()` to register only 28 Gana-level descriptions
- Each Gana description includes a summary and the `tool` enum values, but NOT the full nested tool schemas

#### D2: On-Demand Schema Injection
- When a PRAT call specifies a `tool` parameter, JIT-load the specific tool's schema into the response context
- Cache loaded schemas for the session duration
- This reduces initial MCP registration payload from ~181 full schemas to 28 summaries

#### D3: Context Window Optimization
- Currently each Gana description lists all nested tools with full parameter schemas
- Restructure: top-level = Gana name + summary + tool list (names only)
- Detail-on-demand: agent queries `tool.graph` or `capability.matrix` for full schemas when needed

**Exit Criteria:**
- MCP PRAT registration uses <50% of current token budget
- First-call latency unchanged (JIT load is transparent)
- Agent can still discover all tools via `capability.matrix` or `tool.graph`

---

### Phase E: Iceoryx2 Foundation 🧊
**Effort:** ~1-2 sessions | **Impact:** HIGH — enables multi-process agent swarms
**Prerequisite:** Phase A + Phase B
**New dependency:** `iceoryx2` crate

This is the bridge from Leap 7 (intra-process) to Leap 8 (inter-process).

#### E1: Iceoryx2 Publisher/Subscriber for StateBoard
- Create `whitemagic-rust/src/ipc_state_board.rs`
- Publisher process: writes StateBoard updates via Iceoryx2 zero-copy loan
- Subscriber processes: receive StateBoard snapshots without copy
- Fallback: if Iceoryx2 unavailable, use current mmap file approach

#### E2: Arrow RecordBatch over Iceoryx2
- Publish Arrow Data Sea updates as Iceoryx2 samples
- Subscribers memory-map the Arrow data directly from shared memory
- No serialization, no copies — true "teleportation" per the paper

#### E3: Process Crash Cleanup
- Leverage Iceoryx2's built-in stale resource detection
- If an agent process crashes, shared memory segments are automatically cleaned
- Remaining agents continue operating without deadlock

#### E4: Security Model
- **Kernel processes** (Python main, Rust accelerators): read-write access to StateBoard + EventRing
- **LLM agent processes** (sandboxed): read-only access to StateBoard, subscribe-only to EventRing
- **Untrusted tool execution**: separate process with no shared memory access (communicates via MCP only)

**Exit Criteria:**
- StateBoard readable from a separate process via Iceoryx2
- Arrow Data Sea accessible cross-process without serialization
- Agent process crash doesn't affect main process
- Read-only vs read-write access enforced

---

### Phase F: Polymorphic Agent Scaffolding 🦎
**Effort:** ~1-2 sessions | **Impact:** TRANSFORMATIVE — enables million-agent scaling
**Prerequisite:** Phase C + Phase E

This is Leap 8c with concrete implementation guidance from the paper.

#### F1: Agent State Machine
```rust
pub enum AgentState {
    Monitor {
        watch_field: BoardOffset,
        threshold: f64,
        waker: EventRingConsumer,
    },                              // ~512 bytes
    Reasoner {
        context: Box<LLMContext>,
        tool_access: Vec<ToolId>,
    },                              // ~100KB
    Executor {
        pipeline: Vec<ToolId>,
        results: Vec<ToolResult>,
    },                              // ~10KB
}
```

#### F2: Monitor Swarm
- `spawn_monitors(count: usize, watch: BoardOffset, threshold: f64)`
- Each monitor: tokio task sleeping on EventRing via async poll
- On wake: read one StateBoard field, compare to threshold
- If triggered: transition to Reasoner state (hydrate LLM context via MCP)
- Target: 1M monitors in <512MB RAM

#### F3: Python Orchestration Layer
- Existing `clone_army.py`, `gana_swarm.py` become orchestrators
- They spawn Rust futures via PyO3, manage lifecycle, handle escalations
- Python handles "slow" decisions (LLM inference, tool selection)
- Rust handles "fast" decisions (monitoring, threshold checking, event routing)

#### F4: Memory Feasibility Verification
Per the paper's analysis:
- 1M monitors × 512 bytes = **512 MB** (feasible on single machine)
- 1% active reasoners = 10,000 × 100KB = **1 GB** (manageable)
- StateBoard + EventRing + Arrow Data Sea = **~50 MB** fixed
- Total: ~1.6 GB for 1M agents (fits in 16GB dev machine with room to spare)

**Exit Criteria:**
- Rust AgentState enum compiles with Monitor/Reasoner/Executor variants
- 10,000 monitor futures spawn and sleep efficiently
- Monitor→Reasoner transition works (EventRing wake → MCP hydration)
- Memory usage verified at <1KB per sleeping monitor

---

## Part 4: Architecture Evolution Diagram

```
CURRENT (v13.6):
┌─────────────────────────────────────────────────┐
│  Python Process                                  │
│  ┌──────────┐  ┌──────────┐  ┌───────────────┐ │
│  │ MCP      │  │ PRAT     │  │ Dispatch      │ │
│  │ (JSON)   │→ │ Router   │→ │ Pipeline (Py) │ │
│  └──────────┘  └──────────┘  └───────────────┘ │
│        ↕              ↕              ↕           │
│  ┌──────────┐  ┌──────────┐  ┌───────────────┐ │
│  │ Harmony  │  │ GanYing  │  │ 28 Engines    │ │
│  │ Vector   │  │ Bus (Py) │  │ (Python)      │ │
│  │ (Python) │  │          │  │               │ │
│  └──────────┘  └──────────┘  └───────────────┘ │
│        ↓              ↓                          │
│  ┌──────────┐  ┌──────────┐                     │
│  │ State    │  │ Event    │  ← Built but not    │
│  │ Board    │  │ Ring     │    primary path      │
│  │ (Rust)   │  │ (Rust)   │                     │
│  └──────────┘  └──────────┘                     │
└─────────────────────────────────────────────────┘

TARGET (v14.2):
┌─────────────────────────────────────────────────┐
│  Kernel Process (Rust + Python)                  │
│  ┌──────────┐     ┌───────────────┐             │
│  │ MCP      │     │ Zig Dispatch  │← Fast deny  │
│  │ (Control │     │ (Comptime)    │             │
│  │  Plane)  │     └───────┬───────┘             │
│  └────┬─────┘             ↓                      │
│       ↓           ┌───────────────┐             │
│  ┌──────────┐     │ PRAT Router   │             │
│  │ JIT      │     │ + Resonance   │             │
│  │ Schema   │     └───────┬───────┘             │
│  │ Loader   │             ↓                      │
│  └──────────┘     ┌───────────────┐             │
│                   │ Handler       │             │
│                   │ (Py/Rust)     │             │
│                   └───────┬───────┘             │
│                           ↓                      │
│  ┌─────────────────────────────────────────┐    │
│  │         Shared Memory (Data Plane)       │    │
│  │  ┌────────┐ ┌────────┐ ┌─────────────┐ │    │
│  │  │ State  │ │ Event  │ │ Arrow Data  │ │    │
│  │  │ Board  │ │ Ring   │ │ Sea         │ │    │
│  │  │ (mmap) │ │ (LMAX) │ │ (embeddings,│ │    │
│  │  │        │ │        │ │  contexts)  │ │    │
│  │  └────────┘ └────────┘ └─────────────┘ │    │
│  └──────────────┬──────────────────────────┘    │
│                 │ Iceoryx2 IPC                   │
└─────────────────┼───────────────────────────────┘
                  ↓
┌─────────────────────────────────────────────────┐
│  Agent Processes (Sandboxed, Read-Only)          │
│  ┌──────┐ ┌──────┐ ┌──────┐      ┌──────────┐ │
│  │ Mon. │ │ Mon. │ │ Mon. │ ×1M  │ Reasoner │ │
│  │ 512B │ │ 512B │ │ 512B │      │ (LLM)    │ │
│  └──────┘ └──────┘ └──────┘      └──────────┘ │
│  Rust tokio futures, sleeping on EventRing      │
│  Wake → read StateBoard → escalate via MCP      │
└─────────────────────────────────────────────────┘
```

---

## Part 5: What The Paper Gets Wrong (Our Advantages)

The research paper optimizes exclusively for **throughput and latency**. WhiteMagic's architecture reveals three dimensions the paper neglects:

### 1. Governance Is Not Optional
A million-agent swarm without ethical governance is a million-agent vulnerability. The paper mentions "read-only access for LLM agents" as the only security measure. WhiteMagic's 8-stage security pipeline (Input Sanitizer → Circuit Breaker → Rate Limiter → RBAC → Maturity Gate → Governor → Dharma Rules → Karma Audit) is not overhead — it's the reason agents should trust the swarm.

**Implication:** When building Iceoryx2 IPC, Dharma evaluation must gate write access. A process doesn't get write access just because it's "trusted" — it must pass Dharma clearance per action.

### 2. Memory Must Outlive The Ring
The paper's overwrite policy ("stale context is irrelevant context") works for sensor data but not for agent memory. WhiteMagic's Galactic Map ensures no memory is ever destroyed — just rotated outward. The EventRing handles ephemeral events; the Arrow Data Sea handles persistent context. Both are needed.

**Implication:** The Data Sea must be append-only for memories, overwrite for transient state. Two different Arrow files with different lifecycle policies.

### 3. Resonance > Raw Speed
The paper measures success in nanoseconds. WhiteMagic measures success in **coherence** — the degree to which the system's internal state reflects meaningful understanding, not just fast access. PRAT resonance context, Wu Xing elemental cycles, and Guna adaptation create a system that doesn't just process faster but *understands its own processing*.

**Implication:** Performance optimizations must preserve resonance metadata. The Arrow Data Sea schema must include resonance fields (predecessor_gana, wu_xing_phase, guna_state).

---

## Part 6: Implementation Timeline

| Phase | Estimated Effort | Dependencies | Deliverable |
|-------|-----------------|--------------|-------------|
| **A: Nervous System Wiring** | 1 session | Rust compiled | StateBoard/EventRing as primary path |
| **B: Arrow Data Sea** | 1 session | Phase A | Zero-copy embedding search |
| **C: Async Disruptor** | 0.5 session | Phase A | Waker-based EventRing consumers |
| **D: JIT Schema Loading** | 0.5 session | None | Lighter MCP PRAT registration |
| **E: Iceoryx2 Foundation** | 1-2 sessions | Phase A + B | Multi-process shared memory |
| **F: Polymorphic Agents** | 1-2 sessions | Phase C + E | Rust tokio agent futures |

**Recommended execution order for next session:** A → D → B → C (most impact per effort)

Phases E and F are Leap 8 territory — they can wait until v14.0 ships (Leap 6) unless we decide to accelerate.

---

## Part 7: Key Decision Points

### 1. Arrow vs Custom Binary for Data Sea?
**Decision: Arrow.** The StateBoard's custom binary format is perfect for its fixed 4KB layout (7 harmony fields, 64 breaker slots, 64 counters). But for extensible data (embeddings, engine outputs, agent contexts), Arrow wins on:
- Language-agnostic access (Rust, Python, Zig, Go all have Arrow libraries)
- SIMD-native columnar operations
- Ecosystem tooling (DuckDB, Polars, DataFusion can query Arrow directly)
- Schema evolution without breaking existing consumers

### 2. Iceoryx2 vs Raw mmap for IPC?
**Decision: Iceoryx2.** Raw mmap works for the current single-process model. But multi-process requires:
- Process crash detection and cleanup (Iceoryx2 provides this)
- Consumer lifecycle management (Iceoryx2 tracks subscribers)
- Zero-copy loan semantics (Iceoryx2's publish/subscribe model)
- The raw mmap StateBoard can be the fallback if Iceoryx2 is unavailable

### 3. When to transition from Python-primary to Rust-primary dispatch?
**Decision: Phase A is the bridge.** Python remains the orchestration layer (tool handlers, LLM integration, MCP protocol). Rust becomes the data plane (StateBoard, EventRing, Arrow Data Sea, rate limiting). Zig handles the pre-check fast path. This is not a Python→Rust migration; it's a separation of concerns:
- **Python:** Semantics, governance, handler logic
- **Rust:** Data storage, event propagation, agent lifecycle
- **Zig:** Pre-check pipeline, SIMD operations

### 4. Should Arrow Data Sea replace SQLite for embeddings?
**Decision: Complement, not replace.** SQLite remains the persistent store (ACID, transactions, complex queries). Arrow Data Sea is the hot cache — a materialized view of the most-accessed data, rebuilt on startup from SQLite. This mirrors the paper's insight: "the Blackboard is a living shared territory, not a database."

---

## Part 8: Non-Goals For This Strategy

- **Full WASM compilation** — Leap 10, deferred
- **GPU acceleration** — Leap 8b (Mojo megakernel), separate strategy
- **x402/XRPL integration** — Leap 5.5, independent track
- **Million-agent deployment** — Phase F scaffolds it; actual deployment needs infrastructure beyond scope
- **Replacing MCP with custom protocol** — MCP stays as control plane; we optimize what's behind it
- **Multi-machine distribution** — Iceoryx2 has network transport but we target single-machine first

---

## Appendix A: Paper Reference Summary

**Title:** "The Resonant Swarm: Architectural Feasibility of Hierarchical Tool Dispatch via Zero-Copy Shared State"

**Core Claims:**
1. JSON-RPC serialization creates a "serialization wall" at scale (~7,000ns/op)
2. LMAX Disruptor pattern achieves ~100ns publish via lock-free ring buffers
3. Shared memory (mmap) is 11× faster than Unix sockets for IPC
4. Apache Arrow enables O(1) zero-copy data access
5. Iceoryx2 adapts LMAX for cross-process IPC with crash safety
6. 1M agents feasible at ~512 bytes/agent via Rust tokio futures
7. Hybrid architecture: MCP for control, shared memory for data

**Relevance to WhiteMagic:** Claims 1-3 validate Leap 7 architecture. Claims 4-5 inform Phases B and E. Claims 6-7 validate Leap 8 design.

---

## Appendix B: Benchmark Targets

| Metric | Current (Python) | Current (Rust/Zig) | Target (After Phase A) | Paper Reference |
|--------|-----------------|-------------------|----------------------|-----------------|
| StateBoard read | ~5-50µs (dict) | 2.3µs (mmap) | <1µs (direct) | <0.1µs (§5.1) |
| Event publish | ~100µs (Redis) | 387ns (ring) | <500ns (primary) | ~100ns (§3) |
| Dispatch pre-check | ~40µs (8 Py calls) | 1.6µs (Zig) | <2µs (authoritative) | <2µs (§7c) |
| Embedding search | 629ms (SQLite warm) | N/A | <50ms (Arrow SIMD) | <10ms (§4.2.2) |
| Agent memory footprint | ~10MB (Python) | N/A | ~512B (Rust future) | ~512B (§6.1) |
| Event throughput | ~10K/sec (Redis) | 2.58M/sec (ring) | >1M/sec (primary) | >1M/sec (§3) |

---

*This document serves as the execution plan for integrating the Resonant Swarm research into WhiteMagic's Leap 7-8 roadmap. It should be read alongside `docs/STRATEGIC_ROADMAP.md` for the full v14.0+ context.*
