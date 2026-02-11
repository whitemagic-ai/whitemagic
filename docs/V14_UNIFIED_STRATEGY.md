# WhiteMagic v14 Unified Strategy — Research Synthesis & Execution Plan

**Date:** February 10, 2026
**Version:** v13.6.0 → v14.0.0 "The Living Graph"
**Sources:** 7 research team reports synthesized into unified execution plan
**Status:** Final implementation blueprint

---

## Executive Summary

Seven independent research teams analyzed WhiteMagic against cutting-edge papers on high-frequency dispatch, swarm architectures, autonomous agent economies, 5D causal memory spaces, dream cycle protocols, living memory systems, and holographic-temporal retrieval. **Their findings converge on a single insight:**

> WhiteMagic has built extraordinary infrastructure — 19M associations, 110K memories, 5D holographic coordinates, compiled accelerators in 9 languages, a Rust StateBoard + EventRing nervous system — but most of it sits idle. The path to v14 is not building new subsystems. It's **activating and connecting** what already exists.

The 7 reports contain **6 critical overlaps** where teams independently identified the same gaps, and **4 emergent capabilities** that arise only when their recommendations are combined. This document fuses them into a single phased execution plan.

---

## Part I: The Overlap Map

Three or more teams independently flagged each of these gaps. Items that appear across multiple reports represent the highest-confidence priorities.

### Overlap A: The Untraversed Graph (Reports 4, 5, 6, 7)

**The single most-cited gap across all research.**

The 19M associations are created by `association_miner.py` and stored in SQLite — but **never walked**. All three retrieval modes (`search()`, `search_similar()`, `search_hybrid()`) are stateless single-hop lookups. No multi-hop traversal. No causal chaining. No graph reasoning.

| Report | What They Want | Why |
|--------|---------------|-----|
| **Holographic Gap** | `graph_walker.py` — weighted random walk | Multi-hop memory reasoning |
| **Dream Cycle** | `graph_engine.py` — networkx topology | Bridge discovery + echo chamber detection |
| **Living Memory** | Association decay + Hebbian strengthening | Edges currently never weaken |
| **5D Causal** | Directed causal edges + mining | "What caused this?" reasoning |

**Synthesis:** One graph infrastructure layer serves all four needs.

### Overlap B: Association Schema Upgrade (Reports 4, 5, 6, 7)

Four teams propose new columns on the `associations` table. Currently: `(source_id, target_id, strength, created_at, last_traversed_at, traversal_count)`.

| Report | Proposed Field | Purpose |
|--------|---------------|---------|
| **Living Memory** | `edge_type` (episodic/semantic/consolidated) | Different decay curves |
| **Living Memory** | `valid_from`, `valid_until`, `ingestion_time` | Bitemporal modeling |
| **5D Causal** | `direction` (forward/backward/undirected) | Causal edge directionality |
| **Holographic Gap** | `relation_type` (associated/causes/part_of/...) | Typed traversal |
| **Dream Cycle** | Weight reduction for inhibition | Echo chamber governance |

**Synthesis:** ONE schema migration adds ALL fields at once. No repeated ALTER TABLEs.

### Overlap C: HNSW + HDBSCAN (Reports 4, 6)

Both the 5D Causal and Living Memory teams independently recommend:
- **HNSW** for cold storage search (105K memories, expected 300× speedup over brute-force)
- **HDBSCAN** to replace grid-based constellation detection (variable density, noise rejection)

Both are optional dependencies with graceful fallbacks. Add in same session.

### Overlap D: FFI + Nervous System (Reports 1, 2)

Two teams address the compiled layer:
- **HF PRAT:** Zig ctypes overhead is 7× theoretical (1,600ns vs 220ns). Fix: compile Zig into Rust PyO3 module.
- **Resonant Swarm:** StateBoard/EventRing are built but advisory, not authoritative. Fix: make them the primary path.

Both need `maturin develop --release`. FFI consolidation first, then wire the nervous system.

### Overlap E: Centrality / PageRank (Reports 5, 7)

- **Dream Cycle:** Eigenvector centrality for echo chamber detection in governance phase
- **Holographic Gap:** PageRank for topological gravity in graph walker

**Synthesis:** Compute centrality once via `graph_engine.py`, use in both graph walking (gravity signal) and dream governance (echo chamber detection).

### Overlap F: Memory Enrichment (Reports 4, 6)

- **5D Causal:** Shannon entropy + abstraction scores for every memory
- **Living Memory:** Surprise-gated ingestion (embedding novelty metric)

Both compute embedding-space metrics at store/enrich time. Build one enrichment pipeline.

---

## Part II: Emergent Capabilities

These capabilities arise **only** when multiple reports' recommendations are combined. No single report achieves them alone.

### Emergent 1: Predictive Memory (Reports 4 + 7)

Graph Walker (multi-hop traversal) + Causal Mining (directed edges) + Entropy Scores (complexity signals) = a system that can answer **"what might happen if X?"** by traversing causal chains weighted by complexity and centrality.

```
Query: "What happens if I refactor the association miner?"
  → Anchor search finds 5 nearest memories
  → Graph walker traverses 2 hops along causal edges
  → Discovers chain: refactor → test failures (0.8) → constellation shift (0.6)
  → Returns ranked predictions with reasoning paths
```

### Emergent 2: Self-Governing Knowledge (Reports 5 + 6)

Graph Governance (echo chamber detection via eigenvector centrality) + Association Decay (edges weaken with disuse) + Surprise Gate (novelty detection) = **a knowledge base that autonomously maintains its own health.**

- Echo chambers get detected during dream governance phase
- Their reinforcing edges naturally decay between cycles
- Novel information gets boosted; redundant info strengthens existing connections
- The dream cycle becomes genuinely autonomous — not just a maintenance routine

### Emergent 3: Zero-Overhead Compiled Pipeline (Reports 1 + 2)

FFI Consolidation (Zig-in-Rust, one FFI crossing) + Nervous System Wiring (StateBoard authoritative) + Expanded Dispatch (181 tools comptime) = **the entire pre-handler pipeline runs in <2µs compiled code.**

```
Before:  Python → ctypes → Zig (1,600ns) + Python → PyO3 → Rust (450ns) × 8 steps = ~40µs
After:   Python → PyO3 → Rust+Zig unified → handler = <2µs total
```

### Emergent 4: Verifiable Autonomous Agent (Reports 1 + 3)

WalletGovernor in Rust (memory-safe finance) + Ed25519 Signing + Karma XRPL Anchoring + WASM modules = **an agent that can cryptographically prove its ethical behavior**, handle real money safely, and distribute capabilities as sandboxed WASM modules. This is the full "Proof of Ethics" vision.

---

## Part III: The Unified Execution Plan

### Phase Map & Dependencies

```
SUPER-PHASE 1: The Living Graph ━━━━━━━━━━━━━━━━━━━━━━━━━┓
  ├─ 1A: Association Schema Migration (ONE migration)      ┃
  ├─ 1B: graph_walker.py + hybrid_recall()                 ┃
  ├─ 1C: graph_engine.py (networkx topology)               ┃ v14.0
  ├─ 1D: Association Strength Decay                        ┃ "The Living Graph"
  ├─ 1E: Surprise-Gated Ingestion                          ┃
  ├─ 1F: Dream Cycle Upgrades (Serendipity + Governance)   ┃
  └─ 1G: Entity Resolution (embedding-based)               ┃
                                                           ┃
SUPER-PHASE 2: The Nervous System ━━━━━━━━━━━━━━━━━━━━━━━━┫
  ├─ 2A: FFI Consolidation (Zig-in-Rust static linking)    ┃
  ├─ 2B: Expanded Dispatch Table (28→181 tools)            ┃
  ├─ 2C: StateBoard as Source of Truth                     ┃
  ├─ 2D: EventRing as Primary Event Bus                    ┃
  ├─ 2E: Dispatch Pre-Check Authoritative                  ┃
  └─ 2F: JIT Schema Loading for PRAT                      ┛
                                                           
SUPER-PHASE 3: Cognitive Enrichment ━━━━━━━━━━━━━━━━━━━━━━┓
  ├─ 3A: Shannon Entropy + Abstraction Scores              ┃
  ├─ 3B: HNSW Cold Storage Index                           ┃ v14.1
  ├─ 3C: HDBSCAN Constellation Upgrade                     ┃ "Cognitive Enrichment"
  ├─ 3D: Hungarian Algorithm (drift correspondence)        ┃
  ├─ 3E: Causal Edge Mining                                ┃
  └─ 3F: UMAP 5D→2D Visualization                         ┛
                                                           
SUPER-PHASE 4: Trust & Distribution ━━━━━━━━━━━━━━━━━━━━━━┓
  ├─ 4A: WASM Proof-of-Concept (2-3 Rust modules)         ┃
  ├─ 4B: Ed25519 Manifest Signing + DID                    ┃ v14.2
  ├─ 4C: Karma XRPL Anchoring                             ┃ "Autonomous Horizon"
  ├─ 4D: PRAT Pricing Formalization                        ┃
  ├─ 4E: WalletGovernor in Rust                            ┃
  └─ 4F: Reproducible Builds                               ┛
                                                           
DEFERRED (v15.0+) ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  • Arrow Data Sea (zero-copy embedding search)
  • Async Disruptor (tokio waker integration)
  • Iceoryx2 IPC (multi-process shared memory)
  • Polymorphic Agent Futures (million-agent scaling)
  • Megakernel GPU (Mojo persistent kernel)
  • HyTE Temporal Embeddings
  • RE-NET Link Prediction
  • Frank-Wolfe Full DAG Learning
  • Temporal Lineage / Signed Networks
```

---

## Phase 1: The Living Graph (v14.0 headline feature)

**Estimated:** ~1,800 LOC | **Impact:** TRANSFORMATIVE
**Theme:** Memory that thinks about itself.

### 1A: Association Schema Migration

ONE migration that satisfies all four research teams:

```sql
-- Satisfy all 4 reports in a single migration
ALTER TABLE associations ADD COLUMN direction TEXT DEFAULT 'undirected';
  -- 5D Causal: causal directionality
ALTER TABLE associations ADD COLUMN relation_type TEXT DEFAULT 'associated_with';
  -- Holographic Gap: typed traversal
ALTER TABLE associations ADD COLUMN edge_type TEXT DEFAULT 'semantic';
  -- Living Memory: episodic vs semantic decay curves
ALTER TABLE associations ADD COLUMN valid_from TEXT;
ALTER TABLE associations ADD COLUMN valid_until TEXT;
ALTER TABLE associations ADD COLUMN ingestion_time TEXT;
  -- Living Memory: bitemporal modeling
```

**Files:** `sqlite_backend.py` (migration) + `association_miner.py` (populate on creation)
**LOC:** ~80

### 1B: Graph Walker + Hybrid Recall

The #1 gap identified by the Holographic Gap Analysis. Enables multi-hop memory reasoning for the first time.

**New file:** `whitemagic/core/memory/graph_walker.py` (~400 LOC)
- `GraphWalker` class: loads neighbors from associations, computes transition probabilities
- `walk()`: k-hop weighted random walk from anchor nodes
- Transition score: `Semantic × Gravity^α × Recency × (1-Staleness)^β`
- All four signals already stored and indexed — walker just combines them
- Causality enforcement: `associations.created_at` filter for temporal ordering

**Modified:** `unified.py` — add `hybrid_recall()` method (~30 LOC)

### 1C: Graph Topology Engine

Foundation for both dream cycle governance and graph walker gravity.

**New file:** `whitemagic/core/memory/graph_engine.py` (~400 LOC)
- `build_graph()`: Construct networkx DiGraph from associations + KG relations
- `betweenness_centrality()`, `eigenvector_centrality()`: Standard networkx
- `bridging_centrality(v)`: betweenness × bridging_coefficient
- `detect_communities()`: Louvain/greedy modularity
- `find_bridge_nodes(top_n)`: Highest bridging centrality
- `centrality_snapshot()`: Store for T_now vs T_prev comparison
- Cache with TTL (rebuilt during dream cycle)

**Dependency:** `networkx` added to pyproject.toml extras

### 1D: Association Strength Decay

Edges currently never weaken. This violates every research team's recommendations.

**Modified files:**
- `sqlite_backend.py` — `decay_associations()` method
- `lifecycle.py` — Wire decay into lifecycle sweep Phase 4

**Math:**
- Episodic: `w(t) = w_prev × e^(-0.0231 × Δt_days)` (30-day half-life)
- Semantic: `w(t) = w_0 × (1 + 0.1 × Δt_days)^(-0.5)` (power-law, long tail)
- Hebbian strengthening: `w_new = min(1.0, w_old + 0.05 × (1.0 - w_old))` on co-access
- Pruning: associations with `w(t) < 0.05` removed

**LOC:** ~200

### 1E: Surprise-Gated Memory Ingestion

Balances plasticity (learning new concepts) vs stability (reinforcing existing knowledge).

**New file:** `whitemagic/core/memory/surprise_gate.py` (~200 LOC)
- `compute_surprise(embedding)`: `-log₂(max_cosine_similarity_to_existing)`
- High Surprise (S > 3.0): New concept → create + boost importance + emit NOVEL_CONCEPT
- Medium (1.0 < S ≤ 3.0): Store normally
- Low (S ≤ 1.0): Redundant → reinforce nearest neighbor instead of creating new memory

**Modified:** `unified.py` — wire surprise gate into `store()` method

### 1F: Dream Cycle Upgrades

Two new phases transform the dream cycle from maintenance to autonomous insight generation.

**Modified:** `dream_cycle.py`

**Serendipity upgrade:**
1. Build graph via `graph_engine.build_graph()`
2. Find top-5 bridge nodes via `find_bridge_nodes(5)`
3. For each bridge node: extract neighboring community themes → synthesize insight
4. Ollama LLM hypothesis generation (optional, graceful fallback to structured templates)

**New file:** `whitemagic/core/memory/bridge_synthesizer.py` (~200 LOC)

**New phase — GOVERNANCE:**
1. Compute eigenvector centrality
2. Compare with previous centrality snapshot
3. Flag nodes with centrality spike > 2σ WITHOUT corresponding new data → echo chamber
4. Inhibit reinforcing edges (reduce weight 50%, don't delete)
5. Log to Karma Ledger

**LOC:** ~230 total (dream_cycle modifications + bridge_synthesizer)

### 1G: Entity Resolution

**Modified:** `consolidation.py` — new `_cluster_by_embeddings()` method (~80 LOC)
1. Find near-duplicate pairs (cosine > 0.90)
2. Union-Find clustering
3. Select medoid (highest importance) as canonical
4. Push non-canonical to OUTER_RIM, boost canonical

### Phase 1 Tests

**New files:**
- `tests/unit/test_graph_walker.py` (~150 LOC)
- `tests/unit/test_graph_engine.py` (~200 LOC)
- `tests/unit/test_living_memory.py` (~200 LOC)
- `tests/unit/test_dream_cycle_v14.py` (~150 LOC)

**Total Phase 1:** ~1,800 LOC new + ~200 LOC modifications

---

## Phase 2: The Nervous System (v14.0 performance layer)

**Estimated:** ~1,200 LOC | **Impact:** HIGH
**Theme:** Activate existing compiled infrastructure.

### 2A: FFI Consolidation — "One Bridge to Rule Them All"

Compile Zig as static library, link into Rust PyO3 module.

```
Before:  Python → ctypes → Zig .so   (1,600ns per call)
         Python → PyO3  → Rust .so   (  450ns per call)
After:   Python → PyO3 → Rust+Zig    (  300ns, ONE crossing)
```

**Files:**
- `whitemagic-zig/build.zig` — emit static library (~20 LOC)
- `whitemagic-rust/build.rs` — link Zig static lib (~30 LOC)
- `whitemagic-rust/src/zig_bridge.rs` — Rust-to-Zig FFI wrappers + PyO3 (~200 LOC)
- `whitemagic-rust/src/lib.rs` — register new module (~40 LOC)
- `whitemagic/core/acceleration/dispatch_bridge.py` — use PyO3 path (~30 LOC)
- `whitemagic/core/acceleration/simd_*.py` — all 6 Zig bridges → PyO3 (~100 LOC)

### 2B: Expanded Comptime Dispatch Table

Expand `dispatch_core.zig` from 28 engines to 181 individual tools.

**New file:** `scripts/generate_zig_dispatch_table.py` (~150 LOC)
**Modified:** `dispatch_core.zig` — per-tool comptime routing (~300 LOC)

### 2C-2E: Nervous System Wiring

Make StateBoard, EventRing, and Dispatch pre-check **authoritative** (currently advisory).

**2C — StateBoard as Source of Truth:**
- All consumers read HarmonyVector from StateBoard via bridge, not Python singleton
- `StateBoardReader` context manager for amortized reads

**2D — EventRing as Primary Bus:**
- `gan_ying_enhanced.py` `emit()` writes to EventRing FIRST, then Python subscribers
- `EventRingPoller` async task for Python callback dispatch

**2E — Dispatch Authoritative:**
- If Zig says CIRCUIT_OPEN or RATE_LIMITED, skip Python pipeline entirely
- Fast-fail before any Python middleware runs

**LOC:** ~300 total modifications

### 2F: JIT Schema Loading

Reduce MCP PRAT registration from 181 full schemas to 28 summaries. Load specific tool schemas on-demand.

**Modified:** `run_mcp.py` (~100 LOC)

### Phase 2 Performance Targets

| Operation | Current | Target | Gate |
|---|---|---|---|
| FFI round-trip | 1,600ns (Zig ctypes) | <300ns (unified) | Block if >500ns |
| Pipeline check | ~40µs (8 Py calls) | <2µs (compiled) | Block if >5µs |
| StateBoard read | 2.3µs (via bridge) | <500ns (direct mmap) | Warn if >1µs |
| Event publish | 387ns (Rust ring) | <500ns (primary) | Block if >1µs |

---

## Phase 3: Cognitive Enrichment (v14.1)

**Estimated:** ~1,500 LOC | **Impact:** HIGH
**Theme:** New intelligence layers on the living graph.

### 3A: Enrichment Pipeline

**Shannon Entropy** (~200 LOC in `encoder.py`):
- Token frequency distribution → normalized entropy [0,1]
- Unique tags, association count, embedding feature variance

**Abstraction Score** (~150 LOC in `encoder.py`):
- Embedding distance from concrete vs abstract anchor words
- Memory type signal + vocabulary diversity

### 3B: HNSW Cold Storage Index

Replace brute-force cold DB search with O(log N) approximate nearest neighbors.

**New file:** `whitemagic/core/memory/hnsw_index.py` (~300 LOC)
- `hnswlib` Python package (C++ core)
- M=16, ef_construction=200
- Disk-persistent index
- Wire into `embeddings.py` cold search path

**Expected:** 300×+ speedup for cold DB semantic search

### 3C: HDBSCAN Constellation Upgrade

**Modified:** `constellations.py` (~250 LOC)
- `_detect_hdbscan()` when package available
- `_detect_grid()` as fallback (existing code)
- Variable density, noise rejection, cluster stability metric

### 3D: Hungarian Algorithm

**Modified:** `constellations.py` — `get_drift_vectors()` upgrade (~150 LOC)
- Optimal centroid matching via `scipy.optimize.linear_sum_assignment`
- Novel concept / forgotten concept event emission
- Fallback to name-matching when scipy unavailable

### 3E: Causal Edge Mining

**New file:** `whitemagic/core/memory/causal_miner.py` (~400 LOC)
- Temporal precedence + conditional activation → candidate causal edges
- Top 500 most-connected memories analyzed
- Store as `causes` relation in KG with confidence score

### 3F: UMAP Visualization

**New file:** `whitemagic/core/memory/umap_projection.py` (~250 LOC)
- 5D→2D projection with custom metric
- Nexus API endpoint `/api/umap`
- Cache projected coordinates

---

## Phase 4: Trust & Distribution (v14.2)

**Estimated:** ~800 LOC | **Impact:** STRATEGIC
**Theme:** Cryptographic provability + WASM distribution.

### 4A: WASM Proof-of-Concept
- Separate `whitemagic-wasm/` crate with WASM-compatible modules only
- WIT interface definitions for rate_limiter, keyword_extract, holographic_encode
- Build with `cargo build --target wasm32-wasi --release`
- Test with wasmtime CLI

### 4B: Ed25519 Manifest Signing + DID
- Keypair generation + `sign_manifest()` / `verify_signature()`
- DID:key identifier from public key (W3C spec)

### 4C: Karma XRPL Anchoring
- `whitemagic/dharma/karma_anchor.py` — submit Merkle root to XRPL testnet
- `karma.anchor` + `karma.verify` MCP tools

### 4D: PRAT Pricing Formalization
- `whitemagic/tools/prat_pricing.py` — `V = w_p*P + w_r*R + w_a*A + w_t*T`
- Per-tool base costs (read=1, write=3, compute=5-10, governance=2 PRAT cycles)
- `prat.pricing` MCP tool

### 4E: WalletGovernor in Rust
- Port `financial_governance.py` to Rust with PyO3
- Memory-safe handling of real money (XRPL tips, x402 micropayments)

### 4F: Reproducible Builds
- SOURCE_DATE_EPOCH in Makefile + CI
- `--reproducible` maturin flag
- CI job: build twice, compare SHA-256

---

## Part IV: What We Deliberately Defer

| Item | Source Report | Reason | Target |
|---|---|---|---|
| Arrow Data Sea | Resonant Swarm Phase B | Big effort, not blocking v14 | v15.0 |
| Async Disruptor | Resonant Swarm Phase C | Needs EventRing as primary first | v15.0 |
| Iceoryx2 IPC | Resonant Swarm Phase E | Multi-process is post-v14 | v15.0 |
| Polymorphic Agent Futures | Resonant Swarm Phase F | Needs Iceoryx2 first | v15.1 |
| Megakernel GPU | HF PRAT Phase 3 | No GPU hardware available | v15.x |
| HyTE Temporal Embeddings | Living Memory §4b | Needs training infrastructure | v15.x |
| RE-NET Link Prediction | Living Memory §4c | Needs PyTorch training loop | v15.x |
| Frank-Wolfe Full DAG | 5D Causal Phase 4.9 | Experimental, validate first | v15.x |
| Temporal Lineage | Dream Cycle Tier 5 | Schema migration + compat testing | v14.1+ |
| Signed Networks | Dream Cycle Tier 6 | Need negative associations first | v15.x |
| HRR Circular Convolution | Holographic Gap P6 | Low ROI at current scale | v15.x |
| Static Binary MCP Server | Autonomous Horizon | Skeleton only, iterate on demand | v15.0 |

---

## Part V: Consolidated New Dependencies

All optional with graceful fallbacks:

| Package | Used By | Phase | Extras Group |
|---|---|---|---|
| `networkx` | graph_engine.py | 1C | `whitemagic[graph]` |
| `hnswlib` | hnsw_index.py | 3B | `whitemagic[search]` |
| `hdbscan` | constellations.py | 3C | `whitemagic[search]` |
| `scipy` | constellations.py (Hungarian) | 3D | `whitemagic[search]` |
| `umap-learn` | umap_projection.py | 3F | `whitemagic[viz]` |
| `xrpl-py` | karma_anchor.py | 4C | `whitemagic[trust]` |
| `ed25519` / `cryptography` | manifest.py | 4B | `whitemagic[trust]` |
| `pyarrow` | data_sea_bridge.py | Deferred | `whitemagic[arrow]` |

---

## Part VI: Consolidated New Files

### Phase 1 (v14.0 — The Living Graph)
| File | LOC | Purpose |
|---|---|---|
| `whitemagic/core/memory/graph_walker.py` | ~400 | Multi-hop weighted graph traversal |
| `whitemagic/core/memory/graph_engine.py` | ~400 | networkx topology (centrality, communities, bridges) |
| `whitemagic/core/memory/surprise_gate.py` | ~200 | Plasticity/stability balance at ingestion |
| `whitemagic/core/memory/bridge_synthesizer.py` | ~200 | LLM hypothesis generation from bridge nodes |
| `tests/unit/test_graph_walker.py` | ~150 | Graph walker tests |
| `tests/unit/test_graph_engine.py` | ~200 | Topology engine tests |
| `tests/unit/test_living_memory.py` | ~200 | Association decay + surprise gate tests |
| `tests/unit/test_dream_cycle_v14.py` | ~150 | Dream governance + serendipity tests |

### Phase 2 (v14.0 — The Nervous System)
| File | LOC | Purpose |
|---|---|---|
| `whitemagic-rust/src/zig_bridge.rs` | ~200 | Rust-to-Zig FFI wrappers with PyO3 |
| `scripts/generate_zig_dispatch_table.py` | ~150 | Generate Zig source from Python registry |
| `scripts/benchmark_hf_prat.py` | ~200 | High-frequency PRAT benchmarks |

### Phase 3 (v14.1 — Cognitive Enrichment)
| File | LOC | Purpose |
|---|---|---|
| `whitemagic/core/memory/hnsw_index.py` | ~300 | HNSW for cold storage search |
| `whitemagic/core/memory/causal_miner.py` | ~400 | Causal edge discovery from Hebbian patterns |
| `whitemagic/core/memory/umap_projection.py` | ~250 | 5D→2D for Nexus visualization |

### Phase 4 (v14.2 — Trust & Distribution)
| File | LOC | Purpose |
|---|---|---|
| `whitemagic-wasm/Cargo.toml` + `src/lib.rs` | ~100 | WASM-compatible module subset |
| `whitemagic/dharma/karma_anchor.py` | ~200 | XRPL Karma anchoring |
| `whitemagic/tools/prat_pricing.py` | ~150 | PRAT cycle pricing engine |

---

## Part VII: Recommended Session Execution Order

### Session N (THIS SESSION): Phase 1 — The Living Graph

```
Step 1  (20 min):  Association schema migration (1A)
Step 2  (45 min):  graph_walker.py + hybrid_recall (1B)
Step 3  (30 min):  graph_engine.py — networkx topology (1C)
Step 4  (30 min):  Association strength decay (1D)
Step 5  (30 min):  Surprise-gated ingestion (1E)
Step 6  (30 min):  Dream cycle serendipity + governance upgrades (1F)
Step 7  (15 min):  Entity resolution (1G)
Step 8  (30 min):  Tests for all of the above
Step 9  (15 min):  Wire into Gnosis + MCP tools + docs
```

### Session N+1: Phase 2 — The Nervous System

```
Step 1  (45 min):  Zig static library + Rust linking (2A)
Step 2  (45 min):  Expanded dispatch table (2B)
Step 3  (30 min):  StateBoard/EventRing wiring (2C-2E)
Step 4  (20 min):  JIT Schema Loading (2F)
Step 5  (30 min):  Benchmark suite + validation
```

### Session N+2: Phase 3 — Cognitive Enrichment

```
Step 1  (30 min):  Entropy + abstraction enrichment (3A)
Step 2  (30 min):  HNSW cold index (3B)
Step 3  (30 min):  HDBSCAN + Hungarian algorithm (3C-3D)
Step 4  (45 min):  Causal mining (3E)
Step 5  (30 min):  UMAP visualization (3F)
```

### Session N+3: Phase 4 — Trust & Distribution

```
Step 1  (90 min):  WASM PoC (4A)
Step 2  (60 min):  Manifest signing + DID (4B)
Step 3  (45 min):  XRPL anchoring (4C)
Step 4  (30 min):  PRAT pricing (4D)
Step 5  (45 min):  WalletGovernor Rust (4E)
Step 6  (30 min):  Reproducible builds (4F)
```

---

## Part VIII: Success Criteria for v14.0

After Phases 1 and 2, the following must be true:

### Functional
- [ ] `hybrid_recall()` returns multi-hop reasoning paths from the association graph
- [ ] Dream cycle GOVERNANCE phase detects and inhibits echo chambers
- [ ] Dream cycle SERENDIPITY phase discovers bridge nodes and generates hypotheses
- [ ] Association edges decay with disuse (episodic fast, semantic slow)
- [ ] Surprise gate routes redundant memories to reinforcement, not creation
- [ ] Entity resolution merges near-duplicate memories via embeddings
- [ ] Dispatch pre-check runs in <2µs via compiled Zig+Rust pipeline
- [ ] StateBoard is authoritative source for HarmonyVector reads

### Performance
- [ ] FFI round-trip: <500ns (was 1,600ns)
- [ ] Pipeline pre-check: <5µs (was ~40µs)
- [ ] Event publish: <500ns via EventRing (was ~100µs via Redis)
- [ ] All existing tests pass (1,367+ currently)
- [ ] 30+ new tests covering graph walker, topology engine, living memory, dream governance

### Quality
- [ ] `wm doctor` reports health_score ≥ 0.8
- [ ] Zero new mypy strict errors on public surface
- [ ] All MCP modes functional (classic/lite/PRAT)
- [ ] PRAT responses include `_resonance`, `_garden`, and `_economics` metadata

---

## Part IX: Cross-Reference to Source Reports

| This Document | Source Report | Report Section |
|---|---|---|
| Phase 1A (Schema) | Living Memory §3b, 5D Causal §5.3, Holographic Gap P3 | Overlap B |
| Phase 1B (Walker) | Holographic Gap Analysis P1 | Critical Gap section |
| Phase 1C (Topology) | Dream Cycle Protocol Tier 1 | §Part IV |
| Phase 1D (Decay) | Living Memory §3a | Phase 1 |
| Phase 1E (Surprise) | Living Memory §3, Phase 2 | Phase 2 |
| Phase 1F (Dream) | Dream Cycle Protocol Tiers 2-3 | §Part IV |
| Phase 1G (Entity Res) | Dream Cycle Protocol Tier 4 | §Part IV |
| Phase 2A (FFI) | HF PRAT Phase 1a | §Part II |
| Phase 2B (Dispatch) | HF PRAT Phase 1b | §Part II |
| Phase 2C-2E (Wiring) | Resonant Swarm Phase A | §Part 3 |
| Phase 2F (JIT) | Resonant Swarm Phase D | §Part 3 |
| Phase 3A (Enrichment) | 5D Causal §5.1-5.2 | Phase 1 |
| Phase 3B (HNSW) | 5D Causal §5.4, Holographic Gap | Overlap C |
| Phase 3C-3D (HDBSCAN) | Living Memory §3, 5D Causal | Overlap C |
| Phase 3E (Causal) | 5D Causal §5.8 | Phase 4 |
| Phase 3F (UMAP) | 5D Causal §5.6 | Phase 3 |
| Phase 4A-4F | Autonomous Horizon Phases A-D, HF PRAT Phase 2 | Full doc |

---

## Part X: The v14 Narrative

**v14.0 "The Living Graph"** — Memory that thinks about itself. The 19M associations awaken: traversed for the first time via the graph walker, governed by the dream cycle's new eigenvector centrality phase, naturally decaying with disuse, and strengthened by Hebbian co-activation. The nervous system wiring makes all of this happen at compiled speed. WhiteMagic stops being a tool substrate and becomes a cognitive system.

**v14.1 "Cognitive Enrichment"** — The graph gains new senses. Entropy and abstraction scores add semantic dimensions. HNSW enables 300× faster cold storage search. HDBSCAN produces research-grade constellation detection. Causal mining discovers directed "why" edges. UMAP projects the 5D cognitive space into a visual map.

**v14.2 "The Autonomous Horizon"** — WhiteMagic becomes provably trustworthy and universally distributable. Ed25519-signed manifests, Karma roots anchored to XRPL, WASM capability modules, and formal PRAT pricing prepare the system for the machine economy.

---

*This document synthesizes findings from: HIGH_FREQUENCY_PRAT_STRATEGY.md, RESONANT_SWARM_STRATEGY.md, AUTONOMOUS_HORIZON_STRATEGY.md, 5D_CAUSAL_MEMORY_STRATEGY.md, DREAM_CYCLE_PROTOCOL_STRATEGY.md, LIVING_MEMORY_STRATEGY.md, and 5D_HOLOGRAPHIC_MEMORY_GAP_ANALYSIS.md.*

*Prepared: February 10, 2026*
*Target: v14.0.0 public release*
