# 5D Holographic-Temporal Memory: Gap Analysis
## Mapping the Paper's Architecture to WhiteMagic's Current State

*Generated: 2026-02-10*
*Paper: "The Architecture of 5D Holographic-Temporal Memory"*
*Codebase: WhiteMagic v13.6.0*

---

## Executive Summary

WhiteMagic already implements **~70% of the paper's infrastructure** — the raw materials are in place. The critical missing piece is the **graph walker**: the 19M associations are created and stored but **never traversed for multi-hop reasoning**. The existing `search_hybrid()` implements RRF (which the paper explicitly identifies as insufficient for causal chaining), while no GGVS equivalent exists.

**Bottom line**: One new module (`graph_walker.py`) + one method addition to `unified.py` + minor schema enrichment can close the gap. No new subsystems needed.

---

## Dimension-by-Dimension Mapping

### Dimension 1: Semantic Vector (v) — 80% Complete

| Paper Concept | WhiteMagic Equivalent | Status |
|---|---|---|
| Dense embedding (R^1024) | `embeddings.py` MiniLM-L6-v2 (R^384) | ✅ Exists |
| Vector index (FAISS/HNSW) | In-memory numpy cache, pre-normalized, BLAS matmul | ✅ Exists |
| Cosine similarity search | `search_similar()` with Zig SIMD fast path | ✅ Exists |
| HRR circular convolution (A ⊗ B) | — | ❌ Missing |
| Role binding/unbinding | — | ❌ Missing |
| "Virtual target" projection (v_next = v_A ⊗ r) | — | ❌ Missing |

**Where it lives**:
- `whitemagic/core/memory/embeddings.py` — 384-dim sentence embeddings, hot+cold DB
- `whitemagic/core/memory/vector_search.py` — SBERT + TF-IDF fallback
- `whitemagic/core/intelligence/hologram/encoder.py` — 5D coordinate encoder (XYZWV), **not** a semantic vector

**Gap**: HRR circular convolution doesn't exist. The paper uses it to "look ahead" one hop during traversal — predicting the ideal next vector via concept binding.

**Recommendation**: **Skip HRR for now.** The ROI is low at this scale. Instead, use the existing embedding cosine similarity as the semantic steering signal during graph walks. The embedding of the query text naturally guides traversal toward semantically relevant neighbors. If multi-hop reasoning quality is insufficient, HRR can be added later as a `graph_walker` plugin.

---

### Dimension 2: Temporal (t) — 60% Complete

| Paper Concept | WhiteMagic Equivalent | Status |
|---|---|---|
| Continuous timestamp per node | `memories.created_at`, `memories.accessed_at` | ✅ Exists |
| Edge timestamp (t_ij) | `associations.created_at` column | ✅ Exists (migration added) |
| Recency bias (exp decay) | `mindful_forgetting._recency_signal()` with half-life | ✅ Exists |
| Causality enforcement (t_A→B ≤ t_B→C) | — | ❌ Missing |
| Temporal PageRank (citation decay) | — | ❌ Missing |

**Where it lives**:
- `whitemagic/core/memory/mindful_forgetting.py:114-128` — exponential recency decay with configurable half-life
- `whitemagic/core/intelligence/hologram/encoder.py:335-394` — Z-axis time encoding (bucketed)
- `whitemagic/core/memory/sqlite_backend.py:149-162` — `associations.created_at`, `last_traversed_at`, `traversal_count` columns

**Gap**: Causality enforcement during traversal is missing. The `associations` table has `created_at` but nothing currently reads it during retrieval.

**Recommendation**: The walker should read `associations.created_at` and enforce `t_A→B ≤ t_B→C` as a hard constraint. This is a simple filter in the neighbor expansion step — no schema changes needed.

---

### Dimension 3: Gravity (g) — 50% Complete

| Paper Concept | WhiteMagic Equivalent | Status |
|---|---|---|
| Node gravity score (0-1) | `galactic_distance` (inverted: 0=core, 1=edge) | ✅ Exists (inverted) |
| Topological centrality (PageRank) | — | ❌ Missing |
| Pre-computed asynchronously | `galactic_map.full_sweep()` on SLOW lane | ✅ Exists |
| Hub attraction in walk | — | ❌ Missing (no walk exists) |

**Where it lives**:
- `whitemagic/core/memory/galactic_map.py` — retention-based distance with Rust batch scoring
- `whitemagic/core/intelligence/hologram/encoder.py:396-446` — W-axis (importance × neuro_score + boosts)
- `whitemagic/core/memory/mindful_forgetting.py` — 5-signal retention score

**Gap**: Gravity is purely **attribute-based** (importance, neuro_score, recall_count). The paper's gravity uses **topological centrality** — a node is heavy because many important nodes cite it, not just because it has high self-importance. With 19M associations, the graph topology encodes significant structural information that's currently unused for gravity.

**Recommendation**: Two options, not mutually exclusive:
1. **Quick win**: Use existing `galactic_distance` as gravity in the walker. Map `g(v) = 1.0 - galactic_distance`. Zero new code.
2. **Upgrade**: Compute PageRank over the association graph asynchronously (Rust accelerated). Store as a new `pagerank` column on `memories`. Run alongside galactic sweeps. This would meaningfully improve traversal quality by surfacing structural hubs.

---

### Dimension 4: Staleness (s) — 75% Complete

| Paper Concept | WhiteMagic Equivalent | Status |
|---|---|---|
| Staleness score (0-1) | `galactic_distance` (0=fresh/core, 1=stale/edge) | ✅ Exists |
| Age-based decay | `galactic_map.decay_drift()` | ✅ Exists |
| Conflict detection (contradictions) | — | ❌ Missing |
| Penalty at query time: (1-s)^β | — | ❌ Missing (applied at sweep time only) |

**Where it lives**:
- `whitemagic/core/memory/galactic_map.py:324-397` — `decay_drift()` pushes inactive memories outward
- `whitemagic/core/memory/lifecycle.py` — periodic retention sweep + galactic rotation + drift
- `whitemagic/core/memory/mindful_forgetting.py` — 5 retention signals → composite score

**Gap**: Staleness is applied during periodic background sweeps, not at query time. The walker needs to apply `(1 - staleness)^β` as a real-time penalty during traversal.

**Recommendation**: Read `galactic_distance` at walk time and apply the paper's penalty formula. The data is already there — it just needs to be used during neighbor scoring. No schema changes.

---

### Dimension 5: Holographic Binding (φ) — 20% Complete

| Paper Concept | WhiteMagic Equivalent | Status |
|---|---|---|
| HRR role binding (Apple⊗Subject) | — | ❌ Missing |
| Role unbinding for query construction | — | ❌ Missing |
| Typed relations on edges | `knowledge_graph.py` Relation(subject, predicate, obj) | ⚠️ Partial |
| Association relation types | `associations` table has no type field | ❌ Missing |

**Where it lives**:
- `whitemagic/core/intelligence/knowledge_graph.py:71-87` — Relation dataclass with predicate field
- `whitemagic/core/intelligence/knowledge_graph.py:98-104` — 5 relation patterns (uses, depends_on, part_of, implements, connects_to)

**Gap**: The main 19M-edge `associations` table has only `(source_id, target_id, strength)` — no relation type. The KG has typed relations but is a separate in-memory graph capped at 10K relations, disconnected from the main association graph.

**Recommendation**: 
1. **Quick win**: Add `relation_type TEXT DEFAULT 'associated_with'` to the `associations` table. Populate during future mining runs.
2. **Later**: Use KG relation types to guide traversal when available. The walker can check both the associations table and the KG adjacency for typed edges.

---

## The Critical Gap: No Graph Walker

### Current State

WhiteMagic has **three retrieval modes**, all stateless:

| Method | Location | Mechanism | Multi-hop? |
|---|---|---|---|
| `search()` | `unified.py:128` | FTS5 full-text + constellation annotation | ❌ No |
| `search_similar()` | `unified.py:167` | Vector cosine + Rust retrieval pipeline re-ranking | ❌ No |
| `search_hybrid()` | `unified.py:298` | RRF fusion of BM25 + embedding cosine | ❌ No |

**None of these traverse the association graph.** The 19M associations are created by `association_miner.py` and stored in SQLite, but they are only ever read to hydrate a Memory object's `.associations` dict — never walked.

### What's Needed

A single new file: **`whitemagic/core/memory/graph_walker.py`** containing:

1. **`GraphWalker` class** — loads neighbor edges from `associations` table, computes transition probabilities using existing 5D coordinates + embeddings
2. **`walk()` method** — performs k-hop weighted random walk from anchor nodes
3. **`hybrid_recall()` method** — the full pipeline: anchor search → graph walk → prediction aggregation

Plus one addition to `unified.py`:
- **`UnifiedMemory.hybrid_recall()`** — delegates to GraphWalker

### Transition Probability (Reusing Existing Infrastructure)

The paper's formula maps directly to existing WhiteMagic data:

```
Score(u, v) = Semantic × Gravity^α × Recency × (1 - Staleness)^β
```

| Paper Term | WhiteMagic Source | Access Method |
|---|---|---|
| **Semantic** = CosineSim(projected, v) | `embeddings.py` cached vectors | `_load_vec_cache()` → numpy matmul |
| **Gravity** = G(v)^α | `memories.galactic_distance` | `g = 1.0 - galactic_distance` |
| **Recency** = exp(-λ × Δt) | `memories.accessed_at` | Compute Δt from current time |
| **Staleness** = (1 - S(v))^β | `memories.galactic_distance` | `s = galactic_distance` |

**All four signals are already stored and indexed.** The walker just needs to combine them.

### Neighbor Loading

```sql
SELECT target_id, strength, created_at 
FROM associations 
WHERE source_id = ?
ORDER BY strength DESC
LIMIT 100
```

This returns the graph edges. For high-degree nodes (hubs with thousands of edges), the `LIMIT` + `ORDER BY strength` acts as a natural pruning mechanism, equivalent to the paper's "Alias Sampling for high-gravity nodes."

---

## Implementation Recommendations (Priority Order)

### P1: Build the Graph Walker (HIGH — closes the critical gap)
- **New file**: `whitemagic/core/memory/graph_walker.py` (~300-400 LOC)
- **Modify**: `unified.py` — add `hybrid_recall()` method
- **Uses**: existing embeddings, galactic_distance, associations table, holographic coords
- **No schema changes required**
- **Estimated impact**: Enables "Memory A → Prediction C" reasoning for the first time

### P2: Edge Timestamps in Walker (MEDIUM — enables causality)
- **Modify**: `graph_walker.py` — read `associations.created_at` during neighbor expansion
- **Logic**: Filter edges where `t_edge < t_previous_hop` (enforce forward causality)
- **Schema**: Already has `created_at` column (migration exists)
- **Issue**: Many existing associations may have NULL `created_at` — backfill script needed

### P3: Relation Types on Associations (MEDIUM — enables guided traversal)
- **Schema**: `ALTER TABLE associations ADD COLUMN relation_type TEXT DEFAULT 'associated_with'`
- **Modify**: `association_miner.py` — classify relation type during mining (uses KG patterns)
- **Modify**: `graph_walker.py` — optionally filter by relation type during traversal

### P4: Topological Gravity via PageRank (MEDIUM — improves hub detection)
- **New script**: `scripts/compute_pagerank.py` — async PageRank over association graph
- **Schema**: `ALTER TABLE memories ADD COLUMN pagerank REAL DEFAULT 0.0`
- **Modify**: `graph_walker.py` — blend `pagerank` with `galactic_distance` for gravity
- **Candidate for Rust**: Iterative PageRank over 19M edges is a perfect Rust/Rayon target

### P5: Traversal Telemetry (LOW — observability)
- **Modify**: `associations` table — update `last_traversed_at` and `traversal_count` when walker visits an edge
- **Feeds into**: `galactic_map.py` sweeps — frequently traversed edges could boost connected node gravity

### P6: HRR Circular Convolution (LOW — research-grade, defer)
- **New file**: `whitemagic/core/memory/hrr.py` — circular convolution/correlation
- **Only if**: P1 proves insufficient for multi-hop reasoning quality
- **Alternative**: Use embedding of "query + relation hint" as the projected vector

---

## What NOT to Build

| Paper Feature | Why Skip |
|---|---|
| CSR Matrix format | SQLite adjacency list is fine at 19M edges. CSR only needed at 100M+. |
| GPU graph traversal (cuGraph) | Python walker with Rust hot paths is sufficient for CPU. |
| FAISS index | In-memory numpy with pre-normalized vectors + BLAS matmul is already fast (~629ms warm). |
| Full HRR algebra | Complex, research-grade. Embedding cosine is a pragmatic substitute. |
| Separate vector DB | SQLite + numpy cache is working well. Don't add infrastructure. |

---

## Architecture After Implementation

```
hybrid_recall(query, hops=2, num_walks=10)
│
├── Step 1: Anchor Search (existing infrastructure)
│   ├── embeddings.search_similar(query, k=20)  ← Vector Index
│   ├── Filter by staleness (galactic_distance < 0.8)
│   └── Top 5 anchors
│
├── Step 2: Weighted Random Walk (NEW — graph_walker.py)
│   ├── For each anchor, for each walk:
│   │   ├── Load neighbors from associations table
│   │   ├── Load 5D coords + embeddings for neighbors
│   │   ├── Compute P(u,v) = Semantic^w × Gravity^α × Recency × (1-Staleness)^β
│   │   ├── Optional: causality filter (t_A→B ≤ t_B→C)
│   │   ├── np.random.choice(neighbors, p=probs)
│   │   └── Repeat for k hops
│   └── Collect all reasoning paths
│
├── Step 3: Prediction Aggregation (NEW — graph_walker.py)
│   ├── Score terminal nodes by accumulated gravity × path count
│   ├── Deduplicate paths reaching same terminal
│   └── Return ranked predictions with paths
│
└── Output: [(terminal_memory, score, [path]), ...]
```

---

## Existing Infrastructure Reuse Summary

| Paper Component | WhiteMagic Module | Reuse Level |
|---|---|---|
| Vector Index | `embeddings.py` | 100% — as-is for anchor search |
| Graph Backend | `sqlite_backend.py` associations table | 90% — add neighbor query method |
| Node Gravity | `galactic_map.py` galactic_distance | 80% — invert for gravity score |
| Node Staleness | `galactic_map.py` galactic_distance | 80% — use directly as staleness |
| Temporal Signal | `memories.accessed_at` + `_recency_signal()` | 70% — adapt for query-time use |
| Semantic Steering | `embeddings.py` cosine similarity | 100% — use for neighbor scoring |
| Constellation Context | `constellations.py` annotate_memories() | 100% — annotate walk results |
| KG Relations | `knowledge_graph.py` | 50% — consult for typed edges |
| Rust Acceleration | `rust_accelerators.py` | Future — PageRank, batch scoring |

**Total new code estimate**: ~400 LOC in `graph_walker.py` + ~30 LOC in `unified.py` + ~50 LOC tests.
**Existing code modified**: Minimal (add neighbor-loading method to sqlite_backend, add hybrid_recall to unified.py).
