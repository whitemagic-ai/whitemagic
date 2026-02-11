# 5D Causal Memory Space — Strategy Document

**Date**: February 10, 2026  
**Version**: v14.0 Planning  
**Status**: Draft for Next Session Execution  
**Source**: Research team mathematical viability analysis of 5D Causal Memory Space (M⁵)

---

## 1. Executive Summary

Our research team produced a rigorous mathematical analysis of a 5D Causal Memory Space modeled as a **Riemannian Product Manifold** (E³ × H¹ × I¹), with Frank-Wolfe causal discovery, UMAP visualization, and HNSW gravitational sharding. This document compares that architecture against Whitemagic's current 5D holographic system and proposes an **evolutionary integration path** — adding cognitive reasoning capabilities on top of the existing galactic infrastructure without breaking the 110K+ memories already encoded.

**Core insight**: WM's current 5D system is a *lifecycle management* tool (where memories live in the galaxy). The research proposes a *cognitive reasoning* framework (how concepts relate causally). These are **complementary, not competing**. The path forward is to layer the cognitive framework on top of the existing infrastructure.

---

## 2. Dimension-by-Dimension Comparison

### 2.1 X-Axis: Logic ↔ Emotion

| Aspect | Research | Whitemagic Current |
|--------|----------|-------------------|
| **Semantic** | Logical vs. Emotional (Modality) | Logic ↔ Emotion |
| **Range** | [-1, +1] bipolar | [-1, +1] bipolar |
| **Computation** | Osgood "Valence" principal component | Keyword counting + emotional_valence + content hash bias |

**Verdict**: ✅ **Aligned**. Both identify the same semantic axis. WM's implementation is heuristic-based rather than learned, but captures the right signal. The keyword lists in both `encoder.py` and `holographic_encoder_5d.rs` cover the domain well.

**Improvement**: Eventually replace keyword counting with a learned logistic regression on embedding features for higher precision. Low priority — current approach works.

---

### 2.2 Y-Axis: Strategic/Tactical vs. Micro/Macro

| Aspect | Research | Whitemagic Current |
|--------|----------|-------------------|
| **Semantic** | Strategic vs. Tactical (Orientation) | Micro ↔ Macro (Concrete ↔ Abstract) |
| **Range** | [-1, +1] bipolar | [-1, +1] bipolar |
| **Computation** | Temporal horizon of action | memory_type classification + tag matching + content keywords |

**Verdict**: ⚠️ **Close but semantically different**. WM's Y-axis (Micro/Macro) maps more closely to what the research recommends for the Z-axis (Abstraction). The research's Y (Strategic/Tactical) is about *planning horizon* — "Global Hegemony" vs "Mobilize Division A" — while WM's is about *level of detail* — "logs" vs "architecture."

**Impact**: In practice, these overlap heavily. A strategic concept tends to be macro; a tactical one tends to be micro. The divergence matters most at edge cases (e.g., a tactical but abstract idea like "Improvisation").

**Recommendation**: No change needed. WM's Micro/Macro actually provides better concept separation for a memory system than pure Strategic/Tactical, because memory granularity (log vs. pattern) is a stronger organizing signal than planning horizon.

---

### 2.3 Z-Axis: Time vs. Abstraction

| Aspect | Research | Whitemagic Current |
|--------|----------|-------------------|
| **Semantic** | Concrete vs. Abstract (Ladder of Abstraction) | Time / Chronos (Past ↔ Future) |
| **Range** | Sigmoid-scaled Imaginability score | [-1, +1] from age + future-oriented tags |
| **Orthogonality** | Explicitly chosen for statistical independence from X/Y | Partially correlated with Y (old things tend to be abstract) |

**Verdict**: ❌ **Major divergence**. The research argues Z should be Abstraction (Concreteness) because it is a *primary independent factor of semantic variance* orthogonal to both Valence (X) and Orientation (Y). WM's Z (Time) is useful for lifecycle management but introduces **correlation with other axes** — old memories tend to drift toward abstract/macro (high Y), and time is partially captured by the V-axis (Galactic Distance) which already encodes recency via retention scoring.

**Key research argument**: "Rules generally cause instances, not the other way around. This is critical for causal reasoning." An Abstraction axis directly supports causal inference — the Time axis doesn't.

**Recommendation**: **Add Abstraction as computed metadata** (a "Z-prime" field). Don't replace the Time axis yet — it's deeply wired — but start computing and storing Abstraction scores so we can evaluate migration later. See §5.2.

---

### 2.4 W-Axis: Importance vs. Entropy

| Aspect | Research | Whitemagic Current |
|--------|----------|-------------------|
| **Semantic** | Complexity / Entropy (Basic vs. Elaborate) | Importance / Gravity [0.0, 2.0+] |
| **Metric** | Shannon entropy: W(c) = -Σ P(fᵢ) log P(fᵢ) | Weighted blend: importance×0.4 + neuro_score×0.6 + type/tag/joy boosts |
| **Purpose** | Separate atomic truths from chaotic systems | Separate important from trivial |

**Verdict**: ❌ **Major divergence**. The research argues W should measure information complexity (Kolmogorov/Shannon entropy), not salience. The paper's key insight: *"Low-W (Basic) concepts tend to be stable axioms, while High-W (Elaborate) concepts are often unstable emergent phenomena."* This is a **predictive** axis — complexity predicts causal stability.

WM's W (Importance) is closer to the research's G (Gravity/Centrality). The system effectively has two importance/centrality signals (W and V) but no complexity signal at all.

**Recommendation**: **Compute Shannon entropy for each memory** and store as a new `entropy_score` field. This can be calculated from the content's token distribution, tag count, association count, and embedding feature spread. See §5.1.

---

### 2.5 V/G-Axis: Vitality vs. Hyperbolic Centrality

| Aspect | Research | Whitemagic Current |
|--------|----------|-------------------|
| **Semantic** | Gravity / Centrality (Poincaré ball) | Vitality / Galactic Distance [0, 1] |
| **Geometry** | Hyperbolic: d_H = arccosh(1 + 2‖u-v‖²/((1-‖u‖²)(1-‖v‖²))) | Linear Euclidean: v = 1.0 - galactic_distance |
| **Property** | Exponential volume growth (matches hierarchical branching) | Linear scaling |

**Verdict**: ⚠️ **Conceptually aligned, mathematically divergent**. Both encode centrality/importance hierarchy. WM's linear [0,1] works but doesn't capture the exponential branching of concept hierarchies. In hyperbolic space, moving from the origin to the boundary covers exponentially more "volume" — this naturally encodes the 1→10→100→1000 branching of taxonomies.

**Practical impact**: For WM's current corpus (5.6K hot + 105K cold), the linear approximation is adequate. The hyperbolic advantage becomes critical at 1M+ nodes where hierarchical resolution matters.

**Recommendation**: **Implement hyperbolic distance as a custom metric option** in the Rust spatial index. The stored coordinates can remain [0,1] — only the distance function changes. See §5.4.

---

## 3. Infrastructure Gap Analysis

### 3.1 Search/Indexing

| Component | Research Rec. | WM Current | Gap |
|-----------|--------------|------------|-----|
| Primary index | HNSW (O(log N)) | KD-tree 5D (O(N^0.8) avg) | **Significant** |
| Embedding search | HNSW | Brute-force numpy cosine | **Hot DB OK (5.6K), Cold DB critical (105K)** |
| Gravitational sharding | 3-tier by G-axis | Hot/Cold split by retention | **Partial match** |
| SIMD distance | AVX-512 for 5D | Zig AVX2 (lane_width=8) | ✅ Close |
| Custom metric | Euclidean + Hyperbolic hybrid | Squared Euclidean only | **Missing** |
| Flat↔HNSW switch | Dynamic by partition size | N/A | **Missing** |

**Key insight**: The hot DB (5.6K memories) is at the threshold where brute-force is competitive with HNSW. The cold DB (105K) is where HNSW would provide 300×+ speedup for KNN queries.

### 3.2 Causal Discovery

| Component | Research Rec. | WM Current | Gap |
|-----------|--------------|------------|-----|
| Graph structure | Directed Acyclic Graph (DAG) | Undirected symmetric associations | **Fundamental** |
| Learning algorithm | Frank-Wolfe optimization | None (Jaccard overlap + cosine similarity) | **Missing entirely** |
| Causal inference | Forward/backward DAG traversal | Find similar, no causation | **Missing entirely** |
| Edge evolution | Gradient-based weight updates | Static (manual recalibration) | **Missing** |
| Hebbian substrate | N/A | Memory.links with activation_count | ✅ **Exists — untapped** |
| Knowledge Graph | N/A | Relations: consolidated_into, shares_theme, associated_with | ✅ **Exists — can store causal edges** |

**Key insight**: WM has the *data substrate* (associations, KG, Hebbian links) on which causal discovery could operate. What's missing is the *algorithm*.

### 3.3 Visualization

| Component | Research Rec. | WM Current | Gap |
|-----------|--------------|------------|-----|
| Manifold projection | UMAP with custom 5D metric | None | **Missing entirely** |
| 2D scatter | UMAP-projected 5D coordinates | Force-directed relational graph | **Different paradigm** |
| Cognitive map | Gravity-centered (core concepts central) | Galactic orbital rings (1D) | **Partial** |
| Interactive query | Project new queries into existing 2D space | N/A | **Missing** |

---

## 4. Risk Assessment

### 4.1 What We Must NOT Do
- **Replace XYZWV wholesale**: 110K+ memories encoded, Rust/Zig/Mojo encoders built, galactic map wired. A full axis redefinition would require re-encoding everything and rewriting 5+ accelerators. Prohibitive cost, marginal benefit at current scale.
- **Force hyperbolic geometry on all queries**: The Poincaré metric adds complexity to every distance computation. Only worth it for the centrality dimension, and only as an option.
- **Over-engineer causal discovery for 5.6K hot memories**: Frank-Wolfe on a 5.6K node graph produces a 5600×5600 adjacency matrix. Start with the Hebbian signal on the ~500 most-connected memories.

### 4.2 What We SHOULD Do
- **Layer new capabilities alongside existing ones**: Add entropy/abstraction as NEW metadata fields. Add HNSW as an ALTERNATIVE index for cold storage. Add causal edges as a NEW relation type in the KG. Nothing is removed.
- **Validate before committing**: Compute entropy and abstraction for a sample, visualize with UMAP, assess whether concept separation actually improves before re-encoding.

---

## 5. Execution Plan (Next Session)

### Phase 1: Enrichment (No Breaking Changes)

#### 5.1 Shannon Entropy Computation
**Goal**: Add `entropy_score` field to each memory.  
**Method**: For each memory, compute Shannon entropy from:
- Token frequency distribution of content (primary signal)
- Number of unique tags (proxy for topic spread)
- Number of associations (connectivity complexity)
- Embedding feature variance (semantic spread)

**Formula**: `W_entropy = -Σ P(token_i) · log₂(P(token_i))` normalized to [0, 1]

**Implementation**:
- New function in `encoder.py`: `_calculate_entropy(memory) → float`
- Store in `memory_embeddings` table as `entropy_score` column
- Script to batch-compute for all 5.6K hot memories
- ~200 LOC Python, ~100 LOC Rust (optional accelerator)

#### 5.2 Abstraction Score Computation
**Goal**: Add `abstraction_score` field as a proto-Z' axis.  
**Method**: Compute via:
- Embedding distance from concrete anchor words ("file", "line", "byte", "pixel") vs abstract anchors ("principle", "philosophy", "strategy", "truth")
- Memory type signal: short_term→concrete, pattern/wisdom→abstract
- Content length-normalized vocabulary diversity (type-token ratio)

**Implementation**:
- New function in `encoder.py`: `_calculate_abstraction(memory) → float`
- Range [0, 1]: 0=fully concrete, 1=fully abstract
- Store alongside entropy in enrichment table
- ~150 LOC Python

#### 5.3 Directed Association Edges
**Goal**: Add temporal directionality to associations.  
**Method**:
- Add `direction` column to `associations` table: `'undirected'` (default), `'forward'`, `'backward'`
- When creating new associations, record which memory was created/accessed first → that one is the likely "cause" (forward edge)
- The Hebbian `activation_count` on Memory.links already tracks co-activation — expose this as edge weight

**Implementation**:
- ALTER TABLE associations ADD COLUMN direction TEXT DEFAULT 'undirected'
- Modify `association_miner.py` to set direction based on `created_at` ordering
- ~50 LOC SQL + Python

### Phase 2: HNSW Index for Cold Storage

#### 5.4 HNSW Implementation
**Goal**: O(log N) KNN search for the 105K cold DB.  
**Method**: Use `hnswlib` Python package (C++ core, Python bindings) or implement in Rust.

**Option A — Python hnswlib** (faster to implement):
- `pip install hnswlib`
- Build index from cold DB embedding vectors (384-dim)
- Parameters: M=16, ef_construction=200
- Save index to disk for persistence
- Integrate into `embeddings.py` `search_similar(include_cold=True)` path

**Option B — Rust HNSW** (higher performance, matches polyglot strategy):
- Use `hnsw` crate or `instant-distance`
- Expose via PyO3 in `whitemagic_rs`
- Custom metric support for future hyperbolic distance

**Recommendation**: Start with Option A for immediate value, plan Option B for v15.

**Implementation**:
- New file: `whitemagic/core/memory/hnsw_index.py` (~300 LOC)
- Modify `embeddings.py` cold search path to use HNSW
- Build script in `scripts/build_cold_hnsw.py`
- Expected speedup: 300×+ for cold DB semantic search

#### 5.5 Custom Hybrid Metric (Preparation)
**Goal**: Prepare for Euclidean + Hyperbolic hybrid distance.  
**Method**:
- Define `hybrid_distance(a, b)` = `euclidean(a[0:3], b[0:3]) + λ · poincare(a[4], b[4])`
- Where `poincare(u, v) = arccosh(1 + 2|u-v|² / ((1-u²)(1-v²)))`
- λ is a tunable parameter (default 0.5)

**Implementation**:
- Add to `simd_distance.py` as `hybrid_5d_distance()`
- Add to Rust `spatial_index_5d.rs` as alternative distance function
- ~100 LOC Python + ~80 LOC Rust

### Phase 3: UMAP Visualization

#### 5.6 UMAP Projection Pipeline
**Goal**: Project 5D holographic coordinates → 2D for Nexus visualization.  
**Method**:
- Install `umap-learn` package
- Load all 5D coordinates from hot DB
- Project to 2D using custom metric (§5.5 or standard Euclidean initially)
- Serve via Nexus API endpoint `/api/umap`
- Cache projected coordinates (re-compute on significant changes)

**Implementation**:
- New file: `whitemagic/core/memory/umap_projection.py` (~200 LOC)
- New API endpoint in `nexus_api.py` (~50 LOC)
- New Nexus component: `CognitiveMap.tsx` (~300 LOC)
  - 2D scatter plot of all memories
  - Color by: zone, garden, memory_type, constellation
  - Size by: importance (W-axis)
  - Click to inspect memory details
  - Hover to see constellation membership
  - Zoom/pan with canvas renderer

#### 5.7 UMAP Parameters
- `n_neighbors=15` (balance local/global structure)
- `min_dist=0.1` (allow tight clusters)
- `metric='precomputed'` with custom hybrid distance matrix
- `n_components=2`
- Re-project every 100 new memories or on demand

### Phase 4: Causal Discovery (Foundation)

#### 5.8 Causal Edge Mining
**Goal**: Learn directed causal edges from co-activation patterns.  
**Method**:
- Extract temporal ordering from Hebbian links: if Memory A is consistently accessed *before* Memory B, and they have strong co-activation, A→B is a candidate causal edge
- Use Granger-like causality test: does knowing A improve prediction of B's activation?
- Store as `causes` relation type in Knowledge Graph

**Implementation**:
- New file: `whitemagic/core/memory/causal_miner.py` (~400 LOC)
- Analyze top 500 most-connected memories (by link count)
- For each pair with activation_count > 5:
  - Compute temporal precedence score
  - Compute conditional activation probability
  - If both above threshold → propose causal edge
- Feed into KG with relation="causes", metadata includes confidence score

#### 5.9 Frank-Wolfe Skeleton (Research Phase)
**Goal**: Prototype Frank-Wolfe DAG learning for future integration.  
**Method**:
- Implement CFW algorithm on a small subgraph (top 100 memories)
- Adjacency matrix A ∈ R^{100×100}
- Loss: predictive error on co-activation patterns
- L1 regularization for sparsity
- Acyclicity penalty via trace exponential
- Compare learned structure against heuristic causal edges from §5.8

**Implementation**:
- New file: `whitemagic/core/memory/frank_wolfe_causal.py` (~500 LOC)
- Uses numpy/scipy only (no new deps)
- Outputs: sparse adjacency matrix, top causal chains, convergence metrics
- This is EXPERIMENTAL — validate before integrating into production paths

---

## 6. Estimated Effort & Priority

| Phase | Task | LOC | Priority | Risk |
|-------|------|-----|----------|------|
| 1.1 | Shannon Entropy computation | ~300 | **High** | Low |
| 1.2 | Abstraction Score computation | ~150 | **High** | Low |
| 1.3 | Directed Association edges | ~50 | **High** | Low |
| 2.4 | HNSW cold storage index | ~400 | **High** | Medium |
| 2.5 | Hybrid distance metric | ~180 | Medium | Low |
| 3.6 | UMAP projection pipeline | ~250 | **High** | Low |
| 3.7 | Nexus CognitiveMap component | ~300 | Medium | Medium |
| 4.8 | Causal edge mining | ~400 | Medium | Medium |
| 4.9 | Frank-Wolfe prototype | ~500 | Low | High |
| — | **Total** | **~2,530** | — | — |

### Session Execution Order
1. **Entropy + Abstraction enrichment** (Phase 1.1 + 1.2) — Foundation for everything else
2. **Directed associations** (Phase 1.3) — 50 LOC, instant value
3. **HNSW cold index** (Phase 2.4) — Biggest performance win
4. **UMAP projection** (Phase 3.6) — Biggest UX win
5. **Causal mining** (Phase 4.8) — Foundation for predictive engine
6. **Remaining items** — As time permits

---

## 7. Validation Criteria

Before committing to full integration, we validate each phase:

### Phase 1 Validation
- [ ] Entropy scores span [0, 1] with meaningful spread (not clustered)
- [ ] Abstraction scores show clear separation between "log/debug" memories and "architecture/philosophy" memories
- [ ] Entropy + Abstraction are statistically independent of existing W (Importance) — correlation < 0.5

### Phase 2 Validation
- [ ] HNSW cold search returns same top-10 results as brute force (recall > 0.95)
- [ ] HNSW cold search is >100× faster than brute force for K=10
- [ ] Hybrid metric produces more intuitive neighbor rankings than pure Euclidean (manual spot check)

### Phase 3 Validation
- [ ] UMAP projection preserves constellations (cluster members stay together in 2D)
- [ ] CORE memories cluster centrally, FAR_EDGE memories scatter peripherally
- [ ] Different gardens/types form visually distinct regions
- [ ] Interactive in Nexus at 60fps for 5.6K points

### Phase 4 Validation
- [ ] Causal edges make intuitive sense (manual review of top 20)
- [ ] Causal chains of length 3+ exist and correspond to real development sequences
- [ ] Frank-Wolfe converges in <1000 iterations on 100-node subgraph

---

## 8. Long-Term Vision: The Predictive Engine

Once Phases 1-4 are validated, WM evolves from a *memory retrieval system* to a **cognitive reasoning engine**:

```
Query: "What might happen if I refactor the association miner?"
                    │
                    ▼
            ┌───────────────┐
            │  5D Coordinate │ ← Encode query in M⁵
            │    Encoding    │
            └───────┬───────┘
                    │
                    ▼
            ┌───────────────┐
            │  HNSW KNN     │ ← Find 20 nearest memories
            │  (Hot + Cold) │
            └───────┬───────┘
                    │
                    ▼
            ┌───────────────┐
            │  Causal DAG   │ ← Traverse forward: "refactor" → "tests break"
            │  Traversal    │   → "constellation shift" → "galactic resweep"
            └───────┬───────┘
                    │
                    ▼
            ┌───────────────┐
            │  UMAP Project │ ← Show affected region in Cognitive Map
            │  + Highlight  │
            └───────┬───────┘
                    │
                    ▼
            ┌───────────────┐
            │  Prediction   │ ← "Based on 3 similar past refactors,
            │  Report       │    expect: test failures (0.8), perf
            └───────────────┘    improvement (0.6), doc staleness (0.9)"
```

This is the **Predictive Engine** the research paper envisions. WM already has 80% of the substrate — the enrichment, indexing, and causal mining phases close the remaining gaps.

---

## 9. Relationship to Existing Roadmap

This strategy **complements** the existing v14 roadmap (docs/V14_RELEASE_STRATEGY.md):

| Existing Roadmap Item | Relationship |
|----------------------|-------------|
| Mypy burndown | Independent — can proceed in parallel |
| Loose root file cleanup | Independent |
| PyPI publish prep | Independent — enrichment fields are additive |
| Strategic Roadmap Leaps 7-10 | **Synergistic** — Leap 7 (Nervous System) StateBoard can serve UMAP coords; Leap 9 (Cryptographic Trust) manifests benefit from causal provenance |
| Cold storage search | **Directly addressed** by Phase 2 (HNSW) |
| Embedding deduplication | Assisted by entropy scores (high-entropy duplicates are more suspicious) |

---

*This document was prepared by comparing the research team's mathematical analysis against a thorough audit of Whitemagic's current 5D holographic memory, search infrastructure, association mining, and visualization systems as of v13.6.0.*
