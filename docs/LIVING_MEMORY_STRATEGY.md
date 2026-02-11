# Living Memory Strategy: Research Integration Plan

**Date:** February 10, 2026
**Version:** v14.x (next session)
**Source:** "Living Memory Systems: Theoretical Foundations and Architectural Implementation" (Research Team Report)

---

## 1. Executive Summary

Our research team produced a comprehensive report on Living Memory Systems, covering Temporal Knowledge Graphs (Zep/Graphiti, TTransE, HyTE, RE-NET), interaction decay physics, constellation drift tracking, and concept drift vs. catastrophic forgetting detection. This document compares those findings against WhiteMagic's current memory architecture, identifies gaps, and lays out a concrete implementation plan.

**Key finding:** WhiteMagic already implements ~60% of the paper's recommendations through its own architectural choices. The 5D holographic coordinate system, exponential decay with 30-day half-life, constellation detection with drift tracking, and the Galactic Map no-delete policy are all validated by the research. WhiteMagic additionally goes *beyond* the paper's scope with Dharma governance, polyglot acceleration, PRAT meta-tool routing, and a full cognitive architecture — areas the paper doesn't address.

The remaining ~40% gap is concentrated in five areas:
1. **Association strength decay** (edges never weaken)
2. **Surprise-gated memory ingestion** (no plasticity/stability balance)
3. **HDBSCAN clustering** (grid-based is crude)
4. **Cluster correspondence** (no Hungarian algorithm for drift tracking)
5. **Temporal KG enrichment** (no bitemporal modeling, no link prediction)

Items 1-4 are achievable in a single session. Item 5 is multi-session.

---

## 2. Current State vs. Research Recommendations

### 2.1 Comparison Matrix

| Dimension | Paper Recommends | WM Current State | Status |
|-----------|-----------------|------------------|--------|
| **Graph Structure** | Temporal KG with quadruples (h, r, t, τ) | KnowledgeGraph: in-memory triples + associations table | ⚠️ Partial |
| **Bitemporal Modeling** | event_time + ingestion_time on every fact | Associations have `created_at`, `last_traversed_at`, `traversal_count` | ⚠️ Foundation exists |
| **Hierarchical Subgraphs** | Episodic → Semantic Entity → Community | Flat graph + separate constellations (community detection) | ⚠️ Partial |
| **TKG Embeddings** | TTransE / HyTE temporal projection | Static MiniLM-L6-v2 (384d) + 5D holographic coordinates | ⚠️ Static only |
| **Link Prediction** | RE-NET autoregressive forecasting | None | ❌ Missing |
| **Exponential Decay** | w(t) = w₀·e^(-0.0231·Δt), 30-day half-life | `Memory.decay()`: `0.5^(days/half_life_days)`, default 30.0 | ✅ Match |
| **Power-Law Decay** | w(t) = w₀(1+αt)^(-ψ) for semantic edges | All decay is exponential | ❌ Missing |
| **Association Decay** | Edge weights decay with disuse | Associations never decay — strength is immutable after creation | ❌ Missing |
| **5D Particle Space** | semantic×3 + time + valence/importance | XYZWV: Logic↔Emotion, Micro↔Macro, Time, Importance, Vitality | ✅ Match |
| **HDBSCAN Clustering** | Variable-density hierarchical clustering | Grid-based density scan (8 bins/axis) | ⚠️ Crude |
| **Hungarian Algorithm** | Optimal cluster correspondence across time | Name-based matching (fragile) | ❌ Missing |
| **Drift Tracking** | Centroid displacement vectors | `get_drift_vectors()` with history + 5D displacement | ✅ Implemented |
| **Concept Drift Detection** | ADWIN / Page-Hinkley / Surprise metric | SelfModel linear regression + threshold alerts | ⚠️ Basic |
| **Plasticity/Stability** | Surprise gates new concept vs. reinforcement | No formal gating mechanism | ❌ Missing |
| **4-Layer Architecture** | Ingestion → Embedding → Dynamics → Forecasting | All 4 layers exist but loosely coupled | ⚠️ Partial |
| **No-Delete Policy** | Not addressed | Galactic Map: archive to edge, never destroy | ✅ WM exceeds |
| **Governance Layer** | Not addressed | Dharma + Karma + Circuit Breakers + Maturity Gates | ✅ WM exceeds |
| **Polyglot Acceleration** | Not addressed | Rust/Zig/Mojo/Haskell/Elixir/Go/Julia hot paths | ✅ WM exceeds |

### 2.2 What The Paper Validates

The research paper independently arrives at several design choices WhiteMagic already made:

- **5D holographic space** maps directly to the paper's recommended "5D particle model" (semantic dims + time + importance/valence). WM's axis definitions (XYZWV) are semantically richer than the paper's generic PCA reduction.
- **30-day half-life exponential decay** is the paper's exact recommendation (λ ≈ 0.0231). WM's `Memory.decay()` implements this formula.
- **Constellation detection with drift tracking** is precisely what the paper calls "Constellation Drift" — WM has both detection and `get_drift_vectors()` for tracking centroid evolution.
- **No-delete / Galactic rotation** aligns with the paper's philosophy that memories should decay, not be destroyed. WM's Galactic Map enforces this structurally.
- **Multi-signal retention scoring** (5 independent signals in Mindful Forgetting) mirrors the paper's call for composite metrics rather than single-dimensional relevance.
- **Hippocampal consolidation** (consolidation.py) directly implements what the paper calls the "replay" mechanism for strengthening important traces.

### 2.3 Where WM Exceeds The Paper

The paper focuses narrowly on memory dynamics. WhiteMagic embeds memory in a full cognitive architecture:

- **28 Gana meta-tool routing** (PRAT) with resonance context, Wu Xing phase, Guna classification
- **Dharma ethical governance** with YAML rules, graduated actions, karmic audit trail
- **Homeostatic Loop** with OBSERVE→ADVISE→CORRECT→INTERVENE graduated response
- **Dream Cycle** (5-phase hippocampal replay) running on temporal scheduler SLOW lane
- **9-language polyglot acceleration** with graceful fallback chains
- **181 MCP tools** with dispatch pipeline (Sanitizer → Breaker → Rate Limiter → RBAC → Maturity → Governor → Handler)

---

## 3. Implementation Plan

### Phase 1: Association Dynamics (HIGH PRIORITY)

**Problem:** Association edges (19M+ in hot DB) have immutable strength. Once created, they never weaken, violating the paper's core principle that "without a decay mechanism, the graph grows unbounded."

**Solution:** Hybrid decay on association traversal.

#### 3a. Association Strength Decay

**Files to modify:**
- `whitemagic/core/memory/sqlite_backend.py` — Add `decay_associations()` method
- `whitemagic/core/memory/lifecycle.py` — Wire association decay into Phase 4 of lifecycle sweep
- `whitemagic/core/memory/unified_types.py` — Add `edge_type` field to association metadata

**Math (from paper §4):**

Episodic associations (recently created, transient):
```
w(t) = w_prev × e^(-0.0231 × Δt_days)
```

Semantic associations (consolidated, strategy-derived):
```
w(t) = w_0 × (1 + 0.1 × Δt_days)^(-0.5)
```

Where power-law exponent ψ = 0.5 gives long-tail retention (Ebbinghaus curve).

**Pruning threshold:** Associations with w(t) < 0.05 are pruned (removed from DB).

**Strengthening on co-access:** When both source and target memories are recalled in the same session:
```
w_new = min(1.0, w_old + 0.05 × (1.0 - w_old))  # Hebbian learning
```

**LOC estimate:** ~200

#### 3b. Bitemporal Fields on Associations

**Files to modify:**
- `whitemagic/core/memory/sqlite_backend.py` — ALTER TABLE migration for `valid_from`, `valid_until`, `ingestion_time`
- `whitemagic/core/memory/association_miner.py` — Populate temporal fields on creation

**Schema addition:**
```sql
ALTER TABLE associations ADD COLUMN valid_from TEXT;    -- When the relationship became true
ALTER TABLE associations ADD COLUMN valid_until TEXT;   -- When it ceased being true (NULL = still valid)
ALTER TABLE associations ADD COLUMN ingestion_time TEXT; -- When WM learned about it
ALTER TABLE associations ADD COLUMN edge_type TEXT DEFAULT 'semantic'; -- 'episodic' | 'semantic' | 'consolidated'
```

**LOC estimate:** ~100

---

### Phase 2: Surprise-Gated Memory Ingestion (HIGH PRIORITY)

**Problem:** New memories are stored unconditionally. There's no mechanism to distinguish genuinely novel information (high plasticity needed) from redundant reinforcement (stability mode).

**Solution:** Compute information-theoretic Surprise at ingestion time.

**File to create:**
- `whitemagic/core/memory/surprise_gate.py` — Surprise metric computation + gating logic

**Files to modify:**
- `whitemagic/core/memory/unified.py` — Wire surprise gate into `store()` method
- `whitemagic/core/memory/embeddings.py` — Add `compute_novelty()` method

**Math (from paper §6.3):**
```
S(x) = -log₂ P(x | M)
```

Where P(x|M) is approximated by the maximum cosine similarity between the new memory's embedding and all existing embeddings:
```python
max_sim = max(cosine_similarity(new_embedding, existing_embeddings))
P_approx = max_sim  # In [0, 1]
surprise = -math.log2(max(P_approx, 1e-10))
```

**Gating logic:**
- **High Surprise (S > 3.0):** New concept detected → create new memory, boost importance by +0.15, emit NOVEL_CONCEPT event, add "novel" tag
- **Medium Surprise (1.0 < S ≤ 3.0):** Moderate novelty → store normally
- **Low Surprise (S ≤ 1.0):** Redundant → reinforce existing similar memory instead of creating new one (Hebbian strengthening of nearest neighbor's associations + importance)

**LOC estimate:** ~200

---

### Phase 3: HDBSCAN Constellation Upgrade (MEDIUM PRIORITY)

**Problem:** Grid-based density scanning (8 bins per axis = 32K cells) is crude. It can't handle variable-density regions, doesn't reject noise, and produces blocky cluster shapes.

**Solution:** Replace grid scan with HDBSCAN when available, keep grid as fallback.

**Files to modify:**
- `whitemagic/core/memory/constellations.py` — Add HDBSCAN path in `detect()`, retain grid fallback

**Dependency:** `hdbscan` package (pip-installable, BSD license). Optional — graceful fallback to grid.

**Implementation:**
```python
try:
    import hdbscan
    _HDBSCAN_AVAILABLE = True
except ImportError:
    _HDBSCAN_AVAILABLE = False

def detect(self, ...):
    # ... load coordinates as before ...

    if _HDBSCAN_AVAILABLE and len(coords_array) >= 50:
        return self._detect_hdbscan(coords_array, rows, ranges)
    else:
        return self._detect_grid(coords_array, rows, ranges)  # existing code
```

**HDBSCAN advantages (from paper §5.2):**
- Variable density handling (recent memories dense, old memories sparse)
- Probabilistic cluster membership (soft assignments)
- Noise rejection (outliers not forced into clusters)
- Cluster stability metric for robust hierarchy cuts

**LOC estimate:** ~250

---

### Phase 4: Hungarian Algorithm for Cluster Correspondence (MEDIUM PRIORITY)

**Problem:** `get_drift_vectors()` currently matches clusters across time by name. If a constellation's name changes (e.g., dominant tags shift), correspondence breaks.

**Solution:** Use the Hungarian algorithm (scipy.optimize.linear_sum_assignment) to find optimal centroid matching.

**Files to modify:**
- `whitemagic/core/memory/constellations.py` — Upgrade `get_drift_vectors()` and add `detect_concept_events()`

**Dependency:** `scipy` (already commonly available). Optional — fallback to name matching.

**Implementation (from paper §5.3):**
```python
from scipy.optimize import linear_sum_assignment

# Build cost matrix: distance between all pairs of centroids at T and T+1
cost_matrix = np.zeros((len(old_centroids), len(new_centroids)))
for i, old_c in enumerate(old_centroids):
    for j, new_c in enumerate(new_centroids):
        cost_matrix[i, j] = euclidean_distance_5d(old_c, new_c)

row_ind, col_ind = linear_sum_assignment(cost_matrix)
```

**Novel detection:**
- Unmatched new clusters (col_ind not fully covered) → **Novel Concept** event
- Unmatched old clusters (row_ind not fully covered) → **Forgotten Concept** event
- Both emit events to GanYingBus for downstream reaction

**LOC estimate:** ~150

---

### Phase 5: Drift Detection Algorithms (LOWER PRIORITY, SAME SESSION IF TIME PERMITS)

**Problem:** SelfModel uses simple linear regression. The paper recommends more sophisticated drift detection.

**Solution:** Add ADWIN and Surprise metric to SelfModel.

**File to create:**
- `whitemagic/core/memory/drift_detector.py` — ADWIN implementation + Page-Hinkley test

**Files to modify:**
- `whitemagic/core/intelligence/self_model.py` — Wire drift detection into `forecast()`

**ADWIN (from paper §6.2.1):**
Maintains a sliding window W. Automatically splits into W₀ and W₁ when the difference in means exceeds a Hoeffding-bound threshold. A "cut" signals distribution change.

**LOC estimate:** ~300

---

## 4. Deferred Items (Multi-Session)

These items from the paper require significant infrastructure and should be planned for future sessions:

### 4a. Temporal Knowledge Graph Upgrade
- Hierarchical subgraphs (Episodic → Semantic Entity → Community)
- Replace JSON KG persistence with SQLite-backed graph
- Time-aware queries ("what did WM know at time T?")
- **Estimate:** ~500-800 LOC, 1-2 sessions

### 4b. HyTE Temporal Embeddings
- Time-specific entity representations via hyperplane projection
- Per-timestamp normal vectors defining temporal hyperplanes
- Entity projections trace semantic drift trajectories
- **Estimate:** ~600-900 LOC, needs training infrastructure, 2+ sessions

### 4c. RE-NET Link Prediction
- Autoregressive model predicting future graph edges
- GRU encoder for entity history + RGCN for neighborhood aggregation
- Would give WM "anticipatory memory" — pre-fetching context
- **Estimate:** ~800-1200 LOC, needs PyTorch training loop, 3+ sessions

### 4d. Full Graphiti Integration
- Bitemporal storage with event_time + ingestion_time on all nodes
- Label propagation community detection (vs current grid/HDBSCAN)
- Deep Memory Retrieval (DMR) benchmark compatibility
- **Estimate:** Research spike needed to evaluate Zep/Graphiti as dependency vs homebrew

---

## 5. Test Strategy

### New test file: `tests/unit/test_living_memory.py`

```
TestAssociationDecay:
  - test_exponential_decay_30day_halflife
  - test_power_law_decay_semantic_edges
  - test_hebbian_strengthening_on_co_access
  - test_pruning_below_threshold
  - test_protected_associations_immune

TestSurpriseGate:
  - test_high_surprise_new_concept
  - test_low_surprise_reinforcement
  - test_medium_surprise_normal_store
  - test_surprise_without_embeddings_fallback
  - test_surprise_metric_math

TestHDBSCAN:
  - test_hdbscan_finds_variable_density_clusters
  - test_hdbscan_rejects_noise
  - test_grid_fallback_when_no_hdbscan
  - test_cluster_stability_metric

TestHungarianCorrespondence:
  - test_optimal_matching_simple
  - test_novel_concept_detection
  - test_forgotten_concept_detection
  - test_drift_vector_from_matched_pairs
  - test_fallback_to_name_matching

TestDriftDetection:
  - test_adwin_detects_distribution_change
  - test_page_hinkley_abrupt_change
  - test_no_false_alarm_on_stable_signal
```

**Estimated:** ~25-30 tests

---

## 6. Execution Order

```
Session Start
  │
  ├─ Phase 1a: Association Strength Decay         (~200 LOC, 45 min)
  │   └─ sqlite_backend.py + lifecycle.py
  │
  ├─ Phase 1b: Bitemporal Fields                  (~100 LOC, 20 min)
  │   └─ sqlite_backend.py + association_miner.py
  │
  ├─ Phase 2: Surprise Gate                        (~200 LOC, 40 min)
  │   └─ NEW surprise_gate.py + unified.py + embeddings.py
  │
  ├─ Phase 3: HDBSCAN Upgrade                      (~250 LOC, 45 min)
  │   └─ constellations.py
  │
  ├─ Phase 4: Hungarian Algorithm                   (~150 LOC, 30 min)
  │   └─ constellations.py
  │
  ├─ Phase 5: Drift Detection (if time)             (~300 LOC, 45 min)
  │   └─ NEW drift_detector.py + self_model.py
  │
  ├─ Tests                                          (~25 tests, 30 min)
  │   └─ test_living_memory.py
  │
  └─ Docs + Version Bump                            (15 min)
      └─ CHANGELOG.md, SYSTEM_MAP.md, VERSION
```

**Total estimated:** ~900-1200 LOC, 4-5 hours

---

## 7. Success Criteria

After implementation, the following should be true:

1. **Association decay runs on lifecycle sweep** — 19M associations gradually thin to semantically meaningful links
2. **Surprise metric gates ingestion** — redundant memories reinforce existing neighbors instead of creating duplicates
3. **HDBSCAN produces higher-quality constellations** — variable density, noise rejection, probabilistic membership
4. **Hungarian algorithm tracks constellation evolution** — novel/forgotten concept events emitted
5. **All existing tests still pass** — zero regressions
6. **New tests cover all 5 improvements** — ~25-30 tests

---

## 8. Strategic Significance

This work transforms WhiteMagic's memory from a *well-organized static repository* into a genuinely **living memory system** — one that:

- **Evolves autonomously** — associations strengthen with use and decay with neglect
- **Balances plasticity and stability** — surprise metric gates whether to learn or reinforce
- **Tracks conceptual drift** — HDBSCAN + Hungarian algorithm monitor how knowledge clusters migrate
- **Anticipates** — drift detection warns when concepts are shifting before they fully transform

Combined with WM's existing Galactic Map, Dharma governance, Dream Cycle, and polyglot acceleration, this positions WhiteMagic as a cognitive architecture that doesn't merely store data — it *experiences and adapts to it over time*.

---

*Prepared for execution in the next session. All phases designed to build on existing infrastructure with graceful fallbacks.*
