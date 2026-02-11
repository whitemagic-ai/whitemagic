# Dream Cycle Protocol — Strategy for v14.1+

## Executive Summary

This document compares the research team's "Autonomous Insight Generation Pipeline: The Dream Cycle Protocol" against Whitemagic's current architecture, identifies gaps and strengths, and proposes a concrete execution plan for the next session.

**Bottom line**: Whitemagic already implements ~60% of the research vision — but the missing 40% is the *most transformative* part: graph-topology-driven discovery and spectral governance. The current system consolidates and decays memories effectively but doesn't yet **think about** its own knowledge structure to discover novel insights or detect toxic patterns.

---

## Part I: Current State vs. Research — Comparison Matrix

### Pillar 1: Consolidation (Deep Sleep)

| Capability | Research Proposes | WM Current State | Gap Level |
|---|---|---|---|
| **Dual-Store Memory** | Episodic (Vector DB) → Semantic (Graph DB) | Hot DB (5,623 mem) + Cold DB (105,194 mem), both SQLite | ✅ Exists (different design) |
| **Entity Resolution** | DBSCAN → Medoid → Symbolic Verify → Graph Merge | Tag-overlap clustering + MinHash pre-filter | 🔴 Critical Gap |
| **Temporal Lineage** | Anchor+Delta versioned edges with valid_from/valid_to | Associations have created_at but NO validity windows | 🟡 Moderate Gap |
| **Compression / Abstraction** | 50 instances → 1 pattern node (O(n) → O(log n)) | Strategy memories synthesized but originals not compressed | 🟡 Moderate Gap |
| **Batch Graph Writes** | UNWIND operations in Cypher | SQLite executemany() — already batched | ✅ Exists |
| **No-Delete Policy** | Not mentioned (research uses "prune/delete") | Galactic Map rotation — **AHEAD of research** | ✅ Strength |
| **Mindful Forgetting** | Not mentioned | 5-signal retention engine with graduated decay | ✅ Strength |
| **Embedding-Based Search** | Cosine similarity for retrieval | MiniLM-L6-v2 (384 dims), 5,562 indexed, hybrid RRF | ✅ Exists |

### Pillar 2: Serendipity (REM Sleep)

| Capability | Research Proposes | WM Current State | Gap Level |
|---|---|---|---|
| **Community Detection** | Louvain algorithm on graph | Constellation detection in 5D space (grid density) | 🟡 Different approach, functional |
| **Bridging Centrality** | CR(v) = BC(v) × CB(v) for structural holes | None — association mining uses Jaccard/cosine only | 🔴 Critical Gap |
| **LLM Hypothesis Generation** | "What If" prompts from bridge nodes | None — dream cycle doesn't generate novel hypotheses | 🔴 Critical Gap |
| **Counterfactual Simulation** | Sandbox "Dream Graph" for testing | None — insights committed directly | 🟡 Moderate Gap |
| **Cognitive Variability** | Temperature modulation + Lévy flights | Zone-diverse sampling only | 🟡 Moderate Gap |
| **Synthetic Insight Nodes** | Tagged dream_cycle source | ✅ Dream insights persisted as memories | ✅ Exists |
| **Bicameral Enrichment** | Not mentioned | Left/right hemisphere cross-cluster bridging | ✅ Strength |
| **Constellation Drift Tracking** | Not mentioned | Centroid history + drift vectors over time | ✅ Strength |
| **Knowledge Graph Integration** | Central to architecture | In-memory JSON KG, 10K relation cap | 🟡 Exists but limited |

### Pillar 3: Governance (Sanitization Sweep)

| Capability | Research Proposes | WM Current State | Gap Level |
|---|---|---|---|
| **Eigenvector Centrality** | Detect feedback loop hotspots | None on graph topology | 🔴 Critical Gap |
| **Echo Chamber Detection** | T_now vs T_yesterday centrality comparison | None | 🔴 Critical Gap |
| **Signed Network Analysis** | +/- edges, structural balance theory | None — all associations are positive | 🟡 Moderate Gap |
| **Network Dismantling** | Min-Sum algorithm, edge inhibition | None at graph level | 🔴 Critical Gap |
| **Bias Detection** | Statistical analysis of disproportionate links | None | 🟡 Moderate Gap |
| **Action-Level Governance** | Basic rate limiting mentioned | 8-stage dispatch pipeline — **FAR AHEAD** | ✅ Strength |
| **Dharma Rules Engine** | Not mentioned | YAML-driven, 3 profiles, Haskell primary | ✅ Strength |
| **Karma Ledger** | Not mentioned | Declared vs actual side-effects + Merkle tree | ✅ Strength |
| **Circuit Breakers** | Not mentioned | Per-tool CLOSED→OPEN→HALF_OPEN state machine | ✅ Strength |
| **Homeostatic Loop** | Not mentioned | OBSERVE→ADVISE→CORRECT→INTERVENE | ✅ Strength |
| **Cryptographic Integrity** | Not mentioned | SHA-256 handler hashes, Karma Merkle root | ✅ Strength |

---

## Part II: Strengths — Where Whitemagic is AHEAD of the Research

The research paper, while rigorous in graph theory, operates from a standard RAG+Neo4j perspective. Whitemagic has several capabilities the research doesn't envision:

1. **5D Holographic Coordinate System (XYZWV)**: A spatial memory model encoding Logic↔Emotion, Micro↔Macro, Time, Importance, and Vitality. Memories have *geometric* relationships, not just graph edges. This enables spatial clustering (constellations) that graph-only approaches miss.

2. **Galactic Map / No-Delete Philosophy**: The research talks about "pruning" and "garbage collection." WM's approach — rotate to the far edge, never destroy — is philosophically and practically superior. Every memory remains searchable. The Galactic Map provides a continuous position metric (0.0–1.0) that's more nuanced than the research's binary keep/prune.

3. **8-Stage Dispatch Pipeline**: Input Sanitizer → Circuit Breaker → Rate Limiter → RBAC → Maturity Gate → Governor → Handler → Compact Response. This is production-grade action governance. The research's governance focuses only on graph topology.

4. **Polyglot Acceleration**: 9 languages (Rust PyO3, Zig SIMD, Haskell FFI, etc.) providing native-speed hot paths. The research assumes Python+Ray.

5. **Resonance Architecture**: 28 Ganas, Wu Xing elemental phases, Guna classification, predecessor/successor chains. The PRAT router provides contextual tool routing that makes each interaction resonance-aware.

6. **Harmony Vector (7 Dimensions)**: Continuous system health monitoring (balance, throughput, latency, error_rate, dharma, karma_debt, energy) that feeds back into memory lifecycle. More sophisticated than the research's binary wake/sleep.

7. **Self-Reinforcing Dream Loop**: Dream insights are already persisted as memories, making them discoverable by future scans. The research proposes this; WM already does it.

---

## Part III: Critical Gaps — What's Missing

### Gap 1: Graph Topology Engine (Foundation)
**Impact**: Blocks all three pillars' advanced features.

The in-memory JSON KnowledgeGraph (`knowledge_graph.py`) stores entities and relations but has:
- No centrality algorithms (betweenness, eigenvector, bridging coefficient)
- No community detection
- 10K relation hard cap
- No persistence beyond JSON dump

**Without this**, we cannot compute bridging centrality, detect echo chambers, or run spectral analysis. This is the single biggest architectural gap.

### Gap 2: Entity Resolution Pipeline
**Impact**: Fragmentation degrades retrieval quality over time.

The consolidation phase clusters by tag overlap, not by semantic similarity of content. Duplicate entities (same concept described differently) remain as separate memories, fragmenting the system's understanding. The research's DBSCAN→Medoid→Verify→Merge pipeline is entirely absent.

### Gap 3: LLM-Driven Hypothesis Generation
**Impact**: The system mines connections but doesn't REASON about them.

The serendipity phase discovers keyword/embedding overlaps and constellation bridges, but never asks "what does this connection MEAN?" An LLM hypothesis generation step would transform raw association data into actionable insights.

### Gap 4: Graph-Level Governance
**Impact**: The knowledge base can develop echo chambers unchecked.

All current governance operates at the action level (tool dispatch). The graph itself — associations, KG relations, constellation memberships — is ungoverned. Feedback loops where self-reinforcing associations amplify each other are undetectable and unprunable.

### Gap 5: Temporal Lineage
**Impact**: Cannot answer "what was true at time T?" questions.

Associations lack validity windows. When a fact changes, the old association is not closed — the new one is simply added alongside it. This means the system cannot reason about change over time.

---

## Part IV: Execution Plan — Next Session

### Tier 1: Graph Topology Engine (MUST DO — Foundation)

**New file**: `whitemagic/core/memory/graph_engine.py` (~400 LOC)

```
Dependencies: networkx (pure Python, add to pyproject.toml extras)
```

Implements:
- `build_graph()`: Construct networkx DiGraph from association table + KG relations
- `betweenness_centrality()`: Standard networkx implementation
- `eigenvector_centrality()`: For governance spectral analysis
- `bridging_coefficient(v)`: 1/deg(v) × Σ(1/deg(u)) for u in neighbors
- `bridging_centrality(v)`: betweenness × bridging_coefficient
- `detect_communities()`: Louvain/greedy modularity via networkx.community
- `find_structural_holes()`: Identify gaps between communities
- `find_bridge_nodes(top_n)`: Nodes with highest bridging centrality
- `centrality_snapshot()`: Store centrality distribution for T_now vs T_prev comparison
- Cache with TTL (rebuild during dream cycle)
- MCP tool: `graph.topology` for introspection

**Wiring**:
- Constellation detector can cross-reference graph communities
- Dream cycle serendipity phase uses bridge nodes
- Dream cycle governance phase uses eigenvector centrality

### Tier 2: Upgrade Dream Cycle Serendipity (HIGH VALUE)

**Modified file**: `whitemagic/core/dreaming/dream_cycle.py`

Upgrade `_dream_serendipity()`:
1. Build graph via `graph_engine.build_graph()`
2. Find top-5 bridge nodes via `find_bridge_nodes(5)`
3. For each bridge node:
   - Get its two neighboring communities
   - Extract dominant themes from each community
   - Generate a synthesis prompt: "Connection between [Theme A] and [Theme B] via [Bridge Node]: what insight emerges?"
   - If Ollama is available: send to local LLM for hypothesis generation
   - If not: generate a structured insight from the themes + bridge metadata
4. Persist as Dream Insight memory (existing mechanism)
5. Record bridge discoveries in KG

**New helper**: `whitemagic/core/memory/bridge_synthesizer.py` (~200 LOC)
- `synthesize_from_bridge(bridge_node, community_a, community_b)` → insight text
- Ollama integration (optional, graceful fallback)
- Structured insight template for non-LLM path

### Tier 3: Graph Governance Phase (HIGH VALUE)

**Modified file**: `whitemagic/core/dreaming/dream_cycle.py`

Add Phase 6: `GOVERNANCE` to DreamPhase enum and `_dream_governance()`:
1. Build graph via `graph_engine.build_graph()`
2. Compute eigenvector centrality
3. Load previous centrality snapshot (if exists)
4. Compare distributions:
   - Find nodes with centrality increase > 2σ
   - Check if increase correlates with new external data (new memories in that cluster)
   - If centrality spike WITHOUT new data → flag as potential echo chamber
5. For flagged nodes:
   - Identify the edges causing the spike (highest-weight incoming edges)
   - "Inhibit" by reducing edge weight by 50% (not delete)
   - Log action to Karma Ledger
6. Save new centrality snapshot for next cycle
7. Emit DREAM_PHASE_GOVERNANCE event

### Tier 4: Embedding-Based Entity Resolution (MODERATE)

**Modified file**: `whitemagic/core/memory/consolidation.py`

New method `_cluster_by_embeddings()` as alternative to `_cluster_by_tags()`:
1. Load embedding vectors from cache
2. Find near-duplicate pairs (cosine > 0.90 via existing `find_duplicates()`)
3. Union-Find clustering on duplicate pairs
4. For each cluster:
   - Select medoid (highest importance score = canonical)
   - Create "consolidated_into" KG edges from non-canonical → canonical
   - Push non-canonical memories to OUTER_RIM (preserve, don't delete)
   - Boost canonical memory importance by +0.1
5. Wire into consolidation phase as Step 2.5 (after tag clustering, before synthesis)

### Tier 5: Temporal Association Lineage (DEFERRED)

**Schema change**: Add to associations table:
- `valid_from TEXT` — when this association became true
- `valid_to TEXT` — when superseded (NULL = current)
- `superseded_by TEXT` — the association that replaced this one

**Invalidation Agent**: During consolidation, when a new association is created that contradicts an existing one (same source, same target, different strength direction), close the old one.

*Deferred because*: Requires schema migration + careful backward compatibility testing.

### Tier 6: Signed Networks + Contradiction Detection (DEFERRED)

**Schema change**: Add `polarity REAL DEFAULT 1.0` to associations.
**New algorithm**: Check structural balance in short cycles (triangles).

*Deferred because*: Requires schema migration + WM's associations are currently all positive. Need a mechanism for negative associations first.

---

## Part V: Session Execution Order

```
Phase A (30 min): Graph Topology Engine [Tier 1]
  - Create graph_engine.py
  - Add networkx to pyproject.toml extras
  - Wire into KnowledgeGraph for data source
  - Basic tests

Phase B (30 min): Dream Cycle Serendipity Upgrade [Tier 2]
  - Create bridge_synthesizer.py
  - Upgrade _dream_serendipity() to use graph topology
  - Ollama integration (optional)
  - Tests

Phase C (20 min): Graph Governance Phase [Tier 3]
  - Add GOVERNANCE phase to DreamPhase enum
  - Implement _dream_governance()
  - Centrality snapshot persistence
  - Wire inhibition to Karma Ledger
  - Tests

Phase D (20 min): Entity Resolution [Tier 4]
  - Add _cluster_by_embeddings() to consolidation.py
  - Wire into consolidation cycle
  - Tests

Phase E (10 min): Integration + Docs
  - Wire graph_engine into Gnosis portal
  - Add MCP tools: graph.topology, graph.bridges, graph.governance
  - Update CHANGELOG, VERSION, AI_PRIMARY, STRATEGIC_ROADMAP
  - Run full test suite
```

---

## Part VI: Architecture After Implementation

```
WAKE PHASE (Online):
  User Query → PRAT Router → 28 Ganas → Tool Dispatch Pipeline
                                          ↓
  Memory Store → SQLite Backend → Holographic Index → Embedding Cache
                                          ↓
  Response ← Hybrid RRF Search ← BM25 + Semantic + Constellation Annotation

SLEEP PHASE (Offline — Dream Cycle):
  Phase 1: CONSOLIDATION
    - Tag clustering + embedding-based entity resolution [NEW]
    - Strategy synthesis + galactic promotion
    - KG enrichment
  Phase 2: SERENDIPITY
    - Graph topology analysis → bridge node discovery [NEW]
    - LLM hypothesis generation from structural holes [NEW]
    - Association mining (keyword + semantic)
    - Constellation bridge surfacing
  Phase 3: KAIZEN
    - Harmony Vector health analysis
    - Emergence Engine scan
    - Dream insight persistence
  Phase 4: ORACLE
    - Grimoire consultation
  Phase 5: DECAY
    - Mindful forgetting → retention sweep
    - Galactic rotation → decay drift
  Phase 6: GOVERNANCE [NEW]
    - Eigenvector centrality → echo chamber detection [NEW]
    - Centrality snapshot comparison (T_now vs T_prev) [NEW]
    - Edge inhibition for feedback loops [NEW]
    - Karma Ledger audit trail
```

---

## Part VII: Key Design Decisions

### Why networkx, not Neo4j?
- WM's memory is SQLite-backed with 5,623 hot + 105,194 cold memories
- networkx handles graphs up to ~100K nodes easily in-memory
- Zero infrastructure overhead (no separate DB process)
- Pure Python = works everywhere WM works
- Can be accelerated with Rust (graph-tools or petgraph) later if needed

### Why Union-Find, not DBSCAN for entity resolution?
- Embeddings are already computed and cached (5,562 in hot DB)
- `find_duplicates(threshold=0.90)` already returns pairs
- Union-Find over these pairs is O(n·α(n)) — practically O(n)
- DBSCAN would require re-embedding and epsilon tuning
- We're not clustering arbitrary points; we're merging known duplicates

### Why edge inhibition, not deletion?
- Consistent with WM's no-delete philosophy
- Inhibited edges can be un-inhibited if the echo chamber assessment was wrong
- Transparent via Karma Ledger audit trail
- The research explicitly recommends "inhibit, don't delete" (analogous to synaptic pruning)

### Why Ollama for hypothesis generation (optional)?
- WM already has Ollama integration (handlers/ollama.py)
- Local LLM = no API costs, no data leakage
- Graceful fallback: structured template-based insights when Ollama unavailable
- The research's hypothesis generation is the single most novel contribution — worth implementing even in degraded form

---

## Appendix: File Map

| File | Action | LOC Est. |
|---|---|---|
| `whitemagic/core/memory/graph_engine.py` | CREATE | ~400 |
| `whitemagic/core/memory/bridge_synthesizer.py` | CREATE | ~200 |
| `whitemagic/core/dreaming/dream_cycle.py` | MODIFY | +100 |
| `whitemagic/core/memory/consolidation.py` | MODIFY | +80 |
| `tests/unit/test_graph_engine.py` | CREATE | ~200 |
| `tests/unit/test_bridge_synthesizer.py` | CREATE | ~100 |
| `tests/unit/test_dream_cycle_v14_1.py` | CREATE | ~150 |
| `pyproject.toml` | MODIFY | +1 line |
| `whitemagic/tools/handlers/introspection.py` | MODIFY | +30 |
| `whitemagic/tools/dispatch_table.py` | MODIFY | +10 |
| Docs (CHANGELOG, VERSION, AI_PRIMARY, etc.) | MODIFY | ~50 |
| **TOTAL** | | **~1,320 LOC** |

---

*Document created: Feb 10, 2026*
*Target version: v14.1.0*
*Codename: "The Dreaming Graph"*
