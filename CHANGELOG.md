# Changelog

All notable changes to WhiteMagic are documented in this file.

Format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).
Versioning follows [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

## [14.1.0] — 2026-02-10

### MCP 3.0 Upgrades, Multi-Galaxy, Ollama Agent Loop, Edgerunner Violet

#### Added
- **MCP Server Instructions** — 4,858-char markdown doc auto-injected into AI client context at startup (`mcp_instructions.md`)
- **Streamable HTTP transport** — `--http` flag starts uvicorn/starlette MCP server on port 8770
- **Per-Gana tool icons** — 28 lunar mansion Chinese character SVG data-URIs embedded in tool metadata
- **Task-optional execution modes** — 5 slow Ganas marked TASK_OPTIONAL (abundance, three_stars, extended_net, chariot, ghost)
- **MCP Registry listing** — `mcp-registry.json` for official MCP registry submission
- **3 MCP resources** — `ai-primary`, `server-instructions`, `system-map` readable by clients
- **Multi-Galaxy system** (`core/memory/galaxy_manager.py`) — Project-scoped memory databases. 6 new tools: `galaxy.create`, `galaxy.switch`, `galaxy.list`, `galaxy.status`, `galaxy.ingest`, `galaxy.delete`. Registry at `~/.whitemagic/galaxies.json`.
- **Ollama Agent Loop** (`handlers/ollama_agent.py`) — `ollama.agent` tool runs an agentic loop where local LLMs autonomously call WhiteMagic tools. Injects relevant memories, parses tool-call blocks, max 10 iterations.
- **Quickstart seed memories** (`scripts/seed_quickstart_memories.py`) — 12 foundational guide memories covering system overview, 5D memory, governance, dream cycle, multi-galaxy, local AI, and quick reference.
- **Edgerunner Violet Security Layer** — 15 new tools across 6 features:
  - MCP Integrity (SHA-256 schema fingerprinting): `mcp_integrity.snapshot`, `.verify`, `.status`
  - Model Signing (OMS-compatible): `model.register`, `.verify`, `.list`, `.hash`, `.signing_status`
  - Engagement Tokens (HMAC-SHA256 scoped auth): `engagement.issue`, `.validate`, `.revoke`, `.list`, `.status`
  - Security Monitor (anomaly detection): `security.alerts`, `.monitor_status`
  - Violet Dharma Profile (5 security rules)
  - Crypto-chained Karma with ops_class field
- **Simplified tool aliases** — `remember`, `recall`, `think`, `check`, `dream` as shorthand Gana tools
- **Auto-context injection** — `session_bootstrap` auto-loads quickstart guides and recent memories

#### Fixed
- **Surprise gate stats** — `evaluate()` now increments `_total_evaluations` on all code paths (including when embeddings are unavailable)
- **Drive bias test** — Fixed mock injection for `_drive_bias` caution boost test
- **Ingest targets** — Fixed session handoff paths (moved from root to `docs/sessions/`)
- **PRAT orphans** — Added `galaxy.*`, `ollama.agent` registry definitions; all PRAT mappings now have backing registry entries

#### Phase 2: Cognitive Enrichment (Living Graph Intelligence Layers)
- **HNSW Approximate Nearest-Neighbor Index** — Integrated `hnswlib` 0.8.0 into `EmbeddingEngine`. O(log N) search replaces O(N) brute-force for both hot and cold DB. Params: ef_construction=200, M=32, ef=100 (recall ~99%). Graceful fallback to numpy brute-force if hnswlib unavailable. `embedding_stats()` reports HNSW status.
- **Entropy & Abstraction Scoring** (`core/memory/entropy_scorer.py`) — Shannon entropy (normalized) for information density, abstraction level via concrete/abstract keyword detection, vocabulary richness (type-token ratio). Composite score = 0.6×entropy + 0.4×abstraction. `RetentionEngine` evaluator plugin (weight=0.15). Batch sweep with optional metadata persistence.
- **Causal Edge Mining** (`core/memory/causal_miner.py`) — Directed causal edges between memories. Blends semantic similarity (0.50), temporal proximity (0.35), and tag overlap (0.15). Exponential decay temporal window (24h half-life, 7d max). Relation types: `led_to`, `influenced`, `preceded`, `related_to`. Persists as directed associations in the Living Graph.
- **UMAP Visualization** (`core/memory/umap_projection.py`) — Projects 384-dim embeddings to 2D/3D via UMAP (n_neighbors=15, min_dist=0.1, cosine metric). Optional k-means clustering on projected coordinates. Memory metadata hydration (title, tags, importance, galactic_distance). Result caching with vector count invalidation.

#### PyPI & Build Preparation
- **License format** — Updated to PEP 639 SPDX identifier (`license = "MIT"`)
- **New optional dependency groups** — `graph` (networkx), `search` (hnswlib), `viz` (umap-learn, scikit-learn)
- **Makefile build target** — `make build` runs `python -m build --sdist --wheel`
- **Machine-readable discovery files synced** — `.well-known/agent.json`, `llms.txt`, `llms-full.txt`, `mcp-registry.json` all updated to v14.1.0 with 208 tools

#### Metrics
- **Tools**: 186 → 208 (22 new: 15 Violet + 6 galaxy + 1 ollama.agent)
- **Nested MCP tool enums**: 180 across 28 Ganas
- **Tests**: 1,656 passed, 0 failures (46 new: 15 entropy + 20 causal + 11 UMAP)
- **Benchmarks**: 36/36 gauntlet + 22/22 MCP = 58/58

---

## [14.0.0] — 2026-02-10

### The Living Graph (Super-Phase 1)

Activates the association graph as a first-class reasoning substrate. Memories are no longer isolated records — they form a living, evolving network with weighted edges, multi-hop traversal, topology-aware governance, and surprise-gated ingestion.

#### Added
- **Graph Walker** (`core/memory/graph_walker.py`) — Multi-hop weighted random walk over the association graph. Transition probability: P(v|u) ∝ Strength × Gravity^α × Recency × (1-Staleness)^β. Returns ranked paths with edge weights and relation types.
- **Graph Engine** (`core/memory/graph_engine.py`) — networkx-based topology engine providing eigenvector/betweenness/PageRank centrality, Louvain community detection, bridging centrality, and echo chamber detection via centrality snapshot drift.
- **Surprise Gate** (`core/memory/surprise_gate.py`) — Novelty-gated memory ingestion. Computes surprise as S = -log₂(max_cosine_similarity). High surprise → boost importance + emit NOVEL_CONCEPT. Low surprise → reinforce existing memory instead of creating duplicate.
- **Bridge Synthesizer** (`core/memory/bridge_synthesizer.py`) — Generates hypotheses about why bridge nodes connect disparate communities. Uses LLM (Ollama) with template fallback. Persists insights as LONG_TERM memories.
- **Entity Resolution** (`consolidation.py`) — Embedding-based deduplication via `resolve_entities()`. Finds near-duplicate memories by cosine similarity, merges tags into canonical, pushes duplicates to FAR_EDGE.
- **Hybrid Recall** (`unified.py`) — `hybrid_recall()` combines BM25 + embedding anchor search with graph walk expansion for multi-hop memory reasoning.
- **Dream Cycle GOVERNANCE phase** — Echo chamber detection and inhibition. Identifies nodes with centrality spikes without new data, injects inhibitory signals.
- **Enhanced SERENDIPITY phase** — Now uses graph engine for bridge detection and bridge synthesizer for insight generation during dream cycles.
- **Association Strength Decay** (`sqlite_backend.py`) — Exponential decay for episodic edges (half-life 30 days), power-law decay for semantic edges. Wired into lifecycle sweep as Phase 4.
- **Hebbian Strengthening** (`sqlite_backend.py`) — Co-accessed memories get their association edge reinforced via `hebbian_strengthen()`.
- **5 new MCP tools**: `hybrid_recall`, `graph_topology`, `graph_walk`, `surprise_stats`, `entity_resolve`
- **Gnosis portal** — New `living_graph` section aggregating graph engine, walker, surprise gate, and bridge synthesizer stats.

#### Changed
- **Association schema migration** — 6 new columns on `associations` table: `direction`, `relation_type`, `edge_type`, `valid_from`, `valid_until`, `ingestion_time`. 4 new indexes for graph traversal queries.
- **Association miner** — `mine()` and `mine_semantic()` now populate v14 columns (edge_type, direction, relation_type).
- **Memory store** — `unified.py store()` now evaluates content through surprise gate before persisting.
- **Dream cycle** — 5 → 6 phases (added GOVERNANCE between SERENDIPITY and KAIZEN).
- **Lifecycle sweep** — Phase 4 added for association strength decay.

#### Metrics
- **Tests**: 1,484 total (43 new in `test_living_graph.py`), 0 regressions
- **New files**: 5 modules + 1 test file + 1 handler + 1 registry def
- **Modified files**: 9 (sqlite_backend, association_miner, unified, lifecycle, dream_cycle, consolidation, gnosis, conftest, 2 existing test files)

---

## [13.6.0] — 2026-02-10

### Cold Storage Awakening (Leap 2)

Brings the other 95% of the memory corpus online with semantic embeddings, wires cross-DB search, ingests development history archives, and performs content-mining data quality fixes.

#### Added
- **Cold DB embedding indexing** (`scripts/cold_db_embed.py`) — Batch-encodes all 105,194 cold storage memories with MiniLM-L6-v2 (384 dims). Resumable, supports `--batch-size`, `--limit`, `--dry-run`. Stores in `memory_embeddings` table within the cold DB. Rate: ~20/sec on CPU.
- **Cross-DB semantic search** (`embeddings.py`) — `search_similar()` now accepts `include_cold=True` to search both hot and cold DB embeddings. Hot-first, cold-fallback with deduplication. Separate in-memory vector cache for cold DB (`_load_cold_vec_cache`). Results tagged with `source: 'hot'` or `source: 'cold'`.
- **Cold DB connection in EmbeddingEngine** — `_get_cold_db()` lazy-connects to cold DB, checks for `memory_embeddings` table, retries if table not yet created (supports concurrent encoding).
- **Hybrid search with cold storage** (`unified.py`) — `search_hybrid()` now accepts `include_cold` parameter, passes through to embedding engine. Cold memories tagged with `storage_tier: 'cold'` metadata.
- **Archive ingestion** (`scripts/ingest_archives.py`) — Ingested 15 development history files (8 session handoffs, dream journal, AI Primary, System Map, Strategic Roadmap, Vision, Polyglot Status/Strategy) into hot DB as LONG_TERM memories with semantic embeddings. Total: 5,623 hot memories.
- **Content mining data quality fixes** — Deduplicated 10→1 "Singularity of Wisdom" copies (9 pushed to FAR_EDGE). Demoted 6 mypy cache artifacts from importance 0.95→0.20.

#### Changed
- **`embedding_stats()`** now reports `hot_embeddings`, `cold_embeddings`, and `total_embeddings` separately.
- **`search_similar()`** return dicts now include `source` field (`'hot'` or `'cold'`).

#### Metrics
- **Tests**: 772 unit tests passed, 0 failed (20 new in `test_cold_storage_embeddings.py`)
- **Hot DB**: 5,623 memories, 5,562 embeddings
- **Cold DB**: 105,194 memories, encoding in progress (~20/sec)
- **Archive ingested**: 15 files (session handoffs + key architecture docs)
- **Data quality**: 9 duplicates archived, 6 mypy artifacts demoted

---

## [13.5.0] — 2026-02-10

### Semantic Memory Revolution (Leap 1)

Replaces keyword Jaccard association mining with true semantic similarity via embedding cosine, adds embedding-powered deduplication, and introduces hybrid retrieval combining BM25 + embeddings via Reciprocal Rank Fusion (RRF).

#### Added
- **Semantic association mining** (`mine_semantic()` in `association_miner.py`) — Uses embedding cosine similarity instead of keyword Jaccard for association discovery. Strong threshold (≥0.70) and weak threshold (≥0.50) with automatic Knowledge Graph feeding for strong links. Falls back to keyword mining when embeddings unavailable.
- **Embedding-powered deduplication** (`find_duplicates()` in `embeddings.py`) — Finds near-duplicate memory pairs via cosine similarity ≥0.95. Catches semantic duplicates (same meaning, different wording) that MinHash misses.
- **Pairwise similarity search** (`find_similar_pairs()` in `embeddings.py`) — Batch pairwise cosine computation across all cached embedding vectors with configurable threshold and max pairs. Uses Zig SIMD acceleration when available.
- **Hybrid retrieval pipeline** (`search_hybrid()` in `unified.py`) — Combines BM25 lexical search (Rust) + embedding semantic search via Reciprocal Rank Fusion (RRF, k=60). Tags results with retrieval channel metadata (`lexical`, `semantic`, or `lexical+semantic`). Falls back gracefully: BM25-only if no embeddings, FTS-only if no Rust.
- **Strategic Roadmap** (`docs/STRATEGIC_ROADMAP.md`) — 7-leap plan from v13.4 to v14.0 release, including the Gratitude Architecture economic layer (Leap 5.5) with XRPL tip jar, x402 micropayments, MCP registry listings, OpenClaw skill, llms.txt, and A2A Agent Card.

#### Metrics
- **Tests**: 753 unit tests passed, 0 failed (21 new in `test_semantic_memory.py`)
- **New methods**: 4 (`mine_semantic`, `find_similar_pairs`, `find_duplicates`, `search_hybrid`)

---

## [13.4.0] — 2026-02-10

### Semantic Embedding Layer + Data Quality Overhaul

Populates the semantic embedding cache, runs constellation detection, fixes data quality issues across the memory core, and optimizes embedding search with an in-memory vector cache.

#### Added
- **Semantic embedding indexing** — 5,547 LONG_TERM memories encoded with `all-MiniLM-L6-v2` (384 dims). Cached in `memory_embeddings` SQLite table. Semantic search verified end-to-end.
- **Constellation detection** — 19 constellations discovered across 5,608 memories in 5D holographic space. Largest cluster: 4,230 members. Fed into Knowledge Graph.
- **In-memory vector cache** in `embeddings.py` — `_load_vec_cache()` keeps unpacked vectors in RAM after first DB load. Auto-invalidated on new embeddings. **28.6× speedup** on repeated queries (18s cold → 629ms warm).
- **Unique constellation naming** — `_generate_name()` now tracks used names and appends roman numeral suffixes (II, III, IV…) to prevent collisions. 19/19 names unique.

#### Changed
- **Tag cleanup** — Removed 6,416 bulk Go module tags (`go`, `mod`, `pkg`, `toolchains`), 584 version-specific tags. Consolidated `github.com`/`golang.org`/`go.opentelemetry.io` → `golang_ecosystem`. Tags: 20,718 → 13,718 (−34%), unique: 1,373.
- **Importance recalibration** — Demoted 1,712 bulk-ingested external files (Go modules → 0.20, bitnet/llama → 0.25, testdata → 0.20). Boosted 25 WM-relevant memories (conversations → 0.80+, WM-tagged → 0.75+).
- **Protected memory audit** — Unprotected 52 misaligned external files (deep_archive/scavenged). Protected 120 high-importance WM conversation memories. Net: 96 → 164 protected.
- **Zone rebalancing** — Pushed 1,699 low-importance memories to OUTER_RIM, pulled 330 high-importance WM memories to INNER_RIM, pushed 52 deep_archive/scavenged to FAR_EDGE. Distribution: CORE 42, INNER_RIM 432, MID_BAND 3,082, OUTER_RIM 2,000, FAR_EDGE 52.
- **Batch indexing optimization** — `index_memories()` now does raw DB inserts and invalidates vector cache once at the end (was invalidating per row).

#### Metrics
- **Tests**: 1203 passed, 0 failed (zero regressions)
- **Embedding cache**: 5,547 vectors (384 dims each, ~8.1MB)
- **Search performance**: Cold 18s → Warm 629ms (28.6× speedup)
- **Constellations**: 19 detected, all uniquely named
- **Tags**: 13,718 total, 1,373 unique (was 20,718)
- **Protected**: 164 memories (was 96)
- **Zone distribution**: CORE 42 / INNER_RIM 432 / MID_BAND 3,082 / OUTER_RIM 2,000 / FAR_EDGE 52

---

## [13.3.3] — 2026-02-10

### SQLite Performance Optimization (P6/P7) + Accelerator Full Wiring

Completes the deferred P6 (prepared statements) and P7 (io_uring) performance optimizations via practical alternatives, and wires remaining polyglot accelerators into hot paths.

#### Added
- **P6a: Memory-mapped I/O** — `PRAGMA mmap_size=268435456` (256MB) on all SQLite connections. Bypasses read() syscalls; OS page cache serves data directly.
- **P6b: 64MB page cache** — `PRAGMA cache_size=-65536` (was default ~2MB). Avoids re-reading hot pages.
- **P6c: In-memory temp store** — `PRAGMA temp_store=MEMORY`. Eliminates temp file I/O for sorting/grouping.
- **P6d: Busy timeout** — `PRAGMA busy_timeout=5000` centralized in connection pool.
- **6 new SQLite indexes** — `galactic_distance`, `memory_type`, `neuro_score`, `accessed_at`, `associations.source_id`, `is_protected`. Covers all hot query patterns.
- **Zig SIMD batch cosine** wired into `embeddings.py` `search_similar()` — replaces pure-Python serial cosine loop with `batch_cosine()` from `simd_cosine.py`.
- **Rust rate limiter** cached at module level in `rate_limiter.py` — eliminates per-call import overhead (was re-importing on every `check()`).

#### Changed
- **PRAGMA centralization** — All SQLite PRAGMAs now set in `db_manager.ConnectionPool._create_connection()`. Removed redundant PRAGMAs from `sqlite_backend._init_db()` and `store()`.
- **Rate limiter architecture** — Rust atomic pre-check (0.45μs) runs first; if Rust blocks, immediate return. Python configurable per-tool/global limits always enforced as secondary check.
- **Embedding cache DB** — Now uses same PRAGMA tuning (mmap, cache, temp_store) as main DB.

#### Metrics
- **Tests**: 1203 passed, 0 failed (zero regressions)
- **Python**: 763 files, 140,913 LOC
- **Polyglot**: 24,694 LOC across 8 languages (14.9% of total)
- **Hot DB**: 1.48GB, 5,608 memories | **Cold DB**: 5.05GB, 105,194 memories

#### Accelerator Wiring Audit (v13.3.3)
| Hot Path | Accelerator | Status |
|---|---|---|
| Embedding similarity search | Zig SIMD batch_cosine | **NEW** (was pure Python) |
| Constellation 5D distance | Zig SIMD holographic_5d | Already wired |
| Rate limiting pre-check | Rust atomic (cached) | **UPGRADED** |
| Galactic Map full_sweep | Rust galactic_batch_score | Already wired |
| Galactic decay_drift | Rust sqlite_accel | Already wired |
| Zone stats | Rust sqlite_zone_stats | Already wired |
| Keyword extraction | Rust PyO3 keyword_extract | Already wired |
| Retrieval pipeline | Rust multi-pass reranker | Already wired |
| MinHash near-duplicate | Rust minhash | Already wired |
| Holographic encoding | Rust PyO3 batch encoder | Already wired |

---

## [13.3.2] — 2026-02-09

### Memory Core Optimization + Cold Storage Tiering

Deep optimization pass across the entire memory subsystem: association pruning, tag cleanup, holographic recalibration, constellation-based recall, cold storage tiering, Rust keyword extraction, semantic embedding layer, and GPU roadmap.

### Added

- **`whitemagic/core/memory/embeddings.py`** — Semantic embedding layer using sentence-transformers (MiniLM-L6-v2, 384 dims). Lazy-loaded model, SQLite-cached embeddings, batch encoding, cosine similarity search. Graceful fallback when sentence-transformers not installed.
- **`whitemagic-rust/src/keyword_extract.rs`** — Rust PyO3 keyword extraction (replaces Zig ctypes path). Zero-copy string borrowing, HashSet stopword filtering, frequency-based top-N selection. Registered in `lib.rs` with `keyword_extract` and `keyword_extract_batch` PyO3 functions.
- **`docs/GPU_ACCELERATION_ROADMAP.md`** — Comprehensive GPU acceleration roadmap: 6 targets ordered by impact, language selection guide (Mojo/CUDA/Rust+wgpu/Triton), what stays CPU-only, implementation phases, hardware requirements.
- **Cold storage fallback** in `sqlite_backend.py` — `recall()` now transparently falls back to cold DB (`whitemagic_cold.db`) when a memory is not found in hot DB. Lazy-loaded cold pool.
- **Constellation-based recall** in `unified.py` — `search()` now annotates results with constellation context (name, zone, distance, dominant tags) when the detector has cached results. New `find_nearest_constellation()` and `annotate_memories()` methods on `ConstellationDetector`.
- **`COLD_DB_PATH`** in `config/paths.py` — Canonical path for cold storage DB.
- **Rust keyword bridge** in `rust_accelerators.py` — `keyword_extract()`, `keyword_extract_batch()`, `rust_keywords_available()` functions with Python fallback.

### Changed

- **Association miner** (`association_miner.py`) — Zig SIMD keyword path disabled (benchmarked 15× slower than Python due to ctypes overhead). Replaced with Rust PyO3 fast path for texts >200 chars, Python regex fallback for all sizes.
- **Hot DB size**: 5.76 GB → 1.56 GB (105,194 deep_archive + scavenged memories moved to cold storage)
- **Associations**: 27.8M → 19.0M (pruned 8.8M weak links below strength 0.30, avg strength 0.468 → 0.607)
- **Tags**: 129,712 → 20,718 (removed 88K "archive" + 19K "scavenged" + 2K "recovered" structural tags)
- **Importance**: 3,486 memories recalibrated (54 session handoffs protected at 0.95, WM-relevant content boosted to 0.65-0.90, 2 empty memories dampened to 0.30)
- **Holographic centroid** (LONG_TERM): Z-axis -0.202 → -0.014 (near-centered), CORE Z-axis -0.478 → -0.039
- **POLYGLOT_STATUS.md** — Updated to v13.3.2 with Rust keyword extraction entry

### Database Operations

- **A1**: `DELETE FROM associations WHERE strength < 0.30` — removed 8,805,898 weak associations
- **A3**: Removed structural tags: archive (88,331), scavenged (18,789), recovered (1,874)
- **B1**: Re-encoded 5,604 LONG_TERM + protected memories with fresh holographic coordinates
- **B3**: Importance recalibrated for 4,329 memories with default importance (0.49-0.51)
- **C1**: Migrated 105,194 memories (with tags, associations, coords, FTS) to cold storage DB

### Metrics

- **Tests**: 1203 passed, 0 failed
- **Hot DB**: 5,608 memories (5,547 LONG_TERM, 48 deep_archive, 5 PATTERN, 4 scavenged, 4 SHORT_TERM)
- **Cold DB**: 105,194 memories (86,409 deep_archive, 18,785 scavenged)
- **Total DB size**: 1.56 GB hot + 5.30 GB cold = 6.86 GB (was 5.76 GB single)
- **Protected memories**: 96
- **Rust**: cargo check clean (keyword_extract.rs added, 1 pre-existing warning)

### Performance Optimizations (P1-P7)

- **P1: Batch rate checking** — `rate_check_batch()` in Rust, single FFI call for N tools. At batch-100: 2.21M ops/sec (0.45μs/tool). Registered in `lib.rs`, Python bridge in `rust_accelerators.py`.
- **P2: Multi-pass retrieval pipeline** — `retrieval_pipeline.rs` (512 LOC): 7-stage composable pipeline (text score → type filter → tag filter → min-importance → importance+recency rerank → holographic proximity boost → dedup → finalize). 10 candidates: 31μs, 50: 181μs, 200: 591μs, 500: 1.67ms. Wired into `unified.py search_similar()`.
- **P3: Zero-copy strings** — PyO3 `keyword_extract` borrows Python strings directly. Rust wins 1.8× on 6K+ texts, Python wins on short texts (crossover ~1-2K chars).
- **P4: Shared memory (mmap)** — `shared_state.rs`: 16KB memory-mapped file at `~/.whitemagic/cache/shared_state.bin`. Cross-process atomic counters for rate checks, pipeline calls, diagnostics. Lock-free via `AtomicU64`.
- **P5: Batch association mining** — `keyword_extract_batch` wired into `association_miner.py` fallback path. Single FFI call extracts keywords for all N memories vs N serial calls.
- **P6/P7: SQLite prepared statements and io_uring** — Deferred (Python sqlite3 doesn't expose stmt cache control; io_uring requires async runtime redesign).

### Performance Benchmarks (release build, maturin develop --release)

| Operation | Latency | Throughput | Notes |
|-----------|---------|------------|-------|
| Single rate_check | 0.45μs | 2.21M ops/sec | PyO3 overhead is sub-microsecond |
| Batch rate_check (100 tools) | 45μs/call | 2.21M ops/sec | Same throughput, amortized FFI |
| Keyword extract (6K text) | 161μs | 1.8× vs Python | PyO3 zero-copy string borrow |
| Keyword extract (300 text) | 22μs | 0.8× vs Python | Python wins on short texts |
| Pipeline (10 candidates, 7 stages) | 31μs | 31.8K/sec | Entire rerank in one FFI call |
| Pipeline (50 candidates) | 181μs | 5.5K/sec | Tag filter + importance + dedup |
| Pipeline (200 candidates) | 591μs | 1.7K/sec | Scales linearly |
| Pipeline (500 candidates) | 1.67ms | 600/sec | N² dedup is the bottleneck |

---

## [13.3.1] — 2026-02-09

### Polyglot Benchmarking, Distillation & Memory Mining

Comprehensive head-to-head benchmarks of all polyglot accelerators, intelligence shim tree elimination, and first full-corpus association mining + constellation detection on the unified 110K memory core.

### Added

- **`scripts/benchmark_polyglot.py`** — Full polyglot benchmark suite: Zig SIMD (keywords, cosine, distance matrix), Rust PyO3 (holographic, MinHash, BM25, rate limiter), Python baselines. 1000-iteration runs with mean/median/min/p99
- **`SESSION_HANDOFF_2026_02_09_AFTERNOON.md`** — Detailed handoff with objectives A (quick wins), B (medium effort), C (future GPU acceleration roadmap)
- **50 new associations** discovered via cross-zone mining (500-memory sample)
- **6 constellations** detected in 5D holographic space (largest: 47,381 members)

### Changed

- **Benchmark results** in `docs/POLYGLOT_STATUS.md` — Complete Rust, Zig, and Python tables with head-to-head speedups. Key finding: Zig distance matrix 13× faster, Zig keywords 15× slower (ctypes overhead)
- **All `whitemagic.intelligence.*` imports** rewired to canonical `whitemagic.core.intelligence.*` (45+ files, 60+ import statements)
- **`whitemagic/__init__.py`** — Lazy module paths updated from `intelligence.*` to `core.intelligence.*`
- **`whitemagic/cli/cli_fast.py`** — String-based lazy imports updated to canonical paths

### Removed

- **Intelligence shim tree** — 38 files, 153 LOC of `__getattr__` redirect shims in `whitemagic/intelligence/`. Archived to `wm_archive/phase7_intelligence_shims/`
- **8 dead/shim root files** archived to `wm_archive/phase8_loose_root/`: `haskell_bridge.py`, `patterns.py`, `recall.py`, `stats.py`, `strategy.py`, `symbolic.py`, `resonance.py`, `ai_contract.py`
- **`resonance.py`** redirect shim — 2 importers rewired to `core.resonance`

### Fixed

- **`test_kaizen_apply_fixes_dry_run`** — Pre-existing test failure caused by stale mock path `whitemagic.intelligence.synthesis...` → `whitemagic.core.intelligence.synthesis...`

### Metrics

- **Tests**: 1203 passed, 0 failed (was 1202 + 1 pre-existing failure)
- **Active Python files**: 762 (was 808)
- **Active Python LOC**: 140,223 (was 142,397)
- **Associations**: 27.8M across 106K sources
- **Holographic coverage**: 97.3% (107,821 / 110,802)
- **Constellations**: 6 detected

---

## [13.3.0] — 2026-02-09

### Memory Database Unification — Split-Brain Resolved

The two separate memory databases (Primary: 3,631 memories, Legacy Galaxy: 107,168 memories) have been merged into a single unified source of truth with 110,802 memories.

### Added

- **`scripts/merge_databases.py`** — Database merge tool: schema alignment, memory/tag/coord/FTS transfer with zero ID collisions
- **Benchmark Results** in `docs/POLYGLOT_STATUS.md` — Rust accelerator performance numbers with hardware specs (50–111× speedups)
- **3 session handoff files** ingested as protected LONG_TERM memories in INNER_RIM

### Changed

- **Memory DB unified**: Primary (31 MB, 3,631 memories, no galactic fields) merged into Legacy Galaxy (7.1 GB, 107,168 memories, full 5D). Single DB at `~/.whitemagic/memory/whitemagic.db` with 110,802 memories
- **Galactic sweep**: All 110,802 memories re-scored with 7-signal retention scorer. Zone distribution: 39 CORE, 241 INNER_RIM, 37,345 MID_BAND, 73,096 OUTER_RIM, 78 FAR_EDGE
- **Memory consolidation**: 3,193 SHORT_TERM memories promoted to LONG_TERM (importance > 0.8)
- **Schema extended**: `gana_processed`, `gana_context`, `quadrant` columns added to unified DB

### Removed

- **5 dead files** (924 LOC, 0 importers) archived to `wm_archive/phase6_dead_files/`: `cdn.py`, `summaries.py`, `arrow.py`, `diagnose.py`, `shell.py`
- **Primary DB** archived — split-brain eliminated

### Metrics

- **Tests**: 1203 passed, 0 failed
- **Total memories**: 110,802 (was 107,168 + 3,631 separate)
- **Protected memories**: 42 (was 39)
- **Active Python files**: 808 (was 813)

---

## [13.2.0] — 2026-02-09

### Polyglot Expansion Closeout

Every module across all 9 languages (Python, Rust, Zig, Haskell, Elixir, Mojo, Go, Julia, TypeScript) now has a Python bridge with graceful fallback. The polyglot expansion plan from `POLYGLOT_EXPANSION_STRATEGY.md` is **complete**.

### Added

- **3 new polyglot source files** (1,038 LOC):
  - `elixir/lib/whitemagic_core/redis_bridge.ex` (374 LOC) — Redis bridge with backpressure, echo suppression, temporal lane classification, reconnection with exponential backoff
  - `whitemagic-julia/src/memory_stats.jl` (345 LOC) — Statistical memory analysis: importance distributions, Markov zone transitions, MAD-based outlier detection, cluster significance testing
  - `whitemagic-julia/src/self_model_forecast.jl` (319 LOC) — Holt-Winters exponential smoothing with confidence intervals, anomaly detection via residual z-scores, multi-metric correlation
- **8 Python bridge files** in `core/acceleration/` (1,841 LOC):
  - `simd_holographic.py` — Zig SIMD 5D distance, KNN, centroid
  - `simd_constellation.py` — Zig SIMD grid density scan, flood-fill merge
  - `simd_vector_batch.py` — Zig SIMD batch top-K cosine, normalize, centroid
  - `mojo_bridge.py` — Mojo subprocess bridge for batch_encoder, embedding_quantize, neuro_batch
  - `haskell_bridge.py` — Haskell FFI for BoundaryDetection, MaturityGate, RuleComposer
  - `elixir_bridge.py` — Elixir OTP bridge via Redis for cascade_executor, garden_pubsub, harmony_monitor, redis_bridge
  - `go_mesh_bridge.py` — Go mesh bridge via Redis for gossip, agent_stream
  - `julia_bridge.py` — Julia subprocess bridge for memory_stats, self_model_forecast
- **Rust bridge additions** (~190 LOC in `rust_accelerators.py`) — BM25 search engine + atomic rate limiter functions

### Consumer Wiring (bridges integrated into hot paths)

- `constellations.py` → Zig SIMD holographic 5D distance
- `vector_search.py` → Zig SIMD batch top-K cosine (>50 vectors)
- `input_sanitizer.py` → Haskell BoundaryDetection (first-pass check)
- `maturity_check.py` → Haskell MaturityGate (advisory check)
- `rate_limiter.py` → Rust atomic rate limiter (lock-free fast path)
- `self_model.py` → Julia Holt-Winters batch forecasting
- `nexus/src/lib/api.ts` → TypeScript SDK wmClient singleton

### Metrics

- **Tests**: 1203 passed, 0 failed
- **Polyglot languages**: 9 (Python + Rust, Zig, Haskell, Elixir, Mojo, Go, Julia, TypeScript)
- **Total new LOC this version**: ~3,069

---

## [13.1.0] — 2026-02-09

### Polyglot Core Expansion + Wiring

Structural distillation, new polyglot core modules, and wiring Rust/Zig accelerators into Python hot paths.

### Added

- **3 new Rust modules** (+980 LOC, total 10,228):
  - `holographic_encoder_5d.rs` (387 LOC) — 5D XYZWV coordinate encoding, Rayon batch parallelism, garden/element blending, 3 PyO3 functions
  - `minhash.rs` (248 LOC) — 128-hash MinHash LSH near-duplicate detection, 2 PyO3 functions
  - `sqlite_accel.rs` (345 LOC) — rusqlite batch operations: galactic updates, decay drift, FTS5 search, zone stats, 5 PyO3 functions
- **2 new Zig SIMD modules** (+608 LOC, total 2,170):
  - `keyword_extract.zig` (338 LOC) — SIMD 16-byte tokenizer, vectorized lowercase, comptime bloom filter stopwords
  - `distance_matrix.zig` (270 LOC) — SIMD pairwise cosine with 64×64 blocked tiling, top-K nearest neighbors
- **2 Zig ctypes bridges**: `simd_keywords.py` (~180 LOC), `simd_distance.py` (~230 LOC)

### Rust Accelerators Wired (via maturin develop --release)

- `holographic_encoder_5d` → `core/intelligence/hologram/encoder.py` (single + batch fast path)
- `minhash` → `core/memory/consolidation.py` (near-duplicate pre-filter in `_cluster_by_tags()`)
- `sqlite_accel` → `core/memory/galactic_map.py` (decay_drift + zone_stats)

### Distillation

- `systems/` (319 LOC, 18 importers) rewired to `core/`, archived to `wm_archive/phase_systems_archived/`
- 15 root `cli_*.py` shims removed — all callers rewired to `whitemagic.cli.cli_*`, archived to `wm_archive/phase_cli_shims/`
- `pyproject.toml` entry points + `__main__.py` updated to canonical paths

### Fixed

- 4 pre-existing Zig syntax errors (`->` return type in `holographic_5d.zig`, `constellation.zig`)
- Broken venv path (stale `VIRTUAL_ENV` in `.venv/bin/activate`)
- Stale `whitemagic_rs.so` in `~/.local/lib/` shadowing venv install

### Metrics

- **Tests**: 1203 passed, 0 failed (up from 1199)
- **Rust**: cargo check clean (1 pre-existing warning)
- **Zig**: zig build clean

---

## [13.0.0] — 2026-02-09

### The Public Release

The first public-ready release of WhiteMagic. Major structural cleanup, documentation overhaul, deprecation of legacy paths, and a complete polyglot expansion strategy.

### Added

- **`benchmarks/performance_suite.py`** — Restored from archive and modernized. Import latency, tool dispatch overhead, custom callable timing, and state footprint benchmarks. Stdlib-only.
- **`safety/resource_limiter.py`** — Rewritten from scratch. Resource monitoring and enforcement (thread count, memory, CPU). Stdlib-only — no psutil dependency. Context manager `resource_guard()` for safe execution.
- **`cli/doctor.py`** — Extracted health/doctor/immune/homeostasis CLI commands from monolithic `cli_app.py`. New `register_all()` hub in `cli/__init__.py`.
- **`autonomous/executor/`** — New subpackage merging `autonomous_execution/` into `autonomous/`. Contains `ContinuousExecutor`, `ObjectiveGenerator`, `ProgressAssessor`, `ResourceLimits`.
- **`CONTRIBUTING.md`** — Fully rewritten for v13 architecture (dispatch table path, extras groups, polyglot builds, test baseline).
- **`docs/VISION.md`** — Comprehensive philosophy document: White Magician as Memory-Keeper, three-layer memory model, local-first principles, strategic direction.
- **`docs/POLYGLOT_EXPANSION_STRATEGY.md`** — 7-phase expansion plan (Rust core → Zig SIMD → TypeScript SDK → Mojo → Elixir → Haskell → Go). Benchmark protocol, priority order, risk mitigation.
- **`PRIVACY_POLICY.md`** — Ported from archive, updated for local-first model (no telemetry by default).
- **`TERMS_OF_SERVICE.md`** — Ported from archive, updated for v13 feature set.

### Changed

- **`core/bridge/`** — Marked as **deprecated**. Emits `DeprecationWarning` at import time. All new tool development must use `tools/handlers/` via `unified_api.call_tool()`.
- **`autonomous_execution/`** — Now a backward-compatibility shim that re-exports from `autonomous.executor`. Emits `DeprecationWarning`.
- **Dashboard** (`wmfrontend/dashboard-app`) — Version bumped to 13.0.0, tool count fallback updated to 178, subtitle shows "28 Ganas".
- **Version references** — Updated across `pyproject.toml`, `VERSION`, `README.md`, `AI_PRIMARY.md`, `SYSTEM_MAP.md`.

### Prior Work (v12.8 → v13.0 prep)

- Zero-state first-run validated (152MB install, 256KB state, 0 errors)
- Empty directories cleaned (`collaboration/`, `dashboard/`, `integrations/`)
- Broken shims fixed (`benchmarks/__init__.py`, `safety/__init__.py`)
- Mypy per-package config: 2,454 → 1,226 errors
- Mojo updated to 0.26.1 (13 categories of breaking changes)
- All 6 polyglot languages verified clean

### Metrics

- **Tests**: 1199 passed, 4 skipped
- **MCP tools**: 178 (28 Gana meta-tools in PRAT mode)
- **Active fusions**: 28
- **Polyglot languages**: 7 (Python + Rust, Haskell, Mojo, Elixir, Zig, Go)

---

## [12.8.0] — 2026-02-08

### 28 Fusions Complete (The Sacred Number)

All cross-system fusions wired — 28 active fusions matching the 28 Ganas.

### Added

- 5 final fusion functions in `core/fusions.py`: Gana Chain → Harmony Vector, PRAT Router → Gana Chain, Mojo SIMD → Holographic Encoding, Elixir Event Bus → Gan Ying, Go Mesh → Memory Sync
- `tests/unit/test_v12_8_fusions.py` (38 tests)

### Fixed

- Archaeology module graceful import for archived files
- Removed `timeout = 30` pytest config (requires uninstalled plugin)

---

## [12.7.0] — 2026-02-08

### Polyglot Hot Paths

- Wired 10 new cross-system fusions (15 → 23 active)
- Mojo 0.26 migration (13 categories of breaking changes)
- All 6 polyglot builds verified

---

## [12.6.0] — 2026-02-08

### PRAT Resonance & Capability Matrix

- PRAT resonance protocol — per-session state, predecessor/successor context
- Capability Matrix in Gnosis portal (3 new MCP tools)
- 3 cross-system fusions (Self-Model → Dream, Wu Xing → Gana, Resonance → Drive)
- Tool count: 175 → 178

---

## [12.5.0] — 2026-02-07

### Synthesis Gap Closure

- PRAT Router mapping all 175 tools to 28 Ganas
- Handler refactoring and Rust wiring
- Nexus API and Dream Cycle E2E

---

*For earlier history, see `SYSTEM_MAP.md` sections v12.4 and below.*
