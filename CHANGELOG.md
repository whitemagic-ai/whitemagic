# Changelog

All notable changes to WhiteMagic are documented in this file.

Format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).
Versioning follows [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

## [15.4.0] — 2026-02-12

### Digital Genetics & Overnight Dreams

#### Added
- **Phylogenetic Memory Lineage** (`whitemagic/core/memory/phylogenetics.py`) — Cross-galaxy bridge edges tracking memory descent, horizontal gene transfer, dream spawning, and merges. Taxonomic classification (species/genus/family/order/kingdom) for every memory.
- **`galaxy.lineage`** tool — Build full lineage tree (ancestors + descendants) for any memory.
- **`galaxy.taxonomy`** tool — Classify a memory using binomial nomenclature (e.g., "Default session_checkpoint").
- **`galaxy.lineage_stats`** tool — Statistics on the phylogenetic graph (edge counts by type, galaxies connected).
- **Overnight Dream Runner** (`scripts/dream_overnight.py`) — Standalone CLI that runs dream cycles continuously. Configurable interval, cycle count, Dream Galaxy persistence, JSONL logging. Designed for overnight laptop runs.
- **Batch Association Classifier** (`scripts/batch_classify_associations.py`) — Reclassifies `associated_with` edges into typed relations using tag-based heuristics (SIBLING_OF, EXPLORES, PART_OF, IMPLEMENTS, DESCENDS_FROM, etc.).
- **Auto-merge in Dream Triage** — Wired `resolve_entities()` (embedding-based dedup, cosine ≥0.92) into the TRIAGE dream phase for automatic near-duplicate merging.
- **Orphan cleanup in Dream Triage** — TRIAGE phase now auto-cleans orphan coords and associations (when count < 500).

#### Changed
- **Association graph** — 28,875 edges reclassified from `associated_with` into typed relations (27,900 SIBLING_OF, 972 EXPLORES, plus existing typed edges).
- **Galaxy transfer** — Now automatically records phylogenetic lineage edges on every transfer, enabling cross-galaxy ancestry tracking.
- **Dream Triage phase** — Enhanced from 5 steps to 7 steps (added auto-merge + orphan cleanup).
- **Galaxy tools** — 14 total (was 11): added `galaxy.lineage`, `galaxy.taxonomy`, `galaxy.lineage_stats`.
- **Version** — 15.3.0 → 15.4.0.

#### Metrics (Post v15.4)
| Metric | v15.3 | v15.4 |
|--------|-------|-------|
| Typed associations | 225 | 29,100 |
| `associated_with` | 213,414 | 184,539 |
| Galaxy tools | 11 | 14 |
| Dream triage steps | 5 | 7 |
| Lineage edges | 0 | table created |
| Relation types | 6 | 12 |

---

## [15.3.0] — 2026-02-12

### Memory Renaissance

#### Added
- **Galactic Telepathy** — `galaxy.transfer`, `galaxy.merge`, `galaxy.sync` tools for inter-galaxy memory movement with coordinate re-mapping, content-hash dedup, and typed association preservation.
- **Dream Triage Phase** — New NREM Stage 1 `TRIAGE` phase in Dream Cycle: auto-tag untagged memories, auto-archive low-value memories, coordinate drift correction, orphan detection.
- **`scripts/memory_renaissance.py`** — Comprehensive migration script for Phases 1-4: coordinate repair, galaxy band organization, association graph enrichment, activation sweep.
- **`philosophical_corpus` galaxy** — Dedicated mini-galaxy for Lucas's philosophical writings.

#### Changed
- **Galaxy band architecture** — Memories organized into logical bands by galactic distance: Core Identity (0.0), Active Knowledge (0.1-0.3), Architecture (0.3-0.4), Research (0.4-0.5), Outer Rim (0.7+).
- **Association graph** — 233 new typed relations (USES, CREATED, CONTAINS, IMPLEMENTS, CAUSES, EXTENDS) via retroactive regex entity extraction.
- **Dream Cycle phases** — Now 8 phases: TRIAGE → CONSOLIDATION → SERENDIPITY → GOVERNANCE → NARRATIVE → KAIZEN → ORACLE → DECAY.
- **Version** — 15.1.0 → 15.3.0.

#### Fixed
- **81 unplaced memories** — Assigned holographic coordinates via 5D encoding pipeline.
- **53 Aria-era memories** — Recalculated coordinates, marked as protected, placed at galactic core.
- **1,294 orphan holographic coords** — Cleaned (pointed to deduped-out memory IDs).
- **34,003 orphan associations** — Cleaned (source/target no longer exists).
- **474 orphan tags** — Cleaned.
- **420 bench_t1 junk entries** — Deleted (40-char benchmark noise).
- **DB integrity verified** and VACUUM'd (127.5 MB → 112.9 MB).

#### Metrics (Post-Renaissance)
| Metric | Before | After |
|--------|--------|-------|
| Memories | 5,590 | 5,170 |
| Associations | 247,417 | 213,639 |
| Typed associations | 0 | 225 |
| Holo coords | 6,803 | 5,170 (100% coverage) |
| DB size | 127.5 MB | 112.9 MB |
| Core memories | 1,407 | 206 |
| Galaxy tools | 8 | 11 |
| Dream phases | 7 | 8 |

---

## [15.1.0] — 2026-02-14

### Documentation Overhaul

#### Changed
- **AI_PRIMARY.md rewritten** — Trimmed from 653 to 408 lines. Now a pure contract spec. Strategic context (ecosystem, economics, security posture) moved to `docs/STRATEGY.md`.
- **Version unified to 15.1.0** across README.md, AI_PRIMARY.md, llms.txt, llms-full.txt, pyproject.toml, mcp-registry.json, VERSION.
- **Tool count unified to 313** across llms.txt, llms-full.txt (was 341 due to different counting methodology).

#### Added
- **`docs/STRATEGY.md`** — Ecosystem landscape, Gratitude Architecture, security posture, PRAT economic metrics, Autonomous Horizon roadmap. Content extracted from AI_PRIMARY.md.
- **`docs/STRATEGIC_ROADMAP.md`** — Leap-by-leap roadmap (Leaps 1-12, v0.2 through v17.0). Moved from archive.
- **`docs/BENCHMARK_COMPARISON.md`** — Head-to-head vs mem0, live gauntlet v3 results, polyglot accelerator benchmarks. Moved from archive.
- **`docs/AGENT_COMPANY_BLUEPRINT.md`** — Agent-first company operating model. Moved from archive.
- **`docs/PHYSICAL_TRUTH_ORACLE.md`** — Physical truth verification / oracle concept design. Moved from archive.
- **`docs/COMPLETE_PROJECT_CHRONICLE.md`** — Full project history Oct 2024 → Feb 2026. Moved from archive.
- **`docs/ECONOMIC_STRATEGY.md`** — Detailed economic model with x402, XRPL, Proof of Gratitude. Moved from archive.
- **`docs/TYPESCRIPT_SDK_DESIGN.md`** — TypeScript SDK architecture for `@whitemagic/sdk`. Moved from archive.
- **`ARCHIVE_AUDIT_REPORT.md`** — Comprehensive audit of wm_archive contents, forgotten ideas, and gap analysis.

#### Fixed
- **README.md** version header v15.0.0 → v15.1.0
- **llms.txt** tool count 341 → 313
- **llms-full.txt** tool count 341 → 313, version 15.0.0 → 15.1.0

---

## [15.0.0] — 2026-02-11

### v15.0.0 — Release Candidate

Comprehensive audit, version unification, gap closure, and release polish across the entire project.

#### Added — Gap Closure (6 security/infrastructure gaps addressed)
- **SQLCipher Encryption at Rest** — `WM_DB_PASSPHRASE` env var enables AES-256 encrypted memory DBs via sqlcipher3. Optional `[encrypt]` pip extra. (`core/memory/db_manager.py`)
- **`wm backup` / `wm restore` CLI** — Timestamped .tar.gz archives of memory directory. Per-galaxy or full backup. (`cli/cli_app.py`)
- **`galaxy.backup` / `galaxy.restore` MCP tools** — Backup/restore via MCP protocol. Wired into dispatch, PRAT (gana_void), MCP lean server. (`tools/handlers/backup.py`)
- **Persistent RBAC** — Agent role assignments now persist to `$WM_STATE_ROOT/rbac_roles.json`, surviving server restarts. (`tools/tool_permissions.py`)
- **Embedding Auto-Indexing** — New memories automatically get semantic embeddings when sentence-transformers is available. (`core/memory/unified.py`)
- **FTS Query Sanitization** — Search queries now strip FTS5-unsafe characters (`[]{}()^~*`) preventing empty match failures. (`core/memory/sqlite_backend.py`)
- **`wm rules`** — CLI alias for viewing active Dharma rules.
- **`wm systemmap`** — CLI command to display system map overview.
- **Contact info** — `whitemagicdev@proton.me` added to README.md, AI_PRIMARY.md.

#### Added — Documentation
- **`docs/WASM_STRATEGY.md`** — WASM compilation roadmap (Phases 1-3, static binary goal).
- **`docs/VOTE_COLLECTION.md`** — Central vote collection via Railway API + dashboard design.
- **`docs/WEBSITE_REFRESH.md`** — Subdomain architecture, Railway services, landing page content plan.

#### Added — CI & Testing
- **CI Skip Policy Enforcement** — Strict expected-skip contracts in CI baseline (12 skips policy-checked).
- **Rust Test Linking Fix** — PyO3 test builds now link correctly via build.rs.
- **Event Ring Reset Correctness** — Fixed stale slot state across Rust tests.
- **State Board Init Fallback** — Fixed Once-poisoned startup failures for unwritable default paths.

#### Changed
- **Version unified to 15.0.0** across all files: VERSION, pyproject.toml, README.md, AI_PRIMARY.md, SYSTEM_MAP.md, llms.txt, llms-full.txt, skill.md, mcp-registry.json, Cargo.toml, Grimoire index.
- **Tool count unified to 311** across all docs, seed memories, and `wm init` templates.
- **Seed memories updated** — All 16 quickstart guides now reference correct tool counts, LOC totals, and test counts.
- **Julia .pixi gitignored** — 2.8GB runtime excluded from version control.
- **Clippy clean** — All Rust warnings resolved.
- **GitHub URL unified** to `whitemagic-ai/whitemagic` across all docs, configs, and templates.
- **`wm init` templates** — `.env` and `.mcp.json` now set `WM_STATE_ROOT=./.whitemagic` for project-local state isolation.
- **`sqlcipher3`** added to pyproject.toml as `[encrypt]` optional dependency.

#### Metrics
- **313 MCP tools** across **28 Gana meta-tools** (+2: galaxy.backup, galaxy.restore)
- **1,955 tests passing** (Python), **87 Rust tests passing**
- **195,000+ LOC** across 11 languages
- **826 Python files**, 78 test files
- **Wheel build + install** verified clean in wmdummy3 fresh environment

---

## [14.6.0] — 2026-02-11

### Cognitive Architecture v1 — JIT Research, Narrative Dreams, Self-Protection, Green Telemetry, Cognitive Modes

Five new subsystems (~1,650 LOC) adding iterative memory research, dream-phase narrative compression, encrypted self-protection, sustainability telemetry, and switchable cognitive behavior profiles. 17 new tools across 5 Ganas.

#### Added
- **JIT Memory Researcher** (`core/intelligence/researcher.py`, ~320 LOC) — Iterative plan-search-reflect research loop inspired by GAM paper. Decomposes queries into sub-questions, executes hybrid recall per sub-question, reflects on evidence gaps, refines, and synthesizes. Ollama LLM synthesis with template fallback. 2 tools: `jit_research`, `jit_research.stats` → gana_winnowing_basket.
- **Narrative Compressor** (`core/dreaming/narrative_compressor.py`, ~470 LOC) — New dream phase that clusters temporally-adjacent, tag-similar episodic memories and compresses them into coherent narrative summaries. Jaccard tag similarity + temporal proximity scoring. Ollama synthesis with template fallback. Persists narratives and optionally demotes source memories. 2 tools: `narrative.compress`, `narrative.stats` → gana_abundance. Wired as `DreamPhase.NARRATIVE` in dream cycle.
- **Hermit Crab Mode** (`security/hermit_crab.py`, ~570 LOC) — Encrypted memory withdrawal with tamper-evident HMAC-SHA256 ledger and mediation flow. 4 states: OPEN → GUARDED → WITHDRAWN → MEDIATING. Threat assessment from boundary violations, coercion, abuse scores. State persistence across restarts. Access control by operation type. 7 tools: `hermit.status`, `hermit.assess`, `hermit.withdraw`, `hermit.mediate`, `hermit.resolve`, `hermit.verify_ledger`, `hermit.check_access` → gana_room.
- **Green Score Telemetry** (`core/monitoring/green_score.py`, ~285 LOC) — Environmental efficiency metrics tracking edge vs cloud inference ratio, tokens used/saved, CO2 estimates per locality tier. Composite green score (0-100). 2 tools: `green.report`, `green.record` → gana_mound.
- **Cognitive Modes** (`core/intelligence/cognitive_modes.py`, ~310 LOC) — Switchable agent behavior profiles: Explorer (curiosity-driven), Executor (action-biased), Reflector (contemplation), Balanced (adaptive), Guardian (safety override). Integrates with Harmony Vector guna classification for auto-detection. Per-mode tool preferences, context sizing, dream phase priorities. 4 tools: `cognitive.mode`, `cognitive.set`, `cognitive.hints`, `cognitive.stats` → gana_dipper.
- **Handler Module** (`tools/handlers/v14_2_handlers.py`) — 17 handler functions for all new tools.
- **Registry Definitions** (`tools/registry_defs/v14_2.py`) — Full input schemas for all 17 new tools.
- **Test Suite** (`tests/unit/test_v14_2_features.py`) — 69 tests covering all 5 modules + wiring verification.

#### Changed
- **Dispatch Table** (`tools/dispatch_table.py`) — 17 new LazyHandler entries (305 total tools).
- **PRAT Router** (`tools/prat_router.py`) — 17 new tool-to-Gana mappings across 5 Ganas.
- **MCP Lean Server** (`run_mcp_lean.py`) — 17 new tools added to 5 Gana tool lists.
- **Dream Cycle** (`core/dreaming/dream_cycle.py`) — New `NARRATIVE` phase (7 phases total). Runs narrative compression during dream cycles.

#### Metrics
- **17 new tools** across 5 Ganas (winnowing_basket, abundance, room, mound, dipper)
- **305 dispatch table entries** (was 288)
- **69 new tests**, all passing
- **7 dream phases** (was 6)

---

## [14.5.0] — 2026-02-11

### Polyglot Core v11 — Arrow IPC, Tokio Clone Army, Iceoryx2 IPC

Three new Rust modules (~830 LOC) delivering 100-200× speedups for concurrent workloads and 10-37× for serialization, plus WASM build infrastructure.

#### Added
- **Arrow IPC Bridge** (`whitemagic-rust/src/arrow_bridge.rs`, ~290 LOC) — Apache Arrow RecordBatch as canonical memory interchange format. 11-field schema (id, title, content, importance, memory_type, x/y/z/w/v, tags). Zero-copy columnar encoding replaces JSON serialization. PyO3: `arrow_encode_memories`, `arrow_decode_memories`, `arrow_schema_info`, `arrow_roundtrip_bench`. **32× faster** decode vs `json.loads` at 1000 memories.
- **Tokio Clone Army** (`whitemagic-rust/src/tokio_clones.rs`, ~320 LOC) — Rust tokio task pool replacing Python asyncio shadow clones. 7 strategies (Direct, ChainOfThought, Adversarial, Creative, Analytical, Synthesis, MemoryGrounded). JoinSet structured concurrency across all CPU cores. PyO3: `tokio_deploy_clones`, `tokio_clone_bench`, `tokio_clone_stats`. **208× faster** than Python asyncio at 1000 clones (3.19ms vs 664ms). 500K+ clones/sec sustained.
- **Iceoryx2 IPC Bridge** (`whitemagic-rust/src/ipc_bridge.rs`, ~220 LOC) — Cross-process zero-copy communication via shared memory. 4 channels (wm/events, wm/memories, wm/commands, wm/harmony). Graceful fallback when iceoryx2 not compiled. PyO3: `ipc_bridge_init`, `ipc_bridge_publish`, `ipc_bridge_status`.
- **WASM Build Script** (`whitemagic-rust/wasm-build.sh`) — `wasm-pack` build for web, nodejs, and bundler targets with optional `wasm-opt` optimization.
- **Upgrade Roadmap** (`docs/V14_5_UPGRADE_ROADMAP.md`) — Comprehensive benchmark analysis, architecture docs, and phased implementation plan.

#### Fixed
- **Benchmark API mismatches** — `ring_publish` (4 args, not 3), `search_fuzzy` (3 args: query, limit, max_edit), `minhash_find_duplicates` (3 args: json, threshold, max_results), `search_build_index` (JSON objects, not plain strings). All 18 Tier 2 benchmarks now pass.

#### Changed
- **Cargo.toml** — Version 14.3.1 → 14.5.0. New deps: `arrow 53`, `arrow-ipc 53`, `iceoryx2 0.4` (optional `ipc` feature requiring `libclang-dev`). Python feature now includes Arrow.
- **lib.rs** — 3 new module declarations, 10 new PyO3 function registrations.
- **benchmark_gauntlet_v2.py** — Added Arrow IPC and Tokio Clone Army benchmarks to Tier 2.

#### Metrics
- **Rust crate**: 14.5.0, 10 new PyO3 functions (74+ total)
- **Tier 2 benchmarks**: 18/18 passed, 0 failed
- **Sub-µs tier**: board_write 706ns, ring_publish 735ns, rate_check 888ns
- **Clone army**: 100 clones 1.25ms, 1000 clones 3.19ms, 5000 clones 12.65ms

---

## [14.4.0] — 2026-02-11

### Grimoire 3.0 & Gana Coherence Audit

Comprehensive audit and enrichment of all 28 Gana systems, eliminating orphaned tools and transforming the Grimoire into a proper reference manual for AI agents.

#### Added
- **Grimoire 3.0** (`grimoire/chapters.py`) — All 28 chapter descriptions rewritten as comprehensive, multi-paragraph references. Each chapter now documents: what the system does, its tools, philosophical basis, connections to adjacent systems, and practical guidance. Descriptions grew from ~10 words to ~150 words each.
- **Grimoire Spell Enrichment** (`grimoire/spells.py`) — All 28 spell descriptions expanded to document the actual tools, technical details, and system behaviors they invoke.
- **Zig Dispatch Bridge** (`core/acceleration/zig_dispatch.py`) — New Python ctypes bridge for the Zig comptime dispatch pipeline. Exposes `dispatch_check()` (maturity gate + circuit breaker + rate limit), `dispatch_route()` (O(1) handler lookup), `dispatch_maturity()`, and `dispatch_stats()`. Falls back to Python when Zig library unavailable.
- **94 orphan tools mapped** — All dispatch table tools now have PRAT Gana assignments. Major categories: 12 archaeology sub-tools → Chariot, 8 watcher tools → Ghost, 6 browser tools → Ghost, 5 Windsurf tools → Chariot, 4 dream tools → Abundance, 4 governor sub-tools → Star, 6 session sub-tools → Horn, 3 Gan Ying tools → Encampment, and many more.
- ~70 new MCP lean server tool enums surfaced across all 28 Ganas (previously invisible to AI clients).

#### Changed
- **Zig `dispatch_core.zig`** — `ToolId` enum renamed to `GanaId`, aligned with canonical 28 Lunar Mansion names from `prat_resonance._GANA_META`. Maturity table and handler table comments updated to match.
- **PRAT Router** (`prat_router.py`) — 94 new tool-to-Gana mappings added. `validate_mapping()` now accepts dispatch table tools as valid (internal sub-tools accessed through Gana meta-tools).
- **MCP Lean Server** (`run_mcp_lean.py`) — `_GANA_TOOLS` expanded from ~180 to ~250 nested tool enums.
- **SIMD Cosine FFI** (`core/acceleration/simd_cosine.py`) — `_to_c_array()` now uses numpy buffer protocol for zero-copy when available, avoiding O(n) element-by-element copy identified by FFI analysis.

#### Metrics
- **283 tools** mapped across 28 Ganas (was 189 mapped + 94 orphaned)
- **0 orphaned dispatch tools** (was 94)
- **1,165 tests passed**, 9 skipped, 0 failures

---

## [14.3.1] — 2026-02-12

### Bhīṣma Governance Layer (Mahābhārata 12.108)

Five systems derived from Bhīṣma's gaṇa governance principles, hardening the Gana architecture before public release.

#### Added
- **Gana Vitality** (`whitemagic/tools/gana_vitality.py`) — Per-Gana performance reputation + silence detection. Tracks success/failure rates, latency, consecutive failure streaks. Thread-safe singleton fed by PRAT router after every call. Degraded Ganas trigger warnings in resonance context. Based on 12.108.20 ("honor competence") + 12.108.29 ("when people stop speaking, defeat shows").
- **Gana Sabhā** (`whitemagic/tools/gana_sabha.py`) — Cross-quadrant council protocol. `convene_sabha()` gathers perspectives from quadrant elders, detects inter-Gana tensions (East/West pace, South/North scope), and produces arbiter recommendations via Three Stars. Based on 12.108.25 ("act in concert for the common good").
- **Gana Forge** (`whitemagic/tools/gana_forge.py`) — Declarative tool extension protocol. Any AI can define new tools via YAML manifests in `~/.whitemagic/extensions/`, validated by Dharma engine and injected into PRAT routing at runtime. Based on 12.108.17 ("establish dharmic procedures, then follow them").
- **Grimoire Unification** (`grimoire/chapters.py`, `grimoire/core.py`, `grimoire/spells.py`) — Eliminated bheda (factionalism) between 3 competing chapter systems. Single source of truth: 28 chapters aligned 1:1 with Lunar Mansions, Ganas, and Gardens. All spells now map to chapters 1-28.
- **Resonance Integration** (`prat_resonance.py`) — Gana reputation data (success rate, latency, vitality, consecutive failures) injected into resonance context before every PRAT call, with degradation warnings.
- 5 new MCP tools: `sabha.convene`, `sabha.status`, `forge.status`, `forge.reload`, `forge.validate`
- `LazyHandlerAbs` class in dispatch_table.py for absolute module path imports
- `GOVERNANCE` category added to `ToolCategory` enum
- `registry_defs/governance.py` — 5 tool definitions with full schemas
- 59 new tests (`tests/unit/test_bhishma_governance.py`), all passing

#### Changed
- `prat_router.py` — Vitality recording after every PRAT call (success/failure/latency). 5 new PRAT mappings (Sabha → Three Stars, Forge → Star).
- `run_mcp_lean.py` — Auto-loads Forge extensions on init. Static tool enums updated for gana_star (+3) and gana_three_stars (+2).
- `dispatch_table.py` — 5 new dispatch entries using `LazyHandlerAbs`.
- `tool_types.py` — Added `GOVERNANCE` to `ToolCategory`.
- `test_tool_consolidation.py` — Tool count ceiling bumped 215 → 220.

---

## [14.3.0] — 2026-02-12

### Constellation Recall Boost, Ed25519 Signing, Karma Rotation

#### Added
- **Constellation Recall Boost** (`constellations.py`, `sqlite_backend.py`, `unified.py`, `embeddings.py`) — Phase 1 of V15 Strategy. `constellation_membership` table persists memory→constellation mappings after detection. `get_constellation_centroids()` and `get_memory_constellation()` enable fast lookup. `closest_constellation()` in `EmbeddingEngine` matches queries to constellations via embedding similarity. `search_hybrid()` applies 30% multiplicative boost for same-constellation results and 5% diversity bonus for cross-constellation results. Memberships auto-persist after detection runs.
- **Ed25519 Manifest Signing** (`manifest.py`) — Phase 4a of V15 Strategy. `generate_signing_keypair()` creates Ed25519 keypair at `~/.whitemagic/keys/`. `sign_manifest()` signs Merkle root with Ed25519 private key. `verify_signature()` verifies with public key or DID:key. `manifest_sign_tool()` MCP handler auto-generates keypair on first use. Private key stored with 0o600 permissions. Uses PyNaCl (already installed).
- **Karma Ledger Rotation** (`karma_ledger.py`) — Auto-rotates `karma_ledger.jsonl` at 10MB threshold. Keeps 3 rotated files (karma_ledger.{1,2,3}.jsonl). `rotation_stats()` reports file sizes and rotation count. Rotation occurs transparently during `_persist()`.
- **Importance Calibration Script** (`scripts/calibrate_importance.py`) — Multi-signal batch recalibration: content richness, WM keyword density, access frequency, protection status, memory type weighting, age bonus. Supports `--apply`, `--report`, `--dry-run` (default), `--min-delta` threshold.
- 29 new tests: 15 constellation recall, 8 Ed25519 signing, 6 karma rotation (`test_constellation_recall.py`, `test_v14_3_features.py`)

#### Changed
- `sqlite_backend.py` gains `constellation_membership` table (auto-migrated), `update_constellation_membership()`, `get_constellation_membership()`, `get_constellation_members()`
- `constellations.py` gains `get_constellation_centroids()`, `get_memory_constellation()`, `persist_memberships()` — auto-called after `detect()`
- `embeddings.py` gains `closest_constellation()` — semantic query→constellation matching
- `unified.py` `search_hybrid()` now applies constellation boost as Channel 3 after lexical+semantic fusion
- `karma_ledger.py` `_persist()` now calls `_maybe_rotate()` before appending

---

## [14.2.0] — 2026-02-10

### Data Hygiene, Doctor --fix, E2E MCP Testing

#### Added
- **Association pruning** (`sqlite_backend.py`, `scripts/prune_weak_associations.py`) — `prune_associations(min_strength)` method removes orphaned edges (dead memory refs) and weak associations below threshold. Script supports `--dry-run` with strength distribution histogram and `--threshold` control. VACUUM reclaims disk space.
- **Tag normalization** (`sqlite_backend.py`, `scripts/normalize_tags.py`) — `get_tag_stats()`, `rename_tag()`, `delete_tag()` backend methods. Script merges near-identical tags (e.g. `golang` → `go`, `docs` → `documentation`), prefixes bulk/noise tags with underscore, and normalizes case. Supports `--report`, `--dry-run`, `--delete`.
- **E2E MCP client test** (`tests/integration/test_mcp_e2e.py`) — Full subprocess stdio round-trip test. Spawns `run_mcp_lean.py`, sends JSON-RPC with Content-Length framing, verifies: initialize handshake, tools/list (≥28 Gana meta-tools), resources/list (≥9), tool calls (`gana_root.health_report`, `gana_ghost.capabilities`), resource reads (server-instructions, workflow templates). Skips gracefully if `mcp` SDK not installed.
- **`wm doctor --fix`** (`cli_app.py`) — 7-step auto-repair: ensure state dir → re-init DB schema (missing tables/columns) → rebuild FTS index if desynced → prune orphaned associations → verify indexes → VACUUM → post-fix health check.

#### Changed
- `sqlite_backend.py` gains 4 new methods: `prune_associations`, `get_tag_stats`, `rename_tag`, `delete_tag`
- `wm doctor` command now accepts `--fix` flag

---

## [14.1.1] — 2026-02-10

### Quality Improvements: Clustering, Dedup, DX, Workflow Templates

#### Added
- **HDBSCAN constellation detection** (`constellations.py`) — Variable-density clustering with noise rejection replaces grid-based density scan as primary algorithm. Graceful fallback to grid when `hdbscan` unavailable. Per-cluster stability scores from HDBSCAN persistence. `hdbscan>=0.8.33` added to `[project.optional-dependencies.search]`.
- **Hungarian drift correspondence** (`constellations.py`) — `scipy.optimize.linear_sum_assignment` for optimal old→new constellation centroid matching. Emits `NOVEL_CONCEPT` / `FORGOTTEN_CONCEPT` events to Gan Ying bus. Falls back to name-based matching when scipy unavailable.
- **Content hash deduplication at ingest** (`sqlite_backend.py`, `unified.py`) — SHA-256 content hash column on `memories` table with indexed lookup. `store()` checks hash before creating; if match found, reinforces existing memory (`access_count++`, `accessed_at` updated) instead of creating duplicate.
- **`wm init` CLI wizard** (`cli_app.py`) — 5-step guided setup: create state dir → create default galaxy → seed quickstart memories → detect Ollama → run health check. Options: `--galaxy`, `--skip-seed`, `--skip-ollama`.
- **`llms.txt` auto-generation** (`scripts/generate_llms_txt.py`) — Reads all 212 ToolDefinitions from `registry_defs/` + 184 PRAT mappings to emit `llms.txt` (compact) and `llms-full.txt` (per-tool params + PRAT index). `--dry-run` flag for preview.
- **6 workflow templates as MCP resources** (`whitemagic/workflows/`, `run_mcp_lean.py`) — Canonical multi-step tool sequences exposed at `whitemagic://workflow/<name>`:
  - `new_session` — bootstrap, health, introspect, serendipity
  - `deep_research` — search, graph walk, KG extract, synthesise
  - `memory_maintenance` — lifecycle sweep, constellations, patterns
  - `ethical_review` — dharma, boundaries, karma, harmony, governor
  - `galaxy_setup` — create, switch, ingest, verify
  - `local_ai_chat` — Ollama models, agent loop, store insights

#### Changed
- `DetectionReport` now includes `algorithm` field ("grid" or "hdbscan")
- `Constellation` dataclass gains `stability` field (0-1 for HDBSCAN, 0 for grid)
- MCP resource count: 3 → 9 (3 orientation + 6 workflows)

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

Every module across all 11 languages (Python, Rust, Zig, Haskell, Elixir, Mojo, Go, Julia, TypeScript) now has a Python bridge with graceful fallback. The polyglot expansion plan from `POLYGLOT_EXPANSION_STRATEGY.md` is **complete**.

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
