# Changelog

> **Canonical location: [`/CHANGELOG.md`](../CHANGELOG.md)** — this file is kept for historical reference only.
> The root CHANGELOG.md has the complete history through v13.6.0+.

---

*Below is a historical snapshot from v12.3.0. See the root CHANGELOG.md for current entries.*

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [12.3.0] - 2026-02-07
### Added — Synthesis Gaps, Missing Subsystems & Structural Debt
- **Middleware Pipeline** (`whitemagic/tools/middleware.py`): Refactored monolithic `dispatch()` into a composable 7-stage middleware chain: input_sanitizer → circuit_breaker → rate_limiter → tool_permissions → maturity_gate → governor → core_router. Each stage is a discrete function that can be added/removed/reordered. `dispatch_table.py` preserved for backward compatibility.
- **Registry Domain Split** (`whitemagic/tools/tool_types.py`, `whitemagic/tools/registry_defs/`): Base classes (`ToolCategory`, `ToolDefinition`, `ToolSafety`) extracted to `tool_types.py`. Domain definition files in `registry_defs/` are auto-discovered via `pkgutil.iter_modules`. GANA tools (28 Lunar Mansions) migrated to `registry_defs/gana.py` as proof-of-concept. Pattern established for future domain migrations.
- **Explain This** (`whitemagic/tools/explain_this.py`): Pre-execution impact preview system. Before running any tool, call `explain_this` to see: Dharma evaluation, resource estimate, dependency chain, risk assessment, karma forecast, maturity gate status, and circuit breaker state. Returns verdict: SAFE_TO_PROCEED, PROCEED_WITH_CAUTION, or BLOCKED. New MCP tool: `explain_this`.
- **Agent Trust Scores** (`whitemagic/tools/agent_trust.py`): Per-agent reputation derived from Karma Ledger data. Computes reliability, mismatch_rate, debt_contribution, call_volume, and composite_trust score. Trust tiers: EXEMPLARY → TRUSTED → STANDARD → PROBATIONARY → RESTRICTED. New MCP tool: `agent.trust`.
- **Cross-Node Mesh Awareness** (`whitemagic/mesh/awareness.py`): Python-side peer tracking from Go libp2p mesh node. Listens on Redis `ganying` channel for PEER_DISCOVERED/PEER_LEFT/HOLOGRAM_RECEIVED events. Maintains live peer registry. Wired into `redis_bridge.py` inbound path. New MCP tools: `mesh.status`, `mesh.broadcast`.
- **Salience Arbiter ↔ Homeostatic Loop Coupling**: Homeostatic Loop now queries the Salience Arbiter spotlight for high-urgency events, boosting correction thresholds when urgency > 0.7. Corrective actions (CORRECT/INTERVENE) are emitted back to the arbiter as events, creating a bidirectional awareness loop.
- **Dependency Graph ↔ Pipeline Engine Validation**: `pipeline.create` now validates step ordering against the Tool Dependency Graph before execution. Warns about missing hard prerequisites, out-of-order dependencies, and suggests strong follow-up tools (affinity ≥ 70%).
- **Consolidation ↔ Bicameral Reasoner Creative Clustering**: Memory consolidation Step 3.5 uses bicameral dual-hemisphere heuristics. Left hemisphere finds logical cross-cluster links (shared tags). Right hemisphere detects creative bridges (emotional affinity, importance contrast). Results stored as strategy memories.
- **Galactic Map → Harmony Vector Energy**: Energy dimension now blends runtime pressure (60%, slow-call ratio) with galactic memory vitality (40%, zone-weighted distribution). Added `GalacticMap.get_zone_counts()` for lightweight DB queries. Zone weights: CORE=1.0, INNER_RIM=0.8, MID_BAND=0.5, OUTER_RIM=0.2, FAR_EDGE=0.05.
- **Test Suite**: 43 new tests in `tests/unit/test_synthesis_gaps.py` covering all changes.

### New MCP Tools (6)
`explain_this`, `agent.trust`, `mesh.status`, `mesh.broadcast`

### Files Created (7)
- `whitemagic/tools/middleware.py`
- `whitemagic/tools/tool_types.py`
- `whitemagic/tools/registry_defs/__init__.py`
- `whitemagic/tools/registry_defs/gana.py`
- `whitemagic/tools/explain_this.py`
- `whitemagic/tools/agent_trust.py`
- `whitemagic/mesh/awareness.py`

### Files Modified (9)
- `whitemagic/tools/dispatch_table.py` — middleware pipeline + 4 new LazyHandler entries
- `whitemagic/tools/registry.py` — base class re-export, GANA section replaced with auto-collection comment, 6 new ToolDefinitions
- `whitemagic/tools/unified_api.py` — 1 new alias
- `whitemagic/tools/handlers/introspection.py` — 5 new handlers
- `whitemagic/tools/handlers/pipeline.py` — dependency graph validation
- `whitemagic/harmony/homeostatic_loop.py` — salience arbiter coupling
- `whitemagic/harmony/vector.py` — galactic vitality in energy dimension
- `whitemagic/core/memory/consolidation.py` — bicameral creative clustering
- `whitemagic/core/memory/galactic_map.py` — get_zone_counts()
- `whitemagic/core/resonance/redis_bridge.py` — mesh awareness forwarding

## [11.3.2] - 2026-02-07
### Added — Galactic Map, 5D Holographic Coordinates & Memory Lifecycle
- **Galactic Map** (`whitemagic/core/memory/galactic_map.py`): Memory lifecycle engine using a galactic rotation metaphor. Every memory lives at a computed distance from the galactic core (0.0=active spotlight) to the far edge (1.0=deep archive). Five zones: CORE, INNER_RIM, MID_BAND, OUTER_RIM, FAR_EDGE. Full sweep executed on 107,168 memories.
- **No-Delete Policy**: `sqlite_backend.delete()` now redirects to `archive_to_edge()` — no memory is ever truly destroyed.
- **V Dimension (Vitality)**: Holographic coordinate system extended from 4D (XYZW) to 5D (XYZWV). V-axis represents galactic distance/vitality: 1.0=core, 0.0=edge. Encoder, dataclass, DB schema, Python wrapper, and all hydration paths updated.
- **Galactic-Distance-Aware Search**: FTS queries now weighted by `ABS(rank) * (0.5 + galactic_distance)` so CORE memories surface before FAR_EDGE ones. Non-FTS queries also ordered by galactic distance.
- **Memory Re-Promotion**: `recall()` spirals memories inward by 5% per access — frequently-recalled memories naturally migrate toward the galactic core.
- **Automatic Galactic Rotation**: `lifecycle.py` now runs a Phase 2 galactic rotation after each retention sweep, updating distances for all memories via `galactic_map.full_sweep()`.
- **Cross-Memory Association Miner** (`whitemagic/core/memory/association_miner.py`): Discovers hidden semantic links between memories using keyword fingerprinting, Jaccard overlap scoring, and zone-diverse sampling. Creates bidirectional associations with strength proportional to overlap.
- **Galactic Sweep Scripts**: `scripts/galactic_sweep.py` (full system init) and `scripts/galactic_sweep_direct.py` (lightweight direct-DB) for batch-tagging all memories.
- **Schema Extensions**: Three new columns on `memories` table (`galactic_distance`, `retention_score`, `last_retention_sweep`), `v` column on `holographic_coords`. All auto-migrated.
- **Test Suite**: 43 new tests across `test_galactic_map.py` (21), `test_galactic_improvements.py` (22). Updated `test_holographic_encoding.py` for 5D.

### Changed
- **`unified.py`**: `prune()` uses galactic rotation; `recall()` promotes memories inward; `store()` persists galactic fields; holographic index loads 5D coords.
- **`mindful_forgetting.py`**: `sweep()` supports `persist=True`; `archive` action rotates to edge.
- **`lifecycle.py`**: `run_sweep()` now includes Phase 2 galactic rotation with zone distribution reporting.
- **`sqlite_backend.py`**: `store()` persists `galactic_distance`, `retention_score`, `last_retention_sweep`. `store_coords()`/`get_coords()`/`get_all_coords()` extended to 5D. Search weighted by galactic distance.
- **`encoder.py`**: `HolographicCoordinate` extended with `v` field. New `_calculate_v()` method. `_blend_with_garden()` handles 5D.
- **`holographic.py`**: `index_memory()` returns 5-tuple. `add_memory_with_coords()` accepts 5D.
- **`unified_types.py` Memory**: Extended with `galactic_distance`, `retention_score`, `last_retention_sweep` fields + serialization.

## [11.3.0] - 2026-02-07
### Added — Deep Integration & Self-Regulation
- **Memory Lifecycle Manager** (`whitemagic/core/memory/lifecycle.py`): Bridges mindful forgetting with the temporal scheduler's SLOW lane. Every N slow-lane flushes triggers a retention sweep that evaluates all memories against 5 independent signals, applies decay/archive, and feeds results into the Harmony Vector's energy dimension. New MCP tools: `memory.lifecycle_sweep`, `memory.lifecycle_stats`.
- **Homeostatic Loop** (`whitemagic/harmony/homeostatic_loop.py`): Closes the feedback loop on the Harmony Vector. Periodically samples all 7 health dimensions and applies graduated corrective actions (OBSERVE→ADVISE→CORRECT→INTERVENE): triggers memory sweeps on low energy, tightens Dharma to 'secure' on low dharma score, emits warning events on high error rates, and fires SYSTEM_HEALTH_CHANGED on critical composite scores. New MCP tools: `homeostasis.status`, `homeostasis.check`.
- **Maturity Gate Integration** (`whitemagic/tools/maturity_check.py`): Maps tool names and categories to minimum maturity stages. Wired into `dispatch()` as step 0.5 between circuit breaker and governor. Dangerous tools (mesh, multi-agent coordination) require higher maturity stages; the system gracefully rejects calls that exceed its current developmental level. New MCP tool: `maturity.assess`.
- **Memory Consolidation** (`whitemagic/core/memory/consolidation.py`): Hippocampal replay engine for cross-session memory strengthening. Clusters recent memories by tag similarity, synthesizes compressed "strategy memories" (promoted to LONG_TERM), promotes frequently-accessed short-term memories, and emits MEMORY_CONSOLIDATED + INSIGHT_CRYSTALLIZED events. New MCP tools: `memory.consolidate`, `memory.consolidation_stats`.
- **Dharma YAML Rules Directory** (`whitemagic/dharma/rules.py` upgraded): Users can now drop `.yaml`/`.yml` rule files into `$WM_STATE_ROOT/dharma/rules.d/`. Files are merged with built-in defaults (last-write-wins by rule name). Supports hot-reload via `check_reload()` (detects mtime changes). New MCP tool: `dharma.reload`.
- **Tool Dependency Graph** (`whitemagic/tools/dependency_graph.py`): Static affinity map of tool relationships (requires/suggests/provides edges). AI callers can query `next_steps(tool)`, `prerequisites(tool)`, and `plan(goal)` to plan multi-step operations. Supports learned edges from pipeline history. New MCP tools: `tool.graph`, `tool.graph_full`.
- **Test Suite**: 44 regression tests (`tests/unit/test_v11_3_modules.py`).

### Changed
- **`whitemagic/tools/dispatch_table.py`**: Dispatch pipeline now: Circuit Breaker → Maturity Gate → Governor → Gana Routing → Handler → Bridge → Breaker Feedback. Registered 7 new tool handlers.
- **`whitemagic/tools/gnosis.py`**: Fixed datetime serialization bug — `_json_safe()` now applied to entire snapshot output.
- **`whitemagic/tools/unified_api.py`**: Added 7 canonical name aliases for new dot-style tools.
- **`whitemagic/tools/registry.py`**: Added 10 new tool definitions.
- **`whitemagic/tools/handlers/introspection.py`**: Added 10 new handler functions.
- **`whitemagic/dharma/rules.py`**: `_load_rules()` now scans `rules_dir/*.yaml` + single `rules_path`, merges with built-in defaults, de-duplicates by name. Added `check_reload()` for mtime-based hot-reload.

## [11.2.0] - 2026-02-07
### Added — MandalaOS Synergy
- **Harmony Vector** (`whitemagic/harmony/vector.py`): Multi-dimensional real-time health metric inspired by MandalaOS's Lakshmi Subsystem and Tiferet Engine. Tracks 7 normalized [0-1] dimensions: balance (yin/yang ratio), throughput, latency, error_rate, dharma (ethical score), karma_debt, and energy (resource pressure). Includes Guna classification (sattvic/rajasic/tamasic) for every tool invocation. Auto-fed by every `call_tool()` invocation. New MCP tool: `harmony_vector`.
- **Declarative Dharma Rules Engine** (`whitemagic/dharma/rules.py`): YAML-driven ethical policy engine inspired by MandalaOS's Yama. Replaces hardcoded keyword matching with declarative rules supporting graduated actions (LOG→TAG→WARN→THROTTLE→BLOCK), configurable profiles (default/creative/secure), Karmic Trace audit trail, and hot-reload. Wired into `DharmaSystem.evaluate_action()` as Layer 1. New MCP tools: `dharma_rules`, `set_dharma_profile`, `karmic_trace`.
- **Karma Ledger** (`whitemagic/dharma/karma_ledger.py`): Tracks declared vs actual side-effects for every tool call. READ tools that secretly write accrue debt; persistent mismatches feed into the Harmony Vector's karma_debt dimension. Persisted to `$WM_STATE_ROOT/dharma/karma_ledger.jsonl`. New MCP tool: `karma_report`.
- **Circuit Breaker** (`whitemagic/tools/circuit_breaker.py`): Stoic resilience pattern — per-tool breaker with CLOSED→OPEN→HALF_OPEN states. 5 failures in 60s trips the breaker; 30s cooldown; half-open probe. Wired into `dispatch_table.dispatch()` as step 0 before Governor and handler lookup.
- **Gnosis Portal** (`whitemagic/tools/gnosis.py`): Unified read-only introspection across all subsystems in a single call — harmony, dharma, karma, circuit breakers, yin/yang, telemetry, and state paths. Inspired by MandalaOS's Gnosis Portals. New MCP tool: `gnosis`.
- **Test Suite**: 41 regression tests (`tests/unit/test_mandala_subsystems.py`) covering all new modules.

### Changed
- **`whitemagic/tools/unified_api.py`**: `_record_telemetry()` now feeds Harmony Vector and Karma Ledger on every tool call; harmony_score injected into envelope metrics.
- **`whitemagic/tools/dispatch_table.py`**: `dispatch()` now runs circuit breaker check (step 0), then governor, then handler, then circuit breaker feedback (step 5). Registered 7 new tool handlers.
- **`whitemagic/dharma/__init__.py`**: `DharmaSystem.evaluate_action()` and `check_boundaries()` delegate to declarative rules engine first, legacy keyword matching as fallback.
- **`whitemagic/harmony/__init__.py`**: Exports `HarmonyVector`, `HarmonySnapshot`, `Guna`, `get_harmony_vector`.
- **`whitemagic/tools/registry.py`**: Added 7 new tool definitions (harmony_vector, karma_report, karmic_trace, dharma_rules, set_dharma_profile, gnosis).

## [11.1.0] - 2026-02-07
### Added
- **Temporal Scheduler** (`whitemagic/core/resonance/temporal_scheduler.py`): Time-bucketed event processing with three lanes — FAST (<10ms, safety/reflexes), MEDIUM (~1s, planning), SLOW (~60s, consolidation). Inspired by CyberBrains' CNS multi-timescale architecture. Supports pre/post-flush hooks and per-lane statistics.
- **Salience Arbiter** (`whitemagic/core/resonance/salience_arbiter.py`): Global Workspace-style attention routing. Scores events by urgency×novelty×confidence using a weighted geometric mean. Maintains a ranked "spotlight" of the most important active events. Plugs into the Temporal Scheduler as a pre-flush hook.
- **Mindful Forgetting** (`whitemagic/core/memory/mindful_forgetting.py`): Multi-signal memory retention engine. Memories survive only when vouched for by multiple independent signals: semantic importance, emotional salience, recency/recall, connection density, and protection flags. Periodic sweep support for batch evaluation.
- **Maturity Gates** (`whitemagic/core/governance/maturity_gates.py`): Gated developmental milestones (Seed→Bicameral→Reflective→Radiant→Collective→Logos). Each stage unlocks specific capabilities only after safety gate checks pass. Prevents use of advanced features before foundations are verified.
- **Bicameral Reasoner** (`whitemagic/core/intelligence/bicameral.py`): Dual-hemisphere reasoning — Left (precise, analytical, low-temperature) vs Right (creative, holistic, high-temperature) — with corpus callosum cross-critique. Produces tension-aware synthesis combining both perspectives.
- **Test Suite**: 40 unit tests covering all new modules (`tests/unit/test_cyberbrain_modules.py`).

### Changed
- **Resonance `__init__.py`**: Now exports `TemporalScheduler`, `TemporalLane`, `SalienceArbiter`, `SalienceScorer`, and their singletons.
- **SYSTEM_MAP.md**: Added CyberBrains-Inspired Modules section.

## [11.0.0] - 2026-02-06
### Added
- **Configurable Path System**: All paths now use `whitemagic.config.paths` module with environment variable overrides (`WM_STATE_ROOT`, `WM_DB_PATH`, etc.).
- **Strategy Distillation Pipeline**: Holographic clustering for synthesizing high-value strategies from memory corpus.
- **Satkona Dual-Engine**: Yin (Rust IDF) + Yang (Mojo cosine sim) pattern scoring with MMR selection.
- **Bulk Ingest**: Unified ingestion from legacy DBs and JSONL archives.

### Changed
- **Documentation Cleanup**: Consolidated conflicting docs, archived outdated SYSTEM_MAP v2.
- **Path Standardization**: Removed hardcoded `/home/<user>/...` paths across Python, Elixir, and Mojo.
- **Config Module**: Fixed conflict between `whitemagic.config.__init__.py` and `paths.py` - now uses centralized paths.

### Fixed
- **Elixir Paths**: Librarian and WisdomIngestor now use Application.get_env/env vars.
- **Mojo Orchestra**: Uses `WHITEMAGIC_ROOT` env var with fallback.
- **Audit Scripts**: All use configurable DB paths with fallback to `~/.whitemagic/memory/whitemagic.db`.

## [9.0.0] - 2026-02-04
### Added
- **Tiered Memory System**: Short-term, Long-term, and Archive storage with automatic promotion/demotion.
- **Polyglot Architecture**: 
  - Rust bindings (`whitemagic-rust`) for performance critical paths.
  - Go Mesh (`mesh/`) for distributed networking.
  - Mojo Accelerator (`accelerator/`) for AI compute.
  - Elixir Supervision (`elixir/`) for fault tolerance.
- **Holographic Hub**: Tauri-based frontend for interacting with system state.
- **MCP Integration**: Native Model Context Protocol support for Cursor, Windsurf, and Claude Desktop.
- **Security Hardening**: 
  - Automated secret generation.
  - Permissions enforcement (0600) for secrets files.
  - Deployment mode validation (local vs cloud).

### Changed
- **Performance**: Achieved 87% token cost reduction via context optimization.
- **Scaling**: Validated up to 16,000 concurrent async operations.
- **Structure**: Reorganized codebase into modular components.

### Fixed
- **Memory Leaks**: Resolved circular references in long-running sessions.
- **Type Safety**: strict `mypy` enforcement across Python core.

## [5.1.0] - 2026-01-26
### Secure
- Security audit and hardening.
- Dependency updates.

## [5.0.0] - 2025-12-01
### Added
- Initial Core Memory Architecture.
