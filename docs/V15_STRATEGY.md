# WhiteMagic v15 Strategy — From v14.2 to the Autonomous Horizon

**Prepared:** 2026-02-11
**Current Version:** 14.3.1
**Scope:** Everything between now and v15.0 release

---

## What's Shipped (v14.0 → v14.2)

Everything below is **done** and should not be re-planned:

| Version | Key Deliverables |
|---------|-----------------|
| **14.0** | MCP 3.0, Multi-Galaxy, Ollama Agent Loop, Edgerunner Violet Security, 208 tools |
| **14.1.0** | Streamable HTTP transport, per-Gana icons, MCP resources (3), MCP registry, quickstart seeds |
| **14.1.1** | HDBSCAN constellations, Hungarian drift, content hash dedup, `wm init`, `llms.txt` auto-gen, 6 workflow templates |
| **14.2.0** | Association pruning, tag normalization, E2E MCP test, `wm doctor --fix` |
| **14.3.0** | Constellation Recall Boost (Phase 1), Ed25519 Manifest Signing (Phase 4A), Karma Ledger Rotation, importance calibration script |
| **14.3.1** | Bhīṣma Governance Layer (Phase 0): Gana Vitality, Sabhā, Forge, Grimoire unification, resonance integration. 285 tools. |

---

## Phase 0: Bhīṣma Governance Layer (v14.3-pre → v14.3.1) ✅ DONE

**Effort:** 1 session
**Theme:** Apply Mahābhārata 12.108 gaṇa governance principles to harden the Gana system before public release.

### What Was Built

Five systems, derived from Bhīṣma's advice to Yudhiṣṭhira on how organized collectives (gaṇas) maintain strength:

| System | Sanskrit Principle | Verse | Files |
|--------|-------------------|-------|-------|
| **Gana Vitality** | 12.108.20 + 12.108.29 | "Honor competence" + "When people stop speaking, defeat shows" | `gana_vitality.py` |
| **Gana Sabhā** | 12.108.25 | "Leaders should act in concert for the common good" | `gana_sabha.py` |
| **Gana Forge** | 12.108.17 | "Establish dharmic procedures, then follow them" | `gana_forge.py` |
| **Grimoire Unification** | 12.108.25 (bheda) | "Disunited, they fall an easy prey to foes" | `chapters.py`, `core.py`, `spells.py` |
| **Resonance Integration** | 12.108.20 | Reputation data injected into PRAT resonance context | `prat_resonance.py`, `prat_router.py` |

### Key Metrics

- **5 new MCP tools**: `sabha.convene`, `sabha.status`, `forge.status`, `forge.reload`, `forge.validate`
- **59 new tests**, all passing
- **1,165 total unit tests**, 0 regressions
- **Grimoire unified**: 3 competing chapter systems (20-ch legacy, 28-ch walkthrough, 28-ch markdown) → 1 source of truth
- **Spells aligned**: All 28 spells now map 1:1 to Lunar Mansions
- **Tool count**: 208 → 213 → 285 (governance + subsequent additions)
- **Extension protocol**: Any AI can now define tools via YAML in `~/.whitemagic/extensions/`

---

## Phase 1: Constellation-Based Recall Boost (v14.3.0) ✅ DONE

**Shipped in:** v14.3.0
**Theme:** Use detected constellations as a retrieval signal to improve search quality.

### What Was Built

- **Constellation membership index** — `constellation_membership` table persists memory→constellation mappings after detection. Auto-migrated.
- **Query constellation detection** — `closest_constellation()` in `EmbeddingEngine` matches queries to constellations via embedding similarity.
- **Boost factor** — `search_hybrid()` applies 30% multiplicative boost for same-constellation results.
- **Cross-constellation serendipity** — 5% diversity bonus for cross-constellation results.
- **Files modified:** `constellations.py`, `sqlite_backend.py`, `unified.py`, `embeddings.py`
- **15 new tests** in `test_constellation_recall.py`, all passing

---

## Phase 2: Packaging & Distribution Completion (v14.4)

**Effort:** 1 week
**Theme:** Make `pip install whitemagic` work for everyone.

### Remaining Items (from Leap 5)

1. **TestPyPI dry run** — `python -m build` → `twine upload --repository testpypi` → fresh venv install → `wm doctor`
2. **Clean install verification** — Matrix: Python 3.10, 3.11, 3.12 on Ubuntu + macOS
3. **Docker image** — Multi-stage build verification. `docker run whitemagic` starts MCP stdio server
4. **GitHub Actions release workflow** — Tag → build → publish → GitHub Release with RELEASE_NOTES
5. **Dependency audit** — Verify extras groups resolve, pin vulnerable transitive deps

### Operational Items (No Code)

- **Galaxy data hygiene** — Reset default galaxy to 12 seed memories before public release
- **MCP registry submission** — Submit to `registry.modelcontextprotocol.io` and PulseMCP

---

## Phase 3: The Nervous System (v14.5)

**Effort:** 3-5 sessions
**Theme:** Separate Control Plane from Data Plane. Make Rust/Zig accelerators authoritative.

### What Exists (Built, Advisory Only)

| Component | File | Perf | Status |
|-----------|------|------|--------|
| **StateBoard** | `whitemagic-rust/src/state_board.rs` (418 LOC) | 2.3µs read, 432K ops/sec | Built, advisory |
| **EventRing** | `whitemagic-rust/src/event_ring.rs` (383 LOC) | 387ns pub, 2.58M ops/sec | Built, advisory |
| **Dispatch Core** | `whitemagic-zig/src/dispatch_core.zig` (275 LOC) | 1.6µs pipeline, 641K ops/sec | Built, advisory |

### What's Needed to Make Authoritative

1. **StateBoard as Source of Truth** — Python reads HarmonyVector from StateBoard via mmap bridge, not Python singleton. Rust writes, Python reads.
2. **EventRing as Primary Bus** — `gan_ying_enhanced.py emit()` writes to EventRing FIRST, then Python subscribers. Rust ring is the authority.
3. **Dispatch Authoritative** — If Zig says CIRCUIT_OPEN or RATE_LIMITED, skip Python pipeline entirely. Zig is the fast-path gatekeeper.
4. **FFI Consolidation** — Compile Zig as static lib, link into Rust PyO3 module. One FFI crossing instead of two.

### Files to Modify

- `whitemagic/core/acceleration/state_board_bridge.py` — Make reads authoritative
- `whitemagic/core/acceleration/event_ring_bridge.py` — Make publishes authoritative
- `whitemagic/core/acceleration/dispatch_bridge.py` — Gate Python pipeline
- `whitemagic/harmony/vector.py` — Read from StateBoard instead of internal dict
- `whitemagic/core/resonance/gan_ying_enhanced.py` — Publish to EventRing first
- `whitemagic/tools/dispatch_table.py` — Check Zig dispatch before Python middleware

---

## Phase 4: Cryptographic Trust (v15.0)

**Effort:** 2-3 weeks
**Theme:** Make WhiteMagic the most trustworthy tool substrate in the agent ecosystem.

### 4A: Ed25519 Manifest Signing ✅ DONE (shipped v14.3.0)

Every tool gets a **Secure Tool Manifest** — a machine-verifiable passport:

- **Canonical Identity** — DID:key (W3C Decentralized Identifier) for WhiteMagic Labs
- **Integrity Hash** — SHA-256 of handler function source
- **Permission Scope** — `reads_memory`, `writes_memory`, `network_access`, `file_system`, `side_effects`
- **Digital Signature** — Ed25519 signature over manifest

**Shipped:** `generate_signing_keypair()`, `sign_manifest()`, `verify_signature()`, DID:key format. Keypair at `~/.whitemagic/keys/`. Uses PyNaCl. MCP tool: `manifest_sign_tool`.
**Remaining:** Full per-tool permission scope annotation (currently only signing infrastructure)

### 4B: Karma XRPL Anchoring

Extend Karma Ledger into a publicly verifiable transparency log:

1. Rolling Merkle tree of karma trace entries
2. Hourly anchor: Merkle root → XRPL transaction memo field (~0.00001 XRP)
3. Verification tool: any agent checks audit trail against on-chain anchor

**What exists:** `_merkle_tree_root()` in `karma_ledger.py`
**What's needed:** `xrpl-py` integration, testnet submission, verification tool

### 4C: Reproducible Builds

- Pin all build tool versions
- Deterministic wheels via `SOURCE_DATE_EPOCH`
- CI publishes build hashes to transparency ledger
- Agents verify software by checking hash against ledger

### Exit Criteria (v15.0 Tag)

- Every tool has a signed manifest with permission scopes
- Karma Merkle root anchored to XRPL testnet
- Wheel builds are reproducible (same source → same hash)

---

## Phase 5: The Autonomous Horizon (v15.1+)

**Effort:** Multi-month
**Theme:** From Python package to universal capability substrate.

### 5A: WASM Capability Modules

Compile core tools into portable `.wasm` components:

- `create_memory.wasm` — Memory creation with SQLite
- `search_memory.wasm` — BM25 + semantic hybrid search
- `holographic_encode.wasm` — 5D coordinate encoding
- `rate_limiter.wasm` — Atomic rate limiting

Target: `wasm32-wasi` via the Rust codebase. KB-sized modules, millisecond link time.

### 5B: Static Binary MCP Server

Single static Rust binary for air-gapped/edge deployments:

- Embeds SQLite + core memory ops + MCP stdio server
- Zero runtime dependencies (no Python)
- Targets: `x86_64-unknown-linux-musl`, `aarch64-unknown-linux-musl`
- MandalaOS governance compiled in

### 5C: Gratitude Economy (Phases b-d)

| Phase | What | When |
|-------|------|------|
| **5.5b** | `whitemagic.tip` XRPL tool, Proof of Gratitude verification | After v15.0 |
| **5.5c** | x402 endpoint middleware, Gratitude dashboard in Nexus | Month after |
| **5.5d** | Micro-bounties, weighted voting, transparency reports | Quarter after |

### 5D: Advanced Graph Intelligence

From DEFERRED items across strategy docs:

- Causality enforcement during graph walker traversal
- Temporal PageRank with citation decay
- Echo chamber detection via eigenvector centrality
- LLM hypothesis generation from bridge nodes during dream cycle
- Counterfactual simulation via sandbox "Dream Graph"
- HyTE temporal embeddings (needs training infrastructure)
- RE-NET link prediction (needs PyTorch training loop)

### 5E: GPU Acceleration

**Blocked by:** No discrete GPU on current hardware. Resume when available.

- Batch holographic encoding (110K → <1s on GPU)
- N² pairwise distance matrix
- Embedding similarity (110K × 384)
- Local neural inference via Mojo megakernel

---

## Remaining Items Not Covered Above

These are tracked but have no planned phase yet:

| Item | Source | Blocker |
|------|--------|---------|
| MCP Bundle (.mcpb) | BEYOND_V14_1 | MCP bundle spec not finalized |
| MCP Elicitation for Governance | BEYOND_V14_1 | MCP SDK elicitation support in clients |
| Holographic Y/Z re-encoding | NEXT_PATCH P2 #13 | Low priority, runtime-heavy batch job |
| Iceoryx2 IPC | STRATEGIC_ROADMAP Leap 8a | Depends on Nervous System completion |
| Mojo GPU megakernel | STRATEGIC_ROADMAP Leap 8b | GPU hardware + Mojo maturity |
| PRAT Compute Marketplace | STRATEGIC_ROADMAP Leap 10c | Depends on Cryptographic Trust |
| MCP Apps Dashboard (Nexus) | BEYOND_V14_1 Horizon 3 | UI work, lower priority than core |
| Importance calibration batch | BEYOND_V14_1 | Data quality, can be done anytime |
| MCP transport encryption | BEYOND_V14_1 | `--https` flag, self-signed certs |
| Audit log rotation | BEYOND_V14_1 | Karma ledger growth management |
| Fuzz testing | BEYOND_V14_1 | Testing infrastructure |
| Load testing (100+ parallel) | BEYOND_V14_1 | Testing infrastructure |

---

## Recommended Execution Order

```
Phase 1: Constellation Recall Boost (v14.3)     ✅ DONE
Phase 0: Bhīṣma Governance Layer (v14.3.1)       ✅ DONE
Phase 4A: Ed25519 Manifest Signing (v14.3.0)     ✅ DONE
Phase 2: Packaging & Distribution (v14.4)        ~ 1 week
Phase 3: Nervous System Authoritative (v14.5)    ~ 3-5 sessions
  ── v14.x complete, prepare for v15.0 ──
Phase 4B-C: Karma Anchoring + Repro Builds (v15.0) ~ 2-3 weeks
Phase 5: Autonomous Horizon (v15.1+)             ~ ongoing
```

---

## Metrics at v14.3.1

| Metric | Value |
|--------|-------|
| **Python** | ~770 files, ~170K LOC |
| **Polyglot** | ~25K LOC, 9 languages |
| **MCP Tools** | 285 (28 PRAT Gana meta-tools) |
| **MCP Resources** | 9 (3 orientation + 6 workflows) |
| **Tests** | 1,060+ unit passing |
| **Embeddings** | 384 dims, MiniLM-L6-v2, HNSW indexed |
| **Constellations** | 19 detected (HDBSCAN + Hungarian drift), recall-boosted search |
| **Engines** | 28 (mapped to Gardens/Ganas/Grimoire) |
| **Governance** | Bhīṣma Layer: Vitality, Sabhā, Forge, Ed25519 signing, Karma rotation |
| **Accelerators** | Rust ✅, Zig ✅, Haskell ❌ (runtime), Julia ❌ (runtime) |

---

*This document supersedes `BEYOND_V14_1.md`, `NEXT_PATCH_v14_1_1.md`, and `STRATEGIC_ROADMAP.md`, all of which are archived.*
