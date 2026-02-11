# High-Frequency PRAT Strategy — Research-Informed Architecture Evolution

**Date:** February 10, 2026
**Version:** v13.6.0 → v14.x
**Source:** "Architectural Analysis of High-Frequency Agentic Primitives: Rust, Zig, and Mojo"
**Status:** Draft — For Next Session Execution

---

## Executive Summary

The research team's analysis of high-frequency agentic primitives validates WhiteMagic's polyglot architecture while exposing three critical gaps: **FFI bridge overhead**, **incomplete pipeline compilation**, and **missing Megakernel architecture** for GPU batching. WhiteMagic uniquely implements all three of the paper's recommended scenarios simultaneously (Zig for CPU real-time, Mojo for GPU batch, Rust for safety-critical finance) — something the paper doesn't envision as a single system. This document translates the research findings into an actionable execution plan.

### The Core Thesis

> WhiteMagic's PRAT system is a Polymorphic Resonant Adaptive Tool architecture. The research paper defines exactly this class of system and concludes that the optimal substrate is a multi-language compiled runtime with Zig for latency-critical dispatch, Rust for safety-critical state, and Mojo for throughput-critical batching. **We already have all three.** What we lack is optimal *integration* between them.

---

## Part I — Gap Analysis: Research vs Current State

### 1. FFI Overhead (The #1 Problem)

The paper identifies FFI overhead as the single most critical bottleneck. Our benchmarks confirm this empirically:

| Bridge Model | Paper's Theoretical | WM Measured | Gap Factor |
|---|---|---|---|
| Rust PyO3 (baseline call) | ~230ns | ~450ns (rate limiter) | **2×** — acceptable |
| Rust PyO3 (data marshal) | ~100-200ns extra | ~2µs (BM25 with strings) | **~4×** — acceptable |
| Zig C-ABI (direct) | ~220ns | N/A — not using direct | — |
| Zig ctypes (our model) | — | **1,600ns** (dispatch check) | **7× vs theoretical** |
| Zig ctypes (keyword extract) | — | **15× SLOWER than Python** | **Fatal** — disabled |
| Mojo subprocess | — | **Milliseconds** | **~5000× vs theoretical** |

**Root Cause Diagnosis:**

- **Zig**: We use Python `ctypes` which adds buffer allocation, type coercion, GIL interaction, and pointer marshaling per call. The paper's 220ns assumes Ziggy Pydust or direct C-extension compilation against `Python.h`. The gap is the bridge model, not the Zig code.
- **Mojo**: We launch a full subprocess per call (JSON serialization → process spawn → execute → JSON parse). This is the worst possible interop model. Even Mojo's native `PythonObject` wrapper (the paper's ~300ns+) would be 1000× better.
- **Rust**: PyO3 is already near-optimal. The `Extract` trait copying and `catch_unwind` wrapper are the main overhead sources, but these are acceptable for our workloads.

### 2. Tool Dispatch Pipeline

The paper recommends moving the **entire dispatch pipeline** into a single compiled function. Our current state:

```
Current Pipeline (Python, ~40µs total):
  1. Extract flags        — Python dict access
  2. Input Sanitizer      — Python regex + depth check (~1µs)
  3. Circuit Breaker      — Python dict → Zig via ctypes (~1.6µs)
  4. Rate Limiter         — Python → Rust PyO3 (~0.45µs)
  5. RBAC Permissions     — Python dict lookup (~0.5µs)
  6. Maturity Gate        — Python dict → Zig via ctypes (included in #3)
  7. Governor check       — Python function call (~1µs)
  8. Gana routing         — Python PRAT router (~2µs)
  ── FFI BOUNDARY ──
  9. Handler execution    — Python (most work happens here)
  10. Compact response    — Python post-processing
```

The paper says: steps 1-8 should be **one compiled function call**. We cross the FFI boundary multiple times (Python→Zig, Python→Rust, back to Python, repeat). Each crossing costs ~200-1600ns of pure overhead.

**Ideal pipeline:**
```
Proposed Pipeline (~2-5µs total):
  1. Python receives MCP request, extracts tool_name + args
  2. Single FFI call → compiled pipeline check:
     - Sanitize (Zig comptime bounds check)
     - Circuit breaker (Zig reads StateBoard mmap)
     - Rate limit (Rust atomic counter)
     - RBAC (Zig comptime role table)
     - Maturity gate (Zig comptime, already done)
     - Governor (Zig reads StateBoard harmony vector)
     - Route → handler_id (Zig comptime)
  3. Return: { allowed: bool, handler_id: u32, denial_reason: u8 }
  4. Python calls the handler (actual tool work)
```

### 3. SIMD Vector Search

The paper validates our architecture:
- ✅ Zig `@Vector(8, f32)` with AVX2 — we use this, it works (6.5-13× on distance matrices)
- ✅ Small-batch L1/L2 cache locality — our 50-5000 vector batches fit
- ✅ CPU-only for latency-sensitive paths — BM25 (2µs), rate limiter (0.45µs)
- ⚠️ Single-vector cosine is a wash — ctypes overhead ≈ SIMD gain (confirmed by paper's FFI analysis)

The fix is the same as #1: replace ctypes with proper FFI. The Zig SIMD *code* is good; the *bridge* is the bottleneck.

### 4. GPU / Megakernel Architecture

| Aspect | Paper's Recommendation | WM Current | Action |
|---|---|---|---|
| GPU for batch (>1000 ops) | ✅ Correct | ✅ GPU_ACCELERATION_ROADMAP.md agrees | None |
| CPU for latency (<5µs) | ✅ Correct | ✅ BM25/rate limiter stay CPU | None |
| Megakernel pattern | Persistent kernel + ring buffer | ❌ Not in Leap 8b detail | **Add to roadmap** |
| Mojo for unified host+device | Recommended | ⚠️ Mojo is CPU-only, subprocess | Deferred until GPU hardware |
| Kernel launch overhead | 5-25µs, violates budget | N/A (no GPU) | Awareness noted |

### 5. Safety Trade-off

The paper identifies three scenarios. WhiteMagic implements all three:

| Paper Scenario | Description | WM Implementation | Status |
|---|---|---|---|
| **A: CPU Real-time** | Zig primary, sub-5µs | Leap 7 dispatch core | ✅ Built, needs FFI upgrade |
| **B: Batch Throughput** | Mojo primary, GPU megakernel | Leap 8 GPU path | ⬜ Deferred (no GPU) |
| **C: Safety-Critical** | Rust primary, memory-safe finance | Leap 5.5 + 9 | ⚠️ WalletGovernor is Python |

---

## Part II — Execution Plan

### Phase 1: FFI Consolidation — "One Bridge to Rule Them All" (HIGH IMPACT)

**Goal:** Eliminate the ctypes bottleneck by compiling Zig into the Rust PyO3 module.

**Architecture:**
```
Before:
  Python → ctypes → Zig .so     (1,600ns overhead per call)
  Python → PyO3  → Rust .so     (  450ns overhead per call)
  Python → subprocess → Mojo     (     ms overhead per call)

After:
  Python → PyO3 → Rust+Zig unified .so  (450ns overhead, ONE crossing)
```

**Implementation:**

#### 1a. Zig-in-Rust Static Linking

Compile Zig modules as a C-compatible static library (`libwhitemagic_zig.a`), then link into the Rust PyO3 crate via `build.rs`.

```rust
// build.rs addition
println!("cargo:rustc-link-search=native=../whitemagic-zig/zig-out/lib");
println!("cargo:rustc-link-lib=static=whitemagic_zig");
```

```rust
// New file: whitemagic-rust/src/zig_bridge.rs
// Thin Rust wrappers around Zig C-ABI functions
extern "C" {
    fn wm_dispatch_check(tool_id: u32, board_ptr: *const u8) -> i32;
    fn wm_dispatch_route(tool_id: u32) -> u32;
    // ... all Zig SIMD exports
}

#[pyfunction]
fn dispatch_check(tool_id: u32) -> PyResult<i32> {
    let board = get_board_ptr();  // From state_board.rs mmap
    Ok(unsafe { wm_dispatch_check(tool_id, board) })
}
```

**Effect:** Zig dispatch check drops from 1,600ns to ~250-300ns (PyO3 overhead + Zig native). Zig SIMD functions become accessible at PyO3 speed instead of ctypes speed.

**Files to create/modify:**
- `whitemagic-rust/build.rs` — Add Zig static library linking
- `whitemagic-rust/src/zig_bridge.rs` — Rust-to-Zig FFI wrappers with PyO3 bindings
- `whitemagic-rust/src/lib.rs` — Register new zig_bridge module
- `whitemagic-zig/build.zig` — Ensure static library output (`-femit-bin` → static `.a`)
- `whitemagic/core/acceleration/dispatch_bridge.py` — Use PyO3 path instead of ctypes
- `whitemagic/core/acceleration/simd_*.py` — All 6 Zig bridges → use PyO3 wrappers

#### 1b. Expanded Comptime Dispatch Table

Expand `dispatch_core.zig` from 28 engines to 181 individual tools. Use comptime to generate:

```zig
// Generated from registry at build time
const TOOL_COUNT = 181;

const tool_table: [TOOL_COUNT]ToolEntry = comptime blk: {
    var table: [TOOL_COUNT]ToolEntry = undefined;
    // Per-tool: handler_id, engine_slot, maturity, rbac_min_role, max_input_size
    table[0] = .{ .handler_id = 100, .engine = 0, .maturity = .mature, .role = .agent, .max_input = 65536 };
    // ... 180 more entries
    break :blk table;
};
```

This enables per-tool routing at the same speed as per-engine routing — O(1) comptime array index.

**Files to create/modify:**
- `whitemagic-zig/src/dispatch_core.zig` — Expand to 181-tool table
- `scripts/generate_zig_dispatch_table.py` — Generate Zig source from Python registry

#### 1c. Unified Pipeline Check

Collapse the 8-step Python pipeline into a single compiled function:

```zig
pub const PipelineResult = extern struct {
    allowed: bool,
    handler_id: u32,
    denial_reason: u8,  // 0=ok, 1=sanitize, 2=breaker, 3=rate, 4=rbac, 5=maturity, 6=governor
    engine_slot: u8,
};

export fn wm_pipeline_check(
    tool_id: u32,
    board_ptr: [*]const u8,
    agent_role: u8,
    input_size: u32,
) PipelineResult {
    // 1. Bounds / sanitize
    if (input_size > tool_table[tool_id].max_input) return .{ .denied, .sanitize };
    // 2. Circuit breaker (read mmap)
    // 3. Rate limit (read mmap counter)
    // 4. RBAC (comptime role comparison)
    // 5. Maturity gate (comptime)
    // 6. Governor (read mmap harmony thresholds)
    // 7. Route
    return .{ .allowed = true, .handler_id = tool_table[tool_id].handler_id, ... };
}
```

**Target:** Full pipeline in <1µs (vs current ~40µs in Python).

### Phase 2: Rust Financial Safety Layer (MEDIUM IMPACT)

**Goal:** Move financial operations from Python to Rust for memory-safe handling of real money.

The paper is clear: "If the cost of a segmentation fault outweighs the benefit of shaving 2-3µs, Rust is the mandatory choice." Financial governance handling XRPL tips and x402 micropayments MUST be in Rust.

**Implementation:**

#### 2a. WalletGovernor in Rust

Port `dharma/financial_governance.py` to Rust with PyO3:

```rust
// whitemagic-rust/src/wallet_governor.rs
#[pyclass]
struct WalletGovernor {
    max_per_tx: f64,
    max_per_session: f64,
    max_per_day: f64,
    blocked_addresses: HashSet<String>,
    session_total: AtomicF64,
    daily_total: AtomicF64,
}

#[pymethods]
impl WalletGovernor {
    fn approve_transaction(&self, amount: f64, recipient: &str) -> PyResult<bool> {
        // Memory-safe, no GIL needed for the check
    }
}
```

#### 2b. Karma Merkle Tree in Rust

The Merkle tree computation (SHA-256 hashing of karma entries) is already supported by our Rust deps (`sha2`, `hex`). Move `_merkle_tree_root()` from Python to Rust for:
- Constant-time hashing (no Python object overhead)
- Memory-safe tree construction
- Future XRPL anchoring integration

**Files to create/modify:**
- `whitemagic-rust/src/wallet_governor.rs` — New Rust WalletGovernor
- `whitemagic-rust/src/karma_merkle.rs` — Merkle tree in Rust
- `whitemagic-rust/src/lib.rs` — Register new modules
- `whitemagic/dharma/financial_governance.py` — Call Rust, Python fallback

### Phase 3: Megakernel Architecture Specification (DEFERRED — GPU Required)

**Goal:** When GPU hardware is available, implement the persistent kernel pattern for batch operations.

The paper's Megakernel architecture is the **only way** to use GPU for sub-5µs operations:

```
Architecture:
  1. At startup: Launch persistent Mojo kernel on GPU (runs infinite loop)
  2. CPU writes batch inputs to pinned memory ring buffer
  3. GPU kernel polls the ring buffer (spin-wait on flag)
  4. GPU processes batch immediately (no launch overhead)
  5. GPU writes results to output ring buffer
  6. CPU reads results

Ring Buffer Design (shared with Rust EventRing):
  - Same 128-byte slot format as event_ring.rs
  - Pinned memory (cudaMallocHost / Mojo equivalent)
  - Atomic sequence numbers for producer-consumer coordination
  - Overwrite policy (stale context = irrelevant context)
```

**First target:** Batch embedding similarity (5,562 × 384-dim vectors). Currently minutes on CPU, target <5s on GPU, <100ms with persistent kernel.

**Second target:** Batch holographic encoding (110K memories × 5D). Currently ~55s Rust Rayon, target <1s on GPU.

**Files to create (when GPU available):**
- `whitemagic-mojo/src/megakernel.mojo` — Persistent kernel with polling loop
- `whitemagic-mojo/src/ring_buffer.mojo` — Pinned memory ring buffer
- `whitemagic/core/acceleration/mojo_gpu_bridge.py` — Python bridge for GPU batch ops

### Phase 4: Benchmarking Discipline & Performance Gates

**Goal:** Establish quantitative performance gates informed by the paper's latency analysis.

#### Performance Budget (per the paper's 5µs framework)

| Operation | Budget | Current | Target | Gate |
|---|---|---|---|---|
| FFI round-trip (empty) | — | 450ns (Rust), 1600ns (Zig) | <300ns (unified) | Block release if >500ns |
| Pipeline check (full) | 2µs | ~40µs (Python) | <2µs (compiled) | Block release if >5µs |
| Vector search (100 vectors) | 10µs | 7.1ms (Zig batch) | <1ms (PyO3 bridge) | Warn if >5ms |
| BM25 search | 5µs | 2µs (Rust) | 2µs (already met) | Block if >5µs |
| Rate limiter | 1µs | 0.45µs (Rust) | 0.45µs (already met) | Block if >1µs |
| Event publish | 500ns | 387ns (Rust EventRing) | 387ns (already met) | Block if >1µs |
| StateBoard read | 100ns | 2.3µs (via Python bridge) | <500ns (mmap direct) | Warn if >1µs |

#### Benchmark Suite Extension

```python
# scripts/benchmark_hf_prat.py — High-Frequency PRAT benchmark suite
# Tests:
# 1. FFI round-trip: empty call through each bridge (PyO3, ctypes, subprocess)
# 2. Pipeline check: full 8-step dispatch for 28 engines × 100 iterations
# 3. Pipeline check (compiled): single FFI call through Zig unified pipeline
# 4. Vector search: 50, 100, 500, 1000 vectors at 384 dims
# 5. Sustained throughput: 10K calls in 100ms window (target: 100K/sec)
```

---

## Part III — Prioritized Execution Order

### Next Session: Phase 1a + 1b + Benchmark Suite

| Step | Task | Est. LOC | Impact |
|---|---|---|---|
| **1** | Modify `build.zig` to emit static library | ~20 | Prerequisite |
| **2** | Add Zig static linking to Rust `build.rs` | ~30 | Prerequisite |
| **3** | Create `zig_bridge.rs` — dispatch + SIMD wrappers | ~200 | Core FFI fix |
| **4** | Register new PyO3 functions in `lib.rs` | ~40 | Wiring |
| **5** | Update `dispatch_bridge.py` — use PyO3 path | ~30 | Consumer fix |
| **6** | Update `simd_*.py` bridges — use PyO3 wrappers | ~100 | Consumer fix |
| **7** | Expand `dispatch_core.zig` to 181 tools | ~300 | Per-tool routing |
| **8** | Create `generate_zig_dispatch_table.py` | ~150 | Build automation |
| **9** | Create `benchmark_hf_prat.py` | ~200 | Measurement |
| **10** | Benchmark before/after: measure FFI + pipeline gains | — | Validation |

**Expected results:**
- Zig dispatch: 1,600ns → ~300ns (5× improvement)
- Zig SIMD cosine: wash → competitive (ctypes overhead eliminated)
- Zig keyword extraction: 15× slower → competitive (may beat Python for large texts)
- Full pipeline: ~40µs → <5µs (8× improvement, goal of paper)

### Follow-Up Sessions

| Phase | Session | Dependencies | Impact |
|---|---|---|---|
| **1c** Unified pipeline check | Session N+1 | Phase 1a/1b done | 40µs → <2µs |
| **2a** WalletGovernor Rust | Session N+2 | None (independent) | Safety for Leap 5.5 |
| **2b** Karma Merkle Rust | Session N+2 | None (independent) | Safety for Leap 9 |
| **3** Megakernel spec | When GPU available | GPU hardware | Batch throughput |
| **4** Performance gates CI | Session N+3 | Benchmark suite | Regression prevention |

---

## Part IV — What the Paper Gets Wrong (For Us)

The research paper optimizes for a **single-language** solution. Its conclusion — "Zig for sub-5µs" — assumes you pick ONE language. WhiteMagic's polyglot architecture is the more nuanced answer:

1. **"Zig has unsafe manual RefCount"** — True, but our Zig code doesn't manage Python objects. It reads from mmap (StateBoard) and returns integers. The safety concern is minimal for our dispatch use case.

2. **"Mojo is only viable with Megakernels"** — True for sub-5µs *individual* calls. But our batch operations (holographic encoding, distance matrices) don't need sub-5µs per element — they need high throughput. Standard kernel launches are fine when batching 110K operations.

3. **"Rust's PyO3 overhead requires unsafe to match Zig"** — Our benchmarks show PyO3 at 450ns is adequate for all non-dispatch paths. We don't need to write unsafe Rust for memory operations, embedding search, or association mining. The 230ns vs 450ns difference is irrelevant when the handler does 2ms of actual work.

4. **"Python 3.13 free-threading risks"** — Valid concern. We should pin to Python 3.12 for production and test 3.13 in CI only. The 30-40% single-thread regression would hurt our pipeline.

---

## Part V — Alignment with Existing Roadmap

| Existing Leap | Research-Informed Enhancement |
|---|---|
| **Leap 7 (Nervous System)** | Phase 1: FFI consolidation + expanded dispatch table |
| **Leap 5.5 (Gratitude)** | Phase 2: Rust WalletGovernor for financial safety |
| **Leap 8 (Swarm)** | Phase 3: Megakernel architecture for Mojo GPU |
| **Leap 9 (Crypto Trust)** | Phase 2b: Rust Karma Merkle tree |
| **Leap 3 (Test Fortress)** | Phase 4: Performance gates in CI |

---

## Appendix A — Key Paper Citations for Implementation

> "The FFI toll constitutes 5-10% of the total latency budget... Zig achieves FFI call overheads as low as 220 nanoseconds, effectively matching raw C extensions."

→ Our ctypes overhead is 7× this. Fix: compile Zig into Rust PyO3 module.

> "Zig's comptime capability allows for the generation of static dispatch tables for thousands of tools at compile time, eliminating CPU branch misprediction penalties."

→ We have this for 28 engines. Expand to 181 tools.

> "Standard GPU kernel launches introduce latencies (5-25µs) that violate the sub-5µs requirement. Mojo is only viable if utilized within a Megakernel architecture."

→ Incorporate persistent kernel pattern into Leap 8b specification.

> "If the cost of a segmentation fault outweighs the benefit of shaving 2-3µs, Rust is the mandatory choice."

→ Move financial governance from Python to Rust before Leap 5.5 goes live.

## Appendix B — Current Benchmark Baseline (v13.6.0)

For regression testing after Phase 1 implementation:

```
Rust PyO3:
  Rate limiter (single):     0.00045ms  (2.21M ops/sec)
  BM25 search (500 docs):    0.002ms    (500K queries/sec)
  Keyword extract (6K text):  0.161ms   (1.8× vs Python)
  Holographic encode (single): 0.050ms
  Holographic encode (batch 100): 3.274ms
  MinHash (60 sets):          1.019ms
  Pipeline (10 candidates):   0.031ms

Zig ctypes:
  Dispatch check:             0.0016ms  (641K ops/sec)
  Distance matrix (100×128):  10.1ms    (13× vs Python)
  Cosine similarity (384):    0.084ms   (0.9× — wash)

Rust EventRing:
  Publish:                    0.000387ms (2.58M events/sec)

Rust StateBoard:
  Read:                       0.0023ms  (432K ops/sec)
```
