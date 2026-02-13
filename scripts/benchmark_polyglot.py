#!/usr/bin/env python3
"""
Comprehensive Polyglot Benchmark Suite
=======================================
Head-to-head comparison of all polyglot accelerators vs Python fallbacks.
Tests: Zig SIMD, Rust PyO3, Haskell FFI, and pure Python baselines.
"""
import time
import random
import math
import sys
import os
import statistics

# Ensure we can import whitemagic
sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
os.environ["WM_SILENT_INIT"] = "1"

ITERATIONS = 1000
WARMUP = 50


def timed(fn, iterations=ITERATIONS, warmup=WARMUP, label=""):
    """Run fn `iterations` times, return (mean_ms, median_ms, min_ms, p99_ms, total_s)."""
    # Warmup
    for _ in range(warmup):
        fn()
    
    times = []
    for _ in range(iterations):
        t0 = time.perf_counter_ns()
        fn()
        t1 = time.perf_counter_ns()
        times.append((t1 - t0) / 1e6)  # ms
    
    mean = statistics.mean(times)
    median = statistics.median(times)
    mn = min(times)
    p99 = sorted(times)[int(len(times) * 0.99)]
    total = sum(times) / 1000  # seconds
    return mean, median, mn, p99, total


def fmt_row(name, mean, median, mn, p99, total, speedup=None):
    sp = f"{speedup:.1f}×" if speedup else "baseline"
    print(f"  {name:<40} {mean:>8.3f}ms  med={median:.3f}  min={mn:.3f}  p99={p99:.3f}  {sp}")


def section(title):
    print(f"\n{'='*70}")
    print(f"  {title}")
    print(f"{'='*70}")


# ─────────────────────────────────────────────────────────────────────
# 1. ZIG SIMD: Keyword Extraction
# ─────────────────────────────────────────────────────────────────────
def bench_keyword_extraction():
    section("1. KEYWORD EXTRACTION — Zig SIMD vs Python")
    
    from whitemagic.core.acceleration.simd_keywords import (
        extract_keywords, _py_extract_keywords, simd_keywords_status,
    )
    
    status = simd_keywords_status()
    print(f"  Zig available: {status['has_zig_simd']}")
    print(f"  Backend: {status['backend']}")
    
    # Generate test texts of varying sizes
    sample_words = "whitemagic memory galactic holographic coordinate encoding system architecture agent tool dispatch pipeline harmony dharma karma vector resonance constellation association miner consolidation lifecycle retention score importance neuro novelty decay drift sweep temporal scheduler salience arbiter mindful forgetting maturity gate bicameral reasoner".split()
    
    for size_label, text_len in [("small (500 chars)", 500), ("medium (3K chars)", 3000), ("large (10K chars)", 10000)]:
        text = " ".join(random.choice(sample_words) for _ in range(text_len // 6))
        
        # Python fallback
        py_mean, py_med, py_min, py_p99, _ = timed(
            lambda t=text: _py_extract_keywords(t, 50),
            iterations=ITERATIONS,
        )
        
        if status['has_zig_simd']:
            # Force Zig path
            zig_mean, zig_med, zig_min, zig_p99, _ = timed(
                lambda t=text: extract_keywords(t, 50),
                iterations=ITERATIONS,
            )
            speedup = py_mean / zig_mean if zig_mean > 0 else 0
            fmt_row(f"Zig SIMD  [{size_label}]", zig_mean, zig_med, zig_min, zig_p99, 0, speedup)
        
        fmt_row(f"Python    [{size_label}]", py_mean, py_med, py_min, py_p99, 0)
        print()


# ─────────────────────────────────────────────────────────────────────
# 2. ZIG SIMD: Cosine Similarity
# ─────────────────────────────────────────────────────────────────────
def bench_cosine_similarity():
    section("2. COSINE SIMILARITY — Zig SIMD vs Python")
    
    from whitemagic.core.acceleration.simd_cosine import (
        cosine_similarity, batch_cosine, _py_cosine, simd_status,
    )
    
    status = simd_status()
    print(f"  Zig available: {status['has_zig_simd']}, lane_width: {status.get('lane_width', 'N/A')}")
    
    for dim in [64, 128, 384, 768]:
        a = [random.gauss(0, 1) for _ in range(dim)]
        b = [random.gauss(0, 1) for _ in range(dim)]
        
        py_mean, py_med, py_min, py_p99, _ = timed(
            lambda: _py_cosine(a, b), iterations=ITERATIONS,
        )
        
        if status['has_zig_simd']:
            zig_mean, zig_med, zig_min, zig_p99, _ = timed(
                lambda: cosine_similarity(a, b), iterations=ITERATIONS,
            )
            speedup = py_mean / zig_mean if zig_mean > 0 else 0
            fmt_row(f"Zig SIMD  [dim={dim}]", zig_mean, zig_med, zig_min, zig_p99, 0, speedup)
        
        fmt_row(f"Python    [dim={dim}]", py_mean, py_med, py_min, py_p99, 0)
        print()
    
    # Batch cosine (100 vectors × 384 dims)
    print("  --- Batch Cosine (100 vectors × 384 dims) ---")
    query = [random.gauss(0, 1) for _ in range(384)]
    vectors = [[random.gauss(0, 1) for _ in range(384)] for _ in range(100)]
    
    py_mean, py_med, py_min, py_p99, _ = timed(
        lambda: [_py_cosine(query, v) for v in vectors], iterations=200,
    )
    
    if status['has_zig_simd']:
        zig_mean, zig_med, zig_min, zig_p99, _ = timed(
            lambda: batch_cosine(query, vectors), iterations=200,
        )
        speedup = py_mean / zig_mean if zig_mean > 0 else 0
        fmt_row("Zig batch [100×384]", zig_mean, zig_med, zig_min, zig_p99, 0, speedup)
    
    fmt_row("Python loop [100×384]", py_mean, py_med, py_min, py_p99, 0)


# ─────────────────────────────────────────────────────────────────────
# 3. ZIG SIMD: Distance Matrix
# ─────────────────────────────────────────────────────────────────────
def bench_distance_matrix():
    section("3. DISTANCE MATRIX — Zig SIMD vs Python")
    
    from whitemagic.core.acceleration.simd_distance import (
        pairwise_distance_matrix, _py_distance_matrix, cosine_similarity_zig,
        _py_cosine, simd_distance_status,
    )
    
    status = simd_distance_status()
    print(f"  Zig available: {status['has_zig_simd']}")
    
    for n_vecs in [20, 50, 100]:
        vectors = [[random.gauss(0, 1) for _ in range(128)] for _ in range(n_vecs)]
        iters = max(50, 500 // n_vecs)
        
        py_mean, py_med, py_min, py_p99, _ = timed(
            lambda v=vectors: _py_distance_matrix(v), iterations=iters,
        )
        
        if status['has_zig_simd']:
            zig_mean, zig_med, zig_min, zig_p99, _ = timed(
                lambda v=vectors: pairwise_distance_matrix(v), iterations=iters,
            )
            speedup = py_mean / zig_mean if zig_mean > 0 else 0
            fmt_row(f"Zig SIMD  [{n_vecs}×128]", zig_mean, zig_med, zig_min, zig_p99, 0, speedup)
        
        fmt_row(f"Python    [{n_vecs}×128]", py_mean, py_med, py_min, py_p99, 0)
        print()


# ─────────────────────────────────────────────────────────────────────
# 4. RUST: Holographic Encoding
# ─────────────────────────────────────────────────────────────────────
def bench_rust_holographic():
    section("4. HOLOGRAPHIC ENCODING — Rust PyO3 vs Python")
    
    try:
        from whitemagic.optimization.rust_accelerators import (
            rust_available, rust_v131_available,
            holographic_encode_single, holographic_encode_batch,
        )
        has_rust = rust_v131_available()
    except ImportError:
        has_rust = False
    
    print(f"  Rust v131 available: {has_rust}")
    
    if not has_rust:
        print("  SKIP — Rust accelerators not built")
        return
    
    # Single encode
    mem = {"content": "Test memory for holographic encoding benchmark", "importance": 0.8,
           "emotional_valence": 0.3, "memory_type": "LONG_TERM", "access_count": 5,
           "tags": ["benchmark", "test"], "created_at": "2026-02-09T12:00:00"}
    
    rust_mean, rust_med, rust_min, rust_p99, _ = timed(
        lambda: holographic_encode_single(mem), iterations=ITERATIONS,
    )
    fmt_row("Rust single encode", rust_mean, rust_med, rust_min, rust_p99, 0)
    
    # Batch encode
    batch = [dict(mem, content=f"Memory #{i} for batch encoding") for i in range(100)]
    rust_mean, rust_med, rust_min, rust_p99, _ = timed(
        lambda: holographic_encode_batch(batch), iterations=500,
    )
    fmt_row("Rust batch [100 memories]", rust_mean, rust_med, rust_min, rust_p99, 0)


# ─────────────────────────────────────────────────────────────────────
# 5. RUST: MinHash Duplicate Detection
# ─────────────────────────────────────────────────────────────────────
def bench_rust_minhash():
    section("5. MINHASH DUPLICATE DETECTION — Rust PyO3")
    
    try:
        from whitemagic.optimization.rust_accelerators import (
            rust_v131_available, minhash_find_duplicates,
        )
        has_rust = rust_v131_available()
    except ImportError:
        has_rust = False
    
    if not has_rust:
        print("  SKIP — Rust accelerators not built")
        return
    
    # Generate keyword sets with some overlap
    all_words = [f"word_{i}" for i in range(200)]
    keyword_sets = []
    for i in range(50):
        base = random.sample(all_words, 20)
        keyword_sets.append(base)
    # Add some near-duplicates
    for i in range(10):
        dup = list(keyword_sets[i])
        dup[0] = f"different_{i}"
        keyword_sets.append(dup)
    
    rust_mean, rust_med, rust_min, rust_p99, _ = timed(
        lambda: minhash_find_duplicates(keyword_sets, threshold=0.7),
        iterations=ITERATIONS,
    )
    fmt_row(f"Rust MinHash [{len(keyword_sets)} sets]", rust_mean, rust_med, rust_min, rust_p99, 0)


# ─────────────────────────────────────────────────────────────────────
# 6. RUST: BM25 Search
# ─────────────────────────────────────────────────────────────────────
def bench_rust_bm25():
    section("6. BM25 SEARCH — Rust PyO3")
    
    try:
        from whitemagic.optimization.rust_accelerators import (
            rust_v131_available, search_build_index, search_query,
        )
        has_rust = rust_v131_available()
    except ImportError:
        has_rust = False
    
    if not has_rust:
        print("  SKIP — Rust accelerators not built")
        return
    
    # Build corpus
    topics = ["memory management", "galactic map", "holographic encoding",
              "dharma rules", "harmony vector", "tool dispatch", "mcp protocol",
              "agent coordination", "dream cycle", "constellation detection"]
    docs = []
    for i in range(500):
        topic = random.choice(topics)
        docs.append(f"Document {i} about {topic}. This discusses {topic} in detail with implementation notes.")
    
    # Build index
    search_build_index(docs)
    
    queries = ["memory management", "holographic encoding", "dharma harmony"]
    
    for q in queries:
        rust_mean, rust_med, rust_min, rust_p99, _ = timed(
            lambda query=q: search_query(query, 10), iterations=ITERATIONS,
        )
        fmt_row(f"Rust BM25 [{q[:25]}]", rust_mean, rust_med, rust_min, rust_p99, 0)


# ─────────────────────────────────────────────────────────────────────
# 7. RUST: Rate Limiter
# ─────────────────────────────────────────────────────────────────────
def bench_rust_rate_limiter():
    section("7. RATE LIMITER — Rust Atomic vs Python")
    
    try:
        from whitemagic.optimization.rust_accelerators import (
            rust_v131_available, rate_check,
        )
        has_rust = rust_v131_available()
    except ImportError:
        has_rust = False
    
    if not has_rust:
        print("  SKIP — Rust accelerators not built")
        return
    
    # Rust atomic rate limiter
    rust_mean, rust_med, rust_min, rust_p99, _ = timed(
        lambda: rate_check("benchmark_tool"),
        iterations=5000, warmup=100,
    )
    ops_per_sec = 1000.0 / rust_mean if rust_mean > 0 else 0
    fmt_row("Rust atomic rate check", rust_mean, rust_med, rust_min, rust_p99, 0)
    print(f"  → {ops_per_sec:,.0f} ops/sec")


# ─────────────────────────────────────────────────────────────────────
# 8. HASKELL: Dharma Rules Evaluation
# ─────────────────────────────────────────────────────────────────────
def bench_haskell_dharma():
    section("8. DHARMA RULES — Haskell FFI vs Python")
    
    try:
        from whitemagic.dharma.rules import DharmaRulesEngine
        engine = DharmaRulesEngine()
        # Check if Haskell is available by looking at the engine
        has_haskell = hasattr(engine, '_haskell_bridge') and engine._haskell_bridge is not None
    except Exception:
        has_haskell = False
    
    print(f"  Haskell available: {has_haskell}")
    
    # Test the Python evaluation path directly
    try:
        from whitemagic.dharma.rules import DharmaRulesEngine
        engine = DharmaRulesEngine()
        
        test_actions = [
            {"action": "create_memory", "content": "Test memory creation"},
            {"action": "delete_file", "path": "/etc/passwd"},
            {"action": "search_memories", "query": "important project notes"},
            {"action": "execute_shell", "command": "ls -la"},
        ]
        
        for action in test_actions:
            py_mean, py_med, py_min, py_p99, _ = timed(
                lambda a=action: engine.evaluate(a),
                iterations=ITERATIONS,
            )
            action_name = action.get("action", "unknown")[:30]
            fmt_row(f"Python Dharma [{action_name}]", py_mean, py_med, py_min, py_p99, 0)
    except Exception as e:
        print(f"  Dharma benchmark error: {e}")


# ─────────────────────────────────────────────────────────────────────
# Summary
# ─────────────────────────────────────────────────────────────────────
def print_summary():
    section("SUMMARY — Accelerator Availability")
    
    checks = []
    
    # Zig
    try:
        from whitemagic.core.acceleration.simd_cosine import simd_status
        s = simd_status()
        checks.append(("Zig SIMD (cosine)", s['has_zig_simd'], s.get('lane_width', 'N/A')))
    except Exception:
        checks.append(("Zig SIMD (cosine)", False, "import error"))
    
    try:
        from whitemagic.core.acceleration.simd_keywords import simd_keywords_status
        s = simd_keywords_status()
        checks.append(("Zig SIMD (keywords)", s['has_zig_simd'], s['backend']))
    except Exception:
        checks.append(("Zig SIMD (keywords)", False, "import error"))
    
    try:
        from whitemagic.core.acceleration.simd_distance import simd_distance_status
        s = simd_distance_status()
        checks.append(("Zig SIMD (distance)", s['has_zig_simd'], s['backend']))
    except Exception:
        checks.append(("Zig SIMD (distance)", False, "import error"))
    
    # Rust
    try:
        from whitemagic.optimization.rust_accelerators import rust_available, rust_v131_available
        checks.append(("Rust PyO3 (base)", rust_available(), ""))
        checks.append(("Rust PyO3 (v131)", rust_v131_available(), ""))
    except Exception:
        checks.append(("Rust PyO3", False, "import error"))
    
    # Haskell
    try:
        from whitemagic.core.acceleration.haskell_bridge import haskell_available
        checks.append(("Haskell FFI", haskell_available(), ""))
    except Exception:
        checks.append(("Haskell FFI", False, "not available"))
    
    for name, available, detail in checks:
        icon = "✅" if available else "❌"
        print(f"  {icon} {name:<30} {detail}")


def main():
    print("=" * 70)
    print("  WhiteMagic Polyglot Benchmark Suite")
    print(f"  Iterations: {ITERATIONS} (warmup: {WARMUP})")
    print("=" * 70)
    
    print_summary()
    
    bench_keyword_extraction()
    bench_cosine_similarity()
    bench_distance_matrix()
    bench_rust_holographic()
    bench_rust_minhash()
    bench_rust_bm25()
    bench_rust_rate_limiter()
    bench_haskell_dharma()
    
    print(f"\n{'='*70}")
    print("  BENCHMARK COMPLETE")
    print(f"{'='*70}")


if __name__ == "__main__":
    main()
