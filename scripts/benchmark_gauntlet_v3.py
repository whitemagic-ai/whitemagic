#!/usr/bin/env python3
"""WhiteMagic Benchmark Gauntlet v3 — Arrow IPC, Iceoryx2 & Tokio Clone Army
=============================================================================
Tests the three new v14.5+ acceleration cores:

  Tier 1: Core baselines (memory store/recall, dispatch, harmony)
  Tier 2: Arrow IPC (export/import vs JSON, bulk serialization)
  Tier 3: Tokio Clone Army (parallel clone deployment, batch inference)
  Tier 4: Iceoryx2 IPC (zero-copy publish/subscribe, bridge init)
  Tier 5: Holographic encoding (Rust 5D encoder vs Python fallback)

Usage:
    python scripts/benchmark_gauntlet_v3.py [--tier 1|2|3|4|5|all] [--verbose]
"""

import asyncio
import json
import os
import statistics
import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

os.environ.setdefault("WM_STATE_ROOT", str(Path.home() / ".whitemagic"))
os.environ.setdefault("WM_SILENT_INIT", "1")

# ═══════════════════════════════════════════════════════════════════
# Utilities
# ═══════════════════════════════════════════════════════════════════

RESULTS: list[dict] = []

def _fmt_time(ns: float) -> str:
    if ns < 1_000:
        return f"{ns:.0f}ns"
    elif ns < 1_000_000:
        return f"{ns / 1_000:.1f}µs"
    elif ns < 1_000_000_000:
        return f"{ns / 1_000_000:.2f}ms"
    else:
        return f"{ns / 1_000_000_000:.3f}s"

def _fmt_rate(ops_per_sec: float) -> str:
    if ops_per_sec >= 1_000_000:
        return f"{ops_per_sec / 1_000_000:.2f}M ops/s"
    elif ops_per_sec >= 1_000:
        return f"{ops_per_sec / 1_000:.1f}K ops/s"
    else:
        return f"{ops_per_sec:.0f} ops/s"

def bench(label: str, fn, iterations: int = 1000, warmup: int = 10, tier: int = 1):
    """Run a benchmark, collecting timing stats."""
    for _ in range(warmup):
        try:
            fn()
        except Exception:
            break

    times_ns = []
    errors = 0
    for _ in range(iterations):
        start = time.perf_counter_ns()
        try:
            fn()
        except Exception:
            errors += 1
            continue
        elapsed = time.perf_counter_ns() - start
        times_ns.append(elapsed)

    if not times_ns:
        print(f"  ✗ {label}: ALL ERRORS ({errors}/{iterations})")
        RESULTS.append({"label": label, "tier": tier, "status": "error", "errors": errors})
        return

    median = statistics.median(times_ns)
    p95 = sorted(times_ns)[int(len(times_ns) * 0.95)]
    ops = len(times_ns) / (sum(times_ns) / 1e9) if sum(times_ns) > 0 else 0

    status = "✓" if errors == 0 else f"⚠ {errors} errs"
    print(f"  {status} {label}: median={_fmt_time(median)}  p95={_fmt_time(p95)}  {_fmt_rate(ops)}  ({len(times_ns)} iters)")

    RESULTS.append({
        "label": label, "tier": tier, "status": "pass",
        "median_ns": median, "p95_ns": p95, "ops_per_sec": ops,
        "iterations": len(times_ns), "errors": errors,
    })

def bench_async(label: str, coro_fn, iterations: int = 100, warmup: int = 5, tier: int = 1):
    """Benchmark an async function."""
    async def _run():
        for _ in range(warmup):
            try:
                await coro_fn()
            except Exception:
                break

        times_ns = []
        errors = 0
        for _ in range(iterations):
            start = time.perf_counter_ns()
            try:
                await coro_fn()
            except Exception:
                errors += 1
                continue
            elapsed = time.perf_counter_ns() - start
            times_ns.append(elapsed)

        if not times_ns:
            print(f"  ✗ {label}: ALL ERRORS ({errors}/{iterations})")
            RESULTS.append({"label": label, "tier": tier, "status": "error", "errors": errors})
            return

        median = statistics.median(times_ns)
        p95 = sorted(times_ns)[int(len(times_ns) * 0.95)]
        ops = len(times_ns) / (sum(times_ns) / 1e9) if sum(times_ns) > 0 else 0

        status = "✓" if errors == 0 else f"⚠ {errors} errs"
        print(f"  {status} {label}: median={_fmt_time(median)}  p95={_fmt_time(p95)}  {_fmt_rate(ops)}  ({len(times_ns)} iters)")

        RESULTS.append({
            "label": label, "tier": tier, "status": "pass",
            "median_ns": median, "p95_ns": p95, "ops_per_sec": ops,
            "iterations": len(times_ns), "errors": errors,
        })

    asyncio.get_event_loop().run_until_complete(_run())


# ═══════════════════════════════════════════════════════════════════
# TIER 1: Core Baselines
# ═══════════════════════════════════════════════════════════════════

def tier1_core():
    print("\n═══ TIER 1: Core Baselines ═══")

    # Memory store
    from whitemagic.core.memory.unified import UnifiedMemory, MemoryType
    um = UnifiedMemory()

    bench("Memory store (SHORT_TERM)", lambda: um.store(
        content="Benchmark test memory " + str(time.time()),
        memory_type=MemoryType.SHORT_TERM,
        title="bench_t1",
        importance=0.5,
    ), iterations=200, tier=1)

    # Memory search
    bench("Memory search (top-5)", lambda: um.search("benchmark test", limit=5),
          iterations=200, tier=1)

    # Dispatch table lookup
    from whitemagic.tools.dispatch_table import DISPATCH_TABLE
    tools = list(DISPATCH_TABLE.keys())
    bench("Dispatch table lookup", lambda: DISPATCH_TABLE.get(tools[0]),
          iterations=10000, tier=1)

    # Harmony vector
    try:
        from whitemagic.harmony import get_harmony_vector
        hv = get_harmony_vector()
        bench("Harmony vector snapshot", lambda: hv.snapshot(), iterations=5000, tier=1)
    except Exception as e:
        print(f"  ⊘ Harmony vector: {e}")


# ═══════════════════════════════════════════════════════════════════
# TIER 2: Arrow IPC
# ═══════════════════════════════════════════════════════════════════

def tier2_arrow():
    print("\n═══ TIER 2: Arrow IPC ═══")

    from whitemagic.core.memory.unified import UnifiedMemory
    um = UnifiedMemory()

    # Arrow export
    arrow_bytes = None
    def _arrow_export():
        nonlocal arrow_bytes
        arrow_bytes = um.arrow_export(limit=100)

    bench("Arrow IPC export (100 memories)", _arrow_export, iterations=100, tier=2)

    if arrow_bytes:
        print(f"    → Arrow payload: {len(arrow_bytes):,} bytes")

        # Arrow import (into same DB, will deduplicate)
        bench("Arrow IPC import (100 memories)", lambda: um.arrow_import(arrow_bytes),
              iterations=50, tier=2)

    # Compare: JSON export baseline
    import json as _json
    def _json_export():
        memories = um.search(query=None, limit=100)
        return _json.dumps([{"id": m.id, "title": m.title, "content": str(m.content)[:1000]} for m in memories])

    bench("JSON export baseline (100 memories)", _json_export, iterations=100, tier=2)

    # Rust Arrow encode/decode directly
    try:
        from whitemagic.optimization.rust_accelerators import (
            arrow_available, arrow_encode_memories, arrow_decode_memories,
        )
        if arrow_available():
            sample = _json.dumps([{
                "id": f"bench_{i}", "title": f"Test {i}", "content": f"Content {i}",
                "importance": 0.5, "memory_type": "SHORT_TERM",
                "x": 0.0, "y": 0.0, "z": 0.0, "w": 0.5, "v": 0.5,
                "tags": ["bench"],
            } for i in range(100)])

            bench("Rust arrow_encode (100 docs)", lambda: arrow_encode_memories(sample),
                  iterations=500, tier=2)

            ipc = arrow_encode_memories(sample)
            if ipc:
                print(f"    → Rust Arrow IPC: {len(ipc):,} bytes")
                bench("Rust arrow_decode (100 docs)", lambda: arrow_decode_memories(ipc),
                      iterations=500, tier=2)
        else:
            print("  ⊘ Rust Arrow bridge not available")
    except ImportError:
        print("  ⊘ Rust accelerators not installed")


# ═══════════════════════════════════════════════════════════════════
# TIER 3: Tokio Clone Army
# ═══════════════════════════════════════════════════════════════════

def tier3_tokio():
    print("\n═══ TIER 3: Tokio Clone Army ═══")

    try:
        from whitemagic.optimization.rust_accelerators import (
            tokio_deploy_clones, tokio_clone_bench, tokio_clone_stats,
        )

        # Clone deployment benchmark
        bench("Tokio deploy 10 clones", lambda: tokio_deploy_clones("benchmark consciousness", 10),
              iterations=100, tier=3)

        bench("Tokio deploy 100 clones", lambda: tokio_deploy_clones("benchmark consciousness", 100),
              iterations=50, tier=3)

        bench("Tokio deploy 1000 clones", lambda: tokio_deploy_clones("benchmark consciousness", 1000),
              iterations=20, tier=3)

        # Tokio internal benchmark
        result = tokio_clone_bench(100)
        print(f"    → Tokio self-bench (100 clones): {result}")

        # Stats
        stats = tokio_clone_stats()
        print(f"    → Clone army stats: {stats}")

    except ImportError:
        print("  ⊘ Tokio Clone Army not available (Rust accelerators not installed)")
    except Exception as e:
        print(f"  ⊘ Tokio Clone Army error: {e}")

    # Python fallback comparison
    try:
        from whitemagic.core.bridge.reasoning import deploy_thought_clones

        bench_async("Python async clones (10)", lambda: deploy_thought_clones(
            query="What is consciousness?",
            strategies=["analytical", "creative", "skeptical", "integrative", "historical"],
            num_clones=10,
            use_tokio=False,
        ), iterations=20, tier=3)

        bench_async("Tokio-backed clones (10)", lambda: deploy_thought_clones(
            query="What is consciousness?",
            strategies=["analytical", "creative", "skeptical", "integrative", "historical"],
            num_clones=10,
            use_tokio=True,
        ), iterations=20, tier=3)

    except Exception as e:
        print(f"  ⊘ Thought clones benchmark: {e}")


# ═══════════════════════════════════════════════════════════════════
# TIER 4: Iceoryx2 IPC
# ═══════════════════════════════════════════════════════════════════

def tier4_iceoryx():
    print("\n═══ TIER 4: Iceoryx2 IPC ═══")

    try:
        from whitemagic.optimization.rust_accelerators import (
            ipc_bridge_init, ipc_bridge_publish, ipc_bridge_status,
        )
        # Verify the bridge is actually available
        if ipc_bridge_status() is None:
            raise ImportError("IPC bridge compiled but not initialized")

        # Init
        bench("Iceoryx2 bridge init", lambda: ipc_bridge_init(), iterations=50, tier=4)

        # Publish
        bench("Iceoryx2 publish (small)", lambda: ipc_bridge_publish("bench", "hello"),
              iterations=500, tier=4)

        bench("Iceoryx2 publish (1KB)", lambda: ipc_bridge_publish("bench", "x" * 1024),
              iterations=500, tier=4)

        bench("Iceoryx2 publish (10KB)", lambda: ipc_bridge_publish("bench", "x" * 10240),
              iterations=200, tier=4)

        # Status
        status = ipc_bridge_status()
        print(f"    → IPC bridge status: {status}")

    except ImportError:
        print("  ⊘ Iceoryx2 IPC not available (Rust accelerators not installed)")
    except Exception as e:
        print(f"  ⊘ Iceoryx2 IPC error: {e}")


# ═══════════════════════════════════════════════════════════════════
# TIER 5: Holographic Encoding
# ═══════════════════════════════════════════════════════════════════

def tier5_holographic():
    print("\n═══ TIER 5: Holographic 5D Encoding ═══")

    try:
        import whitemagic_rs
        if hasattr(whitemagic_rs, "holographic_encode_5d"):
            bench("Rust 5D encode (single)", lambda: whitemagic_rs.holographic_encode_5d(
                "Test content for holographic encoding", 0.7, "LONG_TERM", "test,bench",
            ), iterations=5000, tier=5)

            # Batch encoding
            bench("Rust 5D encode (batch 100)", lambda: [
                whitemagic_rs.holographic_encode_5d(
                    f"Content {i}", 0.5 + i * 0.005, "SHORT_TERM", f"tag{i}",
                ) for i in range(100)
            ], iterations=50, tier=5)
        else:
            print("  ⊘ holographic_encode_5d not in Rust bridge")
    except ImportError:
        print("  ⊘ Rust bridge not available")

    # Python fallback
    try:
        from whitemagic.core.memory.holographic import HolographicIndex
        hi = HolographicIndex()
        bench("Python 5D encode (single)", lambda: hi.encode(
            content="Test content for holographic encoding",
            importance=0.7,
            memory_type="LONG_TERM",
            tags={"test", "bench"},
        ), iterations=2000, tier=5)
    except Exception as e:
        print(f"  ⊘ Python holographic: {e}")

    # BM25 search
    try:
        import whitemagic_rs
        if hasattr(whitemagic_rs, "bm25_search"):
            bench("Rust BM25 search", lambda: whitemagic_rs.bm25_search("consciousness memory", 10),
                  iterations=1000, tier=5)
    except Exception:
        pass


# ═══════════════════════════════════════════════════════════════════
# Summary & Comparison
# ═══════════════════════════════════════════════════════════════════

def print_summary():
    print("\n" + "═" * 70)
    print("  BENCHMARK GAUNTLET v3 — SUMMARY")
    print("═" * 70)

    passed = sum(1 for r in RESULTS if r["status"] == "pass")
    failed = sum(1 for r in RESULTS if r["status"] == "error")
    print(f"\n  Total: {len(RESULTS)} benchmarks | {passed} passed | {failed} failed")

    # Find key comparisons
    arrow_export = next((r for r in RESULTS if "Arrow IPC export" in r["label"]), None)
    json_export = next((r for r in RESULTS if "JSON export" in r["label"]), None)
    if arrow_export and json_export and arrow_export.get("median_ns") and json_export.get("median_ns"):
        speedup = json_export["median_ns"] / arrow_export["median_ns"]
        print(f"\n  Arrow vs JSON export: {speedup:.1f}× speedup")

    tokio_10 = next((r for r in RESULTS if "Tokio deploy 10" in r["label"]), None)
    tokio_100 = next((r for r in RESULTS if "Tokio deploy 100" in r["label"]), None)
    tokio_1000 = next((r for r in RESULTS if "Tokio deploy 1000" in r["label"]), None)
    if tokio_10 and tokio_10.get("median_ns"):
        print("\n  Tokio Clone Army:")
        print(f"    10 clones:   {_fmt_time(tokio_10['median_ns'])}")
        if tokio_100 and tokio_100.get("median_ns"):
            print(f"    100 clones:  {_fmt_time(tokio_100['median_ns'])}")
        if tokio_1000 and tokio_1000.get("median_ns"):
            print(f"    1000 clones: {_fmt_time(tokio_1000['median_ns'])}")

    rust_enc = next((r for r in RESULTS if "Rust 5D encode (single)" in r["label"]), None)
    py_enc = next((r for r in RESULTS if "Python 5D encode (single)" in r["label"]), None)
    if rust_enc and py_enc and rust_enc.get("median_ns") and py_enc.get("median_ns"):
        speedup = py_enc["median_ns"] / rust_enc["median_ns"]
        print(f"\n  Rust vs Python 5D encode: {speedup:.1f}× speedup")

    # Save results
    out_path = Path(__file__).parent / "benchmark_v3_results.json"
    with open(out_path, "w") as f:
        json.dump({
            "timestamp": time.strftime("%Y-%m-%dT%H:%M:%S"),
            "results": RESULTS,
        }, f, indent=2)
    print(f"\n  Results saved to: {out_path}")
    print("═" * 70)


# ═══════════════════════════════════════════════════════════════════
# Main
# ═══════════════════════════════════════════════════════════════════

def main():
    import argparse
    parser = argparse.ArgumentParser(description="WhiteMagic Benchmark Gauntlet v3")
    parser.add_argument("--tier", default="all", help="Tier to run: 1|2|3|4|5|all")
    parser.add_argument("--verbose", action="store_true")
    args = parser.parse_args()

    print("═" * 70)
    print("  WhiteMagic Benchmark Gauntlet v3")
    print("  Arrow IPC • Tokio Clone Army • Iceoryx2 IPC")
    print("═" * 70)

    tier = args.tier.lower()
    start = time.time()

    if tier in ("1", "all"):
        tier1_core()
    if tier in ("2", "all"):
        tier2_arrow()
    if tier in ("3", "all"):
        tier3_tokio()
    if tier in ("4", "all"):
        tier4_iceoryx()
    if tier in ("5", "all"):
        tier5_holographic()

    elapsed = time.time() - start
    print(f"\n  Total time: {elapsed:.1f}s")
    print_summary()


if __name__ == "__main__":
    main()
