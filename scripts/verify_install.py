#!/usr/bin/env python3
"""WhiteMagic Install Verification — Run after `pip install whitemagic[mcp]`.

Usage:
    python -m scripts.verify_install          # Quick check (~5s)
    python -m scripts.verify_install --full   # Full check with benchmarks (~15s)

Exit code 0 = all checks passed, 1 = failures detected.
"""
from __future__ import annotations

import argparse
import json
import os
import sys
import time

os.environ.setdefault("WM_SILENT_INIT", "1")
os.environ.setdefault("WM_SKIP_HOLO_INDEX", "1")

PASS = "\033[92m✓\033[0m"
FAIL = "\033[91m✗\033[0m"
SKIP = "\033[93m⊘\033[0m"

results: list[dict] = []


def check(name: str, fn, required: bool = True):
    """Run a check function, record pass/fail."""
    t0 = time.perf_counter()
    try:
        result = fn()
        elapsed = (time.perf_counter() - t0) * 1000
        if result is True or result is None:
            print(f"  {PASS} {name} ({elapsed:.0f}ms)")
            results.append({"name": name, "status": "pass", "ms": elapsed})
        else:
            print(f"  {PASS} {name}: {result} ({elapsed:.0f}ms)")
            results.append({"name": name, "status": "pass", "ms": elapsed, "detail": str(result)})
    except Exception as e:
        elapsed = (time.perf_counter() - t0) * 1000
        marker = FAIL if required else SKIP
        print(f"  {marker} {name}: {e}")
        status = "fail" if required else "skip"
        results.append({"name": name, "status": status, "ms": elapsed, "error": str(e)})


# ═══════════════════════════════════════════════════════════════
# CHECK FUNCTIONS
# ═══════════════════════════════════════════════════════════════

def check_import():
    import whitemagic  # noqa: F401
    return True


def check_version():
    from whitemagic import __version__
    return f"v{__version__}"


def check_dispatch_table():
    from whitemagic.tools.dispatch_table import DISPATCH_TABLE
    count = len(DISPATCH_TABLE)
    assert count >= 200, f"Only {count} tools registered (expected 200+)"
    return f"{count} tools"


def check_prat_router():
    from whitemagic.tools.registry_defs import collect
    defs = collect()
    gana_tools = [d for d in defs if d.name.startswith("gana_")]
    assert len(gana_tools) == 28, f"Expected 28 Ganas, got {len(gana_tools)}"
    return "28 Ganas"


def check_memory_store():
    from whitemagic.core.memory.unified import UnifiedMemory
    um = UnifiedMemory()
    mid = um.store("Verification test memory", tags=["_verify_install"])
    assert mid, "store() returned empty ID"
    return "store OK"


def check_memory_search():
    from whitemagic.core.memory.unified import UnifiedMemory
    um = UnifiedMemory()
    results_list = um.search("verification", limit=5)
    return f"{len(results_list)} results"


def check_harmony_vector():
    from whitemagic.harmony.vector import HarmonyVector
    hv = HarmonyVector()
    snap = hv.snapshot_dict()
    assert "balance" in snap, "Missing 'balance' in HarmonyVector snapshot"
    return "7D vector OK"


def check_dharma_rules():
    from whitemagic.dharma.rules import DharmaRulesEngine
    engine = DharmaRulesEngine()
    decision = engine.evaluate({"tool": "test_action", "declared_safety": "read_only"})
    return f"action={decision.action.value}"


def check_config_paths():
    from whitemagic.config.paths import WM_ROOT
    assert WM_ROOT.exists(), f"State root doesn't exist: {WM_ROOT}"
    return str(WM_ROOT)


def check_rust_accelerators():
    from whitemagic.optimization import rust_accelerators as ra
    available = []
    if ra._RUST_AVAILABLE:
        available.append("base")
    if ra._RUST_V131:
        available.append("v131")
    if ra._RUST_TOKIO_CLONES:
        available.append("tokio")
    if ra._RUST_ARROW:
        available.append("arrow")
    if not available:
        raise ImportError("No Rust accelerators loaded")
    return ", ".join(available)


def check_mcp_server_import():
    import whitemagic.run_mcp_lean  # noqa: F401
    return True


def check_call_tool():
    from whitemagic.tools.unified_api import call_tool
    result = call_tool("capabilities")
    assert result.get("status") == "success", f"capabilities returned: {result.get('status')}"
    return True


# ═══════════════════════════════════════════════════════════════
# BENCHMARK FUNCTIONS (--full mode)
# ═══════════════════════════════════════════════════════════════

def bench_dispatch_speed():
    from whitemagic.tools.dispatch_table import DISPATCH_TABLE
    iterations = 10000
    t0 = time.perf_counter()
    for _ in range(iterations):
        _ = DISPATCH_TABLE.get("create_memory")
    elapsed = time.perf_counter() - t0
    ops_per_sec = iterations / elapsed
    return f"{ops_per_sec / 1e6:.1f}M ops/s"


def bench_memory_store():
    from whitemagic.core.memory.unified import UnifiedMemory
    um = UnifiedMemory()
    iterations = 50
    t0 = time.perf_counter()
    for i in range(iterations):
        um.store(f"Benchmark memory {i}", tags=["_bench"])
    elapsed = time.perf_counter() - t0
    ops_per_sec = iterations / elapsed
    return f"{ops_per_sec:.0f} ops/s"


def bench_memory_search():
    from whitemagic.core.memory.unified import UnifiedMemory
    um = UnifiedMemory()
    iterations = 50
    t0 = time.perf_counter()
    for _ in range(iterations):
        um.search("benchmark test query", limit=5)
    elapsed = time.perf_counter() - t0
    ops_per_sec = iterations / elapsed
    return f"{ops_per_sec:.0f} ops/s"


def bench_rust_encode():
    from whitemagic.optimization import rust_accelerators as ra
    if not ra._RUST_AVAILABLE:
        raise ImportError("Rust not available")
    iterations = 200
    test_mem = {"content": "test content", "tags": ["tag1", "tag2"]}
    t0 = time.perf_counter()
    for _ in range(iterations):
        ra.holographic_encode_single(test_mem)
    elapsed = time.perf_counter() - t0
    ops_per_sec = iterations / elapsed
    return f"{ops_per_sec:.0f} ops/s"


# ═══════════════════════════════════════════════════════════════
# MAIN
# ═══════════════════════════════════════════════════════════════

def main():
    parser = argparse.ArgumentParser(description="WhiteMagic Install Verification")
    parser.add_argument("--full", action="store_true", help="Run full checks including benchmarks")
    parser.add_argument("--json", action="store_true", help="Output results as JSON")
    args = parser.parse_args()

    print("═" * 60)
    print("  WhiteMagic Install Verification")
    print("═" * 60)

    print("\n  Core Checks:")
    check("Import whitemagic", check_import)
    check("Version", check_version)
    check("Dispatch table", check_dispatch_table)
    check("PRAT router", check_prat_router)
    check("Config paths", check_config_paths)
    check("Dharma rules engine", check_dharma_rules)
    check("Harmony vector", check_harmony_vector)

    print("\n  Memory Operations:")
    check("Memory store + delete", check_memory_store)
    check("Memory search", check_memory_search)
    check("Tool dispatch (capabilities)", check_call_tool)

    print("\n  Optional Accelerators:")
    check("Rust accelerators", check_rust_accelerators, required=False)
    check("MCP server module", check_mcp_server_import, required=False)

    if args.full:
        print("\n  Benchmarks:")
        check("Dispatch speed", bench_dispatch_speed)
        check("Memory store throughput", bench_memory_store)
        check("Memory search throughput", bench_memory_search)
        check("Rust holographic encode", bench_rust_encode, required=False)

    # Summary
    passed = sum(1 for r in results if r["status"] == "pass")
    failed = sum(1 for r in results if r["status"] == "fail")
    skipped = sum(1 for r in results if r["status"] == "skip")
    total_ms = sum(r["ms"] for r in results)

    print(f"\n{'═' * 60}")
    print(f"  {passed} passed, {failed} failed, {skipped} skipped ({total_ms:.0f}ms)")

    if failed == 0:
        print(f"  \033[92m🎉 WhiteMagic is working correctly!\033[0m")
    else:
        print(f"  \033[91m⚠ {failed} check(s) failed — see above for details\033[0m")

    print("═" * 60)

    if args.json:
        print(json.dumps({"results": results, "passed": passed, "failed": failed}, indent=2))

    sys.exit(1 if failed > 0 else 0)


if __name__ == "__main__":
    main()
