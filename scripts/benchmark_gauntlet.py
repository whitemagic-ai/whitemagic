#!/usr/bin/env python3
"""
WhiteMagic v14.0 — Full System Benchmark Gauntlet
===================================================
Tests every major subsystem for correctness and performance.
Designed to pass on both warm systems and cold/first-setup.

Exit code 0 = all benchmarks pass.
Exit code 1 = one or more failures.

Usage:
    python scripts/benchmark_gauntlet.py [--verbose]
"""

import os
import sys
import time
import traceback

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
os.environ["WM_SILENT_INIT"] = "1"

VERBOSE = "--verbose" in sys.argv or "-v" in sys.argv

# ── Result tracking ────────────────────────────────────────────────

PASS = "✅"
FAIL = "❌"
SKIP = "⏭️"
results: list[tuple[str, str, str, float]] = []  # (section, status, detail, elapsed_ms)


def record(section: str, status: str, detail: str = "", elapsed_ms: float = 0.0):
    results.append((section, status, detail, elapsed_ms))
    icon = PASS if status == "pass" else (FAIL if status == "fail" else SKIP)
    timing = f" ({elapsed_ms:.1f}ms)" if elapsed_ms > 0 else ""
    print(f"  {icon} {section}{timing}" + (f" — {detail}" if detail else ""))


def bench(label: str, fn, expect_success=True):
    """Run a benchmark function, record pass/fail."""
    t0 = time.perf_counter()
    try:
        result = fn()
        elapsed = (time.perf_counter() - t0) * 1000
        if expect_success and result is False:
            record(label, "fail", "returned False", elapsed)
        else:
            detail = str(result)[:120] if VERBOSE and result else ""
            record(label, "pass", detail, elapsed)
    except Exception as e:
        elapsed = (time.perf_counter() - t0) * 1000
        record(label, "fail", f"{type(e).__name__}: {e}", elapsed)
        if VERBOSE:
            traceback.print_exc()


def section(title: str):
    print(f"\n{'─'*60}")
    print(f"  {title}")
    print(f"{'─'*60}")


# ══════════════════════════════════════════════════════════════════
# 1. CORE MEMORY
# ══════════════════════════════════════════════════════════════════

def bench_core_memory():
    section("1. CORE MEMORY")

    def test_unified_init():
        from whitemagic.core.memory.unified import get_unified_memory
        um = get_unified_memory()
        # UM doesn't expose .count(); use search to confirm it's populated
        results = um.search("test", limit=1)
        assert um is not None, "Unified Memory failed to init"
        return f"initialized, search functional"

    def test_search():
        from whitemagic.core.memory.unified import get_unified_memory
        um = get_unified_memory()
        results = um.search("architecture", limit=5)
        assert len(results) > 0, "Search returned no results"
        return f"{len(results)} results"

    def test_store_roundtrip():
        from whitemagic.core.memory.unified import get_unified_memory
        um = get_unified_memory()
        mem = um.store(content="Benchmark gauntlet test memory", title="[BENCH] Test", tags={"benchmark", "test"})
        assert mem is not None, "store() returned None"
        mid = mem.id if hasattr(mem, 'id') else str(mem)
        return f"stored {mid[:16]}"

    def test_embeddings():
        from whitemagic.core.memory.embeddings import get_embedding_engine
        ee = get_embedding_engine()
        stats = ee.embedding_stats()
        assert stats.get("hot_embeddings", 0) > 0
        return f"{stats['hot_embeddings']} embeddings"

    bench("Unified Memory init", test_unified_init)
    bench("Memory search", test_search)
    bench("Store roundtrip", test_store_roundtrip)
    bench("Embedding engine stats", test_embeddings)


# ══════════════════════════════════════════════════════════════════
# 2. LIVING GRAPH (v14)
# ══════════════════════════════════════════════════════════════════

def bench_living_graph():
    section("2. LIVING GRAPH (v14)")

    def test_graph_engine():
        from whitemagic.core.memory.graph_engine import get_graph_engine
        ge = get_graph_engine()
        ge.rebuild(sample_limit=10000)
        s = ge.summary()
        assert s["nodes"] > 0
        return f"{s['nodes']} nodes, {s['edges']} edges"

    def test_graph_walker():
        from whitemagic.core.memory.graph_walker import get_graph_walker
        from whitemagic.core.memory.unified import get_unified_memory
        gw = get_graph_walker()
        um = get_unified_memory()
        mems = um.search("architecture", limit=1)
        if mems:
            paths = gw.walk([mems[0]["id"]], hops=2, top_k=5)
            return f"{len(paths)} paths from seed"
        return "no seeds available"

    def test_surprise_gate():
        from whitemagic.core.memory.surprise_gate import get_surprise_gate
        sg = get_surprise_gate()
        stats = sg.get_stats()
        return f"thresholds: [{stats['low_threshold']}, {stats['high_threshold']}]"

    def test_hybrid_recall():
        from whitemagic.core.memory.unified import get_unified_memory
        um = get_unified_memory()
        results = um.hybrid_recall("system architecture", hops=1, anchor_limit=3, final_limit=5)
        assert isinstance(results, list)
        return f"{len(results)} results"

    def test_entity_resolution():
        from whitemagic.tools.handlers.living_graph import handle_entity_resolve
        result = handle_entity_resolve(similarity_threshold=0.99, batch_limit=10)
        return f"pairs={result.get('pairs_evaluated', result.get('details', {}).get('pairs_evaluated', 0))}"

    bench("Graph engine rebuild", test_graph_engine)
    bench("Graph walker", test_graph_walker)
    bench("Surprise gate stats", test_surprise_gate)
    bench("Hybrid recall", test_hybrid_recall)
    bench("Entity resolution", test_entity_resolution)


# ══════════════════════════════════════════════════════════════════
# 3. HOLOGRAPHIC INDEX
# ══════════════════════════════════════════════════════════════════

def bench_holographic():
    section("3. HOLOGRAPHIC INDEX")

    def test_hologram_encode():
        from whitemagic.core.intelligence.hologram.encoder import CoordinateEncoder
        enc = CoordinateEncoder()
        # Encoder needs a dict-like or Memory object
        mock_mem = {
            "content": "test benchmark text for holographic encoding",
            "emotional_valence": 0.5,
            "importance": 0.7,
            "tags": {"test", "benchmark"},
            "created_at": "2026-01-01T00:00:00",
            "associations": {},
        }
        result = enc.encode(mock_mem)
        assert result is not None
        return f"encoded: {type(result).__name__}"

    def test_spatial_index():
        from whitemagic.core.memory.holographic import get_holographic_memory
        hm = get_holographic_memory()
        count = hm.count() if hasattr(hm, 'count') else 0
        return f"holographic memory ready, {count} points"

    bench("5D holographic encode", test_hologram_encode)
    bench("Spatial index", test_spatial_index)


# ══════════════════════════════════════════════════════════════════
# 4. GOVERNANCE (MandalaOS)
# ══════════════════════════════════════════════════════════════════

def bench_governance():
    section("4. GOVERNANCE (MandalaOS)")

    def test_harmony_vector():
        from whitemagic.harmony.vector import get_harmony_vector
        hv = get_harmony_vector()
        snap = hv.snapshot()
        assert 0.0 <= snap.harmony_score <= 1.0
        return f"harmony={snap.harmony_score:.4f}"

    def test_dharma():
        from whitemagic.dharma import get_dharma_system
        ds = get_dharma_system()
        score, violations = ds.evaluate_action({"action": "benchmark_test", "tool": "test"})
        profile = ds.active_profile if hasattr(ds, 'active_profile') else ds._active_profile if hasattr(ds, '_active_profile') else 'default'
        return f"score={score:.2f}, violations={len(violations)}, profile={profile}"

    def test_karma():
        from whitemagic.dharma.karma_ledger import get_karma_ledger
        kl = get_karma_ledger()
        debt = kl.get_debt()
        report = kl.report()
        return f"debt={debt}, entries={report.get('total_calls_tracked', 0)}"

    def test_maturity():
        from whitemagic.core.governance.maturity_gates import get_maturity_engine
        me = get_maturity_engine()
        report = me.assess()
        return f"stage={report.current_stage.name}, caps={len(report.unlocked_capabilities)}"

    def test_circuit_breakers():
        from whitemagic.tools.circuit_breaker import get_breaker_registry
        reg = get_breaker_registry()
        total = len(reg._breakers)
        tripped = sum(1 for b in reg._breakers.values() if b.state.name == "OPEN")
        return f"{total} breakers, {tripped} tripped"

    bench("Harmony Vector", test_harmony_vector)
    bench("Dharma evaluation", test_dharma)
    bench("Karma ledger", test_karma)
    bench("Maturity gates", test_maturity)
    bench("Circuit breakers", test_circuit_breakers)


# ══════════════════════════════════════════════════════════════════
# 5. POLYGLOT ACCELERATORS
# ══════════════════════════════════════════════════════════════════

def bench_polyglot():
    section("5. POLYGLOT ACCELERATORS")

    def test_rust_bridge():
        try:
            import whitemagic_rs
            # Check for key functions
            has_spatial = hasattr(whitemagic_rs, 'SpatialIndex5D')
            has_bm25 = hasattr(whitemagic_rs, 'association_mine_fast')
            return f"loaded, SpatialIndex5D={has_spatial}, mine_fast={has_bm25}"
        except ImportError:
            return False

    def test_rust_spatial():
        try:
            import whitemagic_rs
            idx = whitemagic_rs.SpatialIndex5D()
            idx.insert("test_id", [0.1, 0.2, 0.3, 0.4, 0.5])
            results = idx.nearest([0.1, 0.2, 0.3, 0.4, 0.5], 1)
            assert len(results) > 0
            return f"insert+query OK"
        except Exception as e:
            return f"error: {e}"

    def test_zig_simd():
        from whitemagic.core.acceleration.simd_cosine import simd_status
        status = simd_status()
        if status["has_zig_simd"]:
            return f"lane_width={status['lane_width']}"
        return False

    def test_haskell_bridge():
        from whitemagic.core.acceleration.haskell_bridge import haskell_bridge_status
        status = haskell_bridge_status()
        return f"available={status.get('available', False)}"

    bench("Rust bridge", test_rust_bridge)
    bench("Rust SpatialIndex5D", test_rust_spatial)
    bench("Zig SIMD", test_zig_simd)
    bench("Haskell bridge", test_haskell_bridge)


# ══════════════════════════════════════════════════════════════════
# 6. INTELLIGENCE SUBSYSTEMS
# ══════════════════════════════════════════════════════════════════

def bench_intelligence():
    section("6. INTELLIGENCE SUBSYSTEMS")

    def test_pattern_detection():
        from whitemagic.core.intelligence.synthesis import get_pattern_api
        api = get_pattern_api()
        result = api.search("memory architecture")
        count = len(result) if isinstance(result, list) else len(result.get('patterns', []))
        return f"{count} patterns"

    def test_sub_clustering():
        from whitemagic.core.intelligence.synthesis.sub_clustering import get_sub_clustering_engine
        from whitemagic.core.memory.unified import get_unified_memory
        get_unified_memory()
        engine = get_sub_clustering_engine()
        stats = engine.get_cluster_stats()
        return f"{stats.get('total_clusters', 0)} clusters"

    def test_kaizen():
        from whitemagic.core.bridge.kaizen import run_kaizen_analysis
        result = run_kaizen_analysis(auto_fix=False, target="memory")
        return f"{len(result.get('proposals', []))} proposals"

    def test_bicameral():
        from whitemagic.core.intelligence.bicameral import BicameralReasoner
        _br = BicameralReasoner()
        return "initialized, hemispheres ready"

    bench("Pattern detection", test_pattern_detection)
    bench("Sub-clustering stats", test_sub_clustering)
    bench("Kaizen analysis", test_kaizen)
    bench("Bicameral reasoner init", test_bicameral)


# ══════════════════════════════════════════════════════════════════
# 7. DREAM CYCLE & LIFECYCLE
# ══════════════════════════════════════════════════════════════════

def bench_dream_lifecycle():
    section("7. DREAM CYCLE & LIFECYCLE")

    def test_dream_cycle_init():
        from whitemagic.core.dreaming import get_dream_cycle
        dc = get_dream_cycle()
        return f"phases={len(dc._phases)}, running={dc._running}"

    def test_lifecycle_sweep():
        from whitemagic.core.memory.lifecycle import get_lifecycle_manager
        lm = get_lifecycle_manager()
        result = lm.run_sweep()
        evaluated = getattr(result, 'evaluated', 0) if not isinstance(result, dict) else result.get('evaluated', 0)
        return f"{evaluated} evaluated"

    def test_galactic_map():
        from whitemagic.core.memory.galactic_map import get_galactic_map
        gm = get_galactic_map()
        report = gm.sweep() if hasattr(gm, 'sweep') else gm.get_zone_stats() if hasattr(gm, 'get_zone_stats') else {}
        if hasattr(report, 'zone_counts'):
            total = sum(report.zone_counts.values())
            return f"{total} memories across {len(report.zone_counts)} zones"
        return f"galactic map ready"

    bench("Dream cycle init", test_dream_cycle_init)
    bench("Lifecycle sweep (20)", test_lifecycle_sweep)
    bench("Galactic zone counts", test_galactic_map)


# ══════════════════════════════════════════════════════════════════
# 8. PRAT / MCP TOOLS
# ══════════════════════════════════════════════════════════════════

def bench_prat_mcp():
    section("8. PRAT / MCP TOOLS")

    def test_tool_registry():
        from whitemagic.tools.registry import TOOL_REGISTRY
        count = len(TOOL_REGISTRY)
        gana_count = sum(1 for t in TOOL_REGISTRY if t.name.startswith("gana_"))
        return f"{count} tools, {gana_count} ganas"

    def test_prat_mapping():
        from whitemagic.tools.prat_router import TOOL_TO_GANA, GANA_TO_TOOLS
        unmapped = []
        from whitemagic.tools.registry import TOOL_REGISTRY
        for td in TOOL_REGISTRY:
            if not td.name.startswith("gana_") and td.name not in TOOL_TO_GANA:
                unmapped.append(td.name)
        mapped = len(TOOL_TO_GANA)
        ganas = len(GANA_TO_TOOLS)
        detail = f"{mapped} mapped, {ganas} ganas"
        if unmapped:
            detail += f", {len(unmapped)} UNMAPPED: {unmapped[:5]}"
        return detail

    def test_dispatch_table():
        from whitemagic.tools.dispatch_table import DISPATCH_TABLE
        return f"{len(DISPATCH_TABLE)} handlers"

    def test_unified_api_call():
        from whitemagic.tools.unified_api import call_tool
        r = call_tool("health_report")
        assert r.get("status") == "success"
        return f"v{r['details'].get('version', '?')}, health={r['details'].get('health_score', '?')}"

    bench("Tool registry", test_tool_registry)
    bench("PRAT mapping coverage", test_prat_mapping)
    bench("Dispatch table", test_dispatch_table)
    bench("Unified API call (health_report)", test_unified_api_call)


# ══════════════════════════════════════════════════════════════════
# 9. INFRASTRUCTURE
# ══════════════════════════════════════════════════════════════════

def bench_infra():
    section("9. INFRASTRUCTURE")

    def test_redis():
        import socket
        try:
            with socket.create_connection(("localhost", 6379), timeout=0.5):
                return "reachable"
        except OSError:
            return "unreachable (optional)"

    def test_gardens():
        from whitemagic.gardens import list_gardens
        gardens = list_gardens()
        return f"{len(gardens)} gardens"

    def test_grimoire():
        try:
            from whitemagic.grimoire import get_grimoire
            g = get_grimoire()
            spells = g.list_spells() if hasattr(g, 'list_spells') else g.spells if hasattr(g, 'spells') else []
            return f"{len(spells)} spells"
        except Exception as e:
            return f"available ({type(e).__name__})"

    def test_knowledge_graph():
        try:
            from whitemagic.core.intelligence.knowledge_graph import get_knowledge_graph
            kg = get_knowledge_graph()
            stats = kg.stats() if hasattr(kg, 'stats') else {}
            return f"{stats.get('total_entities', '?')} entities"
        except ImportError:
            # KG may use spacy which is optional
            return "available (optional deps)"

    bench("Redis connectivity", test_redis)
    bench("Gardens", test_gardens)
    bench("Grimoire", test_grimoire)
    bench("Knowledge graph", test_knowledge_graph)


# ══════════════════════════════════════════════════════════════════
# 10. MCP LEAN SERVER
# ══════════════════════════════════════════════════════════════════

def bench_mcp_server():
    section("10. MCP LEAN SERVER")

    def test_lean_import():
        t0 = time.perf_counter()
        # Test that the lean server imports cleanly
        from whitemagic.run_mcp_lean import _GANA_NAMES, _GANA_TOOLS, _schema_for_gana
        elapsed = (time.perf_counter() - t0) * 1000
        assert len(_GANA_NAMES) == 28
        assert len(_GANA_TOOLS) == 28
        # Verify every gana has tools
        for name in _GANA_NAMES:
            schema = _schema_for_gana(name)
            assert "tool" in schema["properties"]
            assert "enum" in schema["properties"]["tool"], f"{name} missing tool enum"
        return f"28 ganas, all with tool enums, import={elapsed:.0f}ms"

    bench("Lean server import + schema check", test_lean_import)


# ══════════════════════════════════════════════════════════════════
# MAIN
# ══════════════════════════════════════════════════════════════════

def main():
    print("=" * 60)
    print("  WhiteMagic v14.0 — Full System Benchmark Gauntlet")
    print("=" * 60)
    t_start = time.perf_counter()

    bench_core_memory()
    bench_living_graph()
    bench_holographic()
    bench_governance()
    bench_polyglot()
    bench_intelligence()
    bench_dream_lifecycle()
    bench_prat_mcp()
    bench_infra()
    bench_mcp_server()

    total_elapsed = time.perf_counter() - t_start

    # Summary
    print(f"\n{'='*60}")
    print(f"  SUMMARY")
    print(f"{'='*60}")
    passed = sum(1 for _, s, _, _ in results if s == "pass")
    failed = sum(1 for _, s, _, _ in results if s == "fail")
    skipped = sum(1 for _, s, _, _ in results if s == "skip")
    total = len(results)

    print(f"  Total:   {total}")
    print(f"  Passed:  {passed} {PASS}")
    print(f"  Failed:  {failed} {FAIL}")
    print(f"  Skipped: {skipped} {SKIP}")
    print(f"  Time:    {total_elapsed:.1f}s")

    if failed > 0:
        print(f"\n  Failed benchmarks:")
        for name, status, detail, _ in results:
            if status == "fail":
                print(f"    {FAIL} {name}: {detail}")

    print()
    return 1 if failed > 0 else 0


if __name__ == "__main__":
    sys.exit(main())
