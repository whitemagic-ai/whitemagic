#!/usr/bin/env python3
"""
WhiteMagic MCP Benchmark Suite
================================
Tests MCP server features added in v14.1:
- Server instructions & metadata
- Tool icons and execution modes
- Resource listing and reading
- Tool call latency (serial + parallel)
- HTTP transport startup
- Schema correctness
"""

import asyncio
import json
import sys
import time
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

PASSED = 0
FAILED = 0
RESULTS: list[tuple[str, bool, float, str]] = []


def bench(label: str, fn, section_results=None):
    global PASSED, FAILED
    start = time.perf_counter()
    try:
        result = fn()
        elapsed = (time.perf_counter() - start) * 1000
        if result is False:
            print(f"  \u23ed\ufe0f  {label} (skipped)")
            return
        msg = str(result) if result else "OK"
        print(f"  \u2705 {label} ({elapsed:.1f}ms) \u2014 {msg}")
        PASSED += 1
        RESULTS.append((label, True, elapsed, msg))
    except Exception as e:
        elapsed = (time.perf_counter() - start) * 1000
        print(f"  \u274c {label} ({elapsed:.1f}ms) \u2014 {type(e).__name__}: {e}")
        FAILED += 1
        RESULTS.append((label, False, elapsed, str(e)))


def section(title):
    print(f"\u2500" * 60)
    print(f"  {title}")
    print(f"\u2500" * 60)


# ══════════════════════════════════════════════════════════════════
# 1. SERVER METADATA & INSTRUCTIONS
# ══════════════════════════════════════════════════════════════════

def bench_metadata():
    section("1. SERVER METADATA & INSTRUCTIONS")

    def test_server_creation():
        from whitemagic.run_mcp_lean import server, _VERSION
        assert server.name == "WhiteMagic Core"
        assert server.version == _VERSION
        assert server.website_url is not None
        return f"v{server.version}, name={server.name}"

    def test_instructions_loaded():
        from whitemagic.run_mcp_lean import server
        assert server.instructions is not None
        assert len(server.instructions) > 1000
        assert "gana_winnowing_basket" in server.instructions
        return f"{len(server.instructions)} chars, has tool guidance"

    def test_init_options():
        from whitemagic.run_mcp_lean import server
        opts = server.create_initialization_options()
        assert opts.instructions is not None
        assert opts.server_name == "WhiteMagic Core"
        return f"server_name={opts.server_name}, has_instructions=True"

    bench("Server creation & metadata", test_server_creation)
    bench("Instructions loaded", test_instructions_loaded)
    bench("Initialization options", test_init_options)


# ══════════════════════════════════════════════════════════════════
# 2. TOOL SCHEMA — ICONS, EXECUTION MODES, ENUMS
# ══════════════════════════════════════════════════════════════════

def bench_schema():
    section("2. TOOL SCHEMA — ICONS, EXECUTION, ENUMS")

    def test_tool_count():
        from whitemagic.run_mcp_lean import list_tools
        tools = asyncio.run(list_tools())
        assert len(tools) == 28
        return f"{len(tools)} Gana tools"

    def test_all_have_icons():
        from whitemagic.run_mcp_lean import list_tools
        tools = asyncio.run(list_tools())
        with_icons = [t for t in tools if t.icons]
        assert len(with_icons) == 28, f"Only {len(with_icons)}/28 have icons"
        # Check icon format
        sample = with_icons[0].icons[0]
        assert sample.src.startswith("data:image/svg+xml,")
        assert sample.mimeType == "image/svg+xml"
        return f"28/28 with SVG data-URI icons"

    def test_task_optional_tools():
        from whitemagic.run_mcp_lean import list_tools, _SLOW_GANAS
        import mcp.types as types
        tools = asyncio.run(list_tools())
        task_tools = [t for t in tools if t.execution and t.execution.mode == types.TASK_OPTIONAL]
        assert len(task_tools) == len(_SLOW_GANAS)
        names = sorted(t.name for t in task_tools)
        return f"{len(task_tools)} task-optional: {', '.join(names)}"

    def test_per_gana_enums():
        from whitemagic.run_mcp_lean import list_tools
        tools = asyncio.run(list_tools())
        total_nested = 0
        for t in tools:
            props = t.inputSchema.get("properties", {})
            tool_prop = props.get("tool", {})
            enum = tool_prop.get("enum", [])
            total_nested += len(enum)
        assert total_nested >= 150, f"Only {total_nested} nested tools"
        return f"{total_nested} nested tool enums across 28 Ganas"

    def test_schema_structure():
        from whitemagic.run_mcp_lean import list_tools
        tools = asyncio.run(list_tools())
        for t in tools:
            props = t.inputSchema.get("properties", {})
            assert "tool" in props, f"{t.name} missing 'tool' property"
            assert "args" in props, f"{t.name} missing 'args' property"
            assert "operation" in props, f"{t.name} missing 'operation' property"
            ops = props["operation"].get("enum", [])
            assert set(ops) == {"search", "analyze", "transform", "consolidate"}
        return "all 28 have tool/args/operation with correct operation enum"

    bench("Tool count (28 Ganas)", test_tool_count)
    bench("Icons on all tools", test_all_have_icons)
    bench("Task-optional execution modes", test_task_optional_tools)
    bench("Per-Gana nested tool enums", test_per_gana_enums)
    bench("Schema structure validation", test_schema_structure)


# ══════════════════════════════════════════════════════════════════
# 3. RESOURCES
# ══════════════════════════════════════════════════════════════════

def bench_resources():
    section("3. RESOURCES")

    def test_list_resources():
        from whitemagic.run_mcp_lean import list_resources
        resources = asyncio.run(list_resources())
        assert len(resources) == 3
        uris = [str(r.uri) for r in resources]
        assert any("ai-primary" in u for u in uris)
        assert any("server-instructions" in u for u in uris)
        assert any("system-map" in u for u in uris)
        return f"{len(resources)} resources: ai-primary, server-instructions, system-map"

    def test_read_instructions():
        from whitemagic.run_mcp_lean import read_resource
        content = asyncio.run(read_resource("whitemagic://orientation/server-instructions"))
        assert len(content) > 1000
        assert "Quick Start" in content
        return f"{len(content)} chars"

    def test_read_ai_primary():
        from whitemagic.run_mcp_lean import read_resource
        content = asyncio.run(read_resource("whitemagic://orientation/ai-primary"))
        assert len(content) > 100
        assert "Unavailable" not in content or "WhiteMagic" in content
        return f"{len(content)} chars"

    bench("List resources (3)", test_list_resources)
    bench("Read server-instructions", test_read_instructions)
    bench("Read ai-primary", test_read_ai_primary)


# ══════════════════════════════════════════════════════════════════
# 4. TOOL CALL LATENCY — SERIAL
# ══════════════════════════════════════════════════════════════════

def bench_serial_calls():
    section("4. TOOL CALL LATENCY — SERIAL")

    def test_health_report():
        from whitemagic.run_mcp_lean import _sync_dispatch
        result = _sync_dispatch("gana_root", "health_report", {}, None)
        assert result.get("status") != "error", result.get("error", "")
        return f"status={result.get('status')}"

    def test_search_memories():
        from whitemagic.run_mcp_lean import _sync_dispatch
        result = _sync_dispatch("gana_winnowing_basket", "search_memories", {"query": "test", "limit": 3}, None)
        assert result.get("status") != "error", result.get("error", "")
        return f"status={result.get('status')}"

    def test_gnosis():
        from whitemagic.run_mcp_lean import _sync_dispatch
        result = _sync_dispatch("gana_ghost", "gnosis", {}, None)
        assert result.get("status") != "error", result.get("error", "")
        return f"status={result.get('status')}"

    def test_harmony_vector():
        from whitemagic.run_mcp_lean import _sync_dispatch
        result = _sync_dispatch("gana_straddling_legs", "harmony_vector", {}, None)
        assert result.get("status") != "error", result.get("error", "")
        return f"status={result.get('status')}"

    def test_yin_yang():
        from whitemagic.run_mcp_lean import _sync_dispatch
        result = _sync_dispatch("gana_mound", "get_yin_yang_balance", {}, None)
        assert result.get("status") != "error", result.get("error", "")
        return f"status={result.get('status')}"

    bench("health_report (cold start)", test_health_report)
    bench("search_memories", test_search_memories)
    bench("gnosis", test_gnosis)
    bench("harmony_vector", test_harmony_vector)
    bench("yin_yang_balance", test_yin_yang)


# ══════════════════════════════════════════════════════════════════
# 5. PARALLEL TOOL CALLS
# ══════════════════════════════════════════════════════════════════

def bench_parallel_calls():
    section("5. PARALLEL TOOL CALLS")

    def test_parallel_3():
        from whitemagic.run_mcp_lean import _sync_dispatch
        calls = [
            ("gana_root", "health_report", {}, None),
            ("gana_straddling_legs", "harmony_vector", {}, None),
            ("gana_mound", "get_yin_yang_balance", {}, None),
        ]
        start = time.perf_counter()
        with ThreadPoolExecutor(max_workers=3) as pool:
            futures = [pool.submit(_sync_dispatch, *c) for c in calls]
            results = [f.result() for f in futures]
        elapsed = (time.perf_counter() - start) * 1000
        errors = [r for r in results if r.get("status") == "error"]
        assert len(errors) == 0, f"{len(errors)} errors: {[r.get('error') for r in errors]}"
        return f"3 calls in {elapsed:.0f}ms, all success"

    def test_parallel_5():
        from whitemagic.run_mcp_lean import _sync_dispatch
        calls = [
            ("gana_root", "health_report", {}, None),
            ("gana_winnowing_basket", "search_memories", {"query": "architecture", "limit": 2}, None),
            ("gana_straddling_legs", "harmony_vector", {}, None),
            ("gana_ghost", "gnosis", {}, None),
            ("gana_mound", "get_yin_yang_balance", {}, None),
        ]
        start = time.perf_counter()
        with ThreadPoolExecutor(max_workers=5) as pool:
            futures = [pool.submit(_sync_dispatch, *c) for c in calls]
            results = [f.result() for f in futures]
        elapsed = (time.perf_counter() - start) * 1000
        errors = [r for r in results if r.get("status") == "error"]
        assert len(errors) == 0, f"{len(errors)} errors: {[r.get('error') for r in errors]}"
        return f"5 calls in {elapsed:.0f}ms, all success"

    def test_parallel_10():
        from whitemagic.run_mcp_lean import _sync_dispatch
        calls = [
            ("gana_root", "health_report", {}, None),
            ("gana_root", "state.summary", {}, None),
            ("gana_winnowing_basket", "search_memories", {"query": "test", "limit": 1}, None),
            ("gana_straddling_legs", "harmony_vector", {}, None),
            ("gana_ghost", "gnosis", {}, None),
            ("gana_mound", "get_yin_yang_balance", {}, None),
            ("gana_willow", "grimoire_suggest", {}, None),
            ("gana_star", "governor_check_drift", {}, None),
            ("gana_dipper", "maturity.assess", {}, None),
            ("gana_extended_net", "cluster_stats", {}, None),
        ]
        start = time.perf_counter()
        with ThreadPoolExecutor(max_workers=10) as pool:
            futures = [pool.submit(_sync_dispatch, *c) for c in calls]
            results = [f.result() for f in futures]
        elapsed = (time.perf_counter() - start) * 1000
        successes = sum(1 for r in results if r.get("status") != "error")
        errors = [r.get("error") for r in results if r.get("status") == "error"]
        return f"10 calls in {elapsed:.0f}ms, {successes}/10 success" + (f", errors: {errors}" if errors else "")

    def test_serial_vs_parallel():
        from whitemagic.run_mcp_lean import _sync_dispatch
        calls = [
            ("gana_root", "health_report", {}, None),
            ("gana_straddling_legs", "harmony_vector", {}, None),
            ("gana_mound", "get_yin_yang_balance", {}, None),
        ]
        # Serial
        start = time.perf_counter()
        for c in calls:
            _sync_dispatch(*c)
        serial_ms = (time.perf_counter() - start) * 1000

        # Parallel
        start = time.perf_counter()
        with ThreadPoolExecutor(max_workers=3) as pool:
            futures = [pool.submit(_sync_dispatch, *c) for c in calls]
            [f.result() for f in futures]
        parallel_ms = (time.perf_counter() - start) * 1000

        speedup = serial_ms / parallel_ms if parallel_ms > 0 else 1.0
        return f"serial={serial_ms:.0f}ms, parallel={parallel_ms:.0f}ms, speedup={speedup:.1f}x"

    bench("3 parallel calls", test_parallel_3)
    bench("5 parallel calls", test_parallel_5)
    bench("10 parallel calls", test_parallel_10)
    bench("Serial vs parallel (3 calls)", test_serial_vs_parallel)


# ══════════════════════════════════════════════════════════════════
# 6. HTTP TRANSPORT
# ══════════════════════════════════════════════════════════════════

def bench_http():
    section("6. HTTP TRANSPORT")

    def test_http_import():
        from mcp.server.streamable_http import StreamableHTTPServerTransport
        from starlette.applications import Starlette
        import uvicorn
        return "StreamableHTTPServerTransport + Starlette + Uvicorn available"

    def test_http_transport_creation():
        from mcp.server.streamable_http import StreamableHTTPServerTransport
        import uuid
        transport = StreamableHTTPServerTransport(
            mcp_session_id=str(uuid.uuid4()),
            is_json_response_enabled=True,
        )
        assert transport is not None
        assert hasattr(transport, "_read_stream") or hasattr(transport, "read_stream")
        assert hasattr(transport, "_write_stream") or hasattr(transport, "write_stream")
        return "transport created with read/write streams"

    bench("HTTP transport dependencies", test_http_import)
    bench("HTTP transport creation", test_http_transport_creation)


# ══════════════════════════════════════════════════════════════════
# SUMMARY
# ══════════════════════════════════════════════════════════════════

def main():
    print("=" * 60)
    print("  WhiteMagic MCP Benchmark Suite")
    print("=" * 60)

    bench_metadata()
    bench_schema()
    bench_resources()
    bench_serial_calls()
    bench_parallel_calls()
    bench_http()

    print("=" * 60)
    print("  SUMMARY")
    print("=" * 60)
    total = PASSED + FAILED
    print(f"  Total:   {total}")
    print(f"  Passed:  {PASSED} \u2705")
    print(f"  Failed:  {FAILED} \u274c")

    # Latency summary for serial calls
    serial_results = [(l, t) for l, ok, t, _ in RESULTS if ok and "SERIAL" not in l and t > 10]
    if serial_results:
        avg = sum(t for _, t in serial_results) / len(serial_results)
        print(f"  Avg tool call: {avg:.0f}ms")

    if FAILED:
        print(f"\n  Failed benchmarks:")
        for label, ok, _, msg in RESULTS:
            if not ok:
                print(f"    \u274c {label}: {msg}")

    return 0 if FAILED == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
