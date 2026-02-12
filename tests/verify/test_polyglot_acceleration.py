"""
Verification Script: Phase 12 Polyglot Integration

Tests:
1. deep_search in GhostGana (Rust parallel_grep)
2. accelerated_task in TailGana (Rust/Mojo neuro-scoring)
3. Julia/Haskell/Zig bridge stubs
"""

import time
import json
from whitemagic.tools.unified_api import call_tool
from whitemagic.optimization.polyglot_router import get_router

def test_ghost_deep_search():
    print("\n--- Testing GhostGana Deep Search (Rust) ---")
    start = time.time()
    result = call_tool("gana_ghost", operation="search", task="deep_search", pattern="TODO", context_lines=1)
    duration = time.time() - start
    
    # print(json.dumps(result, indent=2))
    
    if result.get("status") == "success":
        details = result.get("details", {})
        output = details.get("output", {})
        count = output.get("match_count", 0)
        engine = output.get("matches", [{}])[0].get("engine", "unknown")
        print(f"Success: Found {count} matches in {duration:.4f}s using {engine}")
    else:
        print(f"Failed: {result.get('error')}")

def test_tail_acceleration():
    print("\n--- Testing TailGana Acceleration (Mojo/Rust) ---")
    payload = {
        "current_score": 0.5,
        "access_count": 10,
        "total_memories": 1000,
        "days_since_access": 5.0,
        "importance": 0.8
    }
    
    start = time.time()
    result = call_tool("gana_tail", operation="transform", task="accelerated_task", task_name="neuro_scoring", payload=payload)
    duration = time.time() - start
    
    if result.get("status") == "success":
        details = result.get("details", {})
        output = details.get("output", {})
        score = output.get("result", {}).get("score")
        print(f"Success: Neuro-score calculated: {score} in {duration:.4f}s")
    else:
        print(f"Failed: {result.get('error')}")

def test_bridge_stubs():
    print("\n--- Testing Bridge Stubs (Julia, Haskell, Zig) ---")
    from whitemagic.core.bridge.julia import get_julia_bridge
    from whitemagic.core.bridge.haskell import get_haskell_bridge
    from whitemagic.core.bridge.zig import get_zig_bridge
    
    julia = get_julia_bridge().resolve_resonance({"a": 0.5})
    haskell = get_haskell_bridge().calculate_balance([0.1, 0.9])
    zig = get_zig_bridge().process_genome([1, 0, 1])
    
    print(f"Julia: {julia['status']}")
    print(f"Haskell: {haskell['status']}")
    print(f"Zig: {zig['status']}")

if __name__ == "__main__":
    print("=== POLYGLOT INTEGRATION VERIFICATION ===")
    test_ghost_deep_search()
    test_tail_acceleration()
    test_bridge_stubs()
    
    router = get_router()
    stats = router.get_stats()
    print("\n=== POLYGLOT ROUTER STATS ===")
    print(json.dumps(stats, indent=2))
