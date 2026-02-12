
import sys
import os
import asyncio
import json

# Add module to path
sys.path.insert(0, os.path.abspath('.'))

from whitemagic.mcp_api_bridge import (
    gana_horn, gana_neck, gana_root, gana_room, 
    gana_heart, gana_tail, gana_winnowing_basket
)

async def run_test(name, func, **kwargs):
    print(f"\nTesting {name}...")
    try:
        result = await func(**kwargs)
        print(f"✓ {name} Success")
        print(json.dumps(result, indent=2, default=str))
        return True
    except Exception as e:
        print(f"✗ {name} Failed: {e}")
        return False

async def main():
    print("=== Verifying Eastern Quadrant Ganas ===")

    # 1. Horn
    await run_test("Horn (Init)", gana_horn, task="session_init", session_name="Test Session")

    # 2. Neck
    await run_test("Neck (Memory)", gana_neck, task="memory_create", title="Test Memory", content="This is a test.")

    # 3. Root
    await run_test("Root (Health)", gana_root, task="check_system_health")

    # 4. Room
    await run_test("Room (Locks)", gana_room, task="manage_resource_locks", operation="acquire", resource_id="test_lock")

    # 5. Heart
    await run_test("Heart (Context)", gana_heart, task="session_get_context")

    # 6. Tail
    await run_test("Tail (Acceleration)", gana_tail, task="check_acceleration")

    # 7. Winnowing Basket
    await run_test("Winnowing Basket (Wisdom)", gana_winnowing_basket, task="extract_wisdom")

    print("\n=== Verification Complete ===")

if __name__ == "__main__":
    asyncio.run(main())
