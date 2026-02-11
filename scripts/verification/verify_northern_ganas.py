
import sys
import os
import asyncio
import json

# Add module to path
sys.path.insert(0, os.path.abspath('.'))

from whitemagic.mcp_api_bridge import (
    gana_dipper, gana_ox, gana_girl, gana_void,
    gana_roof, gana_encampment, gana_wall
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
    print("=== Verifying Northern Quadrant Ganas ===")

    # 1. Dipper (Governance / Deep Search)
    await run_test("Dipper (Search)", gana_dipper, task="search_memories", query="test", limit=3)

    # 2. Ox (Endurance / Temporal)
    await run_test("Ox (Temporal)", gana_ox, task="get_system_time")

    # 3. Girl (Nurture / Profile)
    await run_test("Girl (Learn)", gana_girl, task="learn", action="learn", adaptation="session start", context="verification")

    # 4. Void (Emptiness / Kaizen)
    await run_test("Void (Kaizen)", gana_void, task="kaizen")

    # 5. Roof (Shelter / Zodiac)
    await run_test("Roof (Zodiac)", gana_roof, task="manage_zodiac")

    # 6. Encampment (Structure / Handoff)
    await run_test("Encampment (Structure)", gana_encampment, task="check_structure")

    # 7. Wall (Boundaries / Alerts)
    await run_test("Wall (Boundary)", gana_wall, task="check_boundary", message="Verification ping", priority="low")

    print("\n=== Verification Complete ===")

if __name__ == "__main__":
    asyncio.run(main())
