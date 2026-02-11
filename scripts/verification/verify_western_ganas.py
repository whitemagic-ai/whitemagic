
import sys
import os
import asyncio
import json

# Add module to path
sys.path.insert(0, os.path.abspath('.'))

from whitemagic.mcp_api_bridge import (
    gana_straddling_legs, gana_mound, gana_stomach, gana_hairy_head,
    gana_net, gana_turtle_beak, gana_three_stars
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
    print("=== Verifying Western Quadrant Ganas ===")

    # 1. Straddling Legs (Balance)
    await run_test("Straddling Legs (Balance)", gana_straddling_legs, task="check_balance")

    # 2. Mound (Accumulation)
    await run_test("Mound (Cache)", gana_mound, task="check_storage")

    # 3. Stomach (Digestion)
    await run_test("Stomach (Energy)", gana_stomach, task="check_energy")

    # 4. Hairy Head (Validation)
    await run_test("Hairy Head (Debug)", gana_hairy_head, task="validate_integrations")

    # 5. Net (Capture)
    await run_test("Net (Pattern)", gana_net, task="detect_patterns")

    # 6. Turtle Beak (Precision)
    await run_test("Turtle Beak (Validate)", gana_turtle_beak, task="validate_command", command="ls -la")

    # 7. Three Stars (Judgment)
    await run_test("Three Stars (Wisdom)", gana_three_stars, task="consult_wisdom_council", question="Should we proceed to Phase 4?")

    print("\n=== Verification Complete ===")

if __name__ == "__main__":
    asyncio.run(main())
