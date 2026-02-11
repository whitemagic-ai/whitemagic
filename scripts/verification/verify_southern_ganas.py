
import sys
import os
import asyncio
import json

# Add module to path
sys.path.insert(0, os.path.abspath('.'))

from whitemagic.mcp_api_bridge import (
    gana_ghost, gana_willow, gana_star, gana_extended_net,
    gana_wings, gana_chariot, gana_abundance
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
    print("=== Verifying Southern Quadrant Ganas ===")

    # 1. Ghost (Introspection)
    await run_test("Ghost (Metric)", gana_ghost, task="track_metric", metric="test_metric", value=100.0)

    # 2. Willow (Flexibility)
    await run_test("Willow (Adapt)", gana_willow, task="adapt_ui")

    # 3. Star (Governance)
    await run_test("Star (Context)", gana_star, task="prat_get_context")

    # 4. Extended Net (Connection)
    await run_test("Extended Net (Monitor)", gana_extended_net, task="manage_resonance", operation="monitor")

    # 5. Wings (Expansion)
    await run_test("Wings (Parallel)", gana_wings, task="check_status")

    # 6. Chariot (Mobility)
    await run_test("Chariot (Scan)", gana_chariot, task="manage_archaeology", operation="scan", directory=".")

    # 7. Abundance (Surplus)
    await run_test("Abundance (Check)", gana_abundance, task="check_surplus")

    print("\n=== Verification Complete ===")

if __name__ == "__main__":
    asyncio.run(main())
