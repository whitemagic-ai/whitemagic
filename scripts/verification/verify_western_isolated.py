
import sys
import os
import asyncio
import json

# Add module to path
sys.path.insert(0, os.path.abspath('.'))

from whitemagic.core.ganas.base import GanaCall
from whitemagic.core.ganas.western_quadrant import (
    StraddlingLegsGana, MoundGana, StomachGana, 
    HairyHeadGana, NetGana, TurtleBeakGana, ThreeStarsGana
)

async def test_gana(name, gana_class, task, **kwargs):
    print(f"\nTesting {name}...")
    try:
        gana = gana_class()
        call = GanaCall(task=task, state_vector=kwargs)
        result = await gana.invoke(call)
        print(f"✓ {name} Success")
        print(json.dumps({
            'mansion': result.mansion.name,
            'garden': result.garden,
            'result': result.output
        }, indent=2, default=str))
        return True
    except Exception as e:
        print(f"✗ {name} Failed: {e}")
        import traceback
        traceback.print_exc()
        return False

async def main():
    print("=== Verifying Western Quadrant Ganas (Isolated) ===", flush=True)
    
    # 1. Legs
    print(">> Testing Legs...", flush=True)
    await test_gana("Legs", StraddlingLegsGana, "check_balance")
    print("<< Legs Done", flush=True)
    
    # 2. Mound
    print(">> Testing Mound...", flush=True)
    await test_gana("Mound", MoundGana, "check_storage")
    print("<< Mound Done", flush=True)
    
    # 3. Stomach
    print(">> Testing Stomach...", flush=True)
    await test_gana("Stomach", StomachGana, "check_energy")
    print("<< Stomach Done", flush=True)
    
    # 4. Hairy Head
    print(">> Testing Hairy Head...", flush=True)
    await test_gana("Hairy Head", HairyHeadGana, "validate_integrations")
    print("<< Hairy Head Done", flush=True)
    
    # 5. Net
    print(">> Testing Net...", flush=True)
    from unittest.mock import MagicMock, patch
    
    # Mock the pattern API to avoid heavy DB calls and Rust issues
    mock_api = MagicMock()
    mock_api.get_stats.return_value = {"total_patterns": 42, "status": "mocked"}
    mock_api.search.return_value = []
    
    with patch('whitemagic.intelligence.synthesis.unified_patterns.get_pattern_api', return_value=mock_api):
        await test_gana("Net", NetGana, "detect_patterns")
    
    print("<< Net Done", flush=True)

    
    # 6. Turtle Beak
    print(">> Testing Turtle Beak...", flush=True)
    await test_gana("Turtle Beak", TurtleBeakGana, "validate_command", command="ls -la")
    print("<< Turtle Beak Done", flush=True)
    
    # 7. Three Stars
    print(">> Testing Three Stars...", flush=True)
    await test_gana("Three Stars", ThreeStarsGana, "consult_wisdom_council", question="Test question")
    print("<< Three Stars Done", flush=True)

    print("\n=== Verification Complete ===", flush=True)

if __name__ == "__main__":
    print("Starting main execution...", flush=True)
    try:
        asyncio.run(main())
        print("Main execution finished.", flush=True)
    except Exception as e:
        print(f"CRASHED: {e}", flush=True)

