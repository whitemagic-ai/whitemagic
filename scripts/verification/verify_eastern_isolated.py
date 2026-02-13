
import asyncio
import json
import logging
import sys
from typing import Any, Dict, Type
from unittest.mock import MagicMock, patch

# Configure logging
logging.basicConfig(level=logging.ERROR)

# Import Gana base and Eastern implementations
try:
    from whitemagic.core.ganas.base import GanaCall, BaseGana, LunarMansion
    from whitemagic.core.ganas.eastern_quadrant import (
        HornGana, NeckGana, RootGana, RoomGana, HeartGana, TailGana, WinnowingBasketGana
    )
except ImportError as e:
    print(f"ImportError: {e}")
    sys.exit(1)

async def test_gana(name: str, gana_class: Type[BaseGana], task: str, **kwargs: Any):
    print(f"\nTesting {name}...")
    try:
        gana = gana_class()
        call = GanaCall(task=task, state_vector=kwargs)
        
        # Invoke
        result = await gana.invoke(call)
        
        # Check result
        print(f"✓ {name} Success")
        print(json.dumps({
            "mansion": result.mansion.name,
            "garden": result.garden,
            "result": result.output
        }, indent=2, default=str))
        
    except Exception as e:
        print(f"✗ {name} Failed: {e}")
        import traceback
        traceback.print_exc()

async def main():
    print("=== Verifying Eastern Quadrant Ganas (Isolated) ===")
    
    # Mock global dependencies with CORRECT paths
    with patch('whitemagic.core.memory.manager.MemoryManager') as MockMemMan, \
         patch('whitemagic.core.embodiment.get_harmony_monitor') as MockHealth, \
         patch('whitemagic.gardens.sangha.resources.get_resources') as MockLocker, \
         patch('whitemagic.optimization.polyglot_router.get_router') as MockRouter:
        
        # 1. Horn (Init)
        await test_gana("Horn", HornGana, "session_init")
        
        # 2. Neck (Memory)
        await test_gana("Neck", NeckGana, "memory_create", title="Test", content="Content")
        
        # 3. Root (Health)
        MockHealth.return_value.get_current.return_value = {'is_harmonious': True, 'harmony_score': 0.9}
        await test_gana("Root", RootGana, "check_system_health", deep_scan=True)
        
        # 4. Room (Locks)
        MockLocker.return_value.acquire_lock.return_value = True
        await test_gana("Room", RoomGana, "acquire_lock", resource_id="test")
        
        # 5. Heart (Context)
        await test_gana("Heart", HeartGana, "session_get_context")
        
        # 6. Tail (Acceleration)
        MockRouter.return_value.get_stats.return_value = {'rust_available': True}
        await test_gana("Tail", TailGana, "check_acceleration")
        
        # 7. Winnowing Basket (Wisdom)
        MockMemMan.return_value.consolidate.return_value = 5
        await test_gana("WinnowingBasket", WinnowingBasketGana, "consolidate_memories")

    print("\n=== Verification Complete ===")

if __name__ == "__main__":
    asyncio.run(main())
