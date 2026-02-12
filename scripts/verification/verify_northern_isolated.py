
import asyncio
import json
import logging
import sys
from typing import Any, Dict, Type
from unittest.mock import MagicMock, patch

# Configure logging
logging.basicConfig(level=logging.ERROR)

# Import Gana base and Northern implementations
try:
    from whitemagic.core.ganas.base import GanaCall, BaseGana, LunarMansion
    from whitemagic.core.ganas.northern_quadrant import (
        DipperGana, OxGana, GirlGana, VoidGana, RoofGana, EncampmentGana, WallGana
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
    print("=== Verifying Northern Quadrant Ganas (Isolated) ===")
    
    # Mock heavy dependencies globally for this session
    # We mock MemoryManager, SerendipityEngine, HealthMonitor, UserManager, etc.
    
    with patch('whitemagic.core.memory.manager.MemoryManager') as MockMemMan, \
         patch('whitemagic.intelligence.synthesis.serendipity_engine.get_serendipity_engine') as MockSerendipity, \
         patch('whitemagic.systems.health_monitor.HealthMonitor') as MockHealth, \
         patch('whitemagic.systems.user.get_user_manager') as MockUser, \
         patch('whitemagic.intelligence.synthesis.kaizen_engine.get_kaizen_engine') as MockKaizen, \
         patch('whitemagic.gardens.sangha.chat.get_chat') as MockChat, \
         patch('whitemagic.gardens.sangha.session_handoff.get_handoff') as MockHandoff, \
         patch('whitemagic.zodiac.zodiac_cores.get_zodiac_cores') as MockCores, \
         patch('whitemagic.core.temporal.get_temporal_context') as MockTemporal:
        
        # Setup Mocks
        MockMemMan.return_value.search_memories.return_value = [{'title': 'Mock Memory', 'relevance': 0.9}]
        MockSerendipity.return_value.surface.return_value = []
        MockHealth.return_value.check_system_health.return_value = {'status': 'healthy'}
        MockUser.return_value.profile.name = "Test User"
        MockKaizen.return_value.analyze.return_value.proposals = []
        MockKaizen.return_value.analyze.return_value.by_category = {}
        MockCores.return_value.get_core.return_value.activation_count = 1
        MockTemporal.return_value.to_dict.return_value = {'timestamp': 12345}
    
        # 1. Dipper (Dou) - Governance/Search
        # Uses MemoryManager (mocked)
        await test_gana("Dipper", DipperGana, "search_memories", query="test")
        
        # 2. Ox (Niu) - Endurance/Time
        await test_gana("Ox", OxGana, "get_system_time")
        
        # 3. Girl (Nu) - Nurture/Profile
        await test_gana("Girl", GirlGana, "learn", adaptation="Testing nurture")
        
        # 4. Void (Xu) - Emptiness/Optimization
        await test_gana("Void", VoidGana, "clear_cache")
        
        # 5. Roof (Wei) - Shelter/Zodiac
        await test_gana("Roof", RoofGana, "manage_zodiac")
        
        # 6. Encampment (Shi) - Structure/Handoff
        await test_gana("Encampment", EncampmentGana, "session_handoff", session_id="mock_session")
        
        # 7. Wall (Bi) - Boundaries/Alerts
        await test_gana("Wall", WallGana, "send_alert", message="Test alert")
    
    print("\n=== Verification Complete ===")

if __name__ == "__main__":
    asyncio.run(main())
