
import asyncio
import json
import logging
import sys
from typing import Any, Dict, Type
from unittest.mock import MagicMock, patch

# Configure logging
logging.basicConfig(level=logging.ERROR)

# Import Gana base and Southern implementations
try:
    from whitemagic.core.ganas.base import GanaCall, BaseGana, LunarMansion
    from whitemagic.core.ganas.southern_quadrant import (
        GhostGana, WillowGana, StarGana, ExtendedNetGana, WingsGana, ChariotGana, AbundanceGana
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
    print("=== Verifying Southern Quadrant Ganas (Isolated) ===")
    
    # Mock global dependencies
    # Note: We must patch where the function is DEFINED or IMPORTED
    with patch('whitemagic.metrics.get_tracker') as MockTracker, \
         patch('whitemagic.intelligence.synthesis.kaizen_engine.get_kaizen_engine') as MockKaizen, \
         patch('whitemagic.cascade.context_synthesizer.get_context_synthesizer') as MockSynth, \
         patch('whitemagic.core.resonance.gan_ying.get_bus') as MockBus, \
         patch('whitemagic.intelligence.synthesis.unified_patterns.get_pattern_api') as MockPatterns, \
         patch('whitemagic.cascade.advanced_parallel.AdaptiveParallelExecutor') as MockPar, \
         patch('whitemagic.archaeology.find_unread') as MockArch, \
         patch('whitemagic.systems.token_economy.get_token_economy') as MockEcon:
        
        # 1. Ghost (Introspection)
        MockTracker.return_value.get_summary.return_value = {}
        await test_gana("Ghost", GhostGana, "get_metrics_summary", categories=['test'])
        
        # 2. Willow (Flexibility)
        await test_gana("Willow", WillowGana, "adapt")
        
        # 3. Star (Illumination)
        from dataclasses import dataclass
        @dataclass
        class MockContext:
            dominant_influence: str = 'sun'
            recommended_morphology: str = 'bright'
            def get_dominant_influence(self): return self.dominant_influence
            def get_recommended_morphology(self): return self.recommended_morphology

        MockSynth.return_value.gather.return_value = MockContext()
        await test_gana("Star", StarGana, "prat_get_context")
        
        # 4. Extended Net (Connection)
        MockPatterns.return_value.search.return_value = []
        await test_gana("ExtendedNet", ExtendedNetGana, "search_all_patterns", query="test")
        
        # 5. Wings (Expansion)
        MockPar.return_value.get_stats.return_value = {'workers': 4}
        await test_gana("Wings", WingsGana, "check_parallel_status")
        
        # 6. Chariot (Movement)
        MockArch.return_value = []
        await test_gana("Chariot", ChariotGana, "manage_archaeology", operation="scan")
        
        # 7. Abundance (Surplus)
        MockEcon.return_value.get_budget_status.return_value = {'tokens_remaining': 1000, 'total_budget': 1000, 'status': 'optimal'}
        await test_gana("Abundance", AbundanceGana, "check_surplus")

    print("\n=== Verification Complete ===")

if __name__ == "__main__":
    asyncio.run(main())
