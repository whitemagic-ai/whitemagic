
import unittest
import sys
import os
from datetime import datetime

# Path setup
sys.path.append(os.path.join(os.getcwd(), "staging/core_system"))

from whitemagic.gardens.metal.zodiac.zodiac_cores import get_zodiac_cores, ZodiacCore
from whitemagic.gardens.metal.zodiac.api import get_unified_zodiac, UnifiedZodiacSystem

class TestZodiacSystem(unittest.TestCase):
    
    def setUp(self):
        self.cores = get_zodiac_cores()
        self.unified = get_unified_zodiac()
        self.test_context = {
            "operation": "unit test execution",
            "intention": "verification",
            "urgency": "high"
        }

    def test_all_cores_exist(self):
        """Verify all 12 signs are present"""
        expected_signs = {
            "aries", "taurus", "gemini", "cancer", 
            "leo", "virgo", "libra", "scorpio", 
            "sagittarius", "capricorn", "aquarius", "pisces"
        }
        actual_signs = set(self.cores.get_all_cores().keys())
        self.assertEqual(expected_signs, actual_signs)

    def test_core_properties(self):
        """Verify element and mode assignments"""
        aries = self.cores.get_core("aries")
        self.assertEqual(aries.element, "fire")
        self.assertEqual(aries.mode, "cardinal")
        self.assertEqual(aries.ruler, "mars")
        
        taurus = self.cores.get_core("taurus")
        self.assertEqual(taurus.element, "earth")
        self.assertEqual(taurus.mode, "fixed")
        
        cancer = self.cores.get_core("cancer")
        self.assertEqual(cancer.element, "water")

    def test_core_activation(self):
        """Test activating a core returns a valid response"""
        response = self.cores.activate_core("aries", self.test_context)
        self.assertIsNotNone(response)
        self.assertEqual(response.core_name, "aries")
        self.assertIsInstance(response.resonance, float)
        self.assertIsInstance(response.wisdom, str)
        self.assertTrue(len(response.wisdom) > 0)

    def test_can_handle_logic(self):
        """Test the capability scoring logic"""
        aries = self.cores.get_core("aries") # Fire
        virgo = self.cores.get_core("virgo") # Earth (Detail)
        
        # Action-oriented task -> Aries should score higher
        action_context = {"operation": "start new project", "intention": "action", "urgency": "high"}
        aries_score = aries.can_handle(action_context)
        virgo_score = virgo.can_handle(action_context)
        self.assertTrue(aries_score > virgo_score, f"Aries ({aries_score}) should beat Virgo ({virgo_score}) for action")
        
        # Detail-oriented task -> Virgo should score higher
        detail_context = {"operation": "analyze detailed report", "intention": "optimize"}
        aries_score = aries.can_handle(detail_context)
        virgo_score = virgo.can_handle(detail_context)
        self.assertTrue(virgo_score > aries_score, f"Virgo ({virgo_score}) should beat Aries ({aries_score}) for details")

    def test_unified_api_perspective(self):
        """Test the Unified API facade"""
        perspective = self.unified.get_perspective("scorpio", self.test_context)
        self.assertEqual(perspective.sign, "scorpio")
        self.assertEqual(perspective.element, "water")
        self.assertIsNotNone(perspective.wisdom)
        
    def test_unified_api_trine(self):
        """Test activating an elemental trine (3 signs)"""
        fire_trine = self.unified.activate_trine("fire", self.test_context)
        self.assertEqual(len(fire_trine), 3)
        signs = {p.sign for p in fire_trine}
        self.assertEqual(signs, {"aries", "leo", "sagittarius"})

if __name__ == '__main__':
    unittest.main()
