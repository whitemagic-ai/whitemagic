"""
Tests for Cross-System Fusions.

Tests:
- Self-Model → Dream: proactive dreaming on energy trough
- Wu Xing → Gana Quadrant Boost: elemental amplification
- PRAT Resonance → Emotion/Drive: mood modulation by quadrant
- Fusion status introspection
"""

import unittest
from unittest.mock import patch, MagicMock

from whitemagic.core.fusions import (
    check_proactive_dream,
    get_wuxing_quadrant_boost,
    modulate_drive_from_resonance,
    get_fusion_status,
    _ELEMENT_TO_QUADRANT,
    _QUADRANT_TO_ELEMENT,
)


class TestWuXingGanaBoost(unittest.TestCase):
    """Test Wu Xing → Gana Quadrant Boost fusion."""

    def test_element_quadrant_mapping_complete(self):
        self.assertEqual(len(_ELEMENT_TO_QUADRANT), 5)
        self.assertIn("wood", _ELEMENT_TO_QUADRANT)
        self.assertIn("fire", _ELEMENT_TO_QUADRANT)
        self.assertIn("metal", _ELEMENT_TO_QUADRANT)
        self.assertIn("water", _ELEMENT_TO_QUADRANT)
        self.assertIn("earth", _ELEMENT_TO_QUADRANT)

    def test_earth_has_no_quadrant(self):
        self.assertIsNone(_ELEMENT_TO_QUADRANT["earth"])

    def test_reverse_mapping(self):
        self.assertEqual(_QUADRANT_TO_ELEMENT["East"], "wood")
        self.assertEqual(_QUADRANT_TO_ELEMENT["South"], "fire")
        self.assertEqual(_QUADRANT_TO_ELEMENT["West"], "metal")
        self.assertEqual(_QUADRANT_TO_ELEMENT["North"], "water")

    @patch("whitemagic.core.fusions._get_dominant_element")
    def test_boost_when_aligned(self, mock_element):
        mock_element.return_value = ("wood", 0.8)
        result = get_wuxing_quadrant_boost("gana_horn")  # East quadrant
        self.assertTrue(result["boosted"])
        self.assertGreater(result["boost_factor"], 1.0)

    @patch("whitemagic.core.fusions._get_dominant_element")
    def test_no_boost_when_misaligned(self, mock_element):
        mock_element.return_value = ("water", 0.7)  # North
        result = get_wuxing_quadrant_boost("gana_horn")  # East
        self.assertFalse(result["boosted"])
        self.assertEqual(result["boost_factor"], 1.0)

    @patch("whitemagic.core.fusions._get_dominant_element")
    def test_boost_factor_scales_with_energy(self, mock_element):
        mock_element.return_value = ("fire", 1.0)  # max energy
        result = get_wuxing_quadrant_boost("gana_ghost")  # South
        self.assertEqual(result["boost_factor"], 1.5)  # 1.0 + 1.0*0.5

    @patch("whitemagic.core.fusions._get_dominant_element")
    def test_earth_element_no_boost(self, mock_element):
        mock_element.return_value = ("earth", 0.9)
        result = get_wuxing_quadrant_boost("gana_horn")
        self.assertFalse(result["boosted"])

    @patch("whitemagic.core.fusions._get_dominant_element")
    def test_overcoming_penalty(self, mock_element):
        # Wood overcomes Earth; if dominant is Wood and gana is Earth-mapped...
        # Actually the overcoming cycle checks if dominant element's target
        # matches the quadrant's element. Let's test:
        # Metal overcomes Wood. So if Metal is dominant, East (Wood) is penalized.
        mock_element.return_value = ("metal", 0.8)
        result = get_wuxing_quadrant_boost("gana_horn")  # East = Wood
        self.assertTrue(result["penalized"])
        self.assertLess(result["boost_factor"], 1.0)

    def test_unknown_gana_safe_fallback(self):
        result = get_wuxing_quadrant_boost("gana_nonexistent")
        self.assertIn("boost_factor", result)


class TestSelfModelDream(unittest.TestCase):
    """Test Self-Model → Dream fusion."""

    @patch("whitemagic.core.intelligence.self_model.get_self_model")
    def test_no_data_returns_not_triggered(self, mock_model_fn):
        model = MagicMock()
        model.forecast.return_value = None
        mock_model_fn.return_value = model
        result = check_proactive_dream()
        self.assertFalse(result["triggered"])
        self.assertIn("insufficient", result["reason"])

    @patch("whitemagic.core.intelligence.self_model.get_self_model")
    def test_stable_energy_not_triggered(self, mock_model_fn):
        model = MagicMock()
        forecast = MagicMock()
        forecast.current = 0.8
        forecast.predicted = 0.75
        forecast.trend = "stable"
        forecast.slope = -0.001
        forecast.alert = None
        forecast.threshold_eta = None
        model.forecast.return_value = forecast
        mock_model_fn.return_value = model
        result = check_proactive_dream()
        self.assertFalse(result["triggered"])
        self.assertEqual(result["energy_trend"], "stable")

    @patch("whitemagic.core.fusions._emit_fusion_event")
    @patch("whitemagic.core.dreaming.dream_cycle.get_dream_cycle")
    @patch("whitemagic.core.intelligence.self_model.get_self_model")
    def test_falling_energy_triggers_dream(self, mock_model_fn, mock_dc_fn, mock_emit):
        model = MagicMock()
        forecast = MagicMock()
        forecast.current = 0.35
        forecast.predicted = 0.12
        forecast.trend = "falling"
        forecast.slope = -0.023
        forecast.alert = "energy predicted to hit critical low"
        forecast.threshold_eta = 10
        model.forecast.return_value = forecast
        mock_model_fn.return_value = model

        dc = MagicMock()
        dc._dreaming = False
        dc._running = True
        mock_dc_fn.return_value = dc

        result = check_proactive_dream()
        self.assertTrue(result["triggered"])
        self.assertIn("dream_phase", result)


class TestResonanceDriveModulation(unittest.TestCase):
    """Test PRAT Resonance → Emotion/Drive fusion."""

    def setUp(self):
        import whitemagic.tools.prat_resonance as mod
        from whitemagic.tools.prat_resonance import PratResonanceState
        mod._state = PratResonanceState()

    @patch("whitemagic.core.intelligence.emotion_drive.get_drive_core")
    def test_east_quadrant_boosts_curiosity(self, mock_drive_fn):
        drive = MagicMock()
        mock_drive_fn.return_value = drive
        result = modulate_drive_from_resonance("gana_horn")  # East
        self.assertEqual(result["drive_modulated"], "curiosity")
        self.assertEqual(result["quadrant"], "East")
        drive.process_event.assert_called_once()

    @patch("whitemagic.core.intelligence.emotion_drive.get_drive_core")
    def test_south_quadrant_boosts_satisfaction(self, mock_drive_fn):
        drive = MagicMock()
        mock_drive_fn.return_value = drive
        result = modulate_drive_from_resonance("gana_ghost")  # South
        self.assertEqual(result["drive_modulated"], "satisfaction")

    @patch("whitemagic.core.intelligence.emotion_drive.get_drive_core")
    def test_mood_deepening_same_quadrant(self, mock_drive_fn):
        drive = MagicMock()
        mock_drive_fn.return_value = drive

        # Record a predecessor in the same quadrant (East)
        from whitemagic.tools.prat_resonance import ResonanceSnapshot, get_resonance_state
        state = get_resonance_state()
        state.record(ResonanceSnapshot(
            gana_name="gana_horn", tool_name=None, operation="search",
            output_preview="test",
        ))

        result = modulate_drive_from_resonance("gana_neck")  # Also East
        self.assertEqual(result["mood_amplifier"], 1.5)

    @patch("whitemagic.core.intelligence.emotion_drive.get_drive_core")
    def test_no_mood_deepening_different_quadrant(self, mock_drive_fn):
        drive = MagicMock()
        mock_drive_fn.return_value = drive

        from whitemagic.tools.prat_resonance import ResonanceSnapshot, get_resonance_state
        state = get_resonance_state()
        state.record(ResonanceSnapshot(
            gana_name="gana_horn", tool_name=None, operation="search",
            output_preview="test",
        ))

        result = modulate_drive_from_resonance("gana_ghost")  # South, not East
        self.assertEqual(result["mood_amplifier"], 1.0)


class TestFusionStatus(unittest.TestCase):
    """Test fusion status introspection."""

    def test_all_fusions_reported(self):
        status = get_fusion_status()
        self.assertIn("selfmodel_dream", status)
        self.assertIn("wuxing_gana_boost", status)
        self.assertIn("resonance_emotion", status)
        self.assertIn("depgraph_pipeline", status)

    def test_all_fusions_wired(self):
        status = get_fusion_status()
        for name, info in status.items():
            self.assertTrue(info["wired"], f"{name} should be wired")


if __name__ == "__main__":
    unittest.main()
