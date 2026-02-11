"""
Tests for PRAT Resonance — session-level Gana resonance state.

Tests:
- ResonanceSnapshot creation and serialization
- PratResonanceState singleton behavior
- Predecessor/successor lookup in 28-mansion circle
- build_resonance_context() with and without predecessor
- record_resonance() captures output preview
- Resonance metadata injection into PRAT responses
- get_resonance_summary() for Gnosis portal
- Lunar alignment detection
- Guna mode hints (minimal/optimal/normal)
"""

import time
import unittest
from unittest.mock import patch, MagicMock

from whitemagic.tools.prat_resonance import (
    ResonanceSnapshot,
    PratResonanceState,
    get_resonance_state,
    build_resonance_context,
    record_resonance,
    get_resonance_summary,
    _get_predecessor_gana,
    _get_successor_gana,
    _get_meta,
    _GANA_META,
    _GANA_ORDER,
    _GANA_INDEX,
)


class TestGanaMetadata(unittest.TestCase):
    """Test Gana metadata lookups."""

    def test_all_28_ganas_have_metadata(self):
        self.assertEqual(len(_GANA_META), 28)

    def test_all_28_ganas_in_order(self):
        self.assertEqual(len(_GANA_ORDER), 28)

    def test_index_matches_order(self):
        for i, name in enumerate(_GANA_ORDER):
            self.assertEqual(_GANA_INDEX[name], i)

    def test_get_meta_known(self):
        meta = _get_meta("gana_horn")
        self.assertEqual(meta["mansion_num"], 1)
        self.assertEqual(meta["quadrant"], "East")
        self.assertEqual(meta["meaning"], "Sharp initiation")
        self.assertEqual(meta["garden"], "Courage")
        self.assertEqual(meta["chinese"], "角")
        self.assertEqual(meta["pinyin"], "Jiao")

    def test_get_meta_unknown(self):
        meta = _get_meta("gana_nonexistent")
        self.assertEqual(meta["mansion_num"], 0)
        self.assertEqual(meta["quadrant"], "Unknown")

    def test_quadrants_correct(self):
        east = [g for g in _GANA_ORDER[:7]]
        south = [g for g in _GANA_ORDER[7:14]]
        west = [g for g in _GANA_ORDER[14:21]]
        north = [g for g in _GANA_ORDER[21:28]]
        for g in east:
            self.assertEqual(_get_meta(g)["quadrant"], "East")
        for g in south:
            self.assertEqual(_get_meta(g)["quadrant"], "South")
        for g in west:
            self.assertEqual(_get_meta(g)["quadrant"], "West")
        for g in north:
            self.assertEqual(_get_meta(g)["quadrant"], "North")


class TestPredecessorSuccessor(unittest.TestCase):
    """Test circular 28-mansion predecessor/successor lookups."""

    def test_horn_predecessor_is_wall(self):
        self.assertEqual(_get_predecessor_gana("gana_horn"), "gana_wall")

    def test_horn_successor_is_neck(self):
        self.assertEqual(_get_successor_gana("gana_horn"), "gana_neck")

    def test_wall_successor_is_horn(self):
        self.assertEqual(_get_successor_gana("gana_wall"), "gana_horn")

    def test_full_circle(self):
        """Walking 28 successors from horn returns to horn."""
        current = "gana_horn"
        for _ in range(28):
            current = _get_successor_gana(current)
        self.assertEqual(current, "gana_horn")

    def test_full_circle_predecessor(self):
        """Walking 28 predecessors from horn returns to horn."""
        current = "gana_horn"
        for _ in range(28):
            current = _get_predecessor_gana(current)
        self.assertEqual(current, "gana_horn")


class TestResonanceSnapshot(unittest.TestCase):
    """Test ResonanceSnapshot dataclass."""

    def test_creation(self):
        snap = ResonanceSnapshot(
            gana_name="gana_ghost",
            tool_name="gnosis",
            operation=None,
            output_preview="status=ok",
            timestamp=time.time(),
        )
        self.assertEqual(snap.gana_name, "gana_ghost")
        self.assertEqual(snap.tool_name, "gnosis")

    def test_to_dict(self):
        snap = ResonanceSnapshot(
            gana_name="gana_heart",
            tool_name=None,
            operation="search",
            output_preview="test",
        )
        d = snap.to_dict()
        self.assertIsInstance(d, dict)
        self.assertEqual(d["gana_name"], "gana_heart")
        self.assertIn("lunar_phase", d)


class TestPratResonanceState(unittest.TestCase):
    """Test PratResonanceState state management."""

    def setUp(self):
        self.state = PratResonanceState()

    def test_initial_state_empty(self):
        self.assertIsNone(self.state.get_predecessor())
        self.assertEqual(self.state.call_count, 0)
        self.assertEqual(self.state.get_gana_counts(), {})

    def test_record_and_retrieve(self):
        snap = ResonanceSnapshot(
            gana_name="gana_ghost",
            tool_name="gnosis",
            operation=None,
            output_preview="status=ok",
        )
        self.state.record(snap)
        self.assertEqual(self.state.call_count, 1)
        pred = self.state.get_predecessor()
        self.assertIsNotNone(pred)
        self.assertEqual(pred.gana_name, "gana_ghost")

    def test_multiple_records(self):
        for i, name in enumerate(["gana_horn", "gana_heart", "gana_ghost"]):
            self.state.record(ResonanceSnapshot(
                gana_name=name, tool_name=None, operation="search",
                output_preview=f"call {i}",
            ))
        self.assertEqual(self.state.call_count, 3)
        self.assertEqual(self.state.get_predecessor().gana_name, "gana_ghost")

    def test_gana_counts(self):
        self.state.record(ResonanceSnapshot(
            gana_name="gana_horn", tool_name=None, operation=None, output_preview="",
        ))
        self.state.record(ResonanceSnapshot(
            gana_name="gana_horn", tool_name=None, operation=None, output_preview="",
        ))
        self.state.record(ResonanceSnapshot(
            gana_name="gana_ghost", tool_name=None, operation=None, output_preview="",
        ))
        counts = self.state.get_gana_counts()
        self.assertEqual(counts["gana_horn"], 2)
        self.assertEqual(counts["gana_ghost"], 1)

    def test_recent_history(self):
        for i in range(15):
            self.state.record(ResonanceSnapshot(
                gana_name=f"gana_{i}", tool_name=None, operation=None,
                output_preview=f"call {i}",
            ))
        history = self.state.get_recent_history(5)
        self.assertEqual(len(history), 5)

    def test_reset(self):
        self.state.record(ResonanceSnapshot(
            gana_name="gana_horn", tool_name=None, operation=None, output_preview="",
        ))
        self.state.reset()
        self.assertIsNone(self.state.get_predecessor())
        self.assertEqual(self.state.call_count, 0)


class TestBuildResonanceContext(unittest.TestCase):
    """Test build_resonance_context()."""

    def setUp(self):
        # Reset singleton state
        import whitemagic.tools.prat_resonance as mod
        mod._state = PratResonanceState()

    @patch("whitemagic.tools.prat_resonance._get_harmony_snapshot")
    @patch("whitemagic.tools.prat_resonance._get_lunar_phase")
    def test_first_call_no_predecessor(self, mock_lunar, mock_harmony):
        mock_lunar.return_value = (0.25, 5)
        mock_harmony.return_value = {
            "harmony_score": 0.85, "guna_dominant": "rajasic",
            "energy": 0.9, "error_rate": 0.95, "dharma": 1.0,
        }
        ctx = build_resonance_context("gana_ghost")
        self.assertEqual(ctx["gana"], "gana_ghost")
        self.assertEqual(ctx["quadrant"], "South")
        self.assertEqual(ctx["meaning"], "Introspection")
        self.assertNotIn("predecessor", ctx)
        self.assertEqual(ctx["chain_position"], 0)
        self.assertEqual(ctx["mode_hint"], "normal")

    @patch("whitemagic.tools.prat_resonance._get_harmony_snapshot")
    @patch("whitemagic.tools.prat_resonance._get_lunar_phase")
    def test_with_predecessor(self, mock_lunar, mock_harmony):
        mock_lunar.return_value = (0.5, 14)
        mock_harmony.return_value = {
            "harmony_score": 0.9, "guna_dominant": "sattvic",
            "energy": 1.0, "error_rate": 1.0, "dharma": 1.0,
        }
        # Record a predecessor
        state = get_resonance_state()
        state.record(ResonanceSnapshot(
            gana_name="gana_horn", tool_name="session_bootstrap",
            operation=None, output_preview="status=ok; action=initialize",
            timestamp=time.time(),
        ))

        ctx = build_resonance_context("gana_heart")
        self.assertIn("predecessor", ctx)
        self.assertEqual(ctx["predecessor"]["gana"], "gana_horn")
        self.assertEqual(ctx["mode_hint"], "optimal")  # sattvic → optimal

    @patch("whitemagic.tools.prat_resonance._get_harmony_snapshot")
    @patch("whitemagic.tools.prat_resonance._get_lunar_phase")
    def test_tamasic_mode_hint(self, mock_lunar, mock_harmony):
        mock_lunar.return_value = (0.1, 3)
        mock_harmony.return_value = {
            "harmony_score": 0.3, "guna_dominant": "tamasic",
            "energy": 0.2, "error_rate": 0.5, "dharma": 0.8,
        }
        ctx = build_resonance_context("gana_root")
        self.assertEqual(ctx["mode_hint"], "minimal")

    @patch("whitemagic.tools.prat_resonance._get_harmony_snapshot")
    @patch("whitemagic.tools.prat_resonance._get_lunar_phase")
    def test_lunar_alignment(self, mock_lunar, mock_harmony):
        mock_lunar.return_value = (0.5, 8)  # mansion 8 = Ghost
        mock_harmony.return_value = {
            "harmony_score": 0.9, "guna_dominant": "rajasic",
            "energy": 1.0, "error_rate": 1.0, "dharma": 1.0,
        }
        ctx = build_resonance_context("gana_ghost")
        self.assertTrue(ctx["lunar_aligned"])
        self.assertIn("lunar_amplification", ctx)

    @patch("whitemagic.tools.prat_resonance._get_harmony_snapshot")
    @patch("whitemagic.tools.prat_resonance._get_lunar_phase")
    def test_successor_info(self, mock_lunar, mock_harmony):
        mock_lunar.return_value = (0.5, 1)
        mock_harmony.return_value = {
            "harmony_score": 1.0, "guna_dominant": "rajasic",
            "energy": 1.0, "error_rate": 1.0, "dharma": 1.0,
        }
        ctx = build_resonance_context("gana_horn")
        self.assertEqual(ctx["successor"]["gana"], "gana_neck")
        self.assertEqual(ctx["successor"]["meaning"], "Stability")


class TestRecordResonance(unittest.TestCase):
    """Test record_resonance()."""

    def setUp(self):
        import whitemagic.tools.prat_resonance as mod
        mod._state = PratResonanceState()

    @patch("whitemagic.tools.prat_resonance._get_harmony_snapshot")
    @patch("whitemagic.tools.prat_resonance._get_lunar_phase")
    def test_records_and_returns_metadata(self, mock_lunar, mock_harmony):
        mock_lunar.return_value = (0.3, 10)
        mock_harmony.return_value = {
            "harmony_score": 0.8, "guna_dominant": "rajasic",
            "energy": 0.9, "error_rate": 0.95, "dharma": 1.0,
        }
        result = {"status": "ok", "data": [1, 2, 3]}
        meta = record_resonance("gana_ghost", "gnosis", None, result)
        self.assertEqual(meta["gana"], "gana_ghost")
        self.assertEqual(meta["chain_position"], 1)
        self.assertIn("successor_hint", meta)
        self.assertIn("garden", meta)

    @patch("whitemagic.tools.prat_resonance._get_harmony_snapshot")
    @patch("whitemagic.tools.prat_resonance._get_lunar_phase")
    def test_output_preview_from_dict(self, mock_lunar, mock_harmony):
        mock_lunar.return_value = (0.5, 5)
        mock_harmony.return_value = {
            "harmony_score": 1.0, "guna_dominant": "sattvic",
            "energy": 1.0, "error_rate": 1.0, "dharma": 1.0,
        }
        result = {"status": "ok", "note": "all good", "error": None}
        record_resonance("gana_heart", "scratchpad", None, result)
        state = get_resonance_state()
        pred = state.get_predecessor()
        self.assertIn("status=ok", pred.output_preview)


class TestGetResonanceSummary(unittest.TestCase):
    """Test get_resonance_summary() for Gnosis portal."""

    def setUp(self):
        import whitemagic.tools.prat_resonance as mod
        mod._state = PratResonanceState()

    @patch("whitemagic.tools.prat_resonance._get_harmony_snapshot")
    @patch("whitemagic.tools.prat_resonance._get_lunar_phase")
    def test_empty_summary(self, mock_lunar, mock_harmony):
        mock_lunar.return_value = (0.0, 1)
        mock_harmony.return_value = {
            "harmony_score": 1.0, "guna_dominant": "rajasic",
            "energy": 1.0, "error_rate": 1.0, "dharma": 1.0,
        }
        summary = get_resonance_summary()
        self.assertEqual(summary["session_calls"], 0)
        self.assertIsNone(summary["last_gana"])
        self.assertIn("lunar_phase", summary)

    @patch("whitemagic.tools.prat_resonance._get_harmony_snapshot")
    @patch("whitemagic.tools.prat_resonance._get_lunar_phase")
    def test_summary_after_calls(self, mock_lunar, mock_harmony):
        mock_lunar.return_value = (0.5, 14)
        mock_harmony.return_value = {
            "harmony_score": 0.7, "guna_dominant": "rajasic",
            "energy": 0.8, "error_rate": 0.9, "dharma": 1.0,
        }
        state = get_resonance_state()
        state.record(ResonanceSnapshot(
            gana_name="gana_ghost", tool_name="gnosis", operation=None,
            output_preview="status=ok",
        ))
        state.record(ResonanceSnapshot(
            gana_name="gana_horn", tool_name="session_bootstrap", operation=None,
            output_preview="status=initiated",
        ))
        summary = get_resonance_summary()
        self.assertEqual(summary["session_calls"], 2)
        self.assertEqual(summary["last_gana"], "gana_horn")
        self.assertEqual(summary["gana_counts"]["gana_ghost"], 1)
        self.assertEqual(summary["gana_counts"]["gana_horn"], 1)
        self.assertEqual(len(summary["recent_history"]), 2)


class TestPRATRouterResonanceIntegration(unittest.TestCase):
    """Test that route_prat_call injects resonance metadata."""

    def setUp(self):
        import whitemagic.tools.prat_resonance as mod
        mod._state = PratResonanceState()

    @patch("whitemagic.tools.prat_resonance._get_harmony_snapshot")
    @patch("whitemagic.tools.prat_resonance._get_lunar_phase")
    def test_native_operation_has_resonance(self, mock_lunar, mock_harmony):
        mock_lunar.return_value = (0.5, 8)
        mock_harmony.return_value = {
            "harmony_score": 0.9, "guna_dominant": "rajasic",
            "energy": 1.0, "error_rate": 1.0, "dharma": 1.0,
        }
        from whitemagic.tools.prat_router import route_prat_call
        result = route_prat_call("gana_ghost", operation="search")
        self.assertIn("_resonance", result)
        self.assertEqual(result["_resonance"]["gana"], "gana_ghost")
        self.assertIn("successor_hint", result["_resonance"])

    @patch("whitemagic.tools.prat_resonance._get_harmony_snapshot")
    @patch("whitemagic.tools.prat_resonance._get_lunar_phase")
    def test_sequential_calls_build_predecessor(self, mock_lunar, mock_harmony):
        mock_lunar.return_value = (0.3, 5)
        mock_harmony.return_value = {
            "harmony_score": 0.85, "guna_dominant": "sattvic",
            "energy": 0.9, "error_rate": 0.95, "dharma": 1.0,
        }
        from whitemagic.tools.prat_router import route_prat_call

        # First call — no predecessor
        r1 = route_prat_call("gana_horn", operation="search")
        self.assertNotIn("predecessor_context", r1)

        # Second call — should have predecessor from first
        from whitemagic.tools.prat_resonance import build_resonance_context
        ctx = build_resonance_context("gana_ghost")
        self.assertIn("predecessor", ctx)
        self.assertEqual(ctx["predecessor"]["gana"], "gana_horn")

    @patch("whitemagic.tools.prat_resonance._get_harmony_snapshot")
    @patch("whitemagic.tools.prat_resonance._get_lunar_phase")
    def test_lunar_amplification_in_native(self, mock_lunar, mock_harmony):
        mock_lunar.return_value = (0.5, 1)  # mansion 1 = Horn
        mock_harmony.return_value = {
            "harmony_score": 1.0, "guna_dominant": "rajasic",
            "energy": 1.0, "error_rate": 1.0, "dharma": 1.0,
        }
        from whitemagic.tools.prat_router import route_prat_call
        result = route_prat_call("gana_horn", operation="analyze")
        self.assertIn("lunar_amplification", result)


if __name__ == "__main__":
    unittest.main()
