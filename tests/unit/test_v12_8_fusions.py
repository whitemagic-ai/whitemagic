"""
Tests for v12.8 Fusions — The Final Five (28 Active Fusions)
=============================================================
Covers:
  - Fusion 10: Gana Chain → Harmony Vector adaptation
  - Fusion 11: PRAT Router → Gana Chain auto-sequencing
  - Fusion 12: Mojo SIMD → Holographic Encoding bridge
  - Fusion 13: Elixir Event Bus → Python Gan Ying bridge
  - Fusion 14: Go Mesh → Memory Sync protocol
  - Capability matrix: 28 active, 0 unexplored
  - Fusion status: 17 tracked entries (all wired)
"""

import unittest
from unittest.mock import patch, MagicMock


# =========================================================================
# Fusion 10: Gana Chain → Harmony Vector
# =========================================================================

class TestGanaChainHarmonyAdapt(unittest.TestCase):
    """Gana chain adapts length based on Harmony Vector health."""

    def test_default_returns_valid_structure(self):
        from whitemagic.core.fusions import gana_chain_harmony_adapt
        result = gana_chain_harmony_adapt(planned_steps=7)
        self.assertIn("max_steps", result)
        self.assertIn("harmony_score", result)
        self.assertIn("guna", result)
        self.assertIn("reason", result)
        self.assertIn("adapted", result)
        self.assertIn("planned_steps", result)
        self.assertEqual(result["planned_steps"], 7)
        self.assertGreaterEqual(result["max_steps"], 1)

    def test_stressed_system_truncates(self):
        """Tamasic / low harmony → truncate chain."""
        from whitemagic.core.fusions import gana_chain_harmony_adapt

        mock_snap = MagicMock()
        mock_snap.harmony_score = 0.2
        mock_snap.guna_sattvic_pct = 0.1
        mock_snap.guna_rajasic_pct = 0.2
        mock_snap.guna_tamasic_pct = 0.7

        mock_hv = MagicMock()
        mock_hv.snapshot.return_value = mock_snap

        with patch("whitemagic.harmony.vector.get_harmony_vector", return_value=mock_hv):
            result = gana_chain_harmony_adapt(planned_steps=9)

        self.assertEqual(result["max_steps"], 3)  # 9 // 3
        self.assertEqual(result["reason"], "system_stressed_truncate")
        self.assertTrue(result["adapted"])

    def test_healthy_system_full_chain(self):
        """Sattvic / high harmony → full chain + bonus."""
        from whitemagic.core.fusions import gana_chain_harmony_adapt

        mock_snap = MagicMock()
        mock_snap.harmony_score = 0.9
        mock_snap.guna_sattvic_pct = 0.8
        mock_snap.guna_rajasic_pct = 0.1
        mock_snap.guna_tamasic_pct = 0.1

        mock_hv = MagicMock()
        mock_hv.snapshot.return_value = mock_snap

        with patch("whitemagic.harmony.vector.get_harmony_vector", return_value=mock_hv):
            result = gana_chain_harmony_adapt(planned_steps=7)

        self.assertEqual(result["max_steps"], 8)  # 7 + 1 bonus
        self.assertEqual(result["reason"], "optimal_health_full_chain")
        self.assertTrue(result["adapted"])

    def test_moderate_health_trims(self):
        """Rajasic / moderate harmony → trim to 70%."""
        from whitemagic.core.fusions import gana_chain_harmony_adapt

        mock_snap = MagicMock()
        mock_snap.harmony_score = 0.55
        mock_snap.guna_sattvic_pct = 0.2
        mock_snap.guna_rajasic_pct = 0.6
        mock_snap.guna_tamasic_pct = 0.2

        mock_hv = MagicMock()
        mock_hv.snapshot.return_value = mock_snap

        with patch("whitemagic.harmony.vector.get_harmony_vector", return_value=mock_hv):
            result = gana_chain_harmony_adapt(planned_steps=10)

        self.assertEqual(result["max_steps"], 7)  # int(10 * 0.7)
        self.assertEqual(result["reason"], "moderate_health_trim")

    def test_fallback_on_error(self):
        """Graceful fallback if HarmonyVector unavailable."""
        from whitemagic.core.fusions import gana_chain_harmony_adapt

        with patch("whitemagic.harmony.vector.get_harmony_vector", side_effect=RuntimeError("no HV")):
            result = gana_chain_harmony_adapt(planned_steps=5)

        self.assertEqual(result["max_steps"], 5)
        self.assertEqual(result["reason"], "fallback_no_harmony")
        self.assertFalse(result["adapted"])

    def test_minimum_one_step(self):
        """Even severely stressed system gets at least 1 step."""
        from whitemagic.core.fusions import gana_chain_harmony_adapt

        mock_snap = MagicMock()
        mock_snap.harmony_score = 0.05
        mock_snap.guna_sattvic_pct = 0.0
        mock_snap.guna_rajasic_pct = 0.0
        mock_snap.guna_tamasic_pct = 1.0

        mock_hv = MagicMock()
        mock_hv.snapshot.return_value = mock_snap

        with patch("whitemagic.harmony.vector.get_harmony_vector", return_value=mock_hv):
            result = gana_chain_harmony_adapt(planned_steps=2)

        self.assertGreaterEqual(result["max_steps"], 1)


# =========================================================================
# Fusion 11: PRAT Router → Gana Chain
# =========================================================================

class TestPratAutoChainDetect(unittest.TestCase):
    """Sequential same-Gana calls trigger chain recommendation."""

    def test_no_history_no_chain(self):
        from whitemagic.core.fusions import prat_auto_chain_detect
        result = prat_auto_chain_detect("gana_horn")
        self.assertFalse(result["chain_detected"])
        self.assertEqual(result["consecutive_same_gana"], 0)
        self.assertIsNone(result["recommendation"])

    def test_consecutive_calls_detected(self):
        """3+ consecutive same-Gana calls trigger chain detection."""
        from whitemagic.core.fusions import prat_auto_chain_detect
        from whitemagic.tools.prat_resonance import get_resonance_state, ResonanceSnapshot

        state = get_resonance_state()
        state.reset()

        # Record 4 consecutive calls to gana_ghost
        for _ in range(4):
            state.record(ResonanceSnapshot(
                gana_name="gana_ghost",
                tool_name="gnosis",
                operation="search",
                output_preview="test output",
                timestamp=0.0,
            ))

        result = prat_auto_chain_detect("gana_ghost")
        self.assertTrue(result["chain_detected"])
        self.assertEqual(result["consecutive_same_gana"], 4)
        self.assertIsNotNone(result["recommendation"])
        self.assertEqual(result["recommendation"]["quadrant"], "South")
        state.reset()

    def test_mixed_history_no_chain(self):
        """Non-consecutive calls don't trigger chain."""
        from whitemagic.core.fusions import prat_auto_chain_detect
        from whitemagic.tools.prat_resonance import get_resonance_state, ResonanceSnapshot

        state = get_resonance_state()
        state.reset()

        # Alternate between two Ganas
        for gana in ["gana_horn", "gana_ghost", "gana_horn", "gana_ghost"]:
            state.record(ResonanceSnapshot(
                gana_name=gana,
                tool_name="test",
                operation="search",
                output_preview="test",
                timestamp=0.0,
            ))

        result = prat_auto_chain_detect("gana_ghost")
        self.assertFalse(result["chain_detected"])
        self.assertEqual(result["consecutive_same_gana"], 1)
        state.reset()

    def test_two_consecutive_below_threshold(self):
        """2 consecutive calls: detected but below chain threshold."""
        from whitemagic.core.fusions import prat_auto_chain_detect
        from whitemagic.tools.prat_resonance import get_resonance_state, ResonanceSnapshot

        state = get_resonance_state()
        state.reset()

        for _ in range(2):
            state.record(ResonanceSnapshot(
                gana_name="gana_net",
                tool_name="test",
                operation="search",
                output_preview="test",
                timestamp=0.0,
            ))

        result = prat_auto_chain_detect("gana_net")
        self.assertFalse(result["chain_detected"])
        self.assertEqual(result["consecutive_same_gana"], 2)
        state.reset()

    def test_session_call_count_tracked(self):
        from whitemagic.core.fusions import prat_auto_chain_detect
        from whitemagic.tools.prat_resonance import get_resonance_state, ResonanceSnapshot

        state = get_resonance_state()
        state.reset()

        for _ in range(5):
            state.record(ResonanceSnapshot(
                gana_name="gana_horn",
                tool_name="test",
                operation="search",
                output_preview="test",
                timestamp=0.0,
            ))

        result = prat_auto_chain_detect("gana_horn")
        self.assertEqual(result["total_session_calls"], 5)
        state.reset()


# =========================================================================
# Fusion 12: Mojo SIMD → Holographic Encoding
# =========================================================================

class TestMojoHolographicBatchEncode(unittest.TestCase):
    """Mojo holographic batch encoding with Python fallback."""

    def test_empty_memories_returns_empty(self):
        from whitemagic.core.fusions import mojo_holographic_batch_encode
        result = mojo_holographic_batch_encode([])
        self.assertEqual(result["count"], 0)
        self.assertEqual(result["batch_size"], 0)

    def test_python_fallback_encodes(self):
        """Python encoder produces 5D coordinates."""
        from whitemagic.core.fusions import mojo_holographic_batch_encode
        memories = [
            {"content": "test memory 1", "importance": 0.7, "tags": ["test"]},
            {"content": "test memory 2", "importance": 0.5, "tags": ["data"]},
            {"content": "test memory 3", "importance": 0.9, "tags": ["critical"]},
        ]
        result = mojo_holographic_batch_encode(memories)
        self.assertEqual(result["count"], 3)
        self.assertEqual(result["batch_size"], 3)
        # Backend should be python (Mojo not installed in test env)
        self.assertIn(result["backend"], ("python", "fallback"))
        # Each coordinate should be 5D
        for coord in result["coordinates"]:
            self.assertEqual(len(coord), 5)

    def test_coordinate_values_are_floats(self):
        from whitemagic.core.fusions import mojo_holographic_batch_encode
        memories = [{"content": "float test", "importance": 0.5}]
        result = mojo_holographic_batch_encode(memories)
        self.assertEqual(result["count"], 1)
        for val in result["coordinates"][0]:
            self.assertIsInstance(val, float)

    def test_large_batch_capped_at_100(self):
        """Output coordinates capped at 100 to prevent huge responses."""
        from whitemagic.core.fusions import mojo_holographic_batch_encode
        memories = [{"content": f"mem {i}", "importance": 0.5} for i in range(150)]
        result = mojo_holographic_batch_encode(memories)
        self.assertLessEqual(len(result["coordinates"]), 100)
        self.assertEqual(result["batch_size"], 150)

    def test_mojo_available_flag(self):
        from whitemagic.core.fusions import mojo_holographic_batch_encode
        result = mojo_holographic_batch_encode([{"content": "test"}])
        self.assertIn("mojo_available", result)
        self.assertIsInstance(result["mojo_available"], bool)


# =========================================================================
# Fusion 13: Elixir Event Bus → Python Gan Ying
# =========================================================================

class TestElixirEventBridge(unittest.TestCase):
    """Elixir OTP event bus bridges with Python Gan Ying."""

    def test_default_event_dispatches(self):
        from whitemagic.core.fusions import elixir_event_bridge
        result = elixir_event_bridge()
        self.assertIn("event_type", result)
        self.assertIn("lane", result)
        self.assertIn("backend", result)
        self.assertIn("elixir_available", result)
        self.assertEqual(result["event_type"], "TOOL_INVOKED")

    def test_fast_lane_classification(self):
        from whitemagic.core.fusions import _classify_event_lane
        self.assertEqual(_classify_event_lane("TOOL_INVOKED"), "FAST")
        self.assertEqual(_classify_event_lane("CIRCUIT_BREAKER_TRIP"), "FAST")
        self.assertEqual(_classify_event_lane("ERROR_OCCURRED"), "FAST")

    def test_slow_lane_classification(self):
        from whitemagic.core.fusions import _classify_event_lane
        self.assertEqual(_classify_event_lane("MEMORY_SWEEP"), "SLOW")
        self.assertEqual(_classify_event_lane("GALACTIC_ROTATION"), "SLOW")
        self.assertEqual(_classify_event_lane("DECAY_DRIFT"), "SLOW")

    def test_medium_lane_default(self):
        from whitemagic.core.fusions import _classify_event_lane
        self.assertEqual(_classify_event_lane("UNKNOWN_EVENT"), "MEDIUM")
        self.assertEqual(_classify_event_lane("GARDEN_ACTIVATED"), "MEDIUM")

    def test_payload_keys_recorded(self):
        from whitemagic.core.fusions import elixir_event_bridge
        result = elixir_event_bridge(
            event_type="MEMORY_UPDATED",
            payload={"memory_id": "abc", "zone": "CORE"},
        )
        self.assertIn("memory_id", result["payload_keys"])
        self.assertIn("zone", result["payload_keys"])

    def test_elixir_available_is_bool(self):
        from whitemagic.core.fusions import elixir_event_bridge
        result = elixir_event_bridge()
        self.assertIsInstance(result["elixir_available"], bool)

    def test_custom_event_type(self):
        from whitemagic.core.fusions import elixir_event_bridge
        result = elixir_event_bridge(event_type="CONSOLIDATION_COMPLETE")
        self.assertEqual(result["event_type"], "CONSOLIDATION_COMPLETE")
        self.assertEqual(result["lane"], "SLOW")


# =========================================================================
# Fusion 14: Go Mesh → Memory Sync
# =========================================================================

class TestMeshMemorySync(unittest.TestCase):
    """Go mesh memory sync protocol."""

    def test_status_operation(self):
        from whitemagic.core.fusions import mesh_memory_sync
        result = mesh_memory_sync(operation="status")
        self.assertEqual(result["operation"], "status")
        self.assertIn("peer_count", result)

    def test_announce_requires_memory_id(self):
        from whitemagic.core.fusions import mesh_memory_sync
        result = mesh_memory_sync(operation="announce", memory_id="")
        self.assertIn("error", result)
        self.assertIn("memory_id required", result["error"])

    def test_announce_with_memory_id(self):
        from whitemagic.core.fusions import mesh_memory_sync
        result = mesh_memory_sync(
            operation="announce",
            memory_id="mem_123",
            payload={"title": "Test", "memory_type": "SHORT_TERM"},
        )
        self.assertEqual(result["operation"], "announce")
        self.assertEqual(result["memory_id"], "mem_123")
        self.assertIn("peer_count", result)

    def test_request_operation(self):
        from whitemagic.core.fusions import mesh_memory_sync
        result = mesh_memory_sync(operation="request", memory_id="mem_456")
        self.assertEqual(result["operation"], "request")
        self.assertEqual(result["memory_id"], "mem_456")

    def test_request_requires_memory_id(self):
        from whitemagic.core.fusions import mesh_memory_sync
        result = mesh_memory_sync(operation="request", memory_id="")
        self.assertIn("error", result)

    def test_unknown_operation(self):
        from whitemagic.core.fusions import mesh_memory_sync
        result = mesh_memory_sync(operation="purge")
        self.assertIn("error", result)
        self.assertIn("unknown operation", result["error"])

    def test_announce_sync_event_structure(self):
        from whitemagic.core.fusions import mesh_memory_sync
        result = mesh_memory_sync(
            operation="announce",
            memory_id="mem_789",
            payload={"title": "Test", "tags": ["a", "b"], "importance": 0.8},
        )
        if "sync_event" in result:
            event = result["sync_event"]
            self.assertEqual(event["type"], "MEMORY_SYNC")
            self.assertEqual(event["sub_type"], "announce")
            self.assertEqual(event["memory_id"], "mem_789")


# =========================================================================
# Capability Matrix — 28 active fusions, 0 unexplored
# =========================================================================

class TestCapabilityMatrix28(unittest.TestCase):
    """Capability matrix reflects 28 active fusions — matching the 28 Ganas."""

    def test_28_active_fusions(self):
        from whitemagic.tools.capability_matrix import ACTIVE_FUSIONS
        self.assertEqual(len(ACTIVE_FUSIONS), 28,
                         f"Expected 28 active fusions (28 Ganas), got {len(ACTIVE_FUSIONS)}")

    def test_zero_unexplored(self):
        from whitemagic.tools.capability_matrix import UNEXPLORED_FUSIONS
        self.assertEqual(len(UNEXPLORED_FUSIONS), 0,
                         f"Expected 0 unexplored fusions, got {len(UNEXPLORED_FUSIONS)}")

    def test_all_fusion_ids_unique(self):
        from whitemagic.tools.capability_matrix import ACTIVE_FUSIONS
        ids = [f["id"] for f in ACTIVE_FUSIONS]
        self.assertEqual(len(ids), len(set(ids)), "Duplicate fusion IDs found")

    def test_new_fusions_present(self):
        from whitemagic.tools.capability_matrix import ACTIVE_FUSIONS
        ids = {f["id"] for f in ACTIVE_FUSIONS}
        expected_new = {
            "gana_chain_harmony", "prat_gana_chain", "mojo_holographic",
            "elixir_event_bus", "go_mesh_sync",
        }
        self.assertTrue(expected_new.issubset(ids),
                        f"Missing fusions: {expected_new - ids}")

    def test_matrix_api_returns_28(self):
        from whitemagic.tools.capability_matrix import get_capability_matrix
        matrix = get_capability_matrix()
        self.assertEqual(matrix["summary"]["active_fusions"], 28)
        self.assertEqual(matrix["summary"]["unexplored_fusions"], 0)


# =========================================================================
# Fusion Status — All 17 entries wired
# =========================================================================

class TestFusionStatus17(unittest.TestCase):
    """Fusion status tracker has all 17 entries, all wired."""

    def test_17_fusions_tracked(self):
        from whitemagic.core.fusions import get_fusion_status
        status = get_fusion_status()
        self.assertEqual(len(status), 17,
                         f"Expected 17 tracked fusions, got {len(status)}")

    def test_all_fusions_wired(self):
        from whitemagic.core.fusions import get_fusion_status
        status = get_fusion_status()
        for name, info in status.items():
            self.assertTrue(info["wired"], f"Fusion {name} not wired")

    def test_new_5_present_in_status(self):
        from whitemagic.core.fusions import get_fusion_status
        status = get_fusion_status()
        expected = {
            "gana_chain_harmony", "prat_gana_chain", "mojo_holographic",
            "elixir_event_bus", "go_mesh_sync",
        }
        self.assertTrue(expected.issubset(set(status.keys())),
                        f"Missing from status: {expected - set(status.keys())}")


if __name__ == "__main__":
    unittest.main()
