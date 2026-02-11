"""
Tests for polyglot bridge coverage — Rust accelerators, Haskell bridge, Elixir bridge.
"""
import json
import pytest


# =========================================================================
# 1. Rust Accelerator Bridge
# =========================================================================

class TestRustAcceleratorBridge:
    """Test the Rust accelerator bridge with Python fallback."""

    def test_rust_available_check(self):
        from whitemagic.optimization.rust_accelerators import rust_available
        # Should be a bool regardless of whether Rust is installed
        assert isinstance(rust_available(), bool)

    def test_galactic_batch_score_python_fallback(self):
        from whitemagic.optimization.rust_accelerators import _galactic_batch_score_python
        memories = [
            {"id": "m1", "importance": 0.9, "neuro_score": 0.8, "emotional_valence": 0.5,
             "recall_count": 15, "is_protected": False},
            {"id": "m2", "importance": 0.2, "neuro_score": 0.1, "emotional_valence": 0.0,
             "recall_count": 0, "is_protected": False},
            {"id": "m3", "importance": 0.5, "is_protected": True},
        ]
        results = _galactic_batch_score_python(memories, quick=True)
        assert len(results) == 3
        # Protected memory should be at core
        assert results[2]["galactic_distance"] == 0.0
        assert results[2]["zone"] == "core"
        # High importance should be closer than low importance
        assert results[0]["galactic_distance"] < results[1]["galactic_distance"]

    def test_galactic_batch_score_full(self):
        from whitemagic.optimization.rust_accelerators import _galactic_batch_score_python
        memories = [
            {"id": "m1", "memory_type_weight": 0.8, "richness": 0.6, "activity": 0.3,
             "recency": 0.9, "importance": 0.7, "emotion": 0.2, "protection": 0.0},
        ]
        results = _galactic_batch_score_python(memories, quick=False)
        assert len(results) == 1
        r = results[0]
        assert "retention_score" in r
        assert "galactic_distance" in r
        assert "zone" in r
        assert 0.0 <= r["retention_score"] <= 1.0
        assert 0.0 <= r["galactic_distance"] <= 1.0

    def test_galactic_batch_score_unified(self):
        """Test via the unified entry point (auto-selects Rust or Python)."""
        from whitemagic.optimization.rust_accelerators import galactic_batch_score
        memories = [
            {"id": "test1", "importance": 0.5, "neuro_score": 0.5,
             "emotional_valence": 0.0, "recall_count": 5, "is_protected": False},
        ]
        results = galactic_batch_score(memories, quick=True)
        assert len(results) == 1
        assert results[0]["id"] == "test1"

    def test_association_mine_python_fallback(self):
        from whitemagic.optimization.rust_accelerators import _association_mine_python
        texts = [
            ("m1", "python machine learning deep neural network training"),
            ("m2", "python deep learning neural network inference optimization"),
            ("m3", "javascript react frontend web development deployment"),
        ]
        result = _association_mine_python(texts, max_keywords=50, min_score=0.1, max_results=100)
        assert result["memory_count"] == 3
        assert "pair_count" in result
        assert "overlaps" in result
        # m1 and m2 should have higher overlap than m1 and m3
        overlaps = result["overlaps"]
        if len(overlaps) >= 2:
            m1_m2 = [o for o in overlaps if {o["source_id"], o["target_id"]} == {"m1", "m2"}]
            m1_m3 = [o for o in overlaps if {o["source_id"], o["target_id"]} == {"m1", "m3"}]
            if m1_m2 and m1_m3:
                assert m1_m2[0]["overlap_score"] > m1_m3[0]["overlap_score"]

    def test_association_mine_unified(self):
        from whitemagic.optimization.rust_accelerators import association_mine
        texts = [
            ("a", "hello world test python"),
            ("b", "hello world test java"),
        ]
        result = association_mine(texts, max_keywords=20, min_score=0.1)
        assert result["memory_count"] == 2

    def test_spatial_index_5d_python_fallback(self):
        from whitemagic.optimization.rust_accelerators import PythonSpatialIndex5D
        idx = PythonSpatialIndex5D()
        idx.add("m1", [0.1, 0.2, 0.3, 0.4, 0.5])
        idx.add("m2", [0.9, 0.8, 0.7, 0.6, 0.5])
        idx.add("m3", [0.1, 0.2, 0.3, 0.4, 0.6])

        assert idx.size() == 3

        # Nearest to m1's location should be m3 (closest neighbor)
        results = idx.query_nearest([0.1, 0.2, 0.3, 0.4, 0.5], n=2)
        assert len(results) == 2
        assert results[0][0] == "m1"  # exact match
        assert results[1][0] == "m3"  # closest

    def test_spatial_index_5d_radius_query(self):
        from whitemagic.optimization.rust_accelerators import PythonSpatialIndex5D
        idx = PythonSpatialIndex5D()
        idx.add("close", [0.0, 0.0, 0.0, 0.0, 0.0])
        idx.add("far", [1.0, 1.0, 1.0, 1.0, 1.0])

        results = idx.query_within_radius([0.0, 0.0, 0.0, 0.0, 0.0], radius_sq=0.1)
        assert len(results) == 1
        assert results[0][0] == "close"

    def test_spatial_index_5d_batch_add(self):
        from whitemagic.optimization.rust_accelerators import PythonSpatialIndex5D
        idx = PythonSpatialIndex5D()
        items = [("a", [0.1]*5), ("b", [0.2]*5), ("c", [0.3]*5)]
        count = idx.add_batch(items)
        assert count == 3
        assert idx.size() == 3

    def test_spatial_index_5d_snapshot(self):
        from whitemagic.optimization.rust_accelerators import PythonSpatialIndex5D
        idx = PythonSpatialIndex5D()
        idx.add("m1", [0.5]*5)
        snap = idx.get_snapshot()
        assert len(snap) == 1
        assert snap[0][0] == "m1"

    def test_get_spatial_index_5d(self):
        """Test the singleton getter."""
        from whitemagic.optimization.rust_accelerators import get_spatial_index_5d
        idx = get_spatial_index_5d()
        assert hasattr(idx, "add")
        assert hasattr(idx, "query_nearest")
        assert hasattr(idx, "size")


# =========================================================================
# 2. Rust .so Module (if available)
# =========================================================================

class TestRustNativeModule:
    """Test the native Rust module directly (skipped if not available)."""

    @pytest.fixture(autouse=True)
    def _check_rust(self):
        try:
            import whitemagic_rs
            self.rs = whitemagic_rs
        except ImportError:
            pytest.skip("whitemagic_rs not installed")

    def test_module_has_accelerators(self):
        assert hasattr(self.rs, "galactic_batch_score")
        assert hasattr(self.rs, "association_mine_fast")
        assert hasattr(self.rs, "SpatialIndex5D")
        assert hasattr(self.rs, "batch_nearest_5d")

    def test_galactic_batch_score_rust(self):
        memories = [{"id": "r1", "importance": 0.7, "neuro_score": 0.6,
                     "emotional_valence": 0.3, "recall_count": 10,
                     "is_protected": False, "memory_type_weight": 0.5,
                     "richness": 0.4, "activity": 0.2, "recency": 0.8,
                     "emotion": 0.1, "protection": 0.0}]
        result_json = self.rs.galactic_batch_score(json.dumps(memories))
        results = json.loads(result_json)
        assert len(results) == 1
        assert "retention_score" in results[0]

    def test_association_extract_keywords_rust(self):
        texts = [["m1", "python machine learning deep neural network training"]]
        result = self.rs.association_extract_keywords(json.dumps(texts), 10)
        assert isinstance(result, str)
        parsed = json.loads(result)
        assert isinstance(parsed, list)
        assert len(parsed) == 1
        assert parsed[0][0] == "m1"
        assert len(parsed[0][1]) > 0  # has keywords

    def test_spatial_index_5d_rust(self):
        idx = self.rs.SpatialIndex5D()
        idx.add("test1", [0.1, 0.2, 0.3, 0.4, 0.5])
        idx.add("test2", [0.9, 0.8, 0.7, 0.6, 0.5])
        assert idx.size() == 2
        results = idx.query_nearest([0.1, 0.2, 0.3, 0.4, 0.5], 1)
        assert len(results) == 1


# =========================================================================
# 3. Haskell Bridge
# =========================================================================

class TestHaskellBridge:
    def test_haskell_bridge_fallback(self):
        from whitemagic.core.bridge.haskell import get_haskell_bridge
        bridge = get_haskell_bridge()
        result = bridge.calculate_balance([0.3, 0.7])
        assert result["status"] == "fallback"
        assert abs(result["balance"] - 0.5) < 0.01

    def test_haskell_bridge_availability(self):
        from whitemagic.core.bridge.haskell import get_haskell_bridge
        bridge = get_haskell_bridge()
        assert isinstance(bridge.check_availability(), bool)

    def test_haskell_bridge_singleton(self):
        from whitemagic.core.bridge.haskell import get_haskell_bridge
        a = get_haskell_bridge()
        b = get_haskell_bridge()
        assert a is b


# =========================================================================
# 4. Dream Cycle
# =========================================================================

class TestDreamCycle:
    def test_dream_cycle_init(self):
        from whitemagic.core.dreaming.dream_cycle import DreamCycle
        dc = DreamCycle(idle_threshold_seconds=5.0, cycle_interval_seconds=1.0)
        assert not dc._running
        assert not dc._dreaming

    def test_dream_cycle_status(self):
        from whitemagic.core.dreaming.dream_cycle import DreamCycle
        dc = DreamCycle()
        status = dc.status()
        assert "running" in status
        assert "dreaming" in status
        assert "idle_seconds" in status
        assert "total_cycles" in status
        assert "current_phase" in status
        assert status["running"] is False

    def test_dream_cycle_touch(self):
        import time
        from whitemagic.core.dreaming.dream_cycle import DreamCycle
        dc = DreamCycle()
        old_activity = dc._last_activity
        time.sleep(0.05)
        dc.touch()
        assert dc._last_activity > old_activity

    def test_dream_cycle_start_stop(self):
        from whitemagic.core.dreaming.dream_cycle import DreamCycle
        dc = DreamCycle(idle_threshold_seconds=9999.0, cycle_interval_seconds=0.1)
        dc.start()
        assert dc._running
        dc.stop()
        assert not dc._running

    def test_dream_phases_enum(self):
        from whitemagic.core.dreaming.dream_cycle import DreamPhase
        phases = list(DreamPhase)
        assert len(phases) == 6
        assert DreamPhase.CONSOLIDATION in phases
        assert DreamPhase.SERENDIPITY in phases
        assert DreamPhase.GOVERNANCE in phases
        assert DreamPhase.KAIZEN in phases
        assert DreamPhase.ORACLE in phases
        assert DreamPhase.DECAY in phases

    def test_dream_report_to_dict(self):
        from whitemagic.core.dreaming.dream_cycle import DreamReport, DreamPhase
        report = DreamReport(
            phase=DreamPhase.KAIZEN,
            started_at="2026-02-08T12:00:00",
            duration_ms=42.5,
            details={"hints": ["test"]},
            success=True,
        )
        d = report.to_dict()
        assert d["phase"] == "kaizen"
        assert d["duration_ms"] == 42.5
        assert d["success"] is True

    def test_dream_cycle_singleton(self):
        from whitemagic.core.dreaming.dream_cycle import get_dream_cycle
        a = get_dream_cycle()
        b = get_dream_cycle()
        assert a is b

    def test_dream_cycle_forced_phase(self):
        """Force-run a single dream phase without starting the background loop."""
        from whitemagic.core.dreaming.dream_cycle import DreamCycle
        dc = DreamCycle()
        # Force run a phase directly
        dc._run_phase()
        assert dc._total_cycles == 1
        assert len(dc._history) == 1
        report = dc._history[0]
        assert report.phase.value in ["consolidation", "serendipity", "kaizen", "oracle", "decay"]
