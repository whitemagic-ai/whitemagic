"""
Tests for the Galactic Map system:
  - GalacticMap distance computation and zone classification
  - No-delete policy enforcement (archive_to_edge replaces delete)
  - Memory dataclass galactic fields
  - Serialization round-trip
"""

import pytest
from datetime import datetime


# ---------------------------------------------------------------------------
# Galactic Zone Classification
# ---------------------------------------------------------------------------

class TestGalacticZones:
    """Tests for zone classification from galactic distance."""

    def test_core_zone(self):
        from whitemagic.core.memory.galactic_map import classify_zone, GalacticZone
        assert classify_zone(0.0) == GalacticZone.CORE
        assert classify_zone(0.10) == GalacticZone.CORE
        assert classify_zone(0.14) == GalacticZone.CORE

    def test_inner_rim_zone(self):
        from whitemagic.core.memory.galactic_map import classify_zone, GalacticZone
        assert classify_zone(0.15) == GalacticZone.INNER_RIM
        assert classify_zone(0.30) == GalacticZone.INNER_RIM
        assert classify_zone(0.39) == GalacticZone.INNER_RIM

    def test_mid_band_zone(self):
        from whitemagic.core.memory.galactic_map import classify_zone, GalacticZone
        assert classify_zone(0.40) == GalacticZone.MID_BAND
        assert classify_zone(0.50) == GalacticZone.MID_BAND
        assert classify_zone(0.64) == GalacticZone.MID_BAND

    def test_outer_rim_zone(self):
        from whitemagic.core.memory.galactic_map import classify_zone, GalacticZone
        assert classify_zone(0.65) == GalacticZone.OUTER_RIM
        assert classify_zone(0.75) == GalacticZone.OUTER_RIM
        assert classify_zone(0.84) == GalacticZone.OUTER_RIM

    def test_far_edge_zone(self):
        from whitemagic.core.memory.galactic_map import classify_zone, GalacticZone
        assert classify_zone(0.85) == GalacticZone.FAR_EDGE
        assert classify_zone(0.95) == GalacticZone.FAR_EDGE
        assert classify_zone(1.0) == GalacticZone.FAR_EDGE


# ---------------------------------------------------------------------------
# Galactic Distance Computation
# ---------------------------------------------------------------------------

class TestGalacticDistance:
    """Tests for the GalacticMap distance computation."""

    def _make_memory(self, **overrides):
        from whitemagic.core.memory.unified_types import Memory, MemoryType
        defaults = dict(
            id="test-mem-001",
            content="Test memory content",
            memory_type=MemoryType.SHORT_TERM,
            title="Test Memory",
            importance=0.5,
            emotional_valence=0.0,
            neuro_score=0.8,
            novelty_score=0.7,
            recall_count=3,
            half_life_days=30.0,
            is_protected=False,
            is_core_identity=False,
            is_sacred=False,
            is_pinned=False,
            emotional_weight=0.5,
        )
        defaults.update(overrides)
        return Memory(**defaults)

    def test_protected_memory_at_core(self):
        from whitemagic.core.memory.galactic_map import GalacticMap
        gmap = GalacticMap()

        mem = self._make_memory(is_protected=True, neuro_score=0.01, importance=0.01)
        assert gmap.compute_distance(mem) == 0.0

    def test_core_identity_at_core(self):
        from whitemagic.core.memory.galactic_map import GalacticMap
        gmap = GalacticMap()

        mem = self._make_memory(is_core_identity=True)
        assert gmap.compute_distance(mem) == 0.0

    def test_sacred_at_core(self):
        from whitemagic.core.memory.galactic_map import GalacticMap
        gmap = GalacticMap()

        mem = self._make_memory(is_sacred=True)
        assert gmap.compute_distance(mem) == 0.0

    def test_pinned_at_core(self):
        from whitemagic.core.memory.galactic_map import GalacticMap
        gmap = GalacticMap()

        mem = self._make_memory(is_pinned=True)
        assert gmap.compute_distance(mem) == 0.0

    def test_strong_memory_near_core(self):
        from whitemagic.core.memory.galactic_map import GalacticMap
        gmap = GalacticMap()

        mem = self._make_memory(importance=0.95, neuro_score=0.95, recall_count=50)
        distance = gmap.compute_distance(mem)
        assert distance < 0.40  # Should be in CORE or INNER_RIM zone

    def test_weak_memory_near_edge(self):
        from whitemagic.core.memory.galactic_map import GalacticMap
        gmap = GalacticMap()

        mem = self._make_memory(importance=0.05, neuro_score=0.1, recall_count=0, emotional_valence=0.0)
        distance = gmap.compute_distance(mem)
        assert distance > 0.65  # Should be in OUTER_RIM or FAR_EDGE

    def test_explicit_retention_score_overrides_heuristic(self):
        from whitemagic.core.memory.galactic_map import GalacticMap
        gmap = GalacticMap()

        mem = self._make_memory(importance=0.95, neuro_score=0.95)
        # With explicit low retention, distance should be far
        distance = gmap.compute_distance(mem, retention_score=0.1)
        assert distance == pytest.approx(0.9, abs=0.01)

    def test_distance_bounded_zero_to_one(self):
        from whitemagic.core.memory.galactic_map import GalacticMap
        gmap = GalacticMap()

        mem = self._make_memory(importance=0.0, neuro_score=0.0, recall_count=0, emotional_valence=0.0)
        distance = gmap.compute_distance(mem)
        assert 0.0 <= distance <= 1.0

        mem2 = self._make_memory(importance=1.0, neuro_score=1.0, recall_count=100, emotional_valence=1.0)
        distance2 = gmap.compute_distance(mem2)
        assert 0.0 <= distance2 <= 1.0

    def test_emotional_memory_closer_to_core(self):
        from whitemagic.core.memory.galactic_map import GalacticMap
        gmap = GalacticMap()

        neutral = self._make_memory(emotional_valence=0.0, importance=0.3, neuro_score=0.3)
        emotional = self._make_memory(emotional_valence=0.9, importance=0.3, neuro_score=0.3)

        d_neutral = gmap.compute_distance(neutral)
        d_emotional = gmap.compute_distance(emotional)
        assert d_emotional < d_neutral


# ---------------------------------------------------------------------------
# No-Delete Policy
# ---------------------------------------------------------------------------

class TestNoDeletePolicy:
    """Tests that no memory is ever truly deleted."""

    def test_delete_redirects_to_archive(self):
        """The delete() method should now redirect to archive_to_edge()."""
        from whitemagic.core.memory.sqlite_backend import SQLiteBackend
        import tempfile
        from pathlib import Path

        with tempfile.TemporaryDirectory() as tmpdir:
            db_path = Path(tmpdir) / "test.db"
            backend = SQLiteBackend(db_path)

            # Store a test memory
            from whitemagic.core.memory.unified_types import Memory, MemoryType
            mem = Memory(
                id="nodelete-001",
                content="This memory shall not be destroyed",
                memory_type=MemoryType.SHORT_TERM,
                title="Sacred Test",
                importance=0.5,
            )
            backend.store(mem)

            # Call delete — should redirect to archive
            backend.delete("nodelete-001")

            # Memory should STILL exist
            recalled = backend.recall("nodelete-001")
            assert recalled is not None
            assert recalled.id == "nodelete-001"
            assert recalled.galactic_distance == pytest.approx(0.95, abs=0.01)

    def test_archive_to_edge_preserves_content(self):
        """archive_to_edge should keep the memory but push it to the galactic edge."""
        from whitemagic.core.memory.sqlite_backend import SQLiteBackend
        from whitemagic.core.memory.unified_types import Memory, MemoryType
        import tempfile
        from pathlib import Path

        with tempfile.TemporaryDirectory() as tmpdir:
            db_path = Path(tmpdir) / "test.db"
            backend = SQLiteBackend(db_path)

            mem = Memory(
                id="archive-001",
                content="Important content that must survive",
                memory_type=MemoryType.LONG_TERM,
                title="Archive Test",
                importance=0.8,
                neuro_score=0.9,
            )
            backend.store(mem)

            # Archive to edge
            backend.archive_to_edge("archive-001", galactic_distance=0.92)

            # Recall and verify
            recalled = backend.recall("archive-001")
            assert recalled is not None
            assert recalled.content == "Important content that must survive"
            assert recalled.galactic_distance == pytest.approx(0.92, abs=0.01)
            # neuro_score should be reduced but not zero
            assert recalled.neuro_score > 0
            assert recalled.neuro_score < 0.9  # Reduced from original

    def test_batch_update_galactic(self):
        """batch_update_galactic should update multiple memories at once."""
        from whitemagic.core.memory.sqlite_backend import SQLiteBackend
        from whitemagic.core.memory.unified_types import Memory, MemoryType
        import tempfile
        from pathlib import Path

        with tempfile.TemporaryDirectory() as tmpdir:
            db_path = Path(tmpdir) / "test.db"
            backend = SQLiteBackend(db_path)

            for i in range(5):
                mem = Memory(
                    id=f"batch-{i:03d}",
                    content=f"Batch memory {i}",
                    memory_type=MemoryType.SHORT_TERM,
                    title=f"Batch {i}",
                )
                backend.store(mem)

            updates = [
                ("batch-000", 0.1, 0.9),
                ("batch-001", 0.3, 0.7),
                ("batch-002", 0.5, 0.5),
                ("batch-003", 0.7, 0.3),
                ("batch-004", 0.95, 0.1),
            ]
            count = backend.batch_update_galactic(updates)
            assert count == 5

            # Verify
            m0 = backend.recall("batch-000")
            m4 = backend.recall("batch-004")
            assert m0.galactic_distance == pytest.approx(0.1, abs=0.01)
            assert m0.retention_score == pytest.approx(0.9, abs=0.01)
            assert m4.galactic_distance == pytest.approx(0.95, abs=0.01)
            assert m4.retention_score == pytest.approx(0.1, abs=0.01)


# ---------------------------------------------------------------------------
# Memory Dataclass Galactic Fields
# ---------------------------------------------------------------------------

class TestMemoryGalacticFields:
    """Tests for galactic fields on the Memory dataclass."""

    def test_default_galactic_distance(self):
        from whitemagic.core.memory.unified_types import Memory, MemoryType
        mem = Memory(id="test", content="test", memory_type=MemoryType.SHORT_TERM)
        assert mem.galactic_distance == 0.0
        assert mem.retention_score == 0.5
        assert mem.last_retention_sweep is None

    def test_to_dict_includes_galactic_fields(self):
        from whitemagic.core.memory.unified_types import Memory, MemoryType
        mem = Memory(
            id="test", content="test", memory_type=MemoryType.SHORT_TERM,
            galactic_distance=0.42, retention_score=0.78,
        )
        d = mem.to_dict()
        assert d['galactic_distance'] == 0.42
        assert d['retention_score'] == 0.78
        assert d['last_retention_sweep'] is None

    def test_from_dict_round_trip(self):
        from whitemagic.core.memory.unified_types import Memory, MemoryType
        mem = Memory(
            id="round-trip",
            content="Test round trip",
            memory_type=MemoryType.LONG_TERM,
            title="Round Trip",
            galactic_distance=0.65,
            retention_score=0.33,
            last_retention_sweep=datetime(2026, 2, 7, 12, 0, 0),
        )
        d = mem.to_dict()
        restored = Memory.from_dict(d)

        assert restored.galactic_distance == 0.65
        assert restored.retention_score == 0.33
        assert restored.last_retention_sweep is not None


# ---------------------------------------------------------------------------
# Sweep Report
# ---------------------------------------------------------------------------

class TestGalacticSweepReport:
    """Tests for the GalacticSweepReport data structure."""

    def test_report_to_dict(self):
        from whitemagic.core.memory.galactic_map import GalacticSweepReport
        report = GalacticSweepReport(
            total_memories=100,
            memories_updated=95,
            zone_counts={"core": 20, "inner_rim": 30, "mid_band": 25, "outer_rim": 15, "far_edge": 10},
            avg_retention=0.55,
            avg_distance=0.45,
            core_count=20,
            edge_count=10,
            protected_count=5,
            sweep_duration_ms=150.0,
        )
        d = report.to_dict()
        assert d["total_memories"] == 100
        assert d["core_count"] == 20
        assert d["edge_count"] == 10
        assert d["avg_retention"] == 0.55
        assert "timestamp" in d
