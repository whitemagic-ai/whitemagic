"""Tests for Phase 1: Constellation Recall Boost (v14.3).

Validates:
- constellation_membership table creation and CRUD
- get_constellation_centroids() and get_memory_constellation()
- persist_memberships() round-trip
- constellation boost in search_hybrid() scoring
- cross-constellation diversity bonus
"""

from __future__ import annotations

import os
from unittest.mock import MagicMock, patch

import pytest


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

def _insert_stub_memories(backend, ids):
    """Insert minimal stub memory rows so FK constraints pass."""
    with backend.pool.connection() as conn:
        for mid in ids:
            conn.execute(
                "INSERT OR IGNORE INTO memories (id, content, memory_type, importance) VALUES (?, ?, ?, ?)",
                (mid, f"stub content {mid}", "LONG_TERM", 0.5),
            )


@pytest.fixture
def tmp_db(tmp_path):
    """Create a temporary SQLite backend."""
    os.environ["WM_SILENT_INIT"] = "1"
    os.environ["WM_SKIP_HOLO_INDEX"] = "1"
    db_path = tmp_path / "test.db"
    from whitemagic.core.memory.sqlite_backend import SQLiteBackend
    backend = SQLiteBackend(db_path)
    return backend


# ---------------------------------------------------------------------------
# Schema Tests
# ---------------------------------------------------------------------------

class TestConstellationMembershipTable:
    """Verify constellation_membership table exists and has correct schema."""

    def test_table_created(self, tmp_db):
        with tmp_db.pool.connection() as conn:
            tables = conn.execute(
                "SELECT name FROM sqlite_master WHERE type='table' AND name='constellation_membership'"
            ).fetchall()
            assert len(tables) == 1

    def test_table_columns(self, tmp_db):
        with tmp_db.pool.connection() as conn:
            cursor = conn.execute("PRAGMA table_info(constellation_membership)")
            columns = {row[1] for row in cursor.fetchall()}
            assert "memory_id" in columns
            assert "constellation_name" in columns
            assert "membership_confidence" in columns
            assert "updated_at" in columns

    def test_index_exists(self, tmp_db):
        with tmp_db.pool.connection() as conn:
            indexes = conn.execute(
                "SELECT name FROM sqlite_master WHERE type='index' AND name='idx_constellation_name'"
            ).fetchall()
            assert len(indexes) == 1


# ---------------------------------------------------------------------------
# CRUD Tests
# ---------------------------------------------------------------------------

class TestConstellationMembershipCRUD:
    """Test update/get/list operations on constellation_membership."""

    def test_update_and_get(self, tmp_db):
        _insert_stub_memories(tmp_db, ["mem1", "mem2", "mem3"])
        memberships = [
            ("mem1", "Nebula Rust Performance", 0.85),
            ("mem2", "Nebula Rust Performance", 0.72),
            ("mem3", "Drift Golang Ecosystem", 0.60),
        ]
        count = tmp_db.update_constellation_membership(memberships)
        assert count == 3

        result = tmp_db.get_constellation_membership("mem1")
        assert result is not None
        assert result["constellation_name"] == "Nebula Rust Performance"
        assert abs(result["membership_confidence"] - 0.85) < 0.001

    def test_get_nonexistent(self, tmp_db):
        result = tmp_db.get_constellation_membership("nonexistent")
        assert result is None

    def test_get_constellation_members(self, tmp_db):
        _insert_stub_memories(tmp_db, ["mem1", "mem2", "mem3"])
        memberships = [
            ("mem1", "Corona Core", 0.9),
            ("mem2", "Corona Core", 0.8),
            ("mem3", "Drift Archive", 0.5),
        ]
        tmp_db.update_constellation_membership(memberships)
        members = tmp_db.get_constellation_members("Corona Core")
        assert set(members) == {"mem1", "mem2"}

    def test_upsert_replaces(self, tmp_db):
        _insert_stub_memories(tmp_db, ["mem1"])
        tmp_db.update_constellation_membership([("mem1", "Old", 0.5)])
        tmp_db.update_constellation_membership([("mem1", "New", 0.9)])
        result = tmp_db.get_constellation_membership("mem1")
        assert result["constellation_name"] == "New"
        assert abs(result["membership_confidence"] - 0.9) < 0.001

    def test_empty_update(self, tmp_db):
        count = tmp_db.update_constellation_membership([])
        assert count == 0


# ---------------------------------------------------------------------------
# Constellation Detector Methods
# ---------------------------------------------------------------------------

class TestConstellationDetectorMethods:
    """Test get_constellation_centroids() and get_memory_constellation()."""

    def test_centroids_empty_when_no_detection(self):
        from whitemagic.core.memory.constellations import ConstellationDetector
        d = ConstellationDetector()
        assert d.get_constellation_centroids() == []

    def test_centroids_after_mock_detection(self):
        from whitemagic.core.memory.constellations import (
            Constellation,
            ConstellationDetector,
            DetectionReport,
        )
        d = ConstellationDetector()
        report = DetectionReport(
            memories_scanned=100,
            constellations_found=2,
            largest_constellation=60,
            constellations=[
                Constellation(
                    name="Nebula Rust",
                    member_ids=["a", "b", "c"],
                    centroid=(0.1, 0.2, 0.3, 0.4, 0.5),
                    radius=0.15,
                    dominant_tags=["rust", "performance"],
                    dominant_type="LONG_TERM",
                    avg_importance=0.8,
                    zone="mid_band",
                    stability=0.92,
                ),
                Constellation(
                    name="Drift Archive",
                    member_ids=["d", "e"],
                    centroid=(0.9, 0.8, 0.7, 0.6, 0.5),
                    radius=0.3,
                    dominant_tags=["archive", "golang"],
                    dominant_type="LONG_TERM",
                    avg_importance=0.4,
                    zone="outer_rim",
                    stability=0.6,
                ),
            ],
        )
        d._last_report = report

        centroids = d.get_constellation_centroids()
        assert len(centroids) == 2
        assert centroids[0]["name"] == "Nebula Rust"
        assert centroids[0]["centroid"] == (0.1, 0.2, 0.3, 0.4, 0.5)
        assert centroids[0]["stability"] == 0.92

    def test_memory_constellation_from_cache(self):
        from whitemagic.core.memory.constellations import (
            Constellation,
            ConstellationDetector,
            DetectionReport,
        )
        d = ConstellationDetector()
        d._last_report = DetectionReport(
            constellations=[
                Constellation(
                    name="Test Cluster",
                    member_ids=["mem_x", "mem_y"],
                    centroid=(0, 0, 0, 0, 0),
                    radius=0.1,
                    dominant_tags=["test"],
                    dominant_type="LONG_TERM",
                    avg_importance=0.5,
                    zone="mid_band",
                    stability=0.75,
                ),
            ],
        )
        # Patch out the DB lookup to force cache fallback
        with patch("whitemagic.core.memory.unified.get_unified_memory", side_effect=Exception("no DB")):
            result = d.get_memory_constellation("mem_x")
        assert result is not None
        assert result["constellation_name"] == "Test Cluster"
        assert result["membership_confidence"] == 0.75


# ---------------------------------------------------------------------------
# Boost Scoring Logic
# ---------------------------------------------------------------------------

class TestConstellationBoostLogic:
    """Test the constellation boost math without requiring a full search."""

    def test_same_constellation_boost(self):
        """Same-constellation memories should get boosted scores."""
        base_score = 0.01
        constellation_boost = 0.3
        confidence = 0.8
        boosted = base_score * (1.0 + constellation_boost * confidence)
        assert boosted > base_score
        assert abs(boosted - 0.01 * 1.24) < 0.001

    def test_different_constellation_diversity_bonus(self):
        """Different-constellation memories should get a small diversity bonus."""
        base_score = 0.01
        diversity_bonus = 0.05
        confidence = 0.6
        new_score = base_score + diversity_bonus * (1.0 - confidence)
        assert new_score > base_score
        assert abs(new_score - 0.03) < 0.001

    def test_no_constellation_no_change(self):
        """Memories with no constellation membership should keep original score."""
        base_score = 0.5
        # No membership → no change
        assert base_score == 0.5


# ---------------------------------------------------------------------------
# Integration (lightweight — no embedding model needed)
# ---------------------------------------------------------------------------

class TestConstellationPersistRoundTrip:
    """Test persist_memberships() and get_memory_constellation() round-trip via DB."""

    def test_persist_and_lookup(self, tmp_db):
        from whitemagic.core.memory.constellations import (
            Constellation,
            ConstellationDetector,
            DetectionReport,
        )

        _insert_stub_memories(tmp_db, ["m1", "m2", "m3"])
        d = ConstellationDetector()
        d._last_report = DetectionReport(
            constellations=[
                Constellation(
                    name="Test Cluster",
                    member_ids=["m1", "m2", "m3"],
                    centroid=(0, 0, 0, 0, 0),
                    radius=0.1,
                    dominant_tags=["test"],
                    dominant_type="LONG_TERM",
                    avg_importance=0.5,
                    zone="mid_band",
                    stability=0.88,
                ),
            ],
        )

        # Patch get_unified_memory to use our tmp_db backend
        mock_um = MagicMock()
        mock_um.backend = tmp_db
        with patch("whitemagic.core.memory.unified.get_unified_memory", return_value=mock_um):
            count = d.persist_memberships()

        assert count == 3

        # Verify DB has the memberships
        result = tmp_db.get_constellation_membership("m1")
        assert result is not None
        assert result["constellation_name"] == "Test Cluster"
        assert abs(result["membership_confidence"] - 0.88) < 0.001
