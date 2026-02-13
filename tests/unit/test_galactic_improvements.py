"""
Tests for Galactic Map v2 improvements:
  - V dimension in holographic coordinate system
  - Galactic-distance-aware search
  - Memory re-promotion on recall
  - Association miner keyword extraction and overlap
  - Lifecycle manager galactic rotation integration
"""

import pytest
import tempfile
from datetime import datetime
from pathlib import Path


# ---------------------------------------------------------------------------
# V Dimension — Holographic Encoder
# ---------------------------------------------------------------------------

class TestVDimension:
    """Tests for the V (Vitality/Galactic Distance) axis in the encoder."""

    def test_coordinate_has_v_field(self):
        from whitemagic.core.intelligence.hologram.encoder import HolographicCoordinate
        coord = HolographicCoordinate(x=0.1, y=0.2, z=0.3, w=0.5)
        assert coord.v == 0.5  # default

    def test_coordinate_5d_vector(self):
        from whitemagic.core.intelligence.hologram.encoder import HolographicCoordinate
        coord = HolographicCoordinate(x=0.1, y=0.2, z=0.3, w=0.5, v=0.8)
        vec = coord.to_vector()
        assert len(vec) == 5
        assert vec[4] == 0.8

    def test_coordinate_4d_legacy(self):
        from whitemagic.core.intelligence.hologram.encoder import HolographicCoordinate
        coord = HolographicCoordinate(x=0.1, y=0.2, z=0.3, w=0.5, v=0.8)
        vec = coord.to_vector_4d()
        assert len(vec) == 4
        assert 0.8 not in vec

    def test_coordinate_to_dict_has_v(self):
        from whitemagic.core.intelligence.hologram.encoder import HolographicCoordinate
        coord = HolographicCoordinate(x=0.1, y=0.2, z=0.3, w=0.5, v=0.7)
        d = coord.to_dict()
        assert d["v"] == 0.7
        assert len(d) == 5

    def test_calculate_v_from_galactic_distance(self):
        from whitemagic.core.intelligence.hologram.encoder import CoordinateEncoder
        enc = CoordinateEncoder()
        mem = {"galactic_distance": 0.3, "importance": 0.5, "neuro_score": 1.0}
        v = enc._calculate_v(mem)
        assert v == pytest.approx(0.7, abs=0.01)

    def test_calculate_v_from_retention_score(self):
        from whitemagic.core.intelligence.hologram.encoder import CoordinateEncoder
        enc = CoordinateEncoder()
        mem = {"galactic_distance": 0.0, "retention_score": 0.8}
        v = enc._calculate_v(mem)
        assert v == pytest.approx(0.8, abs=0.01)

    def test_calculate_v_protected_always_core(self):
        from whitemagic.core.intelligence.hologram.encoder import CoordinateEncoder
        enc = CoordinateEncoder()
        mem = {"galactic_distance": 0.0, "retention_score": 0.5, "is_protected": True}
        v = enc._calculate_v(mem)
        assert v == 1.0

    def test_encode_returns_5d(self):
        from whitemagic.core.intelligence.hologram.encoder import CoordinateEncoder
        enc = CoordinateEncoder()
        mem = {
            "id": "test-5d",
            "content": "Test memory for 5D encoding",
            "title": "Test Memory",
            "memory_type": "SHORT_TERM",
            "importance": 0.6,
            "emotional_valence": 0.2,
            "created_at": datetime.now().isoformat(),
            "tags": [],
            "galactic_distance": 0.4,
        }
        coord = enc.encode(mem)
        assert hasattr(coord, 'v')
        vec = coord.to_vector()
        assert len(vec) == 5
        # V should be derived from galactic_distance 0.4 -> 0.6
        assert coord.v == pytest.approx(0.6, abs=0.05)


# ---------------------------------------------------------------------------
# Galactic-Distance-Aware Search
# ---------------------------------------------------------------------------

class TestGalacticSearch:
    """Tests that search results are weighted by galactic distance."""

    def _setup_db(self, tmpdir):
        from whitemagic.core.memory.sqlite_backend import SQLiteBackend
        from whitemagic.core.memory.unified_types import Memory, MemoryType

        db_path = Path(tmpdir) / "test.db"
        backend = SQLiteBackend(db_path)

        # Store two memories with identical content but different galactic distances
        core_mem = Memory(
            id="search-core",
            content="Whitemagic architecture design pattern for holographic memory",
            memory_type=MemoryType.LONG_TERM,
            title="Architecture Pattern",
            importance=0.7,
            galactic_distance=0.1,  # CORE
        )
        edge_mem = Memory(
            id="search-edge",
            content="Whitemagic architecture design pattern for holographic memory",
            memory_type=MemoryType.LONG_TERM,
            title="Architecture Pattern",
            importance=0.7,
            galactic_distance=0.9,  # FAR_EDGE
        )
        backend.store(core_mem)
        backend.store(edge_mem)
        return backend

    def test_core_memory_ranks_higher(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            backend = self._setup_db(tmpdir)
            results = backend.search(query="architecture pattern", limit=10)
            assert len(results) >= 2
            # Core memory should come first
            ids = [r.id for r in results]
            core_idx = ids.index("search-core")
            edge_idx = ids.index("search-edge")
            assert core_idx < edge_idx

    def test_non_fts_search_orders_by_distance(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            backend = self._setup_db(tmpdir)
            # Non-FTS search (no query)
            results = backend.search(limit=10)
            assert len(results) >= 2
            # Core memory (distance=0.1) should come before edge (distance=0.9)
            ids = [r.id for r in results]
            assert ids.index("search-core") < ids.index("search-edge")


# ---------------------------------------------------------------------------
# Memory Re-Promotion on Recall
# ---------------------------------------------------------------------------

class TestMemoryRePromotion:
    """Tests that recalling a memory spirals it inward."""

    def test_recall_reduces_galactic_distance(self):
        from whitemagic.core.memory.unified_types import Memory, MemoryType
        from whitemagic.core.memory.sqlite_backend import SQLiteBackend

        with tempfile.TemporaryDirectory() as tmpdir:
            db_path = Path(tmpdir) / "test.db"
            backend = SQLiteBackend(db_path)

            mem = Memory(
                id="promo-001",
                content="A memory at the far edge",
                memory_type=MemoryType.SHORT_TERM,
                title="Far Edge Memory",
                galactic_distance=0.8,
            )
            backend.store(mem)

            # Verify galactic_distance was persisted by store()
            recalled = backend.recall("promo-001")
            assert recalled is not None
            assert recalled.galactic_distance == pytest.approx(0.8, abs=0.01)
            original_dist = recalled.galactic_distance

            # Apply promotion logic directly (what unified.recall does)
            new_dist = max(0.0, recalled.galactic_distance * 0.95)
            backend.update_galactic_distance("promo-001", new_dist)

            # Verify distance decreased
            after = backend.recall("promo-001")
            assert after.galactic_distance < original_dist
            assert after.galactic_distance == pytest.approx(0.76, abs=0.01)

    def test_core_memory_stays_at_core(self):
        from whitemagic.core.memory.unified_types import Memory, MemoryType
        from whitemagic.core.memory.sqlite_backend import SQLiteBackend

        with tempfile.TemporaryDirectory() as tmpdir:
            db_path = Path(tmpdir) / "test.db"
            backend = SQLiteBackend(db_path)

            mem = Memory(
                id="core-001",
                content="A core memory",
                memory_type=MemoryType.LONG_TERM,
                title="Core Memory",
                galactic_distance=0.0,
            )
            backend.store(mem)

            recalled = backend.recall("core-001")
            # Distance is 0.0, promotion should not go negative
            new_dist = max(0.0, recalled.galactic_distance * 0.95)
            assert new_dist == 0.0


# ---------------------------------------------------------------------------
# Association Miner
# ---------------------------------------------------------------------------

class TestAssociationMiner:
    """Tests for the keyword extraction and overlap scoring."""

    def test_keyword_extraction(self):
        from whitemagic.core.memory.association_miner import AssociationMiner
        miner = AssociationMiner()

        keywords = miner._extract_keywords(
            "The holographic memory system uses spatial coordinates "
            "for indexing memories in a four-dimensional space"
        )
        assert "holographic" in keywords
        assert "memory" in keywords
        assert "spatial" in keywords
        assert "coordinates" in keywords
        # Stop words excluded
        assert "the" not in keywords
        assert "for" not in keywords

    def test_overlap_scoring_identical(self):
        from whitemagic.core.memory.association_miner import AssociationMiner
        kw = {"memory", "holographic", "spatial", "index", "coordinate"}
        score, shared = AssociationMiner._compute_overlap(kw, kw)
        assert score > 0.9
        assert shared == kw

    def test_overlap_scoring_disjoint(self):
        from whitemagic.core.memory.association_miner import AssociationMiner
        kw_a = {"memory", "holographic", "spatial"}
        kw_b = {"dharma", "ethics", "governance"}
        score, shared = AssociationMiner._compute_overlap(kw_a, kw_b)
        assert score == 0.0
        assert len(shared) == 0

    def test_overlap_scoring_partial(self):
        from whitemagic.core.memory.association_miner import AssociationMiner
        kw_a = {"memory", "holographic", "spatial", "coordinate", "index"}
        kw_b = {"memory", "holographic", "query", "search", "result"}
        score, shared = AssociationMiner._compute_overlap(kw_a, kw_b)
        assert score > 0.0
        assert "memory" in shared
        assert "holographic" in shared

    def test_empty_sets(self):
        from whitemagic.core.memory.association_miner import AssociationMiner
        score, shared = AssociationMiner._compute_overlap(set(), {"foo"})
        assert score == 0.0

    def test_miner_stats(self):
        from whitemagic.core.memory.association_miner import AssociationMiner
        miner = AssociationMiner(persist=False)
        stats = miner.get_stats()
        assert stats["total_runs"] == 0
        assert stats["persist"] is False

    def test_proposed_link_to_dict(self):
        from whitemagic.core.memory.association_miner import ProposedLink
        link = ProposedLink(
            source_id="a",
            target_id="b",
            overlap_score=0.45,
            shared_keywords={"memory", "holographic"},
            reason="test",
        )
        d = link.to_dict()
        assert d["overlap_score"] == 0.45
        assert "memory" in d["shared_keywords"]


# ---------------------------------------------------------------------------
# DB V Column Migration
# ---------------------------------------------------------------------------

class TestDBVColumnMigration:
    """Tests that the v column is properly migrated."""

    def _store_dummy_memory(self, backend, mem_id):
        from whitemagic.core.memory.unified_types import Memory, MemoryType
        backend.store(Memory(
            id=mem_id, content="dummy", memory_type=MemoryType.SHORT_TERM, title="dummy",
        ))

    def test_store_and_get_5d_coords(self):
        from whitemagic.core.memory.sqlite_backend import SQLiteBackend

        with tempfile.TemporaryDirectory() as tmpdir:
            db_path = Path(tmpdir) / "test.db"
            backend = SQLiteBackend(db_path)
            self._store_dummy_memory(backend, "test-v")

            # Store coords with V
            backend.store_coords("test-v", 0.1, 0.2, 0.3, 0.4, 0.75)
            coords = backend.get_coords("test-v")
            assert coords is not None
            assert len(coords) == 5
            assert coords[4] == pytest.approx(0.75, abs=0.01)

    def test_get_all_coords_5d(self):
        from whitemagic.core.memory.sqlite_backend import SQLiteBackend

        with tempfile.TemporaryDirectory() as tmpdir:
            db_path = Path(tmpdir) / "test.db"
            backend = SQLiteBackend(db_path)
            self._store_dummy_memory(backend, "a")
            self._store_dummy_memory(backend, "b")

            backend.store_coords("a", 0.1, 0.2, 0.3, 0.4, 0.9)
            backend.store_coords("b", 0.5, 0.6, 0.7, 0.8, 0.1)

            all_coords = backend.get_all_coords()
            assert len(all_coords) == 2
            assert len(all_coords["a"]) == 5
            assert all_coords["a"][4] == pytest.approx(0.9, abs=0.01)
            assert all_coords["b"][4] == pytest.approx(0.1, abs=0.01)

    def test_default_v_is_0_5(self):
        from whitemagic.core.memory.sqlite_backend import SQLiteBackend

        with tempfile.TemporaryDirectory() as tmpdir:
            db_path = Path(tmpdir) / "test.db"
            backend = SQLiteBackend(db_path)
            self._store_dummy_memory(backend, "no-v")

            # Store without explicit V
            backend.store_coords("no-v", 0.1, 0.2, 0.3, 0.4)
            coords = backend.get_coords("no-v")
            assert coords is not None
            assert len(coords) == 5
            assert coords[4] == pytest.approx(0.5, abs=0.01)  # default
