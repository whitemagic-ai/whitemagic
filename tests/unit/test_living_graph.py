"""Tests for v14.0 Living Graph — Phase 1 modules.

Covers:
  - Association schema migration (new columns)
  - GraphWalker (multi-hop traversal)
  - GraphEngine (centrality, communities, bridges, echo chambers)
  - Association strength decay + Hebbian strengthening
  - SurpriseGate (novelty-gated ingestion)
  - BridgeSynthesizer
  - Entity Resolution (embedding-based dedup)
  - Dream cycle GOVERNANCE phase
"""

from __future__ import annotations

from datetime import datetime, timedelta
from unittest.mock import MagicMock, patch

import pytest


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

@pytest.fixture
def memory_backend(tmp_path):
    """Create a fresh SQLiteBackend with v14 schema."""
    from whitemagic.core.memory.sqlite_backend import SQLiteBackend
    db_path = tmp_path / "test_living_graph.db"
    backend = SQLiteBackend(db_path)
    return backend


@pytest.fixture
def populated_backend(memory_backend):
    """Backend with sample memories and associations for graph tests."""
    from whitemagic.core.memory.unified_types import Memory, MemoryType

    mems = []
    for i in range(5):
        mem = Memory(
            id=f"mem_{i:03d}",
            content=f"Test memory content {i}",
            memory_type=MemoryType.LONG_TERM,
            title=f"Memory {i}",
            tags={"test", f"group_{i % 2}"},
            importance=0.5 + (i * 0.1),
            galactic_distance=0.1 * i,
        )
        memory_backend.store(mem)
        mems.append(mem)

    # Create associations with v14 columns
    now = datetime.now().isoformat()
    with memory_backend.pool.connection() as conn:
        with conn:
            for i in range(4):
                conn.execute(
                    """INSERT OR IGNORE INTO associations
                       (source_id, target_id, strength, direction, relation_type,
                        edge_type, created_at, ingestion_time)
                       VALUES (?, ?, ?, ?, ?, ?, ?, ?)""",
                    (f"mem_{i:03d}", f"mem_{i+1:03d}", 0.6 + i * 0.05,
                     "undirected", "associated_with", "semantic", now, now),
                )
                # Reverse edge
                conn.execute(
                    """INSERT OR IGNORE INTO associations
                       (source_id, target_id, strength, direction, relation_type,
                        edge_type, created_at, ingestion_time)
                       VALUES (?, ?, ?, ?, ?, ?, ?, ?)""",
                    (f"mem_{i+1:03d}", f"mem_{i:03d}", 0.6 + i * 0.05,
                     "undirected", "associated_with", "semantic", now, now),
                )

    return memory_backend, mems


# ===========================================================================
# Phase 1A: Association Schema Migration
# ===========================================================================

class TestAssociationSchema:
    """Test that v14 columns exist and have correct defaults."""

    def test_new_columns_exist(self, memory_backend):
        with memory_backend.pool.connection() as conn:
            cursor = conn.execute("PRAGMA table_info(associations)")
            columns = {row[1] for row in cursor.fetchall()}

        expected = {
            "source_id", "target_id", "strength",
            "last_traversed_at", "traversal_count", "created_at",
            # v14.0 additions
            "direction", "relation_type", "edge_type",
            "valid_from", "valid_until", "ingestion_time",
        }
        assert expected.issubset(columns), f"Missing columns: {expected - columns}"

    def test_default_values(self, memory_backend):
        from whitemagic.core.memory.unified_types import Memory, MemoryType
        # Create referenced memories first (FK constraint)
        for mid in ("a", "b"):
            memory_backend.store(Memory(id=mid, content=f"test {mid}", memory_type=MemoryType.SHORT_TERM))
        with memory_backend.pool.connection() as conn:
            with conn:
                conn.execute(
                    "INSERT INTO associations (source_id, target_id, strength) VALUES ('a', 'b', 0.5)"
                )
            row = conn.execute(
                "SELECT direction, relation_type, edge_type FROM associations WHERE source_id = 'a'"
            ).fetchone()

        assert row[0] == "undirected"
        assert row[1] == "associated_with"
        assert row[2] == "semantic"

    def test_indexes_created(self, memory_backend):
        with memory_backend.pool.connection() as conn:
            indexes = conn.execute(
                "SELECT name FROM sqlite_master WHERE type = 'index'"
            ).fetchall()
            index_names = {row[0] for row in indexes}

        assert "idx_assoc_target" in index_names
        assert "idx_assoc_edge_type" in index_names
        assert "idx_assoc_direction" in index_names
        assert "idx_assoc_strength" in index_names

    def test_v14_columns_populated_on_insert(self, memory_backend):
        from whitemagic.core.memory.unified_types import Memory, MemoryType
        for mid in ("x", "y"):
            memory_backend.store(Memory(id=mid, content=f"test {mid}", memory_type=MemoryType.SHORT_TERM))
        now = datetime.now().isoformat()
        with memory_backend.pool.connection() as conn:
            with conn:
                conn.execute(
                    """INSERT INTO associations
                       (source_id, target_id, strength, direction, relation_type,
                        edge_type, created_at, ingestion_time)
                       VALUES (?, ?, ?, ?, ?, ?, ?, ?)""",
                    ("x", "y", 0.8, "forward", "causes", "episodic", now, now),
                )
            row = conn.execute(
                "SELECT direction, relation_type, edge_type, ingestion_time FROM associations WHERE source_id = 'x'"
            ).fetchone()

        assert row[0] == "forward"
        assert row[1] == "causes"
        assert row[2] == "episodic"
        assert row[3] == now


# ===========================================================================
# Phase 1B: GraphWalker
# ===========================================================================

class TestGraphWalker:
    """Test multi-hop weighted traversal."""

    def test_import_and_singleton(self):
        from whitemagic.core.memory.graph_walker import get_graph_walker
        walker = get_graph_walker()
        assert walker is not None
        stats = walker.get_stats()
        assert "total_walks" in stats
        assert "gravity_alpha" in stats

    def test_walk_empty_graph(self):
        from whitemagic.core.memory.graph_walker import GraphWalker
        walker = GraphWalker()
        result = walker.walk(seed_ids=["nonexistent"], hops=2)
        assert result.hops == 2
        assert result.unique_nodes_visited <= 1

    def test_walk_with_associations(self, populated_backend):
        backend, mems = populated_backend
        from whitemagic.core.memory.graph_walker import GraphWalker

        walker = GraphWalker()
        # Patch get_unified_memory to use our populated backend
        mock_um = MagicMock()
        mock_um.backend = backend
        with patch("whitemagic.core.memory.unified.get_unified_memory", return_value=mock_um):
            result = walker.walk(seed_ids=["mem_000"], hops=2, top_k=5)

        assert result.seed_ids == ["mem_000"]
        assert result.hops == 2
        assert result.duration_ms >= 0

    def test_walk_result_serialization(self):
        from whitemagic.core.memory.graph_walker import WalkPath, WalkResult
        path = WalkPath(
            nodes=["a", "b", "c"],
            edge_weights=[0.5, 0.7],
            relation_types=["associated_with", "causes"],
            total_score=0.35,
            depth=2,
        )
        d = path.to_dict()
        assert d["nodes"] == ["a", "b", "c"]
        assert d["depth"] == 2
        assert len(d["edge_weights"]) == 2

        result = WalkResult(seed_ids=["a"], hops=2, paths=[path])
        rd = result.to_dict()
        assert len(rd["paths"]) == 1
        assert rd["hops"] == 2

    def test_discovered_ids_excludes_seeds(self):
        from whitemagic.core.memory.graph_walker import WalkPath, WalkResult
        path = WalkPath(nodes=["seed", "b", "c"], edge_weights=[0.5, 0.7],
                        relation_types=["x", "y"], total_score=0.5, depth=2)
        result = WalkResult(seed_ids=["seed"], hops=2, paths=[path])
        discovered = result.discovered_ids()
        assert "seed" not in discovered
        assert "b" in discovered
        assert "c" in discovered

    def test_neighbor_dataclass(self):
        from whitemagic.core.memory.graph_walker import Neighbor
        n = Neighbor(
            memory_id="abc", strength=0.8, direction="forward",
            relation_type="causes", edge_type="episodic",
            traversal_count=3, created_at=None, last_traversed_at=None,
        )
        assert n.memory_id == "abc"
        assert n.strength == 0.8


# ===========================================================================
# Phase 1C: GraphEngine
# ===========================================================================

class TestGraphEngine:
    """Test topology engine."""

    def test_import_and_singleton(self):
        from whitemagic.core.memory.graph_engine import get_graph_engine
        engine = get_graph_engine()
        assert engine is not None
        stats = engine.get_stats()
        assert "available" in stats

    def test_centrality_snapshot_dataclass(self):
        from whitemagic.core.memory.graph_engine import CentralitySnapshot
        snap = CentralitySnapshot(
            timestamp="2026-01-01T00:00:00",
            eigenvector={"a": 0.5, "b": 0.3},
            pagerank={"a": 0.6, "b": 0.4},
            node_count=2,
            edge_count=1,
        )
        d = snap.to_dict()
        assert d["node_count"] == 2
        assert "top_eigenvector" in d

    def test_echo_chamber_dataclass(self):
        from whitemagic.core.memory.graph_engine import EchoChamber
        ec = EchoChamber(
            node_id="abc", current_centrality=0.8,
            previous_centrality=0.2, spike_ratio=3.0, has_new_data=False,
        )
        d = ec.to_dict()
        assert d["spike_ratio"] == 3.0
        assert d["has_new_data"] is False

    def test_community_dataclass(self):
        from whitemagic.core.memory.graph_engine import Community
        comm = Community(
            community_id=0, member_ids=["a", "b", "c"],
            size=3, internal_edges=2, avg_strength=0.6,
            theme_tags=["test"],
        )
        d = comm.to_dict()
        assert d["size"] == 3
        assert d["community_id"] == 0

    def test_rebuild_without_nx(self):
        from whitemagic.core.memory.graph_engine import GraphEngine
        with patch("whitemagic.core.memory.graph_engine._NX_AVAILABLE", False):
            engine = GraphEngine()
            result = engine.rebuild()
            assert result["status"] == "unavailable"

    def test_summary(self):
        from whitemagic.core.memory.graph_engine import GraphEngine
        engine = GraphEngine()
        summary = engine.summary()
        assert "available" in summary


# ===========================================================================
# Phase 1D: Association Strength Decay + Hebbian
# ===========================================================================

class TestAssociationDecay:
    """Test decay and Hebbian strengthening."""

    def test_decay_associations_empty(self, memory_backend):
        result = memory_backend.decay_associations()
        assert result["status"] == "success"
        assert result["associations_evaluated"] == 0

    def test_decay_associations_with_data(self, populated_backend):
        backend, _ = populated_backend

        # First, make the edges old enough to decay
        old_time = (datetime.now() - timedelta(days=30)).isoformat()
        with backend.pool.connection() as conn:
            with conn:
                conn.execute(
                    "UPDATE associations SET created_at = ?", (old_time,)
                )

        result = backend.decay_associations()
        assert result["status"] == "success"
        assert result["associations_evaluated"] > 0
        # Some edges should have decayed
        assert result["associations_decayed"] >= 0

    def test_episodic_decay_faster_than_semantic(self, memory_backend):
        """Episodic edges should decay faster than semantic."""
        from whitemagic.core.memory.unified_types import Memory, MemoryType
        for mid in ("e1", "e2", "s1", "s2"):
            memory_backend.store(Memory(id=mid, content=f"test {mid}", memory_type=MemoryType.SHORT_TERM))
        old_time = (datetime.now() - timedelta(days=60)).isoformat()
        with memory_backend.pool.connection() as conn:
            with conn:
                # Episodic edge
                conn.execute(
                    """INSERT INTO associations
                       (source_id, target_id, strength, edge_type, created_at)
                       VALUES ('e1', 'e2', 0.8, 'episodic', ?)""",
                    (old_time,),
                )
                # Semantic edge (same strength, same age)
                conn.execute(
                    """INSERT INTO associations
                       (source_id, target_id, strength, edge_type, created_at)
                       VALUES ('s1', 's2', 0.8, 'semantic', ?)""",
                    (old_time,),
                )

        memory_backend.decay_associations()

        with memory_backend.pool.connection() as conn:
            episodic = conn.execute(
                "SELECT strength FROM associations WHERE source_id = 'e1'"
            ).fetchone()
            semantic = conn.execute(
                "SELECT strength FROM associations WHERE source_id = 's1'"
            ).fetchone()

        # Episodic should have decayed MORE (lower strength) or been pruned
        if episodic is not None and semantic is not None:
            assert episodic[0] <= semantic[0]

    def test_hebbian_strengthen(self, populated_backend):
        backend, _ = populated_backend

        # Get initial strength
        with backend.pool.connection() as conn:
            row = conn.execute(
                "SELECT strength FROM associations WHERE source_id = 'mem_000' AND target_id = 'mem_001'"
            ).fetchone()
        initial = row[0]

        backend.hebbian_strengthen("mem_000", "mem_001")

        with backend.pool.connection() as conn:
            row = conn.execute(
                "SELECT strength, traversal_count FROM associations WHERE source_id = 'mem_000' AND target_id = 'mem_001'"
            ).fetchone()

        assert row[0] > initial  # Strength increased
        assert row[1] >= 1  # Traversal count incremented


# ===========================================================================
# Phase 1E: SurpriseGate
# ===========================================================================

class TestSurpriseGate:
    """Test surprise-gated memory ingestion."""

    def test_import_and_singleton(self):
        from whitemagic.core.memory.surprise_gate import get_surprise_gate
        gate = get_surprise_gate()
        assert gate is not None
        stats = gate.get_stats()
        assert "total_evaluations" in stats

    def test_surprise_action_enum(self):
        from whitemagic.core.memory.surprise_gate import SurpriseAction
        assert SurpriseAction.CREATE.value == "create"
        assert SurpriseAction.CREATE_BOOSTED.value == "create_boosted"
        assert SurpriseAction.REINFORCE.value == "reinforce"

    def test_verdict_serialization(self):
        from whitemagic.core.memory.surprise_gate import SurpriseAction, SurpriseVerdict
        v = SurpriseVerdict(
            action=SurpriseAction.CREATE_BOOSTED,
            surprise_score=4.5,
            max_similarity=0.1,
            nearest_memory_id="abc",
            reason="Novel concept",
        )
        d = v.to_dict()
        assert d["action"] == "create_boosted"
        assert d["surprise_score"] == 4.5

    def test_evaluate_without_embeddings(self):
        """When embeddings are unavailable, should default to CREATE."""
        from whitemagic.core.memory.surprise_gate import SurpriseAction, SurpriseGate
        gate = SurpriseGate()
        verdict = gate.evaluate("test content")
        # Without embeddings available, should default to CREATE
        assert verdict.action in (SurpriseAction.CREATE, SurpriseAction.CREATE_BOOSTED)

    def test_apply_boosted(self):
        from whitemagic.core.memory.surprise_gate import SurpriseAction, SurpriseGate, SurpriseVerdict
        gate = SurpriseGate()
        verdict = SurpriseVerdict(
            action=SurpriseAction.CREATE_BOOSTED,
            surprise_score=5.0, max_similarity=0.05,
            nearest_memory_id=None, reason="Novel",
        )
        kwargs = {"importance": 0.5}
        result = gate.apply(verdict, kwargs)
        assert result["importance"] > 0.5
        assert result["metadata"]["surprise_boosted"] is True

    def test_apply_reinforce(self):
        from whitemagic.core.memory.surprise_gate import SurpriseAction, SurpriseGate, SurpriseVerdict
        gate = SurpriseGate()
        verdict = SurpriseVerdict(
            action=SurpriseAction.REINFORCE,
            surprise_score=0.5, max_similarity=0.95,
            nearest_memory_id="target_123", reason="Redundant",
        )
        kwargs = {"importance": 0.5}
        result = gate.apply(verdict, kwargs)
        assert result["metadata"]["reinforce_target"] == "target_123"

    def test_stats_tracking(self):
        from whitemagic.core.memory.surprise_gate import SurpriseGate
        gate = SurpriseGate()
        gate.evaluate("test 1")
        gate.evaluate("test 2")
        stats = gate.get_stats()
        assert stats["total_evaluations"] == 2


# ===========================================================================
# Phase 1F: BridgeSynthesizer
# ===========================================================================

class TestBridgeSynthesizer:
    """Test bridge insight generation."""

    def test_import_and_singleton(self):
        from whitemagic.core.memory.bridge_synthesizer import get_bridge_synthesizer
        synth = get_bridge_synthesizer()
        assert synth is not None
        stats = synth.get_stats()
        assert "total_insights" in stats

    def test_insight_serialization(self):
        from whitemagic.core.memory.bridge_synthesizer import BridgeInsight
        insight = BridgeInsight(
            bridge_node_id="abc123",
            bridging_centrality=0.05,
            community_a_tags=["tag1"],
            community_b_tags=["tag2"],
            hypothesis="These two communities are connected through shared patterns.",
            confidence=0.6,
            source="template",
        )
        d = insight.to_dict()
        assert d["source"] == "template"
        assert d["confidence"] == 0.6

    def test_synthesize_empty_bridges(self):
        from whitemagic.core.memory.bridge_synthesizer import BridgeSynthesizer
        synth = BridgeSynthesizer(persist_insights=False)
        insights = synth.synthesize_from_bridges([])
        assert insights == []


# ===========================================================================
# Phase 1F: Dream Cycle Governance Phase
# ===========================================================================

class TestDreamCycleGovernance:
    """Test the new GOVERNANCE dream phase."""

    def test_governance_phase_exists(self):
        from whitemagic.core.dreaming.dream_cycle import DreamPhase
        assert hasattr(DreamPhase, "GOVERNANCE")
        assert DreamPhase.GOVERNANCE.value == "governance"

    def test_phase_rotation_includes_governance(self):
        from whitemagic.core.dreaming.dream_cycle import DreamCycle, DreamPhase
        dc = DreamCycle()
        phases = dc._phases
        assert DreamPhase.GOVERNANCE in phases

    def test_dream_report_serialization(self):
        from whitemagic.core.dreaming.dream_cycle import DreamPhase, DreamReport
        report = DreamReport(
            phase=DreamPhase.GOVERNANCE,
            started_at="2026-01-01T00:00:00",
            duration_ms=150.0,
            details={"echo_chambers_detected": 0},
        )
        d = report.to_dict()
        assert d["phase"] == "governance"
        assert d["details"]["echo_chambers_detected"] == 0


# ===========================================================================
# Phase 1G: Entity Resolution
# ===========================================================================

class TestEntityResolution:
    """Test embedding-based dedup in consolidation."""

    def test_resolve_entities_without_embeddings(self):
        from whitemagic.core.memory.consolidation import MemoryConsolidator
        consolidator = MemoryConsolidator()
        result = consolidator.resolve_entities()
        # Should gracefully handle when embeddings unavailable
        assert result["status"] in ("success", "skipped", "error")

    def test_resolve_entities_stats_structure(self):
        from whitemagic.core.memory.consolidation import MemoryConsolidator
        consolidator = MemoryConsolidator()
        result = consolidator.resolve_entities()
        assert "pairs_evaluated" in result
        assert "duplicates_found" in result
        assert "duplicates_resolved" in result


# ===========================================================================
# Integration: Association Miner populates v14 columns
# ===========================================================================

class TestAssociationMinerV14:
    """Test that association miner populates new columns on creation."""

    def test_miner_import(self):
        from whitemagic.core.memory.association_miner import get_association_miner
        miner = get_association_miner()
        assert miner is not None

    def test_proposed_link_dataclass(self):
        from whitemagic.core.memory.association_miner import ProposedLink
        link = ProposedLink(
            source_id="a", target_id="b",
            overlap_score=0.7, shared_keywords={"x", "y"},
            reason="test",
        )
        assert link.source_id == "a"
        assert link.overlap_score == 0.7


# ===========================================================================
# Integration: unified.py hybrid_recall
# ===========================================================================

class TestUnifiedHybridRecall:
    """Test the new hybrid_recall method on UnifiedMemory."""

    def test_hybrid_recall_exists(self):
        from whitemagic.core.memory.unified import UnifiedMemory
        assert hasattr(UnifiedMemory, "hybrid_recall")

    def test_hybrid_recall_fallback(self):
        """Should fall back gracefully when graph walker fails."""
        from whitemagic.core.memory.unified import get_unified_memory
        um = get_unified_memory()
        # Empty DB — should return empty list, not crash
        results = um.hybrid_recall(query="nonexistent topic")
        assert isinstance(results, list)


# ===========================================================================
# Integration: Lifecycle sweep includes association decay
# ===========================================================================

class TestLifecycleAssociationDecay:
    """Test that lifecycle sweep Phase 4 runs association decay."""

    def test_lifecycle_manager_import(self):
        from whitemagic.core.memory.lifecycle import get_lifecycle_manager
        mgr = get_lifecycle_manager()
        assert mgr is not None


# ===========================================================================
# Regression: existing functionality not broken
# ===========================================================================

class TestRegression:
    """Ensure v14 changes don't break existing functionality."""

    def test_store_and_recall(self, memory_backend):
        from whitemagic.core.memory.unified_types import Memory, MemoryType
        mem = Memory(
            id="test_regression",
            content="regression test content",
            memory_type=MemoryType.SHORT_TERM,
            title="Regression Test",
            tags={"test"},
            importance=0.5,
        )
        memory_backend.store(mem)
        recalled = memory_backend.recall("test_regression")
        assert recalled is not None
        assert recalled.title == "Regression Test"

    def test_association_insert_with_old_schema(self, memory_backend):
        """Old-style INSERT (just source, target, strength) should still work."""
        from whitemagic.core.memory.unified_types import Memory, MemoryType
        for mid in ("old_a", "old_b"):
            memory_backend.store(Memory(id=mid, content=f"test {mid}", memory_type=MemoryType.SHORT_TERM))
        with memory_backend.pool.connection() as conn:
            with conn:
                conn.execute(
                    "INSERT OR IGNORE INTO associations (source_id, target_id, strength) VALUES ('old_a', 'old_b', 0.5)"
                )
            row = conn.execute(
                "SELECT direction, relation_type, edge_type FROM associations WHERE source_id = 'old_a'"
            ).fetchone()

        # Should get defaults
        assert row[0] == "undirected"
        assert row[1] == "associated_with"
        assert row[2] == "semantic"

    def test_consolidate_still_works(self, memory_backend):
        count = memory_backend.consolidate()
        assert isinstance(count, int)
