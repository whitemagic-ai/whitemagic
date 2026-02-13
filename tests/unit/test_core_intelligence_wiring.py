"""Tests for Holographic Core Intelligence Wiring.

Tests the CoreAccessLayer, InsightPipeline, and engine integrations.
All tests use in-memory SQLite databases to avoid touching production data.
"""

from __future__ import annotations

import sqlite3
from datetime import datetime, timedelta
from importlib.util import find_spec
from unittest.mock import MagicMock, patch

import pytest

_has_numpy = find_spec("numpy") is not None


# ---------------------------------------------------------------------------
# Fixtures: in-memory DB with schema
# ---------------------------------------------------------------------------

@pytest.fixture
def mem_db():
    """Create an in-memory SQLite database with the WM schema."""
    conn = sqlite3.connect(":memory:")
    conn.row_factory = sqlite3.Row
    conn.execute("PRAGMA journal_mode = WAL")

    # Core tables
    conn.execute("""
        CREATE TABLE memories (
            id TEXT PRIMARY KEY,
            title TEXT,
            content TEXT,
            memory_type TEXT DEFAULT 'LONG_TERM',
            importance REAL DEFAULT 0.5,
            access_count INTEGER DEFAULT 0,
            created_at TEXT,
            accessed_at TEXT,
            galactic_distance REAL DEFAULT 0.5
        )
    """)
    conn.execute("""
        CREATE TABLE tags (
            memory_id TEXT,
            tag TEXT,
            FOREIGN KEY (memory_id) REFERENCES memories(id)
        )
    """)
    conn.execute("""
        CREATE TABLE associations (
            source_id TEXT,
            target_id TEXT,
            strength REAL,
            last_traversed_at TEXT,
            traversal_count INTEGER DEFAULT 0,
            created_at TEXT,
            PRIMARY KEY (source_id, target_id),
            FOREIGN KEY (source_id) REFERENCES memories(id),
            FOREIGN KEY (target_id) REFERENCES memories(id)
        )
    """)
    conn.execute("""
        CREATE TABLE holographic_coords (
            memory_id TEXT PRIMARY KEY,
            x REAL, y REAL, z REAL, w REAL, v REAL DEFAULT 0.5,
            FOREIGN KEY (memory_id) REFERENCES memories(id)
        )
    """)

    yield conn
    conn.close()


@pytest.fixture
def populated_db(mem_db):
    """Populate the in-memory DB with test data."""
    now = datetime.now().isoformat()
    week_ago = (datetime.now() - timedelta(days=7)).isoformat()
    month_ago = (datetime.now() - timedelta(days=30)).isoformat()

    # Memories
    memories = [
        ("mem_001", "Rust Acceleration Architecture", "Detailed plan for Rust hot paths", "LONG_TERM", 0.9, 5, now, now, 0.1),
        ("mem_002", "Zig SIMD Holographic Engine", "SIMD distance calculations in Zig", "LONG_TERM", 0.85, 3, week_ago, week_ago, 0.15),
        ("mem_003", "Memory Consolidation Strategy", "Hippocampal replay for memory clusters", "LONG_TERM", 0.8, 2, month_ago, None, 0.2),
        ("mem_004", "Dream Cycle Architecture", "5-phase dream cycle design", "LONG_TERM", 0.75, 1, month_ago, None, 0.3),
        ("mem_005", "Ancient Archive Entry", "Bulk imported file from archive", "LONG_TERM", 0.3, 0, month_ago, None, 0.9),
        ("mem_006", "Elixir OTP Bridge", "GenServer for event bus", "LONG_TERM", 0.7, 0, now, None, 0.25),
        ("mem_007", "Haskell Dharma Rules", "Declarative ethical evaluation", "LONG_TERM", 0.65, 0, now, None, 0.35),
        ("mem_008", "Go Mesh Protocol", "Cross-node memory sync", "LONG_TERM", 0.6, 0, now, None, 0.4),
        ("mem_009", "TypeScript SDK Client", "MCP client for web", "LONG_TERM", 0.55, 0, week_ago, None, 0.5),
        ("mem_010", "Julia Forecast Model", "Holt-Winters time series", "LONG_TERM", 0.5, 0, week_ago, None, 0.55),
    ]
    for m in memories:
        mem_db.execute(
            "INSERT INTO memories (id, title, content, memory_type, importance, access_count, created_at, accessed_at, galactic_distance) "
            "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)", m,
        )

    # Tags
    tags = [
        ("mem_001", "rust"), ("mem_001", "acceleration"), ("mem_001", "architecture"),
        ("mem_002", "zig"), ("mem_002", "simd"), ("mem_002", "holographic"),
        ("mem_003", "memory"), ("mem_003", "consolidation"),
        ("mem_004", "dream"), ("mem_004", "architecture"),
        ("mem_005", "archive"), ("mem_005", "scavenged"),
        ("mem_006", "elixir"), ("mem_006", "otp"),
        ("mem_007", "haskell"), ("mem_007", "dharma"),
        ("mem_008", "go"), ("mem_008", "mesh"),
        ("mem_009", "typescript"), ("mem_009", "sdk"),
        ("mem_010", "julia"), ("mem_010", "forecast"),
    ]
    for t in tags:
        mem_db.execute("INSERT INTO tags (memory_id, tag) VALUES (?, ?)", t)

    # Holographic coordinates (spread across 5D space)
    coords = [
        ("mem_001", -0.5, -0.3, 0.2, 0.9, 0.8),
        ("mem_002", -0.4, -0.2, 0.1, 0.85, 0.75),
        ("mem_003", 0.1, 0.3, -0.2, 0.8, 0.6),
        ("mem_004", 0.2, 0.4, -0.3, 0.75, 0.5),
        ("mem_005", 0.5, 0.6, -0.8, 0.3, 0.1),
        ("mem_006", -0.3, -0.1, 0.3, 0.7, 0.65),
        ("mem_007", 0.3, 0.2, 0.0, 0.65, 0.55),
        ("mem_008", -0.2, 0.0, 0.4, 0.6, 0.5),
        ("mem_009", 0.0, -0.4, 0.1, 0.55, 0.45),
        ("mem_010", 0.1, -0.5, -0.1, 0.5, 0.4),
    ]
    for c in coords:
        mem_db.execute(
            "INSERT INTO holographic_coords (memory_id, x, y, z, w, v) VALUES (?, ?, ?, ?, ?, ?)", c,
        )

    # Associations (form a small graph)
    assocs = [
        ("mem_001", "mem_002", 0.8),  # Rust ↔ Zig (acceleration cluster)
        ("mem_001", "mem_006", 0.6),  # Rust ↔ Elixir
        ("mem_002", "mem_003", 0.5),  # Zig ↔ Consolidation
        ("mem_003", "mem_004", 0.7),  # Consolidation ↔ Dream
        ("mem_004", "mem_005", 0.3),  # Dream ↔ Archive (weak)
        ("mem_006", "mem_007", 0.55), # Elixir ↔ Haskell
        ("mem_008", "mem_009", 0.45), # Go ↔ TypeScript
    ]
    for a in assocs:
        mem_db.execute(
            "INSERT INTO associations (source_id, target_id, strength) VALUES (?, ?, ?)", a,
        )

    mem_db.commit()
    return mem_db


# ---------------------------------------------------------------------------
# CoreAccessLayer tests
# ---------------------------------------------------------------------------

class TestCoreAccessLayer:
    """Test the CoreAccessLayer with an in-memory database."""

    def _make_cal(self, db_conn):
        """Create a CoreAccessLayer wired to the test DB."""
        from whitemagic.core.intelligence.core_access import CoreAccessLayer
        cal = CoreAccessLayer()
        cal._conn = db_conn
        cal._conn_injected = True
        return cal

    def test_query_association_subgraph_basic(self, populated_db):
        cal = self._make_cal(populated_db)
        nodes = cal.query_association_subgraph(["mem_001"], depth=1, min_strength=0.3)

        # mem_001 (seed) + mem_002 (strength 0.8) + mem_006 (strength 0.6)
        assert len(nodes) >= 2
        ids = {n.memory_id for n in nodes}
        assert "mem_001" in ids  # seed
        assert "mem_002" in ids  # direct neighbor

    def test_query_association_subgraph_depth_2(self, populated_db):
        cal = self._make_cal(populated_db)
        nodes = cal.query_association_subgraph(["mem_001"], depth=2, min_strength=0.3)

        # Should reach mem_003 via mem_002 (2 hops)
        ids = {n.memory_id for n in nodes}
        assert "mem_003" in ids

    def test_query_association_subgraph_respects_min_strength(self, populated_db):
        cal = self._make_cal(populated_db)
        nodes = cal.query_association_subgraph(["mem_004"], depth=1, min_strength=0.5)

        # mem_004 → mem_003 (0.7) should pass, but mem_004 → mem_005 (0.3) should not
        ids = {n.memory_id for n in nodes}
        assert "mem_003" in ids
        assert "mem_005" not in ids

    def test_query_association_subgraph_max_nodes(self, populated_db):
        cal = self._make_cal(populated_db)
        nodes = cal.query_association_subgraph(["mem_001"], depth=5, min_strength=0.1, max_nodes=3)
        assert len(nodes) <= 3

    def test_get_association_stats(self, populated_db):
        cal = self._make_cal(populated_db)
        stats = cal.get_association_stats()
        assert stats["total_associations"] == 7
        assert 0 < stats["avg_strength"] < 1

    def test_query_holographic_neighbors(self, populated_db):
        cal = self._make_cal(populated_db)
        # Query near mem_001's coordinates
        neighbors = cal.query_holographic_neighbors(
            coords=(-0.5, -0.3, 0.2, 0.9, 0.8), k=3,
        )
        assert len(neighbors) == 3
        # mem_001 should be closest (distance ~0)
        assert neighbors[0].memory_id == "mem_001"
        assert neighbors[0].distance < 0.01

    def test_query_temporal_activity(self, populated_db):
        cal = self._make_cal(populated_db)
        buckets = cal.query_temporal_activity(time_window="30d", bucket="7d")
        assert len(buckets) > 0
        # All buckets should have period strings
        for b in buckets:
            assert b.period is not None

    def test_get_velocity_metrics(self, populated_db):
        cal = self._make_cal(populated_db)
        metrics = cal.get_velocity_metrics()
        assert metrics["total"] == 10
        assert "acceleration" in metrics

    def test_find_broken_associations(self, populated_db):
        cal = self._make_cal(populated_db)
        # mem_005 has galactic_distance 0.9 and is linked to mem_004 (strength 0.3)
        # But strength < 0.5, so it shouldn't appear
        broken = cal.find_broken_associations(limit=20)
        # Should be empty since our only FAR_EDGE link has strength 0.3 < 0.5
        assert isinstance(broken, list)

    def test_find_association_orphans(self, populated_db):
        cal = self._make_cal(populated_db)
        orphans = cal.find_association_orphans(min_gravity=0.5, limit=10)
        # mem_010 has gravity 0.5 and only 0 direct associations
        assert isinstance(orphans, list)

    def test_record_traversals(self, populated_db):
        """Verify that walking the graph updates traversal_count."""
        cal = self._make_cal(populated_db)
        # Walk from mem_001
        cal.query_association_subgraph(["mem_001"], depth=1, min_strength=0.3)

        # Check traversal_count was incremented
        row = populated_db.execute(
            "SELECT traversal_count, last_traversed_at FROM associations WHERE source_id = 'mem_001' AND target_id = 'mem_002'"
        ).fetchone()
        assert row is not None
        assert row["traversal_count"] >= 1
        assert row["last_traversed_at"] is not None


@pytest.mark.skipif(not _has_numpy, reason="numpy not installed (embeddings module requires it)")
class TestCoreAccessLayerHybridRecall:
    """Test hybrid recall with mocked embedding engine."""

    def _make_cal(self, db_conn):
        from whitemagic.core.intelligence.core_access import CoreAccessLayer
        cal = CoreAccessLayer()
        cal._conn = db_conn
        cal._conn_injected = True
        return cal

    @patch("whitemagic.core.memory.embeddings.get_embedding_engine")
    def test_hybrid_recall_vector_only(self, mock_get_engine, populated_db):
        """When embedding engine works, vector results are returned."""
        mock_engine = MagicMock()
        mock_engine.search_similar.return_value = [
            {"memory_id": "mem_001", "similarity": 0.95},
            {"memory_id": "mem_002", "similarity": 0.85},
        ]
        mock_get_engine.return_value = mock_engine

        cal = self._make_cal(populated_db)
        results = cal.hybrid_recall("rust acceleration", k=5)

        assert len(results) > 0
        # mem_001 and mem_002 should appear (from vector)
        ids = {r.memory_id for r in results}
        assert "mem_001" in ids

    @patch("whitemagic.core.memory.embeddings.get_embedding_engine")
    def test_hybrid_recall_fuses_graph(self, mock_get_engine, populated_db):
        """Vector results seed graph walk, producing combined results."""
        mock_engine = MagicMock()
        mock_engine.search_similar.return_value = [
            {"memory_id": "mem_001", "similarity": 0.9},
        ]
        mock_get_engine.return_value = mock_engine

        cal = self._make_cal(populated_db)
        results = cal.hybrid_recall("rust", k=10, graph_depth=2)

        # Should include graph neighbors of mem_001
        ids = {r.memory_id for r in results}
        # mem_002 is a direct neighbor of mem_001
        assert "mem_002" in ids
        # Check sources annotation
        for r in results:
            if r.memory_id == "mem_002":
                assert "graph" in r.sources

    @patch("whitemagic.core.memory.embeddings.get_embedding_engine")
    def test_hybrid_recall_rrf_scoring(self, mock_get_engine, populated_db):
        """RRF scoring combines vector and graph channels."""
        mock_engine = MagicMock()
        mock_engine.search_similar.return_value = [
            {"memory_id": "mem_001", "similarity": 0.95},
            {"memory_id": "mem_003", "similarity": 0.80},
        ]
        mock_get_engine.return_value = mock_engine

        cal = self._make_cal(populated_db)
        results = cal.hybrid_recall("test query", k=10, graph_depth=2)

        # All results should have positive scores
        for r in results:
            assert r.score > 0

        # Results should be sorted by score descending
        scores = [r.score for r in results]
        assert scores == sorted(scores, reverse=True)


# ---------------------------------------------------------------------------
# InsightPipeline tests
# ---------------------------------------------------------------------------

class TestInsightPipeline:
    """Test the InsightPipeline with mocked engines."""

    def test_pipeline_instantiation(self):
        from whitemagic.core.intelligence.insight_pipeline import InsightPipeline
        pipeline = InsightPipeline(max_items=20)
        assert pipeline._max_items == 20

    @patch("whitemagic.core.intelligence.insight_pipeline.InsightPipeline._run_predictive")
    @patch("whitemagic.core.intelligence.insight_pipeline.InsightPipeline._run_kaizen")
    @patch("whitemagic.core.intelligence.insight_pipeline.InsightPipeline._run_serendipity")
    @patch("whitemagic.core.intelligence.insight_pipeline.InsightPipeline._run_emergence")
    @patch("whitemagic.core.intelligence.insight_pipeline.InsightPipeline._get_core_access")
    def test_generate_briefing_empty(self, mock_cal, mock_emergence, mock_serendipity, mock_kaizen, mock_predictive):
        """Pipeline produces a valid briefing even with no engine results."""
        mock_cal.return_value = None
        mock_predictive.return_value = []
        mock_kaizen.return_value = []
        mock_serendipity.return_value = []
        mock_emergence.return_value = []

        from whitemagic.core.intelligence.insight_pipeline import InsightPipeline
        pipeline = InsightPipeline()
        briefing = pipeline.generate_briefing()

        assert briefing.timestamp is not None
        assert briefing.duration_ms >= 0
        assert len(briefing.items) == 0
        assert briefing.summary["total_insights"] == 0

    @patch("whitemagic.core.intelligence.insight_pipeline.InsightPipeline._run_predictive")
    @patch("whitemagic.core.intelligence.insight_pipeline.InsightPipeline._run_kaizen")
    @patch("whitemagic.core.intelligence.insight_pipeline.InsightPipeline._run_serendipity")
    @patch("whitemagic.core.intelligence.insight_pipeline.InsightPipeline._run_emergence")
    @patch("whitemagic.core.intelligence.insight_pipeline.InsightPipeline._get_core_access")
    def test_generate_briefing_with_items(self, mock_cal, mock_emergence, mock_serendipity, mock_kaizen, mock_predictive):
        """Pipeline correctly synthesizes items from multiple engines."""
        mock_cal.return_value = None

        from whitemagic.core.intelligence.insight_pipeline import BriefingItem
        mock_predictive.return_value = [
            BriefingItem(id="p1", category="prediction", title="Test Prediction",
                        description="A test", priority="high", confidence=0.8,
                        source_engine="predictive"),
        ]
        mock_kaizen.return_value = [
            BriefingItem(id="k1", category="improvement", title="Fix Quality",
                        description="Quality issue", priority="critical", confidence=0.7,
                        source_engine="kaizen"),
        ]
        mock_serendipity.return_value = []
        mock_emergence.return_value = [
            BriefingItem(id="e1", category="emergence", title="Convergence",
                        description="Two constellations merging", priority="medium",
                        confidence=0.6, source_engine="emergence"),
        ]

        from whitemagic.core.intelligence.insight_pipeline import InsightPipeline
        pipeline = InsightPipeline()
        briefing = pipeline.generate_briefing()

        assert len(briefing.items) == 3
        # Critical items first
        assert briefing.items[0].priority == "critical"
        assert briefing.summary["by_engine"]["predictive"] == 1
        assert briefing.summary["by_engine"]["kaizen"] == 1

    def test_briefing_to_dict(self):
        """InsightBriefing.to_dict() produces valid structure."""
        from whitemagic.core.intelligence.insight_pipeline import BriefingItem, InsightBriefing
        briefing = InsightBriefing(
            timestamp="2026-02-11T12:00:00",
            duration_ms=42.5,
            items=[
                BriefingItem(id="t1", category="prediction", title="Test",
                            description="Desc", priority="high", confidence=0.9,
                            source_engine="predictive"),
            ],
            summary={"total_insights": 1, "by_category": {}, "by_priority": {}, "by_engine": {}},
            constellation_context=[],
            velocity_metrics={"total": 100},
        )
        d = briefing.to_dict()
        assert d["total_items"] == 1
        assert d["timestamp"] == "2026-02-11T12:00:00"
        assert d["duration_ms"] == 42.5

    def test_briefing_format_text(self):
        """format_text() produces readable string."""
        from whitemagic.core.intelligence.insight_pipeline import BriefingItem, InsightBriefing
        briefing = InsightBriefing(
            timestamp="2026-02-11T12:00:00",
            duration_ms=42.5,
            items=[
                BriefingItem(id="t1", category="prediction", title="Test Prediction",
                            description="Something important", priority="critical",
                            confidence=0.9, source_engine="predictive",
                            suggested_actions=["Do the thing"]),
            ],
            summary={"total_insights": 1},
            constellation_context=[],
            velocity_metrics={"daily_avg_7d": 5, "acceleration": 1.2},
        )
        text = briefing.format_text()
        assert "Intelligence Briefing" in text
        assert "CRITICAL" in text
        assert "Test Prediction" in text

    def test_deduplication(self):
        """Duplicate items are removed."""
        from whitemagic.core.intelligence.insight_pipeline import BriefingItem, InsightPipeline
        items = [
            BriefingItem(id="dup", category="prediction", title="Dup",
                        description="", priority="high", confidence=0.8,
                        source_engine="predictive"),
            BriefingItem(id="dup", category="prediction", title="Dup Copy",
                        description="", priority="medium", confidence=0.5,
                        source_engine="predictive"),
            BriefingItem(id="unique", category="improvement", title="Unique",
                        description="", priority="low", confidence=0.3,
                        source_engine="kaizen"),
        ]
        deduped = InsightPipeline._deduplicate(items)
        assert len(deduped) == 2

    def test_priority_sorting(self):
        """Items are sorted by priority then confidence."""
        from whitemagic.core.intelligence.insight_pipeline import BriefingItem, InsightPipeline
        items = [
            BriefingItem(id="low", category="a", title="Low", description="",
                        priority="low", confidence=0.9, source_engine="x"),
            BriefingItem(id="crit", category="a", title="Critical", description="",
                        priority="critical", confidence=0.5, source_engine="x"),
            BriefingItem(id="high", category="a", title="High", description="",
                        priority="high", confidence=0.7, source_engine="x"),
        ]
        sorted_items = InsightPipeline._prioritize(items)
        assert sorted_items[0].id == "crit"
        assert sorted_items[1].id == "high"
        assert sorted_items[2].id == "low"


# ---------------------------------------------------------------------------
# EmergenceEngine tests
# ---------------------------------------------------------------------------

class TestEmergenceEngine:
    """Test the enhanced EmergenceEngine."""

    @patch("whitemagic.core.resonance.gan_ying_enhanced.get_bus")
    def test_creation(self, mock_bus):
        mock_bus.return_value = MagicMock()
        from whitemagic.core.intelligence.agentic.emergence_engine import EmergenceEngine
        engine = EmergenceEngine(threshold=3)
        assert engine._threshold == 3
        assert len(engine._insights) == 0

    @patch("whitemagic.core.resonance.gan_ying_enhanced.get_bus")
    def test_get_status(self, mock_bus):
        mock_bus.return_value = MagicMock()
        from whitemagic.core.intelligence.agentic.emergence_engine import EmergenceEngine
        engine = EmergenceEngine()
        status = engine.get_status()
        assert status["active"] is False
        assert status["total_insights"] == 0

    @patch("whitemagic.core.resonance.gan_ying_enhanced.get_bus")
    def test_insight_recording(self, mock_bus):
        mock_bus.return_value = MagicMock()
        from whitemagic.core.intelligence.agentic.emergence_engine import (
            EmergenceEngine,
            EmergenceInsight,
        )
        engine = EmergenceEngine()
        insight = EmergenceInsight(
            id="test_1", title="Test Insight", description="A test",
            source="test", confidence=0.8,
        )
        engine._record_insight(insight)
        assert len(engine._insights) == 1
        assert engine.get_insights()[0]["id"] == "test_1"

    @patch("whitemagic.core.resonance.gan_ying_enhanced.get_bus")
    def test_insight_bounded_history(self, mock_bus):
        mock_bus.return_value = MagicMock()
        from whitemagic.core.intelligence.agentic.emergence_engine import (
            EmergenceEngine,
            EmergenceInsight,
        )
        engine = EmergenceEngine()
        engine._max_insights = 5

        for i in range(10):
            engine._record_insight(EmergenceInsight(
                id=f"insight_{i}", title=f"Insight {i}",
                description="", source="test", confidence=0.5,
            ))

        assert len(engine._insights) == 5
        # Should keep most recent
        assert engine._insights[-1].id == "insight_9"

    @patch("whitemagic.core.resonance.gan_ying_enhanced.get_bus")
    def test_scan_temporal_bursts(self, mock_bus):
        mock_bus.return_value = MagicMock()
        from whitemagic.core.intelligence.agentic.emergence_engine import EmergenceEngine
        from whitemagic.core.intelligence.core_access import TemporalBucket

        engine = EmergenceEngine()

        # Mock CoreAccessLayer
        mock_cal = MagicMock()
        buckets = [
            TemporalBucket(period=f"2026-02-{i:02d}", memories_created=2)
            for i in range(1, 11)
        ]
        # Add a burst day
        buckets.append(TemporalBucket(period="2026-02-11", memories_created=20))
        mock_cal.query_temporal_activity.return_value = buckets

        insights = engine._scan_temporal_bursts(mock_cal)
        assert len(insights) >= 1
        assert insights[0].source == "temporal_burst"


# ---------------------------------------------------------------------------
# Data structure tests
# ---------------------------------------------------------------------------

class TestDataStructures:
    """Test to_dict() methods on all data structures."""

    def test_constellation_context_to_dict(self):
        from whitemagic.core.intelligence.core_access import ConstellationContext
        c = ConstellationContext(
            name="Test Cluster", size=42,
            centroid=(0.1, -0.2, 0.3, 0.8, 0.5),
            dominant_tags=["rust", "simd"], zone="inner_rim", distance=0.123,
        )
        d = c.to_dict()
        assert d["name"] == "Test Cluster"
        assert d["size"] == 42
        assert d["centroid"]["x"] == 0.1
        assert d["distance"] == 0.123

    def test_association_node_to_dict(self):
        from whitemagic.core.intelligence.core_access import AssociationNode
        n = AssociationNode(memory_id="mem_001", title="Test", strength=0.75, depth=2)
        d = n.to_dict()
        assert d["memory_id"] == "mem_001"
        assert d["strength"] == 0.75
        assert d["depth"] == 2

    def test_temporal_bucket_to_dict(self):
        from whitemagic.core.intelligence.core_access import TemporalBucket
        b = TemporalBucket(period="2026-02-11", memories_created=5, memories_accessed=3)
        d = b.to_dict()
        assert d["memories_created"] == 5
        assert d["memories_accessed"] == 3

    def test_holographic_neighbor_to_dict(self):
        from whitemagic.core.intelligence.core_access import HolographicNeighbor
        n = HolographicNeighbor(memory_id="mem_001", title="Test", distance=0.42)
        d = n.to_dict()
        assert d["distance"] == 0.42

    def test_hybrid_result_to_dict(self):
        from whitemagic.core.intelligence.core_access import HybridResult
        r = HybridResult(
            memory_id="mem_001", title="Test",
            content_preview="Some content", score=0.0123,
            sources=["vector", "graph"],
        )
        d = r.to_dict()
        assert d["score"] == 0.0123
        assert d["sources"] == ["vector", "graph"]

    def test_emergence_insight_to_dict(self):
        from whitemagic.core.intelligence.agentic.emergence_engine import EmergenceInsight
        i = EmergenceInsight(
            id="test_1", title="Convergence", description="Two merge",
            source="constellation_convergence", confidence=0.85,
            related_ids=["a", "b"], metadata={"key": "val"},
        )
        d = i.to_dict()
        assert d["source"] == "constellation_convergence"
        assert d["confidence"] == 0.85

    def test_briefing_item_to_dict(self):
        from whitemagic.core.intelligence.insight_pipeline import BriefingItem
        item = BriefingItem(
            id="b1", category="prediction", title="Test",
            description="Desc", priority="high", confidence=0.9,
            source_engine="predictive",
            suggested_actions=["Do X"],
            related_constellations=["Cluster A"],
        )
        d = item.to_dict()
        assert d["priority"] == "high"
        assert d["suggested_actions"] == ["Do X"]
        assert d["related_constellations"] == ["Cluster A"]


# ---------------------------------------------------------------------------
# Singleton tests
# ---------------------------------------------------------------------------

class TestSingletons:
    """Test that singleton getters work."""

    def test_get_core_access(self):
        from whitemagic.core.intelligence.core_access import get_core_access
        cal1 = get_core_access()
        cal2 = get_core_access()
        assert cal1 is cal2

    def test_get_insight_pipeline(self):
        from whitemagic.core.intelligence.insight_pipeline import get_insight_pipeline
        p1 = get_insight_pipeline()
        p2 = get_insight_pipeline()
        assert p1 is p2
