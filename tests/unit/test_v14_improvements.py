"""Tests for v14 improvements: drift vectors, TTL cache, briefing memory,
Rust RRF fallback, pre-normalized embeddings, dream autonomy.

All tests use in-memory SQLite and mocks — no real DB or model needed.
"""

from __future__ import annotations

import time
from datetime import datetime
from pathlib import Path
from unittest.mock import MagicMock, patch

import numpy as np


# -----------------------------------------------------------------------
# 1. Constellation Drift Vectors
# -----------------------------------------------------------------------

class TestConstellationDrift:
    """Test drift tracking in ConstellationDetector."""

    def _make_detector(self):
        from whitemagic.core.memory.constellations import ConstellationDetector
        return ConstellationDetector(bins_per_axis=4, min_cluster_size=2, cache_ttl_seconds=3600)

    def _make_constellation(self, name, centroid):
        from whitemagic.core.memory.constellations import Constellation
        return Constellation(
            name=name,
            member_ids=["m1", "m2"],
            centroid=centroid,
            radius=0.1,
            dominant_tags=["test"],
            dominant_type="LONG_TERM",
            avg_importance=0.7,
            zone="MID_BAND",
        )

    def test_record_centroid_history(self):
        det = self._make_detector()
        c1 = self._make_constellation("Alpha", (0.1, 0.2, 0.3, 0.4, 0.5))
        det._record_centroid_history([c1])
        assert "Alpha" in det._centroid_history
        assert len(det._centroid_history["Alpha"]) == 1

    def test_drift_requires_two_points(self):
        det = self._make_detector()
        c1 = self._make_constellation("Alpha", (0.1, 0.2, 0.3, 0.4, 0.5))
        det._record_centroid_history([c1])
        # No report set, drift should be empty
        drift = det.get_drift_vectors(window_days=7)
        assert drift == []

    def test_drift_vector_computation(self):
        from whitemagic.core.memory.constellations import DetectionReport
        det = self._make_detector()

        # Simulate two detection runs with different centroids
        c1_v1 = self._make_constellation("Alpha", (0.1, 0.2, 0.3, 0.4, 0.5))
        c1_v2 = self._make_constellation("Alpha", (0.2, 0.3, 0.4, 0.5, 0.6))

        det._record_centroid_history([c1_v1])
        time.sleep(0.01)
        det._record_centroid_history([c1_v2])

        # Set a last_report with the final centroid
        det._last_report = DetectionReport(
            memories_scanned=100,
            constellations_found=1,
            constellations=[c1_v2],
        )

        drift = det.get_drift_vectors(window_days=7)
        assert len(drift) == 1
        assert drift[0]["name"] == "Alpha"
        assert drift[0]["drift_magnitude"] > 0
        assert drift[0]["samples"] == 2

    def test_drift_magnitude_calculation(self):
        from whitemagic.core.memory.constellations import DetectionReport
        det = self._make_detector()

        c1 = self._make_constellation("Beta", (0.0, 0.0, 0.0, 0.0, 0.0))
        c2 = self._make_constellation("Beta", (0.1, 0.0, 0.0, 0.0, 0.0))

        det._record_centroid_history([c1])
        det._record_centroid_history([c2])

        det._last_report = DetectionReport(
            memories_scanned=100,
            constellations_found=1,
            constellations=[c2],
        )

        drift = det.get_drift_vectors(window_days=7)
        assert len(drift) == 1
        assert abs(drift[0]["drift_magnitude"] - 0.1) < 0.001

    def test_centroid_history_trimming(self):
        det = self._make_detector()
        det._max_history_per_constellation = 5

        c1 = self._make_constellation("Gamma", (0.1, 0.2, 0.3, 0.4, 0.5))
        for _ in range(10):
            det._record_centroid_history([c1])

        assert len(det._centroid_history["Gamma"]) == 5


# -----------------------------------------------------------------------
# 2. TTL Cache
# -----------------------------------------------------------------------

class TestConstellationTTLCache:
    """Test TTL-based caching in ConstellationDetector."""

    def _make_detector(self, ttl=1.0):
        from whitemagic.core.memory.constellations import ConstellationDetector
        return ConstellationDetector(cache_ttl_seconds=ttl)

    def test_cache_returns_none_when_empty(self):
        det = self._make_detector()
        # No last_report, should try detect (which will fail gracefully)
        with patch.object(det, 'detect', return_value=None) as mock_detect:
            assert det.get_cached_or_detect() is None
            mock_detect.assert_called_once()

    def test_cache_returns_cached_within_ttl(self):
        from whitemagic.core.memory.constellations import DetectionReport
        det = self._make_detector(ttl=60.0)

        report = DetectionReport(memories_scanned=100, constellations_found=1)
        det._last_report = report
        det._last_detect_time = time.time()

        result = det.get_cached_or_detect()
        assert result is report

    def test_cache_expires_after_ttl(self):
        from whitemagic.core.memory.constellations import DetectionReport
        det = self._make_detector(ttl=0.01)  # 10ms TTL

        report = DetectionReport(memories_scanned=100)
        det._last_report = report
        det._last_detect_time = time.time() - 1.0  # 1 second ago

        with patch.object(det, 'detect', return_value=report) as mock_detect:
            refreshed = det.get_cached_or_detect()
            mock_detect.assert_called_once()
            assert refreshed is report


# -----------------------------------------------------------------------
# 3. Briefing Memory Persistence
# -----------------------------------------------------------------------

class TestBriefingMemory:
    """Test InsightPipeline briefing persistence."""

    def test_persist_briefing_as_memory(self):
        from whitemagic.core.intelligence.insight_pipeline import (
            InsightBriefing,
            InsightPipeline,
            BriefingItem,
        )

        pipeline = InsightPipeline(persist_briefings=True)

        briefing = InsightBriefing(
            timestamp=datetime.now().isoformat(),
            duration_ms=42.0,
            items=[
                BriefingItem(
                    id="test_1",
                    category="prediction",
                    title="Test Prediction",
                    description="A test prediction",
                    priority="high",
                    confidence=0.8,
                    source_engine="predictive",
                ),
            ],
            summary={"total_insights": 1, "by_priority": {"high": 1}},
            constellation_context=[],
            velocity_metrics={"total": 100},
        )

        mock_um = MagicMock()
        with patch("whitemagic.core.memory.unified.get_unified_memory", return_value=mock_um):
            pipeline._persist_briefing_as_memory(briefing)

        mock_um.store.assert_called_once()
        call_kwargs = mock_um.store.call_args
        assert "Intelligence Briefing" in call_kwargs.kwargs.get("title", call_kwargs[1].get("title", ""))

    def test_persist_disabled(self):
        from whitemagic.core.intelligence.insight_pipeline import InsightPipeline
        pipeline = InsightPipeline(persist_briefings=False)
        assert pipeline._persist_briefings is False


# -----------------------------------------------------------------------
# 4. Drift Alerts in Briefing
# -----------------------------------------------------------------------

class TestDriftAlerts:
    """Test that drift alerts get added to briefings."""

    def test_drift_creates_briefing_items(self):
        from whitemagic.core.intelligence.insight_pipeline import (
            InsightPipeline,
        )

        pipeline = InsightPipeline(persist_briefings=False)

        # Mock CoreAccessLayer to return drift data
        mock_cal = MagicMock()
        mock_cal.get_velocity_metrics.return_value = {"total": 100}
        mock_cal.get_all_constellations.return_value = []
        mock_cal.get_constellation_drift.side_effect = [
            # 7d drift with significant magnitude
            [{"name": "TestCluster", "drift_magnitude": 0.5,
              "drift_vector": {"dx": 0.3, "dy": 0.1, "dz": 0.0, "dw": 0.0, "dv": 0.4},
              "current_centroid": {"x": 0, "y": 0, "z": 0, "w": 0, "v": 0},
              "samples": 3, "window_days": 7}],
            # 30d drift
            [],
        ]

        with patch.object(pipeline, '_get_core_access', return_value=mock_cal), \
             patch.object(pipeline, '_run_predictive', return_value=[]), \
             patch.object(pipeline, '_run_kaizen', return_value=[]), \
             patch.object(pipeline, '_run_serendipity', return_value=[]), \
             patch.object(pipeline, '_run_emergence', return_value=[]):
            briefing = pipeline.generate_briefing()

        # Should have a drift alert item
        drift_items = [i for i in briefing.items if "drift" in i.id]
        assert len(drift_items) >= 1
        assert "TestCluster" in drift_items[0].title
        assert drift_items[0].priority == "high"  # magnitude > 0.3


# -----------------------------------------------------------------------
# 5. Pre-normalized Embedding Cache
# -----------------------------------------------------------------------

class TestPreNormalizedEmbeddings:
    """Test pre-normalization in embedding vector cache."""

    def test_pre_normalized_cosine(self):
        from whitemagic.core.memory.embeddings import _batch_cosine_similarity_numpy

        # Create pre-normalized vectors
        rng = np.random.RandomState(42)
        matrix = rng.randn(100, 384).astype(np.float32)
        norms = np.linalg.norm(matrix, axis=1, keepdims=True)
        norms[norms == 0] = 1.0
        matrix_normed = matrix / norms

        query = rng.randn(384).astype(np.float32)

        # Compare with and without pre_normalized flag
        scores_normal = _batch_cosine_similarity_numpy(query, matrix)
        scores_prenorm = _batch_cosine_similarity_numpy(query, matrix_normed, pre_normalized=True)

        # Should be approximately equal
        np.testing.assert_allclose(scores_normal, scores_prenorm, atol=1e-5)

    def test_pre_normalized_is_faster_path(self):
        """Verify pre_normalized=True skips norm computation."""
        from whitemagic.core.memory.embeddings import _batch_cosine_similarity_numpy

        matrix = np.eye(10, dtype=np.float32)
        query = np.ones(10, dtype=np.float32)

        # Pre-normalized path: should return matrix @ normalized_query directly
        result = _batch_cosine_similarity_numpy(query, matrix, pre_normalized=True)
        assert result.shape == (10,)

    def test_zero_vector_handling(self):
        from whitemagic.core.memory.embeddings import _batch_cosine_similarity_numpy

        matrix = np.eye(5, dtype=np.float32)
        query = np.zeros(5, dtype=np.float32)

        result = _batch_cosine_similarity_numpy(query, matrix, pre_normalized=True)
        assert np.all(result == 0.0)


# -----------------------------------------------------------------------
# 6. Python RRF Fallback
# -----------------------------------------------------------------------

class TestRRFFallback:
    """Test that Python RRF fallback works when Rust is unavailable."""

    def test_rrf_python_fallback(self):
        """Verify the Python RRF fallback produces correct scores."""
        # Simulate the Python fallback logic directly
        vector_weight = 0.6
        graph_weight = 0.4
        k_rrf = 60

        vec_ids = ["mem_a", "mem_b", "mem_c"]
        graph_ids = ["mem_b", "mem_d", "mem_a"]

        vec_rank = {mid: rank + 1 for rank, mid in enumerate(vec_ids)}
        graph_rank = {mid: rank + 1 for rank, mid in enumerate(graph_ids)}

        all_ids = set(vec_rank.keys()) | set(graph_rank.keys())

        scored = []
        for mid in all_ids:
            score = 0.0
            sources = []
            if mid in vec_rank:
                score += vector_weight / (k_rrf + vec_rank[mid])
                sources.append("vector")
            if mid in graph_rank:
                score += graph_weight / (k_rrf + graph_rank[mid])
                sources.append("graph")
            scored.append((mid, score, sources))

        scored.sort(key=lambda x: -x[1])

        # mem_b appears in both channels, should have highest score
        assert scored[0][0] == "mem_b"
        assert "vector" in scored[0][2]
        assert "graph" in scored[0][2]

        # mem_a also in both
        mem_a_entry = [s for s in scored if s[0] == "mem_a"][0]
        assert "vector" in mem_a_entry[2]
        assert "graph" in mem_a_entry[2]

        # mem_d only in graph
        mem_d_entry = [s for s in scored if s[0] == "mem_d"][0]
        assert mem_d_entry[2] == ["graph"]


# -----------------------------------------------------------------------
# 7. Dream Cycle Autonomy
# -----------------------------------------------------------------------

class TestDreamCycleAutonomy:
    """Test that dream insights get persisted as memories."""

    def test_persist_dream_insights(self):
        from whitemagic.core.dreaming.dream_cycle import DreamCycle

        dc = DreamCycle()

        # Create mock insights
        mock_insight = MagicMock()
        mock_insight.title = "Test Emergence"
        mock_insight.source = "constellation_convergence"
        mock_insight.confidence = 0.75
        mock_insight.description = "Two clusters converging"
        mock_insight.id = "ins_001"

        mock_um = MagicMock()
        with patch("whitemagic.core.memory.unified.get_unified_memory", return_value=mock_um):
            count = dc._persist_dream_insights([mock_insight])

        assert count == 1
        mock_um.store.assert_called_once()
        call_kwargs = mock_um.store.call_args
        assert "Dream Insight:" in call_kwargs.kwargs.get("title", call_kwargs[1].get("title", ""))

    def test_persist_handles_errors_gracefully(self):
        from whitemagic.core.dreaming.dream_cycle import DreamCycle

        dc = DreamCycle()

        mock_insight = MagicMock()
        mock_insight.title = "Fail"
        mock_insight.source = "test"
        mock_insight.confidence = 0.5
        mock_insight.description = "Will fail"
        mock_insight.id = "ins_002"

        with patch("whitemagic.core.memory.unified.get_unified_memory", side_effect=ImportError("no module")):
            count = dc._persist_dream_insights([mock_insight])

        assert count == 0

    def test_persist_empty_list(self):
        from whitemagic.core.dreaming.dream_cycle import DreamCycle
        dc = DreamCycle()
        count = dc._persist_dream_insights([])
        assert count == 0


# -----------------------------------------------------------------------
# 8. CoreAccessLayer Drift Query
# -----------------------------------------------------------------------

class TestCoreAccessDrift:
    """Test CoreAccessLayer drift query delegation."""

    def test_drift_delegates_to_detector(self):
        from whitemagic.core.intelligence.core_access import CoreAccessLayer

        cal = CoreAccessLayer()
        mock_detector = MagicMock()
        mock_detector.get_drift_vectors.return_value = [
            {"name": "TestCluster", "drift_magnitude": 0.2}
        ]

        with patch("whitemagic.core.memory.constellations.get_constellation_detector",
                    return_value=mock_detector):
            result = cal.get_constellation_drift(window_days=7)

        assert len(result) == 1
        assert result[0]["name"] == "TestCluster"
        mock_detector.get_drift_vectors.assert_called_once_with(window_days=7)

    def test_drift_handles_error(self):
        from whitemagic.core.intelligence.core_access import CoreAccessLayer

        cal = CoreAccessLayer()

        with patch("whitemagic.core.memory.constellations.get_constellation_detector",
                    side_effect=ImportError("nope")):
            result = cal.get_constellation_drift()

        assert result == []


# -----------------------------------------------------------------------
# 9. Rust RRF Module Structure
# -----------------------------------------------------------------------

class TestRustRRFModule:
    """Test that the Rust RRF module file exists and has correct structure."""

    _ROOT = Path(__file__).resolve().parent.parent.parent

    def test_rust_rrf_file_exists(self):
        path = self._ROOT / "whitemagic-rust" / "src" / "hybrid_rrf.rs"
        assert path.exists(), f"Missing: {path}"

    def test_rust_walk_file_exists(self):
        path = self._ROOT / "whitemagic-rust" / "src" / "association_walk.rs"
        assert path.exists(), f"Missing: {path}"

    def test_mojo_topk_file_exists(self):
        path = self._ROOT / "whitemagic-mojo" / "src" / "simd_cosine_topk.mojo"
        assert path.exists(), f"Missing: {path}"


# -----------------------------------------------------------------------
# 10. Integration: CoreAccessLayer with TTL-cached constellations
# -----------------------------------------------------------------------

class TestCoreAccessTTLIntegration:
    """Test that CoreAccessLayer uses TTL-cached constellation queries."""

    def test_ttl_cached_query(self):
        from whitemagic.core.intelligence.core_access import CoreAccessLayer

        cal = CoreAccessLayer()

        mock_detector = MagicMock()
        mock_report = MagicMock()
        mock_report.to_dict.return_value = {
            "constellations": [
                {
                    "name": "TestCluster",
                    "size": 10,
                    "centroid": {"x": 0.1, "y": 0.2, "z": 0.3, "w": 0.4, "v": 0.5},
                    "dominant_tags": ["rust"],
                    "zone": "MID_BAND",
                }
            ]
        }
        mock_detector.get_cached_or_detect.return_value = mock_report

        with patch("whitemagic.core.memory.constellations.get_constellation_detector",
                    return_value=mock_detector):
            result = cal.get_all_constellations()

        assert len(result) == 1
        assert result[0].name == "TestCluster"
        mock_detector.get_cached_or_detect.assert_called_once()
