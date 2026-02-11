"""Tests for the UMAP memory visualization projector."""

import pytest
from unittest.mock import patch, MagicMock

import numpy as np

from whitemagic.core.memory.umap_projection import (
    ProjectionPoint,
    ProjectionResult,
    UMAPProjector,
)


@pytest.fixture
def projector():
    return UMAPProjector()


class TestProjectionPoint:
    def test_to_dict_2d(self):
        pt = ProjectionPoint(
            memory_id="abc", x=1.0, y=2.0, title="Test",
            tags=["a", "b"], importance=0.9, galactic_distance=0.1,
            cluster=3,
        )
        d = pt.to_dict()
        assert d["memory_id"] == "abc"
        assert d["x"] == 1.0
        assert d["y"] == 2.0
        assert "z" not in d
        assert d["cluster"] == 3

    def test_to_dict_3d(self):
        pt = ProjectionPoint(memory_id="xyz", x=1.0, y=2.0, z=3.0)
        d = pt.to_dict()
        assert d["z"] == 3.0


class TestProjectionResult:
    def test_to_dict(self):
        result = ProjectionResult(
            n_memories=10, n_components=2, n_clusters=3, duration_ms=150.0,
        )
        d = result.to_dict()
        assert d["n_memories"] == 10
        assert d["n_components"] == 2
        assert d["n_clusters"] == 3
        assert d["duration_ms"] == 150.0
        assert isinstance(d["points"], list)


class TestUMAPProjector:
    def test_available(self, projector):
        # umap-learn was installed above
        assert projector.available() is True

    def test_available_caches(self, projector):
        projector.available()
        assert projector._umap_available is not None

    def test_project_too_few_vectors(self, projector):
        """UMAP needs >= 5 vectors."""
        with patch("whitemagic.core.memory.embeddings.get_embedding_engine") as mock_engine:
            engine = MagicMock()
            engine._load_vec_cache.return_value = (
                ["a", "b"],
                np.random.randn(2, 384).astype(np.float32),
            )
            mock_engine.return_value = engine

            result = projector.project()
            assert result.n_memories == 2
            assert "Too few" in result.umap_params.get("error", "")

    def test_project_with_mocked_vectors(self, projector):
        """Full projection with mocked 10 vectors."""
        n = 10
        ids = [f"mem_{i}" for i in range(n)]
        vecs = np.random.randn(n, 384).astype(np.float32)
        # Normalize
        norms = np.linalg.norm(vecs, axis=1, keepdims=True)
        vecs = vecs / norms

        with patch("whitemagic.core.memory.embeddings.get_embedding_engine") as mock_eng:
            engine = MagicMock()
            engine._load_vec_cache.return_value = (ids, vecs)
            mock_eng.return_value = engine

            # Also mock unified memory for metadata
            with patch("whitemagic.core.memory.unified.get_unified_memory") as mock_um:
                mock_um.return_value.backend.recall.return_value = None

                result = projector.project(n_components=2, force_recompute=True)

                assert result.n_memories == 10
                assert result.n_components == 2
                assert len(result.points) == 10
                assert result.duration_ms > 0

                # Each point has x, y
                for pt in result.points:
                    assert isinstance(pt.x, float)
                    assert isinstance(pt.y, float)
                    assert pt.z is None  # 2D mode

    def test_project_3d(self, projector):
        """3D projection mode."""
        n = 10
        ids = [f"mem_{i}" for i in range(n)]
        vecs = np.random.randn(n, 384).astype(np.float32)
        norms = np.linalg.norm(vecs, axis=1, keepdims=True)
        vecs = vecs / norms

        with patch("whitemagic.core.memory.embeddings.get_embedding_engine") as mock_eng:
            engine = MagicMock()
            engine._load_vec_cache.return_value = (ids, vecs)
            mock_eng.return_value = engine

            with patch("whitemagic.core.memory.unified.get_unified_memory") as mock_um:
                mock_um.return_value.backend.recall.return_value = None

                result = projector.project(n_components=3, force_recompute=True)

                assert result.n_components == 3
                # Points should have z coordinate
                for pt in result.points:
                    assert pt.z is not None

    def test_caching(self, projector):
        """Second call should return cached result."""
        n = 10
        ids = [f"mem_{i}" for i in range(n)]
        vecs = np.random.randn(n, 384).astype(np.float32)
        norms = np.linalg.norm(vecs, axis=1, keepdims=True)
        vecs = vecs / norms

        with patch("whitemagic.core.memory.embeddings.get_embedding_engine") as mock_eng:
            engine = MagicMock()
            engine._load_vec_cache.return_value = (ids, vecs)
            mock_eng.return_value = engine

            with patch("whitemagic.core.memory.unified.get_unified_memory") as mock_um:
                mock_um.return_value.backend.recall.return_value = None

                r1 = projector.project(force_recompute=True)
                r2 = projector.project(force_recompute=False)
                assert r1 is r2  # Same cached object

    def test_get_stats(self, projector):
        stats = projector.get_stats()
        assert "available" in stats
        assert "cached" in stats


class TestSingleton:
    def test_get_umap_projector(self):
        from whitemagic.core.memory.umap_projection import get_umap_projector
        p1 = get_umap_projector()
        p2 = get_umap_projector()
        assert p1 is p2
