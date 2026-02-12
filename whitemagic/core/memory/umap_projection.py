"""UMAP Memory Visualization (v14.1).
=====================================
Projects high-dimensional memory embeddings (384-dim) into 2D or 3D
coordinates for interactive visualization using UMAP (Uniform Manifold
Approximation and Projection).

Design:
  - Lazy UMAP import (optional dependency)
  - Reads embeddings from the vector cache (same as HNSW search)
  - Projects to 2D (default) or 3D
  - Returns JSON-serializable results with memory metadata for rendering
  - Caches projection results (invalidated when vector cache changes)

Output format per point:
  {
    "memory_id": "abc123",
    "x": 1.234,
    "y": -0.567,
    "z": 0.891,          # only in 3D mode
    "title": "...",
    "tags": [...],
    "importance": 0.85,
    "galactic_distance": 0.3,
    "cluster": 2,         # optional HDBSCAN cluster label
  }

Usage:
    from whitemagic.core.memory.umap_projection import get_umap_projector
    proj = get_umap_projector()
    result = proj.project(n_components=2)
"""

from __future__ import annotations

import logging
import threading
import time
from dataclasses import dataclass, field
from typing import Any

import numpy as np

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Result types
# ---------------------------------------------------------------------------

@dataclass
class ProjectionPoint:
    """A single projected memory point."""
    memory_id: str
    x: float
    y: float
    z: float | None = None
    title: str = ""
    tags: list[str] = field(default_factory=list)
    importance: float = 0.5
    galactic_distance: float = 0.0
    cluster: int = -1  # -1 = unclustered

    def to_dict(self) -> dict[str, Any]:
        d: dict[str, Any] = {
            "memory_id": self.memory_id,
            "x": round(self.x, 4),
            "y": round(self.y, 4),
            "title": self.title,
            "tags": self.tags,
            "importance": round(self.importance, 3),
            "galactic_distance": round(self.galactic_distance, 3),
            "cluster": self.cluster,
        }
        if self.z is not None:
            d["z"] = round(self.z, 4)
        return d


@dataclass
class ProjectionResult:
    """Full projection result."""
    points: list[ProjectionPoint] = field(default_factory=list)
    n_components: int = 2
    n_memories: int = 0
    n_clusters: int = 0
    duration_ms: float = 0.0
    umap_params: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        return {
            "n_memories": self.n_memories,
            "n_components": self.n_components,
            "n_clusters": self.n_clusters,
            "duration_ms": round(self.duration_ms, 1),
            "umap_params": self.umap_params,
            "points": [p.to_dict() for p in self.points],
        }


# ---------------------------------------------------------------------------
# Core projector
# ---------------------------------------------------------------------------

class UMAPProjector:
    """Projects memory embeddings into low-dimensional space for visualization."""

    def __init__(self) -> None:
        self._lock = threading.Lock()
        self._umap_available: bool | None = None
        self._cached_projection: ProjectionResult | None = None
        self._cached_vec_count: int = 0

    def available(self) -> bool:
        """Check if umap-learn is installed."""
        if self._umap_available is not None:
            return self._umap_available
        try:
            import umap  # type: ignore[import-untyped]  # noqa: F401
            self._umap_available = True
        except ImportError:
            self._umap_available = False
        return self._umap_available

    def project(
        self,
        n_components: int = 2,
        n_neighbors: int = 15,
        min_dist: float = 0.1,
        metric: str = "cosine",
        include_metadata: bool = True,
        force_recompute: bool = False,
    ) -> ProjectionResult:
        """Project all memory embeddings into 2D or 3D coordinates.

        Args:
            n_components: 2 for 2D, 3 for 3D projection.
            n_neighbors: UMAP n_neighbors (higher = more global structure).
            min_dist: UMAP min_dist (lower = tighter clusters).
            metric: Distance metric for UMAP.
            include_metadata: Whether to hydrate memory metadata (title, tags, etc.).
            force_recompute: Force recomputation even if cache is valid.

        Returns:
            ProjectionResult with all projected points.
        """
        if not self.available():
            return ProjectionResult(
                umap_params={"error": "umap-learn not installed"},
            )

        start = time.perf_counter()

        # Load embeddings from the engine's vector cache
        try:
            from whitemagic.core.memory.embeddings import get_embedding_engine
            engine = get_embedding_engine()
            ids, vectors = engine._load_vec_cache()
        except Exception as e:
            logger.error(f"UMAP projection: could not load vectors: {e}")
            return ProjectionResult(umap_params={"error": str(e)})

        if not ids or not hasattr(vectors, 'shape') or vectors.shape[0] < 5:
            return ProjectionResult(
                n_memories=len(ids) if ids else 0,
                umap_params={"error": "Too few embeddings for UMAP (need >= 5)"},
            )

        # Check cache
        if (not force_recompute
                and self._cached_projection is not None
                and self._cached_vec_count == len(ids)
                and self._cached_projection.n_components == n_components):
            return self._cached_projection

        # Run UMAP
        import umap as umap_lib

        n_neighbors_clamped = min(n_neighbors, len(ids) - 1)
        reducer = umap_lib.UMAP(
            n_components=n_components,
            n_neighbors=max(2, n_neighbors_clamped),
            min_dist=min_dist,
            metric=metric,
            random_state=42,
            n_jobs=1,  # deterministic
        )

        try:
            coords = reducer.fit_transform(vectors)
        except Exception as e:
            logger.error(f"UMAP fit_transform failed: {e}")
            return ProjectionResult(umap_params={"error": str(e)})

        # Hydrate metadata if requested
        mem_meta: dict[str, dict[str, Any]] = {}
        if include_metadata:
            try:
                from whitemagic.core.memory.unified import get_unified_memory
                um = get_unified_memory()
                for mid in ids:
                    mem = um.backend.recall(mid)
                    if mem:
                        mem_meta[mid] = {
                            "title": mem.title or "",
                            "tags": sorted(mem.tags) if mem.tags else [],
                            "importance": mem.importance,
                            "galactic_distance": mem.galactic_distance,
                        }
            except Exception as e:
                logger.debug(f"UMAP: metadata hydration failed: {e}")

        # Build points
        points: list[ProjectionPoint] = []
        for i, mid in enumerate(ids):
            meta = mem_meta.get(mid, {})
            pt = ProjectionPoint(
                memory_id=mid,
                x=float(coords[i, 0]),
                y=float(coords[i, 1]),
                z=float(coords[i, 2]) if n_components >= 3 else None,
                title=meta.get("title", ""),
                tags=meta.get("tags", []),
                importance=meta.get("importance", 0.5),
                galactic_distance=meta.get("galactic_distance", 0.0),
            )
            points.append(pt)

        # Optional: cluster the projected points
        n_clusters = 0
        try:
            n_clusters = self._cluster_points(points, coords)
        except Exception:
            pass

        elapsed = (time.perf_counter() - start) * 1000

        result = ProjectionResult(
            points=points,
            n_components=n_components,
            n_memories=len(points),
            n_clusters=n_clusters,
            duration_ms=elapsed,
            umap_params={
                "n_neighbors": n_neighbors_clamped,
                "min_dist": min_dist,
                "metric": metric,
            },
        )

        # Cache
        with self._lock:
            self._cached_projection = result
            self._cached_vec_count = len(ids)

        logger.info(
            f"🗺️ UMAP projection: {len(points)} memories → {n_components}D, "
            f"{n_clusters} clusters ({elapsed:.0f}ms)",
        )
        return result

    @staticmethod
    def _cluster_points(
        points: list[ProjectionPoint],
        coords: np.ndarray,
    ) -> int:
        """Attempt simple k-means clustering on projected coordinates.

        Falls back gracefully if sklearn not available.
        Returns number of clusters found.
        """
        if len(points) < 5:
            return 0

        try:
            from sklearn.cluster import KMeans  # type: ignore[import-untyped]
        except ImportError:
            # Fallback: simple grid-based clustering
            return 0

        # Choose k based on data size: sqrt(n) / 2, clamped to [2, 15]
        import math
        k = max(2, min(15, int(math.sqrt(len(points)) / 2)))

        km = KMeans(n_clusters=k, random_state=42, n_init=5)
        labels = km.fit_predict(coords[:, :2])  # cluster on 2D even for 3D

        for i, pt in enumerate(points):
            pt.cluster = int(labels[i])

        return k

    def get_stats(self) -> dict[str, Any]:
        cached = self._cached_projection is not None
        return {
            "available": self.available(),
            "cached": cached,
            "cached_count": self._cached_vec_count if cached else 0,
        }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_projector_instance: UMAPProjector | None = None
_projector_lock = threading.Lock()


def get_umap_projector() -> UMAPProjector:
    """Get or create the global UMAPProjector singleton."""
    global _projector_instance
    with _projector_lock:
        if _projector_instance is None:
            _projector_instance = UMAPProjector()
        return _projector_instance
