"""Holographic Memory Wrapper
Bridges Python Memory objects to the Rust Holographic Spatial Index.
"""

import logging
import os
from dataclasses import dataclass
from typing import Any

from whitemagic.core.intelligence.hologram.encoder import CoordinateEncoder
from whitemagic.utils.rust_helper import get_rust_module, is_rust_available

logger = logging.getLogger(__name__)

@dataclass
class HolographicResult:
    memory_id: str
    distance: float
    # coordinates: Optional[List[float]] = None # Rust index might not return coords query results yet

class HolographicMemory:
    """Python wrapper for the Rust HolographicIndex.
    Manages encoding of memories into 5D coordinates and querying the spatial index.
    Prefers SpatialIndex5D (full 5D) → legacy 4D HolographicIndex → Python fallback.
    """

    def __init__(self) -> None:
        self._rust_available = is_rust_available()
        self._rs = get_rust_module()
        self._index = None
        self._index_5d = None
        self._encoder = CoordinateEncoder()

        if self._rust_available and self._rs:
            # Prefer 5D SpatialIndex5D
            try:
                if hasattr(self._rs, "SpatialIndex5D"):
                    self._index_5d = self._rs.SpatialIndex5D()
                    if not os.getenv("WM_SILENT_INIT"):
                        logger.info("✅ Rust SpatialIndex5D initialized (full 5D)")
            except Exception as e:
                logger.debug(f"SpatialIndex5D unavailable: {e}")
            # Fallback to legacy 4D
            if not self._index_5d:
                try:
                    if hasattr(self._rs, "holographic") and hasattr(self._rs.holographic, "HolographicIndex"):
                        self._index = self._rs.holographic.HolographicIndex()
                        if not os.getenv("WM_SILENT_INIT"):
                            logger.info("✅ Rust HolographicIndex initialized (4D)")
                    elif hasattr(self._rs, "HolographicIndex"):
                        self._index = self._rs.HolographicIndex()
                        if not os.getenv("WM_SILENT_INIT"):
                            logger.info("✅ Rust HolographicIndex initialized (4D)")
                    else:
                        logger.warning("Rust module loaded but no spatial index class found.")
                except Exception as e:
                    logger.error(f"❌ Failed to initialize Rust HolographicIndex: {e}")
        else:
            if not os.getenv("WM_SILENT_INIT"):
                logger.info("Rust Holographic Core not found. Using Python fallback implementation.")


    @property
    def _has_index(self) -> bool:
        return self._index_5d is not None or self._index is not None

    def check_health(self) -> dict[str, Any]:
        """Check the health of the holographic memory system.
        Returns explicit mode (rust5d/rust4d/python) and availability status.
        """
        if self._index_5d:
            backend = "rust5d"
        elif self._index:
            backend = "rust4d"
        else:
            backend = "python"
        return {
            "status": "active" if self._has_index else "degraded",
            "backend": backend,
            "rust_available": self._rust_available,
            "performance_mode": "optimized" if self._has_index else "slow_fallback",
        }

    def index_memory(self, memory_id: str, memory_data: dict[str, Any]) -> tuple[float, float, float, float, float] | None:
        """Encode and add a memory to the spatial index.
        Returns the calculated coordinates (x, y, z, w, v) if successful, None otherwise.
        """
        # Always try to encode, even if Rust index is missing
        try:
            # Encode memory to 5D coordinates
            coord = self._encoder.encode(memory_data)

            # Add to Rust index — prefer 5D, fall back to 4D
            if self._index_5d:
                self._index_5d.add(memory_id, [coord.x, coord.y, coord.z, coord.w, coord.v])
            elif self._index:
                self._index.add(memory_id, coord.x, coord.y, coord.z, coord.w)

            # Return 5D coords regardless of Rust index availability
            return (coord.x, coord.y, coord.z, coord.w, coord.v)
        except Exception as e:
            logger.error(f"Failed to index memory {memory_id}: {e}")
            print(f"❌ HOLOGRAPHIC ERROR: {e}")
            import traceback
            traceback.print_exc()
            return None

    def add_memory_with_coords(self, memory_id: str, x: float, y: float, z: float, w: float, v: float = 0.5) -> bool:
        """Add a memory to the spatial index using pre-calculated coordinates.
        Useful for loading from persistence.
        """
        if not self._has_index:
            return False

        try:
            if self._index_5d:
                self._index_5d.add(memory_id, [x, y, z, w, v])
            elif self._index:
                self._index.add(memory_id, x, y, z, w)
            return True
        except Exception as e:
            logger.error(f"Failed to add memory {memory_id} with coords: {e}")
            return False

    def query_nearest(self, query_data: dict[str, Any], k: int = 5) -> list[HolographicResult]:
        """Find k nearest memories to the query context.
        """
        if not self._has_index:
            return []

        try:
            coord = self._encoder.encode(query_data)

            if self._index_5d:
                results = self._index_5d.query_nearest(
                    [coord.x, coord.y, coord.z, coord.w, coord.v], k,
                )
            else:
                assert self._index is not None
                results = self._index.query_nearest(
                    coord.x, coord.y, coord.z, coord.w, k,
                )

            return [HolographicResult(mid, dist) for mid, dist in results]
        except Exception as e:
            logger.error(f"Holographic query failed: {e}")
            return []

    def query_radius(self, query_data: dict[str, Any], radius: float = 1.0) -> list[HolographicResult]:
        """Find all memories within radius of the query context.
        """
        if not self._has_index:
            return []

        try:
            coord = self._encoder.encode(query_data)
            if self._index_5d:
                # SpatialIndex5D uses query_nearest; filter by radius
                results = self._index_5d.query_nearest(
                    [coord.x, coord.y, coord.z, coord.w, coord.v], 100,
                )
                return [HolographicResult(mid, dist) for mid, dist in results if dist <= radius * radius]
            else:
                assert self._index is not None
                results = self._index.query_radius(
                    coord.x, coord.y, coord.z, coord.w, radius,
                )
                return [HolographicResult(mid, dist) for mid, dist in results]
        except Exception as e:
            logger.error(f"Holographic radius query failed: {e}")
            return []

    def get_stats(self) -> dict[str, Any]:
        if self._index_5d:
            try:
                return {"status": "active", "backend": "rust5d", "size": self._index_5d.size()}
            except Exception:
                return {"status": "error", "backend": "rust5d"}
        if not self._index:
            return {"status": "unavailable"}
        try:
            stats = self._index.stats()
            return {"status": "active", "backend": "rust4d", **stats}
        except Exception:
            return {"status": "error"}

    def find_clusters(self, radius: float = 0.35, min_size: int = 2) -> list[tuple[tuple[float, float, float, float], list[str]]]:
        """Find clusters of memories in 4D space using Rust backend.
        Returns list of (center_point, memory_ids).
        """
        if not self._index:
            return []

        try:
            # Call Rust implementation
            # Rust returns Vec<(Vec<f64>, Vec<String>)>
            # We convert Vec<f64> to Tuple[float, float, float, float]
            raw_clusters = self._index.find_clusters(radius, min_size)

            result = []
            for center, mem_ids in raw_clusters:
                if len(center) == 4:
                    center_tuple = (center[0], center[1], center[2], center[3])
                    result.append((center_tuple, mem_ids))
            return result
        except Exception as e:
            logger.error(f"Rust clustering failed: {e}")
            return []

# Singleton
_holo_memory: HolographicMemory | None = None

def get_holographic_memory() -> HolographicMemory:
    global _holo_memory
    if _holo_memory is None:
        _holo_memory = HolographicMemory()
    return _holo_memory
