"""Zig SIMD Constellation Detection — Python Bridge.
==================================================
Loads the compiled Zig shared library and exposes SIMD-accelerated
grid-based density scanning for constellation detection in 3D space.
Falls back to pure Python when the Zig library is not available.

The Zig implementation provides:
- Grid-based 3D density scanning with configurable resolution
- Flood-fill merge of adjacent dense cells
- SIMD-accelerated cell assignment

Usage:
    from whitemagic.core.acceleration.simd_constellation import (
        grid_density_scan, simd_constellation_status
    )
"""
from __future__ import annotations

import ctypes
import logging
import threading
from collections import defaultdict
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)

_lib = None
_lib_lock = threading.Lock()
_HAS_ZIG = False


def _find_zig_lib() -> str | None:
    """Locate the compiled Zig shared library."""
    import os
    base = Path(__file__).resolve().parent.parent.parent.parent / "whitemagic-zig"
    candidates = [
        os.environ.get("WM_ZIG_LIB", ""),
        str(base / "zig-out" / "lib" / "libwhitemagic.so"),
        str(base / "libwhitemagic.so"),
        str(base / "zig-out" / "lib" / "libwhitemagic.dylib"),
    ]
    for path in candidates:
        if path and os.path.isfile(path):
            return path
    return None


def _load_lib() -> Any:
    """Load the Zig shared library and set up constellation FFI."""
    global _lib, _HAS_ZIG
    if _lib is not None:
        return _lib
    with _lib_lock:
        if _lib is not None:
            return _lib
        path = _find_zig_lib()
        if not path:
            logger.debug("Zig SIMD library not found — using Python fallback for constellation")
            return None
        try:
            lib = ctypes.CDLL(path)
            if not hasattr(lib, "wm_grid_density_scan"):
                logger.debug("Zig library missing constellation symbols — using Python fallback")
                return None

            # wm_grid_density_scan(coords_ptr, n, dim, resolution, threshold, out_ptr, out_capacity) -> count
            lib.wm_grid_density_scan.argtypes = [
                ctypes.POINTER(ctypes.c_float),
                ctypes.c_size_t,
                ctypes.c_size_t,
                ctypes.c_float,
                ctypes.c_size_t,
                ctypes.POINTER(ctypes.c_int),
                ctypes.c_size_t,
            ]
            lib.wm_grid_density_scan.restype = ctypes.c_size_t

            _lib = lib
            _HAS_ZIG = True
            logger.info("Zig SIMD constellation detection loaded: path=%s", path)
            return lib
        except Exception as e:
            logger.debug("Failed to load Zig constellation: %s", e)
            return None


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def grid_density_scan(
    coordinates: list[tuple[float, float, float]],
    resolution: float = 0.1,
    min_density: int = 3,
) -> list[dict[str, Any]]:
    """Detect dense clusters in 3D space using grid-based density scanning.

    Args:
        coordinates: List of (x, y, z) coordinates.
        resolution: Grid cell size.
        min_density: Minimum points per cell to be considered dense.

    Returns:
        List of cluster dicts with members, centroid, size.

    """
    lib = _load_lib()
    if lib is not None and len(coordinates) > 50:
        try:
            n = len(coordinates)
            flat: list[float] = []
            for c in coordinates:
                flat.extend(c[:3])
            coords_arr = (ctypes.c_float * len(flat))(*flat)
            out_capacity = n
            out_arr = (ctypes.c_int * out_capacity)()

            count = lib.wm_grid_density_scan(
                coords_arr, n, 3,
                ctypes.c_float(resolution),
                min_density,
                out_arr, out_capacity,
            )

            if count > 0:
                # Parse cluster assignments from output
                assignments = list(out_arr[:n])
                return _build_clusters(coordinates, assignments)
        except Exception as e:
            logger.debug("Zig grid_density_scan failed, using Python: %s", e)

    # Python fallback
    return _py_grid_density_scan(coordinates, resolution, min_density)


def _py_grid_density_scan(
    coordinates: list[tuple[float, float, float]],
    resolution: float,
    min_density: int,
) -> list[dict[str, Any]]:
    """Pure Python grid-based density scan."""
    if not coordinates:
        return []

    # Assign points to grid cells
    cells: dict[tuple[int, int, int], list[int]] = defaultdict(list)
    for i, (x, y, z) in enumerate(coordinates):
        cx = int(x / resolution)
        cy = int(y / resolution)
        cz = int(z / resolution)
        cells[(cx, cy, cz)].append(i)

    # Find dense cells
    dense_cells = {k: v for k, v in cells.items() if len(v) >= min_density}
    if not dense_cells:
        return []

    # Flood-fill merge adjacent dense cells
    visited: set[tuple[int, int, int]] = set()
    clusters = []

    for cell_key in dense_cells:
        if cell_key in visited:
            continue
        # BFS to find connected dense cells
        cluster_members: list[int] = []
        queue = [cell_key]
        while queue:
            current = queue.pop(0)
            if current in visited:
                continue
            if current not in dense_cells:
                continue
            visited.add(current)
            cluster_members.extend(dense_cells[current])
            # Check 26 neighbors (3D adjacency)
            cx, cy, cz = current
            for dx in (-1, 0, 1):
                for dy in (-1, 0, 1):
                    for dz in (-1, 0, 1):
                        if dx == 0 and dy == 0 and dz == 0:
                            continue
                        neighbor = (cx + dx, cy + dy, cz + dz)
                        if neighbor in dense_cells and neighbor not in visited:
                            queue.append(neighbor)

        if cluster_members:
            # Compute centroid
            pts = [coordinates[i] for i in cluster_members]
            centroid = tuple(sum(p[d] for p in pts) / len(pts) for d in range(3))
            clusters.append({
                "member_indices": cluster_members,
                "size": len(cluster_members),
                "centroid": centroid,
            })

    return clusters


def _build_clusters(
    coordinates: list[tuple[float, float, float]],
    assignments: list[int],
) -> list[dict[str, Any]]:
    """Build cluster dicts from Zig cluster assignment output."""
    cluster_map: dict[int, list[int]] = defaultdict(list)
    for i, cid in enumerate(assignments):
        if cid >= 0:
            cluster_map[cid].append(i)

    clusters = []
    for cid, members in cluster_map.items():
        pts = [coordinates[i] for i in members]
        centroid = tuple(sum(p[d] for p in pts) / len(pts) for d in range(3))
        clusters.append({
            "member_indices": members,
            "size": len(members),
            "centroid": centroid,
        })
    return clusters


def simd_constellation_status() -> dict[str, Any]:
    """Get SIMD constellation detection status."""
    _load_lib()
    return {
        "has_zig_simd": _HAS_ZIG,
        "lib_path": _find_zig_lib() or "not found",
        "backend": "zig_simd" if _HAS_ZIG else "python_fallback",
    }
