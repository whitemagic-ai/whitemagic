"""Relationship Extractor.
======================
Populates memory associations using 4D holographic coordinates.

Pipeline:
  1. Load all memory coords from SQLite (holographic_coords table)
  2. Compute pairwise Euclidean distances in 4D
     - Falls back to brute-force Python when HolographicIndex is unavailable
  3. Filter by distance < threshold  (default cluster radius 0.35)
  4. Persist as bidirectional associations via UnifiedMemory

Usage:
  .venv/bin/python -m whitemagic.intelligence.learning.relationship_extractor
  .venv/bin/python -m whitemagic.intelligence.learning.relationship_extractor --dry-run
"""

import logging
import sqlite3
from pathlib import Path

logger = logging.getLogger(__name__)

# Project root relative to this file:  learning/ → intelligence/ → whitemagic/ → project root
_PROJECT_ROOT = Path(__file__).resolve().parent.parent.parent.parent
_MEM_DIR = _PROJECT_ROOT / "memory"

DEFAULT_THRESHOLD = 0.35


def _dist_sq(a: tuple[float, ...], b: tuple[float, ...]) -> float:
    return sum((ai - bi) ** 2 for ai, bi in zip(a, b))


def _kdtree_neighbors(
    coords_map: dict[str, tuple[float, float, float, float]],
    threshold_sq: float,
    k: int = 50,
) -> list[tuple[str, str, float]]:
    """Use Rust HolographicIndex KD-tree for O(n·k·log n) neighbor search."""
    import whitemagic_rs
    index = whitemagic_rs.HolographicIndex()
    for mem_id, (x, y, z, w) in coords_map.items():
        index.add(mem_id, x, y, z, w)

    seen: set = set()
    edges: list[tuple[str, str, float]] = []
    for mem_id, (x, y, z, w) in coords_map.items():
        neighbors = index.query_nearest(x, y, z, w, k + 1)  # +1 because self is included
        for nid, d_sq in neighbors:
            if nid == mem_id:
                continue
            if d_sq >= threshold_sq:
                continue
            edge_key = (min(mem_id, nid), max(mem_id, nid))
            if edge_key in seen:
                continue
            seen.add(edge_key)
            edges.append((edge_key[0], edge_key[1], d_sq))
    return edges



def _numpy_grid_neighbors(
    coords_map: dict[str, tuple[float, float, float, float]],
    threshold: float,
) -> list[tuple[str, str, float]]:
    """Fast spatial hashing using simple grid partitioning.
    O(N) complxity for fixed radius.
    """
    from collections import defaultdict

    import numpy as np

    # 1. Build Grid
    # Cell size = threshold. Points in cell C can only see points in C and neighbors.
    cell_size = threshold
    grid = defaultdict(list)

    ids = []
    points = []

    for mid, coord in coords_map.items():
        ids.append(mid)
        points.append(coord)

        # Grid key: integer coordinates of the cell
        key = (
            int(coord[0] / cell_size),
            int(coord[1] / cell_size),
            int(coord[2] / cell_size),
            int(coord[3] / cell_size),
        )
        grid[key].append(len(ids) - 1) # Store index

    points_arr = np.array(points, dtype=np.float32)
    threshold_sq = threshold * threshold
    edges = []

    # 2. Query Grid
    # For each cell, check itself and forward neighbors (to avoid duplicates)
    # We iterate over all populated cells

    # Precompute neighbor offsets (3^4 = 81 offsets, but we only need half for unique pairs?
    # Actually, simpler to just check all 81 and filter by index i < j to avoid duplicates.
    import itertools
    offsets = list(itertools.product([-1, 0, 1], repeat=4))

    len(grid)

    for cell_idx, (cell_key, indices_in_cell) in enumerate(grid.items()):
        if not indices_in_cell:
            continue

        # Get all candidate indices from this cell and neighbors
        candidates = []
        for offset in offsets:
            neighbor_key = (
                cell_key[0] + offset[0],
                cell_key[1] + offset[1],
                cell_key[2] + offset[2],
                cell_key[3] + offset[3],
            )
            if neighbor_key in grid:
                candidates.extend(grid[neighbor_key])

        # Optimization: Cap candidates to prevent O(N^2) in dense clusters
        # If we have 30,000 candidates, checking all of them for 30,000 points is 900M checks.
        # We limit to 500 random candidates to keep it O(N).
        MAX_CANDIDATES = 500
        if len(candidates) > MAX_CANDIDATES:
            import random
            candidates = random.sample(candidates, MAX_CANDIDATES)
            # Ensure we include ourselves? Not strictly necessary if we rely on neighbors
            # But better to include some local points.
            # Actually random sampling from the whole block is fine.

        # Now we have a list of indices. We need to compute distances between
        # indices_in_cell (A) and candidates (B).
        # Optimization: only compute if i < j to avoid double counting and self-loops.

        # This is still potentially O(M^2) where M is points in local neighborhood.
        # But M << N.

        idx_a = np.array(indices_in_cell)
        idx_b = np.array(candidates)

        # Retrieve coordinates
        pos_a = points_arr[idx_a] # (Na, 4)
        pos_b = points_arr[idx_b] # (Nb, 4)

        # Compute distances pairwise?
        # If Na and Nb are small, brute force is fine.
        # Let's simple python loop constraint for robustness if numpy broadcasting blows up memory

        for i_local, idx_i in enumerate(indices_in_cell):
            p_i = pos_a[i_local]

            # Vectorized distance to all candidates
            # d_sq = sum((pos_b - p_i)^2, axis=1)
            diff = pos_b - p_i
            d_sqs = np.sum(diff * diff, axis=1)

            # Filter
            matches = np.where(d_sqs < threshold_sq)[0]

            for m_local in matches:
                idx_j = candidates[m_local]
                if idx_i < idx_j:
                    edges.append((ids[idx_i], ids[idx_j], float(d_sqs[m_local])))

    return edges


def extract_relationships(
    base_path: Path | None = None,
    threshold: float = DEFAULT_THRESHOLD,
    dry_run: bool = False,
) -> dict[str, int]:
    """Extract and persist pairwise relationships between all memories
    with holographic coordinates.

    Args:
        base_path: Path to directory containing whitemagic.db.
                   Defaults to <project_root>/memory/.
        threshold: Euclidean distance threshold in 4D space.
        dry_run:   If True, log pairs but don't write to DB.

    Returns:
        Stats dict: extracted, persisted, total_memories

    """
    if base_path is None:
        base_path = _MEM_DIR
    db_path = base_path / "whitemagic.db"
    if not db_path.exists():
        logger.error(f"Database not found at {db_path}")
        return {}

    # Connect to DB with long timeout for WAL recovery
    conn = sqlite3.connect(str(db_path), timeout=300.0)
    conn.row_factory = sqlite3.Row

    # Check if we need to recover WAL
    try:
        conn.execute("PRAGMA wal_checkpoint(TRUNCATE)")
    except Exception as e:
        logger.warning(f"WAL checkpoint failed (might be locked): {e}")

    # 1. Load all persisted coordinates (only for memories that actually exist)
    rows = conn.execute("""
        SELECT h.memory_id, h.x, h.y, h.z, h.w
        FROM holographic_coords h
        INNER JOIN memories m ON h.memory_id = m.id
    """).fetchall()
    coords_map: dict[str, tuple[float, float, float, float]] = {
        r[0]: (r[1], r[2], r[3], r[4]) for r in rows
    }
    if not coords_map:
        logger.warning("No holographic coordinates in DB — nothing to link.")
        return {"extracted": 0, "persisted": 0, "total_memories": 0}

    threshold_sq = threshold * threshold
    total_memories = len(coords_map)
    logger.info("Loaded %d coords from DB. Computing pairwise distances…", total_memories)

    # 2. Nearest-neighbor search (KD-tree preferred; brute-force fallback)
    try:
        edges = _kdtree_neighbors(coords_map, threshold_sq)
        logger.info("Used Rust KD-tree for neighbor search.")
    except Exception as e:
        logger.warning("KD-tree unavailable (%s). Checking for NumPy...", e)
        try:
            edges = _numpy_grid_neighbors(coords_map, threshold)
            logger.info("Used NumPy Spatial Hash for neighbor search.")
        except ImportError:
            logger.warning("NumPy unavailable — falling back to brute-force (SLOW).")
            raise e
    extracted = len(edges)
    logger.info("Found %d edges within threshold. Persisting…", extracted)

    if not dry_run:
        # Clear stale associations and rebuild
        conn.execute("DELETE FROM associations")
        # Build bidirectional edge list
        # Process in chunks to avoid massive memory usage and transaction lock
        BATCH_SIZE = 100000
        total_persisted = 0

        # Clear old associations first
        conn.execute("DELETE FROM associations")
        conn.commit()

        chunk = []
        for id_a, id_b, d_sq in edges:
            strength = max(0.1, 1.0 - (d_sq / threshold_sq))
            chunk.append((id_a, id_b, strength))
            chunk.append((id_b, id_a, strength))

            if len(chunk) >= BATCH_SIZE:
                 conn.executemany(
                    "INSERT OR REPLACE INTO associations (source_id, target_id, strength) VALUES (?,?,?)",
                    chunk,
                 )
                 conn.commit()
                 total_persisted += len(chunk)
                 chunk = []
                 logger.info(f"Persisted {total_persisted} associations...")

        if chunk:
             conn.executemany(
                "INSERT OR REPLACE INTO associations (source_id, target_id, strength) VALUES (?,?,?)",
                chunk,
             )
             conn.commit()
             total_persisted += len(chunk)

        persisted = len(edges)
    else:
        for id_a, id_b, d_sq in edges[:20]:
            strength = max(0.1, 1.0 - (d_sq / threshold_sq))
            logger.info("  [DRY] %s.. <-> %s..  dist_sq=%.4f  strength=%.3f",
                        id_a[:10], id_b[:10], d_sq, strength)
        persisted = 0

    conn.close()
    stats = {
        "extracted": extracted,
        "persisted": persisted,
        "total_memories": total_memories,
    }
    logger.info("Relationship extraction complete: %s", stats)
    return stats


if __name__ == "__main__":
    import sys

    logging.basicConfig(level=logging.INFO, format="%(asctime)s [REL] %(message)s")

    dry = "--dry-run" in sys.argv
    stats = extract_relationships(dry_run=dry)

    print(f"Total memories with coords: {stats['total_memories']}")
    print(f"Relationships extracted:    {stats['extracted']}")
    print(f"Relationships persisted:    {stats['persisted']}")
