import asyncio
import hashlib
import itertools
import json
import logging
import math
import sqlite3
from concurrent.futures import ProcessPoolExecutor
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Any, cast

from whitemagic.config.paths import DB_PATH

logger = logging.getLogger(__name__)

@dataclass
class MemoryCluster:
    """A cluster of related memories."""

    center: tuple[float, float, float, float]  # x, y, z, w
    memory_ids: list[str]
    titles: list[str]
    avg_importance: float
    radius: float  # How spread out the cluster is

@dataclass
class ConsolidationResult:
    """Result of consolidation."""

    clusters_found: int
    summaries_created: int
    memories_linked: int
    source_memories_preserved: int

class HolographicConsolidator:
    """Consolidate memories by 4D spatial proximity with O(N) Spatial Hashing and Batched Async Synthesis."""

    def __init__(self, db_path: Path | None = None) -> None:
        self.db_path = db_path or DB_PATH
        self.synthesizer: Any | None = None
        self._executor: ProcessPoolExecutor | None = None

    def _get_executor(self) -> ProcessPoolExecutor:
        executor = self._executor
        if executor is None:
            # Prevent CPU saturation on T480s
            import os
            executor = ProcessPoolExecutor(max_workers=max(1, (os.cpu_count() or 4) // 2))
            self._executor = executor
        return executor

    def _get_synthesizer(self) -> Any:
        if self.synthesizer is None:
            from .sector_synthesis import SectorSynthesizer  # type: ignore[import-not-found]
            self.synthesizer = SectorSynthesizer()
        return self.synthesizer

    def _distance(self, c1: tuple, c2: tuple) -> float:
        """4D Euclidean distance."""
        return math.sqrt(sum((a - b) ** 2 for a, b in zip(c1, c2)))

    def get_all_coords_batched(self, batch_size: int = 50000) -> Any:
        """Generator to fetch coordinates without loading everything into RAM at once."""
        conn = sqlite3.connect(self.db_path)
        conn.row_factory = sqlite3.Row

        cursor = conn.execute("""
            SELECT m.id, h.x, h.y, h.z, h.w
            FROM memories m
            JOIN holographic_coords h ON m.id = h.memory_id
        """)

        while True:
            rows = cursor.fetchmany(batch_size)
            if not rows:
                break
            yield [(r["id"], (r["x"], r["y"], r["z"], r["w"])) for r in rows]

        conn.close()

    def find_clusters(self, radius: float = 0.35) -> list[MemoryCluster]:
        """Memory-efficient O(N) Spatial Hashing Clustering.
        Optimized for 276K+ memories to prevent OOM/Blackouts.
        """
        logger.info(f"🔮 Finding clusters with O(N) Spatial Hashing (radius={radius})...")

        buckets: dict[tuple[int, int, int, int], list[str]] = {}
        coords_map: dict[str, tuple[float, float, float, float]] = {}

        record_count = 0
        for batch in self.get_all_coords_batched():
            for mem_id, coords in batch:
                coords_map[mem_id] = coords
                # Bucket key
                bk = cast("tuple[int, int, int, int]", tuple(int(c / max(radius, 0.01)) for c in coords))
                if bk not in buckets:
                    buckets[bk] = []
                buckets[bk].append(mem_id)
                record_count += 1
            print(f"   - Bucketed {record_count} points...")

        print(f"   - Building neighbors for {len(buckets)} spatial cells...")

        clusters = []
        used = set()

        # Iterate over buckets to find seed points
        bucket_count = 0
        neighbor_offsets = list(itertools.product([-1, 0, 1], repeat=4))

        for bk, bucket_ids in buckets.items():
            bucket_count += 1
            if bucket_count % 2000 == 0:
                print(f"   - Processing bucket {bucket_count}/{len(buckets)}...")

            for mem_id in bucket_ids:
                if mem_id in used:
                    continue

                current_coords = coords_map[mem_id]
                neighbors = [mem_id]

                # Check neighbors
                bx, by, bz, bw = bk
                for dx, dy, dz, dw in neighbor_offsets:
                    nb_key = (bx+dx, by+dy, bz+dz, bw+dw)
                    if nb_key in buckets:
                        for other_id in buckets[nb_key]:
                            if other_id == mem_id or other_id in used:
                                continue

                            other_coords = coords_map[other_id]
                            dist_sq = sum((a - b) ** 2 for a, b in zip(current_coords, other_coords))
                            if dist_sq <= radius * radius:
                                neighbors.append(other_id)

                if len(neighbors) >= 3:
                    center = tuple(sum(coords_map[n][i] for n in neighbors) / len(neighbors) for i in range(4))
                    clusters.append(MemoryCluster(
                        center=cast("tuple[float, float, float, float]", center),
                        memory_ids=neighbors,
                        titles=[],
                        avg_importance=0.0,
                        radius=radius,
                    ))
                    used.update(neighbors)

        print(f"   ✓ Clustering complete: {len(clusters)} clusters found.")
        return clusters

    async def hydrate_clusters(self, clusters: list[MemoryCluster], batch_size: int = 100) -> Any:
        """Batch-hydrate titles and importance for multiple clusters."""
        conn = sqlite3.connect(self.db_path)
        conn.row_factory = sqlite3.Row

        for i in range(0, len(clusters), batch_size):
            batch = clusters[i:i+batch_size]
            for cluster in batch:
                placeholders = ",".join("?" * len(cluster.memory_ids))
                rows = conn.execute(f"SELECT title, importance FROM memories WHERE id IN ({placeholders})", cluster.memory_ids).fetchall()
                cluster.titles = [r["title"] for r in rows]
                if rows:
                    cluster.avg_importance = sum(r["importance"] for r in rows) / len(rows)
        conn.close()

    async def create_summary(self, cluster: MemoryCluster) -> dict[str, Any]:
        """Create a hierarchical summary memory for a cluster."""
        synth = self._get_synthesizer()
        principle = await synth.synthesize_cluster(cluster)

        summary_content = f"# Sector Summary: {principle.title}\n\n"
        summary_content += f"**Quadrant**: {principle.quadrant.title()}\n"
        summary_content += f"**Density**: {len(cluster.memory_ids)} memories\n\n"
        summary_content += f"## Analysis\n{principle.summary}\n\n"
        summary_content += "## Core Principles\n" + "\n".join(f"- {p}" for p in principle.principles)

        summary_id = hashlib.sha256(f"sector:{':'.join(sorted(cluster.memory_ids))}".encode()).hexdigest()[:16]

        return {
            "id": summary_id,
            "title": principle.title,
            "content": summary_content,
            "coords": cluster.center,
            "importance": max(cluster.avg_importance, 0.7),
            "source_ids": cluster.memory_ids,
        }

    async def consolidate(self, radius: float = 0.35, dry_run: bool = True) -> ConsolidationResult:
        """Run hierarchical consolidation with batched async synthesis."""
        clusters = self.find_clusters(radius)

        result = ConsolidationResult(
            clusters_found=len(clusters),
            summaries_created=0,
            memories_linked=0,
            source_memories_preserved=0,
        )

        if not clusters:
            return result

        if dry_run:
            for c in clusters:
                result.memories_linked += len(c.memory_ids)
            return result

        # Real consolidation with batch synthesis
        logger.info(f"⏳ Hydrating {len(clusters)} clusters...")
        await self.hydrate_clusters(clusters)

        conn = sqlite3.connect(self.db_path)

        # Semaphore to limit concurrent LLM calls
        semaphore = asyncio.Semaphore(3)

        async def process_cluster(c: Any) -> Any:
            async with semaphore:
                try:
                    summary = await self.create_summary(c)
                    return summary
                except Exception as e:
                    logger.error(f"Synthesis failed for cluster: {e}")
                    return None

        batch_size = 10
        for i in range(0, len(clusters), batch_size):
            batch = clusters[i:i+batch_size]
            summaries = await asyncio.gather(*(process_cluster(c) for c in batch))

            for summary in summaries:
                if summary:
                    # Generic store logic
                    conn.execute("""
                        INSERT OR REPLACE INTO memories
                        (id, content, memory_type, created_at, accessed_at, title, importance, metadata)
                        VALUES (?, ?, 'PATTERN', ?, ?, ?, ?, ?)
                    """, (
                        summary["id"], summary["content"], datetime.now().isoformat(),
                        datetime.now().isoformat(), summary["title"], summary["importance"],
                        json.dumps({"source_ids": summary["source_ids"]}),
                    ))
                    conn.execute("INSERT OR REPLACE INTO holographic_coords (memory_id, x, y, z, w) VALUES (?, ?, ?, ?, ?)",
                                (summary["id"], *summary["coords"]))
                    result.summaries_created += 1

            conn.commit()
            logger.info(f"✅ Processed batch {i//batch_size + 1}, Total Summaries: {result.summaries_created}")

        conn.close()
        return result

def get_consolidator() -> HolographicConsolidator:
    return HolographicConsolidator()

if __name__ == "__main__":
    # Standard log setup for CLI
    logging.basicConfig(level=logging.INFO)
    consolidator = get_consolidator()
    asyncio.run(consolidator.consolidate(radius=0.35, dry_run=True))
