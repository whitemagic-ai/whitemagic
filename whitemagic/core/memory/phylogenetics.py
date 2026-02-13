"""Phylogenetic Memory Lineage — Cross-Galaxy Bridge Edges (v15.4).

Digital genetics for the memory system. When memories are transferred
between galaxies, this module creates and maintains "bridge edges" —
cross-galaxy associations that track lineage, descent, and horizontal
gene transfer between memory populations.

Concepts:
  - **Lineage edge**: Created when a memory is transferred/copied between
    galaxies. Links source → child across galaxy boundaries.
  - **Horizontal gene transfer (HGT)**: When a memory in galaxy A spawns
    an insight in galaxy B (e.g., during dream cycles), an HGT edge
    records the cross-pollination event.
  - **Phylogenetic tree**: The full lineage graph of how memories evolved,
    split, and merged across galaxies over time.
  - **Taxonomic classification**: Memories grouped into species (by tag
    cluster), genus (by galaxy), family (by theme), order (by era).

Usage:
    from whitemagic.core.memory.phylogenetics import get_phylogenetics
    pg = get_phylogenetics()
    pg.record_transfer("mem_abc", "default", "dreams", "mem_xyz")
    pg.record_hgt("mem_abc", "default", "mem_xyz", "dreams", mechanism="dream_serendipity")
    tree = pg.build_lineage_tree("mem_abc")
    taxonomy = pg.classify_memory("mem_abc")
"""

from __future__ import annotations

import hashlib
import logging
import threading
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

logger = logging.getLogger(__name__)


@dataclass
class LineageEdge:
    """A directed edge in the phylogenetic tree."""

    source_id: str
    source_galaxy: str
    target_id: str
    target_galaxy: str
    edge_type: str  # "transfer", "hgt", "merge", "dream_spawn", "split"
    mechanism: str  # How the edge was created
    created_at: str = field(default_factory=lambda: datetime.now().isoformat())
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        return {
            "source_id": self.source_id,
            "source_galaxy": self.source_galaxy,
            "target_id": self.target_id,
            "target_galaxy": self.target_galaxy,
            "edge_type": self.edge_type,
            "mechanism": self.mechanism,
            "created_at": self.created_at,
            "metadata": self.metadata,
        }


@dataclass
class TaxonomicRank:
    """Taxonomic classification of a memory."""

    species: str       # Tag cluster (e.g., "session_checkpoint")
    genus: str         # Galaxy name
    family: str        # Theme/dominant tag
    order: str         # Era (e.g., "aria_era", "v15", "pre_v14")
    kingdom: str       # Memory type (short_term, long_term, etc.)
    lineage_depth: int  # How many transfers/splits from original

    def to_dict(self) -> dict[str, Any]:
        return {
            "species": self.species,
            "genus": self.genus,
            "family": self.family,
            "order": self.order,
            "kingdom": self.kingdom,
            "lineage_depth": self.lineage_depth,
        }

    @property
    def full_name(self) -> str:
        """Binomial nomenclature: Genus species."""
        return f"{self.genus.capitalize()} {self.species}"


class PhylogeneticTracker:
    """Tracks memory lineage, descent, and horizontal gene transfer.

    Maintains a cross-galaxy lineage graph stored in the active
    galaxy's SQLite backend via a dedicated `lineage_edges` table.
    """

    _instance: PhylogeneticTracker | None = None
    _lock = threading.Lock()

    def __init__(self) -> None:
        self._initialized = False

    @classmethod
    def get_instance(cls) -> PhylogeneticTracker:
        if cls._instance is None:
            with cls._lock:
                if cls._instance is None:
                    cls._instance = cls()
        return cls._instance

    def _ensure_table(self) -> None:
        """Create the lineage_edges table if it doesn't exist."""
        if self._initialized:
            return
        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
            with um.backend.pool.connection() as conn:
                conn.execute("""
                    CREATE TABLE IF NOT EXISTS lineage_edges (
                        id TEXT PRIMARY KEY,
                        source_id TEXT NOT NULL,
                        source_galaxy TEXT NOT NULL,
                        target_id TEXT NOT NULL,
                        target_galaxy TEXT NOT NULL,
                        edge_type TEXT NOT NULL DEFAULT 'transfer',
                        mechanism TEXT NOT NULL DEFAULT 'manual',
                        created_at TEXT NOT NULL,
                        metadata TEXT DEFAULT '{}'
                    )
                """)
                conn.execute("""
                    CREATE INDEX IF NOT EXISTS idx_lineage_source
                    ON lineage_edges(source_id, source_galaxy)
                """)
                conn.execute("""
                    CREATE INDEX IF NOT EXISTS idx_lineage_target
                    ON lineage_edges(target_id, target_galaxy)
                """)
                conn.execute("""
                    CREATE INDEX IF NOT EXISTS idx_lineage_type
                    ON lineage_edges(edge_type)
                """)
                conn.commit()
            self._initialized = True
        except Exception as e:
            logger.debug(f"Phylogenetics table init: {e}")

    # ── Recording edges ──────────────────────────────────────────────

    def record_transfer(
        self,
        source_id: str,
        source_galaxy: str,
        target_galaxy: str,
        target_id: str,
        mechanism: str = "galaxy.transfer",
        metadata: dict[str, Any] | None = None,
    ) -> LineageEdge:
        """Record a memory transfer between galaxies."""
        return self._record_edge(
            source_id=source_id,
            source_galaxy=source_galaxy,
            target_id=target_id,
            target_galaxy=target_galaxy,
            edge_type="transfer",
            mechanism=mechanism,
            metadata=metadata or {},
        )

    def record_hgt(
        self,
        source_id: str,
        source_galaxy: str,
        target_id: str,
        target_galaxy: str,
        mechanism: str = "unknown",
        metadata: dict[str, Any] | None = None,
    ) -> LineageEdge:
        """Record a horizontal gene transfer event.

        HGT occurs when a memory in one galaxy inspires or spawns
        a new memory in another galaxy through indirect means
        (e.g., dream serendipity, bridge synthesis).
        """
        return self._record_edge(
            source_id=source_id,
            source_galaxy=source_galaxy,
            target_id=target_id,
            target_galaxy=target_galaxy,
            edge_type="hgt",
            mechanism=mechanism,
            metadata=metadata or {},
        )

    def record_dream_spawn(
        self,
        source_id: str,
        source_galaxy: str,
        target_id: str,
        target_galaxy: str,
        dream_phase: str = "unknown",
    ) -> LineageEdge:
        """Record a memory spawned during a dream cycle."""
        return self._record_edge(
            source_id=source_id,
            source_galaxy=source_galaxy,
            target_id=target_id,
            target_galaxy=target_galaxy,
            edge_type="dream_spawn",
            mechanism=f"dream:{dream_phase}",
            metadata={"dream_phase": dream_phase},
        )

    def record_merge(
        self,
        source_ids: list[str],
        source_galaxy: str,
        merged_id: str,
        target_galaxy: str,
    ) -> list[LineageEdge]:
        """Record a merge event where multiple memories combine into one."""
        edges = []
        for src_id in source_ids:
            edge = self._record_edge(
                source_id=src_id,
                source_galaxy=source_galaxy,
                target_id=merged_id,
                target_galaxy=target_galaxy,
                edge_type="merge",
                mechanism="auto_merge",
                metadata={"merge_group_size": len(source_ids)},
            )
            edges.append(edge)
        return edges

    def _record_edge(
        self,
        source_id: str,
        source_galaxy: str,
        target_id: str,
        target_galaxy: str,
        edge_type: str,
        mechanism: str,
        metadata: dict[str, Any],
    ) -> LineageEdge:
        """Internal: persist a lineage edge."""
        self._ensure_table()

        edge_id = hashlib.sha256(
            f"{source_id}:{source_galaxy}:{target_id}:{target_galaxy}:{edge_type}".encode()
        ).hexdigest()[:16]

        edge = LineageEdge(
            source_id=source_id,
            source_galaxy=source_galaxy,
            target_id=target_id,
            target_galaxy=target_galaxy,
            edge_type=edge_type,
            mechanism=mechanism,
            metadata=metadata,
        )

        try:
            import json
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
            with um.backend.pool.connection() as conn:
                conn.execute(
                    """INSERT OR IGNORE INTO lineage_edges
                       (id, source_id, source_galaxy, target_id, target_galaxy,
                        edge_type, mechanism, created_at, metadata)
                       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)""",
                    (edge_id, source_id, source_galaxy, target_id, target_galaxy,
                     edge_type, mechanism, edge.created_at,
                     json.dumps(metadata, default=str)),
                )
                conn.commit()
        except Exception as e:
            logger.debug(f"Lineage edge persist: {e}")

        return edge

    # ── Querying lineage ─────────────────────────────────────────────

    def build_lineage_tree(self, memory_id: str, max_depth: int = 10) -> dict[str, Any]:
        """Build the full lineage tree for a memory (ancestors + descendants).

        Returns a tree dict with 'ancestors' (upstream lineage) and
        'descendants' (downstream progeny).
        """
        self._ensure_table()
        ancestors = self._walk_ancestors(memory_id, max_depth)
        descendants = self._walk_descendants(memory_id, max_depth)

        return {
            "memory_id": memory_id,
            "ancestors": ancestors,
            "descendants": descendants,
            "lineage_depth": len(ancestors),
            "progeny_count": len(descendants),
            "total_edges": len(ancestors) + len(descendants),
        }

    def _walk_ancestors(self, memory_id: str, max_depth: int) -> list[dict[str, Any]]:
        """Walk upstream to find all ancestor memories."""
        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
            ancestors = []
            visited = {memory_id}
            frontier = [memory_id]

            for _depth in range(max_depth):
                if not frontier:
                    break
                next_frontier = []
                with um.backend.pool.connection() as conn:
                    conn.row_factory = __import__("sqlite3").Row
                    for mid in frontier:
                        rows = conn.execute(
                            """SELECT * FROM lineage_edges
                               WHERE target_id = ?
                               ORDER BY created_at""",
                            (mid,),
                        ).fetchall()
                        for row in rows:
                            src = row["source_id"]
                            if src not in visited:
                                visited.add(src)
                                next_frontier.append(src)
                                ancestors.append({
                                    "id": src,
                                    "galaxy": row["source_galaxy"],
                                    "edge_type": row["edge_type"],
                                    "mechanism": row["mechanism"],
                                    "created_at": row["created_at"],
                                    "depth": _depth + 1,
                                })
                frontier = next_frontier
            return ancestors
        except Exception as e:
            logger.debug(f"Ancestor walk: {e}")
            return []

    def _walk_descendants(self, memory_id: str, max_depth: int) -> list[dict[str, Any]]:
        """Walk downstream to find all descendant memories."""
        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
            descendants = []
            visited = {memory_id}
            frontier = [memory_id]

            for _depth in range(max_depth):
                if not frontier:
                    break
                next_frontier = []
                with um.backend.pool.connection() as conn:
                    conn.row_factory = __import__("sqlite3").Row
                    for mid in frontier:
                        rows = conn.execute(
                            """SELECT * FROM lineage_edges
                               WHERE source_id = ?
                               ORDER BY created_at""",
                            (mid,),
                        ).fetchall()
                        for row in rows:
                            tgt = row["target_id"]
                            if tgt not in visited:
                                visited.add(tgt)
                                next_frontier.append(tgt)
                                descendants.append({
                                    "id": tgt,
                                    "galaxy": row["target_galaxy"],
                                    "edge_type": row["edge_type"],
                                    "mechanism": row["mechanism"],
                                    "created_at": row["created_at"],
                                    "depth": _depth + 1,
                                })
                frontier = next_frontier
            return descendants
        except Exception as e:
            logger.debug(f"Descendant walk: {e}")
            return []

    # ── Taxonomic classification ─────────────────────────────────────

    def classify_memory(self, memory_id: str, galaxy_name: str = "default") -> TaxonomicRank:
        """Assign a taxonomic classification to a memory.

        Species:  Tag cluster (sorted top-3 tags joined by '_')
        Genus:    Galaxy name
        Family:   Dominant tag / theme
        Order:    Era (aria_era, v14, v15, unknown)
        Kingdom:  Memory type
        """
        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
            mem = um.backend.recall(memory_id)
            if not mem:
                return TaxonomicRank("unknown", galaxy_name, "unknown", "unknown", "unknown", 0)

            tags = sorted(mem.tags)[:3] if mem.tags else ["untagged"]
            species = "_".join(tags)
            family = tags[0] if tags else "unknown"

            # Determine era/order
            order = "unknown"
            all_tags = mem.tags or set()
            if all_tags & {"aria_era", "aria", "consciousness", "awakening"}:
                order = "aria_era"
            elif all_tags & {"v15", "v15.2", "v15.3", "v15.4"}:
                order = "v15"
            elif all_tags & {"v14", "v14.0", "v14.1", "v14.2"}:
                order = "v14"
            elif mem.created_at and mem.created_at.year <= 2025:
                order = "pre_v14"
            else:
                order = "v15"

            # Lineage depth
            tree = self.build_lineage_tree(memory_id, max_depth=5)
            depth = tree.get("lineage_depth", 0)

            return TaxonomicRank(
                species=species,
                genus=galaxy_name,
                family=family,
                order=order,
                kingdom=str(mem.memory_type.value) if hasattr(mem.memory_type, "value") else str(mem.memory_type),
                lineage_depth=depth,
            )
        except Exception as e:
            logger.debug(f"Classify memory: {e}")
            return TaxonomicRank("unknown", galaxy_name, "unknown", "unknown", "unknown", 0)

    # ── Statistics ───────────────────────────────────────────────────

    def get_stats(self) -> dict[str, Any]:
        """Return lineage graph statistics."""
        self._ensure_table()
        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
            with um.backend.pool.connection() as conn:
                total = conn.execute("SELECT COUNT(*) FROM lineage_edges").fetchone()[0]
                by_type = {}
                for row in conn.execute(
                    "SELECT edge_type, COUNT(*) as cnt FROM lineage_edges GROUP BY edge_type"
                ).fetchall():
                    by_type[row[0]] = row[1]
                galaxies = set()
                for row in conn.execute(
                    "SELECT DISTINCT source_galaxy FROM lineage_edges "
                    "UNION SELECT DISTINCT target_galaxy FROM lineage_edges"
                ).fetchall():
                    galaxies.add(row[0])

                return {
                    "total_edges": total,
                    "by_type": by_type,
                    "galaxies_connected": len(galaxies),
                    "galaxy_names": sorted(galaxies),
                }
        except Exception as e:
            return {"error": str(e)}


def get_phylogenetics() -> PhylogeneticTracker:
    """Get the singleton PhylogeneticTracker instance."""
    return PhylogeneticTracker.get_instance()
