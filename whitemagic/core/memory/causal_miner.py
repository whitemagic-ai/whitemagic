"""Causal Edge Miner (v14.1).
============================
Discovers directed causal relationships between memories by combining
temporal ordering with semantic similarity. Unlike the AssociationMiner
which creates undirected `associated_with` edges, the CausalMiner creates
directed edges: `led_to`, `caused_by`, `preceded`, `followed_by`.

Causal signals:
  1. **Temporal proximity**: Memories created close in time are more likely
     causally related than distant ones.
  2. **Semantic similarity**: Via embedding cosine (HNSW accelerated).
  3. **Tag overlap**: Shared tags reinforce causal hypotheses.
  4. **Title sequence**: "[GUIDE] X" → "[TUTORIAL] X" suggests progression.

Edges are always directed: source.created_at < target.created_at.
Strength = blend of semantic similarity × temporal proximity × tag overlap.

Usage:
    from whitemagic.core.memory.causal_miner import get_causal_miner
    miner = get_causal_miner()
    report = miner.mine(sample_size=200)
"""

from __future__ import annotations

import logging
import math
import threading
import time
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Data structures
# ---------------------------------------------------------------------------

@dataclass
class CausalEdge:
    """A proposed directed causal edge between two memories."""

    source_id: str        # earlier memory
    target_id: str        # later memory
    relation: str         # led_to, caused_by, preceded, etc.
    strength: float       # composite causal strength 0.0–1.0
    semantic_sim: float   # embedding cosine similarity
    temporal_proximity: float  # 0.0=very far, 1.0=very close in time
    tag_overlap: float    # Jaccard of tag sets
    time_delta_hours: float
    reason: str

    def to_dict(self) -> dict[str, Any]:
        return {
            "source_id": self.source_id,
            "target_id": self.target_id,
            "relation": self.relation,
            "strength": round(self.strength, 4),
            "semantic_sim": round(self.semantic_sim, 4),
            "temporal_proximity": round(self.temporal_proximity, 4),
            "tag_overlap": round(self.tag_overlap, 4),
            "time_delta_hours": round(self.time_delta_hours, 2),
            "reason": self.reason,
        }


@dataclass
class CausalMiningReport:
    """Results from a causal mining run."""

    memories_sampled: int = 0
    pairs_evaluated: int = 0
    edges_proposed: int = 0
    edges_created: int = 0
    avg_strength: float = 0.0
    duration_ms: float = 0.0
    timestamp: str = field(default_factory=lambda: datetime.now().isoformat())
    top_edges: list[CausalEdge] = field(default_factory=list)

    def to_dict(self) -> dict[str, Any]:
        return {
            "memories_sampled": self.memories_sampled,
            "pairs_evaluated": self.pairs_evaluated,
            "edges_proposed": self.edges_proposed,
            "edges_created": self.edges_created,
            "avg_strength": round(self.avg_strength, 4),
            "duration_ms": round(self.duration_ms, 1),
            "timestamp": self.timestamp,
            "top_edges": [e.to_dict() for e in self.top_edges[:10]],
        }


# ---------------------------------------------------------------------------
# Core miner
# ---------------------------------------------------------------------------

# Temporal decay: memories more than this many hours apart get rapidly
# diminishing temporal proximity scores.
_MAX_CAUSAL_WINDOW_HOURS = 168.0  # 7 days


class CausalMiner:
    """Discovers directed causal edges between memories.

    Strategy:
    1. Find semantically similar memory pairs (HNSW or brute-force).
    2. For each pair, determine temporal order (earlier → later).
    3. Compute temporal proximity signal (exponential decay).
    4. Compute tag overlap signal (Jaccard).
    5. Blend signals into a composite causal strength.
    6. Classify relation type based on strength + context.
    7. Persist as directed associations.
    """

    def __init__(
        self,
        min_semantic_sim: float = 0.35,
        min_causal_strength: float = 0.20,
        max_edges_per_run: int = 100,
        persist: bool = True,
    ):
        self._min_semantic_sim = min_semantic_sim
        self._min_causal_strength = min_causal_strength
        self._max_edges = max_edges_per_run
        self._persist = persist
        self._lock = threading.Lock()
        self._total_runs: int = 0
        self._total_edges_created: int = 0

    # ------------------------------------------------------------------
    # Signal computation
    # ------------------------------------------------------------------

    @staticmethod
    def _temporal_proximity(dt_hours: float) -> float:
        """Exponential decay: close-in-time → high score."""
        if dt_hours <= 0:
            return 1.0
        # Half-life of 24 hours: after 24h proximity = 0.5, after 48h = 0.25
        return math.exp(-0.693 * dt_hours / 24.0)

    @staticmethod
    def _tag_jaccard(tags_a: set[str], tags_b: set[str]) -> float:
        """Jaccard similarity of tag sets."""
        if not tags_a and not tags_b:
            return 0.0
        union = tags_a | tags_b
        if not union:
            return 0.0
        return len(tags_a & tags_b) / len(union)

    @staticmethod
    def _classify_relation(strength: float, semantic_sim: float,
                           temporal_prox: float) -> str:
        """Classify the causal relationship type."""
        if strength >= 0.6 and temporal_prox >= 0.5:
            return "led_to"       # strong causal, close in time
        elif strength >= 0.4:
            return "influenced"   # moderate causal signal
        elif temporal_prox >= 0.7:
            return "preceded"     # close in time, weaker semantic
        else:
            return "related_to"   # distant but connected

    def _compute_causal_strength(
        self, semantic_sim: float, temporal_prox: float, tag_overlap: float,
    ) -> float:
        """Blend signals into composite causal strength.

        Weights:
          - semantic: 0.50 (primary signal)
          - temporal: 0.35 (causation requires proximity)
          - tags:     0.15 (reinforcing signal)
        """
        return (
            0.50 * semantic_sim
            + 0.35 * temporal_prox
            + 0.15 * tag_overlap
        )

    # ------------------------------------------------------------------
    # Mining
    # ------------------------------------------------------------------

    def mine(self, sample_size: int = 200) -> CausalMiningReport:
        """Run a causal edge mining pass.

        Args:
            sample_size: Max memories to include in the candidate pool.
        """
        start = time.perf_counter()
        report = CausalMiningReport()

        # Get embedding engine for semantic similarity
        try:
            from whitemagic.core.memory.embeddings import get_embedding_engine
            engine = get_embedding_engine()
        except Exception as e:
            logger.error(f"Causal mining: embedding engine unavailable: {e}")
            return report

        if not engine.available():
            logger.info("Causal mining: embeddings not available, skipping")
            return report

        # Find similar pairs
        pairs = engine.find_similar_pairs(
            min_similarity=self._min_semantic_sim,
            max_pairs=self._max_edges * 5,
        )

        if not pairs:
            report.duration_ms = (time.perf_counter() - start) * 1000
            return report

        report.pairs_evaluated = len(pairs)

        # Hydrate memory metadata for temporal + tag signals
        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
        except Exception as e:
            logger.error(f"Causal mining: memory system unavailable: {e}")
            return report

        # Collect all candidate IDs
        candidate_ids: set[str] = set()
        for p in pairs:
            candidate_ids.add(p["source_id"])
            candidate_ids.add(p["target_id"])

        # Batch hydrate: get created_at and tags for all candidates
        mem_meta: dict[str, dict[str, Any]] = {}
        try:
            for mid in candidate_ids:
                mem = um.backend.recall(mid)
                if mem:
                    mem_meta[mid] = {
                        "created_at": mem.created_at,
                        "tags": mem.tags or set(),
                        "title": mem.title or "",
                    }
        except Exception as e:
            logger.debug(f"Causal mining: hydration partially failed: {e}")

        report.memories_sampled = len(mem_meta)

        # Get existing directed associations to avoid duplicates
        existing_directed: set[tuple[str, str]] = set()
        try:
            import sqlite3
            with um.backend.pool.connection() as conn:
                rows = conn.execute(
                    """SELECT source_id, target_id FROM associations
                       WHERE direction = 'directed'""",
                ).fetchall()
                for row in rows:
                    existing_directed.add((row[0], row[1]))
        except Exception:
            pass

        # Score each pair
        edges: list[CausalEdge] = []
        total_strength = 0.0

        for p in pairs:
            src, tgt, sim = p["source_id"], p["target_id"], p["similarity"]

            # Need metadata for both
            if src not in mem_meta or tgt not in mem_meta:
                continue

            src_meta = mem_meta[src]
            tgt_meta = mem_meta[tgt]

            # Determine temporal order: earlier → later
            src_time = src_meta["created_at"]
            tgt_time = tgt_meta["created_at"]

            if src_time > tgt_time:
                # Swap so source is always earlier
                src, tgt = tgt, src
                src_meta, tgt_meta = tgt_meta, src_meta
                src_time, tgt_time = tgt_time, src_time

            # Skip if already directed-linked
            if (src, tgt) in existing_directed:
                continue

            # Compute signals
            dt_hours = (tgt_time - src_time).total_seconds() / 3600.0
            if dt_hours > _MAX_CAUSAL_WINDOW_HOURS:
                continue  # Too far apart for causal inference

            temporal_prox = self._temporal_proximity(dt_hours)
            tag_overlap = self._tag_jaccard(src_meta["tags"], tgt_meta["tags"])
            strength = self._compute_causal_strength(sim, temporal_prox, tag_overlap)

            if strength < self._min_causal_strength:
                continue

            relation = self._classify_relation(strength, sim, temporal_prox)

            edges.append(CausalEdge(
                source_id=src,
                target_id=tgt,
                relation=relation,
                strength=strength,
                semantic_sim=sim,
                temporal_proximity=temporal_prox,
                tag_overlap=tag_overlap,
                time_delta_hours=dt_hours,
                reason=(
                    f"{relation}: sem={sim:.2f}, time={dt_hours:.1f}h, "
                    f"tags={tag_overlap:.2f}"
                ),
            ))
            total_strength += strength

            if len(edges) >= self._max_edges:
                break

        # Sort by strength
        edges.sort(key=lambda e: e.strength, reverse=True)
        report.edges_proposed = len(edges)
        report.top_edges = edges
        if edges:
            report.avg_strength = total_strength / len(edges)

        # Persist directed edges
        if self._persist and edges:
            try:
                with um.backend.pool.connection() as conn:
                    with conn:
                        for edge in edges:
                            try:
                                _now = datetime.now().isoformat()
                                conn.execute(
                                    """INSERT OR IGNORE INTO associations
                                       (source_id, target_id, strength,
                                        direction, relation_type, edge_type,
                                        created_at, ingestion_time)
                                       VALUES (?, ?, ?, 'directed', ?, 'causal', ?, ?)""",
                                    (
                                        edge.source_id,
                                        edge.target_id,
                                        edge.strength,
                                        edge.relation,
                                        _now,
                                        _now,
                                    ),
                                )
                                report.edges_created += 1
                            except Exception:
                                pass
            except Exception as e:
                logger.error(f"Causal mining: persistence failed: {e}")

        elapsed = (time.perf_counter() - start) * 1000
        report.duration_ms = elapsed

        with self._lock:
            self._total_runs += 1
            self._total_edges_created += report.edges_created

        logger.info(
            f"⚡ Causal mining: {report.memories_sampled} sampled, "
            f"{report.pairs_evaluated} pairs, {report.edges_proposed} proposed, "
            f"{report.edges_created} created ({elapsed:.0f}ms)",
        )
        return report

    # ------------------------------------------------------------------
    # Stats
    # ------------------------------------------------------------------

    def get_stats(self) -> dict[str, Any]:
        return {
            "total_runs": self._total_runs,
            "total_edges_created": self._total_edges_created,
            "min_semantic_sim": self._min_semantic_sim,
            "min_causal_strength": self._min_causal_strength,
            "max_edges_per_run": self._max_edges,
        }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_miner_instance: CausalMiner | None = None
_miner_lock = threading.Lock()


def get_causal_miner(
    min_semantic_sim: float = 0.35,
    min_causal_strength: float = 0.20,
    max_edges: int = 100,
    persist: bool = True,
) -> CausalMiner:
    """Get or create the global CausalMiner singleton."""
    global _miner_instance
    with _miner_lock:
        if _miner_instance is None:
            _miner_instance = CausalMiner(
                min_semantic_sim=min_semantic_sim,
                min_causal_strength=min_causal_strength,
                max_edges_per_run=max_edges,
                persist=persist,
            )
        return _miner_instance
