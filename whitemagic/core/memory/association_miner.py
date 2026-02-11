"""Cross-Memory Association Miner.
===============================
Discovers hidden semantic links between memories that haven't been
explicitly associated. Uses lightweight content analysis (keyword overlap,
title similarity, temporal proximity) to propose new associations.

Works alongside the Consolidation engine — consolidation clusters recent
memories, while the miner discovers deep cross-temporal connections
across the entire Data Sea.

No memory is ever deleted or modified — only new association links are
created. New links are bidirectional with initial strength proportional
to the semantic overlap score.

Usage:
    from whitemagic.core.memory.association_miner import get_association_miner
    miner = get_association_miner()
    report = miner.mine(sample_size=500)
"""

from __future__ import annotations

import logging
import re
import threading
import time
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, cast

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Stop words to ignore during keyword extraction
# ---------------------------------------------------------------------------
_STOP_WORDS = frozenset({
    "the", "a", "an", "is", "are", "was", "were", "be", "been", "being",
    "have", "has", "had", "do", "does", "did", "will", "would", "shall",
    "should", "may", "might", "can", "could", "must", "to", "of", "in",
    "for", "on", "with", "at", "by", "from", "as", "into", "through",
    "during", "before", "after", "above", "below", "between", "under",
    "again", "further", "then", "once", "here", "there", "when", "where",
    "why", "how", "all", "each", "every", "both", "few", "more", "most",
    "other", "some", "such", "no", "nor", "not", "only", "own", "same",
    "so", "than", "too", "very", "just", "because", "but", "and", "or",
    "if", "while", "about", "up", "out", "off", "over", "this", "that",
    "these", "those", "it", "its", "my", "your", "his", "her", "our",
    "their", "what", "which", "who", "whom", "me", "him", "them", "we",
    "you", "they", "i", "he", "she", "us", "self", "none", "also", "any",
    "def", "class", "import", "return", "true", "false", "none",
})

_WORD_RE = re.compile(r"[a-z_][a-z0-9_]{2,}")


# ---------------------------------------------------------------------------
# Data structures
# ---------------------------------------------------------------------------

@dataclass
class ProposedLink:
    """A proposed association between two memories."""

    source_id: str
    target_id: str
    overlap_score: float  # 0.0-1.0 semantic overlap
    shared_keywords: set[str]
    reason: str  # Human-readable explanation

    def to_dict(self) -> dict[str, Any]:
        return {
            "source_id": self.source_id,
            "target_id": self.target_id,
            "overlap_score": round(self.overlap_score, 3),
            "shared_keywords": sorted(self.shared_keywords)[:10],
            "reason": self.reason,
        }


@dataclass
class MiningReport:
    """Results from an association mining run."""

    memories_sampled: int = 0
    pairs_evaluated: int = 0
    links_proposed: int = 0
    links_created: int = 0
    duration_ms: float = 0.0
    timestamp: str = field(default_factory=lambda: datetime.now().isoformat())
    top_proposals: list[ProposedLink] = field(default_factory=list)

    def to_dict(self) -> dict[str, Any]:
        return {
            "memories_sampled": self.memories_sampled,
            "pairs_evaluated": self.pairs_evaluated,
            "links_proposed": self.links_proposed,
            "links_created": self.links_created,
            "duration_ms": round(self.duration_ms, 1),
            "timestamp": self.timestamp,
            "top_proposals": [p.to_dict() for p in self.top_proposals[:10]],
        }


# ---------------------------------------------------------------------------
# Core miner
# ---------------------------------------------------------------------------

class AssociationMiner:
    """Discovers hidden semantic connections between memories.

    Strategy:
    1. Sample a diverse set of memories (mix of zones, types, ages).
    2. Extract keyword fingerprints from each memory's title + content.
    3. Compare all pairs (or a smart subset) for keyword overlap.
    4. Propose links for pairs above a threshold that aren't already associated.
    5. Optionally persist new associations to the backend.
    """

    def __init__(
        self,
        min_overlap: float = 0.15,
        max_proposals_per_run: int = 50,
        persist: bool = True,
    ):
        self._min_overlap = min_overlap
        self._max_proposals = max_proposals_per_run
        self._persist = persist
        self._lock = threading.Lock()
        self._total_runs = 0
        self._total_links_created = 0

    # ------------------------------------------------------------------
    # Keyword extraction
    # ------------------------------------------------------------------

    @staticmethod
    def _extract_keywords(text: str, max_keywords: int = 50) -> set[str]:
        """Extract meaningful keywords from text.

        Uses Rust PyO3 extraction when available (v13.3.2), falls back to
        Python regex. Zig SIMD keyword path was disabled (v13.3.1) due to
        ctypes marshaling overhead making it 15× slower than Python.
        """
        # Try Rust PyO3 keyword extraction (v13.3.2) — zero-copy strings
        if len(text) > 200:
            try:
                from whitemagic.optimization.rust_accelerators import (
                    keyword_extract as rust_kw,
                )
                result = rust_kw(text, max_keywords)
                if result is not None:
                    return cast(set[str], result)
            except Exception:
                pass

        # Python extraction (fastest path for keywords)
        text_lower = text.lower()
        words = _WORD_RE.findall(text_lower)
        keywords = {w for w in words if w not in _STOP_WORDS and len(w) > 2}

        # Frequency-based selection: keep most frequent keywords
        if len(keywords) > max_keywords:
            freq: defaultdict[str, int] = defaultdict(int)
            for w in words:
                if w in keywords:
                    freq[w] += 1
            sorted_kw = sorted(keywords, key=lambda k: freq[k], reverse=True)
            keywords = set(sorted_kw[:max_keywords])

        return keywords

    # ------------------------------------------------------------------
    # Overlap scoring
    # ------------------------------------------------------------------

    @staticmethod
    def _compute_overlap(kw_a: set[str], kw_b: set[str]) -> tuple[float, set[str]]:
        """Compute Jaccard-like overlap between two keyword sets."""
        if not kw_a or not kw_b:
            return 0.0, set()

        shared = kw_a & kw_b
        union_size = len(kw_a | kw_b)
        if union_size == 0:
            return 0.0, set()

        # Weighted Jaccard: penalize very small shared sets
        raw_jaccard = len(shared) / union_size
        # Boost if many shared keywords (absolute count matters)
        count_bonus = min(1.0, len(shared) / 5.0) * 0.3
        score = min(1.0, raw_jaccard + count_bonus)
        return score, shared

    # ------------------------------------------------------------------
    # Mining
    # ------------------------------------------------------------------

    def mine(self, sample_size: int = 200) -> MiningReport:
        """Run a single association mining pass.

        Args:
            sample_size: How many memories to sample for comparison.

        """
        start = time.perf_counter()
        report = MiningReport()

        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
        except Exception as e:
            logger.error(f"Association mining: could not get memory system: {e}")
            return report

        # Sample diverse memories: mix of zones
        all_mems = []
        try:
            # Get a mix: some from core/inner_rim, some from mid/outer
            core_mems = um.backend.list_recent(limit=sample_size // 4)
            # Also get some random from deeper zones via SQL
            import sqlite3
            with um.backend.pool.connection() as conn:
                conn.row_factory = sqlite3.Row
                # Sample from different galactic zones
                rows = conn.execute(
                    """SELECT * FROM memories
                       WHERE galactic_distance < 0.40
                       ORDER BY RANDOM() LIMIT ?""",
                    (sample_size // 4,),
                ).fetchall()
                inner_mems = um.backend._batch_hydrate(rows, conn)

                rows = conn.execute(
                    """SELECT * FROM memories
                       WHERE galactic_distance BETWEEN 0.40 AND 0.70
                       ORDER BY RANDOM() LIMIT ?""",
                    (sample_size // 4,),
                ).fetchall()
                mid_mems = um.backend._batch_hydrate(rows, conn)

                rows = conn.execute(
                    """SELECT * FROM memories
                       WHERE galactic_distance > 0.70
                       ORDER BY RANDOM() LIMIT ?""",
                    (sample_size // 4,),
                ).fetchall()
                outer_mems = um.backend._batch_hydrate(rows, conn)

            all_mems = core_mems + inner_mems + mid_mems + outer_mems
        except Exception as e:
            logger.warning(f"Association mining: sampling failed, using recent: {e}")
            all_mems = um.backend.list_recent(limit=sample_size)

        if len(all_mems) < 2:
            return report

        report.memories_sampled = len(all_mems)

        # Get existing associations to avoid duplicates
        existing_assoc: set[tuple[str, str]] = set()
        for mem in all_mems:
            for target_id in mem.associations:
                existing_assoc.add((mem.id, target_id))
                existing_assoc.add((target_id, mem.id))

        # Try Rust accelerated path first (bulk keyword + pairwise in one shot)
        proposals: list[ProposedLink] = []
        used_rust = False
        try:
            from whitemagic.optimization.rust_accelerators import (
                association_mine,
                rust_available,
            )
            if rust_available():
                texts = [(mem.id, f"{mem.title or ''} {str(mem.content)[:2000]}") for mem in all_mems]
                rust_result = association_mine(
                    texts,
                    max_keywords=50,
                    min_score=self._min_overlap,
                    max_results=self._max_proposals * 2,
                )
                report.pairs_evaluated = rust_result.get("pair_count", 0)
                for ov in rust_result.get("overlaps", []):
                    a_id, b_id = ov["source_id"], ov["target_id"]
                    if (a_id, b_id) in existing_assoc:
                        continue
                    shared = set(ov.get("shared_keywords", []))
                    if len(shared) >= 3:
                        top_kw = sorted(shared)[:5]
                        proposals.append(ProposedLink(
                            source_id=a_id,
                            target_id=b_id,
                            overlap_score=ov["overlap_score"],
                            shared_keywords=shared,
                            reason=f"Semantic overlap ({len(shared)} shared keywords: {', '.join(top_kw)})",
                        ))
                used_rust = True
                logger.debug("Association mining used Rust accelerator")
        except Exception as e:
            logger.debug(f"Rust association mining unavailable, using Python: {e}")

        # Python fallback path (with batch Rust keyword extraction)
        if not used_rust:
            fingerprints: dict[str, set[str]] = {}
            texts_for_batch = [(mem.id, f"{mem.title or ''} {str(mem.content)[:2000]}") for mem in all_mems]

            # Try batch Rust keyword extraction (single FFI call for all texts)
            batch_done = False
            try:
                from whitemagic.optimization.rust_accelerators import (
                    keyword_extract_batch as rust_kw_batch,
                )
                result = rust_kw_batch([t[1] for t in texts_for_batch], 50)
                if result is not None:
                    for (mid, _), kw_set in zip(texts_for_batch, result):
                        fingerprints[mid] = kw_set
                    batch_done = True
            except Exception:
                pass

            if not batch_done:
                for mid, text in texts_for_batch:
                    fingerprints[mid] = self._extract_keywords(text)

            mem_ids = [m.id for m in all_mems if len(fingerprints.get(m.id, set())) >= 3]

            for i in range(len(mem_ids)):
                if len(proposals) >= self._max_proposals * 2:
                    break
                for j in range(i + 1, len(mem_ids)):
                    a_id, b_id = mem_ids[i], mem_ids[j]
                    if a_id == b_id or (a_id, b_id) in existing_assoc:
                        continue

                    report.pairs_evaluated += 1
                    score, shared = self._compute_overlap(
                        fingerprints[a_id], fingerprints[b_id],
                    )

                    if score >= self._min_overlap and len(shared) >= 3:
                        top_kw = sorted(shared)[:5]
                        proposals.append(ProposedLink(
                            source_id=a_id,
                            target_id=b_id,
                            overlap_score=score,
                            shared_keywords=shared,
                            reason=f"Semantic overlap ({len(shared)} shared keywords: {', '.join(top_kw)})",
                        ))

        # Sort by score descending, take top N
        proposals.sort(key=lambda p: p.overlap_score, reverse=True)
        proposals = proposals[:self._max_proposals]
        report.links_proposed = len(proposals)
        report.top_proposals = proposals

        # Persist if enabled
        if self._persist and proposals:
            try:
                import sqlite3
                with um.backend.pool.connection() as conn:
                    with conn:
                        for p in proposals:
                            # Bidirectional links with overlap_score as strength
                            try:
                                _now = datetime.now().isoformat()
                                conn.execute(
                                    """INSERT OR IGNORE INTO associations
                                       (source_id, target_id, strength,
                                        direction, relation_type, edge_type,
                                        created_at, ingestion_time)
                                       VALUES (?, ?, ?, 'undirected', 'associated_with', 'semantic', ?, ?)""",
                                    (p.source_id, p.target_id, p.overlap_score, _now, _now),
                                )
                                conn.execute(
                                    """INSERT OR IGNORE INTO associations
                                       (source_id, target_id, strength,
                                        direction, relation_type, edge_type,
                                        created_at, ingestion_time)
                                       VALUES (?, ?, ?, 'undirected', 'associated_with', 'semantic', ?, ?)""",
                                    (p.target_id, p.source_id, p.overlap_score, _now, _now),
                                )
                                report.links_created += 1
                            except Exception:
                                pass
            except Exception as e:
                logger.error(f"Association mining: persistence failed: {e}")

        # Gap A3 synthesis: Feed strong associations into the Knowledge Graph
        self._feed_knowledge_graph(proposals)

        elapsed = (time.perf_counter() - start) * 1000
        report.duration_ms = elapsed

        with self._lock:
            self._total_runs += 1
            self._total_links_created += report.links_created

        logger.info(
            f"🔗 Association mining: {report.memories_sampled} sampled, "
            f"{report.pairs_evaluated} pairs, {report.links_proposed} proposed, "
            f"{report.links_created} created ({elapsed:.0f}ms)",
        )
        return report

    # ------------------------------------------------------------------
    # Gap A3 synthesis: Association Miner → Knowledge Graph
    # ------------------------------------------------------------------

    def _feed_knowledge_graph(self, proposals: list[ProposedLink]) -> None:
        """Create Knowledge Graph edges from strong association discoveries.

        When the miner finds semantically linked memories via keyword overlap,
        those links represent real semantic relationships that the KG should
        know about. We create 'associated_with' relations with the shared
        keywords as context.
        """
        if not proposals:
            return
        try:
            from whitemagic.core.intelligence.knowledge_graph import get_knowledge_graph
            kg = get_knowledge_graph()

            edges_created = 0
            for p in proposals:
                if p.overlap_score < 0.3:  # Only strong links
                    continue
                # Create a KG edge: source_id --[associated_with]--> target_id
                kg.add_relation(
                    source=p.source_id,
                    relation="associated_with",
                    target=p.target_id,
                    metadata={
                        "overlap_score": p.overlap_score,
                        "shared_keywords": sorted(p.shared_keywords)[:5],
                        "origin": "association_miner",
                    },
                )
                edges_created += 1

            if edges_created:
                logger.info(f"KG enrichment: {edges_created} association edges created")
        except Exception as e:
            logger.debug(f"KG enrichment skipped: {e}")

    # ------------------------------------------------------------------
    # Semantic mining (Leap 1a — replaces keyword Jaccard)
    # ------------------------------------------------------------------

    def mine_semantic(
        self,
        min_similarity: float = 0.50,
        strong_threshold: float = 0.70,
        max_proposals: int = 100,
        persist: bool = True,
    ) -> MiningReport:
        """Run semantic association mining using embedding cosine similarity.

        This replaces the keyword Jaccard approach with true semantic
        understanding via sentence-transformer embeddings (384 dims).

        Args:
            min_similarity: Minimum cosine similarity for a weak association.
            strong_threshold: Cosine threshold for a strong association.
            max_proposals: Maximum proposals to generate.
            persist: Whether to write associations to the DB.

        Returns:
            MiningReport with semantic association results.

        """
        start = time.perf_counter()
        report = MiningReport()

        try:
            from whitemagic.core.memory.embeddings import get_embedding_engine
            engine = get_embedding_engine()
        except Exception as e:
            logger.error(f"Semantic mining: embedding engine unavailable: {e}")
            return report

        if not engine.available():
            logger.info("Semantic mining: embeddings not available, falling back to keyword mining")
            return self.mine(sample_size=200)

        # Find all pairs above similarity threshold
        pairs = engine.find_similar_pairs(
            min_similarity=min_similarity,
            max_pairs=max_proposals * 3,  # over-fetch, filter later
        )

        if not pairs:
            elapsed = (time.perf_counter() - start) * 1000
            report.duration_ms = elapsed
            logger.info(f"Semantic mining: no pairs above {min_similarity} threshold ({elapsed:.0f}ms)")
            return report

        report.pairs_evaluated = len(pairs)

        # Filter out existing associations
        existing_assoc: set[tuple[str, str]] = set()
        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
            with um.backend.pool.connection() as conn:
                # Get existing associations for candidate IDs
                candidate_ids = set()
                for pair_item in pairs:
                    candidate_ids.add(pair_item["source_id"])
                    candidate_ids.add(pair_item["target_id"])

                if candidate_ids:
                    placeholders = ",".join("?" * len(candidate_ids))
                    rows = conn.execute(
                        f"""SELECT source_id, target_id FROM associations
                            WHERE source_id IN ({placeholders})
                            OR target_id IN ({placeholders})""",
                        list(candidate_ids) + list(candidate_ids),
                    ).fetchall()
                    for row in rows:
                        existing_assoc.add((row[0], row[1]))
                        existing_assoc.add((row[1], row[0]))
        except Exception as e:
            logger.debug(f"Semantic mining: could not load existing associations: {e}")

        # Build proposals
        proposals: list[ProposedLink] = []
        for p in pairs:
            src, tgt, sim = p["source_id"], p["target_id"], p["similarity"]
            if (src, tgt) in existing_assoc:
                continue

            strength_label = "strong" if sim >= strong_threshold else "weak"
            proposals.append(ProposedLink(
                source_id=src,
                target_id=tgt,
                overlap_score=sim,
                shared_keywords=set(),  # semantic, not keyword-based
                reason=f"Semantic similarity ({strength_label}, cosine={sim:.3f})",
            ))

            if len(proposals) >= max_proposals:
                break

        report.links_proposed = len(proposals)
        report.top_proposals = proposals
        report.memories_sampled = len(
            {pair_item["source_id"] for pair_item in pairs}
            | {pair_item["target_id"] for pair_item in pairs}
        )

        # Persist
        if persist and proposals:
            try:
                with um.backend.pool.connection() as conn:
                    with conn:
                        for proposal in proposals:
                            try:
                                _now = datetime.now().isoformat()
                                conn.execute(
                                    """INSERT OR IGNORE INTO associations
                                       (source_id, target_id, strength,
                                        direction, relation_type, edge_type,
                                        created_at, ingestion_time)
                                       VALUES (?, ?, ?, 'undirected', 'associated_with', 'semantic', ?, ?)""",
                                    (
                                        proposal.source_id,
                                        proposal.target_id,
                                        proposal.overlap_score,
                                        _now,
                                        _now,
                                    ),
                                )
                                conn.execute(
                                    """INSERT OR IGNORE INTO associations
                                       (source_id, target_id, strength,
                                        direction, relation_type, edge_type,
                                        created_at, ingestion_time)
                                       VALUES (?, ?, ?, 'undirected', 'associated_with', 'semantic', ?, ?)""",
                                    (
                                        proposal.target_id,
                                        proposal.source_id,
                                        proposal.overlap_score,
                                        _now,
                                        _now,
                                    ),
                                )
                                report.links_created += 1
                            except Exception:
                                pass
            except Exception as e:
                logger.error(f"Semantic mining: persistence failed: {e}")

        # Feed strong links to Knowledge Graph
        strong_proposals = [p for p in proposals if p.overlap_score >= strong_threshold]
        self._feed_knowledge_graph(strong_proposals)

        elapsed = (time.perf_counter() - start) * 1000
        report.duration_ms = elapsed

        with self._lock:
            self._total_runs += 1
            self._total_links_created += report.links_created

        logger.info(
            f"\U0001f9e0 Semantic mining: {report.memories_sampled} memories, "
            f"{report.pairs_evaluated} pairs evaluated, "
            f"{report.links_proposed} proposed ({len(strong_proposals)} strong), "
            f"{report.links_created} created ({elapsed:.0f}ms)",
        )
        return report

    def get_stats(self) -> dict[str, Any]:
        return {
            "total_runs": self._total_runs,
            "total_links_created": self._total_links_created,
            "min_overlap": self._min_overlap,
            "max_proposals_per_run": self._max_proposals,
            "persist": self._persist,
        }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_miner_instance: AssociationMiner | None = None
_miner_lock = threading.Lock()


def get_association_miner(
    min_overlap: float = 0.15,
    max_proposals: int = 50,
    persist: bool = True,
) -> AssociationMiner:
    """Get or create the global AssociationMiner singleton."""
    global _miner_instance
    with _miner_lock:
        if _miner_instance is None:
            _miner_instance = AssociationMiner(
                min_overlap=min_overlap,
                max_proposals_per_run=max_proposals,
                persist=persist,
            )
        return _miner_instance
