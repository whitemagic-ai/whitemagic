"""Narrative Compressor — Episodic Memory → Meaningful Stories.
=============================================================
A new dream phase that compresses clusters of episodic memories
into coherent narrative summaries. Instead of retaining hundreds
of granular session memories, the compressor:

  1. CLUSTER  — Group temporally-adjacent memories by tag overlap
  2. EXTRACT  — Pull key events, actors, outcomes from each cluster
  3. NARRATE  — Synthesize a narrative memory (template or LLM)
  4. LINK     — Associate the narrative with source memories
  5. ARCHIVE  — Mark source memories as compressed (lower importance)

The narrative memories become high-importance LONG_TERM entries
that serve as efficient recall anchors — one narrative replaces
dozens of individual episodic fragments.

Gana: gana_abundance (Regeneration & Dream Cycle)

Usage:
    from whitemagic.core.dreaming.narrative_compressor import get_narrative_compressor
    nc = get_narrative_compressor()
    result = nc.compress(max_clusters=5)
"""

from __future__ import annotations

import logging
import threading
import time
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

logger = logging.getLogger(__name__)


@dataclass
class NarrativeCluster:
    """A group of related episodic memories."""

    memory_ids: list[str] = field(default_factory=list)
    titles: list[str] = field(default_factory=list)
    shared_tags: set[str] = field(default_factory=set)
    time_range: tuple[str, str] = ("", "")
    size: int = 0

    def to_dict(self) -> dict[str, Any]:
        return {
            "memory_count": self.size,
            "shared_tags": sorted(self.shared_tags)[:10],
            "time_range": list(self.time_range),
            "sample_titles": self.titles[:5],
        }


@dataclass
class NarrativeResult:
    """Result of a narrative compression run."""

    clusters_found: int = 0
    narratives_created: int = 0
    memories_compressed: int = 0
    source_memories_demoted: int = 0
    narratives: list[dict[str, Any]] = field(default_factory=list)
    duration_ms: float = 0.0

    def to_dict(self) -> dict[str, Any]:
        return {
            "clusters_found": self.clusters_found,
            "narratives_created": self.narratives_created,
            "memories_compressed": self.memories_compressed,
            "source_memories_demoted": self.source_memories_demoted,
            "narratives": self.narratives[:10],
            "duration_ms": round(self.duration_ms, 1),
        }


class NarrativeCompressor:
    """Compresses episodic memory clusters into narrative summaries.

    Runs as a dream phase during idle time. Identifies clusters of
    temporally-adjacent memories with overlapping tags and compresses
    them into single narrative memories.
    """

    def __init__(
        self,
        min_cluster_size: int = 5,
        max_cluster_size: int = 50,
        tag_overlap_threshold: float = 0.3,
        demotion_factor: float = 0.5,
    ) -> None:
        self._min_cluster = min_cluster_size
        self._max_cluster = max_cluster_size
        self._tag_overlap = tag_overlap_threshold
        self._demotion_factor = demotion_factor
        self._lock = threading.Lock()
        self._total_compressions = 0
        self._total_narratives = 0

    # ------------------------------------------------------------------
    # Main compression pipeline
    # ------------------------------------------------------------------

    def compress(
        self,
        max_clusters: int = 5,
        sample_limit: int = 500,
        dry_run: bool = False,
    ) -> NarrativeResult:
        """Run the narrative compression pipeline.

        Args:
            max_clusters: Maximum clusters to process per run.
            sample_limit: How many recent memories to scan for clustering.
            dry_run: If True, don't persist narratives or demote sources.

        Returns:
            NarrativeResult with compression statistics.
        """
        start = time.perf_counter()
        result = NarrativeResult()

        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
        except Exception as e:
            logger.debug(f"Narrative compressor: cannot access memory: {e}")
            result.duration_ms = (time.perf_counter() - start) * 1000
            return result

        # Step 1: Load candidate memories
        candidates = self._load_candidates(um, sample_limit)
        if not candidates:
            result.duration_ms = (time.perf_counter() - start) * 1000
            return result

        # Step 2: Cluster by temporal adjacency + tag overlap
        clusters = self._cluster_memories(candidates)
        result.clusters_found = len(clusters)

        # Step 3: Process top clusters
        for cluster in clusters[:max_clusters]:
            narrative = self._compress_cluster(cluster, um, dry_run)
            if narrative:
                result.narratives.append(narrative)
                result.narratives_created += 1
                result.memories_compressed += cluster.size

                # Demote source memories (reduce importance)
                if not dry_run:
                    demoted = self._demote_sources(cluster, um)
                    result.source_memories_demoted += demoted

        result.duration_ms = (time.perf_counter() - start) * 1000

        with self._lock:
            self._total_compressions += 1
            self._total_narratives += result.narratives_created

        logger.info(
            "📖 Narrative compression: %d clusters → %d narratives, "
            "%d memories compressed (%.0fms)",
            result.clusters_found, result.narratives_created,
            result.memories_compressed, result.duration_ms,
        )
        return result

    # ------------------------------------------------------------------
    # Step 1: Load candidate memories
    # ------------------------------------------------------------------

    def _load_candidates(self, um: Any, limit: int) -> list[dict[str, Any]]:
        """Load recent episodic memories that are candidates for compression."""
        try:
            memories = um.list_recent(limit=limit)
            candidates = []
            for m in memories:
                # Skip already-compressed narratives and protected memories
                tags = set(getattr(m, "tags", []) or [])
                if "narrative_compressed" in tags or "protected" in tags:
                    continue
                if "narrative" in tags and "auto_generated" in tags:
                    continue

                candidates.append({
                    "id": m.id,
                    "title": m.title or "",
                    "content": str(m.content or ""),
                    "tags": tags,
                    "importance": getattr(m, "importance", 0.5) or 0.5,
                    "created_at": str(getattr(m, "created_at", "")),
                })
            return candidates
        except Exception as e:
            logger.debug(f"Failed to load candidates: {e}")
            return []

    # ------------------------------------------------------------------
    # Step 2: Cluster memories
    # ------------------------------------------------------------------

    def _cluster_memories(
        self, candidates: list[dict[str, Any]],
    ) -> list[NarrativeCluster]:
        """Cluster memories by tag overlap and temporal proximity."""
        if not candidates:
            return []

        # Sort by creation time
        candidates.sort(key=lambda m: m.get("created_at", ""))

        # Build clusters using sliding window + tag overlap
        clusters: list[NarrativeCluster] = []
        used: set[str] = set()

        for i, seed in enumerate(candidates):
            if seed["id"] in used:
                continue

            seed_tags = seed.get("tags", set())
            if not seed_tags:
                continue

            cluster = NarrativeCluster(
                memory_ids=[seed["id"]],
                titles=[seed.get("title", "")],
                shared_tags=set(seed_tags),
                size=1,
            )
            used.add(seed["id"])

            # Look ahead for similar memories
            for j in range(i + 1, min(i + self._max_cluster * 2, len(candidates))):
                cand = candidates[j]
                if cand["id"] in used:
                    continue

                cand_tags = cand.get("tags", set())
                if not cand_tags:
                    continue

                # Compute Jaccard overlap
                intersection = seed_tags & cand_tags
                union = seed_tags | cand_tags
                overlap = len(intersection) / max(1, len(union))

                if overlap >= self._tag_overlap:
                    cluster.memory_ids.append(cand["id"])
                    cluster.titles.append(cand.get("title", ""))
                    cluster.shared_tags &= cand_tags
                    cluster.size += 1
                    used.add(cand["id"])

                    if cluster.size >= self._max_cluster:
                        break

            if cluster.size >= self._min_cluster:
                # Set time range
                first_time = candidates[i].get("created_at", "")
                last_idx = max(
                    (k for k, c in enumerate(candidates) if c["id"] in set(cluster.memory_ids)),
                    default=i,
                )
                last_time = candidates[last_idx].get("created_at", "")
                cluster.time_range = (first_time, last_time)
                clusters.append(cluster)

        # Sort clusters by size (largest first)
        clusters.sort(key=lambda c: c.size, reverse=True)
        return clusters

    # ------------------------------------------------------------------
    # Step 3: Compress a cluster into a narrative
    # ------------------------------------------------------------------

    def _compress_cluster(
        self,
        cluster: NarrativeCluster,
        um: Any,
        dry_run: bool,
    ) -> dict[str, Any] | None:
        """Compress a memory cluster into a narrative memory."""
        # Load full content of cluster memories
        contents: list[str] = []
        for mid in cluster.memory_ids[:20]:  # Cap to avoid huge prompts
            try:
                mem = um.recall(mid)
                if mem:
                    title = mem.title or "untitled"
                    content = str(mem.content or "")[:300]
                    contents.append(f"[{title}]: {content}")
            except Exception:
                pass

        if not contents:
            return None

        # Generate narrative
        narrative_text = self._generate_narrative(cluster, contents)
        if not narrative_text:
            return None

        narrative_title = self._generate_title(cluster)

        # Persist the narrative memory
        narrative_id = None
        if not dry_run:
            try:
                from whitemagic.core.memory.unified_types import MemoryType
                stored = um.store(
                    content=narrative_text,
                    title=narrative_title,
                    memory_type=MemoryType.LONG_TERM,
                    importance=0.8,  # High importance — narrative anchor
                    tags={
                        "narrative", "auto_generated", "dream_narrative",
                        *sorted(cluster.shared_tags)[:5],
                    },
                    metadata={
                        "narrative_compression": True,
                        "source_count": cluster.size,
                        "source_ids": cluster.memory_ids[:20],
                        "time_range": list(cluster.time_range),
                        "compressed_at": datetime.now().isoformat(),
                    },
                )
                narrative_id = getattr(stored, "id", None) if stored else None
            except Exception as e:
                logger.debug(f"Failed to store narrative: {e}")

        return {
            "title": narrative_title,
            "narrative": narrative_text[:500],
            "source_count": cluster.size,
            "shared_tags": sorted(cluster.shared_tags)[:10],
            "narrative_id": narrative_id,
            "time_range": list(cluster.time_range),
        }

    def _generate_narrative(
        self, cluster: NarrativeCluster, contents: list[str],
    ) -> str:
        """Generate narrative text from cluster contents.

        Tries Ollama first, falls back to template.
        """
        # Try LLM synthesis
        try:
            from whitemagic.tools.handlers.ollama import handle_ollama_generate as _ollama_generate
            evidence_text = "\n".join(contents[:10])
            tags_text = ", ".join(sorted(cluster.shared_tags)[:10])
            prompt = (
                f"Compress the following {cluster.size} related memories into a "
                f"coherent narrative summary (2-4 paragraphs). "
                f"Common themes: {tags_text}\n\n"
                f"Memories:\n{evidence_text}\n\n"
                f"Narrative summary:"
            )
            result = _ollama_generate(prompt=prompt, model=None)
            if isinstance(result, dict) and result.get("response"):
                return str(result["response"]).strip()
        except Exception:
            pass

        # Template fallback
        tags_text = ", ".join(sorted(cluster.shared_tags)[:10])
        lines = [
            f"Narrative Summary ({cluster.size} memories compressed)",
            f"Themes: {tags_text}",
            f"Period: {cluster.time_range[0][:10]} to {cluster.time_range[1][:10]}",
            "",
            "Key memories:",
        ]
        for content in contents[:8]:
            lines.append(f"  - {content[:150]}")

        return "\n".join(lines)

    def _generate_title(self, cluster: NarrativeCluster) -> str:
        """Generate a title for the narrative."""
        tags = sorted(cluster.shared_tags)[:3]
        tag_str = ", ".join(tags) if tags else "mixed topics"
        return f"Narrative: {tag_str} ({cluster.size} memories)"

    # ------------------------------------------------------------------
    # Step 4: Demote source memories
    # ------------------------------------------------------------------

    def _demote_sources(self, cluster: NarrativeCluster, um: Any) -> int:
        """Reduce importance of compressed source memories."""
        demoted = 0
        try:
            pool = um.backend.pool
            with pool.connection() as conn:
                with conn:
                    for mid in cluster.memory_ids:
                        conn.execute(
                            """UPDATE memories
                               SET importance = importance * ?,
                                   tags = COALESCE(tags, '') || ',narrative_compressed'
                               WHERE id = ? AND importance > 0.1""",
                            (self._demotion_factor, mid),
                        )
                        demoted += 1
        except Exception as e:
            logger.debug(f"Failed to demote sources: {e}")
        return demoted

    # ------------------------------------------------------------------
    # Introspection
    # ------------------------------------------------------------------

    def get_stats(self) -> dict[str, Any]:
        """Get compressor statistics."""
        with self._lock:
            return {
                "total_compressions": self._total_compressions,
                "total_narratives": self._total_narratives,
                "min_cluster_size": self._min_cluster,
                "max_cluster_size": self._max_cluster,
                "tag_overlap_threshold": self._tag_overlap,
                "demotion_factor": self._demotion_factor,
            }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_compressor: NarrativeCompressor | None = None
_compressor_lock = threading.Lock()


def get_narrative_compressor(**kwargs: Any) -> NarrativeCompressor:
    """Get the global Narrative Compressor singleton."""
    global _compressor
    if _compressor is None:
        with _compressor_lock:
            if _compressor is None:
                _compressor = NarrativeCompressor(**kwargs)
    return _compressor
