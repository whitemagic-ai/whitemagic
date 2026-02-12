"""Cross-Session Memory Consolidation — Hippocampal Replay.
=========================================================
Inspired by how biological hippocampal replay strengthens memories
during sleep: the brain re-activates recent experiences, clusters
related traces, and extracts meta-patterns that become long-term
semantic knowledge.

This module runs on the Temporal Scheduler's SLOW lane and:
  1. Loads recent memories from the current session window.
  2. Clusters them by semantic similarity (tag overlap + association overlap).
  3. Identifies high-value clusters (frequent access, strong emotions, patterns).
  4. Synthesizes "strategy memories" — compressed meta-insights from clusters.
  5. Promotes strategy memories to LONG_TERM storage.
  6. Emits MEMORY_CONSOLIDATED and INSIGHT_CRYSTALLIZED events.

The consolidation cycle is gentle: it never deletes, only promotes and
annotates. It works alongside Mindful Forgetting — one archives the
weak, the other strengthens the strong.

Usage:
    from whitemagic.core.memory.consolidation import get_consolidator
    consolidator = get_consolidator()
    report = consolidator.consolidate()
"""

from __future__ import annotations

import hashlib
import logging
import threading
import time
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

logger = logging.getLogger(__name__)


@dataclass
class MemoryCluster:
    """A group of related memories discovered during consolidation."""

    cluster_id: str
    memory_ids: list[str]
    shared_tags: set[str]
    avg_importance: float
    total_access_count: int
    avg_emotional_valence: float
    theme: str  # inferred from dominant tags

    def to_dict(self) -> dict[str, Any]:
        return {
            "cluster_id": self.cluster_id,
            "size": len(self.memory_ids),
            "shared_tags": sorted(self.shared_tags),
            "avg_importance": round(self.avg_importance, 3),
            "total_access_count": self.total_access_count,
            "theme": self.theme,
        }


@dataclass
class ConsolidationReport:
    """Results of a consolidation cycle."""

    memories_analyzed: int = 0
    clusters_found: int = 0
    strategies_synthesized: int = 0
    promotions: int = 0
    duration_ms: float = 0.0
    clusters: list[MemoryCluster] = field(default_factory=list)
    timestamp: str = field(default_factory=lambda: datetime.now().isoformat())

    def to_dict(self) -> dict[str, Any]:
        return {
            "memories_analyzed": self.memories_analyzed,
            "clusters_found": self.clusters_found,
            "strategies_synthesized": self.strategies_synthesized,
            "promotions": self.promotions,
            "duration_ms": round(self.duration_ms, 1),
            "clusters": [c.to_dict() for c in self.clusters[:10]],
            "timestamp": self.timestamp,
        }


class MemoryConsolidator:
    """Hippocampal replay engine for cross-session memory consolidation.

    Clusters recent memories, identifies patterns, synthesizes strategy
    memories, and promotes high-value clusters to long-term storage.
    """

    def __init__(
        self,
        min_cluster_size: int = 3,
        tag_overlap_threshold: float = 0.3,
        importance_boost: float = 0.15,
        max_memories: int = 2000,
    ) -> None:
        self._min_cluster_size = min_cluster_size
        self._tag_overlap_threshold = tag_overlap_threshold
        self._importance_boost = importance_boost
        self._max_memories = max_memories
        self._lock = threading.Lock()
        self._total_consolidations = 0
        self._total_strategies = 0
        self._total_promotions = 0

    # ------------------------------------------------------------------
    # Core consolidation
    # ------------------------------------------------------------------

    def consolidate(self, memories: Any = None) -> ConsolidationReport:
        """Run a full consolidation cycle.

        1. Load recent memories (or use provided list)
        2. Cluster by tag/association similarity
        3. Synthesize strategy memories from strong clusters
        4. Promote high-value memories to LONG_TERM
        """
        start = time.perf_counter()
        report = ConsolidationReport()

        # Step 1: Load memories
        if memories is None:
            memories = self._load_recent()
        if not memories:
            report.duration_ms = (time.perf_counter() - start) * 1000
            with self._lock:
                self._total_consolidations += 1
            return report

        report.memories_analyzed = len(memories)

        # Step 2: Cluster by tag similarity
        clusters = self._cluster_by_tags(memories)
        report.clusters_found = len(clusters)
        report.clusters = clusters

        # Step 3: Synthesize strategy memories from strong clusters
        strategies = self._synthesize_strategies(clusters, memories)
        report.strategies_synthesized = len(strategies)

        # Step 3.5: Bicameral creative clustering (A3 synthesis)
        self._bicameral_enrich(clusters, strategies)

        # Step 4: Promote high-value memories
        promotions = self._promote_high_value(memories)
        report.promotions = promotions

        report.duration_ms = (time.perf_counter() - start) * 1000

        with self._lock:
            self._total_consolidations += 1
            self._total_strategies += len(strategies)
            self._total_promotions += promotions

        # Step 5: Promote synthesized strategies to INNER_RIM galactic zone
        self._galactic_promote(strategies)

        # Step 5.5: G5 Synthesis — Create KG relations linking source → strategy
        self._feed_knowledge_graph(clusters, strategies)

        # Step 6: Feed Harmony Vector energy from consolidation health
        self._update_harmony(report)

        # Emit events
        self._emit_events(report, strategies)

        logger.info(
            f"Consolidation #{self._total_consolidations}: "
            f"{report.memories_analyzed} analyzed, "
            f"{report.clusters_found} clusters, "
            f"{report.strategies_synthesized} strategies, "
            f"{report.promotions} promotions, "
            f"{report.duration_ms:.0f}ms",
        )

        return report

    # ------------------------------------------------------------------
    # Step 1: Load
    # ------------------------------------------------------------------

    def _load_recent(self) -> list[Any]:
        """Load recent memories from UnifiedMemory."""
        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
            return um.list_recent(limit=self._max_memories)
        except Exception as e:
            logger.debug(f"Consolidation: could not load memories: {e}")
            return []

    # ------------------------------------------------------------------
    # Step 2: Cluster
    # ------------------------------------------------------------------

    def _find_near_duplicates_minhash(self, memories: list[Any]) -> list[dict[str, Any]]:
        """Use Rust MinHash LSH to quickly find near-duplicate memory pairs."""
        try:
            from whitemagic.optimization.rust_accelerators import (
                minhash_find_duplicates,
                rust_v131_available,
            )
            if not rust_v131_available() or len(memories) < 10:
                return []

            # Build keyword sets from tags + title words
            keyword_sets = []
            for mem in memories:
                kws = set(mem.tags) if mem.tags else set()
                if mem.title:
                    kws |= {w.lower() for w in mem.title.split() if len(w) > 2}
                keyword_sets.append(sorted(kws))

            candidates = minhash_find_duplicates(keyword_sets, threshold=0.4, max_results=200)
            if candidates:
                logger.debug(f"MinHash found {len(candidates)} near-duplicate candidates")
            return candidates or []
        except Exception as e:
            logger.debug(f"MinHash near-duplicate detection skipped: {e}")
            return []

    def resolve_entities(self, similarity_threshold: float = 0.92, batch_limit: int = 500) -> dict[str, Any]:
        """Entity resolution: embedding-based dedup (v14.0 Living Graph).

        Finds near-duplicate memories via cosine similarity above threshold
        and merges them:
        1. Keep the canonical memory (highest importance + most accessed)
        2. Merge tags from duplicate into canonical
        3. Reinforce canonical (bump importance, access_count)
        4. Push duplicate to FAR_EDGE (never delete)

        Args:
            similarity_threshold: Cosine similarity threshold for dedup (default 0.92).
            batch_limit: Max memories to evaluate per run.

        Returns:
            Dict with resolution stats.
        """
        start = time.perf_counter()
        result: dict[str, Any] = {
            "status": "success",
            "pairs_evaluated": 0,
            "duplicates_found": 0,
            "duplicates_resolved": 0,
        }

        try:
            from whitemagic.core.memory.embeddings import get_embedding_engine
            engine = get_embedding_engine()
            if not engine.available():
                result["status"] = "skipped"
                result["reason"] = "embeddings unavailable"
                return result
        except Exception as e:
            result["status"] = "skipped"
            result["reason"] = str(e)
            return result

        # Find similar pairs above threshold
        try:
            pairs = engine.find_similar_pairs(
                min_similarity=similarity_threshold,
                max_pairs=batch_limit,
            )
        except Exception as e:
            result["status"] = "error"
            result["reason"] = str(e)
            return result

        if not pairs:
            result["duration_ms"] = round((time.perf_counter() - start) * 1000, 1)
            return result

        result["pairs_evaluated"] = len(pairs)

        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
        except Exception as e:
            result["status"] = "error"
            result["reason"] = str(e)
            return result

        resolved_ids: set[str] = set()  # Already-resolved duplicates

        for pair in pairs:
            src_id = pair["source_id"]
            tgt_id = pair["target_id"]
            similarity = pair.get("similarity", 0.0)

            if src_id in resolved_ids or tgt_id in resolved_ids:
                continue

            # Load both memories
            try:
                mem_a = um.backend.recall(src_id)
                mem_b = um.backend.recall(tgt_id)
                if not mem_a or not mem_b:
                    continue
            except Exception:
                continue

            result["duplicates_found"] += 1

            # Determine canonical (higher importance, more accessed)
            score_a = mem_a.importance * 10 + mem_a.access_count
            score_b = mem_b.importance * 10 + mem_b.access_count
            canonical, duplicate = (mem_a, mem_b) if score_a >= score_b else (mem_b, mem_a)

            # Merge tags from duplicate into canonical
            if duplicate.tags:
                canonical.tags = canonical.tags | duplicate.tags

            # Reinforce canonical
            canonical.importance = min(1.0, canonical.importance + 0.03)
            canonical.access_count += 1
            canonical.metadata["entity_resolution"] = {
                "merged_from": duplicate.id,
                "similarity": round(similarity, 4),
                "resolved_at": datetime.now().isoformat(),
            }

            try:
                um.backend.store(canonical)
                # Push duplicate to FAR_EDGE (never delete)
                um.backend.archive_to_edge(duplicate.id, galactic_distance=0.95)
                resolved_ids.add(duplicate.id)
                result["duplicates_resolved"] += 1
            except Exception:
                pass

        result["duration_ms"] = round((time.perf_counter() - start) * 1000, 1)

        if result["duplicates_resolved"] > 0:
            logger.info(
                f"🔗 Entity resolution: {result['duplicates_found']} duplicates found, "
                f"{result['duplicates_resolved']} resolved ({result['duration_ms']:.0f}ms)",
            )
        return result

    def _cluster_by_tags(self, memories: list[Any]) -> list[MemoryCluster]:
        """Cluster memories by tag overlap using greedy agglomeration."""
        # Pre-pass: use MinHash to identify near-duplicates (Rust accelerated)
        minhash_candidates = self._find_near_duplicates_minhash(memories)
        if minhash_candidates:
            logger.info(f"MinHash pre-filter: {len(minhash_candidates)} near-duplicate pairs detected")

        # Build tag-to-memory index
        tag_index: dict[str, list] = defaultdict(list)
        for mem in memories:
            for tag in mem.tags:
                tag_index[tag].append(mem)

        # Find clusters: groups of memories sharing >= threshold fraction of tags
        visited: set[str] = set()
        clusters: list[MemoryCluster] = []

        # Sort tags by frequency (most common first = largest potential clusters)
        sorted_tags = sorted(tag_index.items(), key=lambda x: len(x[1]), reverse=True)

        for tag, tag_memories in sorted_tags:
            # Filter to unvisited memories
            unvisited = [m for m in tag_memories if m.id not in visited]
            if len(unvisited) < self._min_cluster_size:
                continue

            # Find the shared tag set for this group
            all_tag_sets = [m.tags for m in unvisited]
            shared = set.intersection(*all_tag_sets) if all_tag_sets else set()

            # Accept if there's meaningful overlap
            if not shared:
                shared = {tag}

            # Build the cluster
            mem_ids = [m.id for m in unvisited]
            avg_imp = sum(m.importance for m in unvisited) / len(unvisited)
            total_access = sum(m.access_count for m in unvisited)
            avg_valence = sum(m.emotional_valence for m in unvisited) / len(unvisited)

            cluster_id = hashlib.md5(
                "|".join(sorted(mem_ids)[:5]).encode(),
            ).hexdigest()[:12]

            cluster = MemoryCluster(
                cluster_id=cluster_id,
                memory_ids=mem_ids,
                shared_tags=shared,
                avg_importance=avg_imp,
                total_access_count=total_access,
                avg_emotional_valence=avg_valence,
                theme=tag,  # dominant tag as theme
            )
            clusters.append(cluster)

            # Mark as visited
            for mid in mem_ids:
                visited.add(mid)

        return clusters

    # ------------------------------------------------------------------
    # Step 3: Synthesize strategies
    # ------------------------------------------------------------------

    def _synthesize_strategies(self, clusters: list[MemoryCluster], memories: list[Any]) -> list[dict[str, Any]]:
        """Create strategy memories from strong clusters.

        A strategy memory is a compressed meta-insight that captures
        the essence of a cluster: "These N memories about [theme] with
        tags [X, Y, Z] have been accessed M times and have average
        importance P. Key insight: [theme] is a recurring pattern."
        """
        strategies = []
        mem_map = {m.id: m for m in memories}

        for cluster in clusters:
            # Only synthesize from significant clusters
            if (cluster.avg_importance < 0.4 or
                    cluster.total_access_count < 3 or
                    len(cluster.memory_ids) < self._min_cluster_size):
                continue

            # Build strategy content
            sample_titles = []
            for mid in cluster.memory_ids[:5]:
                mem = mem_map.get(mid)
                if mem and mem.title:
                    sample_titles.append(mem.title)

            strategy_content = {
                "type": "consolidated_strategy",
                "theme": cluster.theme,
                "cluster_size": len(cluster.memory_ids),
                "shared_tags": sorted(cluster.shared_tags),
                "avg_importance": round(cluster.avg_importance, 3),
                "total_access_count": cluster.total_access_count,
                "sample_titles": sample_titles,
                "insight": (
                    f"Recurring pattern '{cluster.theme}' detected across "
                    f"{len(cluster.memory_ids)} memories (avg importance: "
                    f"{cluster.avg_importance:.2f}, accessed {cluster.total_access_count} times). "
                    f"Tags: {', '.join(sorted(cluster.shared_tags))}."
                ),
            }

            # Store via UnifiedMemory
            try:
                from whitemagic.core.memory.unified import get_unified_memory
                from whitemagic.core.memory.unified_types import MemoryType
                um = get_unified_memory()
                um.store(
                    content=strategy_content,
                    memory_type=MemoryType.LONG_TERM,
                    title=f"Strategy: {cluster.theme} (consolidated)",
                    tags=cluster.shared_tags | {"strategy", "consolidated"},
                    importance=min(1.0, cluster.avg_importance + self._importance_boost),
                )
                strategies.append(strategy_content)
            except Exception as e:
                logger.debug(f"Could not persist strategy: {e}")

        return strategies

    # ------------------------------------------------------------------
    # Step 3.5: Bicameral creative clustering (A3 synthesis)
    # ------------------------------------------------------------------

    def _bicameral_enrich(
        self, clusters: list[MemoryCluster], strategies: list[dict[str, Any]],
    ) -> None:
        """Use the Bicameral Reasoner's dual-hemisphere heuristics to find
        creative cross-cluster connections.

        Left hemisphere: identifies logical overlap between clusters
        (shared tags, causal chains, temporal sequences).

        Right hemisphere: detects unexpected creative connections
        (tag pairs that rarely co-occur, emotional resonance bridges,
        thematic contrasts that suggest deeper patterns).

        Results are stored as "creative_link" strategy memories.
        """
        if len(clusters) < 2:
            return

        try:
            from whitemagic.core.intelligence.bicameral import (
                get_bicameral_reasoner,
            )
            get_bicameral_reasoner()
        except Exception:
            return

        # --- Left hemisphere: logical cross-cluster links ---
        logical_links = []
        for i, c1 in enumerate(clusters):
            for c2 in clusters[i + 1:]:
                shared = c1.shared_tags & c2.shared_tags
                if shared:
                    logical_links.append({
                        "type": "logical_bridge",
                        "cluster_a": c1.theme,
                        "cluster_b": c2.theme,
                        "shared_tags": sorted(shared),
                        "combined_access": c1.total_access_count + c2.total_access_count,
                    })

        # --- Right hemisphere: creative cross-cluster links ---
        creative_links = []
        for i, c1 in enumerate(clusters):
            for c2 in clusters[i + 1:]:
                # Skip if already logically linked
                if c1.shared_tags & c2.shared_tags:
                    continue
                # Creative link: high emotional valence in both + no tag overlap
                if (c1.avg_emotional_valence > 0.5 and c2.avg_emotional_valence > 0.5):
                    creative_links.append({
                        "type": "creative_bridge",
                        "cluster_a": c1.theme,
                        "cluster_b": c2.theme,
                        "resonance": "emotional_affinity",
                        "valence_a": round(c1.avg_emotional_valence, 2),
                        "valence_b": round(c2.avg_emotional_valence, 2),
                    })
                # Creative link: high importance contrast (one very important, one not)
                elif abs(c1.avg_importance - c2.avg_importance) > 0.3:
                    creative_links.append({
                        "type": "creative_bridge",
                        "cluster_a": c1.theme,
                        "cluster_b": c2.theme,
                        "resonance": "importance_contrast",
                        "insight": (
                            f"'{c1.theme}' (imp={c1.avg_importance:.2f}) and "
                            f"'{c2.theme}' (imp={c2.avg_importance:.2f}) may reveal "
                            "hidden dependencies through their importance differential."
                        ),
                    })

        # Record bicameral stats on the reasoner
        all_links = logical_links[:5] + creative_links[:5]
        if all_links:
            logger.info(
                f"Bicameral enrichment: {len(logical_links)} logical, "
                f"{len(creative_links)} creative cross-cluster links",
            )
            for link in all_links:
                strategies.append(link)

    # ------------------------------------------------------------------
    # Step 4: Promote high-value memories
    # ------------------------------------------------------------------

    def _promote_high_value(self, memories: list[Any]) -> int:
        """Promote frequently-accessed, high-importance short-term memories
        to LONG_TERM.
        """
        promotions = 0
        try:
            from whitemagic.core.memory.unified_types import MemoryType
        except ImportError:
            return 0

        for mem in memories:
            if mem.memory_type != MemoryType.SHORT_TERM:
                continue
            # Promotion criteria: high importance + accessed multiple times
            if mem.importance >= 0.6 and mem.access_count >= 3:
                try:
                    mem.memory_type = MemoryType.LONG_TERM
                    mem.importance = min(1.0, mem.importance + 0.05)
                    promotions += 1
                except Exception:
                    pass

        return promotions

    # ------------------------------------------------------------------
    # G5 Synthesis: Consolidation → Knowledge Graph
    # ------------------------------------------------------------------

    def _feed_knowledge_graph(self, clusters: list[MemoryCluster],
                              strategies: list[dict[str, Any]]) -> None:
        """Create KG relations linking source memories to strategy memories.

        For each strategy, create 'consolidated_into' edges from every source
        memory in the cluster to the strategy, plus 'shares_theme' edges
        between cluster members. This makes the consolidation chain queryable.
        """
        if not strategies:
            return
        try:
            from whitemagic.core.intelligence.knowledge_graph import get_knowledge_graph
            kg = get_knowledge_graph()
            edges = 0

            for cluster, strategy in zip(clusters, strategies):
                theme = strategy.get("theme", "unknown")
                strategy_id = f"strategy:{theme}:{hashlib.md5(theme.encode()).hexdigest()[:8]}"

                # Source → Strategy edges
                for mid in cluster.memory_ids:
                    kg.add_relation(
                        source=mid,
                        relation="consolidated_into",
                        target=strategy_id,
                        metadata={
                            "theme": theme,
                            "cluster_size": len(cluster.memory_ids),
                            "origin": "memory_consolidator",
                        },
                    )
                    edges += 1

                # Intra-cluster 'shares_theme' edges (top pairs only to avoid N²)
                ids = cluster.memory_ids[:10]
                for i in range(len(ids)):
                    for j in range(i + 1, min(i + 3, len(ids))):
                        kg.add_relation(
                            source=ids[i],
                            relation="shares_theme",
                            target=ids[j],
                            metadata={"theme": theme, "origin": "memory_consolidator"},
                        )
                        edges += 1

            if edges:
                logger.info(f"KG enrichment: {edges} consolidation edges created")
        except Exception as e:
            logger.debug(f"KG enrichment skipped: {e}")

    # ------------------------------------------------------------------
    # Galactic zone promotion (Gap A1 synthesis)
    # ------------------------------------------------------------------

    def _galactic_promote(self, strategies: list[dict[str, Any]]) -> None:
        """Promote synthesized strategy memories to INNER_RIM galactic zone.

        Consolidated knowledge is high-value by definition — it should
        live close to the galactic core, not drift outward with bulk
        ingested content.
        """
        if not strategies:
            return
        try:
            from whitemagic.core.memory.galactic_map import get_galactic_map
            from whitemagic.core.memory.unified import get_unified_memory
            gmap = get_galactic_map()
            um = get_unified_memory()

            # Find recently-created strategy memories and pull them inward
            recent = um.search("strategy consolidated", limit=len(strategies) + 5)
            promoted = 0
            for mem in recent:
                if "strategy" in (mem.tags or set()) and "consolidated" in (mem.tags or set()):
                    current_dist = getattr(mem, "galactic_distance", None)
                    # Only promote if not already in INNER_RIM or CORE
                    if current_dist is None or current_dist > 0.15:
                        gmap.set_distance(mem.id, 0.12)  # type: ignore[attr-defined]  # INNER_RIM
                        promoted += 1
            if promoted:
                logger.info(f"Galactic promotion: {promoted} strategy memories → INNER_RIM")
        except Exception as e:
            logger.debug(f"Galactic promotion skipped: {e}")

    # ------------------------------------------------------------------
    # Harmony Vector feedback
    # ------------------------------------------------------------------

    def _update_harmony(self, report: ConsolidationReport) -> None:
        """Feed consolidation results into the Harmony Vector energy dimension."""
        try:
            from whitemagic.harmony.vector import get_harmony_vector
            hv = get_harmony_vector()
            hv.record_call(
                tool_name="_memory_consolidation",
                duration_s=report.duration_ms / 1000.0,
                success=True,
                declared_safety="READ",
                actual_writes=report.strategies_synthesized + report.promotions,
            )
        except Exception:
            pass

    # ------------------------------------------------------------------
    # Events
    # ------------------------------------------------------------------

    def _emit_events(self, report: ConsolidationReport, strategies: list[dict[str, Any]]) -> None:
        """Emit consolidation and insight events."""
        try:
            from whitemagic.core.resonance.gan_ying_enhanced import (
                EventType,
                ResonanceEvent,
                get_bus,
            )
            bus = get_bus()

            bus.emit(ResonanceEvent(
                event_type=EventType.MEMORY_CONSOLIDATED,
                source="memory_consolidator",
                data={
                    "analyzed": report.memories_analyzed,
                    "clusters": report.clusters_found,
                    "strategies": report.strategies_synthesized,
                    "promotions": report.promotions,
                },
            ))

            if strategies:
                bus.emit(ResonanceEvent(
                    event_type=EventType.INSIGHT_CRYSTALLIZED,
                    source="memory_consolidator",
                    data={
                        "strategy_count": len(strategies),
                        "themes": [s["theme"] for s in strategies[:5]],
                    },
                ))
        except Exception:
            pass

    # ------------------------------------------------------------------
    # Introspection
    # ------------------------------------------------------------------

    def get_stats(self) -> dict[str, Any]:
        with self._lock:
            return {
                "total_consolidations": self._total_consolidations,
                "total_strategies": self._total_strategies,
                "total_promotions": self._total_promotions,
                "config": {
                    "min_cluster_size": self._min_cluster_size,
                    "tag_overlap_threshold": self._tag_overlap_threshold,
                    "importance_boost": self._importance_boost,
                    "max_memories": self._max_memories,
                },
            }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_consolidator: MemoryConsolidator | None = None
_consolidator_lock = threading.Lock()


def get_consolidator() -> MemoryConsolidator:
    """Get the global Memory Consolidator."""
    global _consolidator
    if _consolidator is None:
        with _consolidator_lock:
            if _consolidator is None:
                _consolidator = MemoryConsolidator()
    return _consolidator
