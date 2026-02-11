"""Emergence Engine - Systemic Insight Synthesis.
=============================================

"When a hundred paths meet, a single gate opens."

The EmergenceEngine monitors the Gan Ying bus for SYMPATHETIC_RESONANCE.
If a cluster of related resonators is triggered, it initiates a high-order
synthesis of the emergent knowledge.

v14 Enhancement: Proactive core scanning via CoreAccessLayer.
- Scans association graph for emergent clustering (new themes forming)
- Monitors constellation growth for convergence events
- Feeds proactive discoveries into the Gan Ying bus
"""

import logging
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

from whitemagic.core.resonance.gan_ying_enhanced import (
    EventType,
    ResonanceEvent,
    get_bus,
)

logger = logging.getLogger(__name__)


@dataclass
class EmergenceInsight:
    """An emergent insight discovered by proactive scanning."""

    id: str
    title: str
    description: str
    source: str  # "resonance_cascade", "constellation_convergence", "association_cluster"
    confidence: float
    related_ids: list[str] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)
    discovered_at: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> dict[str, Any]:
        return {
            "id": self.id,
            "title": self.title,
            "description": self.description,
            "source": self.source,
            "confidence": round(self.confidence, 3),
            "related_ids": self.related_ids[:10],
            "discovered_at": self.discovered_at.isoformat(),
        }


class EmergenceEngine:
    """Watches resonance cascades and crystallizes insights.

    Now also proactively scans the holographic core via CoreAccessLayer
    to detect emergent patterns before they crystallize into full
    resonance cascades.
    """

    def __init__(self, threshold: int = 4) -> None:
        self._bus = get_bus()
        self._cluster_map: dict[str, list[str]] = defaultdict(list)
        self._threshold = threshold
        self._is_active = False
        self._insights: list[EmergenceInsight] = []
        self._max_insights = 50

    def _get_core_access(self) -> Any:
        """Lazy-load the CoreAccessLayer."""
        try:
            from whitemagic.core.intelligence.core_access import get_core_access
            return get_core_access()
        except Exception:
            return None

    def start(self) -> Any:
        """Register listeners on the bus."""
        if self._is_active:
            return

        logger.info("🛰️ Emergence Engine ONLINE. Monitoring resonance clusters...")
        self._bus.listen(EventType.SYMPATHETIC_RESONANCE, self._on_sympathetic_resonance)
        self._is_active = True

    def _on_sympathetic_resonance(self, event: ResonanceEvent) -> Any:
        """Callback for sympathetic events."""
        original_pattern = event.data.get("original_pattern")
        pattern_id = event.data.get("pattern_id")

        if not original_pattern or not pattern_id:
            return

        self._cluster_map[original_pattern].append(pattern_id)

        count = len(self._cluster_map[original_pattern])

        if count >= self._threshold:
            self._crystallize_insight(original_pattern)

    def _crystallize_insight(self, pattern: str) -> Any:
        """Perform synthesis when a cluster matures."""
        related_ids = self._cluster_map.pop(pattern)

        logger.info(f"✨ EMERGENCE DETECTED around '{pattern}'! Fusing {len(related_ids)} related nodes...")

        bonus = max(0, (len(related_ids) - self._threshold) * 0.02)
        total_gain = 0.05 + bonus

        insight = EmergenceInsight(
            id=f"resonance_{pattern[:20]}_{len(related_ids)}",
            title=f"Resonance Cascade: {pattern}",
            description=f"Cluster of {len(related_ids)} related patterns crystallized around '{pattern}'",
            source="resonance_cascade",
            confidence=min(0.99, 0.7 + (len(related_ids) * 0.05)),
            related_ids=related_ids,
            metadata={"resonance_gain": round(total_gain, 3)},
        )
        self._record_insight(insight)

        self._bus.emit(ResonanceEvent(
            source="emergence_engine",
            event_type=EventType.BREAKTHROUGH_ACHIEVED,
            data={
                "core_pattern": pattern,
                "cluster_size": len(related_ids),
                "insight_nodes": related_ids,
                "resonance_gain": round(total_gain, 3),
            },
            timestamp=datetime.now(),
            confidence=insight.confidence,
        ))

        logger.info(f"🏆 System breakthrough recorded for cluster '{pattern}' (Gain: +{total_gain:.2f})")

    # ------------------------------------------------------------------
    # Proactive Core Scanning (new in v14)
    # ------------------------------------------------------------------

    def scan_for_emergence(self) -> list[EmergenceInsight]:
        """Proactively scan the holographic core for emergent patterns.

        This is the key upgrade: instead of just waiting for resonance
        events, the engine actively queries the Data Sea through the
        CoreAccessLayer to detect:
        1. Constellation convergence (two constellations growing toward each other)
        2. Association graph clustering (dense subgraphs forming)
        3. High-velocity memory regions (burst activity in a narrow domain)

        Returns newly discovered insights.
        """
        cal = self._get_core_access()
        if not cal:
            return []

        new_insights: list[EmergenceInsight] = []

        # 1. Constellation convergence: find pairs with shared tags but no bridges
        new_insights.extend(self._scan_constellation_convergence(cal))

        # 2. Association density hotspots: find memories with unusually many connections
        new_insights.extend(self._scan_association_hotspots(cal))

        # 3. Temporal burst detection: find recent activity spikes
        new_insights.extend(self._scan_temporal_bursts(cal))

        for insight in new_insights:
            self._record_insight(insight)

        if new_insights:
            logger.info(f"🔭 Emergence scan found {len(new_insights)} new insights")

        return new_insights

    def _scan_constellation_convergence(self, cal: Any) -> list[EmergenceInsight]:
        """Detect constellations that share themes and are spatially close."""
        insights: list[EmergenceInsight] = []
        constellations = cal.get_all_constellations()
        if len(constellations) < 2:
            return insights

        import math
        for i in range(len(constellations)):
            for j in range(i + 1, len(constellations)):
                c1, c2 = constellations[i], constellations[j]

                # Check tag overlap
                overlap = set(c1.dominant_tags) & set(c2.dominant_tags)
                if not overlap:
                    continue

                # Check spatial proximity
                dist = math.sqrt(sum(
                    (a - b) ** 2
                    for a, b in zip(c1.centroid, c2.centroid)
                ))

                # Close constellations with shared themes = convergence
                if dist < 1.0 and len(overlap) >= 1:
                    insights.append(EmergenceInsight(
                        id=f"convergence_{c1.name[:15]}_{c2.name[:15]}",
                        title=f"Constellation Convergence: {c1.name} ↔ {c2.name}",
                        description=(
                            f"Constellations '{c1.name}' ({c1.size} members) and "
                            f"'{c2.name}' ({c2.size} members) share tags "
                            f"[{', '.join(overlap)}] and are only {dist:.2f} apart "
                            f"in 5D space. They may be merging into a new domain."
                        ),
                        source="constellation_convergence",
                        confidence=min(0.95, 0.5 + len(overlap) * 0.15 + (1.0 - dist) * 0.3),
                        metadata={
                            "distance": round(dist, 3),
                            "shared_tags": list(overlap),
                            "c1_size": c1.size,
                            "c2_size": c2.size,
                        },
                    ))

        return insights[:5]  # Cap to avoid noise

    def _scan_association_hotspots(self, cal: Any) -> list[EmergenceInsight]:
        """Find memories with unusually dense association neighborhoods."""
        insights: list[EmergenceInsight] = []

        try:
            conn = cal._get_conn()
            rows = conn.execute("""
                SELECT m.id, m.title,
                       COUNT(*) as assoc_count,
                       AVG(a.strength) as avg_strength
                FROM associations a
                JOIN memories m ON a.source_id = m.id
                GROUP BY a.source_id
                HAVING assoc_count > 20 AND avg_strength > 0.5
                ORDER BY assoc_count DESC
                LIMIT 5
            """).fetchall()
        except Exception:
            return insights

        for row in rows:
            insights.append(EmergenceInsight(
                id=f"hotspot_{row['id'][:16]}",
                title=f"Association Hotspot: {row['title'] or 'Untitled'}",
                description=(
                    f"Memory '{row['title']}' has {row['assoc_count']} associations "
                    f"with avg strength {row['avg_strength']:.2f}. This is a hub node "
                    f"in the knowledge graph — emerging theme center."
                ),
                source="association_cluster",
                confidence=min(0.9, 0.5 + row["assoc_count"] * 0.01),
                related_ids=[row["id"]],
                metadata={
                    "assoc_count": row["assoc_count"],
                    "avg_strength": round(row["avg_strength"], 3),
                },
            ))

        return insights

    def _scan_temporal_bursts(self, cal: Any) -> list[EmergenceInsight]:
        """Detect temporal bursts in memory creation."""
        insights: list[EmergenceInsight] = []

        buckets = cal.query_temporal_activity(time_window="14d", bucket="1d")
        if len(buckets) < 3:
            return insights

        # Find days with creation significantly above average
        avg_creation = sum(b.memories_created for b in buckets) / len(buckets)
        if avg_creation == 0:
            return insights

        for bucket in buckets[-7:]:  # Only recent week
            if bucket.memories_created > avg_creation * 3 and bucket.memories_created > 5:
                insights.append(EmergenceInsight(
                    id=f"burst_{bucket.period}",
                    title=f"Activity Burst on {bucket.period}",
                    description=(
                        f"Created {bucket.memories_created} memories on {bucket.period} "
                        f"(average: {avg_creation:.1f}/day). This burst may contain "
                        f"emerging themes worth consolidating."
                    ),
                    source="temporal_burst",
                    confidence=min(0.85, 0.5 + (bucket.memories_created / avg_creation) * 0.1),
                    metadata={
                        "date": bucket.period,
                        "count": bucket.memories_created,
                        "avg": round(avg_creation, 1),
                    },
                ))

        return insights[:3]

    # ------------------------------------------------------------------
    # Insight management
    # ------------------------------------------------------------------

    def _record_insight(self, insight: EmergenceInsight) -> None:
        """Record an insight, maintaining bounded history."""
        self._insights.append(insight)
        if len(self._insights) > self._max_insights:
            self._insights = self._insights[-self._max_insights:]

    def get_insights(self, limit: int = 10) -> list[dict[str, Any]]:
        """Get recent emergence insights."""
        return [i.to_dict() for i in self._insights[-limit:]]

    def get_status(self) -> dict[str, Any]:
        """Get engine status."""
        return {
            "active": self._is_active,
            "pending_clusters": len(self._cluster_map),
            "total_insights": len(self._insights),
            "threshold": self._threshold,
        }


_engine = None
def get_emergence_engine() -> EmergenceEngine:
    global _engine
    if _engine is None:
        _engine = EmergenceEngine()
    return _engine
