"""Holographic Pattern Engine.
===========================

Detects patterns by analyzing 4D memory distribution:
- Density clusters (many memories in same region)
- Axis correlations (patterns along axes)
- Temporal flows (Z-axis progressions)
- Importance gradients (W-axis patterns)

Outputs insights for decision-making engines.
"""

import logging
import sqlite3
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any

from whitemagic.config.paths import DB_PATH

logger = logging.getLogger(__name__)

@dataclass
class Pattern:
    """A detected pattern in memory space."""

    pattern_type: str  # density, flow, correlation, gradient
    description: str
    confidence: float
    evidence: list[str] = field(default_factory=list)
    location: tuple[float, float, float, float] | None = None

@dataclass
class Insight:
    """An actionable insight derived from patterns."""

    title: str
    description: str
    related_patterns: list[str]
    actionable: bool = True
    priority: float = 0.5

class HolographicPatternEngine:
    """Detect patterns in 4D memory space."""

    def __init__(self, db_path: Path | None = None):
        self.db_path = db_path or DB_PATH

    def _get_memories(self) -> list[dict]:
        conn = sqlite3.connect(self.db_path)
        conn.row_factory = sqlite3.Row
        results = conn.execute("""
            SELECT m.id, m.title, m.content, m.importance, m.memory_type,
                   h.x, h.y, h.z, h.w
            FROM memories m
            JOIN holographic_coords h ON m.id = h.memory_id
        """).fetchall()
        conn.close()
        return [dict(r) for r in results]

    def detect_density_patterns(self) -> list[Pattern]:
        """Find regions of high memory density using SQL grouping."""
        conn = sqlite3.connect(self.db_path)
        conn.row_factory = sqlite3.Row

        # SQL-based spatial bucketing
        results = conn.execute("""
            SELECT
                ROUND(h.x * 2) / 2.0 as sx,
                ROUND(h.y * 2) / 2.0 as sy,
                ROUND(h.z * 2) / 2.0 as sz,
                ROUND(h.w * 2) / 2.0 as sw,
                COUNT(*) as density
            FROM memories m
            JOIN holographic_coords h ON m.id = h.memory_id
            GROUP BY sx, sy, sz, sw
            HAVING density >= 5
            ORDER BY density DESC
            LIMIT 50
        """).fetchall()

        patterns = []
        for r in results:
            loc = (r["sx"], r["sy"], r["sz"], r["sw"])
            density = r["density"]

            axis_desc = []
            if loc[0] < 0:
                axis_desc.append("logical")
            elif loc[0] > 0:
                axis_desc.append("emotional")
            if loc[1] < 0:
                axis_desc.append("detailed")
            elif loc[1] > 0:
                axis_desc.append("strategic")
            if loc[3] > 0.7:
                axis_desc.append("important")

            patterns.append(Pattern(
                pattern_type="density",
                description=f"High concentration of {' '.join(axis_desc)} memories ({density} items)",
                confidence=min(1.0, density / 20),
                location=loc,
            ))
        conn.close()
        return patterns

    def detect_temporal_flows(self) -> list[Pattern]:
        """Detect patterns in time dimension (Z-axis) using SQL."""
        conn = sqlite3.connect(self.db_path)

        counts = conn.execute("""
            SELECT
                SUM(CASE WHEN z < -0.1 THEN 1 ELSE 0 END) as past,
                SUM(CASE WHEN z BETWEEN -0.1 AND 0.1 THEN 1 ELSE 0 END) as present,
                SUM(CASE WHEN z > 0.1 THEN 1 ELSE 0 END) as future
            FROM holographic_coords
        """).fetchone()
        conn.close()

        patterns = []
        past, _present, future = counts[0] or 0, counts[1] or 0, counts[2] or 0

        if past > future * 2 and past > 10:
            patterns.append(Pattern(
                pattern_type="flow",
                description=f"Memory bias toward past ({past} past vs {future} future)",
                confidence=0.8,
            ))
        elif future > past * 2 and future > 10:
            patterns.append(Pattern(
                pattern_type="flow",
                description=f"Memory bias toward future planning ({future} future-oriented)",
                confidence=0.8,
            ))

        return patterns

    def detect_importance_gradient(self) -> list[Pattern]:
        """Analyze importance distribution."""
        memories = self._get_memories()
        patterns = []

        importances = [m["w"] for m in memories]
        avg_importance = sum(importances) / len(importances) if importances else 0
        high_importance = [m for m in memories if m["w"] > 0.8]

        if avg_importance > 0.7:
            patterns.append(Pattern(
                pattern_type="gradient",
                description=f"High average importance ({avg_importance:.2f}) - may need priority filtering",
                confidence=0.7,
                evidence=[(m["title"] or "Untitled")[:40] for m in high_importance[:5]],
            ))

        # Check for importance clusters by topic
        type_importance: dict[str, list[float]] = {}
        for m in memories:
            t = m["memory_type"] or "unknown"
            if t not in type_importance:
                type_importance[t] = []
            type_importance[t].append(m["w"])

        for mem_type, imps in type_importance.items():
            if len(imps) > 10:
                avg = sum(imps) / len(imps)
                if avg > 0.8:
                    patterns.append(Pattern(
                        pattern_type="gradient",
                        description=f"{mem_type} memories have high importance ({avg:.2f} avg)",
                        confidence=0.75,
                    ))

        return patterns

    def detect_correlations(self) -> list[Pattern]:
        """Find correlations between axes."""
        memories = self._get_memories()
        patterns = []

        # Check X-W correlation (logic/emotion vs importance)
        logical = [m for m in memories if m["x"] < -0.2]
        emotional = [m for m in memories if m["x"] > 0.2]

        if logical and emotional:
            avg_logic_importance = sum(m["w"] for m in logical) / len(logical)
            avg_emotion_importance = sum(m["w"] for m in emotional) / len(emotional)

            if abs(avg_logic_importance - avg_emotion_importance) > 0.15:
                higher = "logical" if avg_logic_importance > avg_emotion_importance else "emotional"
                patterns.append(Pattern(
                    pattern_type="correlation",
                    description=f"{higher.title()} memories rated higher importance on average",
                    confidence=0.7,
                    evidence=[f"Logical avg: {avg_logic_importance:.2f}",
                             f"Emotional avg: {avg_emotion_importance:.2f}"],
                ))

        return patterns

    def generate_insights(self, patterns: list[Pattern]) -> list[Insight]:
        """Generate actionable insights from patterns."""
        insights = []

        density_patterns = [p for p in patterns if p.pattern_type == "density"]
        if len(density_patterns) >= 3:
            insights.append(Insight(
                title="Memory Clustering Opportunity",
                description=f"Found {len(density_patterns)} high-density regions. Consider creating topic summaries.",
                related_patterns=[p.description[:50] for p in density_patterns[:3]],
                priority=0.8,
            ))

        flow_patterns = [p for p in patterns if p.pattern_type == "flow"]
        for p in flow_patterns:
            if "past" in p.description.lower():
                insights.append(Insight(
                    title="Balance Time Perspective",
                    description="Memory collection is past-heavy. Consider adding future goals and plans.",
                    related_patterns=[p.description],
                    priority=0.6,
                ))

        gradient_patterns = [p for p in patterns if p.pattern_type == "gradient"]
        for p in gradient_patterns:
            if "high average" in p.description.lower():
                insights.append(Insight(
                    title="Importance Inflation",
                    description="Most memories rated high importance. Consider recalibrating or tiering.",
                    related_patterns=[p.description],
                    priority=0.5,
                ))

        return insights

    def analyze(self) -> dict[str, Any]:
        """Run full pattern analysis."""
        patterns = []
        patterns.extend(self.detect_density_patterns())
        patterns.extend(self.detect_temporal_flows())
        patterns.extend(self.detect_importance_gradient())
        patterns.extend(self.detect_correlations())

        insights = self.generate_insights(patterns)

        return {
            "timestamp": datetime.now().isoformat(),
            "total_patterns": len(patterns),
            "patterns": patterns,
            "total_insights": len(insights),
            "insights": insights,
        }

def get_pattern_engine() -> HolographicPatternEngine:
    return HolographicPatternEngine()

if __name__ == "__main__":
    engine = get_pattern_engine()
    result = engine.analyze()

    logger.info("Holographic Pattern Analysis")
    logger.info("=" * 50)
    logger.info(f"Patterns found: {result['total_patterns']}")
    logger.info(f"Insights generated: {result['total_insights']}")

    logger.info("\n--- Patterns ---")
    for p in result["patterns"]:
        logger.info(f"  [{p.pattern_type}] {p.description}")
        if p.evidence:
            logger.info(f"       Evidence: {p.evidence[:2]}")

    logger.info("\n--- Insights ---")
    for i in result["insights"]:
        logger.info(f"  [{i.priority:.1f}] {i.title}")
        logger.info(f"       {i.description}")
