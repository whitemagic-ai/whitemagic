"""InsightPipeline — Proactive Intelligence Briefing System.
=========================================================
Orchestrates all four insight engines (Predictive, Kaizen, Serendipity,
Emergence) through the CoreAccessLayer, synthesizes their outputs into
a unified, decision-ready briefing.

The pipeline can be run:
- At session_bootstrap (proactive briefing on startup)
- During Dream Cycle phases (autonomous intelligence)
- On-demand via MCP tool

Output is an InsightBriefing: a prioritized, deduplicated summary of
predictions, improvement proposals, surfaced memories, and emergent
patterns — all cross-referenced against constellations and the
association graph.

Usage:
    from whitemagic.core.intelligence.insight_pipeline import get_insight_pipeline
    pipeline = get_insight_pipeline()
    briefing = pipeline.generate_briefing()
    print(briefing.to_dict())
"""

from __future__ import annotations

import logging
import time
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Data structures
# ---------------------------------------------------------------------------

@dataclass
class BriefingItem:
    """A single actionable item in the briefing."""

    id: str
    category: str  # "prediction", "improvement", "discovery", "emergence"
    title: str
    description: str
    priority: str  # "critical", "high", "medium", "low"
    confidence: float
    source_engine: str  # "predictive", "kaizen", "serendipity", "emergence"
    suggested_actions: list[str] = field(default_factory=list)
    related_constellations: list[str] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        return {
            "id": self.id,
            "category": self.category,
            "title": self.title,
            "description": self.description,
            "priority": self.priority,
            "confidence": round(self.confidence, 3),
            "source_engine": self.source_engine,
            "suggested_actions": self.suggested_actions[:5],
            "related_constellations": self.related_constellations[:3],
        }


@dataclass
class InsightBriefing:
    """A complete intelligence briefing."""

    timestamp: str
    duration_ms: float
    items: list[BriefingItem]
    summary: dict[str, Any]
    constellation_context: list[dict[str, Any]]
    velocity_metrics: dict[str, Any]

    def to_dict(self) -> dict[str, Any]:
        return {
            "timestamp": self.timestamp,
            "duration_ms": round(self.duration_ms, 1),
            "total_items": len(self.items),
            "items": [item.to_dict() for item in self.items],
            "summary": self.summary,
            "constellation_context": self.constellation_context[:5],
            "velocity_metrics": self.velocity_metrics,
        }

    @property
    def critical_items(self) -> list[BriefingItem]:
        return [i for i in self.items if i.priority == "critical"]

    @property
    def high_items(self) -> list[BriefingItem]:
        return [i for i in self.items if i.priority == "high"]

    def format_text(self, max_items: int = 10) -> str:
        """Format briefing as human-readable text for session handoff."""
        lines = [
            f"=== Intelligence Briefing ({self.timestamp}) ===",
            f"Generated in {self.duration_ms:.0f}ms | {len(self.items)} insights\n",
        ]

        # Velocity snapshot
        vm = self.velocity_metrics
        if vm:
            lines.append(f"Memory Velocity: {vm.get('daily_avg_7d', 0)}/day "
                        f"(acceleration: {vm.get('acceleration', 1.0):.1f}x)")
            lines.append("")

        # Critical items first
        critical = self.critical_items
        if critical:
            lines.append("!! CRITICAL !!")
            for item in critical[:3]:
                lines.append(f"  - [{item.source_engine}] {item.title}")
                if item.suggested_actions:
                    lines.append(f"    Action: {item.suggested_actions[0]}")
            lines.append("")

        # Top items by priority
        shown = set(i.id for i in critical)
        remaining = [i for i in self.items if i.id not in shown]
        remaining.sort(key=lambda i: {"high": 0, "medium": 1, "low": 2}.get(i.priority, 3))

        for item in remaining[:max_items - len(critical)]:
            emoji = {"prediction": "🔮", "improvement": "🔧",
                     "discovery": "💡", "emergence": "✨"}.get(item.category, "📌")
            lines.append(f"{emoji} [{item.priority.upper()}] {item.title}")
            if item.description:
                lines.append(f"   {item.description[:120]}")

        # Constellation context
        if self.constellation_context:
            lines.append("")
            lines.append(f"Active Constellations: {len(self.constellation_context)}")
            for c in self.constellation_context[:3]:
                lines.append(f"  - {c.get('name', '?')} ({c.get('size', 0)} members, "
                           f"zone: {c.get('zone', '?')})")

        return "\n".join(lines)


# ---------------------------------------------------------------------------
# Pipeline
# ---------------------------------------------------------------------------

class InsightPipeline:
    """Orchestrates insight generation from all four engines.

    Runs each engine, normalizes outputs into BriefingItems,
    cross-references with constellation context, deduplicates,
    and produces a prioritized InsightBriefing.
    """

    def __init__(self, max_items: int = 30, persist_briefings: bool = True) -> None:
        self._max_items = max_items
        self._persist_briefings = persist_briefings
        self._briefing_count = 0

    def generate_briefing(
        self,
        include_predictions: bool = True,
        include_kaizen: bool = True,
        include_serendipity: bool = True,
        include_emergence: bool = True,
        serendipity_count: int = 5,
    ) -> InsightBriefing:
        """Generate a complete intelligence briefing.

        Each engine is called independently and gracefully — if one fails,
        the others still contribute.
        """
        start = time.perf_counter()
        items: list[BriefingItem] = []

        # --- CoreAccessLayer context ---
        cal = self._get_core_access()
        velocity = cal.get_velocity_metrics() if cal else {}
        constellations = [c.to_dict() for c in cal.get_all_constellations()] if cal else []
        drift_7d = cal.get_constellation_drift(window_days=7) if cal else []
        drift_30d = cal.get_constellation_drift(window_days=30) if cal else []

        # --- Predictive Engine ---
        if include_predictions:
            items.extend(self._run_predictive())

        # --- Kaizen Engine ---
        if include_kaizen:
            items.extend(self._run_kaizen())

        # --- Serendipity Engine ---
        if include_serendipity:
            items.extend(self._run_serendipity(serendipity_count))

        # --- Emergence Engine ---
        if include_emergence:
            items.extend(self._run_emergence())

        # --- Cross-reference with constellations ---
        if constellations:
            constellation_tags: dict[str, list[str]] = {}
            for c in constellations:
                for tag in c.get("dominant_tags", []):
                    if tag not in constellation_tags:
                        constellation_tags[tag] = []
                    constellation_tags[tag].append(c["name"])

            for item in items:
                # Match item keywords against constellation tags
                title_words = set(item.title.lower().split())
                for tag, names in constellation_tags.items():
                    if tag.lower() in title_words:
                        item.related_constellations.extend(names)
                item.related_constellations = list(set(item.related_constellations))[:3]

        # --- Deduplicate and prioritize ---
        items = self._deduplicate(items)
        items = self._prioritize(items)
        items = items[:self._max_items]

        # --- Build summary ---
        by_category: dict[str, int] = {}
        by_priority: dict[str, int] = {}
        by_engine: dict[str, int] = {}
        for item in items:
            by_category[item.category] = by_category.get(item.category, 0) + 1
            by_priority[item.priority] = by_priority.get(item.priority, 0) + 1
            by_engine[item.source_engine] = by_engine.get(item.source_engine, 0) + 1
        summary: dict[str, Any] = {
            "total_insights": len(items),
            "by_category": by_category,
            "by_priority": by_priority,
            "by_engine": by_engine,
        }

        duration = (time.perf_counter() - start) * 1000

        # Add drift alerts as briefing items
        for drift in drift_7d[:3]:
            if drift["drift_magnitude"] > 0.1:
                items.append(BriefingItem(
                    id=f"drift_{drift['name'][:20]}",
                    category="emergence",
                    title=f"Constellation drift: {drift['name']}",
                    description=f"Moved {drift['drift_magnitude']:.3f} units in 7d "
                                f"(dx={drift['drift_vector']['dx']}, dv={drift['drift_vector']['dv']})",
                    priority="medium" if drift["drift_magnitude"] < 0.3 else "high",
                    confidence=min(0.95, drift["drift_magnitude"] * 3),
                    source_engine="drift_tracker",
                    metadata=drift,
                ))

        # Re-deduplicate and prioritize after adding drift items
        items = self._deduplicate(items)
        items = self._prioritize(items)
        items = items[:self._max_items]

        # Rebuild summary with updated items
        by_category = {}
        by_priority = {}
        by_engine = {}
        for item in items:
            by_category[item.category] = by_category.get(item.category, 0) + 1
            by_priority[item.priority] = by_priority.get(item.priority, 0) + 1
            by_engine[item.source_engine] = by_engine.get(item.source_engine, 0) + 1
        summary = {
            "total_insights": len(items),
            "by_category": by_category,
            "by_priority": by_priority,
            "by_engine": by_engine,
            "drift_7d_count": len(drift_7d),
            "drift_30d_count": len(drift_30d),
            "max_drift_7d": drift_7d[0]["drift_magnitude"] if drift_7d else 0.0,
        }

        duration = (time.perf_counter() - start) * 1000

        briefing = InsightBriefing(
            timestamp=datetime.now().isoformat(),
            duration_ms=duration,
            items=items,
            summary=summary,
            constellation_context=constellations[:5],
            velocity_metrics=velocity,
        )

        logger.info(
            f"📋 InsightBriefing generated: {len(items)} items in {duration:.0f}ms "
            f"({summary.get('by_priority', {})})"
        )

        # Persist briefing as memory for calibration tracking
        if self._persist_briefings:
            self._persist_briefing_as_memory(briefing)

        return briefing

    # ------------------------------------------------------------------
    # Engine runners (each isolated, graceful failure)
    # ------------------------------------------------------------------

    def _run_predictive(self) -> list[BriefingItem]:
        """Run PredictiveEngine and normalize output."""
        try:
            from whitemagic.core.intelligence.synthesis.predictive_engine import get_predictive_engine
            engine = get_predictive_engine()
            report = engine.predict()

            items = []
            for pred in report.predictions[:15]:
                priority = self._confidence_to_priority(
                    pred.confidence.value if hasattr(pred.confidence, 'value') else str(pred.confidence),
                    pred.impact_score,
                )
                items.append(BriefingItem(
                    id=f"pred_{pred.id}",
                    category="prediction",
                    title=pred.title,
                    description=pred.description,
                    priority=priority,
                    confidence=pred.impact_score,
                    source_engine="predictive",
                    suggested_actions=pred.suggested_actions[:3],
                    metadata={
                        "prediction_type": pred.prediction_type.value if hasattr(pred.prediction_type, 'value') else str(pred.prediction_type),
                        "time_horizon": pred.time_horizon,
                    },
                ))
            return items
        except Exception as e:
            logger.warning(f"PredictiveEngine failed: {e}")
            return []

    def _run_kaizen(self) -> list[BriefingItem]:
        """Run KaizenEngine and normalize output."""
        try:
            from whitemagic.core.intelligence.synthesis.kaizen_engine import get_kaizen_engine
            engine = get_kaizen_engine()
            report = engine.analyze()

            items = []
            for proposal in report.proposals[:15]:
                priority = self._effort_impact_to_priority(proposal.impact, proposal.effort)
                items.append(BriefingItem(
                    id=f"kaizen_{proposal.id}",
                    category="improvement",
                    title=proposal.title,
                    description=proposal.description,
                    priority=priority,
                    confidence=0.7 if proposal.auto_fixable else 0.5,
                    source_engine="kaizen",
                    suggested_actions=[proposal.fix_action] if proposal.fix_action else [],
                    metadata={
                        "kaizen_category": proposal.category,
                        "auto_fixable": proposal.auto_fixable,
                    },
                ))
            return items
        except Exception as e:
            logger.warning(f"KaizenEngine failed: {e}")
            return []

    def _run_serendipity(self, count: int = 5) -> list[BriefingItem]:
        """Run SerendipityEngine and normalize output."""
        try:
            from whitemagic.core.intelligence.synthesis.serendipity_engine import get_serendipity_engine
            engine = get_serendipity_engine()
            surfaced = engine.surface(count=count, mode="balanced")

            items = []
            for mem in surfaced:
                items.append(BriefingItem(
                    id=f"serendipity_{mem.id[:16]}",
                    category="discovery",
                    title=f"Rediscover: {mem.title}",
                    description=f"{mem.reason}. Preview: {mem.content_preview[:100]}",
                    priority="low",
                    confidence=mem.relevance_score,
                    source_engine="serendipity",
                    metadata={
                        "memory_id": mem.id,
                        "gravity": mem.gravity,
                        "access_count": mem.access_count,
                    },
                ))
            return items
        except Exception as e:
            logger.warning(f"SerendipityEngine failed: {e}")
            return []

    def _run_emergence(self) -> list[BriefingItem]:
        """Run EmergenceEngine proactive scan and normalize output."""
        try:
            from whitemagic.core.intelligence.agentic.emergence_engine import get_emergence_engine
            engine = get_emergence_engine()

            # Run proactive scan
            insights = engine.scan_for_emergence()

            # Also include any past insights from resonance cascades
            past_insights = engine.get_insights(limit=5)

            items = []
            for insight in insights:
                items.append(BriefingItem(
                    id=f"emergence_{insight.id}",
                    category="emergence",
                    title=insight.title,
                    description=insight.description,
                    priority=self._confidence_to_priority_float(insight.confidence),
                    confidence=insight.confidence,
                    source_engine="emergence",
                    metadata={"source_type": insight.source},
                ))

            for past in past_insights:
                pid = past.get("id", "unknown")
                if not any(i.id == f"emergence_{pid}" for i in items):
                    items.append(BriefingItem(
                        id=f"emergence_{pid}",
                        category="emergence",
                        title=past.get("title", "Unknown Insight"),
                        description=past.get("description", ""),
                        priority="medium",
                        confidence=past.get("confidence", 0.5),
                        source_engine="emergence",
                        metadata={"source_type": past.get("source", "unknown")},
                    ))

            return items
        except Exception as e:
            logger.warning(f"EmergenceEngine failed: {e}")
            return []

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    def _get_core_access(self) -> Any:
        try:
            from whitemagic.core.intelligence.core_access import get_core_access
            return get_core_access()
        except Exception:
            return None

    @staticmethod
    def _confidence_to_priority(confidence_str: str, impact: float) -> str:
        """Map prediction confidence + impact to priority."""
        if impact >= 0.85:
            return "critical"
        if impact >= 0.7 or confidence_str == "high":
            return "high"
        if impact >= 0.5 or confidence_str == "medium":
            return "medium"
        return "low"

    @staticmethod
    def _confidence_to_priority_float(confidence: float) -> str:
        if confidence >= 0.85:
            return "high"
        if confidence >= 0.6:
            return "medium"
        return "low"

    @staticmethod
    def _effort_impact_to_priority(impact: str, effort: str) -> str:
        """Map kaizen impact/effort to priority (high impact + low effort = critical)."""
        if impact == "high" and effort == "low":
            return "critical"
        if impact == "high":
            return "high"
        if impact == "medium" and effort == "low":
            return "high"
        if impact == "medium":
            return "medium"
        return "low"

    def _persist_briefing_as_memory(self, briefing: InsightBriefing) -> None:
        """Persist a briefing as a LONG_TERM memory for calibration.

        This enables retroactive analysis: "I predicted X last week —
        did it come true?" The memory includes the full briefing summary
        and top items for future self-referencing.
        """
        try:
            from whitemagic.core.memory.unified import get_unified_memory
            from whitemagic.core.memory.unified_types import MemoryType
            um = get_unified_memory()

            self._briefing_count += 1
            title = f"Intelligence Briefing #{self._briefing_count} ({briefing.timestamp[:10]})"

            top_items_text = []
            for item in briefing.items[:8]:
                top_items_text.append(
                    f"[{item.priority.upper()}] ({item.source_engine}) {item.title}: {item.description[:120]}"
                )

            content = (
                f"InsightPipeline briefing generated at {briefing.timestamp}\n"
                f"Duration: {briefing.duration_ms:.0f}ms\n"
                f"Total insights: {len(briefing.items)}\n"
                f"Summary: {briefing.summary}\n\n"
                f"Top items:\n" + "\n".join(top_items_text)
            )

            um.store(
                content=content,
                title=title,
                memory_type=MemoryType.LONG_TERM,
                importance=0.6,
                tags={"insight_briefing", "intelligence", "calibration", "auto_generated"},
                metadata={
                    "briefing_number": self._briefing_count,
                    "total_items": len(briefing.items),
                    "by_priority": briefing.summary.get("by_priority", {}),
                    "velocity": briefing.velocity_metrics,
                },
            )
            logger.debug(f"Persisted briefing #{self._briefing_count} as memory")
        except Exception as e:
            logger.debug(f"Failed to persist briefing as memory: {e}")

    @staticmethod
    def _deduplicate(items: list[BriefingItem]) -> list[BriefingItem]:
        """Deduplicate by ID."""
        seen: set[str] = set()
        unique: list[BriefingItem] = []
        for item in items:
            if item.id not in seen:
                seen.add(item.id)
                unique.append(item)
        return unique

    @staticmethod
    def _prioritize(items: list[BriefingItem]) -> list[BriefingItem]:
        """Sort by priority then confidence."""
        priority_order = {"critical": 0, "high": 1, "medium": 2, "low": 3}
        items.sort(key=lambda i: (priority_order.get(i.priority, 4), -i.confidence))
        return items


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_pipeline: InsightPipeline | None = None


def get_insight_pipeline() -> InsightPipeline:
    global _pipeline
    if _pipeline is None:
        _pipeline = InsightPipeline()
    return _pipeline
