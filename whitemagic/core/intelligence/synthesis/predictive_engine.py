"""PredictiveEngine - Future-Forward Self-Improvement.

Analyzes patterns, milestones, and roadmaps to predict what WhiteMagic
(and any AI using it) needs next. Generates proactive improvement proposals
before they're requested.

Philosophy: The system should anticipate its own evolution, not just react.

Knowledge Gaps Addressed:
- Gap 1: Detail + Future (specific implementation plans)
- Gap 4: High Future + High Importance (strategic vision)
"""

import sqlite3
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any


class PredictionType(Enum):
    """Types of predictions the engine can generate."""

    NEXT_FEATURE = "next_feature"           # What feature to build next
    BOTTLENECK = "bottleneck"               # Where system will hit limits
    OPPORTUNITY = "opportunity"             # Unexplored potential
    MAINTENANCE = "maintenance"             # Upcoming maintenance needs
    INTEGRATION = "integration"             # Systems to connect
    EVOLUTION = "evolution"                 # Structural changes needed


class Confidence(Enum):
    """Confidence levels for predictions."""

    HIGH = "high"        # Strong pattern evidence
    MEDIUM = "medium"    # Reasonable inference
    LOW = "low"          # Speculative but valuable
    EMERGENT = "emergent"  # Novel insight, needs validation


@dataclass
class Prediction:
    """A prediction about future needs or opportunities."""

    id: str
    prediction_type: PredictionType
    title: str
    description: str
    confidence: Confidence
    time_horizon: str  # "immediate", "short_term", "medium_term", "long_term"
    evidence: list[str]  # Memory IDs or patterns supporting this
    suggested_actions: list[str]
    impact_score: float  # 0.0 to 1.0
    metadata: dict[str, Any] = field(default_factory=dict)
    generated_at: datetime = field(default_factory=datetime.now)


@dataclass
class PredictiveReport:
    """Complete prediction report."""

    predictions: list[Prediction]
    patterns_analyzed: int
    memories_scanned: int
    knowledge_gaps: list[dict[str, Any]]
    velocity_metrics: dict[str, float]
    generated_at: datetime = field(default_factory=datetime.now)

    @property
    def high_confidence_predictions(self) -> list[Prediction]:
        return [p for p in self.predictions if p.confidence == Confidence.HIGH]

    @property
    def by_type(self) -> dict[PredictionType, list[Prediction]]:
        result: dict[PredictionType, list[Prediction]] = {}
        for p in self.predictions:
            if p.prediction_type not in result:
                result[p.prediction_type] = []
            result[p.prediction_type].append(p)
        return result

    @property
    def by_horizon(self) -> dict[str, list[Prediction]]:
        result: dict[str, list[Prediction]] = {}
        for p in self.predictions:
            if p.time_horizon not in result:
                result[p.time_horizon] = []
            result[p.time_horizon].append(p)
        return result


class PredictiveEngine:
    """Predicts future needs and opportunities for self-improvement.

    Uses:
    - Milestone patterns (what was completed, what's next)
    - Roadmap analysis (stated goals vs actual progress)
    - Velocity metrics (development speed trends)
    - Gap analysis (underserved areas in 4D space)
    - Pattern extrapolation (recurring themes leading somewhere)
    """

    def __init__(self, db_path: str | None = None) -> None:
        from whitemagic.config.paths import DB_PATH
        self.db_path = str(Path(db_path)) if db_path else str(DB_PATH)
        self._conn: sqlite3.Connection | None = None

    def _get_conn(self) -> sqlite3.Connection:
        if self._conn is None:
            conn = sqlite3.connect(self.db_path)
            conn.row_factory = sqlite3.Row
            self._conn = conn
        return self._conn

    def _get_core_access(self) -> Any:
        """Lazy-load the CoreAccessLayer."""
        try:
            from whitemagic.core.intelligence.core_access import get_core_access
            return get_core_access()
        except Exception:
            return None

    def predict(self) -> PredictiveReport:
        """Generate comprehensive predictions."""
        predictions = []

        # Analyze different prediction sources
        predictions.extend(self._predict_from_milestones())
        predictions.extend(self._predict_from_roadmaps())
        predictions.extend(self._predict_from_velocity())
        predictions.extend(self._predict_from_gaps())
        predictions.extend(self._predict_from_patterns())
        predictions.extend(self._predict_from_automation())
        predictions.extend(self._predict_from_development_gardens())

        # Core-connected predictions (constellation, association, temporal)
        predictions.extend(self._predict_from_constellations())
        predictions.extend(self._predict_from_association_paths())
        predictions.extend(self._predict_from_temporal_trends())

        # Deduplicate and rank
        predictions = self._deduplicate_predictions(predictions)
        predictions.sort(key=lambda p: p.impact_score, reverse=True)

        # Get supporting metrics
        conn = self._get_conn()
        cur = conn.cursor()
        cur.execute("SELECT COUNT(*) FROM memories")
        mem_count = cur.fetchone()[0]

        return PredictiveReport(
            predictions=predictions,
            patterns_analyzed=self._count_patterns(),
            memories_scanned=mem_count,
            knowledge_gaps=self._get_knowledge_gaps(),
            velocity_metrics=self._calculate_velocity(),
        )

    def _predict_from_milestones(self) -> list[Prediction]:
        """Analyze milestone patterns to predict next steps."""
        predictions = []
        conn = self._get_conn()
        cur = conn.cursor()

        # Find milestone-tagged memories
        cur.execute("""
            SELECT m.id, m.title, m.content, m.created_at, h.w as importance
            FROM memories m
            JOIN tags t ON m.id = t.memory_id
            LEFT JOIN holographic_coords h ON m.id = h.memory_id
            WHERE t.tag IN ('milestone', 'complete', 'ready', 'phase', 'version')
            ORDER BY m.created_at DESC
            LIMIT 50
        """)
        milestones = cur.fetchall()

        if len(milestones) >= 3:
            # Analyze milestone velocity
            recent = milestones[:10]

            # Look for incomplete sequences
            phase_mentions: dict[str, list[str]] = {}
            for m in milestones:
                content = (m["content"] or "").lower()
                for phase in ["phase 1", "phase 2", "phase 3", "phase 4", "phase 5", "phase 6"]:
                    if phase in content:
                        if phase not in phase_mentions:
                            phase_mentions[phase] = []
                        phase_mentions[phase].append(m["title"])

            # Predict next phase if pattern detected
            if phase_mentions:
                max_phase = max(phase_mentions.keys())
                phase_num = int(max_phase.split()[-1])
                next_phase = f"phase {phase_num + 1}"

                if next_phase not in phase_mentions:
                    predictions.append(Prediction(
                        id=f"milestone_next_{next_phase.replace(' ', '_')}",
                        prediction_type=PredictionType.NEXT_FEATURE,
                        title=f"Next Phase: {next_phase.title()}",
                        description=f"Based on milestone patterns, {next_phase} should be planned next.",
                        confidence=Confidence.MEDIUM,
                        time_horizon="short_term",
                        evidence=[m["id"] for m in recent[:5]],
                        suggested_actions=[
                            f"Define goals for {next_phase}",
                            "Review lessons from previous phases",
                            "Create phase roadmap memory",
                        ],
                        impact_score=0.8,
                    ))

        return predictions

    def _predict_from_roadmaps(self) -> list[Prediction]:
        """Extract predictions from roadmap documents."""
        predictions = []
        conn = self._get_conn()
        cur = conn.cursor()

        # Find roadmap memories
        cur.execute("""
            SELECT m.id, m.title, m.content, h.z as future_score
            FROM memories m
            JOIN tags t ON m.id = t.memory_id
            LEFT JOIN holographic_coords h ON m.id = h.memory_id
            WHERE t.tag IN ('roadmap', 'strategy', 'plan', 'vision')
            ORDER BY h.z DESC NULLS LAST
            LIMIT 20
        """)
        roadmaps = cur.fetchall()

        # Extract action items from roadmaps
        action_patterns = [
            "TODO:", "NEXT:", "[ ]", "- [ ]", "upcoming:", "planned:",
            "will implement", "need to", "should add", "consider adding",
        ]

        unfulfilled_items = []
        for r in roadmaps:
            content = r["content"] or ""
            for pattern in action_patterns:
                if pattern.lower() in content.lower():
                    # Extract the line containing the pattern
                    for line in content.split("\n"):
                        if pattern.lower() in line.lower():
                            unfulfilled_items.append({
                                "source": r["title"],
                                "source_id": r["id"],
                                "item": line.strip()[:100],
                            })

        if unfulfilled_items:
            predictions.append(Prediction(
                id="roadmap_unfulfilled",
                prediction_type=PredictionType.NEXT_FEATURE,
                title=f"Unfulfilled Roadmap Items ({len(unfulfilled_items)})",
                description="Action items from roadmaps that may still be pending.",
                confidence=Confidence.MEDIUM,
                time_horizon="immediate",
                evidence=[item["source_id"] for item in unfulfilled_items[:5]],
                suggested_actions=[
                    f"Review: {item['item'][:50]}..." for item in unfulfilled_items[:3]
                ],
                impact_score=0.7,
                metadata={"items": unfulfilled_items[:10]},
            ))

        return predictions

    def _predict_from_velocity(self) -> list[Prediction]:
        """Analyze development velocity using unified metrics to predict capacity."""
        from whitemagic.core.intelligence.synthesis.shared import (
            calculate_unified_velocity,
        )

        predictions = []
        stats = calculate_unified_velocity(db_path=self.db_path)
        mem_stats = stats["memory"]
        task_stats = stats["tasks"]

        # 1. Accelerating Output (Memories)
        if mem_stats["last_7_days"] > (mem_stats["last_30_days"] / 4) * 1.5:
            predictions.append(Prediction(
                id="velocity_accelerating_memories",
                prediction_type=PredictionType.OPPORTUNITY,
                title="Memory Generation Accelerating",
                description=f"Output increased to {mem_stats['last_7_days']} memories/week. High capacity for synthesis.",
                confidence=Confidence.HIGH,
                time_horizon="immediate",
                evidence=[],
                suggested_actions=["Synthesize recent memories into patterns", "Execute multi-step plans"],
                impact_score=0.7,
            ))

        # 2. High Willpower (Tasks)
        if task_stats["willpower"] > 0.8:
            predictions.append(Prediction(
                id="velocity_high_willpower",
                prediction_type=PredictionType.OPPORTUNITY,
                title="High Task Willpower Detected",
                description=f"Task completion momentum is at {task_stats['willpower']*100:.0f}%. System is highly effective at execution.",
                confidence=Confidence.HIGH,
                time_horizon="immediate",
                evidence=[],
                suggested_actions=["Tackle high-risk tasks", "Expand project scope"],
                impact_score=0.8,
            ))
        elif task_stats["willpower"] < 0.3 and task_stats["tasks_total"] > 5:
            predictions.append(Prediction(
                id="velocity_low_willpower",
                prediction_type=PredictionType.BOTTLENECK,
                title="Low Task Momentum",
                description=f"Task completion momentum is low ({task_stats['willpower']*100:.0f}%). May indicate burnout or blockers.",
                confidence=Confidence.MEDIUM,
                time_horizon="short_term",
                evidence=[],
                suggested_actions=["Audit blockers", "Simplify active tasks", "Focus on low-effort wins"],
                impact_score=0.75,
            ))

        return predictions

    def _predict_from_gaps(self) -> list[Prediction]:
        """Generate predictions from 4D knowledge gaps."""
        predictions = []
        conn = self._get_conn()
        cur = conn.cursor()

        # Gap 1: Detail + Future
        cur.execute("SELECT COUNT(*) FROM holographic_coords WHERE y < -0.3 AND z > 0.2")
        if cur.fetchone()[0] == 0:
            predictions.append(Prediction(
                id="gap_detail_future",
                prediction_type=PredictionType.OPPORTUNITY,
                title="Knowledge Gap: Specific Implementation Plans",
                description="No memories in Detail+Future quadrant. Create specific, dated implementation plans.",
                confidence=Confidence.HIGH,
                time_horizon="short_term",
                evidence=[],
                suggested_actions=[
                    "Create 'Implementation Plan: [Feature]' memories",
                    "Add specific dates and milestones",
                    "Break roadmaps into concrete steps",
                ],
                impact_score=0.85,
                metadata={"gap_location": "Y<-0.3, Z>0.2"},
            ))

        # Gap 4: Strategic Vision (Future + High Importance)
        cur.execute("SELECT COUNT(*) FROM holographic_coords WHERE z > 0.3 AND w > 0.8")
        if cur.fetchone()[0] == 0:
            predictions.append(Prediction(
                id="gap_strategic_vision",
                prediction_type=PredictionType.EVOLUTION,
                title="Knowledge Gap: Strategic Vision Documents",
                description="No critical future-oriented memories. Create high-level vision documents.",
                confidence=Confidence.HIGH,
                time_horizon="medium_term",
                evidence=[],
                suggested_actions=[
                    "Create 'WhiteMagic 2027 Vision' memory",
                    "Document long-term architectural goals",
                    "Define success metrics for 6-12 months out",
                ],
                impact_score=0.9,
                metadata={"gap_location": "Z>0.3, W>0.8"},
            ))

        return predictions

    def _predict_from_patterns(self) -> list[Prediction]:
        """Extrapolate from recurring patterns using UnifiedPatternAPI."""
        from whitemagic.core.intelligence.synthesis.unified_patterns import (
            get_pattern_api,
        )

        predictions = []
        api = get_pattern_api()

        # 1. Tag Co-occurrences
        co_tags = api.search_tag_cooccurrence(min_count=5, limit=20)
        strong_pairs = [t for t in co_tags if t["co_count"] >= 8]

        if strong_pairs:
            predictions.append(Prediction(
                id="pattern_cooccurrence",
                prediction_type=PredictionType.INTEGRATION,
                title=f"Strong Topic Correlations ({len(strong_pairs)} pairs)",
                description="Frequently co-occurring tags suggest natural integration points.",
                confidence=Confidence.MEDIUM,
                time_horizon="medium_term",
                evidence=[],
                suggested_actions=[
                    f"Consider integrating '{p['tag1']}' + '{p['tag2']}' (appears {p['co_count']}x together)"
                    for p in strong_pairs[:3]
                ],
                impact_score=0.65,
                metadata={"pairs": strong_pairs[:10]},
            ))

        # 2. Pattern Correlations (Cross-Engine)
        correlations = api.find_correlations()
        if correlations:
            top = correlations[0]
            predictions.append(Prediction(
                id=f"pattern_correlation_{top['pattern1_id'][:8]}",
                prediction_type=PredictionType.EVOLUTION,
                title="Cross-Engine Pattern Convergence",
                description=f"High similarity ({top['similarity']:.2f}) detected between patterns in {top['engines'][0]} and {top['engines'][1]}.",
                confidence=Confidence.HIGH,
                time_horizon="short_term",
                evidence=[top["pattern1_id"], top["pattern2_id"]],
                suggested_actions=[
                    f"Consolidate logic for '{top['pattern1_title']}'",
                    "Update shared heuristics",
                ],
                impact_score=0.7,
            ))

        return predictions

    def _predict_from_automation(self) -> list[Prediction]:
        """Identify automation opportunities."""
        predictions = []
        conn = self._get_conn()
        cur = conn.cursor()

        # Find memories tagged with automation-related terms
        cur.execute("""
            SELECT COUNT(*) as cnt FROM tags
            WHERE tag IN ('automation', 'auto', 'automated', 'automatic', 'script', 'daemon')
        """)
        auto_count = cur.fetchone()["cnt"]

        # Find manual process mentions
        cur.execute("""
            SELECT COUNT(*) as cnt FROM memories
            WHERE content LIKE '%manually%' OR content LIKE '%by hand%' OR content LIKE '%repeat%'
        """)
        manual_count = cur.fetchone()["cnt"]

        if manual_count > auto_count:
            predictions.append(Prediction(
                id="automation_opportunity",
                prediction_type=PredictionType.OPPORTUNITY,
                title="Automation Opportunity Detected",
                description=f"Found {manual_count} mentions of manual processes vs {auto_count} automated ones.",
                confidence=Confidence.MEDIUM,
                time_horizon="short_term",
                evidence=[],
                suggested_actions=[
                    "Review manual process mentions",
                    "Prioritize automation of repetitive tasks",
                    "Consider daemon/background processing",
                ],
                impact_score=0.7,
            ))

        return predictions

    def _predict_from_development_gardens(self) -> list[Prediction]:
        """Analyze development-focused gardens for self-improvement predictions."""
        predictions = []
        conn = self._get_conn()
        cur = conn.cursor()

        # Development gardens to analyze
        dev_gardens = ["milestone", "automation", "roadmap", "strategy", "production_ready"]

        # Get recent activity in each garden
        garden_activity = {}
        for garden in dev_gardens:
            cur.execute("""
                SELECT COUNT(*) as count,
                       MAX(m.created_at) as latest,
                       AVG(h.w) as avg_gravity
                FROM memories m
                JOIN tags t ON m.id = t.memory_id
                LEFT JOIN holographic_coords h ON m.id = h.memory_id
                WHERE t.tag = ?
            """, (garden,))
            row = cur.fetchone()
            garden_activity[garden] = {
                "count": row["count"] or 0,
                "latest": row["latest"],
                "avg_gravity": row["avg_gravity"] or 0.5,
            }

        # Predict based on garden patterns
        # 1. If milestone count is high but automation is low -> suggest automation
        if garden_activity["milestone"]["count"] > 10 and garden_activity["automation"]["count"] < 5:
            predictions.append(Prediction(
                id="garden_automation_opportunity",
                prediction_type=PredictionType.OPPORTUNITY,
                title="Automation Gap: Many milestones, few automations",
                description=f"Found {garden_activity['milestone']['count']} milestones but only {garden_activity['automation']['count']} automations. Consider automating repetitive milestone patterns.",
                confidence=Confidence.MEDIUM,
                time_horizon="short_term",
                evidence=[],
                suggested_actions=[
                    "Review milestone patterns for automation opportunities",
                    "Create daemon for recurring tasks",
                    "Auto-generate milestone memories on phase completion",
                ],
                impact_score=0.75,
            ))

        # 2. If roadmap is stale (no recent activity) -> suggest roadmap update
        if garden_activity["roadmap"]["count"] > 0:
            cur.execute("""
                SELECT COUNT(*) FROM memories m
                JOIN tags t ON m.id = t.memory_id
                WHERE t.tag = 'roadmap'
                AND m.created_at > datetime('now', '-14 days')
            """)
            recent_roadmap = cur.fetchone()[0]
            if recent_roadmap == 0:
                predictions.append(Prediction(
                    id="garden_stale_roadmap",
                    prediction_type=PredictionType.MAINTENANCE,
                    title="Stale Roadmap: No updates in 14+ days",
                    description="Roadmap memories haven't been updated recently. Consider refreshing strategic direction.",
                    confidence=Confidence.MEDIUM,
                    time_horizon="immediate",
                    evidence=[],
                    suggested_actions=[
                        "Review and update roadmap memories",
                        "Check if milestones align with current roadmap",
                        "Create new roadmap for next phase",
                    ],
                    impact_score=0.7,
                ))

        # 3. High-gravity strategy memories -> prioritize their execution
        cur.execute("""
            SELECT m.id, m.title, h.w as gravity
            FROM memories m
            JOIN tags t ON m.id = t.memory_id
            JOIN holographic_coords h ON m.id = h.memory_id
            WHERE t.tag = 'strategy' AND h.w > 0.8
            ORDER BY h.w DESC
            LIMIT 5
        """)
        high_priority_strategies = cur.fetchall()

        if len(high_priority_strategies) >= 2:
            predictions.append(Prediction(
                id="garden_priority_strategies",
                prediction_type=PredictionType.NEXT_FEATURE,
                title=f"Priority: {len(high_priority_strategies)} high-gravity strategies",
                description="Multiple high-importance strategy memories identified. These should drive development priorities.",
                confidence=Confidence.HIGH,
                time_horizon="immediate",
                evidence=[s["id"] for s in high_priority_strategies],
                suggested_actions=[
                    f"Execute: {s['title'][:50]}..." for s in high_priority_strategies[:3]
                ],
                impact_score=0.85,
                metadata={"strategies": [dict(s) for s in high_priority_strategies]},
            ))

        # 4. Production readiness check
        cur.execute("""
            SELECT COUNT(*) FROM memories m
            JOIN tags t ON m.id = t.memory_id
            WHERE t.tag = 'production_ready'
            AND m.created_at > datetime('now', '-7 days')
        """)
        recent_prod_ready = cur.fetchone()[0]

        if recent_prod_ready == 0 and garden_activity["production_ready"]["count"] > 0:
            predictions.append(Prediction(
                id="garden_prod_readiness",
                prediction_type=PredictionType.MAINTENANCE,
                title="Production Readiness: No recent validation",
                description="No production-ready validations in the past week. Consider running readiness checks.",
                confidence=Confidence.LOW,
                time_horizon="short_term",
                evidence=[],
                suggested_actions=[
                    "Run full test suite",
                    "Verify MCP tool functionality",
                    "Check Rust module integration",
                ],
                impact_score=0.6,
            ))

        return predictions

    def _predict_from_constellations(self) -> list[Prediction]:
        """Predict from constellation growth patterns and density anomalies."""
        predictions: list[Prediction] = []
        cal = self._get_core_access()
        if not cal:
            return predictions

        constellations = cal.get_all_constellations()
        if not constellations:
            return predictions

        # 1. Identify the largest constellation — predict it will dominate
        largest = max(constellations, key=lambda c: c.size)
        if largest.size > 50:
            predictions.append(Prediction(
                id=f"constellation_dominant_{largest.name[:20]}",
                prediction_type=PredictionType.OPPORTUNITY,
                title=f"Dominant Constellation: {largest.name}",
                description=(
                    f"Constellation '{largest.name}' has {largest.size} members in zone "
                    f"{largest.zone}. Topics: {', '.join(largest.dominant_tags[:3])}. "
                    f"Expect continued growth in this area."
                ),
                confidence=Confidence.HIGH,
                time_horizon="short_term",
                evidence=[],
                suggested_actions=[
                    f"Consolidate knowledge in '{largest.dominant_tags[0]}' domain" if largest.dominant_tags else "Review largest cluster",
                    "Consider subdividing if cluster becomes too broad",
                ],
                impact_score=0.7,
                metadata={"constellation": largest.name, "size": largest.size},
            ))

        # 2. Find small isolated constellations — predict they need attention
        small = [c for c in constellations if c.size < 10]
        if small:
            predictions.append(Prediction(
                id="constellation_sparse_clusters",
                prediction_type=PredictionType.MAINTENANCE,
                title=f"{len(small)} Small Constellations Need Enrichment",
                description=(
                    "Small constellations may represent emerging themes that need "
                    "more memories to crystallize. Topics: "
                    + ", ".join(s.dominant_tags[0] for s in small[:3] if s.dominant_tags)
                ),
                confidence=Confidence.MEDIUM,
                time_horizon="medium_term",
                evidence=[],
                suggested_actions=[
                    f"Create more memories about '{s.dominant_tags[0]}'" if s.dominant_tags else f"Enrich constellation '{s.name}'"
                    for s in small[:3]
                ],
                impact_score=0.6,
                metadata={"small_constellations": [s.name for s in small]},
            ))

        # 3. Cross-constellation bridges — predict integration opportunities
        bridges = cal.find_constellation_bridges(limit=5)
        if bridges:
            linked_pairs = set()
            for b in bridges:
                linked_pairs.add((b["constellation_1"], b["constellation_2"]))

            predictions.append(Prediction(
                id="constellation_bridge_opportunity",
                prediction_type=PredictionType.INTEGRATION,
                title=f"Cross-Constellation Bridges Detected ({len(bridges)})",
                description=(
                    f"Found {len(bridges)} association links bridging {len(linked_pairs)} "
                    f"constellation pairs. These represent cross-domain knowledge connections."
                ),
                confidence=Confidence.HIGH,
                time_horizon="immediate",
                evidence=[b["source_id"] for b in bridges[:5]],
                suggested_actions=[
                    f"Strengthen bridge between '{p[0]}' and '{p[1]}'"
                    for p in list(linked_pairs)[:3]
                ],
                impact_score=0.8,
                metadata={"bridges": bridges[:5]},
            ))

        return predictions

    def _predict_from_association_paths(self) -> list[Prediction]:
        """Multi-hop reasoning: find chains that suggest undiscovered connections."""
        predictions: list[Prediction] = []
        cal = self._get_core_access()
        if not cal:
            return predictions

        # Find high-gravity orphans (valuable but disconnected knowledge)
        orphans = cal.find_association_orphans(min_gravity=0.6, limit=10)
        if orphans:
            predictions.append(Prediction(
                id="association_orphans",
                prediction_type=PredictionType.OPPORTUNITY,
                title=f"Disconnected High-Value Memories ({len(orphans)})",
                description=(
                    "Found memories with high importance (gravity > 0.6) but fewer than "
                    "3 associations. These are isolated knowledge that should be connected."
                ),
                confidence=Confidence.HIGH,
                time_horizon="immediate",
                evidence=[o.get("id", "") for o in orphans[:5]],
                suggested_actions=[
                    f"Connect '{o.get('title', 'Untitled')[:40]}' (gravity={o.get('gravity', 0):.2f})"
                    for o in orphans[:3]
                ],
                impact_score=0.75,
                metadata={"orphan_count": len(orphans)},
            ))

        # Find broken associations (high strength but one end at FAR_EDGE)
        broken = cal.find_broken_associations(limit=10)
        if broken:
            predictions.append(Prediction(
                id="association_broken_links",
                prediction_type=PredictionType.MAINTENANCE,
                title=f"Broken Associations ({len(broken)} links)",
                description=(
                    "High-strength associations where one memory has drifted to FAR_EDGE. "
                    "These represent decayed knowledge connections that may need repair."
                ),
                confidence=Confidence.MEDIUM,
                time_horizon="short_term",
                evidence=[b.get("source_id", "") for b in broken[:5]],
                suggested_actions=[
                    "Run association strength recalibration",
                    "Prune broken links or re-anchor drifted memories",
                ],
                impact_score=0.6,
                metadata={"broken_count": len(broken)},
            ))

        return predictions

    def _predict_from_temporal_trends(self) -> list[Prediction]:
        """Analyze temporal velocity for trend predictions."""
        predictions: list[Prediction] = []
        cal = self._get_core_access()
        if not cal:
            return predictions

        metrics = cal.get_velocity_metrics()
        if not metrics or metrics.get("total", 0) == 0:
            return predictions

        acceleration = metrics.get("acceleration", 1.0)

        # Accelerating memory creation
        if acceleration > 1.5:
            predictions.append(Prediction(
                id="temporal_acceleration",
                prediction_type=PredictionType.OPPORTUNITY,
                title="Memory Creation Accelerating",
                description=(
                    f"Creation velocity is {acceleration:.1f}x the 30-day average. "
                    f"Last 7 days: {metrics.get('last_7d', 0)} memories "
                    f"(avg {metrics.get('daily_avg_7d', 0)}/day). "
                    f"High capacity for consolidation and pattern extraction."
                ),
                confidence=Confidence.HIGH,
                time_horizon="immediate",
                evidence=[],
                suggested_actions=[
                    "Run consolidation to cluster recent memories",
                    "Mine associations from the burst of new content",
                    "Check constellation evolution after this growth period",
                ],
                impact_score=0.7,
            ))

        # Decelerating — possible stagnation
        elif acceleration < 0.5 and metrics.get("last_30d", 0) > 10:
            predictions.append(Prediction(
                id="temporal_deceleration",
                prediction_type=PredictionType.BOTTLENECK,
                title="Memory Creation Slowing Down",
                description=(
                    f"Creation velocity is {acceleration:.1f}x the 30-day average. "
                    f"This deceleration may indicate reduced activity or saturation."
                ),
                confidence=Confidence.MEDIUM,
                time_horizon="short_term",
                evidence=[],
                suggested_actions=[
                    "Surface dormant memories via Serendipity",
                    "Run Kaizen analysis to identify quality opportunities",
                    "Check if constellations are stable or drifting",
                ],
                impact_score=0.6,
            ))

        # Check temporal activity pattern
        buckets = cal.query_temporal_activity(time_window="14d", bucket="1d")
        if len(buckets) >= 7:
            recent_7 = sum(b.memories_created for b in buckets[-7:])
            earlier_7 = sum(b.memories_created for b in buckets[:7])

            if recent_7 > 0 and earlier_7 > 0:
                trend_ratio = recent_7 / earlier_7
                if trend_ratio > 2.0:
                    predictions.append(Prediction(
                        id="temporal_surge",
                        prediction_type=PredictionType.OPPORTUNITY,
                        title="Recent Activity Surge Detected",
                        description=(
                            f"Last 7 days had {recent_7} memories vs {earlier_7} the week before "
                            f"({trend_ratio:.1f}x). A burst of new knowledge is entering the system."
                        ),
                        confidence=Confidence.HIGH,
                        time_horizon="immediate",
                        evidence=[],
                        suggested_actions=[
                            "Run full constellation detection to find new clusters",
                            "Mine associations in the new content",
                        ],
                        impact_score=0.75,
                    ))

        return predictions

    def _deduplicate_predictions(self, predictions: list[Prediction]) -> list[Prediction]:
        """Remove duplicate predictions based on ID."""
        seen = set()
        unique = []
        for p in predictions:
            if p.id not in seen:
                seen.add(p.id)
                unique.append(p)
        return unique

    def _count_patterns(self) -> int:
        """Count patterns across all engines using UnifiedPatternAPI."""
        try:
            from whitemagic.core.intelligence.synthesis.unified_patterns import (
                get_pattern_api,
            )
            api = get_pattern_api()
            stats = api.get_stats()
            return int(stats["total_patterns"])
        except Exception:
            return 0

    def _get_knowledge_gaps(self) -> list[dict[str, Any]]:
        """Return identified knowledge gaps."""
        conn = self._get_conn()
        cur = conn.cursor()

        gaps = []

        # Check each gap region
        gap_queries = [
            ("Detail + Future", "y < -0.3 AND z > 0.2"),
            ("Emotional + Historical", "x > 0.3 AND z < -0.2"),
            ("Logical + Low Gravity", "x < -0.3 AND w < 0.4"),
            ("Strategic Vision", "z > 0.3 AND w > 0.8"),
        ]

        for name, condition in gap_queries:
            cur.execute(f"SELECT COUNT(*) FROM holographic_coords WHERE {condition}")
            count = cur.fetchone()[0]
            if count < 3:  # Sparse region
                gaps.append({
                    "name": name,
                    "condition": condition,
                    "current_count": count,
                    "severity": "critical" if count == 0 else "moderate",
                })

        return gaps

    def _calculate_velocity(self) -> dict[str, float]:
        """Calculate development velocity metrics."""
        conn = self._get_conn()
        cur = conn.cursor()

        cur.execute("""
            SELECT
                COUNT(*) as total,
                COUNT(CASE WHEN created_at > datetime('now', '-7 days') THEN 1 END) as last_7d,
                COUNT(CASE WHEN created_at > datetime('now', '-30 days') THEN 1 END) as last_30d
            FROM memories
        """)
        row = cur.fetchone()

        return {
            "total_memories": row["total"],
            "last_7_days": row["last_7d"],
            "last_30_days": row["last_30d"],
            "daily_avg_7d": row["last_7d"] / 7,
            "daily_avg_30d": row["last_30d"] / 30,
        }


# Global instance
_predictive_engine: PredictiveEngine | None = None

def get_predictive_engine() -> PredictiveEngine:
    global _predictive_engine
    if _predictive_engine is None:
        _predictive_engine = PredictiveEngine()
    return _predictive_engine
