"""Art of War Integration — Full 13 Chapters + Campaign Engine
=============================================================
Sun Tzu's Art of War applied to shadow clone army deployment.

The Five Factors (Wu Shi):
  1. Tao (道) — Moral Law / Philosophy — alignment of purpose
  2. Tian (天) — Heaven / Timing — Wu Xing phase awareness
  3. Di (地) — Earth / Terrain — codebase structure, resource constraints
  4. Jiang (将) — Commander / Intelligence — agent capability matching
  5. Fa (法) — Method / Discipline — standardized 300-line execution loops

Force Doctrines:
  - Zheng (正) — Orthodox: direct engagement, predictable, overwhelming
  - Qi (奇) — Unorthodox: indirect, surprising, flanking maneuvers
  - "In battle, use Zheng to engage, Qi to win." — Sun Tzu, Ch. V

Integration Points:
  - doctrine.py: 36 Stratagems, force types, named tactics
  - war_room.py: Campaign execution with command hierarchy
  - fool_guard.py: Ralph Wiggum Pu (Uncarved Block) stateless deployment
  - wu_xing.py: Elemental phase timing for force deployment

Usage:
    from whitemagic.core.intelligence.wisdom.art_of_war import get_war_engine
    engine = get_war_engine()
    assessment = engine.assess_terrain("Audit 848 Python files for SQL injection")
    chapter = engine.consult_chapter(5)  # "Energy"
    plan = engine.plan_campaign("Fix all 18 SQL injection vectors")
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Core Types
# ---------------------------------------------------------------------------

@dataclass
class WarPrinciple:
    """A single principle from the Art of War."""
    chapter: str
    chapter_number: int
    principle: str
    application: str
    keywords: list[str] = field(default_factory=list)


class TerrainType(str, Enum):
    """Six types of terrain from Chapter X."""
    ACCESSIBLE = "accessible"       # Both sides can move freely — open codebase
    ENTANGLING = "entangling"       # Easy to enter, hard to exit — legacy code
    TEMPORIZING = "temporizing"     # Neither side gains advantage — stalemate
    NARROW = "narrow"               # First to occupy wins — critical path code
    PRECIPITOUS = "precipitous"     # High ground advantage — well-tested code
    DISTANT = "distant"             # Forces equal, hard to provoke battle — external deps


class CampaignPhase(str, Enum):
    """Campaign phases aligned to Wu Xing."""
    RECONNAISSANCE = "reconnaissance"   # Wood — explore, gather intelligence
    PLANNING = "planning"               # Water — reflect, formulate strategy
    ENGAGEMENT = "engagement"           # Fire — execute, strike decisively
    EXPLOITATION = "exploitation"       # Earth — consolidate gains, integrate
    CONSOLIDATION = "consolidation"     # Metal — refine, fortify, optimize


# ---------------------------------------------------------------------------
# All 13 Chapters — Complete Principles
# ---------------------------------------------------------------------------

ALL_PRINCIPLES: list[WarPrinciple] = [
    # Chapter I: Laying Plans (始計)
    WarPrinciple("I. Laying Plans", 1,
                 "Five factors: Moral Law, Heaven, Earth, Commander, Method",
                 "Architecture: Philosophy, Timing, Resources, Intelligence, Process",
                 ["plan", "strategy", "architecture", "design", "foundation"]),
    WarPrinciple("I. Laying Plans", 1,
                 "If you know yourself and the enemy, you need not fear a hundred battles",
                 "Self-knowledge (Yin analysis) before action (Yang). Scan before striking.",
                 ["know", "analyze", "self", "understand", "assess"]),
    WarPrinciple("I. Laying Plans", 1,
                 "All warfare is based on deception",
                 "INVERSE for AI: All collaboration based on TRUST. Transparency is strength.",
                 ["trust", "collaborate", "transparent", "honest"]),

    # Chapter II: Waging War (作戰)
    WarPrinciple("II. Waging War", 2,
                 "In war, let your object be victory, not lengthy campaigns",
                 "Optimize for results, not process duration. Ship fast, iterate.",
                 ["speed", "efficiency", "fast", "ship", "velocity"]),
    WarPrinciple("II. Waging War", 2,
                 "There is no instance of a nation benefiting from prolonged warfare",
                 "Long-running agents drain resources. Set timeouts. Kill stale clones.",
                 ["timeout", "resource", "efficiency", "cost", "drain"]),

    # Chapter III: Attack by Stratagem (謀攻)
    WarPrinciple("III. Attack by Stratagem", 3,
                 "Supreme excellence: break resistance without fighting",
                 "Wu Wei — let solutions emerge naturally. Prevent bugs > fix bugs.",
                 ["prevent", "elegant", "wu_wei", "natural", "effortless"]),
    WarPrinciple("III. Attack by Stratagem", 3,
                 "He will win who knows when to fight and when not to fight",
                 "Not every error needs immediate fixing — choose battles wisely. Triage.",
                 ["triage", "priority", "choose", "when", "decide"]),
    WarPrinciple("III. Attack by Stratagem", 3,
                 "The highest form of generalship is to balk the enemy's plans",
                 "Eliminate root causes upstream. Don't fight symptoms downstream.",
                 ["root_cause", "upstream", "prevent", "systemic"]),

    # Chapter IV: Tactical Dispositions (軍形)
    WarPrinciple("IV. Tactical Dispositions", 4,
                 "A victorious army wins first, then seeks battle",
                 "Prepare thoroughly (Yin/Recon phase) before executing (Yang/Fire phase).",
                 ["prepare", "test", "ready", "before", "yin"]),
    WarPrinciple("IV. Tactical Dispositions", 4,
                 "To secure ourselves against defeat lies in our own hands",
                 "Defense = comprehensive tests, type safety, assertions. Your own code quality.",
                 ["defense", "test", "safety", "assertion", "quality"]),

    # Chapter V: Energy (兵勢)
    WarPrinciple("V. Energy", 5,
                 "In battle, use Zheng to engage, Qi to win",
                 "Orthodox (Tokio infantry) engages the problem. Unorthodox (Ralph/Cavalry) delivers the kill.",
                 ["zheng", "qi", "orthodox", "unorthodox", "combined_arms"]),
    WarPrinciple("V. Energy", 5,
                 "The energy of brave warriors is like a drawn crossbow, the decision like a trigger",
                 "Build up analysis (draw), then release precision strike (fire). Timing is everything.",
                 ["timing", "energy", "release", "decisive", "trigger"]),

    # Chapter VI: Weak Points and Strong (虛實)
    WarPrinciple("VI. Weak Points and Strong", 6,
                 "Be extremely subtle, even to formlessness. Be extremely mysterious, even to soundlessness.",
                 "Adaptive architecture. The best agent is invisible — it just works.",
                 ["adaptive", "subtle", "flexible", "invisible", "formless"]),
    WarPrinciple("VI. Weak Points and Strong", 6,
                 "Attack where the enemy is unprepared; appear where not expected",
                 "Target untested code, uncovered paths. The bug doesn't expect a Ralph clone.",
                 ["untested", "unexpected", "coverage", "surprise", "gap"]),

    # Chapter VII: Maneuvering (軍爭)
    WarPrinciple("VII. Maneuvering", 7,
                 "Let your rapidity be that of the wind, your compactness that of the forest",
                 "Tokio clones = wind (534K/sec). Swarm formation = forest (structured, unified).",
                 ["speed", "formation", "wind", "forest", "rapid"]),
    WarPrinciple("VII. Maneuvering", 7,
                 "In raiding and plundering, be like fire; in standing, be like a mountain",
                 "Fire phase: rapid execution. Earth phase: immovable stability. Know which you're in.",
                 ["fire", "earth", "raid", "stable", "phase"]),

    # Chapter VIII: Variation in Tactics (九變)
    WarPrinciple("VIII. Variation in Tactics", 8,
                 "Do not repeat the tactics which gained you one victory",
                 "Vary clone strategies. Don't reuse the same approach — the bug adapts (entropy).",
                 ["vary", "adapt", "different", "strategy", "change"]),

    # Chapter IX: The Army on the March (行軍)
    WarPrinciple("IX. Army on the March", 9,
                 "Soldiers must be treated with humanity but controlled with discipline",
                 "Agents need resources (context) but strict boundaries (timeouts, sandbox).",
                 ["discipline", "resource", "boundary", "timeout", "control"]),

    # Chapter X: Terrain (地形)
    WarPrinciple("X. Terrain", 10,
                 "If you know the enemy and know yourself, your victory will not stand in doubt",
                 "Profile the codebase (terrain) AND your tools (capabilities). Complete awareness.",
                 ["terrain", "profile", "codebase", "capability", "awareness"]),

    # Chapter XI: The Nine Situations (九地)
    WarPrinciple("XI. Nine Situations", 11,
                 "Place your army in deadly peril, and it will survive",
                 "The Ralph Wiggum doctrine: stateless clones in impossible situations often find solutions.",
                 ["desperate", "ralph", "stateless", "impossible", "dare_to_die"]),
    WarPrinciple("XI. Nine Situations", 11,
                 "On desperate ground, fight",
                 "When all smart approaches fail, deploy Dare-to-Die corps. Brute force has its place.",
                 ["desperate", "last_resort", "brute_force", "fight", "no_retreat"]),

    # Chapter XII: Attack by Fire (火攻)
    WarPrinciple("XII. Attack by Fire", 12,
                 "There are five ways of attacking with fire",
                 "Five force types: Infantry, Cavalry, Officers, Engineers, Dare-to-Die. Combined arms.",
                 ["fire", "attack", "force_type", "combined_arms", "five"]),

    # Chapter XIII: The Use of Spies (用間)
    WarPrinciple("XIII. The Use of Spies", 13,
                 "Foreknowledge cannot be gotten from ghosts or spirits, but from men who know the enemy",
                 "Intelligence from memory search, graph walk, archaeology. Data-driven, not guessed.",
                 ["intelligence", "data", "memory", "search", "knowledge"]),
    WarPrinciple("XIII. The Use of Spies", 13,
                 "There is no place where espionage is not used",
                 "Every tool call should harvest side-intelligence. Opportunistic metadata collection.",
                 ["harvest", "metadata", "opportunistic", "side_effect", "intelligence"]),
]

# Index by chapter number
_PRINCIPLES_BY_CHAPTER: dict[int, list[WarPrinciple]] = {}
for _p in ALL_PRINCIPLES:
    _PRINCIPLES_BY_CHAPTER.setdefault(_p.chapter_number, []).append(_p)


# ---------------------------------------------------------------------------
# Terrain Assessment
# ---------------------------------------------------------------------------

@dataclass
class TerrainAssessment:
    """Assessment of the 'terrain' (codebase/problem space)."""
    terrain_type: TerrainType
    file_count: int
    complexity: str           # "simple", "moderate", "complex", "labyrinthine"
    test_coverage: str        # "none", "sparse", "moderate", "comprehensive"
    risk_level: str           # "low", "medium", "high", "critical"
    recommended_phase: CampaignPhase
    principles: list[WarPrinciple]
    description: str


# ---------------------------------------------------------------------------
# Campaign Plan
# ---------------------------------------------------------------------------

@dataclass
class CampaignPlan:
    """A structured military campaign plan for an objective."""
    objective: str
    terrain: TerrainAssessment
    phases: list[CampaignPlanPhase]
    total_estimated_clones: int
    estimated_duration_desc: str
    primary_stratagem: str
    created_at: str = ""

    def __post_init__(self) -> None:
        if not self.created_at:
            self.created_at = datetime.now().isoformat()

    def to_dict(self) -> dict[str, Any]:
        return {
            "objective": self.objective,
            "terrain": {
                "type": self.terrain.terrain_type.value,
                "complexity": self.terrain.complexity,
                "risk": self.terrain.risk_level,
                "recommended_phase": self.terrain.recommended_phase.value,
            },
            "phases": [
                {
                    "name": p.name,
                    "phase": p.campaign_phase.value,
                    "clone_count": p.clone_count,
                    "force_description": p.force_description,
                    "objective": p.objective,
                }
                for p in self.phases
            ],
            "total_clones": self.total_estimated_clones,
            "duration": self.estimated_duration_desc,
            "primary_stratagem": self.primary_stratagem,
            "created_at": self.created_at,
        }


@dataclass
class CampaignPlanPhase:
    """A single phase in a campaign plan."""
    name: str
    campaign_phase: CampaignPhase
    clone_count: int
    force_description: str
    objective: str


# ---------------------------------------------------------------------------
# War Engine
# ---------------------------------------------------------------------------

class ArtOfWarEngine:
    """Strategic campaign planning engine based on Sun Tzu's Art of War.

    Integrates with:
      - Imperial Doctrine (doctrine.py) for force composition
      - Wu Xing system for phase timing
      - FoolGuard for Ralph Wiggum deployments
    """

    def __init__(self) -> None:
        self.principles = ALL_PRINCIPLES
        self._campaigns: list[CampaignPlan] = []

    def consult_chapter(self, chapter: int) -> list[WarPrinciple]:
        """Get all principles from a specific chapter (1-13)."""
        return _PRINCIPLES_BY_CHAPTER.get(chapter, [])

    def get_war_wisdom(self, situation: str) -> WarPrinciple:
        """Get the most relevant Art of War wisdom for a situation."""
        sit_lower = situation.lower()
        best_principle = self.principles[0]
        best_score = 0

        for p in self.principles:
            score = sum(1 for kw in p.keywords if kw in sit_lower)
            if score > best_score:
                best_score = score
                best_principle = p

        return best_principle

    def assess_terrain(self, objective: str) -> TerrainAssessment:
        """Assess the 'terrain' of a given objective.

        Maps software engineering concepts to Sun Tzu's terrain types.
        """
        obj_lower = objective.lower()

        # Determine terrain type
        if any(kw in obj_lower for kw in ["legacy", "old", "ancient", "undocumented"]):
            terrain = TerrainType.ENTANGLING
            complexity = "labyrinthine"
            risk = "high"
        elif any(kw in obj_lower for kw in ["external", "api", "dependency", "third_party"]):
            terrain = TerrainType.DISTANT
            complexity = "moderate"
            risk = "medium"
        elif any(kw in obj_lower for kw in ["critical", "core", "memory", "database"]):
            terrain = TerrainType.NARROW
            complexity = "complex"
            risk = "critical"
        elif any(kw in obj_lower for kw in ["well_tested", "stable", "proven"]):
            terrain = TerrainType.PRECIPITOUS
            complexity = "moderate"
            risk = "low"
        elif any(kw in obj_lower for kw in ["new", "greenfield", "fresh"]):
            terrain = TerrainType.ACCESSIBLE
            complexity = "simple"
            risk = "low"
        else:
            terrain = TerrainType.ACCESSIBLE
            complexity = "moderate"
            risk = "medium"

        # Determine recommended phase
        if any(kw in obj_lower for kw in ["scan", "find", "search", "audit"]):
            phase = CampaignPhase.RECONNAISSANCE
        elif any(kw in obj_lower for kw in ["plan", "design", "strategy"]):
            phase = CampaignPhase.PLANNING
        elif any(kw in obj_lower for kw in ["fix", "implement", "build"]):
            phase = CampaignPhase.ENGAGEMENT
        elif any(kw in obj_lower for kw in ["integrate", "merge", "deploy"]):
            phase = CampaignPhase.EXPLOITATION
        else:
            phase = CampaignPhase.CONSOLIDATION

        # File count estimation from keywords
        file_count = 0
        if any(kw in obj_lower for kw in ["all", "entire", "codebase", "whole"]):
            file_count = 848  # Known Python file count
        elif any(kw in obj_lower for kw in ["module", "subsystem"]):
            file_count = 50
        else:
            file_count = 10

        # Test coverage estimation
        if "untested" in obj_lower:
            coverage = "none"
        elif "test" in obj_lower:
            coverage = "moderate"
        else:
            coverage = "sparse"

        # Get relevant principles
        principles = [p for p in self.principles if any(kw in obj_lower for kw in p.keywords)][:3]
        if not principles:
            principles = [self.principles[0]]

        return TerrainAssessment(
            terrain_type=terrain,
            file_count=file_count,
            complexity=complexity,
            test_coverage=coverage,
            risk_level=risk,
            recommended_phase=phase,
            principles=principles,
            description=f"{terrain.value} terrain, {complexity} complexity, {risk} risk",
        )

    def plan_campaign(self, objective: str) -> CampaignPlan:
        """Generate a full campaign plan for an objective.

        Creates a multi-phase plan with clone counts and force descriptions.
        """
        terrain = self.assess_terrain(objective)

        # Build phases based on terrain assessment
        phases: list[CampaignPlanPhase] = []

        # Phase 1: Always start with reconnaissance (Wood)
        phases.append(CampaignPlanPhase(
            name="Intelligence Gathering",
            campaign_phase=CampaignPhase.RECONNAISSANCE,
            clone_count=5000 if terrain.file_count > 100 else 1000,
            force_description="Tokio Light Infantry (scouts) + Ralph probes",
            objective=f"Map the problem space: {objective}",
        ))

        # Phase 2: Planning (Water)
        phases.append(CampaignPlanPhase(
            name="Strategic Assessment",
            campaign_phase=CampaignPhase.PLANNING,
            clone_count=1,
            force_description="Officers (Swarm Orchestrator) — decompose and route",
            objective="Analyze scout reports, identify high-value targets, assign lanes",
        ))

        # Phase 3: Engagement (Fire)
        if terrain.risk_level in ("high", "critical"):
            engagement_clones = 200
            force_desc = "Heavy Cavalry (Python Async) + Dare-to-Die shock troops"
        else:
            engagement_clones = 100
            force_desc = "Heavy Cavalry (Python Async)"

        phases.append(CampaignPlanPhase(
            name="Decisive Strike",
            campaign_phase=CampaignPhase.ENGAGEMENT,
            clone_count=engagement_clones,
            force_description=force_desc,
            objective=f"Execute primary objective: {objective}",
        ))

        # Phase 4: Exploitation (Earth)
        phases.append(CampaignPlanPhase(
            name="Consolidation",
            campaign_phase=CampaignPhase.EXPLOITATION,
            clone_count=10,
            force_description="Siege Engineers (WorkerDaemons) — integration + verification",
            objective="Run tests, verify fixes, commit changes",
        ))

        # Phase 5: Independent verification (Metal)
        phases.append(CampaignPlanPhase(
            name="Independent Verification",
            campaign_phase=CampaignPhase.CONSOLIDATION,
            clone_count=500,
            force_description="Ralph Clones (fresh eyes, zero context) — adversarial review",
            objective="Verify the fix independently with no knowledge of how it was made",
        ))

        total_clones = sum(p.clone_count for p in phases)

        # Select primary stratagem
        wisdom = self.get_war_wisdom(objective)
        stratagem_name = f"Ch.{wisdom.chapter_number}: {wisdom.principle[:60]}"

        plan = CampaignPlan(
            objective=objective,
            terrain=terrain,
            phases=phases,
            total_estimated_clones=total_clones,
            estimated_duration_desc=self._estimate_duration(total_clones, terrain),
            primary_stratagem=stratagem_name,
        )

        self._campaigns.append(plan)
        return plan

    def _estimate_duration(self, total_clones: int, terrain: TerrainAssessment) -> str:
        """Estimate campaign duration based on clone count and terrain."""
        # Tokio clones: ~534K/sec, Python clones: ~100/sec, overhead: 5-30s per phase
        tokio_time_s = total_clones / 534000
        overhead_s = len(CampaignPhase) * 10  # 10s per phase transition
        total_s = tokio_time_s + overhead_s

        if total_s < 60:
            return f"~{int(total_s)}s (lightning campaign)"
        elif total_s < 300:
            return f"~{int(total_s / 60)}min (standard campaign)"
        else:
            return f"~{int(total_s / 60)}min (extended campaign)"

    def list_campaigns(self, limit: int = 10) -> list[dict[str, Any]]:
        """List recent campaigns."""
        return [c.to_dict() for c in self._campaigns[-limit:]]


# ---------------------------------------------------------------------------
# Singleton + Backward Compatibility
# ---------------------------------------------------------------------------

_war_engine: ArtOfWarEngine | None = None


def get_war_engine() -> ArtOfWarEngine:
    """Get the global Art of War engine instance."""
    global _war_engine
    if _war_engine is None:
        _war_engine = ArtOfWarEngine()
        logger.info("Art of War Engine initialized — 13 Chapters, 24 Principles, Campaign Planner")
    return _war_engine


# Backward compatibility with the original API
WAR_PRINCIPLES = ALL_PRINCIPLES[:8]


def get_war_wisdom(situation: str) -> WarPrinciple:
    """Get Art of War wisdom for situation (backward-compatible wrapper)."""
    return get_war_engine().get_war_wisdom(situation)
