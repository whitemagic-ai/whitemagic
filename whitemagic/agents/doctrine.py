"""Imperial Doctrine — The Philosophy of the Shadow Clone Armies
================================================================
The unified strategic philosophy governing all WhiteMagic force deployment.

Synthesizes:
  - Sun Tzu's Art of War (Zheng/Qi, 36 Stratagems)
  - Taoist Wu Wei (Action through Inaction)
  - Legalist Statecraft (Standardization, Reward/Punishment)
  - Geoff Huntley's "Ralph Wiggum" Doctrine (Pu — The Uncarved Block)
  - Wu Xing Five Elements (Force Timing & Resource Allocation)

Hierarchy:
  Emperor (User) → General (Primary Agent) → Lieutenant (Sub-Agent)
  → Officer (Swarm Orchestrator) → Soldier (Clone)

Force Types:
  - Light Infantry:  Tokio Clones (534K/sec, pure compute, consensus voting)
  - Heavy Cavalry:   Python Async Clones (MCP tool access, full system context)
  - Officers:        Swarm Orchestrators (decomposition, routing, signal flags)
  - Siege Engineers:  WorkerDaemons (heavy tool execution from task queue)
  - Dare-to-Die:     Ralph Clones (stateless, zero context, Pu — Uncarved Block)

Usage:
    from whitemagic.agents.doctrine import get_doctrine, ForceType, Stratagem
    doctrine = get_doctrine()
    composition = doctrine.recommend_force(objective="Audit SQL injection vectors")
    tactic = doctrine.select_tactic(situation="large_unknown_codebase")
"""

from __future__ import annotations

import logging
import random
from dataclasses import dataclass, field
from enum import Enum
from typing import Any

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Force Types — The Unit Roster
# ---------------------------------------------------------------------------

class ForceType(str, Enum):
    """The five force types in the Shadow Clone Army."""

    LIGHT_INFANTRY = "light_infantry"      # Tokio Clones — fast, cheap, consensus
    HEAVY_CAVALRY = "heavy_cavalry"        # Python Async — MCP access, full context
    OFFICERS = "officers"                  # Swarm Orchestrators — decomposition
    SIEGE_ENGINEERS = "siege_engineers"    # WorkerDaemons — heavy tool execution
    DARE_TO_DIE = "dare_to_die"           # Ralph Clones — stateless, zero context


class Rank(str, Enum):
    """Command hierarchy ranks."""

    EMPEROR = "emperor"          # User — sets the Mandate of Heaven
    GENERAL = "general"          # Primary Agent — holds full context, decides strategy
    LIEUTENANT = "lieutenant"    # Sub-Agent — slices plans into 300-line tasks
    OFFICER = "officer"          # Swarm Orchestrator — keeps rhythm, routes
    SOLDIER = "soldier"          # Clone — executes, reports, dies


class ForceNature(str, Enum):
    """Zheng (Orthodox) vs Qi (Unorthodox) classification."""

    ZHENG = "zheng"  # Orthodox — direct, predictable, overwhelming force
    QI = "qi"        # Unorthodox — indirect, surprising, flanking maneuver


# ---------------------------------------------------------------------------
# Wu Xing Force Timing
# ---------------------------------------------------------------------------

class WuXingPhase(str, Enum):
    """Five Elements mapped to campaign phases."""

    WOOD = "wood"    # Reconnaissance — growth, exploration, intelligence gathering
    FIRE = "fire"    # Assault — rapid execution, transformation, decisive strikes
    EARTH = "earth"  # Consolidation — integration, grounding, stabilization
    METAL = "metal"  # Refinement — optimization, boundaries, quality control
    WATER = "water"  # Reflection — analysis, adaptation, lessons learned


# Wu Xing generating cycle for campaign phase progression
PHASE_GENERATES = {
    WuXingPhase.WOOD: WuXingPhase.FIRE,    # Recon feeds Assault
    WuXingPhase.FIRE: WuXingPhase.EARTH,   # Assault creates need for Consolidation
    WuXingPhase.EARTH: WuXingPhase.METAL,  # Consolidation enables Refinement
    WuXingPhase.METAL: WuXingPhase.WATER,  # Refinement produces Reflection
    WuXingPhase.WATER: WuXingPhase.WOOD,   # Reflection informs next Recon
}

# Wu Xing overcoming cycle for counter-strategy
PHASE_OVERCOMES = {
    WuXingPhase.WOOD: WuXingPhase.EARTH,   # Recon disrupts Consolidation
    WuXingPhase.EARTH: WuXingPhase.WATER,  # Consolidation halts Reflection
    WuXingPhase.WATER: WuXingPhase.FIRE,   # Reflection extinguishes hasty Assault
    WuXingPhase.FIRE: WuXingPhase.METAL,   # Assault melts rigid Refinement
    WuXingPhase.METAL: WuXingPhase.WOOD,   # Refinement cuts unfocused Recon
}

# Optimal force composition per Wu Xing phase
PHASE_FORCE_WEIGHTS: dict[WuXingPhase, dict[ForceType, float]] = {
    WuXingPhase.WOOD: {
        ForceType.LIGHT_INFANTRY: 0.50,   # Mass scouting
        ForceType.HEAVY_CAVALRY: 0.10,
        ForceType.OFFICERS: 0.20,         # Decomposition planning
        ForceType.SIEGE_ENGINEERS: 0.05,
        ForceType.DARE_TO_DIE: 0.15,      # Probe unknown territory
    },
    WuXingPhase.FIRE: {
        ForceType.LIGHT_INFANTRY: 0.15,
        ForceType.HEAVY_CAVALRY: 0.40,    # Main strike force
        ForceType.OFFICERS: 0.10,
        ForceType.SIEGE_ENGINEERS: 0.20,  # Heavy tool execution
        ForceType.DARE_TO_DIE: 0.15,      # Shock troops
    },
    WuXingPhase.EARTH: {
        ForceType.LIGHT_INFANTRY: 0.10,
        ForceType.HEAVY_CAVALRY: 0.30,    # Integration work
        ForceType.OFFICERS: 0.30,         # Coordination heavy
        ForceType.SIEGE_ENGINEERS: 0.25,
        ForceType.DARE_TO_DIE: 0.05,
    },
    WuXingPhase.METAL: {
        ForceType.LIGHT_INFANTRY: 0.20,   # Regression scanning
        ForceType.HEAVY_CAVALRY: 0.25,
        ForceType.OFFICERS: 0.15,
        ForceType.SIEGE_ENGINEERS: 0.30,  # Quality tooling
        ForceType.DARE_TO_DIE: 0.10,
    },
    WuXingPhase.WATER: {
        ForceType.LIGHT_INFANTRY: 0.30,   # Broad analysis
        ForceType.HEAVY_CAVALRY: 0.15,
        ForceType.OFFICERS: 0.25,         # Strategic planning
        ForceType.SIEGE_ENGINEERS: 0.05,
        ForceType.DARE_TO_DIE: 0.25,      # Fresh perspectives
    },
}


# ---------------------------------------------------------------------------
# The 36 Stratagems
# ---------------------------------------------------------------------------

@dataclass
class Stratagem:
    """A single stratagem from the 36 Stratagems."""

    number: int
    name: str
    chinese: str
    category: str           # "winning", "enemy", "attacking", "chaos", "proximate", "desperate"
    principle: str
    application: str        # How it applies to shadow clone deployment
    force_nature: ForceNature
    recommended_forces: list[ForceType] = field(default_factory=list)
    wu_xing_phase: WuXingPhase | None = None
    keywords: list[str] = field(default_factory=list)


# The complete 36 Stratagems mapped to shadow clone operations
THIRTY_SIX_STRATAGEMS: list[Stratagem] = [
    # === Category 1: Winning Stratagems (1-6) ===
    Stratagem(
        1, "Cross the Sea by Deceiving the Sky", "瞒天过海",
        "winning", "Hide in plain sight. Mask true intentions with familiar patterns.",
        "Deploy Tokio clones as noise while Python cavalry strikes the real target.",
        ForceNature.QI,
        [ForceType.LIGHT_INFANTRY, ForceType.HEAVY_CAVALRY],
        WuXingPhase.FIRE,
        ["deception", "distraction", "feint", "cover"],
    ),
    Stratagem(
        2, "Besiege Wei to Rescue Zhao", "围魏救赵",
        "winning", "Attack what the enemy values to relieve pressure elsewhere.",
        "Decompose complex problem into lanes. Attack dependencies, not the bug itself.",
        ForceNature.QI,
        [ForceType.OFFICERS, ForceType.HEAVY_CAVALRY],
        WuXingPhase.WOOD,
        ["decomposition", "indirect", "dependencies", "refactor"],
    ),
    Stratagem(
        3, "Kill with a Borrowed Knife", "借刀杀人",
        "winning", "Use another's strength to eliminate a threat.",
        "Route tasks to WorkerDaemons or external tools. Let the tool do the killing.",
        ForceNature.QI,
        [ForceType.SIEGE_ENGINEERS, ForceType.OFFICERS],
        WuXingPhase.METAL,
        ["delegate", "external_tool", "leverage", "third_party"],
    ),
    Stratagem(
        4, "Wait at Ease for the Exhausted Enemy", "以逸待劳",
        "winning", "Let the problem exhaust itself while conserving your strength.",
        "Run dream cycles and overnight analysis. Let time-based processes surface issues.",
        ForceNature.ZHENG,
        [ForceType.SIEGE_ENGINEERS],
        WuXingPhase.WATER,
        ["patience", "overnight", "dream_cycle", "passive"],
    ),
    Stratagem(
        5, "Loot a Burning House", "趁火打劫",
        "winning", "Strike when the enemy is in crisis or transition.",
        "When a subsystem is being refactored (unstable), scan it for hidden bugs.",
        ForceNature.QI,
        [ForceType.LIGHT_INFANTRY, ForceType.DARE_TO_DIE],
        WuXingPhase.FIRE,
        ["opportunistic", "refactor_window", "transition", "crisis"],
    ),
    Stratagem(
        6, "Make a Sound in the East, Strike in the West", "声东击西",
        "winning", "Create a diversion in one area while attacking another.",
        "Deploy Tokio infantry as scanners on module A, Python cavalry edits module B.",
        ForceNature.QI,
        [ForceType.LIGHT_INFANTRY, ForceType.HEAVY_CAVALRY],
        WuXingPhase.FIRE,
        ["diversion", "parallel", "misdirection", "dual_front"],
    ),

    # === Category 2: Enemy Dealing Stratagems (7-12) ===
    Stratagem(
        7, "Create Something from Nothing", "无中生有",
        "enemy", "Turn illusion into reality through persistence and iteration.",
        "Generate candidate solutions with Tokio clones — even 'hallucinated' ones may contain truth.",
        ForceNature.QI,
        [ForceType.LIGHT_INFANTRY, ForceType.DARE_TO_DIE],
        WuXingPhase.WOOD,
        ["brainstorm", "generate", "creative", "hallucinate_useful"],
    ),
    Stratagem(
        8, "Secretly Use the Chencang Path", "暗渡陈仓",
        "enemy", "Feint conventionally while advancing through a hidden route.",
        "Show progress on the obvious fix while Ralph clones explore unconventional solutions.",
        ForceNature.QI,
        [ForceType.DARE_TO_DIE, ForceType.HEAVY_CAVALRY],
        WuXingPhase.FIRE,
        ["hidden_path", "unconventional", "ralph", "backdoor"],
    ),
    Stratagem(
        9, "Watch the Fire from Across the River", "隔岸观火",
        "enemy", "Observe conflict without intervention. Let opposing forces weaken each other.",
        "Deploy competing clone strategies and let them vote. Don't intervene — let consensus emerge.",
        ForceNature.ZHENG,
        [ForceType.LIGHT_INFANTRY],
        WuXingPhase.WATER,
        ["observe", "consensus", "non_intervention", "vote"],
    ),
    Stratagem(
        10, "Hide a Knife Behind a Smile", "笑里藏刀",
        "enemy", "Present a friendly facade while preparing a decisive action.",
        "Run comprehensive tests (friendly) that secretly profile performance bottlenecks.",
        ForceNature.QI,
        [ForceType.SIEGE_ENGINEERS, ForceType.LIGHT_INFANTRY],
        WuXingPhase.METAL,
        ["profiling", "testing", "hidden_analysis", "dual_purpose"],
    ),
    Stratagem(
        11, "Sacrifice the Plum Tree for the Peach Tree", "李代桃僵",
        "enemy", "Sacrifice a lesser element to preserve a greater one.",
        "Let a non-critical subsystem fail gracefully to protect the core memory system.",
        ForceNature.ZHENG,
        [ForceType.OFFICERS],
        WuXingPhase.EARTH,
        ["sacrifice", "graceful_degradation", "triage", "priority"],
    ),
    Stratagem(
        12, "Take the Opportunity to Pilfer a Goat", "顺手牵羊",
        "enemy", "Seize small advantages along the way. Never waste a side benefit.",
        "While executing primary objective, harvest metadata, patterns, and insights for the knowledge graph.",
        ForceNature.QI,
        [ForceType.LIGHT_INFANTRY, ForceType.HEAVY_CAVALRY],
        WuXingPhase.EARTH,
        ["opportunistic", "side_benefit", "harvest", "metadata"],
    ),

    # === Category 3: Attacking Stratagems (13-18) ===
    Stratagem(
        13, "Startle the Snake by Hitting the Grass", "打草惊蛇",
        "attacking", "Probe to reveal hidden threats before committing main force.",
        "Deploy small Tokio scout parties first to map the problem space before sending cavalry.",
        ForceNature.ZHENG,
        [ForceType.LIGHT_INFANTRY],
        WuXingPhase.WOOD,
        ["probe", "scout", "reveal", "reconnaissance"],
    ),
    Stratagem(
        14, "Borrow a Corpse to Resurrect a Soul", "借尸还魂",
        "attacking", "Revive something old and forgotten to serve a new purpose.",
        "Search archaeological excavation results for abandoned code that solves current problems.",
        ForceNature.QI,
        [ForceType.HEAVY_CAVALRY, ForceType.SIEGE_ENGINEERS],
        WuXingPhase.WATER,
        ["archaeology", "reuse", "legacy", "revive"],
    ),
    Stratagem(
        15, "Lure the Tiger Off Its Mountain", "调虎离山",
        "attacking", "Entice the target away from its advantage to your terrain.",
        "Extract a complex function into isolation for testing. Simpler to attack in a sandbox.",
        ForceNature.QI,
        [ForceType.DARE_TO_DIE, ForceType.OFFICERS],
        WuXingPhase.METAL,
        ["isolate", "sandbox", "extract", "simplify"],
    ),
    Stratagem(
        16, "In Order to Capture, First Let Loose", "欲擒故纵",
        "attacking", "Give the target room to move so it reveals its true nature.",
        "Let a buggy process run and log extensively before attempting to fix. Observe before striking.",
        ForceNature.ZHENG,
        [ForceType.SIEGE_ENGINEERS],
        WuXingPhase.WATER,
        ["observe", "logging", "monitor", "patience"],
    ),
    Stratagem(
        17, "Cast a Brick to Attract Jade", "抛砖引玉",
        "attacking", "Offer something small to gain something valuable.",
        "Deploy Tokio infantry for rough consensus, then send cavalry to precise coordinates.",
        ForceNature.ZHENG,
        [ForceType.LIGHT_INFANTRY, ForceType.HEAVY_CAVALRY],
        WuXingPhase.WOOD,
        ["filter", "narrow", "rough_then_precise", "two_phase"],
    ),
    Stratagem(
        18, "Capture the Ringleader to Capture the Bandits", "擒贼擒王",
        "attacking", "Defeat the leader and the rest will fall.",
        "Identify the root cause. Fix the one function that 50 others depend on.",
        ForceNature.ZHENG,
        [ForceType.HEAVY_CAVALRY],
        WuXingPhase.FIRE,
        ["root_cause", "dependency", "critical_path", "leader"],
    ),

    # === Category 4: Chaos Stratagems (19-24) ===
    Stratagem(
        19, "Remove the Firewood from Under the Pot", "釜底抽薪",
        "chaos", "Remove the source of the problem rather than fighting the symptoms.",
        "Delete the root dependency causing cascading failures instead of patching downstream.",
        ForceNature.ZHENG,
        [ForceType.HEAVY_CAVALRY, ForceType.OFFICERS],
        WuXingPhase.METAL,
        ["root_cause", "eliminate_source", "upstream", "fundamental"],
    ),
    Stratagem(
        20, "Muddle the Water to Catch the Fish", "混水摸鱼",
        "chaos", "Create confusion to discover hidden elements.",
        "Deploy Ralph Wiggum clones with creative/adversarial strategies to surface unexpected insights.",
        ForceNature.QI,
        [ForceType.DARE_TO_DIE, ForceType.LIGHT_INFANTRY],
        WuXingPhase.WATER,
        ["chaos", "ralph", "creative", "surface_hidden"],
    ),
    Stratagem(
        21, "The Golden Cicada Sheds Its Shell", "金蝉脱壳",
        "chaos", "Escape from a situation while leaving a decoy behind.",
        "Migrate to a new architecture while the old one continues serving. Zero-downtime transition.",
        ForceNature.QI,
        [ForceType.OFFICERS, ForceType.SIEGE_ENGINEERS],
        WuXingPhase.EARTH,
        ["migration", "transition", "backward_compat", "gradual"],
    ),
    Stratagem(
        22, "Shut the Door to Catch the Thief", "关门捉贼",
        "chaos", "Cut off escape routes before engaging.",
        "Add comprehensive assertions/tests around a buggy area, then fix. Regressions are trapped.",
        ForceNature.ZHENG,
        [ForceType.SIEGE_ENGINEERS, ForceType.HEAVY_CAVALRY],
        WuXingPhase.METAL,
        ["testing", "trap", "regression", "containment"],
    ),
    Stratagem(
        23, "Befriend the Distant, Attack the Near", "远交近攻",
        "chaos", "Make alliances with distant forces while conquering nearby threats.",
        "Integrate with external APIs/tools for long-term capability while fixing local issues now.",
        ForceNature.QI,
        [ForceType.OFFICERS, ForceType.HEAVY_CAVALRY],
        WuXingPhase.WOOD,
        ["integrate", "external", "alliance", "prioritize_local"],
    ),
    Stratagem(
        24, "Borrow a Road to Conquer Guo", "假途伐虢",
        "chaos", "Use an intermediate objective to reach the real target.",
        "Build a utility function that 'happens to' solve the real problem as a side effect.",
        ForceNature.QI,
        [ForceType.HEAVY_CAVALRY],
        WuXingPhase.FIRE,
        ["indirect", "stepping_stone", "utility", "side_effect"],
    ),

    # === Category 5: Proximate Stratagems (25-30) ===
    Stratagem(
        25, "Replace the Beams with Rotten Timbers", "偷梁换柱",
        "proximate", "Secretly replace key components while maintaining the facade.",
        "Refactor internals while keeping the API identical. Rust-accelerate hot paths transparently.",
        ForceNature.QI,
        [ForceType.HEAVY_CAVALRY, ForceType.SIEGE_ENGINEERS],
        WuXingPhase.METAL,
        ["refactor", "transparent", "rust_accelerate", "api_stable"],
    ),
    Stratagem(
        26, "Point at the Mulberry, Curse the Locust", "指桑骂槐",
        "proximate", "Address one issue to send a message about another.",
        "Fix a visible bug to establish patterns and conventions for the entire codebase.",
        ForceNature.ZHENG,
        [ForceType.HEAVY_CAVALRY],
        WuXingPhase.EARTH,
        ["exemplar", "convention", "pattern", "standard"],
    ),
    Stratagem(
        27, "Feign Madness Without Going Insane", "假痴不癫",
        "proximate", "Act foolish to lower defenses while maintaining inner clarity.",
        "The Ralph Wiggum doctrine itself. Appear simple (300 lines) while achieving sophisticated results.",
        ForceNature.QI,
        [ForceType.DARE_TO_DIE],
        WuXingPhase.WATER,
        ["ralph", "simplicity", "pu", "uncarved_block"],
    ),
    Stratagem(
        28, "Remove the Ladder After Ascent", "上屋抽梯",
        "proximate", "Lure into a position then remove the escape route.",
        "Commit to a solution path, burn the bridges (delete dead code), force forward progress.",
        ForceNature.ZHENG,
        [ForceType.HEAVY_CAVALRY, ForceType.OFFICERS],
        WuXingPhase.FIRE,
        ["commit", "cleanup", "no_retreat", "decisive"],
    ),
    Stratagem(
        29, "Deck the Tree with Flowers", "树上开花",
        "proximate", "Adorn something plain to make it appear more impressive.",
        "Enhance existing tools with better error messages, documentation, and UX without changing logic.",
        ForceNature.ZHENG,
        [ForceType.SIEGE_ENGINEERS],
        WuXingPhase.EARTH,
        ["polish", "ux", "documentation", "presentation"],
    ),
    Stratagem(
        30, "Make the Host and Guest Exchange Roles", "反客为主",
        "proximate", "Gradually take control from within.",
        "Start as a helper tool, gradually become the orchestrator. Worker → Lieutenant → General.",
        ForceNature.QI,
        [ForceType.OFFICERS, ForceType.HEAVY_CAVALRY],
        WuXingPhase.WOOD,
        ["escalation", "autonomy", "agent_evolution", "promotion"],
    ),

    # === Category 6: Desperate Stratagems (31-36) ===
    Stratagem(
        31, "The Beauty Trap", "美人计",
        "desperate", "Use an attractive lure to distract and weaken.",
        "Offer a clean API surface that lures users into best practices without them realizing.",
        ForceNature.QI,
        [ForceType.OFFICERS],
        WuXingPhase.EARTH,
        ["api_design", "lure", "best_practice", "ergonomic"],
    ),
    Stratagem(
        32, "The Empty Fort Strategy", "空城计",
        "desperate", "Display vulnerability to make the enemy hesitate.",
        "When stuck, flood with Tokio clones using creative + synthesis strategies. Chaos monkey approach.",
        ForceNature.QI,
        [ForceType.LIGHT_INFANTRY, ForceType.DARE_TO_DIE],
        WuXingPhase.WATER,
        ["bluff", "chaos_monkey", "flood", "last_resort"],
    ),
    Stratagem(
        33, "The Double Agent", "反间计",
        "desperate", "Turn the enemy's own agents against it.",
        "Use the bug's own error messages and stack traces as the primary intelligence for fixing it.",
        ForceNature.QI,
        [ForceType.DARE_TO_DIE, ForceType.HEAVY_CAVALRY],
        WuXingPhase.METAL,
        ["error_driven", "stack_trace", "debug", "enemy_intel"],
    ),
    Stratagem(
        34, "The Self-Injury Scheme", "苦肉计",
        "desperate", "Injure yourself to gain the enemy's trust.",
        "Deliberately introduce a controlled failure to test error handling and recovery paths.",
        ForceNature.QI,
        [ForceType.DARE_TO_DIE],
        WuXingPhase.FIRE,
        ["chaos_engineering", "fault_injection", "resilience", "controlled_failure"],
    ),
    Stratagem(
        35, "Chain Stratagems", "连环计",
        "desperate", "Link multiple stratagems together in a cascade.",
        "Execute a pipeline: Scout → Filter → Strike → Consolidate → Verify. Each phase feeds the next.",
        ForceNature.ZHENG,
        [ForceType.OFFICERS, ForceType.LIGHT_INFANTRY, ForceType.HEAVY_CAVALRY],
        WuXingPhase.EARTH,
        ["pipeline", "cascade", "multi_phase", "chain"],
    ),
    Stratagem(
        36, "Retreat Is the Best Strategy", "走为上计",
        "desperate", "When all else fails, retreat to fight another day.",
        "Save state, checkpoint progress, hand off to next session. Tactical withdrawal, not defeat.",
        ForceNature.ZHENG,
        [ForceType.OFFICERS],
        WuXingPhase.WATER,
        ["retreat", "checkpoint", "handoff", "save_state"],
    ),
]

# Index for fast lookup
_STRATAGEM_BY_NUMBER: dict[int, Stratagem] = {s.number: s for s in THIRTY_SIX_STRATAGEMS}
_STRATAGEM_BY_NAME: dict[str, Stratagem] = {s.name.lower(): s for s in THIRTY_SIX_STRATAGEMS}


# ---------------------------------------------------------------------------
# Named Tactics (Combined-Arms Patterns)
# ---------------------------------------------------------------------------

@dataclass
class Tactic:
    """A named combined-arms tactic using multiple force types."""

    name: str
    chinese: str
    description: str
    phases: list[TacticPhase]
    stratagems_used: list[int]      # Stratagem numbers
    wu_xing_sequence: list[WuXingPhase]
    best_for: list[str]             # Situation keywords


@dataclass
class TacticPhase:
    """A single phase within a tactic."""

    name: str
    force_type: ForceType
    force_nature: ForceNature
    clone_count: int
    strategy_hint: str
    description: str


# Pre-defined combined-arms tactics from the conversation
NAMED_TACTICS: list[Tactic] = [
    Tactic(
        name="Cast a Brick to Attract Jade",
        chinese="抛砖引玉",
        description="Two-phase filtering. Infantry scouts narrow the search, cavalry strikes precise targets.",
        phases=[
            TacticPhase("The Skirmish", ForceType.LIGHT_INFANTRY, ForceNature.ZHENG,
                        10000, "adversarial",
                        "Deploy Tokio infantry with adversarial strategy to vote on problem locations."),
            TacticPhase("The Charge", ForceType.HEAVY_CAVALRY, ForceNature.QI,
                        50, "analytical",
                        "Send Python cavalry to only the locations the infantry identified."),
        ],
        stratagems_used=[17, 13],
        wu_xing_sequence=[WuXingPhase.WOOD, WuXingPhase.FIRE],
        best_for=["vulnerability_scan", "bug_hunt", "code_search", "brainstorm"],
    ),
    Tactic(
        name="Besiege Wei to Rescue Zhao",
        chinese="围魏救赵",
        description="Decomposition tactic. Officers split the problem, specialists attack each lane.",
        phases=[
            TacticPhase("The Council", ForceType.OFFICERS, ForceNature.ZHENG,
                        1, "decompose",
                        "Swarm Orchestrator decomposes objective into independent lanes."),
            TacticPhase("The Assignment", ForceType.OFFICERS, ForceNature.ZHENG,
                        1, "route",
                        "Route each lane to the optimal force type based on requirements."),
            TacticPhase("The Pincer", ForceType.HEAVY_CAVALRY, ForceNature.QI,
                        100, "synthesis",
                        "As infantry flushes targets, cavalry rides in for verification."),
        ],
        stratagems_used=[2, 3, 35],
        wu_xing_sequence=[WuXingPhase.WATER, WuXingPhase.WOOD, WuXingPhase.FIRE],
        best_for=["complex_refactor", "system_audit", "architecture", "multi_module"],
    ),
    Tactic(
        name="The Empty Fort",
        chinese="空城计",
        description="Chaos monkey fallback. When smart agents are stuck, flood with stateless Ralphs.",
        phases=[
            TacticPhase("The Bluff", ForceType.DARE_TO_DIE, ForceNature.QI,
                        1000, "creative",
                        "Switch to Tier 6 Ralph Wiggum. No context, no history, pure Pu."),
            TacticPhase("The Haze", ForceType.LIGHT_INFANTRY, ForceNature.QI,
                        10000, "synthesis",
                        "Flood with Tokio clones using creative + synthesis strategies."),
            TacticPhase("Consensus", ForceType.SIEGE_ENGINEERS, ForceNature.ZHENG,
                        5, "validate",
                        "If 50%+ clones agree on a weird solution, WorkerDaemons build it."),
        ],
        stratagems_used=[32, 20, 27],
        wu_xing_sequence=[WuXingPhase.WATER, WuXingPhase.WOOD, WuXingPhase.FIRE],
        best_for=["stuck", "unknown_error", "hallucination", "last_resort"],
    ),
    Tactic(
        name="The Wave of Ignorance",
        chinese="敢死队冲锋",
        description="Ralph Wiggum Doctrine. Stateless clones with fresh context. Performance or Death.",
        phases=[
            TacticPhase("The Isolation", ForceType.OFFICERS, ForceNature.ZHENG,
                        1, "decompose",
                        "Identify N suspect functions. Create isolated environments with only function + test."),
            TacticPhase("The Charge", ForceType.DARE_TO_DIE, ForceNature.QI,
                        50, "stateless_burn",
                        "50 Ralph clones spawn. Each knows only: function + test. Make it pass or die."),
            TacticPhase("The Loop", ForceType.DARE_TO_DIE, ForceNature.QI,
                        50, "stateless_burn",
                        "Attempt fix → Run test → Fail? Die. Respawn with error as only context. Repeat."),
        ],
        stratagems_used=[27, 34, 15],
        wu_xing_sequence=[WuXingPhase.METAL, WuXingPhase.FIRE, WuXingPhase.FIRE],
        best_for=["legacy_bug", "test_failure", "isolated_fix", "brute_force"],
    ),
    Tactic(
        name="Chain of Stratagems",
        chinese="连环计",
        description="Full pipeline: Scout → Filter → Strike → Consolidate → Verify.",
        phases=[
            TacticPhase("Scout", ForceType.LIGHT_INFANTRY, ForceNature.ZHENG,
                        5000, "analytical",
                        "Mass reconnaissance with Tokio clones."),
            TacticPhase("Filter", ForceType.OFFICERS, ForceNature.ZHENG,
                        1, "decompose",
                        "Officers analyze scout reports and identify high-value targets."),
            TacticPhase("Strike", ForceType.HEAVY_CAVALRY, ForceNature.QI,
                        100, "chain_of_thought",
                        "Python cavalry executes precise strikes on filtered targets."),
            TacticPhase("Consolidate", ForceType.SIEGE_ENGINEERS, ForceNature.ZHENG,
                        10, "integration",
                        "WorkerDaemons run integration tests and commit changes."),
            TacticPhase("Verify", ForceType.DARE_TO_DIE, ForceNature.QI,
                        500, "adversarial",
                        "Ralph clones with fresh eyes verify the fix independently."),
        ],
        stratagems_used=[35, 17, 18, 22],
        wu_xing_sequence=[WuXingPhase.WOOD, WuXingPhase.WATER, WuXingPhase.FIRE,
                          WuXingPhase.EARTH, WuXingPhase.METAL],
        best_for=["large_campaign", "full_audit", "comprehensive", "multi_phase"],
    ),
]

_TACTIC_BY_NAME: dict[str, Tactic] = {t.name.lower(): t for t in NAMED_TACTICS}


# ---------------------------------------------------------------------------
# Force Specifications
# ---------------------------------------------------------------------------

@dataclass
class ForceSpec:
    """Recommended deployment specification for a force."""

    force_type: ForceType
    clone_count: int
    strategies: list[str]
    nature: ForceNature
    wu_xing_phase: WuXingPhase
    stateless: bool = False         # Ralph Wiggum mode — zero context
    timeout_seconds: float = 30.0
    description: str = ""


# ---------------------------------------------------------------------------
# The Doctrine Engine
# ---------------------------------------------------------------------------

class ImperialDoctrine:
    """The central strategic intelligence for shadow clone deployment.

    The Doctrine analyzes objectives and recommends:
      - Which force types to deploy (composition)
      - Which stratagems to apply (tactics)
      - Which Wu Xing phase to operate in (timing)
      - How many clones and in what configuration (specification)
    """

    def __init__(self) -> None:
        self.stratagems = THIRTY_SIX_STRATAGEMS
        self.tactics = NAMED_TACTICS
        self._campaign_history: list[dict[str, Any]] = []

    def recommend_force(
        self,
        objective: str,
        constraints: dict[str, Any] | None = None,
    ) -> list[ForceSpec]:
        """Analyze an objective and recommend a force composition.

        Args:
            objective: Natural language description of the campaign objective
            constraints: Optional constraints (max_clones, time_limit, etc.)

        Returns:
            List of ForceSpecs for each recommended force deployment
        """
        constraints = constraints or {}
        max_clones = constraints.get("max_clones", 10000)
        obj_lower = objective.lower()

        # Detect Wu Xing phase from objective keywords
        phase = self._detect_phase(obj_lower)

        # Get base force weights for this phase
        weights = dict(PHASE_FORCE_WEIGHTS.get(phase, PHASE_FORCE_WEIGHTS[WuXingPhase.EARTH]))

        # Adjust weights based on objective keywords
        if any(kw in obj_lower for kw in ["scan", "search", "find", "audit", "check"]):
            weights[ForceType.LIGHT_INFANTRY] += 0.15
            weights[ForceType.DARE_TO_DIE] += 0.10
        if any(kw in obj_lower for kw in ["fix", "implement", "build", "create", "write"]):
            weights[ForceType.HEAVY_CAVALRY] += 0.20
        if any(kw in obj_lower for kw in ["complex", "architecture", "multi", "decompose"]):
            weights[ForceType.OFFICERS] += 0.15
        if any(kw in obj_lower for kw in ["test", "run", "execute", "deploy", "tool"]):
            weights[ForceType.SIEGE_ENGINEERS] += 0.15
        if any(kw in obj_lower for kw in ["stuck", "unknown", "weird", "chaos", "brute"]):
            weights[ForceType.DARE_TO_DIE] += 0.20

        # Normalize weights
        total_weight = sum(weights.values())
        weights = {k: v / total_weight for k, v in weights.items()}

        # Generate ForceSpecs
        specs = []
        for force_type, weight in sorted(weights.items(), key=lambda x: -x[1]):
            count = max(1, int(max_clones * weight))
            if count < 5 and force_type not in (ForceType.OFFICERS, ForceType.SIEGE_ENGINEERS):
                continue  # Too small to matter

            strategies = self._strategies_for_force(force_type, phase)
            nature = ForceNature.QI if force_type in (ForceType.DARE_TO_DIE, ForceType.HEAVY_CAVALRY) else ForceNature.ZHENG

            specs.append(ForceSpec(
                force_type=force_type,
                clone_count=count,
                strategies=strategies,
                nature=nature,
                wu_xing_phase=phase,
                stateless=(force_type == ForceType.DARE_TO_DIE),
                description=f"{force_type.value}: {count} clones, phase={phase.value}",
            ))

        return specs

    def select_tactic(self, situation: str) -> Tactic:
        """Select the best named tactic for a given situation.

        Args:
            situation: Description of the current situation

        Returns:
            The best matching Tactic
        """
        sit_lower = situation.lower()
        best_tactic = NAMED_TACTICS[0]
        best_score = 0

        for tactic in NAMED_TACTICS:
            score = sum(1 for kw in tactic.best_for if kw in sit_lower)
            # Also check description
            score += sum(1 for word in sit_lower.split() if word in tactic.description.lower())
            if score > best_score:
                best_score = score
                best_tactic = tactic

        return best_tactic

    def select_stratagems(
        self,
        situation: str,
        max_results: int = 5,
    ) -> list[Stratagem]:
        """Select the most applicable stratagems for a situation.

        Args:
            situation: Description of the situation
            max_results: Maximum number of stratagems to return

        Returns:
            Ranked list of applicable stratagems
        """
        sit_lower = situation.lower()
        scored: list[tuple[int, Stratagem]] = []

        for s in self.stratagems:
            score = sum(1 for kw in s.keywords if kw in sit_lower)
            # Boost from application text match
            score += sum(1 for word in sit_lower.split() if word in s.application.lower()) * 0.5
            scored.append((int(score * 10), s))

        scored.sort(key=lambda x: -x[0])
        return [s for _, s in scored[:max_results]]

    def get_stratagem(self, number: int) -> Stratagem | None:
        """Get a specific stratagem by number (1-36)."""
        return _STRATAGEM_BY_NUMBER.get(number)

    def get_tactic(self, name: str) -> Tactic | None:
        """Get a named tactic by name."""
        return _TACTIC_BY_NAME.get(name.lower())

    def campaign_summary(self, objective: str) -> dict[str, Any]:
        """Generate a complete campaign summary for an objective.

        Returns a dict with recommended force composition, tactics,
        stratagems, and Wu Xing phase progression.
        """
        force = self.recommend_force(objective)
        tactic = self.select_tactic(objective)
        stratagems = self.select_stratagems(objective, max_results=3)
        phase = self._detect_phase(objective.lower())

        summary = {
            "objective": objective,
            "wu_xing_phase": phase.value,
            "phase_generates": PHASE_GENERATES[phase].value,
            "recommended_tactic": {
                "name": tactic.name,
                "chinese": tactic.chinese,
                "description": tactic.description,
                "phases": [
                    {"name": p.name, "force": p.force_type.value, "count": p.clone_count}
                    for p in tactic.phases
                ],
            },
            "force_composition": [
                {
                    "type": spec.force_type.value,
                    "count": spec.clone_count,
                    "nature": spec.nature.value,
                    "strategies": spec.strategies,
                    "stateless": spec.stateless,
                }
                for spec in force
            ],
            "applicable_stratagems": [
                {"number": s.number, "name": s.name, "chinese": s.chinese, "application": s.application}
                for s in stratagems
            ],
            "total_clones": sum(s.clone_count for s in force),
        }

        self._campaign_history.append(summary)
        return summary

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    def _detect_phase(self, obj_lower: str) -> WuXingPhase:
        """Detect the appropriate Wu Xing phase from objective text."""
        phase_keywords = {
            WuXingPhase.WOOD: ["explore", "scout", "search", "find", "discover", "research", "recon"],
            WuXingPhase.FIRE: ["fix", "implement", "build", "execute", "deploy", "attack", "strike"],
            WuXingPhase.EARTH: ["integrate", "consolidate", "document", "test", "stabilize", "merge"],
            WuXingPhase.METAL: ["optimize", "refine", "audit", "quality", "boundary", "benchmark"],
            WuXingPhase.WATER: ["analyze", "reflect", "plan", "strategy", "review", "assess"],
        }

        scores = {phase: 0 for phase in WuXingPhase}
        for phase, keywords in phase_keywords.items():
            for kw in keywords:
                if kw in obj_lower:
                    scores[phase] += 1

        best_phase = max(scores, key=lambda p: scores[p])
        return best_phase if scores[best_phase] > 0 else WuXingPhase.EARTH

    def _strategies_for_force(self, force_type: ForceType, phase: WuXingPhase) -> list[str]:
        """Select clone strategies appropriate for force type and phase."""
        base = {
            ForceType.LIGHT_INFANTRY: ["direct", "analytical", "creative"],
            ForceType.HEAVY_CAVALRY: ["chain_of_thought", "synthesis", "memory_grounded"],
            ForceType.OFFICERS: ["analytical", "synthesis"],
            ForceType.SIEGE_ENGINEERS: ["direct", "systematic"],
            ForceType.DARE_TO_DIE: ["direct"],  # Minimal. Pu — the Uncarved Block.
        }

        strategies = list(base.get(force_type, ["direct"]))

        # Phase-specific adjustments
        if phase == WuXingPhase.WOOD:
            strategies.append("creative")
        elif phase == WuXingPhase.FIRE:
            strategies.append("adversarial")
        elif phase == WuXingPhase.WATER:
            strategies.append("synthesis")

        return list(dict.fromkeys(strategies))  # Dedupe preserving order


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_doctrine: ImperialDoctrine | None = None


def get_doctrine() -> ImperialDoctrine:
    """Get the global Imperial Doctrine instance."""
    global _doctrine
    if _doctrine is None:
        _doctrine = ImperialDoctrine()
        logger.info("Imperial Doctrine initialized — 36 Stratagems, 5 Tactics, 5 Force Types")
    return _doctrine
