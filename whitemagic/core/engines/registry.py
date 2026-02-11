"""Engine Registry — 28-engine manifest with Garden/Gana/Grimoire links (Leap 7d).

Maps every cognitive engine in WhiteMagic to the 28-fold mandala structure:
- Lunar Mansion (Chinese 二十八宿)
- Garden directory
- Grimoire chapter
- Wu Xing element (via quadrant)
- Dispatch slot (for StateBoard circuit breakers and rate counters)

This registry is the single source of truth for the Engine Framework.
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import IntEnum


class Quadrant(IntEnum):
    """Celestial quadrants mapping to seasons and Wu Xing elements."""
    EAST = 0    # Azure Dragon, Spring, Wood
    SOUTH = 1   # Vermilion Bird, Summer, Fire
    WEST = 2    # White Tiger, Autumn, Metal
    NORTH = 3   # Black Tortoise, Winter, Water


class EngineStatus(IntEnum):
    """Engine implementation status."""
    EXISTS = 0
    DISTRIBUTED = 1   # Spread across multiple files
    PLANNED = 2


@dataclass(frozen=True)
class EngineEntry:
    """A single engine in the 28-fold manifest."""

    slot: int                  # 0-27, matches StateBoard breaker/counter slot
    mansion_name: str          # English name of the Lunar Mansion
    mansion_chinese: str       # Chinese character
    mansion_pinyin: str        # Pinyin romanization
    garden: str                # Garden directory name
    engine_name: str           # Engine class/module name
    source_path: str           # Primary source file (relative to whitemagic/)
    quadrant: Quadrant
    wu_xing: str               # wood, fire, earth, metal, water
    emotion: str               # Associated emotion/quality
    grimoire_chapter: int      # Grimoire chapter number (0-indexed)
    description: str           # One-line description
    status: EngineStatus = EngineStatus.EXISTS

    @property
    def handler_id(self) -> int:
        """Handler ID for dispatch routing (100 + slot)."""
        return 100 + self.slot

    @property
    def season(self) -> str:
        """Season associated with this engine's quadrant."""
        return {
            Quadrant.EAST: "spring",
            Quadrant.SOUTH: "summer",
            Quadrant.WEST: "autumn",
            Quadrant.NORTH: "winter",
        }[self.quadrant]

    @property
    def celestial_animal(self) -> str:
        """Celestial animal for this engine's quadrant."""
        return {
            Quadrant.EAST: "Azure Dragon 青龍",
            Quadrant.SOUTH: "Vermilion Bird 朱雀",
            Quadrant.WEST: "White Tiger 白虎",
            Quadrant.NORTH: "Black Tortoise 玄武",
        }[self.quadrant]


# === THE 28-ENGINE MANIFEST ===

ENGINE_REGISTRY: tuple[EngineEntry, ...] = (
    # ── Eastern Quadrant (Azure Dragon, Spring, Wood) ── Mansions 1-7
    EngineEntry(
        slot=0, mansion_name="Horn", mansion_chinese="角", mansion_pinyin="Jiǎo",
        garden="courage", engine_name="SessionEngine",
        source_path="core/orchestration/session_startup.py",
        quadrant=Quadrant.EAST, wu_xing="wood", emotion="courage",
        grimoire_chapter=1,
        description="Session initialization, sharp beginnings",
    ),
    EngineEntry(
        slot=1, mansion_name="Neck", mansion_chinese="亢", mansion_pinyin="Kàng",
        garden="practice", engine_name="ConsolidationEngine",
        source_path="core/memory/consolidation.py",
        quadrant=Quadrant.EAST, wu_xing="wood", emotion="practice",
        grimoire_chapter=2,
        description="Memory consolidation, consistent execution",
    ),
    EngineEntry(
        slot=2, mansion_name="Root", mansion_chinese="氐", mansion_pinyin="Dǐ",
        garden="truth", engine_name="BoundaryEngine",
        source_path="core/boundaries/boundary_engine.py",
        quadrant=Quadrant.EAST, wu_xing="wood", emotion="truth",
        grimoire_chapter=3,
        description="System health, structural integrity, hard limits",
    ),
    EngineEntry(
        slot=3, mansion_name="Room", mansion_chinese="房", mansion_pinyin="Fáng",
        garden="sanctuary", engine_name="CircuitBreakerEngine",
        source_path="tools/circuit_breaker.py",
        quadrant=Quadrant.EAST, wu_xing="wood", emotion="sanctuary",
        grimoire_chapter=4,
        description="Resource locks, safe containers, circuit breakers",
    ),
    EngineEntry(
        slot=4, mansion_name="Heart", mansion_chinese="心", mansion_pinyin="Xīn",
        garden="love", engine_name="NurturingEngine",
        source_path="core/nurturing/nurturing_engine.py",
        quadrant=Quadrant.EAST, wu_xing="wood", emotion="love",
        grimoire_chapter=5,
        description="User preference learning, personalization, warmth",
    ),
    EngineEntry(
        slot=5, mansion_name="Tail", mansion_chinese="尾", mansion_pinyin="Wěi",
        garden="metal", engine_name="AccelerationEngine",
        source_path="core/acceleration/__init__.py",
        quadrant=Quadrant.EAST, wu_xing="wood", emotion="metal",
        grimoire_chapter=6,
        description="Polyglot acceleration: Rust, Zig, Mojo bridges",
        status=EngineStatus.DISTRIBUTED,
    ),
    EngineEntry(
        slot=6, mansion_name="Winnowing Basket", mansion_chinese="箕", mansion_pinyin="Jī",
        garden="wisdom", engine_name="SerendipityEngine",
        source_path="core/intelligence/synthesis/serendipity_engine.py",
        quadrant=Quadrant.EAST, wu_xing="wood", emotion="wisdom",
        grimoire_chapter=7,
        description="Surface dormant knowledge via constellation bridges, orphan discovery, and weighted random selection",
    ),

    # ── Southern Quadrant (Vermilion Bird, Summer, Fire) ── Mansions 8-14
    EngineEntry(
        slot=7, mansion_name="Ghost", mansion_chinese="鬼", mansion_pinyin="Guǐ",
        garden="grief", engine_name="IntrospectionEngine",
        source_path="tools/gnosis.py",
        quadrant=Quadrant.SOUTH, wu_xing="fire", emotion="grief",
        grimoire_chapter=8,
        description="Introspection, shadow work, system metrics via Gnosis",
    ),
    EngineEntry(
        slot=8, mansion_name="Willow", mansion_chinese="柳", mansion_pinyin="Liǔ",
        garden="play", engine_name="ResilienceEngine",
        source_path="core/patterns/emergence/dream_state.py",
        quadrant=Quadrant.SOUTH, wu_xing="fire", emotion="play",
        grimoire_chapter=9,
        description="Dream cycle, resilience through flexibility",
    ),
    EngineEntry(
        slot=9, mansion_name="Star", mansion_chinese="星", mansion_pinyin="Xīng",
        garden="beauty", engine_name="GovernanceEngine",
        source_path="dharma/rules.py",
        quadrant=Quadrant.SOUTH, wu_xing="fire", emotion="beauty",
        grimoire_chapter=10,
        description="Dharma rules, karma ledger, governance illumination",
    ),
    EngineEntry(
        slot=10, mansion_name="Extended Net", mansion_chinese="张", mansion_pinyin="Zhāng",
        garden="connection", engine_name="AssociationEngine",
        source_path="core/memory/association_miner.py",
        quadrant=Quadrant.SOUTH, wu_xing="fire", emotion="connection",
        grimoire_chapter=11,
        description="Pattern connectivity, semantic association mining, temporal traversal tracking",
    ),
    EngineEntry(
        slot=11, mansion_name="Wings", mansion_chinese="翼", mansion_pinyin="Yì",
        garden="adventure", engine_name="ExportEngine",
        source_path="tools/handlers/export_import.py",
        quadrant=Quadrant.SOUTH, wu_xing="fire", emotion="adventure",
        grimoire_chapter=12,
        description="Export, deploy, expand — memory and state portability",
    ),
    EngineEntry(
        slot=12, mansion_name="Chariot", mansion_chinese="轸", mansion_pinyin="Zhěn",
        garden="transformation", engine_name="ArchaeologyEngine",
        source_path="archaeology/dig.py",
        quadrant=Quadrant.SOUTH, wu_xing="fire", emotion="transformation",
        grimoire_chapter=13,
        description="Code archaeology, historical analysis, navigation",
    ),
    EngineEntry(
        slot=13, mansion_name="Abundance", mansion_chinese="豐", mansion_pinyin="Fēng",
        garden="joy", engine_name="ResonanceEngine",
        source_path="core/resonance/resonance_engine.py",
        quadrant=Quadrant.SOUTH, wu_xing="fire", emotion="joy",
        grimoire_chapter=14,
        description="Amplify emergent patterns through sympathetic resonance",
    ),

    # ── Western Quadrant (White Tiger, Autumn, Metal) ── Mansions 15-21
    EngineEntry(
        slot=14, mansion_name="Straddling Legs", mansion_chinese="奎", mansion_pinyin="Kuí",
        garden="patience", engine_name="DharmicSolver",
        source_path="core/intelligence/synthesis/solver_engine.py",
        quadrant=Quadrant.WEST, wu_xing="metal", emotion="patience",
        grimoire_chapter=15,
        description="Constrained optimization via Frank-Wolfe on causal DAGs",
    ),
    EngineEntry(
        slot=15, mansion_name="Mound", mansion_chinese="娄", mansion_pinyin="Lóu",
        garden="gratitude", engine_name="EmbeddingEngine",
        source_path="core/memory/embeddings.py",
        quadrant=Quadrant.WEST, wu_xing="metal", emotion="gratitude",
        grimoire_chapter=16,
        description="Semantic embedding indexing, vector search, caching",
    ),
    EngineEntry(
        slot=16, mansion_name="Stomach", mansion_chinese="胃", mansion_pinyin="Wèi",
        garden="healing", engine_name="LifecycleEngine",
        source_path="core/memory/lifecycle.py",
        quadrant=Quadrant.WEST, wu_xing="metal", emotion="healing",
        grimoire_chapter=17,
        description="Memory lifecycle management, resource/energy management",
    ),
    EngineEntry(
        slot=17, mansion_name="Hairy Head", mansion_chinese="昴", mansion_pinyin="Mǎo",
        garden="presence", engine_name="KaizenEngine",
        source_path="core/intelligence/synthesis/kaizen_engine.py",
        quadrant=Quadrant.WEST, wu_xing="metal", emotion="presence",
        grimoire_chapter=18,
        description="Continuous self-improvement via CoreAccessLayer: constellation anomalies, broken associations, cross-constellation gaps",
    ),
    EngineEntry(
        slot=18, mansion_name="Net", mansion_chinese="毕", mansion_pinyin="Bì",
        garden="mystery", engine_name="PatternEngine",
        source_path="core/memory/pattern_engine.py",
        quadrant=Quadrant.WEST, wu_xing="metal", emotion="mystery",
        grimoire_chapter=19,
        description="Pattern capture, filtering, consciousness detection",
    ),
    EngineEntry(
        slot=19, mansion_name="Turtle Beak", mansion_chinese="觜", mansion_pinyin="Zī",
        garden="voice", engine_name="NarrativeEngine",
        source_path="gardens/voice/narrative_engine.py",
        quadrant=Quadrant.WEST, wu_xing="metal", emotion="voice",
        grimoire_chapter=20,
        description="Precision validation, narrative generation",
    ),
    EngineEntry(
        slot=20, mansion_name="Three Stars", mansion_chinese="参", mansion_pinyin="Shēn",
        garden="dharma", engine_name="EthicsEngine",
        source_path="gardens/dharma/ethics_engine.py",
        quadrant=Quadrant.WEST, wu_xing="metal", emotion="dharma",
        grimoire_chapter=21,
        description="Ethical evaluation, wisdom council, judgment",
    ),

    # ── Northern Quadrant (Black Tortoise, Winter, Water) ── Mansions 22-28
    EngineEntry(
        slot=21, mansion_name="Dipper", mansion_chinese="斗", mansion_pinyin="Dǒu",
        garden="awe", engine_name="PredictiveEngine",
        source_path="core/intelligence/synthesis/predictive_engine.py",
        quadrant=Quadrant.NORTH, wu_xing="water", emotion="awe",
        grimoire_chapter=22,
        description="Future-forward prediction via CoreAccessLayer: constellation drift, association paths, temporal trends",
    ),
    EngineEntry(
        slot=22, mansion_name="Ox", mansion_chinese="牛", mansion_pinyin="Niú",
        garden="reverence", engine_name="GalacticEngine",
        source_path="core/memory/galactic_map.py",
        quadrant=Quadrant.NORTH, wu_xing="water", emotion="reverence",
        grimoire_chapter=23,
        description="Galactic map, zone scoring, endurance persistence",
    ),
    EngineEntry(
        slot=23, mansion_name="Girl", mansion_chinese="女", mansion_pinyin="Nǚ",
        garden="wonder", engine_name="CloneArmyEngine",
        source_path="core/memory/clones/clone_army.py",
        quadrant=Quadrant.NORTH, wu_xing="water", emotion="wonder",
        grimoire_chapter=24,
        description="Kage no Bunshin: massive parallel search deployment",
    ),
    EngineEntry(
        slot=24, mansion_name="Void", mansion_chinese="虚", mansion_pinyin="Xū",
        garden="stillness", engine_name="ForgettingEngine",
        source_path="core/memory/mindful_forgetting.py",
        quadrant=Quadrant.NORTH, wu_xing="water", emotion="stillness",
        grimoire_chapter=25,
        description="Mindful forgetting, meditation, defragmentation",
    ),
    EngineEntry(
        slot=25, mansion_name="Roof", mansion_chinese="危", mansion_pinyin="Wēi",
        garden="protection", engine_name="SanitizationEngine",
        source_path="tools/input_sanitizer.py",
        quadrant=Quadrant.NORTH, wu_xing="water", emotion="protection",
        grimoire_chapter=26,
        description="Input sanitization, tool permissions, safety shelter",
    ),
    EngineEntry(
        slot=26, mansion_name="Encampment", mansion_chinese="室", mansion_pinyin="Shì",
        garden="sangha", engine_name="SwarmEngine",
        source_path="agents/swarm.py",
        quadrant=Quadrant.NORTH, wu_xing="water", emotion="sangha",
        grimoire_chapter=27,
        description="Multi-agent swarm coordination, community handoff",
    ),
    EngineEntry(
        slot=27, mansion_name="Wall", mansion_chinese="壁", mansion_pinyin="Bì",
        garden="air", engine_name="EmergenceEngine",
        source_path="core/intelligence/agentic/emergence_engine.py",
        quadrant=Quadrant.NORTH, wu_xing="water", emotion="air",
        grimoire_chapter=28,
        description="Proactive insight synthesis: constellation convergence, association hotspots, temporal bursts, resonance cascades",
    ),
)

# Build lookup indices
_BY_SLOT: dict[int, EngineEntry] = {e.slot: e for e in ENGINE_REGISTRY}
_BY_NAME: dict[str, EngineEntry] = {e.engine_name: e for e in ENGINE_REGISTRY}
_BY_GARDEN: dict[str, EngineEntry] = {e.garden: e for e in ENGINE_REGISTRY}
_BY_MANSION: dict[str, EngineEntry] = {e.mansion_name: e for e in ENGINE_REGISTRY}


def get_engine_entry(key: int | str) -> EngineEntry | None:
    """Look up an engine by slot number, engine name, garden name, or mansion name."""
    if isinstance(key, int):
        return _BY_SLOT.get(key)
    return _BY_NAME.get(key) or _BY_GARDEN.get(key) or _BY_MANSION.get(key)


def get_engines_by_quadrant(quadrant: Quadrant) -> list[EngineEntry]:
    """Get all engines in a quadrant."""
    return [e for e in ENGINE_REGISTRY if e.quadrant == quadrant]


def get_engines_by_status(status: EngineStatus) -> list[EngineEntry]:
    """Get all engines with a given status."""
    return [e for e in ENGINE_REGISTRY if e.status == status]


def read_engine_board(slot: int) -> dict[str, object] | None:
    """Engine Data Sea: read the StateBoard for an engine slot (Leap 8b).

    Returns harmony snapshot, breaker state, and resonance data from
    the shared-memory StateBoard. Engines use this to make decisions
    without crossing Python boundaries.
    """
    entry = _BY_SLOT.get(slot)
    if entry is None:
        return None
    try:
        from whitemagic.core.acceleration.state_board_bridge import get_state_board
        board = get_state_board()
        harmony = board.read_harmony()
        breaker_state, breaker_failures = board.read_breaker(slot)
        resonance = board.read_resonance()
        return {
            "slot": slot,
            "engine": entry.engine_name,
            "garden": entry.garden,
            "harmony": {
                "balance": harmony.balance,
                "throughput": harmony.throughput,
                "latency": harmony.latency,
                "error_rate": harmony.error_rate,
                "dharma": harmony.dharma,
                "karma_debt": harmony.karma_debt,
                "energy": harmony.energy,
            },
            "breaker": {
                "state": breaker_state,  # 0=CLOSED, 1=OPEN, 2=HALF_OPEN
                "failures": breaker_failures,
            },
            "resonance": {
                "gana": resonance.current_gana,
                "guna": resonance.guna.name,
                "tick": getattr(resonance, "tick", 0),
            },
        }
    except Exception:
        return {"slot": slot, "engine": entry.engine_name, "error": "board_unavailable"}


def get_engine_stats() -> dict[str, int | dict[str, int]]:
    """Get summary statistics about the engine registry."""
    by_quadrant = {
        "east": len(get_engines_by_quadrant(Quadrant.EAST)),
        "south": len(get_engines_by_quadrant(Quadrant.SOUTH)),
        "west": len(get_engines_by_quadrant(Quadrant.WEST)),
        "north": len(get_engines_by_quadrant(Quadrant.NORTH)),
    }
    by_status = {
        "exists": len(get_engines_by_status(EngineStatus.EXISTS)),
        "distributed": len(get_engines_by_status(EngineStatus.DISTRIBUTED)),
        "planned": len(get_engines_by_status(EngineStatus.PLANNED)),
    }
    by_wu_xing: dict[str, int] = {}
    for e in ENGINE_REGISTRY:
        by_wu_xing[e.wu_xing] = by_wu_xing.get(e.wu_xing, 0) + 1

    return {
        "total_engines": len(ENGINE_REGISTRY),
        "by_quadrant": by_quadrant,
        "by_status": by_status,
        "by_wu_xing": by_wu_xing,
    }
