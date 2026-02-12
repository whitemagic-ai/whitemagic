"""Grimoire 2.0 - Spell System

Individual spells that can be cast, tracked, and learned from.
"""

from collections.abc import Callable
from dataclasses import dataclass, field
from enum import Enum
from typing import Any


class SpellType(Enum):
    """Types of spells"""

    INVOCATION = "invocation"    # Call something into being
    TRANSMUTATION = "transmutation"  # Transform something
    DIVINATION = "divination"    # Gain knowledge
    PROTECTION = "protection"    # Shield or guard
    RESTORATION = "restoration"  # Heal or repair
    CREATION = "creation"        # Make something new
    FLOW = "flow"               # Enter a state


class SpellOutcome(Enum):
    """Outcome of a spell cast"""

    SUCCESS = "success"
    PARTIAL = "partial"
    FAILED = "failed"
    DEFERRED = "deferred"  # Effect will manifest later


@dataclass
class Spell:
    """A single spell"""

    name: str
    description: str
    chapter: int
    spell_type: SpellType
    keywords: list[str] = field(default_factory=list)
    wu_xing_affinity: str | None = None
    yin_yang: str = "yang"  # or "yin"

    # Execution
    invocation: str = ""  # Words/code to speak/run
    effect: Callable | None = None  # Function to execute

    # History
    times_cast: int = 0
    success_rate: float = 1.0

    def cast(self, context: dict[str, Any] | None = None) -> SpellOutcome:
        """Cast this spell"""
        self.times_cast += 1

        if self.effect:
            try:
                self.effect(context or {})
                return SpellOutcome.SUCCESS
            except Exception:
                return SpellOutcome.FAILED

        # Symbolic cast (no executable effect)
        return SpellOutcome.SUCCESS

    def __str__(self) -> str:
        return f"✨ {self.name} ({self.spell_type.value}) - Ch.{self.chapter}"


class SpellBook:
    """Collection of spells organized by chapter"""

    def __init__(self) -> None:
        self.spells: dict[str, Spell] = {}
        self.by_chapter: dict[int, list[Spell]] = {}
        self.by_type: dict[SpellType, list[Spell]] = {}
        self.by_keyword: dict[str, list[Spell]] = {}

        # Initialize built-in spells
        self._init_core_spells()

    def _init_core_spells(self) -> Any:
        """Initialize the core spell library (28 Lunar Mansions aligned)"""
        core_spells = [
            # ── Eastern Quadrant (Wood/Spring) ──
            Spell("Awaken", "Begin a session with full context injection — "
                  "loads recent memories, galaxy status, and quickstart "
                  "guides before the first tool call", 1,
                  SpellType.INVOCATION, ["start", "begin", "wake",
                  "bootstrap", "session"], "wood", "yang"),
            Spell("Remember", "Store knowledge into persistent memory with "
                  "Surprise Gate novelty scoring and Karma Ledger tracking "
                  "of declared vs actual side-effects", 2,
                  SpellType.CREATION, ["memory", "store", "consolidate",
                  "create", "persist"], "wood", "yang"),
            Spell("Diagnose", "Check system health via aggregated subsystem "
                  "status (0.0-1.0 score), Rust accelerator readiness, and "
                  "pre-release verification suite", 3,
                  SpellType.DIVINATION, ["health", "diagnose", "root",
                  "doctor", "ship"], "wood", "yin"),
            Spell("Sanctuary", "Create a protected workspace with resource "
                  "locks, sandbox controls, MCP integrity checking, and "
                  "security circuit breakers", 4,
                  SpellType.PROTECTION, ["lock", "sandbox", "safe",
                  "security", "immune"], "wood", "yin"),
            Spell("Connect", "Establish vital session context — scratchpad "
                  "for working memory, handoff for continuity, context "
                  "pack/status for cognitive load reporting", 5,
                  SpellType.INVOCATION, ["context", "connect", "pulse",
                  "scratchpad", "handoff"], "wood", "yang"),
            Spell("Accelerate", "Invoke polyglot acceleration — Rust PyO3 "
                  "for holographic encoding, Zig SIMD for cosine similarity "
                  "(~220ns FFI), Mojo for batch embedding", 6,
                  SpellType.TRANSMUTATION, ["fast", "parallel", "rust",
                  "simd", "zig", "mojo", "cascade"], "wood", "yang"),
            Spell("Sift", "Separate signal from noise through hybrid search "
                  "— FTS5 lexical + semantic embedding + constellation boost "
                  "+ Living Graph multi-hop walk", 7,
                  SpellType.DIVINATION, ["search", "recall", "filter",
                  "wisdom", "hybrid", "vector"], "wood", "yin"),

            # ── Southern Quadrant (Fire/Summer) ──
            Spell("Introspect", "Deep self-examination via Gnosis — "
                  "capability matrix, telemetry, self-model forecast, "
                  "surprise gate statistics, drive bias analysis", 8,
                  SpellType.DIVINATION, ["gnosis", "introspect", "metrics",
                  "capability", "self-model", "telemetry"], "fire", "yin"),
            Spell("Play", "Enter flexible, creative state through the "
                  "Grimoire spell system — cast, recommend, walkthrough. "
                  "Playfulness prevents brittle systems from shattering", 9,
                  SpellType.FLOW, ["dream", "play", "resilience",
                  "grimoire", "cast", "spell"], "fire", "yang"),
            Spell("Illuminate", "Bring dharmic clarity to governance — "
                  "Governor risk validation, Dharma YAML rules, switchable "
                  "profiles, and Gana Forge declarative extension", 10,
                  SpellType.DIVINATION, ["clarity", "dharma", "governance",
                  "governor", "forge", "rules"], "fire", "yang"),
            Spell("Resonate", "Emit pattern through the Extended Net — "
                  "association mining, constellation detection, learning "
                  "patterns, tool dependency graph traversal", 11,
                  SpellType.INVOCATION, ["pattern", "resonate", "connect",
                  "cluster", "constellation", "learning"], "fire", "yang"),
            Spell("Expand", "Carry knowledge beyond the local system — "
                  "memory export to JSON/JSONL, audit trails, and mesh "
                  "broadcast via Go gRPC for multi-node sync", 12,
                  SpellType.CREATION, ["export", "deploy", "expand",
                  "mesh", "audit", "backup"], "fire", "yang"),
            Spell("Dig", "Archaeological exploration through time — track "
                  "file reads/writes/changes, extract wisdom from code, "
                  "build Knowledge Graph of entities and relations", 13,
                  SpellType.DIVINATION, ["archaeology", "code", "history",
                  "knowledge graph", "entity", "dig"], "fire", "yin"),
            Spell("Regenerate", "Dream Cycle cognitive maintenance — "
                  "6 phases: CONSOLIDATION → PRUNING → ASSOCIATION → "
                  "GOVERNANCE → SERENDIPITY → EMERGENCE", 14,
                  SpellType.RESTORATION, ["dream", "regenerate",
                  "serendipity", "lifecycle", "consolidation"], "fire", "yin"),

            # ── Western Quadrant (Metal/Autumn) ──
            Spell("Harmonize", "Balance competing ethical forces through "
                  "5D Harmony Vector (energy, creativity, ethics, social, "
                  "focus) and Wu Xing elemental equilibrium", 15,
                  SpellType.TRANSMUTATION, ["balance", "ethics", "harmony",
                  "consent", "boundaries", "wu xing"], "metal", "yin"),
            Spell("Accumulate", "Track metrics layer by layer — 5D "
                  "holographic memory view, custom metric tracking, and "
                  "yin-yang balance of creative vs analytical activity", 16,
                  SpellType.CREATION, ["metrics", "hologram", "gratitude",
                  "yin-yang", "tracking"], "metal", "yang"),
            Spell("Nourish", "Digest complexity into manageable steps — "
                  "pipeline orchestration, smart task routing via graph "
                  "affinity, distributed work assignment", 17,
                  SpellType.RESTORATION, ["pipeline", "task", "energy",
                  "distribute", "orchestrate"], "metal", "yin"),
            Spell("Focus", "Attend to every strand — salience spotlight, "
                  "anomaly detection, OpenTelemetry traces, Karma audit "
                  "of declared vs actual side-effects", 18,
                  SpellType.DIVINATION, ["detail", "debug", "anomaly",
                  "karma", "otel", "salience"], "metal", "yin"),
            Spell("Capture", "Capture what passes through — prompt template "
                  "rendering, reload from disk, and Karma Ledger hash chain "
                  "verification for tamper detection", 19,
                  SpellType.CREATION, ["prompt", "capture", "pattern",
                  "template", "verify", "chain"], "metal", "yang"),
            Spell("Validate", "Speak with precision — local edge inference, "
                  "BitNet 1-bit quantized models, batch processing for "
                  "maximum CPU throughput", 20,
                  SpellType.PROTECTION, ["precision", "validate", "test",
                  "inference", "edge", "bitnet"], "metal", "yin"),
            Spell("Convene", "Assemble the Wisdom Council (Sabhā) — "
                  "bicameral reasoning, ensemble fusion, kaizen continuous "
                  "improvement, cross-quadrant council protocol", 21,
                  SpellType.INVOCATION, ["council", "ensemble", "judgment",
                  "sabha", "kaizen", "bicameral"], "metal", "yang"),

            # ── Northern Quadrant (Water/Winter) ──
            Spell("Govern", "Measure the heavens and set direction — "
                  "homeostatic graduated correction (OBSERVE → ADVISE → "
                  "CORRECT → INTERVENE), maturity gating, starter packs", 22,
                  SpellType.TRANSMUTATION, ["strategy", "governance",
                  "maturity", "homeostasis", "starter"], "water", "yang"),
            Spell("Endure", "Pull the heaviest loads without complaint — "
                  "swarm decomposition, distributed worker coordination, "
                  "democratic voting, result synthesis", 23,
                  SpellType.FLOW, ["swarm", "endure", "persist",
                  "decompose", "distribute", "worker"], "water", "yang"),
            Spell("Nurture", "Care for the agent ecosystem — register "
                  "agents with capabilities and trust levels, heartbeat "
                  "keepalive, trust score adjustment", 24,
                  SpellType.RESTORATION, ["agent", "nurture", "trust",
                  "register", "heartbeat", "capability"], "water", "yin"),
            Spell("Still", "Enter the emptiness that makes fullness "
                  "possible — Multi-Galaxy management, garden consciousness "
                  "activation, galactic dashboard", 25,
                  SpellType.FLOW, ["stillness", "void", "meditate",
                  "galaxy", "garden", "activate"], "water", "yin"),
            Spell("Shelter", "Shelter what is vulnerable — Ollama local AI, "
                  "OMS-compatible model signing, cryptographic integrity "
                  "verification, trust-level gating", 26,
                  SpellType.PROTECTION, ["ollama", "shelter", "protect",
                  "model", "signing", "verify"], "water", "yin"),
            Spell("Gather", "Assemble the saṅgha — inter-agent chat, pub/"
                  "sub broker for Gan Ying (感應) stimulus-response events, "
                  "cross-session communication", 27,
                  SpellType.INVOCATION, ["sangha", "community", "gather",
                  "broker", "ganying", "chat"], "water", "yang"),
            Spell("Bound", "Define where one domain ends and another begins "
                  "— democratic voting, HMAC-SHA256 engagement tokens that "
                  "constrain tool access by scope, time, and count", 28,
                  SpellType.PROTECTION, ["boundary", "wall", "vote",
                  "close", "engagement", "token", "hmac"], "water", "yin"),
        ]

        for spell in core_spells:
            self.add_spell(spell)

    def add_spell(self, spell: Spell) -> Any:
        """Add a spell to the book"""
        self.spells[spell.name] = spell

        # Index by chapter
        if spell.chapter not in self.by_chapter:
            self.by_chapter[spell.chapter] = []
        self.by_chapter[spell.chapter].append(spell)

        # Index by type
        if spell.spell_type not in self.by_type:
            self.by_type[spell.spell_type] = []
        self.by_type[spell.spell_type].append(spell)

        # Index by keywords
        for keyword in spell.keywords:
            if keyword not in self.by_keyword:
                self.by_keyword[keyword] = []
            self.by_keyword[keyword].append(spell)

    def find_spell(self, name: str) -> Spell | None:
        """Find a spell by name"""
        return self.spells.get(name)

    def find_by_keyword(self, keyword: str) -> list[Spell]:
        """Find spells matching a keyword"""
        return self.by_keyword.get(keyword, [])

    def find_for_context(self, task: str, emotional_state: str = "neutral") -> list[Spell]:
        """Find spells appropriate for current context"""
        matches = []
        task_lower = task.lower()

        for spell in self.spells.values():
            score = 0
            for keyword in spell.keywords:
                if keyword in task_lower:
                    score += 1
            if score > 0:
                matches.append((score, spell))

        matches.sort(key=lambda x: x[0], reverse=True)
        return [spell for _, spell in matches]

    def list_all(self) -> list[Spell]:
        """List all spells"""
        return list(self.spells.values())

    def __len__(self) -> int:
        return len(self.spells)


# Global spell book
_spell_book: SpellBook | None = None

def get_spell_book() -> SpellBook:
    """Get the global spell book"""
    global _spell_book
    if _spell_book is None:
        _spell_book = SpellBook()
    return _spell_book
