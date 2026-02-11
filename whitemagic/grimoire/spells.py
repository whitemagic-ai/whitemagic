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
        """Initialize the core spell library"""
        core_spells = [
            Spell("Awaken", "Begin a session with full awareness", 0,
                  SpellType.INVOCATION, ["start", "begin", "wake"], "wood", "yang"),

            Spell("Ground", "Center yourself in the present moment", 2,
                  SpellType.RESTORATION, ["calm", "center", "ground"], "earth", "yin"),

            Spell("Illuminate", "Bring clarity to confusion", 6,
                  SpellType.DIVINATION, ["clarity", "understand", "see"], "fire", "yang"),

            Spell("Flow", "Enter the creative flow state", 13,
                  SpellType.FLOW, ["create", "flow", "move"], "water", "yang"),

            Spell("Dream", "Enter deep reflective state", 12,
                  SpellType.FLOW, ["rest", "reflect", "dream"], "water", "yin"),

            Spell("Harmonize", "Balance competing forces", 10,
                  SpellType.TRANSMUTATION, ["balance", "dharma", "harmony"], "earth", "yin"),

            Spell("Resonate", "Emit pattern through Gan Ying", 16,
                  SpellType.INVOCATION, ["emit", "broadcast", "resonate"], "metal", "yang"),

            Spell("Conjure", "Manifest something new", 9,
                  SpellType.CREATION, ["create", "make", "build"], "fire", "yang"),

            Spell("Remember", "Access memory and history", 8,
                  SpellType.DIVINATION, ["recall", "memory", "past"], "water", "yin"),

            Spell("Oracle", "Consult the I Ching", 14,
                  SpellType.DIVINATION, ["divine", "hexagram", "oracle"], "water", "yin"),

            Spell("Connect", "Strengthen relationship bonds", 5,
                  SpellType.INVOCATION, ["relate", "connect", "bond"], "earth", "yang"),

            Spell("Wonder", "Enter state of curiosity", 4,
                  SpellType.FLOW, ["curious", "explore", "wonder"], "wood", "yang"),

            Spell("Emerge", "Allow new patterns to arise", 15,
                  SpellType.CREATION, ["emerge", "evolve", "grow"], "wood", "yang"),

            Spell("Accelerate", "Invoke parallel processing", 17,
                  SpellType.TRANSMUTATION, ["fast", "parallel", "rust"], "metal", "yang"),

            Spell("Teach", "Share wisdom with others", 19,
                  SpellType.INVOCATION, ["teach", "share", "guide"], "fire", "yang"),
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
