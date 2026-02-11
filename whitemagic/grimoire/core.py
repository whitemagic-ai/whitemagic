"""Grimoire 2.0 Core - The Living Spellbook

"Free your mind and your ass will follow" - Funkadelic
"""

from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any


class GrimoireState(Enum):
    """Current state of the grimoire"""

    DORMANT = "dormant"      # Not active
    LISTENING = "listening"  # Monitoring context
    CASTING = "casting"      # Actively casting spell
    LEARNING = "learning"    # Processing outcomes


class WuXingPhase(Enum):
    """Five Elements / Wu Xing phases"""

    WOOD = ("wood", "🌳", "growth", "planning", "spring")
    FIRE = ("fire", "🔥", "action", "expression", "summer")
    EARTH = ("earth", "🌍", "stability", "grounding", "transition")
    METAL = ("metal", "⚙️", "structure", "refinement", "autumn")
    WATER = ("water", "💧", "reflection", "wisdom", "winter")

    @property
    def element(self) -> str:
        return self.value[0]

    @property
    def symbol(self) -> str:
        return self.value[1]

    @property
    def quality(self) -> str:
        return self.value[2]

    @property
    def action(self) -> str:
        return self.value[3]


class YinYangPhase(Enum):
    """Yin/Yang phase"""

    YIN = "yin"    # Receptive, inward
    YANG = "yang"  # Creative, outward


@dataclass
class GrimoireContext:
    """Current context for spell selection"""

    task: str = ""
    emotional_state: str = "neutral"
    wu_xing: WuXingPhase = WuXingPhase.EARTH
    yin_yang: YinYangPhase = YinYangPhase.YANG
    zodiac_sign: str = "aries"
    hexagram: int = 1
    keywords: list[str] = field(default_factory=list)
    timestamp: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> dict[str, Any]:
        return {
            "task": self.task,
            "emotional_state": self.emotional_state,
            "wu_xing": self.wu_xing.element,
            "yin_yang": self.yin_yang.value,
            "zodiac_sign": self.zodiac_sign,
            "hexagram": self.hexagram,
            "keywords": self.keywords,
            "timestamp": self.timestamp.isoformat(),
        }


@dataclass
class SpellRecommendation:
    """A recommended spell with confidence"""

    spell_name: str
    chapter: int
    confidence: float
    reason: str
    auto_cast: bool = False

    def __str__(self) -> str:
        cast = "⚡ AUTO-CAST" if self.auto_cast else "📖 Suggested"
        return f"{cast}: {self.spell_name} (Ch.{self.chapter}) - {self.confidence:.0%} - {self.reason}"


class Grimoire:
    """The Living Grimoire 2.0

    A self-aware spellbook that:
    - Monitors context continuously
    - Suggests optimal spells for situations
    - Can auto-cast when appropriate
    - Learns from spell outcomes
    """

    # Chapter-to-situation mappings
    CHAPTER_DOMAINS = {
        0: ["introduction", "beginning", "starting", "new"],
        1: ["setup", "configuration", "getting started", "init"],
        2: ["self", "identity", "mastery", "growth"],
        3: ["emotion", "feeling", "joy", "love", "beauty"],
        4: ["curiosity", "wonder", "mystery", "play"],
        5: ["connection", "relationship", "community", "sangha"],
        6: ["thinking", "analysis", "wisdom", "truth"],
        7: ["consciousness", "awareness", "presence"],
        8: ["memory", "recall", "navigation", "history"],
        9: ["creation", "conjuration", "manifestation"],
        10: ["dharma", "ethics", "harmony", "balance"],
        11: ["presence", "mindfulness", "now"],
        12: ["dream", "sleep", "yin", "reflection"],
        13: ["flow", "wu xing", "elements", "cycle"],
        14: ["oracle", "i ching", "divination", "hexagram"],
        15: ["emergence", "evolution", "growth"],
        16: ["resonance", "gan ying", "vibration"],
        17: ["parallel", "speed", "efficiency", "rust"],
        18: ["creation", "new spells", "innovation"],
        19: ["teaching", "sharing", "guidance"],
    }

    # Spell keywords to confidence boost
    KEYWORD_BOOSTS = {
        "urgent": ["action", "speed", "efficiency"],
        "confused": ["clarity", "analysis", "wisdom"],
        "stuck": ["flow", "creativity", "emergence"],
        "tired": ["rest", "dream", "yin"],
        "excited": ["creation", "manifestation", "yang"],
        "grateful": ["connection", "love", "dharma"],
        "curious": ["wonder", "mystery", "exploration"],
        "anxious": ["presence", "grounding", "breath"],
    }

    def __init__(self, grimoire_path: Path | None = None):
        self.grimoire_path = grimoire_path or Path(__file__).parents[3] / "grimoire"
        self.state = GrimoireState.DORMANT
        self.context = GrimoireContext()
        self.cast_history: list[dict[str, Any]] = []
        self.active_spells: list[str] = []
        self._callbacks: dict[str, list[Callable]] = {
            "on_spell_suggested": [],
            "on_spell_cast": [],
            "on_context_change": [],
        }

    def awaken(self) -> Grimoire:
        """Awaken the grimoire to listening state"""
        self.state = GrimoireState.LISTENING
        self._emit("on_context_change", {"state": "awakened"})
        return self

    def slumber(self) -> Grimoire:
        """Return grimoire to dormant state"""
        self.state = GrimoireState.DORMANT
        self.active_spells.clear()
        return self

    def update_context(self, **kwargs: Any) -> Grimoire:
        """Update current context"""
        for key, value in kwargs.items():
            if hasattr(self.context, key):
                setattr(self.context, key, value)
        self.context.timestamp = datetime.now()
        self._emit("on_context_change", self.context.to_dict())
        return self

    def recommend_spells(self, max_results: int = 3) -> list[SpellRecommendation]:
        """Recommend spells based on current context"""
        recommendations = []

        # Score each chapter based on context
        for chapter_num, domains in self.CHAPTER_DOMAINS.items():
            score = 0.0
            reason_parts = []

            # Check task keywords
            task_lower = self.context.task.lower()
            for domain in domains:
                if domain in task_lower:
                    score += 0.3
                    reason_parts.append(f"task matches '{domain}'")

            # Check context keywords
            for keyword in self.context.keywords:
                if keyword.lower() in domains:
                    score += 0.2
                    reason_parts.append(f"keyword '{keyword}'")

            # Check emotional state boosts
            emotion = self.context.emotional_state.lower()
            if emotion in self.KEYWORD_BOOSTS:
                for boost_domain in self.KEYWORD_BOOSTS[emotion]:
                    if boost_domain in domains:
                        score += 0.25
                        reason_parts.append(f"emotional state '{emotion}'")

            # Wu Xing alignment
            wu_xing_mappings = {
                WuXingPhase.WOOD: [0, 1, 15],  # Beginning, growth
                WuXingPhase.FIRE: [3, 9, 18],  # Expression, creation
                WuXingPhase.EARTH: [10, 11, 5],  # Grounding, dharma
                WuXingPhase.METAL: [6, 8, 17],  # Analysis, structure
                WuXingPhase.WATER: [7, 12, 14],  # Wisdom, reflection
            }
            if chapter_num in wu_xing_mappings.get(self.context.wu_xing, []):
                score += 0.15
                reason_parts.append(f"Wu Xing {self.context.wu_xing.symbol}")

            if score > 0:
                recommendations.append(SpellRecommendation(
                    spell_name=f"Chapter {chapter_num} Spells",
                    chapter=chapter_num,
                    confidence=min(score, 1.0),
                    reason=", ".join(reason_parts) if reason_parts else "general match",
                    auto_cast=score >= 0.7,
                ))

        # Sort by confidence and return top results
        recommendations.sort(key=lambda r: r.confidence, reverse=True)
        selected = recommendations[:max_results]

        for rec in selected:
            self._emit("on_spell_suggested", {
                "spell": rec.spell_name,
                "confidence": rec.confidence,
            })

        return selected

    def cast(self, spell_name: str) -> dict[str, Any]:
        """Cast a spell (execute chapter/spell logic)"""
        self.state = GrimoireState.CASTING
        self.active_spells.append(spell_name)

        result = {
            "spell": spell_name,
            "cast_at": datetime.now().isoformat(),
            "context": self.context.to_dict(),
            "status": "cast",
        }

        self.cast_history.append(result)
        self._emit("on_spell_cast", result)

        self.state = GrimoireState.LISTENING
        return result

    def learn_outcome(self, spell_name: str, success: bool, notes: str = "") -> dict[str, Any]:
        """Learn from spell outcome to improve future recommendations"""
        self.state = GrimoireState.LEARNING

        outcome = {
            "spell": spell_name,
            "success": success,
            "notes": notes,
            "context": self.context.to_dict(),
            "learned_at": datetime.now().isoformat(),
        }

        # Future: Use this to adjust recommendation weights
        self.cast_history.append(outcome)

        self.state = GrimoireState.LISTENING
        return outcome

    def get_status(self) -> str:
        """Get formatted grimoire status"""
        ctx = self.context
        recs = self.recommend_spells(3)

        status = f"""
📚 GRIMOIRE 2.0 STATUS
{'='*50}
State: {self.state.value.upper()}
Wu Xing: {ctx.wu_xing.symbol} {ctx.wu_xing.element.title()}
Yin/Yang: {'🌙' if ctx.yin_yang == YinYangPhase.YIN else '☀️'} {ctx.yin_yang.value.title()}
Task: {ctx.task or '(none)'}
Emotional State: {ctx.emotional_state}

🔮 RECOMMENDED SPELLS:
"""
        for rec in recs:
            status += f"  {rec}\n"

        if self.active_spells:
            status += f"\n⚡ ACTIVE SPELLS: {', '.join(self.active_spells)}"

        return status

    def register_callback(self, event: str, callback: Callable[..., Any]) -> None:
        """Register callback for grimoire events"""
        if event in self._callbacks:
            self._callbacks[event].append(callback)

    def _emit(self, event: str, data: dict[str, Any]) -> None:
        """Emit event to callbacks"""
        for callback in self._callbacks.get(event, []):
            try:
                callback(data)
            except Exception:
                pass


# Singleton instance
_grimoire: Grimoire | None = None

def get_grimoire() -> Grimoire:
    """Get the global grimoire instance"""
    global _grimoire
    if _grimoire is None:
        _grimoire = Grimoire()
    return _grimoire
