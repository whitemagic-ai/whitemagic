"""Grimoire 2.0 - Auto-Caster

Monitors context and automatically casts appropriate spells.
"""

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any

from .core import get_grimoire
from .spells import Spell, SpellOutcome, get_spell_book


class CastMode(Enum):
    """How aggressively to auto-cast"""

    SUGGEST_ONLY = "suggest"     # Only suggest, never auto-cast
    ASK_FIRST = "ask"            # Ask before casting
    HIGH_CONFIDENCE = "high"     # Auto-cast if >80% confidence
    ALWAYS = "always"            # Auto-cast any match


@dataclass
class CastContext:
    """Context for a spell cast"""

    task: str
    emotional_state: str = "neutral"
    wu_xing: str = "earth"
    yin_yang: str = "yang"
    urgency: float = 0.5
    keywords: list[str] = field(default_factory=list)

    def to_dict(self) -> dict[str, Any]:
        return {
            "task": self.task,
            "emotional_state": self.emotional_state,
            "wu_xing": self.wu_xing,
            "yin_yang": self.yin_yang,
            "urgency": self.urgency,
            "keywords": self.keywords,
        }


@dataclass
class CastResult:
    """Result of an auto-cast attempt"""

    spell: Spell | None
    outcome: SpellOutcome
    confidence: float
    auto_cast: bool
    reason: str
    timestamp: datetime = field(default_factory=datetime.now)


class AutoCaster:
    """Automatic spell casting based on context.

    Monitors the current situation and either:
    - Suggests appropriate spells
    - Auto-casts when confidence is high enough
    """

    def __init__(self, mode: CastMode = CastMode.HIGH_CONFIDENCE) -> None:
        self.mode = mode
        self.grimoire = get_grimoire()
        self.spell_book = get_spell_book()
        self.cast_history: list[CastResult] = []
        self.active = False

    def activate(self) -> Any:
        """Activate auto-casting"""
        self.active = True
        self.grimoire.awaken()
        return self

    def deactivate(self) -> Any:
        """Deactivate auto-casting"""
        self.active = False
        self.grimoire.slumber()
        return self

    def process_context(self, context: CastContext) -> list[CastResult]:
        """Process context and potentially cast spells"""
        if not self.active:
            return []

        results = []

        # Update grimoire context
        self.grimoire.update_context(
            task=context.task,
            emotional_state=context.emotional_state,
            keywords=context.keywords,
        )

        # Find matching spells
        matching_spells = self.spell_book.find_for_context(
            context.task,
            context.emotional_state,
        )

        for spell in matching_spells[:3]:  # Top 3 matches
            confidence = self._calculate_confidence(spell, context)
            should_cast = self._should_auto_cast(confidence)

            if should_cast:
                outcome = spell.cast(context.to_dict())
                auto_cast = True
            else:
                outcome = SpellOutcome.DEFERRED
                auto_cast = False

            result = CastResult(
                spell=spell,
                outcome=outcome,
                confidence=confidence,
                auto_cast=auto_cast,
                reason=f"Matched keywords for '{context.task}'",
            )

            results.append(result)
            self.cast_history.append(result)

        return results

    def _calculate_confidence(self, spell: Spell, context: CastContext) -> float:
        """Calculate confidence for a spell match, modulated by Emotion/Drive Core."""
        score = 0.3  # Base score for any match

        # Keyword matches
        task_lower = context.task.lower()
        for keyword in spell.keywords:
            if keyword in task_lower:
                score += 0.2

        # Wu Xing alignment
        if spell.wu_xing_affinity == context.wu_xing:
            score += 0.15

        # Yin/Yang alignment
        if spell.yin_yang == context.yin_yang:
            score += 0.1

        # Urgency factor
        if context.urgency > 0.7:
            score += 0.1

        # Historical success
        if spell.times_cast > 0:
            score += 0.05 * spell.success_rate

        # G3 Synthesis: Emotion/Drive Core modulates spell selection
        score += self._drive_bias(spell, task_lower)

        return min(score, 1.0)

    def _drive_bias(self, spell: Spell, task_lower: str) -> float:
        """Compute drive-based confidence bias from the Emotion/Drive Core."""
        try:
            import whitemagic.core.intelligence.emotion_drive as emotion_drive

            get_emotion_drive = getattr(emotion_drive, "get_emotion_drive", None)
            if get_emotion_drive is None:
                return 0.0

            drives = get_emotion_drive()
            snap = drives.snapshot()

            bias = 0.0
            # High curiosity → boost exploration/discovery/search spells
            if snap.get("curiosity", 0.5) > 0.7:
                if any(k in task_lower for k in ("explore", "search", "discover", "pattern", "wonder")):
                    bias += 0.1
            # High caution → boost safety/audit/check spells
            if snap.get("caution", 0.5) > 0.7:
                if any(k in task_lower for k in ("check", "audit", "verify", "protect", "safe")):
                    bias += 0.1
            # Low energy → boost maintenance/consolidation spells
            if snap.get("energy", 0.5) < 0.3:
                if any(k in task_lower for k in ("consolidat", "maintenance", "optimize", "clean")):
                    bias += 0.1
            # High satisfaction → boost creative/synthesis spells
            if snap.get("satisfaction", 0.5) > 0.7:
                if any(k in task_lower for k in ("create", "synth", "compose", "dream")):
                    bias += 0.1

            return bias
        except Exception:
            return 0.0

    def _should_auto_cast(self, confidence: float) -> bool:
        """Determine if we should auto-cast based on mode and confidence"""
        if self.mode == CastMode.ALWAYS:
            return True
        if self.mode == CastMode.HIGH_CONFIDENCE:
            return confidence >= 0.8
        if self.mode == CastMode.ASK_FIRST:
            return False  # Would need user input
        # SUGGEST_ONLY
        return False

    def suggest(self, task: str) -> list[str]:
        """Get spell suggestions for a task"""
        CastContext(task=task)
        spells = self.spell_book.find_for_context(task)
        return [f"{spell.name}: {spell.description}" for spell in spells[:5]]

    def quick_cast(self, spell_name: str, context: CastContext | None = None) -> CastResult:
        """Quickly cast a spell by name"""
        spell = self.spell_book.find_spell(spell_name)

        if not spell:
            return CastResult(
                spell=None,
                outcome=SpellOutcome.FAILED,
                confidence=0.0,
                auto_cast=False,
                reason=f"Spell '{spell_name}' not found",
            )

        ctx = context or CastContext(task="quick cast")
        outcome = spell.cast(ctx.to_dict())

        return CastResult(
            spell=spell,
            outcome=outcome,
            confidence=1.0,
            auto_cast=False,
            reason="Manual quick cast",
        )

    def get_status(self) -> str:
        """Get auto-caster status"""
        status = f"""
⚡ AUTO-CASTER STATUS
{'='*50}
Active: {'✅ Yes' if self.active else '❌ No'}
Mode: {self.mode.value}
Spells Available: {len(self.spell_book)}
Spells Cast: {len(self.cast_history)}
"""

        if self.cast_history:
            status += "\nRecent Casts:\n"
            for result in self.cast_history[-5:]:
                if result.spell:
                    status += f"  ✨ {result.spell.name} - {result.outcome.value}\n"

        return status


# Convenience function
def auto_suggest(task: str) -> list[str]:
    """Get spell suggestions for a task"""
    caster = AutoCaster(mode=CastMode.SUGGEST_ONLY)
    caster.activate()
    suggestions = caster.suggest(task)
    caster.deactivate()
    return suggestions
