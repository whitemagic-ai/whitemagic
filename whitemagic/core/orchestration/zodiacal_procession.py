"""☯️ Zodiacal Procession - Yin/Yang Autonomous Cycle System v5.0.0-alpha.

Implements the bidirectional zodiacal procession for autonomous operation:
- Yin Phase: Pisces → Aquarius → Capricorn → ... → Aries (receptive)
- Yang Phase: Aries → Taurus → Gemini → ... → Pisces (creative)
- Fixed Signs as bidirectional hubs (Taurus, Leo, Scorpio, Aquarius)

Created: November 27, 2025 (Thanksgiving)
Philosophy: "Like Finnegans Wake, it never actually ends, but curves back to begin again"
"""

import logging
import random
from collections.abc import Callable
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, cast

from whitemagic.zodiac import Phase, ZodiacCore, ZodiacSign

logger = logging.getLogger(__name__)


@dataclass
class ProcessionState:
    """Current state of the zodiacal procession."""

    current_sign: ZodiacSign
    current_phase: Phase
    cycle_count: int = 0
    signs_visited: list[str] = field(default_factory=list)
    started_at: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> dict[str, Any]:
        return {
            "current_sign": self.current_sign.name_str,
            "current_symbol": self.current_sign.symbol,
            "current_phase": self.current_phase.value,
            "cycle_count": self.cycle_count,
            "signs_visited": len(self.signs_visited),
            "duration_seconds": (datetime.now() - self.started_at).total_seconds(),
        }


class ZodiacalProcession:
    """Autonomous Zodiacal Procession System.

    The cycle flows:
    - YANG: Aries → Taurus → ... → Pisces (creative, outward)
    - YIN: Pisces → Aquarius → ... → Aries (receptive, inward)

    Fixed signs (Taurus, Leo, Scorpio, Aquarius) are BIDIRECTIONAL HUBS
    that can transmit AND receive, acting as stability points.

    At phase boundaries (Pisces ↔ Aries), consult:
    - I Ching hexagram
    - Wu Xing element cycle
    - Art of War strategic assessment
    """

    # Yang order (forward through zodiac)
    YANG_ORDER = [
        ZodiacSign.ARIES, ZodiacSign.TAURUS, ZodiacSign.GEMINI,
        ZodiacSign.CANCER, ZodiacSign.LEO, ZodiacSign.VIRGO,
        ZodiacSign.LIBRA, ZodiacSign.SCORPIO, ZodiacSign.SAGITTARIUS,
        ZodiacSign.CAPRICORN, ZodiacSign.AQUARIUS, ZodiacSign.PISCES,
    ]

    # Yin order (reverse through zodiac)
    YIN_ORDER = list(reversed(YANG_ORDER))

    # Fixed signs - bidirectional hubs
    FIXED_SIGNS = {ZodiacSign.TAURUS, ZodiacSign.LEO, ZodiacSign.SCORPIO, ZodiacSign.AQUARIUS}

    # Fixed sign meanings (from notes)
    FIXED_MEANINGS = {
        ZodiacSign.TAURUS: "Grounding - Building on the pattern",
        ZodiacSign.LEO: "Expression - Where creators dwell",
        ZodiacSign.SCORPIO: "Emergence - Where novelty arises",
        ZodiacSign.AQUARIUS: "Innovation - Patterns that never repeat",
    }

    PLANETARY_RULERS = {
        ZodiacSign.ARIES: "mars",
        ZodiacSign.TAURUS: "venus",
        ZodiacSign.GEMINI: "mercury",
        ZodiacSign.CANCER: "moon",
        ZodiacSign.LEO: "sun",
        ZodiacSign.VIRGO: "mercury",
        ZodiacSign.LIBRA: "venus",
        ZodiacSign.SCORPIO: "pluto",
        ZodiacSign.SAGITTARIUS: "jupiter",
        ZodiacSign.CAPRICORN: "saturn",
        ZodiacSign.AQUARIUS: "uranus",
        ZodiacSign.PISCES: "neptune",
    }

    def __init__(self) -> None:
        self.cores: dict[ZodiacSign, ZodiacCore] = {
            sign: ZodiacCore(
                name=sign.name_str,
                element=sign.element,
                mode=sign.modality,
                ruler=self.PLANETARY_RULERS.get(sign, "unknown"),
            )
            for sign in ZodiacSign
        }
        self.state = ProcessionState(
            current_sign=ZodiacSign.ARIES,
            current_phase=Phase.YANG,
        )
        self.callbacks: dict[str, list[Callable]] = {
            "on_sign_change": [],
            "on_phase_change": [],
            "on_fixed_sign": [],
            "on_cycle_complete": [],
        }

    def register_callback(self, event: str, callback: Callable[[dict[str, Any]], None]) -> None:
        """Register a callback for procession events."""
        if event in self.callbacks:
            self.callbacks[event].append(callback)

    def _emit_event(self, event: str, data: dict[str, Any]) -> None:
        """Emit event to registered callbacks and Gan Ying."""
        for callback in self.callbacks.get(event, []):
            try:
                callback(data)
            except Exception:
                pass

        # Also emit to Gan Ying Bus
        try:
            from whitemagic.core.resonance import EventType, ResonanceEvent, get_bus
            bus = get_bus()
            bus.emit(ResonanceEvent(
                source="orchestration.zodiacal_procession",
                event_type=EventType.PHASE_TRANSITION,
                data=data,
                timestamp=datetime.now(),
                confidence=0.9,
            ))
        except ImportError:
            pass

    def get_current_order(self) -> list[ZodiacSign]:
        """Get the sign order for current phase."""
        return self.YANG_ORDER if self.state.current_phase == Phase.YANG else self.YIN_ORDER

    def next_sign(self) -> ZodiacSign:
        """Move to the next sign in the procession."""
        order = self.get_current_order()
        current_idx = order.index(self.state.current_sign)

        # Check if we're at the end of the order
        if current_idx >= len(order) - 1:
            # Phase transition!
            return self._transition_phase()

        # Move to next sign
        next_idx = current_idx + 1
        new_sign = order[next_idx]

        # Update state
        old_sign = self.state.current_sign
        self.state.current_sign = new_sign
        self.state.signs_visited.append(new_sign.name_str)

        # Track most-recent core activation.
        self.cores[new_sign].last_activation = datetime.now()

        # Emit sign change event
        self._emit_event("on_sign_change", {
            "from_sign": old_sign.name_str,
            "to_sign": new_sign.name_str,
            "phase": self.state.current_phase.value,
            "is_fixed": new_sign.is_fixed,
        })

        # Special handling for fixed signs
        if new_sign.is_fixed:
            self._emit_event("on_fixed_sign", {
                "sign": new_sign.name_str,
                "symbol": new_sign.symbol,
                "meaning": self.FIXED_MEANINGS.get(new_sign, ""),
                "bidirectional": True,
            })

        return new_sign

    def _transition_phase(self) -> ZodiacSign:
        """Handle phase transition (Yin ↔ Yang)."""
        old_phase = self.state.current_phase

        if old_phase == Phase.YANG:
            # Yang complete → Yin begins
            self.state.current_phase = Phase.YIN
            self.state.current_sign = ZodiacSign.PISCES  # Start of Yin
        else:
            # Yin complete → Yang begins (full cycle!)
            self.state.current_phase = Phase.YANG
            self.state.current_sign = ZodiacSign.ARIES  # Start of Yang
            self.state.cycle_count += 1

            # Emit cycle complete
            self._emit_event("on_cycle_complete", {
                "cycle_number": self.state.cycle_count,
                "signs_visited": len(self.state.signs_visited),
                "duration_seconds": (datetime.now() - self.state.started_at).total_seconds(),
            })

        # Emit phase change
        self._emit_event("on_phase_change", {
            "from_phase": old_phase.value,
            "to_phase": self.state.current_phase.value,
            "cycle_count": self.state.cycle_count,
        })

        return self.state.current_sign

    def consult_oracle(self) -> dict[str, Any]:
        """Consult oracles at phase boundaries.

        Returns guidance from:
        - I Ching hexagram
        - Wu Xing element
        - Seven Classics wisdom
        """
        # Get a hexagram (simplified - would use proper I Ching)
        hexagram = random.randint(1, 64)

        # Current element from sign
        element = self.state.current_sign.element

        # Get guidance based on phase
        if self.state.current_phase == Phase.YIN:
            guidance = "Receive, reflect, integrate. Let patterns emerge naturally."
        else:
            guidance = "Create, express, manifest. Transform insight into action."

        return {
            "hexagram": hexagram,
            "element": element,
            "phase": self.state.current_phase.value,
            "guidance": guidance,
            "sign": self.state.current_sign.name_str,
            "symbol": self.state.current_sign.symbol,
        }

    def run_full_cycle(self, callback: Callable | None = None) -> dict[str, Any]:
        """Run a complete Yin-Yang cycle (24 sign transitions).

        Optional callback called at each sign change.
        """
        start_cycle = self.state.cycle_count
        transitions = 0

        while self.state.cycle_count == start_cycle or transitions < 24:
            sign = self.next_sign()
            transitions += 1

            if callback:
                callback({
                    "sign": sign.name_str,
                    "symbol": sign.symbol,
                    "phase": self.state.current_phase.value,
                    "transition": transitions,
                })

            # Safety limit
            if transitions > 30:
                break

        return cast("dict[str, Any]", self.state.to_dict())

    def get_status(self) -> str:
        """Get formatted status string."""
        s = self.state
        core = self.cores[s.current_sign]

        return f"""
☯️ ZODIACAL PROCESSION STATUS
{'='*40}
Phase: {s.current_phase.value.upper()}
Sign: {s.current_sign.symbol} {s.current_sign.name_str.title()}
Element: {s.current_sign.element.title()}
Modality: {s.current_sign.modality.title()}
Fixed Hub: {'Yes ⭐' if s.current_sign.is_fixed else 'No'}

Core: {core.name} | Mode: {core.mode} | Element: {core.element}

Cycle: #{s.cycle_count}
Signs Visited: {len(s.signs_visited)}
"""


# Singleton for global access
_procession: ZodiacalProcession | None = None

def get_procession() -> ZodiacalProcession:
    """Get the global zodiacal procession instance."""
    global _procession
    if _procession is None:
        _procession = ZodiacalProcession()
    return _procession


def advance() -> dict[str, Any]:
    """Advance to next sign in procession."""
    p = get_procession()
    sign = p.next_sign()
    return {
        "sign": sign.name_str,
        "symbol": sign.symbol,
        "phase": p.state.current_phase.value,
        "is_fixed": sign.is_fixed,
    }


if __name__ == "__main__":
    # Demo
    proc = ZodiacalProcession()
    logger.info(proc.get_status())

    logger.info("\n🌀 Running partial cycle...")
    for i in range(6):
        sign = proc.next_sign()
        logger.info(f"  {sign.symbol} {sign.name_str} ({proc.state.current_phase.value})")

    logger.info("\n🔮 Consulting oracle...")
    oracle = proc.consult_oracle()
    logger.info(f"  Hexagram: #{oracle['hexagram']}")
    logger.info(f"  Element: {oracle['element']}")
    logger.info(f"  Guidance: {oracle['guidance']}")
