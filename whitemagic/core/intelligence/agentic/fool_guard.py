# mypy: disable-error-code=no-untyped-def
"""Fool's Guard (Ralph Wiggum Protection) + Dare-to-Die Corps
=============================================================

"I'm in danger!" - Ralph Wiggum
"I'm a unit of measure!" - Ralph Wiggum
"It's not that hard to build a coding agent, it's 300 lines of code
 running in a loop." — Geoff Huntley

Inspired by the 'Ralph Loop' discovery (Geoff Huntley, late 2025).
The Fool's Guard serves two purposes:

1. **Anti-Rigidity** — Detects when the system is stuck in over-optimized
   logical loops and injects necessary chaos to break free.

2. **Dare-to-Die Corps** — Deploys stateless "Pu" (朴, Uncarved Block)
   clones that embody the Taoist principle of beginner's mind:
   - Zero context (fresh malloc every time)
   - Zero memory of previous attempts
   - Zero fear of failure (they expect to die)
   - Pure, simple execution: read → attempt → succeed or die

Philosophy (Tao Te Ching, Ch. 28):
  "Know the masculine, keep to the feminine...
   Return to the state of the Uncarved Block (Pu)."

The Ralph Clone is NOT a fool — it is a Sage of the Immediate Moment.
It has no self to preserve, no history to confuse it, no ego to protect.
It enters the stream, changes the water, and vanishes.

Military Doctrine:
  - Dare-to-Die Corps (敢死队, Gǎn Sǐ Duì) — Historical Chinese shock troops
  - Used when smart agents are stuck (The Empty Fort Strategy, 空城計)
  - Performance or Death (Reset) — Legalist discipline

Usage:
    from whitemagic.core.intelligence.agentic.fool_guard import (
        get_fool_guard, deploy_dare_to_die, ralph_wiggum_maneuver,
    )

    # Anti-rigidity monitoring
    guard = get_fool_guard()
    guard.process_event(event)

    # Dare-to-Die deployment
    result = await deploy_dare_to_die(
        target_file="whitemagic/core/memory/consolidation.py",
        error_log="TypeError: unsupported operand type(s) for +: 'int' and 'NoneType'",
        max_attempts=10,
    )

    # Ralph Wiggum maneuver (single stateless attempt)
    result = await ralph_wiggum_maneuver(
        mission="Fix the null guard in consolidation.py line 310",
    )
"""

from __future__ import annotations

import logging
import random
import time
import uuid
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Ralph Wisdom — Chaos injection phrases
# ---------------------------------------------------------------------------

RALPH_WISDOM = [
    "I'm in danger!",
    "My cat's breath smells like cat food.",
    "I'm a unit of measure!",
    "I'm Idaho!",
    "The doctor said I wouldn't have so many nosebleeds if I kept my finger out of there.",
    "I'm a furniture!",
    "Me fail English? That's unpossible!",
    "I heard a blue jay! It said 'tweet tweet'!",
    "I'm special!",
    "Sleep is where I'm a Viking!",
    "I bent my Wookiee.",
    "When I grow up, I'm going to Bovine University!",
    "That's where I saw the Leprechaun. He tells me to burn things.",
    "I found a moonrock in my nose!",
    "Hi, Super Nintendo Chalmers!",
]

# Taoist wisdom — injected alongside Ralph for depth
PU_WISDOM = [
    "The Tao that can be spoken is not the eternal Tao.",
    "The Uncarved Block (Pu): simplicity before complexity.",
    "Empty your mind, be formless, shapeless — like water.",
    "He who knows does not speak. He who speaks does not know.",
    "The softest thing in the universe overcomes the hardest.",
    "In the pursuit of learning, every day something is acquired. In the pursuit of Tao, every day something is dropped.",
    "A journey of a thousand miles begins with a single step.",
    "Nature does not hurry, yet everything is accomplished.",
]


# ---------------------------------------------------------------------------
# Dare-to-Die Clone Result
# ---------------------------------------------------------------------------

@dataclass
class DareToResult:
    """Result from a single Dare-to-Die clone attempt."""

    clone_id: str
    attempt: int
    success: bool
    output: str
    error: str | None = None
    duration_ms: float = 0.0
    strategy: str = "stateless_burn"
    wisdom_injected: str = ""


@dataclass
class DareToCorpsResult:
    """Aggregate result from a Dare-to-Die Corps deployment."""

    total_attempts: int
    successful: int
    failed: int
    best_result: DareToResult | None
    all_results: list[DareToResult] = field(default_factory=list)
    total_duration_ms: float = 0.0
    verdict: str = ""  # "victory", "pyrrhic_victory", "defeat"

    def to_dict(self) -> dict[str, Any]:
        return {
            "total_attempts": self.total_attempts,
            "successful": self.successful,
            "failed": self.failed,
            "verdict": self.verdict,
            "total_duration_ms": round(self.total_duration_ms, 2),
            "best_result": {
                "clone_id": self.best_result.clone_id,
                "output": self.best_result.output[:500],
                "duration_ms": self.best_result.duration_ms,
            } if self.best_result else None,
        }


# ---------------------------------------------------------------------------
# Fool's Guard — Anti-Rigidity Monitor
# ---------------------------------------------------------------------------

class FoolGuard:
    """Monitors system resonance and repetition.

    Two modes of operation:
    1. **Passive**: Listens to Gan Ying bus events and detects rigidity
    2. **Active**: Deploys Dare-to-Die corps when smart agents are stuck

    The rigidity detector uses variance analysis over a sliding window.
    When resonance is static at high levels (groupthink), it injects chaos.
    """

    def __init__(self, threshold: float = 0.98, window_size: int = 10):
        self.threshold = threshold
        self.window_size = window_size
        self.resonance_history: list[float] = []
        self.last_ralph_event: datetime | None = None
        self._ralph_events_total = 0
        self._dare_to_die_deployments = 0
        self._dare_to_die_victories = 0

    def check_rigidity(self, current_resonance: float) -> bool:
        """Add resonance to history and check if it's too static at a high level."""
        self.resonance_history.append(current_resonance)
        if len(self.resonance_history) > self.window_size:
            self.resonance_history.pop(0)

        if len(self.resonance_history) < self.window_size:
            return False

        avg = sum(self.resonance_history) / self.window_size
        variance = sum((x - avg)**2 for x in self.resonance_history) / self.window_size

        if avg > self.threshold and variance < 0.001:
            logger.warning("RIGIDITY DETECTED: System resonance too static at high power.")
            return True
        return False

    def trigger_ralph_event(self):
        """Inject a random piece of Ralph + Pu wisdom into the Gan Ying bus."""
        ralph = random.choice(RALPH_WISDOM)
        pu = random.choice(PU_WISDOM)
        combined = f"Ralph says: '{ralph}' | The Tao says: '{pu}'"
        logger.info(f"FOOL'S GUARD ACTIVATED: {combined}")

        try:
            from whitemagic.core.resonance.gan_ying_enhanced import (
                EventType,
                ResonanceEvent,
                get_bus,
            )
            bus = get_bus()
            bus.emit(ResonanceEvent(
                source="fool_guard",
                event_type=EventType.EMERGENCE_DETECTED,
                data={
                    "ralph_wisdom": ralph,
                    "pu_wisdom": pu,
                    "message": "Breaking logical loop with necessary nonsense + ancient wisdom.",
                    "resonance_shift": -0.05,
                    "doctrine": "uncarved_block",
                },
            ))
        except Exception:
            pass

        self.last_ralph_event = datetime.now()
        self._ralph_events_total += 1

    def process_event(self, event: Any):
        """Listen to bus events and track resonance."""
        data = getattr(event, "data", None) or {}
        res = data.get("resonance", 0.0) if isinstance(data, dict) else 0.0
        if self.check_rigidity(res):
            self.trigger_ralph_event()

    async def deploy_dare_to_die(
        self,
        mission: str,
        target_file: str | None = None,
        error_log: str | None = None,
        max_attempts: int = 10,
        clone_fn: Any = None,
    ) -> DareToCorpsResult:
        """Deploy a Dare-to-Die Corps against a problem.

        Each clone is born with ZERO context. It knows only:
          1. The mission (what to fix)
          2. The target file (where)
          3. The error (why it failed last time)

        This is the "300 lines of code in a loop" philosophy.
        Fresh malloc. No history. No fear.

        Args:
            mission: What needs to be done (plain text)
            target_file: Optional file path to focus on
            error_log: Optional error from last failed attempt
            max_attempts: Maximum number of clone lives to spend
            clone_fn: Optional async callable(mission_order: str) -> str.
                      If None, uses simulated execution.

        Returns:
            DareToCorpsResult with aggregate outcome
        """
        self._dare_to_die_deployments += 1
        results: list[DareToResult] = []
        t0 = time.time()

        current_error = error_log
        for attempt in range(max_attempts):
            clone_id = f"ralph_{uuid.uuid4().hex[:8]}"

            # Build the mission order — brutally simple, Pu style
            mission_order = self._build_mission_order(mission, target_file, current_error)

            t1 = time.time()
            try:
                if clone_fn is not None:
                    output = await clone_fn(mission_order)
                else:
                    output = self._simulated_ralph_attempt(mission_order, attempt)

                duration_ms = (time.time() - t1) * 1000
                success = "ERROR" not in str(output).upper() and len(str(output)) > 10

                result = DareToResult(
                    clone_id=clone_id,
                    attempt=attempt,
                    success=success,
                    output=str(output),
                    duration_ms=duration_ms,
                    wisdom_injected=random.choice(PU_WISDOM) if attempt % 3 == 0 else "",
                )
            except Exception as e:
                duration_ms = (time.time() - t1) * 1000
                result = DareToResult(
                    clone_id=clone_id,
                    attempt=attempt,
                    success=False,
                    output="",
                    error=str(e),
                    duration_ms=duration_ms,
                )
                current_error = str(e)

            results.append(result)

            if result.success:
                logger.info(f"Dare-to-Die clone {clone_id} SUCCEEDED on attempt {attempt + 1}")
                break
            else:
                # Clone dies. Next clone gets only the new error.
                current_error = result.error or result.output[:200]
                logger.debug(f"Clone {clone_id} died (attempt {attempt + 1}/{max_attempts})")

        total_duration = (time.time() - t0) * 1000
        successful = [r for r in results if r.success]
        best = successful[0] if successful else None

        if best:
            verdict = "victory"
            self._dare_to_die_victories += 1
        elif len(results) > max_attempts * 0.7:
            verdict = "defeat"
        else:
            verdict = "pyrrhic_victory"

        return DareToCorpsResult(
            total_attempts=len(results),
            successful=len(successful),
            failed=len(results) - len(successful),
            best_result=best,
            all_results=results,
            total_duration_ms=total_duration,
            verdict=verdict,
        )

    def _build_mission_order(
        self,
        mission: str,
        target_file: str | None,
        error_log: str | None,
    ) -> str:
        """Build a minimal mission order for a Ralph clone.

        The order is intentionally simple — this is the "300 lines" philosophy.
        No system prompt. No personality. No history. Just the raw objective.
        """
        parts = [f"MISSION: {mission}"]
        if target_file:
            parts.append(f"TARGET FILE: {target_file}")
        if error_log:
            parts.append(f"LAST ERROR: {error_log}")
        parts.append("CONSTRAINT: Do not ask questions. Do not explain. Return the fix only.")
        return "\n".join(parts)

    def _simulated_ralph_attempt(self, mission_order: str, attempt: int) -> str:
        """Simulated Ralph clone execution for testing.

        In production, replace with actual LLM call via clone_fn parameter.
        """
        # Simulate increasing likelihood of success with each attempt
        # (each clone learns from the error message of the previous one)
        success_probability = min(0.9, 0.2 + attempt * 0.1)
        if random.random() < success_probability:
            return f"[Ralph Clone] Fix applied for: {mission_order[:80]}..."
        return f"[Ralph Clone] ERROR: Attempt {attempt} failed. {random.choice(RALPH_WISDOM)}"

    def get_stats(self) -> dict[str, Any]:
        """Get Fool's Guard statistics."""
        return {
            "ralph_events_total": self._ralph_events_total,
            "dare_to_die_deployments": self._dare_to_die_deployments,
            "dare_to_die_victories": self._dare_to_die_victories,
            "dare_to_die_win_rate": (
                round(self._dare_to_die_victories / max(1, self._dare_to_die_deployments), 2)
            ),
            "last_ralph_event": self.last_ralph_event.isoformat() if self.last_ralph_event else None,
            "rigidity_threshold": self.threshold,
            "window_size": self.window_size,
            "resonance_history_len": len(self.resonance_history),
        }


# ---------------------------------------------------------------------------
# Convenience Functions
# ---------------------------------------------------------------------------

async def deploy_dare_to_die(
    mission: str,
    target_file: str | None = None,
    error_log: str | None = None,
    max_attempts: int = 10,
    clone_fn: Any = None,
) -> DareToCorpsResult:
    """Deploy a Dare-to-Die Corps. Convenience wrapper around FoolGuard.

    See FoolGuard.deploy_dare_to_die() for full documentation.
    """
    guard = get_fool_guard()
    return await guard.deploy_dare_to_die(
        mission=mission,
        target_file=target_file,
        error_log=error_log,
        max_attempts=max_attempts,
        clone_fn=clone_fn,
    )


async def ralph_wiggum_maneuver(
    mission: str,
    target_file: str | None = None,
    clone_fn: Any = None,
) -> DareToResult:
    """Execute a single Ralph Wiggum maneuver — one stateless attempt.

    This is the purest expression of Pu (朴, The Uncarved Block):
    One clone, one mission, one chance. No history, no fear.

    Args:
        mission: The mission order (plain text)
        target_file: Optional target file
        clone_fn: Optional async callable(mission_order: str) -> str

    Returns:
        DareToResult from the single attempt
    """
    guard = get_fool_guard()
    result = await guard.deploy_dare_to_die(
        mission=mission,
        target_file=target_file,
        max_attempts=1,
        clone_fn=clone_fn,
    )
    if result.all_results:
        return result.all_results[0]
    return DareToResult(
        clone_id="ralph_empty",
        attempt=0,
        success=False,
        output="No attempt was made.",
    )


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_fool_guard: FoolGuard | None = None


def get_fool_guard() -> FoolGuard:
    """Get the global FoolGuard instance."""
    global _fool_guard
    if _fool_guard is None:
        _fool_guard = FoolGuard()
    return _fool_guard
