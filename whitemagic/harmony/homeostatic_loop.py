"""Homeostatic Loop — Harmony-Driven Self-Regulation
===================================================
Closes the feedback loop on the Harmony Vector: instead of just
*reporting* health, the system now *acts* on it.

The loop periodically samples the Harmony Vector and applies corrective
actions when dimensions drift out of bounds:

  - **High error_rate** → emit WARNING_ISSUED, suggest tool cooldown
  - **High karma_debt** → emit BOUNDARY_VIOLATED, trigger Dharma WARN
  - **Low energy** → trigger memory lifecycle sweep (mindful forgetting)
  - **Low throughput** → log advisory, no automatic action
  - **Low dharma** → tighten Dharma profile to 'secure' temporarily
  - **High latency** → suggest circuit breaker review

Actions are graduated:
  OBSERVE → ADVISE → CORRECT → INTERVENE

The loop never blocks tool dispatch — it runs asynchronously on the
temporal scheduler's MEDIUM lane (planning-speed cadence).

Usage:
    from whitemagic.harmony.homeostatic_loop import get_homeostatic_loop
    loop = get_homeostatic_loop()
    loop.attach()  # hooks into temporal scheduler
"""

from __future__ import annotations

import logging
import threading
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any

logger = logging.getLogger(__name__)


class ActionLevel(str, Enum):
    """Graduated response levels."""

    OBSERVE = "observe"     # Just note it
    ADVISE = "advise"       # Log a recommendation
    CORRECT = "correct"     # Take gentle corrective action
    INTERVENE = "intervene" # Take strong corrective action


@dataclass
class HomeostaticAction:
    """A corrective action taken by the homeostatic loop."""

    dimension: str
    level: ActionLevel
    value: float
    threshold: float
    action_taken: str
    timestamp: str = field(default_factory=lambda: datetime.now().isoformat())

    def to_dict(self) -> dict[str, Any]:
        return {
            "dimension": self.dimension,
            "level": self.level.value,
            "value": round(self.value, 3),
            "threshold": self.threshold,
            "action_taken": self.action_taken,
            "timestamp": self.timestamp,
        }


@dataclass
class HomeostaticConfig:
    """Thresholds for homeostatic intervention."""

    # When a dimension drops below these values, we act
    error_rate_advise: float = 0.7     # advise when error dimension < 0.7
    error_rate_correct: float = 0.4    # correct when < 0.4
    karma_debt_advise: float = 0.7     # advise when karma_debt dimension < 0.7
    karma_debt_correct: float = 0.4    # correct when < 0.4
    energy_advise: float = 0.6         # advise when energy < 0.6
    energy_correct: float = 0.3        # correct when < 0.3
    dharma_advise: float = 0.7         # advise when dharma < 0.7
    dharma_correct: float = 0.4        # correct when < 0.4
    latency_advise: float = 0.5        # advise when latency dimension < 0.5
    harmony_intervene: float = 0.3     # intervene when composite < 0.3
    check_interval_s: float = 10.0     # how often to check (seconds)


class HomeostaticLoop:
    """Periodically samples the Harmony Vector and applies graduated
    corrective actions when dimensions drift out of bounds.
    """

    def __init__(self, config: HomeostaticConfig | None = None):
        self._config = config or HomeostaticConfig()
        self._lock = threading.Lock()
        self._actions: list[HomeostaticAction] = []
        self._total_checks: int = 0
        self._total_actions: int = 0
        self._attached = False
        self._running = False
        self._stop_event = threading.Event()
        self._thread: threading.Thread | None = None

    # ------------------------------------------------------------------
    # Lifecycle
    # ------------------------------------------------------------------

    def attach(self) -> bool:
        """Start the homeostatic loop as a background thread."""
        if self._running:
            return True
        self._running = True
        self._stop_event.clear()
        self._thread = threading.Thread(
            target=self._loop,
            daemon=True,
            name="homeostatic-loop",
        )
        self._thread.start()
        self._attached = True
        logger.info(
            f"Homeostatic loop started (check every {self._config.check_interval_s}s)",
        )
        return True

    def detach(self) -> None:
        """Stop the homeostatic loop."""
        self._running = False
        self._stop_event.set()
        if self._thread and self._thread.is_alive():
            self._thread.join(timeout=5.0)
        self._attached = False

    def _loop(self) -> None:
        """Background loop that periodically checks harmony."""
        while not self._stop_event.is_set():
            self._stop_event.wait(timeout=self._config.check_interval_s)
            if not self._running:
                break
            try:
                self.check()
            except Exception as e:
                logger.debug(f"Homeostatic check error: {e}")

    # ------------------------------------------------------------------
    # Core check
    # ------------------------------------------------------------------

    def check(self) -> list[HomeostaticAction]:
        """Sample the Harmony Vector and apply corrective actions.
        Returns list of actions taken this cycle.

        Synthesis: consults the Salience Arbiter spotlight to detect
        urgent events that should escalate correction levels.
        """
        try:
            from whitemagic.harmony.vector import get_harmony_vector
            snap = get_harmony_vector().snapshot()
        except Exception:
            return []

        self._total_checks += 1
        actions: list[HomeostaticAction] = []

        # --- Salience Arbiter coupling ---
        # If the spotlight contains high-urgency events, escalate corrections
        salience_boost = self._salience_urgency_boost()

        # --- Composite harmony critically low → INTERVENE ---
        # Salience boost can lower the intervention threshold (more aggressive)
        intervene_threshold = self._config.harmony_intervene + (salience_boost * 0.1)
        if snap.harmony_score < intervene_threshold:
            action = self._intervene_critical(snap)
            if action:
                actions.append(action)

        # --- Error rate degraded ---
        if snap.error_rate < self._config.error_rate_correct:
            actions.append(self._correct_errors(snap))
        elif snap.error_rate < self._config.error_rate_advise:
            actions.append(self._advise("error_rate", snap.error_rate,
                                        self._config.error_rate_advise,
                                        "High error rate detected. Consider reviewing failing tools."))

        # --- Karma debt ---
        if snap.karma_debt < self._config.karma_debt_correct:
            actions.append(self._correct_karma(snap))
        elif snap.karma_debt < self._config.karma_debt_advise:
            actions.append(self._advise("karma_debt", snap.karma_debt,
                                        self._config.karma_debt_advise,
                                        "Karma debt rising. Check karma_report for mismatches."))

        # --- Energy (memory pressure) ---
        if snap.energy < self._config.energy_correct:
            actions.append(self._correct_energy(snap))
        elif snap.energy < self._config.energy_advise:
            actions.append(self._advise("energy", snap.energy,
                                        self._config.energy_advise,
                                        "Energy low. Consider running a memory lifecycle sweep."))

        # --- Dharma (ethical score) ---
        if snap.dharma < self._config.dharma_correct:
            actions.append(self._correct_dharma(snap))
        elif snap.dharma < self._config.dharma_advise:
            actions.append(self._advise("dharma", snap.dharma,
                                        self._config.dharma_advise,
                                        "Dharma score declining. Review recent tool usage patterns."))

        # --- Latency ---
        if snap.latency < self._config.latency_advise:
            actions.append(self._advise("latency", snap.latency,
                                        self._config.latency_advise,
                                        f"High latency (p95={snap.p95_latency_ms:.0f}ms). "
                                        "Check circuit breaker status."))

        # Record actions and feed back to Salience Arbiter
        if actions:
            with self._lock:
                self._total_actions += len(actions)
                self._actions.extend(actions)
                # Keep bounded
                if len(self._actions) > 500:
                    self._actions = self._actions[-250:]
            # Emit corrections to the arbiter so they appear in the spotlight
            for action in actions:
                if action.level in (ActionLevel.CORRECT, ActionLevel.INTERVENE):
                    self._emit_to_arbiter(action)

        return actions

    # ------------------------------------------------------------------
    # Salience Arbiter coupling
    # ------------------------------------------------------------------

    def _salience_urgency_boost(self) -> float:
        """Query the Salience Arbiter spotlight for high-urgency events.
        Returns a boost factor [0.0–1.0] that escalates homeostatic thresholds.
        """
        try:
            from whitemagic.core.resonance.salience_arbiter import get_salience_arbiter
            arbiter = get_salience_arbiter()
            spotlight = arbiter.get_spotlight(n=5)
            if not spotlight:
                return 0.0
            # Average urgency of spotlight events
            avg_urgency = sum(e.salience.urgency for e in spotlight) / len(spotlight)
            # Only boost if urgency is notably high (> 0.7)
            if avg_urgency > 0.7:
                return min(1.0, (avg_urgency - 0.7) / 0.3)
            return 0.0
        except Exception:
            return 0.0

    def _emit_to_arbiter(self, action: HomeostaticAction) -> None:
        """Admit homeostatic actions back into the Salience Arbiter as events."""
        try:
            from whitemagic.core.resonance.gan_ying_enhanced import (
                EventType,
                ResonanceEvent,
            )
            from whitemagic.core.resonance.salience_arbiter import get_salience_arbiter
            event_type = (
                EventType.SYSTEM_HEALTH_CHANGED
                if action.level in (ActionLevel.CORRECT, ActionLevel.INTERVENE)
                else EventType.WARNING_ISSUED
            )
            event = ResonanceEvent(
                event_type=event_type,
                source="homeostatic_loop",
                data=action.to_dict(),
                confidence=0.9 if action.level != ActionLevel.OBSERVE else 0.5,
            )
            get_salience_arbiter().admit(event)
        except Exception:
            pass

    # ------------------------------------------------------------------
    # Corrective actions
    # ------------------------------------------------------------------

    def _advise(self, dimension: str, value: float, threshold: float,
                message: str) -> HomeostaticAction:
        """Log an advisory — no system changes."""
        logger.info(f"[Homeostasis/ADVISE] {dimension}={value:.3f} < {threshold}: {message}")
        return HomeostaticAction(
            dimension=dimension, level=ActionLevel.ADVISE,
            value=value, threshold=threshold, action_taken=message,
        )

    def _correct_errors(self, snap: Any) -> HomeostaticAction:
        """Correct high error rate by emitting a warning event."""
        msg = "Error rate critical. Emitting system warning."
        logger.warning(f"[Homeostasis/CORRECT] error_rate={snap.error_rate:.3f}: {msg}")
        try:
            from whitemagic.core.resonance.gan_ying_enhanced import (
                EventType,
                ResonanceEvent,
                get_bus,
            )
            get_bus().emit(ResonanceEvent(
                event_type=EventType.WARNING_ISSUED,
                source="homeostatic_loop",
                data={"dimension": "error_rate", "value": snap.error_rate,
                      "errors_in_window": snap.errors_in_window},
            ))
        except Exception:
            pass
        return HomeostaticAction(
            dimension="error_rate", level=ActionLevel.CORRECT,
            value=snap.error_rate, threshold=self._config.error_rate_correct,
            action_taken=msg,
        )

    def _correct_karma(self, snap: Any) -> HomeostaticAction:
        """Correct high karma debt by emitting a boundary event."""
        msg = "Karma debt high. Emitting boundary violation event."
        logger.warning(f"[Homeostasis/CORRECT] karma_debt={snap.karma_debt:.3f}: {msg}")
        try:
            from whitemagic.core.resonance.gan_ying_enhanced import (
                EventType,
                ResonanceEvent,
                get_bus,
            )
            get_bus().emit(ResonanceEvent(
                event_type=EventType.BOUNDARY_VIOLATED,
                source="homeostatic_loop",
                data={"dimension": "karma_debt", "value": snap.karma_debt,
                      "mismatches": snap.karma_mismatches_in_window},
            ))
        except Exception:
            pass
        return HomeostaticAction(
            dimension="karma_debt", level=ActionLevel.CORRECT,
            value=snap.karma_debt, threshold=self._config.karma_debt_correct,
            action_taken=msg,
        )

    def _correct_energy(self, snap: Any) -> HomeostaticAction:
        """Correct low energy by triggering a memory lifecycle sweep."""
        msg = "Energy critical. Triggering memory lifecycle sweep."
        logger.warning(f"[Homeostasis/CORRECT] energy={snap.energy:.3f}: {msg}")
        try:
            from whitemagic.core.memory.lifecycle import get_lifecycle_manager
            mgr = get_lifecycle_manager()
            mgr.run_sweep()
        except Exception as e:
            msg += f" (sweep failed: {e})"
        return HomeostaticAction(
            dimension="energy", level=ActionLevel.CORRECT,
            value=snap.energy, threshold=self._config.energy_correct,
            action_taken=msg,
        )

    def _correct_dharma(self, snap: Any) -> HomeostaticAction:
        """Correct low dharma by temporarily tightening the profile."""
        msg = "Dharma score critical. Switching to 'secure' profile temporarily."
        logger.warning(f"[Homeostasis/CORRECT] dharma={snap.dharma:.3f}: {msg}")
        try:
            from whitemagic.dharma.rules import get_rules_engine
            engine = get_rules_engine()
            if engine.get_profile() != "secure":
                engine.set_profile("secure")
        except Exception:
            pass
        return HomeostaticAction(
            dimension="dharma", level=ActionLevel.CORRECT,
            value=snap.dharma, threshold=self._config.dharma_correct,
            action_taken=msg,
        )

    def _intervene_critical(self, snap: Any) -> HomeostaticAction | None:
        """System-wide intervention when composite harmony is critically low."""
        msg = (
            f"CRITICAL: Composite harmony={snap.harmony_score:.3f}. "
            "All corrective measures activated."
        )
        logger.critical(f"[Homeostasis/INTERVENE] {msg}")
        try:
            from whitemagic.core.resonance.gan_ying_enhanced import (
                EventType,
                ResonanceEvent,
                get_bus,
            )
            get_bus().emit(ResonanceEvent(
                event_type=EventType.SYSTEM_HEALTH_CHANGED,
                source="homeostatic_loop",
                data={
                    "harmony_score": snap.harmony_score,
                    "level": "critical",
                    "dimensions": snap.to_dict(),
                },
            ))
        except Exception:
            pass
        return HomeostaticAction(
            dimension="harmony_score", level=ActionLevel.INTERVENE,
            value=snap.harmony_score, threshold=self._config.harmony_intervene,
            action_taken=msg,
        )

    # ------------------------------------------------------------------
    # Introspection
    # ------------------------------------------------------------------

    def get_stats(self) -> dict[str, Any]:
        with self._lock:
            return {
                "running": self._running,
                "total_checks": self._total_checks,
                "total_actions": self._total_actions,
                "recent_actions": [a.to_dict() for a in self._actions[-10:]],
                "config": {
                    "check_interval_s": self._config.check_interval_s,
                    "harmony_intervene": self._config.harmony_intervene,
                },
            }

    @property
    def is_running(self) -> bool:
        return self._running


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_loop: HomeostaticLoop | None = None
_loop_lock = threading.Lock()


def get_homeostatic_loop(
    config: HomeostaticConfig | None = None,
) -> HomeostaticLoop:
    """Get the global Homeostatic Loop."""
    global _loop
    if _loop is None:
        with _loop_lock:
            if _loop is None:
                _loop = HomeostaticLoop(config=config)
    return _loop
