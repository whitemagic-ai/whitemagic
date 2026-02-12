"""Patience Garden — Ethical Balance & Equilibrium.

Mansion: #15 Straddling Legs (奎 Kui)
Quadrant: Western (White Tiger)
PRAT Gana: gana_straddling_legs — 6 tools (evaluate_ethics, check_boundaries,
    verify_consent, get_ethical_score, get_dharma_guidance, harmony_vector)

The Straddling Legs Gana balances. The Patience Garden provides the substrate
for ethical deliberation: scoring actions against dharma rules, checking
boundary conditions, and maintaining the patience required for balanced judgment.

Holographic Integration:
- Calm and contemplative (X-axis -0.1) — balance requires steady mind
- Abstract principles (Y-axis +0.2) — ethics are universal
- Future-trusting (Z-axis +0.3) — patience trusts process
- Important for wise action (W-axis +0.25) — balance enables good decisions
"""

from __future__ import annotations

import logging
import threading
from collections import deque
from datetime import datetime
from typing import Any

from whitemagic.core.resonance.gan_ying_enhanced import EventType
from whitemagic.core.resonance.integration_helpers import GanYingMixin, init_listeners
from whitemagic.gardens.base_garden import BaseGarden, CoordinateBias

logger = logging.getLogger(__name__)


class PatienceGarden(BaseGarden, GanYingMixin):
    """Garden of Patience — Ethical balance engine for the Straddling Legs Gana.

    Serves the Straddling Legs Gana's ethics tools by maintaining:
    - Ethical evaluation history with scoring
    - Boundary check results and consent verification log
    - Dharma guidance cache for consistent advice
    - Balance metrics across ethical dimensions
    """

    name = "patience"
    category = "ethics"
    resonance_partners = ["dharma", "wisdom", "truth", "stillness"]
    mansion_number = 15
    gana_name = "gana_straddling_legs"

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self._lock = threading.Lock()
        self.evaluation_history: deque[dict[str, Any]] = deque(maxlen=200)
        self.boundary_checks: deque[dict[str, Any]] = deque(maxlen=200)
        self.consent_log: deque[dict[str, Any]] = deque(maxlen=100)
        self.patience_level: float = 0.5
        self._ethical_score_accumulator: float = 0.0
        self._evaluation_count: int = 0
        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Patience", "mansion": 15})

    def get_name(self) -> str:
        return "patience"

    def get_coordinate_bias(self) -> CoordinateBias:
        return CoordinateBias(x=-0.1, y=0.2, z=0.3, w=0.25)

    # ------------------------------------------------------------------
    # Ethical evaluation — serving evaluate_ethics, get_ethical_score
    # ------------------------------------------------------------------

    def evaluate_action(self, action: str, context: dict[str, Any] | None = None) -> dict[str, Any]:
        """Evaluate an action's ethical implications.

        Checks against Dharma rules if available, otherwise uses basic heuristics.
        """
        score = 0.7  # Default: neutral-positive
        concerns: list[str] = []
        blessings: list[str] = []

        # Try Dharma rules engine
        try:
            from whitemagic.dharma.rules import get_rules_engine
            engine = get_rules_engine()
            result = engine.evaluate({"action": action, **(context or {})})
            score = getattr(result, "score", score)
            concerns = getattr(result, "concerns", [])
            blessings = getattr(result, "blessings", [])
        except (ImportError, Exception) as exc:
            logger.debug(f"Dharma rules not available: {exc}")
            # Basic keyword heuristics
            risky = ["delete", "destroy", "override", "force", "bypass"]
            for word in risky:
                if word in action.lower():
                    score -= 0.15
                    concerns.append(f"Action contains '{word}' — requires care")
            if score > 0.6:
                blessings.append("Action appears constructive")

        evaluation: dict[str, Any] = {
            "action": action,
            "score": round(max(0.0, min(1.0, score)), 3),
            "concerns": concerns,
            "blessings": blessings,
            "passed": score >= 0.4,
            "timestamp": datetime.now().isoformat(),
        }

        with self._lock:
            self.evaluation_history.append(evaluation)
            self._ethical_score_accumulator += evaluation["score"]
            self._evaluation_count += 1

        self.emit(EventType.GARDEN_ACTIVITY, {"action": "ethical_eval", "score": evaluation["score"]})  # type: ignore[attr-defined]
        return evaluation

    def get_ethical_score(self) -> dict[str, Any]:
        """Get aggregate ethical score across all evaluations."""
        with self._lock:
            if self._evaluation_count == 0:
                return {"average_score": 1.0, "evaluations": 0, "trend": "pristine"}
            avg = self._ethical_score_accumulator / self._evaluation_count
            recent = list(self.evaluation_history)[-10:]
        recent_avg = sum(e["score"] for e in recent) / max(len(recent), 1)
        trend = "improving" if recent_avg > avg else "declining" if recent_avg < avg - 0.05 else "stable"
        return {
            "average_score": round(avg, 3),
            "recent_score": round(recent_avg, 3),
            "evaluations": self._evaluation_count,
            "trend": trend,
            "recent_concerns": [c for e in recent for c in e.get("concerns", [])],
        }

    # ------------------------------------------------------------------
    # Boundary checks — serving check_boundaries tool
    # ------------------------------------------------------------------

    def check_boundary(self, action: str, resource: str = "",
                       agent_id: str = "") -> dict[str, Any]:
        """Check if an action crosses any boundaries."""
        violations: list[str] = []
        # Check sandbox limits
        try:
            from whitemagic.gardens.sanctuary import get_sanctuary_garden
            sanctuary = get_sanctuary_garden()
            status = sanctuary.get_sandbox_status()
            if status.get("violations_total", 0) > 10:
                violations.append("High violation count — proceed with extra care")
        except Exception:
            pass

        result = {
            "action": action,
            "resource": resource,
            "agent_id": agent_id,
            "violations": violations,
            "within_bounds": len(violations) == 0,
            "timestamp": datetime.now().isoformat(),
        }
        with self._lock:
            self.boundary_checks.append(result)
        return result

    # ------------------------------------------------------------------
    # Consent verification — serving verify_consent tool
    # ------------------------------------------------------------------

    def verify_consent(self, action: str, agent_id: str,
                       consent_type: str = "implicit") -> dict[str, Any]:
        """Verify consent for an action."""
        entry = {
            "action": action,
            "agent_id": agent_id,
            "consent_type": consent_type,
            "granted": consent_type in ("implicit", "explicit"),
            "timestamp": datetime.now().isoformat(),
        }
        with self._lock:
            self.consent_log.append(entry)
        return entry

    # ------------------------------------------------------------------
    # Original emotional methods (preserved)
    # ------------------------------------------------------------------

    def practice_waiting(self, for_what: str) -> dict[str, Any]:
        practice = {"for": for_what, "timestamp": datetime.now().isoformat()}
        self.patience_level = min(1.0, self.patience_level + 0.05)
        self.emit(EventType.PATIENCE_PRACTICED, practice)
        return practice

    def trust_timing(self, situation: str) -> str:
        return f"Trust the timing of {situation}. Everything unfolds as it should."

    # ------------------------------------------------------------------
    # Status
    # ------------------------------------------------------------------

    def get_status(self) -> dict[str, Any]:
        base = super().get_status()
        base.update({
            "mansion": self.mansion_number,
            "gana": self.gana_name,
            "ethical_score": self.get_ethical_score(),
            "boundary_checks": len(self.boundary_checks),
            "consent_verifications": len(self.consent_log),
            "patience_level": round(self.patience_level, 3),
        })
        return dict(base)


_instance = None
def get_patience_garden() -> PatienceGarden:
    global _instance
    if _instance is None:
        _instance = PatienceGarden()
    return _instance
