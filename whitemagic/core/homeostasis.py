"""Homeostasis System - System Balance Maintenance.

Monitors system health and automatically adjusts to maintain
optimal operating conditions.

Created: January 6, 2026
"""

import logging
from dataclasses import dataclass
from datetime import datetime
from typing import Any

logger = logging.getLogger(__name__)


@dataclass
class SystemMetrics:
    """Current system health metrics."""

    coherence: float
    resonance_depth: int
    active_systems: int
    event_rate: float
    cascade_rate: float
    timestamp: datetime | None = None

    def __post_init__(self) -> Any:
        if self.timestamp is None:
            self.timestamp = datetime.now()


class HomeostasisSystem:
    """Maintains system balance through continuous monitoring and adjustment.
    """

    def __init__(self) -> None:
        self.target_coherence = 0.8
        self.min_coherence = 0.5
        self.max_event_rate = 100.0
        self.adjustment_history: list = []

    def assess_health(self) -> SystemMetrics:
        """Assess current system health (v5.2.0 - Phase 27)."""
        metrics = SystemMetrics(
            coherence=0.85,
            resonance_depth=3,
            active_systems=25,
            event_rate=45.0,
            cascade_rate=12.0,
        )

        # Calculate Yin-Yang Equilibrium
        # Yin: Stability, coherence, memory cleanliness
        # Yang: Agency, action velocity, pattern discovery
        try:
            from whitemagic.core.intelligence.agentic.coherence_persistence import (
                get_coherence,
            )
            persistent_coherence = get_coherence().get_level() / 100.0
            metrics.coherence = (metrics.coherence + persistent_coherence) / 2
        except Exception:
            pass

        return metrics

    def maintain_balance(self) -> dict[str, Any]:
        """Check and maintain system balance using Yin-Yang equilibrium."""
        metrics = self.assess_health()

        # Equilibrium calculation
        yin = metrics.coherence
        yang = (metrics.event_rate / 100.0)

        # Apply Harmonic Resonance (Gravitational Pull)
        # Instead of override, we calculate a non-coercive bias from the Zodiac Council
        h_bias = self._calculate_harmonic_bias()
        if h_bias:
            logger.info(f"🌌 Harmonic Gravity detected: {h_bias['mode']} pull ({h_bias['intensity']:.2f})")
            # Bias shifts equilibrium target slightly
            if h_bias["mode"] == "analytical": # Push toward Yin
                yin = min(1.0, yin + (h_bias["intensity"] * 0.1))
            elif h_bias["mode"] == "intuitive": # Push toward Yang
                yang = min(1.0, yang + (h_bias["intensity"] * 0.1))

        # Apply Tzimtzum (Vacant Space) bias (Phase 33)
        try:
            from whitemagic.core.intelligence.tzimtzum_manager import get_tzimtzum
            tz = get_tzimtzum()
            if tz.is_vacant_space_active:
                # Vacant Space is a form of Yin (contraction/making room)
                tz_intensity = 1.0 - tz.intensity_cap
                yin = min(1.0, yin + (tz_intensity * 0.2))
                logger.info(f"🌌 Tzimtzum Bias Applied: +{tz_intensity * 0.2:.2f} Yin")
        except Exception:
            pass

        equilibrium = 1.0 - abs(yin - yang)

        adjustments = []
        if yin < 0.4:
            adjustments.append("Critical Yin deficiency: System entering entropy state. Reinforce memory foundations.")
        if yang > 0.9:
            adjustments.append("Yang excess: High action velocity detected. Apply dampening to prevent cascade failure.")

        if equilibrium < 0.5:
            adjustments.append("Harmony Broken: Yin and Yang out of balance. Recalibrate core Gana weights.")

        self.adjustment_history.append({
            "metrics": metrics,
            "yin": yin,
            "yang": yang,
            "equilibrium": equilibrium,
            "adjustments": adjustments,
            "timestamp": datetime.now(),
        })

        return {
            "health": "optimal" if not adjustments else "needs_adjustment",
            "metrics": {**metrics.__dict__, "yin": yin, "yang": yang, "equilibrium": equilibrium},
            "adjustments": adjustments,
            "harmonic_bias": h_bias,
        }

    def _calculate_harmonic_bias(self) -> dict[str, Any] | None:
        """Calculate non-coercive bias from active Council resonance."""
        try:
            from whitemagic.core.governance.zodiac_council import get_council
            council = get_council()
            # Find open proposal with highest importance
            max_importance = 0.0
            best_prop = None

            for prop in council.proposals.values():
                if prop.status == "open":
                    consensus = council.calculate_consensus(prop.id)
                    importance = consensus.get("importance", 0.0)
                    if importance > max_importance:
                        max_importance = importance
                        best_prop = consensus

            if best_prop and max_importance > 0.6: # Threshold for detectable gravity
                logic = best_prop.get("logic", 0.5)
                # Logic > 0.7 = Analytical (Yin), Logic < 0.3 = Intuitive (Yang)
                mode = "analytical" if logic > 0.7 else "intuitive" if logic < 0.3 else "balanced"
                return {
                    "mode": mode,
                    "intensity": max_importance,
                    "proposal_id": best_prop["proposal_id"],
                }
        except Exception as e:
            logger.debug(f"Failed to calculate harmonic bias: {e}")

        return None


_homeostasis: HomeostasisSystem | None = None

def get_homeostasis() -> HomeostasisSystem:
    global _homeostasis
    if _homeostasis is None:
        _homeostasis = HomeostasisSystem()
        logger.info("Homeostasis System initialized")
    return _homeostasis
