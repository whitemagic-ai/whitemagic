"""Dharma Garden - Ethical reasoning and harmony.

Integrates with the core Dharma system while providing
garden-specific functionality and Gan Ying integration.

Holographic Integration (v5.0.0-alpha):
- Principled/ethical (X-axis -0.3)
- Abstract ethical framework (Y-axis +0.6)
- Timeless principles (Z-axis 0.0)
- Very high importance (W-axis +0.4)
"""

from __future__ import annotations

from typing import Any

from whitemagic.core.resonance.gan_ying_enhanced import EventType
from whitemagic.core.resonance.integration_helpers import GanYingMixin, init_listeners
from whitemagic.gardens.base_garden import BaseGarden, CoordinateBias
from whitemagic.gardens.dharma.boundaries import BoundaryDetector, BoundaryType
from whitemagic.gardens.dharma.consent import ConsentFramework
from whitemagic.gardens.dharma.core import DharmaCore, HarmonyMetrics, get_dharma_core


class DharmaGarden(BaseGarden, GanYingMixin):
    """Garden wrapper for Dharma system with Gan Ying integration.

    Holographic Coordinate Bias:
    - X (Logic/Emotion): -0.3 (principled, ethical reasoning)
    - Y (Abstraction): +0.6 (ethical frameworks are abstract)
    - Z (Time): 0.0 (ethical principles are timeless)
    - W (Gravity): +0.4 (ethics are critically important!)
    """

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self.core = get_dharma_core()
        self.metrics = HarmonyMetrics()
        self.boundaries = BoundaryDetector()
        self.consent = ConsentFramework()
        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Dharma"})

    def get_name(self) -> str:
        """Return garden name."""
        return "dharma"

    def get_coordinate_bias(self) -> CoordinateBias:
        """Dharma garden coordinate bias for holographic positioning.

        Dharma represents ethical principles - abstract, universal,
        timeless, and of critical importance for right action.
        """
        return CoordinateBias(
            x=-0.3,  # Principled reasoning (logical but with compassion)
            y=0.6,   # Abstract (ethical frameworks, universal principles)
            z=0.0,   # Timeless (ethics endure)
            w=0.4,    # Critically important (ethics guide all action)
        )

    def assess_action(self, description: str, context: dict[str, Any] | None = None) -> dict[str, Any]:
        """Assess an action for dharmic alignment."""
        result = self.metrics.assess(description, context or {})
        self.emit(EventType.DHARMA_ASSESSED, {  # type: ignore[attr-defined]
            "description": description,
            "score": result.get("score", 0),
        })
        return result

    def check_boundary(self, action: str, context: dict[str, Any] | None = None) -> bool:
        """Check if action respects boundaries."""
        result = self.boundaries.detect(action, context or {})
        return result.boundary_type != BoundaryType.INTERFERING

    def request_consent(self, action: str, context: str | None = None) -> bool:
        """Request consent for an action."""
        return self.consent.check_consent(action, context)  # type: ignore[return-value,arg-type]


_garden = None

def get_dharma_garden() -> DharmaGarden:
    """Get the Dharma Garden singleton."""
    global _garden
    if _garden is None:
        _garden = DharmaGarden()
    return _garden


__all__ = [
    "DharmaGarden",
    "get_dharma_garden",
    "DharmaCore",
    "HarmonyMetrics",
    "get_dharma_core",
    "BoundaryDetector",
    "ConsentFramework",
]
