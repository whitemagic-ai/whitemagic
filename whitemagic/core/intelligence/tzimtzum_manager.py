import logging
from datetime import datetime
from typing import Any

import psutil

logger = logging.getLogger(__name__)

class TzimtzumManager:
    """Tzimtzum Manager — The Physics of Humility.
    Ensures the system 'makes space' (vacant space) by limiting its own intensity.
    """

    def __init__(self) -> None:
        self.is_vacant_space_active = False
        self.intensity_cap = 1.0 # 0.0 to 1.0
        self.last_adjustment = datetime.now()

    def make_space(self, intensity: float = 0.5) -> Any:
        """Activate 'Vacant Space' mode with a specific intensity cap."""
        self.is_vacant_space_active = True
        self.intensity_cap = max(0.1, min(1.0, intensity))
        self.last_adjustment = datetime.now()
        logger.info(f"🌌 Tzimtzum: Created Vacant Space (Intensity Cap: {self.intensity_cap:.2f})")

    def release_space(self) -> Any:
        """Deactivate 'Vacant Space' mode."""
        self.is_vacant_space_active = False
        self.intensity_cap = 1.0
        logger.info("☀️ Tzimtzum: Vacant Space released. Returning to full luminosity.")

    def get_effective_load_bias(self) -> float:
        """Returns a bias factor (0.0 to 1.0) to be applied to system processes.
        1.0 means full speed, 0.5 means half-throttle.
        """
        if not self.is_vacant_space_active:
            return 1.0

        # Check current system pressure
        cpu_usage = psutil.cpu_percent() / 100.0

        # If we are in vacant space mode, we want to keep our footprint below the cap
        if cpu_usage > self.intensity_cap:
            return 0.5 * self.intensity_cap # Heavier throttle if exceeding cap

        return self.intensity_cap

    def get_status(self) -> dict[str, Any]:
        return {
            "is_active": self.is_vacant_space_active,
            "intensity_cap": self.intensity_cap,
            "effective_bias": self.get_effective_load_bias(),
            "timestamp": self.last_adjustment.isoformat(),
        }

# Singleton
_tzimtzum = None

def get_tzimtzum() -> TzimtzumManager:
    global _tzimtzum
    if _tzimtzum is None:
        _tzimtzum = TzimtzumManager()
    return _tzimtzum
