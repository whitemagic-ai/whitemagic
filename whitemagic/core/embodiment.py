"""Embodiment Layer - Hardware-aware AI (Phase 1 TAURUS).

Maps to MandalaOS "Prana Allocation" - AI that knows its physical constraints.
Bridges Rust HarmonyVector with Python PRAT system.
"""

import logging
import time
from typing import Any

logger = logging.getLogger(__name__)

try:
    from whitemagic_rs import get_harmony_vector
    RUST_AVAILABLE = True
except ImportError:
    RUST_AVAILABLE = False
    logger.info("⚠️  Rust HarmonyVector not available - using Python fallback")


class HarmonyMonitor:
    """Continuous system monitoring for embodied AI.

    Provides real-time awareness of:
    - CPU load
    - Memory pressure
    - Thermal state
    - I/O wait
    - Composite harmony score
    """

    def __init__(self, update_interval: float = 1.0):
        self.update_interval = update_interval
        self.last_update = 0.0
        self._current: dict | None = None

    def get_current(self, force_update: bool = False) -> dict[str, Any]:
        """Get current harmony state, updating if needed."""
        now = time.time()

        if force_update or self._current is None or (now - self.last_update) >= self.update_interval:
            if RUST_AVAILABLE:
                hv = get_harmony_vector()
                self._current = {
                    "cpu_load": hv.cpu_load,
                    "memory_pressure": hv.memory_pressure,
                    "thermal_celsius": hv.thermal_celsius,
                    "io_wait": hv.io_wait,
                    "harmony_score": hv.harmony_score,
                    "guna_tag": hv.guna_tag(),
                    "is_harmonious": hv.is_harmonious(),
                    "timestamp": now,
                }
            else:
                # Python fallback - basic metrics
                self._current = {
                    "cpu_load": 0.0,
                    "memory_pressure": 0.0,
                    "thermal_celsius": 45.0,
                    "io_wait": 0.0,
                    "harmony_score": 1.0,
                    "guna_tag": "Sattva",
                    "is_harmonious": True,
                    "timestamp": now,
                }

            self.last_update = now

        return self._current

    def check_resource_availability(self) -> dict[str, Any]:
        """Check if system can handle resource-intensive operations.

        Returns dict with recommendations for:
        - heavy_processing: CPU/memory intensive ops
        - parallel_execution: Multi-threaded work
        - disk_io: File operations
        - network_io: External connections
        """
        state = self.get_current()

        return {
            "heavy_processing": state["harmony_score"] > 0.6,  # Lowered from 0.7
            "parallel_execution": state["cpu_load"] < 0.8 and state["harmony_score"] > 0.5,  # Lowered from 0.6
            "disk_io": state["io_wait"] < 0.6,  # More lenient (was 0.5)
            "network_io": state["harmony_score"] > 0.45,  # Lowered from 0.5
            "recommended_action": self._get_recommendation(state),
        }

    def _get_recommendation(self, state: dict[str, Any]) -> str:
        """Get action recommendation based on harmony state."""
        score = state["harmony_score"]

        if score > 0.7:
            return "Optimal - proceed with any operations"
        elif score > 0.5:
            return "Good - prioritize essential work"
        elif score > 0.35:
            return "Moderate load - defer heavy tasks"
        else:
            return "Critical - throttle all operations"

    def get_guna_priority(self) -> int:
        """Get scheduling priority adjustment based on Guna tag.

        Returns:
        - Sattva: 0 (normal priority)
        - Rajas: -5 (lower, throttle active processes)
        - Tamas: +5 (higher, help recover)

        """
        state = self.get_current()
        guna = state.get("guna_tag", "Sattva")

        priority_map = {
            "Sattva": 0,
            "Rajas": -5,
            "Tamas": 5,
        }

        return priority_map.get(guna, 0)


# Global singleton for easy access
_monitor: HarmonyMonitor | None = None

def get_harmony_monitor() -> HarmonyMonitor:
    """Get or create global HarmonyMonitor singleton."""
    global _monitor
    if _monitor is None:
        _monitor = HarmonyMonitor()
    return _monitor


def check_embodiment() -> dict[str, Any]:
    """Quick check of embodiment layer status.

    Returns system state + recommendations for AI orchestration.
    """
    monitor = get_harmony_monitor()
    state = monitor.get_current(force_update=True)
    availability = monitor.check_resource_availability()

    return {
        "rust_available": RUST_AVAILABLE,
        "harmony_vector": state,
        "resource_availability": availability,
        "embodied": True,
    }


if __name__ == "__main__":
    # Test embodiment layer
    logger.info("=== Embodiment Layer Test ===\n")

    result = check_embodiment()

    logger.info("Rust acceleration:", "✅ Available" if result["rust_available"] else "⚠️  Fallback")
    logger.info("\nHarmony Vector:")
    for key, val in result["harmony_vector"].items():
        if key != "timestamp":
            logger.info(f"  {key}: {val}")

    logger.info("\nResource Availability:")
    for key, val in result["resource_availability"].items():
        if isinstance(val, bool):
            status = "✅" if val else "⛔"
            logger.info(f"  {status} {key}")
        else:
            logger.info(f"  → {val}")
