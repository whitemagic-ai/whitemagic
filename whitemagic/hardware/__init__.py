"""Hardware awareness for adaptive inference."""

from whitemagic.hardware.monitor import (
    HardwareMonitor,
    HardwareState,
    ThermalState,
    get_hardware_monitor,
    get_hardware_recommendations
)

__all__ = [
    'HardwareMonitor',
    'HardwareState',
    'ThermalState',
    'get_hardware_monitor',
    'get_hardware_recommendations'
]
