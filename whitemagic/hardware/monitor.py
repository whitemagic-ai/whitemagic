"""
Hardware Monitor for Adaptive Inference
Tracks CPU, memory, thermal state, and battery to adapt inference behavior.
"""

import psutil
import time
from typing import Dict, Any, Optional
from dataclasses import dataclass
from enum import Enum

try:
    from whitemagic.core.resonance.gan_ying_enhanced import emit_event, EventType
    HAS_EVENTS = True
except ImportError:
    HAS_EVENTS = False


class ThermalState(Enum):
    """System thermal state."""
    COOL = "cool"
    WARM = "warm"
    HOT = "hot"
    CRITICAL = "critical"


@dataclass
class HardwareState:
    """Current hardware state snapshot."""
    cpu_percent: float
    memory_percent: float
    memory_available_gb: float
    thermal_state: ThermalState
    on_battery: bool
    timestamp: float


class HardwareMonitor:
    """Monitor hardware state and emit events on changes."""

    def __init__(self, poll_interval: float = 5.0):
        self.poll_interval = poll_interval
        self.last_state: Optional[HardwareState] = None
        self.thresholds = {
            'cpu_high': 80.0,
            'cpu_critical': 95.0,
            'memory_high': 85.0,
            'memory_critical': 95.0,
        }

    def get_thermal_state(self) -> ThermalState:
        """Detect thermal state (Linux-specific, fallback for others)."""
        try:
            # Try to read CPU temperature (Linux)
            temps = psutil.sensors_temperatures()
            if 'coretemp' in temps:
                avg_temp = sum(t.current for t in temps['coretemp']) / len(temps['coretemp'])
                if avg_temp > 85:
                    return ThermalState.CRITICAL
                elif avg_temp > 75:
                    return ThermalState.HOT
                elif avg_temp > 60:
                    return ThermalState.WARM
                else:
                    return ThermalState.COOL
        except (AttributeError, KeyError):
            pass

        # Fallback: use CPU usage as proxy
        cpu = psutil.cpu_percent(interval=0.1)
        if cpu > 95:
            return ThermalState.HOT
        elif cpu > 80:
            return ThermalState.WARM
        else:
            return ThermalState.COOL

    def get_battery_status(self) -> bool:
        """Check if system is on battery power."""
        try:
            battery = psutil.sensors_battery()
            if battery:
                return not battery.power_plugged
        except (AttributeError, RuntimeError):
            pass
        return False

    def sample_state(self) -> HardwareState:
        """Sample current hardware state."""
        mem = psutil.virtual_memory()

        return HardwareState(
            cpu_percent=psutil.cpu_percent(interval=0.1),
            memory_percent=mem.percent,
            memory_available_gb=mem.available / (1024**3),
            thermal_state=self.get_thermal_state(),
            on_battery=self.get_battery_status(),
            timestamp=time.time()
        )

    def check_and_emit_events(self, current: HardwareState) -> None:
        """Check for significant changes and emit events."""
        if not HAS_EVENTS or not self.last_state:
            return

        # CPU load changed significantly
        if abs(current.cpu_percent - self.last_state.cpu_percent) > 20:
            emit_event(EventType.SYSTEM_STATE_CHANGED, {  # type: ignore[call-arg,attr-defined,arg-type]
                'type': 'cpu_load',
                'old': self.last_state.cpu_percent,
                'new': current.cpu_percent
            })

        # Thermal state changed
        if current.thermal_state != self.last_state.thermal_state:
            emit_event(EventType.SYSTEM_STATE_CHANGED, {  # type: ignore[call-arg,attr-defined,arg-type]
                'type': 'thermal',
                'old': self.last_state.thermal_state.value,
                'new': current.thermal_state.value
            })

            if current.thermal_state == ThermalState.HOT:
                emit_event(EventType.SYSTEM_STATE_CHANGED, {  # type: ignore[call-arg,attr-defined,arg-type]
                    'type': 'thermal_warning',
                    'state': 'hot',
                    'recommendation': 'reduce_inference_load'
                })

        # Battery status changed
        if current.on_battery != self.last_state.on_battery:
            emit_event(EventType.SYSTEM_STATE_CHANGED, {  # type: ignore[call-arg,attr-defined,arg-type]
                'type': 'power',
                'on_battery': current.on_battery,
                'recommendation': 'disable_deep_tier' if current.on_battery else 'enable_all_tiers'
            })

    def get_inference_recommendations(self, state: HardwareState) -> Dict[str, Any]:
        """Get inference recommendations based on hardware state."""
        recommendations: Dict[str, Any] = {
            'allow_fast': True,
            'allow_explore': True,
            'allow_deep': True,
            'max_parallel': 4,
            'reason': []
        }

        # High CPU load
        if state.cpu_percent > self.thresholds['cpu_high']:
            recommendations['allow_deep'] = False
            recommendations['max_parallel'] = 1
            recommendations['reason'].append('High CPU load - prefer fast tier')

        # Low memory
        if state.memory_percent > self.thresholds['memory_high']:
            recommendations['allow_deep'] = False
            recommendations['reason'].append('Low memory - disable deep tier')

        # Thermal throttling
        if state.thermal_state in [ThermalState.HOT, ThermalState.CRITICAL]:
            recommendations['allow_deep'] = False
            recommendations['allow_explore'] = False
            recommendations['reason'].append('Thermal throttling - fast tier only')

        # On battery
        if state.on_battery:
            recommendations['allow_deep'] = False
            recommendations['max_parallel'] = 2
            recommendations['reason'].append('On battery - conserve power')

        return recommendations

    def get_stats(self) -> Dict[str, Any]:
        """Get current hardware statistics."""
        state = self.sample_state()
        recommendations = self.get_inference_recommendations(state)

        return {
            'current_state': {
                'cpu_percent': state.cpu_percent,
                'memory_percent': state.memory_percent,
                'memory_available_gb': state.memory_available_gb,
                'thermal_state': state.thermal_state.value,
                'on_battery': state.on_battery,
            },
            'recommendations': recommendations,
            'thresholds': self.thresholds
        }


# Global monitor instance
_hardware_monitor = None

def get_hardware_monitor() -> HardwareMonitor:
    """Get or create global hardware monitor."""
    global _hardware_monitor
    if _hardware_monitor is None:
        _hardware_monitor = HardwareMonitor()
    return _hardware_monitor


def get_hardware_recommendations() -> Dict[str, Any]:
    """Get current hardware-based inference recommendations."""
    monitor = get_hardware_monitor()
    state = monitor.sample_state()
    return monitor.get_inference_recommendations(state)
