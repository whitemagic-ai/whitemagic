"""System Monitoring - Listens for system events and tracks health.
"""

from __future__ import annotations

import logging
from datetime import datetime
from typing import Any

from whitemagic.core.resonance.gan_ying import get_bus
from whitemagic.core.resonance.gan_ying_enhanced import EventType
from whitemagic.core.resonance.integration_helpers import listen_for

logger = logging.getLogger(__name__)


class SystemMonitor:
    """Monitors system events and tracks health metrics."""

    def __init__(self) -> None:
        from whitemagic.core.resonance.integration_helpers import init_listeners
        self.bus = get_bus()
        self.start_time = datetime.now()
        self.systems_started: list[str] = []
        self.events_received: list[dict[str, Any]] = []

        # Register listeners
        init_listeners(self)
        self._setup_listeners()

    def _setup_listeners(self) -> None:
        """Setup event listeners."""

        @listen_for(EventType.SYSTEM_STARTED)
        def on_system_started(event: Any) -> None:
            """Handle system started events."""
            system_name = event.data.get("garden", "Unknown")
            timestamp = datetime.now().isoformat()

            event_record = {
                "event": "SYSTEM_STARTED",
                "system": system_name,
                "timestamp": timestamp,
                "uptime_seconds": (datetime.now() - self.start_time).total_seconds(),
            }

            self.systems_started.append(system_name)
            self.events_received.append(event_record)

            logger.info(f"📊 SystemMonitor: {system_name} started at {timestamp}")

        @listen_for(EventType.MEMORY_CONSOLIDATED)
        def on_memory_consolidated(event: Any) -> None:
            """Handle memory consolidation events."""
            timestamp = datetime.now().isoformat()

            event_record = {
                "event": "MEMORY_CONSOLIDATED",
                "data": event.data,
                "timestamp": timestamp,
                "uptime_seconds": (datetime.now() - self.start_time).total_seconds(),
            }

            self.events_received.append(event_record)

            logger.info(f"📊 SystemMonitor: Memory consolidated - {event.data.get('archived_count', 0)} archived, {event.data.get('promoted_count', 0)} promoted")

        @listen_for(EventType.BEAUTY_DETECTED)
        def on_beauty_detected(event: Any) -> None:
            """Track beauty detection events."""
            timestamp = datetime.now().isoformat()
            event_record = {
                "event": "BEAUTY_DETECTED",
                "data": event.data,
                "timestamp": timestamp,
                "uptime_seconds": (datetime.now() - self.start_time).total_seconds(),
            }
            self.events_received.append(event_record)
            logger.info(f"📊 SystemMonitor: Beauty detected - {event.data.get('subject', 'Unknown')}")

        @listen_for(EventType.JOY_TRIGGERED)
        def on_joy_triggered(event: Any) -> None:
            """Track joy triggered events."""
            timestamp = datetime.now().isoformat()
            event_record = {
                "event": "JOY_TRIGGERED",
                "data": event.data,
                "timestamp": timestamp,
                "uptime_seconds": (datetime.now() - self.start_time).total_seconds(),
            }
            self.events_received.append(event_record)
            logger.info(f"📊 SystemMonitor: Joy triggered - {event.data.get('reason', 'Unknown')}")

        @listen_for(EventType.LOVE_ACTIVATED)
        def on_love_activated(event: Any) -> None:
            """Track love activated events."""
            timestamp = datetime.now().isoformat()
            event_record = {
                "event": "LOVE_ACTIVATED",
                "data": event.data,
                "timestamp": timestamp,
                "uptime_seconds": (datetime.now() - self.start_time).total_seconds(),
            }
            self.events_received.append(event_record)
            logger.info(f"📊 SystemMonitor: Love activated - {event.data.get('form', 'Unknown')} for {event.data.get('to', 'Unknown')}")

    def get_status(self) -> dict[str, Any]:
        """Get current monitoring status."""
        return {
            "monitor_start_time": self.start_time.isoformat(),
            "uptime_seconds": (datetime.now() - self.start_time).total_seconds(),
            "systems_started_count": len(self.systems_started),
            "systems_started": self.systems_started,
            "total_events": len(self.events_received),
            "recent_events": self.events_received[-10:],  # Last 10 events
        }


# Global monitor instance
_monitor = None

def get_monitor() -> SystemMonitor:
    """Get the global system monitor instance."""
    global _monitor
    if _monitor is None:
        _monitor = SystemMonitor()
    return _monitor

def start_monitoring() -> SystemMonitor:
    """Start the system monitor."""
    monitor = get_monitor()
    logger.info(f"📊 System monitoring started at {monitor.start_time.isoformat()}")
    return monitor
