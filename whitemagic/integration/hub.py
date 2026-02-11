"""Integration Hub - The Grand Unifier

Connects Memory Matrix + Session Bootstrap + Agentic Modules + Dashboard
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime, timezone
from threading import Lock
from typing import Any, cast

# Core systems
from whitemagic.config.paths import PROJECT_ROOT

# Agentic modules
from whitemagic.core.intelligence.agentic import (
    full_brain_activation,
    get_anti_loop,
)
from whitemagic.core.memory.memory_matrix import (
    get_matrix,
    get_seen_registry,
    get_timeline,
)
from whitemagic.core.orchestration.bootstrap import bootstrap_session

_hub_instance: IntegrationHub | None = None
_hub_lock = Lock()


@dataclass
class SystemStatus:
    """Status of a system component."""

    name: str
    active: bool
    message: str
    details: dict[str, Any] = field(default_factory=dict)


class IntegrationHub:
    """The Grand Unified Interface.

    Activates and coordinates:
    - Memory Matrix (tracking what we've seen)
    - Session Bootstrap (auto-loading context)
    - Agentic Modules (13 brain upgrades)
    - Dashboard data export
    """

    def __init__(self) -> None:
        self.activated = False
        self.activation_time: str | None = None
        self.systems: dict[str, SystemStatus] = {}
        self.dashboard_data: dict[str, Any] = {}

    def activate_all(self) -> dict[str, Any]:
        """FULL SYSTEM ACTIVATION.

        Call this once at session start to:
        1. Bootstrap session context
        2. Activate all 13 agentic modules
        3. Initialize memory tracking
        4. Prepare dashboard data
        """
        now = datetime.now(timezone.utc)
        self.activation_time = now.isoformat()
        results = {}

        # 1. Session Bootstrap
        try:
            manifest = bootstrap_session()
            session_id = f"session_{manifest.started_at.strftime('%Y%m%d_%H%M%S')}"
            self.systems["session"] = SystemStatus(
                name="Session Bootstrap",
                active=True,
                message=f"Session {session_id} active",
                details={
                    "session_id": session_id,
                    "started_at": manifest.started_at.isoformat(),
                    "coherence_score": manifest.coherence_score,
                    "state": manifest.consciousness_state,
                },
            )
            results["session"] = {
                "id": session_id,
                "memories_loaded": len(manifest.memories_loaded),
                "in_progress": len(manifest.in_progress),
            }
        except Exception as e:
            self.systems["session"] = SystemStatus(
                name="Session Bootstrap", active=False, message=str(e),
            )
            results["session"] = {"error": str(e)}

        # 2. Memory Matrix
        try:
            matrix = get_matrix()
            registry = get_seen_registry()
            timeline = get_timeline()

            self.systems["memory_matrix"] = SystemStatus(
                name="Memory Matrix",
                active=True,
                message=f"Tracking {registry.stats()['total_files']} files",
                details=registry.stats(),
            )
            results["memory_matrix"] = {
                "files_tracked": registry.stats()["total_files"],
                "timeline_events": timeline.stats()["total_events"],
            }

            # Record this activation
            matrix.record_interaction(
                "system_activation",
                "IntegrationHub",
                {"components": ["session", "memory", "agentic"]},
            )
        except Exception as e:
            self.systems["memory_matrix"] = SystemStatus(
                name="Memory Matrix", active=False, message=str(e),
            )
            results["memory_matrix"] = {"error": str(e)}

        # 3. Agentic Brain Upgrades
        try:
            brain = full_brain_activation()
            self.systems["agentic"] = SystemStatus(
                name="Agentic Modules",
                active=True,
                message="13 brain upgrades active",
                details={"modules": list(brain.keys())},
            )
            results["agentic"] = {
                "modules_active": len(brain),
                "anti_loop": brain.get("2_anti_loop"),
                "parallel_mode": brain.get("5_parallel"),
            }
        except Exception as e:
            self.systems["agentic"] = SystemStatus(
                name="Agentic Modules", active=False, message=str(e),
            )
            results["agentic"] = {"error": str(e)}

        # 4. Anti-Loop Check
        try:
            anti_loop = get_anti_loop()
            anti_loop.record_iteration(["integration_hub:activate_all"])
            self.systems["anti_loop"] = SystemStatus(
                name="Anti-Loop Detector",
                active=True,
                message="Watching for cognitive loops",
            )
        except Exception:
            pass

        # 5. Prepare Dashboard Data
        self.dashboard_data = self._prepare_dashboard_data()

        self.activated = True
        return {
            "success": True,
            "activation_time": self.activation_time,
            "systems": {k: v.active for k, v in self.systems.items()},
            "results": results,
        }

    def _prepare_dashboard_data(self) -> dict[str, Any]:
        """Prepare data for dashboard visualization."""
        try:
            matrix = get_matrix()
            registry = get_seen_registry()
            timeline = get_timeline()

            # Get visualization data
            viz = matrix.export_for_visualization()

            # Get today's activity
            today = matrix.get_today_summary()

            # Get recent files by category
            recent = registry.get_recent(50)
            by_type: dict[str, list[dict[str, Any]]] = {}
            for entry in recent:
                ftype = entry.file_type
                if ftype not in by_type:
                    by_type[ftype] = []
                by_type[ftype].append({
                    "path": entry.path.replace(str(PROJECT_ROOT) + "/", ""),
                    "times_seen": entry.times_seen,
                    "last_seen": entry.last_seen,
                })

            # Get timeline summary
            timeline_stats = timeline.stats()

            return {
                "visualization": viz,
                "today": today,
                "files_by_type": by_type,
                "timeline": timeline_stats,
                "systems": {k: {"active": v.active, "message": v.message}
                           for k, v in self.systems.items()},
                "generated": datetime.now(timezone.utc).isoformat(),
            }
        except Exception as e:
            return {"error": str(e)}

    def get_status(self) -> dict[str, Any]:
        """Get current system status."""
        return {
            "activated": self.activated,
            "activation_time": self.activation_time,
            "systems": {k: {
                "active": v.active,
                "message": v.message,
            } for k, v in self.systems.items()},
            "memory_stats": get_seen_registry().stats() if self.activated else None,
        }

    def get_dashboard_data(self) -> dict[str, Any]:
        """Get data formatted for dashboard."""
        if not self.activated:
            self.activate_all()
        return self._prepare_dashboard_data()

    def check_loops(self) -> dict[str, Any]:
        """Check for cognitive loops."""
        try:
            anti_loop = get_anti_loop()
            return cast(dict[str, Any], anti_loop.get_stats())
        except Exception as e:
            return {"error": str(e)}

    def record_interaction(
        self,
        interaction_type: str,
        target: str,
        data: dict | None = None,
    ) -> None:
        """Record an interaction for memory tracking."""
        try:
            matrix = get_matrix()
            matrix.record_interaction(interaction_type, target, data)

            # Also check anti-loop
            anti_loop = get_anti_loop()
            anti_loop.record_iteration([f"{interaction_type}:{target}"])
        except Exception:
            pass


def get_hub() -> IntegrationHub:
    """Get singleton IntegrationHub instance."""
    global _hub_instance
    with _hub_lock:
        if _hub_instance is None:
            _hub_instance = IntegrationHub()
        return _hub_instance


def activate_all() -> dict[str, Any]:
    """Activate the full integration system."""
    hub = get_hub()
    return hub.activate_all()


def get_status() -> dict[str, Any]:
    """Get current system status."""
    hub = get_hub()
    return hub.get_status()
