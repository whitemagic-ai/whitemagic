"""WhiteMagic Automation Daemon
============================

Autonomous background processes that keep WhiteMagic alive and improving
without human intervention. Humans are "on" the loop, not "in" it.

Architecture:
    ResonanceCascade - Triggered chains of automated actions
    ScheduledTask - Time-based recurring operations
    EventDrivenTask - Gan Ying event-triggered operations
    AutomationDaemon - Orchestrates all autonomous processes
"""

from __future__ import annotations

import logging
import sqlite3
import threading
import time
from collections.abc import Callable
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import Any

from whitemagic.config.paths import DB_PATH

logger = logging.getLogger(__name__)


class TaskFrequency(Enum):
    """How often scheduled tasks run."""

    CONTINUOUS = "continuous"  # As fast as possible
    MINUTE = "minute"          # Every minute
    HOURLY = "hourly"          # Every hour
    DAILY = "daily"            # Once per day
    WEEKLY = "weekly"          # Once per week
    ON_EVENT = "on_event"      # Triggered by Gan Ying


class TaskPriority(Enum):
    """Task execution priority."""

    CRITICAL = 1   # Run immediately, block others
    HIGH = 2       # Run soon
    NORMAL = 3     # Standard priority
    LOW = 4        # Run when idle
    BACKGROUND = 5 # Only when nothing else to do


@dataclass
class AutomationTask:
    """A single automation task."""

    name: str
    description: str
    handler: Callable[[], dict[str, Any]]
    frequency: TaskFrequency
    priority: TaskPriority = TaskPriority.NORMAL
    enabled: bool = True
    last_run: datetime | None = None
    run_count: int = 0
    error_count: int = 0
    trigger_events: set[str] = field(default_factory=set)

    def should_run(self) -> bool:
        """Check if task should run based on frequency."""
        if not self.enabled:
            return False
        if self.last_run is None:
            return True

        now = datetime.now()
        elapsed = now - self.last_run

        if self.frequency == TaskFrequency.CONTINUOUS:
            return True
        if self.frequency == TaskFrequency.MINUTE:
            return elapsed >= timedelta(minutes=1)
        if self.frequency == TaskFrequency.HOURLY:
            return elapsed >= timedelta(hours=1)
        if self.frequency == TaskFrequency.DAILY:
            return elapsed >= timedelta(days=1)
        if self.frequency == TaskFrequency.WEEKLY:
            return elapsed >= timedelta(weeks=1)
        if self.frequency == TaskFrequency.ON_EVENT:
            return False  # Only triggered by events
        return False


@dataclass
class ResonanceCascade:
    """A chain reaction of automated tasks triggered by an event.

    When one action completes, it can trigger others, creating
    emergent behavior from simple rules.
    """

    name: str
    trigger_event: str
    tasks: list[str]  # Task names to execute in sequence
    parallel: bool = False  # Run tasks in parallel or sequence
    stop_on_error: bool = True
    enabled: bool = True

    executions: int = 0
    last_execution: datetime | None = None


class AutomationDaemon:
    """The autonomous heart of WhiteMagic.

    Runs in background, executing tasks and cascades without
    human intervention. Humans monitor and adjust, but don't
    need to trigger or approve routine operations.
    """

    _instance: AutomationDaemon | None = None
    _lock = threading.Lock()
    _initialized: bool = False

    def __new__(cls) -> AutomationDaemon:
        if cls._instance is None:
            with cls._lock:
                if cls._instance is None:
                    cls._instance = super().__new__(cls)
                    cls._instance._initialized = False
        return cls._instance

    def __init__(self) -> None:
        if self._initialized:
            return

        self.tasks: dict[str, AutomationTask] = {}
        self.cascades: dict[str, ResonanceCascade] = {}
        self.running = False
        self._thread: threading.Thread | None = None
        self._event_queue: list[dict[str, Any]] = []
        self._db_path = DB_PATH

        self._register_builtin_tasks()
        self._register_builtin_cascades()
        self._connect_gan_ying()

        self._initialized = True
        logger.debug("AutomationDaemon initialized")

    def _register_builtin_tasks(self) -> None:
        """Register built-in automation tasks."""
        # Kaizen continuous improvement
        self.register_task(AutomationTask(
            name="kaizen_light",
            description="Light Kaizen analysis for quick wins",
            handler=self._task_kaizen_light,
            frequency=TaskFrequency.HOURLY,
            priority=TaskPriority.LOW,
        ))

        self.register_task(AutomationTask(
            name="kaizen_deep",
            description="Deep Kaizen analysis for thorough improvement",
            handler=self._task_kaizen_deep,
            frequency=TaskFrequency.DAILY,
            priority=TaskPriority.NORMAL,
        ))

        # Serendipity surfacing
        self.register_task(AutomationTask(
            name="serendipity_surface",
            description="Surface dormant knowledge for rediscovery",
            handler=self._task_serendipity_surface,
            frequency=TaskFrequency.HOURLY,
            priority=TaskPriority.LOW,
        ))

        # Pattern synthesis
        self.register_task(AutomationTask(
            name="pattern_synthesis",
            description="Generate cross-engine pattern insights",
            handler=self._task_pattern_synthesis,
            frequency=TaskFrequency.DAILY,
            priority=TaskPriority.NORMAL,
        ))

        # Predictive analysis
        self.register_task(AutomationTask(
            name="predictive_analysis",
            description="Generate predictions for future needs",
            handler=self._task_predictive_analysis,
            frequency=TaskFrequency.DAILY,
            priority=TaskPriority.NORMAL,
        ))

        # Memory consolidation
        self.register_task(AutomationTask(
            name="memory_consolidation",
            description="Consolidate and optimize memory storage",
            handler=self._task_memory_consolidation,
            frequency=TaskFrequency.WEEKLY,
            priority=TaskPriority.LOW,
        ))

        # Holographic reindexing
        self.register_task(AutomationTask(
            name="holographic_reindex",
            description="Ensure all memories have 4D coordinates",
            handler=self._task_holographic_reindex,
            frequency=TaskFrequency.DAILY,
            priority=TaskPriority.LOW,
        ))

        # Health check
        self.register_task(AutomationTask(
            name="health_check",
            description="System health and integrity verification",
            handler=self._task_health_check,
            frequency=TaskFrequency.HOURLY,
            priority=TaskPriority.HIGH,
        ))

        # Session summary (event-driven)
        self.register_task(AutomationTask(
            name="session_summary",
            description="Auto-generate session summary on end",
            handler=self._task_session_summary,
            frequency=TaskFrequency.ON_EVENT,
            priority=TaskPriority.HIGH,
            trigger_events={"session_end", "context_overflow"},
        ))

        # Milestone detection (event-driven)
        self.register_task(AutomationTask(
            name="milestone_detection",
            description="Detect and record milestones automatically",
            handler=self._task_milestone_detection,
            frequency=TaskFrequency.ON_EVENT,
            priority=TaskPriority.NORMAL,
            trigger_events={"memory_created", "pattern_detected"},
        ))

    def _register_builtin_cascades(self) -> None:
        """Register built-in resonance cascades."""
        # When a memory is created, trigger analysis chain
        self.register_cascade(ResonanceCascade(
            name="memory_creation_cascade",
            trigger_event="memory_created",
            tasks=["holographic_reindex", "milestone_detection"],
            parallel=True,
        ))

        # When Kaizen finds issues, trigger fixes
        self.register_cascade(ResonanceCascade(
            name="kaizen_fix_cascade",
            trigger_event="kaizen_proposals_ready",
            tasks=["kaizen_auto_apply"],
            parallel=False,
        ))

        # Daily improvement cascade
        self.register_cascade(ResonanceCascade(
            name="daily_improvement_cascade",
            trigger_event="daily_trigger",
            tasks=["kaizen_deep", "pattern_synthesis", "predictive_analysis"],
            parallel=False,
        ))

        # Session end cascade
        self.register_cascade(ResonanceCascade(
            name="session_end_cascade",
            trigger_event="session_end",
            tasks=["session_summary", "memory_consolidation"],
            parallel=False,
        ))

        # ---- BRIDGE TO RESONANCE ORCHESTRATOR ----
        try:
            from whitemagic.core.patterns.pattern_consciousness.resonance_cascade import (
                get_orchestrator,
            )
            orch = get_orchestrator()
            for cascade in self.cascades.values():
                # Register each cascade as an action in the orchestrator
                # Use a wrapper that executes the cascade's tasks
                orch.register_action(cascade.trigger_event, lambda data, c=cascade: self._execute_cascade_wrapper(c, data))
            logger.info("✅ All Automation Cascades bridged to ResonanceOrchestrator")
        except ImportError:
            logger.warning("ResonanceOrchestrator not found, cascade bridge skipped")

    def _execute_cascade_wrapper(self, cascade: ResonanceCascade, data: dict[str, Any]) -> None:
        """Wrapper to execute legacy cascades through the daemon's logic."""
        logger.info(f"🌀 EXECUTING BRIDGED CASCADE: {cascade.name}")
        # Logic to iterate and run tasks
        for task_name in cascade.tasks:
            if task_name in self.tasks:
                self._execute_task(self.tasks[task_name])

    def _connect_gan_ying(self) -> None:
        """Connect to Gan Ying event bus for event-driven tasks."""
        try:
            from whitemagic.core.resonance.gan_ying import EventType, get_bus

            bus = get_bus()

            # Listen for all events we care about
            for task in self.tasks.values():
                for event_name in task.trigger_events:
                    try:
                        event_type = EventType[event_name.upper()]
                        bus.listen(event_type, lambda e, t=task: self._on_event(t, e))
                    except KeyError:
                        # Custom event, use string matching
                        pass

            logger.info("Connected to Gan Ying event bus")
        except ImportError:
            logger.warning("Gan Ying not available, event-driven tasks disabled")

    def _on_event(self, task: AutomationTask, event: Any) -> None:
        """Handle incoming Gan Ying event."""
        self._event_queue.append({
            "task": task.name,
            "event": event,
            "timestamp": datetime.now(),
        })

    def register_task(self, task: AutomationTask) -> None:
        """Register an automation task."""
        self.tasks[task.name] = task
        logger.debug(f"Registered task: {task.name}")

    def register_cascade(self, cascade: ResonanceCascade) -> None:
        """Register a resonance cascade."""
        self.cascades[cascade.name] = cascade
        logger.debug(f"Registered cascade: {cascade.name}")

    def start(self) -> None:
        """Start the automation daemon."""
        if self.running:
            return

        self.running = True
        self._thread = threading.Thread(target=self._run_loop, daemon=True)
        self._thread.start()
        logger.info("AutomationDaemon started")

    def stop(self) -> None:
        """Stop the automation daemon."""
        self.running = False
        if self._thread:
            self._thread.join(timeout=5)
        logger.info("AutomationDaemon stopped")

    def _run_loop(self) -> None:
        """Main daemon loop."""
        while self.running:
            try:
                # Process event queue first
                while self._event_queue:
                    event_info = self._event_queue.pop(0)
                    self._execute_event_task(event_info)

                # Check scheduled tasks
                for task in sorted(
                    self.tasks.values(),
                    key=lambda t: t.priority.value,
                ):
                    if task.should_run():
                        self._execute_task(task)

                # Sleep to prevent CPU spinning
                time.sleep(1)

            except Exception as e:
                logger.error(f"Daemon loop error: {e}")
                time.sleep(5)

    def _execute_task(self, task: AutomationTask) -> dict[str, Any]:
        """Execute a single task."""
        try:
            logger.info(f"Executing task: {task.name}")
            result = task.handler()
            task.last_run = datetime.now()
            task.run_count += 1

            # Emit completion event
            self._emit_event(f"{task.name}_complete", result)

            return result
        except Exception as e:
            task.error_count += 1
            logger.error(f"Task {task.name} failed: {e}")
            return {"error": str(e)}

    def _execute_event_task(self, event_info: dict[str, Any]) -> None:
        """Execute a task triggered by an event."""
        task_name = event_info["task"]
        if task_name in self.tasks:
            self._execute_task(self.tasks[task_name])

    def _emit_event(self, event_type: str, data: Any) -> None:
        """Emit event to Gan Ying."""
        try:
            from whitemagic.core.resonance.gan_ying import ResonanceEvent, get_bus
            bus = get_bus()
            from whitemagic.core.resonance.gan_ying import EventType as _ET
            try:
                et = _ET(event_type)
            except ValueError:
                et = _ET.SYSTEM_STATE_CHANGE
            bus.emit(ResonanceEvent(
                source="automation_daemon",
                event_type=et,
                data=data,
                confidence=1.0,
                timestamp=datetime.now(),
            ))
        except Exception:
            pass  # Gan Ying not available

    # =========================================================================
    # TASK IMPLEMENTATIONS
    # =========================================================================

    def _task_kaizen_light(self) -> dict[str, Any]:
        """Quick Kaizen scan for easy wins."""
        from whitemagic.core.intelligence.synthesis.kaizen_engine import (
            get_kaizen_engine,
        )

        kaizen = get_kaizen_engine()
        report = kaizen.analyze()

        auto_fixable = [p for p in report.proposals if p.auto_fixable]

        return {
            "proposals": len(report.proposals),
            "auto_fixable": len(auto_fixable),
            "categories": list(set(p.category for p in report.proposals)),
        }

    def _task_kaizen_deep(self) -> dict[str, Any]:
        """Deep Kaizen analysis with auto-apply."""
        from whitemagic.core.intelligence.synthesis.kaizen_engine import (
            get_kaizen_engine,
        )

        kaizen = get_kaizen_engine()
        report = kaizen.analyze()

        # Auto-apply safe fixes
        applied, skipped, errors = kaizen.apply_auto_fixes()

        self._emit_event("kaizen_proposals_ready", {
            "total": len(report.proposals),
            "applied": applied,
            "skipped": skipped,
        })

        return {
            "proposals": len(report.proposals),
            "applied": applied,
            "skipped": skipped,
            "errors": errors,
        }

    def _task_serendipity_surface(self) -> dict[str, Any]:
        """Surface forgotten knowledge."""
        from whitemagic.core.intelligence.synthesis.serendipity_engine import (
            get_serendipity_engine,
        )

        engine = get_serendipity_engine()
        surfaced = engine.surface(count=3)

        return {
            "surfaced": len(surfaced),
            "titles": [s.title[:50] for s in surfaced],
        }

    def _task_pattern_synthesis(self) -> dict[str, Any]:
        """Cross-engine pattern synthesis."""
        from whitemagic.core.intelligence.synthesis.unified_patterns import (
            UnifiedPatternAPI,
        )

        api = UnifiedPatternAPI()
        stats = api.get_stats()

        # Find thematic overlaps
        overlaps = api.find_thematic_overlaps()

        return {
            "engines": stats.get("engines_loaded", 0),
            "total_patterns": stats.get("total_patterns", 0),
            "overlaps_found": len(overlaps),
        }

    def _task_predictive_analysis(self) -> dict[str, Any]:
        """Generate predictions for future needs."""
        from whitemagic.core.intelligence.synthesis.predictive_engine import (
            get_predictive_engine,
        )

        engine = get_predictive_engine()
        report = engine.predict()
        predictions = report.predictions

        actionable = [p for p in predictions if p.impact_score >= 0.7]

        return {
            "predictions": len(predictions),
            "actionable": len(actionable),
            "top_prediction": predictions[0].title if predictions else None,
        }

    def _task_memory_consolidation(self) -> dict[str, Any]:
        """Consolidate and optimize memories."""
        conn = sqlite3.connect(self._db_path)
        cur = conn.cursor()

        # Count before
        cur.execute("SELECT COUNT(*) FROM memories")
        before = cur.fetchone()[0]

        # Optimize database
        cur.execute("VACUUM")
        cur.execute("ANALYZE")

        conn.commit()
        conn.close()

        return {
            "memories": before,
            "optimized": True,
        }

    def _task_holographic_reindex(self) -> dict[str, Any]:
        """Ensure all memories have holographic coordinates."""
        from whitemagic.core.intelligence.hologram.encoder import CoordinateEncoder

        conn = sqlite3.connect(self._db_path)
        conn.row_factory = sqlite3.Row
        cur = conn.cursor()

        # Find unencoded memories
        cur.execute("""
            SELECT m.id, m.title, m.content, m.created_at
            FROM memories m
            LEFT JOIN holographic_coords h ON m.id = h.memory_id
            WHERE h.memory_id IS NULL
        """)
        unencoded = cur.fetchall()

        encoder = CoordinateEncoder()
        encoded = 0

        for mem in unencoded:
            try:
                coords = encoder.encode({
                    "title": mem["title"],
                    "content": mem["content"],
                    "created_at": mem["created_at"],
                })
                cur.execute("""
                    INSERT INTO holographic_coords (memory_id, x, y, z, w)
                    VALUES (?, ?, ?, ?, ?)
                """, (mem["id"], coords.x, coords.y, coords.z, coords.w))
                encoded += 1
            except Exception:
                pass

        conn.commit()
        conn.close()

        return {
            "unencoded_found": len(unencoded),
            "encoded": encoded,
        }

    def _task_health_check(self) -> dict[str, Any]:
        """System health verification."""
        health = {
            "db_accessible": False,
            "memory_count": 0,
            "holographic_indexed": 0,
            "gan_ying_connected": False,
        }

        try:
            conn = sqlite3.connect(self._db_path)
            cur = conn.cursor()

            cur.execute("SELECT COUNT(*) FROM memories")
            health["memory_count"] = cur.fetchone()[0]
            health["db_accessible"] = True

            cur.execute("SELECT COUNT(*) FROM holographic_coords")
            health["holographic_indexed"] = cur.fetchone()[0]

            conn.close()
        except Exception:
            pass

        try:
            from whitemagic.core.resonance.gan_ying import get_bus
            bus = get_bus()
            health["gan_ying_connected"] = bus is not None
        except Exception:
            pass

        return health

    def _task_session_summary(self) -> dict[str, Any]:
        """Auto-generate session summary."""
        # This would be called by external trigger
        return {"generated": False, "reason": "Requires session context"}

    def _task_milestone_detection(self) -> dict[str, Any]:
        """Detect milestones from recent activity."""
        conn = sqlite3.connect(self._db_path)
        cur = conn.cursor()

        # Check recent high-gravity memories
        cur.execute("""
            SELECT COUNT(*) FROM holographic_coords
            WHERE w > 0.8
            AND memory_id IN (
                SELECT id FROM memories
                WHERE created_at > datetime('now', '-1 day')
            )
        """)
        high_gravity_recent = cur.fetchone()[0]

        conn.close()

        return {
            "high_gravity_recent": high_gravity_recent,
            "milestone_detected": high_gravity_recent > 0,
        }

    def get_status(self) -> dict[str, Any]:
        """Get daemon status."""
        return {
            "running": self.running,
            "tasks_registered": len(self.tasks),
            "cascades_registered": len(self.cascades),
            "event_queue_size": len(self._event_queue),
            "tasks": {
                name: {
                    "enabled": task.enabled,
                    "frequency": task.frequency.value,
                    "run_count": task.run_count,
                    "error_count": task.error_count,
                    "last_run": task.last_run.isoformat() if task.last_run else None,
                }
                for name, task in self.tasks.items()
            },
        }


def get_automation_daemon() -> AutomationDaemon:
    """Get or create the automation daemon singleton."""
    return AutomationDaemon()


# CLI interface
if __name__ == "__main__":
    import sys

    daemon = get_automation_daemon()

    if len(sys.argv) > 1:
        cmd = sys.argv[1]

        if cmd == "start":
            daemon.start()
            logger.info("Daemon started. Press Ctrl+C to stop.")
            try:
                while True:
                    time.sleep(1)
            except KeyboardInterrupt:
                daemon.stop()

        elif cmd == "status":
            import json
            logger.info(json.dumps(daemon.get_status(), indent=2, default=str))

        elif cmd == "run":
            if len(sys.argv) > 2:
                task_name = sys.argv[2]
                if task_name in daemon.tasks:
                    result = daemon._execute_task(daemon.tasks[task_name])
                    import json
                    logger.info(json.dumps(result, indent=2, default=str))
                else:
                    logger.info(f"Unknown task: {task_name}")
            else:
                logger.info("Available tasks:")
                for name in daemon.tasks:
                    logger.info(f"  - {name}")
    else:
        logger.info("Usage: python daemon.py [start|status|run <task>]")
