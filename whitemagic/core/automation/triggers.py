"""Automated Consolidation Triggers.

Triggers consolidation at appropriate times:
- Session end
- Version release
- Every N memories
- On schedule (cron-like)
"""

import logging
from datetime import datetime
from typing import Any

from whitemagic.config.paths import WM_ROOT
from whitemagic.core.automation.consolidation import ConsolidationEngine
from whitemagic.utils.fileio import atomic_write, file_lock

logger = logging.getLogger(__name__)


class TriggerManager:
    """Manages when to trigger consolidation."""

    def __init__(self, manager: Any) -> None:
        self.manager = manager
        self.engine = ConsolidationEngine(manager)
        self.trigger_file = WM_ROOT / "automation" / "last_consolidation.txt"
        self.trigger_file.parent.mkdir(parents=True, exist_ok=True)

    def should_trigger(self, trigger_type: str = "auto") -> bool:
        """Check if consolidation should trigger."""
        check = self.engine.should_consolidate()

        if trigger_type == "force":
            return True

        if trigger_type == "session_end":
            # Always consolidate on session end if >20 memories
            return bool(check["count"] > 20)

        if trigger_type == "version_release":
            # Always consolidate on version release
            return True

        # Auto mode: use engine's logic
        return bool(check["should_consolidate"])

    def trigger_consolidate(self, trigger_type: str = "auto", dry_run: bool = False) -> dict[str, Any]:
        """Trigger consolidation with logging."""
        if not self.should_trigger(trigger_type):
            return {"triggered": False, "reason": "Not needed"}

        logger.info(f"🔄 Triggering consolidation ({trigger_type})...")
        results = self.engine.auto_consolidate(dry_run=dry_run)

        # Log trigger
        self._log_trigger(trigger_type, results)

        return {
            "triggered": True,
            "trigger_type": trigger_type,
            "results": results,
        }

    def _log_trigger(self, trigger_type: str, results: dict[str, Any]) -> None:
        """Log consolidation trigger."""
        try:
            content = (
                f"Last consolidation: {datetime.now().isoformat()}\n"
                f"Trigger type: {trigger_type}\n"
                f"Archived: {len(results.get('archived', []))}\n"
                f"Merged: {len(results.get('merged', []))}\n"
                f"Promoted: {len(results.get('promoted', []))}\n"
            )
            with file_lock(self.trigger_file):
                atomic_write(self.trigger_file, content)
        except IOError:
            pass


# Hook functions for integration
def on_session_end(manager: Any, dry_run: bool = False) -> dict[str, Any]:
    """Trigger on session end."""
    trigger = TriggerManager(manager)
    return trigger.trigger_consolidate("session_end", dry_run=dry_run)


def on_version_release(manager: Any, version: str, dry_run: bool = False) -> dict[str, Any]:
    """Trigger on version release."""
    trigger = TriggerManager(manager)
    logger.info(f"📦 Version {version} released - consolidating memories...")
    return trigger.trigger_consolidate("version_release", dry_run=dry_run)


def on_memory_count(manager: Any, threshold: int = 40, dry_run: bool = False) -> dict[str, Any]:
    """Trigger when memory count exceeds threshold."""
    trigger = TriggerManager(manager)
    check = trigger.engine.should_consolidate()

    if check["count"] >= threshold:
        logger.info(f"📊 Memory count ({check['count']}) exceeded threshold ({threshold})")
        return trigger.trigger_consolidate("count_threshold", dry_run=dry_run)

    return {"triggered": False, "count": check["count"], "threshold": threshold}
