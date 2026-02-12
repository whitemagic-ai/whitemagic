"""WhiteMagic Automation Module
============================

Autonomous processes that run without human intervention.
Humans are "on" the loop, not "in" it.
"""

from .daemon import (
    AutomationDaemon,
    AutomationTask,
    ResonanceCascade,
    TaskFrequency,
    TaskPriority,
    get_automation_daemon,
)

__all__ = [
    "AutomationDaemon",
    "AutomationTask",
    "ResonanceCascade",
    "TaskFrequency",
    "TaskPriority",
    "get_automation_daemon",
]
