"""Autonomous systems for WhiteMagic.

Subpackages:
    autonomous.executor  — Multi-step objective execution engine
                           (merged from autonomous_execution/ in v13.0.0)

Modules:
    diary           — Hourly / breakthrough logging
    maintenance     — Self-healing & autonomous maintenance
    self_prompting  — Autonomous work queue + human-in-the-loop questions
    depth_gauge     — Execution depth tracking
    token_economy   — Token budget management
    yin_controller  — Yin-Yang pacing for autonomous loops
"""

from .diary import (
    DiarySystem,
    get_diary,
    log_breakthrough,
    log_hourly,
    session_end,
    session_start,
)

# Re-export executor subpackage top-level names for convenience
from .executor import (
    ContinuousExecutor,
    ExecutorConfig,
    Objective,
    ObjectiveGenerator,
    ProgressAssessor,
)
from .maintenance import (
    AutonomousMaintenance,
    auto_heal,
    get_maintenance,
    run_maintenance,
)
from .self_prompting import ask_human, process_queue, queue_work

__all__ = [
    # daemon capabilities
    "queue_work", "process_queue", "ask_human",
    "get_diary", "log_hourly", "log_breakthrough",
    "session_start", "session_end", "DiarySystem",
    "get_maintenance", "run_maintenance", "auto_heal",
    "AutonomousMaintenance",
    # executor (merged from autonomous_execution)
    "ContinuousExecutor", "ExecutorConfig", "Objective",
    "ObjectiveGenerator", "ProgressAssessor",
]
