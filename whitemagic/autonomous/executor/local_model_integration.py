# ruff: noqa: F403
"""Autonomous Execution Local-Model Integration (Archived)

Relocated to:
`whitemagic/_archived/local_models/autonomous_execution/local_model_integration.py`

`ContinuousExecutor.execute_complex_task()` already treats this integration as
optional and will fall back gracefully when unavailable.
"""

from __future__ import annotations

import os


def _enabled() -> bool:
    return os.getenv("WHITEMAGIC_ENABLE_LOCAL_MODELS", "").strip().lower() in {
        "1",
        "true",
        "yes",
        "on",
    }


if _enabled():  # pragma: no cover - legacy opt-in path
    from whitemagic._archived.local_models.autonomous_execution.local_model_integration import *  # type: ignore

