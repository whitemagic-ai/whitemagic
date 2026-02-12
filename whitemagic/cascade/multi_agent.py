# ruff: noqa: F403
"""Cascade Multi-Agent (Archived)
==============================

This module enabled multi-agent collaboration driven by embedded local models.

The implementation has been physically relocated to:
`whitemagic/_archived/local_models/cascade/multi_agent.py`
"""

from __future__ import annotations

import os
from typing import Any


def _enabled() -> bool:
    return os.getenv("WHITEMAGIC_ENABLE_LOCAL_MODELS", "").strip().lower() in {
        "1",
        "true",
        "yes",
        "on",
    }


def _disabled_error() -> RuntimeError:
    return RuntimeError(
        "Multi-agent local-model support is archived/disabled. "
        "Set WHITEMAGIC_ENABLE_LOCAL_MODELS=1 to enable legacy local-model support.",
    )


if _enabled():  # pragma: no cover - legacy opt-in path
    from whitemagic._archived.local_models.cascade.multi_agent import *  # type: ignore
else:
    class LocalModelAgent:  # pragma: no cover - legacy shim
        def __init__(self, *_: Any, **__: Any) -> None:
            raise _disabled_error()

    async def create_agent_team(*_: Any, **__: Any) -> Any:
        raise _disabled_error()

