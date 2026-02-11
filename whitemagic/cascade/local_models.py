# ruff: noqa: F403
"""Cascade Local Models (Archived)
==============================

This module previously provided an abstraction layer over local LLM backends
(Ollama, LM Studio, etc.) for Whitemagic's cascade/orchestration features.

The implementation has been physically relocated to:
`whitemagic/_archived/local_models/cascade/local_models.py`

Whitemagic's preferred architecture is now:
External model ↔ (MCP/REST) ↔ Whitemagic tools/memory
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
        "Cascade local model support is archived/disabled. "
        "Set WHITEMAGIC_ENABLE_LOCAL_MODELS=1 to enable legacy local-model support.",
    )


if _enabled():  # pragma: no cover - legacy opt-in path
    from whitemagic._archived.local_models.cascade.local_models import *  # type: ignore
else:
    # Minimal stubs so imports don't crash; callers get a clear runtime error.
    class LocalModelManager:  # pragma: no cover - legacy shim
        def __init__(self, *_: Any, **__: Any) -> None:
            raise _disabled_error()

    async def setup_manager_from_config(*_: Any, **__: Any) -> Any:
        raise _disabled_error()

    async def setup_default_manager(*_: Any, **__: Any) -> Any:
        raise _disabled_error()

