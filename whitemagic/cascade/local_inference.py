# ruff: noqa: F403
"""Cascade Local Inference (Archived)
=================================

This module implemented memory-grounded generation (RAG) using local models.

The implementation has been physically relocated to:
`whitemagic/_archived/local_models/cascade/local_inference.py`
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
        "Cascade local inference is archived/disabled. "
        "Set WHITEMAGIC_ENABLE_LOCAL_MODELS=1 to enable legacy local-model support.",
    )


if _enabled():  # pragma: no cover - legacy opt-in path
    from whitemagic._archived.local_models.cascade.local_inference import *  # type: ignore
else:
    async def memory_grounded_generate(*_: Any, **__: Any) -> Any:
        raise _disabled_error()

    async def quick_recall(*_: Any, **__: Any) -> str:
        raise _disabled_error()

    async def analyze_with_context(*_: Any, **__: Any) -> dict[str, Any]:
        raise _disabled_error()

