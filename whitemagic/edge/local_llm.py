# ruff: noqa: F403
"""Edge Local LLM (Archived)
========================

This module called a locally-running Ollama instance as part of the edge cascade.

The implementation has been physically relocated to:
`whitemagic/_archived/local_models/edge/local_llm.py`
"""

from __future__ import annotations

import os
from dataclasses import dataclass
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
        "Local LLM execution is archived/disabled. "
        "Set WHITEMAGIC_ENABLE_LOCAL_MODELS=1 to enable legacy local-model support.",
    )


if _enabled():  # pragma: no cover - legacy opt-in path
    from whitemagic._archived.local_models.edge.local_llm import *  # type: ignore
else:
    @dataclass
    class CascadeResult:  # pragma: no cover - legacy shim
        query: str
        answer: str
        method: str
        confidence: float
        latency_ms: float
        tokens_saved: int = 0

    class LocalLLM:  # pragma: no cover - legacy shim
        def __init__(self, *_: Any, **__: Any) -> None:
            raise _disabled_error()

    class CascadingInference:  # pragma: no cover - legacy shim
        def __init__(self, *_: Any, **__: Any) -> None:
            raise _disabled_error()

    def get_cascade() -> Any:
        raise _disabled_error()

