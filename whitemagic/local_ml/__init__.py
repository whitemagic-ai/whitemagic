"""
Local ML (Archived)
===================

This package used to embed local model execution (BitNet/Ollama/etc) directly
inside Whitemagic.

Whitemagic's current direction is to be model-agnostic and expose capabilities
via MCP/REST so *external* models (local or cloud) can use it as a cognitive and
memory substrate.

The implementation has been physically relocated to:
`whitemagic/_archived/local_models/local_ml/`

To re-enable the archived local-model stack:
- set `WHITEMAGIC_ENABLE_LOCAL_MODELS=1` before importing/starting Whitemagic
"""

from __future__ import annotations

import os
from typing import Any, Dict, cast


def _enabled() -> bool:
    return os.getenv("WHITEMAGIC_ENABLE_LOCAL_MODELS", "").strip().lower() in {
        "1",
        "true",
        "yes",
        "on",
    }


def _disabled_error() -> RuntimeError:
    return RuntimeError(
        "Local model execution is archived/disabled. "
        "Set WHITEMAGIC_ENABLE_LOCAL_MODELS=1 to enable legacy local-model support."
    )


def get_model_info() -> Dict[str, object]:
    if not _enabled():
        return {"available": False, "models": [], "backend": "none", "archived": True}

    from whitemagic._archived.local_models.local_ml import get_model_info as _impl  # type: ignore[import-not-found]

    result = _impl()
    if isinstance(result, dict):
        return cast("Dict[str, object]", result)
    return {"available": False, "models": [], "backend": "none", "archived": True}


def get_local_ml_engine() -> Any:
    if not _enabled():
        raise _disabled_error()

    from whitemagic._archived.local_models.local_ml.engine import get_local_ml_engine as _impl  # type: ignore[import-not-found]

    return _impl()


def get_engine() -> Any:
    if not _enabled():
        raise _disabled_error()

    from whitemagic._archived.local_models.local_ml.inference_engine import get_engine as _impl  # type: ignore[import-not-found]

    return _impl()


__all__ = [
    "get_model_info",
    "get_local_ml_engine",
    "get_engine",
]
