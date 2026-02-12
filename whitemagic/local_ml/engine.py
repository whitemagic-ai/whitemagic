"""
Archived Wrapper: `whitemagic.local_ml.engine`

The implementation was moved to:
`whitemagic._archived.local_models.local_ml.engine`
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


class LocalMLEngine:  # pragma: no cover - legacy shim
    def __init__(self, *_: Any, **__: Any) -> None:
        if not _enabled():
            raise RuntimeError(
                "LocalMLEngine is archived/disabled. "
                "Set WHITEMAGIC_ENABLE_LOCAL_MODELS=1 to enable legacy local-model support."
            )

        from whitemagic._archived.local_models.local_ml.engine import LocalMLEngine as _Impl  # type: ignore[import-not-found]

        # Delegate by swapping class at runtime.
        self.__class__ = _Impl  # type: ignore[misc]
        _Impl.__init__(self)  # type: ignore[misc]


def get_local_ml_engine() -> Any:
    if not _enabled():
        raise RuntimeError(
            "Local ML is archived/disabled. "
            "Set WHITEMAGIC_ENABLE_LOCAL_MODELS=1 to enable legacy local-model support."
        )

    from whitemagic._archived.local_models.local_ml.engine import get_local_ml_engine as _impl  # type: ignore[import-not-found]

    return _impl()
