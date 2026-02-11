"""
Archived Wrapper: `whitemagic.local_ml.bitnet_inference`

The implementation was moved to:
`whitemagic._archived.local_models.local_ml.bitnet_inference`
"""

from __future__ import annotations

import os
from typing import Any, Dict


def _enabled() -> bool:
    return os.getenv("WHITEMAGIC_ENABLE_LOCAL_MODELS", "").strip().lower() in {
        "1",
        "true",
        "yes",
        "on",
    }


class BitNetInference:  # pragma: no cover - legacy shim
    def __init__(self, *_: Any, **__: Any) -> None:
        if not _enabled():
            raise RuntimeError(
                "BitNetInference is archived/disabled. "
                "Set WHITEMAGIC_ENABLE_LOCAL_MODELS=1 to enable legacy local-model support."
            )

        from whitemagic._archived.local_models.local_ml.bitnet_inference import (  # type: ignore[import-not-found]
            BitNetInference as _Impl,
        )

        self.__class__ = _Impl  # type: ignore[misc]
        _Impl.__init__(self)  # type: ignore[misc]


def get_bitnet_engine() -> Any:
    if not _enabled():
        raise RuntimeError(
            "BitNet is archived/disabled. "
            "Set WHITEMAGIC_ENABLE_LOCAL_MODELS=1 to enable legacy local-model support."
        )

    from whitemagic._archived.local_models.local_ml.bitnet_inference import (
        get_bitnet_engine as _impl,
    )

    return _impl()


class BitNetInferenceEngine(BitNetInference):
    """
    Backwards-compatible alias used by older API routes.
    """


def get_bitnet_status() -> Dict[str, Any]:
    if not _enabled():
        return {"available": False, "models": [], "archived": True}
    engine = get_bitnet_engine()
    try:
        return dict(engine.get_status())
    except Exception as exc:  # pragma: no cover - best effort legacy shim
        return {"available": False, "error": str(exc), "archived": True}
