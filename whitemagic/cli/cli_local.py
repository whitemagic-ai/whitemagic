# ruff: noqa: F403
"""Local Model CLI (Archived)

Relocated to:
`whitemagic/_archived/local_models/cli/cli_local.py`
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
    from whitemagic._archived.local_models.cli.cli_local import *  # type: ignore

