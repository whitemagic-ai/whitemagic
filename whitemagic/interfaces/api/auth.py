# ruff: noqa: F403
"""API authentication (Deprecated - use auth_unified instead)."""

import warnings

from .auth_unified import *

warnings.warn(
    "whitemagic.interfaces.api.auth is deprecated and will be removed in v13. "
    "Use whitemagic.interfaces.api.auth_unified instead.",
    DeprecationWarning,
    stacklevel=2,
)
