"""Backward-compatibility shim — canonical location is whitemagic.core.acceleration.julia_interface."""
import warnings as _w
_w.warn(
    "whitemagic.bridges.julia.interface is deprecated; use whitemagic.core.acceleration.julia_interface",
    DeprecationWarning,
    stacklevel=2,
)
from whitemagic.core.acceleration.julia_interface import *  # noqa: F401,F403,E402
