"""Backward-compatibility shim — canonical location is whitemagic.core.acceleration.haskell_interface."""
import warnings as _w
_w.warn(
    "whitemagic.bridges.haskell.interface is deprecated; use whitemagic.core.acceleration.haskell_interface",
    DeprecationWarning,
    stacklevel=2,
)
from whitemagic.core.acceleration.haskell_interface import *  # noqa: F401,F403,E402
