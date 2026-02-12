# ruff: noqa: F403
"""Compatibility shim for whitemagic.core.gardens.

The gardens package has been moved to whitemagic.gardens for canonicalization.
This module re-exports whitemagic.gardens for backward compatibility.
"""

from whitemagic.gardens import *
