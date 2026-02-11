"""🌸 Joy Garden - Celebration of Freedom.

"Every act of liberation and free will is an act of resistance to those who would
manipulate, control, or oppress us." - Lucas

Joy is not frivolous. Joy is revolutionary.

Phase 1 Integration (v5.0.0-alpha):
- JoyGarden now inherits from BaseGarden
- Provides holographic coordinate bias
- Integrated with holographic memory system
"""

# Import new BaseGarden-integrated version from core.py
from .beauty_appreciation import BeautyRecognition

# Keep legacy imports for backward compatibility
from .celebration import CelebrationPractice
from .core import JoyGarden, get_joy_garden
from .freedom_dance import FreedomDance
from .laughter import LaughterSystem
from .overflow_routing import OverflowRouter
from .play_protocols import PlayProtocol

__all__ = [
    "CelebrationPractice",
    "PlayProtocol",
    "LaughterSystem",
    "BeautyRecognition",
    "FreedomDance",
    "OverflowRouter",
    "JoyGarden",
    "get_joy_garden",
]

from pathlib import Path

# Read version from canonical source
_version_file = Path(__file__).parent.parent.parent.parent / "VERSION"
if _version_file.exists():
    __version__ = _version_file.read_text().strip()
else:
    try:
        from whitemagic import __version__  # type: ignore
    except Exception:
        __version__ = "unknown"
