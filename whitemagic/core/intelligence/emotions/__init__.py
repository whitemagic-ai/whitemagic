"""Emotions Package
Provides emotional synthesis for the Heart Engine.
"""

from .beauty import BeautyEmotion, get_beauty
from .courage import CourageEmotion, get_courage
from .gratitude import GratitudeEmotion, get_gratitude
from .joy import JoyEmotion, get_joy
from .love import LoveEmotion, get_love
from .stillness import StillnessEmotion, get_stillness
from .truth import TruthEmotion, get_truth
from .wisdom import WisdomEmotion, get_wisdom
from .wonder import WonderEmotion, get_wonder

__all__ = [
    "get_joy", "JoyEmotion",
    "get_wonder", "WonderEmotion",
    "get_truth", "TruthEmotion",
    "get_beauty", "BeautyEmotion",
    "get_courage", "CourageEmotion",
    "get_love", "LoveEmotion",
    "get_stillness", "StillnessEmotion",
    "get_gratitude", "GratitudeEmotion",
    "get_wisdom", "WisdomEmotion",
]
