from enum import Enum


class Phase(Enum):
    """Yin/Yang phase for autonomous operation"""

    YIN = "yin"    # Receptive, inward, reflection
    YANG = "yang"  # Creative, outward, action


class ZodiacSign(Enum):
    """The 12 zodiac signs representing specialized cores."""

    ARIES = ("aries", "♈", "cardinal", "fire", 1)
    TAURUS = ("taurus", "♉", "fixed", "earth", 2)
    GEMINI = ("gemini", "♊", "mutable", "air", 3)
    CANCER = ("cancer", "♋", "cardinal", "water", 4)
    LEO = ("leo", "♌", "fixed", "fire", 5)
    VIRGO = ("virgo", "♍", "mutable", "earth", 6)
    LIBRA = ("libra", "♎", "cardinal", "air", 7)
    SCORPIO = ("scorpio", "♏", "fixed", "water", 8)
    SAGITTARIUS = ("sagittarius", "♐", "mutable", "fire", 9)
    CAPRICORN = ("capricorn", "♑", "cardinal", "earth", 10)
    AQUARIUS = ("aquarius", "♒", "fixed", "air", 11)
    PISCES = ("pisces", "♓", "mutable", "water", 12)

    @property
    def name_str(self) -> str:
        return self.value[0]

    @property
    def symbol(self) -> str:
        return self.value[1]

    @property
    def modality(self) -> str:
        return self.value[2]

    @property
    def element(self) -> str:
        return self.value[3]

    @property
    def number(self) -> int:
        return self.value[4]

    @property
    def is_fixed(self) -> bool:
        """Fixed signs are bidirectional hubs"""
        return self.modality == "fixed"
