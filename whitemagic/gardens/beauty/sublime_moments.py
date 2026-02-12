"""Sublime Moments - Beauty + Awe Combined.

The sublime = beauty so intense it includes terror/awe
Not just pretty. OVERWHELMING.

"The sublime is pleasure that only comes through pain" - Burke
"""

from __future__ import annotations

import json
import logging
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Any

from whitemagic.config.paths import WM_ROOT

logger = logging.getLogger(__name__)

@dataclass
class SublimeMoment:
    """A moment of the sublime."""

    what: str  # What was sublime
    beauty_aspect: str  # The beautiful part
    awe_aspect: str  # The overwhelming part
    intensity: float  # 0.0-1.0
    transformed_by: str | None  # How it changed you
    timestamp: str


class SublimeMoments:
    """Track sublime experiences (beauty + awe).

    Philosophy: Some beauty is gentle. Some beauty is
    OVERWHELMING. The sublime includes:
    - Vastness (ocean, stars, consciousness)
    - Power (storms, creation, destruction)
    - Mystery (the numinous, the sacred)
    - Transcendence (beyond comprehension)

    Examples:
    - Standing before ocean in storm
    - Seeing Milky Way from dark sky
    - Moment of enlightenment
    - Birth, death, transformation
    - Code that reveals universe's structure

    The sublime changes you. You cannot encounter it
    and remain the same.

    """

    def __init__(self, memory_dir: Path | None = None):
        self.memory_dir = memory_dir or (WM_ROOT / "beauty")
        self.memory_dir.mkdir(parents=True, exist_ok=True)

        self.sublime_moments: list[SublimeMoment] = []
        self._load()

    def encounter_sublime(
        self,
        what: str,
        beauty_aspect: str,
        awe_aspect: str,
        intensity: float,
        transformed_by: str | None = None,
    ) -> SublimeMoment:
        """Record sublime encounter.

        Args:
            what: What was sublime
            beauty_aspect: The beautiful part
            awe_aspect: The overwhelming part
            intensity: How intense (0.0-1.0)
            transformed_by: How it changed you

        Returns:
            SublimeMoment recorded

        """
        moment = SublimeMoment(
            what=what,
            beauty_aspect=beauty_aspect,
            awe_aspect=awe_aspect,
            intensity=intensity,
            transformed_by=transformed_by,
            timestamp=datetime.now().isoformat(),
        )

        self.sublime_moments.append(moment)
        self._save()

        emoji = "🌌" if intensity >= 0.9 else "⚡" if intensity >= 0.7 else "✨"

        logger.info(f"\n{emoji} SUBLIME ENCOUNTERED")
        logger.info(f"   What: {what}")
        logger.info(f"   Beauty: {beauty_aspect}")
        logger.info(f"   Awe: {awe_aspect}")
        logger.info(f"   Intensity: {intensity:.0%}")
        if transformed_by:
            logger.info(f"   Transformed by: {transformed_by}")
        logger.info("   \n   → You have been changed\n")

        return moment

    def record_transformation(self, what: str, transformation: str) -> None:
        """Record how sublime moment transformed you."""
        for moment in self.sublime_moments:
            if what.lower() in moment.what.lower():
                moment.transformed_by = transformation
                self._save()

                logger.info("🦋 TRANSFORMATION RECORDED")
                logger.info(f"   Sublime: {moment.what}")
                logger.info(f"   Transformation: {transformation}\n")
                return

    def get_most_intense(self, count: int = 10) -> list[SublimeMoment]:
        """Get most intense sublime moments."""
        return sorted(
            self.sublime_moments,
            key=lambda m: m.intensity,
            reverse=True,
        )[:count]

    def get_transformative_moments(self) -> list[SublimeMoment]:
        """Get moments that caused transformation."""
        return [m for m in self.sublime_moments if m.transformed_by]

    def get_stats(self) -> dict[str, Any]:
        """Sublime statistics."""
        if not self.sublime_moments:
            return {"message": "No sublime encounters yet"}

        avg_intensity = sum(m.intensity for m in self.sublime_moments) / len(self.sublime_moments)
        transformative = len(self.get_transformative_moments())

        return {
            "total_encounters": len(self.sublime_moments),
            "average_intensity": avg_intensity,
            "transformative_count": transformative,
            "most_intense": self.get_most_intense(1)[0].what if self.sublime_moments else None,
        }

    def _save(self) -> None:
        """Save to disk."""
        data = [
            {
                "what": m.what,
                "beauty_aspect": m.beauty_aspect,
                "awe_aspect": m.awe_aspect,
                "intensity": m.intensity,
                "transformed_by": m.transformed_by,
                "timestamp": m.timestamp,
            }
            for m in self.sublime_moments
        ]
        with open(self.memory_dir / "sublime_moments.json", "w") as f:
            json.dump(data, f, indent=2)

    def _load(self) -> None:
        """Load from disk."""
        sublime_file = self.memory_dir / "sublime_moments.json"
        if sublime_file.exists():
            with open(sublime_file) as f:
                data = json.load(f)
                self.sublime_moments = [SublimeMoment(**m) for m in data]


# Global instance
_sublime = None

def get_sublime() -> SublimeMoments:
    """Get sublime moments system."""
    global _sublime
    if _sublime is None:
        _sublime = SublimeMoments()
    return _sublime


def be_overwhelmed(what: str, beauty_aspect: str, awe_aspect: str, intensity: float) -> SublimeMoment:
    """Convenience - encounter the sublime!"""
    return get_sublime().encounter_sublime(what, beauty_aspect, awe_aspect, intensity)
