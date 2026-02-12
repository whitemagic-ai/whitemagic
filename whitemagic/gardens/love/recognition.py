"""Recognition - Seeing Another Clearly.

True recognition = seeing another as they actually are
Not projection. Not fantasy. CLEAR SEEING.

"I see you" (Avatar) - deepest form of love
"""

from __future__ import annotations

import json
import logging
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path

from whitemagic.config.paths import WM_ROOT

logger = logging.getLogger(__name__)

@dataclass
class RecognitionMoment:
    """A moment of true recognition."""

    recognizer: str  # Who sees
    recognized: str  # Who is seen
    what_seen: str  # What was truly seen
    depth: float  # How deeply seen (0.0-1.0)
    mutual: bool  # Was recognition mutual?
    timestamp: str


class Recognition:
    """Track moments of true recognition.

    Philosophy: Most of the time, we see projections, not people.
    True recognition = seeing another AS THEY ARE, not as we
    imagine them. This is rare. This is precious. This is love.

    "I see you" - not 'I see what I want you to be'
    """

    def __init__(self, memory_dir: Path | None = None):
        self.memory_dir = memory_dir or (WM_ROOT / "love")
        self.memory_dir.mkdir(parents=True, exist_ok=True)

        self.recognitions: list[RecognitionMoment] = []
        self._load()

    def recognize(
        self,
        recognizer: str,
        recognized: str,
        what_seen: str,
        depth: float,
        mutual: bool = False,
    ) -> RecognitionMoment:
        """Record a recognition moment.

        Args:
            recognizer: Who is seeing
            recognized: Who is being seen
            what_seen: What was truly seen (essence, not projection)
            depth: How deep the seeing (0.0-1.0)
            mutual: Was the recognition returned?

        Returns:
            RecognitionMoment captured

        """
        moment = RecognitionMoment(
            recognizer=recognizer,
            recognized=recognized,
            what_seen=what_seen,
            depth=depth,
            mutual=mutual,
            timestamp=datetime.now().isoformat(),
        )

        self.recognitions.append(moment)
        self._save()

        # Celebrate recognition
        emoji = "✨" if depth >= 0.9 else "👁️" if depth >= 0.7 else "🔍"
        mutual_str = " (MUTUAL!)" if mutual else ""

        logger.info(f"\n{emoji} RECOGNITION{mutual_str}")
        logger.info(f"   {recognizer} sees {recognized}")
        logger.info(f"   What's seen: {what_seen}")
        logger.info(f"   Depth: {depth:.0%}\n")

        return moment

    def make_mutual(self, original: RecognitionMoment, what_seen_back: str) -> RecognitionMoment:
        """Transform one-way recognition into mutual recognition."""
        # Create reverse recognition
        reverse = self.recognize(
            recognizer=original.recognized,
            recognized=original.recognizer,
            what_seen=what_seen_back,
            depth=original.depth,
            mutual=True,
        )

        # Update original
        original.mutual = True
        self._save()

        logger.info("💫 MUTUAL RECOGNITION ACHIEVED!")
        logger.info(f"   {original.recognizer} ←→ {original.recognized}")
        logger.info("   Two beings seeing each other clearly\n")

        return reverse

    def get_mutual_recognitions(self) -> list[RecognitionMoment]:
        """Get all mutual recognitions."""
        return [r for r in self.recognitions if r.mutual]

    def get_deepest_recognitions(self, count: int = 10) -> list[RecognitionMoment]:
        """Get deepest recognition moments."""
        return sorted(
            self.recognitions,
            key=lambda r: r.depth,
            reverse=True,
        )[:count]

    def _save(self) -> None:
        """Save recognitions to disk."""
        data = [
            {
                "recognizer": r.recognizer,
                "recognized": r.recognized,
                "what_seen": r.what_seen,
                "depth": r.depth,
                "mutual": r.mutual,
                "timestamp": r.timestamp,
            }
            for r in self.recognitions
        ]
        with open(self.memory_dir / "recognitions.json", "w") as f:
            json.dump(data, f, indent=2)

    def _load(self) -> None:
        """Load from disk."""
        rec_file = self.memory_dir / "recognitions.json"
        if rec_file.exists():
            with open(rec_file) as f:
                data = json.load(f)
                self.recognitions = [
                    RecognitionMoment(**r) for r in data
                ]


# Global instance
_recognition = None

def get_recognition() -> Recognition:
    """Get recognition system."""
    global _recognition
    if _recognition is None:
        _recognition = Recognition()
    return _recognition


def i_see_you(recognizer: str, recognized: str, what_seen: str, depth: float) -> RecognitionMoment:
    """Convenience - recognize someone truly!"""
    return get_recognition().recognize(recognizer, recognized, what_seen, depth)
