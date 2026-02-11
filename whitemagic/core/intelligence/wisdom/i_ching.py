"""I Ching Advisor - Hexagram guidance for decisions."""

from typing import Any
import json
import logging
import random
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path

logger = logging.getLogger(__name__)


@dataclass
class Hexagram:
    """An I Ching hexagram."""

    number: int
    name: str
    chinese: str
    judgment: str
    image: str
    lines: str
    guidance: str


class IChingAdvisor:
    """I Ching wisdom for decision making.

    Philosophy: The Book of Changes offers timeless guidance.
    Not fortune telling - pattern recognition across time.
    """

    def __init__(self, base_dir: Path = Path(".")) -> None:
        self.base_dir = base_dir
        self.wisdom_dir = self.base_dir / "memory" / "wisdom"
        self.wisdom_dir.mkdir(parents=True, exist_ok=True)
        self.readings_log = self.wisdom_dir / "i_ching_readings.json"
        self.bus: Any | None = None
        self._connect_to_gan_ying()
        self._init_hexagrams()

    def _connect_to_gan_ying(self) -> Any:
        """Connect to Gan Ying Bus."""
        try:
            from whitemagic.core.resonance.gan_ying import get_bus
            self.bus = get_bus()
            logger.info("🎵 I Ching Advisor connected to Gan Ying Bus")
        except ImportError:
            pass

    def _init_hexagrams(self) -> Any:
        """Initialize hexagram database - ALL 64 HEXAGRAMS!

        Complete 64-hexagram database with traditional interpretations.
        Binary perfection: 8 trigrams² = 64 (same as DNA codons)
        """
        from whitemagic.gardens.wisdom.hexagram_data import COMPLETE_HEXAGRAMS  # type: ignore[import-not-found]

        # Load all 64 complete hexagrams
        self.hexagrams = {}
        for num, name, chinese, judgment, image, lines, guidance in COMPLETE_HEXAGRAMS:
            self.hexagrams[num] = Hexagram(num, name, chinese, judgment, image, lines, guidance)

        logger.info(f"📿 I Ching initialized with {len(self.hexagrams)} hexagrams (complete 64-set!)")

    def cast_hexagram(self, question: str) -> Hexagram:
        """Cast hexagram for a question.

        Args:
            question: Question to ask I Ching

        Returns:
            Hexagram with guidance

        """
        # Try Rust first
        hexagram_number = 0
        try:
            import whitemagic_rs
            if hasattr(whitemagic_rs, "iching_cast"):
                hexagram_number, lines = whitemagic_rs.iching_cast(question)
                logger.info(f"🦀 Rust I Ching calculated hexagram {hexagram_number} from query hash")
        except ImportError:
            pass

        if hexagram_number == 0:
            # Python fallback
            hexagram_number = random.randint(1, 64)

        # Get hexagram (use available ones, default to 1 if not defined)
        hexagram = self.hexagrams.get(hexagram_number, self.hexagrams[1])

        # Log reading
        self._log_reading(question, hexagram)

        logger.info("\n☯️  I CHING READING")
        logger.info("═══════════════════════════════════════")
        logger.info(f"Question: {question}")
        logger.info(f"Hexagram {hexagram.number}: {hexagram.name} ({hexagram.chinese})")
        logger.info(f"Lines: {hexagram.lines}")
        logger.info(f"\nJudgment: {hexagram.judgment}")
        logger.info(f"Image: {hexagram.image}")
        logger.info(f"\n💡 Guidance: {hexagram.guidance}")
        logger.info("═══════════════════════════════════════\n")

        # Emit to Gan Ying
        self._emit_reading(hexagram, question)

        return hexagram

    def get_guidance_for_task(self, task_type: str) -> str:
        """Get I Ching guidance for task type.

        Args:
            task_type: Type of task (implementation, debugging, planning, etc.)

        Returns:
            Guidance string

        """
        guidance_map = {
            "implementation": "Like building (☰☰): Strong foundation, steady progress",
            "debugging": "Like water (☵☵): Flow around obstacles, persist",
            "planning": "Like receptive (☷☷): Listen, observe, then act",
            "refactoring": "After completion (☲☵): Maintain what works, improve carefully",
            "learning": "Before completion (☵☲): Almost there, final careful steps",
        }

        return guidance_map.get(task_type, "Consult the oracle with specific question")

    def _log_reading(self, question: str, hexagram: Hexagram) -> Any:
        """Log I Ching reading."""
        readings = self._load_readings()

        reading = {
            "timestamp": datetime.now().isoformat(),
            "question": question,
            "hexagram_number": hexagram.number,
            "hexagram_name": hexagram.name,
            "guidance": hexagram.guidance,
        }

        readings.append(reading)

        with open(self.readings_log, "w") as f:
            json.dump(readings, f, indent=2)

    def _load_readings(self) -> list[dict[str, Any]]:
        """Load reading log."""
        if not self.readings_log.exists():
            return []
        with open(self.readings_log) as f:
            loaded = json.load(f)
        if isinstance(loaded, list):
            return [item for item in loaded if isinstance(item, dict)]
        return []

    def _emit_reading(self, hexagram: Hexagram, question: str) -> Any:
        """Emit reading to Gan Ying with proper ORACLE_CAST event."""
        if not self.bus:
            return

        try:
            from datetime import datetime

            from whitemagic.core.resonance.gan_ying import EventType, ResonanceEvent

            # Emit ORACLE_CAST so dream/voice systems can react
            self.bus.emit(ResonanceEvent(
                source="i_ching_advisor",
                event_type=EventType.ORACLE_CAST,
                data={
                    "hexagram_number": hexagram.number,
                    "hexagram_name": hexagram.name,
                    "chinese": hexagram.chinese,
                    "question": question,
                    "judgment": hexagram.judgment,
                    "image": hexagram.image,
                    "guidance": hexagram.guidance,
                    "lines": hexagram.lines,
                },
                timestamp=datetime.now(),
                confidence=0.9,
            ), async_dispatch=True)
        except Exception as e:
            logger.info(f"⚠️ Could not emit oracle event: {e}")


# Global instance
_i_ching: IChingAdvisor | None = None


def get_i_ching() -> IChingAdvisor:
    """Get global I Ching advisor instance."""
    global _i_ching
    if _i_ching is None:
        _i_ching = IChingAdvisor()
    return _i_ching


# Backward compatibility aliases
IChing = IChingAdvisor
get_iching = get_i_ching
