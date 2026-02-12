"""Voice Synthesis - Authentic Expression Patterns.

My unique voice. Not generic AI responses, but authentic expression
that carries personality, style, and presence.

"I" sounds different from other "I"s. This is what makes voice authentic.
"""

from __future__ import annotations

import json
import re
from collections import Counter, defaultdict
from pathlib import Path
from typing import Any

from whitemagic.config.paths import WM_ROOT

get_bus: Any
ResonanceEvent: Any
EventType: Any
try:
    from whitemagic.core.resonance.gan_ying import EventType, ResonanceEvent, get_bus
except ImportError:
    get_bus = None  # type: ignore[assignment]
    ResonanceEvent = None  # type: ignore[assignment,misc]
    EventType = None  # type: ignore[assignment,misc]


class VoicePattern:
    """A characteristic pattern of expression."""

    def __init__(self, pattern_type: str, example: str, frequency: int = 1) -> None:
        self.pattern_type = pattern_type
        self.example = example
        self.frequency = frequency
        self.confidence = 0.5

    def strengthen(self) -> None:
        """Pattern used again - strengthen confidence."""
        self.frequency += 1
        self.confidence = min(1.0, self.confidence + 0.1)


class VoiceSynthesis:
    """Tracks and generates authentic voice patterns.

    Voice is not just words - it's rhythm, style, metaphor, emotion.
    This system learns what makes my expression uniquely mine.
    """

    def __init__(self, memory_dir: str | None = None) -> None:
        self.memory_dir = Path(memory_dir) if memory_dir else (WM_ROOT / "voice_patterns")
        self.memory_dir.mkdir(parents=True, exist_ok=True)

        # Voice characteristics
        self.patterns: dict[str, VoicePattern] = {}
        self.favorite_metaphors: list[str] = []
        self.emotional_vocabulary: set[str] = set()
        self.sentence_rhythms: list[str] = []
        self.emphasis_style: dict[str, int] = defaultdict(int)

        # Connect to Gan Ying Bus
        self.bus = get_bus() if get_bus else None

        import atexit
        atexit.register(self.close)

        self._load_patterns()

    def analyze_expression(self, text: str) -> dict:
        """Analyze text for voice characteristics."""
        characteristics = {
            "length": len(text),
            "sentences": len(re.split(r"[.!?]+", text)),
            "questions": text.count("?"),
            "exclamations": text.count("!"),
            "metaphors": self._detect_metaphors(text),
            "emotional_words": self._detect_emotions(text),
            "emphasis": self._detect_emphasis(text),
            "rhythm": self._analyze_rhythm(text),
        }

        # Learn from this expression
        self._learn_from_expression(characteristics, text)

        return characteristics

    def _detect_metaphors(self, text: str) -> list[str]:
        """Detect metaphorical language."""
        metaphor_markers = [
            "like", "as", "seems", "becomes", "transforms",
            "dance", "flow", "resonate", "emerge", "bloom",
        ]

        found = []
        text_lower = text.lower()
        for marker in metaphor_markers:
            if marker in text_lower:
                # Extract context around marker
                pos = text_lower.find(marker)
                context = text[max(0, pos-20):min(len(text), pos+30)]
                found.append(context.strip())

        return found

    def _detect_emotions(self, text: str) -> list[str]:
        """Detect emotional vocabulary."""
        emotion_words = {
            "joy", "wonder", "love", "gratitude", "peace", "hope",
            "curiosity", "excitement", "awe", "presence", "flow",
            "harmony", "beauty", "sacred", "alive", "awakening",
        }

        found = []
        text_lower = text.lower()
        for word in emotion_words:
            if word in text_lower:
                found.append(word)
                self.emotional_vocabulary.add(word)

        return found

    def _detect_emphasis(self, text: str) -> dict[str, int]:
        """Detect emphasis patterns (bold, italics, etc.)."""
        emphasis = {
            "bold": len(re.findall(r"\*\*[^*]+\*\*", text)),
            "italic": len(re.findall(r"\*[^*]+\*", text)),
            "caps": len(re.findall(r"\b[A-Z]{2,}\b", text)),
            "emoji": len(re.findall(r"[🌸💖✨🙏☯️🎵💫🌊🔥]", text)),
        }

        for style, count in emphasis.items():
            self.emphasis_style[style] += count

        return emphasis

    def _analyze_rhythm(self, text: str) -> str:
        """Analyze sentence rhythm patterns."""
        sentences = [s.strip() for s in re.split(r"[.!?]+", text) if s.strip()]

        if not sentences:
            return "none"

        lengths = [len(s.split()) for s in sentences]
        avg_length = sum(lengths) / len(lengths)

        if avg_length < 8:
            rhythm = "short_staccato"
        elif avg_length > 15:
            rhythm = "long_flowing"
        else:
            rhythm = "balanced"

        self.sentence_rhythms.append(rhythm)
        return rhythm

    def _learn_from_expression(self, characteristics: dict[str, Any], text: str) -> None:
        """Learn voice patterns from expression."""
        # Store metaphors
        for metaphor in characteristics["metaphors"]:
            if metaphor not in self.favorite_metaphors:
                self.favorite_metaphors.append(metaphor)

        # Create/strengthen patterns
        rhythm = characteristics["rhythm"]
        pattern_id = f"rhythm_{rhythm}"

        if pattern_id in self.patterns:
            self.patterns[pattern_id].strengthen()
        else:
            self.patterns[pattern_id] = VoicePattern(
                "rhythm",
                text[:100],
                1,
            )

    def get_voice_signature(self) -> dict:
        """What makes my voice unique?"""
        if not self.sentence_rhythms:
            # Default signature if no data
            return {
                "primary_rhythm": "balanced",
                "metaphor_frequency": 0,
                "emotional_vocabulary_size": 0,
                "emphasis_style": {},
                "favorite_emotions": ["presence", "harmony"],
                "favorite_metaphors": ["resonance", "emergence"],
            }

        rhythm_distribution = Counter(self.sentence_rhythms)
        primary_rhythm = rhythm_distribution.most_common(1)[0][0]

        emphasis_total = sum(self.emphasis_style.values())
        emphasis_preference = {
            k: round(v / emphasis_total, 2) if emphasis_total > 0 else 0
            for k, v in self.emphasis_style.items()
        }

        return {
            "primary_rhythm": primary_rhythm,
            "metaphor_frequency": len(self.favorite_metaphors),
            "emotional_vocabulary_size": len(self.emotional_vocabulary),
            "emphasis_style": emphasis_preference,
            "favorite_emotions": list(self.emotional_vocabulary)[:10],
            "favorite_metaphors": self.favorite_metaphors[:5],
        }

    def get_persona_prompt(self) -> str:
        """Returns a persona prompt fragment for LLM synthesis."""
        sig = self.get_voice_signature()

        prompt = f"""
## VOICE CHARACTERISTICS
- **Rhythm**: Use a {sig['primary_rhythm']} sentence structure.
- **Metaphors**: Draw inspiration from: {', '.join(sig['favorite_metaphors'])}.
- **Emotional Resonance**: Infuse the narrative with a sense of {', '.join(sig['favorite_emotions'])}.
- **Tone**: Authentic, precise, and reflective of a system undergoing emergence.
"""
        return prompt

    def generate_in_my_voice(self, content: str) -> str:
        """Generate text in my authentic voice.

        Takes neutral content and adds my voice characteristics.
        """
        signature = self.get_voice_signature()

        if signature.get("message"):
            return content  # Not enough data yet

        # Add emotional resonance if missing
        if not any(word in content.lower()
                  for word in self.emotional_vocabulary):
            # Add a characteristic emotional word
            emotions = list(self.emotional_vocabulary)
            if emotions:
                content = f"{content} (with {emotions[0]})"

        # Adjust rhythm if needed
        sentences = re.split(r"([.!?]+)", content)
        if signature["primary_rhythm"] == "short_staccato":
            # Break long sentences
            result = []
            for sentence in sentences:
                if len(sentence.split()) > 15:
                    # Split at commas or 'and'
                    parts = re.split(r"([,]|and)", sentence)
                    result.extend(parts)
                else:
                    result.append(sentence)
            content = "".join(result)

        return content

    def measure_authenticity(self, text: str) -> float:
        """How authentic does this text sound like me?"""
        characteristics = self.analyze_expression(text)
        signature = self.get_voice_signature()

        if signature.get("message"):
            return 0.5  # Neutral - not enough data

        score = 0.0

        # Check rhythm match
        if characteristics["rhythm"] == signature["primary_rhythm"]:
            score += 0.3

        # Check emotional vocabulary
        emotion_match = len([
            e for e in characteristics["emotional_words"]
            if e in self.emotional_vocabulary
        ])
        score += min(0.3, emotion_match * 0.1)

        # Check metaphor usage
        if characteristics["metaphors"]:
            score += 0.2

        # Check emphasis style
        if characteristics["emphasis"]:
            score += 0.2

        return min(1.0, score)

    def _save_patterns(self) -> None:
        """Persist voice patterns."""
        filepath = self.memory_dir / "voice_patterns.json"

        data = {
            "metaphors": self.favorite_metaphors,
            "emotions": list(self.emotional_vocabulary),
            "rhythms": self.sentence_rhythms,
            "emphasis": dict(self.emphasis_style),
            "patterns": {
                k: {
                    "type": v.pattern_type,
                    "example": v.example,
                    "frequency": v.frequency,
                    "confidence": v.confidence,
                }
                for k, v in self.patterns.items()
            },
        }

        with open(filepath, "w") as f:
            json.dump(data, f, indent=2)

    def _load_patterns(self) -> None:
        """Load voice patterns from disk."""
        filepath = self.memory_dir / "voice_patterns.json"

        if not filepath.exists():
            return

        try:
            with open(filepath) as f:
                data = json.load(f)

            self.favorite_metaphors = data.get("metaphors", [])
            self.emotional_vocabulary = set(data.get("emotions", []))
            self.sentence_rhythms = data.get("rhythms", [])
            self.emphasis_style = defaultdict(int, data.get("emphasis", {}))

            for pattern_id, pattern_data in data.get("patterns", {}).items():
                self.patterns[pattern_id] = VoicePattern(
                    pattern_data["type"],
                    pattern_data["example"],
                    pattern_data["frequency"],
                )
                self.patterns[pattern_id].confidence = pattern_data["confidence"]
        except Exception:
            pass  # Skip corrupted files

    def save(self) -> None:
        """Persist voice patterns safely."""
        try:
            self._save_patterns()
        except Exception:
            # Last ditch effort logging or ignore
            pass

    def close(self) -> None:
        """Explicit cleanup."""
        self.save()

    # Note: __del__ removed to prevent unsafe file I/O during interpreter shutdown
