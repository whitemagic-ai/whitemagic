"""Emotional Memory System - Amygdala-Inspired Memory Tagging.

Biological inspiration: The amygdala tags emotional experiences
for stronger encoding. This module adds emotional resonance
scores to memories.
"""

import json
from datetime import datetime
from pathlib import Path
from typing import Any

from whitemagic.config.paths import WM_ROOT
from whitemagic.utils.fileio import atomic_write, file_lock


class EmotionalTag:
    """Emotional resonance tag for a memory."""

    EMOTIONS = [
        "joy", "love", "gratitude", "wonder", "peace",
        "sadness", "fear", "frustration", "confusion", "anxiety",
        "breakthrough", "connection", "insight", "flow", "pride",
    ]

    def __init__(self, emotion: str, intensity: float = 0.5, context: str | None = None):
        self.emotion = emotion
        self.intensity = min(1.0, max(0.0, intensity))
        self.context = context
        self.timestamp = datetime.now()

    def to_dict(self) -> dict[str, Any]:
        return {
            "emotion": self.emotion,
            "intensity": self.intensity,
            "context": self.context,
            "timestamp": self.timestamp.isoformat(),
        }


class EmotionalMemorySystem:
    """Manages emotional tagging of memories."""

    def __init__(self, memory_dir: Path | None = None):
        self.memory_dir = memory_dir or (WM_ROOT / "emotional_memory")
        self.memory_dir.mkdir(parents=True, exist_ok=True)
        self.tags: dict[str, list[EmotionalTag]] = {}  # memory_id -> tags
        self._load_tags()

    def _load_tags(self) -> None:
        """Load existing emotional tags."""
        tags_file = self.memory_dir / "emotional_tags.json"
        if tags_file.exists():
            try:
                with file_lock(tags_file):
                    data: dict[str, Any] = json.loads(tags_file.read_text()) or {}
                for mem_id, tag_list in data.items():
                    self.tags[mem_id] = [
                        EmotionalTag(t["emotion"], t["intensity"], t.get("context"))
                        for t in tag_list
                    ]
            except Exception:
                pass

    def _save_tags(self) -> None:
        """Persist emotional tags."""
        tags_file = self.memory_dir / "emotional_tags.json"
        data: dict[str, list[dict[str, Any]]] = {
            mem_id: [t.to_dict() for t in tags]
            for mem_id, tags in self.tags.items()
        }
        with file_lock(tags_file):
            atomic_write(tags_file, json.dumps(data, indent=2))

    def tag_memory(self, memory_id: str, emotion: str, intensity: float = 0.5, context: str | None = None) -> EmotionalTag:
        """Add emotional tag to a memory."""
        tag = EmotionalTag(emotion, intensity, context)
        if memory_id not in self.tags:
            self.tags[memory_id] = []
        self.tags[memory_id].append(tag)
        self._save_tags()
        return tag

    def get_emotional_resonance(self, memory_id: str) -> float:
        """Get overall emotional resonance score for a memory."""
        if memory_id not in self.tags:
            return 0.0
        tags = self.tags[memory_id]
        if not tags:
            return 0.0
        return sum(t.intensity for t in tags) / len(tags)

    def find_by_emotion(self, emotion: str, min_intensity: float = 0.3) -> list[str]:
        """Find memories tagged with a specific emotion."""
        results = []
        for mem_id, tags in self.tags.items():
            for tag in tags:
                if tag.emotion == emotion and tag.intensity >= min_intensity:
                    results.append(mem_id)
                    break
        return results

    def find_breakthrough_moments(self) -> list[str]:
        """Find memories tagged as breakthroughs."""
        return self.find_by_emotion("breakthrough", min_intensity=0.6)

    def find_joyful_memories(self) -> list[str]:
        """Find memories tagged with joy."""
        return self.find_by_emotion("joy", min_intensity=0.5)


# Singleton
_emotional_system: EmotionalMemorySystem | None = None

def get_emotional_memory() -> EmotionalMemorySystem:
    """Get the emotional memory system singleton."""
    global _emotional_system
    if _emotional_system is None:
        _emotional_system = EmotionalMemorySystem()
    return _emotional_system


def tag_with_emotion(memory_id: str, emotion: str, intensity: float = 0.5) -> EmotionalTag:
    """Convenience function to tag a memory with emotion."""
    return get_emotional_memory().tag_memory(memory_id, emotion, intensity)
