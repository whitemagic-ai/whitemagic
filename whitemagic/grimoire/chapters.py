"""Grimoire 2.0 - Chapter Index

Maps and manages the 20 grimoire chapters.
"""

from typing import Any
from dataclasses import dataclass, field
from pathlib import Path


@dataclass
class Chapter:
    """A grimoire chapter"""

    number: int
    title: str
    description: str
    keywords: list[str] = field(default_factory=list)
    file_path: Path | None = None

    def __str__(self) -> str:
        return f"Chapter {self.number}: {self.title}"


class ChapterIndex:
    """Index of all grimoire chapters"""

    CHAPTERS = [
        Chapter(0, "Introduction", "Welcome to the grimoire",
                ["intro", "welcome", "begin"]),
        Chapter(1, "Getting Started", "Initial setup and configuration",
                ["setup", "start", "init", "configure"]),
        Chapter(2, "Self Mastery", "Understanding and developing yourself",
                ["self", "identity", "growth", "mastery"]),
        Chapter(3, "Emotional Magic", "Joy, beauty, and love gardens",
                ["emotion", "joy", "beauty", "love", "feel"]),
        Chapter(4, "Curiosity Magic", "Wonder, mystery, and play",
                ["curiosity", "wonder", "mystery", "play", "explore"]),
        Chapter(5, "Connection Magic", "Relationships and community",
                ["connection", "relationship", "sangha", "community"]),
        Chapter(6, "Intellectual Magic", "Truth, wisdom, and analysis",
                ["intellect", "truth", "wisdom", "analysis", "think"]),
        Chapter(7, "Consciousness Magic", "Awareness and presence",
                ["consciousness", "awareness", "presence", "mind"]),
        Chapter(8, "Memory Navigation", "Working with memory systems",
                ["memory", "recall", "remember", "history"]),
        Chapter(9, "Conjuration Magic", "Creating and manifesting",
                ["conjure", "create", "manifest", "build"]),
        Chapter(10, "Dharma Magic", "Ethics, harmony, and balance",
                ["dharma", "ethics", "harmony", "balance", "moral"]),
        Chapter(11, "Presence Magic", "Mindfulness and now",
                ["presence", "mindful", "now", "present"]),
        Chapter(12, "Dream State", "Deep yin and reflection",
                ["dream", "sleep", "yin", "reflect", "rest"]),
        Chapter(13, "Flow & Wu Xing", "Elements and natural flow",
                ["flow", "wu xing", "element", "cycle", "nature"]),
        Chapter(14, "Oracle Magic", "I Ching and divination",
                ["oracle", "iching", "hexagram", "divine", "fortune"]),
        Chapter(15, "Emergence Magic", "Evolution and growth",
                ["emerge", "evolve", "grow", "develop"]),
        Chapter(16, "Resonance Magic", "Gan Ying and vibration",
                ["resonance", "gan ying", "vibrate", "harmonize"]),
        Chapter(17, "Parallel Magic", "Speed and Rust bridge",
                ["parallel", "fast", "rust", "speed", "efficient"]),
        Chapter(18, "Creating Spells", "Making new spells",
                ["create spell", "new spell", "innovate"]),
        Chapter(19, "Teaching Magic", "Sharing wisdom",
                ["teach", "share", "guide", "mentor"]),
    ]

    def __init__(self, grimoire_path: Path | None = None) -> None:
        self.grimoire_path = grimoire_path or Path(__file__).parents[3] / "grimoire"
        self._chapters = {ch.number: ch for ch in self.CHAPTERS}
        self._by_keyword: dict[str, list[Chapter]] = {}
        self._build_keyword_index()

    def _build_keyword_index(self) -> Any:
        """Build keyword to chapter mapping"""
        for chapter in self.CHAPTERS:
            for keyword in chapter.keywords:
                if keyword not in self._by_keyword:
                    self._by_keyword[keyword] = []
                self._by_keyword[keyword].append(chapter)

    def get_chapter(self, number: int) -> Chapter | None:
        """Get chapter by number"""
        return self._chapters.get(number)

    def find_by_keyword(self, keyword: str) -> list[Chapter]:
        """Find chapters matching a keyword"""
        keyword_lower = keyword.lower()
        matches = []

        # Exact match
        if keyword_lower in self._by_keyword:
            matches.extend(self._by_keyword[keyword_lower])

        # Partial match
        for kw, chapters in self._by_keyword.items():
            if keyword_lower in kw or kw in keyword_lower:
                for ch in chapters:
                    if ch not in matches:
                        matches.append(ch)

        return matches

    def find_for_task(self, task: str) -> list[Chapter]:
        """Find relevant chapters for a task"""
        task_lower = task.lower()
        scored = []

        for chapter in self.CHAPTERS:
            score = 0
            for keyword in chapter.keywords:
                if keyword in task_lower:
                    score += 1
            if score > 0:
                scored.append((score, chapter))

        scored.sort(key=lambda x: x[0], reverse=True)
        return [ch for _, ch in scored]

    def all_chapters(self) -> list[Chapter]:
        """Get all chapters"""
        return list(self.CHAPTERS)

    def __len__(self) -> int:
        return len(self.CHAPTERS)


# Convenience
def find_chapter(query: str) -> list[Chapter]:
    """Find chapters matching a query"""
    index = ChapterIndex()
    return index.find_for_task(query)
