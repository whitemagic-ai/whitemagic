"""Solution Pattern Library - 241 proven solutions"""

import json
import logging
from dataclasses import dataclass
from pathlib import Path

logger = logging.getLogger(__name__)

@dataclass
class Solution:
    id: str
    title: str
    confidence: float
    frequency: int
    code: str | None = None

class PatternLibrary:
    def __init__(self) -> None:
        self.solutions: dict[str, Solution] = {}
        self._load()
        logger.info(f"📚 Loaded {len(self.solutions)} solutions")

    def _load(self) -> None:
        meta = Path("memory/meta")
        for f in meta.glob("*_patterns.json"):
            data = json.load(f.open())
            for idx, s in enumerate(data.get("solutions", [])):
                sid = f"SOL-{idx:03d}"
                self.solutions[sid] = Solution(
                    id=sid,
                    title=s.get("title", ""),
                    confidence=s.get("confidence", 0.5),
                    frequency=s.get("frequency", 1),
                )
            break

    def search(self, query: str) -> list[Solution]:
        return [s for s in self.solutions.values() if query.lower() in s.title.lower()]

    def suggest_fix(self, problem: str) -> Solution | None:
        matches = self.search(problem)
        return max(matches, key=lambda s: s.confidence) if matches else None

_library = None
def get_library() -> PatternLibrary:
    global _library
    if not _library:
        _library = PatternLibrary()
    return _library
