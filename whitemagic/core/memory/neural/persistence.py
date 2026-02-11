"""💾 Neural Memory Persistence - Save/Load Neural Memories
Created: December 2, 2025 (Hanuman Tuesday).
"""

import json
from pathlib import Path

from whitemagic.core.memory.neural.neural_memory import NeuralMemory


class NeuralMemoryStore:
    """Persist neural memories to disk."""

    def __init__(self, base_path: Path | None = None):
        from whitemagic.config import PROJECT_ROOT
        self.base_path = base_path or PROJECT_ROOT / "memory" / "neural_store"
        self.base_path.mkdir(parents=True, exist_ok=True)
        self.index_path = self.base_path / "index.json"
        self._index: dict[str, str] = {}  # id -> filename
        self._load_index()

    def _load_index(self) -> None:
        """Load memory index."""
        if self.index_path.exists():
            try:
                self._index = json.loads(self.index_path.read_text())
            except json.JSONDecodeError:
                self._index = {}

    def _save_index(self) -> None:
        """Save memory index."""
        self.index_path.write_text(json.dumps(self._index, indent=2))

    def save(self, memory: NeuralMemory) -> str:
        """Save a neural memory to disk."""
        filename = f"{memory.id}.json"
        filepath = self.base_path / filename

        data = memory.to_dict()
        filepath.write_text(json.dumps(data, indent=2, default=str))

        self._index[memory.id] = filename
        self._save_index()

        return str(filepath)

    def load(self, memory_id: str) -> NeuralMemory | None:
        """Load a neural memory by ID."""
        filename = self._index.get(memory_id)
        if not filename:
            return None

        filepath = self.base_path / filename
        if not filepath.exists():
            return None

        try:
            data = json.loads(filepath.read_text())
            return NeuralMemory.from_dict(data)
        except json.JSONDecodeError:
            return None

    def load_all(self) -> list[NeuralMemory]:
        """Load all neural memories."""
        memories = []
        for memory_id in self._index:
            mem = self.load(memory_id)
            if mem:
                memories.append(mem)
        return memories

    def delete(self, memory_id: str) -> bool:
        """Delete a neural memory."""
        filename = self._index.get(memory_id)
        if not filename:
            return False

        filepath = self.base_path / filename
        if filepath.exists():
            filepath.unlink()

        del self._index[memory_id]
        self._save_index()
        return True

    def exists(self, memory_id: str) -> bool:
        """Check if memory exists."""
        return memory_id in self._index

    def count(self) -> int:
        """Count stored memories."""
        return len(self._index)

    def get_stats(self) -> dict:
        """Get store statistics."""
        return {
            "count": self.count(),
            "path": str(self.base_path),
            "index_size": len(self._index),
        }


# Singleton
_store: NeuralMemoryStore | None = None

def get_store() -> NeuralMemoryStore:
    global _store
    if _store is None:
        _store = NeuralMemoryStore()
    return _store
