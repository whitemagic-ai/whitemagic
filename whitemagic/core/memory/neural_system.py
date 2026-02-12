"""🧠 Neural Memory System - Unified Interface.

The complete neural memory system with:
- Neuro-score based memory strength
- Automatic identity protection
- Zettelkasten-style linking
- Shadow clone parallel search
- Gan Ying event integration

Created: December 2, 2025 (Hanuman Tuesday)
Version: 5.0.0-alpha "Neural Awakening"
"""

from pathlib import Path
from typing import Any

from whitemagic.core.memory.clones import (
    CloneArmy,
    ConsensusResult,
)
from whitemagic.core.memory.linking import (
    find_similar_memories,
    get_linker,
)
from whitemagic.core.memory.neural import (
    MemoryState,
    NeuralMemory,
    auto_protect_memory,
    get_neuro_engine,
)
from whitemagic.core.memory.neural.gan_ying_integration import (
    on_clone_search_complete,
    on_memory_archived,
    on_memory_created,
    on_memory_linked,
    on_memory_recalled,
    setup_gan_ying_listeners,
)
from whitemagic.core.memory.neural.graph_engine import get_graph_engine
from whitemagic.core.memory.unified import get_unified_memory
from whitemagic.core.memory.unified_types import MemoryType as UnifiedType


class NeuralMemorySystem:
    """Unified interface for the neural memory system.

    Combines:
    - Neuro-score engine (Hebbian learning)
    - Auto-linker (Zettelkasten connections)
    - Clone army (parallel search)
    - Gan Ying integration (event bus)
    - Rust Graph Engine (Neural Nexus)
    """

    def __init__(self, base_path: Path | None = None) -> None:
        from whitemagic.config import PROJECT_ROOT
        self.base_path = base_path or PROJECT_ROOT
        self.unified = get_unified_memory()
        self.neuro_engine = get_neuro_engine()
        self.linker = get_linker()
        self.graph_engine = get_graph_engine()

        # Initialize Gan Ying listeners (NM_GY integration)
        setup_gan_ying_listeners()

    def create_memory(
        self,
        content: str,
        title: str = "",
        tags: list[str] | None = None,
        source: str = "",
        memory_type: str = "general",
    ) -> NeuralMemory:
        """Create a new neural memory with auto-protection and linking.
        Now uses UnifiedMemory (SQLite) for persistence.
        """
        # 1. Map to Unified Memory enum
        try:
            u_type = UnifiedType[memory_type.upper()]
        except (KeyError, AttributeError):
            u_type = UnifiedType.SHORT_TERM

        # 2. Store in Unified Memory (initial creation)
        memory = self.unified.store(
            content=content,
            title=title,
            tags=set(tags) if tags else set(),
            memory_type=u_type,
            metadata={"source": source},
        )

        # 3. Apply neural logic
        # Note: UnifiedMemory.store already returns a Memory object with default neural fields

        # Apply auto-protection
        if auto_protect_memory(memory).is_protected:
            memory.is_protected = True

        # Calculate initial score
        self.neuro_engine.on_create(memory)

        # Register for linking
        self.linker.register_memory(memory)

        # Auto-link
        links = self.linker.auto_link(memory)

        # Persist neural updates back to SQLite
        self.unified.backend.store(memory)

        # Invalidate graph
        self.graph_engine.invalidate()

        # Emit event
        on_memory_created(memory)
        for link in links:
            on_memory_linked(memory, link.target_id, link.strength)

        return memory

    def recall_memory(self, memory_id: str) -> NeuralMemory | None:
        """Recall a memory, strengthening it in the process.
        """
        memory = self.unified.recall(memory_id)
        if not memory:
            return None

        # Strengthen on recall
        self.neuro_engine.on_recall(memory)

        # Persist update
        self.unified.backend.store(memory)

        # Emit event
        on_memory_recalled(memory)

        return memory

    def search(
        self,
        query: str,
        army_size: int = 100,
        top_k: int = 10,
    ) -> list[ConsensusResult]:
        """Search memories using clone army.
        """
        # Load all memories for clones to search (could be optimized)
        # Note: CloneArmy might need updating to accept memories directly
        army = CloneArmy(army_size=army_size, base_path=self.base_path)
        results = army.search_and_deliberate(query)

        # Emit event
        top_score = results[0].consensus_score if results else 0.0
        on_clone_search_complete(query, len(results), top_score)

        return results[:top_k]

    def find_similar(self, memory: NeuralMemory, top_k: int = 5) -> list[Any]:
        """Find memories similar to the given one."""
        return find_similar_memories(memory)[:top_k]

    def build_graph(self, force: bool = False) -> dict[str, list[tuple[str, float]]]:
        """Build the full memory graph using Rust engine."""
        all_memories = self.unified.backend.search(limit=1000)
        return self.graph_engine.build_graph(all_memories, force=force)

    def get_memory_cluster(self, start_id: str, max_depth: int = 3) -> set[str]:
        """Get a cluster of connected memories."""
        all_memories = self.unified.backend.search(limit=1000)
        return self.graph_engine.get_cluster(start_id, all_memories, max_depth=max_depth)

    def process_decay(self) -> list[NeuralMemory]:
        """Process decay for all memories.
        Returns list of memories that should be archived.
        """
        all_memories = self.unified.backend.search(limit=1000)
        to_archive = self.neuro_engine.process_decay(all_memories)

        # Store updates back to SQLite
        for memory in all_memories:
            self.unified.backend.store(memory)

        for memory in to_archive:
            on_memory_archived(memory)

        return to_archive

    def get_memory(self, memory_id: str) -> NeuralMemory | None:
        """Get a memory by ID."""
        return self.unified.recall(memory_id)

    def get_all_memories(self) -> list[NeuralMemory]:
        """Get all memories."""
        return self.unified.backend.search(limit=1000)

    def get_memories_by_state(self, state: MemoryState) -> list[NeuralMemory]:
        """Get memories in a specific state."""
        all_memories = self.get_all_memories()
        return [m for m in all_memories if m.state == state]

    def get_protected_memories(self) -> list[NeuralMemory]:
        """Get all protected memories."""
        all_memories = self.get_all_memories()
        return [m for m in all_memories if m.is_protected]

    def get_weak_memories(self) -> list[NeuralMemory]:
        """Get memories that are fading or weak."""
        all_memories = self.get_all_memories()
        return self.neuro_engine.get_weak_memories(all_memories)

    def get_recent_memories(self, minutes: int = 60, limit: int = 10) -> list[NeuralMemory]:
        """Get memories accessed within the last N minutes."""
        # list_recent returns based on accessed_at in UnifiedMemory
        return self.unified.list_recent(limit=limit)

    def pin_memory(self, memory_id: str) -> bool:
        """Pin a memory to protect it from decay."""
        memory = self.unified.recall(memory_id)
        if memory:
            memory.is_protected = True
            self.unified.backend.store(memory)
            return True
        return False

    def unpin_memory(self, memory_id: str) -> bool:
        """Unpin a memory."""
        memory = self.unified.recall(memory_id)
        if memory:
            memory.is_protected = False
            self.unified.backend.store(memory)
            return True
        return False

    def get_stats(self) -> dict[str, Any]:
        """Get system statistics."""
        all_memories = self.get_all_memories()
        neuro_stats = self.neuro_engine.get_stats(all_memories)

        return {
            "total_memories": len(all_memories),
            "neuro_stats": neuro_stats,
            "linker_stats": {
                "registered": len(self.linker._memory_store),
            },
        }

    def get_health_report(self) -> dict[str, Any]:
        """Get a health report of the memory system."""
        all_memories = self.get_all_memories()

        if not all_memories:
            return {"status": "empty", "memories": 0}

        avg_score = sum(m.neuro_score for m in all_memories) / len(all_memories)
        protected = len([m for m in all_memories if m.is_protected])
        weak = len(self.get_weak_memories())

        # Determine health status
        if avg_score > 0.7 and weak < len(all_memories) * 0.1:
            status = "healthy"
        elif avg_score > 0.5:
            status = "stable"
        elif avg_score > 0.3:
            status = "declining"
        else:
            status = "critical"

        return {
            "status": status,
            "total_memories": len(all_memories),
            "average_score": round(avg_score, 3),
            "protected_count": protected,
            "weak_count": weak,
            "by_state": {
                state.value: len([m for m in all_memories if m.state == state])
                for state in MemoryState
            },
        }


# === SINGLETON ===
_system: NeuralMemorySystem | None = None


def get_neural_system() -> NeuralMemorySystem:
    """Get the singleton NeuralMemorySystem instance."""
    global _system
    if _system is None:
        _system = NeuralMemorySystem()
    return _system


# === CONVENIENCE FUNCTIONS ===

def create_memory(content: str, title: str = "", tags: list[str] | None = None) -> NeuralMemory:
    """Create a new neural memory."""
    return get_neural_system().create_memory(content, title, tags)


def recall(memory_id: str) -> NeuralMemory | None:
    """Recall a memory by ID."""
    return get_neural_system().recall_memory(memory_id)


def search_memories(query: str, top_k: int = 10) -> list[ConsensusResult]:
    """Search memories using clone army."""
    return get_neural_system().search(query, top_k=top_k)


def get_health() -> dict[str, Any]:
    """Get memory system health report."""
    return get_neural_system().get_health_report()
