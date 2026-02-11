"""🌊 Connection Cascades - Friend-of-Friend Links
If A→B and B→C, create weak A→C link.
Created: December 2, 2025 (Hanuman Tuesday).
"""


from whitemagic.core.memory.neural.neural_memory import LinkType, NeuralMemory


class CascadeDiscovery:
    """Discovers indirect connections between memories."""

    def __init__(self, min_cascade_strength: float = 0.1, decay_factor: float = 0.5):
        self.min_cascade_strength = min_cascade_strength
        self.decay_factor = decay_factor
        self._memory_store: dict[str, NeuralMemory] = {}

    def register_memory(self, memory: NeuralMemory) -> None:
        self._memory_store[memory.id] = memory

    def discover_cascades(self, memory: NeuralMemory) -> list[tuple]:
        """Find cascade links (friend-of-friend connections)."""
        cascades = []

        for first_hop_id, first_link in memory.links.items():
            first_hop = self._memory_store.get(first_hop_id)
            if not first_hop:
                continue

            for second_hop_id, second_link in first_hop.links.items():
                # Skip if already directly linked or self-loop
                if second_hop_id == memory.id or second_hop_id in memory.links:
                    continue

                # Calculate cascade strength
                cascade_strength = first_link.strength * second_link.strength * self.decay_factor

                if cascade_strength >= self.min_cascade_strength:
                    cascades.append((second_hop_id, cascade_strength, first_hop_id))

        return cascades

    def create_cascade_links(self, memory: NeuralMemory) -> int:
        """Create cascade links for a memory."""
        cascades = self.discover_cascades(memory)
        created = 0

        for target_id, strength, via_id in cascades:
            memory.add_link(target_id, LinkType.CASCADE, strength)

            target = self._memory_store.get(target_id)
            if target:
                target.add_link(memory.id, LinkType.CASCADE, strength)

            created += 1

        return created

    def discover_all_cascades(self) -> int:
        """Discover cascade links across all memories."""
        total = 0
        for memory in self._memory_store.values():
            total += self.create_cascade_links(memory)
        return total


_cascade: CascadeDiscovery | None = None

def get_cascade() -> CascadeDiscovery:
    global _cascade
    if _cascade is None:
        _cascade = CascadeDiscovery()
    return _cascade

def discover_cascade_links(memory: NeuralMemory) -> list[tuple]:
    return get_cascade().discover_cascades(memory)

def create_cascade_link(memory: NeuralMemory) -> int:
    return get_cascade().create_cascade_links(memory)
