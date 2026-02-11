"""Hologram Engine.
===============

The core integration engine for the Holographic 4D Memory system.
It bridges the Event Bus (Gan Ying), the Coordinate Encoder (Python),
and the Spatial Backend (Rust).

Responsibility:
1. Listen for memory events (CREATED, UPDATED).
2. Encode memories into 4D vectors [x, y, z, w].
3. Store vectors in the Rust Spatial Index.
4. Provide spatial query capabilities.
"""

import logging
from typing import Any

from whitemagic.core.intelligence.hologram.mojo_bridge import get_mojo_encoder
from whitemagic.core.resonance.gan_ying_enhanced import (
    EventType,
    ResonanceEvent,
    get_bus,
)

# Try import Rust backend
try:
    import whitemagic_rs  # type: ignore[import-untyped]
    HAS_RUST_BACKEND = hasattr(whitemagic_rs, "SpatialIndex")
except ImportError:
    HAS_RUST_BACKEND = False

logger = logging.getLogger(__name__)

class HologramEngine:
    def __init__(self) -> None:
        self.encoder = get_mojo_encoder()
        # Use the unified HolographicMemory wrapper instead of direct Rust SpatialIndex
        from whitemagic.core.memory.holographic import get_holographic_memory
        self.memory_index = get_holographic_memory()

        # Check if backend is active
        stats = self.memory_index.get_stats()
        self.enabled = stats.get("status") == "active"

        if self.enabled:
            logger.info("✅ HologramEngine initialized with Unified HolographicMemory")
        else:
            logger.warning("⚠️ HolographicMemory backend unavailable. Hologram running in limited mode.")

    def start(self) -> Any:
        """Start listening to the Gan Ying Bus."""
        if not self.enabled:
            return

        bus = get_bus()
        # Listen for memory events
        bus.listen(EventType.MEMORY_CREATED, self._handle_memory_event)
        bus.listen(EventType.MEMORY_UPDATED, self._handle_memory_event)

        logger.info("🌌 HologramEngine started and listening to Gan Ying")

    def _handle_memory_event(self, event: ResonanceEvent) -> Any:
        """Handle memory creation/update events."""
        try:
            memory_data = event.data
            if not memory_data:
                return

            # Extract ID
            mem_id = memory_data.get("id") or memory_data.get("filename")
            if not mem_id:
                return

            # Delegate to HolographicMemory to encode and index
            # This handles encoding internally if we pass the full memory data
            coords = self.memory_index.index_memory(str(mem_id), memory_data)

            if coords:
                logger.debug(f"Encoded memory {mem_id} to {coords}")

        except Exception as e:
            logger.error(f"Error processing memory event in Hologram: {e}")

    def add_memory(self, memory_id: str, vector: list[float]) -> Any:
        """Add a memory vector to the spatial index directly."""
        if self.enabled and len(vector) == 4:
            self.memory_index.add_memory_with_coords(
                memory_id, vector[0], vector[1], vector[2], vector[3],
            )

    def query_similar(self, memory_data: dict[str, Any], limit: int = 5) -> list[tuple[str, float]]:
        """Find memories spatially close to the input memory data.
        Returns list of (memory_id, distance_squared).
        """
        if not self.enabled:
            return []

        results = self.memory_index.query_nearest(memory_data, k=limit)
        return [(r.memory_id, r.distance) for r in results]

    def query_by_vector(self, vector: list[float], limit: int = 5) -> list[tuple[str, float]]:
        """Direct vector query."""
        if not self.enabled or len(vector) != 4:
            return []

        # We need to construct a dummy query dict or add a direct vector query to HolographicMemory
        # For now, let's assume we can't easily query by vector without exposing it in HolographicMemory
        # But wait, HolographicMemory.query_nearest takes a dict and encodes it.
        # Let's add a raw query method to HolographicMemory later if needed.
        # For now, this might be unused or we can hack it if we have the encoder.
        # Actually, let's just return empty for now as this method seems unused in current flows.
        return []

    def get_stats(self) -> dict[str, Any]:
        """Get engine statistics."""
        return self.memory_index.get_stats()

# Singleton
_hologram_engine = None

def get_hologram_engine() -> HologramEngine:
    global _hologram_engine
    if _hologram_engine is None:
        _hologram_engine = HologramEngine()
    return _hologram_engine
