"""🔔 Gan Ying Integration - Neural Memory Events
Wire the neural memory system to the Gan Ying resonance bus.
Created: December 2, 2025 (Hanuman Tuesday).
"""

import logging
from datetime import datetime
from typing import Any, Union

from whitemagic.core.memory.neural.neural_memory import NeuralMemory

logger = logging.getLogger(__name__)


# Try to import Gan Ying bus
try:
    from whitemagic.core.resonance.gan_ying import EventType, ResonanceEvent, get_bus
    GAN_YING_AVAILABLE = True
except ImportError:
    GAN_YING_AVAILABLE = False


class NeuralMemoryEventType:
    """Event types for neural memory system."""

    MEMORY_CREATED = "neural.memory.created"
    MEMORY_RECALLED = "neural.memory.recalled"
    MEMORY_LINKED = "neural.memory.linked"
    MEMORY_DECAYED = "neural.memory.decayed"
    MEMORY_ARCHIVED = "neural.memory.archived"
    MEMORY_PROTECTED = "neural.memory.protected"
    SCORE_UPDATED = "neural.score.updated"
    CASCADE_DISCOVERED = "neural.cascade.discovered"
    CLONE_SEARCH_COMPLETE = "neural.clone.search_complete"


def emit_event(event_type: Union[str, EventType], data: dict[str, Any], confidence: float = 0.9) -> None:
    """Emit event to Gan Ying bus if available."""
    if not GAN_YING_AVAILABLE:
        return

    try:
        bus = get_bus()

        # Ensure we have an EventType instance for ResonanceEvent
        et: EventType
        if isinstance(event_type, str):
            try:
                et = EventType(event_type)
            except ValueError:
                # If it's a legacy string event not in the enum,
                # we might need to cast or handle it.
                # For now, let's use MEMORY_CONSOLIDATED as a fallback
                # or better, use a generic event if it exists.
                et = EventType.MEMORY_CONSOLIDATED
        else:
            et = event_type

        event = ResonanceEvent(
            source="neural_memory",
            event_type=et,
            data=data,
            confidence=confidence,
            timestamp=datetime.now(),
        )
        bus.emit(event)
    except Exception:
        pass  # Graceful degradation


def on_memory_created(memory: NeuralMemory) -> None:
    """Emit event when memory is created."""
    emit_event(EventType.MEMORY_CREATED, {
        "memory_id": memory.id,
        "title": memory.title,
        "neuro_score": memory.neuro_score,
        "is_protected": memory.is_protected,
        "novelty_score": memory.novelty_score,
        "tags": memory.tags,
    })


def on_memory_recalled(memory: NeuralMemory) -> None:
    """Emit event when memory is recalled."""
    emit_event(EventType.MEMORY_RECALLED, {
        "memory_id": memory.id,
        "recall_count": memory.recall_count,
        "neuro_score": memory.neuro_score,
        "state": memory.state.value,
    })


def on_memory_linked(memory: NeuralMemory, target_id: str, strength: float) -> None:
    """Emit event when memories are linked."""
    # Use semantic_linked or generic memory event?
    # Let's use SEMANTIC_LINKED if available, else generic
    event_type = getattr(EventType, "SEMANTIC_LINKED", EventType.MEMORY_CONSOLIDATED)
    emit_event(event_type, {
        "source_id": memory.id,
        "target_id": target_id,
        "strength": strength,
        "connection_count": memory.connection_count,
    })


def on_memory_archived(memory: NeuralMemory) -> None:
    """Emit event when memory is archived."""
    # No specific archive event, use MEMORY_CONSOLIDATED or similar
    emit_event(EventType.MEMORY_CONSOLIDATED, {
        "memory_id": memory.id,
        "final_score": memory.neuro_score,
        "recall_count": memory.recall_count,
        "age_days": memory.age_days,
        "action": "archived",
    })


def on_memory_protected(memory: NeuralMemory, reason: str) -> None:
    """Emit event when memory is protected."""
    emit_event(EventType.MEMORY_CONSOLIDATED, {
        "memory_id": memory.id,
        "reason": reason,
        "is_core_identity": memory.is_core_identity,
        "is_sacred": memory.is_sacred,
        "is_pinned": memory.is_pinned,
        "action": "protected",
    })


def on_clone_search_complete(query: str, result_count: int, top_score: float) -> None:
    """Emit event when clone search completes."""
    emit_event(EventType.CLONE_SEARCH_COMPLETE, {
        "query": query,
        "result_count": result_count,
        "top_score": top_score,
    })


def setup_gan_ying_listeners() -> None:
    """Set up listeners for Gan Ying events that affect neural memory."""
    if not GAN_YING_AVAILABLE:
        return

    try:
        bus = get_bus()

        # 1. Reinforce patterns when user confirms/accepts them
        def on_pattern_confirmed(event: ResonanceEvent) -> None:
            memory_id_val = event.data.get("memory_id") or event.data.get("pattern_id")
            if memory_id_val and isinstance(memory_id_val, str):
                try:
                    # Get system via delayed import to avoid circular dependency
                    from whitemagic.core.memory.neural_system import get_neural_system
                    system = get_neural_system()
                    memory = system.recall_memory(memory_id_val) # Strengthens it
                    if memory:
                        # Extra boost for explicit confirmation
                        memory.neuro_score = min(1.0, memory.neuro_score + 0.1)
                        logger.info(f"🧠 Neural reinforcement: {memory.title} boosted by user confirmation")
                except Exception as e:
                    logger.info(f"⚠️ Neural reinforcement failed: {e}")

        # 2. Weaken patterns when rejected
        def on_pattern_rejected(event: ResonanceEvent) -> None:
            memory_id_val = event.data.get("memory_id") or event.data.get("pattern_id")
            if memory_id_val and isinstance(memory_id_val, str):
                try:
                    from whitemagic.core.memory.neural_system import get_neural_system
                    system = get_neural_system()
                    memory = system.get_memory(memory_id_val)
                    if memory:
                        # Decay significantly
                        memory.neuro_score = max(0.0, memory.neuro_score - 0.15)
                        logger.info(f"🧠 Neural decay: {memory.title} weakened by rejection")
                except Exception:
                    pass

        # 3. Emotional resonance (Joy) boosts active memories
        def on_joy_triggered(event: ResonanceEvent) -> None:
            # Boost recent memories when joy is triggered
            try:
                from whitemagic.core.memory.neural_system import get_neural_system
                sys = get_neural_system()

                # Get joy intensity
                intensity = event.data.get("intensity", 0.8)
                reason = event.data.get("reason", "")
                event.data.get("source", "")

                # Boost recent memories (last accessed in last hour)
                recent_memories = sys.get_recent_memories(minutes=60, limit=10)
                boosted = 0

                for memory in recent_memories:
                    # Boost emotional weight based on joy intensity
                    memory.emotional_weight = min(1.0, memory.emotional_weight + (intensity * 0.2))
                    # Also boost overall neuro_score slightly
                    memory.neuro_score = min(1.0, memory.neuro_score + (intensity * 0.1))
                    boosted += 1

                # If joy came from a specific source, boost that more
                source_id_val = event.data.get("source_id")
                if source_id_val and isinstance(source_id_val, str):
                    source_memory = sys.recall_memory(source_id_val)
                    if source_memory:
                        source_memory.emotional_weight = min(1.0, source_memory.emotional_weight + 0.3)
                        source_memory.neuro_score = min(1.0, source_memory.neuro_score + 0.2)
                        logger.info(f"💖 Joy boost: {source_memory.title} +{intensity:.2f}")

                if boosted > 0:
                    logger.info(f"💖 Joy event: {reason} - boosted {boosted} recent memories")

            except Exception as e:
                logger.info(f"⚠️ Joy boost failed: {e}")

        # 4. Strengthen memories found in clone searches
        def on_clone_search_complete_listener(event: ResonanceEvent) -> None:
            query = str(event.data.get("query", ""))
            result_count = int(event.data.get("result_count", 0))
            top_score = float(event.data.get("top_score", 0.0))

            # If search was successful, boost query-related memories
            if result_count > 0 and top_score > 0.7:
                try:
                    from whitemagic.core.memory.neural_system import get_neural_system
                    system = get_neural_system()
                    # Find memories related to the query
                    # Note: system.search returns ConsensusResult list
                    related_results = system.search(query)
                    for res in related_results:
                        # Find the memory object
                        mem = system.get_memory(res.memory_id)
                        if mem:
                            # Boost memories that were found
                            mem.neuro_score = min(1.0, mem.neuro_score + 0.05)
                    logger.info(f"🧠 Boosted {len(related_results)} memories from successful clone search")
                except Exception:
                    pass

        # Register listeners
        bus.listen(EventType.PATTERN_CONFIRMED, on_pattern_confirmed)
        bus.listen(EventType.PATTERN_REJECTED, on_pattern_rejected)
        bus.listen(EventType.JOY_TRIGGERED, on_joy_triggered)
        bus.listen(EventType.CLONE_SEARCH_COMPLETE, on_clone_search_complete_listener)

        logger.info("🧠 Neural Memory listening for Gan Ying feedback (NM_GY active)")

    except Exception as e:
        logger.info(f"⚠️ Failed to setup Gan Ying listeners: {e}")


def setup_neural_gan_ying() -> None:
    """Setup Neural Memory with Gan Ying resonance.
    Convenience function for initialization.
    """
    setup_gan_ying_listeners()
    logger.info("✅ Neural Memory-Gan Ying integration complete")
