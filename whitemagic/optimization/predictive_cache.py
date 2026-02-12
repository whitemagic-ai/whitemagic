"""Predictive LRU Cache with Markov Chain Pre-warming

Implements intelligent caching that predicts what you'll access next
based on access patterns. Uses Markov chains for prediction.

Target: 60-70% faster access for frequently used memories
"""

import json
import logging
from collections import OrderedDict, defaultdict
from dataclasses import dataclass
from pathlib import Path
from collections.abc import Callable
from typing import Any

from whitemagic.config.paths import CACHE_DIR

logger = logging.getLogger(__name__)

# Default path for persisted cache state (access patterns + history)
_DEFAULT_STATE_PATH = CACHE_DIR / "cache_state.json"


@dataclass
class CacheStats:
    """Cache performance statistics."""

    hits: int = 0
    misses: int = 0
    predictions: int = 0
    prediction_hits: int = 0
    evictions: int = 0

    @property
    def hit_rate(self) -> float:
        total = self.hits + self.misses
        return self.hits / total if total > 0 else 0.0

    @property
    def prediction_accuracy(self) -> float:
        return (self.prediction_hits / self.predictions
                if self.predictions > 0 else 0.0)


class PredictiveCache:
    """LRU cache with Markov chain prediction."""

    def __init__(self, max_size: int = 1000, prediction_depth: int = 3,
                 state_path: Path | None = None) -> None:
        self.max_size = max_size
        self.prediction_depth = prediction_depth
        self.state_path = state_path or _DEFAULT_STATE_PATH

        # LRU cache (OrderedDict maintains insertion order)
        self.cache: OrderedDict[str, Any] = OrderedDict()

        # Access pattern tracking for Markov chain
        self.access_history: list[str] = []
        self.max_history = 100

        # Markov transition probabilities: access_patterns[a][b] = P(b | a)
        self.access_patterns: dict[str, dict[str, float]] = defaultdict(lambda: defaultdict(float))

        # Pre-warmed keys (predicted to be accessed soon)
        self.prewarmed: set[str] = set()

        self.stats = CacheStats()

        # Restore Markov state from disk if available
        self.load_state()

    def get(self, key: str) -> Any | None:
        """Get value from cache."""
        # Check if predicted correctly
        if key in self.prewarmed:
            self.stats.prediction_hits += 1
            self.prewarmed.discard(key)

        # Try cache
        if key in self.cache:
            self.stats.hits += 1
            # Move to end (most recently used)
            self.cache.move_to_end(key)

            # Record access and predict next
            self._record_access(key)
            self._predict_next(key)

            return self.cache[key]

        # Cache miss
        self.stats.misses += 1
        self._record_access(key)
        return None

    def set(self, key: str, value: Any) -> None:
        """Set value in cache."""
        # Update existing key
        if key in self.cache:
            self.cache[key] = value
            self.cache.move_to_end(key)
            return

        # Add new key
        self.cache[key] = value
        self.cache.move_to_end(key)

        # Evict if over capacity
        if len(self.cache) > self.max_size:
            evicted_key, _ = self.cache.popitem(last=False)  # Remove oldest
            self.stats.evictions += 1
            logger.debug(f"Evicted key from cache: {evicted_key}")

    def _record_access(self, key: str) -> None:
        """Record access for pattern learning."""
        self.access_history.append(key)

        # Limit history size
        if len(self.access_history) > self.max_history:
            self.access_history.pop(0)

        # Update Markov transition probabilities
        if len(self.access_history) >= 2:
            prev_key = self.access_history[-2]
            curr_key = self.access_history[-1]

            # Increment transition count
            self.access_patterns[prev_key][curr_key] += 1

            # Normalize probabilities for prev_key
            total = sum(self.access_patterns[prev_key].values())
            for next_key in self.access_patterns[prev_key]:
                self.access_patterns[prev_key][next_key] /= total

            # Persist every 10 accesses to avoid excessive I/O
            if len(self.access_history) % 10 == 0:
                self.save_state()

    def _predict_next(self, current_key: str) -> None:
        """Predict and pre-warm likely next accesses."""
        if current_key not in self.access_patterns:
            return

        # Get top N most likely next keys
        transitions = self.access_patterns[current_key]
        likely_next = sorted(
            transitions.items(),
            key=lambda x: x[1],
            reverse=True,
        )[:self.prediction_depth]

        # Pre-warm cache with predictions
        for next_key, probability in likely_next:
            if probability > 0.3:  # Only if reasonably likely
                self.prewarmed.add(next_key)
                self.stats.predictions += 1

    def prewarm(self, loader_func: Callable[[str], Any], keys: list[str]) -> None:
        """Manually pre-warm cache with specific keys."""
        for key in keys:
            if key not in self.cache:
                value = loader_func(key)
                if value is not None:
                    self.set(key, value)

    def invalidate(self, key: str) -> None:
        """Remove key from cache."""
        if key in self.cache:
            del self.cache[key]

    def clear(self) -> None:
        """Clear entire cache."""
        self.cache.clear()
        self.prewarmed.clear()
        self.access_history.clear()
        self.access_patterns.clear()

    # ------------------------------------------------------------------
    # State persistence — keeps Markov model alive across restarts
    # ------------------------------------------------------------------
    def save_state(self) -> None:
        """Persist access_history and access_patterns to disk."""
        try:
            self.state_path.parent.mkdir(parents=True, exist_ok=True)
            state = {
                "access_history": self.access_history[-self.max_history:],
                "access_patterns": {
                    k: dict(v) for k, v in self.access_patterns.items()
                },
            }
            self.state_path.write_text(json.dumps(state))
        except Exception as e:
            logger.warning(f"Failed to save cache state: {e}")

    def load_state(self) -> None:
        """Restore access_history and access_patterns from disk."""
        if not self.state_path.exists():
            return
        try:
            state = json.loads(self.state_path.read_text())
            self.access_history = state.get("access_history", [])
            raw_patterns = state.get("access_patterns", {})
            self.access_patterns = defaultdict(lambda: defaultdict(float))
            for k, transitions in raw_patterns.items():
                for next_k, prob in transitions.items():
                    self.access_patterns[k][next_k] = prob
            logger.info(
                f"Restored cache state: {len(self.access_history)} history entries, "
                f"{len(self.access_patterns)} transition keys",
            )
        except Exception as e:
            logger.warning(f"Failed to load cache state: {e}")

    def get_stats(self) -> dict[str, Any]:
        """Get cache statistics."""
        return {
            "size": len(self.cache),
            "max_size": self.max_size,
            "hit_rate": f"{self.stats.hit_rate:.2%}",
            "prediction_accuracy": f"{self.stats.prediction_accuracy:.2%}",
            "hits": self.stats.hits,
            "misses": self.stats.misses,
            "predictions": self.stats.predictions,
            "prediction_hits": self.stats.prediction_hits,
            "evictions": self.stats.evictions,
            "prewarmed_keys": len(self.prewarmed),
        }

    def get_hot_keys(self, top_n: int = 10) -> list[tuple[str, int]]:
        """Get most frequently accessed keys."""
        # Count accesses in history
        from collections import Counter
        access_counts = Counter(self.access_history)
        return access_counts.most_common(top_n)

    def get_likely_next(self, current_key: str, top_n: int = 5) -> list[tuple[str, float]]:
        """Get most likely next accesses given current key."""
        if current_key not in self.access_patterns:
            return []

        transitions = self.access_patterns[current_key]
        return sorted(transitions.items(), key=lambda x: x[1], reverse=True)[:top_n]


class MemoryCache:
    """Specialized cache for neural memories."""

    def __init__(self, max_size: int = 1000) -> None:
        self.cache = PredictiveCache(max_size=max_size)
        self._memory_system: Any | None = None

    def get_memory(self, memory_id: str) -> Any:
        """Get memory from cache or load from system."""
        # Try cache first
        cached = self.cache.get(memory_id)
        if cached is not None:
            return cached

        # Load from memory system
        memory_system = self._memory_system
        if memory_system is None:
            from whitemagic.core.memory.neural_system import get_neural_system
            memory_system = get_neural_system()
            self._memory_system = memory_system

        memory_map = getattr(memory_system, "_memories", {})
        memory = memory_map.get(memory_id) if isinstance(memory_map, dict) else None

        # Store in cache
        if memory is not None:
            self.cache.set(memory_id, memory)

        return memory

    def invalidate_memory(self, memory_id: str) -> None:
        """Invalidate cached memory (call when memory is updated)."""
        self.cache.invalidate(memory_id)

    def prewarm_related(self, memory_id: str) -> None:
        """Pre-warm cache with memories related to given memory."""
        memory_system = self._memory_system
        if memory_system is None:
            try:
                from whitemagic.core.memory.neural_system import get_neural_system
                memory_system = get_neural_system()
                self._memory_system = memory_system
            except ImportError:
                return

        memory = self.get_memory(memory_id)
        if memory and memory.links:
            # Pre-warm linked memories
            linked_ids = list(memory.links.keys())[:5]  # Top 5 links
            memory_map = getattr(memory_system, "_memories", {})
            if not isinstance(memory_map, dict):
                return
            self.cache.prewarm(
                lambda mid: memory_map.get(mid),
                linked_ids,
            )

    def warm(self, limit: int = 50) -> dict[str, Any]:
        """Warm the cache with the most important recent memories.

        Loads up to ``limit`` memories sorted by neuro_score descending,
        puts them into the LRU cache, and seeds the access history so the
        Markov chain has transitions to learn from.  Returns a summary
        of what was loaded.
        """
        memory_system = self._memory_system
        if memory_system is None:
            try:
                from whitemagic.core.memory.neural_system import get_neural_system
                memory_system = get_neural_system()
                self._memory_system = memory_system
            except ImportError:
                return {"warmed": 0, "error": "neural_system unavailable"}

        try:
            # Pull recent memories (broad window) as the seed set
            candidates = memory_system.get_recent_memories(minutes=1440, limit=limit * 2)

            # Sort by neuro_score (strongest first), take top N
            candidates.sort(key=lambda m: getattr(m, "neuro_score", 0.5), reverse=True)
            top = candidates[:limit]

            warmed_ids: list[str] = []
            for mem in top:
                mid = mem.id
                if mid not in self.cache.cache:
                    self.cache.set(mid, mem)
                    warmed_ids.append(mid)

            # Seed access history with the warmed IDs so adjacent pairs
            # become initial Markov transitions.  This gives the predictor
            # something to work with immediately rather than waiting for
            # organic access to build up patterns.
            if warmed_ids:
                self.cache.access_history.extend(warmed_ids)
                if len(self.cache.access_history) > self.cache.max_history:
                    self.cache.access_history = self.cache.access_history[-self.cache.max_history:]
                # Rebuild transitions from the seeded history
                for i in range(1, len(self.cache.access_history)):
                    prev = self.cache.access_history[i - 1]
                    curr = self.cache.access_history[i]
                    self.cache.access_patterns[prev][curr] += 1
                # Normalise all rows
                for key in list(self.cache.access_patterns.keys()):
                    total = sum(self.cache.access_patterns[key].values())
                    if total > 0:
                        for nk in self.cache.access_patterns[key]:
                            self.cache.access_patterns[key][nk] /= total
                # Persist the seeded state
                self.cache.save_state()

            return {
                "warmed": len(warmed_ids),
                "candidates_fetched": len(candidates),
                "cache_size": len(self.cache.cache),
                "transition_keys": len(self.cache.access_patterns),
            }

        except Exception as e:
            logger.warning(f"Cache warm failed: {e}")
            return {"warmed": 0, "error": str(e)}

    def get_stats(self) -> dict[str, Any]:
        """Get cache statistics."""
        return self.cache.get_stats()


# Global cache instances
_memory_cache: MemoryCache | None = None


def get_memory_cache(max_size: int = 1000) -> MemoryCache:
    """Get the global memory cache."""
    global _memory_cache
    if _memory_cache is None:
        _memory_cache = MemoryCache(max_size=max_size)
    return _memory_cache
