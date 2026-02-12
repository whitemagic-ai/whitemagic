"""⏰ Decay Daemon - Background Memory Decay Processing
Runs periodically to decay memories and archive weak ones.
Created: December 2, 2025 (Hanuman Tuesday).
"""

import logging
import threading
from collections.abc import Callable
from datetime import datetime

from whitemagic.core.memory.neural.neural_memory import NeuralMemory
from whitemagic.core.memory.neural.neuro_score import get_engine
from whitemagic.core.memory.neural.persistence import get_store

logger = logging.getLogger(__name__)


class DecayDaemon:
    """Background daemon for memory decay processing."""

    def __init__(
        self,
        interval_hours: float = 24.0,
        archive_threshold: float = 0.2,
        on_archive: Callable[[NeuralMemory], None] | None = None,
    ):
        self.interval_hours = interval_hours
        self.archive_threshold = archive_threshold
        self.on_archive = on_archive
        self._running = False
        self._thread: threading.Thread | None = None
        self._last_run: datetime | None = None
        self._stats = {
            "runs": 0,
            "memories_processed": 0,
            "memories_archived": 0,
        }

    def start(self) -> None:
        """Start the decay daemon in background thread."""
        if self._running:
            return

        self._running = True
        self._thread = threading.Thread(target=self._run_loop, daemon=True)
        self._thread.start()
        logger.info("Decay daemon started")

    def stop(self) -> None:
        """Stop the decay daemon."""
        self._running = False
        if self._thread:
            self._thread.join(timeout=5.0)
        logger.info("Decay daemon stopped")

    def _run_loop(self) -> None:
        """Main daemon loop."""
        while self._running:
            try:
                self.process_decay()
            except Exception as e:
                logger.error(f"Decay processing error: {e}")

            # Sleep until next run
            sleep_seconds = self.interval_hours * 3600
            for _ in range(int(sleep_seconds)):
                if not self._running:
                    break
                threading.Event().wait(1.0)

    def process_decay(self) -> dict:
        """Process decay for all memories."""
        store = get_store()
        engine = get_engine()

        memories = store.load_all()
        archived = []

        for memory in memories:
            # Apply decay
            memory.decay()

            # Recalculate score
            score = engine.calculate_score(memory)
            if isinstance(score, float):
                memory.neuro_score = score
            else:
                memory.neuro_score = score.final_score

            # Check archive threshold
            if memory.neuro_score < self.archive_threshold and not memory.is_protected:
                archived.append(memory)
                if self.on_archive:
                    self.on_archive(memory)

            # Save updated memory
            store.save(memory)

        self._last_run = datetime.now()
        self._stats["runs"] += 1
        self._stats["memories_processed"] += len(memories)
        self._stats["memories_archived"] += len(archived)

        logger.info(f"Decay processed: {len(memories)} memories, {len(archived)} archived")

        return {
            "processed": len(memories),
            "archived": len(archived),
            "timestamp": self._last_run.isoformat(),
        }

    def run_once(self) -> dict:
        """Run decay processing once (manual trigger)."""
        return self.process_decay()

    def get_stats(self) -> dict:
        """Get daemon statistics."""
        return {
            **self._stats,
            "running": self._running,
            "last_run": self._last_run.isoformat() if self._last_run else None,
            "interval_hours": self.interval_hours,
            "archive_threshold": self.archive_threshold,
        }


# Singleton
_daemon: DecayDaemon | None = None


def get_daemon() -> DecayDaemon:
    """Get the singleton decay daemon."""
    global _daemon
    if _daemon is None:
        _daemon = DecayDaemon()
    return _daemon


def start_decay_daemon() -> None:
    """Start the background decay daemon."""
    get_daemon().start()


def stop_decay_daemon() -> None:
    """Stop the background decay daemon."""
    get_daemon().stop()


def run_decay_now() -> dict:
    """Run decay processing immediately."""
    return get_daemon().run_once()
