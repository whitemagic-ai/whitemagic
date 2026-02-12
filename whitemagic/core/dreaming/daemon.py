"""
WhiteMagic Dream Daemon
Purpose: Offline processing, memory consolidation, and system maintenance.
Runs in the background when the system is "idle" (or continuously).
"""
import time
import logging
import threading
import signal
import sys
import os
from pathlib import Path
from logging.handlers import RotatingFileHandler
from typing import Optional

# Logging configuration
LOG_FILE = os.getenv("WM_DREAM_LOG_FILE", "whitemagic_dream.log")
MAX_LOG_BYTES = int(os.getenv("WM_DREAM_LOG_MAX_BYTES", str(5 * 1024 * 1024)))
LOG_BACKUPS = int(os.getenv("WM_DREAM_LOG_BACKUPS", "5"))
MAX_LOG_TOTAL_BYTES = int(os.getenv("WM_DREAM_LOG_TOTAL_MAX_BYTES", str(50 * 1024 * 1024)))

# Configure logging for the daemon
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - [DREAM] - %(levelname)s - %(message)s',
    handlers=[
        RotatingFileHandler(LOG_FILE, maxBytes=MAX_LOG_BYTES, backupCount=LOG_BACKUPS),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger("dream_daemon")

class DreamDaemon:
    def __init__(self, interval_seconds: int = 600) -> None:
        self.interval = interval_seconds
        self.running = False
        self.thread: Optional[threading.Thread] = None
        self._stop_event = threading.Event()

    def start(self) -> None:
        """Start the dream loop in a separate thread (or main thread if daemon mode)."""
        if self.running:
            logger.warning("Dream daemon already running")
            return

        logger.info(f"Starting Dream Daemon (Interval: {self.interval}s)")
        self.running = True
        self._stop_event.clear()

        # Setup signal handlers for graceful shutdown if running as main process
        signal.signal(signal.SIGINT, self._signal_handler)
        signal.signal(signal.SIGTERM, self._signal_handler)

        self.run_loop()

    def stop(self) -> None:
        """Stop the dream daemon."""
        logger.info("Stopping Dream Daemon...")
        self.running = False
        self._stop_event.set()

    def _signal_handler(self, signum: int, frame: object) -> None:
        logger.info(f"Received signal {signum}, stopping...")
        self.stop()
        sys.exit(0)

    def run_loop(self) -> None:
        """Main processing loop."""
        while self.running and not self._stop_event.is_set():
            try:
                self.dream_cycle()
            except Exception as e:
                logger.error(f"Error in dream cycle: {e}", exc_info=True)

            # Sleep for interval, checking for stop event
            if self._stop_event.wait(self.interval):
                break

        logger.info("Dream Daemon stopped.")

    def dream_cycle(self) -> None:
        """Execute one cycle of dream processing tasks."""
        logger.info("💤 Entering Dream State...")
        start_time = time.time()

        # 1. Memory Consolidation (Mock)
        self._consolidate_memories()

        # 2. Log Rotation/Compression (Mock)
        self._maintain_logs()

        # 3. Insight Generation (Mock)
        self._generate_insights()

        duration = time.time() - start_time
        logger.info(f"✨ Dream State ended. Duration: {duration:.2f}s")

    def _consolidate_memories(self) -> None:
        """Move short-term memories to long-term or prune them."""
        try:
            from whitemagic.core.memory.unified import consolidate
            consolidated = consolidate()
            logger.info(f"  - Consolidated memories: {consolidated}")
        except Exception as e:
            logger.warning(f"  - Memory consolidation skipped: {e}")

    def _maintain_logs(self) -> None:
        """Compress old logs."""
        log_path = Path(LOG_FILE)
        log_dir = log_path.parent if log_path.parent else Path(".")
        total_bytes = 0
        for path in log_dir.glob(f"{log_path.name}*"):
            if path.is_file():
                total_bytes += path.stat().st_size

        if total_bytes > MAX_LOG_TOTAL_BYTES:
            logger.warning(
                "  - Log files exceed max size: %.2fMB (max %.2fMB)",
                total_bytes / (1024 * 1024),
                MAX_LOG_TOTAL_BYTES / (1024 * 1024),
            )
        else:
            logger.info(
                "  - Log maintenance OK: %.2fMB total",
                total_bytes / (1024 * 1024),
            )

    def _generate_insights(self) -> None:
        """Generate insights from recent activity."""
        if os.getenv("WM_DREAM_INSIGHTS", "1").lower() in {"0", "false", "no"}:
            logger.info("  - Insight generation disabled")
            return

        try:
            from whitemagic.core.memory.manager import MemoryManager
            manager = MemoryManager()
            recent = manager.read_recent_memories(limit=5)
            if not recent:
                logger.info("  - No recent memories to summarize")
                return

            titles = [m.get("frontmatter", {}).get("title") for m in recent]
            titles = [t for t in titles if t]
            tag_counts: dict[str, int] = {}
            for memory in recent:
                tags = memory.get("frontmatter", {}).get("tags", [])
                for tag in tags:
                    tag_counts[tag] = tag_counts.get(tag, 0) + 1
            top_tags = sorted(tag_counts.items(), key=lambda x: x[1], reverse=True)[:3]
            tag_summary = ", ".join([f"{tag}({count})" for tag, count in top_tags]) if top_tags else "none"
            logger.info(f"  - Recent memories: {len(recent)} | Top tags: {tag_summary}")
            if titles:
                logger.info(f"  - Recent titles: {', '.join(titles[:3])}")
        except Exception as e:
            logger.warning(f"  - Insight generation skipped: {e}")

# Singleton instance
_daemon: DreamDaemon | None = None

def get_daemon() -> DreamDaemon:
    global _daemon
    if _daemon is None:
        _daemon = DreamDaemon()
    return _daemon

if __name__ == "__main__":
    daemon = DreamDaemon(interval_seconds=60) # Fast interval for testing
    daemon.start()
