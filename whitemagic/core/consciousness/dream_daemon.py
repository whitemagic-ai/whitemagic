"""WhiteMagic Dream Daemon
Purpose: Offline processing, memory consolidation, and system maintenance.
Runs in the background when the system is "idle" (or continuously).
"""
import logging
import signal
import sys
import threading
import time

# Configure logging for the daemon
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - [DREAM] - %(levelname)s - %(message)s",
    handlers=[
        logging.FileHandler("whitemagic_dream.log"),
        logging.StreamHandler(),
    ],
)
logger = logging.getLogger("dream_daemon")

class DreamDaemon:
    def __init__(self, interval_seconds: int = 600) -> None:
        self.interval = interval_seconds
        self.running = False
        self.thread: threading.Thread | None = None
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
            from whitemagic.automation.consolidator import consolidate_now
            report = consolidate_now(minutes=self.interval // 60 or 60)
            created = report.get("semantic_created", 0)
            archived = report.get("episodic_archived", 0)
            logger.info(f"  - Consolidated memories: {created} semantic created, {archived} episodic archived")
        except Exception as e:
            logger.warning(f"  - Memory consolidation skipped: {e}")

    def _maintain_logs(self) -> None:
        """Compress old logs and prune oversized files."""
        import gzip
        from pathlib import Path

        try:
            from whitemagic.config.paths import get_state_root
            log_dir = get_state_root() / "logs"
        except Exception:
            log_dir = Path.home() / ".whitemagic" / "logs"

        if not log_dir.exists():
            logger.info("  - No log directory found, skipping maintenance")
            return

        compressed = 0
        for log_file in log_dir.glob("*.log"):
            try:
                if log_file.stat().st_size > 5_000_000:  # >5MB
                    gz_path = log_file.with_suffix(".log.gz")
                    with open(log_file, "rb") as f_in, gzip.open(gz_path, "wb") as f_out:
                        f_out.writelines(f_in)
                    log_file.unlink()
                    compressed += 1
            except Exception as e:
                logger.warning(f"  - Failed to compress {log_file.name}: {e}")

        logger.info(f"  - Log maintenance: {compressed} files compressed")

    def _generate_insights(self) -> None:
        """Generate insights from recent activity via bridge synthesis and resonance."""
        # Attempt bridge synthesis for serendipitous connections
        insights_found = 0
        try:
            from whitemagic.core.intelligence.synthesis.bridge_synthesizer import BridgeSynthesizer
            synth = BridgeSynthesizer()
            bridges = synth.find_bridges(top_k=3)
            insights_found = len(bridges) if bridges else 0
            if insights_found > 0:
                logger.info(f"  - Bridge synthesis found {insights_found} cross-domain connections")
        except Exception as e:
            logger.debug(f"  - Bridge synthesis unavailable: {e}")

        # Calculate system resonance via Julia
        resonance = self._calculate_resonance("system_state_snapshot")
        logger.info(f"  - Insights: {insights_found} bridges, resonance={resonance:.4f}")
        if resonance > 0.8:
            logger.info("  🌟 HIGH RESONANCE DETECTED! Triggering deep consolidation.")

    def _calculate_resonance(self, impulse_data: str) -> float:
        """Call Julia Gan Ying engine to measure resonance."""
        try:
            import json
            import os
            import subprocess
            from pathlib import Path

            # Find the Julia script relative to this file
            # whitemagic/core/consciousness/dream_daemon.py -> root -> whitemagic-julia
            ROOT = Path(__file__).resolve().parent.parent.parent.parent
            julia_script = str(ROOT / "whitemagic-julia" / "src" / "gan_ying.jl")

            if not os.path.exists(julia_script):
                return 0.5

            # Execute
            input_val = json.dumps({"magnitude": 0.5, "damping": 0.1})
            result = subprocess.run(
                ["julia", julia_script, input_val],
                capture_output=True, text=True, timeout=10,
            )

            if result.returncode == 0:
                data = json.loads(result.stdout)
                # Normalize arbitrary resonance val
                return float(min(data.get("total_resonance", 0.0) / 10000.0, 1.0))
            return 0.5
        except Exception as e:
            logger.error(f"Resonance calc failed: {e}")
            return 0.5

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
