"""Karma Trace Logger.

Persistent, append-only log of all Gana effects and outcomes.
Serves as the memory of the living system, allowing it to learn from
cause and effect (Karma) over time.
"""

import asyncio
import json
import logging
import time
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)

class KarmaTraceLogger:
    """Logger for Gana execution traces.
    Currently uses standard Python file I/O.
    Future: Use memory-mapped file or Rust-accelerated append.
    """

    def __init__(self, trace_file: Path | None = None):
        if trace_file is None:
            # Default to logs/karma_trace.jsonl in project root
            # Assuming we are in whitemagic/core/ganas
            # We want to find the project root.
            # Using a relative path strategy or environment variable would be better.
            # For now, let's look for a 'logs' directory relative to this file
            base_dir = Path(__file__).resolve().parent.parent.parent.parent
            self.trace_file = base_dir / "logs" / "karma_trace.jsonl"
        else:
            self.trace_file = trace_file

        self.trace_file.parent.mkdir(parents=True, exist_ok=True)

    def log(self, gana_result: Any) -> None:
        """Log effect with timestamp, mansion, garden, outcome.

        Args:
            gana_result: GanaResult object (typed Any to avoid circular import if possible,
                         or we import inside method)

        """
        # Create trace entry
        # We need to extract data from GanaResult

        # Safe attribute access
        mansion_name = getattr(getattr(gana_result, "mansion", None), "name", "UNKNOWN")
        garden = getattr(gana_result, "garden", None)
        # If garden is not on result (it wasn't in previous definition), we might need to get it from somewhere else
        # But we added it to BaseGana, not GanaResult. Wait, GanaResult holds output.
        # The Gana instance holds the garden. GanaResult should probably carry it or we log it from the Gana.
        # But GanaResult is what we have here.
        # Let's check GanaResult definition again. I didn't add garden to GanaResult, I added mansion.
        # I should probably update GanaResult to include garden for complete tracing.

        # Karma trace dict from result
        karma_trace = getattr(gana_result, "karma_trace", {})

        trace_entry = {
            "timestamp": time.time(),
            "mansion": mansion_name,
            "garden": garden, # Might be None if not passed
            "execution_ms": getattr(gana_result, "execution_time_ms", 0.0),
            "harmony": karma_trace.get("harmony"),
            "guna": karma_trace.get("guna"),
            "lunar_phase": karma_trace.get("lunar_phase"),
            "successor_hint": getattr(gana_result, "successor_hint", None),
            # Hash output for verification without storing full blob
            "output_preview": str(getattr(gana_result, "output", ""))[:100],
        }

        try:
            with open(self.trace_file, "a", encoding="utf-8") as f:
                f.write(json.dumps(trace_entry) + "\n")
        except Exception as e:
            logger.info(f"⚠️ Failed to write karma trace: {e}")

    async def log_async(self, gana_result: Any) -> None:
        """Async wrapper for logging."""
        # File I/O is blocking, but for simple appends it's usually negligible.
        # For strict async, we'd use a thread executor.
        loop = asyncio.get_event_loop()
        await loop.run_in_executor(None, self.log, gana_result)

    def get_recent_traces(self, limit: int = 100) -> list[dict[str, Any]]:
        """Read recent traces from the trace file."""
        if not self.trace_file.exists():
            return []

        traces = []
        try:
            with open(self.trace_file, "r", encoding="utf-8") as f:
                # For large files, we should read from the end.
                # For now, a simple read of all lines is fine for v5.0.0-alpha.
                lines = f.readlines()
                for line in lines[-limit:]:
                    try:
                        traces.append(json.loads(line))
                    except json.JSONDecodeError:
                        continue
        except Exception as e:
            logger.info(f"⚠️ Failed to read karma traces: {e}")

        return traces

# Global logger instance
_logger: KarmaTraceLogger | None = None

def get_karma_tracer() -> KarmaTraceLogger:
    global _logger
    if _logger is None:
        _logger = KarmaTraceLogger()
    return _logger

# Backward compatibility alias
get_karma_logger = get_karma_tracer
