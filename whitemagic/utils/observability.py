
from typing import Any
import logging

logger = logging.getLogger(__name__)

def track_metric(name: str, value: float, tags: dict[str, str] | None = None) -> None:
    """Stub for metric tracking.
    In the future, this will connect to Prometheus or similar.
    """
    if tags:
        tag_str = " ".join([f"{k}={v}" for k, v in tags.items()])
        logger.debug(f"[METRIC] {name}: {value} ({tag_str})")
    else:
        logger.debug(f"[METRIC] {name}: {value}")

def get_tracker() -> Any:
    """Returns a dummy tracker object for compatibility."""
    class DummyTracker:
        def track(self, *args: Any, **kwargs: Any) -> Any:
            track_metric(*args, **kwargs)
    return DummyTracker()
