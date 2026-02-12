import logging
import json

from datetime import datetime

from typing import Optional, Dict, Any
from .collector import MetricsCollector

from whitemagic.config.paths import WM_ROOT
logger = logging.getLogger(__name__)

# Global metrics collector instance
_tracker = None

def get_tracker() -> MetricsCollector:
    """Get the global metrics tracker instance."""
    global _tracker
    if _tracker is None:
        _tracker = MetricsCollector()
    return _tracker

# Simple metric tracking function for backward compatibility
def track_metric(category: str, metric: Optional[str] = None, value: float = 1.0, context: Optional[str] = "") -> Dict[str, Any]:
    """Track a metric value and persist to JSONL."""
    if metric is None:
        metric = category
        category = "general"
    elif not isinstance(metric, str):
        value = float(metric)
        metric = category
        category = "general"

    metrics_dir = WM_ROOT / "metrics"
    metrics_dir.mkdir(parents=True, exist_ok=True)
    file_path = metrics_dir / f"{category}.jsonl"

    timestamp = datetime.now().isoformat()
    entry = {
        "metric": metric,
        "value": float(value),
        "context": context or "",
        "timestamp": timestamp,
    }

    with file_path.open("a", encoding="utf-8") as handle:
        handle.write(json.dumps(entry) + "\n")

    logger.info(f"METRIC: {category}.{metric}={value} @ {timestamp}")
    return {"success": True}

__all__ = ['MetricsCollector', 'track_metric', 'get_tracker']
