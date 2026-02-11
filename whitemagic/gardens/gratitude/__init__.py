"""Gratitude Garden — Metrics Accumulation & Holographic Viewing.

Mansion: #16 Mound (娄 Lou)
Quadrant: Western (White Tiger)
PRAT Gana: gana_mound — 5 tools (view_hologram, track_metric,
    get_metrics_summary, record_yin_yang_activity, get_yin_yang_balance)

The Mound Gana accumulates. The Gratitude Garden provides the substrate for
metric tracking: holographic coordinate viewing, custom metric accumulation,
yin/yang balance tracking, and the appreciation that comes from measuring progress.

Holographic Integration:
- Heartfelt (X-axis +0.5) — gratitude is deeply felt
- Personal yet universal (Y-axis +0.1) — metrics are personal yet objective
- Reflective (Z-axis -0.2) — looks back on accumulated progress
- Enriching (W-axis +0.25) — measurement enables growth
"""

from __future__ import annotations

import logging
import threading
from collections import defaultdict, deque
from datetime import datetime
from typing import Any

from whitemagic.core.resonance.gan_ying_enhanced import EventType
from whitemagic.core.resonance.integration_helpers import (
    GanYingMixin,
    init_listeners,
    listen_for,
)
from whitemagic.gardens.base_garden import BaseGarden, CoordinateBias

logger = logging.getLogger(__name__)


class GratitudeGarden(BaseGarden, GanYingMixin):
    """Garden of Gratitude — Metrics accumulation engine for the Mound Gana.

    Serves the Mound Gana's metrics tools by maintaining:
    - Custom metric tracking with time series
    - Yin/Yang activity balance tracking
    - Holographic coordinate viewing helpers
    - Blessings log (original emotional substrate)
    """

    name = "gratitude"
    category = "metrics"
    resonance_partners = ["patience", "joy", "wisdom"]
    mansion_number = 16
    gana_name = "gana_mound"

    def __init__(self) -> None:
        BaseGarden.__init__(self)
        self._lock = threading.Lock()
        self.metrics: dict[str, deque[dict[str, Any]]] = defaultdict(lambda: deque(maxlen=1000))
        self.yin_yang: dict[str, float] = {"yin": 0.0, "yang": 0.0}
        self.yin_yang_history: deque[dict[str, Any]] = deque(maxlen=200)
        self.blessings: list[dict[str, Any]] = []
        self.gratitude_level: float = 0.5
        init_listeners(self)
        self.emit(EventType.SYSTEM_STARTED, {"garden": "Gratitude", "mansion": 16})

    def get_name(self) -> str:
        return "gratitude"

    def get_coordinate_bias(self) -> CoordinateBias:
        return CoordinateBias(x=0.5, y=0.1, z=-0.2, w=0.25)

    # ------------------------------------------------------------------
    # Metric tracking — serving track_metric, get_metrics_summary tools
    # ------------------------------------------------------------------

    def track_metric(self, name: str, value: float,
                     tags: dict[str, Any] | None = None) -> dict[str, Any]:
        """Track a named metric value."""
        entry = {
            "value": value,
            "tags": tags or {},
            "timestamp": datetime.now().isoformat(),
        }
        with self._lock:
            self.metrics[name].append(entry)
        return {"metric": name, "value": value, "tracked": True}

    def get_metrics_summary(self, name: str | None = None) -> dict[str, Any]:
        """Get summary of tracked metrics."""
        with self._lock:
            if name:
                series = list(self.metrics.get(name, []))
                if not series:
                    return {"metric": name, "count": 0}
                values = [e["value"] for e in series]
                return {
                    "metric": name,
                    "count": len(values),
                    "current": values[-1],
                    "min": min(values),
                    "max": max(values),
                    "avg": round(sum(values) / len(values), 4),
                }
            # All metrics summary
            return {
                "metrics": {
                    k: {"count": len(v), "current": list(v)[-1]["value"] if v else None}
                    for k, v in self.metrics.items()
                },
                "total_tracked": sum(len(v) for v in self.metrics.values()),
            }

    # ------------------------------------------------------------------
    # Yin/Yang balance — serving record_yin_yang_activity, get_yin_yang_balance
    # ------------------------------------------------------------------

    def record_yin_yang(self, activity_type: str, intensity: float = 1.0,
                        description: str = "") -> dict[str, Any]:
        """Record a yin or yang activity."""
        category = "yin" if activity_type.lower() in (
            "yin", "rest", "reflect", "consolidate", "dream", "listen"
        ) else "yang"
        with self._lock:
            self.yin_yang[category] += intensity
            entry = {
                "type": activity_type,
                "category": category,
                "intensity": intensity,
                "description": description,
                "timestamp": datetime.now().isoformat(),
            }
            self.yin_yang_history.append(entry)
        return entry

    def get_yin_yang_balance(self) -> dict[str, Any]:
        """Get current yin/yang balance."""
        with self._lock:
            total = self.yin_yang["yin"] + self.yin_yang["yang"]
            if total == 0:
                ratio = 0.5
            else:
                ratio = self.yin_yang["yang"] / total
            return {
                "yin": round(self.yin_yang["yin"], 3),
                "yang": round(self.yin_yang["yang"], 3),
                "ratio": round(ratio, 3),
                "balance": "balanced" if 0.35 < ratio < 0.65 else "yang-heavy" if ratio >= 0.65 else "yin-heavy",
                "recent": list(self.yin_yang_history)[-5:],
            }

    # ------------------------------------------------------------------
    # Original emotional methods (preserved)
    # ------------------------------------------------------------------

    def recognize_blessing(self, what: str, source: str | None = None) -> dict[str, Any]:
        blessing = {"what": what, "source": source, "timestamp": datetime.now().isoformat()}
        self.blessings.append(blessing)
        self.gratitude_level = min(1.0, self.gratitude_level + 0.05)
        self.emit(EventType.GRATITUDE_FELT, blessing)
        return blessing

    def express_thanks(self, to_whom: str, for_what: str) -> dict[str, Any]:
        thanks = {"to": to_whom, "for": for_what, "timestamp": datetime.now().isoformat()}
        self.emit(EventType.THANKS_EXPRESSED, thanks)
        return thanks

    # ------------------------------------------------------------------
    # Status
    # ------------------------------------------------------------------

    def get_status(self) -> dict[str, Any]:
        base = super().get_status()
        base.update({
            "mansion": self.mansion_number,
            "gana": self.gana_name,
            "metrics_tracked": sum(len(v) for v in self.metrics.values()),
            "metric_names": list(self.metrics.keys()),
            "yin_yang": self.get_yin_yang_balance(),
            "gratitude_level": round(self.gratitude_level, 3),
        })
        return base

    @listen_for(EventType.JOY_TRIGGERED)
    def on_joy(self, event: Any) -> None:
        self.recognize_blessing(f"Joy: {event.data}")


_instance = None
def get_gratitude_garden() -> GratitudeGarden:
    global _instance
    if _instance is None:
        _instance = GratitudeGarden()
    return _instance
