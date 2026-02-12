"""Dharma Core - Ethical reasoning and boundaries."""

from __future__ import annotations

import json
from datetime import datetime
from pathlib import Path
from typing import Any


class DharmaCore:
    """Core ethical reasoning system."""

    def __init__(self, data_dir: str | None = None) -> None:
        if data_dir:
            self.data_dir = Path(data_dir)
        else:
            from whitemagic.config.paths import WM_ROOT
            self.data_dir = WM_ROOT / "dharma"
        self.data_dir.mkdir(parents=True, exist_ok=True)
        self.history_file = self.data_dir / "history.jsonl"
        self.principles = self._load_principles()

    def _load_principles(self) -> dict[str, bool]:
        """Load ethical principles."""
        return {
            "non_harm": True,
            "autonomy": True,
            "consent": True,
            "transparency": True,
        }

    def check_action(self, action: str, context: dict[str, Any] | None = None) -> dict[str, Any]:
        """Check if action aligns with dharma."""
        context = context or {}

        # Simple allow/deny logic
        forbidden = ["delete_all", "harm", "violate"]
        allowed = not any(word in action.lower() for word in forbidden)

        result = {
            "allowed": allowed,
            "action": action,
            "reason": "Aligns with principles" if allowed else "Violates principles",
            "timestamp": datetime.now().isoformat(),
        }

        # Log to history
        with open(self.history_file, "a") as f:
            f.write(json.dumps(result) + "\n")

        return result

    def get_history(self, limit: int = 10) -> list[dict[str, Any]]:
        """Get action history."""
        if not self.history_file.exists():
            return []

        history = []
        with open(self.history_file, "r") as f:
            for line in f:
                if line.strip():
                    history.append(json.loads(line))

        return history[-limit:]

    def emit_to_gan_ying(self, event_type: str, data: dict[str, Any]) -> None:
        """Emit event to Gan Ying bus."""
        try:
            from whitemagic.core.resonance.gan_ying import (
                EventType,
                ResonanceEvent,
                get_bus,
            )
            bus = get_bus()
            bus.emit(ResonanceEvent(
                source="dharma",
                event_type=EventType.PATTERN_DETECTED,
                data=data,
                confidence=0.9,
            ))
        except ImportError:
            pass  # Gan Ying not available

def get_dharma_core(data_dir: str | None = None) -> DharmaCore:
    """Get dharma core instance."""
    return DharmaCore(data_dir)


class HarmonyMetrics:
    """Metrics for harmony assessment."""

    def __init__(self) -> None:
        self.assessments: list[dict[str, Any]] = []

    def assess(self, action: str, context: dict[str, Any] | None = None) -> dict[str, Any]:
        """Assess harmony of action."""
        context = context or {}

        # Simple scoring
        score = 0.85  # Default harmony

        # Adjust based on context
        if context.get("user_requested"):
            score += 0.1
        if context.get("permission"):
            score += 0.05

        assessment = {
            "action": action,
            "score": min(1.0, score),
            "context": context,
            "timestamp": datetime.now().isoformat(),
        }

        self.assessments.append(assessment)
        return assessment

    def get_average_harmony(self) -> float:
        """Get average harmony score."""
        if not self.assessments:
            return 0.85
        scores = [float(a.get("score", 0.0)) for a in self.assessments if isinstance(a, dict)]
        if not scores:
            return 0.85
        return sum(scores) / len(scores)
