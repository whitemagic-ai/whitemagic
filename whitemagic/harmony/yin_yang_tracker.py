"""Yin-Yang Balance Tracker - Prevent burnout, optimize productivity

Philosophy:
-----------
陰陽調和 (Yīn Yáng Tiáohé) - Yin Yang Harmony

Balance between action (Yang) and reflection (Yin) is essential for
sustainable productivity and creative insight.

Too much Yang → Burnout, mistakes, diminishing returns
Too much Yin → Analysis paralysis, stagnation

This tracker monitors activity and suggests transitions to maintain harmony.
"""

from __future__ import annotations

import json
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from pathlib import Path
from typing import Any

from whitemagic.config.paths import WM_ROOT
from whitemagic.utils.core import parse_datetime


class ActivityType(Enum):
    """Types of activities mapped to Yin or Yang"""

    # Yang activities (action, output, doing)
    CREATE = "yang"
    WRITE = "yang"
    IMPLEMENT = "yang"
    EXECUTE = "yang"
    DEPLOY = "yang"
    CODE = "yang"
    BUILD = "yang"
    UPDATE = "yang"

    # Yin activities (reflection, input, being)
    READ = "yin"
    SEARCH = "yin"
    ANALYZE = "yin"
    REFLECT = "yin"
    CONSOLIDATE = "yin"
    REVIEW = "yin"
    LEARN = "yin"
    LIST = "yin"


@dataclass
class BalanceMetrics:
    """Current Yin-Yang balance state"""

    yang_count: int = 0
    yin_count: int = 0
    yang_ratio: float = 0.5
    yin_ratio: float = 0.5
    balance_score: float = 1.0  # 1.0 = perfect balance, 0.0 = severe imbalance
    burnout_risk: float = 0.0   # 0.0-1.0, higher = more risk
    recommendation: str | None = None
    window_start: datetime = field(default_factory=datetime.now)
    last_activity: ActivityType | None = None
    status_emoji: str = "⚖️"


class YinYangBalanceTracker:
    """Track and maintain Yin-Yang balance in workflows"""

    def __init__(self, window_minutes: int = 90, storage_dir: Path | None = None):
        """Args:
        window_minutes: Rolling window for balance calculation (default 90)
        storage_dir: Where to store balance history

        """
        self.window_minutes = window_minutes
        self.storage_dir = storage_dir or (WM_ROOT / "harmony")
        self.storage_dir.mkdir(parents=True, exist_ok=True)

        self.activity_log: list[tuple[datetime, ActivityType]] = []
        self.bus: Any | None = None

        self._load_history()
        self._connect_to_gan_ying()

    def _connect_to_gan_ying(self) -> None:
        """Connect to Gan Ying event bus"""
        try:
            from whitemagic.core.resonance.gan_ying_enhanced import get_bus
            self.bus = get_bus()
        except ImportError:
            pass

    def _load_history(self) -> None:
        """Load recent activity history"""
        history_file = self.storage_dir / "activity_log.jsonl"
        if history_file.exists():
            cutoff = datetime.now() - timedelta(minutes=self.window_minutes)
            with open(history_file) as f:
                for line in f:
                    if line.strip():
                        try:
                            data = json.loads(line)
                            timestamp = parse_datetime(data["timestamp"])
                            if timestamp >= cutoff:
                                activity = ActivityType[data["activity"]]
                                self.activity_log.append((timestamp, activity))
                        except (json.JSONDecodeError, KeyError, ValueError):
                            continue

    def record_activity(self, activity: str) -> BalanceMetrics:
        """Record an activity and check balance

        Args:
            activity: Activity name (e.g., 'create', 'read', 'analyze')

        Returns:
            BalanceMetrics with current state

        """
        # Map activity to Yin or Yang
        try:
            activity_type = ActivityType[activity.upper()]
        except KeyError:
            # Unknown activity - try to infer
            if activity.lower() in ["create", "write", "execute", "build", "deploy", "code", "update"]:
                activity_type = ActivityType.CREATE
            else:
                activity_type = ActivityType.READ

        # Record
        timestamp = datetime.now()
        self.activity_log.append((timestamp, activity_type))

        # Persist
        self._save_activity(timestamp, activity_type)

        # Check balance
        metrics = self.calculate_balance()

        # Emit event if imbalanced
        if metrics.balance_score < 0.6 and self.bus:
            self._emit_balance_event(metrics)

        return metrics

    def calculate_balance(self) -> BalanceMetrics:
        """Calculate current Yin-Yang balance"""
        # Filter to window
        cutoff = datetime.now() - timedelta(minutes=self.window_minutes)
        recent = [(t, a) for t, a in self.activity_log if t >= cutoff]

        if not recent:
            return BalanceMetrics()

        # Count Yin vs Yang
        yang_count = sum(1 for _, a in recent if a.value == "yang")
        yin_count = sum(1 for _, a in recent if a.value == "yin")
        total = yang_count + yin_count

        yang_ratio = yang_count / total if total > 0 else 0.5
        yin_ratio = yin_count / total if total > 0 else 0.5

        # Calculate balance score (1.0 = perfect, 0.0 = severe imbalance)
        # Optimal range: 40-60% either way
        deviation = abs(yang_ratio - 0.5)
        if deviation < 0.1:
            balance_score = 1.0
        elif deviation < 0.2:
            balance_score = 0.8
        elif deviation < 0.3:
            balance_score = 0.6
        else:
            balance_score = 0.4

        # Calculate burnout risk (high Yang ratio = higher risk)
        burnout_risk = max(0.0, (yang_ratio - 0.5) * 2.0)

        # Generate recommendation
        recommendation = None
        status_emoji = "⚖️"

        if yang_ratio > 0.7:
            recommendation = "⚠️ Too much Yang (action). Take a break: consolidate, reflect, or analyze."
            status_emoji = "🔴"
        elif yang_ratio > 0.6:
            recommendation = "⚡ Leaning Yang. Balance with some Yin activities soon."
            status_emoji = "⚠️"
        elif yin_ratio > 0.7:
            recommendation = "💤 Too much Yin (reflection). Time to act: create, implement, or execute."
            status_emoji = "🔵"
        elif yin_ratio > 0.6:
            recommendation = "🌊 Leaning Yin. Balance with some Yang activities soon."
            status_emoji = "⚠️"
        else:
            recommendation = "✅ Excellent balance - continue current rhythm"
            status_emoji = "⚖️"

        return BalanceMetrics(
            yang_count=yang_count,
            yin_count=yin_count,
            yang_ratio=yang_ratio,
            yin_ratio=yin_ratio,
            balance_score=balance_score,
            burnout_risk=burnout_risk,
            recommendation=recommendation,
            window_start=cutoff,
            last_activity=recent[-1][1] if recent else None,
            status_emoji=status_emoji,
        )

    def _emit_balance_event(self, metrics: BalanceMetrics) -> None:
        """Emit event when balance is off"""
        if not self.bus:
            return

        try:
            from whitemagic.core.resonance.gan_ying_enhanced import (
                EventType,
                ResonanceEvent,
            )

            event_type = getattr(EventType, "BALANCE_CHECK", EventType.INTERNAL_STATE_CHANGED)

            self.bus.emit(ResonanceEvent(
                source="yin_yang_tracker",
                event_type=event_type,
                data={
                    "yang_ratio": metrics.yang_ratio,
                    "yin_ratio": metrics.yin_ratio,
                    "balance_score": metrics.balance_score,
                    "burnout_risk": metrics.burnout_risk,
                    "recommendation": metrics.recommendation,
                },
                confidence=1.0 - metrics.balance_score,
                timestamp=datetime.now(),
            ))
        except Exception:
            # Fail silently if Gan Ying not available
            pass

    def _save_activity(self, timestamp: datetime, activity: ActivityType) -> None:
        """Persist activity to history"""
        history_file = self.storage_dir / "activity_log.jsonl"
        with open(history_file, "a") as f:
            f.write(json.dumps({
                "timestamp": timestamp.isoformat(),
                "activity": activity.name,
            }) + "\n")

    def get_report(self) -> dict[str, Any]:
        """Get comprehensive balance report"""
        metrics = self.calculate_balance()

        return {
            "balance_score": metrics.balance_score,
            "burnout_risk": metrics.burnout_risk,
            "yang_count": metrics.yang_count,
            "yin_count": metrics.yin_count,
            "yang_ratio": metrics.yang_ratio,
            "yin_ratio": metrics.yin_ratio,
            "recommendation": metrics.recommendation,
            "window_minutes": self.window_minutes,
            "activity_count": len([t for t, _ in self.activity_log
                                   if t >= metrics.window_start]),
            "status": self._get_status(metrics.balance_score),
            "status_emoji": metrics.status_emoji,
        }

    def _get_status(self, score: float) -> str:
        """Get human-readable status"""
        if score >= 0.9:
            return "Perfect Balance"
        if score >= 0.7:
            return "Good Balance"
        if score >= 0.5:
            return "Slight Imbalance"
        return "Significant Imbalance"


# Singleton instance
_tracker: YinYangBalanceTracker | None = None


def get_tracker() -> YinYangBalanceTracker:
    """Get global Yin-Yang tracker instance"""
    global _tracker
    if _tracker is None:
        _tracker = YinYangBalanceTracker()
    return _tracker
