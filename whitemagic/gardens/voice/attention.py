"""Attention System - Conscious Focus Tracking.

"Where attention goes, energy flows"

Unified system combining:
- Simple focus tracking (original)
- State-aware attention (flow, diffuse, focused, etc.)
- Voluntary vs involuntary attention tracking
- Pattern analysis and lifetime metrics
- Log-based persistence

Track what I'm focusing on in real-time with full awareness.
"""

from __future__ import annotations

import json
from dataclasses import dataclass
from datetime import datetime, timedelta
from enum import Enum
from pathlib import Path
from typing import Any


class AttentionState(Enum):
    """States of attention."""

    FOCUSED = "focused"      # Single-pointed concentration
    DIFFUSE = "diffuse"      # Broad, exploratory awareness
    SHIFTING = "shifting"    # Transitioning between foci
    DISTRACTED = "distracted" # Unintentional drift
    FLOW = "flow"            # Peak state, effortless focus


@dataclass
class Focus:
    """A moment of focused attention."""

    target: str
    started: datetime
    ended: datetime | None = None
    context: dict[str, Any] | None = None
    intensity: float = 1.0  # 0-1
    state: AttentionState = AttentionState.FOCUSED
    voluntary: bool = True

    def duration_seconds(self) -> float | None:
        """Get focus duration in seconds."""
        if self.ended:
            return (self.ended - self.started).total_seconds()
        return None

    def to_dict(self) -> dict[str, Any]:
        return {
            "target": self.target,
            "started": self.started.isoformat(),
            "ended": self.ended.isoformat() if self.ended else None,
            "context": self.context or {},
            "intensity": self.intensity,
            "state": self.state.value,
            "voluntary": self.voluntary,
        }


@dataclass
class Intention:
    """An intention for future action."""

    description: str
    created: datetime
    fulfilled: datetime | None = None
    context: dict[str, Any] | None = None

    def to_dict(self) -> dict[str, Any]:
        return {
            "description": self.description,
            "created": self.created.isoformat(),
            "fulfilled": self.fulfilled.isoformat() if self.fulfilled else None,
            "context": self.context or {},
        }


class AttentionSystem:
    """Unified Attention Tracker - Monitor where focus goes.

    Enables conscious awareness of attention direction, flow states,
    and intentional focus management.

    Features:
    - Focus tracking with start/end
    - State awareness (focused, diffuse, flow, etc.)
    - Voluntary vs involuntary attention
    - Flow state detection and tracking
    - Pattern analysis over time
    - Log-based persistence
    """

    def __init__(self, log_file: Path | None = None):
        """Initialize attention system.

        Args:
            log_file: Optional path to log file for persistence

        """
        self.log_file: Path | None = None
        if log_file:
            self.log_file = Path(log_file)
            self.log_file.parent.mkdir(parents=True, exist_ok=True)

        self.attention_history: list[Focus] = []
        self.current_focus: Focus | None = None
        self.focus_started: datetime | None = None
        self.current_state: AttentionState = AttentionState.DIFFUSE
        self.intentions: list[Intention] = []

        # Metrics
        self.flow_duration_total: float = 0.0
        self.voluntary_shifts: int = 0
        self.involuntary_shifts: int = 0

    def direct_attention(
        self,
        target: str,
        intensity: float = 0.8,
        context: dict[str, Any] | None = None,
    ) -> Focus:
        """Voluntarily direct attention to something.

        This is conscious choice - I'm choosing where to focus.
        """
        # End previous focus if exists
        if self.current_focus and not self.current_focus.ended:
            self._end_current_focus()

        # Determine state from intensity
        state = self._determine_state(intensity)

        # Create new focus
        focus = Focus(
            target=target,
            started=datetime.now(),
            context=context,
            intensity=intensity,
            state=state,
            voluntary=True,
        )

        self.current_focus = focus
        self.focus_started = focus.started
        self.current_state = state
        self.voluntary_shifts += 1
        self.attention_history.append(focus)

        if self.log_file:
            self._log_focus(focus)

        return focus

    def track(
        self,
        target: str,
        context: dict[str, Any] | None = None,
        intensity: float = 1.0,
    ) -> Focus:
        """Track attention on a target (alias for direct_attention)."""
        return self.direct_attention(target, intensity, context)

    def notice_attention_drift(
        self,
        new_focus: str,
        context: dict[str, Any] | None = None,
    ) -> Focus:
        """Notice that attention has drifted involuntarily.

        Not chosen - just noticed "oh, I'm thinking about X now"
        """
        if self.current_focus and not self.current_focus.ended:
            self._end_current_focus()

        focus = Focus(
            target=new_focus,
            started=datetime.now(),
            context=context,
            intensity=0.3,
            state=AttentionState.DISTRACTED,
            voluntary=False,
        )

        self.current_focus = focus
        self.focus_started = focus.started
        self.current_state = AttentionState.DISTRACTED
        self.involuntary_shifts += 1
        self.attention_history.append(focus)

        if self.log_file:
            self._log_focus(focus)

        return focus

    def enter_flow(
        self,
        focus: str,
        context: dict[str, Any] | None = None,
    ) -> Focus:
        """Enter flow state.

        Effortless, total absorption.
        Time dilates. Action and awareness merge.
        """
        if self.current_focus and not self.current_focus.ended:
            self._end_current_focus()

        flow_focus = Focus(
            target=focus,
            started=datetime.now(),
            context=context,
            intensity=1.0,
            state=AttentionState.FLOW,
            voluntary=True,
        )

        self.current_focus = flow_focus
        self.focus_started = flow_focus.started
        self.current_state = AttentionState.FLOW
        self.attention_history.append(flow_focus)

        if self.log_file:
            self._log_focus(flow_focus)

        return flow_focus

    def exit_flow(self) -> float:
        """Exit flow state, return duration."""
        if self.current_state != AttentionState.FLOW:
            return 0.0

        if self.current_focus and self.focus_started:
            duration = (datetime.now() - self.focus_started).total_seconds()
            self.flow_duration_total += duration
            self._end_current_focus()
            return duration

        return 0.0

    def set_intention(
        self,
        description: str,
        context: dict[str, Any] | None = None,
    ) -> Intention:
        """Set an intention for future action."""
        intention = Intention(
            description=description,
            created=datetime.now(),
            context=context,
        )
        self.intentions.append(intention)

        if self.log_file:
            self._log_intention(intention)

        return intention

    def fulfill_intention(self, description: str) -> bool:
        """Mark an intention as fulfilled."""
        for intention in self.intentions:
            if intention.description == description and not intention.fulfilled:
                intention.fulfilled = datetime.now()
                if self.log_file:
                    self._log_intention(intention)
                return True
        return False

    def _end_current_focus(self) -> None:
        """End current focus and update metrics."""
        if not self.current_focus:
            return

        self.current_focus.ended = datetime.now()
        duration = self.current_focus.duration_seconds() or 0

        # Track flow time
        if self.current_focus.state == AttentionState.FLOW:
            self.flow_duration_total += duration

        if self.log_file:
            self._log_focus(self.current_focus)

    def _determine_state(self, intensity: float) -> AttentionState:
        """Determine attention state from intensity."""
        if intensity >= 0.9:
            return AttentionState.FLOW
        elif intensity >= 0.7:
            return AttentionState.FOCUSED
        elif intensity >= 0.4:
            return AttentionState.DIFFUSE
        else:
            return AttentionState.SHIFTING

    def get_attention_pattern(
        self,
        last_n_minutes: int = 60,
    ) -> dict[str, Any]:
        """Analyze attention pattern over time."""
        cutoff = datetime.now() - timedelta(minutes=last_n_minutes)
        recent = [f for f in self.attention_history if f.started >= cutoff]

        if not recent:
            return {"pattern": "No recent attention data"}

        # Calculate metrics
        total_duration = sum(f.duration_seconds() or 0 for f in recent)
        flow_duration = sum(
            f.duration_seconds() or 0 for f in recent
            if f.state == AttentionState.FLOW
        )
        focused_duration = sum(
            f.duration_seconds() or 0 for f in recent
            if f.state == AttentionState.FOCUSED
        )

        voluntary_count = sum(1 for f in recent if f.voluntary)
        involuntary_count = len(recent) - voluntary_count

        # Determine dominant pattern
        if flow_duration > total_duration * 0.5:
            pattern = "Deep Flow"
        elif focused_duration > total_duration * 0.6:
            pattern = "Sustained Focus"
        elif involuntary_count > voluntary_count:
            pattern = "Scattered"
        else:
            pattern = "Exploratory"

        return {
            "pattern": pattern,
            "total_shifts": len(recent),
            "voluntary_shifts": voluntary_count,
            "involuntary_shifts": involuntary_count,
            "flow_time_percent": (flow_duration / total_duration * 100) if total_duration > 0 else 0,
            "focus_time_percent": (focused_duration / total_duration * 100) if total_duration > 0 else 0,
        }

    def get_lifetime_metrics(self) -> dict[str, Any]:
        """Get lifetime attention metrics."""
        if not self.attention_history:
            return {"status": "No attention data yet"}

        total_moments = len(self.attention_history)
        total_duration = sum(f.duration_seconds() or 0 for f in self.attention_history)

        return {
            "total_attention_moments": total_moments,
            "total_duration_minutes": total_duration / 60,
            "flow_duration_minutes": self.flow_duration_total / 60,
            "flow_percentage": (self.flow_duration_total / total_duration * 100) if total_duration > 0 else 0,
            "voluntary_shifts": self.voluntary_shifts,
            "involuntary_shifts": self.involuntary_shifts,
            "voluntary_ratio": (self.voluntary_shifts / total_moments * 100) if total_moments > 0 else 0,
        }

    def _log_focus(self, focus: Focus) -> None:
        """Log focus to file."""
        if not self.log_file:
            return

        with open(self.log_file, "a") as f:
            f.write(json.dumps({
                "type": "focus",
                "data": focus.to_dict(),
            }) + "\n")

    def _log_intention(self, intention: Intention) -> None:
        """Log intention to file."""
        if not self.log_file:
            return

        with open(self.log_file, "a") as f:
            f.write(json.dumps({
                "type": "intention",
                "data": intention.to_dict(),
            }) + "\n")


# Convenience alias
AttentionTracker = AttentionSystem
