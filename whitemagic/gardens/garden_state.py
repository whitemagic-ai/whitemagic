"""Garden State Tracking System.

Tracks which gardens are active, their activation counts, and last used times.
Persists state to $WM_STATE_ROOT/garden_state.json (defaults to ~/.whitemagic/garden_state.json)
"""

from __future__ import annotations

import json
import logging
from dataclasses import asdict, dataclass
from datetime import datetime
from pathlib import Path
from typing import Any

from whitemagic.config.paths import WM_ROOT
from whitemagic.utils.core import parse_datetime

logger = logging.getLogger(__name__)


@dataclass
class GardenState:
    """State information for a single garden."""

    name: str
    is_active: bool
    activation_count: int
    last_activated: str | None
    last_deactivated: str | None
    total_time_active_seconds: float = 0.0


class GardenStateTracker:
    """Track garden activation state."""

    def __init__(self, state_file: Path | None = None) -> None:
        if state_file is None:
            state_file = WM_ROOT / "garden_state.json"

        self.state_file = state_file
        self.state_file.parent.mkdir(parents=True, exist_ok=True)

        self.states: dict[str, GardenState] = {}
        self._load_state()

    def _load_state(self) -> None:
        """Load state from disk."""
        if self.state_file.exists():
            try:
                with open(self.state_file, "r") as f:
                    data = json.load(f)
                    for name, state_data in data.items():
                        self.states[name] = GardenState(**state_data)
            except Exception as e:
                logger.info(f"Warning: Could not load garden state: {e}")

    def _save_state(self) -> None:
        """Save state to disk."""
        try:
            data = {name: asdict(state) for name, state in self.states.items()}
            with open(self.state_file, "w") as f:
                json.dump(data, f, indent=2)
        except Exception as e:
            logger.info(f"Warning: Could not save garden state: {e}")

    def is_active(self, garden_name: str) -> bool:
        """Check if a garden is currently active."""
        return self.states.get(garden_name, GardenState(
            name=garden_name,
            is_active=False,
            activation_count=0,
            last_activated=None,
            last_deactivated=None,
        )).is_active

    def activate(self, garden_name: str) -> None:
        """Mark a garden as active."""
        if garden_name not in self.states:
            self.states[garden_name] = GardenState(
                name=garden_name,
                is_active=False,
                activation_count=0,
                last_activated=None,
                last_deactivated=None,
            )

        state = self.states[garden_name]
        state.is_active = True
        state.activation_count += 1
        state.last_activated = datetime.now().isoformat()

        self._save_state()

    def deactivate(self, garden_name: str) -> None:
        """Mark a garden as inactive."""
        if garden_name in self.states:
            state = self.states[garden_name]
            state.is_active = False
            state.last_deactivated = datetime.now().isoformat()

            # Calculate time active
            if state.last_activated:
                try:
                    last_active = parse_datetime(state.last_activated)
                    duration = (datetime.now() - last_active).total_seconds()
                    state.total_time_active_seconds += duration
                except Exception:
                    pass

            self._save_state()

    def get_state(self, garden_name: str) -> GardenState:
        """Get the state for a specific garden."""
        if garden_name not in self.states:
            return GardenState(
                name=garden_name,
                is_active=False,
                activation_count=0,
                last_activated=None,
                last_deactivated=None,
            )
        return self.states[garden_name]

    def get_all_active(self) -> list[str]:
        """Get list of all currently active gardens."""
        return [name for name, state in self.states.items() if state.is_active]

    def get_stats(self) -> dict[str, dict[str, Any]]:
        """Get statistics for all gardens."""
        return {
            name: {
                "is_active": state.is_active,
                "activation_count": state.activation_count,
                "last_activated": state.last_activated,
                "total_hours_active": state.total_time_active_seconds / 3600,
            }
            for name, state in self.states.items()
        }


# Global tracker instance
_tracker: GardenStateTracker | None = None


def get_garden_state_tracker() -> GardenStateTracker:
    """Get the global garden state tracker."""
    global _tracker
    if _tracker is None:
        _tracker = GardenStateTracker()
    return _tracker


def is_garden_active(garden_name: str) -> bool:
    """Check if a garden is currently active."""
    tracker = get_garden_state_tracker()
    return tracker.is_active(garden_name)
