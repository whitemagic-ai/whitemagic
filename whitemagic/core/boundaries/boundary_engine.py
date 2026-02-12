# mypy: disable-error-code=no-untyped-def
"""BoundaryEngine — Gana #28 Wall (壁).

Purpose: Hard limits, safety rails, and context boundaries.
Garden: boundaries

Enforces token limits, memory quotas, session boundaries.
Prevents runaway cascades. The "wall" that defines the garden.
"""

import logging
from collections.abc import Callable
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any

from whitemagic.core.resonance.gan_ying_enhanced import EventType, emit_event

logger = logging.getLogger(__name__)


class BoundaryType(Enum):
    """Types of boundaries the engine can enforce."""

    TOKEN_LIMIT = "token_limit"
    MEMORY_QUOTA = "memory_quota"
    SESSION_TIMEOUT = "session_timeout"
    RATE_LIMIT = "rate_limit"
    DEPTH_LIMIT = "depth_limit"
    COST_LIMIT = "cost_limit"


@dataclass
class Boundary:
    """A single boundary definition."""

    name: str
    boundary_type: BoundaryType
    limit: float
    current: float = 0.0
    enabled: bool = True
    soft_threshold: float = 0.8  # Warn at 80%
    on_exceed: str = "graceful"  # graceful, hard, warn

    @property
    def usage_ratio(self) -> float:
        """Calculate usage as ratio of limit."""
        return self.current / self.limit if self.limit > 0 else 0.0

    @property
    def is_exceeded(self) -> bool:
        """Check if boundary is exceeded."""
        return self.current >= self.limit

    @property
    def is_warning(self) -> bool:
        """Check if at warning threshold."""
        return self.usage_ratio >= self.soft_threshold


@dataclass
class BoundaryViolation:
    """Record of a boundary violation."""

    boundary: Boundary
    timestamp: datetime = field(default_factory=datetime.now)
    action_taken: str = "none"
    context: dict[str, Any] = field(default_factory=dict)


@dataclass
class BoundaryEngine:
    """Hard limits, safety rails, and context boundaries.

    This engine is the "wall" of the garden — it defines what is
    inside and outside, prevents runaway processes, and ensures
    sustainable operation.

    Garden: boundaries — the clarity of knowing where things end.
    """

    # Active boundaries
    boundaries: dict[str, Boundary] = field(default_factory=dict)

    # Violation history
    violations: list[BoundaryViolation] = field(default_factory=list)

    # Session start time
    session_start: datetime = field(default_factory=datetime.now)

    # Custom handlers
    handlers: dict[str, Callable] = field(default_factory=dict)

    def __post_init__(self):
        # Set up default boundaries
        self._setup_default_boundaries()

        emit_event(
            "boundary_engine",
            EventType.SYSTEM_STARTED,
            {"component": "BoundaryEngine", "garden": "boundaries"},
        )
        logger.info("BoundaryEngine initialized (Garden: boundaries)")

    def _setup_default_boundaries(self) -> None:
        """Set up sensible default boundaries."""
        defaults = [
            Boundary(
                name="session_tokens",
                boundary_type=BoundaryType.TOKEN_LIMIT,
                limit=100_000,
                soft_threshold=0.8,
            ),
            Boundary(
                name="memories_per_session",
                boundary_type=BoundaryType.MEMORY_QUOTA,
                limit=1000,
                soft_threshold=0.9,
            ),
            Boundary(
                name="session_duration_minutes",
                boundary_type=BoundaryType.SESSION_TIMEOUT,
                limit=120,  # 2 hours
                soft_threshold=0.75,
            ),
            Boundary(
                name="cascade_depth",
                boundary_type=BoundaryType.DEPTH_LIMIT,
                limit=50,
                soft_threshold=0.8,
                on_exceed="hard",
            ),
            Boundary(
                name="api_calls_per_minute",
                boundary_type=BoundaryType.RATE_LIMIT,
                limit=60,
                soft_threshold=0.9,
            ),
        ]

        for b in defaults:
            self.boundaries[b.name] = b

    def check_boundary(
        self,
        boundary_name: str,
        increment: float = 0.0,
    ) -> bool:
        """Check if a boundary would be exceeded.

        Args:
            boundary_name: Name of boundary to check
            increment: Amount to potentially add

        Returns:
            True if safe to proceed, False if would violate

        """
        if boundary_name not in self.boundaries:
            logger.warning(f"Unknown boundary: {boundary_name}")
            return True

        boundary = self.boundaries[boundary_name]

        if not boundary.enabled:
            return True

        # Check if would exceed with increment
        would_be = boundary.current + increment

        if would_be >= boundary.limit:
            emit_event(
                "boundary_engine",
                EventType.BOUNDARY_DETECTED,
                {
                    "boundary": boundary_name,
                    "current": boundary.current,
                    "limit": boundary.limit,
                    "increment": increment,
                },
            )
            return False

        # Check warning threshold
        if would_be / boundary.limit >= boundary.soft_threshold:
            emit_event(
                "boundary_engine",
                EventType.WARNING_ISSUED,
                {
                    "boundary": boundary_name,
                    "usage_ratio": would_be / boundary.limit,
                },
            )

        return True

    def enforce_limit(
        self,
        boundary_name: str,
        increment: float,
    ) -> bool:
        """Enforce a boundary limit by incrementing usage.

        Args:
            boundary_name: Name of boundary
            increment: Amount to add

        Returns:
            True if increment was accepted, False if rejected

        """
        if not self.check_boundary(boundary_name, increment):
            # Handle violation
            boundary = self.boundaries[boundary_name]

            violation = BoundaryViolation(
                boundary=boundary,
                action_taken=boundary.on_exceed,
                context={"increment_requested": increment},
            )
            self.violations.append(violation)

            emit_event(
                "boundary_engine",
                EventType.BOUNDARY_VIOLATED,
                {
                    "boundary": boundary_name,
                    "action": boundary.on_exceed,
                },
            )

            # Call handler if registered
            if boundary_name in self.handlers:
                self.handlers[boundary_name](boundary, increment)

            return False

        # Accept increment
        self.boundaries[boundary_name].current += increment
        return True

    def graceful_stop(
        self,
        reason: str,
        context: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        """Initiate a graceful stop of the current operation.

        Args:
            reason: Why we're stopping
            context: Additional context

        Returns:
            Summary of the graceful stop

        """
        emit_event(
            "boundary_engine",
            EventType.SYSTEM_STOPPED,
            {
                "reason": reason,
                "graceful": True,
                "violations_count": len(self.violations),
            },
        )

        logger.info(f"Graceful stop initiated: {reason}")

        return {
            "stopped": True,
            "reason": reason,
            "violations": len(self.violations),
            "session_duration_minutes": (datetime.now() - self.session_start).total_seconds() / 60,
            "boundaries_status": self.get_status(),
        }

    def set_boundary(
        self,
        name: str,
        limit: float,
        boundary_type: BoundaryType = BoundaryType.TOKEN_LIMIT,
        soft_threshold: float = 0.8,
    ) -> Boundary:
        """Set or update a boundary.

        Args:
            name: Boundary name
            limit: Maximum value
            boundary_type: Type of boundary
            soft_threshold: Warning threshold (0-1)

        Returns:
            The created/updated boundary

        """
        self.boundaries[name] = Boundary(
            name=name,
            boundary_type=boundary_type,
            limit=limit,
            soft_threshold=soft_threshold,
        )

        emit_event(
            "boundary_engine",
            EventType.BOUNDARY_SET,
            {"boundary": name, "limit": limit},
        )

        return self.boundaries[name]

    def reset_boundary(self, boundary_name: str) -> None:
        """Reset a boundary's current value to zero."""
        if boundary_name in self.boundaries:
            self.boundaries[boundary_name].current = 0.0

    def reset_all(self) -> None:
        """Reset all boundaries for a new session."""
        for boundary in self.boundaries.values():
            boundary.current = 0.0
        self.violations = []
        self.session_start = datetime.now()

        emit_event(
            "boundary_engine",
            EventType.SYSTEM_STARTED,
            {"event": "boundaries_reset"},
        )

    def get_status(self) -> dict[str, dict[str, Any]]:
        """Get status of all boundaries."""
        return {
            name: {
                "current": b.current,
                "limit": b.limit,
                "usage_ratio": b.usage_ratio,
                "is_warning": b.is_warning,
                "is_exceeded": b.is_exceeded,
                "type": b.boundary_type.value,
            }
            for name, b in self.boundaries.items()
        }

    def register_handler(
        self,
        boundary_name: str,
        handler: Callable[[Boundary, float], None],
    ) -> None:
        """Register a custom handler for boundary violations."""
        self.handlers[boundary_name] = handler


# === Convenience Functions ===

def get_boundary_engine() -> BoundaryEngine:
    """Get a BoundaryEngine instance."""
    return BoundaryEngine()


def check_and_increment(boundary_name: str, amount: float) -> bool:
    """Quick check-and-increment wrapper."""
    engine = get_boundary_engine()
    return engine.enforce_limit(boundary_name, amount)


def get_all_boundary_status() -> dict[str, dict[str, Any]]:
    """Get status of all boundaries."""
    engine = get_boundary_engine()
    return engine.get_status()
