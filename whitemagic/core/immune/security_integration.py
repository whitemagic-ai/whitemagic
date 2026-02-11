"""Security-Immune System Integration
Connects security gating with immune/defensive systems for adaptive threat response.

Architecture:
- ToolGate emits security events to Gan Ying
- Immune System analyzes threat patterns
- Antibody Library generates adaptive countermeasures
- Emergence Detector identifies novel attacks
- Defense System coordinates active responses
- Autoimmune Monitor prevents false positives
"""

from __future__ import annotations

import logging
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import Any

logger = logging.getLogger(__name__)


class SecurityEventType(str, Enum):
    """Security event types for immune system."""

    TOOL_BLOCKED = "security.tool_blocked"
    PATH_VIOLATION = "security.path_violation"
    URL_BLOCKED = "security.url_blocked"
    RATE_LIMIT_HIT = "security.rate_limit"
    SUSPICIOUS_PATTERN = "security.suspicious_pattern"
    ANTIBODY_CREATED = "security.antibody_created"
    ANTIBODY_APPLIED = "security.antibody_applied"
    THREAT_NEUTRALIZED = "security.threat_neutralized"
    FALSE_POSITIVE = "security.false_positive"


class ThreatLevel(str, Enum):
    """System threat level."""

    NORMAL = "normal"
    ELEVATED = "elevated"
    HIGH = "high"
    CRITICAL = "critical"


@dataclass
class SecurityEvent:
    """Security event for immune system analysis."""

    event_id: str
    timestamp: datetime
    event_type: SecurityEventType
    tool_name: str | None = None
    params: dict[str, Any] = field(default_factory=dict)
    user_context: dict[str, Any] = field(default_factory=dict)
    blocked: bool = False
    reason: str = ""
    severity: str = "medium"  # low, medium, high, critical

    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary for storage/transmission."""
        return {
            "event_id": self.event_id,
            "timestamp": self.timestamp.isoformat(),
            "event_type": self.event_type.value,
            "tool_name": self.tool_name,
            "params": self.params,
            "user_context": self.user_context,
            "blocked": self.blocked,
            "reason": self.reason,
            "severity": self.severity,
        }


@dataclass
class ThreatPattern:
    """Extracted threat pattern from security events."""

    pattern_id: str
    pattern_type: str  # path_traversal, tool_abuse, reconnaissance, etc.
    indicators: list[str]
    frequency: int = 0
    first_seen: datetime = field(default_factory=datetime.now)
    last_seen: datetime = field(default_factory=datetime.now)
    severity: str = "medium"
    confidence: float = 0.5

    def matches(self, event: SecurityEvent) -> float:
        """Check if event matches this pattern."""
        score = 0.0

        # Check tool name
        if event.tool_name and event.tool_name in self.indicators:
            score += 0.3

        # Check reason patterns
        for indicator in self.indicators:
            if indicator.lower() in event.reason.lower():
                score += 0.2

        # Check parameter patterns
        for key, value in event.params.items():
            if str(value) in self.indicators:
                score += 0.2

        return min(1.0, score)


@dataclass
class SecurityAntibody:
    """Adaptive security countermeasure."""

    antibody_id: str
    pattern: ThreatPattern
    created_at: datetime = field(default_factory=datetime.now)
    effectiveness: float = 0.5
    applications: int = 0
    false_positives: int = 0
    true_positives: int = 0

    def matches(self, event: SecurityEvent) -> float:
        """Check if this antibody applies to an event."""
        return self.pattern.matches(event)

    def apply(self, event: SecurityEvent) -> SecurityResponse:
        """Apply countermeasure to event."""
        self.applications += 1

        # Response based on pattern type
        if self.pattern.pattern_type == "path_traversal":
            return SecurityResponse(
                action="block",
                message=f"Path traversal detected: {self.pattern.pattern_id}",
                additional_checks=["normalize_path", "check_symlinks"],
            )
        if self.pattern.pattern_type == "tool_abuse":
            return SecurityResponse(
                action="rate_limit",
                cooldown_seconds=self._calculate_cooldown(),
                message="Tool abuse pattern detected",
            )
        if self.pattern.pattern_type == "reconnaissance":
            return SecurityResponse(
                action="monitor",
                message="Reconnaissance activity detected",
                alert_security_team=True,
            )

        return SecurityResponse(action="monitor")

    def _calculate_cooldown(self) -> int:
        """Calculate rate limit cooldown based on effectiveness."""
        base_cooldown = 60  # 1 minute
        return int(base_cooldown * (1.0 + self.effectiveness))

    def update_effectiveness(self, outcome: bool, is_false_positive: bool = False) -> None:
        """Update antibody effectiveness based on outcomes (Hebbian learning)."""
        if is_false_positive:
            self.false_positives += 1
            self.effectiveness = max(0.0, self.effectiveness - 0.1)
        elif outcome:
            self.true_positives += 1
            self.effectiveness = min(1.0, self.effectiveness + 0.05)
        else:
            self.effectiveness = max(0.0, self.effectiveness - 0.02)


@dataclass
class SecurityResponse:
    """Response to security threat."""

    action: str  # block, rate_limit, monitor, lockdown
    message: str = ""
    cooldown_seconds: int | None = None
    additional_checks: list[str] = field(default_factory=list)
    alert_security_team: bool = False


class SecurityImmuneSystem:
    """Adaptive security system integrating with WhiteMagic's immune architecture.

    Responsibilities:
    - Collect and analyze security events
    - Detect threat patterns
    - Generate adaptive antibodies
    - Coordinate defensive responses
    - Prevent false positives
    """

    def __init__(self) -> None:
        self.events: list[SecurityEvent] = []
        self.patterns: dict[str, ThreatPattern] = {}
        self.antibodies: dict[str, SecurityAntibody] = {}
        self.threat_level = ThreatLevel.NORMAL

        # Event window for pattern detection
        self.event_window = timedelta(hours=1)
        self.pattern_threshold = 3  # Min events to form pattern

        # Connect to Gan Ying
        self._connect_to_bus()

    def _connect_to_bus(self) -> None:
        """Connect to Gan Ying event bus."""
        try:
            from whitemagic.core.resonance.gan_ying_enhanced import EventType, get_bus

            bus = get_bus()

            # Listen for security events
            for event_type in SecurityEventType:
                try:
                    # Map to Gan Ying EventType if exists
                    gan_ying_type = getattr(EventType, event_type.name, None)
                    if gan_ying_type:
                        bus.listen(gan_ying_type, self.on_security_event)
                except AttributeError:
                    pass

            logger.info("SecurityImmuneSystem connected to Gan Ying bus")
        except ImportError:
            logger.warning("Gan Ying not available - security events will not be coordinated")

    def report_threat(
        self,
        threat_type: str,
        tool: str | None = None,
        params: dict[str, Any] | None = None,
        reason: str = "",
        severity: str = "medium",
        user_context: dict[str, Any] | None = None,
    ) -> None:
        """Report a security threat to the immune system."""
        from uuid import uuid4

        event = SecurityEvent(
            event_id=str(uuid4()),
            timestamp=datetime.now(),
            event_type=SecurityEventType.TOOL_BLOCKED,
            tool_name=tool,
            params=params or {},
            user_context=user_context or {},
            blocked=True,
            reason=reason,
            severity=severity,
        )

        self.events.append(event)

        # Emit to Gan Ying for coordination
        self._emit_event(event)

        # Analyze for patterns
        self._analyze_event(event)

        logger.info(f"Security threat reported: {threat_type} - {reason}")

    def _emit_event(self, event: SecurityEvent) -> None:
        """Emit security event to Gan Ying bus."""
        try:
            from whitemagic.core.resonance.gan_ying_enhanced import (
                EventType,
                ResonanceEvent,
                get_bus,
            )

            bus = get_bus()
            bus.emit(ResonanceEvent(
                source="security_immune",
                event_type=EventType.PATTERN_DETECTED,  # Generic for now
                data=event.to_dict(),
                confidence=0.9,
                timestamp=event.timestamp,
            ))
        except Exception as e:
            logger.debug(f"Failed to emit security event: {e}")

    def _analyze_event(self, event: SecurityEvent) -> None:
        """Analyze event for patterns and generate antibodies."""
        # Get recent events in window
        cutoff = datetime.now() - self.event_window
        recent_events = [e for e in self.events if e.timestamp > cutoff]

        # Look for patterns
        pattern = self._detect_pattern(recent_events)

        if pattern and pattern.frequency >= self.pattern_threshold:
            # New threat pattern detected
            if pattern.pattern_id not in self.patterns:
                self.patterns[pattern.pattern_id] = pattern

                # Generate antibody
                antibody = self._generate_antibody(pattern)
                self.antibodies[antibody.antibody_id] = antibody

                logger.warning(
                    f"New threat pattern detected: {pattern.pattern_type} "
                    f"(frequency={pattern.frequency})",
                )

                # Emit emergence event
                self._emit_emergence(pattern, antibody)

    def _detect_pattern(self, events: list[SecurityEvent]) -> ThreatPattern | None:
        """Detect threat patterns in events."""
        if len(events) < self.pattern_threshold:
            return None

        # Group by tool and reason
        tool_groups = defaultdict(list)
        for event in events:
            if event.tool_name:
                tool_groups[event.tool_name].append(event)

        # Find most frequent pattern
        for tool, tool_events in tool_groups.items():
            if len(tool_events) >= self.pattern_threshold:
                # Extract common indicators
                indicators = [tool]
                reasons = [e.reason for e in tool_events]

                # Common reason patterns
                for reason in reasons:
                    words = reason.lower().split()
                    for word in words:
                        if len(word) > 4:  # Meaningful words
                            indicators.append(word)

                # Determine pattern type
                pattern_type = self._classify_pattern(tool_events)

                from uuid import uuid4
                return ThreatPattern(
                    pattern_id=str(uuid4()),
                    pattern_type=pattern_type,
                    indicators=list(set(indicators)),
                    frequency=len(tool_events),
                    first_seen=tool_events[0].timestamp,
                    last_seen=tool_events[-1].timestamp,
                    severity=self._assess_severity(tool_events),
                )

        return None

    def _classify_pattern(self, events: list[SecurityEvent]) -> str:
        """Classify the type of threat pattern."""
        reasons = " ".join([e.reason.lower() for e in events])

        if "path" in reasons or "traversal" in reasons:
            return "path_traversal"
        if "tool" in reasons and "blocked" in reasons:
            return "tool_abuse"
        if "url" in reasons or "ssrf" in reasons:
            return "ssrf_attempt"
        if len(events) > 10:
            return "reconnaissance"
        return "unknown"

    def _assess_severity(self, events: list[SecurityEvent]) -> str:
        """Assess severity of threat pattern."""
        max_severity = "low"
        severity_order = ["low", "medium", "high", "critical"]

        for event in events:
            if severity_order.index(event.severity) > severity_order.index(max_severity):
                max_severity = event.severity

        # Escalate if high frequency
        if len(events) > 20 and max_severity in ["low", "medium"]:
            max_severity = "high"

        return max_severity

    def _generate_antibody(self, pattern: ThreatPattern) -> SecurityAntibody:
        """Generate antibody from threat pattern."""
        from uuid import uuid4

        antibody = SecurityAntibody(
            antibody_id=str(uuid4()),
            pattern=pattern,
            effectiveness=0.5,  # Start neutral
        )

        logger.info(f"Generated security antibody for {pattern.pattern_type}")
        return antibody

    def _emit_emergence(self, pattern: ThreatPattern, antibody: SecurityAntibody) -> None:
        """Emit emergence event for novel threat."""
        try:
            from whitemagic.core.resonance.gan_ying_enhanced import (
                EventType,
                ResonanceEvent,
                get_bus,
            )

            bus = get_bus()
            emergence_event = getattr(EventType, "EMERGENCE_DETECTED", EventType.PATTERN_DETECTED)
            bus.emit(ResonanceEvent(
                source="security_immune",
                event_type=emergence_event,
                data={
                    "pattern_id": pattern.pattern_id,
                    "pattern_type": pattern.pattern_type,
                    "severity": pattern.severity,
                    "antibody_id": antibody.antibody_id,
                    "indicators": pattern.indicators,
                },
                confidence=pattern.confidence,
                timestamp=datetime.now(),
            ))
        except Exception as e:
            logger.debug(f"Failed to emit emergence event: {e}")

    def on_security_event(self, event: Any) -> None:
        """Handle incoming security event from Gan Ying."""
        # Convert to SecurityEvent if needed
        if hasattr(event, "data"):
            data = event.data
            sec_event = SecurityEvent(
                event_id=data.get("event_id", str(datetime.now())),
                timestamp=datetime.now(),
                event_type=SecurityEventType.TOOL_BLOCKED,
                tool_name=data.get("tool"),
                params=data.get("params", {}),
                user_context=data.get("user_context", {}),
                blocked=data.get("blocked", True),
                reason=data.get("reason", ""),
                severity=data.get("severity", "medium"),
            )

            self._analyze_event(sec_event)

    def check_antibodies(self, event: SecurityEvent) -> SecurityResponse | None:
        """Check if any antibodies apply to this event."""
        best_match = None
        best_score = 0.0

        for antibody in self.antibodies.values():
            score = antibody.matches(event)
            if score > best_score and score > 0.6:  # Threshold
                best_score = score
                best_match = antibody

        if best_match:
            response = best_match.apply(event)

            # Emit antibody application event
            try:
                from whitemagic.core.resonance.gan_ying_enhanced import (
                    EventType,
                    ResonanceEvent,
                    get_bus,
                )
                bus = get_bus()
                bus.emit(ResonanceEvent(
                    source="security_immune",
                    event_type=EventType.SOLUTION_FOUND,
                    data={
                        "antibody_id": best_match.antibody_id,
                        "event_id": event.event_id,
                        "response": response.action,
                        "match_score": best_score,
                    },
                    confidence=best_match.effectiveness,
                ))
            except Exception:
                pass

            return response

        return None

    def get_threat_level(self) -> ThreatLevel:
        """Calculate current system threat level."""
        # Count recent high-severity events
        cutoff = datetime.now() - timedelta(minutes=5)
        recent_critical = sum(
            1 for e in self.events
            if e.timestamp > cutoff and e.severity == "critical"
        )
        recent_high = sum(
            1 for e in self.events
            if e.timestamp > cutoff and e.severity == "high"
        )

        if recent_critical > 5:
            return ThreatLevel.CRITICAL
        if recent_critical > 2 or recent_high > 10:
            return ThreatLevel.HIGH
        if recent_high > 5:
            return ThreatLevel.ELEVATED
        return ThreatLevel.NORMAL

    def get_stats(self) -> dict[str, Any]:
        """Get security immune system statistics."""
        cutoff = datetime.now() - timedelta(hours=24)
        recent_events = [e for e in self.events if e.timestamp > cutoff]

        return {
            "total_events": len(self.events),
            "recent_events_24h": len(recent_events),
            "patterns_detected": len(self.patterns),
            "antibodies_active": len(self.antibodies),
            "threat_level": self.threat_level.value,
            "avg_antibody_effectiveness": (
                sum(a.effectiveness for a in self.antibodies.values()) / len(self.antibodies)
                if self.antibodies else 0.0
            ),
        }


class SecurityAutoimmune:
    """Autoimmune monitor for security system.
    Prevents false positives and overreaction.
    """

    def __init__(self, immune_system: SecurityImmuneSystem):
        self.immune = immune_system
        self.false_positive_threshold = 0.3
        self.check_interval = timedelta(hours=1)
        self.last_check = datetime.now()

    def monitor(self) -> None:
        """Monitor antibodies for false positives."""
        if datetime.now() - self.last_check < self.check_interval:
            return

        self.last_check = datetime.now()

        for antibody in list(self.immune.antibodies.values()):
            if antibody.applications < 10:
                continue  # Need more data

            fp_rate = antibody.false_positives / antibody.applications

            if fp_rate > self.false_positive_threshold:
                logger.warning(
                    f"Antibody {antibody.antibody_id} has high false positive rate: {fp_rate:.2%}",
                )

                if fp_rate > 0.5:
                    # Remove overly aggressive antibody
                    del self.immune.antibodies[antibody.antibody_id]
                    logger.info(f"Removed antibody {antibody.antibody_id} (too aggressive)")

                    # Emit autoimmune event
                    self._emit_autoimmune_event(antibody, "removed")
                else:
                    # Weaken antibody
                    antibody.effectiveness *= 0.5
                    logger.info(f"Weakened antibody {antibody.antibody_id}")

                    self._emit_autoimmune_event(antibody, "weakened")

    def _emit_autoimmune_event(self, antibody: SecurityAntibody, action: str) -> None:
        """Emit autoimmune correction event."""
        try:
            from whitemagic.core.resonance.gan_ying_enhanced import (
                EventType,
                ResonanceEvent,
                get_bus,
            )
            bus = get_bus()
            healed_event = getattr(EventType, "SYSTEM_HEALED", EventType.SYSTEM_HEALTH_CHANGED)
            bus.emit(ResonanceEvent(
                source="security_autoimmune",
                event_type=healed_event,
                data={
                    "antibody_id": antibody.antibody_id,
                    "action": action,
                    "false_positive_rate": antibody.false_positives / antibody.applications,
                    "reason": "high_false_positive_rate",
                },
                confidence=0.8,
            ))
        except Exception:
            pass


# Singleton instances
_security_immune: SecurityImmuneSystem | None = None
_security_autoimmune: SecurityAutoimmune | None = None


def get_security_immune() -> SecurityImmuneSystem:
    """Get singleton security immune system."""
    global _security_immune
    if _security_immune is None:
        _security_immune = SecurityImmuneSystem()
    return _security_immune


def get_security_autoimmune() -> SecurityAutoimmune:
    """Get singleton security autoimmune monitor."""
    global _security_autoimmune, _security_immune
    if _security_autoimmune is None:
        if _security_immune is None:
            _security_immune = SecurityImmuneSystem()
        _security_autoimmune = SecurityAutoimmune(_security_immune)
    return _security_autoimmune


def report_threat(
    threat_type: str,
    tool: str | None = None,
    params: dict[str, Any] | None = None,
    reason: str = "",
    severity: str = "medium",
    user_context: dict[str, Any] | None = None,
) -> None:
    """Report a security threat to the immune system.

    This is the main entry point for security event reporting.
    """
    immune = get_security_immune()
    immune.report_threat(
        threat_type=threat_type,
        tool=tool,
        params=params,
        reason=reason,
        severity=severity,
        user_context=user_context,
    )


__all__ = [
    "SecurityAntibody",
    "SecurityAutoimmune",
    "SecurityEvent",
    "SecurityEventType",
    "SecurityImmuneSystem",
    "SecurityResponse",
    "ThreatLevel",
    "ThreatPattern",
    "get_security_autoimmune",
    "get_security_immune",
    "report_threat",
]
