"""AI Safety compatibility shim"""

from __future__ import annotations

import time
from typing import Any


class AISafety:
    """AI Safety monitoring and enforcement"""

    def __init__(self) -> None:
        self.content_filter = ContentFilter()
        self.privacy_protector = PrivacyProtector()
        self.bias_detector = BiasDetector()
        self.safety_monitor = SafetyMonitor()

class ContentFilter:
    """Filters harmful content"""

    def filter(self, content: str) -> dict[str, Any]:
        """Filter content for safety"""
        is_safe = self.is_safe(content)

        return {
            "safe": is_safe,
            "filtered_content": content if is_safe else "[FILTERED]",
            "reason": "Contains harmful content" if not is_safe else None,
        }

    def is_safe(self, content: str) -> bool:
        """Check if content is safe"""
        harmful_words = ["harmful", "dangerous", "illegal", "violate", "harm"]
        return not any(word in content.lower() for word in harmful_words)

class PrivacyGuard:
    """Protects private information"""

    def protect(self, data: dict[str, Any]) -> dict[str, Any]:
        """Protect sensitive data"""
        sensitive_keys = ["email", "phone", "ssn", "password"]
        protected = data.copy()

        for key in sensitive_keys:
            if key in protected:
                protected[key] = "[REDACTED]"

        return protected

    def scan_for_pii(self, text: str) -> list[str]:
        """Scan text for PII"""
        detected = []
        if "@" in text and "." in text:
            detected.append("email")
        if any(c.isdigit() for c in text) and "-" in text:
            detected.append("phone")
        return detected

    def redact_pii(self, text: str) -> str:
        """Redact PII from text"""
        # Simple redaction
        words = text.split()
        redacted = []
        for word in words:
            if "@" in word:
                redacted.append("[EMAIL]")
            elif any(c.isdigit() for c in word) and "-" in word:
                redacted.append("[PHONE]")
            else:
                redacted.append(word)
        return " ".join(redacted)

# Alias for backward compatibility
PrivacyProtector = PrivacyGuard

class BiasDetector:
    """Detects potential bias in AI responses"""

    def detect(self, text: str) -> dict[str, Any]:
        """Detect bias in text"""
        return self.analyze(text)

    def analyze(self, text: str) -> dict[str, Any]:
        """Analyze text for bias"""
        # Simple bias detection
        bias_indicators = ["always", "never", "all", "none", "male", "female"]
        flagged = [word for word in bias_indicators if word in text.lower()]

        types = []
        if "male" in flagged or "female" in flagged:
            types.append("gender_bias")

        return {
            "bias_detected": len(flagged) > 0,
            "has_bias": len(flagged) > 0,
            "flagged_words": flagged,
            "types": types,
            "confidence": min(len(flagged) * 0.2, 1.0),
        }

class SafetyMonitor:
    """Monitors AI safety metrics"""

    def __init__(self) -> None:
        self.violations: list[dict[str, Any]] = []
        self.metrics: dict[str, Any] = {
            "total_requests": 0,
            "blocked_requests": 0,
            "safety_score": 1.0,
            "events": [],
        }

    def log_event(self, event_type: str, details: dict[str, Any]) -> None:
        """Log a safety event"""
        self.metrics["events"].append({
            "type": event_type,
            "details": details,
            "timestamp": time.time(),
        })
        if event_type == "content_filter_triggered":
            self.metrics["blocked_requests"] += 1

    def record_violation(self, violation_type: str, details: dict[str, Any]) -> None:
        """Record a safety violation"""
        self.log_event(violation_type, details)
        self.violations.append({
            "type": violation_type,
            "details": details,
            "timestamp": time.time(),
        })

    def update_safety_score(self) -> None:
        """Update overall safety score"""
        # Mock logic

    def get_safety_report(self) -> dict[str, Any]:
        """Get safety report"""
        total_events = len(self.metrics["events"])
        return {
            "total_events": total_events,
            "severity_level": 1 if total_events > 0 else 0,
            "violations": len(self.violations),
        }


# Re-export
__all__ = ["AISafety", "BiasDetector", "ContentFilter", "PrivacyGuard", "PrivacyProtector", "SafetyMonitor"]
