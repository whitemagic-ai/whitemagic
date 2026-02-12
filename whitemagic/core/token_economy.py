"""Token Economy - Resource Tracking & Optimization.

Tracks token usage across operations and optimizes resource allocation.

Created: January 6, 2026
"""

import logging
from dataclasses import dataclass
from datetime import datetime
from typing import Any

logger = logging.getLogger(__name__)


@dataclass
class TokenUsage:
    """Record of token usage."""

    operation: str
    tokens_used: int
    tokens_available: int
    efficiency: float
    timestamp: datetime | None = None

    def __post_init__(self) -> Any:
        if self.timestamp is None:
            self.timestamp = datetime.now()


class TokenEconomy:
    """Manages token budget and optimizes resource allocation.
    """

    def __init__(self, budget: int = 200000) -> None:
        self.total_budget = budget
        self.tokens_used = 0
        self.usage_history: list = []

    def track_usage(self, operation: str, tokens: int) -> TokenUsage:
        """Track token usage for an operation."""
        self.tokens_used += tokens

        usage = TokenUsage(
            operation=operation,
            tokens_used=tokens,
            tokens_available=self.total_budget - self.tokens_used,
            efficiency=tokens / self.total_budget,
        )

        self.usage_history.append(usage)
        return usage

    def get_budget_status(self) -> dict[str, Any]:
        """Get current budget status."""
        used_percent = (self.tokens_used / self.total_budget) * 100

        return {
            "total_budget": self.total_budget,
            "tokens_used": self.tokens_used,
            "tokens_remaining": self.total_budget - self.tokens_used,
            "usage_percent": used_percent,
            "status": "optimal" if used_percent < 60 else "monitor" if used_percent < 80 else "critical",
            "operations_tracked": len(self.usage_history),
        }

    def optimize_allocation(self) -> dict[str, str]:
        """Provide optimization recommendations."""
        status = self.get_budget_status()

        recommendations = []
        if status["usage_percent"] > 70:
            recommendations.append("Consider using more concise responses")
            recommendations.append("Prioritize high-value operations")

        return {
            "status": status["status"],
            "recommendations": recommendations,
        }


_token_economy: TokenEconomy | None = None

def get_token_economy() -> TokenEconomy:
    global _token_economy
    if _token_economy is None:
        _token_economy = TokenEconomy()
        logger.info("Token Economy initialized")
    return _token_economy
