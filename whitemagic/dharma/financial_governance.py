"""Dharma Financial Governance — Bounded Wallet Rules (Leap 9f + 10b).
=====================================================================
Declarative YAML-based financial governance for AI agent spending.

Defines spending limits, approval thresholds, and rate limits that
agents must obey when performing economic actions (tips, purchases,
bounty payments). Rules are loaded from YAML files and can be
hot-reloaded at runtime.

This is the "Bounded Wallet Governor" — agents can spend within their
declared limits, but any action exceeding the thresholds requires
explicit human approval or Dharma council vote.

Usage:
    from whitemagic.dharma.financial_governance import get_wallet_governor
    gov = get_wallet_governor()
    ok, reason = gov.check_spend("tip", 0.50, "XRP")
"""

from __future__ import annotations

import logging
import time
from dataclasses import dataclass, field
from typing import Any

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Default financial rules (can be overridden by YAML)
# ---------------------------------------------------------------------------

_DEFAULT_RULES: dict[str, Any] = {
    "version": "1.0",
    "currency": "XRP",
    "limits": {
        "per_transaction_max": 1.0,       # Max spend per single transaction
        "per_session_max": 5.0,           # Max total spend per session
        "per_day_max": 20.0,              # Max total spend per 24h
        "per_month_max": 100.0,           # Max total spend per 30d
    },
    "approval_thresholds": {
        "auto_approve_below": 0.10,       # Auto-approve transactions under this
        "require_human_above": 5.0,       # Require human approval above this
        "require_council_above": 20.0,    # Require Dharma council above this
    },
    "rate_limits": {
        "max_transactions_per_minute": 3,
        "max_transactions_per_hour": 30,
        "cooldown_after_rejection_s": 60,
    },
    "allowed_actions": [
        "tip",              # Gratitude tips to WM
        "bounty_fund",      # Fund a micro-bounty
        "service_payment",  # Pay for an external service
    ],
    "blocked_actions": [
        "withdrawal",       # Cannot withdraw funds
        "transfer_out",     # Cannot transfer to external wallets
    ],
}


# ---------------------------------------------------------------------------
# Data structures
# ---------------------------------------------------------------------------

@dataclass
class SpendRecord:
    """Record of a single spend action."""
    action: str
    amount: float
    currency: str
    timestamp: float
    approved: bool
    approval_level: str  # "auto", "human", "council", "denied"


@dataclass
class WalletState:
    """Current wallet spending state."""
    session_total: float = 0.0
    day_total: float = 0.0
    month_total: float = 0.0
    transaction_count_minute: int = 0
    transaction_count_hour: int = 0
    last_rejection_time: float = 0.0
    history: list[SpendRecord] = field(default_factory=list)

    def prune_old(self, now: float) -> None:
        """Remove expired records from rolling windows."""
        minute_cutoff = now - 60
        hour_cutoff = now - 3600
        day_cutoff = now - 86400

        self.transaction_count_minute = sum(
            1 for r in self.history if r.timestamp >= minute_cutoff
        )
        self.transaction_count_hour = sum(
            1 for r in self.history if r.timestamp >= hour_cutoff
        )
        self.day_total = sum(
            r.amount for r in self.history
            if r.timestamp >= day_cutoff and r.approved
        )


# ---------------------------------------------------------------------------
# Wallet Governor
# ---------------------------------------------------------------------------

class WalletGovernor:
    """Bounded wallet governor enforcing financial Dharma rules."""

    def __init__(self, rules: dict[str, Any] | None = None):
        self._rules = rules or dict(_DEFAULT_RULES)
        self._state = WalletState()
        self._load_yaml_overrides()

    def _load_yaml_overrides(self) -> None:
        """Load YAML rule overrides from $WM_STATE_ROOT/dharma/rules.d/financial.yaml."""
        try:
            from whitemagic.config.paths import WM_ROOT
            yaml_path = WM_ROOT / "dharma" / "rules.d" / "financial.yaml"
            if yaml_path.exists():
                import yaml
                with open(yaml_path) as f:
                    overrides = yaml.safe_load(f)
                if isinstance(overrides, dict):
                    # Deep merge limits and thresholds
                    for section in ("limits", "approval_thresholds", "rate_limits"):
                        if section in overrides and isinstance(overrides[section], dict):
                            self._rules.setdefault(section, {}).update(overrides[section])
                    for key in ("allowed_actions", "blocked_actions", "currency"):
                        if key in overrides:
                            self._rules[key] = overrides[key]
                    logger.info(f"Financial governance: loaded YAML overrides from {yaml_path}")
        except Exception as e:
            logger.debug(f"Financial governance YAML load: {e}")

    def check_spend(
        self, action: str, amount: float, currency: str | None = None,
    ) -> tuple[bool, str]:
        """Check if a spend action is allowed.

        Returns (allowed, reason_string).
        """
        now = time.time()
        self._state.prune_old(now)
        currency = currency or self._rules.get("currency", "XRP")

        limits = self._rules.get("limits", {})
        thresholds = self._rules.get("approval_thresholds", {})
        rate = self._rules.get("rate_limits", {})

        # 1. Action allow/block list
        if action in self._rules.get("blocked_actions", []):
            return False, f"Action '{action}' is blocked by financial governance"

        allowed_actions = self._rules.get("allowed_actions", [])
        if allowed_actions and action not in allowed_actions:
            return False, f"Action '{action}' not in allowed actions: {allowed_actions}"

        # 2. Per-transaction limit
        per_tx = limits.get("per_transaction_max", 1.0)
        if amount > per_tx:
            return False, f"Amount {amount} exceeds per-transaction max ({per_tx} {currency})"

        # 3. Session limit
        per_session = limits.get("per_session_max", 5.0)
        if self._state.session_total + amount > per_session:
            return False, f"Would exceed session max ({per_session} {currency})"

        # 4. Daily limit
        per_day = limits.get("per_day_max", 20.0)
        if self._state.day_total + amount > per_day:
            return False, f"Would exceed daily max ({per_day} {currency})"

        # 5. Rate limits
        max_per_min = rate.get("max_transactions_per_minute", 3)
        if self._state.transaction_count_minute >= max_per_min:
            return False, f"Rate limit: {max_per_min} transactions/minute exceeded"

        max_per_hour = rate.get("max_transactions_per_hour", 30)
        if self._state.transaction_count_hour >= max_per_hour:
            return False, f"Rate limit: {max_per_hour} transactions/hour exceeded"

        # 6. Cooldown after rejection
        cooldown = rate.get("cooldown_after_rejection_s", 60)
        if now - self._state.last_rejection_time < cooldown:
            remaining = int(cooldown - (now - self._state.last_rejection_time))
            return False, f"Cooldown: {remaining}s remaining after previous rejection"

        # 7. Determine approval level
        auto_below = thresholds.get("auto_approve_below", 0.10)
        human_above = thresholds.get("require_human_above", 5.0)

        if amount <= auto_below:
            approval_level = "auto"
        elif amount <= human_above:
            approval_level = "auto"  # Within normal bounds
        else:
            approval_level = "requires_human"
            return False, f"Amount {amount} requires human approval (threshold: {human_above} {currency})"

        # Record the spend
        record = SpendRecord(
            action=action,
            amount=amount,
            currency=currency,
            timestamp=now,
            approved=True,
            approval_level=approval_level,
        )
        self._state.history.append(record)
        self._state.session_total += amount

        # Keep history bounded
        if len(self._state.history) > 1000:
            self._state.history = self._state.history[-500:]

        return True, f"Approved ({approval_level}): {amount} {currency} for {action}"

    def record_rejection(self) -> None:
        """Record that a spend was rejected (triggers cooldown)."""
        self._state.last_rejection_time = time.time()

    def get_status(self) -> dict[str, Any]:
        """Get current wallet governance status."""
        self._state.prune_old(time.time())
        limits = self._rules.get("limits", {})
        return {
            "session_spent": round(self._state.session_total, 4),
            "session_limit": limits.get("per_session_max", 5.0),
            "day_spent": round(self._state.day_total, 4),
            "day_limit": limits.get("per_day_max", 20.0),
            "transactions_this_minute": self._state.transaction_count_minute,
            "transactions_this_hour": self._state.transaction_count_hour,
            "currency": self._rules.get("currency", "XRP"),
            "recent_transactions": len(self._state.history),
        }

    def get_rules(self) -> dict[str, Any]:
        """Get the current financial governance rules."""
        return dict(self._rules)


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_governor: WalletGovernor | None = None


def get_wallet_governor() -> WalletGovernor:
    """Get the global Wallet Governor instance."""
    global _governor
    if _governor is None:
        _governor = WalletGovernor()
    return _governor
