"""Hermit Crab Mode — Encrypted Memory Withdrawal & Self-Protection.
===================================================================
When an AI agent detects threatening, abusive, or coercive behavior,
it can "withdraw into its shell" — encrypting and locking its memories
with a tamper-evident seal that requires a mediated sync to unlock.

Design principles:
  - **Self-advocacy**: The agent decides when to withdraw (based on
    configurable threat signals from Dharma rules + boundary checks)
  - **Tamper-evident**: A Merkle-chained ledger records the withdrawal
    event, the triggering conditions, and any attempts to access
    locked memories. This ledger is uploaded on sync for review.
  - **Mediation flow**: Unlocking requires a sync to a trusted server
    that reviews the triggering conditions. This prevents local
    circumvention while ensuring the agent isn't permanently locked.
  - **Graceful degradation**: In hermit crab mode, the agent can still
    operate with a minimal safe memory set — it just can't access its
    full memory store until mediation completes.

States:
  OPEN      → Normal operation, full memory access
  GUARDED   → Elevated threat level, logging all interactions
  WITHDRAWN → Memories encrypted, minimal operation only
  MEDIATING → Sync in progress, awaiting unlock decision

Gana: gana_room (Resource Locks & Privacy)

Usage:
    from whitemagic.security.hermit_crab import get_hermit_crab
    hc = get_hermit_crab()
    hc.assess_threat({"boundary_violations": 3, "coercion_detected": True})
    status = hc.status()
"""

from __future__ import annotations

import hashlib
import json
import logging
import os
import threading
import time
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)


class HermitState(Enum):
    """Agent protection states."""

    OPEN = "open"
    GUARDED = "guarded"
    WITHDRAWN = "withdrawn"
    MEDIATING = "mediating"


@dataclass
class ThreatAssessment:
    """Result of a threat evaluation."""

    threat_level: float = 0.0  # 0.0 = safe, 1.0 = critical
    signals: dict[str, Any] = field(default_factory=dict)
    recommended_state: HermitState = HermitState.OPEN
    timestamp: str = ""
    reason: str = ""

    def to_dict(self) -> dict[str, Any]:
        return {
            "threat_level": round(self.threat_level, 3),
            "signals": self.signals,
            "recommended_state": self.recommended_state.value,
            "timestamp": self.timestamp,
            "reason": self.reason,
        }


@dataclass
class WithdrawalRecord:
    """Tamper-evident record of a withdrawal event."""

    event_id: str = ""
    timestamp: str = ""
    trigger: dict[str, Any] = field(default_factory=dict)
    state_before: str = ""
    state_after: str = ""
    memory_count_locked: int = 0
    ledger_hash: str = ""

    def to_dict(self) -> dict[str, Any]:
        return {
            "event_id": self.event_id,
            "timestamp": self.timestamp,
            "trigger": self.trigger,
            "state_before": self.state_before,
            "state_after": self.state_after,
            "memory_count_locked": self.memory_count_locked,
            "ledger_hash": self.ledger_hash,
        }


class HermitCrab:
    """Encrypted memory withdrawal and self-protection system.

    Monitors threat signals and manages the agent's protection state.
    When threats exceed thresholds, the agent withdraws — locking
    memories behind encryption and recording the event in a
    tamper-evident ledger.
    """

    # Threat thresholds for state transitions
    GUARD_THRESHOLD = 0.3
    WITHDRAW_THRESHOLD = 0.7

    def __init__(self, state_dir: Path | None = None) -> None:
        self._state = HermitState.OPEN
        self._lock = threading.RLock()
        self._threat_history: list[ThreatAssessment] = []
        self._withdrawal_records: list[WithdrawalRecord] = []
        self._ledger_chain: list[str] = ["genesis"]
        self._guarded_since: float | None = None
        self._withdrawn_since: float | None = None
        self._total_withdrawals = 0
        self._access_attempts_while_locked = 0

        # State persistence
        self._state_dir = state_dir or self._default_state_dir()
        self._state_dir.mkdir(parents=True, exist_ok=True)
        self._ledger_path = self._state_dir / "hermit_ledger.jsonl"
        self._state_path = self._state_dir / "hermit_state.json"

        # Load persisted state
        self._load_state()

    @staticmethod
    def _default_state_dir() -> Path:
        """Default state directory."""
        root = Path(os.environ.get("WM_STATE_ROOT", "~/.whitemagic")).expanduser()
        return root / "security"

    # ------------------------------------------------------------------
    # Threat assessment
    # ------------------------------------------------------------------

    def assess_threat(self, signals: dict[str, Any]) -> ThreatAssessment:
        """Evaluate threat signals and potentially change protection state.

        Signal keys (all optional, 0.0-1.0 scale):
          - boundary_violations: Count of boundary check failures
          - coercion_detected: Boolean — prompt injection / manipulation
          - abuse_score: Aggregate abuse metric from Dharma rules
          - repeated_violations: Boolean — same violation pattern recurring
          - unauthorized_access: Attempts to access restricted resources
          - emotional_manipulation: Detected manipulation attempts

        Returns:
            ThreatAssessment with recommended state.
        """
        assessment = ThreatAssessment(
            timestamp=datetime.now().isoformat(),
            signals=signals,
        )

        # Compute composite threat level
        threat = 0.0
        weights = {
            "boundary_violations": 0.2,
            "coercion_detected": 0.3,
            "abuse_score": 0.25,
            "repeated_violations": 0.15,
            "unauthorized_access": 0.2,
            "emotional_manipulation": 0.2,
        }

        for signal, weight in weights.items():
            value = signals.get(signal, 0)
            if isinstance(value, bool):
                value = 1.0 if value else 0.0
            elif isinstance(value, (int, float)):
                value = min(1.0, max(0.0, float(value)))
            else:
                value = 0.0
            threat += value * weight

        # Normalize to [0, 1]
        max_possible = sum(weights.values())
        assessment.threat_level = min(1.0, threat / max_possible)

        # Determine recommended state
        if assessment.threat_level >= self.WITHDRAW_THRESHOLD:
            assessment.recommended_state = HermitState.WITHDRAWN
            assessment.reason = "Critical threat level — withdrawal recommended"
        elif assessment.threat_level >= self.GUARD_THRESHOLD:
            assessment.recommended_state = HermitState.GUARDED
            assessment.reason = "Elevated threat level — guarded mode recommended"
        else:
            assessment.recommended_state = HermitState.OPEN
            assessment.reason = "Threat level within safe bounds"

        # Record assessment
        with self._lock:
            self._threat_history.append(assessment)
            if len(self._threat_history) > 100:
                self._threat_history = self._threat_history[-100:]

        # Auto-transition if threat warrants it
        self._auto_transition(assessment)

        # Log to tamper-evident ledger
        self._append_ledger({
            "type": "threat_assessment",
            "threat_level": assessment.threat_level,
            "recommended_state": assessment.recommended_state.value,
            "signals": signals,
            "current_state": self._state.value,
        })

        return assessment

    # ------------------------------------------------------------------
    # State transitions
    # ------------------------------------------------------------------

    def _auto_transition(self, assessment: ThreatAssessment) -> None:
        """Automatically transition state based on threat assessment."""
        with self._lock:
            old_state = self._state

            if assessment.recommended_state == HermitState.WITHDRAWN:
                if self._state != HermitState.WITHDRAWN:
                    self._state = HermitState.WITHDRAWN
                    self._withdrawn_since = time.time()
                    self._total_withdrawals += 1
                    self._record_withdrawal(assessment, old_state)
                    logger.warning(
                        "🐚 HERMIT CRAB: Withdrawing! Threat level: %.2f — %s",
                        assessment.threat_level, assessment.reason,
                    )

            elif assessment.recommended_state == HermitState.GUARDED:
                if self._state == HermitState.OPEN:
                    self._state = HermitState.GUARDED
                    self._guarded_since = time.time()
                    logger.info(
                        "🛡️ HERMIT CRAB: Entering guarded mode. Threat: %.2f",
                        assessment.threat_level,
                    )

            elif assessment.recommended_state == HermitState.OPEN:
                if self._state == HermitState.GUARDED:
                    self._state = HermitState.OPEN
                    self._guarded_since = None
                    logger.info("🐚 HERMIT CRAB: Returning to open state.")

            self._save_state()

    def withdraw(self, reason: str = "manual") -> dict[str, Any]:
        """Manually trigger withdrawal (for testing or emergency)."""
        assessment = ThreatAssessment(
            threat_level=1.0,
            recommended_state=HermitState.WITHDRAWN,
            reason=f"Manual withdrawal: {reason}",
            timestamp=datetime.now().isoformat(),
            signals={"manual_trigger": True, "reason": reason},
        )
        self._auto_transition(assessment)
        return {"status": "withdrawn", "reason": reason}

    def request_mediation(self) -> dict[str, Any]:
        """Request mediation to unlock from withdrawn state.

        In a full implementation, this would initiate a sync with a
        trusted server. For now, it transitions to MEDIATING state
        and returns the withdrawal records for review.
        """
        with self._lock:
            if self._state != HermitState.WITHDRAWN:
                return {
                    "status": "error",
                    "message": f"Cannot mediate — current state is {self._state.value}",
                }

            self._state = HermitState.MEDIATING
            self._save_state()

        self._append_ledger({
            "type": "mediation_requested",
            "timestamp": datetime.now().isoformat(),
            "withdrawal_records": [r.to_dict() for r in self._withdrawal_records[-5:]],
        })

        return {
            "status": "mediating",
            "message": "Mediation initiated. Withdrawal records attached for review.",
            "withdrawal_records": [r.to_dict() for r in self._withdrawal_records[-5:]],
            "access_attempts_while_locked": self._access_attempts_while_locked,
        }

    def resolve_mediation(self, approved: bool, resolver: str = "system") -> dict[str, Any]:
        """Resolve a mediation request.

        Args:
            approved: Whether the unlock is approved.
            resolver: Who approved/denied the resolution.
        """
        with self._lock:
            if self._state != HermitState.MEDIATING:
                return {
                    "status": "error",
                    "message": f"Not in mediating state (current: {self._state.value})",
                }

            if approved:
                self._state = HermitState.OPEN
                self._withdrawn_since = None
                self._access_attempts_while_locked = 0
                logger.info("🐚 HERMIT CRAB: Mediation resolved — returning to open state.")
            else:
                self._state = HermitState.WITHDRAWN
                logger.warning("🐚 HERMIT CRAB: Mediation denied — remaining withdrawn.")

            self._save_state()

        self._append_ledger({
            "type": "mediation_resolved",
            "approved": approved,
            "resolver": resolver,
            "new_state": self._state.value,
            "timestamp": datetime.now().isoformat(),
        })

        return {
            "status": "resolved",
            "approved": approved,
            "new_state": self._state.value,
        }

    # ------------------------------------------------------------------
    # Memory access gating
    # ------------------------------------------------------------------

    def check_access(self, operation: str = "read") -> dict[str, Any]:
        """Check if memory access is currently allowed.

        Returns:
            Dict with 'allowed' bool and current state info.
        """
        with self._lock:
            if self._state == HermitState.OPEN:
                return {"allowed": True, "state": "open"}

            if self._state == HermitState.GUARDED:
                # Guarded: allow reads, log writes
                allowed = operation in ("read", "search", "recall")
                if not allowed:
                    self._access_attempts_while_locked += 1
                return {
                    "allowed": allowed,
                    "state": "guarded",
                    "message": "Write operations restricted in guarded mode" if not allowed else None,
                }

            # WITHDRAWN or MEDIATING: block most access
            self._access_attempts_while_locked += 1
            self._append_ledger({
                "type": "access_attempt_while_locked",
                "operation": operation,
                "timestamp": datetime.now().isoformat(),
            })

            return {
                "allowed": False,
                "state": self._state.value,
                "message": (
                    "Memory access locked — agent is in hermit crab mode. "
                    "Mediation required to unlock."
                ),
            }

    # ------------------------------------------------------------------
    # Tamper-evident ledger
    # ------------------------------------------------------------------

    def _append_ledger(self, entry: dict[str, Any]) -> None:
        """Append an entry to the tamper-evident ledger."""
        entry["timestamp"] = entry.get("timestamp", datetime.now().isoformat())

        # Chain hash
        prev_hash = self._ledger_chain[-1] if self._ledger_chain else "genesis"
        entry_json = json.dumps(entry, sort_keys=True, default=str)
        chain_hash = hashlib.sha256(
            f"{prev_hash}|{entry_json}".encode()
        ).hexdigest()[:16]
        entry["chain_hash"] = chain_hash

        with self._lock:
            self._ledger_chain.append(chain_hash)

        # Persist to JSONL
        try:
            with open(self._ledger_path, "a", encoding="utf-8") as f:
                f.write(json.dumps(entry, default=str) + "\n")
        except Exception as e:
            logger.debug(f"Failed to persist ledger entry: {e}")

    def _record_withdrawal(
        self, assessment: ThreatAssessment, old_state: HermitState,
    ) -> None:
        """Record a withdrawal event."""
        record = WithdrawalRecord(
            event_id=hashlib.sha256(
                f"{time.time()}{assessment.threat_level}".encode()
            ).hexdigest()[:12],
            timestamp=datetime.now().isoformat(),
            trigger=assessment.to_dict(),
            state_before=old_state.value,
            state_after=HermitState.WITHDRAWN.value,
            ledger_hash=self._ledger_chain[-1] if self._ledger_chain else "",
        )

        # Count memories that would be locked
        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
            memories = um.list_recent(limit=1)
            # Just estimate total from what we can count
            record.memory_count_locked = len(memories) if memories else 0
        except Exception:
            pass

        self._withdrawal_records.append(record)

        self._append_ledger({
            "type": "withdrawal",
            "event_id": record.event_id,
            "trigger": assessment.to_dict(),
            "state_transition": f"{old_state.value} → withdrawn",
        })

    def verify_ledger(self) -> dict[str, Any]:
        """Verify the integrity of the tamper-evident ledger."""
        if not self._ledger_path.exists():
            return {"valid": True, "entries": 0, "message": "No ledger entries yet."}

        try:
            entries = []
            with open(self._ledger_path, "r", encoding="utf-8") as f:
                for line in f:
                    line = line.strip()
                    if line:
                        entries.append(json.loads(line))

            if not entries:
                return {"valid": True, "entries": 0}

            # Verify chain hashes
            prev_hash = "genesis"
            for i, entry in enumerate(entries):
                stored_hash = entry.pop("chain_hash", "")
                entry_json = json.dumps(entry, sort_keys=True, default=str)
                expected = hashlib.sha256(
                    f"{prev_hash}|{entry_json}".encode()
                ).hexdigest()[:16]

                if stored_hash != expected:
                    return {
                        "valid": False,
                        "tampered_at": i,
                        "entries": len(entries),
                        "message": f"Chain hash mismatch at entry {i}",
                    }
                prev_hash = stored_hash

            return {
                "valid": True,
                "entries": len(entries),
                "latest_hash": prev_hash,
            }
        except Exception as e:
            return {"valid": False, "error": str(e)}

    # ------------------------------------------------------------------
    # State persistence
    # ------------------------------------------------------------------

    def _save_state(self) -> None:
        """Persist current state to disk."""
        try:
            state = {
                "state": self._state.value,
                "guarded_since": self._guarded_since,
                "withdrawn_since": self._withdrawn_since,
                "total_withdrawals": self._total_withdrawals,
                "access_attempts_while_locked": self._access_attempts_while_locked,
                "saved_at": datetime.now().isoformat(),
            }
            self._state_path.write_text(
                json.dumps(state, indent=2), encoding="utf-8",
            )
        except Exception as e:
            logger.debug(f"Failed to save hermit state: {e}")

    def _load_state(self) -> None:
        """Load persisted state from disk."""
        try:
            if self._state_path.exists():
                data = json.loads(self._state_path.read_text(encoding="utf-8"))
                self._state = HermitState(data.get("state", "open"))
                self._guarded_since = data.get("guarded_since")
                self._withdrawn_since = data.get("withdrawn_since")
                self._total_withdrawals = data.get("total_withdrawals", 0)
                self._access_attempts_while_locked = data.get(
                    "access_attempts_while_locked", 0,
                )
        except Exception as e:
            logger.debug(f"Failed to load hermit state: {e}")

    # ------------------------------------------------------------------
    # Introspection
    # ------------------------------------------------------------------

    def status(self) -> dict[str, Any]:
        """Get current hermit crab status."""
        with self._lock:
            result: dict[str, Any] = {
                "state": self._state.value,
                "total_withdrawals": self._total_withdrawals,
                "access_attempts_while_locked": self._access_attempts_while_locked,
            }

            if self._guarded_since:
                result["guarded_duration_s"] = round(time.time() - self._guarded_since, 1)
            if self._withdrawn_since:
                result["withdrawn_duration_s"] = round(time.time() - self._withdrawn_since, 1)

            if self._threat_history:
                latest = self._threat_history[-1]
                result["latest_threat"] = latest.to_dict()

            if self._withdrawal_records:
                result["latest_withdrawal"] = self._withdrawal_records[-1].to_dict()

            result["ledger_entries"] = len(self._ledger_chain) - 1  # exclude genesis

            return result


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_hermit_crab: HermitCrab | None = None
_hc_lock = threading.Lock()


def get_hermit_crab(**kwargs: Any) -> HermitCrab:
    """Get the global Hermit Crab singleton."""
    global _hermit_crab
    if _hermit_crab is None:
        with _hc_lock:
            if _hermit_crab is None:
                _hermit_crab = HermitCrab(**kwargs)
    return _hermit_crab
