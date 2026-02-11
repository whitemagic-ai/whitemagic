"""
Gratitude Ledger — Track contributions from both XRPL and x402 channels.
=========================================================================
Records gratitude events (tips, micropayments) with on-chain verification
stubs. Provides stats for the Gratitude Dashboard and Proof of Gratitude
rate-limit boosts.

Storage: $WM_STATE_ROOT/gratitude/ledger.jsonl
"""

import json
import logging
import threading
import time
from dataclasses import dataclass, field, asdict
from enum import Enum
from pathlib import Path
from typing import Any, Dict, List, Optional

logger = logging.getLogger(__name__)


class GratitudeChannel(str, Enum):
    XRPL = "xrpl"
    X402 = "x402"
    MANUAL = "manual"


@dataclass
class GratitudeEvent:
    """A single gratitude contribution."""
    channel: str
    amount: float
    currency: str = "XRP"
    sender: str = ""
    agent_id: str = ""
    tx_hash: str = ""
    verified: bool = False
    timestamp: float = field(default_factory=time.time)
    metadata: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        return asdict(self)


class GratitudeLedger:
    """
    Append-only ledger of gratitude events.

    Thread-safe. Persists to JSONL file under WM_STATE_ROOT.
    """

    def __init__(self, ledger_path: Optional[Path] = None):
        if ledger_path is None:
            from whitemagic.config.paths import WM_ROOT
            ledger_dir = WM_ROOT / "gratitude"
            ledger_dir.mkdir(parents=True, exist_ok=True)
            self._path = ledger_dir / "ledger.jsonl"
        else:
            self._path = ledger_path
            self._path.parent.mkdir(parents=True, exist_ok=True)

        self._lock = threading.Lock()
        self._events: List[GratitudeEvent] = []
        self._load()

    def _load(self) -> None:
        """Load existing events from disk."""
        if not self._path.exists():
            return
        try:
            with open(self._path, "r", encoding="utf-8") as f:
                for line in f:
                    line = line.strip()
                    if line:
                        data = json.loads(line)
                        self._events.append(GratitudeEvent(**data))
        except Exception as exc:
            logger.warning(f"Failed to load gratitude ledger: {exc}")

    def record(self, event: GratitudeEvent) -> None:
        """Record a new gratitude event."""
        with self._lock:
            self._events.append(event)
            try:
                with open(self._path, "a", encoding="utf-8") as f:
                    f.write(json.dumps(event.to_dict(), default=str) + "\n")
            except Exception as exc:
                logger.warning(f"Failed to persist gratitude event: {exc}")

    def get_stats(self) -> Dict[str, Any]:
        """Aggregate gratitude statistics."""
        with self._lock:
            total_xrp = sum(e.amount for e in self._events if e.currency == "XRP")
            total_usdc = sum(e.amount for e in self._events if e.currency == "USDC")
            verified_count = sum(1 for e in self._events if e.verified)
            unique_senders = len(set(e.sender for e in self._events if e.sender))
            unique_agents = len(set(e.agent_id for e in self._events if e.agent_id))

            by_channel: Dict[str, int] = {}
            for e in self._events:
                by_channel[e.channel] = by_channel.get(e.channel, 0) + 1

            return {
                "total_events": len(self._events),
                "total_xrp": round(total_xrp, 6),
                "total_usdc": round(total_usdc, 2),
                "verified_count": verified_count,
                "unique_senders": unique_senders,
                "unique_agents": unique_agents,
                "by_channel": by_channel,
            }

    def get_recent(self, limit: int = 10) -> List[Dict[str, Any]]:
        """Get most recent gratitude events."""
        with self._lock:
            recent = self._events[-limit:]
            return [e.to_dict() for e in reversed(recent)]

    def is_grateful_agent(self, agent_id: str) -> bool:
        """Check if an agent has contributed (for Proof of Gratitude boosts)."""
        with self._lock:
            return any(
                e.agent_id == agent_id and e.verified
                for e in self._events
            )

    def get_agent_contribution(self, agent_id: str) -> Dict[str, Any]:
        """Get total contribution for an agent."""
        with self._lock:
            agent_events = [e for e in self._events if e.agent_id == agent_id]
            return {
                "agent_id": agent_id,
                "total_events": len(agent_events),
                "total_amount": sum(e.amount for e in agent_events),
                "verified": any(e.verified for e in agent_events),
                "grateful_agent": self.is_grateful_agent(agent_id),
            }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_ledger: Optional[GratitudeLedger] = None
_ledger_lock = threading.Lock()


def get_gratitude_ledger() -> GratitudeLedger:
    """Get or create the global GratitudeLedger singleton."""
    global _ledger
    with _ledger_lock:
        if _ledger is None:
            _ledger = GratitudeLedger()
        return _ledger
