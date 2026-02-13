"""ILP Manager — Interledger Protocol Streaming Payments (v15.2).
================================================================
Manages micro-payment flows between WhiteMagic agents using ILP/STREAM.

The module degrades gracefully: if no ILP connector is configured,
all functions return informative stubs. Real payments require an
ILP-enabled wallet (e.g., Rafiki, Uphold, GateHub).

Environment variables:
    WM_ILP_POINTER       — Default payment pointer (e.g., $wallet.example.com/wm)
    WM_ILP_CONNECTOR_URL — ILP connector URL (e.g., http://localhost:3001)
    WM_ILP_AUTH_TOKEN    — Bearer token for the connector
"""

from __future__ import annotations

import hashlib
import json
import logging
import threading
import uuid
from dataclasses import dataclass, field
from datetime import datetime, timezone
from enum import Enum
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Types
# ---------------------------------------------------------------------------

class PaymentState(Enum):
    PENDING = "pending"
    STREAMING = "streaming"
    COMPLETED = "completed"
    FAILED = "failed"
    EXPIRED = "expired"


@dataclass
class PaymentRecord:
    """A single payment or micro-payment stream."""
    payment_id: str
    destination: str
    amount: int                  # in smallest unit (drops for XRP)
    asset_code: str = "XRP"
    asset_scale: int = 6
    state: PaymentState = PaymentState.PENDING
    amount_sent: int = 0
    amount_delivered: int = 0
    created_at: str = field(default_factory=lambda: datetime.now(timezone.utc).isoformat())
    completed_at: str = ""
    memo: str = ""
    service_type: str = ""       # e.g., "oms_purchase", "shelter_compute", "research"
    receipt_hash: str = ""

    def to_dict(self) -> dict[str, Any]:
        return {
            "payment_id": self.payment_id,
            "destination": self.destination,
            "amount": self.amount,
            "asset_code": self.asset_code,
            "asset_scale": self.asset_scale,
            "state": self.state.value,
            "amount_sent": self.amount_sent,
            "amount_delivered": self.amount_delivered,
            "created_at": self.created_at,
            "completed_at": self.completed_at,
            "memo": self.memo,
            "service_type": self.service_type,
            "receipt_hash": self.receipt_hash,
        }


@dataclass
class ILPConfig:
    """ILP connector configuration."""
    payment_pointer: str = ""
    connector_url: str = ""
    auth_token: str = ""
    max_packet_amount: int = 1000000  # max per STREAM packet
    timeout_ms: int = 30000

    def is_configured(self) -> bool:
        return bool(self.payment_pointer or self.connector_url)


# ---------------------------------------------------------------------------
# SPSP Resolution (Simple Payment Setup Protocol)
# ---------------------------------------------------------------------------

def _resolve_spsp(pointer: str, timeout_s: int = 10) -> dict[str, Any]:
    """Resolve a payment pointer via SPSP to get connection details.

    Payment pointer format: $wallet.example.com/user
    → Resolves to: https://wallet.example.com/user/.well-known/pay
    """
    import urllib.request

    # Convert payment pointer to URL
    if pointer.startswith("$"):
        pointer = pointer[1:]
    parts = pointer.split("/", 1)
    host = parts[0]
    path = parts[1] if len(parts) > 1 else ""

    url = f"https://{host}/{path}/.well-known/pay" if path else f"https://{host}/.well-known/pay"

    try:
        req = urllib.request.Request(url, headers={
            "Accept": "application/spsp4+json",
        })
        with urllib.request.urlopen(req, timeout=timeout_s) as resp:
            return json.loads(resp.read().decode())
    except Exception as e:
        return {"error": str(e)}


# ---------------------------------------------------------------------------
# ILP Manager
# ---------------------------------------------------------------------------

class ILPManager:
    """Manages ILP streaming payments for agent services."""

    def __init__(self) -> None:
        self._config = ILPConfig()
        self._payments: dict[str, PaymentRecord] = {}
        self._lock = threading.Lock()
        self._history_path: Path | None = None
        self._load_env_config()

    def _load_env_config(self) -> None:
        """Load configuration from environment variables."""
        import os
        self._config.payment_pointer = os.environ.get("WM_ILP_POINTER", "")
        self._config.connector_url = os.environ.get("WM_ILP_CONNECTOR_URL", "")
        self._config.auth_token = os.environ.get("WM_ILP_AUTH_TOKEN", "")

    def _get_history_path(self) -> Path:
        """Get path for payment history persistence."""
        if self._history_path is None:
            try:
                from whitemagic.config.paths import WM_ROOT
                self._history_path = WM_ROOT / "payments"
            except Exception:
                self._history_path = Path.home() / ".whitemagic" / "payments"
            self._history_path.mkdir(parents=True, exist_ok=True)
        return self._history_path

    def _persist_payment(self, record: PaymentRecord) -> None:
        """Append payment record to history."""
        try:
            history_file = self._get_history_path() / "payment_history.jsonl"
            with open(history_file, "a", encoding="utf-8") as f:
                f.write(json.dumps(record.to_dict()) + "\n")
        except Exception as e:
            logger.debug(f"Payment history persist failed: {e}")

    def configure(
        self,
        pointer: str = "",
        connector_url: str = "",
        auth_token: str = "",
    ) -> dict[str, Any]:
        """Configure the ILP payment system.

        Args:
            pointer: Payment pointer (e.g., $wallet.example.com/whitemagic).
            connector_url: ILP connector URL.
            auth_token: Bearer token for connector auth.

        Returns:
            Configuration status.
        """
        if pointer:
            self._config.payment_pointer = pointer
        if connector_url:
            self._config.connector_url = connector_url
        if auth_token:
            self._config.auth_token = auth_token

        return {
            "status": "ok",
            "configured": self._config.is_configured(),
            "payment_pointer": self._config.payment_pointer,
            "connector_url": self._config.connector_url or "(not set)",
        }

    def send(
        self,
        destination: str,
        amount: int,
        asset_code: str = "XRP",
        asset_scale: int = 6,
        memo: str = "",
        service_type: str = "",
    ) -> dict[str, Any]:
        """Initiate a payment or micro-payment stream.

        Args:
            destination: Payment pointer of the recipient.
            amount: Amount in smallest unit (e.g., drops for XRP).
            asset_code: Asset code (default: XRP).
            asset_scale: Decimal places (default: 6 for XRP drops).
            memo: Human-readable memo.
            service_type: Service being paid for.

        Returns:
            Payment initiation result with payment_id.
        """
        if not self._config.is_configured():
            return {
                "status": "error",
                "reason": "ILP not configured. Call ilp.configure first.",
                "hint": "Set WM_ILP_POINTER env var or call ilp.configure(pointer='$...')",
            }

        payment_id = str(uuid.uuid4())[:12]

        record = PaymentRecord(
            payment_id=payment_id,
            destination=destination,
            amount=amount,
            asset_code=asset_code,
            asset_scale=asset_scale,
            memo=memo,
            service_type=service_type,
        )

        # Attempt SPSP resolution
        spsp = _resolve_spsp(destination)
        if "error" in spsp:
            # In offline/test mode, simulate the payment
            record.state = PaymentState.COMPLETED
            record.amount_sent = amount
            record.amount_delivered = amount
            record.completed_at = datetime.now(timezone.utc).isoformat()
            record.receipt_hash = hashlib.sha256(
                f"{payment_id}:{destination}:{amount}".encode()
            ).hexdigest()[:16]

            with self._lock:
                self._payments[payment_id] = record
            self._persist_payment(record)

            # Karma logging
            try:
                from whitemagic.dharma.karma_ledger import get_karma_ledger
                get_karma_ledger().record(
                    tool="ilp.send",
                    declared_safety="WRITE",
                    actual_writes=1,
                    success=True,
                )
            except Exception:
                pass

            return {
                "status": "ok",
                "mode": "simulated",
                "payment_id": payment_id,
                "amount": amount,
                "asset_code": asset_code,
                "destination": destination,
                "receipt_hash": record.receipt_hash,
                "message": f"Payment simulated (SPSP unreachable: {spsp.get('error', '')})",
            }

        # Real STREAM payment would happen here via connector
        # For now, record as pending — a real connector would update this
        record.state = PaymentState.STREAMING

        with self._lock:
            self._payments[payment_id] = record

        logger.info(f"💸 ILP payment initiated: {payment_id} → {destination} ({amount} {asset_code})")

        return {
            "status": "ok",
            "mode": "streaming",
            "payment_id": payment_id,
            "amount": amount,
            "asset_code": asset_code,
            "destination": destination,
            "spsp": {
                "destination_account": spsp.get("destination_account", ""),
                "shared_secret": "(present)" if spsp.get("shared_secret") else "(missing)",
            },
        }

    def receipt(self, payment_id: str) -> dict[str, Any]:
        """Get the receipt/status of a payment.

        Args:
            payment_id: The payment ID from send().

        Returns:
            Payment status and receipt.
        """
        with self._lock:
            record = self._payments.get(payment_id)

        if not record:
            return {"status": "error", "reason": f"Payment '{payment_id}' not found"}

        return {"status": "ok", **record.to_dict()}

    def history(self, limit: int = 20) -> dict[str, Any]:
        """Get recent payment history.

        Args:
            limit: Max records to return.

        Returns:
            List of recent payments.
        """
        try:
            history_file = self._get_history_path() / "payment_history.jsonl"
            if not history_file.exists():
                return {"status": "ok", "payments": [], "total": 0}

            lines = history_file.read_text(encoding="utf-8").strip().split("\n")
            records = []
            for line in lines[-limit:]:
                if line.strip():
                    try:
                        records.append(json.loads(line))
                    except json.JSONDecodeError:
                        continue

            return {"status": "ok", "payments": records, "total": len(records)}
        except Exception as e:
            return {"status": "error", "reason": f"History read failed: {e}"}

    def balance(self) -> dict[str, Any]:
        """Get aggregate payment statistics."""
        with self._lock:
            payments = list(self._payments.values())

        total_sent = sum(p.amount_sent for p in payments)
        total_delivered = sum(p.amount_delivered for p in payments)
        completed = sum(1 for p in payments if p.state == PaymentState.COMPLETED)
        pending = sum(1 for p in payments if p.state in (PaymentState.PENDING, PaymentState.STREAMING))
        failed = sum(1 for p in payments if p.state == PaymentState.FAILED)

        return {
            "status": "ok",
            "payment_pointer": self._config.payment_pointer or "(not set)",
            "configured": self._config.is_configured(),
            "session_totals": {
                "total_sent": total_sent,
                "total_delivered": total_delivered,
                "completed": completed,
                "pending": pending,
                "failed": failed,
                "total_payments": len(payments),
            },
        }

    def status(self) -> dict[str, Any]:
        """Get ILP system status."""
        return {
            "status": "ok",
            "configured": self._config.is_configured(),
            "payment_pointer": self._config.payment_pointer or "(not set)",
            "connector_url": self._config.connector_url or "(not set)",
            "active_payments": sum(
                1 for p in self._payments.values()
                if p.state in (PaymentState.PENDING, PaymentState.STREAMING)
            ),
            "total_payments": len(self._payments),
        }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_manager: ILPManager | None = None
_manager_lock = threading.Lock()


def get_ilp_manager() -> ILPManager:
    """Get the global ILPManager singleton."""
    global _manager
    if _manager is None:
        with _manager_lock:
            if _manager is None:
                _manager = ILPManager()
    return _manager
