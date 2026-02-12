"""Karma XRPL Anchor — On-Chain Trust Attestation (Phase 4B2).
=============================================================
Anchors the Karma Ledger's Merkle root to the XRP Ledger as a
tamper-evident timestamp proof. Any agent can verify that WhiteMagic's
karma history was intact at the moment of anchoring.

Flow:
    1. ``compute_anchor()`` — snapshot current Merkle root + metadata
    2. ``submit_anchor()`` — write Merkle root as a Memo on an XRPL
       self-payment (0 XRP) to the testnet (or mainnet)
    3. ``verify_anchor()`` — given a tx_hash, retrieve the memo and
       compare against the local Merkle root

Dependencies:
    pip install xrpl-py   (or:  pip install whitemagic[trust])

The module degrades gracefully: if xrpl-py is not installed, all
functions return informative stubs instead of crashing.

Usage:
    from whitemagic.dharma.karma_anchor import compute_anchor, submit_anchor
    snapshot = compute_anchor()
    result = submit_anchor(snapshot["merkle_root"], wallet_seed="s...")
"""

from __future__ import annotations

import hashlib
import json
import logging
from dataclasses import asdict, dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Optional XRPL import
# ---------------------------------------------------------------------------

try:
    import xrpl
    from xrpl.clients import JsonRpcClient
    from xrpl.models.amounts import IssuedCurrencyAmount  # noqa: F401
    from xrpl.models.requests import Tx
    from xrpl.models.transactions import Memo, Payment
    from xrpl.transaction import submit_and_wait
    from xrpl.wallet import Wallet

    _XRPL_AVAILABLE = True
except ImportError:
    _XRPL_AVAILABLE = False

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

XRPL_TESTNET_URL = "https://s.altnet.rippletest.net:51234"
XRPL_MAINNET_URL = "https://s1.ripple.com:51234"
MEMO_TYPE_HEX = b"whitemagic/karma-anchor".hex()
ANCHOR_VERSION = "1"

# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------


@dataclass
class AnchorSnapshot:
    """A snapshot of the karma ledger state for anchoring."""

    merkle_root: str
    chain_head: str
    total_debt: float
    total_entries: int
    chain_valid: bool
    version: str
    timestamp: str

    def canonical_hash(self) -> str:
        """Compute a canonical SHA-256 hash of this snapshot."""
        payload = (
            f"{self.version}|{self.merkle_root}|{self.chain_head}|"
            f"{self.total_debt}|{self.total_entries}|{self.timestamp}"
        )
        return hashlib.sha256(payload.encode()).hexdigest()


@dataclass
class AnchorResult:
    """Result of an XRPL anchor submission."""

    success: bool
    tx_hash: str
    merkle_root: str
    network: str
    ledger_index: int
    fee_drops: str
    message: str


# ---------------------------------------------------------------------------
# Anchor history persistence
# ---------------------------------------------------------------------------

_ANCHOR_DIR: Path | None = None


def _get_anchor_dir() -> Path:
    """Get or create the anchor history directory."""
    global _ANCHOR_DIR
    if _ANCHOR_DIR is None:
        try:
            from whitemagic.config.paths import WM_ROOT
            _ANCHOR_DIR = WM_ROOT / "dharma" / "anchors"
        except Exception:
            _ANCHOR_DIR = Path.home() / ".whitemagic" / "dharma" / "anchors"
    _ANCHOR_DIR.mkdir(parents=True, exist_ok=True)
    return _ANCHOR_DIR


def _persist_anchor(snapshot: AnchorSnapshot, result: AnchorResult) -> None:
    """Append an anchor record to the local history."""
    try:
        anchor_dir = _get_anchor_dir()
        history_file = anchor_dir / "anchor_history.jsonl"
        record = {
            "snapshot": asdict(snapshot),
            "result": asdict(result),
            "recorded_at": datetime.now(timezone.utc).isoformat(),
        }
        with open(history_file, "a", encoding="utf-8") as f:
            f.write(json.dumps(record) + "\n")
    except Exception as e:
        logger.debug(f"Anchor history persist failed: {e}")


def get_anchor_history(limit: int = 20) -> list[dict[str, Any]]:
    """Read recent anchor history from disk."""
    try:
        anchor_dir = _get_anchor_dir()
        history_file = anchor_dir / "anchor_history.jsonl"
        if not history_file.exists():
            return []
        lines = history_file.read_text(encoding="utf-8").strip().split("\n")
        records = []
        for line in lines[-limit:]:
            if line.strip():
                try:
                    records.append(json.loads(line))
                except json.JSONDecodeError:
                    continue
        return records
    except Exception as e:
        logger.debug(f"Anchor history read failed: {e}")
        return []


# ---------------------------------------------------------------------------
# Core Functions
# ---------------------------------------------------------------------------


def compute_anchor() -> dict[str, Any]:
    """Snapshot the current karma ledger state for anchoring.

    Returns a dict with merkle_root, chain_head, total_debt, etc.
    This does NOT touch the network — it's a local read.
    """
    try:
        from whitemagic.dharma.karma_ledger import get_karma_ledger

        ledger = get_karma_ledger()
        report = ledger.report(limit=0)
        chain_status = ledger.verify_chain()
        merkle_root = ledger.merkle_root()

        snapshot = AnchorSnapshot(
            merkle_root=merkle_root,
            chain_head=chain_status.get("chain_head", "unknown"),
            total_debt=report.get("total_debt", 0.0),
            total_entries=report.get("total_calls_tracked", 0),
            chain_valid=chain_status.get("valid", False),
            version=ANCHOR_VERSION,
            timestamp=datetime.now(timezone.utc).isoformat(),
        )

        return {
            "status": "ok",
            **asdict(snapshot),
            "canonical_hash": snapshot.canonical_hash(),
            "xrpl_available": _XRPL_AVAILABLE,
        }
    except Exception as e:
        return {
            "status": "error",
            "reason": f"Failed to compute anchor snapshot: {e}",
        }


def submit_anchor(
    merkle_root: str | None = None,
    wallet_seed: str | None = None,
    network: str = "testnet",
) -> dict[str, Any]:
    """Submit a karma Merkle root to the XRP Ledger.

    Sends a 0-drop self-payment with the Merkle root in the Memo field.
    This creates an immutable, timestamped attestation on-chain.

    Args:
        merkle_root: The Merkle root to anchor. If None, computes fresh.
        wallet_seed: XRPL wallet seed (e.g. "sEdV..."). If None, uses
                     the seed from $WM_STATE_ROOT/dharma/xrpl_seed.txt
                     or returns an error.
        network: "testnet" or "mainnet".

    Returns:
        Dict with tx_hash, ledger_index, fee, etc.
    """
    if not _XRPL_AVAILABLE:
        return {
            "status": "error",
            "reason": "xrpl-py not installed. Run: pip install xrpl-py",
            "install_hint": "pip install 'whitemagic[trust]'",
        }

    # Compute fresh snapshot if no root provided
    if merkle_root is None:
        snapshot_data = compute_anchor()
        if snapshot_data.get("status") != "ok":
            return snapshot_data
        merkle_root = snapshot_data["merkle_root"]
        snapshot = AnchorSnapshot(**{
            k: snapshot_data[k]
            for k in AnchorSnapshot.__dataclass_fields__
        })
    else:
        snapshot = AnchorSnapshot(
            merkle_root=merkle_root,
            chain_head="provided_externally",
            total_debt=0.0,
            total_entries=0,
            chain_valid=True,
            version=ANCHOR_VERSION,
            timestamp=datetime.now(timezone.utc).isoformat(),
        )

    # Resolve wallet seed
    if wallet_seed is None:
        wallet_seed = _load_wallet_seed()
    if wallet_seed is None:
        return {
            "status": "error",
            "reason": "No XRPL wallet seed found.",
            "hint": (
                "Provide wallet_seed parameter, or save your testnet seed to "
                "$WM_STATE_ROOT/dharma/xrpl_seed.txt"
            ),
        }

    # Connect and submit
    url = XRPL_TESTNET_URL if network == "testnet" else XRPL_MAINNET_URL
    try:
        client = JsonRpcClient(url)
        wallet = Wallet.from_seed(wallet_seed)

        memo_data = json.dumps({
            "v": ANCHOR_VERSION,
            "root": merkle_root,
            "ts": snapshot.timestamp,
            "hash": snapshot.canonical_hash(),
        }).encode().hex()

        tx = Payment(
            account=wallet.address,
            destination=wallet.address,  # self-payment (0 XRP)
            amount="0",
            memos=[
                Memo(
                    memo_type=MEMO_TYPE_HEX,
                    memo_data=memo_data,
                ),
            ],
        )

        response = submit_and_wait(tx, client, wallet)
        result_meta = response.result.get("meta", {})
        tx_result = result_meta.get("TransactionResult", "unknown")

        if tx_result == "tesSUCCESS":
            anchor_result = AnchorResult(
                success=True,
                tx_hash=response.result.get("hash", ""),
                merkle_root=merkle_root,
                network=network,
                ledger_index=response.result.get("ledger_index", 0),
                fee_drops=response.result.get("Fee", "0"),
                message=f"Karma root anchored to XRPL {network}",
            )
            _persist_anchor(snapshot, anchor_result)
            return {"status": "ok", **asdict(anchor_result)}
        else:
            return {
                "status": "error",
                "reason": f"XRPL transaction failed: {tx_result}",
                "tx_result": tx_result,
                "response": str(response.result)[:500],
            }

    except Exception as e:
        return {
            "status": "error",
            "reason": f"XRPL submission failed: {e}",
            "network": network,
        }


def verify_anchor(
    tx_hash: str,
    expected_merkle_root: str | None = None,
    network: str = "testnet",
) -> dict[str, Any]:
    """Verify a karma anchor transaction on the XRP Ledger.

    Retrieves the transaction, extracts the Memo, and compares the
    stored Merkle root against the expected value (or the current root).

    Args:
        tx_hash: The XRPL transaction hash to verify.
        expected_merkle_root: Root to compare against. If None, computes
                              the current local Merkle root.
        network: "testnet" or "mainnet".

    Returns:
        Dict with verified (bool), on-chain root, local root, etc.
    """
    if not _XRPL_AVAILABLE:
        return {
            "status": "error",
            "verified": False,
            "reason": "xrpl-py not installed. Run: pip install xrpl-py",
        }

    # Get the expected root
    if expected_merkle_root is None:
        snapshot = compute_anchor()
        if snapshot.get("status") != "ok":
            return {"status": "error", "verified": False, "reason": "Cannot compute local Merkle root"}
        expected_merkle_root = snapshot["merkle_root"]

    url = XRPL_TESTNET_URL if network == "testnet" else XRPL_MAINNET_URL
    try:
        client = JsonRpcClient(url)
        response = client.request(Tx(transaction=tx_hash))

        if not response.is_successful():
            return {
                "status": "error",
                "verified": False,
                "reason": f"Transaction lookup failed: {response.result}",
            }

        tx_data = response.result
        memos = tx_data.get("Memos", [])
        if not memos:
            return {
                "status": "error",
                "verified": False,
                "reason": "Transaction has no memos",
            }

        # Find our memo
        anchor_memo = None
        for memo_wrapper in memos:
            memo = memo_wrapper.get("Memo", {})
            if memo.get("MemoType") == MEMO_TYPE_HEX:
                anchor_memo = memo
                break

        if anchor_memo is None:
            return {
                "status": "error",
                "verified": False,
                "reason": "No whitemagic/karma-anchor memo found in transaction",
            }

        # Decode memo data
        memo_data_hex = anchor_memo.get("MemoData", "")
        try:
            memo_json = json.loads(bytes.fromhex(memo_data_hex).decode())
        except (ValueError, json.JSONDecodeError) as e:
            return {
                "status": "error",
                "verified": False,
                "reason": f"Failed to decode memo: {e}",
            }

        on_chain_root = memo_json.get("root", "")
        on_chain_ts = memo_json.get("ts", "")
        on_chain_hash = memo_json.get("hash", "")

        verified = on_chain_root == expected_merkle_root

        return {
            "status": "ok",
            "verified": verified,
            "on_chain_root": on_chain_root,
            "local_root": expected_merkle_root,
            "match": verified,
            "on_chain_timestamp": on_chain_ts,
            "on_chain_canonical_hash": on_chain_hash,
            "tx_hash": tx_hash,
            "ledger_index": tx_data.get("ledger_index", 0),
            "network": network,
            "message": (
                "Karma root verified — on-chain matches local ledger"
                if verified
                else "Karma root MISMATCH — ledger may have changed since anchoring"
            ),
        }

    except Exception as e:
        return {
            "status": "error",
            "verified": False,
            "reason": f"Verification failed: {e}",
        }


# ---------------------------------------------------------------------------
# Wallet seed management
# ---------------------------------------------------------------------------


def _load_wallet_seed() -> str | None:
    """Load XRPL wallet seed from disk."""
    try:
        from whitemagic.config.paths import WM_ROOT
        seed_file = WM_ROOT / "dharma" / "xrpl_seed.txt"
    except Exception:
        seed_file = Path.home() / ".whitemagic" / "dharma" / "xrpl_seed.txt"

    if seed_file.exists():
        seed = seed_file.read_text(encoding="utf-8").strip()
        if seed:
            return seed
    return None


def save_wallet_seed(seed: str) -> dict[str, Any]:
    """Save an XRPL wallet seed to the secure local store.

    Args:
        seed: The XRPL wallet seed (e.g. "sEdV..." for testnet).

    Returns:
        Status dict.
    """
    try:
        from whitemagic.config.paths import WM_ROOT
        seed_dir = WM_ROOT / "dharma"
    except Exception:
        seed_dir = Path.home() / ".whitemagic" / "dharma"

    seed_dir.mkdir(parents=True, exist_ok=True)
    seed_file = seed_dir / "xrpl_seed.txt"
    seed_file.write_text(seed.strip(), encoding="utf-8")
    seed_file.chmod(0o600)  # Owner-only read/write

    return {
        "status": "ok",
        "message": f"Seed saved to {seed_file} (mode 0600)",
        "path": str(seed_file),
    }


def anchor_status() -> dict[str, Any]:
    """Get the current anchor system status.

    Returns availability, history count, latest anchor, etc.
    """
    history = get_anchor_history(limit=5)
    latest = history[-1] if history else None

    # Check for wallet
    seed = _load_wallet_seed()

    return {
        "xrpl_available": _XRPL_AVAILABLE,
        "xrpl_version": getattr(xrpl, "__version__", "unknown") if _XRPL_AVAILABLE else None,
        "wallet_configured": seed is not None,
        "total_anchors": len(get_anchor_history(limit=10000)),
        "latest_anchor": {
            "tx_hash": latest["result"]["tx_hash"],
            "merkle_root": latest["result"]["merkle_root"],
            "network": latest["result"]["network"],
            "timestamp": latest["recorded_at"],
        }
        if latest
        else None,
    }
