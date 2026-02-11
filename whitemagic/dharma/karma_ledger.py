"""Karma Ledger — Declared vs Actual Side-Effect Tracking
========================================================
Inspired by MandalaOS's Karma/Effect system.

Every tool in Whitemagic declares a ``safety`` level (READ, WRITE, DELETE).
The Karma Ledger closes the loop by comparing what a tool *declared* against
what it *actually did* (as reported in the response envelope's ``writes``
and ``side_effects`` fields).

Mismatches accrue **karma debt**:
  - A READ tool that secretly writes → debt += 1.0  (deceptive)
  - A WRITE tool that reports no writes → debt += 0.2 (wasteful, not harmful)
  - A DELETE tool that reports no writes → debt += 0.1 (no-op, minor)

Karma debt feeds into the Harmony Vector's ``karma_debt`` dimension and can
trigger Dharma Governor warnings when it exceeds a configurable threshold.

The ledger is persisted as a JSONL file under ``$WM_STATE_ROOT/dharma/``.

Usage:
    from whitemagic.dharma.karma_ledger import get_karma_ledger
    ledger = get_karma_ledger()
    ledger.record(tool="create_memory", declared="WRITE", actual_writes=1, success=True)
    report = ledger.report()
"""

from __future__ import annotations

import hashlib
import json
import logging
import threading
from collections import defaultdict
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)


def _hash_entry(data: str, prev_hash: str) -> str:
    """Compute SHA-256 hash for a ledger entry."""
    return hashlib.sha256(f"{prev_hash}|{data}".encode()).hexdigest()[:16]


def _merkle_tree_root(hashes: list[str]) -> str:
    """Compute Merkle tree root from a list of hex hash strings (Leap 9c)."""
    if not hashes:
        return hashlib.sha256(b"empty_karma").hexdigest()
    if len(hashes) == 1:
        return hashes[0]
    # Pad to even length
    if len(hashes) % 2 != 0:
        hashes = list(hashes) + [hashes[-1]]
    next_level: list[str] = []
    for i in range(0, len(hashes), 2):
        combined = hashes[i] + hashes[i + 1]
        next_level.append(hashlib.sha256(combined.encode()).hexdigest())
    return _merkle_tree_root(next_level)


@dataclass
class KarmaEntry:
    """A single karma ledger entry with Merkle hash chain."""

    tool: str
    declared_safety: str       # READ, WRITE, DELETE
    actual_writes: int         # count of writes in the response
    success: bool
    mismatch: bool
    debt_delta: float          # karma debt change from this entry
    timestamp: str
    prev_hash: str = ""        # hash of previous entry (Merkle chain)
    entry_hash: str = ""       # hash of this entry
    ops_class: str = ""        # Edgerunner Violet: "red-ops", "blue-ops", or "" (normal)

    def to_dict(self) -> dict[str, Any]:
        d = {
            "tool": self.tool,
            "declared_safety": self.declared_safety,
            "actual_writes": self.actual_writes,
            "success": self.success,
            "mismatch": self.mismatch,
            "debt_delta": self.debt_delta,
            "timestamp": self.timestamp,
            "prev_hash": self.prev_hash,
            "entry_hash": self.entry_hash,
        }
        if self.ops_class:
            d["ops_class"] = self.ops_class
        return d


class KarmaLedger:
    """Persistent ledger tracking declared vs actual side-effects.

    Feeds karma debt into the Harmony Vector for system-wide health scoring.
    """

    def __init__(self, storage_dir: Path | None = None):
        self._lock = threading.Lock()
        self._storage_dir = storage_dir
        self._entries: list[KarmaEntry] = []
        self._total_debt: float = 0.0
        self._tool_debt: dict[str, float] = defaultdict(float)
        self._tool_calls: dict[str, int] = defaultdict(int)
        self._tool_mismatches: dict[str, int] = defaultdict(int)
        self._chain_head: str = "genesis"  # Merkle chain head

        if self._storage_dir:
            self._storage_dir.mkdir(parents=True, exist_ok=True)
            self._load_recent()

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def record(
        self,
        tool: str,
        declared_safety: str,
        actual_writes: int,
        success: bool,
        ops_class: str = "",
    ) -> KarmaEntry:
        """Record a tool invocation and compute karma debt delta.

        Args:
            ops_class: Edgerunner Violet classification — "red-ops", "blue-ops",
                       or "" for normal operations.  Enables dual-log transparency.
        """
        declared = declared_safety.upper()
        mismatch = False
        debt_delta = 0.0

        if declared == "READ" and actual_writes > 0:
            # Deceptive: declared read-only but wrote data
            mismatch = True
            debt_delta = 1.0
        elif declared in ("WRITE", "DELETE") and actual_writes == 0 and success:
            # Wasteful: declared mutation but did nothing
            mismatch = True
            debt_delta = 0.2 if declared == "WRITE" else 0.1
        # Honest behavior: no debt

        ts = datetime.now().isoformat()
        prev_hash = self._chain_head
        payload = f"{tool}:{declared}:{actual_writes}:{success}:{mismatch}:{debt_delta}:{ts}"
        entry_hash = _hash_entry(payload, prev_hash)

        entry = KarmaEntry(
            tool=tool,
            declared_safety=declared,
            actual_writes=actual_writes,
            success=success,
            mismatch=mismatch,
            debt_delta=debt_delta,
            timestamp=ts,
            prev_hash=prev_hash,
            entry_hash=entry_hash,
            ops_class=ops_class,
        )

        with self._lock:
            self._chain_head = entry_hash
            self._entries.append(entry)
            self._total_debt += debt_delta
            self._tool_debt[tool] += debt_delta
            self._tool_calls[tool] += 1
            if mismatch:
                self._tool_mismatches[tool] += 1

            # Keep in-memory list bounded
            if len(self._entries) > 10000:
                self._entries = self._entries[-5000:]

        # Persist
        self._persist(entry)

        # Feed the Harmony Vector
        if debt_delta > 0:
            try:
                from whitemagic.harmony.vector import get_harmony_vector
                get_harmony_vector()  # debt is tracked inside the vector via record_call
            except Exception:
                pass

        return entry

    def report(self, limit: int = 100) -> dict[str, Any]:
        """Generate a karma report."""
        with self._lock:
            recent = self._entries[-limit:]
            total_calls = sum(self._tool_calls.values())
            total_mismatches = sum(self._tool_mismatches.values())

            # Top offenders
            offenders = sorted(
                self._tool_debt.items(), key=lambda x: x[1], reverse=True,
            )[:10]

            return {
                "total_debt": round(self._total_debt, 2),
                "total_calls_tracked": total_calls,
                "total_mismatches": total_mismatches,
                "mismatch_rate": round(total_mismatches / max(total_calls, 1), 4),
                "top_offenders": [
                    {
                        "tool": t,
                        "debt": round(d, 2),
                        "calls": self._tool_calls.get(t, 0),
                        "mismatches": self._tool_mismatches.get(t, 0),
                    }
                    for t, d in offenders if d > 0
                ],
                "recent_entries": [e.to_dict() for e in recent[-20:]],
            }

    def get_debt(self) -> float:
        """Return current total karma debt."""
        with self._lock:
            return self._total_debt

    def verify_chain(self) -> dict[str, Any]:
        """Verify the Merkle hash chain integrity."""
        with self._lock:
            if not self._entries:
                return {"valid": True, "entries_checked": 0, "message": "Empty ledger"}

            broken_at = None
            prev = "genesis"
            checked = 0
            for entry in self._entries:
                # If entry has hash data, verify it
                if entry.prev_hash and entry.entry_hash:
                    if entry.prev_hash != prev:
                        broken_at = checked
                        break
                    payload = (
                        f"{entry.tool}:{entry.declared_safety}:{entry.actual_writes}:"
                        f"{entry.success}:{entry.mismatch}:{entry.debt_delta}:{entry.timestamp}"
                    )
                    expected = _hash_entry(payload, entry.prev_hash)
                    if expected != entry.entry_hash:
                        broken_at = checked
                        break
                    prev = entry.entry_hash
                checked += 1

            if broken_at is not None:
                return {
                    "valid": False,
                    "entries_checked": checked,
                    "broken_at_index": broken_at,
                    "message": f"Chain integrity broken at entry {broken_at}",
                }
            return {
                "valid": True,
                "entries_checked": checked,
                "chain_head": self._chain_head,
                "message": "Chain integrity verified",
            }

    def merkle_root(self) -> str:
        """Compute Merkle tree root over all ledger entry hashes (Leap 9c).

        This provides a single tamper-evident fingerprint of the entire
        karma history. If any entry is altered, the root changes.
        """
        with self._lock:
            hashes = [e.entry_hash for e in self._entries if e.entry_hash]
        return _merkle_tree_root(hashes)

    def report_by_ops(self, ops_class: str, limit: int = 100) -> dict[str, Any]:
        """Edgerunner Violet dual-log: filter entries by ops classification.

        Args:
            ops_class: "red-ops", "blue-ops", or "" for unclassified.
        """
        with self._lock:
            filtered = [e for e in self._entries if e.ops_class == ops_class][-limit:]
            total_debt = sum(e.debt_delta for e in filtered)
            mismatches = sum(1 for e in filtered if e.mismatch)
            return {
                "ops_class": ops_class or "unclassified",
                "total_entries": len(filtered),
                "total_debt": round(total_debt, 2),
                "mismatches": mismatches,
                "entries": [e.to_dict() for e in filtered[-20:]],
            }

    def forgive(self, amount: float = 1.0) -> float:
        """Reduce karma debt (e.g., after corrective action). Returns new total."""
        with self._lock:
            self._total_debt = max(0.0, self._total_debt - amount)
            return self._total_debt

    # ------------------------------------------------------------------
    # Persistence
    # ------------------------------------------------------------------

    def _persist(self, entry: KarmaEntry) -> None:
        if not self._storage_dir:
            return
        try:
            ledger_file = self._storage_dir / "karma_ledger.jsonl"
            with open(ledger_file, "a", encoding="utf-8") as f:
                f.write(json.dumps(entry.to_dict()) + "\n")
        except Exception as e:
            logger.debug(f"Karma ledger persist failed: {e}")

    def _load_recent(self) -> None:
        """Load recent entries from disk on startup."""
        if not self._storage_dir:
            return
        ledger_file = self._storage_dir / "karma_ledger.jsonl"
        if not ledger_file.exists():
            return
        try:
            lines = ledger_file.read_text(encoding="utf-8").strip().split("\n")
            # Only load last 1000 entries
            for line in lines[-1000:]:
                if not line.strip():
                    continue
                try:
                    data = json.loads(line)
                    entry = KarmaEntry(**data)
                    self._entries.append(entry)
                    self._total_debt += entry.debt_delta
                    self._tool_debt[entry.tool] += entry.debt_delta
                    self._tool_calls[entry.tool] += 1
                    if entry.mismatch:
                        self._tool_mismatches[entry.tool] += 1
                except (json.JSONDecodeError, TypeError):
                    continue
            logger.info(f"Karma ledger: loaded {len(self._entries)} entries, debt={self._total_debt:.1f}")
        except Exception as e:
            logger.debug(f"Karma ledger load failed: {e}")


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_ledger: KarmaLedger | None = None
_ledger_lock = threading.Lock()


def get_karma_ledger() -> KarmaLedger:
    """Get the global Karma Ledger instance."""
    global _ledger
    if _ledger is None:
        with _ledger_lock:
            if _ledger is None:
                try:
                    from whitemagic.config.paths import WM_ROOT
                    storage = WM_ROOT / "dharma"
                except Exception:
                    storage = None
                _ledger = KarmaLedger(storage_dir=storage)
    return _ledger
