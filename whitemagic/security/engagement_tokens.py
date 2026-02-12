"""Scope-of-Engagement Tokens — Edgerunner Violet Security Layer
================================================================
Cryptographic certificates that authorize time-bounded, scope-limited
offensive security actions.  No destructive red-ops action can proceed
without a valid, unexpired engagement token.

Each token specifies:
  - **scope**: what targets/tools are authorized (glob patterns)
  - **expires_at**: UTC expiry timestamp
  - **issuer**: who authorized the engagement
  - **token_hash**: HMAC-SHA256 of the token fields for tamper detection

Inspired by: Edgerunner Violet's "auth-bound offensive modules" concept,
requiring cryptographically signed scope-of-engagement tokens for any
destructive action.

Usage:
    from whitemagic.security.engagement_tokens import get_token_manager
    mgr = get_token_manager()
    token = mgr.issue(scope=["10.0.0.*"], tools=["nmap_*", "fuzz_*"],
                       issuer="lucas", duration_minutes=60)
    result = mgr.validate(token["token_id"], tool="nmap_scan", target="10.0.0.1")
"""

from __future__ import annotations

import hashlib
import hmac
import json
import logging
import secrets
import threading
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)

# Secret key for HMAC — generated once per process, or loaded from disk
_HMAC_KEY: bytes | None = None
_HMAC_LOCK = threading.Lock()


def _get_hmac_key() -> bytes:
    """Get or generate the HMAC signing key."""
    global _HMAC_KEY
    if _HMAC_KEY is not None:
        return _HMAC_KEY
    with _HMAC_LOCK:
        if _HMAC_KEY is not None:
            return _HMAC_KEY
        try:
            from whitemagic.config.paths import WM_ROOT
            key_path = WM_ROOT / "security" / ".engagement_key"
            key_path.parent.mkdir(parents=True, exist_ok=True)
            if key_path.exists():
                _HMAC_KEY = key_path.read_bytes()
            else:
                _HMAC_KEY = secrets.token_bytes(32)
                key_path.write_bytes(_HMAC_KEY)
                key_path.chmod(0o600)
        except Exception:
            _HMAC_KEY = secrets.token_bytes(32)
    return _HMAC_KEY


@dataclass
class EngagementToken:
    """A time-bounded, scope-limited authorization for offensive actions."""

    token_id: str
    scope: list[str]           # Target patterns (IP ranges, domains, glob)
    tools: list[str]           # Authorized tool patterns (fnmatch)
    issuer: str                # Who authorized
    issued_at: float
    expires_at: float
    token_hash: str            # HMAC-SHA256 for tamper detection
    revoked: bool = False
    uses: int = 0
    max_uses: int = 0          # 0 = unlimited

    def is_expired(self) -> bool:
        return time.time() > self.expires_at

    def is_valid(self) -> bool:
        return not self.revoked and not self.is_expired() and (self.max_uses == 0 or self.uses < self.max_uses)

    def remaining_seconds(self) -> float:
        return max(0.0, self.expires_at - time.time())

    def to_dict(self) -> dict[str, Any]:
        return {
            "token_id": self.token_id,
            "scope": self.scope,
            "tools": self.tools,
            "issuer": self.issuer,
            "issued_at": self.issued_at,
            "expires_at": self.expires_at,
            "remaining_s": round(self.remaining_seconds(), 1),
            "valid": self.is_valid(),
            "revoked": self.revoked,
            "uses": self.uses,
            "max_uses": self.max_uses,
        }


def _compute_token_hash(token_id: str, scope: list[str], tools: list[str],
                         issuer: str, expires_at: float) -> str:
    """Compute HMAC-SHA256 of token fields for tamper detection."""
    payload = json.dumps({
        "id": token_id, "scope": sorted(scope), "tools": sorted(tools),
        "issuer": issuer, "expires_at": expires_at,
    }, sort_keys=True, separators=(",", ":"))
    return hmac.new(_get_hmac_key(), payload.encode(), hashlib.sha256).hexdigest()


def _verify_token_hash(token: EngagementToken) -> bool:
    """Verify the HMAC of a token to detect tampering."""
    expected = _compute_token_hash(
        token.token_id, token.scope, token.tools,
        token.issuer, token.expires_at,
    )
    return hmac.compare_digest(expected, token.token_hash)


class EngagementTokenManager:
    """Manages scope-of-engagement tokens for Edgerunner Violet operations."""

    def __init__(self, storage_dir: Path | None = None):
        self._lock = threading.Lock()
        self._tokens: dict[str, EngagementToken] = {}
        self._storage_dir = storage_dir
        self._audit_log: list[dict[str, Any]] = []

        if self._storage_dir:
            self._storage_dir.mkdir(parents=True, exist_ok=True)
            self._load_tokens()

    def issue(
        self,
        scope: list[str],
        tools: list[str],
        issuer: str,
        duration_minutes: int = 60,
        max_uses: int = 0,
    ) -> dict[str, Any]:
        """Issue a new engagement token.

        Args:
            scope: Target patterns (e.g., ["10.0.0.*", "*.internal.corp"]).
            tools: Tool name patterns (fnmatch, e.g., ["nmap_*", "fuzz_*"]).
            issuer: Identity of the person/system authorizing the engagement.
            duration_minutes: How long the token is valid (default 60 min).
            max_uses: Maximum number of uses (0 = unlimited).
        """
        token_id = f"evt_{secrets.token_hex(12)}"
        now = time.time()
        expires_at = now + (duration_minutes * 60)

        token_hash = _compute_token_hash(token_id, scope, tools, issuer, expires_at)

        token = EngagementToken(
            token_id=token_id,
            scope=scope,
            tools=tools,
            issuer=issuer,
            issued_at=now,
            expires_at=expires_at,
            token_hash=token_hash,
            max_uses=max_uses,
        )

        with self._lock:
            self._tokens[token_id] = token

        self._persist()
        self._audit("issue", token_id, issuer=issuer, scope=scope, tools=tools)

        logger.info(
            "Engagement token issued: %s by %s (scope=%s, tools=%s, expires=%dm)",
            token_id, issuer, scope, tools, duration_minutes,
        )
        return {"status": "success", "token": token.to_dict()}

    def validate(
        self,
        token_id: str,
        tool: str = "",
        target: str = "",
    ) -> dict[str, Any]:
        """Validate a token for a specific tool/target combination.

        Args:
            token_id: The engagement token ID.
            tool: Tool being invoked (checked against token's tool patterns).
            target: Target being accessed (checked against token's scope patterns).
        """
        import fnmatch

        with self._lock:
            token = self._tokens.get(token_id)

        if token is None:
            self._audit("validate_fail", token_id, reason="not_found")
            return {
                "status": "success",
                "valid": False,
                "reason": f"Token '{token_id}' not found.",
            }

        if not _verify_token_hash(token):
            self._audit("validate_fail", token_id, reason="tampered")
            return {
                "status": "success",
                "valid": False,
                "reason": f"Token '{token_id}' failed HMAC verification — possible tampering.",
            }

        if token.revoked:
            self._audit("validate_fail", token_id, reason="revoked")
            return {
                "status": "success",
                "valid": False,
                "reason": f"Token '{token_id}' has been revoked.",
            }

        if token.is_expired():
            self._audit("validate_fail", token_id, reason="expired")
            return {
                "status": "success",
                "valid": False,
                "reason": f"Token '{token_id}' expired {-token.remaining_seconds():.0f}s ago.",
            }

        if token.max_uses > 0 and token.uses >= token.max_uses:
            self._audit("validate_fail", token_id, reason="max_uses_exceeded")
            return {
                "status": "success",
                "valid": False,
                "reason": f"Token '{token_id}' has reached its max uses ({token.max_uses}).",
            }

        # Check tool authorization
        if tool and token.tools:
            tool_match = any(fnmatch.fnmatch(tool, pat) for pat in token.tools)
            if not tool_match:
                self._audit("validate_fail", token_id, reason="tool_not_authorized", tool=tool)
                return {
                    "status": "success",
                    "valid": False,
                    "reason": f"Tool '{tool}' is not authorized by token '{token_id}'. "
                             f"Authorized patterns: {token.tools}",
                }

        # Check target scope
        if target and token.scope:
            scope_match = any(fnmatch.fnmatch(target, pat) for pat in token.scope)
            if not scope_match:
                self._audit("validate_fail", token_id, reason="target_out_of_scope", target=target)
                return {
                    "status": "success",
                    "valid": False,
                    "reason": f"Target '{target}' is outside the engagement scope. "
                             f"Authorized scope: {token.scope}",
                }

        # Valid — increment uses
        with self._lock:
            token.uses += 1

        self._audit("validate_pass", token_id, tool=tool, target=target)
        return {
            "status": "success",
            "valid": True,
            "reason": "Token valid.",
            "token": token.to_dict(),
        }

    def revoke(self, token_id: str) -> dict[str, Any]:
        """Revoke an engagement token immediately."""
        with self._lock:
            token = self._tokens.get(token_id)
            if token is None:
                return {"status": "error", "error": f"Token '{token_id}' not found."}
            token.revoked = True

        self._persist()
        self._audit("revoke", token_id)
        logger.info("Engagement token revoked: %s", token_id)
        return {"status": "success", "message": f"Token '{token_id}' revoked.", "token": token.to_dict()}

    def list_tokens(self, include_expired: bool = False) -> dict[str, Any]:
        """List all engagement tokens."""
        with self._lock:
            tokens = list(self._tokens.values())

        if not include_expired:
            tokens = [t for t in tokens if not t.is_expired()]

        return {
            "status": "success",
            "count": len(tokens),
            "tokens": [t.to_dict() for t in tokens],
        }

    def status(self) -> dict[str, Any]:
        """Return engagement token subsystem status."""
        with self._lock:
            active = sum(1 for t in self._tokens.values() if t.is_valid())
            expired = sum(1 for t in self._tokens.values() if t.is_expired())
            revoked = sum(1 for t in self._tokens.values() if t.revoked)
        return {
            "total_tokens": len(self._tokens),
            "active": active,
            "expired": expired,
            "revoked": revoked,
            "recent_audit": self._audit_log[-10:],
        }

    def _audit(self, action: str, token_id: str, **extra: Any) -> None:
        entry = {"action": action, "token_id": token_id, "timestamp": time.time(), **extra}
        with self._lock:
            self._audit_log.append(entry)
            if len(self._audit_log) > 5000:
                self._audit_log = self._audit_log[-2500:]

    def _persist(self) -> None:
        if not self._storage_dir:
            return
        try:
            path = self._storage_dir / "engagement_tokens.json"
            with self._lock:
                data = {
                    tid: {
                        "scope": t.scope, "tools": t.tools, "issuer": t.issuer,
                        "issued_at": t.issued_at, "expires_at": t.expires_at,
                        "token_hash": t.token_hash, "revoked": t.revoked,
                        "uses": t.uses, "max_uses": t.max_uses,
                    }
                    for tid, t in self._tokens.items()
                }
            path.write_text(json.dumps(data, indent=2), encoding="utf-8")
        except Exception as e:
            logger.debug("Engagement token persist failed: %s", e)

    def _load_tokens(self) -> None:
        if not self._storage_dir:
            return
        path = self._storage_dir / "engagement_tokens.json"
        if not path.exists():
            return
        try:
            data = json.loads(path.read_text(encoding="utf-8"))
            for tid, td in data.items():
                self._tokens[tid] = EngagementToken(
                    token_id=tid,
                    scope=td.get("scope", []),
                    tools=td.get("tools", []),
                    issuer=td.get("issuer", ""),
                    issued_at=td.get("issued_at", 0.0),
                    expires_at=td.get("expires_at", 0.0),
                    token_hash=td.get("token_hash", ""),
                    revoked=td.get("revoked", False),
                    uses=td.get("uses", 0),
                    max_uses=td.get("max_uses", 0),
                )
            logger.info("Engagement tokens: loaded %d tokens", len(self._tokens))
        except Exception as e:
            logger.debug("Engagement token load failed: %s", e)


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_manager: EngagementTokenManager | None = None
_manager_lock = threading.Lock()


def get_token_manager() -> EngagementTokenManager:
    """Get the global Engagement Token Manager instance."""
    global _manager
    if _manager is None:
        with _manager_lock:
            if _manager is None:
                try:
                    from whitemagic.config.paths import WM_ROOT
                    storage = WM_ROOT / "security"
                except Exception:
                    storage = None
                _manager = EngagementTokenManager(storage_dir=storage)
    return _manager
