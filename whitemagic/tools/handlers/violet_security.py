"""Edgerunner Violet Security tool handlers.

Provides MCP tool handlers for:
  - MCP integrity checking (snapshot, verify, status)
  - Model signing (register, verify, list, hash, status)
  - Engagement tokens (issue, validate, revoke, list, status)
  - Security monitor (alerts, status)
"""
from typing import Any


# ---------------------------------------------------------------------------
# MCP Integrity
# ---------------------------------------------------------------------------

def handle_mcp_integrity_snapshot(**kwargs: Any) -> dict[str, Any]:
    """Capture current tool registry as the integrity baseline."""
    from whitemagic.security.mcp_integrity import get_mcp_integrity
    return get_mcp_integrity().snapshot()


def handle_mcp_integrity_verify(**kwargs: Any) -> dict[str, Any]:
    """Verify current registry against the baseline snapshot."""
    from whitemagic.security.mcp_integrity import get_mcp_integrity
    return get_mcp_integrity().verify()


def handle_mcp_integrity_status(**kwargs: Any) -> dict[str, Any]:
    """Return MCP integrity subsystem status."""
    from whitemagic.security.mcp_integrity import get_mcp_integrity
    return {"status": "success", **get_mcp_integrity().status()}


# ---------------------------------------------------------------------------
# Model Signing
# ---------------------------------------------------------------------------

def handle_model_register(**kwargs: Any) -> dict[str, Any]:
    """Register or update a model manifest."""
    from whitemagic.security.model_signing import get_model_registry
    model_name = kwargs.get("model_name", "")
    if not model_name:
        return {"status": "error", "error": "model_name is required"}
    sha256 = kwargs.get("sha256", "")
    if not sha256:
        return {"status": "error", "error": "sha256 is required"}
    return get_model_registry().register_model(
        model_name=model_name,
        sha256=sha256,
        trust=kwargs.get("trust", "unsigned"),
        signer=kwargs.get("signer", ""),
        license=kwargs.get("license", ""),
        training_disclosure=kwargs.get("training_disclosure", ""),
        safety_profile=kwargs.get("safety_profile", ""),
    )


def handle_model_verify(**kwargs: Any) -> dict[str, Any]:
    """Verify a model against its registered manifest."""
    from whitemagic.security.model_signing import get_model_registry
    model_name = kwargs.get("model_name", "")
    if not model_name:
        return {"status": "error", "error": "model_name is required"}
    return get_model_registry().verify_model(
        model_name=model_name,
        current_sha256=kwargs.get("current_sha256", ""),
    )


def handle_model_list(**kwargs: Any) -> dict[str, Any]:
    """List all registered model manifests."""
    from whitemagic.security.model_signing import get_model_registry
    return get_model_registry().list_models()


def handle_model_hash(**kwargs: Any) -> dict[str, Any]:
    """Compute SHA-256 hash of a model file on disk."""
    from whitemagic.security.model_signing import get_model_registry
    path = kwargs.get("path", "")
    if not path:
        return {"status": "error", "error": "path is required"}
    return get_model_registry().hash_file(path)


def handle_model_signing_status(**kwargs: Any) -> dict[str, Any]:
    """Return model signing subsystem status."""
    from whitemagic.security.model_signing import get_model_registry
    return {"status": "success", **get_model_registry().status()}


# ---------------------------------------------------------------------------
# Engagement Tokens
# ---------------------------------------------------------------------------

def handle_engagement_issue(**kwargs: Any) -> dict[str, Any]:
    """Issue a new scope-of-engagement token."""
    from whitemagic.security.engagement_tokens import get_token_manager
    scope = kwargs.get("scope", [])
    tools = kwargs.get("tools", [])
    issuer = kwargs.get("issuer", "")
    if not issuer:
        return {"status": "error", "error": "issuer is required"}
    if not scope and not tools:
        return {"status": "error", "error": "At least one of scope or tools is required"}
    return get_token_manager().issue(
        scope=scope,
        tools=tools,
        issuer=issuer,
        duration_minutes=int(kwargs.get("duration_minutes", 60)),
        max_uses=int(kwargs.get("max_uses", 0)),
    )


def handle_engagement_validate(**kwargs: Any) -> dict[str, Any]:
    """Validate an engagement token for a tool/target combination."""
    from whitemagic.security.engagement_tokens import get_token_manager
    token_id = kwargs.get("token_id", "")
    if not token_id:
        return {"status": "error", "error": "token_id is required"}
    return get_token_manager().validate(
        token_id=token_id,
        tool=kwargs.get("tool", ""),
        target=kwargs.get("target", ""),
    )


def handle_engagement_revoke(**kwargs: Any) -> dict[str, Any]:
    """Revoke an engagement token."""
    from whitemagic.security.engagement_tokens import get_token_manager
    token_id = kwargs.get("token_id", "")
    if not token_id:
        return {"status": "error", "error": "token_id is required"}
    return get_token_manager().revoke(token_id)


def handle_engagement_list(**kwargs: Any) -> dict[str, Any]:
    """List all engagement tokens."""
    from whitemagic.security.engagement_tokens import get_token_manager
    return get_token_manager().list_tokens(
        include_expired=bool(kwargs.get("include_expired", False)),
    )


def handle_engagement_status(**kwargs: Any) -> dict[str, Any]:
    """Return engagement token subsystem status."""
    from whitemagic.security.engagement_tokens import get_token_manager
    return {"status": "success", **get_token_manager().status()}


# ---------------------------------------------------------------------------
# Security Monitor
# ---------------------------------------------------------------------------

def handle_security_alerts(**kwargs: Any) -> dict[str, Any]:
    """Return recent security alerts from the anomaly monitor."""
    from whitemagic.security.security_breaker import get_security_monitor
    limit = int(kwargs.get("limit", 50))
    alerts = get_security_monitor().get_alerts(limit=limit)
    return {"status": "success", "count": len(alerts), "alerts": alerts}


def handle_security_monitor_status(**kwargs: Any) -> dict[str, Any]:
    """Return security monitor status."""
    from whitemagic.security.security_breaker import get_security_monitor
    return {"status": "success", **get_security_monitor().status()}
