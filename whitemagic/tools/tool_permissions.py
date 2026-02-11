"""Tool-Level Permission System — Per-Agent Tool Access Control.
=============================================================
Lightweight RBAC for the dispatch pipeline. Maps agent roles to allowed
tool categories and specific tool names. Does NOT require FastAPI.

Roles:
  - **observer**: Read-only introspection tools only
  - **agent**: Standard agent — most tools except admin/dangerous
  - **coordinator**: Can use multi-agent coordination tools
  - **admin**: Full access to everything

Plugs into dispatch at step 0.3 (after rate limiter, before maturity gate).

Usage:
    from whitemagic.tools.tool_permissions import check_tool_permission

    result = check_tool_permission("agent_123", "create_memory", roles=["agent"])
    if result is not None:
        return result  # blocked — insufficient permissions
"""

import logging
import threading
from typing import Any

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Role → Allowed ToolCategory mappings
# ---------------------------------------------------------------------------

# Categories from registry.py ToolCategory enum
_ROLE_ALLOWED_CATEGORIES: dict[str, set[str]] = {
    "observer": {
        "introspection", "memory",  # read-only memory search
    },
    "agent": {
        "introspection", "memory", "session", "garden", "balance",
        "dharma", "intelligence", "archaeology", "inference", "synthesis",
        "system", "utility", "watcher", "metrics", "edge", "governor",
        "browser",
    },
    "coordinator": set(),  # Empty = all categories allowed (like admin)
    "admin": set(),        # Empty = all categories allowed
}

# Specific tool overrides: tools that require specific roles regardless of category
_TOOL_ROLE_REQUIREMENTS: dict[str, str] = {
    # Coordination tools require coordinator role
    "broker.publish": "coordinator",
    "task.distribute": "coordinator",
    "vote.create": "coordinator",
    "agent.register": "coordinator",
    "pipeline.create": "coordinator",
    "dharma.reload": "coordinator",
    "memory.retention_sweep": "coordinator",
    # Dangerous operations require admin
    "set_dharma_profile": "admin",
    "governor_set_goal": "admin",
}

# Tools always allowed regardless of role (basic discovery)
_ALWAYS_ALLOWED: set[str] = {
    "capabilities", "manifest", "gnosis", "maturity.assess",
    "starter_packs.list", "starter_packs.get", "starter_packs.suggest",
}


# ---------------------------------------------------------------------------
# Agent role registry (in-memory, can be backed by persistent store)
# ---------------------------------------------------------------------------

class AgentRoleRegistry:
    """Tracks which roles each agent has."""

    def __init__(self) -> None:
        self._agent_roles: dict[str, list[str]] = {}
        self._lock = threading.Lock()
        # Default role for unregistered agents — coordinator gives full access
        # Use set_roles() to restrict specific agents to observer/agent roles
        self._default_roles = ["coordinator"]

    def set_roles(self, agent_id: str, roles: list[str]) -> None:
        with self._lock:
            self._agent_roles[agent_id] = roles

    def get_roles(self, agent_id: str) -> list[str]:
        with self._lock:
            return self._agent_roles.get(agent_id, list(self._default_roles))

    def set_default_roles(self, roles: list[str]) -> None:
        with self._lock:
            self._default_roles = roles

    def list_agents(self) -> dict[str, list[str]]:
        with self._lock:
            return dict(self._agent_roles)


# ---------------------------------------------------------------------------
# Permission checking
# ---------------------------------------------------------------------------

_registry_instance: AgentRoleRegistry | None = None
_registry_lock = threading.Lock()


def get_agent_role_registry() -> AgentRoleRegistry:
    global _registry_instance
    with _registry_lock:
        if _registry_instance is None:
            _registry_instance = AgentRoleRegistry()
        return _registry_instance


def check_tool_permission(
    agent_id: str,
    tool_name: str,
    roles: list[str] | None = None,
) -> dict[str, Any] | None:
    """Check if an agent has permission to use a tool.

    Returns None if allowed, or an error dict if blocked.
    """
    # Always-allowed tools
    if tool_name in _ALWAYS_ALLOWED:
        return None

    # Get roles
    if roles is None:
        roles = get_agent_role_registry().get_roles(agent_id)

    # Admin bypasses everything
    if "admin" in roles:
        return None

    # Check specific tool role requirements
    if tool_name in _TOOL_ROLE_REQUIREMENTS:
        required_role = _TOOL_ROLE_REQUIREMENTS[tool_name]
        if required_role == "admin" and "admin" not in roles:
            return _blocked(tool_name, roles, "Requires admin role")
        if required_role == "coordinator" and "coordinator" not in roles:
            return _blocked(tool_name, roles, "Requires coordinator role")

    # Check category-based permissions
    tool_category = _get_tool_category(tool_name)
    if tool_category:
        for role in roles:
            allowed_cats = _ROLE_ALLOWED_CATEGORIES.get(role, set())
            if not allowed_cats:  # Empty = admin (all allowed)
                return None
            if tool_category in allowed_cats:
                return None
        return _blocked(tool_name, roles, f"Category '{tool_category}' not allowed for roles {roles}")

    # Unknown tool — allow through (will fail at dispatch lookup anyway)
    return None


def _blocked(tool_name: str, roles: list[str], reason: str) -> dict[str, Any]:
    logger.info(f"Permission denied: tool={tool_name} roles={roles} reason={reason}")
    return {
        "status": "error",
        "error": f"Permission denied: {reason}",
        "error_code": "permission_denied",
        "tool": tool_name,
        "your_roles": roles,
    }


def _get_tool_category(tool_name: str) -> str | None:
    """Look up tool category from registry (cached)."""
    try:
        from whitemagic.tools.registry import TOOL_REGISTRY
        for tool_def in TOOL_REGISTRY:
            if tool_def.name == tool_name:
                category_value = getattr(tool_def.category, "value", None)
                return str(category_value) if category_value is not None else None
    except Exception:
        pass
    return None
