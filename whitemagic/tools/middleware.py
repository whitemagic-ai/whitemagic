"""Dispatch Middleware — Composable Pipeline for Tool Invocation.
==============================================================
Each middleware is a function::

    (ctx: DispatchContext, next_fn: NextFn) -> Optional[Dict[str, Any]]

Middlewares can:
  - **Short-circuit**: return a result without calling ``next_fn()``
  - **Pass through**: call ``next_fn(ctx)`` to continue the chain
  - **Post-process**: call ``result = next_fn(ctx)``, modify result, return it

The pipeline is built declaratively::

    pipeline = DispatchPipeline()
    pipeline.use("sanitizer", mw_input_sanitizer)
    pipeline.use("breaker",   mw_circuit_breaker)
    pipeline.use("router",    mw_core_router)
    result = pipeline.execute("gnosis", compact=True)

The ``DispatchContext`` carries mutable state through the chain so
middlewares can share data (e.g. the circuit breaker instance for
post-processing feedback).
"""

from __future__ import annotations

import logging
from collections.abc import Callable
from dataclasses import dataclass, field
from typing import Any

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Types
# ---------------------------------------------------------------------------

NextFn = Callable[["DispatchContext"], dict[str, Any] | None]
MiddlewareFn = Callable[["DispatchContext", NextFn], dict[str, Any] | None]


# ---------------------------------------------------------------------------
# Context
# ---------------------------------------------------------------------------

@dataclass
class DispatchContext:
    """Mutable context that flows through the middleware chain."""

    tool_name: str
    kwargs: dict[str, Any]
    agent_id: str = "default"
    compact: bool = False
    # When True, Zig dispatch already validated circuit breaker, rate limit,
    # and maturity — Python middleware can skip those redundant checks.
    zig_prevalidated: bool = False
    # Stash for cross-middleware communication (e.g. circuit breaker ref)
    meta: dict[str, Any] = field(default_factory=dict)


# ---------------------------------------------------------------------------
# Pipeline
# ---------------------------------------------------------------------------

class DispatchPipeline:
    """Composable middleware chain for tool dispatch.

    Middlewares execute in registration order.  Each can short-circuit
    by returning a result, or call ``next_fn(ctx)`` to continue.
    """

    def __init__(self) -> None:
        self._middlewares: list[tuple[str, MiddlewareFn]] = []

    def use(self, name: str, middleware: MiddlewareFn) -> "DispatchPipeline":
        """Register a middleware.  Order matters — first registered runs first."""
        self._middlewares.append((name, middleware))
        return self

    def execute(self, tool_name: str, **kwargs: Any) -> dict[str, Any] | None:
        """Execute the full pipeline for a tool call."""
        ctx = DispatchContext(
            tool_name=tool_name,
            kwargs=kwargs,
            agent_id=kwargs.pop("_agent_id", "default"),
            compact=kwargs.pop("_compact", False),
            zig_prevalidated=bool(kwargs.pop("_zig_prevalidated", False)),
        )

        # Build the chain from innermost → outermost
        chain: NextFn = _terminal
        for name, mw in reversed(self._middlewares):
            chain = _wrap(mw, chain, name)

        result = chain(ctx)

        # Post-pipeline: compact response mode
        if ctx.compact and isinstance(result, dict):
            try:
                from whitemagic.tools.compact_response import compact
                result = compact(result)
            except Exception:
                pass

        return result

    def describe(self) -> list[str]:
        """Return middleware names in registration order (for introspection)."""
        return [name for name, _ in self._middlewares]


def _terminal(ctx: DispatchContext) -> dict[str, Any] | None:
    """End of chain — no handler found."""
    return {
        "status": "error",
        "message": f"Tool {ctx.tool_name} not yet implemented in unified_api or bridge",
    }


def _wrap(mw: MiddlewareFn, next_fn: NextFn, name: str) -> NextFn:
    """Wrap a middleware + next into a single NextFn with safety net."""
    def wrapped(ctx: DispatchContext) -> dict[str, Any] | None:
        try:
            return mw(ctx, next_fn)
        except Exception as e:
            logger.debug(f"Middleware '{name}' error: {e}")
            return next_fn(ctx)
    return wrapped


# =========================================================================
# Built-in middlewares
# =========================================================================

def mw_input_sanitizer(ctx: DispatchContext, next_fn: NextFn) -> dict[str, Any] | None:
    """Validate tool arguments before any processing."""
    try:
        from whitemagic.tools.input_sanitizer import sanitize_tool_args
        result = sanitize_tool_args(ctx.tool_name, ctx.kwargs)
        if result is not None:
            return result
    except Exception:
        pass
    return next_fn(ctx)


def mw_circuit_breaker(ctx: DispatchContext, next_fn: NextFn) -> dict[str, Any] | None:
    """Fast-fail if tool is in cooldown; record success/failure afterward."""
    breaker = None
    try:
        from whitemagic.tools.circuit_breaker import get_breaker_registry
        breaker = get_breaker_registry().get(ctx.tool_name)
        # Skip pre-check if Zig dispatch already validated circuit state
        if not ctx.zig_prevalidated and breaker.is_open():
            return breaker.calm_response()
    except Exception:
        breaker = None

    result = next_fn(ctx)

    # Post-processing: breaker feedback
    if breaker is not None and isinstance(result, dict):
        try:
            status_val = str(result.get("status", "")).lower()
            if status_val in ("success", "ok"):
                breaker.record_success()
            elif status_val == "error":
                breaker.record_failure()
        except Exception:
            pass

    return result


def mw_rate_limiter(ctx: DispatchContext, next_fn: NextFn) -> dict[str, Any] | None:
    """Per-agent, per-tool rate limiting."""
    if ctx.zig_prevalidated:
        return next_fn(ctx)  # Zig already checked rate limit
    try:
        from whitemagic.tools.rate_limiter import get_rate_limiter
        rate_result = get_rate_limiter().check(ctx.agent_id, ctx.tool_name)
        if rate_result is not None:
            return rate_result
    except Exception:
        pass
    return next_fn(ctx)


def mw_tool_permissions(ctx: DispatchContext, next_fn: NextFn) -> dict[str, Any] | None:
    """Per-agent RBAC permission check."""
    try:
        from whitemagic.tools.tool_permissions import check_tool_permission
        perm_result = check_tool_permission(ctx.agent_id, ctx.tool_name)
        if perm_result is not None:
            return perm_result
    except Exception:
        pass
    return next_fn(ctx)


def mw_maturity_gate(ctx: DispatchContext, next_fn: NextFn) -> dict[str, Any] | None:
    """Block tools that require a higher maturity stage than currently reached."""
    if ctx.zig_prevalidated:
        return next_fn(ctx)  # Zig already checked maturity
    try:
        from whitemagic.tools.maturity_check import check_maturity_for_tool
        gate_result = check_maturity_for_tool(ctx.tool_name)
        if gate_result is not None:
            return gate_result
    except Exception:
        pass
    return next_fn(ctx)


def mw_security_monitor(ctx: DispatchContext, next_fn: NextFn) -> dict[str, Any] | None:
    """Edgerunner Violet: anomaly detection for suspicious tool-call patterns."""
    try:
        from whitemagic.security.security_breaker import get_security_monitor
        safety = ctx.kwargs.get("safety", "READ")
        if not isinstance(safety, str):
            safety = "READ"
        alert = get_security_monitor().record_call(
            tool=ctx.tool_name,
            safety=safety,
            agent_id=ctx.agent_id,
        )
        if alert and alert.get("action") == "block":
            return {
                "status": "error",
                "error_code": "security_breaker",
                "message": f"Security monitor blocked: {alert.get('detail', 'anomaly detected')}",
                "alert": alert,
            }
    except Exception:
        pass
    return next_fn(ctx)


def mw_governor(ctx: DispatchContext, next_fn: NextFn) -> dict[str, Any] | None:
    """Ethical gate — Governor validates the tool call."""
    try:
        from whitemagic.core.governor import get_governor
        gov = get_governor()
        validation = gov.validate_tool_call(ctx.tool_name, ctx.kwargs)
        if not validation.safe:
            try:
                from whitemagic.tools.unified_api import _emit_gan_ying
                _emit_gan_ying("GOVERNOR_BLOCKED", {
                    "tool": ctx.tool_name, "reason": validation.reason,
                })
            except Exception:
                pass
            return {
                "status": "error",
                "error": f"Governor Blocked: {validation.reason}",
                "risk_level": validation.risk_level.name,
            }
    except ImportError:
        pass
    return next_fn(ctx)
