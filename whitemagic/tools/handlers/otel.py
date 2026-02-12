"""MCP handlers for OpenTelemetry introspection."""

from typing import Any


def handle_otel(**kwargs: Any) -> dict[str, Any]:
    """Unified OpenTelemetry handler — routes by action parameter."""
    action = kwargs.get("action", "metrics")
    dispatch = {
        "spans": handle_otel_spans,
        "metrics": handle_otel_metrics,
        "status": handle_otel_status,
    }
    handler = dispatch.get(action)
    if not handler:
        return {"status": "error", "message": f"Unknown action '{action}'. Valid: {sorted(dispatch.keys())}"}
    return handler(**kwargs)


def handle_otel_spans(**kwargs: Any) -> dict[str, Any]:
    """Get recent OTEL span records."""
    from whitemagic.core.monitoring.otel_export import get_otel

    limit = int(kwargs.get("limit", 20))
    otel = get_otel()
    return {"status": "success", "spans": otel.recent_spans(limit=limit)}


def handle_otel_metrics(**kwargs: Any) -> dict[str, Any]:
    """Get aggregated OTEL metrics summary."""
    from whitemagic.core.monitoring.otel_export import get_otel

    otel = get_otel()
    return {"status": "success", **otel.metrics_summary()}


def handle_otel_status(**kwargs: Any) -> dict[str, Any]:
    """Get OTEL exporter status."""
    from whitemagic.core.monitoring.otel_export import get_otel

    otel = get_otel()
    return {"status": "success", **otel.status()}
