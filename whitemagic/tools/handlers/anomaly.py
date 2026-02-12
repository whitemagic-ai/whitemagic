"""MCP handlers for Harmony Vector Anomaly Detection."""

from typing import Any


def handle_anomaly(**kwargs: Any) -> dict[str, Any]:
    """Unified anomaly handler — routes by action parameter."""
    action = kwargs.get("action", "check")
    dispatch = {
        "check": handle_anomaly_check,
        "history": handle_anomaly_history,
        "status": handle_anomaly_status,
    }
    handler = dispatch.get(action)
    if not handler:
        return {"status": "error", "message": f"Unknown action '{action}'. Valid: {sorted(dispatch.keys())}"}
    return handler(**kwargs)


def handle_anomaly_check(**kwargs: Any) -> dict[str, Any]:
    """Check for active anomalies on Harmony Vector dimensions."""
    from whitemagic.harmony.anomaly_detector import get_anomaly_detector
    detector = get_anomaly_detector()
    alerts = detector.check()
    return {
        "status": "success",
        "active_alerts": alerts,
        "alert_count": len(alerts),
    }


def handle_anomaly_history(**kwargs: Any) -> dict[str, Any]:
    """Get recent anomaly alert history."""
    from whitemagic.harmony.anomaly_detector import get_anomaly_detector
    limit = int(kwargs.get("limit", 20))
    detector = get_anomaly_detector()
    return {
        "status": "success",
        "alerts": detector.recent_alerts(limit=limit),
    }


def handle_anomaly_status(**kwargs: Any) -> dict[str, Any]:
    """Get anomaly detector status and per-dimension statistics."""
    from whitemagic.harmony.anomaly_detector import get_anomaly_detector
    detector = get_anomaly_detector()
    return {"status": "success", **detector.status()}
