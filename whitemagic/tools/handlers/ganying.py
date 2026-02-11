"""Gan Ying event bus tool handlers."""
from typing import Any


def _emit(event_type: str, data: dict[str, Any]) -> None:
    from whitemagic.tools.unified_api import _emit_gan_ying
    _emit_gan_ying(event_type, data)


def handle_ganying_emit(**kwargs: Any) -> dict[str, Any]:
    event_type = kwargs.get("event_type", "CUSTOM")
    data = kwargs.get("data", {})
    payload = data if isinstance(data, dict) else {}
    _emit(event_type, payload)
    return {"status": "success", "event_emitted": event_type}


def handle_ganying_history(**kwargs: Any) -> dict[str, Any]:
    try:
        from whitemagic.core.resonance.gan_ying import get_bus
        bus = get_bus()
        limit = kwargs.get("limit", 50)
        events = bus.get_history(limit=limit)
        return {
            "status": "success",
            "count": len(events),
            "events": [
                {
                    "type": str(e.event_type.value) if hasattr(e.event_type, "value") else str(e.event_type),
                    "source": e.source,
                    "timestamp": str(e.timestamp),
                }
                for e in events
            ],
        }
    except Exception as e:
        return {"status": "error", "error": str(e)}


def handle_ganying_listeners(**kwargs: Any) -> dict[str, Any]:
    try:
        from whitemagic.core.resonance.gan_ying import get_bus
        bus = get_bus()
        listeners = {
            str(k.value) if hasattr(k, "value") else str(k): len(v)
            for k, v in getattr(bus, "_listeners", {}).items()
        }
        return {"status": "success", "listeners": listeners, "total": sum(listeners.values())}
    except Exception as e:
        return {"status": "error", "error": str(e)}


def handle_resonance_trace(**kwargs: Any) -> dict[str, Any]:
    try:
        from datetime import datetime, timedelta

        from whitemagic.core.resonance.gan_ying import get_bus
        bus = get_bus()
        duration = kwargs.get("duration", 5)
        events = bus.get_history(limit=100)
        cutoff = datetime.now() - timedelta(minutes=duration)
        recent = [e for e in events if hasattr(e, "timestamp") and e.timestamp > cutoff]
        return {
            "status": "success",
            "traced_events": len(recent),
            "duration_minutes": duration,
            "events": [
                {
                    "type": str(e.event_type.value) if hasattr(e.event_type, "value") else str(e.event_type),
                    "source": e.source,
                    "timestamp": str(e.timestamp),
                }
                for e in recent
            ],
        }
    except Exception as e:
        return {"status": "error", "error": str(e)}
