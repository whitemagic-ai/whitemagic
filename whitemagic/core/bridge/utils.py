
import json
import logging
from typing import Any

logger = logging.getLogger(__name__)

def ensure_string(value: Any, **kwargs: Any) -> str | None:
    """Ensure value is a string. If it's a dict/list (JSON), dump it to string.
    This fixes issues with models like GLM 4.7 passing JSON objects for string fields.
    """
    if value is None:
        return None
    if isinstance(value, (dict, list)):
        try:
            return json.dumps(value)
        except (TypeError, ValueError):
            return str(value)
    return str(value)

def get_system_time(**kwargs: Any) -> dict[str, Any]:
    """Get system time and temporal context."""
    from whitemagic.core.temporal import get_system_time as _get_time
    return _get_time()

def _emit_resonance_event(event_type: str, data: dict[str, Any], source: str = "mcp_api_bridge", **kwargs: Any) -> None:
    try:
        from whitemagic.core.resonance import EventType, emit_event
        if event_type in EventType.__members__:
            emit_event(EventType[event_type], data)
        else:
            emit_event(EventType.SYSTEM_STATE_CHANGE, {"event_type": event_type, **data})
    except Exception:
        pass
