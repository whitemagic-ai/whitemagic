from datetime import datetime
from typing import Any


def _safe_parse_datetime(val: Any) -> datetime:
    """Robustly parse datetime from string or return if already datetime."""
    if isinstance(val, datetime):
        return val
    if isinstance(val, str):
        try:
            # Handle 'Z' suffix and other standard ISO formats
            return _safe_parse_datetime(val.replace("Z", "+00:00"))
        except (ValueError, TypeError):
            # Fallback if parsing fails
            return datetime.now()
    return datetime.now()

def format_datetime(dt: datetime | None = None) -> str:
    """Format datetime to ISO string."""
    if dt is None:
        dt = datetime.now()
    return dt.isoformat()
