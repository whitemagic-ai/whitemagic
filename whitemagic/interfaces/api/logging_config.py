"""WhiteMagic API - Logging Configuration.

Structured JSON logging for production observability.
"""

import json
import logging
import sys
from datetime import datetime, timezone
from typing import Any


class JSONFormatter(logging.Formatter):
    """Format logs as JSON for easy parsing and analysis."""

    def format(self, record: logging.LogRecord) -> str:
        """Format log record as JSON."""
        log_data: dict[str, Any] = {
            "timestamp": datetime.now(timezone.utc).isoformat() + "Z",
            "level": record.levelname,
            "logger": record.name,
            "message": record.getMessage(),
            "module": record.module,
            "function": record.funcName,
            "line": record.lineno,
        }

        # Add exception info if present
        if record.exc_info:
            log_data["exception"] = self.formatException(record.exc_info)

        # Add extra fields from record
        if hasattr(record, "user_id"):
            log_data["user_id"] = record.user_id
        if hasattr(record, "request_id"):
            log_data["request_id"] = record.request_id
        if hasattr(record, "api_key_id"):
            log_data["api_key_id"] = record.api_key_id
        if hasattr(record, "endpoint"):
            log_data["endpoint"] = record.endpoint
        if hasattr(record, "method"):
            log_data["method"] = record.method
        if hasattr(record, "status_code"):
            log_data["status_code"] = record.status_code
        if hasattr(record, "duration_ms"):
            log_data["duration_ms"] = record.duration_ms

        return json.dumps(log_data)


def setup_logging(log_level: str = "INFO") -> None:
    """Setup structured JSON logging.

    Args:
        log_level: Logging level (DEBUG, INFO, WARNING, ERROR, CRITICAL)

    """
    # Get root logger
    logger = logging.getLogger()
    logger.setLevel(getattr(logging, log_level.upper()))

    # Remove existing handlers
    for handler in logger.handlers[:]:
        logger.removeHandler(handler)

    # Create console handler with JSON formatter
    handler = logging.StreamHandler(sys.stdout)
    handler.setFormatter(JSONFormatter())
    logger.addHandler(handler)

    # Set log level for uvicorn
    logging.getLogger("uvicorn").setLevel(logging.WARNING)
    logging.getLogger("uvicorn.access").setLevel(logging.WARNING)
    logging.getLogger("uvicorn.error").setLevel(logging.INFO)


# Application logger
logger = logging.getLogger("whitemagic.interfaces.api")


def log_request(
    request_id: str,
    method: str,
    endpoint: str,
    user_id: str | None = None,
    status_code: int | None = None,
    duration_ms: int | None = None,
) -> None:
    """Log API request with structured data.

    Args:
        request_id: Unique request identifier
        method: HTTP method
        endpoint: API endpoint
        user_id: User ID if authenticated
        status_code: HTTP status code
        duration_ms: Request duration in milliseconds

    """
    extra: dict[str, Any] = {
        "request_id": request_id,
        "method": method,
        "endpoint": endpoint,
    }

    if user_id:
        extra["user_id"] = user_id
    if status_code is not None:
        extra["status_code"] = status_code
    if duration_ms is not None:
        extra["duration_ms"] = duration_ms

    logger.info(f"{method} {endpoint} {status_code or ''}", extra=extra)


def log_error(
    message: str,
    error: Exception | None = None,
    request_id: str | None = None,
    user_id: str | None = None,
) -> None:
    """Log error with structured data.

    Args:
        message: Error message
        error: Exception object
        request_id: Request ID if applicable
        user_id: User ID if applicable

    """
    extra: dict[str, Any] = {}
    if request_id:
        extra["request_id"] = request_id
    if user_id:
        extra["user_id"] = user_id

    if error:
        logger.error(message, exc_info=(type(error), error, error.__traceback__), extra=extra)
    else:
        logger.error(message, extra=extra)


def log_security_event(
    event_type: str,
    message: str,
    user_id: str | None = None,
    api_key_id: str | None = None,
    request_id: str | None = None,
) -> None:
    """Log security-related events.

    Args:
        event_type: Type of security event (auth_failed, key_revoked, etc.)
        message: Event description
        user_id: User ID if applicable
        api_key_id: API key ID if applicable
        request_id: Request ID if applicable

    """
    extra: dict[str, Any] = {"event_type": event_type}
    if user_id:
        extra["user_id"] = user_id
    if api_key_id:
        extra["api_key_id"] = api_key_id
    if request_id:
        extra["request_id"] = request_id

    logger.warning(f"SECURITY: {message}", extra=extra)
