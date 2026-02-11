"""WhiteMagic Logging Configuration

Structured logging setup for production and development environments.
Provides JSON logging for production and human-readable logs for development.
"""

import functools
import logging
import logging.config
import os
import sys
from collections.abc import Callable
from types import ModuleType
from typing import Any, ParamSpec, TypeVar

from whitemagic.config.paths import LOGS_DIR

_jsonlogger: ModuleType | None = None
_jsonlogger_imported: ModuleType | None = None
try:
    from pythonjsonlogger import json as _jsonlogger_json
    _jsonlogger_imported = _jsonlogger_json
except ImportError:
    try:
        from pythonjsonlogger import jsonlogger as _jsonlogger_legacy  # <3.0 fallback
        _jsonlogger_imported = _jsonlogger_legacy
    except ImportError:  # Optional dependency
        _jsonlogger_imported = None
if _jsonlogger_imported is not None:
    _jsonlogger = _jsonlogger_imported

jsonlogger: ModuleType | None = _jsonlogger

P = ParamSpec("P")
R = TypeVar("R")

class WhiteMagicLogger:
    """Centralized logging configuration for WhiteMagic"""

    def __init__(self) -> None:
        self.configured = False
        # Never write runtime logs into the repo by default. Use WM_STATE_ROOT.
        self.log_dir = LOGS_DIR
        self.log_dir.mkdir(parents=True, exist_ok=True)

    def setup_logging(
        self,
        level: str = "INFO",
        environment: str = "development",
        log_to_file: bool = True
    ) -> None:
        """Setup logging configuration"""

        if self.configured:
            return

        # Determine log level
        getattr(logging, level.upper(), logging.INFO)

        # Base configuration
        json_formatter: dict[str, Any] = {
            "format": "%(asctime)s %(name)s %(levelname)s %(message)s %(pathname)s %(lineno)d",
            "datefmt": "%Y-%m-%d %H:%M:%S",
        }
        if jsonlogger is not None:
            json_formatter = {
                "()": jsonlogger.JsonFormatter,
                **json_formatter,
            }

        config: dict[str, Any] = {
            "version": 1,
            "disable_existing_loggers": False,
            "formatters": {
                "json": json_formatter,
                "detailed": {
                    "format": "%(asctime)s [%(levelname)8s] %(name)s: %(message)s (%(filename)s:%(lineno)d)",
                    "datefmt": "%Y-%m-%d %H:%M:%S"
                },
                "simple": {
                    "format": "%(levelname)s - %(message)s"
                }
            },
            "handlers": {
                "console": {
                    "class": "logging.StreamHandler",
                    "level": level,
                    "formatter": "json" if environment == "production" else "detailed",
                    "stream": sys.stdout
                }
            },
            "loggers": {
                "": {  # Root logger
                    "level": level,
                    "handlers": ["console"],
                    "propagate": False
                },
                "whitemagic": {
                    "level": level,
                    "handlers": ["console"],
                    "propagate": False
                },
                "uvicorn": {
                    "level": "INFO",
                    "handlers": ["console"],
                    "propagate": False
                }
            }
        }

        # Add file handlers if enabled
        if log_to_file:
            # Main application log
            config["handlers"]["file"] = {
                "class": "logging.handlers.RotatingFileHandler",
                "level": level,
                "formatter": "json",
                "filename": self.log_dir / "whitemagic.log",
                "maxBytes": 10485760,  # 10MB
                "backupCount": 5,
                "encoding": "utf8"
            }

            # Error log
            config["handlers"]["error_file"] = {
                "class": "logging.handlers.RotatingFileHandler",
                "level": "ERROR",
                "formatter": "json",
                "filename": self.log_dir / "errors.log",
                "maxBytes": 10485760,  # 10MB
                "backupCount": 5,
                "encoding": "utf8"
            }

            # Add file handlers to loggers
            config["loggers"][""]["handlers"].extend(["file", "error_file"])
            config["loggers"]["whitemagic"]["handlers"].extend(["file", "error_file"])

        # Apply configuration
        logging.config.dictConfig(config)
        self.configured = True

        # Log initialization
        if not os.getenv("WM_SILENT_INIT"):
            logger = logging.getLogger(__name__)
            logger.info(
                "WhiteMagic logging initialized",
                extra={
                    "environment": environment,
                    "level": level,
                    "log_to_file": log_to_file,
                },
            )

    def get_logger(self, name: str) -> logging.Logger:
        """Get a logger instance"""
        if not self.configured:
            self.setup_logging()
        return logging.getLogger(name)

# Singleton instance
_logger_instance: WhiteMagicLogger | None = None

def get_logger(name: str | None = None) -> logging.Logger:
    """Get a logger instance"""
    global _logger_instance
    if _logger_instance is None:
        _logger_instance = WhiteMagicLogger()

    if name is not None:
        return _logger_instance.get_logger(name)
    return _logger_instance.get_logger("whitemagic")

def setup_logging(**kwargs: Any) -> None:
    """Setup logging configuration"""
    global _logger_instance
    if _logger_instance is None:
        _logger_instance = WhiteMagicLogger()
    _logger_instance.setup_logging(**kwargs)

# Convenience decorators
def log_function_call(
    logger: logging.Logger | None = None,
) -> Callable[[Callable[P, R]], Callable[P, R]]:
    """Decorator to log function calls"""
    def decorator(func: Callable[P, R]) -> Callable[P, R]:
        @functools.wraps(func)
        def wrapper(*args: P.args, **kwargs: P.kwargs) -> R:
            log = logger or get_logger(func.__module__)
            log.debug(f"Calling {func.__name__}", extra={"args": args, "kwargs": kwargs})
            try:
                result = func(*args, **kwargs)
                log.debug(f"Completed {func.__name__}")
                return result
            except Exception:
                log.error(f"Error in {func.__name__}", exc_info=True)
                raise
        return wrapper
    return decorator

def log_performance(
    logger: logging.Logger | None = None,
) -> Callable[[Callable[P, R]], Callable[P, R]]:
    """Decorator to log function performance"""
    import time

    def decorator(func: Callable[P, R]) -> Callable[P, R]:
        @functools.wraps(func)
        def wrapper(*args: P.args, **kwargs: P.kwargs) -> R:
            log = logger or get_logger(func.__module__)
            start_time = time.time()
            try:
                result = func(*args, **kwargs)
                duration = time.time() - start_time
                log.info(
                    f"Performance: {func.__name__}",
                    extra={
                        "function": func.__name__,
                        "duration_ms": duration * 1000,
                        "success": True
                    }
                )
                return result
            except Exception as e:
                duration = time.time() - start_time
                log.error(
                    f"Performance: {func.__name__}",
                    extra={
                        "function": func.__name__,
                        "duration_ms": duration * 1000,
                        "success": False,
                        "error": str(e)
                    }
                )
                raise
        return wrapper
    return decorator

# Context manager for logging operations
class LogContext:
    """Context manager for adding context to logs"""

    def __init__(self, logger: logging.Logger, **context: Any) -> None:
        self.logger = logger
        self.context: dict[str, Any] = context
        self.adapter: logging.LoggerAdapter[logging.Logger] | None = None

    def __enter__(self) -> logging.LoggerAdapter[logging.Logger]:
        self.adapter = logging.LoggerAdapter(self.logger, self.context)
        return self.adapter

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc_val: BaseException | None,
        exc_tb: Any,
    ) -> None:
        self.adapter = None

# Example usage:
if __name__ == "__main__":
    # Setup logging
    setup_logging(level="DEBUG", environment="development")

    # Get logger
    logger = get_logger(__name__)

    # Log messages
    logger.info("WhiteMagic starting up")
    logger.warning("This is a warning")
    logger.error("This is an error", extra={"context": "startup"})

    # Use decorators
    @log_function_call()
    def test_function(x: int) -> int:
        return x * 2

    @log_performance()
    def slow_function() -> str:
        import time
        time.sleep(0.1)
        return "done"

    # Test functions
    result = test_function(5)
    slow_function()

    # Use context
    with LogContext(logger, user_id="123", operation="test"):
        logger.info("Operation in context")
