"""Configuration initialization module for WhiteMagic.

This module provides easy access to the configuration system
and handles initialization.

Path definitions are centralized in paths.py:
- PROJECT_ROOT: code/repo location (for scripts, archive)
- WM_ROOT: user state location (configurable, defaults to ~/.whitemagic)
- MEMORY_DIR, DATA_DIR, CACHE_DIR: subdirs of WM_ROOT
"""
import logging

from whitemagic.config.concurrency import (
    ASYNC_TASK_LIMIT,
    CPU_WORKERS,
    IO_WORKERS,
    MAX_WORKERS,
    get_concurrency_config,
    get_max_workers,
)
from whitemagic.config.manager import (
    ConfigManager,
    DatabaseConfig,
    Environment,
    SecurityConfig,
    WhiteMagicConfig,
    get_config,
    get_config_manager,
    setup_config_environment,
)

# Import path configuration from centralized module
from whitemagic.config.paths import (
    ARCHIVE_DIR,
    ARTIFACTS_DIR,
    CACHE_DIR,
    DATA_DIR,
    DB_PATH,
    LOGS_DIR,
    MEMORY_DIR,
    PROJECT_ROOT,
    SCRIPTS_DIR,
    SESSIONS_DIR,
    WM_ROOT,
)

logger = logging.getLogger(__name__)

# Import version from canonical source (VERSION file)
_version_file = PROJECT_ROOT / "VERSION"
if _version_file.exists():
    VERSION = _version_file.read_text().strip()
else:
    # Last-resort import in editable installs
    try:
        from whitemagic import __version__ as VERSION  # type: ignore
    except Exception:
        VERSION = "unknown"


def show_config() -> None:
    """Print a safe, minimal configuration summary for diagnostics."""
    try:
        current = get_config()
    except Exception:
        current = config

    env_value = getattr(current, "environment", "unknown")
    debug_value = getattr(current, "debug", "unknown")

    logger.info("Configuration summary:")
    logger.info(f"  environment: {env_value}")
    logger.info(f"  debug: {debug_value}")


# Initialize configuration on import
try:
    config = get_config()
    setup_config_environment(config)
except Exception as e:
    import warnings

    warnings.warn(f"Failed to initialize configuration: {e}")
    # Use minimal default config
    config = WhiteMagicConfig(
        environment=Environment.DEVELOPMENT,
        debug=True,
        database=DatabaseConfig(
            url=f"sqlite:///{DB_PATH}",
            pool_size=5,
            max_overflow=10,
            echo=False,
        ),
        security=SecurityConfig(
            secret_key="dev-secret-key",
            csrf_enabled=True,
            content_security_policy=True,
            ssl_required=False,
            session_timeout=3600,
        ),
        data_dir=str(DATA_DIR),
        log_dir=str(LOGS_DIR),
        temp_dir=str(CACHE_DIR / "tmp"),
    )

__all__ = [
    "config",
    "get_config",
    "get_config_manager",
    "setup_config_environment",
    "WhiteMagicConfig",
    "ConfigManager",
    "Environment",
    # Path configuration (from paths.py)
    "PROJECT_ROOT",
    "SCRIPTS_DIR",
    "ARCHIVE_DIR",
    "WM_ROOT",
    "MEMORY_DIR",
    "DATA_DIR",
    "CACHE_DIR",
    "SESSIONS_DIR",
    "LOGS_DIR",
    "ARTIFACTS_DIR",
    "DB_PATH",
    "VERSION",
    "show_config",
    # Concurrency configuration
    "get_max_workers",
    "get_concurrency_config",
    "MAX_WORKERS",
    "CPU_WORKERS",
    "IO_WORKERS",
    "ASYNC_TASK_LIMIT",
]
