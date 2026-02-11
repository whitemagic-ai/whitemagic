"""WhiteMagic Configuration Management System

Centralized configuration system with environment-specific settings.
Supports multiple environments (development, staging, production) and
configuration sources (files, environment variables, CLI arguments).
"""

import json
import logging
import os
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Any

from pydantic import BaseModel, ConfigDict, Field, field_validator

logger = logging.getLogger(__name__)


class Environment(str, Enum):
    """Supported environments."""

    DEVELOPMENT = "development"
    TESTING = "testing"
    STAGING = "staging"
    PRODUCTION = "production"


@dataclass
class ConfigSource:
    """Configuration source with priority."""

    name: str
    priority: int
    data: dict[str, Any]


class DatabaseConfig(BaseModel):
    """Database configuration."""

    url: str = Field(..., description="Database URL")
    pool_size: int = Field(5, description="Connection pool size")
    max_overflow: int = Field(10, description="Max overflow connections")
    echo: bool = Field(False, description="Echo SQL statements")

    @field_validator("url")
    @classmethod
    def validate_url(cls, v: str) -> str:
        if not v:
            raise ValueError("Database URL is required")
        return v


class RedisConfig(BaseModel):
    """Redis configuration."""

    url: str = Field("redis://localhost:6379", description="Redis URL")
    decode_responses: bool = Field(True, description="Decode responses")
    socket_timeout: int = Field(5, description="Socket timeout")
    connection_pool_max_connections: int = Field(50, description="Max connections")


class LoggingConfig(BaseModel):
    """Logging configuration."""

    level: str = Field("INFO", description="Log level")
    format: str = Field("json", description="Log format (json or text)")
    file_path: str | None = Field(None, description="Log file path")
    max_file_size: str = Field("10MB", description="Max log file size")
    backup_count: int = Field(5, description="Number of backup files")


class APIConfig(BaseModel):
    """API configuration."""

    host: str = Field("0.0.0.0", description="API host")
    port: int = Field(8000, description="API port")
    workers: int = Field(1, description="Number of workers")
    reload: bool = Field(False, description="Auto-reload on changes")
    cors_origins: list[str] = Field(default_factory=list, description="CORS origins")
    rate_limit_enabled: bool = Field(True, description="Enable rate limiting")
    rate_limit_requests: int = Field(100, description="Rate limit requests per minute")
    auth_enabled: bool = Field(False, description="Enable authentication")
    jwt_secret_key: str | None = Field(None, description="JWT secret key")
    jwt_algorithm: str = Field("HS256", description="JWT algorithm")


class SecurityConfig(BaseModel):
    """Security configuration."""

    secret_key: str = Field(..., description="Secret key for encryption")
    csrf_enabled: bool = Field(True, description="Enable CSRF protection")
    content_security_policy: bool = Field(True, description="Enable CSP headers")
    ssl_required: bool = Field(False, description="Require SSL")
    session_timeout: int = Field(3600, description="Session timeout seconds")


class CacheConfig(BaseModel):
    """Cache configuration."""

    backend: str = Field("redis", description="Cache backend (redis, memory)")
    ttl: int = Field(300, description="Default TTL seconds")
    key_prefix: str = Field("whitemagic:", description="Cache key prefix")


class MonitoringConfig(BaseModel):
    """Monitoring configuration."""

    metrics_enabled: bool = Field(True, description="Enable metrics")
    metrics_port: int = Field(9090, description="Metrics port")
    health_check_enabled: bool = Field(True, description="Enable health checks")
    tracing_enabled: bool = Field(False, description="Enable distributed tracing")


class WhiteMagicConfig(BaseModel):
    """Main WhiteMagic configuration."""

    # Environment
    environment: Environment = Field(Environment.DEVELOPMENT, description="Environment")
    debug: bool = Field(False, description="Debug mode")

    # Sub-configurations
    database: DatabaseConfig
    redis: RedisConfig = Field(
        default_factory=lambda: RedisConfig(
            url="redis://localhost:6379",
            decode_responses=True,
            socket_timeout=5,
            connection_pool_max_connections=50,
        ),
    )
    logging: LoggingConfig = Field(
        default_factory=lambda: LoggingConfig(
            level="INFO",
            format="json",
            file_path=None,
            max_file_size="10MB",
            backup_count=5,
        ),
    )
    api: APIConfig = Field(
        default_factory=lambda: APIConfig(
            host="0.0.0.0",
            port=8000,
            workers=1,
            reload=False,
            cors_origins=[],
            rate_limit_enabled=True,
            rate_limit_requests=100,
            auth_enabled=False,
            jwt_secret_key=None,
            jwt_algorithm="HS256",
        ),
    )
    security: SecurityConfig
    cache: CacheConfig = Field(
        default_factory=lambda: CacheConfig(
            backend="redis", ttl=300, key_prefix="whitemagic:",
        ),
    )
    monitoring: MonitoringConfig = Field(
        default_factory=lambda: MonitoringConfig(
            metrics_enabled=True,
            metrics_port=9090,
            health_check_enabled=True,
            tracing_enabled=False,
        ),
    )

    # Paths
    data_dir: str = Field("./data", description="Data directory")
    log_dir: str = Field("./logs", description="Log directory")
    temp_dir: str = Field("./tmp", description="Temporary directory")

    # Feature flags
    features: dict[str, bool] = Field(default_factory=dict, description="Feature flags")

    model_config = ConfigDict()

    @field_validator("security")
    @classmethod
    def validate_security(
        cls, v: dict[str, Any] | SecurityConfig,
    ) -> dict[str, Any] | SecurityConfig:
        # Generate secret key if not provided in development
        if isinstance(v, dict):
            if not v.get("secret_key"):
                # Note: In V2, validation happens before model creation
                # Secret key generation moved to __init__ or model_validator
                import secrets

                v["secret_key"] = secrets.token_urlsafe(32)
        return v


class ConfigManager:
    """Configuration manager with multiple sources support."""

    def __init__(self, config_dir: Path | None = None) -> None:
        from whitemagic.config.paths import WM_ROOT

        # Default to WM_STATE_ROOT; avoid writing config into the repo.
        self.config_dir = config_dir or WM_ROOT
        self.environment = Environment(os.getenv("WHITEMAGIC_ENV", "development"))
        self._config: WhiteMagicConfig | None = None
        self._sources: list[ConfigSource] = []

    def load_config(self) -> WhiteMagicConfig:
        """Load configuration from all sources."""
        if self._config:
            return self._config

        # Load configuration sources in priority order
        self._load_default_config()
        self._load_file_configs()
        self._load_env_vars()

        # Merge configurations
        merged = self._merge_configs()

        # Validate and create config object
        self._config = WhiteMagicConfig(**merged)

        # Create directories
        self._create_directories()

        return self._config

    def _load_default_config(self) -> None:
        """Load default configuration."""
        from whitemagic.config.paths import DB_PATH

        default_config = {
            "environment": self.environment.value,
            "debug": self.environment == Environment.DEVELOPMENT,
            "database": {
                "url": f"sqlite:///{DB_PATH}"
                if self.environment == Environment.DEVELOPMENT
                else "postgresql://user:pass@localhost/whitemagic",
                "echo": self.environment == Environment.DEVELOPMENT,
            },
            "security": {
                "secret_key": os.getenv("WHITEMAGIC_SECRET_KEY", ""),
                "csrf_enabled": self.environment != Environment.DEVELOPMENT,
                "ssl_required": self.environment == Environment.PRODUCTION,
            },
            "api": {
                "reload": self.environment == Environment.DEVELOPMENT,
                "cors_origins": ["*"]
                if self.environment == Environment.DEVELOPMENT
                else [],
                "auth_enabled": self.environment != Environment.DEVELOPMENT,
            },
            "logging": {
                "level": "DEBUG"
                if self.environment == Environment.DEVELOPMENT
                else "INFO",
                "format": "text"
                if self.environment == Environment.DEVELOPMENT
                else "json",
            },
        }

        self._sources.append(ConfigSource("defaults", 0, default_config))

    def _load_file_configs(self) -> None:
        """Load configuration from files."""
        # Load base config
        base_config = self._load_config_file("base.yaml")
        if base_config:
            self._sources.append(ConfigSource("base.yaml", 10, base_config))

        # Load environment-specific config
        env_file = f"{self.environment.value}.yaml"
        env_config = self._load_config_file(env_file)
        if env_config:
            self._sources.append(ConfigSource(env_file, 20, env_config))

        # Load local overrides
        local_config = self._load_config_file("local.yaml")
        if local_config:
            self._sources.append(ConfigSource("local.yaml", 30, local_config))

    def _load_config_file(self, filename: str) -> dict[str, Any] | None:
        """Load a single configuration file."""
        file_path = self.config_dir / filename

        if not file_path.exists():
            return None

        try:
            with open(file_path) as f:
                content = f.read()
                if filename.endswith(".yaml") or filename.endswith(".yml"):
                    # Simple YAML parser for basic config
                    import yaml

                    result = yaml.safe_load(content)
                    return result if isinstance(result, dict) else None
                if filename.endswith(".json"):
                    result = json.loads(content)
                    return result if isinstance(result, dict) else None
        except ImportError:
            # Fallback if yaml is not installed
            if filename.endswith(".yaml") or filename.endswith(".yml"):
                logger.info(f"Warning: PyYAML not installed, skipping {filename}")
                return None
        except Exception as e:
            logger.info(f"Error loading config file {filename}: {e}")
            return None

        return None  # Ensure we always return a value

    def _load_env_vars(self) -> None:
        """Load configuration from environment variables."""
        env_config: dict[str, Any] = {}

        # Map environment variables to config keys
        env_mappings = {
            "WHITEMAGIC_SECRET_KEY": ("security", "secret_key"),
            "WHITEMAGIC_DATABASE_URL": ("database", "url"),
            "WHITEMAGIC_REDIS_URL": ("redis", "url"),
            "WHITEMAGIC_API_HOST": ("api", "host"),
            "WHITEMAGIC_API_PORT": ("api", "port"),
            "WHITEMAGIC_LOG_LEVEL": ("logging", "level"),
            "WHITEMAGIC_DEBUG": ("debug",),
            "WHITEMAGIC_DATA_DIR": ("data_dir",),
        }

        for env_var, config_path in env_mappings.items():
            raw_value = os.getenv(env_var)
            if raw_value is not None:
                # Navigate to the nested location
                current = env_config
                for key in config_path[:-1]:
                    if key not in current:
                        current[key] = {}
                    current = current[key]

                # Convert value type if needed
                final_key = config_path[-1]
                parsed_value: Any
                if final_key == "port":
                    parsed_value = int(raw_value)
                elif final_key == "debug":
                    parsed_value = raw_value.lower() in ("true", "1", "yes")
                elif final_key in ["file_path"] and raw_value == "null":
                    parsed_value = None
                else:
                    # Keep as string for other fields
                    parsed_value = str(raw_value)

                current[final_key] = parsed_value

        if env_config:
            self._sources.append(ConfigSource("environment", 40, env_config))

    def _merge_configs(self) -> dict[str, Any]:
        """Merge all configuration sources."""
        # Sort by priority (lower first)
        sorted_sources = sorted(self._sources, key=lambda x: x.priority)

        merged: dict[str, Any] = {}

        for source in sorted_sources:
            merged = self._deep_merge(merged, source.data)

        return merged

    def _deep_merge(
        self, base: dict[str, Any], update: dict[str, Any],
    ) -> dict[str, Any]:
        """Deep merge two dictionaries."""
        result = base.copy()

        for key, value in update.items():
            if (
                key in result
                and isinstance(result[key], dict)
                and isinstance(value, dict)
            ):
                result[key] = self._deep_merge(result[key], value)
            else:
                result[key] = value

        return result

    def _create_directories(self) -> None:
        """Create necessary directories."""
        if not self._config:
            return

        directories = [
            self._config.data_dir,
            self._config.log_dir,
            self._config.temp_dir,
        ]

        for directory in directories:
            Path(directory).mkdir(parents=True, exist_ok=True)

    def get_config(self) -> WhiteMagicConfig:
        """Get the loaded configuration."""
        if not self._config:
            return self.load_config()
        return self._config

    def reload(self) -> WhiteMagicConfig:
        """Reload configuration."""
        self._config = None
        self._sources = []
        return self.load_config()


# Global config manager instance
_config_manager: ConfigManager | None = None


def get_config_manager(config_dir: Path | None = None) -> ConfigManager:
    """Get the global configuration manager."""
    global _config_manager
    if _config_manager is None:
        _config_manager = ConfigManager(config_dir)
    return _config_manager


def get_config() -> WhiteMagicConfig:
    """Get the current configuration."""
    return get_config_manager().get_config()


def setup_config_environment(config: WhiteMagicConfig) -> None:
    """Setup environment based on configuration."""
    # Set up logging
    import logging

    logging.basicConfig(
        level=getattr(logging, config.logging.level),
        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
    )

    # Set debug mode
    if config.debug:
        os.environ["DEBUG"] = "true"

    # Store in environment for other modules
    os.environ["WHITEMAGIC_CONFIG"] = config.model_dump_json()


# Example usage and initialization
if __name__ == "__main__":
    # Load configuration
    config = get_config()

    # Print configuration
    logger.info(f"Environment: {config.environment}")
    logger.info(f"Database URL: {config.database.url}")
    logger.info(f"API Host:Port: {config.api.host}:{config.api.port}")
    logger.info(f"Debug: {config.debug}")

    # Setup environment
    setup_config_environment(config)
