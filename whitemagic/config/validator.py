"""Configuration Validator
Ensures secure configuration at startup.

Features:
- Auto-generates secure JWT secrets on first run
- Validates deployment mode (local vs cloud)
- Stores secrets securely in user config directory
- Enforces production security requirements
"""

import json
import logging
import os
import secrets
from pathlib import Path
from typing import Any

from whitemagic.config.paths import WM_ROOT

logger = logging.getLogger(__name__)

def _silent_init() -> bool:
    return os.getenv("WM_SILENT_INIT", "").strip().lower() in {"1", "true", "yes", "on"}


class ConfigValidator:
    """Validates and secures configuration at startup.

    Usage:
        validator = ConfigValidator()
        secrets_data = validator.validate_or_create_secrets()
        mode = validator.validate_deployment_mode()
    """

    def __init__(self, config_dir: Path | None = None):
        """Initialize ConfigValidator.

        Args:
            config_dir: Configuration directory (defaults to WM_ROOT)

        """
        if config_dir is None:
            config_dir = WM_ROOT

        self.config_dir = Path(config_dir)
        self.config_file = self.config_dir / "config.json"
        self.secrets_file = self.config_dir / "secrets.json"

        # Ensure config directory exists
        self.config_dir.mkdir(parents=True, exist_ok=True)

        if not _silent_init():
            logger.info(f"ConfigValidator initialized with config_dir: {self.config_dir}")

    def validate_or_create_secrets(self) -> dict[str, str]:
        """Ensure JWT secret exists and is secure.

        Returns:
            Dictionary with secrets (jwt_secret, api_key)

        """
        if self.secrets_file.exists():
            # Load existing secrets
            try:
                with open(self.secrets_file) as f:
                    raw = json.load(f)
                if not isinstance(raw, dict):
                    raise ValueError("secrets.json must contain an object")
                secrets_data = {str(k): str(v) for k, v in raw.items()}

                if not _silent_init():
                    logger.info("Loaded existing secrets from config")

                # Validate secrets are not defaults
                if secrets_data.get("jwt_secret") in (None, "", "CHANGE_ME_IN_PRODUCTION"):
                    logger.warning("Found default JWT secret, regenerating...")
                    secrets_data = self._generate_secrets()
                    self._save_secrets(secrets_data)

                return secrets_data

            except Exception as e:
                logger.error(f"Failed to load secrets: {e}")
                if not _silent_init():
                    logger.info("Regenerating secrets...")
                secrets_data = self._generate_secrets()
                self._save_secrets(secrets_data)
                return secrets_data
        else:
            # Generate new secrets
            if not _silent_init():
                logger.info("No secrets file found, generating new secrets...")
            secrets_data = self._generate_secrets()
            self._save_secrets(secrets_data)
            return secrets_data

    def _generate_secrets(self) -> dict[str, str]:
        """Generate secure random secrets.

        Returns:
            Dictionary with generated secrets

        """
        return {
            "jwt_secret": secrets.token_urlsafe(64),  # 512 bits
            "api_key": secrets.token_urlsafe(32),     # 256 bits
            "csrf_secret": secrets.token_urlsafe(32),  # 256 bits
        }

    def _save_secrets(self, secrets_data: dict[str, str]) -> None:
        """Save secrets to file with secure permissions.

        Args:
            secrets_data: Secrets to save

        """
        # Write secrets file
        with open(self.secrets_file, "w") as f:
            json.dump(secrets_data, f, indent=2)

        # Set restrictive permissions (Unix only)
        if os.name != "nt":  # Not Windows
            try:
                os.chmod(self.secrets_file, 0o600)  # Owner read/write only
                if not _silent_init():
                    logger.info("Set secrets file permissions to 0600")
            except Exception as e:
                logger.warning(f"Failed to set file permissions: {e}")

        if not _silent_init():
            logger.info(f"Saved secrets to {self.secrets_file}")

    def validate_deployment_mode(self) -> str:
        """Determine if running in local or cloud mode.

        Returns:
            "local" or "cloud"

        Raises:
            ValueError: If invalid mode specified

        """
        mode = os.getenv("WHITEMAGIC_MODE", "local").lower()

        if mode not in ("local", "cloud"):
            raise ValueError(
                f"Invalid WHITEMAGIC_MODE: {mode}. Must be 'local' or 'cloud'",
            )

        if not _silent_init():
            logger.info(f"Deployment mode: {mode}")
        return mode

    def validate_production_config(self) -> None:
        """Validate configuration for production deployment.

        Raises:
            RuntimeError: If production requirements not met

        """
        mode = self.validate_deployment_mode()

        if mode == "cloud":
            # Cloud mode has stricter requirements
            self._validate_cloud_config()
        # Local mode is more permissive
        elif not _silent_init():
            logger.info("Local mode - relaxed validation")

    def _validate_cloud_config(self) -> None:
        """Validate cloud mode configuration.

        Raises:
            RuntimeError: If cloud requirements not met

        """
        if not _silent_init():
            logger.info("Validating cloud mode configuration...")

        # Check JWT secret
        secrets_data = self.validate_or_create_secrets()
        if secrets_data.get("jwt_secret") in (None, "", "CHANGE_ME_IN_PRODUCTION"):
            raise RuntimeError(
                "Cloud mode requires secure JWT secret. "
                "Set WHITEMAGIC_JWT_SECRET environment variable.",
            )

        # Check CORS origins
        allowed_origins = os.getenv("ALLOWED_ORIGINS", "")
        if not allowed_origins or allowed_origins == "*":
            raise RuntimeError(
                "Cloud mode requires explicit ALLOWED_ORIGINS. "
                "Wildcard (*) not allowed in production.",
            )

        # Check database URL (if using database)
        db_url = os.getenv("DATABASE_URL")
        if db_url and "localhost" in db_url:
            logger.warning(
                "Cloud mode with localhost database - "
                "ensure this is intentional",
            )

        if not _silent_init():
            logger.info("Cloud mode configuration validated ✓")

    def get_config(self) -> dict[str, Any]:
        """Get complete configuration.

        Returns:
            Configuration dictionary

        """
        return {
            "mode": self.validate_deployment_mode(),
            "config_dir": str(self.config_dir),
            "secrets_file": str(self.secrets_file),
            "config_file": str(self.config_file),
        }

    def save_config(self, config: dict[str, Any]) -> None:
        """Save configuration to file.

        Args:
            config: Configuration to save

        """
        with open(self.config_file, "w") as f:
            json.dump(config, f, indent=2)

        if not _silent_init():
            logger.info(f"Saved configuration to {self.config_file}")

    def load_config(self) -> dict[str, Any]:
        """Load configuration from file.

        Returns:
            Configuration dictionary

        """
        if self.config_file.exists():
            with open(self.config_file) as f:
                loaded = json.load(f)
            if isinstance(loaded, dict):
                return loaded
        return {}


# Global instance for convenience
_validator: ConfigValidator | None = None


def get_validator() -> ConfigValidator:
    """Get global ConfigValidator instance."""
    global _validator
    if _validator is None:
        _validator = ConfigValidator()
    return _validator


def validate_startup() -> None:
    """Validate configuration at application startup.

    Raises:
        RuntimeError: If validation fails

    """
    validator = get_validator()

    # Ensure secrets exist
    validator.validate_or_create_secrets()

    # Validate deployment mode
    mode = validator.validate_deployment_mode()

    # Validate production config if in cloud mode
    if mode == "cloud":
        validator.validate_production_config()

    logger.info("Startup validation complete ✓")


__all__ = [
    "ConfigValidator",
    "get_validator",
    "validate_startup",
]
