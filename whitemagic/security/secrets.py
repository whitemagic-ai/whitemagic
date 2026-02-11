"""Secrets Vault — Unified secrets management.
============================================
Backend selection via $WM_SECRETS_BACKEND:
  - "env"  (default) — read from environment variables
  - "file" — read from $WM_STATE_ROOT/secrets.json (chmod 600)
  - "vault" — HashiCorp Vault via VAULT_ADDR + VAULT_TOKEN

Usage:
    from whitemagic.security.secrets import get_secret, get_vault
    api_key = get_secret("OPENAI_API_KEY")
    vault = get_vault()
    vault.set("my_key", "my_value")  # persists to file backend
"""

from __future__ import annotations

import json
import logging
import os
import stat
import threading
from abc import ABC, abstractmethod
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)


class SecretBackend(ABC):
    """Abstract secrets backend."""

    @abstractmethod
    def get(self, key: str) -> str | None:
        ...

    @abstractmethod
    def set(self, key: str, value: str) -> bool:
        ...

    @abstractmethod
    def delete(self, key: str) -> bool:
        ...

    @abstractmethod
    def list_keys(self) -> list[str]:
        ...

    @abstractmethod
    def backend_name(self) -> str:
        ...


class EnvBackend(SecretBackend):
    """Read secrets from environment variables."""

    def __init__(self, prefix: str = "WM_SECRET_"):
        self._prefix = prefix

    def get(self, key: str) -> str | None:
        # Try prefixed first, then raw
        return os.environ.get(f"{self._prefix}{key}") or os.environ.get(key)

    def set(self, key: str, value: str) -> bool:
        os.environ[f"{self._prefix}{key}"] = value
        return True

    def delete(self, key: str) -> bool:
        prefixed = f"{self._prefix}{key}"
        if prefixed in os.environ:
            del os.environ[prefixed]
            return True
        return False

    def list_keys(self) -> list[str]:
        return [k[len(self._prefix):] for k in os.environ if k.startswith(self._prefix)]

    def backend_name(self) -> str:
        return "env"


class FileBackend(SecretBackend):
    """Read/write secrets from a JSON file with restricted permissions."""

    def __init__(self, path: str | None = None):
        self._path = Path(path or os.path.join(
            os.environ.get("WM_STATE_ROOT", os.path.expanduser("~/.whitemagic")),
            "secrets.json",
        ))
        self._lock = threading.Lock()
        self._cache: dict[str, str] = {}
        self._load()

    def _load(self) -> None:
        if self._path.exists():
            try:
                with open(self._path) as f:
                    self._cache = json.load(f)
            except Exception as e:
                logger.warning("Failed to load secrets file: %s", e)
                self._cache = {}

    def _save(self) -> None:
        self._path.parent.mkdir(parents=True, exist_ok=True)
        with open(self._path, "w") as f:
            json.dump(self._cache, f, indent=2)
        # Restrict to owner only
        os.chmod(self._path, stat.S_IRUSR | stat.S_IWUSR)

    def get(self, key: str) -> str | None:
        return self._cache.get(key)

    def set(self, key: str, value: str) -> bool:
        with self._lock:
            self._cache[key] = value
            self._save()
        return True

    def delete(self, key: str) -> bool:
        with self._lock:
            if key in self._cache:
                del self._cache[key]
                self._save()
                return True
        return False

    def list_keys(self) -> list[str]:
        return list(self._cache.keys())

    def backend_name(self) -> str:
        return "file"


class VaultBackend(SecretBackend):
    """HashiCorp Vault backend via HTTP API."""

    def __init__(
        self,
        addr: str | None = None,
        token: str | None = None,
        mount: str = "secret",
        path_prefix: str = "whitemagic",
    ):
        self._addr = (addr or os.environ.get("VAULT_ADDR", "http://127.0.0.1:8200")).rstrip("/")
        self._token = token or os.environ.get("VAULT_TOKEN", "")
        self._mount = mount
        self._prefix = path_prefix

    def _request(self, method: str, path: str, data: dict | None = None) -> dict | None:
        try:
            import urllib.error
            import urllib.request

            url = f"{self._addr}/v1/{self._mount}/data/{self._prefix}/{path}"
            body = json.dumps({"data": data}).encode() if data else None
            req = urllib.request.Request(url, data=body, method=method)
            req.add_header("X-Vault-Token", self._token)
            req.add_header("Content-Type", "application/json")

            with urllib.request.urlopen(req, timeout=5) as resp:
                return dict(json.loads(resp.read()))
        except Exception as e:
            logger.debug("Vault request failed: %s", e)
            return None

    def get(self, key: str) -> str | None:
        result = self._request("GET", key)
        if result and "data" in result:
            return str(result["data"].get("data", {}).get("value"))
        return None

    def set(self, key: str, value: str) -> bool:
        result = self._request("POST", key, {"value": value})
        return result is not None

    def delete(self, key: str) -> bool:
        try:
            import urllib.request
            url = f"{self._addr}/v1/{self._mount}/data/{self._prefix}/{key}"
            req = urllib.request.Request(url, method="DELETE")
            req.add_header("X-Vault-Token", self._token)
            urllib.request.urlopen(req, timeout=5)
            return True
        except Exception:
            return False

    def list_keys(self) -> list[str]:
        try:
            import urllib.request
            url = f"{self._addr}/v1/{self._mount}/metadata/{self._prefix}?list=true"
            req = urllib.request.Request(url, method="GET")
            req.add_header("X-Vault-Token", self._token)
            with urllib.request.urlopen(req, timeout=5) as resp:
                data = json.loads(resp.read())
                return list(data.get("data", {}).get("keys", []))
        except Exception:
            return []

    def backend_name(self) -> str:
        return "vault"


class SecretsVault:
    """Unified secrets manager with backend selection.

    Falls back through backends: primary → env (always available as fallback).
    """

    def __init__(self, backend: SecretBackend | None = None):
        self._primary = backend or self._create_backend()
        self._env_fallback = EnvBackend()

    @staticmethod
    def _create_backend() -> SecretBackend:
        backend_type = os.environ.get("WM_SECRETS_BACKEND", "env").lower()
        if backend_type == "file":
            return FileBackend()
        if backend_type == "vault":
            return VaultBackend()
        return EnvBackend()

    def get(self, key: str) -> str | None:
        """Get a secret. Tries primary backend, then env fallback."""
        value = self._primary.get(key)
        if value is None and not isinstance(self._primary, EnvBackend):
            value = self._env_fallback.get(key)
        return value

    def set(self, key: str, value: str) -> bool:
        """Set a secret in the primary backend."""
        return self._primary.set(key, value)

    def delete(self, key: str) -> bool:
        """Delete a secret from the primary backend."""
        return self._primary.delete(key)

    def list_keys(self) -> list[str]:
        """List all secret keys in the primary backend."""
        return self._primary.list_keys()

    def status(self) -> dict[str, Any]:
        """Get vault status."""
        return {
            "backend": self._primary.backend_name(),
            "key_count": len(self.list_keys()),
            "has_env_fallback": not isinstance(self._primary, EnvBackend),
        }


# ---------------------------------------------------------------------------
# Singleton + convenience
# ---------------------------------------------------------------------------

_vault: SecretsVault | None = None
_vault_lock = threading.Lock()


def get_vault() -> SecretsVault:
    """Get the global SecretsVault instance."""
    global _vault
    if _vault is None:
        with _vault_lock:
            if _vault is None:
                _vault = SecretsVault()
    return _vault


def get_secret(key: str) -> str | None:
    """Convenience: get a secret from the global vault."""
    return get_vault().get(key)
