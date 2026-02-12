import secrets
import hashlib
import json
import logging
import os
from datetime import datetime, timedelta, timezone
from typing import Optional, Dict, List
from pathlib import Path
from pydantic import BaseModel

from whitemagic.config.paths import WM_ROOT
logger = logging.getLogger(__name__)

class APIKeyMetadata(BaseModel):
    key_id: str
    name: str
    owner: str
    created_at: datetime
    expires_at: Optional[datetime] = None
    scopes: List[str] = ["*"]
    is_active: bool = True

class APIKeySystem:
    """
    WhiteMagic API Key Management System.
    Provides secure generation and validation of keys.
    """

    def __init__(self, storage_path: Optional[str] = None):
        if storage_path:
            self.storage_path = Path(storage_path)
        else:
            # Default to WM_STATE_ROOT
            self.storage_path = WM_ROOT / "api_keys.json"

        # Ensure directory exists
        if self.storage_path.parent:
            self.storage_path.parent.mkdir(parents=True, exist_ok=True)

        self._keys: Dict[str, str] = {}  # hashed_key -> metadata_json (legacy/unused in new persistence)
        self._metadata: Dict[str, APIKeyMetadata] = {}

        # Load keys from storage
        self._load()

    def generate_key(self, name: str, owner: str, expires_days: Optional[int] = None) -> tuple[str, APIKeyMetadata]:
        """Generate a new API key."""
        # Create a prefix for identification (wm_ for WhiteMagic)
        raw_key = f"wm_{secrets.token_urlsafe(32)}"
        key_id = secrets.token_hex(8)

        expires_at = None
        if expires_days:
            expires_at = datetime.now(timezone.utc) + timedelta(days=expires_days)

        metadata = APIKeyMetadata(
            key_id=key_id,
            name=name,
            owner=owner,
            created_at=datetime.now(timezone.utc),
            expires_at=expires_at
        )

        # Store metadata
        self._metadata[key_id] = metadata

        # We need to store the hash -> key_id mapping persistently too
        # But for simplicity and security, we can just store the list of metadata
        # and checking a key involves hashing it and finding which metadata matches.
        # Wait, checking involves O(1) lookup ideally.
        # We can store a separate map of hash->id.

        hashed_key = self._hash_key(raw_key)
        self._keys[hashed_key] = key_id

        self._save()

        return raw_key, metadata

    def validate_key(self, raw_key: str) -> Optional[APIKeyMetadata]:
        """Validate an API key and return its metadata if valid."""
        hashed_key = self._hash_key(raw_key)
        key_id = self._keys.get(hashed_key)

        if not key_id:
            return None

        metadata = self._metadata.get(key_id)
        if not metadata or not metadata.is_active:
            return None

        if metadata.expires_at and metadata.expires_at < datetime.now(timezone.utc):
            metadata.is_active = False
            self._save()  # Persist expiration status
            return None

        return metadata

    def revoke_key(self, key_id: str) -> bool:
        """Revoke an API key."""
        if key_id in self._metadata:
            self._metadata[key_id].is_active = False
            self._save()
            return True
        return False

    def _hash_key(self, key: str) -> str:
        """Securely hash the key for storage."""
        return hashlib.sha256(key.encode()).hexdigest()

    def _save(self) -> None:
        """Save keys to disk."""
        try:
            data = {
                "keys": self._keys,
                "metadata": {
                    k: v.dict() for k, v in self._metadata.items()
                }
            }
            # Handle datetime serialization
            def json_serial(obj: object) -> str:
                if isinstance(obj, datetime):
                    return obj.isoformat()
                raise TypeError(f"Type {type(obj)} not serializable")

            with open(self.storage_path, 'w') as f:
                json.dump(data, f, default=json_serial, indent=2)

            # Set restrictive permissions (Unix)
            if hasattr(os, 'chmod'):
                os.chmod(self.storage_path, 0o600)

        except Exception as e:
            logger.error(f"Failed to save API keys: {e}")

    def _load(self) -> None:
        """Load keys from disk."""
        if not self.storage_path.exists():
            return

        try:
            with open(self.storage_path, 'r') as f:
                data = json.load(f)

            self._keys = data.get("keys", {})
            metadata_raw = data.get("metadata", {})

            self._metadata = {}
            for k, v in metadata_raw.items():
                try:
                    # Parse datetime strings back to datetime objects
                    # Pydantic usually handles this if passed to constructor/parse_obj
                    self._metadata[k] = APIKeyMetadata.parse_obj(v)
                except Exception as e:
                    logger.error(f"Failed to parse metadata for key {k}: {e}")

            logger.info(f"Loaded {len(self._metadata)} API keys from {self.storage_path}")

        except Exception as e:
            logger.error(f"Failed to load API keys: {e}")

# Global instance for shared use
api_key_system = APIKeySystem()
