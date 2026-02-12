"""WhiteMagic Vault — Encrypted local secret storage.

Stores API keys and secrets in an AES-GCM encrypted SQLite database.
Keys are derived from a passphrase (Argon2-like via PBKDF2) or OS keychain.

Usage:
    from whitemagic.security.vault import get_vault
    vault = get_vault()
    vault.set("OPENAI_API_KEY", "sk-abc123...")
    key = vault.get("OPENAI_API_KEY")
"""

import base64
import hashlib
import hmac
import logging
import os
import sqlite3
from datetime import datetime
from pathlib import Path

logger = logging.getLogger(__name__)

# AES-GCM constants
_NONCE_SIZE = 12
_TAG_SIZE = 16
_KEY_SIZE = 32
_PBKDF2_ITERATIONS = 600_000


def _derive_key(passphrase: str, salt: bytes) -> bytes:
    """Derive a 256-bit encryption key from a passphrase using PBKDF2-HMAC-SHA256."""
    return hashlib.pbkdf2_hmac("sha256", passphrase.encode("utf-8"), salt, _PBKDF2_ITERATIONS)


def _encrypt(plaintext: bytes, key: bytes) -> tuple[bytes, bytes]:
    """Encrypt plaintext with AES-256-GCM. Returns (ciphertext_with_tag, nonce).

    Uses the cryptography library if available, falls back to XOR-HMAC for
    environments without native crypto (still better than plaintext).
    """
    nonce = os.urandom(_NONCE_SIZE)
    try:
        from cryptography.hazmat.primitives.ciphers.aead import AESGCM
        aesgcm = AESGCM(key)
        ct = aesgcm.encrypt(nonce, plaintext, None)
        return ct, nonce
    except ImportError:
        # Fallback: HMAC-based authenticated encryption (not AES-GCM but still authenticated)
        logger.debug("cryptography package not available; using HMAC fallback")
        stream = hashlib.pbkdf2_hmac("sha256", key, nonce, 1, dklen=len(plaintext))
        ct = bytes(a ^ b for a, b in zip(plaintext, stream))
        tag = hmac.new(key, nonce + ct, hashlib.sha256).digest()[:_TAG_SIZE]
        return ct + tag, nonce


def _decrypt(ciphertext_with_tag: bytes, nonce: bytes, key: bytes) -> bytes:
    """Decrypt AES-256-GCM ciphertext. Returns plaintext."""
    try:
        from cryptography.hazmat.primitives.ciphers.aead import AESGCM
        aesgcm = AESGCM(key)
        return aesgcm.decrypt(nonce, ciphertext_with_tag, None)
    except ImportError:
        # Fallback: HMAC-based authenticated decryption
        ct = ciphertext_with_tag[:-_TAG_SIZE]
        tag = ciphertext_with_tag[-_TAG_SIZE:]
        expected_tag = hmac.new(key, nonce + ct, hashlib.sha256).digest()[:_TAG_SIZE]
        if not hmac.compare_digest(tag, expected_tag):
            raise ValueError("Vault decryption failed: authentication tag mismatch")
        stream = hashlib.pbkdf2_hmac("sha256", key, nonce, 1, dklen=len(ct))
        return bytes(a ^ b for a, b in zip(ct, stream))


class Vault:
    """Encrypted local secret storage backed by SQLite."""

    def __init__(self, db_path: Path | None = None, passphrase: str | None = None) -> None:
        from whitemagic.config.paths import WM_ROOT
        self.db_path = db_path or (WM_ROOT / "vault" / "secrets.db")
        self.db_path.parent.mkdir(parents=True, exist_ok=True)

        # Try to chmod the vault directory to owner-only
        try:
            self.db_path.parent.chmod(0o700)
        except OSError:
            pass

        self._conn = sqlite3.connect(str(self.db_path))
        self._conn.execute("""
            CREATE TABLE IF NOT EXISTS vault (
                name TEXT PRIMARY KEY,
                encrypted_value BLOB NOT NULL,
                nonce BLOB NOT NULL,
                created_at TEXT NOT NULL,
                updated_at TEXT NOT NULL
            )
        """)
        self._conn.execute("""
            CREATE TABLE IF NOT EXISTS vault_meta (
                key TEXT PRIMARY KEY,
                value BLOB NOT NULL
            )
        """)
        self._conn.commit()

        # Derive encryption key
        self._master_key = self._resolve_key(passphrase)

    def _resolve_key(self, passphrase: str | None) -> bytes:
        """Resolve the master encryption key from available sources."""
        # Priority 1: Environment variable
        env_key = os.environ.get("WM_ENCRYPTION_KEY")
        if env_key:
            try:
                return base64.b64decode(env_key)
            except Exception:
                logger.warning("WM_ENCRYPTION_KEY is not valid base64; ignoring")

        # Priority 2: OS keychain
        try:
            import keyring
            stored = keyring.get_password("whitemagic", "vault_master_key")
            if stored:
                return base64.b64decode(stored)
        except Exception:
            pass

        # Priority 3: Passphrase
        if passphrase:
            salt = self._get_or_create_salt()
            return _derive_key(passphrase, salt)

        # Priority 4: Default key derived from machine-specific info
        # (Better than plaintext, but not as secure as a real passphrase)
        salt = self._get_or_create_salt()
        machine_id = f"{os.getlogin()}@{os.uname().nodename}" if hasattr(os, "getlogin") else "whitemagic-default"
        return _derive_key(machine_id, salt)

    def _get_or_create_salt(self) -> bytes:
        """Get or create the PBKDF2 salt (stored in DB metadata)."""
        row = self._conn.execute(
            "SELECT value FROM vault_meta WHERE key = 'salt'"
        ).fetchone()
        if row:
            return bytes(row[0])
        salt = os.urandom(32)
        self._conn.execute(
            "INSERT INTO vault_meta (key, value) VALUES ('salt', ?)", (salt,)
        )
        self._conn.commit()
        return salt

    def set(self, name: str, value: str) -> None:
        """Store an encrypted secret."""
        ct, nonce = _encrypt(value.encode("utf-8"), self._master_key)
        now = datetime.now().isoformat()
        self._conn.execute("""
            INSERT INTO vault (name, encrypted_value, nonce, created_at, updated_at)
            VALUES (?, ?, ?, ?, ?)
            ON CONFLICT(name) DO UPDATE SET
                encrypted_value = excluded.encrypted_value,
                nonce = excluded.nonce,
                updated_at = excluded.updated_at
        """, (name, ct, nonce, now, now))
        self._conn.commit()
        logger.info(f"Vault: stored secret '{name}'")

    def get(self, name: str) -> str | None:
        """Retrieve a decrypted secret."""
        row = self._conn.execute(
            "SELECT encrypted_value, nonce FROM vault WHERE name = ?", (name,)
        ).fetchone()
        if not row:
            return None
        ct, nonce = row
        plaintext = _decrypt(ct, nonce, self._master_key)
        return plaintext.decode("utf-8")

    def delete(self, name: str) -> bool:
        """Delete a secret."""
        cursor = self._conn.execute("DELETE FROM vault WHERE name = ?", (name,))
        self._conn.commit()
        return cursor.rowcount > 0

    def list(self) -> list[str]:
        """List all secret names (never returns values)."""
        rows = self._conn.execute("SELECT name FROM vault ORDER BY name").fetchall()
        return [r[0] for r in rows]

    def exists(self, name: str) -> bool:
        """Check if a secret exists."""
        row = self._conn.execute(
            "SELECT 1 FROM vault WHERE name = ?", (name,)
        ).fetchone()
        return row is not None

    def rekey(self, new_passphrase: str) -> int:
        """Re-encrypt all secrets with a new passphrase. Returns count of re-encrypted secrets."""
        old_key = self._master_key
        salt = os.urandom(32)

        # Update salt
        self._conn.execute(
            "INSERT OR REPLACE INTO vault_meta (key, value) VALUES ('salt', ?)", (salt,)
        )
        new_key = _derive_key(new_passphrase, salt)

        # Re-encrypt all secrets
        rows = self._conn.execute("SELECT name, encrypted_value, nonce FROM vault").fetchall()
        count = 0
        for name, ct, nonce in rows:
            plaintext = _decrypt(ct, nonce, old_key)
            new_ct, new_nonce = _encrypt(plaintext, new_key)
            self._conn.execute(
                "UPDATE vault SET encrypted_value = ?, nonce = ?, updated_at = ? WHERE name = ?",
                (new_ct, new_nonce, datetime.now().isoformat(), name),
            )
            count += 1

        self._conn.commit()
        self._master_key = new_key
        logger.info(f"Vault: re-keyed {count} secrets")
        return count

    def close(self) -> None:
        """Close the vault connection."""
        self._conn.close()


# Global singleton
_vault: Vault | None = None


def get_vault(passphrase: str | None = None) -> Vault:
    """Get or create the global Vault instance."""
    global _vault
    if _vault is None:
        _vault = Vault(passphrase=passphrase)
    return _vault
