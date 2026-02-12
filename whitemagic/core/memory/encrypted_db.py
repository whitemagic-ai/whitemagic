"""Encrypted Database Manager — Unified encryption for all WhiteMagic data stores.

Wraps the existing ConnectionPool (db_manager.py) with:
  - OS keychain integration (macOS Keychain, GNOME Keyring, KDE Wallet)
  - Passphrase strength enforcement (min 12 chars, entropy check)
  - Migration tools (plaintext → encrypted, encrypted → plaintext)
  - Status reporting (which stores are encrypted)
  - Secure memory clearing for passphrase buffers

The encryption itself is handled by SQLCipher (AES-256-CBC, page-level)
via the existing db_manager.ConnectionPool._create_connection() which
already checks WM_DB_PASSPHRASE.

Usage:
    from whitemagic.core.memory.encrypted_db import (
        get_db_passphrase,
        encrypt_database,
        decrypt_database,
        encryption_status,
    )
"""

from __future__ import annotations

import ctypes
import logging
import math
import os
import shutil
import sqlite3
from collections import Counter
from pathlib import Path

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Passphrase resolution (env var → keychain → None)
# ---------------------------------------------------------------------------

_KEYCHAIN_SERVICE = "whitemagic"
_KEYCHAIN_ACCOUNT = "db_passphrase"


def get_db_passphrase() -> str | None:
    """Resolve the database encryption passphrase from available sources.

    Priority:
      1. WM_DB_PASSPHRASE environment variable
      2. OS keychain (via `keyring` library)
      3. None (plaintext fallback)
    """
    # Priority 1: Environment variable
    env_pass = os.environ.get("WM_DB_PASSPHRASE")
    if env_pass:
        return env_pass

    # Priority 2: OS keychain
    try:
        import keyring
        stored = keyring.get_password(_KEYCHAIN_SERVICE, _KEYCHAIN_ACCOUNT)
        if stored:
            return stored
    except Exception:
        pass

    return None


def store_passphrase_in_keychain(passphrase: str) -> bool:
    """Store the DB passphrase in the OS keychain. Returns True on success."""
    try:
        import keyring
        keyring.set_password(_KEYCHAIN_SERVICE, _KEYCHAIN_ACCOUNT, passphrase)
        logger.info("DB passphrase stored in OS keychain")
        return True
    except Exception as e:
        logger.warning(f"Failed to store passphrase in keychain: {e}")
        return False


def clear_passphrase_from_keychain() -> bool:
    """Remove the DB passphrase from the OS keychain. Returns True on success."""
    try:
        import keyring
        keyring.delete_password(_KEYCHAIN_SERVICE, _KEYCHAIN_ACCOUNT)
        logger.info("DB passphrase cleared from OS keychain")
        return True
    except Exception as e:
        logger.warning(f"Failed to clear passphrase from keychain: {e}")
        return False


# ---------------------------------------------------------------------------
# Passphrase strength validation
# ---------------------------------------------------------------------------

def _shannon_entropy(text: str) -> float:
    """Calculate Shannon entropy in bits per character."""
    if not text:
        return 0.0
    counts = Counter(text)
    length = len(text)
    entropy = -sum(
        (count / length) * math.log2(count / length)
        for count in counts.values()
    )
    return entropy


def validate_passphrase(passphrase: str) -> tuple[bool, str]:
    """Validate passphrase strength. Returns (is_valid, reason).

    Requirements:
      - Minimum 12 characters
      - Shannon entropy >= 2.5 bits/char (prevents 'aaaaaaaaaaaa')
      - Not a common weak pattern
    """
    if len(passphrase) < 12:
        return False, f"Passphrase must be at least 12 characters (got {len(passphrase)})"

    entropy = _shannon_entropy(passphrase)
    if entropy < 2.5:
        return False, f"Passphrase too predictable (entropy {entropy:.1f} bits/char, need >= 2.5)"

    # Check for trivially weak patterns
    weak_patterns = [
        "password", "123456789012", "qwertyuiop", "abcdefghijkl",
        "whitemagic123", "letmein12345",
    ]
    lower = passphrase.lower()
    for pattern in weak_patterns:
        if pattern in lower:
            return False, "Passphrase contains common weak pattern"

    return True, "OK"


# ---------------------------------------------------------------------------
# Secure memory clearing
# ---------------------------------------------------------------------------

def secure_clear(data: bytearray | memoryview) -> None:
    """Zero-fill a mutable buffer to remove sensitive data from memory.

    Works on bytearray and writable memoryview objects.
    For str objects, convert to bytearray first, use, then clear.
    """
    if isinstance(data, memoryview):
        if not data.readonly:
            ctypes.memset(ctypes.addressof((ctypes.c_char * len(data)).from_buffer(data)), 0, len(data))
    elif isinstance(data, bytearray):
        ctypes.memset(ctypes.addressof((ctypes.c_char * len(data)).from_buffer(data)), 0, len(data))
    else:
        logger.warning("secure_clear: unsupported type %s (use bytearray or writable memoryview)", type(data))


def passphrase_context(passphrase: str):
    """Context manager that clears the passphrase buffer on exit.

    Usage:
        buf = bytearray(passphrase.encode())
        with passphrase_context(buf):
            use_passphrase(buf.decode())
    """
    import contextlib

    @contextlib.contextmanager
    def _ctx():
        buf = bytearray(passphrase.encode("utf-8"))
        try:
            yield buf.decode("utf-8")
        finally:
            secure_clear(buf)

    return _ctx()


# ---------------------------------------------------------------------------
# Database encryption status
# ---------------------------------------------------------------------------

def _is_sqlcipher_db(db_path: Path) -> bool:
    """Check if a database file is SQLCipher-encrypted.

    SQLCipher databases have a non-standard header (not 'SQLite format 3\\000').
    """
    if not db_path.exists():
        return False
    try:
        with open(db_path, "rb") as f:
            header = f.read(16)
        # Standard SQLite header starts with "SQLite format 3\x00"
        return not header.startswith(b"SQLite format 3\x00")
    except Exception:
        return False


def _db_size_mb(db_path: Path) -> float:
    """Get database file size in MB."""
    if not db_path.exists():
        return 0.0
    return db_path.stat().st_size / (1024 * 1024)


def _count_memories(db_path: Path) -> int | None:
    """Count memories in a plaintext SQLite database. Returns None if encrypted or missing."""
    if not db_path.exists():
        return None
    if _is_sqlcipher_db(db_path):
        return None  # Can't read without passphrase
    try:
        conn = sqlite3.connect(str(db_path))
        row = conn.execute("SELECT COUNT(*) FROM memories").fetchone()
        conn.close()
        return row[0] if row else 0
    except Exception:
        return None


def encryption_status() -> dict:
    """Report encryption status of all WhiteMagic data stores.

    Returns a dict with status for each store:
      - encrypted: bool
      - size_mb: float
      - memory_count: int | None
      - path: str
    """
    from whitemagic.config.paths import WM_ROOT

    stores = {
        "vault": WM_ROOT / "vault" / "secrets.db",
        "hot_db": WM_ROOT / "memory" / "whitemagic.db",
        "cold_db": WM_ROOT / "memory" / "whitemagic_cold.db",
        "karma_ledger": WM_ROOT / "dharma" / "karma.jsonl",
        "audit_log": WM_ROOT / "audit" / "audit.jsonl",
    }

    # Discover galaxy DBs
    galaxy_dir = WM_ROOT / "memory" / "galaxies"
    if galaxy_dir.exists():
        for galaxy_path in galaxy_dir.iterdir():
            if galaxy_path.is_dir():
                db_file = galaxy_path / "whitemagic.db"
                if db_file.exists():
                    stores[f"galaxy:{galaxy_path.name}"] = db_file

    result = {}
    for name, path in stores.items():
        if not path.exists():
            result[name] = {
                "path": str(path),
                "exists": False,
                "encrypted": False,
                "size_mb": 0.0,
                "memory_count": None,
            }
            continue

        is_encrypted = False
        if path.suffix == ".db":
            is_encrypted = _is_sqlcipher_db(path)

        result[name] = {
            "path": str(path),
            "exists": True,
            "encrypted": is_encrypted,
            "size_mb": round(_db_size_mb(path), 2),
            "memory_count": _count_memories(path) if not is_encrypted else None,
        }

    # Check if WM_DB_PASSPHRASE is configured
    passphrase_source = "none"
    if os.environ.get("WM_DB_PASSPHRASE"):
        passphrase_source = "env_var"
    else:
        try:
            import keyring
            if keyring.get_password(_KEYCHAIN_SERVICE, _KEYCHAIN_ACCOUNT):
                passphrase_source = "os_keychain"
        except Exception:
            pass

    result["_config"] = {
        "passphrase_source": passphrase_source,
        "sqlcipher_available": _sqlcipher_available(),
        "keyring_available": _keyring_available(),
    }

    return result


def _sqlcipher_available() -> bool:
    """Check if SQLCipher Python bindings are available."""
    try:
        import sqlcipher3  # type: ignore[import-untyped]  # noqa: F401
        return True
    except ImportError:
        return False


def _keyring_available() -> bool:
    """Check if the keyring library is available."""
    try:
        import keyring  # noqa: F401
        return True
    except ImportError:
        return False


# ---------------------------------------------------------------------------
# Database migration (plaintext ↔ encrypted)
# ---------------------------------------------------------------------------

def encrypt_database(db_path: Path, passphrase: str) -> Path:
    """Encrypt a plaintext SQLite database with SQLCipher.

    1. Validates passphrase strength
    2. Creates encrypted copy via SQLCipher's sqlcipher_export
    3. Verifies row counts match
    4. Renames: original → .plaintext.bak, encrypted → original name

    Returns the path to the encrypted database.
    Raises ValueError on passphrase weakness, RuntimeError on migration failure.
    """
    valid, reason = validate_passphrase(passphrase)
    if not valid:
        raise ValueError(f"Weak passphrase: {reason}")

    if not db_path.exists():
        raise FileNotFoundError(f"Database not found: {db_path}")

    if _is_sqlcipher_db(db_path):
        raise RuntimeError(f"Database is already encrypted: {db_path}")

    try:
        import sqlcipher3 as sqlcipher  # type: ignore[import-untyped]
    except ImportError:
        raise RuntimeError(
            "sqlcipher3 not installed. Install with: pip install sqlcipher3"
        )

    # Count rows before encryption
    src_conn = sqlite3.connect(str(db_path))
    try:
        src_tables = src_conn.execute(
            "SELECT name FROM sqlite_master WHERE type='table'"
        ).fetchall()
        src_counts = {}
        for (table_name,) in src_tables:
            if table_name.startswith("sqlite_"):
                continue
            row = src_conn.execute(f"SELECT COUNT(*) FROM [{table_name}]").fetchone()
            src_counts[table_name] = row[0] if row else 0
    finally:
        src_conn.close()

    # Create encrypted copy
    enc_path = db_path.with_suffix(".db.encrypted")
    try:
        src = sqlite3.connect(str(db_path))
        src.execute(f"ATTACH DATABASE '{enc_path}' AS encrypted KEY '{passphrase}'")
        src.execute("SELECT sqlcipher_export('encrypted')")
        src.execute("DETACH DATABASE encrypted")
        src.close()
    except Exception:
        # Fallback: manual copy via SQLCipher
        src = sqlite3.connect(str(db_path))
        dst = sqlcipher.connect(str(enc_path))
        dst.execute(f"PRAGMA key='{passphrase}'")
        dst.execute("PRAGMA cipher_page_size = 4096")
        dst.execute("PRAGMA kdf_iter = 256000")

        # Dump and restore
        for line in src.iterdump():
            try:
                dst.execute(line)
            except Exception:
                pass
        dst.commit()
        src.close()
        dst.close()

    # Verify row counts
    verify = sqlcipher.connect(str(enc_path))
    verify.execute(f"PRAGMA key='{passphrase}'")
    for table_name, expected in src_counts.items():
        try:
            row = verify.execute(f"SELECT COUNT(*) FROM [{table_name}]").fetchone()
            actual = row[0] if row else 0
            if actual != expected:
                verify.close()
                enc_path.unlink(missing_ok=True)
                raise RuntimeError(
                    f"Row count mismatch in {table_name}: expected {expected}, got {actual}"
                )
        except sqlite3.OperationalError:
            pass  # Table might not exist in encrypted DB (e.g., FTS shadow tables)
    verify.close()

    # Swap files
    backup_path = db_path.with_suffix(".db.plaintext.bak")
    shutil.move(str(db_path), str(backup_path))
    shutil.move(str(enc_path), str(db_path))

    logger.info(f"Encrypted {db_path} ({sum(src_counts.values())} rows). Backup: {backup_path}")
    return db_path


def decrypt_database(db_path: Path, passphrase: str) -> Path:
    """Decrypt an encrypted SQLCipher database to plaintext SQLite.

    Returns the path to the decrypted database.
    """
    if not db_path.exists():
        raise FileNotFoundError(f"Database not found: {db_path}")

    if not _is_sqlcipher_db(db_path):
        raise RuntimeError(f"Database is not encrypted: {db_path}")

    try:
        import sqlcipher3 as sqlcipher  # type: ignore[import-untyped]
    except ImportError:
        raise RuntimeError(
            "sqlcipher3 not installed. Install with: pip install sqlcipher3"
        )

    dec_path = db_path.with_suffix(".db.decrypted")

    # Open encrypted source
    src = sqlcipher.connect(str(db_path))
    src.execute(f"PRAGMA key='{passphrase}'")

    # Create plaintext destination
    dst = sqlite3.connect(str(dec_path))

    # Dump and restore
    for line in src.iterdump():
        try:
            dst.execute(line)
        except Exception:
            pass
    dst.commit()
    src.close()
    dst.close()

    # Swap files
    backup_path = db_path.with_suffix(".db.encrypted.bak")
    shutil.move(str(db_path), str(backup_path))
    shutil.move(str(dec_path), str(db_path))

    logger.info(f"Decrypted {db_path}. Encrypted backup: {backup_path}")
    return db_path


def rekey_database(db_path: Path, old_passphrase: str, new_passphrase: str) -> Path:
    """Change the encryption passphrase of a SQLCipher database.

    Uses SQLCipher's PRAGMA rekey for in-place re-encryption.
    """
    valid, reason = validate_passphrase(new_passphrase)
    if not valid:
        raise ValueError(f"Weak new passphrase: {reason}")

    if not db_path.exists():
        raise FileNotFoundError(f"Database not found: {db_path}")

    try:
        import sqlcipher3 as sqlcipher  # type: ignore[import-untyped]
    except ImportError:
        raise RuntimeError("sqlcipher3 not installed")

    # Create backup first
    backup_path = db_path.with_suffix(".db.pre-rekey.bak")
    shutil.copy2(str(db_path), str(backup_path))

    conn = sqlcipher.connect(str(db_path))
    conn.execute(f"PRAGMA key='{old_passphrase}'")
    # Verify we can read the DB
    try:
        conn.execute("SELECT count(*) FROM sqlite_master").fetchone()
    except Exception:
        conn.close()
        raise RuntimeError("Failed to decrypt with old passphrase")

    conn.execute(f"PRAGMA rekey='{new_passphrase}'")
    conn.close()

    # Clean up backup on success
    backup_path.unlink(missing_ok=True)
    logger.info(f"Re-keyed {db_path}")
    return db_path
