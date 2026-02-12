# Encryption at Rest — Design Document

> Protecting WhiteMagic's local data stores with transparent encryption.

---

## Current State

WhiteMagic stores sensitive data in several locations:

| Data Store | Location | Encryption | Status |
|-----------|----------|-----------|--------|
| **Secrets Vault** | `~/.whitemagic/vault/secrets.db` | AES-256-GCM | ✅ Done |
| **Memory DB (hot)** | `~/.whitemagic/whitemagic.db` | None | ❌ Plaintext |
| **Memory DB (cold)** | `~/.whitemagic/memory/whitemagic.db` | None | ❌ Plaintext |
| **Galaxy DBs** | `~/.whitemagic/memory/galaxies/{name}/` | None | ❌ Plaintext |
| **Karma Ledger** | `~/.whitemagic/karma/` | None | ❌ Plaintext |
| **Audit Logs** | `~/.whitemagic/audit/` | None | ❌ Plaintext |
| **State root dir** | `~/.whitemagic/` | `chmod 0o700` | ✅ FS permissions |

The Vault (`whitemagic/security/vault.py`) already provides AES-256-GCM encryption for API keys and secrets. The memory databases are currently plaintext SQLite files protected only by filesystem permissions.

---

## Threat Model

### What We're Protecting Against

1. **Stolen laptop** — Attacker gets physical access to `~/.whitemagic/`
2. **Shared compute** — Other users on the same machine
3. **Cloud backup exposure** — Time Machine, Dropbox, etc. sync plaintext DBs
4. **Forensic analysis** — Deleted memories recoverable from unencrypted DB

### What We're NOT Protecting Against

1. **Root/admin access** — If they have root, they have the key
2. **Memory dump attacks** — Data is decrypted in memory during use
3. **Active malware** — Keyloggers can capture the passphrase

---

## Phase 1: SQLCipher for Memory Databases

### Approach

Replace the standard `sqlite3` module with [SQLCipher](https://www.zetetic.net/sqlcipher/) for memory databases. SQLCipher provides transparent, page-level AES-256-CBC encryption.

### Implementation

```python
# whitemagic/core/memory/encrypted_db.py

import os
from pathlib import Path

def get_connection(db_path: Path, passphrase: str | None = None):
    """Get an encrypted or plaintext SQLite connection."""
    passphrase = passphrase or os.environ.get("WM_DB_PASSPHRASE", "")
    
    if passphrase:
        try:
            from pysqlcipher3 import dbapi2 as sqlcipher
            conn = sqlcipher.connect(str(db_path))
            conn.execute(f"PRAGMA key = '{passphrase}'")
            conn.execute("PRAGMA cipher_page_size = 4096")
            conn.execute("PRAGMA kdf_iter = 256000")
            return conn
        except ImportError:
            import warnings
            warnings.warn(
                "pysqlcipher3 not installed — database will NOT be encrypted. "
                "Install with: pip install pysqlcipher3",
                stacklevel=2,
            )
    
    import sqlite3
    return sqlite3.connect(str(db_path))
```

### Key Management

| Method | Security | UX | Recommendation |
|--------|----------|-----|---------------|
| Environment variable (`WM_DB_PASSPHRASE`) | Medium | Good | Default for servers |
| OS keychain (macOS Keychain, GNOME Keyring) | High | Best | Default for desktop |
| Passphrase prompt | High | Worst | Optional for high-security |
| Derived from machine ID | Low | Best | Not recommended |

**Default behavior:**
1. Check `WM_DB_PASSPHRASE` env var
2. If not set, check OS keychain via `keyring` library
3. If not available, warn and fall back to plaintext

### Migration Path

Existing plaintext databases must be migrated:

```bash
wm vault encrypt-db          # Encrypt existing DBs
wm vault decrypt-db          # Decrypt (for migration/debugging)
wm vault rekey-db            # Change passphrase
wm vault status              # Show encryption status of all data stores
```

Migration is a one-time operation:
1. Create new encrypted DB
2. Copy all tables from plaintext → encrypted
3. Verify row counts match
4. Rename: `whitemagic.db` → `whitemagic.db.plaintext.bak`
5. Rename: `whitemagic.db.encrypted` → `whitemagic.db`

---

## Phase 2: Vault CLI

Extend the existing `whitemagic/security/vault.py` with CLI commands:

```bash
wm vault init                  # Initialize vault with passphrase
wm vault set KEY VALUE         # Store a secret
wm vault get KEY               # Retrieve a secret
wm vault list                  # List stored keys (not values)
wm vault delete KEY            # Remove a secret
wm vault lock                  # Clear cached passphrase
wm vault rekey                 # Change master passphrase
wm vault export --encrypted    # Export vault (encrypted backup)
```

### Auto-Lock

```python
# After N minutes of inactivity, clear the cached passphrase
WM_VAULT_TIMEOUT=30  # minutes (0 = never lock)
```

---

## Phase 3: OS Keychain Integration

Use the `keyring` library for transparent passphrase storage:

```python
try:
    import keyring
    passphrase = keyring.get_password("whitemagic", "db_passphrase")
except ImportError:
    passphrase = os.environ.get("WM_DB_PASSPHRASE")
```

Supported backends:
- **macOS:** Keychain
- **Linux:** GNOME Keyring, KDE Wallet
- **Windows:** Windows Credential Locker

---

## Phase 4: Audit Log Immutability

For Healthcare tier (Tier 6), audit logs must be append-only and tamper-evident:

1. Each log entry gets a SHA-256 hash chaining to the previous entry
2. Periodic Merkle root anchoring to XRPL (reuses `karma_anchor.py` infrastructure)
3. `audit.export` produces a signed, verifiable archive

---

## Dependencies

| Package | Purpose | Optional? |
|---------|---------|----------|
| `pysqlcipher3` | SQLCipher Python bindings | Yes — falls back to plaintext |
| `keyring` | OS keychain access | Yes — falls back to env var |
| `cryptography` | AES-GCM for vault (already used) | Yes — falls back to HMAC |

### pyproject.toml extras

```toml
[project.optional-dependencies]
encryption = ["pysqlcipher3>=1.2.0", "keyring>=25.0"]
```

---

## Security Considerations

1. **Passphrase strength** — Enforce minimum 12 characters for vault init
2. **Key derivation** — PBKDF2-HMAC-SHA256 with 600,000 iterations (already in vault.py)
3. **Memory clearing** — Use `memoryview` + zero-fill for passphrase buffers where possible
4. **Backup encryption** — `wm vault export` always encrypts, never plaintext
5. **No default passphrase** — System must explicitly opt in to encryption

---

## Implementation Priority

1. ✅ Vault secret storage (Done — `security/vault.py`)
2. ✅ FS permissions (Done — `config/paths.py`, `0o700`)
3. ⬜ SQLCipher for hot DB (`core/memory/encrypted_db.py`)
4. ⬜ Vault CLI commands (`cli/vault_command.py`)
5. ⬜ OS keychain integration
6. ⬜ Galaxy DB encryption
7. ⬜ Cold DB encryption
8. ⬜ Audit log immutability
