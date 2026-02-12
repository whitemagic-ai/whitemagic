# Encryption at Rest — Design Document

> Protecting WhiteMagic's local data stores with transparent encryption.

---

## Current State

WhiteMagic stores sensitive data in several locations:

| Data Store | Location | Encryption | Status |
|-----------|----------|-----------|--------|
| **Secrets Vault** | `~/.whitemagic/vault/secrets.db` | AES-256-GCM | ✅ Done |
| **Memory DB (hot)** | `~/.whitemagic/whitemagic.db` | SQLCipher (opt-in) | ✅ Done |
| **Memory DB (cold)** | `~/.whitemagic/memory/whitemagic.db` | SQLCipher (opt-in) | ✅ Done |
| **Galaxy DBs** | `~/.whitemagic/memory/galaxies/{name}/` | SQLCipher (opt-in) | ✅ Done |
| **Karma Ledger** | `~/.whitemagic/dharma/` | SHA-256 hash chain | ✅ Done |
| **Audit Logs** | `~/.whitemagic/audit/` | Merkle root anchoring | ✅ Done |
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

Implemented in two layers:

1. **`core/memory/db_manager.py`** — `ConnectionPool._create_connection()` checks `WM_DB_PASSPHRASE` and uses `sqlcipher3` when available. All memory databases (hot, cold, galaxy) inherit this automatically.

2. **`core/memory/encrypted_db.py`** — Unified encryption manager with:
   - OS keychain integration (macOS Keychain, GNOME Keyring, KDE Wallet)
   - Passphrase strength validation (min 12 chars, Shannon entropy ≥ 2.5 bits/char)
   - Migration tools: `encrypt_database()`, `decrypt_database()`, `rekey_database()`
   - Status reporting: `encryption_status()` scans all data stores
   - Secure memory clearing via `ctypes.memset` for passphrase buffers

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

All commands implemented in `cli/cli_app.py`:

```bash
wm vault init                  # Initialize vault with passphrase       ✅
wm vault set KEY VALUE         # Store a secret                         ✅
wm vault get KEY               # Retrieve a secret                      ✅
wm vault list                  # List stored keys (not values)          ✅
wm vault delete KEY            # Remove a secret                        ✅
wm vault lock                  # Clear cached passphrase from keychain  ✅
wm vault rekey                 # Change master passphrase               ✅
wm vault status                # Show encryption status of all stores   ✅
wm vault encrypt-db [NAME]     # Encrypt a database with SQLCipher      ✅
wm vault decrypt-db [NAME]     # Decrypt back to plaintext              ✅
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
| `sqlcipher3` | SQLCipher Python bindings | Yes — falls back to plaintext |
| `keyring` | OS keychain access | Yes — falls back to env var |
| `cryptography` | AES-GCM for vault (already used) | Yes — falls back to HMAC |

### pyproject.toml extras

```toml
[project.optional-dependencies]
encrypt = ["sqlcipher3>=0.5.0"]  # Already in pyproject.toml
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
3. ✅ SQLCipher for all DBs (Done — `core/memory/db_manager.py` + `encrypted_db.py`)
4. ✅ Vault CLI commands (Done — `cli/cli_app.py`: init/set/get/list/delete/rekey/lock/status/encrypt-db/decrypt-db)
5. ✅ OS keychain integration (Done — `encrypted_db.py` + `vault.py` both use `keyring`)
6. ✅ Galaxy DB encryption (Done — all pools use `ConnectionPool` which auto-enables SQLCipher)
7. ✅ Cold DB encryption (Done — same mechanism as hot DB)
8. ✅ Audit log immutability (Done — `dharma/karma_ledger.py`: SHA-256 hash chain + Merkle tree roots)
