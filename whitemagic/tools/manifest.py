"""Tool Manifest — Cryptographic Trust for WhiteMagic (Leap 9a).
================================================================
Generates and verifies a signed manifest of all registered tools,
including:
- Permission scope declarations per tool
- SHA-256 integrity hashes of handler source files
- Merkle tree root over the entire manifest

The manifest is the foundation for Leap 9's Cryptographic Trust:
agents can verify that the tools they're calling haven't been
tampered with and only request the permissions they declare.

Usage:
    from whitemagic.tools.manifest import generate_manifest, verify_manifest
    manifest = generate_manifest()
    ok, errors = verify_manifest(manifest)
"""

from __future__ import annotations

import base64
import hashlib
import json
import logging
import time
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)

try:
    from nacl.signing import SigningKey, VerifyKey
    from nacl.encoding import RawEncoder
    _NACL_AVAILABLE = True
except ImportError:
    _NACL_AVAILABLE = False

# ---------------------------------------------------------------------------
# Permission Scopes (Leap 9: capability declarations)
# ---------------------------------------------------------------------------

# Standard permission scopes that tools can declare
PERMISSION_SCOPES = {
    "memory:read":     "Read from memory stores (hot/cold DB)",
    "memory:write":    "Create or update memories",
    "memory:delete":   "Delete or archive memories",
    "fs:read":         "Read files from the filesystem",
    "fs:write":        "Write files to the filesystem",
    "net:outbound":    "Make outbound network requests",
    "net:listen":      "Listen for inbound connections",
    "exec:subprocess": "Spawn subprocesses",
    "exec:eval":       "Evaluate dynamic code",
    "gpu:compute":     "Use GPU compute resources",
    "state:read":      "Read system state (harmony, karma, etc.)",
    "state:write":     "Modify system state",
    "governance:read": "Read governance rules and scores",
    "governance:write":"Modify governance rules",
    "crypto:sign":     "Sign data with system keys",
    "crypto:verify":   "Verify signatures",
    "broker:publish":  "Publish to message broker",
    "broker:subscribe":"Subscribe to message broker topics",
    "agent:register":  "Register/deregister agents",
    "agent:trust":     "Modify agent trust levels",
}

# Auto-derive permissions from ToolSafety + ToolCategory
_SAFETY_PERMISSIONS: dict[str, tuple[str, ...]] = {
    "read":   ("memory:read", "state:read"),
    "write":  ("memory:read", "memory:write", "state:read", "state:write"),
    "delete": ("memory:read", "memory:write", "memory:delete", "state:read", "state:write"),
}

_CATEGORY_EXTRA_PERMISSIONS: dict[str, tuple[str, ...]] = {
    "broker":       ("broker:publish", "broker:subscribe"),
    "agent":        ("agent:register",),
    "governor":     ("governance:read", "governance:write"),
    "dharma":       ("governance:read",),
    "inference":    ("exec:subprocess",),
    "browser":      ("net:outbound", "fs:read"),
    "archaeology":  ("fs:read",),
    "edge":         ("exec:subprocess",),
    "system":       ("fs:read", "state:read"),
}


def derive_permissions(safety: str, category: str) -> tuple[str, ...]:
    """Auto-derive permission scopes from safety level and category."""
    perms = set(_SAFETY_PERMISSIONS.get(safety, ("state:read",)))
    perms.update(_CATEGORY_EXTRA_PERMISSIONS.get(category, ()))
    return tuple(sorted(perms))


# ---------------------------------------------------------------------------
# Manifest Entry
# ---------------------------------------------------------------------------

@dataclass
class ManifestEntry:
    """A single tool's entry in the signed manifest."""
    tool_name: str
    category: str
    safety: str
    permissions: tuple[str, ...]
    handler_hash: str       # SHA-256 of the handler source file
    schema_hash: str        # SHA-256 of the input_schema JSON
    entry_hash: str = ""    # SHA-256 of this entry (for Merkle tree)

    def compute_entry_hash(self) -> str:
        """Compute the deterministic hash of this entry."""
        canonical = json.dumps({
            "tool_name": self.tool_name,
            "category": self.category,
            "safety": self.safety,
            "permissions": list(self.permissions),
            "handler_hash": self.handler_hash,
            "schema_hash": self.schema_hash,
        }, sort_keys=True)
        self.entry_hash = hashlib.sha256(canonical.encode()).hexdigest()
        return self.entry_hash


# ---------------------------------------------------------------------------
# Merkle Tree
# ---------------------------------------------------------------------------

def _merkle_root(hashes: list[str]) -> str:
    """Compute Merkle root from a list of hex hashes."""
    if not hashes:
        return hashlib.sha256(b"empty").hexdigest()
    if len(hashes) == 1:
        return hashes[0]

    # Pad to even length
    if len(hashes) % 2 != 0:
        hashes.append(hashes[-1])

    next_level = []
    for i in range(0, len(hashes), 2):
        combined = hashes[i] + hashes[i + 1]
        next_level.append(hashlib.sha256(combined.encode()).hexdigest())

    return _merkle_root(next_level)


# ---------------------------------------------------------------------------
# Handler Hash Computation
# ---------------------------------------------------------------------------

def _hash_file(path: Path) -> str:
    """SHA-256 hash of a file's contents."""
    try:
        return hashlib.sha256(path.read_bytes()).hexdigest()
    except Exception:
        return "file_not_found"


def _find_handler_file(tool_name: str) -> Path | None:
    """Locate the handler source file for a tool."""
    # Tools are dispatched through handlers in whitemagic/tools/handlers/
    handlers_dir = Path(__file__).parent / "handlers"
    if not handlers_dir.exists():
        return None

    # Map tool name prefixes to handler files
    prefix_map = {
        "session": "session.py",
        "create_memory": "memory.py", "update_memory": "memory.py",
        "delete_memory": "memory.py", "read_memory": "memory.py",
        "search_memories": "memory.py", "list_memories": "memory.py",
        "fast_read": "memory.py", "batch_read": "memory.py",
        "import_memories": "export_import.py",
        "export_memories": "export_import.py",
        "gnosis": "introspection.py", "capabilities": "introspection.py",
        "manifest": "introspection.py", "explain": "introspection.py",
        "capability": "introspection.py",
        "evaluate_ethics": "ethics.py", "check_boundaries": "ethics.py",
        "harmony_vector": "ethics.py", "get_ethical": "ethics.py",
        "get_dharma": "ethics.py",
        "dream": "dreaming.py",
        "grimoire": "grimoire.py",
        "archaeology": "archaeology.py",
        "pattern": "patterns.py", "cluster": "patterns.py",
        "kaizen": "synthesis.py", "ensemble": "synthesis.py",
        "reasoning": "synthesis.py",
        "health": "misc.py", "ship": "misc.py", "state": "misc.py",
        "rust_": "misc.py",
        "swarm": "swarm.py",
        "vote": "voting.py",
        "agent": "agents.py",
        "broker": "broker.py",
        "pipeline": "pipeline.py",
        "ollama": "ollama.py",
        "edge_": "edge.py", "bitnet": "bitnet.py",
        "vector": "vector_search.py",
        "karma": "karma.py",
        "dharma": "dharma.py",
        "track_metric": "metrics.py", "get_metrics": "metrics.py",
        "record_yin": "metrics.py", "get_yin": "metrics.py",
        "galactic": "galactic.py",
        "garden": "garden.py",
        "task": "task.py",
        "mesh": "mesh.py",
        "prompt": "prompts.py",
        "sandbox": "sandbox.py",
        "sangha": "sangha.py",
        "scratchpad": "scratchpad.py",
        "context": "context.py",
        "homeostasis": "homeostasis.py",
        "maturity": "maturity.py",
        "tool.graph": "graph.py",
        "learning": "learning.py",
        "rate_limiter": "rate_limiter.py",
        "governor": "governor.py",
        "anomaly": "anomaly.py",
        "otel": "otel.py",
        "salience": "salience.py",
        "selfmodel": "selfmodel.py",
        "drive": "drive.py",
        "starter": "starters.py",
        "worker": "worker.py",
        "set_dharma": "dharma.py",
        "simd": "simd.py",
        "execute_cascade": "cascade.py",
        "list_cascade": "cascade.py",
        "view_hologram": "hologram.py",
        "solve_optimization": "solver.py",
        "serendipity": "serendipity.py",
        "memory.lifecycle": "lifecycle.py",
        "memory.retention": "lifecycle.py",
        "memory.consolidat": "consolidation.py",
    }

    name_lower = tool_name.lower()
    for prefix, filename in prefix_map.items():
        if name_lower.startswith(prefix):
            candidate = handlers_dir / filename
            if candidate.exists():
                return candidate

    # Fallback: check misc.py
    misc = handlers_dir / "misc.py"
    return misc if misc.exists() else None


# ---------------------------------------------------------------------------
# Manifest Generation
# ---------------------------------------------------------------------------

@dataclass
class ToolManifest:
    """Complete signed manifest of all registered tools."""
    version: str
    generated_at: str
    tool_count: int
    entries: list[ManifestEntry]
    merkle_root: str
    permissions_declared: int
    unique_permissions: list[str]

    def to_dict(self) -> dict[str, Any]:
        return {
            "version": self.version,
            "generated_at": self.generated_at,
            "tool_count": self.tool_count,
            "merkle_root": self.merkle_root,
            "permissions_declared": self.permissions_declared,
            "unique_permissions": self.unique_permissions,
            "entries": [asdict(e) for e in self.entries],
        }


def generate_manifest() -> ToolManifest:
    """Generate a complete tool manifest with hashes and Merkle root.

    Reads the tool registry, computes SHA-256 hashes of handler files,
    derives permission scopes, and builds a Merkle tree.
    """
    from whitemagic.tools.registry import get_all_tools

    tools = get_all_tools()
    entries: list[ManifestEntry] = []
    all_perms: set[str] = set()

    for td in tools:
        # Derive permissions if not explicitly set
        perms = td.permissions if td.permissions else derive_permissions(
            td.safety.value, td.category.value
        )
        all_perms.update(perms)

        # Hash handler source file
        handler_path = _find_handler_file(td.name)
        handler_hash = _hash_file(handler_path) if handler_path else "no_handler"

        # Hash input schema
        schema_json = json.dumps(td.input_schema, sort_keys=True)
        schema_hash = hashlib.sha256(schema_json.encode()).hexdigest()

        entry = ManifestEntry(
            tool_name=td.name,
            category=td.category.value,
            safety=td.safety.value,
            permissions=perms,
            handler_hash=handler_hash,
            schema_hash=schema_hash,
        )
        entry.compute_entry_hash()
        entries.append(entry)

    # Sort entries deterministically
    entries.sort(key=lambda e: e.tool_name)

    # Compute Merkle root
    entry_hashes = [e.entry_hash for e in entries]
    root = _merkle_root(entry_hashes)

    # Read version
    try:
        version = Path(__file__).parent.parent.parent.joinpath("VERSION").read_text().strip()
    except Exception:
        version = "unknown"

    return ToolManifest(
        version=version,
        generated_at=time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        tool_count=len(entries),
        entries=entries,
        merkle_root=root,
        permissions_declared=sum(len(e.permissions) for e in entries),
        unique_permissions=sorted(all_perms),
    )


# ---------------------------------------------------------------------------
# Manifest Verification
# ---------------------------------------------------------------------------

def verify_manifest(manifest: ToolManifest) -> tuple[bool, list[str]]:
    """Verify a manifest against the current codebase.

    Checks:
    1. All entry hashes are self-consistent
    2. Merkle root matches recomputed tree
    3. Handler file hashes match current files on disk

    Returns (all_ok, list_of_error_strings).
    """
    errors: list[str] = []

    # 1. Recompute entry hashes
    for entry in manifest.entries:
        expected = entry.entry_hash
        entry.compute_entry_hash()
        if entry.entry_hash != expected:
            errors.append(f"Entry hash mismatch for {entry.tool_name}: "
                          f"stored={expected[:16]}... computed={entry.entry_hash[:16]}...")

    # 2. Recompute Merkle root
    entry_hashes = [e.entry_hash for e in sorted(manifest.entries, key=lambda e: e.tool_name)]
    recomputed_root = _merkle_root(entry_hashes)
    if recomputed_root != manifest.merkle_root:
        errors.append(f"Merkle root mismatch: stored={manifest.merkle_root[:16]}... "
                      f"computed={recomputed_root[:16]}...")

    # 3. Verify handler file hashes
    tampered = 0
    for entry in manifest.entries:
        if entry.handler_hash in ("no_handler", "file_not_found"):
            continue
        handler_path = _find_handler_file(entry.tool_name)
        if handler_path is None:
            continue
        current_hash = _hash_file(handler_path)
        if current_hash != entry.handler_hash:
            tampered += 1
            errors.append(f"Handler tampered: {entry.tool_name} "
                          f"(expected={entry.handler_hash[:16]}... "
                          f"current={current_hash[:16]}...)")

    if tampered > 0:
        errors.insert(0, f"INTEGRITY ALERT: {tampered} handler file(s) modified since manifest generation")

    return (len(errors) == 0, errors)


# ---------------------------------------------------------------------------
# MCP Tool Interface
# ---------------------------------------------------------------------------

def manifest_tool() -> dict[str, Any]:
    """Generate and return the tool manifest (MCP tool handler)."""
    manifest = generate_manifest()
    return {
        "status": "ok",
        "action": "manifest_generated",
        "version": manifest.version,
        "tool_count": manifest.tool_count,
        "merkle_root": manifest.merkle_root,
        "permissions_declared": manifest.permissions_declared,
        "unique_permissions": manifest.unique_permissions,
        "generated_at": manifest.generated_at,
    }


def manifest_verify_tool() -> dict[str, Any]:
    """Verify the current manifest against the codebase (MCP tool handler)."""
    manifest = generate_manifest()
    ok, errors = verify_manifest(manifest)
    return {
        "status": "ok" if ok else "warning",
        "action": "manifest_verified",
        "integrity_ok": ok,
        "error_count": len(errors),
        "errors": errors[:10],  # Cap at 10 to avoid huge responses
        "merkle_root": manifest.merkle_root,
        "tool_count": manifest.tool_count,
    }


# ---------------------------------------------------------------------------
# Ed25519 Cryptographic Signing (v14.3 — Phase 4a of V15 Strategy)
# ---------------------------------------------------------------------------

def _get_key_dir() -> Path:
    """Return the directory for signing keys (~/.whitemagic/keys/)."""
    try:
        from whitemagic.config.paths import WM_ROOT
        key_dir = WM_ROOT / "keys"
    except Exception:
        key_dir = Path.home() / ".whitemagic" / "keys"
    key_dir.mkdir(parents=True, exist_ok=True)
    return key_dir


def generate_signing_keypair(force: bool = False) -> dict[str, Any]:
    """Generate an Ed25519 signing keypair for manifest signing.

    Stores the private key at ~/.whitemagic/keys/manifest_signing.key
    and the public key at ~/.whitemagic/keys/manifest_signing.pub.

    Returns dict with public_key_b64, did_key, and key_path.
    The DID:key format follows did:key:z6Mk... (multicodec Ed25519-pub).
    """
    if not _NACL_AVAILABLE:
        return {"status": "error", "reason": "PyNaCl not installed (pip install pynacl)"}

    key_dir = _get_key_dir()
    priv_path = key_dir / "manifest_signing.key"
    pub_path = key_dir / "manifest_signing.pub"

    if priv_path.exists() and not force:
        return {
            "status": "exists",
            "message": "Keypair already exists. Use force=True to regenerate.",
            "public_key_path": str(pub_path),
        }

    signing_key = SigningKey.generate()
    verify_key = signing_key.verify_key

    # Store raw 32-byte keys
    priv_path.write_bytes(signing_key.encode())
    pub_path.write_bytes(verify_key.encode())
    # Restrict permissions on private key
    priv_path.chmod(0o600)

    pub_b64 = base64.urlsafe_b64encode(verify_key.encode()).decode()

    # DID:key format: multicodec prefix 0xed01 for Ed25519-pub + base58btc
    # Simplified: use base64url encoding with did:key:z prefix
    did_key = f"did:key:z{pub_b64}"

    logger.info(f"Generated Ed25519 keypair at {key_dir}")
    return {
        "status": "ok",
        "public_key_b64": pub_b64,
        "did_key": did_key,
        "private_key_path": str(priv_path),
        "public_key_path": str(pub_path),
    }


def _load_signing_key() -> Any | None:
    """Load the Ed25519 private signing key from disk."""
    if not _NACL_AVAILABLE:
        return None
    key_dir = _get_key_dir()
    priv_path = key_dir / "manifest_signing.key"
    if not priv_path.exists():
        return None
    try:
        return SigningKey(priv_path.read_bytes())
    except Exception as e:
        logger.warning(f"Failed to load signing key: {e}")
        return None


def _load_verify_key() -> Any | None:
    """Load the Ed25519 public verification key from disk."""
    if not _NACL_AVAILABLE:
        return None
    key_dir = _get_key_dir()
    pub_path = key_dir / "manifest_signing.pub"
    if not pub_path.exists():
        return None
    try:
        return VerifyKey(pub_path.read_bytes())
    except Exception as e:
        logger.warning(f"Failed to load verify key: {e}")
        return None


def sign_manifest(manifest: ToolManifest) -> dict[str, Any]:
    """Sign a manifest's Merkle root with the Ed25519 private key.

    Returns dict with signature_b64, merkle_root, public_key_b64, did_key,
    and signed_at timestamp. The signature covers:
        SHA-256(version || merkle_root || tool_count || generated_at)

    This provides non-repudiation: anyone with the public key can verify
    that this specific manifest was approved by the key holder.
    """
    if not _NACL_AVAILABLE:
        return {"status": "error", "reason": "PyNaCl not installed"}

    signing_key = _load_signing_key()
    if signing_key is None:
        return {
            "status": "error",
            "reason": "No signing key found. Run generate_signing_keypair() first.",
        }

    # Canonical message to sign
    message = (
        f"{manifest.version}|{manifest.merkle_root}|"
        f"{manifest.tool_count}|{manifest.generated_at}"
    ).encode()

    signed = signing_key.sign(message, encoder=RawEncoder)
    signature_b64 = base64.urlsafe_b64encode(signed.signature).decode()

    verify_key = signing_key.verify_key
    pub_b64 = base64.urlsafe_b64encode(verify_key.encode()).decode()
    did_key = f"did:key:z{pub_b64}"

    return {
        "status": "ok",
        "signature_b64": signature_b64,
        "merkle_root": manifest.merkle_root,
        "message_signed": message.decode(),
        "public_key_b64": pub_b64,
        "did_key": did_key,
        "signed_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "tool_count": manifest.tool_count,
        "version": manifest.version,
    }


def verify_signature(
    signature_b64: str,
    manifest: ToolManifest,
    public_key_b64: str | None = None,
) -> dict[str, Any]:
    """Verify an Ed25519 signature against a manifest.

    Args:
        signature_b64: Base64url-encoded Ed25519 signature.
        manifest: The ToolManifest to verify against.
        public_key_b64: Optional explicit public key. If None, loads from disk.

    Returns dict with valid (bool), message, and details.
    """
    if not _NACL_AVAILABLE:
        return {"valid": False, "reason": "PyNaCl not installed"}

    # Load or parse verify key
    if public_key_b64:
        try:
            pub_bytes = base64.urlsafe_b64decode(public_key_b64)
            verify_key = VerifyKey(pub_bytes)
        except Exception as e:
            return {"valid": False, "reason": f"Invalid public key: {e}"}
    else:
        loaded = _load_verify_key()
        if loaded is None:
            return {"valid": False, "reason": "No public key found on disk"}
        verify_key = loaded

    # Reconstruct the canonical message
    message = (
        f"{manifest.version}|{manifest.merkle_root}|"
        f"{manifest.tool_count}|{manifest.generated_at}"
    ).encode()

    try:
        sig_bytes = base64.urlsafe_b64decode(signature_b64)
        verify_key.verify(message, sig_bytes)
        return {
            "valid": True,
            "message": "Signature verified — manifest is authentic",
            "merkle_root": manifest.merkle_root,
            "tool_count": manifest.tool_count,
        }
    except Exception as e:
        return {
            "valid": False,
            "reason": f"Signature verification failed: {e}",
            "merkle_root": manifest.merkle_root,
        }


def manifest_sign_tool() -> dict[str, Any]:
    """Generate, sign, and return a manifest (MCP tool handler)."""
    manifest = generate_manifest()
    result = sign_manifest(manifest)
    if result.get("status") == "error":
        # Auto-generate keypair on first use
        if "No signing key" in result.get("reason", ""):
            keygen = generate_signing_keypair()
            if keygen.get("status") == "ok":
                result = sign_manifest(manifest)
    return result
