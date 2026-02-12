"""Model Signing Verification — Edgerunner Violet Security Layer
================================================================
Implements OpenSSF Model Signing (OMS)-compatible verification for
AI models loaded through Ollama, edge inference, or BitNet paths.

Maintains a manifest of known-good model hashes and verifies model
integrity before allowing inference.  Unsigned or tampered models
are flagged/blocked depending on the active Dharma profile.

Inspired by: OpenSSF OMS (June 2025), AIBoMGen (Ghent, Jan 2026),
Microsoft backdoor scanner (Feb 2026).

Usage:
    from whitemagic.security.model_signing import get_model_registry
    reg = get_model_registry()
    reg.register_model("phi-3-mini", sha256="abc123...", trust="verified")
    result = reg.verify_model("phi-3-mini")
"""

from __future__ import annotations

import hashlib
import json
import logging
import threading
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)


class ModelTrust:
    """Trust levels for model provenance."""

    VERIFIED = "verified"        # Cryptographically signed, known-good
    SELF_SIGNED = "self_signed"  # User-attested but no third-party verification
    UNSIGNED = "unsigned"        # No signature — use at your own risk
    BLOCKED = "blocked"          # Known-bad or tampered


@dataclass
class ModelManifest:
    """Signed manifest for a single AI model (OMS-compatible)."""

    model_name: str
    sha256: str                  # SHA-256 of model weights/file
    trust: str = ModelTrust.UNSIGNED
    signer: str = ""             # Who signed (e.g., "openssf", "user", "ollama")
    license: str = ""            # Model license (e.g., "apache-2.0", "llama-community")
    training_disclosure: str = ""  # Training data provenance summary
    safety_profile: str = ""     # Safety evaluation result (e.g., "cyberseceval-4-pass")
    registered_at: float = 0.0
    last_verified: float = 0.0
    verification_count: int = 0

    def to_dict(self) -> dict[str, Any]:
        return {
            "model_name": self.model_name,
            "sha256": self.sha256[:16] + "..." if len(self.sha256) > 16 else self.sha256,
            "sha256_full": self.sha256,
            "trust": self.trust,
            "signer": self.signer,
            "license": self.license,
            "training_disclosure": self.training_disclosure,
            "safety_profile": self.safety_profile,
            "registered_at": self.registered_at,
            "last_verified": self.last_verified,
            "verification_count": self.verification_count,
        }


class ModelSigningRegistry:
    """Registry of model manifests with verification support.

    Models can be registered with their SHA-256 hash and trust level.
    Before inference, ``verify_model()`` checks the model against its
    manifest and returns whether it should be allowed.
    """

    def __init__(self, storage_dir: Path | None = None):
        self._lock = threading.Lock()
        self._manifests: dict[str, ModelManifest] = {}
        self._storage_dir = storage_dir
        self._verification_log: list[dict[str, Any]] = []

        if self._storage_dir:
            self._storage_dir.mkdir(parents=True, exist_ok=True)
            self._load_manifests()

    def register_model(
        self,
        model_name: str,
        sha256: str,
        trust: str = ModelTrust.UNSIGNED,
        signer: str = "",
        license: str = "",
        training_disclosure: str = "",
        safety_profile: str = "",
    ) -> dict[str, Any]:
        """Register or update a model manifest."""
        manifest = ModelManifest(
            model_name=model_name,
            sha256=sha256,
            trust=trust,
            signer=signer,
            license=license,
            training_disclosure=training_disclosure,
            safety_profile=safety_profile,
            registered_at=time.time(),
        )
        with self._lock:
            self._manifests[model_name] = manifest

        self._persist()

        logger.info("Model manifest registered: %s (trust=%s)", model_name, trust)
        return {
            "status": "success",
            "message": f"Model '{model_name}' registered with trust={trust}",
            "manifest": manifest.to_dict(),
        }

    def verify_model(
        self,
        model_name: str,
        current_sha256: str = "",
    ) -> dict[str, Any]:
        """Verify a model against its registered manifest.

        Args:
            model_name: Name of the model (as used in Ollama/edge inference).
            current_sha256: Optional current hash to verify against manifest.
                           If empty, only checks if model is registered.
        """
        with self._lock:
            manifest = self._manifests.get(model_name)

        now = time.time()

        if manifest is None:
            result = {
                "verified": False,
                "trust": ModelTrust.UNSIGNED,
                "reason": f"Model '{model_name}' has no registered manifest. "
                         "Consider registering it with register_model().",
                "action": "warn",
            }
            self._log_verification(model_name, result)
            return {"status": "success", **result}

        if manifest.trust == ModelTrust.BLOCKED:
            result = {
                "verified": False,
                "trust": ModelTrust.BLOCKED,
                "reason": f"Model '{model_name}' is blocked — known-bad or tampered.",
                "action": "block",
            }
            self._log_verification(model_name, result)
            return {"status": "success", **result}

        if current_sha256 and manifest.sha256:
            if current_sha256 != manifest.sha256:
                result = {
                    "verified": False,
                    "trust": ModelTrust.BLOCKED,
                    "reason": (
                        f"Model '{model_name}' hash mismatch! "
                        f"Expected {manifest.sha256[:16]}..., "
                        f"got {current_sha256[:16]}... — possible tampering."
                    ),
                    "action": "block",
                    "expected_hash": manifest.sha256[:16],
                    "actual_hash": current_sha256[:16],
                }
                self._log_verification(model_name, result)
                return {"status": "success", **result}

        # Update verification stats
        with self._lock:
            manifest.last_verified = now
            manifest.verification_count += 1

        result = {
            "verified": True,
            "trust": manifest.trust,
            "reason": f"Model '{model_name}' verified (trust={manifest.trust})",
            "action": "allow",
            "manifest": manifest.to_dict(),
        }
        self._log_verification(model_name, result)
        return {"status": "success", **result}

    def list_models(self) -> dict[str, Any]:
        """List all registered model manifests."""
        with self._lock:
            models = [m.to_dict() for m in self._manifests.values()]
        return {
            "status": "success",
            "count": len(models),
            "models": models,
        }

    def hash_file(self, path: str) -> dict[str, Any]:
        """Compute SHA-256 hash of a model file on disk."""
        p = Path(path)
        if not p.exists():
            return {"status": "error", "error": f"File not found: {path}"}
        try:
            h = hashlib.sha256()
            with open(p, "rb") as f:
                while chunk := f.read(8192):
                    h.update(chunk)
            return {
                "status": "success",
                "path": str(p),
                "sha256": h.hexdigest(),
                "size_bytes": p.stat().st_size,
            }
        except Exception as e:
            return {"status": "error", "error": str(e)}

    def status(self) -> dict[str, Any]:
        """Return model signing subsystem status."""
        with self._lock:
            trust_counts: dict[str, int] = {}
            for m in self._manifests.values():
                trust_counts[m.trust] = trust_counts.get(m.trust, 0) + 1
            return {
                "registered_models": len(self._manifests),
                "trust_distribution": trust_counts,
                "recent_verifications": self._verification_log[-10:],
                "total_verifications": len(self._verification_log),
            }

    def _log_verification(self, model_name: str, result: dict[str, Any]) -> None:
        entry = {
            "model": model_name,
            "verified": result.get("verified", False),
            "trust": result.get("trust", "unknown"),
            "action": result.get("action", "unknown"),
            "timestamp": time.time(),
        }
        with self._lock:
            self._verification_log.append(entry)
            if len(self._verification_log) > 5000:
                self._verification_log = self._verification_log[-2500:]

    def _persist(self) -> None:
        if not self._storage_dir:
            return
        try:
            path = self._storage_dir / "model_manifests.json"
            with self._lock:
                data = {
                    name: {
                        "sha256": m.sha256,
                        "trust": m.trust,
                        "signer": m.signer,
                        "license": m.license,
                        "training_disclosure": m.training_disclosure,
                        "safety_profile": m.safety_profile,
                        "registered_at": m.registered_at,
                    }
                    for name, m in self._manifests.items()
                }
            path.write_text(json.dumps(data, indent=2), encoding="utf-8")
        except Exception as e:
            logger.debug("Model manifest persist failed: %s", e)

    def _load_manifests(self) -> None:
        if not self._storage_dir:
            return
        path = self._storage_dir / "model_manifests.json"
        if not path.exists():
            return
        try:
            data = json.loads(path.read_text(encoding="utf-8"))
            for name, md in data.items():
                self._manifests[name] = ModelManifest(
                    model_name=name,
                    sha256=md.get("sha256", ""),
                    trust=md.get("trust", ModelTrust.UNSIGNED),
                    signer=md.get("signer", ""),
                    license=md.get("license", ""),
                    training_disclosure=md.get("training_disclosure", ""),
                    safety_profile=md.get("safety_profile", ""),
                    registered_at=md.get("registered_at", 0.0),
                )
            logger.info("Model signing: loaded %d manifests", len(self._manifests))
        except Exception as e:
            logger.debug("Model manifest load failed: %s", e)


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_registry: ModelSigningRegistry | None = None
_registry_lock = threading.Lock()


def get_model_registry() -> ModelSigningRegistry:
    """Get the global Model Signing Registry instance."""
    global _registry
    if _registry is None:
        with _registry_lock:
            if _registry is None:
                try:
                    from whitemagic.config.paths import WM_ROOT
                    storage = WM_ROOT / "security"
                except Exception:
                    storage = None
                _registry = ModelSigningRegistry(storage_dir=storage)
    return _registry
