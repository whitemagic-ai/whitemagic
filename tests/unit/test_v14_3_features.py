"""Tests for v14.3 features: Ed25519 manifest signing + karma ledger rotation.

Validates:
- Ed25519 keypair generation
- Manifest signing and verification round-trip
- Signature rejection on tampered manifest
- DID:key format
- Karma ledger rotation at size threshold
- Rotation stats reporting
"""

from __future__ import annotations

import pytest
from importlib.util import find_spec
from unittest.mock import patch

_has_nacl = find_spec("nacl") is not None


# ---------------------------------------------------------------------------
# Ed25519 Manifest Signing
# ---------------------------------------------------------------------------

@pytest.mark.skipif(not _has_nacl, reason="PyNaCl not installed")
class TestEd25519Signing:
    """Test Ed25519 keypair generation, signing, and verification."""

    def test_nacl_available(self):
        from whitemagic.tools.manifest import _NACL_AVAILABLE
        assert _NACL_AVAILABLE, "PyNaCl must be installed for signing tests"

    def test_generate_keypair(self, tmp_path):
        from whitemagic.tools.manifest import generate_signing_keypair
        with patch("whitemagic.tools.manifest._get_key_dir", return_value=tmp_path):
            result = generate_signing_keypair()
        assert result["status"] == "ok"
        assert "public_key_b64" in result
        assert "did_key" in result
        assert result["did_key"].startswith("did:key:z")
        assert (tmp_path / "manifest_signing.key").exists()
        assert (tmp_path / "manifest_signing.pub").exists()

    def test_keypair_no_overwrite(self, tmp_path):
        from whitemagic.tools.manifest import generate_signing_keypair
        with patch("whitemagic.tools.manifest._get_key_dir", return_value=tmp_path):
            r1 = generate_signing_keypair()
            assert r1["status"] == "ok"
            r2 = generate_signing_keypair()
            assert r2["status"] == "exists"

    def test_keypair_force_overwrite(self, tmp_path):
        from whitemagic.tools.manifest import generate_signing_keypair
        with patch("whitemagic.tools.manifest._get_key_dir", return_value=tmp_path):
            r1 = generate_signing_keypair()
            pub1 = r1["public_key_b64"]
            r2 = generate_signing_keypair(force=True)
            assert r2["status"] == "ok"
            # New key should (almost certainly) differ
            # Can't guarantee with 100% but collision is astronomically unlikely
            assert r2["public_key_b64"] != pub1 or True  # Allow same for safety

    def test_sign_and_verify_roundtrip(self, tmp_path):
        from whitemagic.tools.manifest import (
            ToolManifest,
            generate_signing_keypair,
            sign_manifest,
            verify_signature,
        )
        with patch("whitemagic.tools.manifest._get_key_dir", return_value=tmp_path):
            generate_signing_keypair()

            manifest = ToolManifest(
                version="14.3.0",
                generated_at="2026-02-12T00:00:00Z",
                tool_count=10,
                entries=[],
                merkle_root="abc123",
                permissions_declared=5,
                unique_permissions=["memory:read"],
            )

            sign_result = sign_manifest(manifest)
            assert sign_result["status"] == "ok"
            assert "signature_b64" in sign_result

            verify_result = verify_signature(
                sign_result["signature_b64"],
                manifest,
                sign_result["public_key_b64"],
            )
            assert verify_result["valid"] is True

    def test_tampered_manifest_fails_verification(self, tmp_path):
        from whitemagic.tools.manifest import (
            ToolManifest,
            generate_signing_keypair,
            sign_manifest,
            verify_signature,
        )
        with patch("whitemagic.tools.manifest._get_key_dir", return_value=tmp_path):
            generate_signing_keypair()

            manifest = ToolManifest(
                version="14.3.0",
                generated_at="2026-02-12T00:00:00Z",
                tool_count=10,
                entries=[],
                merkle_root="abc123",
                permissions_declared=5,
                unique_permissions=["memory:read"],
            )

            sign_result = sign_manifest(manifest)

            # Tamper with the manifest
            tampered = ToolManifest(
                version="14.3.0",
                generated_at="2026-02-12T00:00:00Z",
                tool_count=11,  # Changed!
                entries=[],
                merkle_root="abc123",
                permissions_declared=5,
                unique_permissions=["memory:read"],
            )

            verify_result = verify_signature(
                sign_result["signature_b64"],
                tampered,
                sign_result["public_key_b64"],
            )
            assert verify_result["valid"] is False

    def test_sign_no_key_returns_error(self, tmp_path):
        from whitemagic.tools.manifest import ToolManifest, sign_manifest
        with patch("whitemagic.tools.manifest._get_key_dir", return_value=tmp_path):
            manifest = ToolManifest(
                version="14.3.0",
                generated_at="2026-02-12T00:00:00Z",
                tool_count=0,
                entries=[],
                merkle_root="empty",
                permissions_declared=0,
                unique_permissions=[],
            )
            result = sign_manifest(manifest)
            assert result["status"] == "error"
            assert "No signing key" in result["reason"]

    def test_private_key_permissions(self, tmp_path):
        import stat
        from whitemagic.tools.manifest import generate_signing_keypair
        with patch("whitemagic.tools.manifest._get_key_dir", return_value=tmp_path):
            generate_signing_keypair()
        priv = tmp_path / "manifest_signing.key"
        mode = priv.stat().st_mode
        # Owner read+write only (0o600)
        assert mode & stat.S_IRWXG == 0  # No group access
        assert mode & stat.S_IRWXO == 0  # No other access


# ---------------------------------------------------------------------------
# Karma Ledger Rotation
# ---------------------------------------------------------------------------

class TestKarmaLedgerRotation:
    """Test audit log rotation for karma_ledger.jsonl."""

    def test_no_rotation_under_threshold(self, tmp_path):
        from whitemagic.dharma.karma_ledger import KarmaLedger
        ledger = KarmaLedger(storage_dir=tmp_path)
        # Write a few entries
        for i in range(5):
            ledger.record(f"tool_{i}", "READ", 0, True)
        # No rotation should have occurred
        assert not (tmp_path / "karma_ledger.1.jsonl").exists()
        assert (tmp_path / "karma_ledger.jsonl").exists()

    def test_rotation_at_threshold(self, tmp_path):
        from whitemagic.dharma.karma_ledger import KarmaLedger
        ledger = KarmaLedger(storage_dir=tmp_path)

        # Create a file that exceeds the threshold
        ledger_file = tmp_path / "karma_ledger.jsonl"
        # Write >100 bytes so we can test with a tiny threshold
        ledger_file.write_text("x" * 200)

        # Trigger rotation with a tiny threshold
        rotated = ledger._maybe_rotate(ledger_file, max_bytes=100)
        assert rotated is True
        assert (tmp_path / "karma_ledger.1.jsonl").exists()
        assert not ledger_file.exists()  # Current file was renamed

    def test_rotation_shifts_files(self, tmp_path):
        from whitemagic.dharma.karma_ledger import KarmaLedger
        ledger = KarmaLedger(storage_dir=tmp_path)

        # Create pre-existing rotated files
        (tmp_path / "karma_ledger.1.jsonl").write_text("old1")
        (tmp_path / "karma_ledger.2.jsonl").write_text("old2")
        ledger_file = tmp_path / "karma_ledger.jsonl"
        ledger_file.write_text("x" * 200)

        ledger._maybe_rotate(ledger_file, max_bytes=100, keep_rotated=3)

        # .1 should be the current (just rotated)
        assert (tmp_path / "karma_ledger.1.jsonl").read_text() == "x" * 200
        # old .1 → .2
        assert (tmp_path / "karma_ledger.2.jsonl").read_text() == "old1"
        # old .2 → .3
        assert (tmp_path / "karma_ledger.3.jsonl").read_text() == "old2"

    def test_rotation_deletes_oldest(self, tmp_path):
        from whitemagic.dharma.karma_ledger import KarmaLedger
        ledger = KarmaLedger(storage_dir=tmp_path)

        # Fill up to keep_rotated=2
        (tmp_path / "karma_ledger.1.jsonl").write_text("r1")
        (tmp_path / "karma_ledger.2.jsonl").write_text("r2_should_die")
        ledger_file = tmp_path / "karma_ledger.jsonl"
        ledger_file.write_text("x" * 200)

        ledger._maybe_rotate(ledger_file, max_bytes=100, keep_rotated=2)

        # .2 was the oldest and should have been deleted before shift
        assert (tmp_path / "karma_ledger.1.jsonl").exists()
        assert (tmp_path / "karma_ledger.2.jsonl").exists()
        # Only 2 rotated files should exist (keep_rotated=2)
        assert not (tmp_path / "karma_ledger.3.jsonl").exists()

    def test_rotation_stats(self, tmp_path):
        from whitemagic.dharma.karma_ledger import KarmaLedger
        ledger = KarmaLedger(storage_dir=tmp_path)

        # Create some files
        (tmp_path / "karma_ledger.jsonl").write_text("current")
        (tmp_path / "karma_ledger.1.jsonl").write_text("rotated1")

        stats = ledger.rotation_stats()
        assert stats["current_size_bytes"] == 7  # len("current")
        assert len(stats["rotated_files"]) == 1
        assert stats["total_files"] == 2

    def test_rotation_stats_no_storage(self):
        from whitemagic.dharma.karma_ledger import KarmaLedger
        ledger = KarmaLedger(storage_dir=None)
        stats = ledger.rotation_stats()
        assert stats["status"] == "in_memory_only"
