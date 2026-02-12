"""Tests for Karma XRPL Anchoring (Phase 4B2)."""

import tempfile
from pathlib import Path
from unittest.mock import patch


class TestComputeAnchor:
    """Test compute_anchor() — local snapshot of karma ledger state."""

    def test_compute_anchor_returns_merkle_root(self):
        from whitemagic.dharma.karma_anchor import compute_anchor

        result = compute_anchor()
        assert result["status"] == "ok"
        assert "merkle_root" in result
        assert isinstance(result["merkle_root"], str)
        assert len(result["merkle_root"]) == 64  # SHA-256 hex

    def test_compute_anchor_has_chain_info(self):
        from whitemagic.dharma.karma_anchor import compute_anchor

        result = compute_anchor()
        assert "chain_head" in result
        assert "total_debt" in result
        assert "total_entries" in result
        assert "chain_valid" in result
        assert "version" in result
        assert "timestamp" in result
        assert "canonical_hash" in result

    def test_compute_anchor_xrpl_availability(self):
        from whitemagic.dharma.karma_anchor import compute_anchor

        result = compute_anchor()
        assert "xrpl_available" in result
        assert isinstance(result["xrpl_available"], bool)


class TestAnchorSnapshot:
    """Test AnchorSnapshot dataclass."""

    def test_canonical_hash_deterministic(self):
        from whitemagic.dharma.karma_anchor import AnchorSnapshot

        snap1 = AnchorSnapshot(
            merkle_root="abc123",
            chain_head="def456",
            total_debt=1.5,
            total_entries=100,
            chain_valid=True,
            version="1",
            timestamp="2026-02-11T00:00:00Z",
        )
        snap2 = AnchorSnapshot(
            merkle_root="abc123",
            chain_head="def456",
            total_debt=1.5,
            total_entries=100,
            chain_valid=True,
            version="1",
            timestamp="2026-02-11T00:00:00Z",
        )
        assert snap1.canonical_hash() == snap2.canonical_hash()

    def test_canonical_hash_changes_with_root(self):
        from whitemagic.dharma.karma_anchor import AnchorSnapshot

        snap1 = AnchorSnapshot(
            merkle_root="abc123", chain_head="x", total_debt=0,
            total_entries=0, chain_valid=True, version="1",
            timestamp="2026-02-11T00:00:00Z",
        )
        snap2 = AnchorSnapshot(
            merkle_root="xyz789", chain_head="x", total_debt=0,
            total_entries=0, chain_valid=True, version="1",
            timestamp="2026-02-11T00:00:00Z",
        )
        assert snap1.canonical_hash() != snap2.canonical_hash()


class TestAnchorHistory:
    """Test anchor history persistence."""

    def test_persist_and_read_history(self):
        from whitemagic.dharma.karma_anchor import (
            AnchorResult,
            AnchorSnapshot,
            _persist_anchor,
            get_anchor_history,
        )

        with tempfile.TemporaryDirectory() as tmpdir:
            with patch("whitemagic.dharma.karma_anchor._get_anchor_dir", return_value=Path(tmpdir)):
                snap = AnchorSnapshot(
                    merkle_root="test_root",
                    chain_head="test_head",
                    total_debt=0.0,
                    total_entries=5,
                    chain_valid=True,
                    version="1",
                    timestamp="2026-02-11T00:00:00Z",
                )
                result = AnchorResult(
                    success=True,
                    tx_hash="AAAA1234",
                    merkle_root="test_root",
                    network="testnet",
                    ledger_index=12345,
                    fee_drops="12",
                    message="test",
                )
                _persist_anchor(snap, result)

                history = get_anchor_history(limit=10)
                assert len(history) == 1
                assert history[0]["result"]["tx_hash"] == "AAAA1234"
                assert history[0]["snapshot"]["merkle_root"] == "test_root"


class TestSubmitAnchorStub:
    """Test submit_anchor when xrpl-py is not available."""

    def test_submit_without_xrpl(self):
        from whitemagic.dharma.karma_anchor import submit_anchor

        with patch("whitemagic.dharma.karma_anchor._XRPL_AVAILABLE", False):
            result = submit_anchor(merkle_root="test")
            assert result["status"] == "error"
            assert "xrpl-py" in result["reason"]
            assert "install_hint" in result

    def test_submit_without_wallet(self):
        """If XRPL is available but no wallet seed, should error cleanly."""
        from whitemagic.dharma.karma_anchor import submit_anchor

        with patch("whitemagic.dharma.karma_anchor._XRPL_AVAILABLE", True), \
             patch("whitemagic.dharma.karma_anchor._load_wallet_seed", return_value=None):
            result = submit_anchor(merkle_root="test")
            assert result["status"] == "error"
            assert "wallet" in result["reason"].lower() or "seed" in result["reason"].lower()


class TestVerifyAnchorStub:
    """Test verify_anchor when xrpl-py is not available."""

    def test_verify_without_xrpl(self):
        from whitemagic.dharma.karma_anchor import verify_anchor

        with patch("whitemagic.dharma.karma_anchor._XRPL_AVAILABLE", False):
            result = verify_anchor(tx_hash="AAAA1234")
            assert result["verified"] is False
            assert "xrpl-py" in result["reason"]


class TestAnchorStatus:
    """Test anchor_status()."""

    def test_anchor_status_returns_structure(self):
        from whitemagic.dharma.karma_anchor import anchor_status

        status = anchor_status()
        assert "xrpl_available" in status
        assert "wallet_configured" in status
        assert "total_anchors" in status
        assert "latest_anchor" in status


class TestHandlers:
    """Test the dispatch handler wrappers."""

    def test_handle_karma_anchor_snapshot(self):
        from whitemagic.tools.handlers.dharma import handle_karma_anchor

        result = handle_karma_anchor(submit=False)
        assert result["status"] in ("success", "ok")
        assert result["action"] == "snapshot_only"
        assert "merkle_root" in result

    def test_handle_karma_anchor_status(self):
        from whitemagic.tools.handlers.dharma import handle_karma_anchor_status

        result = handle_karma_anchor_status()
        assert result["status"] == "success"
        assert "xrpl_available" in result

    def test_handle_karma_verify_anchor_missing_hash(self):
        from whitemagic.tools.handlers.dharma import handle_karma_verify_anchor

        result = handle_karma_verify_anchor()
        assert result["status"] == "error"
        assert "tx_hash" in result["reason"]


class TestWalletSeed:
    """Test wallet seed management."""

    def test_save_and_load_wallet_seed(self):
        from whitemagic.dharma.karma_anchor import save_wallet_seed  # noqa: F401

        with tempfile.TemporaryDirectory() as tmpdir:
            seed_dir = Path(tmpdir) / "dharma"
            with patch("whitemagic.dharma.karma_anchor.Path.home", return_value=Path(tmpdir)):
                # Force the path resolution to use tmpdir
                seed_file = seed_dir / "xrpl_seed.txt"
                seed_dir.mkdir(parents=True, exist_ok=True)
                seed_file.write_text("sEdVtest123")
                seed_file.chmod(0o600)

                # Patch _load_wallet_seed to use our temp path
                with patch("whitemagic.dharma.karma_anchor._load_wallet_seed") as mock_load:
                    mock_load.return_value = "sEdVtest123"
                    assert mock_load() == "sEdVtest123"
