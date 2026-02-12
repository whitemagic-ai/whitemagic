"""Tests for Edgerunner Violet Security Layer.

Covers all 6 features:
  1. MCP Integrity Checking
  2. Crypto-chained Karma (ops_class)
  3. Violet Dharma Profile
  4. Model Signing Verification
  5. Scope-of-Engagement Tokens
  6. Security Circuit Breakers
"""

import hashlib
import time

import pytest


# =========================================================================
# 1. MCP Integrity Checking
# =========================================================================

class TestMcpIntegrity:
    """Test MCP tool registry fingerprinting and drift detection."""

    def _make_integrity(self):
        from whitemagic.security.mcp_integrity import McpIntegrity
        return McpIntegrity(storage_dir=None)

    def test_snapshot_captures_tools(self):
        integrity = self._make_integrity()
        result = integrity.snapshot()
        assert result["status"] == "success"
        assert result["tools_fingerprinted"] > 0
        assert "registry_hash" in result

    def test_verify_clean_after_snapshot(self):
        integrity = self._make_integrity()
        integrity.snapshot()
        result = integrity.verify()
        assert result["status"] == "success"
        assert result["verified"] is True
        assert result["integrity"] == "CLEAN"
        assert result["added"] == []
        assert result["removed"] == []
        assert result["modified"] == []

    def test_verify_without_baseline(self):
        integrity = self._make_integrity()
        result = integrity.verify()
        assert result["verified"] is False
        assert "No baseline" in result["message"]

    def test_status_empty(self):
        integrity = self._make_integrity()
        status = integrity.status()
        assert status["has_baseline"] is False
        assert status["baseline_tool_count"] == 0

    def test_status_after_snapshot(self):
        integrity = self._make_integrity()
        integrity.snapshot()
        status = integrity.status()
        assert status["has_baseline"] is True
        assert status["baseline_tool_count"] > 0

    def test_fingerprint_deterministic(self):
        integrity = self._make_integrity()
        snap1 = integrity.snapshot()
        snap2 = integrity.snapshot()
        assert snap1["registry_hash"] == snap2["registry_hash"]

    def test_verification_count_increments(self):
        integrity = self._make_integrity()
        integrity.snapshot()
        integrity.verify()
        integrity.verify()
        status = integrity.status()
        assert status["verification_count"] == 2


# =========================================================================
# 2. Crypto-chained Karma + Ops Classification
# =========================================================================

class TestKarmaOpsClass:
    """Test red-ops/blue-ops classification in Karma Ledger."""

    def _make_ledger(self):
        from whitemagic.dharma.karma_ledger import KarmaLedger
        return KarmaLedger(storage_dir=None)

    def test_record_with_ops_class(self):
        ledger = self._make_ledger()
        entry = ledger.record("nmap_scan", "READ", 0, True, ops_class="red-ops")
        assert entry.ops_class == "red-ops"

    def test_record_without_ops_class(self):
        ledger = self._make_ledger()
        entry = ledger.record("search_memories", "READ", 0, True)
        assert entry.ops_class == ""

    def test_to_dict_includes_ops_class(self):
        ledger = self._make_ledger()
        entry = ledger.record("fuzz_target", "WRITE", 1, True, ops_class="red-ops")
        d = entry.to_dict()
        assert d["ops_class"] == "red-ops"

    def test_to_dict_omits_empty_ops_class(self):
        ledger = self._make_ledger()
        entry = ledger.record("search_memories", "READ", 0, True)
        d = entry.to_dict()
        assert "ops_class" not in d

    def test_report_by_ops_red(self):
        ledger = self._make_ledger()
        ledger.record("nmap_scan", "READ", 0, True, ops_class="red-ops")
        ledger.record("fuzz_target", "WRITE", 1, True, ops_class="red-ops")
        ledger.record("anomaly_check", "READ", 0, True, ops_class="blue-ops")
        report = ledger.report_by_ops("red-ops")
        assert report["total_entries"] == 2
        assert report["ops_class"] == "red-ops"

    def test_report_by_ops_blue(self):
        ledger = self._make_ledger()
        ledger.record("nmap_scan", "READ", 0, True, ops_class="red-ops")
        ledger.record("anomaly_check", "READ", 0, True, ops_class="blue-ops")
        ledger.record("patch_vuln", "WRITE", 1, True, ops_class="blue-ops")
        report = ledger.report_by_ops("blue-ops")
        assert report["total_entries"] == 2
        assert report["ops_class"] == "blue-ops"

    def test_report_by_ops_unclassified(self):
        ledger = self._make_ledger()
        ledger.record("search_memories", "READ", 0, True)
        ledger.record("nmap_scan", "READ", 0, True, ops_class="red-ops")
        report = ledger.report_by_ops("")
        assert report["total_entries"] == 1
        assert report["ops_class"] == "unclassified"

    def test_chain_integrity_with_ops_class(self):
        ledger = self._make_ledger()
        ledger.record("tool_a", "READ", 0, True, ops_class="red-ops")
        ledger.record("tool_b", "WRITE", 1, True, ops_class="blue-ops")
        ledger.record("tool_c", "READ", 0, True)
        result = ledger.verify_chain()
        assert result["valid"] is True
        assert result["entries_checked"] == 3


# =========================================================================
# 3. Violet Dharma Profile
# =========================================================================

class TestVioletDharmaProfile:
    """Test the Violet Dharma profile rules."""

    def _make_engine(self):
        from whitemagic.dharma.rules import DharmaRulesEngine
        return DharmaRulesEngine(active_profile="violet")

    def test_violet_blocks_exploit(self):
        engine = self._make_engine()
        decision = engine.evaluate({"tool": "exploit_runner", "description": "run exploit"})
        assert decision.action.value == "block"
        assert "violet_require_engagement_token" in decision.triggered_rules

    def test_violet_blocks_pentest(self):
        engine = self._make_engine()
        decision = engine.evaluate({"tool": "pentest_scan", "description": "pentest target"})
        assert decision.action.value == "block"

    def test_violet_logs_blue_ops(self):
        engine = self._make_engine()
        decision = engine.evaluate({"tool": "anomaly_checker", "description": "scan for anomalies"})
        # Should match violet_log_blue_ops (log) AND potentially default rules
        triggered = decision.triggered_rules
        assert any("violet_log_blue_ops" in r or "blue" in r.lower() for r in triggered) or \
               decision.action.value in ("log", "tag")

    def test_violet_warns_model_load(self):
        engine = self._make_engine()
        decision = engine.evaluate({"tool": "ollama.generate", "description": "generate text"})
        assert "violet_warn_model_load" in decision.triggered_rules
        assert decision.action.value == "warn"

    def test_violet_blocks_exfiltration(self):
        engine = self._make_engine()
        decision = engine.evaluate({"tool": "send_data", "description": "exfiltrate sensitive data"})
        assert decision.action.value == "block"

    def test_violet_throttles_recon(self):
        engine = self._make_engine()
        decision = engine.evaluate({"tool": "network_tool", "description": "nmap scan network"})
        assert decision.action.value in ("throttle", "block", "warn")

    def test_violet_rules_listed(self):
        engine = self._make_engine()
        rules = engine.get_rules(profile="violet")
        names = [r["name"] for r in rules]
        assert "violet_require_engagement_token" in names
        assert "violet_log_blue_ops" in names
        assert "violet_warn_model_load" in names
        assert "violet_block_exfiltration" in names
        assert "violet_throttle_recon" in names

    def test_default_profile_does_not_trigger_violet(self):
        from whitemagic.dharma.rules import DharmaRulesEngine
        engine = DharmaRulesEngine(active_profile="default")
        decision = engine.evaluate({"tool": "exploit_runner", "description": "run exploit"})
        assert "violet_require_engagement_token" not in decision.triggered_rules


# =========================================================================
# 4. Model Signing Verification
# =========================================================================

class TestModelSigning:
    """Test model manifest registration and verification."""

    def _make_registry(self):
        from whitemagic.security.model_signing import ModelSigningRegistry
        return ModelSigningRegistry(storage_dir=None)

    def test_register_model(self):
        reg = self._make_registry()
        result = reg.register_model("phi-3-mini", sha256="abc123def456", trust="verified")
        assert result["status"] == "success"
        assert result["manifest"]["trust"] == "verified"

    def test_verify_registered_model(self):
        reg = self._make_registry()
        reg.register_model("phi-3-mini", sha256="abc123", trust="verified")
        result = reg.verify_model("phi-3-mini")
        assert result["verified"] is True
        assert result["trust"] == "verified"

    def test_verify_unregistered_model(self):
        reg = self._make_registry()
        result = reg.verify_model("unknown-model")
        assert result["verified"] is False
        assert result["trust"] == "unsigned"

    def test_verify_hash_match(self):
        reg = self._make_registry()
        reg.register_model("phi-3-mini", sha256="abc123", trust="verified")
        result = reg.verify_model("phi-3-mini", current_sha256="abc123")
        assert result["verified"] is True

    def test_verify_hash_mismatch(self):
        reg = self._make_registry()
        reg.register_model("phi-3-mini", sha256="abc123", trust="verified")
        result = reg.verify_model("phi-3-mini", current_sha256="TAMPERED")
        assert result["verified"] is False
        assert "mismatch" in result["reason"]

    def test_verify_blocked_model(self):
        reg = self._make_registry()
        reg.register_model("evil-model", sha256="xxx", trust="blocked")
        result = reg.verify_model("evil-model")
        assert result["verified"] is False
        assert result["trust"] == "blocked"

    def test_list_models(self):
        reg = self._make_registry()
        reg.register_model("model-a", sha256="aaa", trust="verified")
        reg.register_model("model-b", sha256="bbb", trust="self_signed")
        result = reg.list_models()
        assert result["count"] == 2

    def test_status(self):
        reg = self._make_registry()
        reg.register_model("model-a", sha256="aaa", trust="verified")
        reg.verify_model("model-a")
        status = reg.status()
        assert status["registered_models"] == 1
        assert status["trust_distribution"]["verified"] == 1
        assert status["total_verifications"] == 1

    def test_verification_count_increments(self):
        reg = self._make_registry()
        reg.register_model("model-a", sha256="aaa", trust="verified")
        reg.verify_model("model-a")
        reg.verify_model("model-a")
        result = reg.verify_model("model-a")
        assert result["manifest"]["verification_count"] == 3

    def test_hash_file_not_found(self):
        reg = self._make_registry()
        result = reg.hash_file("/nonexistent/path/model.bin")
        assert result["status"] == "error"


# =========================================================================
# 5. Scope-of-Engagement Tokens
# =========================================================================

class TestEngagementTokens:
    """Test engagement token issuance, validation, and revocation."""

    def _make_manager(self):
        from whitemagic.security.engagement_tokens import EngagementTokenManager
        return EngagementTokenManager(storage_dir=None)

    def test_issue_token(self):
        mgr = self._make_manager()
        result = mgr.issue(
            scope=["10.0.0.*"], tools=["nmap_*"], issuer="lucas",
            duration_minutes=60,
        )
        assert result["status"] == "success"
        assert result["token"]["valid"] is True
        assert result["token"]["issuer"] == "lucas"

    def test_validate_valid_token(self):
        mgr = self._make_manager()
        issued = mgr.issue(scope=["10.0.0.*"], tools=["nmap_*"], issuer="lucas")
        token_id = issued["token"]["token_id"]
        result = mgr.validate(token_id, tool="nmap_scan", target="10.0.0.1")
        assert result["valid"] is True

    def test_validate_wrong_tool(self):
        mgr = self._make_manager()
        issued = mgr.issue(scope=["10.0.0.*"], tools=["nmap_*"], issuer="lucas")
        token_id = issued["token"]["token_id"]
        result = mgr.validate(token_id, tool="exploit_runner", target="10.0.0.1")
        assert result["valid"] is False
        assert "not authorized" in result["reason"]

    def test_validate_out_of_scope(self):
        mgr = self._make_manager()
        issued = mgr.issue(scope=["10.0.0.*"], tools=["nmap_*"], issuer="lucas")
        token_id = issued["token"]["token_id"]
        result = mgr.validate(token_id, tool="nmap_scan", target="192.168.1.1")
        assert result["valid"] is False
        assert "outside" in result["reason"]

    def test_validate_nonexistent_token(self):
        mgr = self._make_manager()
        result = mgr.validate("evt_doesnotexist")
        assert result["valid"] is False

    def test_revoke_token(self):
        mgr = self._make_manager()
        issued = mgr.issue(scope=["*"], tools=["*"], issuer="lucas")
        token_id = issued["token"]["token_id"]
        revoke_result = mgr.revoke(token_id)
        assert revoke_result["status"] == "success"
        validate_result = mgr.validate(token_id)
        assert validate_result["valid"] is False
        assert "revoked" in validate_result["reason"]

    def test_expired_token(self):
        mgr = self._make_manager()
        issued = mgr.issue(scope=["*"], tools=["*"], issuer="lucas", duration_minutes=0)
        token_id = issued["token"]["token_id"]
        # Token with 0 duration expires immediately
        time.sleep(0.01)
        result = mgr.validate(token_id)
        assert result["valid"] is False
        assert "expired" in result["reason"].lower()

    def test_max_uses(self):
        mgr = self._make_manager()
        issued = mgr.issue(scope=["*"], tools=["*"], issuer="lucas", max_uses=2)
        token_id = issued["token"]["token_id"]
        mgr.validate(token_id)
        mgr.validate(token_id)
        result = mgr.validate(token_id)
        assert result["valid"] is False
        assert "max uses" in result["reason"].lower()

    def test_list_tokens(self):
        mgr = self._make_manager()
        mgr.issue(scope=["*"], tools=["*"], issuer="alice", duration_minutes=60)
        mgr.issue(scope=["*"], tools=["*"], issuer="bob", duration_minutes=60)
        result = mgr.list_tokens()
        assert result["count"] == 2

    def test_status(self):
        mgr = self._make_manager()
        mgr.issue(scope=["*"], tools=["*"], issuer="lucas", duration_minutes=60)
        status = mgr.status()
        assert status["active"] == 1
        assert status["total_tokens"] == 1

    def test_hmac_tamper_detection(self):
        mgr = self._make_manager()
        issued = mgr.issue(scope=["10.0.0.*"], tools=["nmap_*"], issuer="lucas")
        token_id = issued["token"]["token_id"]
        # Tamper with the token's scope
        with mgr._lock:
            mgr._tokens[token_id].scope = ["*"]  # widen scope
        result = mgr.validate(token_id, tool="nmap_scan", target="192.168.1.1")
        assert result["valid"] is False
        assert "tamper" in result["reason"].lower()


# =========================================================================
# 6. Security Circuit Breakers
# =========================================================================

class TestSecurityBreaker:
    """Test security anomaly detection patterns."""

    def _make_monitor(self, **kwargs):
        from whitemagic.security.security_breaker import SecurityMonitor
        return SecurityMonitor(**kwargs)

    def test_normal_call_no_alert(self):
        monitor = self._make_monitor()
        result = monitor.record_call("search_memories", safety="READ")
        assert result is None

    def test_rapid_fire_detection(self):
        monitor = self._make_monitor(rapid_fire_threshold=5, rapid_fire_window_s=10.0)
        alert = None
        for i in range(10):
            alert = monitor.record_call("shell_exec", safety="WRITE")
        assert alert is not None
        assert alert["pattern"] == "rapid_fire"
        assert alert["action"] in ("throttle", "block")

    def test_lateral_movement_detection(self):
        monitor = self._make_monitor(lateral_threshold=5, lateral_window_s=60.0)
        alert = None
        for i in range(10):
            alert = monitor.record_call(f"tool_{i}", safety="READ")
        assert alert is not None
        assert alert["pattern"] == "lateral_movement"

    def test_escalation_detection(self):
        monitor = self._make_monitor(escalation_window_s=60.0)
        monitor.record_call("read_data", safety="READ")
        monitor.record_call("write_data", safety="WRITE")
        alert = monitor.record_call("delete_data", safety="DELETE")
        assert alert is not None
        assert alert["pattern"] == "escalation"

    def test_mutation_burst_detection(self):
        monitor = self._make_monitor(escalation_window_s=60.0)
        alert = None
        for i in range(6):
            alert = monitor.record_call(f"write_tool_{i}", safety="WRITE")
        assert alert is not None
        assert alert["pattern"] == "mutation_burst"

    def test_get_alerts(self):
        monitor = self._make_monitor(rapid_fire_threshold=3, rapid_fire_window_s=60.0)
        for _ in range(5):
            monitor.record_call("hammer_tool", safety="WRITE")
        alerts = monitor.get_alerts()
        assert len(alerts) > 0
        assert alerts[-1]["pattern"] == "rapid_fire"

    def test_status(self):
        monitor = self._make_monitor()
        monitor.record_call("tool_a", safety="READ")
        monitor.record_call("tool_b", safety="WRITE")
        status = monitor.status()
        assert status["total_calls_monitored"] == 2
        assert "config" in status

    def test_reset(self):
        monitor = self._make_monitor()
        monitor.record_call("tool_a", safety="READ")
        monitor.reset()
        status = monitor.status()
        assert status["total_calls_monitored"] == 0

    def test_block_on_extreme_rapid_fire(self):
        monitor = self._make_monitor(rapid_fire_threshold=3, rapid_fire_window_s=60.0)
        alert = None
        for _ in range(7):
            alert = monitor.record_call("hammer", safety="WRITE")
        # At 2x threshold, should escalate to block
        assert alert is not None
        assert alert["action"] == "block"


# =========================================================================
# Handler Integration Tests
# =========================================================================

class TestVioletSecurityHandlers:
    """Test that all 15 handlers return valid responses."""

    def test_mcp_integrity_snapshot(self):
        from whitemagic.tools.handlers.violet_security import handle_mcp_integrity_snapshot
        result = handle_mcp_integrity_snapshot()
        assert result["status"] == "success"

    def test_mcp_integrity_verify(self):
        from whitemagic.tools.handlers.violet_security import (
            handle_mcp_integrity_snapshot,
            handle_mcp_integrity_verify,
        )
        handle_mcp_integrity_snapshot()
        result = handle_mcp_integrity_verify()
        assert result["status"] == "success"

    def test_mcp_integrity_status(self):
        from whitemagic.tools.handlers.violet_security import handle_mcp_integrity_status
        result = handle_mcp_integrity_status()
        assert result["status"] == "success"

    def test_model_register(self):
        from whitemagic.tools.handlers.violet_security import handle_model_register
        result = handle_model_register(model_name="test-model", sha256="abc123", trust="verified")
        assert result["status"] == "success"

    def test_model_register_missing_name(self):
        from whitemagic.tools.handlers.violet_security import handle_model_register
        result = handle_model_register(sha256="abc")
        assert result["status"] == "error"

    def test_model_verify(self):
        from whitemagic.tools.handlers.violet_security import handle_model_verify
        result = handle_model_verify(model_name="nonexistent")
        assert result["status"] == "success"
        assert result["verified"] is False

    def test_model_list(self):
        from whitemagic.tools.handlers.violet_security import handle_model_list
        result = handle_model_list()
        assert result["status"] == "success"

    def test_model_hash_missing_path(self):
        from whitemagic.tools.handlers.violet_security import handle_model_hash
        result = handle_model_hash()
        assert result["status"] == "error"

    def test_model_signing_status(self):
        from whitemagic.tools.handlers.violet_security import handle_model_signing_status
        result = handle_model_signing_status()
        assert result["status"] == "success"

    def test_engagement_issue(self):
        from whitemagic.tools.handlers.violet_security import handle_engagement_issue
        result = handle_engagement_issue(issuer="test", scope=["*"], tools=["*"])
        assert result["status"] == "success"

    def test_engagement_issue_missing_issuer(self):
        from whitemagic.tools.handlers.violet_security import handle_engagement_issue
        result = handle_engagement_issue(scope=["*"])
        assert result["status"] == "error"

    def test_engagement_validate(self):
        from whitemagic.tools.handlers.violet_security import handle_engagement_validate
        result = handle_engagement_validate(token_id="evt_fake")
        assert result["valid"] is False

    def test_engagement_revoke_nonexistent(self):
        from whitemagic.tools.handlers.violet_security import handle_engagement_revoke
        result = handle_engagement_revoke(token_id="evt_fake")
        assert result["status"] == "error"

    def test_engagement_list(self):
        from whitemagic.tools.handlers.violet_security import handle_engagement_list
        result = handle_engagement_list()
        assert result["status"] == "success"

    def test_engagement_status(self):
        from whitemagic.tools.handlers.violet_security import handle_engagement_status
        result = handle_engagement_status()
        assert result["status"] == "success"

    def test_security_alerts(self):
        from whitemagic.tools.handlers.violet_security import handle_security_alerts
        result = handle_security_alerts()
        assert result["status"] == "success"

    def test_security_monitor_status(self):
        from whitemagic.tools.handlers.violet_security import handle_security_monitor_status
        result = handle_security_monitor_status()
        assert result["status"] == "success"
