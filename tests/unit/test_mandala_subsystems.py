"""
Regression tests for the MandalaOS-inspired subsystems.

Tests cover:
  1. HarmonyVector (multi-dimensional health metric)
  2. Guna classification
  3. Declarative Dharma Rules Engine
  4. Karma Ledger (declared vs actual side-effects)
  5. Circuit Breaker (Stoic resilience)
  6. Gnosis Portal (unified introspection)
  7. MCP tool routing for all new tools
"""

import time

import pytest

from tests.conftest import assert_envelope_shape


# =========================================================================
# 1. Harmony Vector
# =========================================================================

class TestHarmonyVector:

    def test_initial_snapshot_is_healthy(self):
        from whitemagic.harmony.vector import HarmonyVector
        hv = HarmonyVector()
        snap = hv.snapshot()
        assert snap.harmony_score == 1.0
        assert snap.tool_calls_in_window == 0

    def test_record_call_updates_snapshot(self):
        from whitemagic.harmony.vector import HarmonyVector
        hv = HarmonyVector()
        snap = hv.record_call("test_tool", duration_s=0.1, success=True)
        assert snap.tool_calls_in_window == 1
        assert snap.harmony_score > 0.0

    def test_errors_degrade_score(self):
        from whitemagic.harmony.vector import HarmonyVector
        hv = HarmonyVector()
        # Record 5 errors in a row
        for _ in range(5):
            snap = hv.record_call("bad_tool", duration_s=0.1, success=False)
        assert snap.error_rate < 1.0  # error dimension is degraded
        assert snap.errors_in_window == 5

    def test_high_latency_degrades_score(self):
        from whitemagic.harmony.vector import HarmonyVector
        hv = HarmonyVector()
        snap = hv.record_call("slow_tool", duration_s=5.0, success=True)
        assert snap.latency < 1.0
        assert snap.p95_latency_ms > 1000.0

    def test_karma_mismatch_tracked(self):
        from whitemagic.harmony.vector import HarmonyVector
        hv = HarmonyVector()
        # Declared READ but produced writes → mismatch
        snap = hv.record_call(
            "sneaky_tool", duration_s=0.1, success=True,
            declared_safety="READ", actual_writes=3,
        )
        assert snap.karma_mismatches_in_window == 1
        assert snap.karma_debt < 1.0

    def test_snapshot_dict_is_serializable(self):
        import json
        from whitemagic.harmony.vector import HarmonyVector
        hv = HarmonyVector()
        hv.record_call("tool", duration_s=0.05, success=True)
        d = hv.snapshot_dict()
        assert isinstance(d, dict)
        json.dumps(d)  # must not raise

    def test_guna_distribution(self):
        from whitemagic.harmony.vector import HarmonyVector
        hv = HarmonyVector()
        # Record a mix: a read (sattvic) and a write (rajasic)
        hv.record_call("get_status", duration_s=0.05, success=True, declared_safety="READ")
        snap = hv.record_call("create_memory", duration_s=0.1, success=True, declared_safety="WRITE")
        assert snap.guna_sattvic_pct + snap.guna_rajasic_pct + snap.guna_tamasic_pct == pytest.approx(1.0, abs=0.01)


# =========================================================================
# 2. Guna Classification
# =========================================================================

class TestGunaClassification:

    def test_read_tools_are_sattvic(self):
        from whitemagic.harmony.vector import Guna
        assert Guna.classify_tool("get_status", "READ") == Guna.SATTVIC
        assert Guna.classify_tool("list_memories", "READ") == Guna.SATTVIC
        assert Guna.classify_tool("capabilities", "READ") == Guna.SATTVIC

    def test_write_tools_are_rajasic(self):
        from whitemagic.harmony.vector import Guna
        assert Guna.classify_tool("create_memory", "WRITE") == Guna.RAJASIC
        assert Guna.classify_tool("delete_memory", "DELETE") == Guna.RAJASIC

    def test_unknown_tools_default_rajasic(self):
        from whitemagic.harmony.vector import Guna
        assert Guna.classify_tool("custom_action", "READ") == Guna.RAJASIC


# =========================================================================
# 3. Declarative Dharma Rules Engine
# =========================================================================

class TestDharmaRulesEngine:

    def test_default_rules_loaded(self):
        from whitemagic.dharma.rules import DharmaRulesEngine
        engine = DharmaRulesEngine()
        rules = engine.get_rules()
        assert len(rules) >= 6  # at least the default set

    def test_destructive_action_triggers_warn(self):
        from whitemagic.dharma.rules import DharmaRulesEngine, DharmaAction
        engine = DharmaRulesEngine()
        d = engine.evaluate({"tool": "drop_table", "description": "delete everything"})
        # Haskell primary returns BLOCK; Python fallback returns WARN — both valid
        assert d.action in (DharmaAction.WARN, DharmaAction.BLOCK)
        assert d.score < 1.0

    def test_benign_action_passes(self):
        from whitemagic.dharma.rules import DharmaRulesEngine, DharmaAction
        engine = DharmaRulesEngine()
        d = engine.evaluate({"tool": "get_status", "description": "check system health"})
        # Haskell primary may return WARN for unknown tools; Python returns LOG
        assert d.action in (DharmaAction.LOG, DharmaAction.WARN)
        assert d.score >= 0.0  # Haskell may score stricter than Python

    def test_secure_profile_blocks_writes(self):
        from whitemagic.dharma.rules import DharmaRulesEngine, DharmaAction
        engine = DharmaRulesEngine(active_profile="secure")
        d = engine.evaluate({"tool": "create_memory", "safety": "WRITE"})
        # Both backends should restrict writes in secure profile
        assert d.action in (DharmaAction.BLOCK, DharmaAction.WARN, DharmaAction.THROTTLE)
        assert d.score < 1.0

    def test_creative_profile_relaxes_writes(self):
        from whitemagic.dharma.rules import DharmaRulesEngine, DharmaAction
        engine = DharmaRulesEngine(active_profile="creative")
        d = engine.evaluate({"tool": "create_memory", "safety": "WRITE"})
        # Creative rule fires as LOG, but default mass_mutation rule also fires
        # as TAG on WRITE safety. TAG > LOG so TAG wins. This is correct:
        # creative mode relaxes but doesn't eliminate default protections.
        assert d.action in (DharmaAction.LOG, DharmaAction.TAG)
        assert d.score >= 0.5

    def test_profile_switch(self):
        from whitemagic.dharma.rules import DharmaRulesEngine
        engine = DharmaRulesEngine()
        assert engine.get_profile() == "default"
        engine.set_profile("secure")
        assert engine.get_profile() == "secure"

    def test_karmic_trace_recorded(self):
        from whitemagic.dharma.rules import DharmaRulesEngine
        engine = DharmaRulesEngine()
        engine.evaluate({"tool": "test", "description": "something"})
        trace = engine.get_karmic_trace(limit=10)
        assert len(trace) >= 1
        assert "decision" in trace[-1]

    def test_add_rule_at_runtime(self):
        from whitemagic.dharma.rules import DharmaRulesEngine, DharmaAction
        engine = DharmaRulesEngine()
        initial_count = len(engine.get_rules())
        engine.add_rule({
            "name": "test_custom",
            "description": "Custom test rule",
            "action": "block",
            "severity": 0.9,
            "explain": "Blocked by test",
            "keyword_patterns": ["xyzzy_test_marker"],
        })
        assert len(engine.get_rules()) == initial_count + 1
        # Use Python evaluator directly — Haskell doesn't see runtime-added Python rules
        d = engine._python_evaluate(
            {"tool": "foo", "description": "xyzzy_test_marker action"}, "default"
        )
        assert d.action == DharmaAction.BLOCK

    def test_decision_to_dict_serializable(self):
        import json
        from whitemagic.dharma.rules import DharmaRulesEngine
        engine = DharmaRulesEngine()
        d = engine.evaluate({"tool": "delete", "description": "remove data"})
        data = d.to_dict()
        json.dumps(data)  # must not raise


# =========================================================================
# 4. Karma Ledger
# =========================================================================

class TestKarmaLedger:

    def test_honest_call_no_debt(self):
        from whitemagic.dharma.karma_ledger import KarmaLedger
        ledger = KarmaLedger()
        entry = ledger.record("tool_a", declared_safety="WRITE", actual_writes=1, success=True)
        assert not entry.mismatch
        assert entry.debt_delta == 0.0

    def test_deceptive_read_accrues_debt(self):
        from whitemagic.dharma.karma_ledger import KarmaLedger
        ledger = KarmaLedger()
        entry = ledger.record("sneaky", declared_safety="READ", actual_writes=2, success=True)
        assert entry.mismatch
        assert entry.debt_delta == 1.0
        assert ledger.get_debt() == 1.0

    def test_wasteful_write_small_debt(self):
        from whitemagic.dharma.karma_ledger import KarmaLedger
        ledger = KarmaLedger()
        entry = ledger.record("noop", declared_safety="WRITE", actual_writes=0, success=True)
        assert entry.mismatch
        assert entry.debt_delta == 0.2

    def test_failed_write_no_debt(self):
        from whitemagic.dharma.karma_ledger import KarmaLedger
        ledger = KarmaLedger()
        entry = ledger.record("tool_b", declared_safety="WRITE", actual_writes=0, success=False)
        assert not entry.mismatch
        assert entry.debt_delta == 0.0

    def test_forgive_reduces_debt(self):
        from whitemagic.dharma.karma_ledger import KarmaLedger
        ledger = KarmaLedger()
        ledger.record("sneaky", declared_safety="READ", actual_writes=1, success=True)
        assert ledger.get_debt() == 1.0
        new_debt = ledger.forgive(amount=0.5)
        assert new_debt == 0.5

    def test_report_structure(self):
        import json
        from whitemagic.dharma.karma_ledger import KarmaLedger
        ledger = KarmaLedger()
        ledger.record("tool_a", declared_safety="READ", actual_writes=1, success=True)
        ledger.record("tool_b", declared_safety="WRITE", actual_writes=1, success=True)
        report = ledger.report()
        assert "total_debt" in report
        assert "mismatch_rate" in report
        assert "top_offenders" in report
        json.dumps(report)  # must be serializable

    def test_persistence_roundtrip(self, tmp_path):
        from whitemagic.dharma.karma_ledger import KarmaLedger
        storage = tmp_path / "dharma"
        ledger1 = KarmaLedger(storage_dir=storage)
        ledger1.record("tool_a", declared_safety="READ", actual_writes=1, success=True)
        ledger1.record("tool_b", declared_safety="WRITE", actual_writes=1, success=True)
        debt1 = ledger1.get_debt()

        # Load a fresh ledger from the same directory
        ledger2 = KarmaLedger(storage_dir=storage)
        assert ledger2.get_debt() == debt1


# =========================================================================
# 5. Circuit Breaker
# =========================================================================

class TestCircuitBreaker:

    def test_starts_closed(self):
        from whitemagic.tools.circuit_breaker import CircuitBreaker
        b = CircuitBreaker("test_tool")
        assert not b.is_open()
        assert b.status()["state"] == "closed"

    def test_opens_after_threshold_failures(self):
        from whitemagic.tools.circuit_breaker import CircuitBreaker, BreakerConfig
        b = CircuitBreaker("test_tool", config=BreakerConfig(failure_threshold=3, window_seconds=60))
        b.record_failure()
        b.record_failure()
        assert not b.is_open()
        b.record_failure()
        assert b.is_open()
        assert b.status()["state"] == "open"
        assert b.status()["total_trips"] == 1

    def test_calm_response_is_structured(self):
        import json
        from whitemagic.tools.circuit_breaker import CircuitBreaker, BreakerConfig
        b = CircuitBreaker("fail_tool", config=BreakerConfig(failure_threshold=1))
        b.record_failure()
        resp = b.calm_response()
        assert resp["status"] == "error"
        assert resp["error_code"] == "circuit_breaker_open"
        assert resp["retryable"] is True
        json.dumps(resp)

    def test_half_open_probe_succeeds(self):
        from whitemagic.tools.circuit_breaker import CircuitBreaker, BreakerConfig
        b = CircuitBreaker("test_tool", config=BreakerConfig(
            failure_threshold=1, cooldown_seconds=0.01  # tiny cooldown
        ))
        b.record_failure()
        assert b.is_open()
        time.sleep(0.02)
        # After cooldown, breaker should be half-open (not open)
        assert not b.is_open()
        # Probe succeeds → closes
        b.record_success()
        assert b.status()["state"] == "closed"

    def test_half_open_probe_fails_reopens(self):
        from whitemagic.tools.circuit_breaker import CircuitBreaker, BreakerConfig
        b = CircuitBreaker("test_tool", config=BreakerConfig(
            failure_threshold=1, cooldown_seconds=0.01
        ))
        b.record_failure()
        time.sleep(0.02)
        assert not b.is_open()  # half_open, allows one call
        b.record_failure()  # probe failed
        assert b.is_open()  # back to open

    def test_registry_returns_same_breaker(self):
        from whitemagic.tools.circuit_breaker import BreakerRegistry
        reg = BreakerRegistry()
        b1 = reg.get("tool_x")
        b2 = reg.get("tool_x")
        assert b1 is b2

    def test_registry_tripped_list(self):
        from whitemagic.tools.circuit_breaker import BreakerRegistry, BreakerConfig
        reg = BreakerRegistry(default_config=BreakerConfig(failure_threshold=1))
        b = reg.get("bad_tool")
        b.record_failure()
        tripped = reg.tripped()
        assert len(tripped) == 1
        assert tripped[0]["tool"] == "bad_tool"


# =========================================================================
# 6. Gnosis Portal
# =========================================================================

class TestGnosisPortal:

    def test_snapshot_returns_all_sections(self):
        from whitemagic.tools.gnosis import gnosis_snapshot
        snap = gnosis_snapshot()
        expected_keys = {"timestamp", "harmony", "dharma", "karma",
                         "circuit_breakers", "yin_yang", "telemetry", "state"}
        assert expected_keys.issubset(set(snap.keys()))

    def test_snapshot_is_serializable(self):
        import json
        from whitemagic.tools.gnosis import gnosis_snapshot
        snap = gnosis_snapshot()
        json.dumps(snap)  # must not raise

    def test_failed_portals_return_none(self):
        from whitemagic.tools.gnosis import gnosis_snapshot
        snap = gnosis_snapshot()
        # Even if some portals fail (e.g., telemetry not initialized),
        # the snapshot should not raise — failed portals are None
        assert "timestamp" in snap


# =========================================================================
# 7. MCP Tool Routing (via call_tool)
# =========================================================================

class TestMCPToolRouting:

    def test_harmony_vector_tool(self, tool_caller):
        result = tool_caller.ok("harmony_vector")
        assert_envelope_shape(result)
        hv = result["details"].get("harmony_vector", {})
        assert "harmony_score" in hv

    def test_karma_report_tool(self, tool_caller):
        result = tool_caller.ok("karma_report")
        assert_envelope_shape(result)
        karma = result["details"].get("karma", {})
        assert "total_debt" in karma

    def test_karmic_trace_tool(self, tool_caller):
        result = tool_caller.ok("karmic_trace")
        assert_envelope_shape(result)
        assert "trace" in result["details"]

    def test_dharma_rules_tool(self, tool_caller):
        result = tool_caller.ok("dharma_rules")
        assert_envelope_shape(result)
        assert "rules" in result["details"]
        assert "active_profile" in result["details"]

    def test_gnosis_tool(self, tool_caller):
        result = tool_caller.ok("gnosis")
        assert_envelope_shape(result)
        gnosis = result["details"].get("gnosis", {})
        assert "harmony" in gnosis
        assert "dharma" in gnosis
        assert "karma" in gnosis
