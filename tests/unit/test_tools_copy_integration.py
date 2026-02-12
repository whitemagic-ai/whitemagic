"""
Tests for all modules integrated from Tools(copy) and CyberBrain projects.

Covers:
  1. Emotion & Drive Core
  2. Self-Model / Predictive Introspection
  3. Multi-LLM Ensemble Voting (handler logic, not actual LLM calls)
  4. Cross-Device Session Handoff
  5. Load-Aware Task Routing
  6. Worker Daemon
  7. Mobile Webhook Triggers (route definitions)
  8. Gnosis portal integration
"""

import json
import os
import tempfile
import time
from pathlib import Path
from unittest.mock import patch, MagicMock

import pytest


# =========================================================================
# 1. Emotion & Drive Core
# =========================================================================

class TestEmotionDriveCore:
    """Test the DriveCore intrinsic motivation engine."""

    def test_import(self):
        from whitemagic.core.intelligence.emotion_drive import DriveCore, DriveSnapshot, get_drive_core
        assert DriveCore is not None
        assert DriveSnapshot is not None

    def test_initial_snapshot(self):
        from whitemagic.core.intelligence.emotion_drive import DriveCore
        core = DriveCore()
        snap = core.snapshot()
        assert 0.0 <= snap.curiosity <= 1.0
        assert 0.0 <= snap.satisfaction <= 1.0
        assert 0.0 <= snap.caution <= 1.0
        assert 0.0 <= snap.energy <= 1.0
        assert 0.0 <= snap.social <= 1.0
        assert snap.event_count == 0
        assert snap.trend == "stable"

    def test_tool_success_event(self):
        from whitemagic.core.intelligence.emotion_drive import DriveCore
        core = DriveCore()
        initial = core.snapshot()
        snap = core.on_event("tool_success", {"tool": "search_memories"})
        assert snap.event_count == 1
        # Satisfaction should increase
        assert snap.satisfaction >= initial.satisfaction - 0.01  # Allow for decay

    def test_tool_error_event(self):
        from whitemagic.core.intelligence.emotion_drive import DriveCore
        core = DriveCore()
        snap = core.on_event("tool_error", {"tool": "broken_tool"})
        assert snap.event_count == 1
        # Caution should increase
        assert snap.caution > 0.3  # Above baseline

    def test_novelty_boosts_curiosity(self):
        from whitemagic.core.intelligence.emotion_drive import DriveCore
        core = DriveCore()
        initial_curiosity = core.snapshot().curiosity
        for _ in range(5):
            core.on_event("novelty_detected", {"score": 0.9})
        final = core.snapshot()
        assert final.curiosity > initial_curiosity

    def test_dharma_violation_boosts_caution(self):
        from whitemagic.core.intelligence.emotion_drive import DriveCore
        core = DriveCore()
        for _ in range(3):
            core.on_event("dharma_violation")
        snap = core.snapshot()
        assert snap.caution > 0.3

    def test_social_events(self):
        from whitemagic.core.intelligence.emotion_drive import DriveCore
        core = DriveCore()
        for _ in range(5):
            core.on_event("agent_interaction")
            core.on_event("broker_message")
        snap = core.snapshot()
        assert snap.social > 0.3

    def test_idle_decay(self):
        from whitemagic.core.intelligence.emotion_drive import DriveCore
        core = DriveCore()
        # Boost curiosity high
        for _ in range(10):
            core.on_event("novelty_detected", {"score": 1.0})
        high = core.snapshot().curiosity
        # Now idle
        for _ in range(20):
            core.on_event("idle_tick")
        low = core.snapshot().curiosity
        assert low < high

    def test_to_dict(self):
        from whitemagic.core.intelligence.emotion_drive import DriveCore
        core = DriveCore()
        snap = core.snapshot()
        d = snap.to_dict()
        assert "curiosity" in d
        assert "satisfaction" in d
        assert "composite" in d
        assert "dominant_drive" in d
        assert "trend" in d

    def test_motivation_bias(self):
        from whitemagic.core.intelligence.emotion_drive import DriveCore
        core = DriveCore()
        bias = core.get_motivation_bias()
        assert "explore" in bias
        assert "safe" in bias
        assert "coordinate" in bias
        assert "execute" in bias
        for v in bias.values():
            assert 0.0 <= v <= 1.0

    def test_get_stats(self):
        from whitemagic.core.intelligence.emotion_drive import DriveCore
        core = DriveCore()
        core.on_event("tool_success")
        stats = core.get_stats()
        assert stats["total_events"] == 1
        assert "tool_success" in stats["events_by_type"]

    def test_unknown_event_no_crash(self):
        from whitemagic.core.intelligence.emotion_drive import DriveCore
        core = DriveCore()
        snap = core.on_event("completely_unknown_event")
        assert snap.event_count == 1  # Still counted

    def test_score_scaling(self):
        from whitemagic.core.intelligence.emotion_drive import DriveCore
        core = DriveCore()
        # High score should amplify effect
        snap_high = DriveCore()
        snap_high.on_event("novelty_detected", {"score": 2.0})
        snap_low = DriveCore()
        snap_low.on_event("novelty_detected", {"score": 0.1})
        assert snap_high.snapshot().curiosity > snap_low.snapshot().curiosity


# =========================================================================
# 2. Self-Model / Predictive Introspection
# =========================================================================

class TestSelfModel:
    """Test the SelfModel predictive introspection engine."""

    def test_import(self):
        from whitemagic.core.intelligence.self_model import SelfModel, Forecast, get_self_model
        assert SelfModel is not None
        assert Forecast is not None

    def test_record_and_forecast(self):
        from whitemagic.core.intelligence.self_model import SelfModel
        model = SelfModel()
        # Record a rising series
        for i in range(20):
            model.record("energy", 0.5 + i * 0.01)
        f = model.forecast("energy")
        assert f is not None
        assert f.metric == "energy"
        assert f.trend == "rising"
        assert f.slope > 0

    def test_falling_trend(self):
        from whitemagic.core.intelligence.self_model import SelfModel
        model = SelfModel()
        for i in range(20):
            model.record("energy", 0.8 - i * 0.02)
        f = model.forecast("energy")
        assert f.trend == "falling"
        assert f.slope < 0

    def test_stable_trend(self):
        from whitemagic.core.intelligence.self_model import SelfModel
        model = SelfModel()
        for i in range(20):
            model.record("balance", 0.5)
        f = model.forecast("balance")
        assert f.trend == "stable"

    def test_threshold_alert(self):
        from whitemagic.core.intelligence.self_model import SelfModel
        model = SelfModel()
        # Energy dropping toward critical low (0.15)
        for i in range(30):
            model.record("energy", 0.4 - i * 0.008)
        f = model.forecast("energy")
        assert f.alert is not None
        assert "energy" in f.alert

    def test_no_alert_when_safe(self):
        from whitemagic.core.intelligence.self_model import SelfModel
        model = SelfModel()
        for i in range(20):
            model.record("energy", 0.7 + i * 0.001)
        f = model.forecast("energy")
        assert f.alert is None

    def test_forecast_all(self):
        from whitemagic.core.intelligence.self_model import SelfModel
        model = SelfModel()
        for i in range(10):
            model.record("energy", 0.5 + i * 0.01)
            model.record("karma_debt", 0.1 + i * 0.02)
        forecasts = model.forecast_all()
        assert "energy" in forecasts
        assert "karma_debt" in forecasts

    def test_get_alerts(self):
        from whitemagic.core.intelligence.self_model import SelfModel
        model = SelfModel()
        # Create a dangerous trend
        for i in range(30):
            model.record("karma_debt", 0.3 + i * 0.02)
        alerts = model.get_alerts()
        assert isinstance(alerts, list)

    def test_gnosis_portal(self):
        from whitemagic.core.intelligence.self_model import SelfModel
        model = SelfModel()
        for i in range(10):
            model.record("energy", 0.5)
        portal = model.gnosis_portal()
        assert "tracked_metrics" in portal
        assert "alert_count" in portal
        assert "forecasts" in portal

    def test_forecast_to_dict(self):
        from whitemagic.core.intelligence.self_model import SelfModel
        model = SelfModel()
        for i in range(10):
            model.record("energy", 0.5 + i * 0.01)
        f = model.forecast("energy")
        d = f.to_dict()
        assert "metric" in d
        assert "current" in d
        assert "predicted" in d
        assert "trend" in d
        assert "confidence" in d

    def test_insufficient_data(self):
        from whitemagic.core.intelligence.self_model import SelfModel
        model = SelfModel()
        model.record("energy", 0.5)
        f = model.forecast("energy")
        assert f is None  # Not enough data

    def test_record_batch(self):
        from whitemagic.core.intelligence.self_model import SelfModel
        model = SelfModel()
        for i in range(5):
            model.record_batch({"energy": 0.5, "karma_debt": 0.1})
        stats = model.get_stats()
        assert stats["metric_count"] == 2
        assert stats["total_observations"] == 10


# =========================================================================
# 3. Multi-LLM Ensemble Voting
# =========================================================================

class TestEnsembleVoting:
    """Test ensemble voting handler logic."""

    def test_import(self):
        from whitemagic.tools.handlers.ensemble import (
            handle_ensemble_query, handle_ensemble_status, handle_ensemble_history,
            _extract_confidence, _analyze_results,
        )

    def test_extract_confidence_percentage(self):
        from whitemagic.tools.handlers.ensemble import _extract_confidence
        assert _extract_confidence("I'm 85% confident") == 85
        assert _extract_confidence("Confidence: 92%") == 92
        assert _extract_confidence("My confidence is 70/100") == 70

    def test_extract_confidence_default(self):
        from whitemagic.tools.handlers.ensemble import _extract_confidence
        assert _extract_confidence("No confidence mentioned here") == 70

    def test_analyze_results_strong_consensus(self):
        from whitemagic.tools.handlers.ensemble import _analyze_results
        results = [
            {"model": "a", "response": "yes", "confidence": 90, "success": True},
            {"model": "b", "response": "yes", "confidence": 88, "success": True},
            {"model": "c", "response": "yes", "confidence": 85, "success": True},
        ]
        analysis = _analyze_results(results)
        assert analysis["consensus"] == "reached"
        assert analysis["consensus_strength"] == "strong"
        assert analysis["successful_count"] == 3

    def test_analyze_results_weak_consensus(self):
        from whitemagic.tools.handlers.ensemble import _analyze_results
        results = [
            {"model": "a", "response": "yes", "confidence": 95, "success": True},
            {"model": "b", "response": "no", "confidence": 40, "success": True},
        ]
        analysis = _analyze_results(results)
        assert analysis["consensus_strength"] in ("weak", "moderate")

    def test_analyze_results_no_success(self):
        from whitemagic.tools.handlers.ensemble import _analyze_results
        results = [
            {"model": "a", "error": "timeout", "success": False},
        ]
        analysis = _analyze_results(results)
        assert analysis["consensus"] == "none"
        assert analysis["successful_count"] == 0

    def test_query_requires_prompt(self):
        from whitemagic.tools.handlers.ensemble import handle_ensemble_query
        result = handle_ensemble_query()
        assert result["status"] == "error"
        assert "prompt" in result["error"]

    def test_status_requires_id(self):
        from whitemagic.tools.handlers.ensemble import handle_ensemble_status
        result = handle_ensemble_status()
        assert result["status"] == "error"

    def test_history_returns_list(self):
        from whitemagic.tools.handlers.ensemble import handle_ensemble_history
        result = handle_ensemble_history(limit=5)
        assert result["status"] == "success"
        assert isinstance(result["ensembles"], list)


# =========================================================================
# 4. Cross-Device Session Handoff
# =========================================================================

class TestSessionHandoff:
    """Test cross-device session handoff handlers."""

    def test_import(self):
        from whitemagic.tools.handlers.session import (
            handle_session_handoff_transfer,
            handle_session_accept_handoff,
            handle_session_list_handoffs,
        )

    def test_handoff_transfer_requires_session_id(self):
        from whitemagic.tools.handlers.session import handle_session_handoff_transfer
        result = handle_session_handoff_transfer()
        assert result["status"] == "error"
        assert "session_id" in result["error"]

    def test_accept_handoff_requires_id(self):
        from whitemagic.tools.handlers.session import handle_session_accept_handoff
        result = handle_session_accept_handoff()
        assert result["status"] == "error"
        assert "handoff_id" in result["error"]

    def test_list_handoffs(self):
        from whitemagic.tools.handlers.session import handle_session_list_handoffs
        result = handle_session_list_handoffs()
        assert result["status"] == "success"
        assert "handoffs" in result


# =========================================================================
# 5. Load-Aware Task Routing
# =========================================================================

class TestLoadAwareRouting:
    """Test load-aware task routing."""

    def test_import(self):
        from whitemagic.tools.handlers.task_dist import handle_task_route_smart

    def test_route_smart_returns_local(self):
        from whitemagic.tools.handlers.task_dist import handle_task_route_smart
        result = handle_task_route_smart(task_type="general")
        assert result["status"] == "success"
        assert "recommended_host" in result
        assert "all_hosts" in result
        assert len(result["all_hosts"]) >= 1

    def test_route_smart_local_load(self):
        from whitemagic.tools.handlers.task_dist import _get_local_load
        load = _get_local_load()
        assert load["available"] is True
        assert "load_pct" in load
        assert "cpu_count" in load


# =========================================================================
# 6. Worker Daemon
# =========================================================================

class TestWorkerDaemon:
    """Test the WorkerDaemon class."""

    def test_import(self):
        from whitemagic.agents.worker_daemon import WorkerDaemon

    def test_init(self):
        from whitemagic.agents.worker_daemon import WorkerDaemon
        daemon = WorkerDaemon(poll_interval=1.0, worker_name="test-worker")
        assert daemon.worker_name == "test-worker"
        assert daemon.poll_interval == 1.0

    def test_get_stats(self):
        from whitemagic.agents.worker_daemon import WorkerDaemon
        daemon = WorkerDaemon(worker_name="test-worker")
        stats = daemon.get_stats()
        assert stats["worker_name"] == "test-worker"
        assert stats["running"] is False
        assert stats["tasks_completed"] == 0

    def test_shell_safety_check(self):
        from whitemagic.agents.worker_daemon import WorkerDaemon
        daemon = WorkerDaemon()
        task = {"id": "test-1", "mode": "shell", "command": "rm -rf /", "status": "pending"}
        result = daemon._execute_shell_task(task)
        assert result["success"] is False
        assert "safety" in result["error"].lower() or "rejected" in result["error"].lower()


# =========================================================================
# 7. Mobile Webhook Triggers
# =========================================================================

class TestWebhookTriggers:
    """Test webhook trigger route definitions."""

    def test_import(self):
        try:
            from whitemagic.interfaces.api.routes.webhook_triggers import (
                ALLOWED_ACTIONS, router
            )
            assert len(ALLOWED_ACTIONS) > 0
            assert router is not None
        except ImportError:
            pytest.skip("FastAPI not installed")

    def test_allowed_actions_structure(self):
        try:
            from whitemagic.interfaces.api.routes.webhook_triggers import ALLOWED_ACTIONS
            for name, config in ALLOWED_ACTIONS.items():
                assert "description" in config
                assert "tool" in config
                assert "safety" in config
                assert config["safety"] in ("read", "write")
        except ImportError:
            pytest.skip("FastAPI not installed")


# =========================================================================
# 8. Dispatch Table Wiring
# =========================================================================

class TestDispatchWiring:
    """Verify all new tools are wired into the dispatch table."""

    def test_new_tools_in_dispatch_table(self):
        from whitemagic.tools.dispatch_table import DISPATCH_TABLE
        new_tools = [
            "ensemble.query", "ensemble.status", "ensemble.history",
            "session.handoff_transfer", "session.accept_handoff", "session.list_handoffs",
            "task.route_smart",
            "drive.snapshot", "drive.event",
            "selfmodel.forecast", "selfmodel.alerts",
            "worker.status",
        ]
        for tool in new_tools:
            assert tool in DISPATCH_TABLE, f"{tool} not in DISPATCH_TABLE"


# =========================================================================
# 9. Registry Wiring
# =========================================================================

class TestRegistryWiring:
    """Verify all new tools are registered."""

    def test_new_tools_in_registry(self):
        from whitemagic.tools.registry import get_tool
        # v12.5: ensemble.query/status/history → ensemble (unified)
        # v12.5: session.handoff_transfer/accept/list → session.handoff (unified)
        new_tools = [
            "ensemble",
            "session.handoff",
            "task.route_smart",
            "drive.snapshot", "drive.event",
            "selfmodel.forecast", "selfmodel.alerts",
            "worker.status",
        ]
        for tool_name in new_tools:
            tool = get_tool(tool_name)
            assert tool is not None, f"{tool_name} not in TOOL_REGISTRY"
            assert tool.input_schema is not None


# =========================================================================
# 10. Introspection Handler Wiring
# =========================================================================

class TestIntrospectionHandlers:
    """Verify introspection handlers for new modules work."""

    def test_drive_snapshot_handler(self):
        from whitemagic.tools.handlers.cyberbrain import handle_drive_snapshot
        result = handle_drive_snapshot()
        assert result["status"] == "success"
        assert "drives" in result
        assert "curiosity" in result["drives"]

    def test_drive_event_handler(self):
        from whitemagic.tools.handlers.cyberbrain import handle_drive_event
        result = handle_drive_event(event_type="tool_success")
        assert result["status"] == "success"
        assert "drives" in result

    def test_drive_event_requires_type(self):
        from whitemagic.tools.handlers.cyberbrain import handle_drive_event
        result = handle_drive_event()
        assert result["status"] == "error"

    def test_selfmodel_forecast_handler(self):
        from whitemagic.tools.handlers.cyberbrain import handle_selfmodel_forecast
        # No data yet — should return empty forecasts
        result = handle_selfmodel_forecast()
        assert result["status"] == "success"

    def test_selfmodel_alerts_handler(self):
        from whitemagic.tools.handlers.cyberbrain import handle_selfmodel_alerts
        result = handle_selfmodel_alerts()
        assert result["status"] == "success"
        assert "alert_count" in result

    def test_worker_status_handler(self):
        from whitemagic.tools.handlers.cyberbrain import handle_worker_status
        result = handle_worker_status()
        assert result["status"] == "success"
        assert "worker_count" in result
