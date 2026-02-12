"""
Tests for new tool families: broker, task distribution, voting, ollama.

These tests validate the handler logic and the tool contract integration
without requiring external services (Redis, Ollama).
"""
import json
import os
import pytest
from pathlib import Path
from unittest.mock import patch, MagicMock


# ---------------------------------------------------------------------------
# Task Distribution Tests (no external deps)
# ---------------------------------------------------------------------------

class TestTaskDistribution:
    """Test task.distribute, task.status, task.list, task.complete handlers."""

    def test_distribute_creates_task(self, tmp_path, monkeypatch):
        monkeypatch.setenv("WM_STATE_ROOT", str(tmp_path))
        # Force re-resolve WM_ROOT
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.task_dist import handle_task_distribute
        result = handle_task_distribute(
            command="make build",
            task_type="build",
            description="Build the project",
        )
        assert result["status"] == "success"
        assert "task" in result
        assert result["task"]["task_type"] == "build"
        assert result["task"]["status"] == "pending"
        assert result["task"]["command"] == "make build"

    def test_distribute_requires_command_or_description(self, tmp_path, monkeypatch):
        monkeypatch.setenv("WM_STATE_ROOT", str(tmp_path))
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.task_dist import handle_task_distribute
        result = handle_task_distribute()
        assert result["status"] == "error"

    def test_task_status_summary(self, tmp_path, monkeypatch):
        monkeypatch.setenv("WM_STATE_ROOT", str(tmp_path))
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.task_dist import (
            handle_task_distribute, handle_task_status
        )
        handle_task_distribute(command="echo hi", task_type="general")
        handle_task_distribute(command="echo bye", task_type="testing")

        result = handle_task_status()
        assert result["status"] == "success"
        assert result["total"] == 2
        assert result["counts"]["pending"] == 2

    def test_task_status_specific(self, tmp_path, monkeypatch):
        monkeypatch.setenv("WM_STATE_ROOT", str(tmp_path))
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.task_dist import (
            handle_task_distribute, handle_task_status
        )
        create_result = handle_task_distribute(command="echo hi")
        task_id = create_result["task"]["id"]

        result = handle_task_status(task_id=task_id)
        assert result["status"] == "success"
        assert result["task"]["id"] == task_id

    def test_task_status_not_found(self, tmp_path, monkeypatch):
        monkeypatch.setenv("WM_STATE_ROOT", str(tmp_path))
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.task_dist import handle_task_status
        result = handle_task_status(task_id="nonexistent-123")
        assert result["status"] == "error"

    def test_task_list_with_filters(self, tmp_path, monkeypatch):
        monkeypatch.setenv("WM_STATE_ROOT", str(tmp_path))
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.task_dist import (
            handle_task_distribute, handle_task_list
        )
        handle_task_distribute(command="make", task_type="build")
        handle_task_distribute(command="pytest", task_type="testing")

        result = handle_task_list(filter_type="build")
        assert result["status"] == "success"
        assert result["count"] == 1
        assert result["tasks"][0]["task_type"] == "build"

    def test_task_complete(self, tmp_path, monkeypatch):
        monkeypatch.setenv("WM_STATE_ROOT", str(tmp_path))
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.task_dist import (
            handle_task_distribute, handle_task_complete, handle_task_status
        )
        create_result = handle_task_distribute(command="echo done")
        task_id = create_result["task"]["id"]

        complete_result = handle_task_complete(
            task_id=task_id, success=True, output="done"
        )
        assert complete_result["status"] == "success"

        status_result = handle_task_status(task_id=task_id)
        assert status_result["task"]["status"] == "completed"
        assert status_result["result"]["success"] is True

    def test_task_complete_failure(self, tmp_path, monkeypatch):
        monkeypatch.setenv("WM_STATE_ROOT", str(tmp_path))
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.task_dist import (
            handle_task_distribute, handle_task_complete
        )
        create_result = handle_task_distribute(command="fail_cmd")
        task_id = create_result["task"]["id"]

        complete_result = handle_task_complete(
            task_id=task_id, success=False, error_message="segfault"
        )
        assert complete_result["status"] == "success"
        assert "failed" in complete_result["message"]


# ---------------------------------------------------------------------------
# Voting Tests (no external deps)
# ---------------------------------------------------------------------------

class TestVoting:
    """Test vote.create, vote.cast, vote.analyze, vote.list handlers."""

    def test_create_vote_session(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.voting import handle_vote_create
        result = handle_vote_create(problem="Which database to use?", task_type="architecture")
        assert result["status"] == "success"
        assert "session_id" in result
        assert result["session_id"].startswith("vote-")

    def test_create_requires_problem(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.voting import handle_vote_create
        result = handle_vote_create()
        assert result["status"] == "error"

    def test_cast_vote(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.voting import handle_vote_create, handle_vote_cast
        session = handle_vote_create(problem="Best framework?")
        sid = session["session_id"]

        result = handle_vote_cast(
            session_id=sid,
            voter="claude",
            solution="Use FastAPI. Confidence: 85%",
            reasoning="Modern, async, well-documented",
        )
        assert result["status"] == "success"
        assert result["vote"]["confidence"] == 85
        assert result["total_votes"] == 1

    def test_cast_auto_confidence(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.voting import handle_vote_create, handle_vote_cast
        session = handle_vote_create(problem="Test question")
        sid = session["session_id"]

        result = handle_vote_cast(
            session_id=sid,
            voter="gpt4",
            solution="No confidence mentioned here",
        )
        assert result["vote"]["confidence"] == 70  # default

    def test_analyze_votes(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.voting import (
            handle_vote_create, handle_vote_cast, handle_vote_analyze
        )
        session = handle_vote_create(problem="Optimize query?")
        sid = session["session_id"]

        handle_vote_cast(session_id=sid, voter="claude", confidence=90, solution="Index the table")
        handle_vote_cast(session_id=sid, voter="gpt4", confidence=85, solution="Add caching layer")
        handle_vote_cast(session_id=sid, voter="phi3", confidence=88, solution="Index the table")

        result = handle_vote_analyze(session_id=sid)
        assert result["status"] == "success"
        assert result["analysis"]["total_voters"] == 3
        assert result["analysis"]["consensus_strength"] in ("strong", "moderate", "weak")
        assert result["analysis"]["winner"]["voter"] == "claude"  # highest confidence

    def test_analyze_and_close(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.voting import (
            handle_vote_create, handle_vote_cast, handle_vote_analyze
        )
        session = handle_vote_create(problem="Close test")
        sid = session["session_id"]
        handle_vote_cast(session_id=sid, voter="test", confidence=80, solution="ok")

        result = handle_vote_analyze(session_id=sid, close=True)
        assert result["session_status"] == "closed"

    def test_vote_list(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.voting import handle_vote_create, handle_vote_list
        handle_vote_create(problem="Q1")
        handle_vote_create(problem="Q2")

        result = handle_vote_list()
        assert result["status"] == "success"
        assert result["count"] == 2

    def test_vote_on_closed_session(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.voting import (
            handle_vote_create, handle_vote_cast, handle_vote_analyze
        )
        session = handle_vote_create(problem="Closed test")
        sid = session["session_id"]
        handle_vote_cast(session_id=sid, voter="a", confidence=80, solution="x")
        handle_vote_analyze(session_id=sid, close=True)

        result = handle_vote_cast(session_id=sid, voter="b", solution="too late")
        assert result["status"] == "error"


# ---------------------------------------------------------------------------
# Broker Tests (mock Redis)
# ---------------------------------------------------------------------------

class TestBroker:
    """Test broker handlers with mocked Redis."""

    def test_broker_status_missing_redis(self):
        from whitemagic.tools.handlers.broker import handle_broker_status
        # Without Redis installed, should return a clear error
        with patch("whitemagic.tools.handlers.broker._require_redis", side_effect=ImportError("redis not installed")):
            with patch("whitemagic.tools.handlers.broker._BROKER_INSTANCE", None):
                result = handle_broker_status()
                assert result["connected"] is False

    def test_broker_publish_requires_channel(self):
        from whitemagic.tools.handlers.broker import handle_broker_publish
        result = handle_broker_publish()
        assert result["status"] == "error"

    def test_broker_history_requires_channel(self):
        from whitemagic.tools.handlers.broker import handle_broker_history
        result = handle_broker_history()
        assert result["status"] == "error"


# ---------------------------------------------------------------------------
# Ollama Tests (mock aiohttp)
# ---------------------------------------------------------------------------

class TestOllama:
    """Test ollama handlers with mocked HTTP."""

    def test_ollama_models_no_aiohttp(self):
        from whitemagic.tools.handlers.ollama import handle_ollama_models
        with patch("whitemagic.tools.handlers.ollama._require_aiohttp", side_effect=ImportError("aiohttp not installed")):
            result = handle_ollama_models()
            assert result["status"] == "error"
            assert "missing_dependency" in str(result.get("error_code", "")) or "aiohttp" in result.get("error", "")

    def test_ollama_generate_requires_model(self):
        from whitemagic.tools.handlers.ollama import handle_ollama_generate
        result = handle_ollama_generate(prompt="hello")
        assert result["status"] == "error"
        assert "model" in result["error"]

    def test_ollama_generate_requires_prompt(self):
        from whitemagic.tools.handlers.ollama import handle_ollama_generate
        result = handle_ollama_generate(model="phi3")
        assert result["status"] == "error"
        assert "prompt" in result["error"]

    def test_ollama_chat_requires_messages(self):
        from whitemagic.tools.handlers.ollama import handle_ollama_chat
        result = handle_ollama_chat(model="phi3")
        assert result["status"] == "error"
        assert "messages" in result["error"]


# ---------------------------------------------------------------------------
# Integration: call_tool envelope wrapping
# ---------------------------------------------------------------------------

class TestCallToolIntegration:
    """Test that new tools work through the call_tool contract."""

    def test_task_distribute_via_call_tool(self, tmp_path, monkeypatch):
        monkeypatch.setenv("WM_STATE_ROOT", str(tmp_path))
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.unified_api import call_tool
        result = call_tool("task.distribute", command="echo test", task_type="general")
        assert result["status"] == "success"
        assert result["tool"] == "task.distribute"
        assert "envelope_version" in result

    def test_task_distribute_via_underscore_alias(self, tmp_path, monkeypatch):
        monkeypatch.setenv("WM_STATE_ROOT", str(tmp_path))
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.unified_api import call_tool
        result = call_tool("task_distribute", command="echo test")
        assert result["status"] == "success"
        assert result["tool"] == "task.distribute"

    def test_vote_create_via_call_tool(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.unified_api import call_tool
        result = call_tool("vote.create", problem="Test problem")
        assert result["status"] == "success"
        assert result["tool"] == "vote.create"
        assert "envelope_version" in result

    def test_vote_list_via_call_tool(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.unified_api import call_tool
        result = call_tool("vote.list")
        assert result["status"] == "success"
        assert result["tool"] == "vote.list"
