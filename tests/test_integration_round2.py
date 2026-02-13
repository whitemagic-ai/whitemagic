"""
Tests for Round 2 improvements: Gnosis portals, Agent Registry,
Vote Outcome Tracking, Pipelines, Temporal classification, Circuit Breaker overrides.
"""


# ---------------------------------------------------------------------------
# Agent Registry Tests
# ---------------------------------------------------------------------------

class TestAgentRegistry:

    def test_register_agent(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.agent_registry import handle_agent_register
        result = handle_agent_register(
            name="Claude",
            capabilities=["code_review", "testing", "inference"],
            metadata={"model": "claude-4", "version": "2026-02"},
        )
        assert result["status"] == "success"
        assert result["new"] is True
        assert result["agent"]["name"] == "Claude"
        assert "code_review" in result["agent"]["capabilities"]

    def test_register_updates_existing(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.agent_registry import handle_agent_register
        r1 = handle_agent_register(name="Claude", agent_id="agent-claude")
        assert r1["new"] is True

        r2 = handle_agent_register(name="Claude v2", agent_id="agent-claude", capabilities=["testing"])
        assert r2["new"] is False
        assert r2["agent"]["name"] == "Claude v2"
        assert "testing" in r2["agent"]["capabilities"]

    def test_register_requires_name(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.agent_registry import handle_agent_register
        result = handle_agent_register()
        assert result["status"] == "error"

    def test_heartbeat(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.agent_registry import (
            handle_agent_register, handle_agent_heartbeat
        )
        reg = handle_agent_register(name="Test", agent_id="agent-test")
        assert reg["agent"]["heartbeat_count"] == 0

        hb = handle_agent_heartbeat(agent_id="agent-test", workload=0.5)
        assert hb["status"] == "success"
        assert hb["heartbeat_count"] == 1

        hb2 = handle_agent_heartbeat(agent_id="agent-test", current_task="task-123")
        assert hb2["heartbeat_count"] == 2

    def test_heartbeat_not_found(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.agent_registry import handle_agent_heartbeat
        result = handle_agent_heartbeat(agent_id="nonexistent")
        assert result["status"] == "error"

    def test_list_agents(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.agent_registry import (
            handle_agent_register, handle_agent_list
        )
        handle_agent_register(name="Claude", agent_id="a1", capabilities=["code_review"])
        handle_agent_register(name="GPT-4", agent_id="a2", capabilities=["testing"])
        handle_agent_register(name="Phi-3", agent_id="a3", capabilities=["inference", "code_review"])

        result = handle_agent_list()
        assert result["count"] == 3

        # Filter by capability
        result2 = handle_agent_list(capability="code_review")
        assert result2["count"] == 2

    def test_agent_capabilities(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.agent_registry import (
            handle_agent_register, handle_agent_capabilities
        )
        handle_agent_register(name="Claude", agent_id="a1", capabilities=["code_review", "testing"])
        result = handle_agent_capabilities(agent_id="a1")
        assert result["status"] == "success"
        assert result["capabilities"] == ["code_review", "testing"]

    def test_deregister(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.agent_registry import (
            handle_agent_register, handle_agent_deregister, handle_agent_list
        )
        handle_agent_register(name="Temp", agent_id="temp-1")
        assert handle_agent_list()["count"] == 1

        result = handle_agent_deregister(agent_id="temp-1")
        assert result["status"] == "success"
        assert handle_agent_list()["count"] == 0

    def test_deregister_not_found(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.agent_registry import handle_agent_deregister
        result = handle_agent_deregister(agent_id="ghost")
        assert result["status"] == "error"


# ---------------------------------------------------------------------------
# Vote Outcome Tracking Tests
# ---------------------------------------------------------------------------

class TestVoteOutcome:

    def test_record_outcome_success(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.voting import (
            handle_vote_create, handle_vote_cast, handle_vote_analyze,
            handle_vote_record_outcome
        )
        session = handle_vote_create(problem="Best DB?")
        sid = session["session_id"]
        handle_vote_cast(session_id=sid, voter="claude", confidence=90, solution="PostgreSQL")
        handle_vote_cast(session_id=sid, voter="gpt4", confidence=80, solution="MongoDB")
        handle_vote_analyze(session_id=sid, close=True)

        result = handle_vote_record_outcome(session_id=sid, success=True, notes="PostgreSQL worked great")
        assert result["status"] == "success"
        assert "voter_accuracy" in result
        assert result["outcome"]["success"] is True

    def test_record_outcome_failure(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.voting import (
            handle_vote_create, handle_vote_cast, handle_vote_analyze,
            handle_vote_record_outcome
        )
        session = handle_vote_create(problem="Caching strategy?")
        sid = session["session_id"]
        handle_vote_cast(session_id=sid, voter="phi3", confidence=75, solution="Redis cache")
        handle_vote_analyze(session_id=sid, close=True)

        result = handle_vote_record_outcome(session_id=sid, success=False, notes="Cache invalidation issues")
        assert result["status"] == "success"
        assert result["outcome"]["success"] is False

    def test_accuracy_tracking_accumulates(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.voting import (
            handle_vote_create, handle_vote_cast, handle_vote_analyze,
            handle_vote_record_outcome
        )

        # Session 1: claude wins, outcome success
        s1 = handle_vote_create(problem="Q1")
        handle_vote_cast(session_id=s1["session_id"], voter="claude", confidence=90, solution="A")
        handle_vote_analyze(session_id=s1["session_id"], close=True)
        handle_vote_record_outcome(session_id=s1["session_id"], success=True)

        # Session 2: claude wins again, outcome failure
        s2 = handle_vote_create(problem="Q2")
        handle_vote_cast(session_id=s2["session_id"], voter="claude", confidence=85, solution="B")
        handle_vote_analyze(session_id=s2["session_id"], close=True)
        r2 = handle_vote_record_outcome(session_id=s2["session_id"], success=False)

        # Claude should have 1 correct out of 2 tracked
        assert r2["voter_accuracy"]["claude"] == 0.5

    def test_record_outcome_not_found(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.voting import handle_vote_record_outcome
        result = handle_vote_record_outcome(session_id="nonexistent")
        assert result["status"] == "error"


# ---------------------------------------------------------------------------
# Pipeline Tests
# ---------------------------------------------------------------------------

class TestPipeline:

    def test_create_and_execute_pipeline(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.pipeline import handle_pipeline_create
        result = handle_pipeline_create(
            name="test-pipe",
            steps=[
                {"tool": "task.distribute", "args": {"command": "echo hello", "task_type": "general"}},
                {"tool": "task.list"},
            ],
        )
        assert result["status"] == "success"
        assert result["completed_steps"] == 2
        assert len(result["results"]) == 2
        # First step: task created
        assert result["results"][0]["status"] == "success"
        # Second step: task listed
        assert result["results"][1]["status"] == "success"

    def test_pipeline_with_variable_substitution(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.pipeline import handle_pipeline_create
        result = handle_pipeline_create(
            steps=[
                {"tool": "vote.create", "args": {"problem": "Test pipeline vars"}},
                {"tool": "vote.cast", "args": {
                    "session_id": "$prev.session_id",
                    "voter": "pipeline-agent",
                    "solution": "Automated vote",
                    "confidence": 80,
                }},
            ],
        )
        assert result["status"] == "success"
        assert result["completed_steps"] == 2
        # The second step should have successfully cast a vote using $prev.session_id
        step2 = result["results"][1]
        assert step2["status"] == "success"
        # total_votes may be at top level or inside details (envelope wrapping)
        total_votes = step2.get("total_votes") or step2.get("details", {}).get("total_votes")
        assert total_votes == 1

    def test_pipeline_stops_on_error(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.pipeline import handle_pipeline_create
        result = handle_pipeline_create(
            steps=[
                {"tool": "vote.cast", "args": {"session_id": "nonexistent", "voter": "x"}},
                {"tool": "task.list"},  # Should not execute
            ],
        )
        assert result["status"] == "error"
        assert result["completed_steps"] == 0

    def test_pipeline_continue_on_error(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.pipeline import handle_pipeline_create
        result = handle_pipeline_create(
            steps=[
                {"tool": "vote.cast", "args": {"session_id": "bad", "voter": "x"}, "continue_on_error": True},
                {"tool": "task.list"},
            ],
        )
        assert result["status"] == "success"
        assert result["completed_steps"] == 2
        assert result["results"][0]["status"] == "error"
        assert result["results"][1]["status"] == "success"

    def test_pipeline_create_no_execute(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.pipeline import handle_pipeline_create
        result = handle_pipeline_create(
            steps=[{"tool": "task.list"}],
            execute=False,
        )
        assert result["status"] == "success"
        assert "pipeline_id" in result

    def test_pipeline_status(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.pipeline import (
            handle_pipeline_create, handle_pipeline_status
        )
        created = handle_pipeline_create(steps=[{"tool": "task.list"}])
        pid = created["pipeline_id"]

        status = handle_pipeline_status(pipeline_id=pid)
        assert status["status"] == "success"
        assert status["pipeline"]["pipeline_status"] == "completed"

    def test_pipeline_list(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.pipeline import (
            handle_pipeline_create, handle_pipeline_list
        )
        handle_pipeline_create(name="p1", steps=[{"tool": "task.list"}])
        handle_pipeline_create(name="p2", steps=[{"tool": "vote.list"}])

        result = handle_pipeline_list()
        assert result["count"] == 2

    def test_pipeline_requires_steps(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.handlers.pipeline import handle_pipeline_create
        result = handle_pipeline_create()
        assert result["status"] == "error"


# ---------------------------------------------------------------------------
# Variable Substitution Engine Tests
# ---------------------------------------------------------------------------

class TestVariableSubstitution:

    def test_prev_full_reference(self):
        from whitemagic.tools.handlers.pipeline import _resolve_vars
        results = [{"details": {"session_id": "vote-abc", "problem": "test"}}]
        assert _resolve_vars("$prev", results) == {"session_id": "vote-abc", "problem": "test"}

    def test_prev_key_reference(self):
        from whitemagic.tools.handlers.pipeline import _resolve_vars
        results = [{"details": {"session_id": "vote-abc"}}]
        assert _resolve_vars("$prev.session_id", results) == "vote-abc"

    def test_step_reference(self):
        from whitemagic.tools.handlers.pipeline import _resolve_vars
        results = [
            {"details": {"id": "first"}},
            {"details": {"id": "second"}},
        ]
        assert _resolve_vars("$step[0].id", results) == "first"
        assert _resolve_vars("$step[1].id", results) == "second"

    def test_nested_dict_resolution(self):
        from whitemagic.tools.handlers.pipeline import _resolve_vars
        results = [{"details": {"task_id": "t-123"}}]
        resolved = _resolve_vars({"task_id": "$prev.task_id", "other": "static"}, results)
        assert resolved == {"task_id": "t-123", "other": "static"}

    def test_no_results_passthrough(self):
        from whitemagic.tools.handlers.pipeline import _resolve_vars
        assert _resolve_vars("$prev.key", []) == "$prev.key"


# ---------------------------------------------------------------------------
# Temporal Scheduler Classification Tests
# ---------------------------------------------------------------------------

class TestTemporalClassification:

    def test_broker_disconnect_is_fast(self):
        from whitemagic.core.resonance.gan_ying_enhanced import EventType
        from whitemagic.core.resonance.temporal_scheduler import classify_event, TemporalLane
        assert classify_event(EventType.BROKER_DISCONNECTED) == TemporalLane.FAST

    def test_task_failed_is_fast(self):
        from whitemagic.core.resonance.gan_ying_enhanced import EventType
        from whitemagic.core.resonance.temporal_scheduler import classify_event, TemporalLane
        assert classify_event(EventType.TASK_FAILED) == TemporalLane.FAST

    def test_agent_deregistered_is_fast(self):
        from whitemagic.core.resonance.gan_ying_enhanced import EventType
        from whitemagic.core.resonance.temporal_scheduler import classify_event, TemporalLane
        assert classify_event(EventType.AGENT_DEREGISTERED) == TemporalLane.FAST

    def test_vote_consensus_is_slow(self):
        from whitemagic.core.resonance.gan_ying_enhanced import EventType
        from whitemagic.core.resonance.temporal_scheduler import classify_event, TemporalLane
        assert classify_event(EventType.VOTE_CONSENSUS_REACHED) == TemporalLane.SLOW

    def test_vote_session_closed_is_slow(self):
        from whitemagic.core.resonance.gan_ying_enhanced import EventType
        from whitemagic.core.resonance.temporal_scheduler import classify_event, TemporalLane
        assert classify_event(EventType.VOTE_SESSION_CLOSED) == TemporalLane.SLOW

    def test_task_created_is_medium(self):
        from whitemagic.core.resonance.gan_ying_enhanced import EventType
        from whitemagic.core.resonance.temporal_scheduler import classify_event, TemporalLane
        assert classify_event(EventType.TASK_CREATED) == TemporalLane.MEDIUM

    def test_broker_message_is_medium(self):
        from whitemagic.core.resonance.gan_ying_enhanced import EventType
        from whitemagic.core.resonance.temporal_scheduler import classify_event, TemporalLane
        assert classify_event(EventType.BROKER_MESSAGE_PUBLISHED) == TemporalLane.MEDIUM


# ---------------------------------------------------------------------------
# Circuit Breaker Override Tests
# ---------------------------------------------------------------------------

class TestCircuitBreakerOverrides:

    def test_ollama_tools_have_custom_config(self):
        from whitemagic.tools.circuit_breaker import _TOOL_BREAKER_OVERRIDES
        assert "ollama.models" in _TOOL_BREAKER_OVERRIDES
        assert "ollama.generate" in _TOOL_BREAKER_OVERRIDES
        cfg = _TOOL_BREAKER_OVERRIDES["ollama.models"]
        assert cfg.failure_threshold == 2
        assert cfg.cooldown_seconds == 10.0

    def test_broker_tools_have_custom_config(self):
        from whitemagic.tools.circuit_breaker import _TOOL_BREAKER_OVERRIDES
        assert "broker.publish" in _TOOL_BREAKER_OVERRIDES
        cfg = _TOOL_BREAKER_OVERRIDES["broker.publish"]
        assert cfg.failure_threshold == 3
        assert cfg.cooldown_seconds == 15.0

    def test_registry_uses_overrides(self):
        from whitemagic.tools.circuit_breaker import BreakerRegistry
        reg = BreakerRegistry()
        ollama_breaker = reg.get("ollama.generate")
        assert ollama_breaker.config.failure_threshold == 2

        normal_breaker = reg.get("task.list")
        assert normal_breaker.config.failure_threshold == 5  # default


# ---------------------------------------------------------------------------
# call_tool Integration Tests for new tools
# ---------------------------------------------------------------------------

class TestCallToolRound2:

    def test_agent_register_via_call_tool(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.unified_api import call_tool
        result = call_tool("agent.register", name="TestAgent", capabilities=["testing"])
        assert result["status"] == "success"
        assert result["tool"] == "agent.register"
        assert "envelope_version" in result

    def test_agent_list_via_underscore(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.unified_api import call_tool
        result = call_tool("agent_list")
        assert result["status"] == "success"
        assert result["tool"] == "agent.list"

    def test_pipeline_create_via_call_tool(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.unified_api import call_tool
        result = call_tool("pipeline.create", name="test-pipeline", steps=[{"tool": "task.list"}])
        assert result["status"] == "success"
        assert result["tool"] == "pipeline.create"

    def test_vote_record_outcome_via_call_tool(self, tmp_path, monkeypatch):
        import whitemagic.config.paths as paths_mod
        monkeypatch.setattr(paths_mod, "WM_ROOT", tmp_path)

        from whitemagic.tools.unified_api import call_tool
        session = call_tool("vote.create", problem="Integration test")
        sid = session.get("details", session).get("session_id", session.get("session_id"))
        if not sid:
            sid = session.get("session_id")

        call_tool("vote.cast", session_id=sid, voter="test", confidence=80, solution="ok")
        call_tool("vote.analyze", session_id=sid, close=True)

        result = call_tool("vote.record_outcome", session_id=sid, success=True)
        assert result["status"] == "success"
