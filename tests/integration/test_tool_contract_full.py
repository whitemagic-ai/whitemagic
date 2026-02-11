"""Integration tests: every tool category returns a valid envelope."""
import json

import pytest

from tests.conftest import assert_envelope_shape


class TestMemoryCategory:
    def test_create_memory(self, tool_caller):
        from uuid import uuid4
        result = tool_caller.ok(
            "create_memory",
            title=f"contract-{uuid4().hex[:8]}",
            content="contract test",
            type="short_term",
            tags=["contract_test"],
        )
        assert_envelope_shape(result)

    def test_search_memories(self, tool_caller):
        result = tool_caller.ok("search_memories", query="anything")
        assert_envelope_shape(result)

    def test_list_memories(self, tool_caller):
        result = tool_caller.ok("list_memories", limit=1)
        assert_envelope_shape(result)


class TestSessionCategory:
    def test_session_bootstrap(self, tool_caller):
        result = tool_caller("session_bootstrap")
        assert_envelope_shape(result)
        # session_bootstrap may succeed or fail depending on state, but envelope is valid

    def test_create_session(self, tool_caller):
        result = tool_caller("create_session", name="pytest-session")
        assert_envelope_shape(result)


class TestGardenCategory:
    def test_garden_status(self, tool_caller):
        result = tool_caller("garden_status")
        assert_envelope_shape(result)

    def test_garden_health(self, tool_caller):
        result = tool_caller("garden_health")
        assert_envelope_shape(result)


class TestArchaeologyCategory:
    def test_archaeology_stats(self, tool_caller):
        result = tool_caller("archaeology_stats")
        assert_envelope_shape(result)

    def test_archaeology_recent_reads(self, tool_caller):
        result = tool_caller("archaeology_recent_reads", limit=5)
        assert_envelope_shape(result)


class TestGovernorCategory:
    def test_governor_validate(self, tool_caller):
        result = tool_caller("governor_validate", command="echo hello")
        assert_envelope_shape(result)


class TestEdgeCategory:
    def test_edge_stats(self, tool_caller):
        result = tool_caller("edge_stats")
        assert_envelope_shape(result)


class TestGanaCategory:
    def test_gana_root_status(self, tool_caller):
        result = tool_caller("gana_root", operation="status")
        assert_envelope_shape(result)

    def test_gana_heart_status(self, tool_caller):
        result = tool_caller("gana_heart", operation="status")
        assert_envelope_shape(result)


class TestIntrospectionCategory:
    def test_capabilities(self, tool_caller):
        result = tool_caller.ok("capabilities", include_tools=False, include_env=False)
        assert_envelope_shape(result)

    def test_manifest(self, tool_caller):
        result = tool_caller.ok("manifest", format="summary")
        assert_envelope_shape(result)

    def test_state_paths(self, tool_caller):
        result = tool_caller.ok("state.paths")
        assert_envelope_shape(result)

    def test_state_summary(self, tool_caller):
        result = tool_caller.ok("state.summary")
        assert_envelope_shape(result)
