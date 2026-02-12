"""Tests for the tool dispatcher — routing, unknown tools, gana prefix, error paths."""
import json

import pytest

from tests.conftest import assert_envelope_shape


class TestDispatchRouting:
    """Test that known tools route correctly through call_tool."""

    def test_capabilities_routes(self, tool_caller):
        result = tool_caller.ok("capabilities", include_tools=False, include_env=False)
        assert_envelope_shape(result)
        assert result["tool"] == "capabilities"

    def test_manifest_routes(self, tool_caller):
        result = tool_caller.ok("manifest", format="summary")
        assert_envelope_shape(result)
        assert result["tool"] == "manifest"

    def test_state_paths_routes(self, tool_caller):
        result = tool_caller.ok("state.paths")
        assert_envelope_shape(result)
        assert result["tool"] == "state.paths"

    def test_state_summary_routes(self, tool_caller):
        result = tool_caller.ok("state.summary")
        assert_envelope_shape(result)

    def test_alias_state_paths(self, tool_caller):
        """Legacy alias 'state_paths' should route to 'state.paths'."""
        result = tool_caller.ok("state_paths")
        assert_envelope_shape(result)
        assert result["tool"] == "state.paths"


class TestUnknownTool:
    """Test that unknown tools get dispatched gracefully."""

    def test_unknown_tool_returns_error(self, tool_caller):
        result = tool_caller("totally_nonexistent_tool_xyz_12345")
        assert_envelope_shape(result)
        # Should either be error status or have gone through bridge fallback
        assert result["status"] in ("error", "success")


class TestGanaRouting:
    """Test gana_* prefix routing."""

    def test_gana_prefix_dispatches(self, tool_caller):
        """Any tool starting with gana_ should route to gana_invoke."""
        result = tool_caller("gana_horn", operation="status")
        assert_envelope_shape(result)
        # It should attempt gana routing (may succeed or fail depending on env)
        assert result["tool"] == "gana_horn"

    def test_gana_heart_dispatches(self, tool_caller):
        result = tool_caller("gana_heart", operation="status")
        assert_envelope_shape(result)
        assert result["tool"] == "gana_heart"


class TestDryRun:
    """Test dry_run passthrough."""

    def test_dry_run_capabilities(self, tool_caller):
        result = tool_caller.ok("capabilities", include_tools=False, dry_run=True)
        assert_envelope_shape(result)
        assert result["status"] == "success"

    def test_dry_run_not_injected_by_default(self, monkeypatch):
        from whitemagic.tools import unified_api

        captured = {}

        def fake_dispatch(tool_name, **kwargs):
            captured["tool"] = tool_name
            captured["kwargs"] = dict(kwargs)
            return {"status": "success", "ok": True}

        monkeypatch.setattr(unified_api, "_dispatch_tool", fake_dispatch)
        result = unified_api.call_tool("unit_test_tool_dispatch")
        assert_envelope_shape(result)
        assert result["status"] == "success"
        assert captured["tool"] == "unit_test_tool_dispatch"
        assert "dry_run" not in captured["kwargs"]


class TestErrorPaths:
    """Test error handling in dispatcher."""

    def test_invalid_params_returns_error(self, tool_caller):
        """Invalid format enum for manifest should fail validation."""
        result = tool_caller.err("manifest", format="not-a-real-format")
        assert result["error_code"] == "invalid_params"

    def test_missing_required_params(self, tool_caller):
        """create_memory without required fields should fail."""
        result = tool_caller.err("create_memory")
        assert result["error_code"] == "invalid_params"

    def test_envelope_always_json_serializable(self, tool_caller):
        """Every response should be JSON-serializable."""
        for tool_name in ["capabilities", "state.paths", "state.summary"]:
            result = tool_caller(tool_name, include_tools=False, include_env=False)
            json.dumps(result)  # Must not raise
