"""Regression tests for tool telemetry accounting."""

import pytest


@pytest.fixture
def isolated_telemetry(tmp_path, monkeypatch):
    import whitemagic.core.monitoring.telemetry as telemetry_module

    telemetry = telemetry_module.Telemetry(log_path=tmp_path / "telemetry.jsonl")
    monkeypatch.setattr(telemetry_module, "_telemetry", telemetry)
    return telemetry


def test_telemetry_records_runtime_tool_errors(isolated_telemetry):
    from whitemagic.tools.unified_api import call_tool

    out = call_tool("read_memory", filename="telemetry_missing_memory.md")
    assert out["status"] == "error"

    summary = isolated_telemetry.get_summary()
    assert summary["total_calls"] == 1
    assert summary["error_count"] == 1
    assert summary["success_rate"] == 0.0
    event = summary["recent_events"][-1]
    assert event["tool"] == "read_memory"
    assert event["status"] == "error"
    assert isinstance(event["error_code"], str) and event["error_code"]


def test_telemetry_records_validation_errors(isolated_telemetry):
    from whitemagic.tools.unified_api import call_tool

    out = call_tool("manifest", format="not-a-real-format")
    assert out["status"] == "error"
    assert out["error_code"] == "invalid_params"

    summary = isolated_telemetry.get_summary()
    assert summary["total_calls"] == 1
    assert summary["error_count"] == 1
    assert summary["errors_by_code"].get("invalid_params") == 1
    event = summary["recent_events"][-1]
    assert event["tool"] == "manifest"
    assert event["status"] == "error"
    assert event["error_code"] == "invalid_params"
