import json
from uuid import uuid4


def _assert_envelope_shape(out: dict) -> None:
    expected = {
        "status",
        "tool",
        "request_id",
        "idempotency_key",
        "message",
        "error_code",
        "details",
        "retryable",
        "writes",
        "artifacts",
        "metrics",
        "side_effects",
        "warnings",
        "timestamp",
        "envelope_version",
        "tool_contract_version",
    }
    missing = expected.difference(out.keys())
    assert not missing, f"missing envelope keys: {sorted(missing)}"
    assert isinstance(out["status"], str)
    assert isinstance(out["tool"], str)
    assert isinstance(out["request_id"], str)
    assert isinstance(out["details"], dict)

    # Must always be JSON-serializable.
    json.dumps(out)


def test_capabilities_returns_envelope():
    from whitemagic.tools.unified_api import call_tool

    out = call_tool("capabilities", include_tools=False, include_env=False)
    _assert_envelope_shape(out)
    assert out["status"] == "success"
    assert out["tool"] == "capabilities"


def test_invalid_params_returns_error_envelope():
    from whitemagic.tools.unified_api import call_tool

    out = call_tool("manifest", format="not-a-real-format")
    _assert_envelope_shape(out)
    assert out["status"] == "error"
    assert out["error_code"] == "invalid_params"


def test_idempotency_replay_create_memory():
    from whitemagic.tools.unified_api import call_tool

    key = f"pytest-{uuid4()}"
    first = call_tool(
        "create_memory",
        title="pytest idempotency",
        content="hello",
        type="short_term",
        tags=["pytest"],
        idempotency_key=key,
    )
    _assert_envelope_shape(first)
    assert first["status"] == "success"
    mem_id_1 = first["details"]["memory_id"]

    second = call_tool(
        "create_memory",
        title="pytest idempotency",
        content="hello",
        type="short_term",
        tags=["pytest"],
        idempotency_key=key,
    )
    _assert_envelope_shape(second)
    assert second["status"] == "success"
    assert second["details"]["memory_id"] == mem_id_1
    assert second.get("side_effects", {}).get("idempotency_replay") is True


def test_now_override_sets_timestamp_verbatim():
    from whitemagic.tools.unified_api import call_tool

    ts = "2026-01-01T00:00:00Z"
    out = call_tool("capabilities", include_tools=False, include_env=False, now=ts)
    _assert_envelope_shape(out)
    assert out["timestamp"] == ts

