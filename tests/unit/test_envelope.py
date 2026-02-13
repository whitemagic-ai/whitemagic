"""Tests for whitemagic.tools.envelope — envelope construction & coercion."""
import json
from datetime import datetime, date
from enum import Enum
from pathlib import Path


from whitemagic.tools.envelope import ok, err, is_enveloped, normalize_raw, coerce_jsonable
from whitemagic.tools.contract import ENVELOPE_VERSION, TOOL_CONTRACT_VERSION


# ---------------------------------------------------------------------------
# coerce_jsonable
# ---------------------------------------------------------------------------

class _Color(Enum):
    RED = "red"
    BLUE = 2


def test_coerce_primitives():
    assert coerce_jsonable(None) is None
    assert coerce_jsonable("hello") == "hello"
    assert coerce_jsonable(42) == 42
    assert coerce_jsonable(3.14) == 3.14
    assert coerce_jsonable(True) is True


def test_coerce_datetime():
    dt = datetime(2026, 1, 15, 12, 0, 0)
    assert coerce_jsonable(dt) == "2026-01-15T12:00:00"


def test_coerce_date():
    d = date(2026, 1, 15)
    assert coerce_jsonable(d) == "2026-01-15"


def test_coerce_path():
    p = Path("/tmp/test/file.txt")
    assert coerce_jsonable(p) == "/tmp/test/file.txt"


def test_coerce_enum():
    assert coerce_jsonable(_Color.RED) == "red"
    assert coerce_jsonable(_Color.BLUE) == 2


def test_coerce_bytes():
    result = coerce_jsonable(b"hello")
    assert result["_type"] == "bytes"
    import base64
    assert base64.b64decode(result["b64"]) == b"hello"


def test_coerce_nested_dict():
    data = {"path": Path("/a"), "dt": datetime(2026, 1, 1), "nums": [1, 2]}
    result = coerce_jsonable(data)
    assert result["path"] == "/a"
    assert result["dt"] == "2026-01-01T00:00:00"
    assert result["nums"] == [1, 2]
    # Must be JSON-serializable
    json.dumps(result)


def test_coerce_set_sorted():
    result = coerce_jsonable({"c", "a", "b"})
    assert result == ["a", "b", "c"]


# ---------------------------------------------------------------------------
# ok() / err()
# ---------------------------------------------------------------------------

def test_ok_shape():
    result = ok(tool="test_tool", request_id="r1", message="done")
    assert result["status"] == "success"
    assert result["tool"] == "test_tool"
    assert result["request_id"] == "r1"
    assert result["message"] == "done"
    assert result["error_code"] is None
    assert result["envelope_version"] == ENVELOPE_VERSION
    assert result["tool_contract_version"] == TOOL_CONTRACT_VERSION
    assert isinstance(result["details"], dict)
    assert isinstance(result["writes"], list)
    assert isinstance(result["artifacts"], list)
    assert isinstance(result["warnings"], list)
    json.dumps(result)


def test_ok_default_message():
    result = ok(tool="t", request_id="r")
    assert result["message"] == "ok"


def test_err_shape():
    result = err(tool="t", request_id="r", error_code="bad_input", message="nope")
    assert result["status"] == "error"
    assert result["error_code"] == "bad_input"
    assert result["message"] == "nope"
    json.dumps(result)


def test_ok_details_coerced():
    result = ok(tool="t", request_id="r", details={"p": Path("/x")})
    assert result["details"]["p"] == "/x"
    json.dumps(result)


# ---------------------------------------------------------------------------
# is_enveloped()
# ---------------------------------------------------------------------------

def test_is_enveloped_true():
    envelope = ok(tool="t", request_id="r")
    assert is_enveloped(envelope) is True


def test_is_enveloped_false_missing_keys():
    assert is_enveloped({"status": "ok"}) is False
    assert is_enveloped({"tool": "x", "details": {}}) is False
    assert is_enveloped("string") is False
    assert is_enveloped(None) is False


# ---------------------------------------------------------------------------
# normalize_raw()
# ---------------------------------------------------------------------------

def test_normalize_raw_legacy_success():
    raw = {"status": "success", "data": [1, 2, 3]}
    result = normalize_raw(tool="t", request_id="r", raw=raw)
    assert result["status"] == "success"
    assert result["details"]["data"] == [1, 2, 3]
    assert result["tool"] == "t"
    json.dumps(result)


def test_normalize_raw_legacy_error():
    raw = {"status": "error", "message": "boom", "code": "timeout"}
    result = normalize_raw(tool="t", request_id="r", raw=raw)
    assert result["status"] == "error"
    assert result["error_code"] == "timeout"


def test_normalize_raw_error_without_status():
    raw = {"error": "Memory not found", "filename": "missing.md"}
    result = normalize_raw(tool="read_memory", request_id="r", raw=raw)
    assert result["status"] == "error"
    assert result["message"] == "Memory not found"
    assert result["details"]["filename"] == "missing.md"


def test_normalize_raw_bool_success():
    raw = {"success": True, "count": 5}
    result = normalize_raw(tool="t", request_id="r", raw=raw)
    assert result["status"] == "success"
    assert result["details"]["count"] == 5


def test_normalize_raw_non_dict():
    raw = [1, 2, 3]
    result = normalize_raw(tool="t", request_id="r", raw=raw)
    assert result["status"] == "success"
    assert result["details"]["value"] == [1, 2, 3]


def test_normalize_raw_already_enveloped():
    envelope = ok(tool="original", request_id="r1")
    result = normalize_raw(tool="t", request_id="r2", raw=envelope)
    # Should preserve original tool but add missing meta
    assert result["tool"] == "original"
    assert result["request_id"] == "r1"  # original is kept via setdefault
