"""Tests for whitemagic.tools.idempotency — idempotency ledger replay & dedup."""
from uuid import uuid4


from whitemagic.tools.idempotency import get_record, put_record, IdempotencyRecord


def test_put_and_get_record():
    """put_record stores a result that get_record can retrieve."""
    tool = "test_tool"
    key = f"test-{uuid4()}"
    response = {"status": "success", "tool": tool, "details": {"value": 42}}

    put_record(tool, key, response)
    record = get_record(tool, key)

    assert record is not None
    assert isinstance(record, IdempotencyRecord)
    assert record.tool == tool
    assert record.key == key
    assert record.response["details"]["value"] == 42
    assert record.stored_at  # non-empty timestamp


def test_get_record_missing_returns_none():
    """get_record returns None for unknown keys."""
    record = get_record("nonexistent_tool", f"missing-{uuid4()}")
    assert record is None


def test_same_key_returns_cached():
    """Same (tool, key) pair always returns the same stored result."""
    tool = "cache_test"
    key = f"idem-{uuid4()}"

    put_record(tool, key, {"status": "success", "details": {"run": 1}})
    first = get_record(tool, key)

    # Even if we don't call put_record again, get_record returns the same
    second = get_record(tool, key)
    assert first.response == second.response


def test_none_key_not_stored():
    """None or empty key should not collide with real keys."""
    tool = "null_test"
    real_key = f"real-{uuid4()}"
    put_record(tool, real_key, {"status": "success", "details": {}})

    # Different key should not find the real record
    record = get_record(tool, f"other-{uuid4()}")
    assert record is None
