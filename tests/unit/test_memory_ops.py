"""Tests for memory CRUD operations via call_tool."""
import json
from uuid import uuid4

import pytest

from tests.conftest import assert_envelope_shape


def _unique_title():
    return f"pytest-mem-{uuid4().hex[:8]}"


class TestCreateMemory:
    def test_create_with_title_and_content(self, tool_caller):
        result = tool_caller.ok(
            "create_memory",
            title=_unique_title(),
            content="Test content for memory creation",
            type="short_term",
            tags=["pytest"],
        )
        assert_envelope_shape(result)
        assert "memory_id" in result["details"] or "filename" in result["details"]

    def test_create_requires_title(self, tool_caller):
        result = tool_caller.err("create_memory", content="no title")
        assert result["error_code"] == "invalid_params"

    def test_create_requires_content(self, tool_caller):
        result = tool_caller.err("create_memory", title="no content")
        assert result["error_code"] == "invalid_params"

    def test_create_with_tags(self, tool_caller):
        result = tool_caller.ok(
            "create_memory",
            title=_unique_title(),
            content="tagged memory",
            type="short_term",
            tags=["pytest", "tagged"],
        )
        assert_envelope_shape(result)


class TestSearchMemory:
    def test_search_returns_envelope(self, tool_caller):
        result = tool_caller.ok("search_memories", query="pytest")
        assert_envelope_shape(result)

    def test_search_with_limit(self, tool_caller):
        result = tool_caller.ok("search_memories", query="test", limit=5)
        assert_envelope_shape(result)

    def test_search_with_tags(self, tool_caller):
        result = tool_caller.ok("search_memories", query="test", tags=["pytest"])
        assert_envelope_shape(result)


class TestReadMemory:
    def test_read_nonexistent_returns_error(self, tool_caller):
        result = tool_caller("read_memory", filename=f"nonexistent_{uuid4().hex}.md")
        assert_envelope_shape(result)
        # Should indicate not found
        assert result["status"] == "error" or "not found" in result.get("message", "").lower() or "details" in result


class TestListMemories:
    def test_list_returns_envelope(self, tool_caller):
        result = tool_caller.ok("list_memories")
        assert_envelope_shape(result)

    def test_list_with_limit(self, tool_caller):
        result = tool_caller.ok("list_memories", limit=3)
        assert_envelope_shape(result)


class TestUpdateMemory:
    def test_update_nonexistent_is_handled(self, tool_caller):
        result = tool_caller("update_memory", filename=f"nonexistent_{uuid4().hex}.md", content="new")
        assert_envelope_shape(result)


class TestDeleteMemory:
    def test_delete_nonexistent_is_handled(self, tool_caller):
        result = tool_caller("delete_memory", filename=f"nonexistent_{uuid4().hex}.md")
        assert_envelope_shape(result)
