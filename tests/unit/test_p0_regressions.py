import importlib
import sys
from types import SimpleNamespace

import pytest
from click.testing import CliRunner

import whitemagic.cli.cli_app as cli_app

try:
    from starlette.routing import Match
    from whitemagic.interfaces.api.routes import tools_gateway
    _HAS_API = True
except ImportError:
    _HAS_API = False


def _first_route_endpoint(path: str, method: str = "GET"):
    scope = {
        "type": "http",
        "method": method,
        "path": path,
        "root_path": "",
        "scheme": "http",
        "headers": [],
        "query_string": b"",
    }
    for route in tools_gateway.router.routes:
        match, _ = route.matches(scope)
        if match == Match.FULL:
            return getattr(route, "endpoint", None)
    return None


@pytest.mark.skipif(not _HAS_API, reason="fastapi/starlette not installed (requires [api] extra)")
def test_tools_health_route_precedes_dynamic_tool_lookup():
    endpoint = _first_route_endpoint("/api/tools/health")
    assert endpoint is tools_gateway.tools_health


def test_recall_cli_accepts_memories_shape(monkeypatch):
    def fake_call_tool(tool_name, **kwargs):
        assert tool_name == "search_memories"
        return {
            "status": "success",
            "details": {
                "memories": [
                    {"id": "m-1", "content": "hello from memory"},
                ]
            },
        }

    monkeypatch.setattr("whitemagic.tools.unified_api.call_tool", fake_call_tool)

    runner = CliRunner()
    result = runner.invoke(cli_app.main, ["recall", "hello"])

    assert result.exit_code == 0
    assert "Found 1 memories" in result.output
    assert "m-1" in result.output


def test_stats_command_does_not_raise_name_error(monkeypatch):
    class FakeMemory:
        def get_stats(self):
            return {
                "total_memories": 1,
                "by_type": {"short_term": 1},
                "total_tags": 1,
            }

        def get_tag_counts(self, limit=10):
            return [("audit", 1)]

    monkeypatch.setattr(cli_app, "HAS_CORE", True)
    monkeypatch.setattr(cli_app, "get_memory", lambda: FakeMemory())

    runner = CliRunner()
    result = runner.invoke(cli_app.main, ["stats"])

    assert result.exit_code == 0
    assert "Memory Statistics" in result.output


def test_no_hiding_reconstruct_uses_safe_parsing(monkeypatch):
    monkeypatch.setitem(
        sys.modules,
        "whitemagic.core.gan_ying",
        SimpleNamespace(get_bus=lambda: object(), Event=object, EventType=object),
    )
    no_hiding = importlib.import_module("whitemagic.core.memory.no_hiding")

    fake_memories = [
        SimpleNamespace(content="{'focus': 'deep'}"),
        SimpleNamespace(content="{'danger': __import__('os').system('echo hacked')}"),
    ]

    class FakeUnified:
        def search(self, query):
            return fake_memories

    monkeypatch.setattr(no_hiding, "get_unified_memory", lambda: FakeUnified())
    monkeypatch.setattr(no_hiding, "get_bus", lambda: object())

    controller = no_hiding.NoHidingMemoryController()
    reconstructed = controller.reconstruct("entity-x")

    assert reconstructed is not None
    assert reconstructed["traits"] == {"focus": "deep"}
