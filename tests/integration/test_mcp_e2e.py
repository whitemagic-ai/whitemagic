#!/usr/bin/env python3
"""End-to-end MCP server test — spawns the lean server as a subprocess,
connects via stdio, and verifies tool-call round trips.

Requires: mcp SDK installed (skip if unavailable).

Usage:
    pytest tests/integration/test_mcp_e2e.py -v
"""
from __future__ import annotations

import json
import os
import subprocess
import sys
import time
from pathlib import Path
from typing import Any

import pytest

ROOT = Path(__file__).resolve().parent.parent.parent

# Skip entire module if mcp SDK not installed
try:
    import mcp  # noqa: F401
    HAS_MCP = True
except ImportError:
    HAS_MCP = False

pytestmark = pytest.mark.skipif(not HAS_MCP, reason="mcp SDK not installed")


class MCPClient:
    """Minimal MCP stdio client for testing."""

    def __init__(self, process: subprocess.Popen):
        self.proc = process
        self._id = 0

    def _next_id(self) -> int:
        self._id += 1
        return self._id

    def send(self, method: str, params: dict | None = None, *, timeout: float = 15.0) -> dict:
        """Send a JSON-RPC request and read the response."""
        req_id = self._next_id()
        request: dict[str, Any] = {
            "jsonrpc": "2.0",
            "id": req_id,
            "method": method,
        }
        if params is not None:
            request["params"] = params

        payload = json.dumps(request)
        # MCP stdio uses newline-delimited JSON
        self.proc.stdin.write((payload + "\n").encode())
        self.proc.stdin.flush()

        # Read response (newline-delimited JSON)
        response = self._read_message(timeout=timeout)
        return response

    def _read_message(self, *, timeout: float = 15.0) -> dict:
        """Read a newline-delimited JSON-RPC message from stdout."""
        import selectors

        sel = selectors.DefaultSelector()
        sel.register(self.proc.stdout, selectors.EVENT_READ)

        deadline = time.monotonic() + timeout
        buf = b""
        try:
            while True:
                remaining = deadline - time.monotonic()
                if remaining <= 0:
                    raise TimeoutError(f"No response within {timeout}s")
                events = sel.select(timeout=remaining)
                if not events:
                    raise TimeoutError(f"No response within {timeout}s")
                chunk = self.proc.stdout.read1(4096) if hasattr(self.proc.stdout, 'read1') else self.proc.stdout.readline()
                if not chunk:
                    raise ConnectionError("Server closed stdout")
                buf += chunk
                # Look for complete newline-delimited JSON lines
                while b"\n" in buf:
                    line, buf = buf.split(b"\n", 1)
                    line_str = line.decode("utf-8", errors="replace").strip()
                    if not line_str:
                        continue
                    try:
                        return json.loads(line_str)
                    except json.JSONDecodeError:
                        continue  # skip non-JSON lines (e.g. log output)
        finally:
            sel.unregister(self.proc.stdout)
            sel.close()

    def close(self) -> None:
        """Terminate the server process."""
        try:
            self.proc.stdin.close()
        except Exception:
            pass
        try:
            self.proc.terminate()
            self.proc.wait(timeout=5)
        except Exception:
            self.proc.kill()


@pytest.fixture(scope="module")
def mcp_server():
    """Start the MCP lean server as a subprocess and yield a client."""
    server_path = ROOT / "whitemagic" / "run_mcp_lean.py"
    if not server_path.exists():
        pytest.skip("run_mcp_lean.py not found")

    env = os.environ.copy()
    env["PYTHONPATH"] = str(ROOT)
    # Use a temp state root so we don't touch the user's data
    env["WM_STATE_ROOT"] = str(ROOT / ".test_mcp_state")

    proc = subprocess.Popen(
        [sys.executable, str(server_path)],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        env=env,
        cwd=str(ROOT),
    )

    # Give server a moment to start
    time.sleep(1.0)

    if proc.poll() is not None:
        stderr = proc.stderr.read().decode("utf-8", errors="replace")
        pytest.skip(f"MCP server failed to start: {stderr[:500]}")

    client = MCPClient(proc)
    yield client
    client.close()

    # Cleanup temp state
    import shutil
    test_state = ROOT / ".test_mcp_state"
    if test_state.exists():
        shutil.rmtree(test_state, ignore_errors=True)


class TestMCPInitialize:
    """Test MCP initialization handshake."""

    def test_initialize(self, mcp_server: MCPClient):
        """Server responds to initialize with capabilities."""
        response = mcp_server.send("initialize", {
            "protocolVersion": "2024-11-05",
            "capabilities": {},
            "clientInfo": {"name": "test-client", "version": "1.0.0"},
        })

        assert "result" in response, f"Expected result, got: {response}"
        result = response["result"]
        assert "serverInfo" in result
        assert "capabilities" in result
        assert result["serverInfo"]["name"] == "whitemagic"

    def test_initialized_notification(self, mcp_server: MCPClient):
        """Server accepts initialized notification."""
        # Send as notification (no id expected back, but we send with id for simplicity)
        mcp_server.send("notifications/initialized", {})
        # If no error, notification accepted


class TestMCPToolsList:
    """Test tools/list endpoint."""

    def test_list_tools(self, mcp_server: MCPClient):
        """Server lists 28 PRAT Gana meta-tools."""
        response = mcp_server.send("tools/list", {})
        assert "result" in response, f"Expected result, got: {response}"

        tools = response["result"].get("tools", [])
        assert len(tools) >= 28, f"Expected ≥28 tools, got {len(tools)}"

        tool_names = {t["name"] for t in tools}
        # Verify a few known Ganas exist
        for expected in ["gana_horn", "gana_neck", "gana_root", "gana_ghost"]:
            assert expected in tool_names, f"Missing expected tool: {expected}"


class TestMCPResourcesList:
    """Test resources/list endpoint."""

    def test_list_resources(self, mcp_server: MCPClient):
        """Server lists orientation docs and workflow templates."""
        response = mcp_server.send("resources/list", {})
        assert "result" in response, f"Expected result, got: {response}"

        resources = response["result"].get("resources", [])
        assert len(resources) >= 9, f"Expected ≥9 resources, got {len(resources)}"

        uris = {r["uri"] for r in resources}
        assert "whitemagic://orientation/ai-primary" in uris
        assert "whitemagic://orientation/server-instructions" in uris

        # v14.1.1 workflow templates
        workflow_uris = [u for u in uris if "workflow/" in u]
        assert len(workflow_uris) >= 6, f"Expected ≥6 workflows, got {len(workflow_uris)}"


class TestMCPToolCall:
    """Test actual tool invocation via tools/call."""

    def test_call_gana_root_health(self, mcp_server: MCPClient):
        """Invoke gana_root with health_report tool."""
        response = mcp_server.send("tools/call", {
            "name": "gana_root",
            "arguments": {
                "tool": "health_report",
                "args": {},
            },
        })
        assert "result" in response, f"Expected result, got: {response}"

        content = response["result"].get("content", [])
        assert len(content) > 0, "Expected non-empty content"
        assert content[0]["type"] == "text"

        # Parse the JSON text content
        data = json.loads(content[0]["text"])
        assert "health_score" in data or "status" in data

    def test_call_gana_ghost_capabilities(self, mcp_server: MCPClient):
        """Invoke gana_ghost with capabilities tool."""
        response = mcp_server.send("tools/call", {
            "name": "gana_ghost",
            "arguments": {
                "tool": "capabilities",
                "args": {},
            },
        })
        assert "result" in response, f"Expected result, got: {response}"

        content = response["result"].get("content", [])
        assert len(content) > 0


class TestMCPResourceRead:
    """Test resource reading."""

    def test_read_server_instructions(self, mcp_server: MCPClient):
        """Read the server-instructions resource."""
        response = mcp_server.send("resources/read", {
            "uri": "whitemagic://orientation/server-instructions",
        })
        assert "result" in response, f"Expected result, got: {response}"

        contents = response["result"].get("contents", [])
        assert len(contents) > 0
        text = contents[0].get("text", "")
        assert "WhiteMagic" in text

    def test_read_workflow_template(self, mcp_server: MCPClient):
        """Read a workflow template resource."""
        response = mcp_server.send("resources/read", {
            "uri": "whitemagic://workflow/new_session",
        })
        assert "result" in response, f"Expected result, got: {response}"

        contents = response["result"].get("contents", [])
        assert len(contents) > 0
        text = contents[0].get("text", "")
        assert "session" in text.lower() or "bootstrap" in text.lower()
