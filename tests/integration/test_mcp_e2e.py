#!/usr/bin/env python3
"""End-to-end MCP server test — runs the lean server **in-process** using
anyio memory streams (no subprocess, no pipe buffering) and verifies
tool-call round trips.

Requires: mcp SDK + anyio installed (skip if unavailable).

Usage:
    pytest tests/integration/test_mcp_e2e.py -v
"""
from __future__ import annotations

import asyncio
import json
import os
import shutil
import tempfile
from contextlib import asynccontextmanager
from pathlib import Path
from typing import Any, AsyncGenerator

import pytest

ROOT = Path(__file__).resolve().parent.parent.parent

# Skip entire module if mcp SDK not installed
try:
    import anyio
    import mcp.types as types
    from mcp.shared.message import SessionMessage

    HAS_MCP = True
except ImportError:
    HAS_MCP = False

pytestmark = pytest.mark.skipif(not HAS_MCP, reason="mcp SDK not installed")


# ── Environment setup ───────────────────────────────────────────────────

@pytest.fixture(scope="module", autouse=True)
def _mcp_env():
    """Set up an isolated WM_STATE_ROOT before the server module is imported."""
    state_dir = Path(tempfile.mkdtemp(prefix="wm_mcp_e2e_"))
    prev = {
        "WM_SILENT_INIT": os.environ.get("WM_SILENT_INIT"),
        "WM_STATE_ROOT": os.environ.get("WM_STATE_ROOT"),
    }
    os.environ["WM_SILENT_INIT"] = "1"
    os.environ["WM_STATE_ROOT"] = str(state_dir)
    yield
    for k, v in prev.items():
        if v is None:
            os.environ.pop(k, None)
        else:
            os.environ[k] = v
    shutil.rmtree(state_dir, ignore_errors=True)


# ── In-process MCP session ──────────────────────────────────────────────

class _InProcessClient:
    """Sends JSON-RPC messages through anyio memory streams."""

    def __init__(self, tx: Any, rx: Any) -> None:
        self._tx = tx
        self._rx = rx
        self._id = 0

    def _next_id(self) -> int:
        self._id += 1
        return self._id

    async def request(
        self, method: str, params: dict | None = None, *, timeout: float = 15.0,
    ) -> dict:
        """Send a JSON-RPC *request* and wait for the matching response."""
        req_id = self._next_id()
        raw: dict[str, Any] = {"jsonrpc": "2.0", "id": req_id, "method": method}
        if params is not None:
            raw["params"] = params

        msg = types.JSONRPCMessage.model_validate(raw)
        await self._tx.send(SessionMessage(msg))

        async def _wait() -> dict:
            while True:
                session_msg = await self._rx.receive()
                data = json.loads(
                    session_msg.message.model_dump_json(
                        by_alias=True, exclude_none=True,
                    )
                )
                # Skip server-initiated notifications (no "id" field)
                if "id" in data:
                    return data

        return await asyncio.wait_for(_wait(), timeout=timeout)

    async def notify(self, method: str, params: dict | None = None) -> None:
        """Send a JSON-RPC *notification* (fire-and-forget, no response)."""
        raw: dict[str, Any] = {"jsonrpc": "2.0", "method": method}
        if params is not None:
            raw["params"] = params
        msg = types.JSONRPCMessage.model_validate(raw)
        await self._tx.send(SessionMessage(msg))

    async def initialize(self) -> dict:
        """Perform the full MCP initialize handshake."""
        resp = await self.request("initialize", {
            "protocolVersion": "2024-11-05",
            "capabilities": {},
            "clientInfo": {"name": "test-e2e", "version": "1.0.0"},
        })
        await self.notify("notifications/initialized")
        return resp


@asynccontextmanager
async def mcp_session(
    *, init: bool = True,
) -> AsyncGenerator[_InProcessClient, None]:
    """Start an in-process MCP server session.

    Creates anyio memory streams, starts ``server.run()`` as a background
    task, optionally performs the initialize handshake, then yields a
    client for sending further requests.
    """
    from whitemagic.run_mcp_lean import server

    to_server_tx, to_server_rx = anyio.create_memory_object_stream(16)
    from_server_tx, from_server_rx = anyio.create_memory_object_stream(16)

    server_task = asyncio.create_task(
        server.run(
            to_server_rx, from_server_tx,
            server.create_initialization_options(),
        )
    )

    client = _InProcessClient(to_server_tx, from_server_rx)
    try:
        if init:
            await client.initialize()
        yield client
    finally:
        await to_server_tx.aclose()
        server_task.cancel()
        try:
            await server_task
        except (asyncio.CancelledError, Exception):
            pass


# ── Tests ────────────────────────────────────────────────────────────────

class TestMCPInitialize:
    """Test MCP initialization handshake."""

    async def test_initialize(self):
        """Server responds to initialize with capabilities."""
        async with mcp_session(init=False) as client:
            response = await client.request("initialize", {
                "protocolVersion": "2024-11-05",
                "capabilities": {},
                "clientInfo": {"name": "test-client", "version": "1.0.0"},
            })

            assert "result" in response, f"Expected result, got: {response}"
            result = response["result"]
            assert "serverInfo" in result
            assert "capabilities" in result
            server_name = result["serverInfo"]["name"]
            assert "whitemagic" in server_name.lower()

    async def test_initialized_notification(self):
        """Server accepts initialized notification without error."""
        async with mcp_session(init=False) as client:
            resp = await client.request("initialize", {
                "protocolVersion": "2024-11-05",
                "capabilities": {},
                "clientInfo": {"name": "test-client", "version": "1.0.0"},
            })
            assert "result" in resp
            # Fire-and-forget notification — must not raise
            await client.notify("notifications/initialized")


class TestMCPToolsList:
    """Test tools/list endpoint."""

    async def test_list_tools(self):
        """Server lists 28 PRAT Gana meta-tools."""
        async with mcp_session() as client:
            response = await client.request("tools/list", {})
            assert "result" in response, f"Expected result, got: {response}"

            tools = response["result"].get("tools", [])
            assert len(tools) >= 28, f"Expected ≥28 tools, got {len(tools)}"

            tool_names = {t["name"] for t in tools}
            # Verify a few known Ganas exist
            for expected in ["gana_horn", "gana_neck", "gana_root", "gana_ghost"]:
                assert expected in tool_names, f"Missing expected tool: {expected}"


class TestMCPResourcesList:
    """Test resources/list endpoint."""

    async def test_list_resources(self):
        """Server lists orientation docs and workflow templates."""
        async with mcp_session() as client:
            response = await client.request("resources/list", {})
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

    async def test_call_gana_root_health(self):
        """Invoke gana_root with health_report tool."""
        async with mcp_session() as client:
            response = await client.request("tools/call", {
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

    async def test_call_gana_ghost_capabilities(self):
        """Invoke gana_ghost with capabilities tool."""
        async with mcp_session() as client:
            response = await client.request("tools/call", {
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

    async def test_read_server_instructions(self):
        """Read the server-instructions resource."""
        async with mcp_session() as client:
            response = await client.request("resources/read", {
                "uri": "whitemagic://orientation/server-instructions",
            })
            assert "result" in response, f"Expected result, got: {response}"

            contents = response["result"].get("contents", [])
            assert len(contents) > 0
            text = contents[0].get("text", "")
            assert "WhiteMagic" in text

    async def test_read_workflow_template(self):
        """Read a workflow template resource."""
        async with mcp_session() as client:
            response = await client.request("resources/read", {
                "uri": "whitemagic://workflow/new_session",
            })
            assert "result" in response, f"Expected result, got: {response}"

            contents = response["result"].get("contents", [])
            assert len(contents) > 0
            text = contents[0].get("text", "")
            assert "session" in text.lower() or "bootstrap" in text.lower()
