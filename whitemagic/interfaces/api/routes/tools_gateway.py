"""WhiteMagic Universal REST Gateway
Provides HTTP access to all WhiteMagic tools for any AI system.

Endpoints:
- POST /api/tools/call - Execute a tool
- GET  /api/tools/list - List available tools
- GET  /api/tools/{name} - Get tool details
- POST /api/tools/batch - Execute multiple tools

Works with:
- OpenAI Codex CLI
- Ollama
- Any HTTP client (curl, Python requests, etc.)
- LangChain / LlamaIndex via HTTP
"""

import time
import traceback
from typing import Any

from fastapi import APIRouter, HTTPException, Query
from pydantic import BaseModel, Field

from whitemagic.security.tool_gating import check_tool_execution, get_tool_gate
from whitemagic.tools.registry import (
    TOOL_REGISTRY,
    ToolCategory,
    ToolSafety,
    get_all_tools,
    get_safe_tools,
    get_tool,
)
from whitemagic.tools.unified_api import call_tool

router = APIRouter(prefix="/api/tools", tags=["Tools Gateway"])


# =============================================================================
# REQUEST / RESPONSE MODELS
# =============================================================================

class ToolCallRequest(BaseModel):
    """Request to call a tool."""

    tool: str = Field(..., description="Tool name to call")
    params: dict[str, Any] = Field(default_factory=dict, description="Tool parameters")


class ToolCallResponse(BaseModel):
    """Response from a tool call."""

    success: bool
    tool: str
    result: dict[str, Any] | None = None
    error: str | None = None
    duration_ms: float


class BatchToolCallRequest(BaseModel):
    """Request to call multiple tools."""

    calls: list[ToolCallRequest] = Field(..., description="List of tool calls")
    parallel: bool = Field(default=False, description="Execute in parallel if True")


class BatchToolCallResponse(BaseModel):
    """Response from batch tool calls."""

    results: list[ToolCallResponse]
    total_duration_ms: float


class ToolInfo(BaseModel):
    """Information about a tool."""

    name: str
    description: str
    category: str
    safety: str
    input_schema: dict[str, Any]


class ToolListResponse(BaseModel):
    """Response listing tools."""

    count: int
    tools: list[ToolInfo]
    format: str = "whitemagic"


# =============================================================================
# ENDPOINTS
# =============================================================================

@router.post("/call", response_model=ToolCallResponse)
async def call_tool_endpoint(request: ToolCallRequest) -> ToolCallResponse:
    """Execute a WhiteMagic tool.

    Example:
    ```json
    {
        "tool": "create_memory",
        "params": {
            "title": "API Integration Notes",
            "content": "Successfully connected to WhiteMagic via REST",
            "type": "long_term",
            "tags": ["api", "integration"]
        }
    }
    ```

    """
    # Validate tool exists
    tool_def = get_tool(request.tool)
    if not tool_def:
        raise HTTPException(
            status_code=404,
            detail=f"Tool '{request.tool}' not found. Use GET /api/tools/list to see available tools.",
        )

    # Security gate: check tool and parameter permissions
    gate = get_tool_gate()
    allowed, reason, sanitized_params = check_tool_execution(request.tool, request.params)
    if not allowed:
        raise HTTPException(
            status_code=403,
            detail=f"Tool execution blocked: {reason}",
        )

    # Execute tool
    start_time = time.perf_counter()
    try:
        result = call_tool(request.tool, **sanitized_params)
        duration_ms = (time.perf_counter() - start_time) * 1000

        # Check for error in result
        if result.get("status") == "error":
            return ToolCallResponse(
                success=False,
                tool=request.tool,
                error=gate.sanitize_error(Exception(result.get("message", "Unknown error"))),
                result=result if gate.deployment_mode == "local" else None,
                duration_ms=duration_ms,
            )

        return ToolCallResponse(
            success=True,
            tool=request.tool,
            result=result,
            duration_ms=duration_ms,
        )
    except Exception as e:
        duration_ms = (time.perf_counter() - start_time) * 1000
        return ToolCallResponse(
            success=False,
            tool=request.tool,
            error=gate.sanitize_error(e),
            result={"traceback": traceback.format_exc()} if gate.deployment_mode == "local" else None,
            duration_ms=duration_ms,
        )


@router.post("/batch", response_model=BatchToolCallResponse)
async def batch_call_tools(request: BatchToolCallRequest) -> BatchToolCallResponse:
    """Execute multiple tools in sequence or parallel.

    Example:
    ```json
    {
        "calls": [
            {"tool": "search_memories", "params": {"query": "architecture"}},
            {"tool": "list_memories", "params": {"limit": 5}}
        ],
        "parallel": false
    }
    ```

    """
    start_time = time.perf_counter()
    results = []

    gate = get_tool_gate()

    # Sequential execution (parallel available via asyncio.gather for independent calls)
    for call in request.calls:
        tool_start = time.perf_counter()
        tool_def = get_tool(call.tool)

        if not tool_def:
            results.append(ToolCallResponse(
                success=False,
                tool=call.tool,
                error=f"Tool '{call.tool}' not found",
                duration_ms=(time.perf_counter() - tool_start) * 1000,
            ))
            continue

        # Security gate check
        allowed, reason, sanitized_params = check_tool_execution(call.tool, call.params)
        if not allowed:
            results.append(ToolCallResponse(
                success=False,
                tool=call.tool,
                error=f"Blocked: {reason}",
                duration_ms=(time.perf_counter() - tool_start) * 1000,
            ))
            continue

        try:
            result = call_tool(call.tool, **sanitized_params)
            duration_ms = (time.perf_counter() - tool_start) * 1000

            if result.get("status") == "error":
                results.append(ToolCallResponse(
                    success=False,
                    tool=call.tool,
                    error=gate.sanitize_error(Exception(result.get("message", "Unknown error"))),
                    result=result if gate.deployment_mode == "local" else None,
                    duration_ms=duration_ms,
                ))
            else:
                results.append(ToolCallResponse(
                    success=True,
                    tool=call.tool,
                    result=result,
                    duration_ms=duration_ms,
                ))
        except Exception as e:
            results.append(ToolCallResponse(
                success=False,
                tool=call.tool,
                error=gate.sanitize_error(e),
                duration_ms=(time.perf_counter() - tool_start) * 1000,
            ))

    total_duration = (time.perf_counter() - start_time) * 1000
    return BatchToolCallResponse(results=results, total_duration_ms=total_duration)


@router.get("/list", response_model=ToolListResponse)
async def list_tools(
    category: str | None = Query(None, description="Filter by category"),
    safety: str | None = Query(None, description="Filter by safety level (read, write, delete)"),
    format: str = Query("whitemagic", description="Output format: whitemagic, openai, mcp"),
    safe_only: bool = Query(False, description="Only return read-safe tools"),
) -> ToolListResponse | dict[str, Any]:
    """List available tools with optional filtering.

    Formats:
    - `whitemagic`: Native format with full metadata
    - `openai`: OpenAI function calling format
    - `mcp`: MCP tool format
    """
    tools = get_all_tools()

    # Apply filters
    if safe_only:
        tools = get_safe_tools()
    elif safety:
        try:
            safety_enum = ToolSafety(safety)
            tools = [t for t in tools if t.safety == safety_enum]
        except ValueError:
            raise HTTPException(400, f"Invalid safety level: {safety}")

    if category:
        try:
            category_enum = ToolCategory(category)
            tools = [t for t in tools if t.category == category_enum]
        except ValueError:
            raise HTTPException(400, f"Invalid category: {category}")

    # Format output
    if format == "openai":
        return {
            "count": len(tools),
            "tools": [t.to_openai_function()["function"] for t in tools],
            "format": "openai",
        }
    elif format == "mcp":
        return {
            "count": len(tools),
            "tools": [t.to_mcp_tool() for t in tools],
            "format": "mcp",
        }
    else:
        return ToolListResponse(
            count=len(tools),
            tools=[
                ToolInfo(
                    name=t.name,
                    description=t.description,
                    category=t.category.value,
                    safety=t.safety.value,
                    input_schema=t.input_schema,
                )
                for t in tools
            ],
            format="whitemagic",
        )


@router.get("/categories/list")
async def list_categories() -> dict[str, Any]:
    """List all tool categories."""
    return {
        "categories": [c.value for c in ToolCategory],
        "count": len(ToolCategory),
    }


@router.get("/health")
async def tools_health() -> dict[str, Any]:
    """Health check for tools gateway."""
    return {
        "status": "healthy",
        "tools_available": len(TOOL_REGISTRY),
        "categories": len(ToolCategory),
    }


@router.get("/{tool_name}")
async def get_tool_info(tool_name: str, format: str = Query("whitemagic")) -> dict[str, Any]:
    """Get detailed information about a specific tool.
    """
    tool = get_tool(tool_name)
    if not tool:
        raise HTTPException(404, f"Tool '{tool_name}' not found")

    if format == "openai":
        return tool.to_openai_function()
    elif format == "mcp":
        return tool.to_mcp_tool()
    else:
        return tool.to_dict()
