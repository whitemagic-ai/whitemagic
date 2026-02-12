"""OpenAI-Compatible API Endpoint for WhiteMagic
Allows OpenAI SDK, Codex CLI, and Ollama to use WhiteMagic tools.

Implements:
- POST /v1/chat/completions - Chat completions with function/tool calling
- GET  /v1/models - List available models
- POST /v1/completions - Legacy completions (tool execution only)

Usage with OpenAI SDK:
```python
from openai import OpenAI

client = OpenAI(
    base_url="http://localhost:8000/v1",
    api_key="not-needed-for-local-use"  # WhiteMagic doesn't require API key for local inference
)

response = client.chat.completions.create(
    model="whitemagic",
    messages=[{"role": "user", "content": "Search my memories for architecture notes"}],
    tools=client.get("/tools")  # Or use WhiteMagic's tool definitions
)
```

Usage with Ollama (OpenAI-compat mode):
```bash
curl http://localhost:8000/v1/chat/completions \
  -H "Content-Type: application/json" \
  -d '{
    "model": "whitemagic",
    "messages": [{"role": "user", "content": "Create a memory about this session"}],
    "tool_choice": "auto"
  }'
```
"""

import json
import time
import uuid
from typing import Any, Literal

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, Field

from whitemagic.config import VERSION
from whitemagic.security.tool_gating import check_tool_execution, get_tool_gate
from whitemagic.tools.registry import get_tool, to_openai_tools
from whitemagic.tools.unified_api import call_tool

router = APIRouter(prefix="/v1", tags=["OpenAI Compatible"])


# =============================================================================
# OPENAI-COMPATIBLE REQUEST/RESPONSE MODELS
# =============================================================================

class ChatMessage(BaseModel):
    """OpenAI-compatible chat message."""

    role: Literal["system", "user", "assistant", "tool", "function"]
    content: str | None = None
    name: str | None = None
    tool_calls: list[dict[str, Any]] | None = None
    tool_call_id: str | None = None
    function_call: dict[str, Any] | None = None  # Legacy


class FunctionCall(BaseModel):
    """Function call in a tool call."""

    name: str
    arguments: str  # JSON string


class ToolCall(BaseModel):
    """Tool call from assistant."""

    id: str
    type: Literal["function"] = "function"
    function: FunctionCall


class ChatCompletionRequest(BaseModel):
    """OpenAI-compatible chat completion request."""

    model: str = Field(default="whitemagic")
    messages: list[ChatMessage]
    tools: list[dict[str, Any]] | None = None
    tool_choice: str | dict[str, Any] | None = None  # "auto", "none", or {"type": "function", "function": {"name": "..."}}
    temperature: float = 0.7
    max_tokens: int | None = None
    stream: bool = False
    # WhiteMagic extensions
    execute_tools: bool = Field(default=True, description="If True, execute tool calls automatically")
    safe_mode: bool = Field(default=False, description="If True, only allow read operations")


class Choice(BaseModel):
    """Chat completion choice."""

    index: int
    message: ChatMessage
    finish_reason: Literal["stop", "tool_calls", "length", "content_filter"] | None


class Usage(BaseModel):
    """Token usage (simulated for WhiteMagic)."""

    prompt_tokens: int
    completion_tokens: int
    total_tokens: int


class ChatCompletionResponse(BaseModel):
    """OpenAI-compatible chat completion response."""

    id: str
    object: str = "chat.completion"
    created: int
    model: str
    choices: list[Choice]
    usage: Usage
    # WhiteMagic extensions
    tool_results: list[dict[str, Any]] | None = None


class ModelInfo(BaseModel):
    """Model information."""

    id: str
    object: str = "model"
    created: int
    owned_by: str


class ModelsResponse(BaseModel):
    """List models response."""

    object: str = "list"
    data: list[ModelInfo]


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def parse_tool_calls_from_message(message: ChatMessage) -> list[tuple[str, dict[str, Any]]]:
    """Extract tool calls from a message."""
    calls = []

    # Modern tool_calls format
    if message.tool_calls:
        for tc in message.tool_calls:
            if isinstance(tc, dict):
                func = tc.get("function", {})
                name = func.get("name", "")
                args_str = func.get("arguments", "{}")
            else:
                name = tc.function.name
                args_str = tc.function.arguments

            try:
                args = json.loads(args_str) if args_str else {}
            except json.JSONDecodeError:
                args = {}
            calls.append((name, args))

    # Legacy function_call format
    elif message.function_call:
        name = message.function_call.get("name", "")
        args_str = message.function_call.get("arguments", "{}")
        try:
            args = json.loads(args_str) if args_str else {}
        except json.JSONDecodeError:
            args = {}
        calls.append((name, args))

    return calls


def detect_tool_intent(messages: list[ChatMessage]) -> list[tuple[str, dict[str, Any]]]:
    """Detect tool intent from natural language messages.
    This is a simple pattern matcher for common requests.
    For full LLM-based intent detection, use edge_infer or external model.
    """
    if not messages:
        return []

    last_message = messages[-1]
    if last_message.role != "user" or not last_message.content:
        return []

    content = last_message.content.lower()
    detected: list[tuple[str, dict[str, Any]]] = []

    # Memory operations
    if any(kw in content for kw in ["search", "find", "look for", "query"]) and "memor" in content:
        # Extract query - simple heuristic
        query = last_message.content.split("for")[-1].strip() if "for" in content else content
        detected.append(("search_memories", {"query": query[:100]}))

    elif any(kw in content for kw in ["create", "save", "store", "remember"]) and "memor" in content:
        detected.append(("create_memory", {
            "title": "User Request",
            "content": last_message.content,
            "type": "short_term",
        }))

    elif any(kw in content for kw in ["list", "show", "all"]) and "memor" in content:
        detected.append(("list_memories", {"limit": 20}))

    # Session operations
    elif "session" in content and any(kw in content for kw in ["start", "create", "new"]):
        detected.append(("create_session", {"name": "Auto-created session"}))

    elif "session" in content and "status" in content:
        detected.append(("session_bootstrap", {}))

    # Metrics
    elif "balance" in content or "yin" in content or "yang" in content:
        detected.append(("get_yin_yang_balance", {}))

    # Garden
    elif "garden" in content:
        if "status" in content or "list" in content:
            detected.append(("garden_status", {}))

    return detected


def execute_tool_calls(
    calls: list[tuple[str, dict[str, Any]]],
    safe_mode: bool = False,
) -> list[dict[str, Any]]:
    """Execute a list of tool calls and return results."""
    results = []
    gate = get_tool_gate()

    for tool_name, params in calls:
        tool_def = get_tool(tool_name)
        if not tool_def:
            results.append({
                "tool": tool_name,
                "success": False,
                "error": f"Tool '{tool_name}' not found",
            })
            continue

        # Security gate check - replaces simple safe_mode check
        allowed, reason, sanitized_params = check_tool_execution(tool_name, params)
        if not allowed:
            results.append({
                "tool": tool_name,
                "success": False,
                "error": f"Blocked: {reason}",
            })
            continue

        # Additional safe mode check (stricter than security gate)
        if safe_mode and tool_def.safety.value != "read":
            results.append({
                "tool": tool_name,
                "success": False,
                "error": f"Tool '{tool_name}' requires {tool_def.safety.value} permission (safe_mode=True)",
            })
            continue

        try:
            result = call_tool(tool_name, **sanitized_params)
            results.append({
                "tool": tool_name,
                "success": result.get("status") != "error",
                "result": result,
            })
        except Exception as e:
            results.append({
                "tool": tool_name,
                "success": False,
                "error": gate.sanitize_error(e),
            })

    return results


# =============================================================================
# ENDPOINTS
# =============================================================================

@router.post("/chat/completions", response_model=ChatCompletionResponse)
async def chat_completions(request: ChatCompletionRequest) -> ChatCompletionResponse:
    """OpenAI-compatible chat completions endpoint.

    Supports:
    - Tool/function calling (execute WhiteMagic tools)
    - Natural language tool detection (optional)
    - Automatic tool execution (configurable)

    Example with explicit tool call:
    ```json
    {
        "model": "whitemagic",
        "messages": [
            {"role": "user", "content": "Search memories"},
            {"role": "assistant", "tool_calls": [
                {"id": "call_1", "type": "function", "function": {
                    "name": "search_memories",
                    "arguments": "{\"query\": \"architecture\"}"
                }}
            ]}
        ],
        "execute_tools": true
    }
    ```
    """
    request_id = f"chatcmpl-{uuid.uuid4().hex[:12]}"
    created = int(time.time())

    # Collect tool calls from messages
    tool_calls_to_execute = []

    for msg in request.messages:
        if msg.role == "assistant":
            calls = parse_tool_calls_from_message(msg)
            tool_calls_to_execute.extend(calls)

    # If no explicit tool calls but user message exists, try to detect intent
    if not tool_calls_to_execute and request.tool_choice != "none":
        detected = detect_tool_intent(request.messages)
        tool_calls_to_execute.extend(detected)

    # Execute tools if requested
    tool_results = None
    response_content = None
    finish_reason: Literal["stop", "tool_calls", "length", "content_filter"] = "stop"
    response_tool_calls = None

    if tool_calls_to_execute:
        if request.execute_tools:
            # Execute and return results
            tool_results = execute_tool_calls(tool_calls_to_execute, request.safe_mode)

            # Format response content
            if len(tool_results) == 1:
                result = tool_results[0]
                if result["success"]:
                    response_content = json.dumps(result["result"], indent=2)
                else:
                    response_content = f"Error: {result.get('error', 'Unknown error')}"
            else:
                response_content = json.dumps({
                    "tool_results": tool_results,
                    "summary": f"Executed {len(tool_results)} tools",
                }, indent=2)

            finish_reason = "stop"
        else:
            # Return tool calls without executing
            response_tool_calls = [
                {
                    "id": f"call_{uuid.uuid4().hex[:8]}",
                    "type": "function",
                    "function": {
                        "name": name,
                        "arguments": json.dumps(args),
                    },
                }
                for name, args in tool_calls_to_execute
            ]
            finish_reason = "tool_calls"
    else:
        # No tools detected - return helpful message
        response_content = (
            "No tool calls detected. Available operations:\n"
            "- search_memories: Search your memories\n"
            "- create_memory: Create a new memory\n"
            "- list_memories: List all memories\n"
            "- session_bootstrap: Initialize session\n"
            "- get_yin_yang_balance: Check activity balance\n\n"
            "Use the 'tools' parameter or include tool calls in messages."
        )

    # Build response
    response_message = ChatMessage(
        role="assistant",
        content=response_content,
        tool_calls=response_tool_calls,
    )

    # Estimate token usage (rough approximation)
    prompt_tokens = sum(len(m.content or "") // 4 for m in request.messages)
    completion_tokens = len(response_content or "") // 4 if response_content else 0

    return ChatCompletionResponse(
        id=request_id,
        created=created,
        model=request.model,
        choices=[
            Choice(
                index=0,
                message=response_message,
                finish_reason=finish_reason,
            ),
        ],
        usage=Usage(
            prompt_tokens=prompt_tokens,
            completion_tokens=completion_tokens,
            total_tokens=prompt_tokens + completion_tokens,
        ),
        tool_results=tool_results,
    )


@router.get("/models", response_model=ModelsResponse)
async def list_models() -> ModelsResponse:
    """List available models (WhiteMagic exposes itself as a model)."""
    created = int(time.time())
    return ModelsResponse(
        data=[
            ModelInfo(
                id="whitemagic",
                created=created,
                owned_by="whitemagic",
            ),
            ModelInfo(
                id=f"whitemagic-{VERSION}",
                created=created,
                owned_by="whitemagic",
            ),
            ModelInfo(
                id="whitemagic-tools",
                created=created,
                owned_by="whitemagic",
            ),
        ],
    )


@router.get("/models/{model_id}")
async def get_model(model_id: str) -> ModelInfo:
    """Get model info."""
    if not model_id.startswith("whitemagic"):
        raise HTTPException(404, f"Model '{model_id}' not found")
    return ModelInfo(
        id=model_id,
        created=int(time.time()),
        owned_by="whitemagic",
    )


@router.get("/tools")
async def get_tools() -> list[dict[str, Any]]:
    """Get WhiteMagic tools in OpenAI function format.
    Use this to populate the 'tools' parameter in chat completions.
    """
    return to_openai_tools()


@router.post("/tools/execute")
async def execute_tool_directly(
    tool_name: str,
    arguments: dict[str, Any],
    safe_mode: bool = False,
) -> dict[str, Any]:
    """Direct tool execution endpoint (simpler than chat completions).

    Example:
    ```bash
    curl -X POST "http://localhost:8000/v1/tools/execute?tool_name=search_memories" \
      -H "Content-Type: application/json" \
      -d '{"query": "architecture"}'
    ```

    """
    results = execute_tool_calls([(tool_name, arguments)], safe_mode)
    if not results:
        raise HTTPException(500, "No results returned")
    return results[0]


# =============================================================================
# HEALTH CHECK
# =============================================================================

@router.get("/health")
async def health() -> dict[str, Any]:
    """Health check for OpenAI-compatible API."""
    return {
        "status": "healthy",
        "version": VERSION,
        "endpoints": [
            "/v1/chat/completions",
            "/v1/models",
            "/v1/tools",
            "/v1/tools/execute",
        ],
    }
