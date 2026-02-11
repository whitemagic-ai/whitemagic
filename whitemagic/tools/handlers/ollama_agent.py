"""Ollama Agent Loop — Let local LLMs use WhiteMagic tools.

Implements a tool-calling agent loop where:
1. User gives a task
2. WhiteMagic injects relevant context (memories, system state) as system prompt
3. Sends to local Ollama model
4. Parses response for tool-call intents
5. Executes tool calls through the unified API
6. Feeds results back to the model
7. Repeats until the model signals completion

This bridges the gap between "WhiteMagic can call Ollama" and
"Ollama can call WhiteMagic" — making local AI a first-class citizen.
"""

import json
import logging
import re
import time
from typing import Any

logger = logging.getLogger(__name__)

# Maximum iterations to prevent infinite loops
_MAX_ITERATIONS = 10

# Tools the agent is allowed to call (safe subset)
_ALLOWED_TOOLS = {
    "search_memories", "hybrid_recall", "create_memory", "read_memory",
    "graph_walk", "health_report", "gnosis", "harmony_vector",
    "pattern_search", "cluster_stats", "kg.query", "kg.extract",
    "serendipity_surface", "galaxy.list", "galaxy.status",
    "explain_this", "grimoire_suggest",
}


def _build_system_prompt(task: str, context_memories: list[dict] | None = None) -> str:
    """Build a system prompt that teaches the local model how to use WhiteMagic."""
    tools_desc = "\n".join(f"  - {t}" for t in sorted(_ALLOWED_TOOLS))

    mem_section = ""
    if context_memories:
        mem_lines = []
        for m in context_memories[:5]:
            title = m.get("title", "untitled")
            content = str(m.get("content", ""))[:200]
            mem_lines.append(f"  [{title}]: {content}")
        mem_section = "\n\nRelevant memories from WhiteMagic:\n" + "\n".join(mem_lines)

    return f"""You are a WhiteMagic-powered AI assistant with access to a persistent memory system.
You can call WhiteMagic tools to search memories, create new ones, analyze patterns, and more.

To call a tool, output a JSON block in this exact format:
```tool_call
{{"tool": "tool_name", "args": {{"key": "value"}}}}
```

Available tools:
{tools_desc}

When you're done and have a final answer, just respond normally without a tool_call block.
Do NOT call tools unnecessarily. Search before creating duplicates.
{mem_section}

User's task: {task}"""


def _extract_tool_calls(text: str) -> list[dict[str, Any]]:
    """Extract tool call blocks from model output."""
    calls = []
    # Match ```tool_call ... ``` blocks
    pattern = r'```tool_call\s*\n?(.*?)\n?```'
    matches = re.findall(pattern, text, re.DOTALL)
    for match in matches:
        try:
            parsed = json.loads(match.strip())
            if isinstance(parsed, dict) and "tool" in parsed:
                calls.append(parsed)
        except json.JSONDecodeError:
            continue

    # Also try inline JSON with "tool" key
    if not calls:
        inline_pattern = r'\{[^{}]*"tool"\s*:\s*"[^"]+?"[^{}]*\}'
        for match in re.findall(inline_pattern, text):
            try:
                parsed = json.loads(match)
                if "tool" in parsed:
                    calls.append(parsed)
            except json.JSONDecodeError:
                continue

    return calls


def _get_context_memories(task: str) -> list[dict]:
    """Search WhiteMagic for memories relevant to the task."""
    try:
        from whitemagic.core.memory.unified import get_unified_memory
        um = get_unified_memory()
        results = um.search(task, limit=5)
        return [m.to_dict() if hasattr(m, 'to_dict') else {"content": str(m)} for m in results]
    except Exception:
        return []


def _execute_tool(tool_name: str, args: dict) -> dict[str, Any]:
    """Execute a WhiteMagic tool safely."""
    if tool_name not in _ALLOWED_TOOLS:
        return {"status": "error", "error": f"Tool '{tool_name}' is not allowed. Available: {sorted(_ALLOWED_TOOLS)}"}

    try:
        from whitemagic.tools.unified_api import call_tool
        return call_tool(tool_name, **args)
    except Exception as e:
        return {"status": "error", "error": str(e)}


def handle_ollama_agent(**kwargs: Any) -> dict[str, Any]:
    """Run an agentic loop with a local Ollama model that can use WhiteMagic tools.

    Args:
        model: Ollama model name (e.g. "llama3.2", "phi4", "qwen2.5")
        task: The task or question for the agent
        max_iterations: Maximum tool-call rounds (default 10)
        context: Whether to inject relevant memories (default True)
    """
    model = kwargs.get("model")
    if not model:
        return {"status": "error", "error": "model is required (e.g. 'llama3.2', 'phi4')"}

    task = kwargs.get("task") or kwargs.get("prompt")
    if not task:
        return {"status": "error", "error": "task is required"}

    max_iter = min(kwargs.get("max_iterations", _MAX_ITERATIONS), _MAX_ITERATIONS)
    inject_context = kwargs.get("context", True)

    # Preflight check
    from whitemagic.tools.handlers.ollama import _ollama_preflight, _run, _chat
    try:
        from whitemagic.tools.handlers.ollama import _require_aiohttp
        _require_aiohttp()
    except ImportError as exc:
        return {"status": "error", "error": str(exc), "error_code": "missing_dependency"}

    preflight_error = _ollama_preflight()
    if preflight_error:
        return {"status": "error", "error": preflight_error, "error_code": "service_unavailable"}

    # Build context
    context_memories = _get_context_memories(task) if inject_context else []

    # Initialize conversation
    system_prompt = _build_system_prompt(task, context_memories)
    messages: list[dict[str, str]] = [
        {"role": "system", "content": system_prompt},
        {"role": "user", "content": task},
    ]

    tool_calls_made: list[dict] = []
    total_start = time.time()

    for iteration in range(max_iter):
        try:
            result = _run(_chat(model, messages))
        except Exception as e:
            return {
                "status": "error",
                "error": f"Ollama chat failed: {e}",
                "iterations": iteration,
                "tool_calls": tool_calls_made,
            }

        assistant_text = result.get("response", "")
        messages.append({"role": "assistant", "content": assistant_text})

        # Check for tool calls
        tool_calls = _extract_tool_calls(assistant_text)
        if not tool_calls:
            # No tool calls — agent is done
            return {
                "status": "success",
                "response": assistant_text,
                "model": model,
                "iterations": iteration + 1,
                "tool_calls_made": len(tool_calls_made),
                "tool_calls": tool_calls_made,
                "latency_s": round(time.time() - total_start, 2),
            }

        # Execute tool calls and feed results back
        for tc in tool_calls:
            tool_name = tc["tool"]
            tool_args = tc.get("args", {})

            tc_result = _execute_tool(tool_name, tool_args)
            tool_calls_made.append({
                "iteration": iteration + 1,
                "tool": tool_name,
                "args": tool_args,
                "result_status": tc_result.get("status", "unknown"),
            })

            # Feed result back as a "tool" message
            result_text = json.dumps(tc_result, indent=2, default=str)
            if len(result_text) > 3000:
                result_text = result_text[:3000] + "\n... (truncated)"

            messages.append({
                "role": "user",
                "content": f"Tool result for {tool_name}:\n```json\n{result_text}\n```\n\nContinue with the task or provide your final answer.",
            })

    # Max iterations reached
    return {
        "status": "success",
        "response": messages[-1].get("content", ""),
        "model": model,
        "iterations": max_iter,
        "tool_calls_made": len(tool_calls_made),
        "tool_calls": tool_calls_made,
        "latency_s": round(time.time() - total_start, 2),
        "note": f"Reached max iterations ({max_iter})",
    }
