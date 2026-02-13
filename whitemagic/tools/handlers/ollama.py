"""Ollama tool handlers — optional bridge to local LLM inference via Ollama.

Provides generate, chat, and model listing through the Whitemagic tool
contract.  Requires ``aiohttp`` (``whitemagic[net]``) and a running
Ollama server.

v15.5: Context injection pipeline — automatically enriches prompts with
relevant WhiteMagic memories via hybrid search + graph walk.  Responses
can optionally be stored back (Memory-Augmented Generation).
"""
import asyncio
import logging
import os
import socket
import time
from collections.abc import Coroutine
from typing import Any, TypeVar, cast

T = TypeVar("T")
logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Lazy aiohttp import
# ---------------------------------------------------------------------------

def _require_aiohttp() -> Any:
    try:
        import aiohttp
        return aiohttp
    except ImportError:
        raise ImportError(
            "aiohttp is required for Ollama tools. "
            "Install with: pip install 'whitemagic[net]'",
        )


def _run(coro: Coroutine[Any, Any, T]) -> T:
    """Run an async coroutine from a sync handler context."""
    try:
        asyncio.get_running_loop()
    except RuntimeError:
        return asyncio.run(coro)
    from concurrent.futures import ThreadPoolExecutor
    with ThreadPoolExecutor(max_workers=1) as pool:
        return pool.submit(asyncio.run, coro).result()


def _ollama_url() -> str:
    host, port = _ollama_host_port()
    return f"http://{host}:{port}"


def _ollama_host_port() -> tuple[str, int]:
    host = os.environ.get("OLLAMA_HOST", "localhost")
    port_raw = os.environ.get("OLLAMA_PORT", "11434")
    try:
        port = int(port_raw)
    except ValueError:
        port = 11434
    return host, port


def _request_timeout(default: float) -> float:
    raw = os.environ.get("WHITEMAGIC_OLLAMA_TIMEOUT_S")
    if not raw:
        return default
    try:
        return max(1.0, float(raw))
    except ValueError:
        return default


def _ollama_preflight() -> str | None:
    preflight = os.environ.get("WHITEMAGIC_OLLAMA_PREFLIGHT", "1").strip().lower()
    if preflight in {"0", "false", "no", "off"}:
        return None

    host, port = _ollama_host_port()
    timeout_raw = os.environ.get("WHITEMAGIC_OLLAMA_PREFLIGHT_TIMEOUT_S", "1.0")
    try:
        timeout = max(0.2, float(timeout_raw))
    except ValueError:
        timeout = 1.0

    try:
        with socket.create_connection((host, port), timeout=timeout):
            return None
    except OSError as exc:
        return f"Ollama unavailable at http://{host}:{port} ({exc})"


# ---------------------------------------------------------------------------
# Context injection pipeline (v15.5)
# ---------------------------------------------------------------------------

def _inject_context(
    prompt: str,
    *,
    max_memories: int = 5,
    max_chars_per_memory: int = 400,
    strategy: str = "hybrid",
) -> tuple[str, list[dict[str, Any]]]:
    """Pull relevant WhiteMagic memories and build a context-enriched prompt.

    Returns (enriched_prompt, context_memories_used).
    Strategy can be 'hybrid' (FTS + vector + graph), 'search', or 'none'.
    """
    if strategy == "none":
        return prompt, []

    memories: list[dict[str, Any]] = []
    try:
        from whitemagic.core.memory.unified import get_unified_memory
        um = get_unified_memory()

        # Sanitize query for FTS5 — strip special chars that break syntax
        import re
        safe_query = re.sub(r'[^\w\s]', ' ', prompt).strip()
        if not safe_query:
            return prompt, []

        results: list[Any] = []
        if strategy == "hybrid":
            try:
                results = um.hybrid_recall(safe_query, final_limit=max_memories)
            except Exception:
                results = um.search(safe_query, limit=max_memories)
        else:
            results = um.search(safe_query, limit=max_memories)

        for m in results:
            if isinstance(m, dict):
                memories.append(dict(m))
            elif hasattr(m, "to_dict"):
                memories.append(m.to_dict())
            else:
                memories.append({"content": str(m)})
    except Exception as e:
        logger.debug(f"Context injection failed (non-fatal): {e}")
        return prompt, []

    if not memories:
        return prompt, []

    # Build context block
    ctx_lines = []
    for m in memories:
        title = m.get("title") or "untitled"
        content = str(m.get("content", ""))[:max_chars_per_memory]
        tags = m.get("tags", [])
        tag_str = f" [{', '.join(tags[:5])}]" if tags else ""
        ctx_lines.append(f"- {title}{tag_str}: {content}")

    context_block = (
        "[WhiteMagic Context — relevant memories from your knowledge base]\n"
        + "\n".join(ctx_lines)
        + "\n[End Context]\n\n"
    )
    return context_block + prompt, memories


def _maybe_store_output(
    prompt: str,
    response: str,
    model: str,
    *,
    min_length: int = 100,
) -> str | None:
    """Store a useful Ollama response back into WhiteMagic (Memory-Augmented Generation).

    Returns the memory ID if stored, else None.
    """
    if len(response.strip()) < min_length:
        return None

    try:
        from whitemagic.core.memory.unified import get_unified_memory
        um = get_unified_memory()

        title = f"Ollama [{model}]: {prompt[:80]}"
        mem = um.store(
            content=response,
            title=title,
            tags={"ollama", "generated", f"model:{model}"},
            importance=0.3,
        )
        return mem.id if hasattr(mem, "id") else str(mem)
    except Exception as e:
        logger.debug(f"MAG store failed (non-fatal): {e}")
        return None


# ---------------------------------------------------------------------------
# Internal async client
# ---------------------------------------------------------------------------

async def _list_models() -> list[dict[str, Any]]:
    aiohttp = _require_aiohttp()
    url = f"{_ollama_url()}/api/tags"
    async with aiohttp.ClientSession() as session:
        timeout = aiohttp.ClientTimeout(total=_request_timeout(15.0))
        async with session.get(url, timeout=timeout) as resp:
            resp.raise_for_status()
            data = await resp.json()
            return cast("list[dict[str, Any]]", data.get("models", []))


async def _generate(
    model: str,
    prompt: str,
    stream: bool = False,
    system: str | None = None,
) -> dict[str, Any]:
    aiohttp = _require_aiohttp()
    url = f"{_ollama_url()}/api/generate"
    payload: dict[str, Any] = {"model": model, "prompt": prompt, "stream": False}
    if system:
        payload["system"] = system
    start = time.time()
    async with aiohttp.ClientSession() as session:
        timeout = aiohttp.ClientTimeout(total=_request_timeout(300.0))
        async with session.post(url, json=payload, timeout=timeout) as resp:
            resp.raise_for_status()
            result = await resp.json()
            elapsed = time.time() - start
            return {
                "response": result.get("response", ""),
                "model": model,
                "done": result.get("done", True),
                "total_duration_ns": result.get("total_duration"),
                "eval_count": result.get("eval_count"),
                "latency_s": round(elapsed, 2),
            }


async def _chat(model: str, messages: list[dict[str, Any]]) -> dict[str, Any]:
    aiohttp = _require_aiohttp()
    url = f"{_ollama_url()}/api/chat"
    payload = {"model": model, "messages": messages, "stream": False}
    start = time.time()
    async with aiohttp.ClientSession() as session:
        timeout = aiohttp.ClientTimeout(total=_request_timeout(300.0))
        async with session.post(url, json=payload, timeout=timeout) as resp:
            resp.raise_for_status()
            result = await resp.json()
            elapsed = time.time() - start
            return {
                "response": result.get("message", {}).get("content", ""),
                "model": model,
                "done": result.get("done", True),
                "latency_s": round(elapsed, 2),
            }


# ---------------------------------------------------------------------------
# Public handlers
# ---------------------------------------------------------------------------

def handle_ollama_models(**kwargs: Any) -> dict[str, Any]:
    """List available Ollama models."""
    try:
        _require_aiohttp()
    except ImportError as exc:
        return {"status": "error", "error": str(exc), "error_code": "missing_dependency"}

    preflight_error = _ollama_preflight()
    if preflight_error:
        return {
            "status": "error",
            "error": preflight_error,
            "error_code": "service_unavailable",
            "ollama_url": _ollama_url(),
        }
    try:
        models = _run(_list_models())
        model_list = []
        for m in models:
            model_list.append({
                "name": m.get("name", "unknown"),
                "size_bytes": m.get("size", 0),
                "size_gb": round(m.get("size", 0) / 1e9, 1),
                "modified_at": m.get("modified_at", ""),
            })
        return {
            "status": "success",
            "count": len(model_list),
            "models": model_list,
            "ollama_url": _ollama_url(),
        }
    except ImportError as exc:
        return {"status": "error", "error": str(exc), "error_code": "missing_dependency"}
    except Exception as exc:
        return {
            "status": "error",
            "error": f"Cannot reach Ollama at {_ollama_url()}: {exc}",
            "ollama_url": _ollama_url(),
        }


def handle_ollama_generate(**kwargs: Any) -> dict[str, Any]:
    """Generate text using a local Ollama model.

    Args:
        model: Ollama model name (e.g. "llama3.1:8b")
        prompt: The text prompt
        context: Inject relevant WhiteMagic memories (default True)
        context_strategy: 'hybrid', 'search', or 'none'
        store: Store useful responses back into WhiteMagic (default False)
        system: Optional system prompt override
    """
    model = kwargs.get("model")
    if not model:
        return {"status": "error", "error": "model is required"}
    prompt = kwargs.get("prompt")
    if not prompt:
        return {"status": "error", "error": "prompt is required"}

    try:
        _require_aiohttp()
    except ImportError as exc:
        return {"status": "error", "error": str(exc), "error_code": "missing_dependency"}

    preflight_error = _ollama_preflight()
    if preflight_error:
        return {
            "status": "error",
            "error": preflight_error,
            "error_code": "service_unavailable",
            "ollama_url": _ollama_url(),
        }

    # Context injection
    inject = kwargs.get("context", True)
    strategy = kwargs.get("context_strategy", "hybrid") if inject else "none"
    enriched_prompt, ctx_memories = _inject_context(
        prompt, strategy=strategy,
        max_memories=int(kwargs.get("max_context", 5)),
    )

    system_prompt = kwargs.get("system")

    try:
        result = _run(_generate(model, enriched_prompt, system=system_prompt))
        result["context_injected"] = len(ctx_memories)

        # Memory-Augmented Generation
        if kwargs.get("store", False):
            stored_id = _maybe_store_output(prompt, result.get("response", ""), model)
            if stored_id:
                result["stored_memory_id"] = stored_id

        return {"status": "success", **result}
    except ImportError as exc:
        return {"status": "error", "error": str(exc), "error_code": "missing_dependency"}
    except Exception as exc:
        return {"status": "error", "error": str(exc)}


def handle_ollama_chat(**kwargs: Any) -> dict[str, Any]:
    """Chat with a local Ollama model (multi-turn).

    Args:
        model: Ollama model name (e.g. "llama3.1:8b")
        messages: Array of {role, content} message objects
        context: Inject relevant WhiteMagic memories as system message (default True)
        context_strategy: 'hybrid', 'search', or 'none'
        store: Store useful responses back into WhiteMagic (default False)
    """
    model = kwargs.get("model")
    if not model:
        return {"status": "error", "error": "model is required"}
    messages = kwargs.get("messages")
    if not messages or not isinstance(messages, list):
        return {"status": "error", "error": "messages is required (array of {role, content})"}

    try:
        _require_aiohttp()
    except ImportError as exc:
        return {"status": "error", "error": str(exc), "error_code": "missing_dependency"}

    preflight_error = _ollama_preflight()
    if preflight_error:
        return {
            "status": "error",
            "error": preflight_error,
            "error_code": "service_unavailable",
            "ollama_url": _ollama_url(),
        }

    # Context injection — extract query from last user message
    inject = kwargs.get("context", True)
    strategy = kwargs.get("context_strategy", "hybrid") if inject else "none"
    ctx_memories: list[dict[str, Any]] = []

    if strategy != "none":
        last_user_msg = ""
        for m in reversed(messages):
            if m.get("role") == "user":
                last_user_msg = m.get("content", "")
                break
        if last_user_msg:
            _, ctx_memories = _inject_context(
                last_user_msg, strategy=strategy,
                max_memories=int(kwargs.get("max_context", 5)),
            )

    # Prepend context as system message if we found relevant memories
    enriched_messages = list(messages)
    if ctx_memories:
        ctx_lines = []
        for mem in ctx_memories:
            title = mem.get("title") or "untitled"
            content = str(mem.get("content", ""))[:400]
            ctx_lines.append(f"- {title}: {content}")
        ctx_block = (
            "You have access to a persistent memory system. "
            "Here are relevant memories:\n"
            + "\n".join(ctx_lines)
        )
        # Insert as first system message or prepend
        if enriched_messages and enriched_messages[0].get("role") == "system":
            enriched_messages[0] = {
                "role": "system",
                "content": enriched_messages[0]["content"] + "\n\n" + ctx_block,
            }
        else:
            enriched_messages.insert(0, {"role": "system", "content": ctx_block})

    try:
        result = _run(_chat(model, enriched_messages))
        result["context_injected"] = len(ctx_memories)

        # Memory-Augmented Generation
        if kwargs.get("store", False):
            last_user = next(
                (m["content"] for m in reversed(messages) if m.get("role") == "user"),
                "chat",
            )
            stored_id = _maybe_store_output(last_user, result.get("response", ""), model)
            if stored_id:
                result["stored_memory_id"] = stored_id

        return {"status": "success", **result}
    except ImportError as exc:
        return {"status": "error", "error": str(exc), "error_code": "missing_dependency"}
    except Exception as exc:
        return {"status": "error", "error": str(exc)}
