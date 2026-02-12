"""Ollama tool handlers — optional bridge to local LLM inference via Ollama.

Provides generate, chat, and model listing through the Whitemagic tool
contract.  Requires ``aiohttp`` (``whitemagic[net]``) and a running
Ollama server.
"""
import asyncio
import os
import socket
import time
from collections.abc import Coroutine
from typing import Any, TypeVar, cast

T = TypeVar("T")


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


async def _generate(model: str, prompt: str, stream: bool = False) -> dict[str, Any]:
    aiohttp = _require_aiohttp()
    url = f"{_ollama_url()}/api/generate"
    payload = {"model": model, "prompt": prompt, "stream": False}
    start = time.time()
    async with aiohttp.ClientSession() as session:
        timeout = aiohttp.ClientTimeout(total=_request_timeout(60.0))
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
        timeout = aiohttp.ClientTimeout(total=_request_timeout(60.0))
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
    """Generate text using a local Ollama model."""
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

    try:
        result = _run(_generate(model, prompt))
        return {"status": "success", **result}
    except ImportError as exc:
        return {"status": "error", "error": str(exc), "error_code": "missing_dependency"}
    except Exception as exc:
        return {"status": "error", "error": str(exc)}


def handle_ollama_chat(**kwargs: Any) -> dict[str, Any]:
    """Chat with a local Ollama model (multi-turn)."""
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

    try:
        result = _run(_chat(model, messages))
        return {"status": "success", **result}
    except ImportError as exc:
        return {"status": "error", "error": str(exc), "error_code": "missing_dependency"}
    except Exception as exc:
        return {"status": "error", "error": str(exc)}
