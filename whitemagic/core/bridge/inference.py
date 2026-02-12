
"""Local Model / Inference Bridge (Archived).
========================================

Whitemagic previously included *embedded* local model inference (Ollama/BitNet/etc).
The current direction is to keep Whitemagic model-agnostic and expose capabilities
via MCP/REST so *external* models (local or cloud) can use Whitemagic as a memory
and cognition substrate.

This module is kept for backwards compatibility but is **disabled by default**.
To experiment with the archived local-model stack, set:
  `WHITEMAGIC_ENABLE_LOCAL_MODELS=1`
"""

from __future__ import annotations

import os
from typing import Any


def _local_models_enabled() -> bool:
    return os.getenv("WHITEMAGIC_ENABLE_LOCAL_MODELS", "").strip().lower() in {
        "1",
        "true",
        "yes",
        "on",
    }


def _archived_error() -> dict[str, Any]:
    return {
        "error": "Local model inference is archived/disabled (WHITEMAGIC_ENABLE_LOCAL_MODELS not set).",
        "archived": True,
    }


def local_ml_status(**_: Any) -> dict[str, Any]:
    """Return status for the archived local-model stack (disabled by default)."""
    if not _local_models_enabled():
        return {
            "available": False,
            "backends": {},
            "default_backend": None,
            "models": {},
            "archived": True,
        }
    # Best-effort: fall through to the legacy runner.
    return run_local_inference(operation="status")


def local_ml_infer(
    prompt: str,
    backend: str | None = None,
    max_tokens: int | None = None,
    temperature: float | None = None,
    **kwargs: Any,
) -> dict[str, Any]:
    """Run archived local-model inference (disabled by default)."""
    if not _local_models_enabled():
        return _archived_error()
    return run_local_inference(
        operation="quick_recall",
        query=prompt,
        provider=backend,
        max_tokens=max_tokens,
        temperature=temperature,
        **kwargs,
    )


def bitnet_status(**_: Any) -> dict[str, Any]:
    """Archived BitNet status shim."""
    if not _local_models_enabled():
        return {"available": False, "models": [], "archived": True}
    return _archived_error()


def bitnet_infer(prompt: str, **_: Any) -> dict[str, Any]:
    """Archived BitNet inference shim."""
    if not _local_models_enabled():
        return _archived_error()
    return _archived_error()

def run_local_inference(
    operation: str | None = "quick_recall",
    query: str | None = None,
    prompt: str | None = None,
    prompts: list[str] | None = None,
    mode: str | None = None,
    provider: str | None = None,
    memory_limit: int = 5,
    analysis_type: str = "summary",
    timeout_seconds: int = 5,
    **kwargs: Any,
) -> dict[str, Any]:
    """Run local inference with memory grounding."""
    if not _local_models_enabled():
        return _archived_error()

    import asyncio

    from whitemagic.cascade.local_inference import (
        analyze_with_context,
        memory_grounded_generate,
        quick_recall,
    )

    op = (operation or "").lower()
    if mode and not op:
        op = "batch" if mode == "batch" else "quick_recall"
    if not query and prompt:
        query = prompt
    if not op:
        op = "quick_recall"
    if not query and op != "batch":
        return {"error": "query is required"}

    if op == "quick_recall":
        try:
            quick_coro = quick_recall(query, provider=provider)
            quick_result = asyncio.run(asyncio.wait_for(quick_coro, timeout=timeout_seconds))
            return {"answer": quick_result}
        except asyncio.TimeoutError:
            return {"error": "Local inference timed out", "timeout_seconds": timeout_seconds}
        except Exception as exc:
            return {"error": str(exc)}

    if op == "memory_grounded":
        try:
            memory_coro = memory_grounded_generate(query, provider=provider, memory_limit=memory_limit)
            memory_result = asyncio.run(asyncio.wait_for(memory_coro, timeout=timeout_seconds))

            if isinstance(memory_result, dict):
                return {
                    "answer": memory_result.get("answer", ""),
                    "memories_used": memory_result.get("memories_used", []),
                    "memory_count": memory_result.get("memory_count", 0),
                    "latency_ms": memory_result.get("latency_ms"),
                    "provider": memory_result.get("provider", provider),
                }

            return {
                "answer": getattr(memory_result, "answer", str(memory_result)),
                "memories_used": getattr(memory_result, "memories_used", []),
                "memory_count": getattr(memory_result, "memory_count", 0),
                "latency_ms": getattr(memory_result, "latency_ms", None),
                "provider": getattr(memory_result, "provider", provider),
            }
        except asyncio.TimeoutError:
            return {"error": "Local inference timed out", "timeout_seconds": timeout_seconds}
        except Exception as exc:
            return {"error": str(exc)}

    if op == "analyze":
        try:
            analyze_coro = analyze_with_context(
                query,
                analysis_type=analysis_type,
                memory_limit=memory_limit,
                provider=provider,
            )
            analyze_result = asyncio.run(asyncio.wait_for(analyze_coro, timeout=timeout_seconds))
            return analyze_result
        except asyncio.TimeoutError:
            return {"error": "Local inference timed out", "timeout_seconds": timeout_seconds}
        except Exception as exc:
            return {"error": str(exc)}

    if op == "batch":
        if not prompts:
            return {"error": "prompts is required for batch mode"}
        results: list[dict[str, Any]] = []
        for item in prompts:
            try:
                batch_coro = quick_recall(item, provider=provider)
                answer = asyncio.run(asyncio.wait_for(batch_coro, timeout=timeout_seconds))
                results.append({"prompt": item, "answer": answer})
            except asyncio.TimeoutError:
                results.append({"prompt": item, "error": "Local inference timed out", "timeout_seconds": timeout_seconds})
            except Exception as exc:
                results.append({"prompt": item, "error": str(exc)})
        return {"mode": "batch", "results": results}

    return {"error": f"Unknown local_inference operation: {op}"}
