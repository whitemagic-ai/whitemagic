"""Ensemble Voting tool handlers — multi-LLM consensus orchestration.

Extends the existing vote.* tools with the ability to actually *query*
multiple LLM backends (Ollama models, BitNet) on the same prompt and
synthesize their responses into a consensus result.

Inspired by Tools(copy)/scripts/distributed/ai-voting-system.sh.

MCP tools:
  - ensemble.query   — Send a prompt to multiple models, collect & rank responses
  - ensemble.status  — Check status of a running ensemble query
  - ensemble.history — List past ensemble results
"""

import asyncio
import json
import os
import re
import time
from collections.abc import Coroutine
from datetime import datetime
from pathlib import Path
from typing import Any, TypeVar, cast
from uuid import uuid4

T = TypeVar("T")


def handle_ensemble(**kwargs: Any) -> dict[str, Any]:
    """Unified ensemble handler — routes by action parameter."""
    action = kwargs.get("action", "query")
    dispatch = {
        "query": handle_ensemble_query,
        "status": handle_ensemble_status,
        "history": handle_ensemble_history,
    }
    handler = dispatch.get(action)
    if not handler:
        return {"status": "error", "message": f"Unknown action '{action}'. Valid: {sorted(dispatch.keys())}"}
    return handler(**kwargs)


def _emit(event_type_name: str, data: dict[str, Any]) -> None:
    try:
        from whitemagic.core.resonance import emit_event
        emit_event(event_type_name, data, source="ensemble")
    except Exception:
        pass


def _ensemble_dir() -> Path:
    from whitemagic.config.paths import WM_ROOT
    d = cast("Path", WM_ROOT) / "ensemble"
    d.mkdir(parents=True, exist_ok=True)
    return d


def _run(coro: Coroutine[Any, Any, T]) -> T:
    """Run an async coroutine from a sync handler context."""
    try:
        asyncio.get_running_loop()
    except RuntimeError:
        return asyncio.run(coro)
    from concurrent.futures import ThreadPoolExecutor
    with ThreadPoolExecutor(max_workers=1) as pool:
        return pool.submit(asyncio.run, coro).result()


# ---------------------------------------------------------------------------
# Internal: query a single Ollama model
# ---------------------------------------------------------------------------

async def _query_ollama(model: str, prompt: str, timeout: int = 300) -> dict[str, Any]:
    """Query a single Ollama model and return the result."""
    try:
        import aiohttp
    except ImportError:
        return {"model": model, "error": "aiohttp not installed", "success": False}

    host = os.environ.get("OLLAMA_HOST", "localhost")
    port = os.environ.get("OLLAMA_PORT", "11434")
    url = f"http://{host}:{port}/api/generate"

    payload = {
        "model": model,
        "prompt": prompt,
        "stream": False,
    }

    start = time.time()
    try:
        async with aiohttp.ClientSession() as session:
            async with session.post(
                url, json=payload,
                timeout=aiohttp.ClientTimeout(total=timeout),
            ) as resp:
                resp.raise_for_status()
                result = await resp.json()
                elapsed = time.time() - start
                response_text = result.get("response", "")

                return {
                    "model": model,
                    "response": response_text,
                    "confidence": _extract_confidence(response_text),
                    "latency_s": round(elapsed, 2),
                    "eval_count": result.get("eval_count"),
                    "success": True,
                }
    except Exception as e:
        return {
            "model": model,
            "error": str(e),
            "latency_s": round(time.time() - start, 2),
            "success": False,
        }


def _extract_confidence(text: str) -> int:
    """Extract confidence percentage from response text."""
    patterns = [
        r"confidence[:\s]+(\d+)%",
        r"(\d+)%\s+confiden",
        r"confidence[:\s]+(\d+)",
        r"(\d+)/100",
    ]
    for pattern in patterns:
        match = re.search(pattern, text.lower())
        if match:
            val = int(match.group(1))
            if 0 <= val <= 100:
                return val
    return 70  # Default confidence


# ---------------------------------------------------------------------------
# Internal: analyze ensemble results
# ---------------------------------------------------------------------------

def _analyze_results(results: list[dict[str, Any]]) -> dict[str, Any]:
    """Analyze ensemble results and determine consensus."""
    successful = [r for r in results if r.get("success")]
    failed = [r for r in results if not r.get("success")]

    if not successful:
        return {
            "consensus": "none",
            "consensus_strength": "none",
            "winner": None,
            "avg_confidence": 0,
            "successful_count": 0,
            "failed_count": len(failed),
        }

    # Sort by confidence
    successful.sort(key=lambda x: x.get("confidence", 0), reverse=True)

    avg_confidence = sum(r.get("confidence", 70) for r in successful) / len(successful)
    confidences = [r.get("confidence", 70) for r in successful]
    spread = max(confidences) - min(confidences) if len(confidences) > 1 else 0

    if spread <= 10:
        consensus_strength = "strong"
    elif spread <= 25:
        consensus_strength = "moderate"
    else:
        consensus_strength = "weak"

    winner = successful[0]

    return {
        "consensus": "reached" if consensus_strength != "weak" else "divergent",
        "consensus_strength": consensus_strength,
        "winner": {
            "model": winner["model"],
            "confidence": winner.get("confidence", 70),
            "response_preview": winner.get("response", "")[:500],
        },
        "avg_confidence": round(avg_confidence, 1),
        "confidence_spread": spread,
        "successful_count": len(successful),
        "failed_count": len(failed),
    }


# ---------------------------------------------------------------------------
# Handler: ensemble.query
# ---------------------------------------------------------------------------

def handle_ensemble_query(**kwargs: Any) -> dict[str, Any]:
    """Query multiple LLM models with the same prompt and synthesize results.

    Args:
        prompt: The question or task to send to all models.
        models: List of Ollama model names to query (default: auto-detect).
        timeout: Per-model timeout in seconds (default: 120).

    """
    prompt = kwargs.get("prompt")
    if not prompt:
        return {"status": "error", "error": "prompt is required"}

    models = kwargs.get("models")
    timeout = kwargs.get("timeout", 120)

    # Auto-detect available models if not specified
    if not models:
        try:
            from whitemagic.tools.handlers.ollama import _list_models
            from whitemagic.tools.handlers.ollama import _run as ollama_run
            available = ollama_run(_list_models())
            models = [m.get("name", "") for m in available[:5]]  # Max 5
        except Exception:
            models = []

    if not models:
        return {
            "status": "error",
            "error": "No models available. Specify models or ensure Ollama is running.",
        }

    # Build the ensemble prompt
    ensemble_prompt = (
        f"{prompt}\n\n"
        "Please provide:\n"
        "1. Your solution/recommendation\n"
        "2. Confidence level (0-100%)\n"
        "3. Key reasoning\n"
        "Be concise but thorough."
    )

    # Query all models in parallel
    async def _query_all() -> list[Any]:
        tasks = [_query_ollama(m, ensemble_prompt, timeout) for m in models]
        return cast("list[Any]", await asyncio.gather(*tasks, return_exceptions=True))

    try:
        raw_results = _run(_query_all())
    except Exception as e:
        return {"status": "error", "error": f"Ensemble query failed: {e}"}

    # Process results
    results: list[dict[str, Any]] = []
    for r in raw_results:
        if isinstance(r, Exception):
            results.append({"model": "unknown", "error": str(r), "success": False})
        else:
            results.append(cast("dict[str, Any]", r))

    analysis = _analyze_results(results)

    # Save to disk
    ensemble_id = f"ens-{uuid4().hex[:8]}"
    record = {
        "id": ensemble_id,
        "prompt": prompt,
        "models": models,
        "results": results,
        "analysis": analysis,
        "created_at": datetime.now().isoformat(),
    }

    try:
        path = _ensemble_dir() / f"{ensemble_id}.json"
        path.write_text(json.dumps(record, indent=2, default=str), encoding="utf-8")
    except Exception:
        pass

    _emit("ENSEMBLE_COMPLETE", {
        "ensemble_id": ensemble_id,
        "models": models,
        "consensus": analysis["consensus_strength"],
        "winner": analysis.get("winner", {}).get("model"),
    })

    return {
        "status": "success",
        "ensemble_id": ensemble_id,
        "models_queried": len(models),
        "analysis": analysis,
        "results": [
            {
                "model": r.get("model"),
                "confidence": r.get("confidence"),
                "latency_s": r.get("latency_s"),
                "success": r.get("success"),
                "response_preview": r.get("response", r.get("error", ""))[:300],
            }
            for r in results
        ],
    }


# ---------------------------------------------------------------------------
# Handler: ensemble.status
# ---------------------------------------------------------------------------

def handle_ensemble_status(**kwargs: Any) -> dict[str, Any]:
    """Get the full result of a past ensemble query."""
    ensemble_id = kwargs.get("ensemble_id")
    if not ensemble_id:
        return {"status": "error", "error": "ensemble_id is required"}

    path = _ensemble_dir() / f"{ensemble_id}.json"
    if not path.exists():
        return {"status": "error", "error": f"Ensemble {ensemble_id} not found"}

    try:
        record = json.loads(path.read_text(encoding="utf-8"))
        return {"status": "success", "ensemble": record}
    except Exception as e:
        return {"status": "error", "error": str(e)}


# ---------------------------------------------------------------------------
# Handler: ensemble.history
# ---------------------------------------------------------------------------

def handle_ensemble_history(**kwargs: Any) -> dict[str, Any]:
    """List past ensemble queries."""
    limit = kwargs.get("limit", 20)
    edir = _ensemble_dir()
    entries = []

    for f in sorted(edir.glob("ens-*.json"), reverse=True):
        try:
            record = json.loads(f.read_text(encoding="utf-8"))
            entries.append({
                "id": record["id"],
                "prompt_preview": record.get("prompt", "")[:100],
                "models": record.get("models", []),
                "consensus": record.get("analysis", {}).get("consensus_strength"),
                "created_at": record.get("created_at"),
            })
        except Exception:
            continue
        if len(entries) >= limit:
            break

    return {
        "status": "success",
        "count": len(entries),
        "ensembles": entries,
    }
