"""CyberBrain module handlers — salience, bicameral reasoning, retention, drives, self-model, worker."""
from typing import Any

# ---------------------------------------------------------------------------
# Salience Arbiter
# ---------------------------------------------------------------------------

def handle_salience_spotlight(**kwargs: Any) -> dict[str, Any]:
    """Return the current attention spotlight — top-N most salient events."""
    from whitemagic.core.resonance.salience_arbiter import get_salience_arbiter
    arbiter = get_salience_arbiter()
    n = kwargs.get("limit", 5)
    entries = arbiter.get_spotlight(n=n)
    stats = arbiter.get_stats()
    return {
        "status": "success",
        "spotlight": [
            {
                "event_type": e.event.event_type.value,
                "source": e.event.source,
                "salience": {
                    "urgency": e.salience.urgency,
                    "novelty": e.salience.novelty,
                    "confidence": e.salience.confidence,
                    "composite": e.salience.composite,
                },
                "data_keys": list(e.event.data.keys()) if isinstance(e.event.data, dict) else [],
            }
            for e in entries
        ],
        "stats": stats,
    }


# ---------------------------------------------------------------------------
# Bicameral Reasoner
# ---------------------------------------------------------------------------

def handle_bicameral_reason(**kwargs: Any) -> dict[str, Any]:
    """Run dual-hemisphere reasoning (left=precise, right=creative) on a query."""
    query = kwargs.get("query")
    if not query:
        return {"status": "error", "error": "query is required"}
    context = kwargs.get("context", {})

    import asyncio

    from whitemagic.core.intelligence.bicameral import get_bicameral_reasoner
    reasoner = get_bicameral_reasoner()

    try:
        loop = asyncio.get_running_loop()
    except RuntimeError:
        loop = None

    if loop and loop.is_running():
        from concurrent.futures import ThreadPoolExecutor
        with ThreadPoolExecutor(max_workers=1) as pool:
            result = pool.submit(asyncio.run, reasoner.reason(query, context=context)).result()
    else:
        result = asyncio.run(reasoner.reason(query, context=context))

    return {
        "status": "success",
        "reasoning": result.to_dict(),
    }


# ---------------------------------------------------------------------------
# Mindful Forgetting / Retention Sweep
# ---------------------------------------------------------------------------

def handle_retention_sweep(**kwargs: Any) -> dict[str, Any]:
    """Run a mindful forgetting retention sweep — evaluate what to keep/forget."""
    import whitemagic.core.memory.mindful_forgetting as mindful_forgetting
    engine_factory = getattr(mindful_forgetting, "get_forgetting_engine", None)
    if engine_factory is None:
        engine_factory = getattr(mindful_forgetting, "get_retention_engine")
    engine = engine_factory()

    dry_run = kwargs.get("dry_run", True)
    limit = kwargs.get("limit", 50)

    results = engine.sweep(dry_run=dry_run, limit=limit)
    return {
        "status": "success",
        "dry_run": dry_run,
        "evaluated": results.get("evaluated", 0),
        "would_forget": results.get("would_forget", 0),
        "would_keep": results.get("would_keep", 0),
        "details": results.get("details", [])[:10],
    }


# ---------------------------------------------------------------------------
# Emotion & Drive Core
# ---------------------------------------------------------------------------

def handle_drive_snapshot(**kwargs: Any) -> dict[str, Any]:
    """Get current drive state — curiosity, satisfaction, caution, energy, social."""
    from whitemagic.core.intelligence.emotion_drive import get_drive_core
    core = get_drive_core()
    include_bias = kwargs.get("include_bias", False)
    snap = core.snapshot()
    result = {"status": "success", "drives": snap.to_dict()}
    if include_bias:
        result["motivation_bias"] = core.get_motivation_bias()
    return result


def handle_drive_event(**kwargs: Any) -> dict[str, Any]:
    """Feed an event into the Emotion & Drive Core to update drive levels."""
    from whitemagic.core.intelligence.emotion_drive import get_drive_core
    event_type = kwargs.get("event_type")
    if not event_type:
        return {"status": "error", "error": "event_type is required"}
    data = kwargs.get("data", {})
    core = get_drive_core()
    snap = core.on_event(event_type, data)
    return {"status": "success", "drives": snap.to_dict()}


# ---------------------------------------------------------------------------
# Self-Model / Predictive Introspection
# ---------------------------------------------------------------------------

def handle_selfmodel_forecast(**kwargs: Any) -> dict[str, Any]:
    """Forecast system metric trends and predict threshold crossings."""
    from whitemagic.core.intelligence.self_model import get_self_model
    model = get_self_model()
    metric = kwargs.get("metric")
    steps = kwargs.get("steps_ahead")

    if metric:
        f = model.forecast(metric, steps)
        if f is None:
            return {"status": "error", "error": f"No data for metric '{metric}'"}
        return {"status": "success", "forecast": f.to_dict()}

    all_f = model.forecast_all(steps)
    return {
        "status": "success",
        "forecasts": {k: v.to_dict() for k, v in all_f.items()},
        "count": len(all_f),
    }


def handle_selfmodel_alerts(**kwargs: Any) -> dict[str, Any]:
    """Get only forecasts that have active threshold alerts."""
    from whitemagic.core.intelligence.self_model import get_self_model
    model = get_self_model()
    alerts = model.get_alerts()
    return {
        "status": "success",
        "alert_count": len(alerts),
        "alerts": [a.to_dict() for a in alerts],
    }


# ---------------------------------------------------------------------------
# Worker Daemon Status
# ---------------------------------------------------------------------------

def handle_worker_status(**kwargs: Any) -> dict[str, Any]:
    """Check if a worker daemon is running and get its stats."""
    import json
    from pathlib import Path
    try:
        from whitemagic.config.paths import WM_ROOT
        agents_dir = WM_ROOT / "agents"
    except Exception:
        agents_dir = Path.home() / ".whitemagic" / "agents"

    workers = []
    if agents_dir.exists():
        for f in agents_dir.glob("worker-*.json"):
            try:
                agent = json.loads(f.read_text(encoding="utf-8"))
                if agent.get("metadata", {}).get("type") == "worker_daemon":
                    workers.append({
                        "id": agent.get("id"),
                        "name": agent.get("name"),
                        "host": agent.get("host"),
                        "last_heartbeat": agent.get("last_heartbeat"),
                        "heartbeat_count": agent.get("heartbeat_count", 0),
                        "status": agent.get("status"),
                    })
            except (json.JSONDecodeError, OSError):
                continue

    return {
        "status": "success",
        "worker_count": len(workers),
        "workers": workers,
    }
