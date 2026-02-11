"""Nexus Dashboard API Server.
============================
Lightweight Starlette server providing real-time data from WhiteMagic
subsystems to the Nexus React dashboard.

Endpoints:
  GET  /api/gnosis          — Full Gnosis introspection snapshot
  GET  /api/gnosis/compact  — Compact Gnosis snapshot
  GET  /api/harmony         — HarmonyVector 7-dimension pulse
  GET  /api/galactic        — Galactic zone counts + recent promotions
  GET  /api/dream           — Dream cycle status + recent phases
  GET  /api/health          — Consolidated system health report
  GET  /api/metrics         — Dashboard-ready merged metrics blob
  WS   /ws/ganying          — Real-time Gan Ying event stream

Usage:
  python -m whitemagic.interfaces.nexus_api [--port 8765]
"""

import asyncio
import json
import logging
from collections.abc import Callable
from datetime import datetime
from typing import Any

from starlette.applications import Starlette
from starlette.middleware import Middleware
from starlette.middleware.cors import CORSMiddleware
from starlette.requests import Request
from starlette.responses import JSONResponse
from starlette.routing import Route, WebSocketRoute
from starlette.websockets import WebSocket, WebSocketDisconnect

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _json_safe(obj: Any) -> Any:
    """Recursively make an object JSON-serializable."""
    if isinstance(obj, dict):
        return {k: _json_safe(v) for k, v in obj.items()}
    if isinstance(obj, (list, tuple)):
        return [_json_safe(v) for v in obj]
    if isinstance(obj, set):
        return sorted(_json_safe(v) for v in obj)
    if isinstance(obj, datetime):
        return obj.isoformat()
    if isinstance(obj, bytes):
        return obj.decode("utf-8", errors="replace")
    if hasattr(obj, "to_dict"):
        return _json_safe(obj.to_dict())
    if hasattr(obj, "__dataclass_fields__"):
        from dataclasses import asdict
        return _json_safe(asdict(obj))
    return obj


def _safe_call(fn: Callable[..., Any], *args: Any, **kwargs: Any) -> Any:
    """Call fn, return result or error dict."""
    try:
        return fn(*args, **kwargs)
    except Exception as e:
        return {"error": str(e)}


# ---------------------------------------------------------------------------
# Route handlers
# ---------------------------------------------------------------------------

async def gnosis_full(request: Request) -> JSONResponse:
    """Full Gnosis introspection snapshot."""
    try:
        from whitemagic.tools.gnosis import gnosis_snapshot
        snap = gnosis_snapshot(compact=False)
        return JSONResponse(_json_safe(snap))
    except Exception as e:
        return JSONResponse({"error": str(e)}, status_code=500)


async def gnosis_compact(request: Request) -> JSONResponse:
    """Compact Gnosis snapshot."""
    try:
        from whitemagic.tools.gnosis import gnosis_snapshot
        snap = gnosis_snapshot(compact=True)
        return JSONResponse(_json_safe(snap))
    except Exception as e:
        return JSONResponse({"error": str(e)}, status_code=500)


async def harmony_endpoint(request: Request) -> JSONResponse:
    """HarmonyVector 7-dimension pulse."""
    try:
        from whitemagic.harmony.vector import get_harmony_vector
        hv = get_harmony_vector()
        snap = hv.snapshot()
        result: dict[str, Any] = snap.to_dict() if hasattr(snap, "to_dict") else _json_safe(snap)
        try:
            guna_distribution = getattr(hv, "guna_distribution", None)
            if callable(guna_distribution):
                result["guna"] = guna_distribution()
        except Exception:
            pass
        return JSONResponse(_json_safe(result))
    except Exception as e:
        return JSONResponse({"error": str(e)}, status_code=500)


async def galactic_endpoint(request: Request) -> JSONResponse:
    """Galactic zone counts + zone definitions."""
    zones: dict[str, int] = {"core": 0, "inner_rim": 0, "mid_band": 0, "outer_rim": 0, "far_edge": 0}
    total = 0
    try:
        import sqlite3
        from pathlib import Path

        from whitemagic.config.paths import DB_PATH
        db = Path(DB_PATH)
        if db.exists():
            conn = sqlite3.connect(str(db))
            try:
                total = conn.execute("SELECT COUNT(*) FROM memories").fetchone()[0]
                rows = conn.execute("""
                    SELECT
                        SUM(CASE WHEN galactic_distance < 0.05 THEN 1 ELSE 0 END) as core,
                        SUM(CASE WHEN galactic_distance >= 0.05 AND galactic_distance < 0.15 THEN 1 ELSE 0 END) as inner_rim,
                        SUM(CASE WHEN galactic_distance >= 0.15 AND galactic_distance < 0.40 THEN 1 ELSE 0 END) as mid_band,
                        SUM(CASE WHEN galactic_distance >= 0.40 AND galactic_distance < 0.70 THEN 1 ELSE 0 END) as outer_rim,
                        SUM(CASE WHEN galactic_distance >= 0.70 THEN 1 ELSE 0 END) as far_edge
                    FROM memories WHERE galactic_distance IS NOT NULL
                """).fetchone()
                if rows:
                    zones = {"core": rows[0] or 0, "inner_rim": rows[1] or 0, "mid_band": rows[2] or 0,
                             "outer_rim": rows[3] or 0, "far_edge": rows[4] or 0}
            finally:
                conn.close()
    except Exception as e:
        logger.debug(f"Galactic query: {e}")

    return JSONResponse({
        "zones": zones,
        "total_memories": total,
        "zones_definition": {
            "core": "0.00 – 0.05",
            "inner_rim": "0.05 – 0.15",
            "mid_band": "0.15 – 0.40",
            "outer_rim": "0.40 – 0.70",
            "far_edge": "0.70 – 1.00",
        },
        "timestamp": datetime.now().isoformat(),
    })


async def dream_endpoint(request: Request) -> JSONResponse:
    """Dream cycle status + recent phases."""
    try:
        from whitemagic.core.dreaming.dream_cycle import get_dream_cycle
        dc = get_dream_cycle()
        status = dc.status()
        return JSONResponse(_json_safe(status))
    except Exception as e:
        return JSONResponse({"error": str(e)}, status_code=500)


async def health_endpoint(request: Request) -> JSONResponse:
    """Consolidated system health report."""
    try:
        from whitemagic.tools.handlers.introspection import handle_health_report
        report = handle_health_report()
        return JSONResponse(_json_safe(report))
    except Exception as e:
        return JSONResponse({"error": str(e)}, status_code=500)


async def metrics_endpoint(request: Request) -> JSONResponse:
    """Dashboard-ready merged metrics blob for the Next.js frontend."""
    result: dict[str, Any] = {}

    # Harmony
    try:
        from whitemagic.harmony.vector import get_harmony_vector
        hv = get_harmony_vector()
        snap = hv.snapshot()
        result["harmony"] = {
            "balance": round(snap.balance, 3),
            "energy": round(snap.energy, 3),
            "error_rate": round(snap.error_rate, 3),
            "karma_debt": round(snap.karma_debt, 3),
        }
    except Exception:
        result["harmony"] = {}

    # Galactic zones
    try:
        from whitemagic.tools.gnosis import gnosis_snapshot
        gnosis = gnosis_snapshot(compact=True)
        galactic = gnosis.get("galactic", {}) if isinstance(gnosis, dict) else {}
        result["galactic"] = galactic
    except Exception:
        result["galactic"] = {}

    # Memory count
    try:
        import sqlite3
        from pathlib import Path

        from whitemagic.config.paths import DB_PATH
        db = Path(DB_PATH)
        if db.exists():
            conn = sqlite3.connect(str(db))
            count = conn.execute("SELECT COUNT(*) FROM memories").fetchone()[0]
            conn.close()
            result["memoryCount"] = count
        else:
            result["memoryCount"] = 0
    except Exception:
        result["memoryCount"] = 0

    # Dream status
    try:
        from whitemagic.core.dreaming.dream_cycle import get_dream_cycle
        dc = get_dream_cycle()
        result["dream"] = dc.status()
    except Exception:
        result["dream"] = {}

    # Rust status
    try:
        from whitemagic.utils.rust_helper import is_rust_available
        result["rust_available"] = is_rust_available()
    except Exception:
        result["rust_available"] = False

    # Health
    try:
        from whitemagic.tools.handlers.introspection import handle_health_report
        report = handle_health_report()
        result["health"] = {
            "version": report.get("version", "unknown"),
            "health_score": report.get("health_score", 0),
            "health_status": report.get("health_status", "unknown"),
            "tool_count": report.get("tool_count", 0),
            "rust": report.get("rust", {}),
            "haskell": report.get("haskell", {}),
        }
    except Exception:
        result["health"] = {}

    # Wu Xing phase (time-based)
    hour = datetime.now().hour
    if 3 <= hour < 7:
        phase = "WOOD"
    elif 7 <= hour < 11:
        phase = "FIRE"
    elif 11 <= hour < 15:
        phase = "EARTH"
    elif 15 <= hour < 19:
        phase = "METAL"
    else:
        phase = "WATER"
    result["currentPhase"] = phase

    # Gan Ying event count
    try:
        from whitemagic.core.resonance.gan_ying_enhanced import get_bus
        bus = get_bus()
        result["eventCount"] = getattr(bus, "_total_events", 0)
    except Exception:
        result["eventCount"] = 0

    result["timestamp"] = datetime.now().isoformat()
    return JSONResponse(_json_safe(result))


# ---------------------------------------------------------------------------
# WebSocket: Real-time Gan Ying event stream
# ---------------------------------------------------------------------------

_ws_clients: set[WebSocket] = set()


async def ganying_ws(websocket: WebSocket) -> None:
    """WebSocket endpoint for real-time Gan Ying events."""
    await websocket.accept()
    _ws_clients.add(websocket)
    logger.info(f"WebSocket client connected ({len(_ws_clients)} total)")

    try:
        # Send initial snapshot
        try:
            from whitemagic.core.resonance.gan_ying_enhanced import get_bus
            bus = get_bus()
            recent = getattr(bus, "_recent_events", [])
            for event in recent[-20:]:
                await websocket.send_json(_json_safe({
                    "type": "event",
                    "data": {
                        "event_type": event.event_type.value if hasattr(event.event_type, "value") else str(event.event_type),
                        "source": event.source,
                        "confidence": getattr(event, "confidence", 1.0),
                        "timestamp": event.timestamp.isoformat() if hasattr(event.timestamp, "isoformat") else str(event.timestamp),
                        "data": getattr(event, "data", {}),
                    },
                }))
        except Exception as e:
            await websocket.send_json({"type": "info", "message": f"Gan Ying bus: {e}"})

        # Keep connection alive, send heartbeats
        while True:
            try:
                # Wait for messages from client (ping/pong keepalive)
                data = await asyncio.wait_for(websocket.receive_text(), timeout=30.0)
                if data == "ping":
                    await websocket.send_json({"type": "pong", "timestamp": datetime.now().isoformat()})
            except asyncio.TimeoutError:
                # Send heartbeat
                await websocket.send_json({"type": "heartbeat", "timestamp": datetime.now().isoformat()})
    except WebSocketDisconnect:
        pass
    except Exception as e:
        logger.debug(f"WebSocket error: {e}")
    finally:
        _ws_clients.discard(websocket)
        logger.info(f"WebSocket client disconnected ({len(_ws_clients)} remaining)")


async def broadcast_ganying_event(event_data: dict[str, Any]) -> None:
    """Broadcast a Gan Ying event to all connected WebSocket clients."""
    if not _ws_clients:
        return
    message = json.dumps(_json_safe({"type": "event", "data": event_data}))
    disconnected = set()
    for ws in _ws_clients:
        try:
            await ws.send_text(message)
        except Exception:
            disconnected.add(ws)
    _ws_clients.difference_update(disconnected)


# ---------------------------------------------------------------------------
# Gan Ying subscription (hooks into the event bus)
# ---------------------------------------------------------------------------

_ganying_task: asyncio.Task[Any] | None = None


async def _ganying_poll_loop() -> None:
    """Poll the Gan Ying bus for new events and broadcast to WebSocket clients."""
    last_count = 0
    while True:
        try:
            from whitemagic.core.resonance.gan_ying_enhanced import get_bus
            bus = get_bus()
            recent = getattr(bus, "_recent_events", [])
            current_count = len(recent)

            if current_count > last_count:
                new_events = recent[last_count:]
                for event in new_events:
                    await broadcast_ganying_event({
                        "event_type": event.event_type.value if hasattr(event.event_type, "value") else str(event.event_type),
                        "source": event.source,
                        "confidence": getattr(event, "confidence", 1.0),
                        "timestamp": event.timestamp.isoformat() if hasattr(event.timestamp, "isoformat") else str(event.timestamp),
                        "data": getattr(event, "data", {}),
                    })
                last_count = current_count
        except Exception:
            pass
        await asyncio.sleep(0.5)


async def on_startup() -> None:
    """Start the Gan Ying polling loop."""
    global _ganying_task
    _ganying_task = asyncio.create_task(_ganying_poll_loop())
    logger.info("Nexus API: Gan Ying polling loop started")


async def on_shutdown() -> None:
    """Stop the Gan Ying polling loop."""
    global _ganying_task
    if _ganying_task:
        _ganying_task.cancel()
        try:
            await _ganying_task
        except asyncio.CancelledError:
            pass
    logger.info("Nexus API: Gan Ying polling loop stopped")


# ---------------------------------------------------------------------------
# App
# ---------------------------------------------------------------------------

routes = [
    Route("/api/gnosis", gnosis_full),
    Route("/api/gnosis/compact", gnosis_compact),
    Route("/api/harmony", harmony_endpoint),
    Route("/api/galactic", galactic_endpoint),
    Route("/api/dream", dream_endpoint),
    Route("/api/health", health_endpoint),
    Route("/api/metrics", metrics_endpoint),
    WebSocketRoute("/ws/ganying", ganying_ws),
]

middleware = [
    Middleware(
        CORSMiddleware,
        allow_origins=["http://localhost:3000", "http://127.0.0.1:3000",
                       "http://localhost:3001", "http://127.0.0.1:3001"],
        allow_methods=["GET", "POST", "OPTIONS"],
        allow_headers=["*"],
    ),
]

app = Starlette(
    routes=routes,
    middleware=middleware,
    on_startup=[on_startup],
    on_shutdown=[on_shutdown],
)


# ---------------------------------------------------------------------------
# CLI entrypoint
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    import argparse

    import uvicorn

    parser = argparse.ArgumentParser(description="Nexus Dashboard API Server")
    parser.add_argument("--port", type=int, default=8765, help="Port (default: 8765)")
    parser.add_argument("--host", type=str, default="127.0.0.1", help="Host (default: 127.0.0.1)")
    args = parser.parse_args()

    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(name)s %(levelname)s %(message)s")
    logger.info(f"Starting Nexus API at http://{args.host}:{args.port}")
    logger.info(f"  REST:      http://{args.host}:{args.port}/api/metrics")
    logger.info(f"  WebSocket: ws://{args.host}:{args.port}/ws/ganying")

    uvicorn.run(app, host=args.host, port=args.port, log_level="info")
