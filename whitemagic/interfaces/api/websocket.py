"""WebSocket Live Feed — Gan Ying → Browser Push for Nexus.

Bridges the internal Gan Ying resonance bus to WebSocket clients,
enabling real-time streaming of system events to the Nexus frontend.

Usage:
    # In app.py:
    from .websocket import register_websocket_routes
    register_websocket_routes(app)

    # Client connects to ws://host:port/ws/ganying
"""

import asyncio
import json
import logging
from datetime import datetime
from typing import Any

logger = logging.getLogger(__name__)


class GanYingWebSocketManager:
    """Manages WebSocket connections and bridges Gan Ying events to clients."""

    def __init__(self) -> None:
        self.connections: set[Any] = set()
        self._polling_task: asyncio.Task[None] | None = None
        self._running = False
        self._poll_interval = 1.0  # seconds

    async def connect(self, websocket: Any) -> None:
        """Accept and register a WebSocket connection."""
        await websocket.accept()
        self.connections.add(websocket)
        logger.info(f"WebSocket connected. Total: {len(self.connections)}")

        # Start polling if first connection
        if not self._running:
            self._running = True
            self._polling_task = asyncio.create_task(self._poll_gan_ying())

    def disconnect(self, websocket: Any) -> None:
        """Remove a WebSocket connection."""
        self.connections.discard(websocket)
        logger.info(f"WebSocket disconnected. Total: {len(self.connections)}")

        # Stop polling if no connections
        if not self.connections and self._running:
            self._running = False

    async def broadcast(self, message: dict[str, Any]) -> None:
        """Broadcast a JSON message to all connected clients."""
        if not self.connections:
            return

        data = json.dumps(message, default=str)
        disconnected: set[Any] = set()

        for ws in self.connections:
            try:
                await ws.send_text(data)
            except Exception:
                disconnected.add(ws)

        for ws in disconnected:
            self.disconnect(ws)

    async def _poll_gan_ying(self) -> None:
        """Poll Gan Ying bus for recent events and broadcast them."""
        try:
            from whitemagic.core.resonance.gan_ying_enhanced import get_bus
            bus = get_bus()
        except ImportError:
            logger.info("Gan Ying bus not available — WebSocket feed in stub mode")
            return

        last_check = datetime.now()

        while self._running and self.connections:
            try:
                # Get recent events from bus history
                history = getattr(bus, "_history", [])
                new_events = [
                    e for e in history
                    if hasattr(e, "timestamp") and e.timestamp > last_check
                ]

                for event in new_events[-20:]:  # Cap at 20 per poll
                    await self.broadcast({
                        "type": "ganying_event",
                        "event_type": str(getattr(event, "event_type", "unknown")),
                        "source": getattr(event, "source", ""),
                        "data": getattr(event, "data", {}),
                        "confidence": getattr(event, "confidence", 0),
                        "timestamp": getattr(event, "timestamp", datetime.now()).isoformat(),
                    })

                if new_events:
                    last_check = max(e.timestamp for e in new_events if hasattr(e, "timestamp"))

                # Also send periodic system pulse
                try:
                    from whitemagic.harmony.vector import get_harmony_vector
                    hv = get_harmony_vector()
                    snap = hv.snapshot()
                    await self.broadcast({
                        "type": "system_pulse",
                        "harmony": {
                            "balance": round(snap.balance, 3),
                            "throughput": round(snap.throughput, 3),
                            "energy": round(snap.energy, 3),
                            "dharma": round(snap.dharma, 3),
                        },
                        "timestamp": datetime.now().isoformat(),
                    })
                except Exception:
                    pass

            except Exception as e:
                logger.debug(f"WebSocket poll error: {e}")

            await asyncio.sleep(self._poll_interval)


manager = GanYingWebSocketManager()


def register_websocket_routes(app: Any) -> None:
    """Register WebSocket endpoints on the FastAPI app."""
    try:
        from fastapi import WebSocket
    except ImportError:
        return

    @app.websocket("/ws/ganying")
    async def ganying_feed(websocket: WebSocket) -> None:
        """Real-time Gan Ying event feed for Nexus frontend."""
        await manager.connect(websocket)
        try:
            while True:
                # Keep connection alive; handle client messages if needed
                data = await websocket.receive_text()
                if data == "ping":
                    await websocket.send_text(json.dumps({"type": "pong"}))
        except Exception:
            manager.disconnect(websocket)

    @app.websocket("/ws/health")
    async def health_feed(websocket: WebSocket) -> None:
        """Lightweight health pulse stream."""
        await websocket.accept()
        try:
            while True:
                await websocket.send_text(json.dumps({
                    "type": "health",
                    "status": "alive",
                    "connections": len(manager.connections),
                    "timestamp": datetime.now().isoformat(),
                }))
                await asyncio.sleep(5)
        except Exception:
            pass


async def emit_cycle_complete(cycle_data: dict[str, Any]) -> None:
    """Emit cycle complete event (backward compatibility)."""
    await manager.broadcast({"type": "cycle_complete", "data": cycle_data})
