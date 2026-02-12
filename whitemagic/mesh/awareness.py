"""
Cross-Node Mesh Awareness — Redis ↔ Python Peer Tracking
==========================================================
Listens on the Redis ``ganying`` channel for mesh peer events from the
Go libp2p node.  Maintains a live peer registry and exposes it via
MCP tools ``mesh.status`` and ``mesh.broadcast``.

The Go mesh node publishes events like::

    {"type": "PEER_DISCOVERED", "node_id": "...", "address": "..."}
    {"type": "HOLOGRAM_RECEIVED", "id": "...", "coordinates": [...]}

This module bridges those into the Python Gan Ying bus and the
MeshClient's peer registry.
"""

from __future__ import annotations

import json
import logging
import threading
import time
from typing import Any, Dict, List, Optional

logger = logging.getLogger(__name__)


class MeshAwareness:
    """
    Watches Redis for mesh events and maintains peer awareness.
    Integrates with the existing MeshClient and Gan Ying bus.
    """

    def __init__(self) -> None:
        self._peers: Dict[str, Dict[str, Any]] = {}
        self._lock = threading.Lock()
        self._mesh_events: List[Dict[str, Any]] = []
        self._max_events = 200
        self._listening = False

    def register_peer(self, node_id: str, address: str = "", meta: Optional[Dict] = None) -> None:
        with self._lock:
            self._peers[node_id] = {
                "node_id": node_id,
                "address": address,
                "last_seen": time.time(),
                "meta": meta or {},
            }

    def remove_peer(self, node_id: str) -> None:
        with self._lock:
            self._peers.pop(node_id, None)

    def get_peers(self) -> List[Dict[str, Any]]:
        with self._lock:
            return list(self._peers.values())

    def record_event(self, event: Dict[str, Any]) -> None:
        with self._lock:
            self._mesh_events.append({**event, "_received_at": time.time()})
            if len(self._mesh_events) > self._max_events:
                self._mesh_events = self._mesh_events[-self._max_events:]

    def process_redis_message(self, data: str) -> None:
        """Process a message from the Redis ganying channel for mesh events."""
        try:
            msg = json.loads(data) if isinstance(data, str) else data
        except (json.JSONDecodeError, TypeError):
            return

        msg_type = msg.get("type", msg.get("signal_type", ""))

        if msg_type in ("PEER_DISCOVERED", "peer_discovered"):
            self.register_peer(
                node_id=msg.get("node_id", msg.get("source_id", "unknown")),
                address=msg.get("address", ""),
                meta=msg.get("meta", {}),
            )
            self.record_event(msg)

        elif msg_type in ("PEER_LEFT", "peer_left"):
            self.remove_peer(msg.get("node_id", ""))
            self.record_event(msg)

        elif msg_type in ("HOLOGRAM_RECEIVED", "hologram_broadcast"):
            self.record_event(msg)
            # Forward to mesh client
            try:
                from whitemagic.mesh.client import get_mesh_client
                get_mesh_client()
                coords = msg.get("coordinates", [])
                if coords and msg.get("source_node"):
                    self.register_peer(msg["source_node"], meta={"last_hologram": time.time()})
            except Exception:
                pass

    def status(self) -> Dict[str, Any]:
        """Full mesh awareness status."""
        try:
            from whitemagic.mesh.client import get_mesh_client
            client = get_mesh_client()
            client_status = client.status()
        except Exception:
            client_status = {"connected": False}

        with self._lock:
            return {
                "status": "success",
                "client": client_status,
                "peers": list(self._peers.values()),
                "peer_count": len(self._peers),
                "recent_events": len(self._mesh_events),
                "listening": self._listening,
            }


# Singleton
_awareness: Optional[MeshAwareness] = None
_awareness_lock = threading.Lock()


def get_mesh_awareness() -> MeshAwareness:
    global _awareness
    if _awareness is None:
        with _awareness_lock:
            if _awareness is None:
                _awareness = MeshAwareness()
    return _awareness
