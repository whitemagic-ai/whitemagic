"""
Mesh Client — Python bridge to the WhiteMagic gRPC Mesh
=========================================================
Connects to the mesh service defined in mesh/proto/mesh.proto.
Uses grpcio if available, otherwise provides a mock client for
development.

The mesh enables distributed multi-node WhiteMagic coordination:
  - BroadcastSignal: Send events to all peers
  - BroadcastHologram: Share holographic coordinates
  - DiscoverPeers: Find other WhiteMagic nodes

Configuration:
  - WM_MESH_ADDRESS: gRPC address (default: localhost:50051)
  - WM_MESH_NODE_ID: This node's identifier

Usage:
    from whitemagic.mesh.client import get_mesh_client
    client = get_mesh_client()
    client.broadcast_signal("MEMORY_CREATED", payload="{...}")
    peers = client.discover_peers()
"""

from __future__ import annotations

import importlib
import logging
import os
import threading
import time
from dataclasses import dataclass
from typing import Any, cast

logger = logging.getLogger(__name__)

# Try to import grpcio
HAS_GRPC = False
try:
    import grpc  # type: ignore[import-untyped]
    HAS_GRPC = True
except ImportError:
    pass


@dataclass
class MeshPeer:
    """A discovered mesh peer."""
    node_id: str
    address: str
    last_seen: float = 0.0

    def to_dict(self) -> dict[str, Any]:
        return {
            "node_id": self.node_id,
            "address": self.address,
            "last_seen": self.last_seen,
        }


@dataclass
class SignalResult:
    """Result of a signal broadcast."""
    success: bool
    message: str = ""


class MeshClient:
    """
    gRPC client for the WhiteMagic mesh network.

    Falls back to local-only mode when grpcio is not available
    or mesh server is unreachable.
    """

    def __init__(
        self,
        address: str | None = None,
        node_id: str | None = None,
    ) -> None:
        self._address = address or os.environ.get("WM_MESH_ADDRESS", "localhost:50051")
        self._node_id = node_id or os.environ.get("WM_MESH_NODE_ID", f"node_{os.getpid()}")
        self._lock = threading.Lock()
        self._connected = False
        self._channel: Any | None = None
        self._stub: Any | None = None

        # Local signal log (always available)
        self._signal_log: list[dict[str, Any]] = []
        self._max_log = 500
        self._known_peers: dict[str, MeshPeer] = {}

        if HAS_GRPC:
            self._try_connect()

    def _try_connect(self) -> None:
        """Attempt gRPC connection."""
        if not HAS_GRPC:
            return
        try:
            self._channel = grpc.insecure_channel(self._address)
            # Try to import generated stubs
            try:
                mesh_pb2_grpc = cast(Any, importlib.import_module("whitemagic.mesh.mesh_pb2_grpc"))
                self._stub = mesh_pb2_grpc.MeshServiceStub(self._channel)
                self._connected = True
                logger.info("Mesh connected to %s as %s", self._address, self._node_id)
            except ImportError:
                logger.debug("Mesh proto stubs not generated — running in local mode")
                self._connected = False
        except Exception as e:
            logger.debug("Mesh connection failed: %s", e)
            self._connected = False

    def broadcast_signal(
        self,
        signal_type: str,
        payload: str = "",
    ) -> SignalResult:
        """Broadcast a signal to all mesh peers."""
        record = {
            "source_id": self._node_id,
            "signal_type": signal_type,
            "payload": payload[:10000],
            "timestamp": time.time(),
        }

        with self._lock:
            self._signal_log.append(record)
            if len(self._signal_log) > self._max_log:
                self._signal_log = self._signal_log[-self._max_log:]

        if self._connected and self._stub:
            try:
                mesh_pb2 = cast(Any, importlib.import_module("whitemagic.mesh.mesh_pb2"))
                request = mesh_pb2.SignalRequest(
                    source_id=self._node_id,
                    signal_type=signal_type,
                    payload=payload,
                    timestamp=int(time.time()),
                )
                response = self._stub.BroadcastSignal(request, timeout=5)
                return SignalResult(success=response.success, message=response.message)
            except Exception as e:
                return SignalResult(success=False, message=f"gRPC error: {e}")

        # Local-only mode
        # Emit to Gan Ying bus instead
        try:
            from whitemagic.core.resonance.gan_ying_enhanced import (
                get_bus, ResonanceEvent, EventType,
            )
            bus = get_bus()
            bus.emit(ResonanceEvent(
                event_type=EventType.SYSTEM_STATE_CHANGE,
                source=f"mesh:{self._node_id}",
                data={"signal_type": signal_type, "payload": payload[:500]},
            ))
        except Exception:
            pass

        return SignalResult(success=True, message="local-only broadcast (mesh not connected)")

    def broadcast_hologram(
        self,
        memory_id: str,
        coordinates: list[float],
        content_hash: str = "",
        importance: float = 0.5,
    ) -> SignalResult:
        """Broadcast holographic coordinates to the mesh."""
        if self._connected and self._stub:
            try:
                mesh_pb2 = cast(Any, importlib.import_module("whitemagic.mesh.mesh_pb2"))
                request = mesh_pb2.HolographicSignal(
                    id=memory_id,
                    coordinates=coordinates,
                    content_hash=content_hash,
                    importance=importance,
                    timestamp=int(time.time()),
                    source_node=self._node_id,
                )
                response = self._stub.BroadcastHologram(request, timeout=5)
                return SignalResult(success=response.success, message=response.message)
            except Exception as e:
                return SignalResult(success=False, message=f"gRPC error: {e}")

        return SignalResult(success=True, message="local-only (mesh not connected)")

    def discover_peers(self) -> list[MeshPeer]:
        """Discover mesh peers."""
        if self._connected and self._stub:
            try:
                mesh_pb2 = cast(Any, importlib.import_module("whitemagic.mesh.mesh_pb2"))
                request = mesh_pb2.DiscoveryRequest(
                    node_id=self._node_id,
                    address=self._address,
                )
                response = self._stub.DiscoverPeers(request, timeout=5)
                peers = []
                for p in response.peers:
                    peer = MeshPeer(node_id=p.node_id, address=p.address, last_seen=time.time())
                    self._known_peers[p.node_id] = peer
                    peers.append(peer)
                return peers
            except Exception as e:
                logger.debug("Peer discovery failed: %s", e)

        return list(self._known_peers.values())

    def recent_signals(self, limit: int = 20) -> list[dict[str, Any]]:
        """Get recent signal log."""
        with self._lock:
            return list(reversed(self._signal_log[-limit:]))

    def status(self) -> dict[str, Any]:
        """Get mesh client status."""
        return {
            "node_id": self._node_id,
            "address": self._address,
            "connected": self._connected,
            "has_grpc": HAS_GRPC,
            "known_peers": len(self._known_peers),
            "signal_log_size": len(self._signal_log),
        }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_client: MeshClient | None = None
_client_lock = threading.Lock()


def get_mesh_client() -> MeshClient:
    global _client
    if _client is None:
        with _client_lock:
            if _client is None:
                _client = MeshClient()
    return _client
