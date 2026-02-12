import json
import logging
import os
import subprocess
import threading
import time
from collections.abc import Callable
from pathlib import Path
from typing import Any

# Try to import redis, but don't crash if missing (polyglot is optional)
redis: Any | None
try:
    import redis  # type: ignore[no-redef]
except ImportError:
    redis = None

logger = logging.getLogger(__name__)


class MeshBridge:
    """Bridge to the Go/LibP2P Mesh Network.

    Communicates with the 'whitemagic-mesh' sidecar via Redis Pub/Sub.
    Channel: 'ganying' (Inductive Resonance)
    """

    def __init__(
        self,
        redis_url: str = "redis://localhost:6379/0",
        inbound_channel: str = "ganying",
        outbound_channel: str = "ganying_out",
    ) -> None:
        self.redis_url = redis_url
        self.inbound_channel = inbound_channel
        self.outbound_channel = outbound_channel
        self.redis_client: Any | None = None
        self.pubsub: Any | None = None
        self.mesh_process: subprocess.Popen | None = None
        self._handlers: dict[str, Callable] = {}
        self.running = False
        self._connect_redis()

    def _connect_redis(self) -> Any:
        redis_mod = redis
        if redis_mod is None:
            logger.warning("Redis python client not installed. Mesh bridge disabled.")
            return

        try:
            self.redis_client = redis_mod.from_url(self.redis_url)
            self.redis_client.ping()
            logger.info("Connected to Redis for Mesh Bridge")
        except Exception as e:
            logger.warning(f"Failed to connect to Redis: {e}")
            self.redis_client = None

    async def start_sidecar(self, binary_path: str | None = None) -> bool:
        """Start the Go Mesh sidecar binary."""
        import asyncio
        if not binary_path:
            # Look in standard locations
            # Root is core_system
            root = Path(__file__).parent.parent.parent.parent
            possible_paths = [
                root / "mesh" / "whitemagic-mesh",
                root / "whitemagic-go" / "whitemagic-mesh",  # Keep legacy as backup
            ]
            for p in possible_paths:
                if p.exists():
                    binary_path = str(p)
                    break

        if not binary_path or not os.path.exists(binary_path):
            logger.error(f"Mesh binary not found. Searched: {binary_path}")
            return False

        try:
            logger.info(f"Starting Mesh Sidecar: {binary_path}")
            self.mesh_process = subprocess.Popen(
                [binary_path],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                env={**os.environ},
            )
            # Give it a moment to boot
            await asyncio.sleep(1)
            if self.mesh_process.poll() is not None:
                _, stderr = self.mesh_process.communicate()
                logger.error(f"Mesh sidecar failed to start: {stderr}")
                return False

            return True
        except Exception as e:
            logger.error(f"Failed to start sidecar: {e}")
            return False

    def broadcast(self, message: str, confidence: float = 1.0) -> bool:
        """Broadcast a message to the Swarm."""
        if not self.redis_client:
            logger.warning("Redis not available; cannot broadcast mesh message.")
            return False

        # Include DGA Seed for evolutionary discovery
        try:
            from whitemagic.core.intelligence.hologram.dga_engine import get_dga_engine
            dga_seed = get_dga_engine().generate_signature()
        except Exception:
            dga_seed = "unknown"

        payload = {
            "event_type": "CORE_BROADCAST",
            "data": {
                "message": message,
                "confidence": confidence,
                "timestamp": time.time(),
                "dga_seed": dga_seed,
            },
        }
        try:
            self.redis_client.publish(self.outbound_channel, json.dumps(payload))
            return True
        except Exception as e:
            logger.warning(f"Failed to publish mesh message: {e}")
            return False

    def listen(self) -> Any:
        """Listen for messages from the Mesh."""
        if not self.redis_client:
            return

        self.running = True
        self.pubsub = self.redis_client.pubsub()
        self.pubsub.subscribe(self.inbound_channel)
        pubsub = self.pubsub
        if pubsub is None:
            return

        def _listener() -> Any:
            logger.info("Listening for Mesh signals...")
            for message in pubsub.listen():
                if not self.running:
                    break

                if message["type"] == "message":
                    try:
                        data = json.loads(message["data"])
                        self._handle_signal(data)
                    except Exception as e:
                        logger.error(f"Error processing mesh signal: {e}")

        self.thread = threading.Thread(target=_listener, daemon=True)
        self.thread.start()

    def _handle_signal(self, data: dict[str, Any]) -> Any:
        """Dispatch signal to registered handlers and Gan Ying Bus."""
        event_type = data.get("event_type")
        if event_type == "MESH_SIGNAL":
            payload = data.get("data", {})
            logger.info(
                f"Received Holographic Signal from {payload.get('peer_id')[:8]}",
            )

            # Record DGA Lineage if seed is present
            dga_seed = payload.get("dga_seed")
            if dga_seed:
                try:
                    from whitemagic.core.intelligence.hologram.dga_engine import (
                        get_dga_engine,
                    )
                    from whitemagic.gardens.sangha.collective_memory import (
                        get_collective,
                    )
                    # Calculate distance and record as a potential "cousin" or "ancestor"
                    engine = get_dga_engine()
                    local_sig = engine.generate_signature()
                    dist = engine.calculate_distance(local_sig, dga_seed)

                    if dist < 0.3: # Close relative
                        coll = get_collective()
                        coll.record_lineage(dga_seed, local_sig)
                        logger.info(f"🧬 Detected close P2P relative ({dist:.4f}). Lineage tracked.")
                except Exception as e:
                    logger.debug(f"Failed to process P2P DGA lineage: {e}")

            # Dispatch to Gan Ying Bus
            try:
                from datetime import datetime

                from whitemagic.core.resonance.gan_ying_enhanced import (
                    EventType,
                    ResonanceEvent,
                    get_bus,
                )
                bus = get_bus()
                bus.emit(ResonanceEvent(
                    source=f"mesh_{payload.get('peer_id')[:8]}",
                    event_type=EventType.MESH_SIGNAL,
                    data=payload,
                    timestamp=datetime.now(),
                    confidence=payload.get("importance", 0.9),
                ))
            except ImportError:
                logger.warning("GanYingBus not available for mesh signal dispatch.")
            except Exception as e:
                logger.error(f"Failed to emit mesh signal to GanYingBus: {e}")

    def stop(self) -> Any:
        self.running = False
        if self.pubsub:
            self.pubsub.unsubscribe()
        if self.mesh_process:
            self.mesh_process.terminate()
            self.mesh_process = None


# Singleton
_bridge: MeshBridge | None = None


def get_mesh_bridge() -> MeshBridge:
    global _bridge
    if _bridge is None:
        _bridge = MeshBridge()
    return _bridge
