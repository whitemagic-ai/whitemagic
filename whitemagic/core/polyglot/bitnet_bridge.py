import json
import logging
import time
import uuid
from collections.abc import Callable

# Try to import redis
try:
    import redis
except ImportError:
    redis = None  # type: ignore[assignment]

logger = logging.getLogger(__name__)


class BitNetBridge:
    """Bridge to the BitNet 1-bit LLM.

    Communicates via Redis Pub/Sub on 'ganying' channel.
    Event Type: INFERENCE_REQUEST -> INFERENCE_RESULT
    """

    def __init__(self, redis_url: str = "redis://localhost:6379/0") -> None:
        self.redis_url = redis_url
        self.redis_client = None
        self.pubsub = None
        self._pending_requests: dict[str, Callable] = {}
        self._connect_redis()

    def _connect_redis(self) -> None:
        if not redis:
            logger.warning("Redis python client not installed. BitNet bridge disabled.")
            return

        try:
            self.redis_client = redis.from_url(self.redis_url)
            self._start_listener()
            logger.info("Connected to Redis for BitNet Bridge")
        except Exception as e:
            logger.warning(f"Failed to connect to Redis: {e}")
            self.redis_client = None

    def _start_listener(self) -> None:
        assert self.redis_client is not None
        self.pubsub = self.redis_client.pubsub()
        self.pubsub.subscribe("ganying")

        # We need a background thread for listening if not using async
        import threading

        self.thread = threading.Thread(target=self._listen, daemon=True)
        self.thread.start()

    def _listen(self) -> None:
        assert self.pubsub is not None
        for message in self.pubsub.listen():
            if message["type"] == "message":
                try:
                    data = json.loads(message["data"])
                    if data.get("event_type") == "INFERENCE_RESULT":
                        req_id = data.get("request_id")
                        if req_id in self._pending_requests:
                            callback = self._pending_requests.pop(req_id)
                            callback(data.get("data", {}).get("text", ""))
                except json.JSONDecodeError as e:
                    logger.error(f"Failed to decode message from Redis: {e}")
                except Exception as e:
                    logger.error(f"Error in BitNet listener: {e}")

    def generate(
        self, prompt: str, callback: Callable[[str], None] | None = None,
    ) -> str:
        """Send a generation request to BitNet.

        If callback is provided, it will be called with the result (async).
        If no callback, this returns a generic verification message (sync).
        """
        if not self.redis_client:
            return "Error: Redis not connected"

        request_id = str(uuid.uuid4())

        payload = {
            "event_type": "INFERENCE_REQUEST",
            "target": "bitnet",
            "request_id": request_id,
            "data": {"prompt": prompt},
            "timestamp": time.time(),
        }

        if callback:
            self._pending_requests[request_id] = callback

        self.redis_client.publish("ganying", json.dumps(payload))
        logger.info(f"Sent generation request {request_id}")
        return request_id


# Singleton
_bridge: BitNetBridge | None = None


def get_bitnet_bridge() -> BitNetBridge:
    global _bridge
    if _bridge is None:
        _bridge = BitNetBridge()
    return _bridge
