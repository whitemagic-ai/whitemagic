"""
BitNet Bridge — Opt-in local 1-bit LLM inference via Gan Ying bus.

Ported from wm_archive/BitNet/bridge.py. Communicates with a running
BitNet (llama-cli) process via Redis pub/sub on the Gan Ying channel.

Enable with: WHITEMAGIC_ENABLE_BITNET=1

Usage:
    from whitemagic.inference.bitnet_bridge import infer, is_available

    if is_available():
        result = infer("Summarize this code")
"""

import json
import logging
import os
import subprocess
import time
import uuid
from pathlib import Path
from typing import Any, Dict

logger = logging.getLogger(__name__)

ENABLED = os.environ.get("WHITEMAGIC_ENABLE_BITNET", "").strip() in ("1", "true", "yes")
REDIS_URL = os.environ.get("REDIS_URL", "redis://localhost:6379")
CHANNEL = os.environ.get("BITNET_CHANNEL", "ganying")
MODEL_PATH = os.environ.get("BITNET_MODEL_PATH", "models/bitnet_b1_58-3B/ggml-model-tl2.gguf")
LLAMA_CLI = os.environ.get("BITNET_LLAMA_CLI", "build/bin/llama-cli")
TIMEOUT = int(os.environ.get("BITNET_TIMEOUT", "60"))


def is_available() -> bool:
    """Check if BitNet inference is enabled and reachable."""
    if not ENABLED:
        return False
    # Check if we can reach Redis (for bus mode) or llama-cli (for direct mode)
    try:
        import redis
        r = redis.Redis.from_url(REDIS_URL, socket_timeout=1)
        r.ping()
        return True
    except Exception:
        pass
    # Fallback: check if llama-cli exists for direct mode
    return Path(LLAMA_CLI).exists()


def infer(prompt: str, n_predict: int = 128, temp: float = 0.8,
          mode: str = "auto") -> Dict[str, Any]:
    """
    Run BitNet inference.

    Args:
        prompt: The input prompt.
        n_predict: Max tokens to generate.
        temp: Temperature for sampling.
        mode: "redis" (via Gan Ying bus), "direct" (subprocess), or "auto".

    Returns:
        Dict with text, duration_seconds, model, and mode.
    """
    if not ENABLED:
        return {"status": "error", "message": "BitNet not enabled. Set WHITEMAGIC_ENABLE_BITNET=1"}

    if mode == "auto":
        mode = "redis" if _redis_available() else "direct"

    if mode == "redis":
        return _infer_redis(prompt, n_predict, temp)
    else:
        return _infer_direct(prompt, n_predict, temp)


def _redis_available() -> bool:
    try:
        import redis
        r = redis.Redis.from_url(REDIS_URL, socket_timeout=1)
        r.ping()
        return True
    except Exception:
        return False


def _infer_redis(prompt: str, n_predict: int, temp: float) -> Dict[str, Any]:
    """Send inference request via Redis pub/sub (Gan Ying bus)."""
    import redis

    r = redis.Redis.from_url(REDIS_URL, decode_responses=True)
    request_id = str(uuid.uuid4())[:8]

    # Subscribe to response channel
    pubsub = r.pubsub()
    pubsub.subscribe(CHANNEL)

    # Publish request
    r.publish(CHANNEL, json.dumps({
        "event_type": "INFERENCE_REQUEST",
        "target": "bitnet",
        "request_id": request_id,
        "data": {"prompt": prompt, "n_predict": n_predict, "temp": temp},
        "source": "whitemagic-bridge",
        "timestamp": time.time(),
    }))

    # Wait for response
    deadline = time.time() + TIMEOUT
    while time.time() < deadline:
        msg = pubsub.get_message(timeout=1)
        if msg and msg["type"] == "message":
            try:
                data = json.loads(msg["data"])
                if (data.get("event_type") == "INFERENCE_RESULT" and
                        data.get("request_id") == request_id):
                    pubsub.unsubscribe()
                    result = data.get("data", {})
                    return {
                        "status": "success",
                        "text": result.get("text", ""),
                        "duration_seconds": result.get("duration_seconds", 0),
                        "model": result.get("model", "bitnet_b1_58-3B"),
                        "mode": "redis",
                    }
            except json.JSONDecodeError:
                continue

    pubsub.unsubscribe()
    return {"status": "error", "message": f"Timeout after {TIMEOUT}s waiting for BitNet response"}


def _infer_direct(prompt: str, n_predict: int, temp: float) -> Dict[str, Any]:
    """Run inference directly via llama-cli subprocess."""
    executable = LLAMA_CLI
    if not Path(executable).exists():
        return {"status": "error", "message": f"llama-cli not found at {executable}"}

    command = [
        executable,
        "-m", MODEL_PATH,
        "-n", str(n_predict),
        "-p", prompt,
        "-ngl", "0",
        "-c", "2048",
        "--temp", str(temp),
        "-b", "1",
        "--log-disable",
    ]

    start = time.time()
    try:
        result = subprocess.run(command, capture_output=True, text=True, check=True, timeout=TIMEOUT)
        duration = time.time() - start
        return {
            "status": "success",
            "text": result.stdout,
            "duration_seconds": round(duration, 2),
            "model": "bitnet_b1_58-3B",
            "mode": "direct",
        }
    except subprocess.TimeoutExpired:
        return {"status": "error", "message": f"Inference timed out after {TIMEOUT}s"}
    except subprocess.CalledProcessError as e:
        return {"status": "error", "message": f"Inference failed: {e.stderr[:200]}"}
    except FileNotFoundError:
        return {"status": "error", "message": f"llama-cli not found at {executable}"}


# --- Gan Ying event emission ---

def _emit_inference_event(result: Dict[str, Any], prompt: str) -> Any:
    """Emit inference result to Gan Ying bus."""
    try:
        from whitemagic.core.resonance.gan_ying_enhanced import get_bus, ResonanceEvent, EventType
        bus = get_bus()
        inference_event = getattr(EventType, "INFERENCE_COMPLETE", EventType.PATTERN_DETECTED)
        bus.emit(ResonanceEvent(
            event_type=inference_event,
            source="bitnet_bridge",
            data={
                "model": result.get("model"),
                "mode": result.get("mode"),
                "duration": result.get("duration_seconds"),
                "prompt_length": len(prompt),
                "status": result.get("status"),
            },
        ))
    except Exception:
        pass
