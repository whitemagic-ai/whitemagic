"""MCP handlers for BitNet local inference."""

from typing import Any, cast


def handle_bitnet_infer(**kwargs: Any) -> dict[str, Any]:
    """Run local inference via BitNet 1-bit LLM."""
    from whitemagic.inference.bitnet_bridge import ENABLED, infer

    if not ENABLED:
        return {
            "status": "error",
            "message": "BitNet not enabled. Set WHITEMAGIC_ENABLE_BITNET=1",
        }

    prompt = kwargs.get("prompt", "")
    if not prompt:
        return {"status": "error", "message": "prompt is required"}

    n_predict = int(kwargs.get("n_predict", 128))
    temp = float(kwargs.get("temp", 0.8))
    mode = kwargs.get("mode", "auto")

    result = infer(prompt, n_predict=n_predict, temp=temp, mode=mode)

    # Emit Gan Ying event
    try:
        from whitemagic.inference.bitnet_bridge import _emit_inference_event
        _emit_inference_event(result, prompt)
    except Exception:
        pass

    return cast("dict[str, Any]", result)


def handle_bitnet_status(**kwargs: Any) -> dict[str, Any]:
    """Check BitNet availability and configuration."""
    from whitemagic.inference.bitnet_bridge import (
        ENABLED,
        LLAMA_CLI,
        MODEL_PATH,
        REDIS_URL,
        TIMEOUT,
        is_available,
    )

    return {
        "status": "success",
        "enabled": ENABLED,
        "available": is_available() if ENABLED else False,
        "config": {
            "redis_url": REDIS_URL,
            "model_path": MODEL_PATH,
            "llama_cli": LLAMA_CLI,
            "timeout": TIMEOUT,
        },
    }
