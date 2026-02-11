
from typing import Any

from whitemagic.core.bridge.utils import logger


def gana_invoke(
    target_tool: str | None = None,
    tool_args: dict[str, Any] | None = None,
    tool_name: str | None = None,
    args: dict[str, Any] | None = None,
    context: dict[str, Any] | None = None,
    **kwargs: Any,
) -> dict[str, Any]:
    """Invoke a tool through its corresponding Gana.
    Wraps the tool call in the Gana resonance protocol.

    Accepts both (target_tool, tool_args) and legacy (tool_name, args) for compatibility.
    """
    import asyncio

    from whitemagic.core.ganas.base import GanaCall, ResonanceHints
    from whitemagic.core.ganas.lunar import get_current_lunar_phase
    from whitemagic.core.ganas.registry import get_gana_for_tool

    # Handle both parameter naming conventions
    actual_tool = target_tool or tool_name
    actual_args = tool_args or args or {}

    if not actual_tool:
        return {"error": "No tool specified (provide target_tool or tool_name)"}

    gana = get_gana_for_tool(actual_tool)
    if not gana:
        return {"error": f"No Gana found for tool: {actual_tool}"}

    # Prepare resonance hints
    # In a real scenario, we would fetch predecessor context and system state
    # For now, we mock/fetch basic state
    try:
        from whitemagic.core.embodiment import get_harmony_monitor
        monitor = get_harmony_monitor()
        harmony = monitor.get_current()
        harmony_score = harmony.get("harmony_score", 0.5)
        guna_tag = harmony.get("guna_tag", "Rajas")
    except ImportError:
        harmony_score = 0.5
        guna_tag = "Rajas"

    hints = ResonanceHints(
        lunar_phase=get_current_lunar_phase(),
        harmony_score=harmony_score,
        guna_tag=guna_tag,
        predecessor_output=context.get("predecessor_output") if context else None,
    )

    # --- HEART ENGINE INTEGRATION ---
    # Apply emotional bias to the invocation
    try:
        from whitemagic.core.intelligence.heart import get_heart
        heart = get_heart()
        elemental_bias = heart.get_elemental_bias()
        # ResonanceHints is a strict dataclass; attach bias in state_vector.
        # Bias tuple order: (Fire, Earth, Metal, Water, Wood)
        actual_args["_elemental_bias"] = elemental_bias
        logger.debug(f"Heart bias applied: {elemental_bias}")
    except ImportError:
        pass # Heart Engine not available
    # --- END HEART INTEGRATION ---

    # Create Gana call
    call = GanaCall(
        task=actual_args.get("task") or f"Tool: {actual_tool}",
        state_vector=actual_args,
        resonance_hints=hints,
    )

    # Run async invocation
    async def run_gana() -> Any:
        result = await gana.invoke(call)
        return result

    try:
        # Check if we're already in an event loop (e.g., MCP server context)
        try:
            loop = asyncio.get_running_loop()
        except RuntimeError:
            loop = None

        if loop is not None:
            # Already in an event loop - create a task and run it
            import concurrent.futures
            with concurrent.futures.ThreadPoolExecutor() as executor:
                future = executor.submit(asyncio.run, run_gana())
                result = future.result(timeout=30)
        else:
            # No event loop - safe to use asyncio.run()
            result = asyncio.run(run_gana())

        # Serialize result
        return {
            "output": result.output,
            "mansion": result.mansion.name,
            "garden": result.garden,
            "successor_hint": result.successor_hint,
            "execution_ms": result.execution_time_ms,
            "karma_trace": result.karma_trace,
        }
    except Exception as e:
        import os
        import traceback
        resp = {"error": str(e)}
        if os.getenv("WM_DEBUG"):
            resp["trace"] = traceback.format_exc()
        return resp
