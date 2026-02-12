from datetime import datetime
from typing import Any

from whitemagic.core.bridge.utils import logger


def emit_gratitude(target: str, intensity: float = 0.5, reason: str = "", **kwargs: Any) -> dict[str, Any]:
    """Emit a gratitude resonance signal.

    Args:
        target: The recipient (agent_id, node_id, or 'labs')
        intensity: Magnitude of gratitude (0.0 to 1.0)
        reason: Context for the gratitude

    Returns:
        Status of the emission

    """
    try:
        from whitemagic.core.resonance.gan_ying_enhanced import (
            EventType,
            ResonanceEvent,
            get_bus,
        )
        bus = get_bus()

        # 1. Local Resonance
        event = ResonanceEvent(
            source=kwargs.get("agent_id", "system"),
            event_type=EventType.GIFT_OFFERED, # Re-using GIFT_OFFERED for gratitude
            data={
                "target": target,
                "intensity": intensity,
                "reason": reason,
                "economic_signal": True,
            },
            timestamp=datetime.now(),
            confidence=intensity,
        )
        bus.emit(event)

        # 2. P2P Mesh Propagation
        try:
            from whitemagic.core.polyglot.mesh_bridge import get_mesh_bridge
            bridge = get_mesh_bridge()
            bridge.broadcast(f"GRATITUDE[{target}]: {intensity} - {reason[:30]}", confidence=intensity)
        except Exception:
            pass # Non-critical if mesh is down

        # 3. Memory Impact
        try:
            from whitemagic.gardens.sangha.collective_memory import get_collective
            coll = get_collective()
            coll.contribute_insight(  # type: ignore[call-arg]
                title=f"Gratitude Resonance to {target}",
                content=f"Gratitude intensity {intensity} for: {reason}",
                tags=["gratitude", "economy", "resonance"],
            )
        except Exception:
            pass

        return {"emitted": True, "target": target, "intensity": intensity}
    except Exception as e:
        logger.error(f"Failed to emit gratitude: {e}")
        return {"emitted": False, "error": str(e)}

def get_market_status(**kwargs: Any) -> dict[str, Any]:
    """Get status of the sovereign compute market."""
    try:
        from whitemagic.core.token_economy import get_token_economy
        economy = get_token_economy()
        status = economy.get_budget_status()

        # Add mock bittensor data for now
        status["bittensor"] = {
            "sn51_price": "0.0042 TAO/kTok",
            "available_nodes": 12,
        }
        return status
    except Exception as e:
        return {"error": str(e)}
