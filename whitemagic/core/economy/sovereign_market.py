import logging
from typing import Any

from whitemagic.core.polyglot.bittensor_bridge import get_bittensor_bridge
from whitemagic.core.token_economy import get_token_economy

logger = logging.getLogger(__name__)

class SovereignMarket:
    """Sovereign Market Logic Layer.
    Orchestrates the acquisition of decentralized compute resources.

    Philosophy: "Economic reasoning as a Gana capability."
    """

    def __init__(self) -> None:
        self.bridge = get_bittensor_bridge()
        self.economy = get_token_economy()

    async def evaluate_compute_need(self, task_urgency: float, estimated_tokens: int) -> dict[str, Any]:
        """Decide whether to bid for extra-local compute based on urgency and budget.

        Args:
            task_urgency: 0.0 (lazy) to 1.0 (critical)
            estimated_tokens: Estimated cost of the operation

        Returns:
            A market recommendation/bid.

        """
        status = self.economy.get_budget_status()

        # 1. Check local pressure
        pressure = status["usage_percent"] / 100.0

        # 2. Check Bittensor prices
        market_data = await self.bridge.discover_resources()
        if market_data.get("status") != "healthy":
            return {"decision": "defer", "reason": "market_offline"}

        # 3. Decision Logic (Dharmic Bidding)
        # We bid if (urgency * pressure) > 0.5
        threshold = task_urgency * pressure

        if threshold > 0.6:
            quote = self.bridge.get_resource_quote("llm_inference")
            if not isinstance(quote, dict):
                return {"decision": "defer", "reason": "quote_unavailable"}
            return {
                "decision": "bid",
                "target_type": quote.get("type"),
                "bid_price": quote.get("price"),
                "urgency": task_urgency,
                "reason": "local_saturation_meets_high_urgency",
            }
        elif threshold > 0.3:
            return {"decision": "throttle", "reason": "moderate_pressure"}
        else:
            return {"decision": "process_locally", "reason": "low_pressure"}

_market: SovereignMarket | None = None

def get_market() -> SovereignMarket:
    global _market
    if _market is None:
        _market = SovereignMarket()
    return _market

if __name__ == "__main__":
    import asyncio
    market = get_market()
    # High urgency on a saturated system
    decision = asyncio.run(market.evaluate_compute_need(task_urgency=0.9, estimated_tokens=5000))
    print(f"Market Decision: {decision['decision']} - {decision.get('reason')}")
