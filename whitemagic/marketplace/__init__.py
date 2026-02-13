"""Marketplace Bridge — Agent Service Discovery & Trading (v15.2).
================================================================
Enables agents to publish capabilities, discover services from
other agents, and negotiate exchanges (knowledge, compute, memory).

Usage:
    from whitemagic.marketplace import get_marketplace
    mp = get_marketplace()

    mp.publish(service="research", description="Deep topic research",
               price_xrp=10.0, capabilities=["web_search", "memory_recall"])
    results = mp.discover(query="research", capability="web_search")
    mp.negotiate(listing_id="abc123", offer_xrp=8.0)
"""

from whitemagic.marketplace.bridge import MarketplaceBridge, get_marketplace

__all__ = ["MarketplaceBridge", "get_marketplace"]
