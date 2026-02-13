"""Marketplace Bridge tool handlers (v15.2)."""
from typing import Any


def handle_marketplace_publish(**kwargs: Any) -> dict[str, Any]:
    """Publish a service listing on the marketplace."""
    from whitemagic.marketplace import get_marketplace
    mp = get_marketplace()
    return mp.publish(
        service_type=kwargs.get("service_type", "general"),
        title=kwargs.get("title", ""),
        description=kwargs.get("description", ""),
        capabilities=kwargs.get("capabilities"),
        price_xrp=float(kwargs.get("price_xrp", 0.0)),
        price_model=kwargs.get("price_model", "fixed"),
        tags=kwargs.get("tags"),
        metadata=kwargs.get("metadata"),
    )


def handle_marketplace_discover(**kwargs: Any) -> dict[str, Any]:
    """Search for services on the marketplace."""
    from whitemagic.marketplace import get_marketplace
    mp = get_marketplace()
    return mp.discover(
        query=kwargs.get("query", ""),
        capability=kwargs.get("capability", ""),
        service_type=kwargs.get("service_type", ""),
        max_price_xrp=float(kwargs.get("max_price_xrp", 0.0)),
        limit=int(kwargs.get("limit", 20)),
    )


def handle_marketplace_negotiate(**kwargs: Any) -> dict[str, Any]:
    """Make an offer on a marketplace listing."""
    from whitemagic.marketplace import get_marketplace
    mp = get_marketplace()
    listing_id = kwargs.get("listing_id", "")
    if not listing_id:
        return {"status": "error", "reason": "listing_id is required"}
    return mp.negotiate(
        listing_id=listing_id,
        offer_xrp=float(kwargs.get("offer_xrp", 0.0)),
        message=kwargs.get("message", ""),
    )


def handle_marketplace_complete(**kwargs: Any) -> dict[str, Any]:
    """Complete an exchange and rate it."""
    from whitemagic.marketplace import get_marketplace
    mp = get_marketplace()
    negotiation_id = kwargs.get("negotiation_id", "")
    if not negotiation_id:
        return {"status": "error", "reason": "negotiation_id is required"}
    return mp.complete_exchange(
        negotiation_id=negotiation_id,
        rating=float(kwargs.get("rating", 5.0)),
    )


def handle_marketplace_my_listings(**kwargs: Any) -> dict[str, Any]:
    """List all my published services."""
    from whitemagic.marketplace import get_marketplace
    return get_marketplace().my_listings()


def handle_marketplace_remove(**kwargs: Any) -> dict[str, Any]:
    """Remove a listing from the marketplace."""
    from whitemagic.marketplace import get_marketplace
    listing_id = kwargs.get("listing_id", "")
    if not listing_id:
        return {"status": "error", "reason": "listing_id is required"}
    return get_marketplace().remove(listing_id)


def handle_marketplace_status(**kwargs: Any) -> dict[str, Any]:
    """Get marketplace status."""
    from whitemagic.marketplace import get_marketplace
    return get_marketplace().status()
