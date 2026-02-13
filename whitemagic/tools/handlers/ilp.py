"""ILP (Interledger Protocol) streaming payment tool handlers (v15.2)."""
from typing import Any


def handle_ilp_configure(**kwargs: Any) -> dict[str, Any]:
    """Configure ILP payment system."""
    from whitemagic.payments import get_ilp_manager
    mgr = get_ilp_manager()
    return mgr.configure(
        pointer=kwargs.get("pointer", ""),
        connector_url=kwargs.get("connector_url", ""),
        auth_token=kwargs.get("auth_token", ""),
    )


def handle_ilp_send(**kwargs: Any) -> dict[str, Any]:
    """Send a payment or micro-payment stream."""
    from whitemagic.payments import get_ilp_manager
    mgr = get_ilp_manager()
    destination = kwargs.get("destination", "")
    if not destination:
        return {"status": "error", "reason": "destination is required"}
    return mgr.send(
        destination=destination,
        amount=int(kwargs.get("amount", 0)),
        asset_code=kwargs.get("asset_code", "XRP"),
        asset_scale=int(kwargs.get("asset_scale", 6)),
        memo=kwargs.get("memo", ""),
        service_type=kwargs.get("service_type", ""),
    )


def handle_ilp_receipt(**kwargs: Any) -> dict[str, Any]:
    """Get receipt/status of a payment."""
    from whitemagic.payments import get_ilp_manager
    mgr = get_ilp_manager()
    payment_id = kwargs.get("payment_id", "")
    if not payment_id:
        return {"status": "error", "reason": "payment_id is required"}
    return mgr.receipt(payment_id)


def handle_ilp_history(**kwargs: Any) -> dict[str, Any]:
    """Get recent payment history."""
    from whitemagic.payments import get_ilp_manager
    return get_ilp_manager().history(limit=int(kwargs.get("limit", 20)))


def handle_ilp_balance(**kwargs: Any) -> dict[str, Any]:
    """Get aggregate payment statistics."""
    from whitemagic.payments import get_ilp_manager
    return get_ilp_manager().balance()


def handle_ilp_status(**kwargs: Any) -> dict[str, Any]:
    """Get ILP system status."""
    from whitemagic.payments import get_ilp_manager
    return get_ilp_manager().status()
