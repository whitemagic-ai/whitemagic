"""ILP Streaming Payments — Interledger Protocol Integration (v15.2).
===================================================================
Enables micro-payments for agent services via the Interledger Protocol.

Supports:
  - SPSP (Simple Payment Setup Protocol) wallet lookup
  - STREAM payment initiation and tracking
  - Payment channel management
  - Receipts and verification

Usage:
    from whitemagic.payments import get_ilp_manager
    mgr = get_ilp_manager()

    mgr.configure(pointer="$wallet.example.com/whitemagic")
    receipt = mgr.send(destination="$agent.example.com/research",
                       amount=100, asset_code="XRP", asset_scale=6)
    status = mgr.receipt(receipt["payment_id"])
"""

from whitemagic.payments.ilp_manager import ILPManager, get_ilp_manager

__all__ = ["ILPManager", "get_ilp_manager"]
