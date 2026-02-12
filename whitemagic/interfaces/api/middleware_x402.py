"""x402 Micropayment Middleware — HTTP 402 Payment Required for Machine Agents.
============================================================================
Implements the x402 protocol (Coinbase + Cloudflare) for opt-in per-request
micropayments on premium API endpoints.

Protocol flow:
  1. Client sends request to a premium endpoint
  2. If no payment header: server responds 402 + payment instructions
  3. Client pays in USDC on Base L2
  4. Client retries with payment proof header
  5. Server verifies on-chain, serves response

This is a STUB implementation. Real x402 integration requires:
  - x402-js or equivalent payment verification library
  - Base L2 RPC endpoint for on-chain verification
  - Wallet configuration for receiving payments

Default is ALWAYS FREE. This middleware only activates on explicitly
opted-in routes when WHITEMAGIC_X402_ENABLED=1.

Reference: https://www.x402.org/
"""

import logging
import os
from collections.abc import Callable
from typing import Any

logger = logging.getLogger(__name__)

# x402 is disabled by default — purely opt-in
X402_ENABLED = os.getenv("WHITEMAGIC_X402_ENABLED", "").lower() in ("1", "true", "yes")

# Default price per request in USDC (micro-amount)
DEFAULT_PRICE_USDC = 0.001

# Receiving wallet address (Base L2 / USDC)
RECEIVING_ADDRESS = os.getenv("WHITEMAGIC_X402_ADDRESS", "")

# Premium routes that support x402 (empty by default — all free)
_PREMIUM_ROUTES: dict[str, float] = {}


def register_premium_route(path: str, price_usdc: float = DEFAULT_PRICE_USDC) -> None:
    """Register a route as premium (opt-in x402 micropayment)."""
    _PREMIUM_ROUTES[path] = price_usdc
    logger.info(f"x402: registered premium route {path} at ${price_usdc} USDC")


def get_payment_instructions(path: str) -> dict[str, Any]:
    """Generate 402 Payment Required response body."""
    price = _PREMIUM_ROUTES.get(path, DEFAULT_PRICE_USDC)
    return {
        "status": 402,
        "message": "Payment Required",
        "protocol": "x402",
        "version": "1.0",
        "payment": {
            "currency": "USDC",
            "amount": str(price),
            "chain": "base",
            "recipient": RECEIVING_ADDRESS or "not_configured",
            "memo": f"whitemagic:{path}",
        },
        "instructions": (
            "To access this premium endpoint, send a USDC payment on Base L2 "
            "to the recipient address, then retry with header "
            "'X-Payment-Proof: <tx_hash>'"
        ),
        "free_alternative": (
            "This endpoint is available for free without payment. "
            "x402 micropayments are purely optional and support development."
        ),
    }


def verify_payment_proof(tx_hash: str, expected_amount: float = 0.0) -> dict[str, Any]:
    """Verify an x402 payment proof (on-chain transaction).

    STUB: Returns unverified. Real implementation would query Base L2 RPC.
    """
    logger.info(f"x402: verification requested for tx {tx_hash}")

    # Record in gratitude ledger
    try:
        from whitemagic.gratitude.ledger import GratitudeEvent, get_gratitude_ledger
        event = GratitudeEvent(
            channel="x402",
            amount=expected_amount or DEFAULT_PRICE_USDC,
            currency="USDC",
            tx_hash=tx_hash,
            verified=False,  # Stub — would be True after real verification
        )
        get_gratitude_ledger().record(event)
    except Exception as exc:
        logger.warning(f"x402: failed to record gratitude event: {exc}")

    return {
        "verified": False,
        "reason": "x402 on-chain verification not yet connected — stub implementation",
        "tx_hash": tx_hash,
    }


def x402_middleware_factory() -> Callable | None:
    """Create FastAPI middleware for x402 micropayments.

    Returns None if x402 is not enabled or FastAPI is not available.
    Only activates on routes registered via register_premium_route().
    """
    if not X402_ENABLED:
        return None

    try:
        from starlette.middleware.base import BaseHTTPMiddleware
        from starlette.requests import Request
        from starlette.responses import JSONResponse
    except ImportError:
        logger.info("x402: starlette not available, middleware disabled")
        return None

    class X402Middleware(BaseHTTPMiddleware):
        async def dispatch(self, request: Request, call_next: Callable) -> Any:
            path = request.url.path

            # Only check premium routes
            if path not in _PREMIUM_ROUTES:
                return await call_next(request)

            # Check for payment proof header
            payment_proof = request.headers.get("X-Payment-Proof", "")

            if not payment_proof:
                # Return 402 with payment instructions
                instructions = get_payment_instructions(path)
                return JSONResponse(
                    status_code=402,
                    content=instructions,
                )

            # Verify payment
            result = verify_payment_proof(
                payment_proof,
                expected_amount=_PREMIUM_ROUTES[path],
            )

            if not result.get("verified"):
                # For now, allow through even unverified (graceful degradation)
                # Real implementation would block unverified payments
                logger.info(f"x402: unverified payment for {path}, allowing through (stub mode)")

            return await call_next(request)

    return X402Middleware
