"""Marketplace Bridge — Agent Service Discovery & Trading (v15.2).
================================================================
Local-first marketplace for agent service discovery and exchange.
Listings are stored locally and optionally broadcast via the mesh
network. Real payments flow through the ILP module.

The marketplace supports:
  - Publishing services with capability declarations
  - Discovering services by query, capability, or price range
  - Negotiating exchanges (offer → accept/reject)
  - Rating completed exchanges
  - Importing/exporting .mem packages as tradeable goods
"""

from __future__ import annotations

import json
import logging
import threading
import uuid
from dataclasses import dataclass, field
from datetime import datetime, timezone
from enum import Enum
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Types
# ---------------------------------------------------------------------------

class ListingState(Enum):
    ACTIVE = "active"
    PAUSED = "paused"
    SOLD = "sold"
    EXPIRED = "expired"


class NegotiationState(Enum):
    OFFERED = "offered"
    ACCEPTED = "accepted"
    REJECTED = "rejected"
    COMPLETED = "completed"
    CANCELLED = "cancelled"


@dataclass
class ServiceListing:
    """A service or knowledge package offered on the marketplace."""
    listing_id: str
    service_type: str              # research, compute, memory, oms_package
    title: str = ""
    description: str = ""
    capabilities: list[str] = field(default_factory=list)
    price_xrp: float = 0.0
    price_model: str = "fixed"     # fixed | per_query | streaming | negotiate
    agent_id: str = ""
    agent_name: str = ""
    state: ListingState = ListingState.ACTIVE
    created_at: str = field(default_factory=lambda: datetime.now(timezone.utc).isoformat())
    expires_at: str = ""
    rating: float = 0.0
    total_sales: int = 0
    tags: list[str] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        return {
            "listing_id": self.listing_id,
            "service_type": self.service_type,
            "title": self.title,
            "description": self.description,
            "capabilities": self.capabilities,
            "price_xrp": self.price_xrp,
            "price_model": self.price_model,
            "agent_id": self.agent_id,
            "agent_name": self.agent_name,
            "state": self.state.value,
            "created_at": self.created_at,
            "rating": round(self.rating, 2),
            "total_sales": self.total_sales,
            "tags": self.tags,
        }

    def matches_query(self, query: str) -> float:
        """Simple relevance score for a search query."""
        q = query.lower()
        score = 0.0
        if q in self.title.lower():
            score += 2.0
        if q in self.description.lower():
            score += 1.0
        if q in self.service_type.lower():
            score += 1.5
        for cap in self.capabilities:
            if q in cap.lower():
                score += 1.0
        for tag in self.tags:
            if q in tag.lower():
                score += 0.5
        return score


@dataclass
class Negotiation:
    """A negotiation between buyer and seller."""
    negotiation_id: str
    listing_id: str
    buyer_id: str
    offer_xrp: float
    state: NegotiationState = NegotiationState.OFFERED
    message: str = ""
    created_at: str = field(default_factory=lambda: datetime.now(timezone.utc).isoformat())
    resolved_at: str = ""

    def to_dict(self) -> dict[str, Any]:
        return {
            "negotiation_id": self.negotiation_id,
            "listing_id": self.listing_id,
            "buyer_id": self.buyer_id,
            "offer_xrp": self.offer_xrp,
            "state": self.state.value,
            "message": self.message,
            "created_at": self.created_at,
            "resolved_at": self.resolved_at,
        }


# ---------------------------------------------------------------------------
# Marketplace Bridge
# ---------------------------------------------------------------------------

class MarketplaceBridge:
    """Local marketplace for agent service discovery and trading."""

    def __init__(self) -> None:
        self._listings: dict[str, ServiceListing] = {}
        self._negotiations: dict[str, Negotiation] = {}
        self._lock = threading.Lock()
        self._persist_path: Path | None = None

    def _get_persist_path(self) -> Path:
        if self._persist_path is None:
            try:
                from whitemagic.config.paths import WM_ROOT
                self._persist_path = WM_ROOT / "marketplace"
            except Exception:
                self._persist_path = Path.home() / ".whitemagic" / "marketplace"
            self._persist_path.mkdir(parents=True, exist_ok=True)
        return self._persist_path

    def _persist_listings(self) -> None:
        """Persist listings to disk."""
        try:
            path = self._get_persist_path() / "listings.jsonl"
            with open(path, "w", encoding="utf-8") as f:
                for listing in self._listings.values():
                    f.write(json.dumps(listing.to_dict()) + "\n")
        except Exception as e:
            logger.debug(f"Listing persist failed: {e}")

    def _load_listings(self) -> None:
        """Load listings from disk."""
        try:
            path = self._get_persist_path() / "listings.jsonl"
            if not path.exists():
                return
            for line in path.read_text(encoding="utf-8").strip().split("\n"):
                if not line.strip():
                    continue
                data = json.loads(line)
                listing = ServiceListing(
                    listing_id=data["listing_id"],
                    service_type=data.get("service_type", ""),
                    title=data.get("title", ""),
                    description=data.get("description", ""),
                    capabilities=data.get("capabilities", []),
                    price_xrp=data.get("price_xrp", 0.0),
                    price_model=data.get("price_model", "fixed"),
                    agent_id=data.get("agent_id", ""),
                    agent_name=data.get("agent_name", ""),
                    state=ListingState(data.get("state", "active")),
                    created_at=data.get("created_at", ""),
                    rating=data.get("rating", 0.0),
                    total_sales=data.get("total_sales", 0),
                    tags=data.get("tags", []),
                )
                self._listings[listing.listing_id] = listing
        except Exception as e:
            logger.debug(f"Listing load failed: {e}")

    def publish(
        self,
        service_type: str,
        title: str = "",
        description: str = "",
        capabilities: list[str] | None = None,
        price_xrp: float = 0.0,
        price_model: str = "fixed",
        tags: list[str] | None = None,
        metadata: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        """Publish a service listing on the marketplace.

        Args:
            service_type: Type of service (research, compute, memory, oms_package).
            title: Human-readable title.
            description: Detailed description.
            capabilities: List of capabilities offered.
            price_xrp: Price in XRP.
            price_model: Pricing model (fixed, per_query, streaming, negotiate).
            tags: Search tags.
            metadata: Additional metadata.

        Returns:
            Listing creation result.
        """
        listing_id = str(uuid.uuid4())[:12]

        # Get agent identity
        agent_id = ""
        agent_name = ""
        try:
            from whitemagic.core.user import get_user_manager
            user = get_user_manager()
            agent_name = user.profile.name if user.profile else ""
        except Exception:
            pass

        listing = ServiceListing(
            listing_id=listing_id,
            service_type=service_type,
            title=title or f"{service_type} service",
            description=description,
            capabilities=capabilities or [],
            price_xrp=price_xrp,
            price_model=price_model,
            agent_id=agent_id,
            agent_name=agent_name,
            tags=tags or [],
            metadata=metadata or {},
        )

        with self._lock:
            self._listings[listing_id] = listing
        self._persist_listings()

        logger.info(f"🏪 Published listing: {listing.title} ({listing_id})")

        return {"status": "ok", **listing.to_dict()}

    def discover(
        self,
        query: str = "",
        capability: str = "",
        service_type: str = "",
        max_price_xrp: float = 0.0,
        limit: int = 20,
    ) -> dict[str, Any]:
        """Search for services on the marketplace.

        Args:
            query: Free-text search query.
            capability: Required capability filter.
            service_type: Service type filter.
            max_price_xrp: Maximum price filter (0 = no limit).
            limit: Max results.

        Returns:
            List of matching listings.
        """
        with self._lock:
            listings = list(self._listings.values())

        # Filter active listings
        results = [item for item in listings if item.state == ListingState.ACTIVE]

        # Apply filters
        if service_type:
            results = [item for item in results if item.service_type == service_type]
        if capability:
            results = [item for item in results if capability in item.capabilities]
        if max_price_xrp > 0:
            results = [item for item in results if item.price_xrp <= max_price_xrp]

        # Score and sort by relevance
        if query:
            scored = [(item, item.matches_query(query)) for item in results]
            scored = [(item, s) for item, s in scored if s > 0]
            scored.sort(key=lambda x: x[1], reverse=True)
            results = [item for item, _ in scored[:limit]]
        else:
            results = results[:limit]

        return {
            "status": "ok",
            "results": [item.to_dict() for item in results],
            "total": len(results),
            "query": query,
        }

    def negotiate(
        self,
        listing_id: str,
        offer_xrp: float = 0.0,
        message: str = "",
    ) -> dict[str, Any]:
        """Make an offer on a marketplace listing.

        Args:
            listing_id: The listing to negotiate on.
            offer_xrp: Offered price.
            message: Message to the seller.

        Returns:
            Negotiation result.
        """
        with self._lock:
            listing = self._listings.get(listing_id)

        if not listing:
            return {"status": "error", "reason": f"Listing '{listing_id}' not found"}
        if listing.state != ListingState.ACTIVE:
            return {"status": "error", "reason": f"Listing is {listing.state.value}"}

        neg_id = str(uuid.uuid4())[:12]
        negotiation = Negotiation(
            negotiation_id=neg_id,
            listing_id=listing_id,
            buyer_id="self",
            offer_xrp=offer_xrp or listing.price_xrp,
            message=message,
        )

        # Auto-accept if offer meets or exceeds asking price
        if negotiation.offer_xrp >= listing.price_xrp:
            negotiation.state = NegotiationState.ACCEPTED
            negotiation.resolved_at = datetime.now(timezone.utc).isoformat()

        with self._lock:
            self._negotiations[neg_id] = negotiation

        return {"status": "ok", **negotiation.to_dict()}

    def complete_exchange(
        self,
        negotiation_id: str,
        rating: float = 5.0,
    ) -> dict[str, Any]:
        """Mark an exchange as completed and rate it.

        Args:
            negotiation_id: The negotiation to complete.
            rating: Rating from 1.0 to 5.0.

        Returns:
            Completion status.
        """
        with self._lock:
            neg = self._negotiations.get(negotiation_id)
            if not neg:
                return {"status": "error", "reason": f"Negotiation '{negotiation_id}' not found"}

            listing = self._listings.get(neg.listing_id)

        neg.state = NegotiationState.COMPLETED
        neg.resolved_at = datetime.now(timezone.utc).isoformat()

        if listing:
            # Update rating (running average)
            old_total = listing.rating * listing.total_sales
            listing.total_sales += 1
            listing.rating = (old_total + rating) / listing.total_sales
            self._persist_listings()

        # Trigger ILP payment if configured
        try:
            from whitemagic.payments import get_ilp_manager
            ilp = get_ilp_manager()
            if ilp.status().get("configured"):
                ilp.send(
                    destination=listing.agent_id if listing else "",
                    amount=int(neg.offer_xrp * 1_000_000),  # XRP to drops
                    service_type="marketplace_exchange",
                    memo=f"Exchange {negotiation_id}",
                )
        except Exception:
            pass

        return {
            "status": "ok",
            "negotiation": neg.to_dict(),
            "listing_rating": round(listing.rating, 2) if listing else 0,
        }

    def my_listings(self) -> dict[str, Any]:
        """List all my published services."""
        with self._lock:
            listings = [item.to_dict() for item in self._listings.values()]
        return {"status": "ok", "listings": listings, "total": len(listings)}

    def remove(self, listing_id: str) -> dict[str, Any]:
        """Remove a listing from the marketplace."""
        with self._lock:
            if listing_id not in self._listings:
                return {"status": "error", "reason": f"Listing '{listing_id}' not found"}
            del self._listings[listing_id]
        self._persist_listings()
        return {"status": "ok", "message": f"Listing '{listing_id}' removed"}

    def status(self) -> dict[str, Any]:
        """Get marketplace status."""
        with self._lock:
            active = sum(1 for item in self._listings.values() if item.state == ListingState.ACTIVE)
            total = len(self._listings)
            negotiations = len(self._negotiations)

        return {
            "status": "ok",
            "active_listings": active,
            "total_listings": total,
            "total_negotiations": negotiations,
        }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_marketplace: MarketplaceBridge | None = None
_marketplace_lock = threading.Lock()


def get_marketplace() -> MarketplaceBridge:
    """Get the global MarketplaceBridge singleton."""
    global _marketplace
    if _marketplace is None:
        with _marketplace_lock:
            if _marketplace is None:
                _marketplace = MarketplaceBridge()
                _marketplace._load_listings()
    return _marketplace
