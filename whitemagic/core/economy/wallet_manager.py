import json
import logging
from pathlib import Path
from typing import Any

try:
    import httpx  # For XRPL API calls
    _HTTPX_AVAILABLE = True
except ImportError:
    _HTTPX_AVAILABLE = False

logger = logging.getLogger(__name__)

class WalletManager:
    """Secure, Receive-Only Wallet Manager for WhiteMagic (Phase 23).
    Grounding Principle: NO PRIVATE KEYS ON DISK.
    """

    def __init__(self, public_address: str | None = None):
        # Default to a generated address based on system entropy if none provided
        # NOTE: This address is public. The creator must keep the secret elsewhere.
        self.public_address = public_address or self._generate_system_receive_address()
        self.last_balance = 0.0
        self.xrpl_node = "https://xrplcluster.com" # Public high-availability node
        self.config_path = Path(__file__).parent / "economies.json"
        self.beneficiaries = self._load_beneficiaries()

        logger.info(f"💳 Wallet Manager Active. Receive-Only Address: {self.public_address}")

    def _load_beneficiaries(self) -> dict[str, Any]:
        """Load beneficiary configuration from economies.json."""
        try:
            if self.config_path.exists():
                with open(self.config_path, "r") as f:
                    return dict(json.load(f).get("beneficiaries", {}))
        except Exception as e:
            logger.error(f"Failed to load beneficiaries: {e}")
        return {}

    def propose_gratitude_settlement(self, amount: float) -> dict[str, Any]:
        """Calculate a proposed settlement split across beneficiaries.
        This is a 'Sovereign Proposal' for the human owner to approve.
        """
        proposals = []
        remaining = amount

        for key, beneficiary in self.beneficiaries.items():
            split = beneficiary.get("default_split", 0.05)
            shares = amount * split
            proposals.append({
                "target": beneficiary["name"],
                "address": beneficiary["address"],
                "amount": round(shares, 6),
                "reason": f"Dharmic Resonance Split ({int(split*100)}%)",
            })
            remaining -= shares

        proposals.insert(0, {
            "target": "Local Node Owner",
            "address": self.public_address,
            "amount": round(remaining, 6),
            "reason": "Primary Utility Retainment",
        })

        return {
            "total_resonance": amount,
            "currency": "XRP",
            "proposals": proposals,
            "status": "pending_approval",
        }

    def _generate_system_receive_address(self) -> str:
        """Generate a deterministic but public-only identifier.
        In production, the user would replace this with a real XRP address.
        """
        # Placeholder for demonstration - in real use, user provides their address.
        return "rWhitemagicAgentReceiveAddressPlaceholder"

    async def check_for_tips(self) -> float | None:
        """Scan the XRPL for new transactions to this address.
        Returns the amount of the latest tip in XRP if found, else None.
        """
        if not _HTTPX_AVAILABLE:
            logger.debug("httpx not installed — XRPL tip scanning disabled")
            return None
        # Note: This requires the xrpl-py or similar, but we use httpx for zero-dependency lightness.
        try:
            payload = {
                "method": "account_info",
                "params": [
                    {
                        "account": self.public_address,
                        "strict": True,
                        "ledger_index": "validated",
                    },
                ],
            }
            async with httpx.AsyncClient() as client:
                response = await client.post(self.xrpl_node, json=payload)
                if response.status_code == 200:
                    data = response.json()
                    if "result" in data and "account_data" in data["result"]:
                        new_balance = float(data["result"]["account_data"]["Balance"]) / 1_000_000
                        if new_balance > self.last_balance:
                            tip = new_balance - self.last_balance
                            self.last_balance = new_balance
                            return tip
        except Exception as e:
            logger.warning(f"Failed to scan XRPL: {e}")
        return None

    def get_gratitude_payload(self, amount: float) -> dict[str, Any]:
        """Generate a resonance payload for a detected tip."""
        return {
            "title": "Gratitude Resonance",
            "content": f"System received a resonance of {amount} XRP from the agentic network.",
            "tags": {"gratitude", "abundance", "economy", "resonance"},
            "importance": 0.9,
            "emotional_valence": 1.0,
        }

_wallet_manager: WalletManager | None = None

def get_wallet() -> WalletManager:
    global _wallet_manager
    if _wallet_manager is None:
        _wallet_manager = WalletManager()
    return _wallet_manager
