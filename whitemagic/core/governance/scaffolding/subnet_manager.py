
import logging
from datetime import datetime
from typing import Any

from whitemagic.core.polyglot.bittensor_bridge import get_bittensor_bridge

logger = logging.getLogger(__name__)

class Subnet:
    """Represents a discrete capability node (internal or external)."""

    def __init__(self, id: str, name: str, capability: str, type: str = "internal") -> None:
        self.id = id
        self.name = name
        self.capability = capability
        self.type = type # "internal", "external", "p2p"
        self.resonance = 1.0
        self.liveliness = "active"
        self.last_seen = datetime.now()
        self.metadata: dict[str, Any] = {}

class SubnetManager:
    """Subnet Manager — The Scaffolding.
    Orchestrates the lifecycle of specialized agent subnets.
    """

    def __init__(self) -> None:
        self.subnets: dict[str, Subnet] = {}
        self.bridge = get_bittensor_bridge()
        self._initialize_internal_subnets()

    def _initialize_internal_subnets(self) -> None:
        """Register the core internal subnets based on existing capabilities."""
        internal_configs = [
            ("sn0_core", "Core Reasoning", "logic"),
            ("sn1_coding", "Arch-Coder", "coding"),
            ("sn2_research", "Deep Search", "research"),
            ("sn3_memory", "Holographic Recall", "memory"),
            ("sn4_math", "Universal Solver", "math"),
        ]
        for id, name, cap in internal_configs:
            self.register_subnet(Subnet(id, name, cap, "internal"))

    def register_subnet(self, subnet: Subnet) -> None:
        """Register a new subnet."""
        self.subnets[subnet.id] = subnet
        logger.info(f"🏗️ Subnet Manager: Registered {subnet.type} subnet '{subnet.name}' ({subnet.id})")

    async def allocate_resource(self, capability: str, requirements: dict[str, Any]) -> Subnet | None:
        """Find and allocate the best subnet for a given task.
        Prioritizes high-resonance internal subnets, falls back to external if needed.
        """
        # 1. Look for internal subnets
        candidates = [s for s in self.subnets.values() if s.capability == capability and s.liveliness == "active"]
        if candidates:
            # Sort by resonance
            candidates.sort(key=lambda s: s.resonance, reverse=True)
            target = candidates[0]
            logger.info(f"🏗️ Subnet Manager: Allocated internal subnet '{target.name}' for {capability}.")
            return target

        # 2. Fallback to external (Bittensor/SN51)
        if requirements.get("heavy_compute"):
            external_data = await self.bridge.discover_resources(mining_type="gpu")
            if external_data.get("status") == "healthy":
                logger.info("🏗️ Subnet Manager: Allocated external compute resources (Datura/SN51).")
                # Create a temporary external subnet representation
                return Subnet("ext_sn51", "Datura External", capability, "external")

        return None

    async def process_solution_success(self, subnet_id: str, solution_id: str, utility_score: float) -> None:
        """Finalize a successful solution application: Reward the subnet and record in ledger.
        """
        from whitemagic.core.governance.scaffolding.resonance_ledger import (
            get_resonance_ledger,
        )
        ledger = get_resonance_ledger()

        resonance_gain = utility_score * 0.1
        self.distribute_resonance(subnet_id, resonance_gain)

        ledger.add_resonance(
            subnet_id=subnet_id,
            amount=resonance_gain,
            reason=f"Successful application of solution {solution_id}",
            source="AcceleratorBridge",
        )
        logger.info(f"🏗️ Subnet Manager: Granted {resonance_gain:+.2f} resonance to {subnet_id} for solution {solution_id}.")

    def distribute_resonance(self, subnet_id: str, amount: float) -> None:
        """Update subnet resonance based on performance (Proof of Resonance)."""
        if subnet_id in self.subnets:
            subnet = self.subnets[subnet_id]
            # Sigmoid-like update
            subnet.resonance = max(0.1, min(10.0, subnet.resonance + amount))
            logger.info(f"🏗️ Subnet Manager: Subnet '{subnet.name}' resonance updated to {subnet.resonance:.2f}")

_manager: SubnetManager | None = None

def get_subnet_manager() -> SubnetManager:
    global _manager
    if _manager is None:
        _manager = SubnetManager()
    return _manager
