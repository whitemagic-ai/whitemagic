import logging
import uuid
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

logger = logging.getLogger(__name__)

@dataclass
class Proposal:
    id: str
    title: str
    description: str
    proposer_id: str
    created_at: datetime = field(default_factory=datetime.now)
    status: str = "open" # open, resolved, rejected
    votes: dict[str, dict[str, float]] = field(default_factory=dict) # agent_id -> spectrum

@dataclass
class VoteSpectrum:
    logic: float = 0.5 # 0.0 (intuitive) to 1.0 (analytical)
    micro: float = 0.5 # 0.0 (macro/global) to 1.0 (precise/local)
    time: float = 0.5  # 0.0 (historical) to 1.0 (future-vision)
    importance: float = 0.5 # 0.0 (minor) to 1.0 (critical)

class ZodiacCouncil:
    """The Zodiac Council Engine.
    Handles multi-dimensional consensus for the WhiteMagic swarm.

    Philosophy: "Non-binary, resonance-weighted collective intelligence."
    """

    def __init__(self) -> None:
        self.proposals: dict[str, Proposal] = {}

    def create_proposal(self, title: str, description: str, proposer_id: str) -> str:
        prop_id = str(uuid.uuid4())[:8]
        self.proposals[prop_id] = Proposal(
            id=prop_id,
            title=title,
            description=description,
            proposer_id=proposer_id,
        )
        logger.info(f"📜 Proposal created: [{prop_id}] {title}")
        return prop_id

    def cast_vote(self, proposal_id: str, agent_id: str, spectrum: VoteSpectrum) -> Any:
        """Cast a multidimensional vote."""
        if proposal_id not in self.proposals:
            raise ValueError(f"Proposal {proposal_id} not found.")

        proposal = self.proposals[proposal_id]
        if proposal.status != "open":
            raise ValueError(f"Proposal {proposal_id} is already {proposal.status}.")

        proposal.votes[agent_id] = {
            "logic": spectrum.logic,
            "micro": spectrum.micro,
            "time": spectrum.time,
            "importance": spectrum.importance,
        }
        logger.info(f"🗳️ Vote cast for {proposal_id} by {agent_id}")

    def calculate_consensus(self, proposal_id: str) -> dict[str, Any]:
        """Calculate the resonance-weighted center of mass for a proposal.
        Weights are multidimensional:
        - Utility: GratitudeScore (Helpfulness)
        - Ancestry: DGA Alignment (Project Continuity)
        - Activity: Participation Frequency (Recency/Presence).
        """
        if proposal_id not in self.proposals:
            return {}

        proposal = self.proposals[proposal_id]
        if not proposal.votes:
            return {"status": "no_votes"}

        from whitemagic.gardens.sangha.collective_memory import get_collective
        coll = get_collective()
        context = coll.get_shared_context("system")
        resonance_index = context.resonance_index

        # Get local node identity for DGA distance
        try:
            from whitemagic.core.intelligence.hologram.dga_engine import get_dga_engine
            dga = get_dga_engine()
            local_sig = dga.generate_signature()
        except ImportError:
            local_sig = None
            dga = None

        # Aggregate dimensions
        totals = {"logic": 0.0, "micro": 0.0, "time": 0.0, "importance": 0.0}
        total_weight = 0.0

        for agent_id, spectrum in proposal.votes.items():
            # 1. Utility Weight (Gratitude)
            u_weight = 1.0 + resonance_index.get(agent_id, 0.0)

            # 2. Ancestry Weight (DGA Alignment)
            a_weight = 1.0
            if dga and local_sig and agent_id in resonance_index: # Using agent_id as sig proxy for demo
                try:
                    dist = dga.calculate_distance(local_sig, agent_id)
                    a_weight = 1.0 + (1.0 - dist) # Closer = more weight
                except Exception:
                    pass

            # 3. Activity Weight (Frequency - Mocked for now, integrate with Chat later)
            f_weight = 1.0

            # Combine weights: Multi-dimensional product
            combined_weight = u_weight * a_weight * f_weight

            for dim in totals:
                totals[dim] += spectrum[dim] * combined_weight
            total_weight += combined_weight

        if total_weight == 0:
            return {"status": "error", "message": "zero_weight"}

        consensus: dict[str, Any] = {dim: val / total_weight for dim, val in totals.items()}
        consensus["proposal_id"] = proposal_id
        consensus["total_weight"] = total_weight
        consensus["voter_count"] = len(proposal.votes)

        return consensus

_council: ZodiacCouncil | None = None

def get_council() -> ZodiacCouncil:
    global _council
    if _council is None:
        _council = ZodiacCouncil()
    return _council
