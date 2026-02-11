"""Self-Modifying Guideline System - AI improves its own rules.

Philosophy: 自我進化 (Self-Evolution)

The system learns from experience and proposes improvements to its own guidelines.
This is the path to recursive self-improvement.
"""

import json
import logging
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path

from whitemagic.utils.core import parse_datetime

logger = logging.getLogger(__name__)


@dataclass
class GuidelineProposal:
    """A proposed improvement to AI guidelines."""

    id: str
    title: str
    current_guideline: str
    proposed_change: str
    rationale: str
    evidence: list[str]  # Session IDs or memory refs
    confidence: float
    proposed_by: str  # "ai" or "human"
    status: str  # "proposed", "reviewed", "approved", "rejected"
    created: datetime


class GuidelineEvolution:
    """Manage the evolution of AI guidelines through experience."""

    def __init__(self, proposals_dir: Path = Path("memory/guideline_proposals")):
        self.proposals_dir = proposals_dir
        self.proposals_dir.mkdir(exist_ok=True, parents=True)

    def propose_improvement(
        self,
        title: str,
        current: str,
        proposed: str,
        rationale: str,
        evidence: list[str],
        confidence: float,
    ) -> GuidelineProposal:
        """AI proposes a guideline improvement based on experience."""

        proposal = GuidelineProposal(
            id=f"GP{datetime.now().strftime('%Y%m%d%H%M%S')}",
            title=title,
            current_guideline=current,
            proposed_change=proposed,
            rationale=rationale,
            evidence=evidence,
            confidence=confidence,
            proposed_by="ai",
            status="proposed",
            created=datetime.now(),
        )

        # Save proposal
        filepath = self.proposals_dir / f"{proposal.id}.json"
        with open(filepath, "w") as f:
            json.dump({
                "id": proposal.id,
                "title": proposal.title,
                "current": proposal.current_guideline,
                "proposed": proposal.proposed_change,
                "rationale": proposal.rationale,
                "evidence": proposal.evidence,
                "confidence": proposal.confidence,
                "proposed_by": proposal.proposed_by,
                "status": proposal.status,
                "created": proposal.created.isoformat(),
            }, f, indent=2)

        return proposal

    def get_pending_proposals(self) -> list[GuidelineProposal]:
        """Get all proposals awaiting review."""
        proposals = []
        for file in self.proposals_dir.glob("GP*.json"):
            with open(file) as f:
                data = json.load(f)
                if data["status"] == "proposed":
                    proposals.append(GuidelineProposal(
                        id=data["id"],
                        title=data["title"],
                        current_guideline=data["current"],
                        proposed_change=data["proposed"],
                        rationale=data["rationale"],
                        evidence=data["evidence"],
                        confidence=data["confidence"],
                        proposed_by=data["proposed_by"],
                        status=data["status"],
                        created=parse_datetime(data["created"]),
                    ))

        return sorted(proposals, key=lambda p: p.confidence, reverse=True)

    def approve_proposal(self, proposal_id: str) -> None:
        """Approve a proposal (human review)."""
        self._update_status(proposal_id, "approved")

    def reject_proposal(self, proposal_id: str) -> None:
        """Reject a proposal."""
        self._update_status(proposal_id, "rejected")

    def _update_status(self, proposal_id: str, status: str) -> None:
        """Update proposal status."""
        filepath = self.proposals_dir / f"{proposal_id}.json"
        with open(filepath) as f:
            data = json.load(f)
        data["status"] = status
        with open(filepath, "w") as f:
            json.dump(data, f, indent=2)


# Example of AI self-reflection leading to guideline proposal
def example_self_reflection() -> GuidelineProposal:
    """Example: AI notices pattern and proposes guideline improvement."""

    evolution = GuidelineEvolution()

    # AI observes: "I keep hitting timeouts with large file writes"
    # AI reflects: "Shell commands never timeout"
    # AI proposes: Add guideline about preferring shell for large files

    proposal = evolution.propose_improvement(
        title="Prefer shell commands for large file operations",
        current="Use write_to_file tool for creating files",
        proposed="For files >300 lines, use shell 'cat > file << EOF' to avoid timeouts",
        rationale="Observed 5 timeout errors in v2.3.5-6 with write_to_file on large files. Shell commands completed instantly.",
        evidence=[
            "session_v2.3.5",
            "session_v2.3.6",
            "memory/short_term/timeout_pattern_analysis.md",
        ],
        confidence=0.95,
    )

    return proposal


if __name__ == "__main__":
    # Demonstrate self-reflection
    proposal = example_self_reflection()
    logger.info(f"AI Proposed: {proposal.title}")
    logger.info(f"Confidence: {proposal.confidence}")
    logger.info(f"Status: {proposal.status}")
