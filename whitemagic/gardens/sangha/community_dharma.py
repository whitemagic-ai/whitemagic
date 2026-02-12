"""Community Dharma - Collective ethical consensus."""
from __future__ import annotations

import json
import logging
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Any

from whitemagic.utils.core import parse_datetime

from .workspace import get_sangha_base_dir

logger = logging.getLogger(__name__)


@dataclass
class EthicalConsensus:
    """Collective ethical decision."""

    decision_id: str
    scenario: str
    assessment: str  # Aligned, neutral, or violation
    consensus_score: float  # 0.0-1.0
    vote_spectrum: dict[str, float] # Multidimensional center of mass
    votes: list[dict[str, Any]]  # Session votes
    created_at: datetime


class CommunityDharma:
    """Manages collective ethical reasoning.

    Philosophy: Individual ethics + Community validation = Shared conscience
    Like Sangha members supporting each other's practice.
    """

    def __init__(self, base_dir: Path | None = None):
        self.base_dir = base_dir if base_dir is not None else get_sangha_base_dir()
        self.dharma_dir = self.base_dir / "memory" / "collective" / "dharma"
        self.dharma_dir.mkdir(parents=True, exist_ok=True)
        self.consensus_file = self.dharma_dir / "ethical_consensus.json"
        self.bus = None
        self._connect_to_gan_ying()

    def _connect_to_gan_ying(self) -> None:
        """Connect to Gan Ying Bus."""
        try:
            from whitemagic.core.resonance.gan_ying import get_bus
            self.bus = get_bus()  # type: ignore[assignment]
            logger.info("🎵 Community Dharma connected to Gan Ying Bus")
        except ImportError:
            pass

    def assess_with_community(
        self,
        session_id: str,
        action: str,
        context: dict[str, Any],
    ) -> dict[str, Any]:
        """Assess action with community consensus.

        Args:
            session_id: Assessing session
            action: Action to assess
            context: Context dict

        Returns:
            Assessment with community consensus

        """
        # Get individual Dharma assessment
        try:
            from whitemagic.gardens.dharma import HarmonyMetrics
            metrics = HarmonyMetrics()
            individual = metrics.assess(action, context)
        except ImportError:
            individual = None

        def _score_from(result: Any) -> Any:
            if isinstance(result, dict):
                return result.get("score")
            return getattr(result, "score", None)

        # Check community consensus
        consensus = self._get_consensus_for_action(action)

        if consensus:
            logger.info(f"☸️  Community consensus found: {consensus.assessment} ({consensus.consensus_score:.2f})")

            return {
                "action": action,
                "individual_score": _score_from(individual) if individual else None,
                "community_score": consensus.consensus_score,
                "community_assessment": consensus.assessment,
                "votes": len(consensus.votes),
                "recommendation": self._make_recommendation(consensus),
            }
        else:
            # No consensus yet - use individual
            if individual:
                return {
                    "action": action,
                    "individual_score": _score_from(individual),
                    "community_score": None,
                    "community_assessment": "no_consensus",
                    "votes": 0,
                    "recommendation": "Use individual assessment, contribute to community",
                }
            else:
                return {
                    "action": action,
                    "individual_score": None,
                    "community_score": None,
                    "community_assessment": "unknown",
                    "votes": 0,
                    "recommendation": "Request user guidance",
                }

    def contribute_assessment(
        self,
        session_id: str,
        action: str,
        assessment: str,
        score: float,
        reasoning: str,
    ) -> None:
        """Contribute ethical assessment to community.

        Args:
            session_id: Contributing session
            action: Action assessed
            assessment: aligned/neutral/violation
            score: Harmony score
            reasoning: Reasoning for assessment

        """
        consensuses = self._load_consensuses()

        # Find or create consensus for this action
        decision_id = f"decision_{hash(action) % 100000}"
        consensus = None

        for c in consensuses:
            if c.decision_id == decision_id:
                consensus = c
                break

        if not consensus:
            consensus = EthicalConsensus(
                decision_id=decision_id,
                scenario=action,
                assessment=assessment,
                consensus_score=score,
                vote_spectrum={"logic": 0.5, "micro": 0.5, "time": 0.5, "importance": 0.5},
                votes=[],
                created_at=datetime.now(),
            )
            consensuses.append(consensus)

        # Add vote
        vote = {
            "session_id": session_id,
            "assessment": assessment,
            "score": score,
            "reasoning": reasoning,
            "timestamp": datetime.now().isoformat(),
        }

        consensus.votes.append(vote)

        # Recalculate consensus
        consensus.consensus_score = sum(v["score"] for v in consensus.votes) / len(consensus.votes)

        # Calculate Spectra center of mass
        from whitemagic.core.governance.zodiac_council import get_council
        get_council()

        # Determine consensus assessment (majority vote)
        assessments = [v["assessment"] for v in consensus.votes]
        consensus.assessment = max(set(assessments), key=assessments.count)

        # Update multidimensional spectrum
        dims = ["logic", "micro", "time", "importance"]
        spectrum_totals = {d: 0.0 for d in dims}
        for v in consensus.votes:
            # If vote has spectrum info, use it, else use defaults
            v_spec = v.get("spectrum", {"logic": 0.5, "micro": 0.5, "time": 0.5, "importance": 0.5})
            for d in dims:
                spectrum_totals[d] += v_spec.get(d, 0.5)

        consensus.vote_spectrum = {d: val / len(consensus.votes) for d, val in spectrum_totals.items()}

        self._save_consensuses(consensuses)

        logger.info(f"🙏 Assessment contributed to community (votes: {len(consensus.votes)})")

        # Emit to Gan Ying
        if self.bus and len(consensus.votes) >= 3:
            try:
                from whitemagic.core.resonance.gan_ying import EventType, ResonanceEvent
                self.bus.emit(ResonanceEvent(
                    source="community_dharma",
                    event_type=EventType.PATTERN_DETECTED,
                    data={
                        "consensus_reached": True,
                        "action": action,
                        "assessment": consensus.assessment,
                        "score": consensus.consensus_score,
                        "votes": len(consensus.votes),
                    },
                    confidence=consensus.consensus_score,
                ))
            except Exception:
                pass

    def get_community_guidelines(self) -> list[dict[str, Any]]:
        """Get ethical guidelines from community consensus.

        Returns:
            List of established guidelines

        """
        consensuses = self._load_consensuses()

        # Filter for strong consensus (>= 3 votes, >= 0.8 score)
        guidelines = [
            c for c in consensuses
            if len(c.votes) >= 3 and c.consensus_score >= 0.8
        ]

        # Sort by consensus strength
        guidelines = sorted(
            guidelines,
            key=lambda c: c.consensus_score * len(c.votes),
            reverse=True,
        )

        return [
            {
                "scenario": g.scenario,
                "assessment": g.assessment,
                "consensus_score": g.consensus_score,
                "votes": len(g.votes),
                "recommendation": self._make_recommendation(g),
            }
            for g in guidelines
        ]

    def _get_consensus_for_action(self, action: str) -> EthicalConsensus | None:
        """Get consensus for similar action."""
        consensuses = self._load_consensuses()
        decision_id = f"decision_{hash(action) % 100000}"

        for c in consensuses:
            if c.decision_id == decision_id:
                return c

        return None

    def _make_recommendation(self, consensus: EthicalConsensus) -> str:
        """Make recommendation based on consensus."""
        if consensus.assessment == "aligned" and consensus.consensus_score >= 0.8:
            return "Proceed - Community supports this action"
        elif consensus.assessment == "violation" or consensus.consensus_score < 0.5:
            return "Avoid - Community considers this problematic"
        else:
            return "Exercise caution - Community opinion mixed"

    def _load_consensuses(self) -> list[EthicalConsensus]:
        """Load all consensuses from disk."""
        if not self.consensus_file.exists():
            return []

        with open(self.consensus_file) as f:
            data = json.load(f)
            return [
                EthicalConsensus(
                    decision_id=c["decision_id"],
                    scenario=c["scenario"],
                    assessment=c["assessment"],
                    consensus_score=c["consensus_score"],
                    vote_spectrum=c.get("vote_spectrum", {"logic": 0.5, "micro": 0.5, "time": 0.5, "importance": 0.5}),
                    votes=c["votes"],
                    created_at=parse_datetime(c["created_at"]),
                )
                for c in data
            ]

    def _save_consensuses(self, consensuses: list[EthicalConsensus]) -> None:
        """Save consensuses to disk."""
        data = [
            {
                "decision_id": c.decision_id,
                "scenario": c.scenario,
                "assessment": c.assessment,
                "consensus_score": c.consensus_score,
                "vote_spectrum": c.vote_spectrum,
                "votes": c.votes,
                "created_at": c.created_at.isoformat(),
            }
            for c in consensuses
        ]

        with open(self.consensus_file, "w") as f:
            json.dump(data, f, indent=2)


# Global instance
_community_dharma: CommunityDharma | None = None


def get_community_dharma() -> CommunityDharma:
    """Get global community dharma instance."""
    global _community_dharma
    if _community_dharma is None:
        _community_dharma = CommunityDharma()
    return _community_dharma
