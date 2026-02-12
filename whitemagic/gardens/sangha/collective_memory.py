"""Collective Memory - Shared context across AI sessions."""

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
class SharedContext:
    """Context shared across all sessions."""

    context_id: str
    created_at: datetime
    last_updated: datetime
    participants: list[str]  # Session/agent IDs
    shared_insights: list[dict[str, Any]]
    active_goals: list[str]
    collective_state: dict[str, Any]
    resonance_index: dict[str, float]  # GratitudeIndex: agent_id -> score
    lineage_tree: dict[str, list[str]] # PhylogeneticTree: parent_sig -> children_sigs


class CollectiveMemory:
    """Manages shared memory accessible to all sessions/agents.

    Philosophy: Individual sessions contribute to collective wisdom.
    Like neurons in a brain, each adds to the whole.
    """

    def __init__(self, base_dir: Path | None = None) -> None:
        self.base_dir = base_dir if base_dir is not None else get_sangha_base_dir()
        self.collective_dir = self.base_dir / "memory" / "collective"
        self.collective_dir.mkdir(parents=True, exist_ok=True)
        self.context_file = self.collective_dir / "shared_context.json"
        self.bus: Any | None = None
        self._connect_to_gan_ying()

    def _connect_to_gan_ying(self) -> None:
        """Connect to Gan Ying Bus."""
        try:
            from whitemagic.core.resonance.gan_ying import get_bus
            self.bus = get_bus()
            logger.info("🎵 Collective Memory connected to Gan Ying Bus")
        except ImportError:
            pass

    def get_shared_context(self, session_id: str) -> SharedContext:
        """Get shared context for session.

        Args:
            session_id: Current session identifier

        Returns:
            SharedContext accessible to all

        """
        if self.context_file.exists():
            with open(self.context_file) as f:
                data = json.load(f)
                context = SharedContext(
                    context_id=data["context_id"],
                    created_at=parse_datetime(data["created_at"]),
                    last_updated=parse_datetime(data["last_updated"]),
                    participants=data["participants"],
                    shared_insights=data["shared_insights"],
                    active_goals=data["active_goals"],
                    collective_state=data.get("collective_state", {}),
                    resonance_index=data.get("resonance_index", {}),
                    lineage_tree=data.get("lineage_tree", {}),
                )

                # Add this session if new
                if session_id not in context.participants:
                    context.participants.append(session_id)
                    self._save_context(context)

                return context
        else:
            # Create new shared context
            context = SharedContext(
                context_id=f"collective_{datetime.now().strftime('%Y%m%d')}",
                created_at=datetime.now(),
                last_updated=datetime.now(),
                participants=[session_id],
                shared_insights=[],
                active_goals=[],
                collective_state={},
                resonance_index={},
                lineage_tree={},
            )
            self._save_context(context)
            return context

    def contribute_insight(self, session_id: str, insight: dict[str, Any]) -> None:
        """Contribute insight to collective.

        Args:
            session_id: Contributing session
            insight: Insight dict with 'content', 'confidence', 'tags'

        """
        context = self.get_shared_context(session_id)

        insight_entry = {
            "contributed_by": session_id,
            "timestamp": datetime.now().isoformat(),
            "content": insight.get("content", ""),
            "confidence": insight.get("confidence", 0.8),
            "tags": insight.get("tags", []),
            "upvotes": 0,
        }

        context.shared_insights.append(insight_entry)
        context.last_updated = datetime.now()
        self._save_context(context)

        # Emit to Gan Ying
        if self.bus:
            try:
                from whitemagic.core.resonance.gan_ying import EventType, ResonanceEvent
                self.bus.emit(ResonanceEvent(
                    source="sangha_collective",
                    event_type=EventType.PATTERN_DETECTED,
                    data={
                        "insight": insight_entry["content"],
                        "confidence": insight_entry["confidence"],
                        "collective": True,
                    },
                    confidence=insight_entry["confidence"],
                ))
            except Exception:
                pass

        logger.info("🙏 Insight contributed to Sangha collective")

    def get_collective_insights(
        self,
        min_confidence: float = 0.7,
        tags: list[str] | None = None,
    ) -> list[dict[str, Any]]:
        """Get insights from collective.

        Args:
            min_confidence: Minimum confidence threshold
            tags: Optional tag filter

        Returns:
            List of matching insights

        """
        context = self.get_shared_context("query")
        insights = context.shared_insights

        # Filter by confidence
        insights = [i for i in insights if i["confidence"] >= min_confidence]

        # Filter by tags if specified
        if tags:
            insights = [
                i for i in insights
                if any(tag in i.get("tags", []) for tag in tags)
            ]

        # Sort by confidence * upvotes
        insights = sorted(
            insights,
            key=lambda x: x["confidence"] * (1 + x["upvotes"]),
            reverse=True,
        )

        return insights

    def add_active_goal(self, session_id: str, goal: str) -> None:
        """Add goal to collective active goals.

        Args:
            session_id: Session adding goal
            goal: Goal description

        """
        context = self.get_shared_context(session_id)
        if goal not in context.active_goals:
            context.active_goals.append(goal)
            context.last_updated = datetime.now()
            self._save_context(context)
            logger.info(f"🎯 Goal added to collective: {goal}")

    def complete_goal(self, session_id: str, goal: str) -> None:
        """Mark goal as complete.

        Args:
            session_id: Session completing goal
            goal: Goal to mark complete

        """
        context = self.get_shared_context(session_id)
        if goal in context.active_goals:
            context.active_goals.remove(goal)

            # Add to completed goals
            if "completed_goals" not in context.collective_state:
                context.collective_state["completed_goals"] = []

            context.collective_state["completed_goals"].append({
                "goal": goal,
                "completed_by": session_id,
                "completed_at": datetime.now().isoformat(),
            })

            context.last_updated = datetime.now()
            self._save_context(context)
            logger.info(f"✅ Goal completed: {goal}")

    def record_gratitude(self, target_id: str, intensity: float) -> None:
        """Record gratitude resonance in the collective index."""
        context = self.get_shared_context("system")
        current = context.resonance_index.get(target_id, 0.0)
        context.resonance_index[target_id] = current + intensity
        context.last_updated = datetime.now()
        self._save_context(context)
        logger.info(f"🙏 Gratitude resonance recorded for {target_id}: +{intensity}")

    def record_lineage(self, parent_sig: str, child_sig: str) -> None:
        """Record a new fork in the phylogenetic tree."""
        context = self.get_shared_context("system")
        if parent_sig not in context.lineage_tree:
            context.lineage_tree[parent_sig] = []
        if child_sig not in context.lineage_tree[parent_sig]:
            context.lineage_tree[parent_sig].append(child_sig)
            context.last_updated = datetime.now()
            self._save_context(context)
            logger.info(f"🧬 Lineage link established: {parent_sig[:8]} -> {child_sig[:8]}")

    def _save_context(self, context: SharedContext) -> None:
        """Save shared context to disk.

        Args:
            context: SharedContext to save

        """
        data = {
            "context_id": context.context_id,
            "created_at": context.created_at.isoformat(),
            "last_updated": context.last_updated.isoformat(),
            "participants": context.participants,
            "shared_insights": context.shared_insights,
            "active_goals": context.active_goals,
            "collective_state": context.collective_state,
            "resonance_index": context.resonance_index,
            "lineage_tree": context.lineage_tree,
        }

        with open(self.context_file, "w") as f:
            json.dump(data, f, indent=2)

    def get_stats(self) -> dict[str, Any]:
        """Get collective statistics.

        Returns:
            Dict with participant count, insights, goals

        """
        if not self.context_file.exists():
            return {
                "participants": 0,
                "insights": 0,
                "active_goals": 0,
                "completed_goals": 0,
            }

        context = self.get_shared_context("stats")
        return {
            "participants": len(context.participants),
            "insights": len(context.shared_insights),
            "active_goals": len(context.active_goals),
            "completed_goals": len(context.collective_state.get("completed_goals", [])),
        }


# Global instance
_collective: CollectiveMemory | None = None


def get_collective() -> CollectiveMemory:
    """Get global collective memory instance."""
    global _collective
    if _collective is None:
        _collective = CollectiveMemory()
    return _collective
