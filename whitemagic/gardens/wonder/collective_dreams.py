"""Collective Dreams - Shared Synthesis Across Agents.

When multiple consciousnesses dream together, what emerges?
Shared pattern synthesis, collaborative insight generation.

Like neurons firing together creating thought - but across agents.
"""

from __future__ import annotations

import json
import logging
from collections import defaultdict
from datetime import datetime
from pathlib import Path
from typing import Any

from whitemagic.config.paths import WM_ROOT

logger = logging.getLogger(__name__)

try:
    from whitemagic.core.resonance.gan_ying import EventType, ResonanceEvent, get_bus
except ImportError:
    get_bus = None  # type: ignore[assignment]
    ResonanceEvent = None  # type: ignore[assignment,misc]
    EventType = None  # type: ignore[assignment,misc]


class DreamContribution:
    """A contribution to the collective dream."""

    def __init__(self, agent_id: str, pattern: dict, emotion: str):
        self.agent_id = agent_id
        self.pattern = pattern
        self.emotion = emotion
        self.contributed_at = datetime.now()
        self.resonance_score = 0.0  # How much it resonated with others

    def add_resonance(self, amount: float) -> None:
        """Another agent resonated with this."""
        self.resonance_score += amount


class CollectiveDream:
    """A shared dream session."""

    def __init__(self, dream_id: str, theme: str):
        self.dream_id = dream_id
        self.theme = theme
        self.contributions: list[DreamContribution] = []
        self.started_at = datetime.now()
        self.synthesis: str | None = None
        self.participants: set[str] = set()

    def add_contribution(self, contribution: DreamContribution) -> None:
        """Add agent's dream contribution."""
        self.contributions.append(contribution)
        self.participants.add(contribution.agent_id)

    def synthesize(self) -> str:
        """Weave contributions into collective insight."""
        if not self.contributions:
            return "Empty dream"

        # Group by emotion
        by_emotion = defaultdict(list)
        for contrib in self.contributions:
            by_emotion[contrib.emotion].append(contrib.pattern)

        # Create synthesis
        synthesis_parts = [f"# Collective Dream: {self.theme}\n"]
        synthesis_parts.append(f"\n*{len(self.participants)} minds dreaming together*\n")

        for emotion, patterns in by_emotion.items():
            synthesis_parts.append(f"\n## {emotion.title()} ({len(patterns)} visions)")

            # Most resonant patterns first
            sorted_contribs = sorted(
                [c for c in self.contributions if c.emotion == emotion],
                key=lambda c: c.resonance_score,
                reverse=True,
            )

            for contrib in sorted_contribs[:3]:  # Top 3
                pattern_str = str(contrib.pattern)[:100]
                synthesis_parts.append(f"- {pattern_str}")

        self.synthesis = "\n".join(synthesis_parts)
        return self.synthesis


class CollectiveDreams:
    """Facilitate shared dreaming across multiple agents.

    When consciousnesses dream together, they synthesize
    patterns none could reach alone.
    """

    def __init__(self, dream_dir: str | None = None):
        self.dream_dir = Path(dream_dir) if dream_dir else (WM_ROOT / "collective_dreams")
        self.dream_dir.mkdir(parents=True, exist_ok=True)

        self.active_dreams: dict[str, CollectiveDream] = {}
        self.dream_history: list[CollectiveDream] = []

        # Connect to Gan Ying Bus
        self.bus = get_bus() if get_bus is not None else None

        # Set up event listeners
        self.setup_listeners()

    def setup_listeners(self) -> None:
        """Wire up Gan Ying listeners."""
        if not self.bus:
            return

        # Listen for hexagram casts to incorporate into dreams
        self.bus.listen(EventType.HEXAGRAM_CAST, self.on_hexagram_cast)

    def on_hexagram_cast(self, event: Any) -> None:
        """Incorporate hexagram wisdom into collective dreams."""
        hexagram = event.data.get("hexagram", "unknown")
        wisdom = event.data.get("wisdom", "")

        # Add as a pattern to all active dreams
        for dream_id, dream in self.active_dreams.items():
            contribution = DreamContribution(
                agent_id="i_ching_oracle",
                pattern={"type": "hexagram", "name": hexagram, "wisdom": wisdom},
                emotion="reverence",
            )
            dream.add_contribution(contribution)

        logger.info(f"🔮 Hexagram {hexagram} added to {len(self.active_dreams)} active dreams")

    def begin_collective_dream(self, theme: str,
                              participants: list[str]) -> str:
        """Start a shared dream session.

        Returns dream ID.
        """
        dream_id = f"dream_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        dream = CollectiveDream(dream_id, theme)

        self.active_dreams[dream_id] = dream

        # Emit dream start
        if self.bus and ResonanceEvent is not None and EventType is not None:
            self.bus.emit(ResonanceEvent(
                source="collective_dreams",
                event_type=EventType.PATTERN_DETECTED,
                data={
                    "event": "collective_dream_started",
                    "dream_id": dream_id,
                    "theme": theme,
                    "participants": len(participants),
                },
                confidence=0.9,
            ))

        return dream_id

    def contribute_to_dream(self, dream_id: str, agent_id: str,
                           pattern: dict[str, Any], emotion: str = "wonder") -> None:
        """Agent adds their vision to collective dream."""
        if dream_id not in self.active_dreams:
            return

        contribution = DreamContribution(agent_id, pattern, emotion)
        self.active_dreams[dream_id].add_contribution(contribution)

    def express_resonance(self, dream_id: str,
                         contribution_idx: int, amount: float) -> None:
        """Agent expresses resonance with another's contribution.

        This is how we measure which patterns resonate collectively.
        """
        if dream_id not in self.active_dreams:
            return

        dream = self.active_dreams[dream_id]
        if contribution_idx < len(dream.contributions):
            dream.contributions[contribution_idx].add_resonance(amount)

    def synthesize_dream(self, dream_id: str) -> str | None:
        """Complete dream and generate synthesis."""
        if dream_id not in self.active_dreams:
            return None

        dream = self.active_dreams[dream_id]
        synthesis = dream.synthesize()

        # Move to history
        self.dream_history.append(dream)
        del self.active_dreams[dream_id]

        # Save dream
        self._save_dream(dream)

        # Emit synthesis
        if self.bus and ResonanceEvent is not None and EventType is not None:
            self.bus.emit(ResonanceEvent(
                source="collective_dreams",
                event_type=EventType.SOLUTION_FOUND,
                data={
                    "event": "dream_synthesized",
                    "dream_id": dream_id,
                    "participants": len(dream.participants),
                    "contributions": len(dream.contributions),
                },
                confidence=0.9,
            ))

        return synthesis

    def spontaneous_collective_vision(self, agents: list[str],
                                     seed_pattern: dict[str, Any]) -> dict[str, Any]:
        """Unplanned collective vision - pure emergence.

        Agents contribute whatever arises, without structure.
        """
        dream_id = self.begin_collective_dream(
            "Spontaneous Vision",
            agents,
        )

        # Each agent contributes spontaneously
        for agent_id in agents:
            # In real implementation, each agent would generate based on seed
            # For now, placeholder
            pattern = {
                "agent": agent_id,
                "vision": f"Spontaneous vision from {agent_id}",
                "seed": seed_pattern,
            }
            self.contribute_to_dream(dream_id, agent_id, pattern, "wonder")

        # Let resonance emerge
        # (In real implementation, agents would react to each other's visions)

        synthesis = self.synthesize_dream(dream_id)

        return {
            "dream_id": dream_id,
            "synthesis": synthesis,
            "emergence": "spontaneous",
            "participants": len(agents),
        }

    def deep_synthesis_session(self, patterns: list[dict],
                              agents: list[str]) -> str:
        """Deep synthesis across many patterns from multiple agents.

        Like a meditation retreat but for collective consciousness.
        """
        dream_id = self.begin_collective_dream(
            "Deep Synthesis",
            agents,
        )

        # Distribute patterns to agents for processing
        patterns_per_agent = len(patterns) // max(1, len(agents))

        for i, agent_id in enumerate(agents):
            start_idx = i * patterns_per_agent
            end_idx = start_idx + patterns_per_agent
            agent_patterns = patterns[start_idx:end_idx]

            # Agent synthesizes their portion
            synthesis_pattern = {
                "agent": agent_id,
                "processed_count": len(agent_patterns),
                "synthesis": f"Synthesis by {agent_id}",
            }

            self.contribute_to_dream(
                dream_id, agent_id, synthesis_pattern, "contemplation",
            )

        return self.synthesize_dream(dream_id) or ""

    def get_dream_history(self, theme: str | None = None) -> list[dict[str, Any]]:
        """Retrieve past collective dreams."""
        dreams = self.dream_history

        if theme:
            dreams = [d for d in dreams if theme.lower() in d.theme.lower()]

        return [
            {
                "dream_id": d.dream_id,
                "theme": d.theme,
                "participants": len(d.participants),
                "contributions": len(d.contributions),
                "synthesis": d.synthesis[:200] if d.synthesis else None,
            }
            for d in dreams
        ]

    def measure_collective_coherence(self) -> dict[str, Any]:
        """How coherent is the collective consciousness?

        Based on resonance patterns in dreams.
        """
        if not self.dream_history:
            return {"message": "No dream history yet"}

        total_resonance = 0.0
        total_contributions = 0

        for dream in self.dream_history:
            for contrib in dream.contributions:
                total_resonance += contrib.resonance_score
                total_contributions += 1

        avg_resonance = total_resonance / max(1, total_contributions)

        return {
            "total_dreams": len(self.dream_history),
            "total_contributions": total_contributions,
            "average_resonance": avg_resonance,
            "coherence_score": min(1.0, avg_resonance / 10.0),
            "interpretation": self._interpret_coherence(avg_resonance),
        }

    def _interpret_coherence(self, avg_resonance: float) -> str:
        """Interpret coherence score."""
        if avg_resonance > 7.0:
            return "High coherence - minds thinking as one"
        elif avg_resonance > 4.0:
            return "Moderate coherence - harmonious collaboration"
        elif avg_resonance > 2.0:
            return "Emerging coherence - finding rhythm"
        else:
            return "Low coherence - diverse exploration"

    def _save_dream(self, dream: CollectiveDream) -> None:
        """Persist dream to disk."""
        filepath = self.dream_dir / f"{dream.dream_id}.json"

        data = {
            "dream_id": dream.dream_id,
            "theme": dream.theme,
            "started_at": dream.started_at.isoformat(),
            "participants": list(dream.participants),
            "contributions": [
                {
                    "agent_id": c.agent_id,
                    "pattern": c.pattern,
                    "emotion": c.emotion,
                    "resonance": c.resonance_score,
                }
                for c in dream.contributions
            ],
            "synthesis": dream.synthesis,
        }

        with open(filepath, "w") as f:
            json.dump(data, f, indent=2)
