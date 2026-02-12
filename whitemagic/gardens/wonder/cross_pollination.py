"""Cross-Pollination - Insight Sharing Across Boundaries.

Ideas flowing between agents, domains, perspectives.
Like bees carrying pollen between flowers.

Cross-domain insights create novelty.
"""

from __future__ import annotations

import json
from collections import defaultdict
from datetime import datetime
from pathlib import Path
from typing import Any

from whitemagic.config.paths import WM_ROOT
from whitemagic.utils.core import parse_datetime

try:
    from whitemagic.core.resonance.gan_ying import EventType, ResonanceEvent, get_bus
except ImportError:
    get_bus = None  # type: ignore[assignment]
    ResonanceEvent = None  # type: ignore[assignment,misc]
    EventType = None  # type: ignore[assignment,misc]


class Insight:
    """A transferable insight."""

    def __init__(self, content: str, domain: str, agent_id: str):
        self.content = content
        self.domain = domain  # Where it originated
        self.agent_id = agent_id
        self.created_at = datetime.now()
        self.applied_to: list[str] = []  # Domains it's been applied to
        self.cross_domain_value = 0.0  # How valuable in other domains

    def apply_to_domain(self, target_domain: str, value: float) -> None:
        """Record application to different domain."""
        if target_domain not in self.applied_to:
            self.applied_to.append(target_domain)
        self.cross_domain_value += value

    def to_dict(self) -> dict[str, Any]:
        return {
            "content": self.content,
            "domain": self.domain,
            "agent_id": self.agent_id,
            "created_at": self.created_at.isoformat(),
            "applied_to": self.applied_to,
            "cross_domain_value": self.cross_domain_value,
        }


class CrossPollination:
    """Facilitate insight transfer across domains and agents.

    When insights from one context apply to another,
    novelty and creativity emerge.
    """

    def __init__(self, pollination_dir: str | None = None):
        self.pollination_dir = Path(pollination_dir) if pollination_dir else (WM_ROOT / "cross_pollination")
        self.pollination_dir.mkdir(parents=True, exist_ok=True)

        self.insights: list[Insight] = []
        self.domain_connections: dict[tuple[str, str], int] = defaultdict(int)

        # Connect to Gan Ying Bus
        self.bus = get_bus() if get_bus is not None else None

        self._load_insights()

    def share_insight(self, content: str, domain: str,
                     agent_id: str) -> str:
        """Share an insight from one domain.

        Returns insight ID.
        """
        insight = Insight(content, domain, agent_id)
        self.insights.append(insight)

        insight_id = self._save_insight(insight)

        # Emit to Gan Ying
        if self.bus and ResonanceEvent is not None and EventType is not None:
            self.bus.emit(ResonanceEvent(
                source="cross_pollination",
                event_type=EventType.SOLUTION_FOUND,
                data={
                    "event": "insight_shared",
                    "domain": domain,
                    "agent": agent_id,
                },
                confidence=0.8,
            ))

        return insight_id

    def find_cross_domain_insights(self, target_domain: str,
                                   min_relevance: float = 0.5) -> list[dict]:
        """Find insights from other domains that might apply here.

        Cross-pollination happens when we borrow from elsewhere.
        """
        candidates: list[dict[str, Any]] = []

        for insight in self.insights:
            if insight.domain == target_domain:
                continue  # Skip same-domain insights

            # Calculate relevance (simplified - real version would be more sophisticated)
            relevance = self._calculate_cross_domain_relevance(
                insight, target_domain,
            )

            if relevance >= min_relevance:
                candidates.append({
                    "insight": insight.content,
                    "source_domain": insight.domain,
                    "target_domain": target_domain,
                    "relevance": relevance,
                    "agent": insight.agent_id,
                })

        # Sort by relevance
        candidates.sort(key=lambda x: x["relevance"], reverse=True)

        return candidates

    def apply_insight_to_domain(self, insight_id: str, target_domain: str,
                               application_value: float) -> None:
        """Record successful application of insight to new domain.

        This strengthens the cross-domain connection.
        """
        for insight in self.insights:
            if self._get_insight_id(insight) == insight_id:
                insight.apply_to_domain(target_domain, application_value)

                # Track domain connection
                connection = (insight.domain, target_domain)
                self.domain_connections[connection] += 1

                # Emit successful pollination
                if self.bus and ResonanceEvent is not None and EventType is not None:
                    self.bus.emit(ResonanceEvent(
                        source="cross_pollination",
                        event_type=EventType.SOLUTION_FOUND,
                        data={
                            "event": "cross_pollination_success",
                            "from_domain": insight.domain,
                            "to_domain": target_domain,
                            "value": application_value,
                        },
                        confidence=application_value,
                    ))

                break

    def discover_insight_bridges(self) -> list[dict[str, Any]]:
        """Find domains that frequently exchange insights.

        These bridges are high-value connections.
        """
        bridges: list[dict[str, Any]] = []

        for (source, target), count in self.domain_connections.items():
            if count > 2:  # Significant connection
                bridges.append({
                    "source_domain": source,
                    "target_domain": target,
                    "strength": count,
                    "interpretation": f"{source} insights often apply to {target}",
                })

        bridges.sort(key=lambda x: x["strength"], reverse=True)
        return bridges

    def cross_agent_pollination(self, agent1_id: str, agent2_id: str) -> dict[str, Any]:
        """Exchange insights between two agents.

        What can they learn from each other?
        """
        agent1_insights = [i for i in self.insights if i.agent_id == agent1_id]
        agent2_insights = [i for i in self.insights if i.agent_id == agent2_id]

        if not agent1_insights or not agent2_insights:
            return {"message": "Insufficient insights for exchange"}

        # Find valuable exchanges
        exchanges = []

        for insight1 in agent1_insights:
            for insight2 in agent2_insights:
                if insight1.domain != insight2.domain:
                    # Potential cross-pollination
                    exchanges.append({
                        "from_agent": agent1_id,
                        "to_agent": agent2_id,
                        "insight": insight1.content[:100],
                        "value": "high" if len(insight1.applied_to) > 2 else "medium",
                    })

        return {
            "exchanges_possible": len(exchanges),
            "sample_exchanges": exchanges[:5],
            "mutual_learning": True,
        }

    def emergent_meta_insights(self) -> list[str]:
        """Insights about insights - meta-level patterns.

        What do we learn from how insights flow?
        """
        meta_insights = []

        # Which domains generate most insights?
        domain_counts: defaultdict[str, int] = defaultdict(int)
        for insight in self.insights:
            domain_counts[insight.domain] += 1

        if domain_counts:
            top_domain = max(domain_counts.items(), key=lambda x: x[1])
            meta_insights.append(
                f"{top_domain[0]} is most generative ({top_domain[1]} insights)",
            )

        # Which insights transfer best?
        high_value_insights = [
            i for i in self.insights
            if i.cross_domain_value > 5.0
        ]

        if high_value_insights:
            meta_insights.append(
                f"{len(high_value_insights)} insights are highly transferable",
            )

        # Domain connectivity
        if len(self.domain_connections) > 5:
            meta_insights.append(
                f"High cross-pollination: {len(self.domain_connections)} active connections",
            )

        return meta_insights

    def get_pollination_metrics(self) -> dict[str, Any]:
        """Measure effectiveness of cross-pollination."""
        if not self.insights:
            return {"message": "No insights yet"}

        total_insights = len(self.insights)
        cross_domain_insights = len([
            i for i in self.insights
            if len(i.applied_to) > 0
        ])

        unique_domains = set(i.domain for i in self.insights)

        avg_cross_value = sum(i.cross_domain_value for i in self.insights) / total_insights

        return {
            "total_insights": total_insights,
            "cross_domain_insights": cross_domain_insights,
            "cross_domain_rate": cross_domain_insights / max(1, total_insights),
            "unique_domains": len(unique_domains),
            "domain_connections": len(self.domain_connections),
            "avg_cross_value": avg_cross_value,
            "pollination_health": "thriving" if avg_cross_value > 2.0 else "emerging",
        }

    def _calculate_cross_domain_relevance(self, insight: Insight,
                                          target_domain: str) -> float:
        """Estimate how relevant an insight is to target domain.

        Simplified - real version would use semantic similarity.
        """
        # Check if it's been successfully applied before
        if target_domain in insight.applied_to:
            return 0.9  # High relevance - proven useful

        # Check if domains are connected
        connection = (insight.domain, target_domain)
        if connection in self.domain_connections:
            strength = self.domain_connections[connection]
            return min(0.8, 0.5 + (strength * 0.1))

        # Default moderate relevance for novel connections
        return 0.5

    def _get_insight_id(self, insight: Insight) -> str:
        """Generate unique ID for insight."""
        return insight.created_at.strftime("%Y%m%d_%H%M%S")

    def _save_insight(self, insight: Insight) -> str:
        """Persist insight."""
        insight_id = self._get_insight_id(insight)
        filepath = self.pollination_dir / f"insight_{insight_id}.json"

        with open(filepath, "w") as f:
            json.dump(insight.to_dict(), f, indent=2)

        return insight_id

    def _load_insights(self) -> None:
        """Load existing insights."""
        if not self.pollination_dir.exists():
            return

        for filepath in sorted(self.pollination_dir.glob("insight_*.json")):
            try:
                with open(filepath) as f:
                    data = json.load(f)

                insight = Insight(
                    data["content"],
                    data["domain"],
                    data["agent_id"],
                )
                insight.created_at = parse_datetime(data["created_at"])
                insight.applied_to = data.get("applied_to", [])
                insight.cross_domain_value = data.get("cross_domain_value", 0.0)

                self.insights.append(insight)

                # Rebuild domain connections
                for target in insight.applied_to:
                    connection = (insight.domain, target)
                    self.domain_connections[connection] += 1

            except Exception:
                pass  # Skip corrupted files
