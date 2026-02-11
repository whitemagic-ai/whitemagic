"""Bridge Synthesizer — Insight Generation from Graph Bridge Nodes.
================================================================
During the dream cycle's SERENDIPITY phase, the bridge synthesizer
takes bridge nodes (memories connecting otherwise disconnected communities)
and generates hypotheses about why these connections exist.

Optionally uses Ollama LLM for hypothesis generation, with a structured
template fallback when LLM is unavailable.

Usage:
    from whitemagic.core.memory.bridge_synthesizer import get_bridge_synthesizer
    synth = get_bridge_synthesizer()
    insights = synth.synthesize_from_bridges(bridge_nodes, graph_engine)
"""

from __future__ import annotations

import logging
import threading
import time
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

logger = logging.getLogger(__name__)


@dataclass
class BridgeInsight:
    """An insight synthesized from a bridge node."""

    bridge_node_id: str
    bridging_centrality: float
    community_a_tags: list[str]
    community_b_tags: list[str]
    hypothesis: str
    confidence: float  # 0.0-1.0
    source: str  # 'llm' or 'template'
    timestamp: str = field(default_factory=lambda: datetime.now().isoformat())

    def to_dict(self) -> dict[str, Any]:
        return {
            "bridge_node_id": self.bridge_node_id,
            "bridging_centrality": round(self.bridging_centrality, 6),
            "community_a_tags": self.community_a_tags[:5],
            "community_b_tags": self.community_b_tags[:5],
            "hypothesis": self.hypothesis,
            "confidence": round(self.confidence, 3),
            "source": self.source,
            "timestamp": self.timestamp,
        }


class BridgeSynthesizer:
    """Generates insights from graph bridge nodes.

    For each bridge node:
    1. Identify the communities it connects
    2. Extract theme tags from each community
    3. Generate a hypothesis about the connection
    4. Optionally persist as a LONG_TERM memory
    """

    def __init__(self, persist_insights: bool = True) -> None:
        self._persist = persist_insights
        self._lock = threading.Lock()
        self._total_insights = 0

    def synthesize_from_bridges(
        self,
        bridge_nodes: list[dict[str, Any]],
        top_n: int = 5,
    ) -> list[BridgeInsight]:
        """Generate insights from bridge node data.

        Args:
            bridge_nodes: Output from graph_engine.find_bridge_nodes().
            top_n: Max insights to generate.

        Returns:
            List of BridgeInsight objects.
        """
        start = time.perf_counter()
        insights: list[BridgeInsight] = []

        try:
            from whitemagic.core.memory.graph_engine import get_graph_engine
            engine = get_graph_engine()
        except Exception:
            return insights

        # Get communities for context
        try:
            communities = engine.detect_communities()
        except Exception:
            communities = []

        # Build node → community mapping
        node_to_community: dict[str, int] = {}
        community_tags: dict[int, list[str]] = {}
        for comm in communities:
            for mid in comm.member_ids:
                node_to_community[mid] = comm.community_id
            community_tags[comm.community_id] = comm.theme_tags

        for bridge in bridge_nodes[:top_n]:
            node_id = bridge["node_id"]
            bc = bridge.get("bridging_centrality", 0.0)

            # Find which communities this bridge connects
            connected_communities = self._find_connected_communities(
                node_id, node_to_community, engine,
            )

            if len(connected_communities) < 2:
                continue

            comm_a, comm_b = connected_communities[0], connected_communities[1]
            tags_a = community_tags.get(comm_a, [])
            tags_b = community_tags.get(comm_b, [])

            # Generate hypothesis
            hypothesis, source, confidence = self._generate_hypothesis(
                node_id, tags_a, tags_b,
            )

            insight = BridgeInsight(
                bridge_node_id=node_id,
                bridging_centrality=bc,
                community_a_tags=tags_a,
                community_b_tags=tags_b,
                hypothesis=hypothesis,
                confidence=confidence,
                source=source,
            )
            insights.append(insight)

        # Persist as memories
        if self._persist and insights:
            self._persist_insights(insights)

        with self._lock:
            self._total_insights += len(insights)

        elapsed = (time.perf_counter() - start) * 1000
        if insights:
            logger.info(
                f"🌉 Bridge synthesis: {len(insights)} insights generated ({elapsed:.0f}ms)",
            )
        return insights

    def _find_connected_communities(
        self,
        node_id: str,
        node_to_community: dict[str, int],
        engine: Any,
    ) -> list[int]:
        """Find which communities a bridge node connects."""
        if not engine.available or engine.graph is None:
            return []

        try:
            UG = engine.graph.to_undirected()
            if node_id not in UG:
                return []

            neighbor_communities: set[int] = set()
            for neighbor in UG.neighbors(node_id):
                comm = node_to_community.get(neighbor)
                if comm is not None:
                    neighbor_communities.add(comm)

            return sorted(neighbor_communities)
        except Exception:
            return []

    def _generate_hypothesis(
        self,
        node_id: str,
        tags_a: list[str],
        tags_b: list[str],
    ) -> tuple[str, str, float]:
        """Generate a hypothesis about why this bridge exists.

        Returns: (hypothesis_text, source, confidence)
        """
        # Try LLM first
        hypothesis = self._try_llm_hypothesis(node_id, tags_a, tags_b)
        if hypothesis:
            return hypothesis, "llm", 0.7

        # Structured template fallback
        if tags_a and tags_b:
            hypothesis = (
                f"Memory {node_id[:8]} bridges concepts from "
                f"[{', '.join(tags_a[:3])}] and [{', '.join(tags_b[:3])}]. "
                f"This suggests a latent connection between these domains "
                f"that may yield insights when explored together."
            )
        else:
            hypothesis = (
                f"Memory {node_id[:8]} connects two otherwise isolated "
                f"memory communities. Further investigation may reveal "
                f"cross-domain patterns."
            )
        return hypothesis, "template", 0.4

    def _try_llm_hypothesis(
        self,
        node_id: str,
        tags_a: list[str],
        tags_b: list[str],
    ) -> str | None:
        """Try to generate hypothesis via Ollama LLM."""
        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
            mem = um.backend.recall(node_id)
            if not mem:
                return None

            content_preview = str(mem.content)[:300]
            prompt = (
                f"A memory titled '{mem.title or 'untitled'}' bridges two knowledge communities.\n"
                f"Community A themes: {', '.join(tags_a[:5])}\n"
                f"Community B themes: {', '.join(tags_b[:5])}\n"
                f"Memory content: {content_preview}\n\n"
                f"In one sentence, hypothesize why this memory connects these two domains:"
            )

            import subprocess
            result = subprocess.run(
                ["ollama", "run", "llama3.2:1b", prompt],
                capture_output=True, text=True, timeout=15,
            )
            if result.returncode == 0 and result.stdout.strip():
                return result.stdout.strip()[:500]
        except Exception:
            pass
        return None

    def _persist_insights(self, insights: list[BridgeInsight]) -> int:
        """Persist bridge insights as LONG_TERM memories."""
        persisted = 0
        try:
            from whitemagic.core.memory.unified import get_unified_memory
            from whitemagic.core.memory.unified_types import MemoryType
            um = get_unified_memory()

            for insight in insights:
                try:
                    um.store(
                        content=insight.hypothesis,
                        memory_type=MemoryType.LONG_TERM,
                        tags={"bridge_insight", "dream_synthesis", "v14"},
                        importance=0.7 + (insight.confidence * 0.2),
                        title=f"Bridge Insight: {insight.bridge_node_id[:8]}",
                        metadata={
                            "source": "bridge_synthesizer",
                            "bridge_node": insight.bridge_node_id,
                            "bridging_centrality": insight.bridging_centrality,
                            "community_a_tags": insight.community_a_tags,
                            "community_b_tags": insight.community_b_tags,
                        },
                    )
                    persisted += 1
                except Exception:
                    pass
        except Exception:
            pass
        return persisted

    def get_stats(self) -> dict[str, Any]:
        with self._lock:
            return {
                "total_insights": self._total_insights,
                "persist_enabled": self._persist,
            }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_synthesizer: BridgeSynthesizer | None = None
_synth_lock = threading.Lock()


def get_bridge_synthesizer(**kwargs: Any) -> BridgeSynthesizer:
    """Get the global BridgeSynthesizer singleton."""
    global _synthesizer
    if _synthesizer is None:
        with _synth_lock:
            if _synthesizer is None:
                _synthesizer = BridgeSynthesizer(**kwargs)
    return _synthesizer
