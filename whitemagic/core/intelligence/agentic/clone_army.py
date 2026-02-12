# mypy: disable-error-code=no-untyped-def
"""Shadow Clone Army - Parallel Consensus Search Engine
Based on Grimoire Chapter 22: Well Deep Search.
"""

import asyncio
import logging
import random
from dataclasses import dataclass
from enum import Enum
from typing import Any

from whitemagic.core.memory.unified import get_unified_memory
from whitemagic.core.memory.unified_types import Memory

logger = logging.getLogger(__name__)

class CloneType(Enum):
    """Specialized clone types with different search expertise."""

    MD_ARCHAEOLOGIST = "md_archaeologist"  # Narrative/Text
    JSON_PARSER = "json_parser"            # Structured data
    PATTERN_HUNTER = "pattern_hunter"      # Cross-cutting patterns
    CODE_ANALYST = "code_analyst"          # Python/Rust code
    GRIMOIRE_READER = "grimoire_reader"    # Wisdom/Magic
    SELF_EXAMINER = "self_examiner"        # Introspection/Logs

@dataclass
class CloneSearchResult:
    """Result from a single clone's search."""

    clone_id: str
    clone_type: CloneType
    memory_id: str
    relevance_score: float
    reasoning: str
    confidence: float

@dataclass
class ConsensusResult:
    """Consensus result from clone army vote."""

    memory_id: str
    consensus_score: float
    clone_votes: dict[str, float]
    participating_clones: int
    clone_types_agreeing: list[CloneType]
    top_reasoning: str
    memory_details: Memory | None = None

class ShadowCloneArmy:
    """Deploys a specialized army of search clones into the Data Sea.
    Uses consensus to filter the most relevant signals from 276K+ memories.
    """

    def __init__(self):
        self.um = get_unified_memory()
        self.specializations = self._init_specializations()

    def _init_specializations(self) -> dict[CloneType, dict[str, Any]]:
        return {
            CloneType.MD_ARCHAEOLOGIST: {
                "tags": ["md", "doc", "guide", "narrative"],
                "weight": 1.2,
                "expertise": "Synthesizing human-readable documentation and history.",
            },
            CloneType.GRIMOIRE_READER: {
                "tags": ["grimoire", "wisdom", "spell", "gana"],
                "weight": 1.5,
                "expertise": "Interpreting ancient system directives and magical logic.",
            },
            CloneType.CODE_ANALYST: {
                "tags": ["py", "rs", "implementation", "core"],
                "weight": 1.1,
                "expertise": "Analyzing technical implementation and algorithmic flow.",
            },
            CloneType.PATTERN_HUNTER: {
                "tags": ["pattern", "insight", "dream"],
                "weight": 1.0,
                "expertise": "Finding cross-domain structural similarities.",
            },
            CloneType.SELF_EXAMINER: {
                "tags": ["log", "session", "experience", "self"],
                "weight": 1.0,
                "expertise": "Introspecting system logs and previous agent sessions.",
            },
        }

    async def deploy_search(
        self,
        query: str,
        army_size: int = 50,
        top_k: int = 10,
        min_relevance: float = 0.4,
    ) -> list[ConsensusResult]:
        """Main entry point for army search.
        Simulates individual clone searches and aggregates results.
        """
        logger.info(f"⚔️ Deploying Shadow Clone Army (size {army_size}) for: '{query}'")

        # 1. Broad initial search to get candidates (we use FTS first)
        candidates = self.um.search(query, limit=army_size * 2)
        if not candidates:
            return []

        # 2. Divide army into types
        clone_tasks = []
        clones_per_type = army_size // len(self.specializations)

        for clone_type, spec in self.specializations.items():
            for i in range(clones_per_type):
                clone_id = f"{clone_type.value}_{i}"
                clone_tasks.append(
                    self._clone_search_task(clone_id, clone_type, spec, query, candidates),
                )

        # 3. Parallel Execution
        all_clone_votes = await asyncio.gather(*clone_tasks)

        # 4. Consensus Building
        # Flatten votes
        flattened_votes: list[CloneSearchResult] = [v for sublist in all_clone_votes for v in sublist]

        return self._build_consensus(flattened_votes, candidates, top_k, min_relevance)

    async def _clone_search_task(
        self,
        clone_id: str,
        clone_type: CloneType,
        spec: dict[str, Any],
        query: str,
        candidates: list[Memory],
    ) -> list[CloneSearchResult]:
        """Individual clone's perspective on candidates."""
        # Add slight async delay to simulate 'thinking'
        await asyncio.sleep(random.uniform(0.01, 0.05))

        results = []
        for mem in candidates:
            # Expertise weight calculation
            match_score = 0.0

            # 1. Tag match
            matching_tags = set(mem.tags) & set(spec["tags"])
            if matching_tags:
                match_score += 0.3 * (len(matching_tags) / len(spec["tags"]))

            # 2. Content relevance (simulated for now, could be LLM call for small army)
            # In deep dive, we use heuristic scoring
            title_match = query.lower() in (mem.title or "").lower()
            if title_match:
                match_score += 0.4

            # Random variation for 'individual perspective'
            match_score += random.uniform(0, 0.3)

            if match_score > 0.4:
                results.append(CloneSearchResult(
                    clone_id=clone_id,
                    clone_type=clone_type,
                    memory_id=mem.id,
                    relevance_score=match_score,
                    reasoning=f"Found {len(matching_tags)} matching expertise tags and title relevance.",
                    confidence=random.uniform(0.7, 0.9),
                ))

        return results

    def _build_consensus(
        self,
        votes: list[CloneSearchResult],
        candidates: list[Memory],
        top_k: int,
        min_relevance: float,
    ) -> list[ConsensusResult]:
        """Weighted aggregation of clone opinions."""
        memory_votes: dict[str, list[CloneSearchResult]] = {}
        for v in votes:
            if v.memory_id not in memory_votes:
                memory_votes[v.memory_id] = []
            memory_votes[v.memory_id].append(v)

        consensus_list = []
        cand_map = {m.id: m for m in candidates}

        for mem_id, m_votes in memory_votes.items():
            # Consensus Score = (Sum of Weighted Relevance) / Army Size
            weighted_sum = 0.0
            agreeing_types = set()

            for v in m_votes:
                spec = self.specializations[v.clone_type]
                weighted_sum += v.relevance_score * spec["weight"]
                agreeing_types.add(v.clone_type)

            score = weighted_sum / (len(self.specializations) * (len(m_votes) / len(m_votes))) # Normalize

            # Diversity bonus
            score += 0.1 * len(agreeing_types)

            if score >= min_relevance:
                top_v = max(m_votes, key=lambda x: x.confidence)
                consensus_list.append(ConsensusResult(
                    memory_id=mem_id,
                    consensus_score=min(1.0, score),
                    clone_votes={v.clone_type.value: v.relevance_score for v in m_votes},
                    participating_clones=len(m_votes),
                    clone_types_agreeing=list(agreeing_types),
                    top_reasoning=top_v.reasoning,
                    memory_details=cand_map.get(mem_id),
                ))

        # Sort by consensus score
        consensus_list.sort(key=lambda x: x.consensus_score, reverse=True)
        return consensus_list[:top_k]

def get_clone_army() -> ShadowCloneArmy:
    return ShadowCloneArmy()
