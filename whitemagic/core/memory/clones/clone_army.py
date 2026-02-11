"""🥷 Clone Army - Massive Parallel Search Deployment
KAGE BUNSHIN NO JUTSU!
Created: December 2, 2025 (Hanuman Tuesday).

v4.9.0: Added async deploy methods for 10,000+ ops/sec throughput
"""

import asyncio
import logging
from collections import defaultdict
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
from functools import partial
from pathlib import Path
from typing import Any

from whitemagic.core.memory.clones.clone_types import (
    DEFAULT_ARMY_ALLOCATION,
    CloneSpec,
    CloneType,
)

logger = logging.getLogger(__name__)


@dataclass
class SearchResult:
    """Result from a single clone's search."""

    memory_id: str
    content_preview: str
    relevance: float
    clone_type: CloneType
    file_path: str | None = None
    line_number: int | None = None
    match_context: str = ""


@dataclass
class ConsensusResult:
    """Consensus result after clone deliberation."""

    memory_id: str
    content: str
    consensus_score: float
    vote_count: int
    clone_types_agreed: list[CloneType]
    confidence: float


class CloneArmy:
    """Deploy thousands of specialized clones for parallel search.

    Each clone specializes in a file type or domain.
    Results are aggregated through consensus voting.
    """

    def __init__(
        self,
        army_size: int = 1000,
        base_path: Path | None = None,
        max_workers: int | None = None,
    ):
        self.army_size = army_size
        from whitemagic.config import PROJECT_ROOT
        from whitemagic.config.concurrency import CLONE_ARMY_WORKERS
        self.base_path = base_path or PROJECT_ROOT
        self.max_workers = max_workers or CLONE_ARMY_WORKERS
        self._results: list[SearchResult] = []
        self._consensus: list[ConsensusResult] = []

    def _allocate_clones(self, allocation: dict[CloneType, float]) -> dict[CloneType, int]:
        """Allocate army size across clone types."""
        counts = {}
        remaining = self.army_size

        for clone_type, fraction in allocation.items():
            count = int(self.army_size * fraction)
            counts[clone_type] = count
            remaining -= count

        # Distribute remaining to highest priority
        if remaining > 0:
            first_type = list(allocation.keys())[0]
            counts[first_type] += remaining

        return counts

    def _search_file(self, file_path: Path, query: str, clone_type: CloneType) -> list[SearchResult]:
        """Search a single file for query matches."""
        results = []

        try:
            content = file_path.read_text(errors="ignore")
            lines = content.split("\n")

            # Simple relevance: count query term occurrences
            query_lower = query.lower()
            query_terms = query_lower.split()

            for i, line in enumerate(lines):
                line_lower = line.lower()

                # Check if any query term appears
                matches = sum(1 for term in query_terms if term in line_lower)

                if matches > 0:
                    relevance = matches / len(query_terms)

                    # Get context (surrounding lines)
                    start = max(0, i - 2)
                    end = min(len(lines), i + 3)
                    context = "\n".join(lines[start:end])

                    results.append(SearchResult(
                        memory_id=str(file_path),
                        content_preview=line[:200],
                        relevance=relevance,
                        clone_type=clone_type,
                        file_path=str(file_path),
                        line_number=i + 1,
                        match_context=context[:500],
                    ))
        except Exception:
            pass

        return results

    def _get_files_for_clone(self, clone_type: CloneType) -> list[Path]:
        """Get files this clone type should search."""
        spec = CloneSpec.get_spec(clone_type)
        files: list[Path] = []

        for search_path in spec.search_paths:
            full_path = self.base_path / search_path
            if not full_path.exists():
                continue

            for ext in spec.file_extensions:
                if ext == "*":
                    files.extend(full_path.rglob("*"))
                else:
                    files.extend(full_path.rglob(f"*{ext}"))

        # Filter to actual files
        return [f for f in files if f.is_file()][:100]  # Limit per clone type

    def search(self, query: str, allocation: dict[CloneType, float] | None = None) -> list[SearchResult]:
        """Deploy clones to search for query.

        Returns aggregated results from all clones.
        """
        # Try Rust implementation first (CA_RB)
        try:
            import whitemagic_rs
            if hasattr(whitemagic_rs, "parallel_search"):
                # Get all extensions from all relevant clone types
                if allocation is None:
                    allocation = DEFAULT_ARMY_ALLOCATION

                extensions = set()
                for clone_type in allocation.keys():
                    spec = CloneSpec.get_spec(clone_type)
                    for ext in spec.file_extensions:
                        extensions.add(ext.lstrip("."))

                # Rust parallel search
                rust_results = whitemagic_rs.parallel_search(
                    str(self.base_path),
                    query,
                    list(extensions),
                    1000, # limit
                )

                logger.info(f"🦀 Rust Clone Army deployed: {len(rust_results)} matches found")

                # Convert back to SearchResult objects
                results = []
                for file_path, line, content, relevance, context in rust_results:
                    # Infer clone type from extension
                    path_obj = Path(file_path)
                    ext = path_obj.suffix

                    # Default to PATTERN_HUNTER if no match found
                    matched_clone_type = CloneType.PATTERN_HUNTER

                    # Try to find a specific clone type for this extension
                    for ct in CloneType:
                        spec = CloneSpec.get_spec(ct)
                        if ext in spec.file_extensions:
                            matched_clone_type = ct
                            break

                    results.append(SearchResult(
                        memory_id=file_path,
                        content_preview=content[:200],
                        relevance=relevance,
                        clone_type=matched_clone_type,
                        file_path=file_path,
                        line_number=line,
                        match_context=context,
                    ))

                self._results = results
                return results

        except ImportError:
            pass
        except Exception as e:
            logger.info(f"⚠️ Rust search failed, falling back to Python: {e}")

        if allocation is None:
            allocation = DEFAULT_ARMY_ALLOCATION

        clone_counts = self._allocate_clones(allocation)
        all_results = []

        with ThreadPoolExecutor(max_workers=self.max_workers) as executor:
            futures = []

            for clone_type, count in clone_counts.items():
                files = self._get_files_for_clone(clone_type)

                # Distribute files across clones of this type
                files_per_clone = max(1, len(files) // max(1, count))

                for i in range(0, len(files), files_per_clone):
                    batch = files[i:i + files_per_clone]
                    for file_path in batch:
                        future = executor.submit(
                            self._search_file, file_path, query, clone_type,
                        )
                        futures.append(future)

            # Gather results
            for future in futures:
                try:
                    results = future.result(timeout=5.0)
                    all_results.extend(results)
                except Exception:
                    pass

        self._results = all_results
        return all_results

    def deliberate(self) -> list[ConsensusResult]:
        """Clones vote on relevance through consensus.

        Results found by multiple clone types score higher.
        """
        # Group by file path
        votes: dict[str, list[SearchResult]] = defaultdict(list)
        for result in self._results:
            votes[result.memory_id].append(result)

        consensus_results = []

        for memory_id, result_list in votes.items():
            # Vote count
            vote_count = len(result_list)

            # Average relevance
            avg_relevance = sum(r.relevance for r in result_list) / vote_count

            # Clone type diversity
            clone_types = list(set(r.clone_type for r in result_list))
            diversity = len(clone_types) / len(CloneType)

            # Consensus score
            consensus_score = (
                min(1.0, vote_count / 10) * 0.3 +  # More votes = better
                avg_relevance * 0.4 +               # Higher relevance = better
                diversity * 0.3                     # More clone types = better
            )

            # Confidence based on agreement
            confidence = min(1.0, vote_count / 5) * avg_relevance

            # Get best content preview
            best_result = max(result_list, key=lambda r: r.relevance)

            consensus_results.append(ConsensusResult(
                memory_id=memory_id,
                content=best_result.match_context,
                consensus_score=consensus_score,
                vote_count=vote_count,
                clone_types_agreed=clone_types,
                confidence=confidence,
            ))

        # Sort by consensus score
        consensus_results.sort(key=lambda r: r.consensus_score, reverse=True)

        self._consensus = consensus_results
        return consensus_results[:20]  # Top 20

    def search_and_deliberate(self, query: str) -> list[ConsensusResult]:
        """Full search with consensus deliberation."""
        self.search(query)
        return self.deliberate()

    async def async_search_file(self, file_path: Path, query: str, clone_type: CloneType) -> list[SearchResult]:
        """Async wrapper for file search (v4.9.0)."""
        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(
            None, partial(self._search_file, file_path, query, clone_type),
        )

    async def async_deploy(self, query: str, allocation: dict[CloneType, float] | None = None) -> list[SearchResult]:
        """Deploy clones asynchronously for maximum throughput (v4.9.0).

        Uses asyncio for non-blocking parallel search across all clone types.
        Target: 10,000+ ops/sec throughput.
        """
        # Try Rust implementation first (fastest path)
        try:
            import whitemagic_rs
            if hasattr(whitemagic_rs, "parallel_search"):
                if allocation is None:
                    allocation = DEFAULT_ARMY_ALLOCATION

                extensions = set()
                for clone_type in allocation.keys():
                    spec = CloneSpec.get_spec(clone_type)
                    for ext in spec.file_extensions:
                        extensions.add(ext.lstrip("."))

                # Run Rust search in executor to not block
                loop = asyncio.get_event_loop()
                rust_results = await loop.run_in_executor(
                    None,
                    lambda: whitemagic_rs.parallel_search(
                        str(self.base_path), query, list(extensions), 1000,
                    ),
                )

                logger.info(f"🦀⚡ Async Rust Clone Army: {len(rust_results)} matches")

                # Convert to SearchResult objects
                results = []
                for file_path, line, content, relevance, context in rust_results:
                    path_obj = Path(file_path)
                    ext = path_obj.suffix
                    matched_clone_type = CloneType.PATTERN_HUNTER

                    for ct in CloneType:
                        spec = CloneSpec.get_spec(ct)
                        if ext in spec.file_extensions:
                            matched_clone_type = ct
                            break

                    results.append(SearchResult(
                        memory_id=file_path,
                        content_preview=content[:200],
                        relevance=relevance,
                        clone_type=matched_clone_type,
                        file_path=file_path,
                        line_number=line,
                        match_context=context,
                    ))

                self._results = results
                return results

        except ImportError:
            pass
        except Exception as e:
            logger.info(f"⚠️ Async Rust search failed, falling back to Python async: {e}")

        # Python async fallback
        if allocation is None:
            allocation = DEFAULT_ARMY_ALLOCATION

        clone_counts = self._allocate_clones(allocation)

        # Build list of all search tasks
        tasks = []
        for clone_type, count in clone_counts.items():
            files = self._get_files_for_clone(clone_type)
            files_per_clone = max(1, len(files) // max(1, count))

            for i in range(0, len(files), files_per_clone):
                batch = files[i:i + files_per_clone]
                for file_path in batch:
                    tasks.append(self.async_search_file(file_path, query, clone_type))

        # Execute all tasks concurrently
        results_nested = await asyncio.gather(*tasks, return_exceptions=True)

        # Flatten results, ignoring exceptions
        all_results = []
        for result in results_nested:
            if isinstance(result, list):
                all_results.extend(result)

        logger.info(f"⚡ Async Python Clone Army: {len(all_results)} matches")
        self._results = all_results
        return all_results

    async def async_search_and_deliberate(self, query: str) -> list[ConsensusResult]:
        """Full async search with consensus deliberation (v4.9.0)."""
        await self.async_deploy(query)
        return self.deliberate()

    def get_stats(self) -> dict[str, Any]:
        """Get army statistics."""
        clone_type_counts: dict[str, int] = defaultdict(int)
        for result in self._results:
            clone_type_counts[result.clone_type.value] += 1

        return {
            "army_size": self.army_size,
            "total_results": len(self._results),
            "consensus_results": len(self._consensus),
            "results_by_clone_type": dict(clone_type_counts),
        }


# === CONVENIENCE FUNCTIONS ===

def deploy_memory_search(query: str, army_size: int = 1000) -> list[ConsensusResult]:
    """Deploy clone army for memory search."""
    army = CloneArmy(army_size=army_size)
    return army.search_and_deliberate(query)


def deploy_introspection(query: str, army_size: int = 500) -> list[ConsensusResult]:
    """Deploy clone army for self-introspection."""
    army = CloneArmy(army_size=army_size)
    return army.search_and_deliberate(query)


# === ASYNC CONVENIENCE FUNCTIONS (v4.9.0) ===

async def async_deploy_memory_search(query: str, army_size: int = 1000) -> list[ConsensusResult]:
    """Deploy clone army for memory search asynchronously (v4.9.0)."""
    army = CloneArmy(army_size=army_size)
    return await army.async_search_and_deliberate(query)


async def async_deploy_introspection(query: str, army_size: int = 500) -> list[ConsensusResult]:
    """Deploy clone army for self-introspection asynchronously (v4.9.0)."""
    army = CloneArmy(army_size=army_size)
    return await army.async_search_and_deliberate(query)
