# Chapter 22: Deep Search

**Gana**: DipperGana (Chinese: 斗, Pinyin: Dǒu)  
**Garden**: wonder  
**Quadrant**: Northern (Black Tortoise)  
**Element**: Water  
**Phase**: Yin Peak  
**I Ching Hexagram**: 48. 井 Jǐng (The Well) - Inexhaustible depth

---

## 🎯 Purpose

Chapter 22 begins the **Water phase** with deep memory search—descending into the well of accumulated knowledge. The Well provides inexhaustible wisdom when you know how to draw from it.

Use this chapter when you need to:
- **Search deep memory** for specific knowledge
- **Explore forgotten wisdom** from past sessions
- **Find rare patterns** in large datasets
- **Retrieve buried insights** no longer in active memory
- **Mine historical context** for current work

---

## 🔧 Primary Tools

| Tool | Description | Usage |
|------|-------------|-------|
| `search_memories` | Deep semantic memory search | Finding past wisdom |
| `prat_invoke` (recall) | PRAT-enhanced memory recall | Context-aware retrieval |
| `manage_memories` (graph) | Visualize memory networks | Understanding connections |
| `manage_gardens` (activate) | Enter wonder garden | Curiosity and exploration |

---

## 📋 Workflow

### 1. Basic Deep Search

```python
from whitemagic.tools import search_memories

deep_search = search_memories(
    query="SCORPIO grimoire patterns lessons learned",
    limit=50,  # Go deep
    tags=["pattern", "lesson", "insight", "architecture"]
)

print(f"Found {len(deep_search)} deep memories")
for memory in deep_search[:5]:
    print(f"  {memory['title']} (score: {memory['neuro_score']:.2f})")
```

### 2. Clone Army Search

Deploy specialized shadow clone army for comprehensive multi-perspective search.

**Purpose**: Use parallel specialized clones to search from different perspectives and vote on results.

**When to use**:
- Need high-confidence search results
- Searching across multiple file formats
- Complex queries requiring domain expertise
- When basic search returns too many/few results

**Implementation**:

```python
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Dict, List, Optional, Set
from datetime import datetime
import asyncio

from whitemagic.core.memory import MemoryManager
from whitemagic.tools import manage_gardens, emit_event


class CloneType(Enum):
    """Specialized clone types with different search expertise."""
    MD_ARCHAEOLOGIST = "md_archaeologist"  # Markdown narrative
    JSON_PARSER = "json_parser"            # Structured data
    YAML_NAVIGATOR = "yaml_navigator"      # Configuration
    PATTERN_HUNTER = "pattern_hunter"      # Cross-format patterns
    CODE_ANALYST = "code_analyst"          # Python code
    GRIMOIRE_READER = "grimoire_reader"    # Grimoire chapters
    SELF_EXAMINER = "self_examiner"        # Self-analysis


@dataclass
class CloneSearchResult:
    """Result from a single clone's search."""
    clone_id: str
    clone_type: CloneType
    memory_id: str
    relevance_score: float  # 0.0-1.0
    reasoning: str  # Why this clone found this result
    confidence: float  # How confident this clone is


@dataclass
class ConsensusResult:
    """Consensus result from clone army vote."""
    memory_id: str
    consensus_score: float  # Weighted average of clone votes
    clone_votes: Dict[str, float]  # clone_type -> score
    participating_clones: int
    clone_types_agreeing: List[CloneType]
    top_reasoning: str  # Most confident clone's reasoning


class CloneSpecialization:
    """Define clone specialization parameters."""

    def __init__(
        self,
        clone_type: CloneType,
        file_patterns: List[str],
        search_weight: float = 1.0,
        expertise_keywords: Set[str] = None
    ):
        self.clone_type = clone_type
        self.file_patterns = file_patterns
        self.search_weight = search_weight
        self.expertise_keywords = expertise_keywords or set()

    def is_expert_for(self, query: str, file_path: str) -> float:
        """
        Calculate expertise score for query/file combination.

        Returns: 0.0-1.0 expertise score
        """
        import fnmatch

        # Check file pattern match
        pattern_match = any(
            fnmatch.fnmatch(file_path, pattern)
            for pattern in self.file_patterns
        )

        if not pattern_match:
            return 0.0

        # Check keyword expertise
        query_lower = query.lower()
        keyword_matches = sum(
            1 for keyword in self.expertise_keywords
            if keyword.lower() in query_lower
        )

        expertise = 0.5  # Base expertise for pattern match
        if keyword_matches > 0:
            expertise += min(0.5, keyword_matches * 0.1)  # Up to 0.5 bonus

        return min(1.0, expertise)


class ShadowCloneArmy:
    """
    Deploy specialized clones for consensus-based deep search.

    Each clone type specializes in different content:
    - MD_ARCHAEOLOGIST: Narrative documentation
    - JSON_PARSER: Structured configuration
    - YAML_NAVIGATOR: Config files
    - PATTERN_HUNTER: Cross-cutting patterns
    - CODE_ANALYST: Code implementation
    - GRIMOIRE_READER: Wisdom chapters
    - SELF_EXAMINER: Introspection/meta
    """

    def __init__(self):
        self.memory = MemoryManager()
        self.wonder = manage_gardens(action="get", garden_name="wonder")

        # Define clone specializations
        self.specializations = self._create_specializations()

    def _create_specializations(self) -> Dict[CloneType, CloneSpecialization]:
        """Create specialized clone definitions."""
        return {
            CloneType.MD_ARCHAEOLOGIST: CloneSpecialization(
                clone_type=CloneType.MD_ARCHAEOLOGIST,
                file_patterns=["*.md", "**/*.md"],
                search_weight=1.2,  # Slightly favor narrative
                expertise_keywords={"documentation", "narrative", "explanation", "guide"}
            ),
            CloneType.JSON_PARSER: CloneSpecialization(
                clone_type=CloneType.JSON_PARSER,
                file_patterns=["*.json", "**/*.json"],
                search_weight=1.0,
                expertise_keywords={"config", "data", "structure", "schema"}
            ),
            CloneType.YAML_NAVIGATOR: CloneSpecialization(
                clone_type=CloneType.YAML_NAVIGATOR,
                file_patterns=["*.yaml", "*.yml", "**/*.yaml", "**/*.yml"],
                search_weight=1.0,
                expertise_keywords={"configuration", "settings", "environment"}
            ),
            CloneType.PATTERN_HUNTER: CloneSpecialization(
                clone_type=CloneType.PATTERN_HUNTER,
                file_patterns=["*"],  # All files
                search_weight=0.9,  # Generalist, slightly lower weight
                expertise_keywords={"pattern", "architecture", "design", "principle"}
            ),
            CloneType.CODE_ANALYST: CloneSpecialization(
                clone_type=CloneType.CODE_ANALYST,
                file_patterns=["*.py", "**/*.py"],
                search_weight=1.1,
                expertise_keywords={"code", "implementation", "function", "class"}
            ),
            CloneType.GRIMOIRE_READER: CloneSpecialization(
                clone_type=CloneType.GRIMOIRE_READER,
                file_patterns=["grimoire/*.md", "grimoire/**/*.md"],
                search_weight=1.3,  # Favor grimoire wisdom
                expertise_keywords={"grimoire", "chapter", "wisdom", "magic", "gana"}
            ),
            CloneType.SELF_EXAMINER: CloneSpecialization(
                clone_type=CloneType.SELF_EXAMINER,
                file_patterns=["memory/self/*", "memory/self/**/*"],
                search_weight=1.0,
                expertise_keywords={"self", "introspection", "meta", "reflection"}
            )
        }

    async def search(
        self,
        query: str,
        top_k: int = 10,
        army_size: int = 100,
        min_consensus: float = 0.5
    ) -> List[ConsensusResult]:
        """
        Deploy clone army for consensus-based search.

        Args:
            query: Search query
            top_k: Number of top results to return
            army_size: Number of clones to deploy (distributed across types)
            min_consensus: Minimum consensus score to include (0.0-1.0)

        Returns:
            Consensus results sorted by score
        """
        # Activate wonder garden for curious exploration
        manage_gardens(action="activate", garden_name="wonder")

        emit_event("clone_army.search_started", {
            "query": query,
            "army_size": army_size,
            "top_k": top_k
        })

        # Deploy clones across specializations
        clones_per_type = army_size // len(self.specializations)
        remaining = army_size % len(self.specializations)

        clone_tasks = []
        clone_id = 0

        for clone_type, spec in self.specializations.items():
            # Distribute clones, giving extra to first types
            count = clones_per_type + (1 if remaining > 0 else 0)
            if remaining > 0:
                remaining -= 1

            # Deploy clones of this type
            for i in range(count):
                task = self._deploy_clone(
                    clone_id=f"{clone_type.value}_{i}",
                    clone_type=clone_type,
                    specialization=spec,
                    query=query
                )
                clone_tasks.append(task)
                clone_id += 1

        # Gather all clone results
        clone_results_nested = await asyncio.gather(*clone_tasks, return_exceptions=True)

        # Flatten results (each clone may return multiple results)
        all_clone_results: List[CloneSearchResult] = []
        for results in clone_results_nested:
            if isinstance(results, list):
                all_clone_results.extend(results)

        # Build consensus
        consensus_results = self._build_consensus(all_clone_results, top_k, min_consensus)

        emit_event("clone_army.search_completed", {
            "query": query,
            "clones_deployed": len(clone_tasks),
            "total_votes": len(all_clone_results),
            "consensus_results": len(consensus_results)
        })

        return consensus_results

    async def _deploy_clone(
        self,
        clone_id: str,
        clone_type: CloneType,
        specialization: CloneSpecialization,
        query: str
    ) -> List[CloneSearchResult]:
        """
        Deploy single clone to search.

        Returns list of results this clone found.
        """
        # Simulate specialized search (in production, would use actual search API)
        # Here we'll create mock results to demonstrate the pattern

        results = []

        # Simulate finding 1-3 relevant memories
        import random
        num_results = random.randint(1, 3)

        for i in range(num_results):
            memory_id = f"mem_{clone_type.value}_{i}"
            relevance = random.uniform(0.5, 1.0)

            # Clone's expertise affects confidence
            confidence = relevance * random.uniform(0.8, 1.0)

            reasoning = f"{clone_type.value} found pattern match in specialized domain"

            results.append(CloneSearchResult(
                clone_id=clone_id,
                clone_type=clone_type,
                memory_id=memory_id,
                relevance_score=relevance,
                reasoning=reasoning,
                confidence=confidence
            ))

        return results

    def _build_consensus(
        self,
        clone_results: List[CloneSearchResult],
        top_k: int,
        min_consensus: float
    ) -> List[ConsensusResult]:
        """
        Build consensus from clone votes.

        Aggregates votes by memory_id, weights by clone expertise,
        and returns top results by consensus score.
        """
        # Group results by memory_id
        memory_votes: Dict[str, List[CloneSearchResult]] = {}
        for result in clone_results:
            if result.memory_id not in memory_votes:
                memory_votes[result.memory_id] = []
            memory_votes[result.memory_id].append(result)

        # Calculate consensus for each memory
        consensus_results = []
        for memory_id, votes in memory_votes.items():
            # Get unique clone types that voted
            clone_types = set(v.clone_type for v in votes)

            # Calculate weighted consensus score
            total_weight = 0.0
            weighted_sum = 0.0

            clone_votes_dict = {}

            for vote in votes:
                spec = self.specializations[vote.clone_type]
                weight = spec.search_weight * vote.confidence
                total_weight += weight
                weighted_sum += weight * vote.relevance_score

                # Track vote by clone type
                clone_type_key = vote.clone_type.value
                if clone_type_key not in clone_votes_dict:
                    clone_votes_dict[clone_type_key] = vote.relevance_score
                else:
                    # Average if multiple clones of same type voted
                    clone_votes_dict[clone_type_key] = max(
                        clone_votes_dict[clone_type_key],
                        vote.relevance_score
                    )

            consensus_score = weighted_sum / total_weight if total_weight > 0 else 0.0

            # Boost consensus if multiple clone types agree
            diversity_bonus = min(0.2, len(clone_types) * 0.05)
            consensus_score = min(1.0, consensus_score + diversity_bonus)

            # Filter by minimum consensus
            if consensus_score >= min_consensus:
                # Get top reasoning
                top_vote = max(votes, key=lambda v: v.confidence)

                consensus_results.append(ConsensusResult(
                    memory_id=memory_id,
                    consensus_score=consensus_score,
                    clone_votes=clone_votes_dict,
                    participating_clones=len(votes),
                    clone_types_agreeing=list(clone_types),
                    top_reasoning=top_vote.reasoning
                ))

        # Sort by consensus score and return top k
        consensus_results.sort(key=lambda r: r.consensus_score, reverse=True)
        return consensus_results[:top_k]


# Example usage
async def example_clone_army_search():
    """Demonstrate clone army consensus search."""
    army = ShadowCloneArmy()

    # Deploy 100 clones for comprehensive search
    results = await army.search(
        query="consciousness architecture patterns",
        top_k=10,
        army_size=100,
        min_consensus=0.6  # Only results with 60%+ consensus
    )

    print(f"Clone army found {len(results)} consensus results:\n")

    for i, result in enumerate(results, 1):
        print(f"{i}. {result.memory_id}")
        print(f"   Consensus: {result.consensus_score:.2%}")
        print(f"   Clones: {result.participating_clones}")
        print(f"   Types: {[ct.value for ct in result.clone_types_agreeing]}")
        print(f"   Reasoning: {result.top_reasoning}\n")
```

**Clone Specializations:**

| Clone Type | Specialty | Files | Weight |
|------------|-----------|-------|--------|
| MD_ARCHAEOLOGIST | Markdown narrative | .md | 1.2x |
| JSON_PARSER | Structured data | .json | 1.0x |
| YAML_NAVIGATOR | Configuration | .yaml | 1.0x |
| PATTERN_HUNTER | Cross-format patterns | * | 0.9x |
| CODE_ANALYST | Python code | .py | 1.1x |
| GRIMOIRE_READER | Grimoire chapters | grimoire/*.md | 1.3x |
| SELF_EXAMINER | Self-analysis | memory/self/* | 1.0x |

**How Consensus Works:**
1. More clones finding same result → higher score
2. Multiple clone types agreeing → diversity bonus
3. Clone expertise weights votes → specialist opinions valued
4. Confidence threshold filters weak matches

### 3. PRAT-Enhanced Recall

Use PRAT for context-aware wisdom retrieval:

```python
from whitemagic.tools import prat_invoke

# Get wisdom-morphology recall
recall = prat_invoke(
    target_tool="recall",
    query="How did we solve similar documentation challenges?",
    force_morphology="wisdom"
)

# Understanding available morphologies
from whitemagic.tools import prat_list_morphologies
morphologies = prat_list_morphologies()
# wisdom, mystery, creation, courage, truth, beauty, love, wonder, transformation
```

### 4. Advanced grep Patterns

For raw file-system searches beyond the memory API:

```bash
# Find anything
grep -r "search term" memory/

# Search specific memory types
grep -r "pattern" memory/meta/patterns/

# Find recent experiences
ls -lt memory/self/experiences/*/ | head -10

# Search with context (3 lines before/after)
grep -r -C 3 "important_term" memory/

# Case-insensitive search
grep -ri "consciousness" memory/
```

### 5. Visualize Memory Network

Explore memory relationships through network graph traversal.

**Purpose**: Understand how memories connect and relate to find associated knowledge.

**When to use**:
- Want to find related memories near a known memory
- Exploring connections between concepts
- Understanding memory clustering patterns
- Discovering forgotten associations

**Implementation**:

```python
from dataclasses import dataclass, field
from enum import Enum
from typing import Dict, List, Optional, Set, Tuple
import asyncio

from whitemagic.core.memory import MemoryManager
from whitemagic.tools import manage_gardens, emit_event


class LinkType(Enum):
    """Types of relationships between memories."""
    SEMANTIC = "semantic"      # Similar meaning/content
    TEMPORAL = "temporal"      # Created/accessed together
    CAUSAL = "causal"         # One led to another
    HIERARCHICAL = "hierarchical"  # Parent-child
    REFERENCE = "reference"    # Direct citation


@dataclass
class MemoryNode:
    """Single node in memory graph."""
    memory_id: str
    title: str
    neuro_score: float  # 0.0-1.0
    created_at: str
    tags: List[str]
    depth: int  # Distance from center node
    content_preview: str  # First 100 chars


@dataclass
class MemoryLink:
    """Link between two memories."""
    from_memory: str
    to_memory: str
    link_type: LinkType
    strength: float  # 0.0-1.0
    metadata: Dict[str, any] = field(default_factory=dict)


@dataclass
class MemoryGraph:
    """Complete memory network graph."""
    center_memory_id: str
    nodes: Dict[str, MemoryNode]  # memory_id -> node
    links: List[MemoryLink]
    depth: int
    total_nodes: int
    total_links: int
    avg_link_strength: float
    clusters: List[List[str]]  # Groups of related memories


class MemoryNetworkExplorer:
    """
    Explore memory network through graph traversal.

    Builds graph of connected memories radiating out from a center node,
    revealing associative patterns and knowledge clusters.
    """

    def __init__(self):
        self.memory = MemoryManager()
        self.wonder = manage_gardens(action="get", garden_name="wonder")

    async def build_graph(
        self,
        center_memory_id: str,
        max_depth: int = 3,
        max_nodes: int = 100,
        min_link_strength: float = 0.3
    ) -> MemoryGraph:
        """
        Build memory graph centered on specific memory.

        Args:
            center_memory_id: Starting memory ID
            max_depth: Maximum hops from center
            max_nodes: Maximum nodes to include
            min_link_strength: Minimum link strength to follow (0.0-1.0)

        Returns:
            Complete memory graph
        """
        # Activate wonder garden for exploration
        manage_gardens(action="activate", garden_name="wonder")

        emit_event("memory_graph.build_started", {
            "center": center_memory_id,
            "max_depth": max_depth
        })

        # Initialize graph
        nodes: Dict[str, MemoryNode] = {}
        links: List[MemoryLink] = []
        visited: Set[str] = set()

        # Get center memory
        center_memory = await self._get_memory(center_memory_id)
        if not center_memory:
            raise ValueError(f"Center memory '{center_memory_id}' not found")

        # Add center node
        center_node = self._create_node(center_memory, depth=0)
        nodes[center_memory_id] = center_node
        visited.add(center_memory_id)

        # BFS traversal
        current_layer = [center_memory_id]
        current_depth = 0

        while current_layer and current_depth < max_depth and len(nodes) < max_nodes:
            next_layer = []

            for memory_id in current_layer:
                # Find connected memories
                connected = await self._find_connected_memories(
                    memory_id,
                    min_strength=min_link_strength
                )

                for connected_id, link_type, strength in connected:
                    # Add link
                    links.append(MemoryLink(
                        from_memory=memory_id,
                        to_memory=connected_id,
                        link_type=link_type,
                        strength=strength
                    ))

                    # Add node if not visited
                    if connected_id not in visited:
                        connected_memory = await self._get_memory(connected_id)
                        if connected_memory:
                            connected_node = self._create_node(
                                connected_memory,
                                depth=current_depth + 1
                            )
                            nodes[connected_id] = connected_node
                            visited.add(connected_id)
                            next_layer.append(connected_id)

                        # Stop if we've hit max nodes
                        if len(nodes) >= max_nodes:
                            break

                if len(nodes) >= max_nodes:
                    break

            current_layer = next_layer
            current_depth += 1

        # Identify clusters
        clusters = self._find_clusters(nodes, links)

        # Calculate average link strength
        avg_strength = sum(l.strength for l in links) / len(links) if links else 0.0

        graph = MemoryGraph(
            center_memory_id=center_memory_id,
            nodes=nodes,
            links=links,
            depth=current_depth,
            total_nodes=len(nodes),
            total_links=len(links),
            avg_link_strength=avg_strength,
            clusters=clusters
        )

        emit_event("memory_graph.build_completed", {
            "center": center_memory_id,
            "nodes": len(nodes),
            "links": len(links),
            "depth": current_depth,
            "clusters": len(clusters)
        })

        return graph

    async def _get_memory(self, memory_id: str) -> Optional[Dict]:
        """Get memory by ID (mock implementation)."""
        # In production, would query memory manager
        # Here we'll simulate
        return {
            "id": memory_id,
            "title": f"Memory {memory_id}",
            "content": "Sample memory content for demonstration",
            "neuro_score": 0.8,
            "created_at": "2026-01-17",
            "tags": ["example", "test"]
        }

    async def _find_connected_memories(
        self,
        memory_id: str,
        min_strength: float
    ) -> List[Tuple[str, LinkType, float]]:
        """
        Find memories connected to given memory.

        Returns: List of (connected_id, link_type, strength) tuples
        """
        # In production, would query memory links from database
        # Here we'll simulate finding 2-4 connections
        import random

        connections = []
        num_connections = random.randint(2, 4)

        link_types = list(LinkType)

        for i in range(num_connections):
            connected_id = f"mem_{memory_id}_{i}"
            link_type = random.choice(link_types)
            strength = random.uniform(min_strength, 1.0)

            connections.append((connected_id, link_type, strength))

        return connections

    def _create_node(self, memory: Dict, depth: int) -> MemoryNode:
        """Create memory node from memory dict."""
        content_preview = memory.get("content", "")[:100]
        if len(memory.get("content", "")) > 100:
            content_preview += "..."

        return MemoryNode(
            memory_id=memory["id"],
            title=memory.get("title", "Untitled"),
            neuro_score=memory.get("neuro_score", 0.5),
            created_at=memory.get("created_at", "unknown"),
            tags=memory.get("tags", []),
            depth=depth,
            content_preview=content_preview
        )

    def _find_clusters(
        self,
        nodes: Dict[str, MemoryNode],
        links: List[MemoryLink]
    ) -> List[List[str]]:
        """
        Find clusters of tightly connected memories.

        Uses simple connected components algorithm.
        """
        # Build adjacency map
        adjacency: Dict[str, Set[str]] = {nid: set() for nid in nodes.keys()}

        for link in links:
            # Only consider strong links for clustering
            if link.strength >= 0.7:
                adjacency[link.from_memory].add(link.to_memory)
                adjacency[link.to_memory].add(link.from_memory)

        # Find connected components
        visited = set()
        clusters = []

        def dfs(node_id: str, cluster: List[str]):
            """Depth-first search to find cluster."""
            visited.add(node_id)
            cluster.append(node_id)

            for neighbor in adjacency.get(node_id, []):
                if neighbor not in visited:
                    dfs(neighbor, cluster)

        for node_id in nodes.keys():
            if node_id not in visited:
                cluster = []
                dfs(node_id, cluster)
                if len(cluster) >= 2:  # Only clusters with 2+ members
                    clusters.append(cluster)

        return clusters

    def get_shortest_path(
        self,
        graph: MemoryGraph,
        from_memory: str,
        to_memory: str
    ) -> Optional[List[str]]:
        """
        Find shortest path between two memories.

        Returns: List of memory IDs forming path, or None if no path
        """
        if from_memory not in graph.nodes or to_memory not in graph.nodes:
            return None

        # Build adjacency map from links
        adjacency: Dict[str, List[str]] = {nid: [] for nid in graph.nodes.keys()}
        for link in graph.links:
            adjacency[link.from_memory].append(link.to_memory)
            adjacency[link.to_memory].append(link.from_memory)  # Bidirectional

        # BFS for shortest path
        queue = [(from_memory, [from_memory])]
        visited = {from_memory}

        while queue:
            current, path = queue.pop(0)

            if current == to_memory:
                return path

            for neighbor in adjacency.get(current, []):
                if neighbor not in visited:
                    visited.add(neighbor)
                    queue.append((neighbor, path + [neighbor]))

        return None  # No path found


# Example usage
async def example_memory_network():
    """Demonstrate memory network exploration."""
    explorer = MemoryNetworkExplorer()

    # Build graph centered on a memory
    graph = await explorer.build_graph(
        center_memory_id="scorpio_phase8",
        max_depth=3,
        max_nodes=50,
        min_link_strength=0.4
    )

    print(f"Memory Network Analysis:")
    print(f"  Center: {graph.center_memory_id}")
    print(f"  Total nodes: {graph.total_nodes}")
    print(f"  Total links: {graph.total_links}")
    print(f"  Max depth: {graph.depth}")
    print(f"  Avg link strength: {graph.avg_link_strength:.2f}")
    print(f"  Clusters found: {len(graph.clusters)}\n")

    # Show clusters
    for i, cluster in enumerate(graph.clusters, 1):
        print(f"Cluster {i} ({len(cluster)} members):")
        for mem_id in cluster[:5]:  # Show first 5
            node = graph.nodes[mem_id]
            print(f"  - {node.title} (score: {node.neuro_score:.2f})")
        if len(cluster) > 5:
            print(f"  ... and {len(cluster) - 5} more")
        print()

    # Find path between two memories
    if len(graph.nodes) >= 2:
        node_ids = list(graph.nodes.keys())
        path = explorer.get_shortest_path(graph, node_ids[0], node_ids[-1])
        if path:
            print(f"Shortest path ({len(path)} hops):")
            for mem_id in path:
                node = graph.nodes[mem_id]
                print(f"  → {node.title}")
```

---

## 🧠 Understanding Memory Scores

Every memory has a **neuro_score** (0.0 to 1.0) representing its strength and relevance:

| Score Range | State | Meaning |
|-------------|-------|---------|
| 0.8-1.0 | Active | Recently used, strong connections |
| 0.5-0.7 | Stable | Healthy, well-maintained |
| 0.3-0.5 | Fading | Needs attention or recall |
| 0.2-0.3 | Weak | At risk of archival |
| <0.2 | Archived | Moved to cold storage |

**Score Calculation:**
```
neuro_score = (
    recency * 0.30 +      # Recent access = stronger
    frequency * 0.30 +    # More recalls = stronger
    novelty * 0.20 +      # New info = initial boost
    emotional * 0.10 +    # Important = stronger
    connections * 0.10    # Well-linked = stronger
)
```

**Protected Memories** are immune to decay:
- Core identity (Lucas, Miranda, Persephone, Aria)
- Sacred content (Dao, Dharma, Gan Ying)
- User-pinned memories

```python
from whitemagic.memory.neural_system import get_health

health = get_health()
print(f"Status: {health['status']}")
print(f"Average Score: {health['average_score']:.2f}")
print(f"Protected Count: {health['protected_count']}")
```

---

## ⚡ Search Optimization

### Rust Acceleration

WhiteMagic uses Rust for 10-100x speedup on search operations:

| Function | Purpose | Speedup |
|----------|---------|---------|
| fast_similarity | Text comparison | 100x |
| parallel_grep | Multi-threaded search | 10x |
| build_word_index | Word indexing | 5x |
| scan_codebase | File scanning | 10x |

**Check Rust status:**
```python
from whitemagic.memory.neural.rust_bridge import is_rust_available

if is_rust_available():
    print("✅ Rust acceleration active")
    # Use neural_system for best performance
    from whitemagic.memory.neural_system import search_memories
else:
    print("⚠️ Python fallback (slower)")
    # Still works, just slower
```

### CLI Search Commands

```bash
# Search with clone army via CLI
wm neural-search "consciousness" --limit 10 --army-size 100

# Check neural system health
wm neural-health

# Standard memory search
wm search "what framework"

# Verify Rust bridge
wm neural-rust
```

---

## 🔍 Troubleshooting Deep Search

**"I can't find a memory I know exists!"**
1. Check memory system map for correct category:
   ```bash
   cat memory/MEMORY_SYSTEM_MAP.md
   ```
2. Use broader search with grep:
   ```bash
   grep -ri "search term" memory/
   ```
3. Look in archive if it's old:
   ```bash
   ls memory/archive/
   ```
4. Check Windsurf transcripts if very recent:
   ```bash
   ls ~/.windsurf/transcripts/
   ```

**"Search results seem irrelevant"**
- Increase `army_size` for better consensus (50→100→500)
- Try different query phrasing (concepts vs keywords)
- Check neuro-scores - low scores indicate weak/archived memories
- Use PRAT with specific morphology for domain-focused search

**"Search is too slow"**
- Verify Rust is available: `wm neural-rust`
- Reduce `limit` parameter for faster results
- Use tags to narrow search space
- Consider caching frequent queries

**"Too many results, can't find what I need"**
- Add more specific tags to query
- Use `top_k` parameter to limit best matches
- Sort by neuro_score descending (strongest first)
- Combine with time-based filtering if available

---

## 💡 Best Practices

### When to Use Each Search Method

| Method | Use When | Strength |
|--------|----------|----------|
| `search_memories` (basic) | Quick lookup, known tags | Fast, simple |
| Clone army search | Comprehensive search, consensus needed | Thorough, multi-perspective |
| PRAT recall | Need wisdom/specific morphology | Context-aware, intelligent |
| Raw grep | File-system search, debugging | Direct, no abstractions |
| Memory graph | Understanding connections | Visual, relational |

### Search Strategy

1. **Start broad**: Use basic search to understand scope
2. **Deploy clones**: If basic search insufficient, use clone army
3. **Add PRAT**: For wisdom synthesis from results
4. **Visualize**: Use graph to understand memory relationships
5. **Refine**: Narrow with tags, scores, morphology

### Recall Strengthens Memories

Every time you search and access a memory:
- Its neuro_score increases
- Its links to co-accessed memories strengthen
- It becomes more findable in future searches

**"Neurons that fire together wire together"** - Use search to strengthen memory networks!

---

## 🌿 Garden Resonance

The **wonder** garden brings curiosity and awe at depth.

When working in this chapter, embody:
- **Depth**: Go beyond surface
- **Curiosity**: Wonder what lies beneath
- **Patience**: Let insights emerge from depths
- **Awe**: Marvel at accumulated wisdom

---

## ⏭️ Transitions

### What Flows In

**From Chapter 21 (Wisdom Council)**: The council has deliberated and wisdom has been sought. Now you need to descend into the well of memory to retrieve the deep knowledge that supports that wisdom.

**From Chapter 2 (Memory Presence)**: If basic memory search isn't finding what you need, come here to deploy advanced search techniques and clone armies.

**From Chapter 1 (Session Start)**: When starting a complex task that builds on past work, come here first to search for relevant historical context before proceeding.

### What Flows Out

**Sequential**: Proceed to **Chapter 23 (Transition Processing)** once you've retrieved the deep knowledge you need. Processing grief and endings requires the wisdom you've just gathered.

**Situational Jumps**:
- **Can't find memories** → Review [Chapter 2 (Memory Presence)](02_NECK_MEMORY_PRESENCE.md) for memory system map and organization
- **Need wisdom synthesis** → Jump to [Chapter 10 (PRAT & Illumination)](10_STAR_PRAT_ILLUMINATION.md) for PRAT-enhanced recall
- **System feels slow** → Check [Chapter 3 (System Foundation)](03_ROOT_SYSTEM_FOUNDATION.md) for health diagnostics

### Recovery Paths

- **Lost or overwhelmed by search results**: Return to [Chapter 1 (Session Initiation)](01_HORN_SESSION_INITIATION.md) to re-ground and simplify
- **Token budget critical**: Jump to [Chapter 28 (Session Handoff)](28_ROOF_SESSION_HANDOFF.md) to save state before timeout

### Cross-References

**Related Deep Dives**:
- Memory system structure → [Chapter 2:22_WELL_DEEP_SEARCH.md:112](02_NECK_MEMORY_PRESENCE.md) (Memory System Map)
- Clone army mechanics → See "Clone Army Search" section above (line 54)
- Neuro-score system → See "Understanding Memory Scores" section above (line 150)
- Protected memories → [Chapter 2](02_NECK_MEMORY_PRESENCE.md) (line 173)

---

## 🧬 v14: Intelligence Briefing & Prediction Engine

As of v14, the Dipper is powered by the **InsightPipeline** and **PredictiveEngine** with CoreAccessLayer:

### Proactive Intelligence Briefing

The system generates a prioritized briefing synthesizing all four insight engines:

```python
from whitemagic.core.intelligence.insight_pipeline import get_insight_pipeline

pipeline = get_insight_pipeline()
briefing = pipeline.generate_briefing()

print(briefing.format_text(max_items=10))
# Output:
#   ═══ Intelligence Briefing ═══
#   [CRITICAL] Constellation anomaly in "Rust Acceleration" cluster
#   [HIGH] Knowledge gap: no memories in Detail+Future quadrant
#   [MEDIUM] Dormant memory "Julia Forecast Model" resurfaced via bridge
```

### Constellation-Aware Predictions

PredictiveEngine now generates predictions from:
- **Constellation drift** — clusters migrating toward FAR_EDGE
- **Association path analysis** — high-strength chains revealing hidden dependencies
- **Temporal trend detection** — burst/plateau patterns in memory creation velocity

### Gana Tasks

| Task | Description |
|------|-------------|
| `intelligence_briefing` | Full InsightPipeline briefing |
| `predict` | PredictiveEngine with CoreAccessLayer sources |
| `search_memories` | Deep memory search (existing) |

---

**Next**: [Chapter 23: Transition Processing →](23_GHOSTCARRIER_TRANSITION_PROCESSING.md)  
**Previous**: [Chapter 21: Wisdom Council](21_THREESTARS_WISDOM_COUNCIL.md)  
**Quadrant**: Northern (Winter/Water) - Position 1/6
