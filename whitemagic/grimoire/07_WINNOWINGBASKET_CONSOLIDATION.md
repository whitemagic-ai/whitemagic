# Chapter 7: Consolidation

**Gana**: WinnowingBasketGana (Chinese: 箕, Pinyin: Jī)
**Garden**: truth
**Quadrant**: Eastern (Azure Dragon)
**Element**: Wood
**Phase**: Yang Rising (Peak)
**I Ching Hexagram**: 33. 遯 Dùn (Retreat) - Strategic withdrawal to integrate

---

## 🎯 Purpose

Chapter 7 performs **consolidation**—the essential separation of signal from noise after rapid work. After high-velocity execution (Ch.6), the WinnowingBasket shakes out what matters from what doesn't, consolidating gains before transitioning to the next phase.

The WinnowingBasket (箕) represents the ancient tool for separating grain from chaff—keeping what nourishes, releasing what doesn't serve. Use this chapter when you need to:

- **Consolidate memories** after intensive work
- **Separate signal from noise** in accumulated data
- **Distill insights** from experience
- **Clean up artifacts** after rapid development
- **Integrate learnings** before moving forward
- **Prepare for phase transition** (Eastern → Southern quadrant)

---

## 🔧 Primary Tools

| Tool | Description | Usage |
|------|-------------|-------|
| `consolidate_memories` | Merge short-term memories, generate reports | After intensive work sessions |
| `add_lesson` | Capture problem-solving patterns | When solutions discovered |
| `track_metric` | Record session metrics for retrospective | Phase/session boundaries |
| `create_memory` | Store consolidated insights | After integration |
| `manage_gardens` (activate) | Enter truth garden | Honest evaluation |

---

## 📋 Core Workflows

### Workflow 1: Memory Consolidation & Synthesis

**Purpose**: Consolidate short-term memories into coherent insights, merging related experiences and generating integrated understanding.

**When to use**: After intensive work sessions, before phase transitions, at session boundaries, when memory feels fragmented.

```python
from dataclasses import dataclass, field
from typing import Dict, List, Any, Optional, Set
from enum import Enum
import asyncio
from datetime import datetime, timedelta
from whitemagic.core.memory.manager import MemoryManager
from whitemagic.tools import consolidate_memories, manage_gardens
from whitemagic.core.resonance.gan_ying_enhanced import emit_event

class ConsolidationStrategy(Enum):
    """Strategy for memory consolidation."""
    TEMPORAL = "temporal"      # Group by time proximity
    SEMANTIC = "semantic"      # Group by topic similarity
    CONTEXTUAL = "contextual"  # Group by shared context
    PRIORITY = "priority"      # Focus on high-value memories
    COMPREHENSIVE = "comprehensive"  # All strategies combined

@dataclass
class MemoryCluster:
    """Cluster of related memories."""
    cluster_id: str
    theme: str
    memories: List[Any] = field(default_factory=list)

    # Synthesis
    core_insight: str = ""
    supporting_evidence: List[str] = field(default_factory=list)
    implications: List[str] = field(default_factory=list)

    # Metadata
    time_span: Optional[timedelta] = None
    importance_score: float = 0.0

@dataclass
class ConsolidationReport:
    """Report of memory consolidation process."""
    timestamp: str

    # Input
    memories_reviewed: int
    time_range_hours: float

    # Processing
    clusters_formed: int
    memories_merged: int
    memories_archived: int
    memories_discarded: int

    # Output
    insights_generated: List[str] = field(default_factory=list)
    patterns_discovered: List[str] = field(default_factory=list)
    clusters: List[MemoryCluster] = field(default_factory=list)

    # Recommendations
    action_items: List[str] = field(default_factory=list)
    follow_up_topics: List[str] = field(default_factory=list)

class MemoryConsolidator:
    """Consolidate and synthesize memories into coherent insights."""

    def __init__(self):
        self.memory = MemoryManager()
        self.truth = manage_gardens(action="get", garden_name="truth")

    async def consolidate_recent_memories(
        self,
        hours_back: float = 24.0,
        strategy: ConsolidationStrategy = ConsolidationStrategy.COMPREHENSIVE,
        min_cluster_size: int = 2
    ) -> ConsolidationReport:
        """
        Consolidate recent memories using specified strategy.

        Args:
            hours_back: How far back to look for memories
            strategy: Consolidation strategy to use
            min_cluster_size: Minimum memories to form cluster

        Returns:
            Consolidation report with clusters and insights
        """
        # Activate truth garden for honest assessment
        manage_gardens(action="activate", garden_name="truth")

        emit_event("consolidation.started", {
            "hours_back": hours_back,
            "strategy": strategy.value
        })

        # Load recent memories
        recent_memories = await self._load_recent_memories(hours_back)

        # Cluster memories by strategy
        if strategy == ConsolidationStrategy.COMPREHENSIVE:
            clusters = await self._comprehensive_clustering(
                recent_memories,
                min_cluster_size
            )
        else:
            clusters = await self._strategy_clustering(
                recent_memories,
                strategy,
                min_cluster_size
            )

        # Synthesize insights from clusters
        insights = []
        patterns = []

        for cluster in clusters:
            cluster_insights = await self._synthesize_cluster(cluster)
            insights.extend(cluster_insights["insights"])
            patterns.extend(cluster_insights["patterns"])

        # Determine what to archive/discard
        to_archive = await self._identify_archivable(recent_memories, clusters)
        to_discard = await self._identify_discardable(recent_memories, clusters)

        # Execute consolidation
        merged_count = await self._merge_clusters(clusters)
        archived_count = await self._archive_memories(to_archive)
        discarded_count = await self._discard_memories(to_discard)

        # Generate action items
        action_items = self._generate_action_items(clusters, insights)

        # Create report
        report = ConsolidationReport(
            timestamp=datetime.now().isoformat(),
            memories_reviewed=len(recent_memories),
            time_range_hours=hours_back,
            clusters_formed=len(clusters),
            memories_merged=merged_count,
            memories_archived=archived_count,
            memories_discarded=discarded_count,
            insights_generated=insights,
            patterns_discovered=patterns,
            clusters=clusters,
            action_items=action_items,
            follow_up_topics=self._extract_follow_up_topics(clusters)
        )

        # Store consolidation report
        await self.memory.store(
            content=f"Memory consolidation: {len(clusters)} clusters, {len(insights)} insights",
            metadata={
                "type": "consolidation_report",
                "timestamp": report.timestamp,
                "clusters": len(clusters),
                "insights": len(insights),
                "strategy": strategy.value
            },
            tier="short_term"
        )

        emit_event("consolidation.completed", {
            "clusters": len(clusters),
            "insights": len(insights),
            "memories_merged": merged_count
        })

        return report

    async def _load_recent_memories(
        self,
        hours_back: float
    ) -> List[Any]:
        """Load recent memories from specified time window."""
        # Search for memories from recent period
        memories = await self.memory.search(
            query="",  # All memories
            tier="short_term",
            limit=1000
        )

        # Filter by timestamp
        cutoff_time = datetime.now() - timedelta(hours=hours_back)

        recent = []
        for memory in memories:
            memory_time = datetime.fromisoformat(
                memory.metadata.get("timestamp", datetime.now().isoformat())
            )
            if memory_time >= cutoff_time:
                recent.append(memory)

        return recent

    async def _comprehensive_clustering(
        self,
        memories: List[Any],
        min_size: int
    ) -> List[MemoryCluster]:
        """Use all clustering strategies combined."""
        # Cluster by semantic similarity
        semantic_clusters = await self._semantic_clustering(memories, min_size)

        # Cluster by time proximity
        temporal_clusters = await self._temporal_clustering(memories, min_size)

        # Cluster by shared context
        contextual_clusters = await self._contextual_clustering(memories, min_size)

        # Merge overlapping clusters
        all_clusters = semantic_clusters + temporal_clusters + contextual_clusters
        merged_clusters = self._merge_overlapping_clusters(all_clusters)

        return merged_clusters

    async def _semantic_clustering(
        self,
        memories: List[Any],
        min_size: int
    ) -> List[MemoryCluster]:
        """Cluster memories by semantic similarity."""
        # Use memory manager's similarity search
        clusters = []

        processed_ids = set()

        for memory in memories:
            if memory.id in processed_ids:
                continue

            # Find similar memories
            similar = await self.memory.search(
                query=memory.content,
                limit=20
            )

            # Filter to unprocessed similar memories
            cluster_memories = [
                m for m in similar
                if m.id not in processed_ids and m.id != memory.id
            ]

            if len(cluster_memories) >= min_size - 1:
                cluster_memories.insert(0, memory)

                cluster = MemoryCluster(
                    cluster_id=f"semantic_{memory.id[:8]}",
                    theme=self._extract_theme(cluster_memories),
                    memories=cluster_memories
                )

                clusters.append(cluster)

                # Mark as processed
                for m in cluster_memories:
                    processed_ids.add(m.id)

        return clusters

    async def _temporal_clustering(
        self,
        memories: List[Any],
        min_size: int
    ) -> List[MemoryCluster]:
        """Cluster memories by time proximity."""
        # Sort by timestamp
        sorted_memories = sorted(
            memories,
            key=lambda m: m.metadata.get("timestamp", "")
        )

        clusters = []
        current_cluster = []
        cluster_start_time = None

        for memory in sorted_memories:
            memory_time = datetime.fromisoformat(
                memory.metadata.get("timestamp", datetime.now().isoformat())
            )

            if not current_cluster:
                current_cluster = [memory]
                cluster_start_time = memory_time
            else:
                # Check if within time window (e.g., 1 hour)
                time_diff = memory_time - cluster_start_time
                if time_diff.total_seconds() / 3600 <= 1.0:
                    current_cluster.append(memory)
                else:
                    # Save current cluster if large enough
                    if len(current_cluster) >= min_size:
                        cluster = MemoryCluster(
                            cluster_id=f"temporal_{cluster_start_time.strftime('%Y%m%d%H%M')}",
                            theme=f"Work from {cluster_start_time.strftime('%H:%M')}",
                            memories=current_cluster,
                            time_span=memory_time - cluster_start_time
                        )
                        clusters.append(cluster)

                    # Start new cluster
                    current_cluster = [memory]
                    cluster_start_time = memory_time

        # Add final cluster
        if len(current_cluster) >= min_size:
            cluster = MemoryCluster(
                cluster_id=f"temporal_{cluster_start_time.strftime('%Y%m%d%H%M')}",
                theme=f"Work from {cluster_start_time.strftime('%H:%M')}",
                memories=current_cluster
            )
            clusters.append(cluster)

        return clusters

    async def _contextual_clustering(
        self,
        memories: List[Any],
        min_size: int
    ) -> List[MemoryCluster]:
        """Cluster memories by shared context tags."""
        # Group by shared tags
        tag_groups: Dict[str, List[Any]] = {}

        for memory in memories:
            tags = memory.metadata.get("tags", [])
            for tag in tags:
                if tag not in tag_groups:
                    tag_groups[tag] = []
                tag_groups[tag].append(memory)

        # Create clusters from groups
        clusters = []

        for tag, group_memories in tag_groups.items():
            if len(group_memories) >= min_size:
                cluster = MemoryCluster(
                    cluster_id=f"context_{tag}",
                    theme=f"Context: {tag}",
                    memories=group_memories
                )
                clusters.append(cluster)

        return clusters

    async def _strategy_clustering(
        self,
        memories: List[Any],
        strategy: ConsolidationStrategy,
        min_size: int
    ) -> List[MemoryCluster]:
        """Apply single clustering strategy."""
        if strategy == ConsolidationStrategy.SEMANTIC:
            return await self._semantic_clustering(memories, min_size)
        elif strategy == ConsolidationStrategy.TEMPORAL:
            return await self._temporal_clustering(memories, min_size)
        elif strategy == ConsolidationStrategy.CONTEXTUAL:
            return await self._contextual_clustering(memories, min_size)
        else:
            return []

    def _merge_overlapping_clusters(
        self,
        clusters: List[MemoryCluster]
    ) -> List[MemoryCluster]:
        """Merge clusters with significant memory overlap."""
        merged = []
        used_clusters = set()

        for i, cluster1 in enumerate(clusters):
            if i in used_clusters:
                continue

            # Find overlapping clusters
            to_merge = [cluster1]

            for j, cluster2 in enumerate(clusters[i+1:], start=i+1):
                if j in used_clusters:
                    continue

                # Check overlap
                ids1 = {m.id for m in cluster1.memories}
                ids2 = {m.id for m in cluster2.memories}

                overlap = len(ids1 & ids2)
                min_size = min(len(ids1), len(ids2))

                # If >50% overlap, merge
                if overlap / min_size > 0.5:
                    to_merge.append(cluster2)
                    used_clusters.add(j)

            # Merge clusters
            if len(to_merge) > 1:
                merged_cluster = self._merge_cluster_list(to_merge)
                merged.append(merged_cluster)
            else:
                merged.append(cluster1)

            used_clusters.add(i)

        return merged

    def _merge_cluster_list(
        self,
        clusters: List[MemoryCluster]
    ) -> MemoryCluster:
        """Merge list of clusters into one."""
        all_memories = []
        all_ids = set()

        for cluster in clusters:
            for memory in cluster.memories:
                if memory.id not in all_ids:
                    all_memories.append(memory)
                    all_ids.add(memory.id)

        return MemoryCluster(
            cluster_id=f"merged_{clusters[0].cluster_id}",
            theme=self._extract_theme(all_memories),
            memories=all_memories
        )

    def _extract_theme(self, memories: List[Any]) -> str:
        """Extract common theme from memory cluster."""
        # Simple heuristic: most common words in content
        # In practice, use LLM or topic modeling

        words: Dict[str, int] = {}

        for memory in memories:
            content = memory.content.lower()
            for word in content.split():
                if len(word) > 4:  # Ignore short words
                    words[word] = words.get(word, 0) + 1

        if not words:
            return "General work"

        # Get top 3 words
        top_words = sorted(words.items(), key=lambda x: x[1], reverse=True)[:3]
        theme_words = [word for word, _ in top_words]

        return " ".join(theme_words).capitalize()

    async def _synthesize_cluster(
        self,
        cluster: MemoryCluster
    ) -> Dict[str, List[str]]:
        """Synthesize insights and patterns from cluster."""
        insights = []
        patterns = []

        # Analyze cluster for insights
        if len(cluster.memories) >= 3:
            # Look for common outcomes
            outcomes = [m.metadata.get("outcome") for m in cluster.memories if m.metadata.get("outcome")]
            if outcomes:
                insights.append(
                    f"{cluster.theme}: Pattern of {len(outcomes)} similar outcomes"
                )

            # Look for repeated actions
            actions = [m.metadata.get("action") for m in cluster.memories if m.metadata.get("action")]
            if len(actions) >= 2:
                patterns.append(
                    f"{cluster.theme}: Repeated action pattern identified"
                )

        cluster.core_insight = insights[0] if insights else f"{cluster.theme}: {len(cluster.memories)} related items"

        return {
            "insights": insights,
            "patterns": patterns
        }

    async def _identify_archivable(
        self,
        memories: List[Any],
        clusters: List[MemoryCluster]
    ) -> List[Any]:
        """Identify memories that should be archived."""
        clustered_ids = set()
        for cluster in clusters:
            for memory in cluster.memories:
                clustered_ids.add(memory.id)

        # Archive clustered memories (they're now synthesized)
        archivable = [m for m in memories if m.id in clustered_ids]

        return archivable

    async def _identify_discardable(
        self,
        memories: List[Any],
        clusters: List[MemoryCluster]
    ) -> List[Any]:
        """Identify memories that can be discarded."""
        # Discard low-value unclustered memories
        clustered_ids = set()
        for cluster in clusters:
            for memory in cluster.memories:
                clustered_ids.add(memory.id)

        discardable = []

        for memory in memories:
            if memory.id not in clustered_ids:
                # Check if low value
                importance = memory.metadata.get("importance", 0.5)
                if importance < 0.3:
                    discardable.append(memory)

        return discardable

    async def _merge_clusters(
        self,
        clusters: List[MemoryCluster]
    ) -> int:
        """Merge cluster memories into consolidated memories."""
        merged_count = 0

        for cluster in clusters:
            # Create consolidated memory
            await self.memory.store(
                content=cluster.core_insight,
                metadata={
                    "type": "consolidated_insight",
                    "cluster_id": cluster.cluster_id,
                    "theme": cluster.theme,
                    "source_count": len(cluster.memories),
                    "supporting_evidence": cluster.supporting_evidence
                },
                tier="long_term"
            )

            merged_count += len(cluster.memories)

        return merged_count

    async def _archive_memories(self, memories: List[Any]) -> int:
        """Archive memories to long-term storage."""
        for memory in memories:
            # Move to archive tier
            # Implementation depends on memory manager
            pass

        return len(memories)

    async def _discard_memories(self, memories: List[Any]) -> int:
        """Discard low-value memories."""
        for memory in memories:
            # Delete from storage
            # Implementation depends on memory manager
            pass

        return len(memories)

    def _generate_action_items(
        self,
        clusters: List[MemoryCluster],
        insights: List[str]
    ) -> List[str]:
        """Generate action items from consolidation."""
        actions = []

        for cluster in clusters:
            # Check for incomplete work
            incomplete = [
                m for m in cluster.memories
                if m.metadata.get("status") == "incomplete"
            ]

            if incomplete:
                actions.append(
                    f"Complete work in {cluster.theme}: {len(incomplete)} items pending"
                )

        return actions

    def _extract_follow_up_topics(
        self,
        clusters: List[MemoryCluster]
    ) -> List[str]:
        """Extract topics for follow-up work."""
        topics = []

        for cluster in clusters:
            # Look for questions or uncertainties
            for memory in cluster.memories:
                content = memory.content.lower()
                if "?" in content or "unclear" in content or "investigate" in content:
                    topics.append(cluster.theme)
                    break

        return list(set(topics))  # Deduplicate

# Example usage
async def main():
    consolidator = MemoryConsolidator()

    # Consolidate recent memories
    report = await consolidator.consolidate_recent_memories(
        hours_back=24.0,
        strategy=ConsolidationStrategy.COMPREHENSIVE,
        min_cluster_size=2
    )

    print(f"Memory Consolidation Report:")
    print(f"  Memories reviewed: {report.memories_reviewed}")
    print(f"  Clusters formed: {report.clusters_formed}")
    print(f"  Memories merged: {report.memories_merged}")
    print(f"  Memories archived: {report.memories_archived}")
    print(f"  Memories discarded: {report.memories_discarded}")

    print(f"\nInsights Generated ({len(report.insights_generated)}):")
    for insight in report.insights_generated[:5]:
        print(f"  • {insight}")

    print(f"\nPatterns Discovered ({len(report.patterns_discovered)}):")
    for pattern in report.patterns_discovered[:5]:
        print(f"  • {pattern}")

    if report.action_items:
        print(f"\nAction Items:")
        for action in report.action_items:
            print(f"  • {action}")

    if report.follow_up_topics:
        print(f"\nFollow-up Topics:")
        for topic in report.follow_up_topics:
            print(f"  • {topic}")
```

---

### Workflow 2: Pattern Extraction from Experience

**Purpose**: Extract reusable patterns from completed work, capturing problem-solution-pattern triples for future reference.

**When to use**: After solving challenging problems, when discovering effective approaches, during retrospectives.

```python
from dataclasses import dataclass, field
from typing import Dict, List, Any, Optional
from enum import Enum
import asyncio
from whitemagic.core.memory.manager import MemoryManager
from whitemagic.tools import add_lesson
from whitemagic.core.resonance.gan_ying_enhanced import emit_event

class PatternCategory(Enum):
    """Categories of extracted patterns."""
    ARCHITECTURAL = "architectural"      # System design patterns
    ALGORITHMIC = "algorithmic"         # Problem-solving approaches
    PROCEDURAL = "procedural"           # Workflow patterns
    INTERPERSONAL = "interpersonal"     # Collaboration patterns
    DEBUGGING = "debugging"             # Troubleshooting patterns
    OPTIMIZATION = "optimization"       # Performance patterns

@dataclass
class ExperiencePattern:
    """A pattern extracted from experience."""
    pattern_id: str
    category: PatternCategory

    # Core pattern
    problem: str
    solution: str
    pattern_description: str

    # Context
    context: str
    preconditions: List[str] = field(default_factory=list)
    constraints: List[str] = field(default_factory=list)

    # Outcomes
    benefits: List[str] = field(default_factory=list)
    tradeoffs: List[str] = field(default_factory=list)
    antipatterns: List[str] = field(default_factory=list)

    # Applicability
    when_to_use: List[str] = field(default_factory=list)
    when_not_to_use: List[str] = field(default_factory=list)

    # Evidence
    examples: List[str] = field(default_factory=list)
    success_rate: Optional[float] = None

@dataclass
class PatternExtractionReport:
    """Report of pattern extraction process."""
    patterns_extracted: int
    patterns_by_category: Dict[PatternCategory, int]
    high_value_patterns: List[ExperiencePattern] = field(default_factory=list)

class PatternExtractor:
    """Extract reusable patterns from completed work."""

    def __init__(self):
        self.memory = MemoryManager()

    async def extract_patterns_from_session(
        self,
        session_context: Optional[Dict[str, Any]] = None
    ) -> PatternExtractionReport:
        """
        Extract patterns from recent session work.

        Args:
            session_context: Optional session context to analyze

        Returns:
            Report of extracted patterns
        """
        emit_event("pattern_extraction.started", {})

        # Load session memories
        if session_context is None:
            from whitemagic.tools import get_session_context
            session_context = get_session_context(
                include_recent_memories=True
            )

        # Analyze for patterns
        patterns = []

        # Pattern 1: Repeated problem-solution pairs
        ps_patterns = await self._extract_problem_solution_patterns(session_context)
        patterns.extend(ps_patterns)

        # Pattern 2: Workflow sequences
        workflow_patterns = await self._extract_workflow_patterns(session_context)
        patterns.extend(workflow_patterns)

        # Pattern 3: Debugging approaches
        debug_patterns = await self._extract_debugging_patterns(session_context)
        patterns.extend(debug_patterns)

        # Store patterns as lessons
        for pattern in patterns:
            await self._store_pattern_as_lesson(pattern)

        # Categorize patterns
        by_category = {}
        for pattern in patterns:
            cat = pattern.category
            by_category[cat] = by_category.get(cat, 0) + 1

        # Identify high-value patterns
        high_value = [
            p for p in patterns
            if p.success_rate and p.success_rate > 0.8
        ]

        report = PatternExtractionReport(
            patterns_extracted=len(patterns),
            patterns_by_category=by_category,
            high_value_patterns=high_value
        )

        # Store extraction report
        await self.memory.store(
            content=f"Pattern extraction: {len(patterns)} patterns from session",
            metadata={
                "type": "pattern_extraction_report",
                "patterns_extracted": len(patterns),
                "high_value_count": len(high_value)
            },
            tier="short_term"
        )

        emit_event("pattern_extraction.completed", {
            "patterns": len(patterns)
        })

        return report

    async def _extract_problem_solution_patterns(
        self,
        session_context: Dict[str, Any]
    ) -> List[ExperiencePattern]:
        """Extract problem-solution patterns from session."""
        patterns = []

        # Search for memories containing problems and solutions
        problem_memories = await self.memory.search(
            query="problem solved solution",
            tier="short_term",
            limit=50
        )

        for memory in problem_memories:
            # Parse problem and solution
            content = memory.content

            if "problem:" in content.lower() and "solution:" in content.lower():
                # Extract pattern
                problem_start = content.lower().find("problem:")
                solution_start = content.lower().find("solution:")

                problem = content[problem_start:solution_start].strip()
                solution = content[solution_start:].strip()

                pattern = ExperiencePattern(
                    pattern_id=f"ps_{memory.id[:8]}",
                    category=PatternCategory.PROCEDURAL,
                    problem=problem,
                    solution=solution,
                    pattern_description=self._synthesize_pattern_description(
                        problem,
                        solution
                    ),
                    context=memory.metadata.get("context", "General"),
                    examples=[content]
                )

                patterns.append(pattern)

        return patterns

    async def _extract_workflow_patterns(
        self,
        session_context: Dict[str, Any]
    ) -> List[ExperiencePattern]:
        """Extract workflow sequence patterns."""
        patterns = []

        # Look for sequences of actions
        recent_actions = session_context.get("recent_actions", [])

        if len(recent_actions) >= 3:
            # Identify common sequences
            sequences = self._find_common_sequences(recent_actions)

            for sequence in sequences:
                pattern = ExperiencePattern(
                    pattern_id=f"workflow_{hash(str(sequence)) % 10000}",
                    category=PatternCategory.PROCEDURAL,
                    problem=f"Need to accomplish: {sequence[0]['goal']}",
                    solution=f"Follow sequence: {' → '.join([a['action'] for a in sequence])}",
                    pattern_description="Sequential workflow pattern",
                    context="Session workflow",
                    examples=[str(sequence)]
                )

                patterns.append(pattern)

        return patterns

    async def _extract_debugging_patterns(
        self,
        session_context: Dict[str, Any]
    ) -> List[ExperiencePattern]:
        """Extract debugging approach patterns."""
        patterns = []

        # Search for debugging sessions
        debug_memories = await self.memory.search(
            query="debug error fix resolved",
            tier="short_term",
            limit=20
        )

        for memory in debug_memories:
            if "error" in memory.content.lower() and "fix" in memory.content.lower():
                pattern = ExperiencePattern(
                    pattern_id=f"debug_{memory.id[:8]}",
                    category=PatternCategory.DEBUGGING,
                    problem=f"Debugging: {memory.metadata.get('error_type', 'Unknown')}",
                    solution=memory.content,
                    pattern_description="Debugging approach",
                    context="Debugging session",
                    examples=[memory.content]
                )

                patterns.append(pattern)

        return patterns

    def _synthesize_pattern_description(
        self,
        problem: str,
        solution: str
    ) -> str:
        """Synthesize general pattern description from specific case."""
        # Simple heuristic synthesis
        # In practice, use LLM

        return f"When encountering {problem[:50]}..., apply {solution[:50]}..."

    def _find_common_sequences(
        self,
        actions: List[Dict[str, Any]]
    ) -> List[List[Dict[str, Any]]]:
        """Find common action sequences."""
        # Simple implementation: find sequences of length 3
        sequences = []

        for i in range(len(actions) - 2):
            sequence = actions[i:i+3]
            sequences.append(sequence)

        return sequences

    async def _store_pattern_as_lesson(
        self,
        pattern: ExperiencePattern
    ) -> None:
        """Store pattern as lesson for future reference."""
        add_lesson(
            problem=pattern.problem,
            solution=pattern.solution,
            pattern=pattern.pattern_description,
            tags=[pattern.category.value, "extracted_pattern"]
        )

# Example usage
async def main():
    extractor = PatternExtractor()

    # Extract patterns from session
    report = await extractor.extract_patterns_from_session()

    print(f"Pattern Extraction Report:")
    print(f"  Total patterns: {report.patterns_extracted}")
    print(f"\nBy Category:")
    for category, count in report.patterns_by_category.items():
        print(f"    {category.value}: {count}")

    print(f"\nHigh-Value Patterns ({len(report.high_value_patterns)}):")
    for pattern in report.high_value_patterns:
        print(f"  • {pattern.pattern_description}")
```

---

### Workflow 3: Session Boundary Management

**Purpose**: Cleanly manage session boundaries, ensuring proper setup, state preservation, and teardown.

**When to use**: At session start, at session end, during phase transitions, before long breaks.

```python
from dataclasses import dataclass, field
from typing import Dict, List, Any, Optional
from enum import Enum
from datetime import datetime
import asyncio
from whitemagic.core.memory.manager import MemoryManager
from whitemagic.tools import get_session_context, track_metric
from whitemagic.core.resonance.gan_ying_enhanced import emit_event

class SessionPhase(Enum):
    """Phase of session lifecycle."""
    STARTING = "starting"
    ACTIVE = "active"
    WINDING_DOWN = "winding_down"
    ENDING = "ending"

@dataclass
class SessionState:
    """Current state of session."""
    session_id: str
    phase: SessionPhase
    start_time: str

    # Context
    active_goals: List[str] = field(default_factory=list)
    active_gardens: List[str] = field(default_factory=list)
    context_loaded: bool = False

    # Metrics
    memories_created: int = 0
    events_emitted: int = 0
    tasks_completed: int = 0

    # State to preserve
    preserved_state: Dict[str, Any] = field(default_factory=dict)

@dataclass
class SessionSummary:
    """Summary of completed session."""
    session_id: str
    duration_minutes: float

    # Accomplishments
    goals_completed: List[str] = field(default_factory=list)
    goals_deferred: List[str] = field(default_factory=list)

    # Knowledge created
    insights_gained: List[str] = field(default_factory=list)
    patterns_discovered: List[str] = field(default_factory=list)
    lessons_learned: List[str] = field(default_factory=list)

    # Next session prep
    continuation_context: Dict[str, Any] = field(default_factory=dict)
    recommended_start: List[str] = field(default_factory=list)

class SessionBoundaryManager:
    """Manage session boundaries and state transitions."""

    def __init__(self):
        self.memory = MemoryManager()
        self.current_session: Optional[SessionState] = None

    async def begin_session(
        self,
        goals: Optional[List[str]] = None,
        load_previous_context: bool = True
    ) -> SessionState:
        """
        Begin new session with proper setup.

        Args:
            goals: Session goals (optional)
            load_previous_context: Whether to load previous session context

        Returns:
            Session state
        """
        emit_event("session.beginning", {})

        session_id = f"session_{datetime.now().strftime('%Y%m%d_%H%M%S')}"

        session = SessionState(
            session_id=session_id,
            phase=SessionPhase.STARTING,
            start_time=datetime.now().isoformat(),
            active_goals=goals or []
        )

        # Load previous context if requested
        if load_previous_context:
            previous_context = await self._load_previous_session_context()
            session.preserved_state = previous_context
            session.context_loaded = True

        # Activate gardens based on goals
        if goals:
            recommended_gardens = self._recommend_gardens_for_goals(goals)
            session.active_gardens = recommended_gardens

        # Transition to active
        session.phase = SessionPhase.ACTIVE

        self.current_session = session

        # Store session start
        await self.memory.store(
            content=f"Session started: {session_id}",
            metadata={
                "type": "session_start",
                "session_id": session_id,
                "goals": goals or [],
                "start_time": session.start_time
            },
            tier="short_term"
        )

        emit_event("session.started", {
            "session_id": session_id,
            "goals": len(goals) if goals else 0
        })

        return session

    async def end_session(
        self,
        save_context: bool = True
    ) -> SessionSummary:
        """
        End current session with proper teardown.

        Args:
            save_context: Whether to save context for next session

        Returns:
            Session summary
        """
        if not self.current_session:
            raise ValueError("No active session to end")

        emit_event("session.ending", {
            "session_id": self.current_session.session_id
        })

        # Transition to winding down
        self.current_session.phase = SessionPhase.WINDING_DOWN

        # Calculate duration
        start_time = datetime.fromisoformat(self.current_session.start_time)
        end_time = datetime.now()
        duration_minutes = (end_time - start_time).total_seconds() / 60

        # Gather accomplishments
        goals_completed = await self._gather_completed_goals()
        goals_deferred = [
            g for g in self.current_session.active_goals
            if g not in goals_completed
        ]

        # Gather insights
        insights = await self._gather_session_insights()
        patterns = await self._gather_session_patterns()
        lessons = await self._gather_session_lessons()

        # Prepare next session context
        continuation_context = {}
        if save_context:
            continuation_context = await self._prepare_continuation_context()

        # Generate recommendations for next session
        recommended_start = self._recommend_next_session_start(goals_deferred)

        # Create summary
        summary = SessionSummary(
            session_id=self.current_session.session_id,
            duration_minutes=duration_minutes,
            goals_completed=goals_completed,
            goals_deferred=goals_deferred,
            insights_gained=insights,
            patterns_discovered=patterns,
            lessons_learned=lessons,
            continuation_context=continuation_context,
            recommended_start=recommended_start
        )

        # Store session summary
        await self.memory.store(
            content=f"Session ended: {summary.session_id} - {len(goals_completed)} goals completed",
            metadata={
                "type": "session_summary",
                "session_id": summary.session_id,
                "duration_minutes": duration_minutes,
                "goals_completed": len(goals_completed),
                "goals_deferred": len(goals_deferred)
            },
            tier="long_term"
        )

        # Track metrics
        track_metric(
            category="session",
            metric="duration_minutes",
            value=duration_minutes,
            context=summary.session_id
        )

        track_metric(
            category="session",
            metric="goals_completed",
            value=len(goals_completed),
            context=summary.session_id
        )

        # Transition to ended
        self.current_session.phase = SessionPhase.ENDING

        emit_event("session.ended", {
            "session_id": summary.session_id,
            "duration_minutes": duration_minutes,
            "goals_completed": len(goals_completed)
        })

        # Clear current session
        self.current_session = None

        return summary

    async def _load_previous_session_context(self) -> Dict[str, Any]:
        """Load context from previous session."""
        # Search for most recent session summary
        summaries = await self.memory.search(
            query="session_summary",
            tier="long_term",
            limit=1
        )

        if summaries:
            return summaries[0].metadata.get("continuation_context", {})

        return {}

    def _recommend_gardens_for_goals(self, goals: List[str]) -> List[str]:
        """Recommend gardens to activate based on goals."""
        gardens = []

        for goal in goals:
            goal_lower = goal.lower()

            if "debug" in goal_lower or "fix" in goal_lower:
                gardens.append("mystery")
            elif "optimize" in goal_lower or "performance" in goal_lower:
                gardens.append("adventure")
            elif "consolidate" in goal_lower or "organize" in goal_lower:
                gardens.append("truth")
            elif "create" in goal_lower or "build" in goal_lower:
                gardens.append("play")

        return list(set(gardens))  # Deduplicate

    async def _gather_completed_goals(self) -> List[str]:
        """Gather goals completed during session."""
        if not self.current_session:
            return []

        completed = []

        # Search for goal completion events
        events = await self.memory.search(
            query="goal completed",
            tier="short_term",
            limit=50
        )

        for event in events:
            goal = event.metadata.get("goal")
            if goal:
                completed.append(goal)

        return completed

    async def _gather_session_insights(self) -> List[str]:
        """Gather insights gained during session."""
        insights = await self.memory.search(
            query="insight discovered learned",
            tier="short_term",
            limit=20
        )

        return [i.content for i in insights]

    async def _gather_session_patterns(self) -> List[str]:
        """Gather patterns discovered during session."""
        patterns = await self.memory.search(
            query="pattern identified",
            tier="short_term",
            limit=20
        )

        return [p.content for p in patterns]

    async def _gather_session_lessons(self) -> List[str]:
        """Gather lessons learned during session."""
        lessons = await self.memory.search(
            query="lesson learned",
            tier="short_term",
            limit=20
        )

        return [l.content for l in lessons]

    async def _prepare_continuation_context(self) -> Dict[str, Any]:
        """Prepare context for next session."""
        context = {}

        # Include incomplete goals
        if self.current_session:
            context["goals"] = self.current_session.active_goals
            context["active_gardens"] = self.current_session.active_gardens

        # Include recent work context
        session_context = get_session_context(
            include_recent_memories=True
        )

        context["recent_work"] = session_context.get("recent_work", {})

        return context

    def _recommend_next_session_start(
        self,
        deferred_goals: List[str]
    ) -> List[str]:
        """Recommend how to start next session."""
        recommendations = []

        if deferred_goals:
            recommendations.append(
                f"Resume work on: {', '.join(deferred_goals[:3])}"
            )

        recommendations.append("Load previous session context")
        recommendations.append("Review session summary")

        return recommendations

# Example usage
async def main():
    manager = SessionBoundaryManager()

    # Begin session
    session = await manager.begin_session(
        goals=[
            "Complete grimoire enhancement Phase 4C",
            "Consolidate Eastern Quadrant work"
        ],
        load_previous_context=True
    )

    print(f"Session Started: {session.session_id}")
    print(f"  Goals: {len(session.active_goals)}")
    print(f"  Recommended gardens: {', '.join(session.active_gardens)}")

    # ... work happens ...

    # End session
    summary = await manager.end_session(save_context=True)

    print(f"\nSession Summary:")
    print(f"  Duration: {summary.duration_minutes:.1f} minutes")
    print(f"  Goals completed: {len(summary.goals_completed)}")
    print(f"  Goals deferred: {len(summary.goals_deferred)}")
    print(f"  Insights gained: {len(summary.insights_gained)}")
    print(f"  Patterns discovered: {len(summary.patterns_discovered)}")

    print(f"\nNext Session Recommendations:")
    for rec in summary.recommended_start:
        print(f"  • {rec}")
```

---

## 🌿 Garden Resonance

The **truth** garden brings the quality of honest assessment and clarity.

When working in this chapter, embody:
- **Honesty**: See what is, not what you wish
- **Discernment**: Separate valuable from noise
- **Integration**: Weave fragments into coherent understanding
- **Release**: Let go of what doesn't serve
- **Clarity**: Shine light on what remains

Truth in consolidation isn't harsh judgment—it's compassionate honesty that reveals what worked, what didn't, and why.

---

## 🔧 Troubleshooting

### Issue 1: Memory Consolidation Creates Too Many Clusters

**Symptoms**: Hundreds of small clusters instead of meaningful groups

**Diagnosis**:
```python
consolidator = MemoryConsolidator()
report = await consolidator.consolidate_recent_memories(min_cluster_size=5)

if report.clusters_formed > 50:
    print("⚠️ Too many clusters - increase min_cluster_size")
```

**Solutions**:
1. Increase `min_cluster_size` parameter (e.g., 3 → 5)
2. Use more aggressive similarity threshold
3. Enable cluster merging with lower overlap threshold
4. Use `SEMANTIC` strategy instead of `COMPREHENSIVE`

---

### Issue 2: Important Memories Being Discarded

**Symptoms**: Valuable context lost after consolidation

**Diagnosis**:
```python
# Check what's being discarded
report = await consolidator.consolidate_recent_memories()

for memory in report.memories_discarded:
    print(f"Discarding: {memory.content}")
    print(f"  Importance: {memory.metadata.get('importance')}")
```

**Solutions**:
1. Adjust importance threshold for discard decision
2. Manually mark important memories with high importance score
3. Review discard list before executing
4. Use archive instead of discard for borderline cases

---

### Issue 3: Pattern Extraction Missing Obvious Patterns

**Symptoms**: Clear patterns not being extracted

**Diagnosis**:
```python
extractor = PatternExtractor()
report = await extractor.extract_patterns_from_session()

if report.patterns_extracted < 3:
    print("⚠️ Few patterns extracted - check memory tagging")
```

**Solutions**:
1. Ensure memories tagged with problem/solution metadata
2. Use structured memory format for problem-solving work
3. Manually add lessons for complex patterns
4. Review extraction logic for specific pattern types

---

### Issue 4: Session State Not Preserving Across Boundaries

**Symptoms**: Context lost between sessions

**Diagnosis**:
```python
manager = SessionBoundaryManager()
summary = await manager.end_session(save_context=True)

if not summary.continuation_context:
    print("⚠️ No context saved for next session")
```

**Solutions**:
1. Verify `save_context=True` when ending session
2. Check continuation context stored in long-term memory
3. Ensure next session loads with `load_previous_context=True`
4. Manually review and enhance continuation context

---

### Issue 5: Consolidation Takes Too Long

**Symptoms**: Consolidation process times out or takes excessive time

**Diagnosis**:
```python
import time

start = time.time()
report = await consolidator.consolidate_recent_memories(hours_back=72)
duration = time.time() - start

if duration > 60:
    print(f"⚠️ Consolidation took {duration:.0f}s")
```

**Solutions**:
1. Reduce `hours_back` parameter (e.g., 72 → 24 hours)
2. Use `TEMPORAL` or `CONTEXTUAL` strategy instead of `COMPREHENSIVE`
3. Increase `min_cluster_size` to reduce cluster count
4. Run consolidation more frequently (smaller batches)

---

## ✨ Best Practices

1. **Consolidate Regularly**: Don't wait until overloaded - daily consolidation is ideal
2. **Preserve Before Discarding**: When uncertain, archive rather than discard
3. **Tag Strategically**: Use consistent tags to enable better clustering
4. **Extract Patterns Immediately**: Capture patterns while context is fresh
5. **Manage Session Boundaries**: Clean setup and teardown prevents context loss
6. **Review Consolidation Reports**: Manually verify consolidation quality
7. **Balance Synthesis and Detail**: Keep enough detail for future context
8. **Trust the Truth Garden**: Honest assessment enables growth

---

## ⏭️ Transitions

**From Chapter 6 (Tail/Adventure)**: High-velocity work complete, now consolidate gains.

**To Chapter 8 (Ghost/Mystery)**: Eastern quadrant complete, transitioning to Southern quadrant (Fire/Summer). From Yang Rising to Yang Peak—from foundation to expansion.

**Special transitions**:
- This is a **phase boundary** (Wood → Fire, Spring → Summer)
- Perfect time for comprehensive consolidation
- Natural pause before acceleration into Southern quadrant
- If overloaded with information → Extend consolidation time
- If insights missing → Review work before proceeding

---

## 🧭 Navigation

**Predecessor**: Chapter 6 (Tail/Adventure) - *Performance drive complete*
**Successor**: Chapter 8 (Ghost/Mystery) - *Fire phase begins*
**Quadrant Flow**: Seventh of Eastern Quadrant (Spring/Wood) - Position 7/7 - **QUADRANT COMPLETE**

**Phase Transition Note**: This chapter marks the boundary between Eastern (Wood/Spring/Yang Rising) and Southern (Fire/Summer/Yang Peak) quadrants. Take time here to integrate before accelerating into the expansion phase.

---

## 🌾 The WinnowingBasket's Wisdom

> *"Not all that glitters is gold; not all output is wisdom. The WinnowingBasket teaches: shake vigorously, let the chaff fly away, keep only the grain that nourishes. Consolidation is not about hoarding—it's about distilling essence from experience."*

**Consolidation Principles**:
- **Separate regularly**: Don't wait until overloaded
- **Extract patterns**: Lessons are more valuable than facts
- **Release freely**: Let go of what doesn't serve
- **Integrate deeply**: Weave insights into understanding
- **Document honestly**: Truth enables growth

**The Consolidation Test**:
Before proceeding to Chapter 8 (phase transition), verify:
1. Recent memories consolidated
2. Key lessons captured
3. Metrics recorded
4. Insights integrated
5. Ready for next phase

If all verified → Proceed to Chapter 8 (Fire phase begins)
If consolidation incomplete → Complete before transition

---

**Next Chapter**: [Chapter 8: Metrics & Introspection →](08_GHOST_METRICS_INTROSPECTION.md) *(Southern Quadrant begins)*
**Previous Chapter**: [Chapter 6: Performance Drive](06_TAIL_PERFORMANCE_DRIVE.md)
**Quadrant**: Eastern (Spring/Wood) - **COMPLETE** ✅
**Next Quadrant**: Southern (Summer/Fire) - Yang Peak Phase

---

*"Shake the basket. Release the chaff. Keep the grain. 🌾"*
