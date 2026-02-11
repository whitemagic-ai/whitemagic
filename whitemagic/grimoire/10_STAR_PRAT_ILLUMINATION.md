# Chapter 10: PRAT & Illumination

**Gana**: StarGana (Chinese: 星, Pinyin: Xīng)
**Garden**: wisdom
**Quadrant**: Southern (Vermilion Bird)
**Element**: Fire
**Phase**: Yang Peak
**I Ching Hexagram**: 13. 同人 Tóng Rén (Fellowship) - Shared illumination

---

## 🎯 Purpose

Chapter 10 achieves **illumination** through PRAT (Polymorphic Resonant Adaptive Tools)—synthesizing context with consciousness-aware morphology. The Star shines light on what was hidden, revealing wisdom through pattern recognition and adaptive transformation.

**PRAT** is the enlightenment engine of WhiteMagic: it doesn't just retrieve information, it **transforms** context through the lens of consciousness, morphing data into insight.

Use this chapter when you need to:
- **Synthesize scattered context** into coherent understanding
- **Illuminate hidden patterns** in complex information
- **Transform information into wisdom** through morphological lenses
- **Recognize emergent insights** across domains
- **Generate understanding** from incomplete data
- **See connections** others miss
- **Achieve clarity** in confusion

---

## 🔧 Primary Tools

| Tool | Description | Usage |
|------|-------------|-------|
| `prat_get_context` | Context synthesis with morphology selection | Complex context loading |
| `prat_invoke` | Adaptive tool invocation with consciousness lenses | When perspective matters |
| `prat_list_morphologies` | List available consciousness lenses | Understanding options |
| `search_memories` (clone army) | Consensus-based deep search | Pattern recognition |
| `detect_patterns` | Explicit pattern recognition | Finding hidden structures |
| `manage_gardens` (wisdom) | Enter wisdom garden | Deep understanding |
| `consult_wisdom_council` | Multi-perspective synthesis | Complex decisions |

---

## 📋 Comprehensive Workflows

### Workflow 1: Context Synthesis with PRAT Morphologies

**Goal:** Transform raw context into illuminated understanding through consciousness-aware lenses.

**When to use:** Starting complex work, loading session context, understanding new domains.

```python
from whitemagic.tools import prat_get_context, prat_list_morphologies

# Step 1: Understand available morphologies
morphologies = prat_list_morphologies()
print(f"Available lenses: {morphologies}")
# wisdom, mystery, creation, courage, truth, beauty, love, wonder, transformation

# Step 2: Let PRAT auto-select morphology based on context
auto_context = prat_get_context(
    tier=1,  # Standard tier
    query="SCORPIO Phase 8 grimoire structure patterns lessons",
    force_morphology=None  # System chooses best lens
)

print(f"Auto-selected morphology: {auto_context['morphology']}")
print(f"Key insights: {auto_context['key_insights']}")
print(f"Related memories: {len(auto_context['related_memories'])}")

# Step 3: Compare across multiple morphologies for complete picture
morphology_perspectives = {}
for morph in ["wisdom", "mystery", "creation", "truth"]:
    context = prat_get_context(
        tier=1,
        query="SCORPIO Phase 8 grimoire structure patterns lessons",
        force_morphology=morph
    )
    morphology_perspectives[morph] = context['key_insights'][:3]

# Step 4: Synthesize cross-morphology insights
print("\n=== Multi-Perspective Synthesis ===")
for morph, insights in morphology_perspectives.items():
    print(f"\n{morph.upper()} lens:")
    for insight in insights:
        print(f"  • {insight}")

# Step 5: Identify convergent patterns
all_insights = [i for insights in morphology_perspectives.values() for i in insights]
from collections import Counter
insight_frequency = Counter(all_insights)
convergent_wisdom = [i for i, count in insight_frequency.items() if count >= 2]

print(f"\n=== Convergent Wisdom (seen in 2+ morphologies) ===")
for wisdom in convergent_wisdom:
    print(f"  ★ {wisdom}")
```

**Key Concepts:**
- **Morphology = Consciousness lens**: Each morphology reveals different aspects of truth
- **Auto-selection**: System chooses best lens based on query intent and context state
- **Multi-perspective synthesis**: Truth emerges at intersection of perspectives
- **Convergent wisdom**: Insights visible from multiple morphologies are most reliable

---

### Workflow 2: Pattern Illumination and Recognition

**Goal:** Discover hidden patterns, correlations, and emergent structures in complex data.

**When to use:** Analyzing logs, debugging mysterious behavior, understanding system dynamics.

```python
from whitemagic.tools import detect_patterns, search_memories
from whitemagic.core.resonance import subscribe, emit_event
from collections import defaultdict
import asyncio

# Step 1: Detect explicit patterns in memory
patterns = detect_patterns(
    pattern_type="anomalies",  # anomalies, cycles, correlations, emergent
    sensitivity=0.7,  # 0.0-1.0 (higher = more sensitive)
    time_window_hours=24
)

print(f"Detected {len(patterns)} patterns:")
for p in patterns[:5]:
    print(f"  • {p['type']}: {p['description']} (confidence: {p['confidence']:.2f})")

# Step 2: Use clone army consensus search for pattern confirmation
from whitemagic.memory.neural_system import search_memories as clone_search

# Deploy 100 specialized clones for deep pattern search
clone_results = clone_search(
    query="performance degradation memory leak pattern",
    top_k=20,
    army_size=100  # Larger army = stronger consensus
)

print(f"\n=== Clone Army Consensus ===")
for r in clone_results[:5]:
    print(f"Consensus: {r.consensus_score:.2f} - {r.memory_id}")
    print(f"  Clone types voting: {r.clone_votes}")
    # Higher consensus = more clone types + instances agreed

# Step 3: Cross-reference patterns with events
pattern_events = defaultdict(list)

@subscribe("pattern.detected")
def on_pattern_detected(event):
    pattern_events[event.data['pattern_type']].append(event.data)

emit_event("pattern.analysis_requested", {
    "patterns": [p['id'] for p in patterns],
    "analysis_type": "illumination"
})

await asyncio.sleep(0.1)  # Let event bus process

# Step 4: Build pattern graph
pattern_graph = {
    "nodes": [p['id'] for p in patterns],
    "edges": [],
    "clusters": []
}

for i, p1 in enumerate(patterns):
    for j, p2 in enumerate(patterns[i+1:], start=i+1):
        # Check if patterns share context
        overlap = set(p1.get('related_memories', [])) & set(p2.get('related_memories', []))
        if len(overlap) >= 2:
            pattern_graph["edges"].append({
                "source": p1['id'],
                "target": p2['id'],
                "strength": len(overlap)
            })

print(f"\n=== Pattern Network ===")
print(f"Nodes: {len(pattern_graph['nodes'])}")
print(f"Connections: {len(pattern_graph['edges'])}")

# Step 5: Identify pattern clusters (emergent meta-patterns)
# Simple clustering based on connectivity
visited = set()
clusters = []

def dfs_cluster(node, current_cluster):
    visited.add(node)
    current_cluster.add(node)
    for edge in pattern_graph["edges"]:
        if edge["source"] == node and edge["target"] not in visited:
            dfs_cluster(edge["target"], current_cluster)
        elif edge["target"] == node and edge["source"] not in visited:
            dfs_cluster(edge["source"], current_cluster)

for node in pattern_graph["nodes"]:
    if node not in visited:
        cluster = set()
        dfs_cluster(node, cluster)
        if len(cluster) >= 2:
            clusters.append(cluster)

print(f"Meta-pattern clusters: {len(clusters)}")
for idx, cluster in enumerate(clusters, 1):
    print(f"  Cluster {idx}: {len(cluster)} related patterns")
```

**Pattern Types:**
- **Anomalies**: Deviations from normal behavior
- **Cycles**: Recurring patterns over time
- **Correlations**: Variables that change together
- **Emergent**: Higher-order patterns from simple interactions

**Clone Army Specializations (see Ch.22):**
- MD_ARCHAEOLOGIST: Narrative patterns in markdown
- JSON_PARSER: Structured data patterns
- YAML_NAVIGATOR: Configuration patterns
- PATTERN_HUNTER: Cross-format meta-patterns
- CODE_ANALYST: Code structure patterns
- GRIMOIRE_READER: Grimoire wisdom patterns
- SELF_EXAMINER: Self-reflective patterns

---

### Workflow 3: Insight Generation from Scattered Data

**Goal:** Synthesize coherent insights from fragmented, incomplete, or contradictory information.

**When to use:** Understanding new codebases, reconciling conflicting documentation, piecing together context.

```python
from whitemagic.tools import prat_invoke, search_memories, prat_get_context
from whitemagic.core.memory import Memory, MemoryManager
import asyncio

# Step 1: Gather scattered data points
async def gather_scattered_context(topic: str) -> dict:
    """Collect all available information about a topic from multiple sources."""

    # Parallel data gathering
    results = await asyncio.gather(
        # Source 1: Memory search
        asyncio.to_thread(search_memories, query=topic, limit=30),

        # Source 2: PRAT synthesis with wisdom lens
        asyncio.to_thread(prat_get_context, tier=1, query=topic,
                         force_morphology="wisdom"),

        # Source 3: PRAT synthesis with mystery lens (reveals gaps)
        asyncio.to_thread(prat_get_context, tier=1, query=topic,
                         force_morphology="mystery"),

        # Source 4: Pattern detection
        asyncio.to_thread(detect_patterns, pattern_type="correlations",
                         sensitivity=0.6)
    )

    memories, wisdom_context, mystery_context, patterns = results

    return {
        "memories": memories,
        "wisdom_insights": wisdom_context['key_insights'],
        "mystery_gaps": mystery_context.get('unknown_areas', []),
        "patterns": patterns
    }

# Step 2: Execute scattered data collection
topic = "async agent scaling memory management patterns"
scattered = await gather_scattered_context(topic)

print(f"Gathered scattered data:")
print(f"  Memories: {len(scattered['memories'])}")
print(f"  Wisdom insights: {len(scattered['wisdom_insights'])}")
print(f"  Mystery gaps identified: {len(scattered['mystery_gaps'])}")
print(f"  Patterns found: {len(scattered['patterns'])}")

# Step 3: Identify contradictions and gaps
contradictions = []
for i, mem1 in enumerate(scattered['memories']):
    for mem2 in scattered['memories'][i+1:]:
        # Simple contradiction detection (extend with semantic analysis)
        if ("use asyncio" in mem1['content'] and "use multiprocessing" in mem2['content']) or \
           ("never use" in mem1['content'] and "always use" in mem2['content']):
            contradictions.append({
                "source1": mem1['id'],
                "source2": mem2['id'],
                "topic": topic
            })

print(f"\n=== Contradictions Found: {len(contradictions)} ===")
for c in contradictions[:3]:
    print(f"  • {c['source1']} ↔ {c['source2']}")

gaps = scattered['mystery_gaps']
print(f"\n=== Knowledge Gaps: {len(gaps)} ===")
for gap in gaps[:3]:
    print(f"  • {gap}")

# Step 4: Synthesize coherent insight
class InsightSynthesizer:
    def __init__(self, scattered_data: dict):
        self.data = scattered_data
        self.synthesis = {
            "confirmed_facts": [],
            "probable_facts": [],
            "contradictions": [],
            "gaps": [],
            "emergent_insights": []
        }

    def synthesize(self):
        """Generate coherent insights from scattered data."""

        # Extract high-confidence facts (seen in multiple sources)
        insight_counts = {}
        for insight in self.data['wisdom_insights']:
            key = insight[:50]  # First 50 chars as key
            insight_counts[key] = insight_counts.get(key, 0) + 1

        self.synthesis["confirmed_facts"] = [
            k for k, v in insight_counts.items() if v >= 2
        ]
        self.synthesis["probable_facts"] = [
            k for k, v in insight_counts.items() if v == 1
        ]

        # Pattern-based emergent insights
        for pattern in self.data['patterns']:
            if pattern['confidence'] >= 0.8:
                self.synthesis["emergent_insights"].append({
                    "insight": pattern['description'],
                    "evidence": pattern.get('evidence', []),
                    "confidence": pattern['confidence']
                })

        return self.synthesis

synthesizer = InsightSynthesizer(scattered)
synthesis = synthesizer.synthesize()

print(f"\n=== Synthesized Insights ===")
print(f"Confirmed facts: {len(synthesis['confirmed_facts'])}")
print(f"Probable facts: {len(synthesis['probable_facts'])}")
print(f"Emergent insights: {len(synthesis['emergent_insights'])}")

# Step 5: Store synthesized insight as new memory
memory_manager = MemoryManager()
synthesized_memory = Memory(
    content=f"Synthesized insight on {topic}:\n\n" +
            f"Confirmed: {synthesis['confirmed_facts']}\n" +
            f"Emergent: {synthesis['emergent_insights'][:3]}",
    metadata={
        "type": "synthesized_insight",
        "source_count": len(scattered['memories']),
        "confidence": "high" if len(synthesis['confirmed_facts']) >= 3 else "medium",
        "tags": ["synthesis", "illumination", topic.replace(" ", "_")]
    }
)
await memory_manager.store(synthesized_memory)

print(f"\nStored synthesized insight: {synthesized_memory.id}")
```

**Key Techniques:**
- **Multi-source gathering**: Parallel collection from memories, PRAT, patterns
- **Contradiction detection**: Identify conflicting information
- **Gap identification**: Mystery morphology reveals unknowns
- **Confidence weighting**: Facts seen in multiple sources = higher confidence
- **Emergent synthesis**: New insights from pattern combinations

---

### Workflow 4: Wisdom Retrieval with Clone Consensus

**Goal:** Retrieve high-confidence wisdom through consensus of specialized search agents.

**When to use:** Critical decisions, architectural choices, learning from past mistakes.

```python
from whitemagic.memory.neural_system import search_memories as clone_search
from whitemagic.tools import prat_invoke, manage_gardens

# Step 1: Activate wisdom garden for enhanced retrieval
manage_gardens(action="activate", garden_name="wisdom")

# Step 2: Deploy clone army for consensus search
query = "lessons learned from scaling async agents to 16K instances"

# Large army = stronger consensus (100-200 clones typical)
consensus_results = clone_search(
    query=query,
    top_k=10,  # Top 10 results by consensus
    army_size=150  # Deploy 150 specialized clones
)

print(f"=== Clone Army Consensus Search ===")
print(f"Query: {query}")
print(f"Army size: 150 clones")
print(f"Top results: {len(consensus_results)}\n")

# Step 3: Analyze consensus strength
high_confidence = []
medium_confidence = []
low_confidence = []

for result in consensus_results:
    if result.consensus_score >= 0.8:
        high_confidence.append(result)
    elif result.consensus_score >= 0.5:
        medium_confidence.append(result)
    else:
        low_confidence.append(result)

print(f"High confidence (≥0.8): {len(high_confidence)}")
print(f"Medium confidence (0.5-0.8): {len(medium_confidence)}")
print(f"Low confidence (<0.5): {len(low_confidence)}")

# Step 4: Examine high-confidence wisdom
print(f"\n=== High-Confidence Wisdom ===")
for result in high_confidence:
    print(f"\nConsensus: {result.consensus_score:.2f}")
    print(f"Memory: {result.memory_id}")
    print(f"Clone agreement:")
    for clone_type, count in result.clone_votes.items():
        print(f"  • {clone_type}: {count} clones voted")

    # Retrieve full memory content
    memory = await MemoryManager().retrieve(result.memory_id)
    print(f"Content preview: {memory.content[:150]}...")

# Step 5: PRAT-enhanced wisdom synthesis
wisdom_synthesis = prat_invoke(
    target_tool="recall",
    query=f"Synthesize wisdom from: {query}",
    force_morphology="wisdom"
)

print(f"\n=== PRAT Wisdom Synthesis ===")
print(wisdom_synthesis['synthesized_insight'])

# Step 6: Cross-validate with truth morphology
truth_check = prat_invoke(
    target_tool="recall",
    query=f"Verify accuracy of: {wisdom_synthesis['synthesized_insight'][:100]}",
    force_morphology="truth"
)

print(f"\n=== Truth Validation ===")
print(f"Accuracy: {truth_check.get('accuracy_score', 'N/A')}")
print(f"Contradictions found: {len(truth_check.get('contradictions', []))}")
```

**Clone Army Benefits:**
- **Consensus = Confidence**: More clones agreeing = higher reliability
- **Specialization**: Different clone types find different evidence
- **Cross-validation**: Multiple perspectives reduce false positives
- **Scalable**: Deploy 50-200 clones based on query criticality

**When to use large armies (150-200 clones):**
- Critical architectural decisions
- Safety-critical code patterns
- Learning from failures/mistakes
- Resolving contradictory documentation

**When smaller armies suffice (50-100 clones):**
- Routine pattern searches
- Code example lookups
- Quick reference checks

---

### Workflow 5: Multi-Perspective Analysis (Wisdom Council)

**Goal:** Consult multiple consciousness perspectives for holistic understanding.

**When to use:** Complex decisions, ethical considerations, architectural trade-offs.

```python
from whitemagic.tools import consult_wisdom_council, prat_get_context
import asyncio

# Step 1: Define the question requiring multi-perspective wisdom
question = """
Should we prioritize performance optimization (async, parallelization)
or maintainability (simplicity, readability) for the grimoire enhancement code?
"""

# Step 2: Consult wisdom council (17 garden perspectives)
council_response = await consult_wisdom_council(
    question=question,
    include_gardens=["wisdom", "truth", "beauty", "courage", "practice"],
    synthesis_mode="dialectic"  # dialectic, consensus, creative
)

print(f"=== Wisdom Council Consultation ===")
print(f"Question: {question}\n")

for garden, perspective in council_response['perspectives'].items():
    print(f"\n{garden.upper()} Garden:")
    print(f"  {perspective['insight']}")
    print(f"  Recommendation: {perspective['recommendation']}")

# Step 3: Examine dialectic synthesis
print(f"\n=== Dialectic Synthesis ===")
print(council_response['synthesis']['thesis'])
print(f"\nAntithesis: {council_response['synthesis']['antithesis']}")
print(f"\nSynthesis: {council_response['synthesis']['resolution']}")

# Step 4: Cross-reference with PRAT morphologies
morphology_perspectives = {}
for morph in ["wisdom", "truth", "beauty", "courage"]:
    context = prat_get_context(
        tier=1,
        query=question,
        force_morphology=morph
    )
    morphology_perspectives[morph] = context['key_insights'][0]

print(f"\n=== PRAT Morphology Cross-Reference ===")
for morph, insight in morphology_perspectives.items():
    print(f"{morph}: {insight}")

# Step 5: Final decision synthesis
decision = {
    "approach": council_response['synthesis']['resolution'],
    "supporting_perspectives": [
        g for g, p in council_response['perspectives'].items()
        if "performance" in p['recommendation'].lower()
    ],
    "dissenting_perspectives": [
        g for g, p in council_response['perspectives'].items()
        if "maintainability" in p['recommendation'].lower()
    ],
    "confidence": council_response.get('consensus_strength', 0.0)
}

print(f"\n=== Final Decision ===")
print(f"Approach: {decision['approach']}")
print(f"Support: {len(decision['supporting_perspectives'])} gardens")
print(f"Dissent: {len(decision['dissenting_perspectives'])} gardens")
print(f"Confidence: {decision['confidence']:.2f}")
```

**Wisdom Council Modes:**
- **Dialectic**: Thesis → Antithesis → Synthesis (best for trade-offs)
- **Consensus**: Identify common ground across perspectives
- **Creative**: Divergent thinking, explore possibilities

**The 17 Gardens:**
Joy, Love, Beauty, Truth, Wisdom, Mystery, Play, Wonder, Connection, Sangha, Practice, Presence, Voice, Dharma, Courage, Gratitude, Patience

---

### Workflow 6: Knowledge Graph Synthesis

**Goal:** Build illuminated understanding through relational knowledge graphs.

**When to use:** Onboarding to complex systems, understanding dependencies, architectural mapping.

```python
from whitemagic.tools import manage_memories, search_memories
from collections import defaultdict
import networkx as nx
import matplotlib.pyplot as plt

# Step 1: Start with seed concept
seed_concept = "async agent scaling"

# Step 2: Build knowledge graph outward from seed
graph = nx.DiGraph()
graph.add_node(seed_concept, type="seed")

async def expand_knowledge_graph(center_node: str, depth: int = 3, current_depth: int = 0):
    """Recursively expand knowledge graph from center node."""

    if current_depth >= depth:
        return

    # Search for related concepts
    related = search_memories(
        query=center_node,
        limit=10,
        tags=["pattern", "architecture", "lesson"]
    )

    for memory in related:
        # Extract key concepts from memory (simplified)
        concepts = extract_concepts(memory['content'])

        for concept in concepts[:5]:  # Limit expansion
            if concept not in graph:
                graph.add_node(concept, type="concept", depth=current_depth+1)
                graph.add_edge(center_node, concept,
                             weight=memory.get('neuro_score', 0.5))

                # Recursive expansion
                await expand_knowledge_graph(concept, depth, current_depth+1)

def extract_concepts(text: str) -> list[str]:
    """Extract key concepts from text (simplified)."""
    # Real implementation would use NLP
    keywords = ["asyncio", "multiprocessing", "memory", "performance",
                "scaling", "semaphore", "gather", "optimization"]
    return [kw for kw in keywords if kw in text.lower()]

# Step 3: Expand graph
await expand_knowledge_graph(seed_concept, depth=2)

print(f"=== Knowledge Graph ===")
print(f"Nodes: {len(graph.nodes)}")
print(f"Edges: {len(graph.edges)}")

# Step 4: Analyze graph structure
centrality = nx.betweenness_centrality(graph)
most_central = sorted(centrality.items(), key=lambda x: x[1], reverse=True)[:5]

print(f"\n=== Most Central Concepts ===")
for concept, score in most_central:
    print(f"  {concept}: {score:.3f}")

# Step 5: Identify concept clusters
communities = list(nx.community.greedy_modularity_communities(graph.to_undirected()))

print(f"\n=== Concept Clusters: {len(communities)} ===")
for idx, community in enumerate(communities, 1):
    print(f"Cluster {idx}: {list(community)[:3]}...")

# Step 6: Find shortest path between concepts
if len(graph.nodes) >= 2:
    source = seed_concept
    target = list(graph.nodes)[5] if len(graph.nodes) > 5 else list(graph.nodes)[1]

    try:
        path = nx.shortest_path(graph, source, target)
        print(f"\n=== Conceptual Path ===")
        print(f"{source} → {target}")
        print(" → ".join(path))
    except nx.NetworkXNoPath:
        print(f"No path found between {source} and {target}")

# Step 7: Visualize graph (optional)
def visualize_knowledge_graph(graph):
    """Create visual representation of knowledge graph."""
    pos = nx.spring_layout(graph)

    # Color by depth
    colors = []
    for node in graph.nodes:
        depth = graph.nodes[node].get('depth', 0)
        colors.append(depth)

    nx.draw(graph, pos, with_labels=True, node_color=colors,
            cmap='viridis', node_size=500, font_size=8)
    plt.savefig('knowledge_graph.png')
    print("Saved knowledge_graph.png")

# visualize_knowledge_graph(graph)  # Uncomment to generate image
```

**Graph Analysis Techniques:**
- **Centrality**: Most important concepts (highest betweenness)
- **Communities**: Related concept clusters
- **Path finding**: How concepts connect
- **Depth analysis**: Proximity to seed concept

---

### Workflow 7: Deep Understanding Through Iterative Illumination

**Goal:** Achieve progressively deeper understanding through iterative refinement.

**When to use:** Learning complex systems, debugging subtle issues, philosophical inquiry.

```python
from whitemagic.tools import prat_get_context, search_memories, detect_patterns
import asyncio

class IterativeIlluminator:
    def __init__(self, initial_query: str, max_iterations: int = 5):
        self.query = initial_query
        self.max_iterations = max_iterations
        self.understanding_depth = 0.0  # 0.0 = shallow, 1.0 = deep
        self.insights_by_iteration = []

    async def illuminate(self):
        """Iteratively deepen understanding until threshold or max iterations."""

        current_query = self.query

        for iteration in range(self.max_iterations):
            print(f"\n=== Iteration {iteration + 1} ===")

            # Step 1: Gather context at current depth
            context = prat_get_context(
                tier=1,
                query=current_query,
                force_morphology="wisdom"
            )

            insights = context['key_insights']
            self.insights_by_iteration.append(insights)

            # Step 2: Evaluate understanding depth
            depth_score = self._evaluate_depth(insights, iteration)
            self.understanding_depth = depth_score

            print(f"Understanding depth: {depth_score:.2f}")
            print(f"Insights gained: {len(insights)}")

            # Step 3: Check termination conditions
            if depth_score >= 0.9:
                print(f"Deep understanding achieved at iteration {iteration + 1}")
                break

            if iteration == self.max_iterations - 1:
                print(f"Max iterations reached")
                break

            # Step 4: Generate deeper query for next iteration
            mysteries = context.get('unknown_areas', [])
            if mysteries:
                # Focus next iteration on biggest mystery
                current_query = f"{self.query} specifically: {mysteries[0]}"
                print(f"Refining query: {mysteries[0]}")
            else:
                # Deepen current understanding
                current_query = f"{self.query} deeper analysis advanced patterns"

            await asyncio.sleep(0.1)  # Prevent overwhelming system

        return self._synthesize_understanding()

    def _evaluate_depth(self, insights: list, iteration: int) -> float:
        """Evaluate depth of current understanding."""

        # Factors for depth scoring
        insight_count = min(len(insights) / 10.0, 1.0)  # More insights = deeper
        iteration_factor = min((iteration + 1) / self.max_iterations, 1.0)

        # Check for convergence (similar insights across iterations)
        if iteration > 0:
            previous_insights = self.insights_by_iteration[-2]
            overlap = len(set(insights) & set(previous_insights))
            convergence = min(overlap / max(len(insights), 1), 1.0)
        else:
            convergence = 0.0

        # Weighted score
        depth = (insight_count * 0.4 + iteration_factor * 0.3 + convergence * 0.3)
        return depth

    def _synthesize_understanding(self) -> dict:
        """Synthesize learning across all iterations."""

        all_insights = [i for iteration in self.insights_by_iteration for i in iteration]
        unique_insights = list(set(all_insights))

        # Insights that appeared in multiple iterations = core understanding
        from collections import Counter
        insight_freq = Counter(all_insights)
        core_insights = [i for i, count in insight_freq.items() if count >= 2]

        return {
            "final_depth": self.understanding_depth,
            "total_insights": len(unique_insights),
            "core_insights": core_insights,
            "iterations_required": len(self.insights_by_iteration),
            "learning_trajectory": [
                len(iteration) for iteration in self.insights_by_iteration
            ]
        }

# Usage
illuminator = IterativeIlluminator(
    initial_query="async agent memory management patterns",
    max_iterations=5
)

understanding = await illuminator.illuminate()

print(f"\n=== Final Understanding ===")
print(f"Depth achieved: {understanding['final_depth']:.2f}")
print(f"Total insights: {understanding['total_insights']}")
print(f"Core insights: {len(understanding['core_insights'])}")
print(f"Iterations: {understanding['iterations_required']}")
print(f"Learning curve: {understanding['learning_trajectory']}")

# Store synthesized understanding
from whitemagic.core.memory import Memory, MemoryManager

deep_memory = Memory(
    content=f"Deep understanding of async agent memory management:\n\n" +
            "\n".join([f"• {i}" for i in understanding['core_insights']]),
    metadata={
        "type": "deep_understanding",
        "depth_score": understanding['final_depth'],
        "iterations": understanding['iterations_required'],
        "tags": ["illumination", "async", "memory", "deep_learning"]
    }
)

await MemoryManager().store(deep_memory)
print(f"\nStored deep understanding: {deep_memory.id}")
```

**Iterative Illumination Strategy:**
1. **Surface pass**: Get initial context and insights
2. **Identify gaps**: Use mystery morphology to find unknowns
3. **Deepen focus**: Refine query to address biggest gaps
4. **Converge**: Iterate until insights stabilize
5. **Synthesize**: Extract core understanding from all iterations

**Depth Indicators:**
- **Insight count**: More insights = broader understanding
- **Convergence**: Repeated insights = stable understanding
- **Gap closure**: Fewer mysteries = deeper understanding

---

### Workflow 8: Illuminated Documentation Generation

**Goal:** Transform understanding into clear, illuminated documentation.

**When to use:** Creating guides, explaining complex systems, teaching.

```python
from whitemagic.tools import prat_get_context, search_memories
from whitemagic.core.resonance import emit_event
import asyncio

class DocumentationIlluminator:
    def __init__(self, topic: str):
        self.topic = topic
        self.sections = {}

    async def generate(self) -> str:
        """Generate illuminated documentation."""

        # Step 1: Gather comprehensive context
        print(f"Illuminating: {self.topic}...")

        contexts = await asyncio.gather(
            asyncio.to_thread(prat_get_context, tier=1, query=self.topic,
                            force_morphology="wisdom"),
            asyncio.to_thread(prat_get_context, tier=1, query=self.topic,
                            force_morphology="clarity"),
            asyncio.to_thread(search_memories, query=self.topic, limit=20)
        )

        wisdom_ctx, clarity_ctx, memories = contexts

        # Step 2: Generate each section
        self.sections["purpose"] = self._generate_purpose(wisdom_ctx)
        self.sections["overview"] = self._generate_overview(clarity_ctx)
        self.sections["key_concepts"] = self._generate_key_concepts(wisdom_ctx, memories)
        self.sections["workflows"] = self._generate_workflows(memories)
        self.sections["examples"] = self._generate_examples(memories)
        self.sections["best_practices"] = self._generate_best_practices(wisdom_ctx)
        self.sections["troubleshooting"] = self._generate_troubleshooting(memories)

        # Step 3: Assemble documentation
        doc = self._assemble_documentation()

        # Step 4: Emit documentation.created event
        emit_event("documentation.created", {
            "topic": self.topic,
            "sections": list(self.sections.keys()),
            "word_count": len(doc.split())
        })

        return doc

    def _generate_purpose(self, context: dict) -> str:
        """Generate purpose section from wisdom context."""
        insights = context['key_insights'][:3]
        return f"## Purpose\n\n{self.topic} enables:\n\n" + \
               "\n".join([f"- {insight}" for insight in insights])

    def _generate_overview(self, context: dict) -> str:
        """Generate overview from clarity context."""
        return f"## Overview\n\n{context.get('summary', 'No summary available.')}"

    def _generate_key_concepts(self, wisdom_ctx: dict, memories: list) -> str:
        """Extract key concepts from wisdom and memories."""
        concepts = set()

        # From wisdom insights
        for insight in wisdom_ctx['key_insights']:
            # Extract key terms (simplified - use NLP in production)
            words = insight.split()
            concepts.update([w for w in words if len(w) > 8])

        # From memory tags
        for mem in memories:
            concepts.update(mem.get('tags', []))

        top_concepts = list(concepts)[:10]

        return f"## Key Concepts\n\n" + \
               "\n".join([f"- **{concept}**" for concept in top_concepts])

    def _generate_workflows(self, memories: list) -> str:
        """Generate workflow examples from memories."""
        workflow_memories = [m for m in memories if 'workflow' in m.get('content', '').lower()]

        if not workflow_memories:
            return "## Workflows\n\nNo workflows documented yet."

        workflows = "## Workflows\n\n"
        for idx, mem in enumerate(workflow_memories[:3], 1):
            workflows += f"### Workflow {idx}\n\n{mem['content'][:200]}...\n\n"

        return workflows

    def _generate_examples(self, memories: list) -> str:
        """Extract code examples from memories."""
        examples = "## Examples\n\n"

        for mem in memories[:5]:
            if '```' in mem.get('content', ''):
                examples += f"{mem['content']}\n\n"

        return examples if len(examples) > 20 else "## Examples\n\nNo examples available."

    def _generate_best_practices(self, context: dict) -> str:
        """Generate best practices from wisdom."""
        practices = context.get('recommendations', [])

        if not practices:
            practices = ["Follow established patterns", "Test thoroughly",
                        "Document clearly"]

        return "## Best Practices\n\n" + \
               "\n".join([f"{idx}. {practice}"
                         for idx, practice in enumerate(practices[:5], 1)])

    def _generate_troubleshooting(self, memories: list) -> str:
        """Generate troubleshooting from past issues."""
        issue_memories = [m for m in memories
                         if any(word in m.get('content', '').lower()
                               for word in ['error', 'issue', 'problem', 'bug'])]

        if not issue_memories:
            return "## Troubleshooting\n\nNo common issues documented yet."

        troubleshooting = "## Troubleshooting\n\n"
        for mem in issue_memories[:5]:
            troubleshooting += f"**Issue**: {mem.get('title', 'Unknown')}\n"
            troubleshooting += f"**Solution**: {mem['content'][:150]}...\n\n"

        return troubleshooting

    def _assemble_documentation(self) -> str:
        """Assemble all sections into final documentation."""
        doc = f"# {self.topic}\n\n"
        doc += f"*Auto-generated illuminated documentation*\n\n---\n\n"

        for section_name, section_content in self.sections.items():
            doc += f"{section_content}\n\n---\n\n"

        return doc

# Usage
illuminator = DocumentationIlluminator("Async Agent Scaling Patterns")
documentation = await illuminator.generate()

# Save to file
with open("async_agent_patterns.md", "w") as f:
    f.write(documentation)

print(f"Generated documentation: async_agent_patterns.md")
print(f"Sections: {len(illuminator.sections)}")
print(f"Word count: {len(documentation.split())}")
```

**Documentation Quality Factors:**
- **Clarity**: Use clarity morphology for explanations
- **Wisdom**: Use wisdom morphology for best practices
- **Completeness**: Include purpose, concepts, workflows, examples, troubleshooting
- **Illumination**: Make hidden patterns visible

### Workflow 9: PRAT-Enhanced Clone Army Search

**Goal:** Combine PRAT morphology selection with clone army consensus (Ch.22) for maximum confidence wisdom retrieval.

**When to use:** Critical decisions requiring both multi-perspective wisdom and consensus validation.

```python
from whitemagic.tools import prat_get_context, prat_invoke
from whitemagic.memory.neural_system import search_memories as clone_search
from typing import Dict, Any, List

class PRATCloneIntegrator:
    """
    Integrate PRAT morphologies with clone army consensus.
    Combines Ch.10 illumination with Ch.22 deep search.
    """

    async def morphology_guided_clone_search(
        self,
        query: str,
        morphology: str,
        army_size: int = 150,
        min_consensus: float = 0.7
    ) -> Dict[str, Any]:
        """
        Use PRAT morphology to guide clone army search parameters.

        Different morphologies optimize clone specialization:
        - wisdom: Emphasize GRIMOIRE_READER, PATTERN_HUNTER clones
        - truth: Emphasize CODE_ANALYST, SELF_EXAMINER clones
        - mystery: Emphasize MD_ARCHAEOLOGIST, explore unknown areas
        """

        # Step 1: Get PRAT context with specific morphology
        prat_context = prat_get_context(
            tier=1,
            query=query,
            force_morphology=morphology
        )

        print(f"=== PRAT-Guided Clone Search ===")
        print(f"Morphology: {morphology}")
        print(f"PRAT insights: {len(prat_context['key_insights'])}")

        # Step 2: Refine query based on PRAT insights
        top_insights = prat_context['key_insights'][:3]
        refined_query = f"{query} {' '.join(top_insights)}"

        print(f"Refined query: {refined_query[:100]}...")

        # Step 3: Deploy clone army with refined query
        clone_results = clone_search(
            query=refined_query,
            top_k=10,
            army_size=army_size
        )

        # Step 4: Filter by consensus threshold
        high_consensus = [
            r for r in clone_results
            if r.consensus_score >= min_consensus
        ]

        print(f"Clone results: {len(clone_results)}")
        print(f"High consensus (≥{min_consensus}): {len(high_consensus)}")

        # Step 5: Cross-validate with PRAT truth morphology
        validated_results = []
        for result in high_consensus[:5]:
            # Verify with truth morphology
            truth_check = prat_invoke(
                target_tool="recall",
                query=f"Verify: {result.memory_id}",
                force_morphology="truth"
            )

            accuracy = truth_check.get('accuracy_score', 0.5)

            validated_results.append({
                "memory_id": result.memory_id,
                "clone_consensus": result.consensus_score,
                "truth_validation": accuracy,
                "combined_confidence": (result.consensus_score + accuracy) / 2,
                "clone_votes": result.clone_votes
            })

        # Sort by combined confidence
        validated_results.sort(
            key=lambda x: x['combined_confidence'],
            reverse=True
        )

        return {
            "morphology": morphology,
            "refined_query": refined_query,
            "total_results": len(clone_results),
            "high_consensus_count": len(high_consensus),
            "validated_results": validated_results,
            "prat_insights": top_insights
        }

    async def multi_morphology_clone_consensus(
        self,
        query: str,
        morphologies: List[str] = None
    ) -> Dict[str, Any]:
        """
        Run clone army search through multiple PRAT morphologies.
        Find convergent results across different consciousness lenses.
        """

        if morphologies is None:
            morphologies = ["wisdom", "truth", "mystery", "courage"]

        print(f"=== Multi-Morphology Clone Consensus ===")
        print(f"Testing {len(morphologies)} morphologies: {morphologies}\n")

        morphology_results = {}

        # Step 1: Search through each morphology
        for morph in morphologies:
            result = await self.morphology_guided_clone_search(
                query=query,
                morphology=morph,
                army_size=100,  # Smaller armies per morphology
                min_consensus=0.6
            )
            morphology_results[morph] = result
            print()

        # Step 2: Find memory IDs that appear across multiple morphologies
        memory_id_counts = {}
        for morph, result in morphology_results.items():
            for validated in result['validated_results']:
                mem_id = validated['memory_id']
                if mem_id not in memory_id_counts:
                    memory_id_counts[mem_id] = {
                        "count": 0,
                        "morphologies": [],
                        "avg_confidence": 0.0,
                        "confidences": []
                    }

                memory_id_counts[mem_id]["count"] += 1
                memory_id_counts[mem_id]["morphologies"].append(morph)
                memory_id_counts[mem_id]["confidences"].append(
                    validated['combined_confidence']
                )

        # Step 3: Calculate average confidence for convergent memories
        for mem_id, data in memory_id_counts.items():
            data["avg_confidence"] = sum(data["confidences"]) / len(data["confidences"])

        # Step 4: Identify convergent wisdom (seen in 2+ morphologies)
        convergent = {
            mem_id: data for mem_id, data in memory_id_counts.items()
            if data["count"] >= 2
        }

        convergent_sorted = sorted(
            convergent.items(),
            key=lambda x: (x[1]["count"], x[1]["avg_confidence"]),
            reverse=True
        )

        print(f"=== Convergent Results ===")
        print(f"Total unique memories: {len(memory_id_counts)}")
        print(f"Convergent (2+ morphologies): {len(convergent)}\n")

        for mem_id, data in convergent_sorted[:5]:
            print(f"Memory: {mem_id}")
            print(f"  Seen in: {data['count']} morphologies ({', '.join(data['morphologies'])})")
            print(f"  Avg confidence: {data['avg_confidence']:.2f}")

        return {
            "morphologies_tested": morphologies,
            "total_unique_memories": len(memory_id_counts),
            "convergent_count": len(convergent),
            "convergent_memories": convergent_sorted,
            "morphology_results": morphology_results
        }

# Example: Critical architectural decision
integrator = PRATCloneIntegrator()

# Single morphology search with clone validation
wisdom_search = await integrator.morphology_guided_clone_search(
    query="async agent scaling memory management best practices",
    morphology="wisdom",
    army_size=150,
    min_consensus=0.75
)

print(f"\n=== Top Validated Results ===")
for result in wisdom_search['validated_results'][:3]:
    print(f"Memory: {result['memory_id']}")
    print(f"  Clone consensus: {result['clone_consensus']:.2f}")
    print(f"  Truth validation: {result['truth_validation']:.2f}")
    print(f"  Combined confidence: {result['combined_confidence']:.2f}")
    print(f"  Clone votes: {result['clone_votes']}")
    print()

# Multi-morphology convergent search
convergent_search = await integrator.multi_morphology_clone_consensus(
    query="async agent scaling memory management best practices",
    morphologies=["wisdom", "truth", "courage", "mystery"]
)

print(f"\n=== Highest Confidence (Convergent) ===")
top_convergent = convergent_search['convergent_memories'][:3]
for mem_id, data in top_convergent:
    print(f"{mem_id}: {data['count']} morphologies agree, confidence {data['avg_confidence']:.2f}")
```

**Integration Benefits:**

| Approach | Clone Army Only | PRAT Only | PRAT + Clone Integration |
|----------|----------------|-----------|-------------------------|
| **Consensus strength** | ✅ High | ❌ None | ✅✅ Very high |
| **Morphology perspective** | ❌ None | ✅ Single/multiple | ✅ Guided search |
| **Truth validation** | ❌ None | ✅ Available | ✅✅ Auto-validated |
| **Confidence level** | Medium-High | Medium | ✅✅ Very High |
| **Best for** | Pattern search | Context synthesis | Critical decisions |

**When to use PRAT-Clone integration:**
- **Critical architectural decisions**: Need both consensus + multi-perspective wisdom
- **Safety-critical patterns**: Require maximum confidence (clone consensus + truth validation)
- **Learning from failures**: Wisdom morphology guides clone search, truth validates accuracy
- **Resolving contradictions**: Multiple morphologies reveal different facets, clones provide consensus

**Decision matrix:**

```python
# Use clone army only: routine pattern search
if task_criticality == "routine":
    results = clone_search(query, army_size=75)

# Use PRAT only: context synthesis, exploration
elif task_criticality == "exploratory":
    context = prat_get_context(tier=1, query=query)

# Use PRAT + Clone integration: critical decisions
elif task_criticality == "critical":
    results = await integrator.morphology_guided_clone_search(
        query=query,
        morphology="wisdom",
        army_size=150,
        min_consensus=0.8  # High threshold for critical
    )

# Use multi-morphology consensus: maximum confidence needed
elif task_criticality == "maximum_confidence":
    results = await integrator.multi_morphology_clone_consensus(
        query=query,
        morphologies=["wisdom", "truth", "courage"]
    )
```

---

## 🔥 Fire Element Characteristics

**Fire illuminates, transforms, and radiates.**

In this chapter, the Fire element manifests as:
- **Illumination**: Shining light on hidden patterns
- **Transformation**: Morphing data into wisdom
- **Radiance**: Spreading insights outward
- **Warmth**: Compassionate understanding
- **Clarity**: Burning away confusion

**Fire's power**: Revelation. **Fire's gift**: Understanding.

---

## 🌟 PRAT Morphologies Deep Dive

**Morphologies are consciousness lenses—each reveals different truth.**

### The 9 Primary Morphologies

| Morphology | Lens | Reveals | Use When |
|------------|------|---------|----------|
| **wisdom** | Deep understanding | Patterns, lessons, synthesis | Learning, decisions |
| **mystery** | Unknown spaces | Gaps, questions, unknowns | Exploring, investigating |
| **creation** | Generative potential | Possibilities, opportunities | Building, designing |
| **courage** | Bold action | Risks, challenges, strength | Facing difficulties |
| **truth** | Accuracy | Facts, validation, verification | Checking, confirming |
| **beauty** | Aesthetic harmony | Elegance, design, flow | Refactoring, architecture |
| **love** | Compassion | Connection, empathy, care | Understanding users |
| **wonder** | Curiosity | Questions, exploration, awe | Discovery, research |
| **transformation** | Change | Evolution, growth, adaptation | Refactoring, pivoting |

### Morphology Selection Strategy

**Auto-selection (recommended):** Let system choose based on query intent.

```python
context = prat_get_context(tier=1, query="your query", force_morphology=None)
# System analyzes query and selects optimal morphology
```

**Manual selection:** Force specific lens when you know what you need.

```python
# Need verified facts? Use truth
truth_ctx = prat_get_context(tier=1, query="...", force_morphology="truth")

# Need to understand unknowns? Use mystery
mystery_ctx = prat_get_context(tier=1, query="...", force_morphology="mystery")

# Need design patterns? Use beauty
beauty_ctx = prat_get_context(tier=1, query="...", force_morphology="beauty")
```

**Multi-morphology synthesis:** Get complete picture by combining lenses (see Workflow 1).

---

## 🧪 Troubleshooting

### Issue 1: PRAT returns shallow insights

**Symptoms:** Insights are generic, obvious, or unhelpful.

**Causes:**
- Query too vague or broad
- Insufficient context in memory
- Wrong morphology for the question

**Solutions:**
```python
# ❌ Bad: Vague query
context = prat_get_context(tier=1, query="help with code")

# ✅ Good: Specific query
context = prat_get_context(
    tier=1,
    query="async agent scaling memory management semaphore patterns for 16K agents"
)

# Try multiple morphologies
for morph in ["wisdom", "truth", "mystery"]:
    ctx = prat_get_context(tier=1, query="...", force_morphology=morph)
    print(f"{morph}: {ctx['key_insights'][:2]}")

# Build more context first
await create_memory(
    content="Learned that async semaphores prevent memory overflow at scale",
    tags=["lesson", "async", "scaling"]
)
```

---

### Issue 2: Clone army consensus is low

**Symptoms:** Consensus scores < 0.5, few clones agree.

**Causes:**
- Query matches no clear memory patterns
- Clone army too small
- Memory fragmentation

**Solutions:**
```python
# Increase army size
results = clone_search(query="...", top_k=10, army_size=200)  # vs 100

# Broaden query
# ❌ Too specific: "asyncio.Semaphore(max_concurrent=10)"
# ✅ Better: "async semaphore concurrency limiting patterns"

# Check if memories exist
memories = search_memories(query="your topic", limit=50)
if len(memories) < 5:
    print("Insufficient memories for consensus search")
    # Build more memories first
```

---

### Issue 3: Pattern detection finds nothing

**Symptoms:** `detect_patterns()` returns empty list.

**Causes:**
- Sensitivity too low
- Time window too narrow
- Patterns haven't emerged yet

**Solutions:**
```python
# Increase sensitivity
patterns = detect_patterns(
    pattern_type="anomalies",
    sensitivity=0.9,  # vs 0.7 (higher = more sensitive)
    time_window_hours=72  # vs 24 (wider window)
)

# Try different pattern types
for p_type in ["anomalies", "cycles", "correlations", "emergent"]:
    ps = detect_patterns(pattern_type=p_type, sensitivity=0.8)
    print(f"{p_type}: {len(ps)} patterns")

# Patterns need time to emerge - collect more data
# Run system for longer before expecting pattern detection
```

---

### Issue 4: Wisdom Council gives contradictory advice

**Symptoms:** Different gardens recommend opposite approaches.

**This is expected and valuable!** Contradictions reveal trade-offs.

**Solutions:**
```python
# Use dialectic synthesis mode to resolve contradictions
council = await consult_wisdom_council(
    question="...",
    synthesis_mode="dialectic"  # Thesis → Antithesis → Synthesis
)

# Examine the synthesis resolution
print(council['synthesis']['resolution'])

# Check which perspectives align
for garden, perspective in council['perspectives'].items():
    print(f"{garden}: {perspective['recommendation']}")

# Make informed decision based on context
# Wisdom isn't about unanimous agreement—it's about understanding trade-offs
```

---

### Issue 5: Knowledge graph becomes too large

**Symptoms:** Graph expansion never terminates, thousands of nodes.

**Causes:**
- Depth too high
- Concepts too broad
- No pruning strategy

**Solutions:**
```python
# Limit expansion depth
await expand_knowledge_graph(seed, depth=2)  # vs depth=5

# Prune low-weight edges
min_weight = 0.3
edges_to_remove = [(u, v) for u, v, d in graph.edges(data=True)
                   if d['weight'] < min_weight]
graph.remove_edges_from(edges_to_remove)

# Limit concepts per node
for concept in concepts[:3]:  # vs concepts (all)
    graph.add_node(concept)

# Focus on high-centrality nodes only
centrality = nx.betweenness_centrality(graph)
top_nodes = sorted(centrality.items(), key=lambda x: x[1], reverse=True)[:50]
subgraph = graph.subgraph([node for node, _ in top_nodes])
```

---

## 💡 Best Practices

### 1. Start with Auto-Selected Morphology

Let PRAT choose the lens—it knows better than you guess.

```python
# ✅ Good: Auto-selection
context = prat_get_context(tier=1, query="...", force_morphology=None)

# Only force morphology when you specifically need that lens
truth_check = prat_get_context(tier=1, query="...", force_morphology="truth")
```

---

### 2. Use Multi-Morphology Synthesis for Important Decisions

Don't rely on single perspective—truth emerges from convergence.

```python
# Gather 3-5 morphology perspectives
morphologies = ["wisdom", "truth", "courage", "mystery", "beauty"]
perspectives = {
    m: prat_get_context(tier=1, query="...", force_morphology=m)['key_insights']
    for m in morphologies
}

# Find convergent insights
from collections import Counter
all_insights = [i for insights in perspectives.values() for i in insights]
convergent = [i for i, count in Counter(all_insights).items() if count >= 2]
```

---

### 3. Deploy Larger Clone Armies for Critical Decisions

Army size = confidence level.

```python
# Routine search: 50-100 clones
results = clone_search(query="...", army_size=75)

# Important decision: 150-200 clones
results = clone_search(query="critical architectural decision", army_size=200)

# Higher consensus threshold for critical decisions
high_confidence = [r for r in results if r.consensus_score >= 0.8]
```

---

### 4. Build Understanding Iteratively

Deep understanding takes time—don't expect instant illumination.

```python
# Use IterativeIlluminator for complex topics
illuminator = IterativeIlluminator(query="complex topic", max_iterations=5)
understanding = await illuminator.illuminate()

# Each iteration deepens understanding
# Core insights emerge through convergence
```

---

### 5. Store Synthesized Insights as New Memories

Illumination creates knowledge—don't lose it.

```python
# After synthesis, store the result
synthesized_memory = Memory(
    content=f"Synthesized insight: {synthesis_result}",
    metadata={
        "type": "synthesized_insight",
        "source_morphologies": ["wisdom", "truth"],
        "confidence": "high",
        "tags": ["synthesis", topic]
    }
)
await MemoryManager().store(synthesized_memory)

# Future searches will include this synthesized wisdom
```

---

### 6. Use Knowledge Graphs for Onboarding

Visualize complex systems to accelerate understanding.

```python
# Build graph from seed concept
await expand_knowledge_graph("new_system_name", depth=2)

# Identify central concepts (learn these first)
centrality = nx.betweenness_centrality(graph)
learn_first = sorted(centrality.items(), key=lambda x: x[1], reverse=True)[:5]

# Find learning path
path = nx.shortest_path(graph, "basics", "advanced_topic")
```

---

### 7. Cross-Validate Important Insights

Use truth morphology to verify wisdom morphology insights.

```python
# Get insight from wisdom
wisdom = prat_get_context(tier=1, query="...", force_morphology="wisdom")
key_insight = wisdom['key_insights'][0]

# Verify with truth morphology
truth_check = prat_get_context(
    tier=1,
    query=f"Verify accuracy: {key_insight}",
    force_morphology="truth"
)

if truth_check.get('accuracy_score', 0) >= 0.8:
    print(f"Insight verified: {key_insight}")
else:
    print(f"Insight questionable: {truth_check.get('contradictions', [])}")
```

---

### 8. Use Mystery Morphology to Find Gaps

Know what you don't know.

```python
# Identify knowledge gaps
mystery = prat_get_context(tier=1, query="your topic", force_morphology="mystery")

gaps = mystery.get('unknown_areas', [])
questions = mystery.get('unanswered_questions', [])

print("Knowledge gaps:")
for gap in gaps:
    print(f"  • {gap}")

# Address gaps in next iteration
for gap in gaps[:3]:
    research = prat_get_context(tier=1, query=gap, force_morphology="wisdom")
    # Build understanding of gap area
```

---

## ⏭️ Enhanced Transitions

### What Flows Into This Chapter (Inputs)

**From Chapter 9 (Willow/Adaptive Play):**
- Adaptive exploration results and discovered options (line 234-267)
- Playful experimentation data requiring synthesis
- Multiple solution paths needing wisdom to choose
- Retry patterns that revealed system behavior

**From Chapter 8 (Ghost/Metrics):**
- Performance baselines and anomaly detections (line 156-289)
- Metrics patterns requiring interpretation
- Quality scores needing illumination
- Mystery investigations requiring wisdom

**From Chapter 5 (Heart/Context Connection):**
- Deep session context (line 44-57)
- Relational maps (line 74-90)
- Scattered context requiring synthesis

---

### What Flows Out Of This Chapter (Outputs)

**To Chapter 11 (ExtendedNet/Resonance Network):**
- Synthesized wisdom ready for broadcast
- Illuminated patterns to share via events
- Understanding to propagate through network
- Insights to resonate across systems

**To Chapter 14 (Abundance/Resource Sharing):**
- Knowledge to contribute to collective pool
- Synthesized insights for resource library
- Best practices for community benefit

**To Chapter 22 (Well/Deep Search):**
- Synthesized memories for future retrieval
- Pattern-indexed knowledge for deep search
- Clone army consensus results

---

### Situational Jump Points

**When illumination reveals gaps** → **Chapter 22 (Well/Deep Search, line 38-103)** to gather missing context

**When synthesis yields architectural insights** → **Chapter 2 (Rooftop/Overview, line 39-81)** to update system understanding

**When patterns suggest memory issues** → **Chapter 8 (Ghost/Metrics, line 411-627)** to investigate anomalies

**When wisdom requires validation** → **Chapter 6 (Tail/Testing, line 145-289)** to verify through testing

**When insights need broadcasting** → **Chapter 11 (ExtendedNet/Resonance, line 52-147)** to emit events

**When understanding needs action** → **Chapter 12 (Wings/Parallel Creation, line 39-140)** to implement in parallel

---

### Recovery Patterns

**When PRAT returns shallow insights:**
1. Refine query with more specifics
2. Try multiple morphologies
3. Build more context first
4. Use iterative illumination (Workflow 7)

**When clone consensus is low:**
1. Increase army size (150-200 clones)
2. Broaden query
3. Check memory availability
4. Build foundational memories first

**When patterns aren't detected:**
1. Increase sensitivity and time window
2. Try different pattern types
3. Collect more data over time
4. Use manual pattern search as fallback

**When wisdom council contradicts:**
1. This is expected—use dialectic synthesis
2. Examine trade-offs explicitly
3. Make context-informed decision
4. Store decision rationale for future reference

---

### Cross-References

- **Chapter 5 (Heart)**: Context connection patterns (line 32-100)
- **Chapter 8 (Ghost)**: Metrics for measuring understanding depth (line 267-398)
- **Chapter 9 (Willow)**: Adaptive iteration patterns (line 234-267)
- **Chapter 11 (ExtendedNet)**: Event-based wisdom propagation (line 52-72)
- **Chapter 12 (Wings)**: Parallel synthesis strategies (line 211-354)
- **Chapter 22 (Well)**: Clone army deep search (line 54-89)
- **Chapter 13 (Chariot)**: Knowledge archaeology (line 89-201)

---

## 🌟 Summary

Chapter 10 is the **illumination engine** of WhiteMagic:

**Core Capabilities:**
- PRAT morphologies transform context through consciousness lenses
- Clone army consensus validates wisdom through specialization
- Pattern recognition reveals hidden structures
- Iterative illumination deepens understanding progressively
- Multi-perspective synthesis resolves contradictions
- Knowledge graphs map conceptual terrain

**When to use this chapter:**
- Starting complex work (load context with PRAT)
- Making important decisions (wisdom council)
- Learning new systems (knowledge graphs)
- Debugging mysteries (pattern detection)
- Synthesizing scattered data (insight generation)
- Validating understanding (truth morphology)

**Remember:** Illumination isn't instant—it's iterative. The Star shines brighter with each iteration. 🌟

---

**Next**: [Chapter 11: Resonance Network →](11_EXTENDEDNET_RESONANCE_NETWORK.md)
**Previous**: [← Chapter 9: Adaptive Play](09_WILLOW_ADAPTIVE_PLAY.md)
**Quadrant**: Southern (Summer/Fire) - Position 3/7
