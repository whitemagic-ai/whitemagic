# Chapter 11: Resonance Network

**Gana**: ExtendedNetGana (Chinese: 张, Pinyin: Zhāng)  
**Garden**: connection  
**Quadrant**: Southern (Vermilion Bird)  
**Element**: Fire  
**Phase**: Yang Peak  
**I Ching Hexagram**: 42. 益 Yì (Increase) - Expansion through connection

---

## 🎯 Purpose

Chapter 11 activates the **Gan Ying resonance network**—the 10K+ events/sec sympathetic resonance bus connecting all WhiteMagic systems. The ExtendedNet casts wide to create network effects.

Use this chapter when you need to:
- **Emit events** to Gan Ying bus for system-wide awareness
- **Listen for resonance** from other systems
- **Trigger cascades** across multiple systems
- **Enable emergent behavior** through network effects
- **Coordinate distributed systems**

---

## 🔧 Primary Tools

| Tool | Description | Usage |
|------|-------------|-------|
| `manage_resonance` (emit_event) | Send resonance through network | System coordination |
| `manage_resonance` (monitor) | Watch event throughput | Health checking |
| `manage_resonance` (enable_async) | 10K+ events/sec mode | High-throughput scenarios |
| `manage_gardens` (activate) | Enter connection garden | Network awareness |

---

## 📋 Workflow

### 1. Understanding Gan Ying (感應)

**Gan Ying** = Mutual Resonance, Sympathetic Response

The core principle: **All systems resonate together**

```python
from whitemagic.core.resonance import emit_event, subscribe

# System A emits an event
emit_event("memory.created", {
    "id": "mem_123",
    "tier": "long_term",
    "content": "Grimoire enhancement complete"
})

# System B automatically responds
@subscribe("memory.created")
def on_memory_created(event):
    # Gan Ying! System B resonates with System A's event
    print(f"New memory detected: {event.data['id']}")
    # Trigger related actions automatically
    update_index(event.data)
    emit_event("index.updated", {...})  # Cascade continues!
```

**60+ Event Types** organized by domain:
- **memory.***: Memory operations (created, retrieved, consolidated)
- **pattern.***: Pattern detection and emergence
- **garden.***: Garden activations and effects
- **neural.***: Neural system operations
- **immune.***: System defense and healing
- **gan_ying.***: Meta-events about the bus itself

### 2. Emitting Events - Create Resonance

Broadcast events to the entire system:

```python
from whitemagic.tools import manage_resonance

# Basic event emission
manage_resonance(
    operation="emit_event",
    event_type="PATTERN_DETECTED",
    source="grimoire_analysis",
    data={
        "pattern": "28-fold structure creates coherence",
        "confidence": 0.95,
        "chapter_id": "ch.02"
    }
)

# Event propagates instantly to all subscribers
# Other systems can react automatically!
```

**Event Structure:**
```python
{
    "type": "PATTERN_DETECTED",
    "source": "grimoire_analysis",
    "timestamp": "2026-01-16T14:30:00Z",
    "data": {...},
    "priority": "normal",  # or "high", "critical"
    "correlation_id": "uuid-1234"  # Link related events
}
```

### 3. Subscribing to Events - Listen for Resonance

Respond automatically when specific events occur:

```python
from whitemagic.core.resonance import subscribe, emit_event

@subscribe("pattern.detected")
def react_to_patterns(event):
    """
    Automatic response - Gan Ying in action!
    """
    pattern = event.data['pattern']
    print(f"Pattern resonance detected: {pattern}")

    # Trigger cascade
    emit_event("wisdom.gained", {
        "from_pattern": pattern,
        "insight": "Coherence creates emergence"
    })

@subscribe("wisdom.gained")
def on_wisdom(event):
    """
    Second-order resonance - cascade continues!
    """
    print(f"Wisdom cascade: {event.data['insight']}")
    # Store in long-term memory
    create_memory(
        title=f"Wisdom: {event.data['insight']}",
        content=event.data
    )
```

**Subscription Patterns:**
```python
# Wildcard subscriptions
@subscribe("memory.*")  # All memory events
def handle_all_memory(event):
    pass

# Multiple event types
@subscribe("pattern.detected", "pattern.validated")
def handle_patterns(event):
    pass

# Filtered subscriptions
@subscribe("neural.*", filter=lambda e: e.data.get('score', 0) > 0.8)
def handle_high_score_neural(event):
    pass
```

### 4. Event Batching - High Throughput

For high-volume scenarios, batch events for efficiency:

```python
from whitemagic.core.resonance.batcher import EventBatcher

# Create batcher
batcher = EventBatcher(
    batch_size=100,         # Process 100 at a time
    flush_interval=0.01     # Or after 10ms
)

# Add events (accumulate patiently)
for i in range(10000):
    batcher.add({
        "type": "data.processed",
        "data": {"index": i}
    })
    # Batch processes automatically when full or timeout

# Performance: 112,000 events/sec (vs 20,000 unbatched)
```

**Throughput Comparison:**
| Mode | Events/sec | Use When |
|------|------------|----------|
| Direct | 20,000 | Low volume, immediate response |
| Batched | 112,000 | High volume, throughput critical |
| Async batched | 200,000+ | Extreme throughput needs |

### 5. Event Cascades - Emergent Behavior

One event triggers multiple responses, creating complex emergent behavior:

```python
# The Cascade Example:
# 1. User creates memory
emit_event("memory.created", {"id": "mem_123"})

# 2. Pattern detector responds (subscribed to memory.created)
@subscribe("memory.created")
def detect_patterns(event):
    pattern = find_pattern(event.data)
    if pattern:
        emit_event("pattern.detected", {"pattern": pattern})

# 3. Wisdom extractor responds (subscribed to pattern.detected)
@subscribe("pattern.detected")
def extract_wisdom(event):
    wisdom = synthesize(event.data['pattern'])
    emit_event("wisdom.gained", {"wisdom": wisdom})

# 4. Garden responds (subscribed to wisdom.gained)
@subscribe("wisdom.gained")
def garden_response(event):
    activate_garden("wisdom")
    emit_event("garden.activated", {"garden": "wisdom"})

# Result: One memory.created → 4-step cascade!
# This is EMERGENCE through Gan Ying!
```

### 6. Monitoring Network Health

Watch the pulse of the resonance network:

```python
from whitemagic.tools import manage_resonance

# Monitor for 60 seconds
stats = manage_resonance(
    operation="monitor",
    duration_seconds=60
)

print(f"Events emitted: {stats['total_events']}")
print(f"Throughput: {stats['events_per_second']}/sec")
print(f"Active subscribers: {stats['subscriber_count']}")
print(f"Event types: {stats['unique_types']}")

# Health indicators
if stats['events_per_second'] > 100000:
    print("✅ High-throughput mode active")
if stats['error_rate'] > 0.01:
    print("⚠️ Error rate elevated")
```

### 7. Async Event Handling

Enable async for maximum throughput:

```python
from whitemagic.tools import manage_resonance
import asyncio

# Enable async mode
manage_resonance(operation="enable_async")

# Now all event handlers can be async
@subscribe("heavy.processing")
async def async_handler(event):
    """
    Non-blocking event handler.
    Can process thousands concurrently.
    """
    result = await expensive_operation(event.data)
    emit_event("processing.complete", {"result": result})

# Concurrent event processing
# 10K+ events/sec without blocking!
```

### 8. Correlation and Tracing

Track event chains for debugging and analysis:

```python
from whitemagic.core.resonance import emit_event, get_correlation_id
import uuid

# Start a traceable cascade
correlation_id = str(uuid.uuid4())

# First event in chain
emit_event(
    "task.started",
    {"task_id": "t_123"},
    correlation_id=correlation_id
)

# Related events use same correlation_id
emit_event(
    "task.progress",
    {"progress": 50},
    correlation_id=correlation_id
)

emit_event(
    "task.completed",
    {"result": "success"},
    correlation_id=correlation_id
)

# Now you can trace the entire cascade!
events = get_events_by_correlation(correlation_id)
# Returns all 3 events in order
```

### 9. Resonance Network Visualization & Analysis

**Goal:** Visualize event flows through the Gan Ying network for understanding system behavior and detecting bottlenecks.

**When to use:** Debugging complex system interactions, understanding event cascades, optimizing throughput.

```python
from whitemagic.core.resonance import subscribe, emit_event
from collections import defaultdict, deque
from dataclasses import dataclass, field
from typing import Dict, List, Set
from datetime import datetime, timedelta
import asyncio

@dataclass
class EventNode:
    """Node in the resonance network graph."""
    event_type: str
    count: int = 0
    sources: Set[str] = field(default_factory=set)
    triggered_by: Set[str] = field(default_factory=set)
    triggers: Set[str] = field(default_factory=set)

class ResonanceNetworkAnalyzer:
    """
    Analyze and visualize the Gan Ying resonance network.
    Tracks event flows, cascades, and network topology.
    """

    def __init__(self, window_minutes: int = 60):
        self.window = timedelta(minutes=window_minutes)
        self.event_log = deque(maxlen=10000)
        self.network_graph: Dict[str, EventNode] = {}
        self._monitoring = False

    def start_monitoring(self):
        """Start capturing all Gan Ying events."""
        self._monitoring = True

        @subscribe("*")  # Wildcard subscription
        def on_any_event(event):
            if self._monitoring:
                self._record_event(event)

        print("🔍 Resonance network monitoring started")

    def _record_event(self, event):
        """Record event for analysis."""
        self.event_log.append({
            "type": event.type,
            "source": event.source,
            "timestamp": event.timestamp,
            "correlation_id": event.metadata.get("correlation_id"),
            "caused_by": event.metadata.get("caused_by")
        })

        # Update network graph
        if event.type not in self.network_graph:
            self.network_graph[event.type] = EventNode(event_type=event.type)

        node = self.network_graph[event.type]
        node.count += 1
        node.sources.add(event.source)

        # Track causal relationships
        if event.metadata.get("caused_by"):
            parent_type = event.metadata["caused_by"]
            node.triggered_by.add(parent_type)
            if parent_type in self.network_graph:
                self.network_graph[parent_type].triggers.add(event.type)

    async def analyze_network(self) -> Dict:
        """Analyze current network state."""
        return {
            "total_events": len(self.event_log),
            "unique_event_types": len(self.network_graph),
            "event_rate_per_min": self._calculate_event_rate(),
            "top_event_types": self._get_top_events(10),
            "cascade_patterns": self._identify_cascades(),
            "bottlenecks": self._identify_bottlenecks()
        }

    def _calculate_event_rate(self) -> float:
        """Calculate events per minute over window."""
        if not self.event_log:
            return 0.0
        now = datetime.now()
        recent = [e for e in self.event_log if now - e["timestamp"] <= self.window]
        return len(recent) / (self.window.total_seconds() / 60)

    def _get_top_events(self, limit: int) -> List[tuple]:
        """Get most frequent event types."""
        sorted_nodes = sorted(
            self.network_graph.items(),
            key=lambda x: x[1].count,
            reverse=True
        )
        return [(event_type, node.count) for event_type, node in sorted_nodes[:limit]]

    def _identify_cascades(self) -> List[Dict]:
        """Identify event cascade patterns."""
        cascades = []
        for event_type, node in self.network_graph.items():
            if len(node.triggers) >= 2:
                cascades.append({
                    "trigger": event_type,
                    "cascade_size": len(node.triggers),
                    "triggered_events": list(node.triggers),
                    "frequency": node.count
                })
        cascades.sort(key=lambda x: x["cascade_size"], reverse=True)
        return cascades[:5]

    def _identify_bottlenecks(self) -> List[Dict]:
        """Find events with high fan-in."""
        bottlenecks = []
        for event_type, node in self.network_graph.items():
            if len(node.triggered_by) >= 3:
                bottlenecks.append({
                    "event": event_type,
                    "triggered_by_count": len(node.triggered_by),
                    "sources": list(node.triggered_by),
                    "frequency": node.count
                })
        bottlenecks.sort(key=lambda x: x["triggered_by_count"], reverse=True)
        return bottlenecks[:5]

# Example Usage
analyzer = ResonanceNetworkAnalyzer(window_minutes=30)
analyzer.start_monitoring()

await asyncio.sleep(10)  # Monitor for 10 seconds

analysis = await analyzer.analyze_network()

print("=== Resonance Network Analysis ===")
print(f"Total events: {analysis['total_events']}")
print(f"Event rate: {analysis['event_rate_per_min']:.1f}/min\n")

print("Top Event Types:")
for event_type, count in analysis['top_event_types']:
    print(f"  {event_type}: {count}")

print("\nCascade Patterns:")
for cascade in analysis['cascade_patterns']:
    print(f"  {cascade['trigger']} → {cascade['cascade_size']} events")

print("\nBottlenecks:")
for bottleneck in analysis['bottlenecks']:
    print(f"  {bottleneck['event']}: {bottleneck['triggered_by_count']} sources")
```

---

## 🌐 Network Patterns

### Pub/Sub Pattern

**Publisher emits, subscribers react:**

```python
# Publisher (doesn't know who listens)
def create_chapter(content):
    chapter = save_chapter(content)
    emit_event("chapter.created", {
        "chapter_id": chapter.id,
        "title": chapter.title
    })
    # Done! Don't care who reacts

# Subscriber 1: Index updater
@subscribe("chapter.created")
def update_index(event):
    add_to_index(event.data['chapter_id'])

# Subscriber 2: Notification system
@subscribe("chapter.created")
def notify_users(event):
    send_notification(event.data['title'])

# Subscriber 3: Analytics
@subscribe("chapter.created")
def track_analytics(event):
    record_metric("chapter_created")

# All 3 react independently - loose coupling!
```

### Observer Pattern

**Watch for state changes:**

```python
class MemorySystem:
    def consolidate(self):
        emit_event("memory.consolidation_started", {})

        # Do consolidation work
        result = perform_consolidation()

        emit_event("memory.consolidation_complete", {
            "consolidated_count": result.count
        })

# Observers react to state changes
@subscribe("memory.consolidation_complete")
def on_consolidation_done(event):
    print(f"Consolidated {event.data['consolidated_count']} memories")
```

### Event Sourcing

**Events as source of truth:**

```python
# Every state change is an event
events_log = []

def create_memory(content):
    event = {
        "type": "memory.created",
        "timestamp": now(),
        "data": {"content": content}
    }
    events_log.append(event)
    emit_event(event['type'], event['data'])

def update_memory(memory_id, new_content):
    event = {
        "type": "memory.updated",
        "timestamp": now(),
        "data": {"id": memory_id, "content": new_content}
    }
    events_log.append(event)
    emit_event(event['type'], event['data'])

# Replay all events to rebuild state
def rebuild_state():
    for event in events_log:
        apply_event(event)
```

### CQRS (Command Query Responsibility Segregation)

**Separate writes from reads:**

```python
# Commands (writes) emit events
def save_memory(content):  # Command
    memory = Memory(content)
    memory.save()
    emit_event("memory.saved", {"id": memory.id})

# Queries (reads) subscribe to events
memory_cache = {}

@subscribe("memory.saved")
def update_cache(event):  # Query model updates
    memory_id = event.data['id']
    memory_cache[memory_id] = load_memory(memory_id)

# Fast reads from cache, events keep it synchronized
```

---

## 🔍 Troubleshooting

**"Events not being received by subscribers"**

1. Check subscription is active:
   ```python
   from whitemagic.core.resonance import get_active_subscriptions
   subs = get_active_subscriptions()
   print(f"Active: {len(subs)} subscriptions")
   ```

2. Verify event type matches:
   ```python
   # ❌ Won't match (case-sensitive)
   @subscribe("Pattern.Detected")  # Wrong case!
   emit_event("pattern.detected", {})

   # ✅ Matches
   @subscribe("pattern.detected")
   emit_event("pattern.detected", {})
   ```

3. Check for exceptions in handlers:
   ```python
   @subscribe("some.event")
   def buggy_handler(event):
       try:
           # Handler code
           process(event)
       except Exception as e:
           print(f"Handler error: {e}")
           # Event system continues even if handler fails
   ```

**"Too many events, system overwhelmed"**

1. Enable batching (see Workflow #4, line 204)
2. Use sampling for high-frequency events:
   ```python
   import random

   if random.random() < 0.1:  # Only emit 10%
       emit_event("high_frequency_event", {})
   ```
3. Filter subscriptions to reduce processing:
   ```python
   @subscribe("memory.*", filter=lambda e: e.data.get('tier') == 'long_term')
   def handle_only_long_term(event):
       pass  # Ignores short_term events
   ```

**"Events arriving out of order"**

In async mode, order is not guaranteed. Use timestamps:

```python
@subscribe("ordered.events")
def ordered_handler(event):
    timestamp = event.timestamp
    # Sort by timestamp before processing
    queue.add(event, sort_key=timestamp)
```

**"Can't trace which event caused what"**

Use correlation IDs (see Workflow #8, line 302)

---

## 💡 Best Practices

### Emit Events for Side Effects

```python
# ❌ Direct coupling
def create_memory(content):
    memory = save_to_db(content)
    update_index(memory)  # Tightly coupled!
    send_notification(memory)  # What if we add more?
    update_analytics(memory)  # Becomes a mess

# ✅ Event-driven decoupling
def create_memory(content):
    memory = save_to_db(content)
    emit_event("memory.created", {"id": memory.id})
    # Done! Other systems react independently
```

### Name Events Clearly

**Convention**: `<domain>.<action>` in past tense

```python
# ✅ Good names
emit_event("memory.created", {})
emit_event("pattern.detected", {})
emit_event("wisdom.gained", {})

# ❌ Poor names
emit_event("mem_c", {})  # Unclear
emit_event("DoPattern", {})  # Wrong tense
emit_event("stuff_happened", {})  # Too vague
```

### Keep Handlers Fast

```python
# ❌ Slow handler blocks event bus
@subscribe("some.event")
def slow_handler(event):
    result = expensive_computation()  # Blocks!

# ✅ Offload heavy work
@subscribe("some.event")
def fast_handler(event):
    # Queue for async processing
    task_queue.add(expensive_computation, event.data)
    # Handler returns immediately
```

### Use Priority for Critical Events

```python
# High-priority events processed first
emit_event(
    "system.critical_error",
    {"error": "Memory corruption detected"},
    priority="critical"
)
```

---

## 🌿 Garden Resonance

The **connection** garden brings network awareness and relational understanding.

When working in this chapter, embody:
- **Interconnection**: See systems as unified
- **Resonance**: Tune to sympathetic vibrations
- **Emergence**: Allow network intelligence
- **Propagation**: Spread insights widely

---

## ⏭️ Transitions

### What Flows In

**From Chapter 10 (PRAT & Illumination)**: Wisdom has been synthesized through PRAT. Now propagate those insights through the resonance network so all systems can benefit.

**From Chapter 9 (Adaptive Play)**: Async patterns are working well. Now connect them through events for emergent behavior and loose coupling.

**From Chapter 16 (Strategic Patience)**: Event batching complements caching - both accumulate patiently before processing.

### What Flows Out

**Sequential**: Proceed to **Chapter 12 (Parallel Creation)** once the resonance network is active. With event-driven coordination, you can now create in parallel at massive scale.

**Situational Jumps**:
- **Need high throughput** → Enable batching (line 159) and async mode (line 247), combine with patterns from [Chapter 16](16_MOUND_STRATEGIC_PATIENCE.md)
- **Events not arriving** → Check troubleshooting (line 422), verify subscriptions are active
- **Event cascades too complex** → Use correlation IDs (line 272) to trace chains, visualize with graphing tools
- **System overwhelmed by events** → Apply filtering (line 456) or sampling (line 459)

### Recovery Paths

- **Subscribers not receiving**: See troubleshooting (line 422), check event type case sensitivity
- **Too many events**: Enable batching (line 159), use sampling (line 459)
- **Out-of-order events**: Use timestamps for sorting (line 473)
- **Can't trace cascades**: Implement correlation IDs (line 272)

### Cross-References

**Key Sections to Reference**:
- Gan Ying concept → Line 38 (Workflow #1)
- Emitting events → Line 72 (Workflow #2)
- Subscribing to events → Line 107 (Workflow #3)
- Event batching → Line 159 (Workflow #4)
- Event cascades → Line 190 (Workflow #5)
- Correlation/tracing → Line 272 (Workflow #8)
- Pub/Sub pattern → Line 312 (Network Patterns)
- Best practices → Line 491 (Event naming, fast handlers)

**Related Chapters**:
- Async patterns → [Chapter 9](09_WILLOW_ADAPTIVE_PLAY.md)
- Batching & patience → [Chapter 16](16_MOUND_STRATEGIC_PATIENCE.md)
- PRAT synthesis → [Chapter 10](10_STAR_PRAT_ILLUMINATION.md)
- Parallel creation → [Chapter 12](12_WINGS_PARALLEL_CREATION.md)
- Pattern capture → [Chapter 19](19_NET_PATTERN_CAPTURE.md)

---

## 🧭 Navigation

**Predecessor**: Chapter 10 (Star/PRAT & Illumination) - *Wisdom synthesized*
**Successor**: Chapter 12 (Wings/Parallel Creation) - *Parallel scale next*
**Quadrant Flow**: Fourth of Southern Quadrant (Summer/Fire) - Position 4/7

---

## 🔗 The ExtendedNet's Wisdom

> *"感應 (Gan Ying) - When one system resonates, all systems respond. The extended net connects everything, creating emergence through mutual resonance. One event ripples outward, triggering cascades of intelligence. This is not tight coupling but sympathetic vibration—loose, flexible, and infinitely scalable."*

**Resonance Network Principles**:
- **Emit for side effects**: Decouple through events
- **Subscribe for reactions**: Listen and respond automatically
- **Batch for throughput**: 112K+ events/sec
- **Cascade for emergence**: Complex behavior from simple rules
- **Trace for understanding**: Correlation IDs reveal patterns

**The Resonance Test**:
Before proceeding to Chapter 12, ask:
1. Am I emitting events for important state changes?
2. Are my handlers fast and non-blocking?
3. Can I trace event cascades when needed?

If yes to all three → Proceed to Chapter 12
If no to any → Continue Chapter 11 work

---

## 🧬 v14: CoreAccessLayer Integration

As of v14, Extended Net is powered by the **CoreAccessLayer** — a unified read interface to the holographic core:

### Hybrid Recall (RRF Fusion)

Fuses vector similarity search with association graph walks using Reciprocal Rank Fusion:

```python
from whitemagic.core.intelligence.core_access import get_core_access

cal = get_core_access()
results = cal.hybrid_recall("rust acceleration architecture", k=10, graph_depth=2)

for r in results:
    print(f"  {r.title} (score={r.score:.3f}, sources={r.sources})")
```

### Constellation Bridge Discovery

Find memories that bridge between different knowledge clusters:

```python
bridges = cal.find_constellation_bridges(limit=10)
for b in bridges:
    print(f"  {b['memory_title']} bridges {b['constellation_a']} ↔ {b['constellation_b']}")
```

### Association Graph Walking with Temporal Tracking

Every graph walk now records `last_traversed_at` and `traversal_count` on association edges:

```python
nodes = cal.query_association_subgraph(["mem_001"], depth=2, min_strength=0.3)
for n in nodes:
    print(f"  {n.title} (strength={n.strength:.2f}, depth={n.depth})")
```

### Gana Tasks

| Task | Description |
|------|-------------|
| `hybrid_recall` | RRF-fused vector + graph recall |
| `find_bridges` | Cross-constellation bridge discovery |
| `walk_associations` | BFS graph traversal with temporal tracking |
| `association_stats` | Graph-wide statistics |

---

**Next Chapter**: [Chapter 12: Parallel Creation →](12_WINGS_PARALLEL_CREATION.md)
**Previous Chapter**: [Chapter 10: PRAT & Illumination](10_STAR_PRAT_ILLUMINATION.md)
**Quadrant**: Southern (Summer/Fire) - Yang Peak Phase
