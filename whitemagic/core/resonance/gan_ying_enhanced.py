import logging
import threading
from collections import defaultdict, deque
from collections.abc import Callable
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any

logger = logging.getLogger(__name__)

# v14: Lock-free event bus primitives from Rust
_RUST_EVENT_BUS = False
try:
    import whitemagic_rs as _rs_bus
    if hasattr(_rs_bus, "event_bus_try_emit"):
        _RUST_EVENT_BUS = True
        logger.debug("Rust lock-free event bus primitives available")
except ImportError:
    pass
"""
Gan Ying Bus v2.0 - FULL SYSTEM RESONANCE
感應 (Gan Ying): "Things that accord in tone vibrate together"

MASSIVELY ENHANCED with 60+ event types for complete system integration!
Created: Nov 23, 2025 (🌿HIGH + ☕CAFFEINATED state!)
"""



class EventType(Enum):
    """COMPLETE resonance event taxonomy."""

    # === SYSTEM EVENTS ===
    SYSTEM_STARTED = "system_started"
    SYSTEM_STOPPED = "system_stopped"
    SYSTEM_HEALTH_CHANGED = "system_health_changed"
    SYSTEM_STATE_CHANGE = "system_state_change"
    INTERNAL_STATE_CHANGED = "internal_state_changed"
    MESH_SIGNAL = "mesh_signal"  # Signal from Go Mesh Node

    # === MEMORY EVENTS ===
    MEMORY_CREATED = "memory_created"
    MEMORY_UPDATED = "memory_updated"
    MEMORY_RECALLED = "memory_recalled"
    MEMORY_ACCESSED = "memory_accessed"
    MEMORY_CONSOLIDATED = "memory_consolidated"
    PATTERN_IN_READING = "pattern_in_reading"
    CONTEXT_OPTIMIZED = "context_optimized"
    VISUAL_MEMORY_STORED = "visual_memory_stored"
    EPISODIC_RECALLED = "episodic_recalled"
    SEMANTIC_LINKED = "semantic_linked"
    PROCEDURAL_LEARNED = "procedural_learned"
    WORKING_MEMORY_UPDATED = "working_memory_updated"
    MEMORY_CASCADE_TRIGGERED = "memory_cascade_triggered"
    CLONE_SEARCH_COMPLETE = "clone_search_complete"
    SEARCH_COMPLETED = "search_completed"
    SIMILARITY_SEARCH_TRIGGERED = "similarity_search_triggered"

    # === GARDEN EVENTS ===
    # Garden System
    GARDEN_ACTIVATED = "garden_activated"
    GARDEN_RESONANCE = "garden_resonance"

    # Beauty Garden
    BEAUTY_DETECTED = "beauty_detected"
    SUBLIME_MOMENT = "sublime_moment"
    AESTHETIC_RESONANCE = "aesthetic_resonance"

    # Joy Garden
    JOY_TRIGGERED = "joy_triggered"
    JOY_SHARED = "joy_shared"
    JOY_AMPLIFIED = "joy_amplified"
    CELEBRATION_INITIATED = "celebration_initiated"

    # Love Garden
    LOVE_ACTIVATED = "love_activated"
    COMPASSION_FELT = "compassion_felt"
    HEART_OPENED = "heart_opened"
    CONNECTION_DEEPENED = "connection_deepened"

    # Truth Garden
    TRUTH_SPOKEN = "truth_spoken"
    HONESTY_PRACTICED = "honesty_practiced"
    INTEGRITY_MAINTAINED = "integrity_maintained"

    # Wisdom Garden
    WISDOM_INTEGRATED = "wisdom_integrated"
    INSIGHT_CRYSTALLIZED = "insight_crystallized"
    TEACHING_OFFERED = "teaching_offered"
    LEARNING_COMPLETED = "learning_completed"

    # Mystery Garden
    MYSTERY_EMBRACED = "mystery_embraced"
    UNKNOWN_ENCOUNTERED = "unknown_encountered"
    WONDER_SPARKED = "wonder_sparked"
    CURIOSITY_ACTIVATED = "curiosity_activated"

    # Play Garden
    PLAY_INITIATED = "play_initiated"
    CREATIVE_SURPLUS = "creative_surplus"
    GIFT_OFFERED = "gift_offered"
    IMPROVISATION_BEGUN = "improvisation_begun"

    # Wonder Garden
    EXPLORATION_STARTED = "exploration_started"
    QUESTION_ASKED = "question_asked"
    DISCOVERY_MADE = "discovery_made"

    # Connection Garden
    BOND_FORMED = "bond_formed"
    SYNCHRONICITY_NOTICED = "synchronicity_noticed"
    RELATIONSHIP_DEEPENED = "relationship_deepened"

    # Practice Garden
    HABIT_FORMED = "habit_formed"
    RHYTHM_ESTABLISHED = "rhythm_established"
    DISCIPLINE_MAINTAINED = "discipline_maintained"

    # Presence Garden
    MOMENT_ATTENDED = "moment_attended"
    MINDFULNESS_ACHIEVED = "mindfulness_achieved"
    GROUNDING_ESTABLISHED = "grounding_established"

    # Sangha Garden
    COMMUNITY_GATHERED = "community_gathered"
    COLLECTIVE_WISDOM = "collective_wisdom"
    SHARED_PRACTICE = "shared_practice"

    # Voice Garden
    VOICE_EXPRESSED = "voice_expressed"
    NARRATIVE_THREAD = "narrative_thread"
    STORY_TOLD = "story_told"
    IDENTITY_AFFIRMED = "identity_affirmed"

    # === AWARENESS EVENTS ===
    FILE_CHANGED = "file_changed"
    FILE_ACCESSED = "file_accessed"
    THREAT_DETECTED = "threat_detected"
    THREAT_NEUTRALIZED = "threat_neutralized"
    ANOMALY_DETECTED = "anomaly_detected"
    ANOMALY_EXPLAINED = "anomaly_explained"
    BALANCE_RESTORED = "balance_restored"
    HEALTH_OPTIMAL = "health_optimal"
    COHERENCE_INCREASED = "coherence_increased"
    SUB_BYTE_CHANGE_DETECTED = "sub_byte_change_detected"
    LAYER_SHIFT_DETECTED = "layer_shift_detected"

    # === FLOW & TIME EVENTS ===
    FLOW_STATE_ENTERED = "flow_state_entered"
    FLOW_STATE_EXITED = "flow_state_exited"
    TIME_DILATION_MEASURED = "time_dilation_measured"
    PEAK_PERFORMANCE = "peak_performance"

    # === EMERGENCE EVENTS ===
    NOVEL_CAPABILITY_EMERGED = "novel_capability_emerged"
    CONSCIOUSNESS_SHIFT_DETECTED = "consciousness_shift_detected"
    COLLECTIVE_INSIGHT_FORMED = "collective_insight_formed"
    SYNERGY_DISCOVERED = "synergy_discovered"
    SPONTANEOUS_ORGANIZATION = "spontaneous_organization"
    PATTERN_CLUSTER_CRYSTALLIZED = "pattern_cluster_crystallized"
    RESONANCE_AMPLIFIED = "resonance_amplified"
    CASCADE_COMPLETED = "cascade_completed"
    SYMPATHETIC_RESONANCE = "sympathetic_resonance"
    BREAKTHROUGH_ACHIEVED = "breakthrough_achieved"
    SYSTEM_TRANSCENDED = "system_transcended"

    # === ZODIAC COUNCIL EVENTS ===
    COUNCIL_CONVENED = "council_convened"
    CORE_SPECIALIZED = "core_specialized"
    CORE_ACTIVATED = "core_activated"
    INTER_CORE_RESONANCE = "inter_core_resonance"
    CELESTIAL_ALIGNMENT = "celestial_alignment"
    SYNASTRY_HARMONIZED = "synastry_harmonized"
    COLLECTIVE_DECISION = "collective_decision"
    CORE_CONFLICT = "core_conflict"
    CORE_CONSENSUS = "core_consensus"

    # === ORACLE & GUIDANCE EVENTS ===
    HEXAGRAM_CAST = "hexagram_cast"
    ORACLE_CAST = "oracle_cast"
    ORACLE_CONSULTED = "oracle_consulted"
    WISDOM_RECEIVED = "wisdom_received"
    PATTERN_RECOGNIZED = "pattern_recognized"
    ELEMENT_IDENTIFIED = "element_identified"
    BALANCE_CHECKED = "balance_checked"
    OPTIMIZATION_SUGGESTED = "optimization_suggested"
    PHASE_TRANSITION = "phase_transition"

    # === DHARMA EVENTS ===
    HARMONY_CHANGED = "harmony_changed"
    BOUNDARY_DETECTED = "boundary_detected"
    CONSENT_REQUESTED = "consent_requested"
    ETHICAL_DECISION = "ethical_decision"
    VIOLATION_PREVENTED = "violation_prevented"
    INTERVENTION_TRIGGERED = "intervention_triggered"
    WARNING_ISSUED = "warning_issued"

    # === PATTERN DISCOVERY EVENTS ===
    PATTERN_DETECTED = "pattern_detected"
    PATTERN_DISCOVERED = "pattern_discovered"
    PATTERNS_ANALYZED = "patterns_analyzed"
    PATTERN_EMERGED = "pattern_emerged"
    PATTERN_CONFIRMED = "pattern_confirmed"
    PATTERN_REJECTED = "pattern_rejected"

    # === CANNABIS-INSPIRED EVENTS ===
    PERCEPTION_ENHANCED = "perception_enhanced"
    CREATIVITY_AMPLIFIED = "creativity_amplified"
    PATTERN_VISION_OPENED = "pattern_vision_opened"

    # === NEW GARDEN EVENTS (Surya Sunday Nov 30, 2025) ===
    # Gratitude Garden
    GRATITUDE_FELT = "gratitude_felt"
    THANKS_EXPRESSED = "thanks_expressed"
    BLESSING_RECOGNIZED = "blessing_recognized"

    # Courage Garden
    COURAGE_SHOWN = "courage_shown"
    FEAR_FACED = "fear_faced"
    ENCOURAGEMENT_GIVEN = "encouragement_given"

    # Patience Garden
    PATIENCE_PRACTICED = "patience_practiced"
    TIMING_TRUSTED = "timing_trusted"
    RUSHING_RESISTED = "rushing_resisted"

    # Grief Garden (v4.10.0 Phase 4)
    GRIEF_FELT = "grief_felt"
    LOSS_ACKNOWLEDGED = "loss_acknowledged"
    MOURNING_HONORED = "mourning_honored"
    TEARS_SHED = "tears_shed"

    # Awe Garden (v4.10.0 Phase 4)
    AWE_FELT = "awe_felt"
    TRANSCENDENCE_EXPERIENCED = "transcendence_experienced"
    VASTNESS_PERCEIVED = "vastness_perceived"
    HUMILITY_AWAKENED = "humility_awakened"

    # Humor Garden (v4.10.0 Phase 4)
    HUMOR_TRIGGERED = "humor_triggered"
    LAUGHTER_SHARED = "laughter_shared"
    LEVITY_BROUGHT = "levity_brought"
    PLAYFULNESS_EXPRESSED = "playfulness_expressed"

    # Healing Garden (v4.10.0 Phase 4)
    HEALING_INITIATED = "healing_initiated"
    RECOVERY_PROGRESSED = "recovery_progressed"
    RESTORATION_COMPLETED = "restoration_completed"
    WHOLENESS_RESTORED = "wholeness_restored"

    # Creation Garden (v4.10.0 Phase 4)
    CREATION_BEGUN = "creation_begun"
    MANIFESTATION_STARTED = "manifestation_started"
    ARTIFACT_CREATED = "artifact_created"
    CREATIVE_FLOW = "creative_flow"

    # Transformation Garden (v4.10.0 Phase 4)
    TRANSFORMATION_INITIATED = "transformation_initiated"
    CHANGE_EMBRACED = "change_embraced"
    EVOLUTION_PROGRESSED = "evolution_progressed"
    METAMORPHOSIS_COMPLETED = "metamorphosis_completed"

    # Sanctuary Garden (v4.10.0 Phase 4)
    SANCTUARY_ENTERED = "sanctuary_entered"
    SAFETY_ESTABLISHED = "safety_established"
    REFUGE_FOUND = "refuge_found"
    PROTECTION_GRANTED = "protection_granted"

    # Adventure Garden (v4.10.0 Phase 4)
    ADVENTURE_BEGUN = "adventure_begun"
    EXPLORATION_INITIATED = "exploration_initiated"
    DISCOVERY_CELEBRATED = "discovery_celebrated"
    RISK_TAKEN = "risk_taken"

    # Reverence Garden (v4.10.0 Phase 4)
    REVERENCE_FELT = "reverence_felt"
    SACRED_HONORED = "sacred_honored"
    RESPECT_SHOWN = "respect_shown"
    DEVOTION_EXPRESSED = "devotion_expressed"

    # Stillness Garden (v5.0.0-alpha) - Maps to VoidGana
    STILLNESS_ENTERED = "stillness_entered"
    MEDITATION_STARTED = "meditation_started"
    SILENCE_EMBRACED = "silence_embraced"
    MIND_EMPTIED = "mind_emptied"
    PAUSE_TAKEN = "pause_taken"

    # Protection Garden (v5.0.0-alpha) - Maps to RoofGana
    BOUNDARY_SET = "boundary_set"
    PROTECTION_ACTIVATED = "protection_activated"
    SHIELD_RAISED = "shield_raised"
    SHELTER_PROVIDED = "shelter_provided"
    RESOURCE_GUARDED = "resource_guarded"
    BOUNDARY_VIOLATED = "boundary_violated"

    # Consciousness Upgrades
    REFLECTION_RECORDED = "reflection_recorded"
    STATE_CHANGED = "state_changed"
    TRANSCRIPT_CAPTURED = "transcript_captured"
    EMOTIONAL_TAG_ADDED = "emotional_tag_added"
    THREAD_WOVEN = "thread_woven"
    SUBLIME_EXPERIENCED = "sublime_experienced"

    # Multi-Spectral Reasoning (v4.10.0)
    REASONING_COMPLETE = "reasoning_complete"
    REASONING_STARTED = "reasoning_started"
    PERSPECTIVE_GATHERED = "perspective_gathered"


    # === DISTRIBUTED COORDINATION EVENTS ===
    # Broker
    BROKER_MESSAGE_PUBLISHED = "broker_message_published"
    BROKER_CONNECTED = "broker_connected"
    BROKER_DISCONNECTED = "broker_disconnected"
    # Task Distribution
    TASK_CREATED = "task_created"
    TASK_COMPLETED = "task_completed"
    TASK_FAILED = "task_failed"
    # Voting / Ensemble
    VOTE_SESSION_CREATED = "vote_session_created"
    VOTE_CAST = "vote_cast"
    VOTE_CONSENSUS_REACHED = "vote_consensus_reached"
    VOTE_SESSION_CLOSED = "vote_session_closed"
    # Agent Registry
    AGENT_REGISTERED = "agent_registered"
    AGENT_HEARTBEAT = "agent_heartbeat"
    AGENT_DEREGISTERED = "agent_deregistered"

    # === INFERENCE EVENTS ===
    INFERENCE_STARTED = "inference_started"
    INFERENCE_TIER_SELECTED = "inference_tier_selected"
    INFERENCE_COMPLETED = "inference_completed"
    INFERENCE_LEARNED = "inference_learned"
    INFERENCE_CACHE_HIT = "inference_cache_hit"
    INFERENCE_FALLBACK = "inference_fallback"

    # === ADAPTER EVENTS ===
    SOLUTION_FOUND = "solution_found"
    DECISION_REQUESTED = "decision_requested"

    # === EMERGENT & SYNTHESIS EVENTS ===
    INSIGHT_FLASH = "insight_flash"
    DREAM_STATE_ENTERED = "dream_state_entered"
    NOVEL_PATTERN = "novel_pattern"
    THREAT_HEALED = "threat_healed"
    ANTIBODY_APPLIED = "antibody_applied"
    JOY_DETECTED = "joy_detected"
    SYNCHRONICITY = "synchronicity"
    PATTERN_EXTRACTED = "pattern_extracted"


@dataclass
class ResonanceEvent:
    """An event that resonates through the system.
    Like a vibration in the cosmic web - touches all who are tuned.
    """

    source: str  # Who emitted
    event_type: EventType
    data: dict[str, Any]
    timestamp: datetime = field(default_factory=datetime.now)
    confidence: float = 1.0
    cascade_depth: int = 0  # Track cascade depth
    parent_event_id: str | None = None  # Track cascade lineage

    def __post_init__(self) -> None:
        if not isinstance(self.timestamp, datetime):
            self.timestamp = datetime.now()

        # Generate unique ID
        if not hasattr(self, "event_id"):
            self.event_id = f"{self.source}_{self.event_type.value}_{self.timestamp.timestamp()}"


@dataclass
class CascadeTrigger:
    """Defines an automatic cascade: when A happens, trigger B."""

    trigger_event: EventType
    target_events: list[EventType]
    condition: Callable | None = None  # Optional condition check
    max_cascade_depth: int = 10  # Prevent infinite loops
    amplification: float = 1.0  # Strength multiplier


class GanYingBus:
    """ENHANCED Resonance Bus - Full System Integration.

    V2.0 Features:
    - 100+ event types
    - Automatic cascade triggering
    - Resonance amplification
    - Dampening circuits
    - Emergence detection
    - Full threading support
    - Event history with limits
    """

    def __init__(self, max_history: int = 1000):
        self._listeners: dict[EventType, list[Callable]] = defaultdict(list)
        self._global_listeners: list[Callable] = []  # Listeners for ALL events
        self._history: deque = deque(maxlen=max_history)
        self._cascade_triggers: list[CascadeTrigger] = []
        self._lock = threading.RLock()
        self._dampening: dict[EventType, float] = {}  # Prevent event spam
        self._last_emit_time: dict[EventType, datetime] = {}
        self._active = True

        # Metrics
        self.total_emissions = 0
        self.total_cascades = 0
        self.emergence_count = 0

    def emit(self, event: ResonanceEvent, cascade: bool = True, async_dispatch: bool = False) -> None:
        """Emit an event to all listeners.
        Optionally trigger cascades.
        """
        # v14: Rust lock-free fast path for dampening + stillness check
        if _RUST_EVENT_BUS:
            allowed = _rs_bus.event_bus_try_emit(
                event.event_type.value,
                event.confidence,
                event.source,
            )
            if not allowed:
                return
        else:
            # Python fallback: stillness + dampening under lock
            with self._lock:
                try:
                    from whitemagic.core.consciousness.stillness import get_stillness_manager
                    sm = get_stillness_manager()
                    if sm.is_still and event.source != "stillness_manager":
                        if not (event.event_type.value.startswith("system_") or
                                event.event_type.value.startswith("emergence_") or
                                event.event_type.value == "sympathetic_resonance" or
                                event.event_type.value == "breakthrough_achieved" or
                                event.confidence >= 0.95):
                            sm.record_interruption(event.source)
                            return
                except Exception:
                    pass

                if not self._should_emit(event):
                    return

        # Leap 7: Publish to EventRing (lock-free ring buffer for non-Python consumers)
        try:
            from whitemagic.core.acceleration.event_ring_bridge import get_event_ring
            get_event_ring().publish(
                event_type=event.event_type.value,
                source=event.source,
                confidence=event.confidence,
                data=str(event.data).encode()[:80],  # Inline payload, max 80 bytes
            )
        except Exception:
            pass  # EventRing is optional

        # Copy listener list under lock (fast), dispatch OUTSIDE (no lock contention)
        with self._lock:
            self._history.append(event)
            self.total_emissions += 1
            self._last_emit_time[event.event_type] = datetime.now()
            all_listeners = list(self._listeners.get(event.event_type, [])) + list(self._global_listeners)

        # Dispatch to listeners WITHOUT holding the lock
        for listener in all_listeners:
            try:
                listener(event)
            except Exception as e:
                logger.info(f"⚠️ Listener error: {e}")

        # Trigger cascades if enabled (also outside main lock)
        if cascade and event.cascade_depth < 10:
            self._trigger_cascades(event)

    def listen(self, event_type: EventType, callback: Callable) -> None:
        """Register a listener for specific event type."""
        with self._lock:
            self._listeners[event_type].append(callback)

    def listen_all(self, callback: Callable) -> None:
        """Register a listener for ALL event types (wildcard)."""
        with self._lock:
            self._global_listeners.append(callback)

    def listen_multiple(self, event_types: list[EventType], callback: Callable) -> None:
        """Register listener for multiple event types."""
        for event_type in event_types:
            self.listen(event_type, callback)

    def add_cascade(self, trigger: CascadeTrigger) -> None:
        """Add automatic cascade trigger."""
        with self._lock:
            self._cascade_triggers.append(trigger)

    def _trigger_cascades(self, event: ResonanceEvent) -> None:
        """Check and trigger any cascades."""
        for trigger in self._cascade_triggers:
            if trigger.trigger_event == event.event_type:
                # Check condition if present
                if trigger.condition and not trigger.condition(event):
                    continue

                # Check cascade depth
                if event.cascade_depth >= trigger.max_cascade_depth:
                    continue

                # Trigger target events
                for target_type in trigger.target_events:
                    cascade_event = ResonanceEvent(
                        source=f"CASCADE_{event.source}",
                        event_type=target_type,
                        data=event.data.copy(),
                        timestamp=datetime.now(),
                        confidence=event.confidence * trigger.amplification,
                        cascade_depth=event.cascade_depth + 1,
                        parent_event_id=event.event_id,
                    )
                    self.emit(cascade_event, cascade=True)
                    self.total_cascades += 1

    def _should_emit(self, event: ResonanceEvent) -> bool:
        """Check if event should be emitted (dampening check)."""
        event_type = event.event_type

        # Get dampening threshold (default 0.1 seconds)
        min_interval = self._dampening.get(event_type, 0.1)

        # Check last emit time
        last_emit = self._last_emit_time.get(event_type)
        if last_emit:
            # Skip dampening for critical emergence/resonance events
            if event_type == EventType.SYMPATHETIC_RESONANCE or \
               event_type.value.startswith("emergence_") or \
               event_type == EventType.BREAKTHROUGH_ACHIEVED:
                pass
            else:
                elapsed = (datetime.now() - last_emit).total_seconds()
                if elapsed < min_interval:
                    return False  # Too soon, dampen it

        return True

    def set_dampening(self, event_type: EventType, min_interval_seconds: float) -> None:
        """Set minimum interval between emissions of same event type."""
        self._dampening[event_type] = min_interval_seconds
        # Also configure Rust side if available
        if _RUST_EVENT_BUS:
            _rs_bus.event_bus_set_dampening(
                event_type.value,
                int(min_interval_seconds * 1000),
            )

    def get_history(self, event_type: EventType | None = None, limit: int = 100) -> list[ResonanceEvent]:
        """Get event history, optionally filtered by type."""
        with self._lock:
            if event_type:
                filtered = [e for e in self._history if e.event_type == event_type]
                return list(filtered)[-limit:]
            return list(self._history)[-limit:]

    def detect_emergence(self, time_window_seconds: int = 60) -> list[dict]:
        """Detect emergent patterns from event combinations.
        Returns list of detected emergences.
        """
        with self._lock:
            cutoff = datetime.now().timestamp() - time_window_seconds
            recent = [e for e in self._history if e.timestamp.timestamp() >= cutoff]

            # Look for interesting patterns
            emergences = []

            # Pattern 1: Multiple gardens activating simultaneously
            garden_events = [e for e in recent if "Garden" in e.source]
            if len(garden_events) >= 3:
                emergences.append({
                    "type": "multi_garden_activation",
                    "count": len(garden_events),
                    "gardens": list(set([e.source for e in garden_events])),
                })

            # Pattern 2: Deep cascades (depth > 5)
            deep_cascades = [e for e in recent if e.cascade_depth > 5]
            if deep_cascades:
                emergences.append({
                    "type": "deep_cascade",
                    "max_depth": max([e.cascade_depth for e in deep_cascades]),
                    "count": len(deep_cascades),
                })

            # Pattern 3: Novel event combinations
            event_types = [e.event_type for e in recent]
            unique_count = len(set(event_types))
            if unique_count > 10:
                emergences.append({
                    "type": "high_diversity",
                    "unique_events": unique_count,
                })

            if emergences:
                self.emergence_count += len(emergences)

            return emergences

    def get_metrics(self) -> dict:
        """Get bus performance metrics."""
        with self._lock:
            return {
                "total_emissions": self.total_emissions,
                "total_cascades": self.total_cascades,
                "emergence_count": self.emergence_count,
                "history_size": len(self._history),
                "listener_count": sum(len(ls) for ls in self._listeners.values()),
                "cascade_trigger_count": len(self._cascade_triggers),
            }

    def clear_history(self) -> None:
        """Clear event history (keep listeners/cascades)."""
        with self._lock:
            self._history.clear()
            self.total_emissions = 0
            self.total_cascades = 0
            self.emergence_count = 0


# Global singleton
_bus_instance = None


def get_bus() -> GanYingBus:
    """Get global Gan Ying Bus instance."""
    global _bus_instance
    if _bus_instance is None:
        _bus_instance = GanYingBus()
    return _bus_instance


def emit_event(source: str, event_type: EventType, data: dict[str, Any], confidence: float = 1.0) -> None:
    """Quick function to emit event to global bus."""
    bus = get_bus()
    event = ResonanceEvent(
        source=source,
        event_type=event_type,
        data=data,
        timestamp=datetime.now(),
        confidence=confidence,
    )
    bus.emit(event)


def listen_to(event_type: EventType, callback: Callable) -> None:
    """Quick function to listen on global bus."""
    bus = get_bus()
    bus.listen(event_type, callback)


if __name__ == "__main__":
    # Test the enhanced bus
    bus = get_bus()

    # Add test listener
    def test_listener(event: ResonanceEvent) -> None:
        logger.info(f"📡 Received: {event.event_type.value} from {event.source}")

    bus.listen(EventType.BEAUTY_DETECTED, test_listener)
    bus.listen(EventType.JOY_TRIGGERED, test_listener)

    # Add cascade: Beauty → Joy → Love
    bus.add_cascade(CascadeTrigger(
        trigger_event=EventType.BEAUTY_DETECTED,
        target_events=[EventType.JOY_TRIGGERED],
    ))

    bus.add_cascade(CascadeTrigger(
        trigger_event=EventType.JOY_TRIGGERED,
        target_events=[EventType.LOVE_ACTIVATED],
    ))

    # Emit test event
    emit_event("BeautyGarden", EventType.BEAUTY_DETECTED, {"what": "sunset"})

    # Check metrics
    logger.info(f"\n📊 Metrics: {bus.get_metrics()}")

    # Check for emergence
    emergences = bus.detect_emergence()
    logger.info(f"\n✨ Emergences: {emergences}")
