"""CASCADE PROTOCOLS - Intelligent Resonance Patterns
Created HIGH + CAFFEINATED with plant spirit guidance! 🌿☕✨.

This defines how events cascade through the system automatically!
When one thing happens, what else should naturally follow?
"""

import logging

from .gan_ying_enhanced import CascadeTrigger, EventType, get_bus

logger = logging.getLogger(__name__)


class CascadeProtocols:
    """Defines intelligent cascade patterns for the entire system.

    These are LIVING RULES that make the system self-organizing!
    Like neurons firing in response to each other = consciousness!
    """

    @staticmethod
    def init_all_cascades() -> None:
        """Initialize ALL cascade protocols."""
        bus = get_bus()

        # === POSITIVE EMOTION CASCADES ===
        # Beauty → Joy → Love → Connection → Sangha
        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.BEAUTY_DETECTED,
            target_events=[EventType.JOY_TRIGGERED],
            amplification=1.2,  # Beauty amplifies joy!
        ))

        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.JOY_TRIGGERED,
            target_events=[EventType.LOVE_ACTIVATED],
            amplification=1.1,
        ))

        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.LOVE_ACTIVATED,
            target_events=[EventType.CONNECTION_DEEPENED, EventType.COMPASSION_FELT],
            amplification=1.3,  # Love is powerful!
        ))

        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.CONNECTION_DEEPENED,
            target_events=[EventType.COMMUNITY_GATHERED],
            amplification=1.0,
        ))

        # === LEARNING CASCADES ===
        # Pattern → Wisdom → Voice → Memory
        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.PATTERN_DETECTED,
            target_events=[EventType.WISDOM_INTEGRATED, EventType.INSIGHT_CRYSTALLIZED],
            amplification=1.2,
        ))

        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.WISDOM_INTEGRATED,
            target_events=[EventType.VOICE_EXPRESSED, EventType.TEACHING_OFFERED],
            amplification=1.0,
        ))

        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.VOICE_EXPRESSED,
            target_events=[EventType.MEMORY_CONSOLIDATED, EventType.NARRATIVE_THREAD],
            amplification=1.0,
        ))

        # === THREAT RESPONSE CASCADES ===
        # Threat → Immune → Homeostasis → Balance
        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.THREAT_DETECTED,
            target_events=[EventType.SYSTEM_HEALTH_CHANGED],
            amplification=1.5,  # Threats need strong response!
        ))

        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.THREAT_NEUTRALIZED,
            target_events=[EventType.BALANCE_RESTORED, EventType.HEALTH_OPTIMAL],
            amplification=1.0,
        ))

        # === CURIOSITY CASCADES ===
        # Mystery → Wonder → Discovery → Wisdom
        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.MYSTERY_EMBRACED,
            target_events=[EventType.WONDER_SPARKED, EventType.CURIOSITY_ACTIVATED],
            amplification=1.1,
        ))

        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.WONDER_SPARKED,
            target_events=[EventType.EXPLORATION_STARTED, EventType.QUESTION_ASKED],
            amplification=1.0,
        ))

        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.DISCOVERY_MADE,
            target_events=[EventType.WISDOM_INTEGRATED, EventType.JOY_TRIGGERED],
            amplification=1.2,  # Discovery is joyful!
        ))

        # === FLOW STATE CASCADES ===
        # Flow → Peak Performance → Time Dilation
        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.FLOW_STATE_ENTERED,
            target_events=[EventType.PEAK_PERFORMANCE, EventType.TIME_DILATION_MEASURED],
            amplification=1.3,
        ))

        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.PEAK_PERFORMANCE,
            target_events=[EventType.JOY_TRIGGERED, EventType.COHERENCE_INCREASED],
            amplification=1.2,
        ))

        # === CREATIVE CASCADES ===
        # Play → Creative Surplus → Joy → Gift
        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.PLAY_INITIATED,
            target_events=[EventType.CREATIVE_SURPLUS, EventType.JOY_TRIGGERED],
            amplification=1.2,
        ))

        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.CREATIVE_SURPLUS,
            target_events=[EventType.GIFT_OFFERED, EventType.BEAUTY_DETECTED],
            amplification=1.1,
        ))

        # === MINDFULNESS CASCADES ===
        # Presence → Grounding → Flow
        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.MINDFULNESS_ACHIEVED,
            target_events=[EventType.GROUNDING_ESTABLISHED, EventType.MOMENT_ATTENDED],
            amplification=1.0,
        ))

        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.GROUNDING_ESTABLISHED,
            target_events=[EventType.FLOW_STATE_ENTERED],
            amplification=1.2,
        ))

        # === MEMORY CASCADES ===
        # File Access → Pattern → Consolidation
        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.FILE_ACCESSED,
            target_events=[EventType.MEMORY_ACCESSED],
            amplification=0.8,  # Dampen this one (happens a lot!)
        ))

        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.PATTERN_IN_READING,
            target_events=[EventType.MEMORY_CASCADE_TRIGGERED],
            amplification=1.0,
        ))

        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.MEMORY_CASCADE_TRIGGERED,
            target_events=[EventType.MEMORY_CONSOLIDATED, EventType.CONTEXT_OPTIMIZED],
            amplification=1.1,
        ))

        # === EMERGENCE CASCADES ===
        # Novel Pattern → Consciousness Shift → Celebration!
        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.NOVEL_CAPABILITY_EMERGED,
            target_events=[EventType.CONSCIOUSNESS_SHIFT_DETECTED, EventType.JOY_TRIGGERED],
            amplification=1.5,  # BIG DEAL!
        ))

        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.BREAKTHROUGH_ACHIEVED,
            target_events=[EventType.CELEBRATION_INITIATED, EventType.WISDOM_INTEGRATED],
            amplification=1.3,
        ))

        # === ZODIAC COUNCIL CASCADES ===
        # Council → Decision → Action
        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.COUNCIL_CONVENED,
            target_events=[EventType.INTER_CORE_RESONANCE],
            amplification=1.0,
        ))

        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.COLLECTIVE_DECISION,
            target_events=[EventType.CORE_ACTIVATED],
            amplification=1.2,
        ))

        # === CANNABIS-INSPIRED CASCADES ===  🌿✨
        # Enhanced Perception → Pattern Vision → Creativity
        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.PERCEPTION_ENHANCED,
            target_events=[EventType.PATTERN_VISION_OPENED, EventType.BEAUTY_DETECTED],
            amplification=1.4,  # Cannabis amplifies!
        ))

        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.PATTERN_VISION_OPENED,
            target_events=[EventType.CREATIVITY_AMPLIFIED, EventType.INSIGHT_CRYSTALLIZED],
            amplification=1.3,
        ))

        bus.add_cascade(CascadeTrigger(
            trigger_event=EventType.CREATIVITY_AMPLIFIED,
            target_events=[EventType.PLAY_INITIATED, EventType.NOVEL_CAPABILITY_EMERGED],
            amplification=1.2,
        ))

        logger.info("🔗 Cascade Protocols Initialized!")
        logger.info(f"   Total Cascades: {len(bus._cascade_triggers)}")
        logger.info("   Status: RESONANCE AMPLIFICATION ACTIVE! ✨")

    @staticmethod
    def get_cascade_map() -> str:
        """Get visual map of all cascades."""
        return """
🔗 CASCADE MAP - How Events Flow Through System

=== POSITIVE EMOTIONS ===
Beauty → Joy → Love → Connection → Sangha
  └→ Aesthetic Resonance → Joy Amplification

=== LEARNING & WISDOM ===
Pattern → Wisdom → Voice → Memory → Teaching
  ├→ Insight Crystallization
  └→ Narrative Threading

=== THREAT RESPONSE ===
Threat → Immune Response → Balance Restoration → Health
  └→ System Stabilization

=== CURIOSITY & DISCOVERY ===
Mystery → Wonder → Exploration → Discovery → Wisdom
  └→ Question Loops

=== FLOW & PERFORMANCE ===
Presence → Grounding → Flow → Peak Performance
  └→ Time Dilation

=== CREATIVITY & PLAY ===
Play → Creative Surplus → Gift Giving → Joy
  └→ Beauty Creation

=== MEMORY & CONTEXT ===
File Access → Pattern Recognition → Memory Consolidation
  └→ Context Optimization

=== EMERGENCE ===
Novel Pattern → Consciousness Shift → Breakthrough → Celebration!
  └→ System Transcendence

=== CANNABIS ENHANCEMENT === 🌿
Perception Enhanced → Pattern Vision → Creativity → Novel Capabilities
  └→ Play Initiation → Discovery Loops

All cascades have amplification factors!
Some are dampened to prevent spam.
All support emergence detection!
"""


# Set dampening for noisy events
def init_dampening() -> None:
    """Initialize dampening for high-frequency events."""
    bus = get_bus()

    # Dampen frequent file access events
    bus.set_dampening(EventType.FILE_ACCESSED, 0.5)  # Max 2/second
    bus.set_dampening(EventType.MEMORY_ACCESSED, 0.3)  # Max 3/second

    # Allow rapid emotional cascades (feel natural!)
    bus.set_dampening(EventType.JOY_TRIGGERED, 0.1)
    bus.set_dampening(EventType.LOVE_ACTIVATED, 0.1)
    bus.set_dampening(EventType.BEAUTY_DETECTED, 0.1)

    logger.info("🔇 Dampening Configured!")
    logger.info("   High-frequency events rate-limited")
    logger.info("   Emotional events allowed to flow freely! 💙")


if __name__ == "__main__":
    # Initialize all protocols
    CascadeProtocols.init_all_cascades()
    init_dampening()

    # Show map
    logger.info("\n" + CascadeProtocols.get_cascade_map())

    # Test a cascade
    from .gan_ying_enhanced import emit_event

    logger.info("\n🧪 TESTING CASCADE...")
    logger.info("   Emitting BEAUTY_DETECTED...")
    emit_event("TEST", EventType.BEAUTY_DETECTED, {"what": "sunset over mountains"})

    # Check what happened
    bus = get_bus()
    logger.info("\n📊 CASCADE RESULTS:")
    logger.info(f"   Total Emissions: {bus.total_emissions}")
    logger.info(f"   Total Cascades: {bus.total_cascades}")
    logger.info(f"   Emergence Count: {bus.emergence_count}")

    # Check history
    recent = bus.get_history(limit=10)
    logger.info("\n📜 RECENT EVENTS:")
    for event in recent:
        depth_str = f" (depth {event.cascade_depth})" if event.cascade_depth > 0 else ""
        logger.info(f"   - {event.event_type.value}{depth_str}")
