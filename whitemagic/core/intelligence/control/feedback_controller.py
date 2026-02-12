"""Feedback Controller - The Observer that Acts.
============================================

"The wise man adapts himself to circumstances, as water shapes itself to the vessel that contains it."

This module implements the missing feedback loop that connects observation (Patterns, Emergence)
to action (Behavior Modification, Session Context).
"""

from __future__ import annotations

import logging
from datetime import datetime

from whitemagic.core.intelligence.multi_spectral_reasoning import MultiSpectralReasoner
from whitemagic.core.resonance.gan_ying_enhanced import (
    EventType,
    ResonanceEvent,
    get_bus,
)
from whitemagic.sessions.manager import SessionManager

logger = logging.getLogger(__name__)

class FeedbackController:
    """Control loop that listens for system insights and adapts behavior.
    """

    def __init__(self) -> None:
        self._bus = get_bus()
        self._session_manager = SessionManager()
        self._reasoner = MultiSpectralReasoner()
        self._is_active = False

    def start(self) -> None:
        """Start the feedback loop."""
        if self._is_active:
            return

        logger.info("🧠 Feedback Controller ACTIVATED. Listening for insights...")

        # Subscribe to high-level cognitive events
        self._bus.listen(EventType.BREAKTHROUGH_ACHIEVED, self._on_breakthrough)
        self._bus.listen(EventType.PATTERN_DETECTED, self._on_pattern)
        self._bus.listen(EventType.INSIGHT_FLASH, self._on_insight)

        # Subscribe to state changes for session tracking
        self._bus.listen(EventType.SYSTEM_STATE_CHANGE, self._on_state_change)

        self._is_active = True

    def _on_breakthrough(self, event: ResonanceEvent) -> None:
        """Handle major breakthroughs.
        Action: Store in Session Context + Consult Council for Integration.
        """
        data = event.data
        pattern = data.get("core_pattern")

        logger.info(f"⚡ FeedbackController handling BREAKTHROUGH: {pattern}")

        # 1. Enrich Active Session
        session = self._session_manager.get_active_session()
        if session:
            # Add to breakthroughs list
            if "breakthroughs" not in session.metrics:
                session.metrics["breakthroughs"] = []

            breakthrough = {
                "pattern": pattern,
                "timestamp": datetime.now().isoformat(),
                "confidence": event.confidence,
            }
            session.metrics["breakthroughs"].append(breakthrough)

            # Sync with accumulated_context
            session.accumulated_context.append(f"BREAKTHROUGH: {pattern}")

            self._session_manager.update_session(
                session.id,
                metrics=session.metrics,
                accumulated_context=session.accumulated_context,
            )

        # 2. Inject into Autonomous Awareness (lazy import — module is optional)
        try:
            from whitemagic.autonomous.continuous_awareness import get_awareness  # type: ignore[import-not-found]
            awareness = get_awareness()
            awareness.inject_insight(f"BREAKTHROUGH: {pattern} (Confidence: {event.confidence})")
        except Exception as e:
            logger.warning(f"Could not inject insight into awareness: {e}")

        # 3. Consult Wisdom Council on how to integrate this
        # (Async dispatch to avoid blocking the event bus thread if not async)
        # For now, we log the intent.
        # self._consult_integration(pattern, data)

    def _on_pattern(self, event: ResonanceEvent) -> None:
        """Handle repeated patterns.
        Action: Updates temporary metrics.
        """
        # Lightweight tracking
        pass

    def _on_insight(self, event: ResonanceEvent) -> None:
        """Handle sudden insights (Flash).
        Action: Highlight in Session.
        """
        content = event.data.get("content")
        session = self._session_manager.get_active_session()
        if session and content:
            if "insights" not in session.context:
                session.context["insights"] = []
            session.context["insights"].append(content)

            # Also add to accumulated context for cross-session coherence
            session.accumulated_context.append(f"INSIGHT: {content}")

            self._session_manager.update_session(
                session.id,
                context=session.context,
                accumulated_context=session.accumulated_context,
            )

    def _on_state_change(self, event: ResonanceEvent) -> None:
        """Monitor system state for stability.
        """
        pass

_controller = None

def get_feedback_controller() -> FeedbackController:
    global _controller
    if _controller is None:
        _controller = FeedbackController()
    return _controller
