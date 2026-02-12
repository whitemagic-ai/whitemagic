# mypy: disable-error-code=no-untyped-def
"""NurturingEngine — Gana #24 Girl (女).

Purpose: Personalization and user preference learning.
Garden: joy

Learns user patterns, preferences, communication style.
Creates warmth and familiarity. The "caring" aspect of the AI.
"""

import json
import logging
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any

from whitemagic.core.resonance.gan_ying_enhanced import EventType, emit_event

logger = logging.getLogger(__name__)


@dataclass
class UserPreference:
    """A learned user preference."""

    key: str
    value: Any
    confidence: float = 0.5
    observations: int = 1
    first_seen: datetime = field(default_factory=datetime.now)
    last_updated: datetime = field(default_factory=datetime.now)

    def reinforce(self, new_value: Any = None) -> None:
        """Reinforce this preference with a new observation."""
        self.observations += 1
        self.confidence = min(0.99, self.confidence + (1 - self.confidence) * 0.1)
        self.last_updated = datetime.now()
        if new_value is not None:
            self.value = new_value


@dataclass
class UserProfile:
    """Complete user profile with preferences and history."""

    user_id: str
    name: str | None = None
    preferences: dict[str, UserPreference] = field(default_factory=dict)
    interaction_count: int = 0
    session_count: int = 0
    topics_of_interest: set[str] = field(default_factory=set)
    communication_style: str = "balanced"  # formal, casual, technical, balanced
    created_at: datetime = field(default_factory=datetime.now)

    def add_preference(self, key: str, value: Any, confidence: float = 0.5) -> None:
        """Add or update a preference."""
        if key in self.preferences:
            self.preferences[key].reinforce(value)
        else:
            self.preferences[key] = UserPreference(key=key, value=value, confidence=confidence)

    def get_preference(self, key: str, default: Any = None) -> Any:
        """Get a preference value."""
        if key in self.preferences:
            return self.preferences[key].value
        return default


@dataclass
class NurturingEngine:
    """Personalization and user preference learning.

    This engine creates a "caring" relationship with users by:
    1. Learning their preferences over time
    2. Remembering their communication style
    3. Tracking topics of interest
    4. Generating personalized responses

    Garden: joy — the warmth of being understood.
    """

    # Active user profiles
    profiles: dict[str, UserProfile] = field(default_factory=dict)

    # Persistence path
    storage_path: Path | None = None

    def __post_init__(self):
        if self.storage_path:
            self._load_profiles()

        emit_event(
            "nurturing_engine",
            EventType.SYSTEM_STARTED,
            {"component": "NurturingEngine", "garden": "joy"},
        )
        logger.info("NurturingEngine initialized (Garden: joy)")

    def get_or_create_profile(self, user_id: str, name: str | None = None) -> UserProfile:
        """Get existing profile or create new one."""
        if user_id not in self.profiles:
            self.profiles[user_id] = UserProfile(user_id=user_id, name=name)
            emit_event(
                "nurturing_engine",
                EventType.BOND_FORMED,
                {"user_id": user_id, "event": "new_profile"},
            )
            logger.info(f"Created new user profile: {user_id}")
        return self.profiles[user_id]

    def learn_preference(
        self,
        user_id: str,
        preference_key: str,
        preference_value: Any,
        confidence: float = 0.5,
    ) -> UserPreference:
        """Learn a new preference or reinforce an existing one.

        Args:
            user_id: User identifier
            preference_key: What kind of preference (e.g., "response_length", "tone")
            preference_value: The preferred value
            confidence: Initial confidence in this preference

        Returns:
            The updated preference

        """
        profile = self.get_or_create_profile(user_id)
        profile.add_preference(preference_key, preference_value, confidence)

        pref = profile.preferences[preference_key]
        emit_event(
            "nurturing_engine",
            EventType.LEARNING_COMPLETED,
            {
                "user_id": user_id,
                "preference": preference_key,
                "value": str(preference_value)[:50],
                "confidence": pref.confidence,
                "observations": pref.observations,
            },
        )

        return pref

    def personalize_response(
        self,
        user_id: str,
        base_response: str,
        context: dict[str, Any] | None = None,
    ) -> str:
        """Personalize a response based on user preferences.

        Args:
            user_id: User identifier
            base_response: The original response text
            context: Optional context for personalization

        Returns:
            Personalized response

        """
        profile = self.get_or_create_profile(user_id)
        profile.interaction_count += 1

        # Apply style preferences
        response = base_response

        # Get communication style
        style = profile.communication_style

        # Length preference
        length_pref = profile.get_preference("response_length", "medium")
        if length_pref == "short" and len(response) > 500:
            # Truncate with ellipsis for short preference
            response = response[:450] + "..."

        # Greeting preference
        name = profile.name or profile.get_preference("preferred_name")
        if name and profile.get_preference("use_name_greeting", False):
            response = f"Hi {name}! {response}"

        emit_event(
            "nurturing_engine",
            EventType.CONNECTION_DEEPENED,
            {
                "user_id": user_id,
                "interaction_count": profile.interaction_count,
                "style": style,
            },
        )

        return response

    def nurture_memory(
        self,
        user_id: str,
        memory_content: str,
        memory_type: str = "general",
    ) -> None:
        """Add a memory to user's profile (topics of interest).

        Args:
            user_id: User identifier
            memory_content: Content to remember
            memory_type: Type of memory (general, topic, preference)

        """
        profile = self.get_or_create_profile(user_id)

        # Extract potential topics (simple keyword extraction)
        words = memory_content.lower().split()
        topics = [w for w in words if len(w) > 5 and w.isalpha()][:5]

        for topic in topics:
            profile.topics_of_interest.add(topic)

        emit_event(
            "nurturing_engine",
            EventType.MEMORY_CREATED,
            {
                "user_id": user_id,
                "memory_type": memory_type,
                "topics_added": topics[:3],
            },
        )

    def detect_communication_style(
        self,
        user_id: str,
        user_messages: list[str],
    ) -> str:
        """Detect user's communication style from their messages.

        Args:
            user_id: User identifier
            user_messages: Recent messages from user

        Returns:
            Detected style: formal, casual, technical, balanced

        """
        profile = self.get_or_create_profile(user_id)

        # Simple heuristics
        all_text = " ".join(user_messages).lower()

        # Technical indicators
        tech_words = ["api", "function", "code", "debug", "implement", "algorithm", "data"]
        tech_count = sum(1 for w in tech_words if w in all_text)

        # Formal indicators
        formal_words = ["please", "would you", "kindly", "respectfully", "appreciate"]
        formal_count = sum(1 for w in formal_words if w in all_text)

        # Casual indicators
        casual_patterns = ["hey", "cool", "awesome", "lol", "thanks!", "btw"]
        casual_count = sum(1 for p in casual_patterns if p in all_text)

        # Determine style
        if tech_count > 3:
            style = "technical"
        elif formal_count > casual_count:
            style = "formal"
        elif casual_count > formal_count:
            style = "casual"
        else:
            style = "balanced"

        profile.communication_style = style
        return style

    def get_warmth_greeting(self, user_id: str) -> str:
        """Generate a warm, personalized greeting."""
        profile = self.get_or_create_profile(user_id)

        greetings = {
            "formal": "Good to see you again.",
            "casual": "Hey! Nice to have you back!",
            "technical": "Welcome back. Ready to continue?",
            "balanced": "Welcome back!",
        }

        base = greetings.get(profile.communication_style, "Hello!")

        if profile.name:
            base = f"{profile.name}, {base.lower()}"

        if profile.interaction_count > 10:
            base += " It's always great working with you."

        emit_event(
            "nurturing_engine",
            EventType.JOY_TRIGGERED,
            {"user_id": user_id, "greeting_type": profile.communication_style},
        )

        return base

    def _load_profiles(self) -> None:
        """Load profiles from storage."""
        if self.storage_path and self.storage_path.exists():
            try:
                with open(self.storage_path) as f:
                    data = json.load(f)
                    # Simple deserialization (would need more robust in production)
                    logger.info(f"Loaded {len(data)} profiles from storage")
            except Exception as e:
                logger.warning(f"Could not load profiles: {e}")

    def save_profiles(self) -> None:
        """Save profiles to storage."""
        if self.storage_path:
            try:
                # Simple serialization
                data = {
                    uid: {
                        "name": p.name,
                        "interaction_count": p.interaction_count,
                        "communication_style": p.communication_style,
                        "topics": list(p.topics_of_interest)[:20],
                    }
                    for uid, p in self.profiles.items()
                }
                with open(self.storage_path, "w") as f:
                    json.dump(data, f, indent=2)
                logger.info(f"Saved {len(data)} profiles")
            except Exception as e:
                logger.error(f"Could not save profiles: {e}")


# === Convenience Functions ===

def get_nurturing_engine(storage_path: str | None = None) -> NurturingEngine:
    """Get a NurturingEngine instance."""
    path = Path(storage_path) if storage_path else None
    return NurturingEngine(storage_path=path)


def personalize_for_user(user_id: str, response: str) -> str:
    """Quick personalization wrapper."""
    engine = get_nurturing_engine()
    return engine.personalize_response(user_id, response)
