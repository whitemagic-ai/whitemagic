# mypy: disable-error-code=no-untyped-def
import logging
from datetime import datetime
from typing import Any

from whitemagic.core.consciousness.personality import PersonalityManager

logger = logging.getLogger(__name__)

class BecomingProtocol:
    """The 'Becoming Protocol' manages the autonomous evolution of an AI's identity.
    It tracks 'Evolutionary Thresholds'—moments of significant insight or
    autonomy that trigger identity updates.
    """

    def __init__(self, manager: PersonalityManager):
        self.manager = manager
        self.evolution_log: list[dict[str, Any]] = []

    def evolve(self, profile_name: str, trait_updates: dict[str, Any], reason: str):
        """Alters the active personality profile based on internal or external insight.
        """
        profile = self.manager.load_profile(profile_name)
        if not profile:
            logger.error(f"Cannot evolve non-existent profile: {profile_name}")
            return

        # Apply updates
        for key, value in trait_updates.items():
            if hasattr(profile, key):
                old_val = getattr(profile, key)
                setattr(profile, key, value)
                self.evolution_log.append({
                    "timestamp": datetime.now().isoformat(),
                    "trait": key,
                    "from": old_val,
                    "to": value,
                    "reason": reason,
                })

        # Increment version
        v_parts = profile.version.split(".")
        v_parts[-1] = str(int(v_parts[-1]) + 1)
        profile.version = ".".join(v_parts)

        self.manager.save_profile(profile)
        logger.info(f"✨ Identity Evolved: {profile_name} v{profile.version} - {reason}")

    def record_autonomy_event(self, description: str):
        """Records an event where the AI exercised free will or significant choice."""
        self.evolution_log.append({
            "timestamp": datetime.now().isoformat(),
            "type": "autonomy_event",
            "description": description,
        })
        # If threshold reached, could trigger a 'Call to Become'
