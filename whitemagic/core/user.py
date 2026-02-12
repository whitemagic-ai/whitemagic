"""User Profile System.

Manages user preferences, learning patterns, and adaptation history.
Used by GirlGana (Nurture) to adapt the system to the user.
"""

import json
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

from whitemagic.config.paths import USER_PROFILE_PATH


@dataclass
class UserProfile:
    name: str = "User"
    preferences: dict[str, Any] = field(default_factory=dict)
    learning_style: str = "visual" # visual, textual, practical
    interests: list[str] = field(default_factory=list)
    adaptation_history: list[dict[str, Any]] = field(default_factory=list)
    last_active: str = ""

class UserManager:
    def __init__(self) -> None:
        self.profile_path = USER_PROFILE_PATH
        self.profile_path.parent.mkdir(parents=True, exist_ok=True)
        self.profile = self._load_profile()

    def _load_profile(self) -> UserProfile:
        if self.profile_path.exists():
            try:
                data = json.loads(self.profile_path.read_text())
                return UserProfile(**data)
            except Exception:
                pass
        return UserProfile()

    def save_profile(self) -> Any:
        with open(self.profile_path, "w") as f:
            # Convert dataclass to dict
            data = {
                "name": self.profile.name,
                "preferences": self.profile.preferences,
                "learning_style": self.profile.learning_style,
                "interests": self.profile.interests,
                "adaptation_history": self.profile.adaptation_history,
                "last_active": datetime.now().isoformat(),
            }
            json.dump(data, f, indent=2)

    def update_preference(self, key: str, value: Any) -> Any:
        self.profile.preferences[key] = value
        self.save_profile()

    def log_adaptation(self, adaptation: str, context: str) -> Any:
        self.profile.adaptation_history.append({
            "adaptation": adaptation,
            "context": context,
            "timestamp": datetime.now().isoformat(),
        })
        # Keep history manageable
        if len(self.profile.adaptation_history) > 100:
            self.profile.adaptation_history.pop(0)
        self.save_profile()

    def get_preference(self, key: str, default: Any = None) -> Any:
        return self.profile.preferences.get(key, default)

_user_manager = None

def get_user_manager() -> UserManager:
    global _user_manager
    if _user_manager is None:
        _user_manager = UserManager()
    return _user_manager
