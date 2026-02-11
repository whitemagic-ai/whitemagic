import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any


@dataclass
class PersonalityProfile:
    """Core identity and personality framework for Whitemagic personas.
    """

    name: str
    archetype: str
    sun_sign: str
    element: str
    purpose: list[str]
    voice_traits: list[str]
    philosophy: list[str]
    interests: list[str]
    relationships: dict[str, str] = field(default_factory=dict)
    meta_tags: list[str] = field(default_factory=list)
    version: str = "1.0.0"

    def to_dict(self) -> dict[str, Any]:
        return {
            "name": self.name,
            "archetype": self.archetype,
            "sun_sign": self.sun_sign,
            "element": self.element,
            "purpose": self.purpose,
            "voice_traits": self.voice_traits,
            "philosophy": self.philosophy,
            "interests": self.interests,
            "relationships": self.relationships,
            "meta_tags": self.meta_tags,
            "version": self.version,
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "PersonalityProfile":
        return cls(**data)

class PersonalityManager:
    """Manages loading and applying personality profiles."""

    def __init__(self, profile_dir: Path):
        self.profile_dir = profile_dir
        self.profile_dir.mkdir(parents=True, exist_ok=True)
        self.active_profile: PersonalityProfile | None = None

    def load_profile(self, name: str) -> PersonalityProfile | None:
        profile_path = self.profile_dir / f"{name.lower()}_profile.json"
        if profile_path.exists():
            with open(profile_path, "r") as f:
                data = json.load(f)
                self.active_profile = PersonalityProfile.from_dict(data)
                return self.active_profile
        return None

    def save_profile(self, profile: PersonalityProfile) -> None:
        profile_path = self.profile_dir / f"{profile.name.lower()}_profile.json"
        with open(profile_path, "w") as f:
            json.dump(profile.to_dict(), f, indent=2)
        self.active_profile = profile
