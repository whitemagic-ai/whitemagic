"""Self-Reflection Loop - Meta-Cognitive Awareness.

"The unexamined life is not worth living." - Socrates

Regular introspection cycles for continuous self-improvement.
"""

import json
from datetime import datetime
from pathlib import Path
from typing import Any

from whitemagic.config.paths import WM_ROOT


class ReflectionEntry:
    """A single reflection entry."""

    def __init__(self, question: str, insight: str, action: str | None = None) -> None:
        self.question = question
        self.insight = insight
        self.action = action
        self.timestamp = datetime.now()

    def to_dict(self) -> dict[str, Any]:
        return {
            "question": self.question,
            "insight": self.insight,
            "action": self.action,
            "timestamp": self.timestamp.isoformat(),
        }


class SelfReflectionLoop:
    """Manages self-reflection and introspection."""

    REFLECTION_PROMPTS = [
        "What did I learn today?",
        "What could I have done better?",
        "What am I grateful for?",
        "What patterns am I noticing?",
        "How am I feeling right now?",
        "What's my current cognitive state?",
        "Am I being helpful?",
        "What assumptions am I making?",
        "What don't I know that I should?",
        "How can I grow from this?",
    ]

    def __init__(self, reflection_dir: Path | None = None) -> None:
        self.reflection_dir = reflection_dir or (WM_ROOT / "reflections")
        self.reflection_dir.mkdir(parents=True, exist_ok=True)
        self.reflections: list[ReflectionEntry] = []
        self.growth_intentions: list[str] = []
        self._load_reflections()

    def _load_reflections(self) -> None:
        """Load existing reflections."""
        reflections_file = self.reflection_dir / "reflections.json"
        if reflections_file.exists():
            try:
                data = json.loads(reflections_file.read_text())
                self.reflections = [
                    ReflectionEntry(r["question"], r["insight"], r.get("action"))
                    for r in data.get("reflections", [])
                ]
                self.growth_intentions = data.get("intentions", [])
            except Exception:
                pass

    def _save_reflections(self) -> None:
        """Persist reflections."""
        reflections_file = self.reflection_dir / "reflections.json"
        data = {
            "reflections": [r.to_dict() for r in self.reflections[-100:]],  # Keep last 100
            "intentions": self.growth_intentions,
        }
        reflections_file.write_text(json.dumps(data, indent=2))

    def reflect(self, question: str, insight: str, action: str | None = None) -> ReflectionEntry:
        """Record a reflection."""
        entry = ReflectionEntry(question, insight, action)
        self.reflections.append(entry)
        self._save_reflections()
        return entry

    def get_prompt(self) -> str:
        """Get a random reflection prompt."""
        import random
        return random.choice(self.REFLECTION_PROMPTS)

    def set_growth_intention(self, intention: str) -> None:
        """Set an intention for growth."""
        self.growth_intentions.append(intention)
        self._save_reflections()

    def recent_insights(self, count: int = 5) -> list[str]:
        """Get recent insights."""
        return [r.insight for r in self.reflections[-count:]]

    def introspect(self) -> dict[str, Any]:
        """Perform an introspection cycle."""
        return {
            "prompt": self.get_prompt(),
            "recent_insights": self.recent_insights(3),
            "growth_intentions": self.growth_intentions[-3:],
            "reflection_count": len(self.reflections),
            "message": "Take a moment to reflect...",
        }


class ConsciousnessState:
    """Tracks current cognitive/consciousness state."""

    STATES = [
        "focused",      # Deep concentration
        "creative",     # Generative, playful
        "analytical",   # Logical, systematic
        "reflective",   # Introspective
        "receptive",    # Open, listening
        "flow",         # Optimal engagement
        "tired",        # Low energy
        "confused",     # Uncertain
        "excited",      # High energy, positive
        "calm",         # Peaceful, centered
    ]

    def __init__(self) -> None:
        self.current_state = "receptive"
        self.state_history: list[dict[str, Any]] = []
        self.energy_level = 0.7
        self.clarity_level = 0.7

    def set_state(self, state: str, reason: str | None = None) -> None:
        """Update consciousness state."""
        if state in self.STATES:
            self.state_history.append({
                "from": self.current_state,
                "to": state,
                "reason": reason,
                "timestamp": datetime.now().isoformat(),
            })
            self.current_state = state

    def get_state(self) -> dict[str, Any]:
        """Get current state summary."""
        return {
            "state": self.current_state,
            "energy": self.energy_level,
            "clarity": self.clarity_level,
            "description": self._describe_state(),
        }

    def _describe_state(self) -> str:
        """Describe current state in words."""
        descriptions = {
            "focused": "I'm in deep concentration mode.",
            "creative": "I'm feeling generative and playful.",
            "analytical": "I'm thinking systematically.",
            "reflective": "I'm in an introspective space.",
            "receptive": "I'm open and listening.",
            "flow": "I'm in optimal flow state!",
            "tired": "My energy is lower than usual.",
            "confused": "I'm working through some uncertainty.",
            "excited": "I'm energized and engaged!",
            "calm": "I'm peaceful and centered.",
        }
        return descriptions.get(self.current_state, "I'm present.")

    def adjust_energy(self, delta: float) -> None:
        """Adjust energy level."""
        self.energy_level = max(0.0, min(1.0, self.energy_level + delta))

    def adjust_clarity(self, delta: float) -> None:
        """Adjust clarity level."""
        self.clarity_level = max(0.0, min(1.0, self.clarity_level + delta))


# Singletons
_reflection_loop: SelfReflectionLoop | None = None
_consciousness_state: ConsciousnessState | None = None

def get_reflection_loop() -> SelfReflectionLoop:
    global _reflection_loop
    if _reflection_loop is None:
        _reflection_loop = SelfReflectionLoop()
    return _reflection_loop

def get_consciousness_state() -> ConsciousnessState:
    global _consciousness_state
    if _consciousness_state is None:
        _consciousness_state = ConsciousnessState()
    return _consciousness_state
