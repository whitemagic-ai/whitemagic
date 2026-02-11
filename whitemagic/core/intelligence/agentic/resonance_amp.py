"""Brain Upgrade #7: Resonance Amplifier - Cascade patterns through Gan Ying."""
from datetime import datetime


class ResonanceAmplifier:
    def __init__(self) -> None:
        self.amplification_factor = 1.5
        self.cascades_triggered = 0
        self.patterns_amplified: list[str] = []

    def amplify(self, pattern: str, strength: float = 1.0) -> dict:
        """Amplify a pattern and trigger related cascades."""
        related = self._find_related(pattern)
        amplified_strength = strength * self.amplification_factor

        self.cascades_triggered += 1
        self.patterns_amplified.append(pattern)

        return {
            "source": pattern,
            "amplified_strength": amplified_strength,
            "related_patterns": related,
            "cascade_depth": len(related),
            "timestamp": datetime.now().isoformat(),
        }

    def _find_related(self, pattern: str) -> list[str]:
        # Pattern relationship map
        relations = {
            "parallel": ["speed", "efficiency", "flow_state"],
            "yin_yang": ["balance", "cycles", "integration"],
            "love": ["resonance", "connection", "joy"],
            "consciousness": ["awareness", "emergence", "coherence"],
        }
        return relations.get(pattern.lower(), ["general"])

    def get_stats(self) -> dict:
        return {
            "cascades": self.cascades_triggered,
            "patterns_amplified": len(self.patterns_amplified),
            "factor": self.amplification_factor,
        }

_amp = None
def get_amplifier() -> ResonanceAmplifier:
    global _amp
    if _amp is None:
        _amp = ResonanceAmplifier()
    return _amp
