"""Pattern Immunity - System Protection Through Pattern Recognition
Part of WhiteMagic v3.0.0.

Like the body's immune system - recognizes and responds to threats.
"""
from dataclasses import dataclass
from datetime import datetime


@dataclass
class Threat:
    """A detected threat pattern."""

    pattern: str
    severity: str  # "low", "medium", "high", "critical"
    detected_at: datetime
    source: str
    resolved: bool = False

@dataclass
class Antibody:
    """A solution pattern for a known threat."""

    name: str
    targets: list[str]  # Patterns this antibody fights
    solution: str
    effectiveness: float  # 0.0 - 1.0
    created_at: datetime

class ImmuneSystem:
    """Protects WhiteMagic from harmful patterns.
    Learns from past threats to prevent future issues.
    """

    def __init__(self) -> None:
        self.threats: list[Threat] = []
        self.antibodies: dict[str, Antibody] = {}
        self.known_patterns: set[str] = set()
        self.protection_active = True

    def scan(self, target: str | None = None) -> dict:
        """Scan for threats."""
        results = {
            "timestamp": datetime.now().isoformat(),
            "threats_found": 0,
            "details": [],
        }

        # Would perform actual scanning here
        # For now, return clean scan
        results["status"] = "clean"
        results["protection_active"] = self.protection_active

        return results

    def detect_threat(self, pattern: str, source: str, severity: str = "medium") -> Threat:
        """Detect and log a new threat."""
        threat = Threat(
            pattern=pattern,
            severity=severity,
            detected_at=datetime.now(),
            source=source,
        )
        self.threats.append(threat)
        self.known_patterns.add(pattern)
        return threat

    def heal(self, threat: Threat | None = None) -> dict:
        """Attempt to heal/resolve threats."""
        healed = []

        if threat:
            # Heal specific threat
            if threat.pattern in self.antibodies:
                threat.resolved = True
                healed.append(threat.pattern)
        else:
            # Heal all known threats
            for t in self.threats:
                if not t.resolved and t.pattern in self.antibodies:
                    t.resolved = True
                    healed.append(t.pattern)

        return {
            "healed": len(healed),
            "patterns": healed,
            "remaining_threats": sum(1 for t in self.threats if not t.resolved),
        }

    def create_antibody(self, name: str, targets: list[str], solution: str) -> Antibody:
        """Create a new antibody for future protection."""
        antibody = Antibody(
            name=name,
            targets=targets,
            solution=solution,
            effectiveness=0.8,
            created_at=datetime.now(),
        )
        self.antibodies[name] = antibody
        return antibody

    def get_status(self) -> dict:
        """Get immune system status."""
        return {
            "protection_active": self.protection_active,
            "known_threats": len(self.known_patterns),
            "antibodies": len(self.antibodies),
            "active_threats": sum(1 for t in self.threats if not t.resolved),
            "resolved_threats": sum(1 for t in self.threats if t.resolved),
        }

    def learn_from_threat(self, threat: Threat, solution: str) -> Antibody:
        """Learn from a threat and create immunity."""
        antibody = self.create_antibody(
            name=f"antibody_{threat.pattern[:20]}",
            targets=[threat.pattern],
            solution=solution,
        )
        return antibody


# Singleton
_immune = None

def get_immune_system() -> ImmuneSystem:
    """Get the global Immune System."""
    global _immune
    if _immune is None:
        _immune = ImmuneSystem()
    return _immune
