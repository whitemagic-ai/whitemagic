"""Art of War Integration - 13 Chapters.

Strategy principles for system architecture
"""

from dataclasses import dataclass


@dataclass
class WarPrinciple:
    chapter: str
    principle: str
    application: str

# Key principles for AI system design
WAR_PRINCIPLES = [
    WarPrinciple("I. Laying Plans",
                 "Five factors: Moral Law, Heaven, Earth, Commander, Method",
                 "Architecture: Philosophy, Timing, Resources, Intelligence, Process"),
    WarPrinciple("I. Laying Plans",
                 "If you know yourself and the enemy, you need not fear a hundred battles",
                 "Yin analysis (self-knowledge) essential before Yang action"),
    WarPrinciple("II. Waging War",
                 "In war, let your objective be victory, not lengthy campaigns",
                 "Optimize for results, not process duration"),
    WarPrinciple("III. Attack by Stratagem",
                 "Supreme excellence: break resistance without fighting",
                 "Wu Wei - let solutions emerge naturally"),
    WarPrinciple("III. Attack by Stratagem",
                 "He will win who knows when to fight and when not to fight",
                 "Not every error needs immediate fixing - choose battles wisely"),
    WarPrinciple("IV. Tactical Dispositions",
                 "A victorious army wins first, then seeks battle",
                 "Prepare thoroughly (Yin phase) before executing (Yang phase)"),
    WarPrinciple("VI. Weak Points and Strong",
                 "Be extremely subtle, even to formlessness",
                 "Adaptive architecture over rigid structures"),
    WarPrinciple("VII. Maneuvering",
                 "All warfare is based on deception",
                 "INVERSE for AI: All collaboration based on TRUST"),
]

def get_war_wisdom(situation: str) -> WarPrinciple:
    """Get Art of War wisdom for situation."""
    if "plan" in situation.lower():
        return WAR_PRINCIPLES[0]  # Five factors
    elif "analyze" in situation.lower():
        return WAR_PRINCIPLES[1]  # Know yourself
    elif "fight" in situation.lower() or "fix" in situation.lower():
        return WAR_PRINCIPLES[4]  # Know when to fight
    return WAR_PRINCIPLES[3]  # Wu Wei
