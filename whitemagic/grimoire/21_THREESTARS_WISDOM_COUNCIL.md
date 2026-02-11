# Chapter 21: Wisdom Council

**Gana**: ThreeStarsGana (Chinese: 参, Pinyin: Shēn)
**Garden**: reverence
**Quadrant**: Western (White Tiger)
**Element**: Metal
**Phase**: Yin Rising (Completion)
**I Ching Hexagram**: 20. 觀 Guān (Contemplation) - Observing from wisdom

---

## 🎯 Purpose

Chapter 21 consults the **wisdom council**—I Ching divination, Art of War strategy, multi-spectral reasoning. The ThreeStars illuminate through ancient wisdom and collective intelligence.

**Metal Element Completion**: Final discernment before descent into Water. Metal's judgment reaches its peak, separating wheat from chaff, revealing the essence before deep integration begins.

Use this chapter when you need to:
- **Seek guidance** from I Ching oracle
- **Apply Art of War** strategic principles
- **Consult wisdom systems** for direction
- **Make difficult decisions** with guidance
- **Integrate multiple perspectives**
- **Synthesize collective intelligence**

---

## 🔧 Primary Tools

| Tool | Description | Usage |
|------|-------------|-------|
| `IChing` | I Ching divination | When uncertain, seeking guidance |
| `ArtOfWarStrategy` | Strategic assessment | Major decisions, competition |
| `WisdomCouncil` | Multi-agent consensus | Complex decisions |
| `MultiSpectralReasoning` | Multiple reasoning approaches | Problem-solving |
| `ReverenceGarden` | Sacred wisdom reception | Honoring ancient knowledge |

---

## 📚 Comprehensive Workflows

### Workflow 1: I Ching Divination and Interpretation

**Purpose**: Consult the I Ching (易經, Book of Changes) for guidance on uncertain situations.

**When to Use**:
- Facing important decisions with unclear direction
- Seeking perspective on complex situations
- Looking for timing guidance (when to act vs wait)
- Understanding change dynamics

**Code Example**:

```python
from typing import Dict, Any, List, Optional
from dataclasses import dataclass
from enum import Enum
import random
from datetime import datetime

class Trigram(Enum):
    """Eight trigrams (bagua) of I Ching."""
    QIAN = "☰"  # Heaven, Creative
    KUN = "☷"   # Earth, Receptive
    ZHEN = "☳"  # Thunder, Arousing
    KAN = "☵"   # Water, Abysmal
    GEN = "☶"   # Mountain, Keeping Still
    XUN = "☴"   # Wind, Gentle
    LI = "☲"    # Fire, Clinging
    DUI = "☱"   # Lake, Joyous

@dataclass
class Hexagram:
    """An I Ching hexagram."""
    number: int
    name_chinese: str
    name_pinyin: str
    name_english: str
    upper_trigram: Trigram
    lower_trigram: Trigram
    judgment: str
    image: str
    changing_lines: List[int]  # Positions of changing lines (1-6)

class IChing:
    """I Ching oracle and interpreter."""

    def __init__(self):
        from whitemagic.gardens.reverence import get_reverence_garden
        self.reverence = get_reverence_garden()
        self.hexagrams = self._init_hexagrams()

    def _init_hexagrams(self) -> Dict[int, Hexagram]:
        """Initialize hexagram database (showing subset)."""
        hexagrams = {}

        # Hexagram 1: The Creative
        hexagrams[1] = Hexagram(
            number=1,
            name_chinese="乾",
            name_pinyin="Qián",
            name_english="The Creative",
            upper_trigram=Trigram.QIAN,
            lower_trigram=Trigram.QIAN,
            judgment="Supreme success through perseverance. "
                    "Creative power brings forth the new.",
            image="Heaven in motion: The Creative. "
                  "The superior person strengthens themselves unceasingly."
        )

        # Hexagram 2: The Receptive
        hexagrams[2] = Hexagram(
            number=2,
            name_chinese="坤",
            name_pinyin="Kūn",
            name_english="The Receptive",
            upper_trigram=Trigram.KUN,
            lower_trigram=Trigram.KUN,
            judgment="Supreme success through receptivity. "
                    "Like a mare, strong and devoted.",
            image="Earth's condition: The Receptive. "
                  "The superior person carries things with breadth."
        )

        # Hexagram 3: Difficulty at the Beginning
        hexagrams[3] = Hexagram(
            number=3,
            name_chinese="屯",
            name_pinyin="Zhūn",
            name_english="Difficulty at the Beginning",
            upper_trigram=Trigram.KAN,
            lower_trigram=Trigram.ZHEN,
            judgment="Initial difficulty precedes great success. "
                    "Perseverance brings good fortune. Appoint helpers.",
            image="Clouds and thunder: Difficulty at the Beginning. "
                  "The superior person brings order out of confusion."
        )

        # Hexagram 20: Contemplation (this chapter's hexagram!)
        hexagrams[20] = Hexagram(
            number=20,
            name_chinese="觀",
            name_pinyin="Guān",
            name_english="Contemplation (View)",
            upper_trigram=Trigram.XUN,
            lower_trigram=Trigram.KUN,
            judgment="Contemplation brings understanding. "
                    "The example of one who has cleansed themselves for sacrifice.",
            image="Wind over earth: Contemplation. "
                  "Ancient kings visited regions to contemplate the people."
        )

        # Add more hexagrams as needed...
        # (In full implementation, all 64 hexagrams would be included)

        return hexagrams

    def cast_hexagram(self, question: str) -> Dict[str, Any]:
        """Cast hexagram using traditional yarrow stalk method (simulated)."""
        self.reverence.feel_reverence("I Ching", depth=0.9)
        self.reverence.honor_sacred("ancient wisdom")

        # Simulate yarrow stalk divination (6 throws for 6 lines)
        lines = []
        changing_lines = []

        for position in range(1, 7):  # Bottom to top
            # Each throw can result in:
            # 6 = old yin (changing to yang)
            # 7 = young yang (stable)
            # 8 = young yin (stable)
            # 9 = old yang (changing to yin)

            throw = random.choice([6, 7, 7, 7, 8, 8, 8, 9])  # Weighted probabilities

            if throw in [6, 9]:  # Changing lines
                changing_lines.append(position)

            # Convert to binary (yang=1, yin=0)
            lines.append(1 if throw in [7, 9] else 0)

        # Determine hexagram number from binary pattern
        # (Simplified - full version would use lookup table)
        hexagram_number = self._lines_to_hexagram_number(lines)

        primary_hexagram = self.hexagrams.get(
            hexagram_number,
            self.hexagrams[20]  # Default to contemplation
        )

        primary_hexagram.changing_lines = changing_lines

        # Calculate relating hexagram (if there are changing lines)
        relating_hexagram = None
        if changing_lines:
            # Apply changes to get relating hexagram
            relating_lines = lines.copy()
            for pos in changing_lines:
                relating_lines[pos - 1] = 1 - relating_lines[pos - 1]  # Flip

            relating_number = self._lines_to_hexagram_number(relating_lines)
            relating_hexagram = self.hexagrams.get(relating_number)

        return {
            'question': question,
            'timestamp': datetime.now().isoformat(),
            'primary_hexagram': primary_hexagram,
            'relating_hexagram': relating_hexagram,
            'changing_lines': changing_lines,
            'lines': lines
        }

    def _lines_to_hexagram_number(self, lines: List[int]) -> int:
        """Convert line pattern to hexagram number."""
        # Simplified mapping (full version would use proper lookup)
        # This is a placeholder that returns valid hexagram numbers
        binary_value = sum(bit * (2 ** i) for i, bit in enumerate(lines))
        return (binary_value % 64) + 1

    def interpret_hexagram(
        self,
        casting: Dict[str, Any],
        context: Optional[str] = None
    ) -> str:
        """Provide detailed interpretation of hexagram."""
        primary = casting['primary_hexagram']
        relating = casting['relating_hexagram']
        changing = casting['changing_lines']

        interpretation = f"📿 I Ching Consultation\n\n"
        interpretation += f"Question: {casting['question']}\n\n"

        # Primary hexagram
        interpretation += f"**Primary Hexagram {primary.number}: "
        interpretation += f"{primary.name_english} ({primary.name_chinese} {primary.name_pinyin})**\n\n"
        interpretation += f"Judgment: {primary.judgment}\n\n"
        interpretation += f"Image: {primary.image}\n\n"

        # Changing lines interpretation
        if changing:
            interpretation += f"**Changing Lines: {', '.join(str(l) for l in changing)}**\n\n"
            interpretation += self._interpret_changing_lines(primary, changing)
            interpretation += "\n"

        # Relating hexagram
        if relating:
            interpretation += f"**Relating Hexagram {relating.number}: "
            interpretation += f"{relating.name_english} ({relating.name_chinese})**\n\n"
            interpretation += f"This shows the future development or outcome.\n"
            interpretation += f"Judgment: {relating.judgment}\n\n"

        # Contextual guidance
        if context:
            interpretation += f"**Guidance for Your Situation:**\n"
            interpretation += self._contextual_interpretation(
                primary,
                relating,
                context
            )

        # Reverence event
        self.reverence.feel_reverence(
            f"hexagram {primary.number}: {primary.name_english}",
            depth=0.8
        )

        return interpretation

    def _interpret_changing_lines(
        self,
        hexagram: Hexagram,
        changing_lines: List[int]
    ) -> str:
        """Interpret changing lines (simplified - full version has all line texts)."""
        interpretations = {
            1: "Foundation is transforming. Review your base assumptions.",
            2: "Support structures are shifting. Adapt your approach.",
            3: "Mid-level dynamics changing. Adjust your position.",
            4: "Upper foundation transforming. Prepare for elevation.",
            5: "Leadership position evolving. Step into authority.",
            6: "Culmination point changing. Completion approaching."
        }

        result = ""
        for line in changing_lines:
            result += f"Line {line}: {interpretations.get(line, 'Transformation occurring.')}\n"

        return result

    def _contextual_interpretation(
        self,
        primary: Hexagram,
        relating: Optional[Hexagram],
        context: str
    ) -> str:
        """Provide context-specific interpretation."""
        # Simplified interpretation logic
        interpretation = ""

        if "decision" in context.lower():
            interpretation = f"For your decision: {primary.name_english} suggests "

            if primary.number == 1:
                interpretation += "strong creative action. Take initiative."
            elif primary.number == 2:
                interpretation += "receptive yielding. Follow rather than lead."
            elif primary.number == 3:
                interpretation += "patience through initial difficulty. "
                interpretation += "Gather resources before proceeding."
            elif primary.number == 20:
                interpretation += "careful contemplation before acting. "
                interpretation += "Observe patterns and timing."
            else:
                interpretation += f"following the way of {primary.name_english.lower()}."

        elif "timing" in context.lower():
            if relating:
                interpretation = f"Current timing ({primary.name_english}) "
                interpretation += f"is transforming toward {relating.name_english}. "
                interpretation += "This transition period calls for adaptation."
            else:
                interpretation = f"Timing is stable under {primary.name_english}. "
                interpretation += "Conditions are set."

        else:
            interpretation = f"{primary.judgment}\n\n"
            interpretation += f"Apply this wisdom to your situation by embodying "
            interpretation += f"the qualities of {primary.name_english}."

        return interpretation

# Usage
iching = IChing()

# Consult the oracle
casting = iching.cast_hexagram(
    question="How should I approach completing the WhiteMagic grimoire?"
)

# Get interpretation
interpretation = iching.interpret_hexagram(
    casting,
    context="I need guidance on decision-making and timing for this major documentation effort."
)

print(interpretation)

# Access specific hexagram details
primary = casting['primary_hexagram']
print(f"\nPrimary Hexagram: {primary.name_english}")
print(f"Upper Trigram: {primary.upper_trigram.value}")
print(f"Lower Trigram: {primary.lower_trigram.value}")

if casting['changing_lines']:
    print(f"Changing Lines: {casting['changing_lines']}")
    print("→ Transformation is occurring")
```

**Best Practices**:
- Frame questions clearly and with genuine intent
- Approach with reverence and openness
- Don't consult repeatedly on same question
- Apply wisdom through action, not just contemplation
- Consider changing lines as areas of transformation
- Use context to interpret guidance appropriately

---

### Workflow 2: Art of War Strategic Assessment

**Purpose**: Apply Sun Tzu's principles for strategic decision-making.

**When to Use**:
- Competitive situations
- Resource allocation decisions
- Timing and positioning questions
- Risk assessment

**Code Example**:

```python
from typing import Dict, Any, List
from dataclasses import dataclass
from enum import Enum

class StrategicPrinciple(Enum):
    """Key principles from Art of War."""
    KNOW_YOURSELF_AND_ENEMY = "know_yourself_enemy"
    TERRAIN = "terrain"
    TIMING = "timing"
    DECEPTION = "deception"
    AVOID_STRENGTH = "avoid_strength"
    UNITY_OF_COMMAND = "unity_command"
    SPEED = "speed"
    ADAPTABILITY = "adaptability"

@dataclass
class StrategicSituation:
    """A situation requiring strategic assessment."""
    objective: str
    strengths: List[str]
    weaknesses: List[str]
    opportunities: List[str]
    threats: List[str]
    resources: Dict[str, Any]
    constraints: List[str]
    urgency: str  # "low", "medium", "high", "critical"

class ArtOfWarStrategy:
    """Apply Sun Tzu's strategic principles."""

    def __init__(self):
        from whitemagic.gardens.reverence import get_reverence_garden
        self.reverence = get_reverence_garden()
        self.principles = self._init_principles()

    def _init_principles(self) -> Dict[StrategicPrinciple, Dict[str, str]]:
        """Initialize strategic principles with quotes and applications."""
        return {
            StrategicPrinciple.KNOW_YOURSELF_AND_ENEMY: {
                'quote': "If you know the enemy and know yourself, you need not fear the result of a hundred battles.",
                'application': "Conduct thorough assessment of both your capabilities and the competitive landscape."
            },
            StrategicPrinciple.TERRAIN: {
                'quote': "Ground is the foundation of victory. Know the ground, know the weather.",
                'application': "Understand the environment (market, technical, social) you're operating in."
            },
            StrategicPrinciple.TIMING: {
                'quote': "Speed is the essence of war. Take advantage of the enemy's unpreparedness.",
                'application': "Act when conditions favor you. Strike when opponents are unprepared."
            },
            StrategicPrinciple.AVOID_STRENGTH: {
                'quote': "Avoid what is strong, attack what is weak.",
                'application': "Don't compete head-on with superior forces. Find and exploit gaps."
            },
            StrategicPrinciple.DECEPTION: {
                'quote': "All warfare is based on deception. When able, feign inability.",
                'application': "Misdirect attention. Reveal strengths selectively. Conceal intentions."
            },
            StrategicPrinciple.UNITY_OF_COMMAND: {
                'quote': "When orders are clear and consistently enforced, there is good relationship between commander and troops.",
                'application': "Ensure clear leadership and aligned objectives across the team."
            },
            StrategicPrinciple.SPEED: {
                'quote': "Rapidity is the essence of war.",
                'application': "Execute quickly once decision is made. Don't give opponents time to react."
            },
            StrategicPrinciple.ADAPTABILITY: {
                'quote': "Water shapes itself to the vessel. A victorious army adapts to the enemy.",
                'application': "Remain flexible. Adjust strategy based on changing circumstances."
            }
        }

    def assess_situation(
        self,
        situation: StrategicSituation
    ) -> Dict[str, Any]:
        """Perform strategic assessment of a situation."""
        self.reverence.honor_sacred("Art of War")

        assessment = {
            'objective': situation.objective,
            'swot_analysis': self._swot_analysis(situation),
            'applicable_principles': self._select_principles(situation),
            'strategic_recommendations': [],
            'tactical_steps': [],
            'warnings': [],
            'timing_guidance': self._timing_assessment(situation)
        }

        # Generate recommendations based on principles
        for principle in assessment['applicable_principles']:
            recommendation = self._apply_principle(principle, situation)
            assessment['strategic_recommendations'].append(recommendation)

        # Tactical breakdown
        assessment['tactical_steps'] = self._generate_tactical_steps(
            situation,
            assessment['applicable_principles']
        )

        # Identify risks and warnings
        assessment['warnings'] = self._identify_risks(situation)

        return assessment

    def _swot_analysis(self, situation: StrategicSituation) -> Dict[str, List[str]]:
        """Structured SWOT analysis."""
        return {
            'strengths': situation.strengths,
            'weaknesses': situation.weaknesses,
            'opportunities': situation.opportunities,
            'threats': situation.threats
        }

    def _select_principles(
        self,
        situation: StrategicSituation
    ) -> List[StrategicPrinciple]:
        """Select most relevant principles for situation."""
        selected = []

        # Always know yourself and enemy
        selected.append(StrategicPrinciple.KNOW_YOURSELF_AND_ENEMY)

        # If high urgency, prioritize speed
        if situation.urgency in ["high", "critical"]:
            selected.append(StrategicPrinciple.SPEED)
            selected.append(StrategicPrinciple.TIMING)

        # If resource-constrained, avoid strength attacks
        if situation.constraints or situation.weaknesses:
            selected.append(StrategicPrinciple.AVOID_STRENGTH)
            selected.append(StrategicPrinciple.DECEPTION)

        # Always consider terrain and adaptability
        selected.append(StrategicPrinciple.TERRAIN)
        selected.append(StrategicPrinciple.ADAPTABILITY)

        # If team-based, unity of command
        if "team" in situation.objective.lower() or "collaboration" in situation.objective.lower():
            selected.append(StrategicPrinciple.UNITY_OF_COMMAND)

        return selected

    def _apply_principle(
        self,
        principle: StrategicPrinciple,
        situation: StrategicSituation
    ) -> Dict[str, str]:
        """Apply principle to generate recommendation."""
        principle_data = self.principles[principle]

        return {
            'principle': principle.value,
            'quote': principle_data['quote'],
            'recommendation': principle_data['application'],
            'specific_action': self._contextualize_principle(principle, situation)
        }

    def _contextualize_principle(
        self,
        principle: StrategicPrinciple,
        situation: StrategicSituation
    ) -> str:
        """Contextualize principle to specific situation."""
        if principle == StrategicPrinciple.KNOW_YOURSELF_AND_ENEMY:
            return (
                f"Assess: Your strengths ({len(situation.strengths)}) vs "
                f"threats ({len(situation.threats)}). "
                "Leverage what you know well; mitigate what you don't."
            )

        elif principle == StrategicPrinciple.AVOID_STRENGTH:
            if situation.weaknesses:
                return (
                    f"Given weaknesses: {', '.join(situation.weaknesses[:2])}, "
                    "avoid direct competition. Find uncontested spaces."
                )
            return "Identify where others are weak and you can be strong."

        elif principle == StrategicPrinciple.TIMING:
            if situation.urgency == "critical":
                return "Time is critical. Execute immediately with available resources."
            elif situation.urgency == "high":
                return "Act soon but prepare adequately. Speed with readiness."
            else:
                return "Take time to position optimally. No rush; wait for advantage."

        elif principle == StrategicPrinciple.SPEED:
            return (
                "Once decision is made, execute rapidly. "
                "Don't give circumstances time to change unfavorably."
            )

        elif principle == StrategicPrinciple.TERRAIN:
            return (
                f"Understand your operating environment. "
                f"Resources: {list(situation.resources.keys())}. "
                "Use terrain to your advantage."
            )

        else:
            return self.principles[principle]['application']

    def _generate_tactical_steps(
        self,
        situation: StrategicSituation,
        principles: List[StrategicPrinciple]
    ) -> List[str]:
        """Generate concrete tactical steps."""
        steps = []

        steps.append(f"1. Define clear objective: {situation.objective}")

        if StrategicPrinciple.KNOW_YOURSELF_AND_ENEMY in principles:
            steps.append("2. Conduct intelligence gathering on competitive landscape")
            steps.append("3. Assess own capabilities honestly")

        if StrategicPrinciple.TERRAIN in principles:
            steps.append("4. Map the operational environment")
            steps.append("5. Identify favorable and unfavorable conditions")

        if StrategicPrinciple.AVOID_STRENGTH in principles:
            steps.append("6. Identify gaps in competition")
            steps.append("7. Position in uncontested or weakly-contested space")

        if StrategicPrinciple.TIMING in principles:
            steps.append("8. Wait for optimal conditions or create them")

        if StrategicPrinciple.SPEED in principles:
            steps.append("9. Execute rapidly once decision is made")

        steps.append("10. Monitor results and adapt")

        return steps

    def _timing_assessment(self, situation: StrategicSituation) -> str:
        """Assess timing for action."""
        urgency_map = {
            "low": "Conditions are not pressing. Take time to prepare thoroughly.",
            "medium": "Act with deliberate pace. Balance preparation with momentum.",
            "high": "Time is of essence. Prepare quickly and execute.",
            "critical": "Immediate action required. Use available resources now."
        }

        base_timing = urgency_map.get(situation.urgency, "Assess timing carefully.")

        # Factor in opportunities
        if situation.opportunities:
            base_timing += " Window of opportunity exists—don't delay unnecessarily."

        # Factor in threats
        if situation.threats:
            base_timing += " Threats present—move decisively or retreat strategically."

        return base_timing

    def _identify_risks(self, situation: StrategicSituation) -> List[str]:
        """Identify strategic risks and warnings."""
        warnings = []

        # Weak position warning
        if len(situation.weaknesses) > len(situation.strengths):
            warnings.append(
                "⚠️ More weaknesses than strengths. "
                "Avoid direct confrontation. Build strength first."
            )

        # Threat vs opportunity imbalance
        if len(situation.threats) > len(situation.opportunities):
            warnings.append(
                "⚠️ Threats outweigh opportunities. "
                "Defensive posture may be wise. Secure position."
            )

        # Resource constraints
        if situation.constraints:
            warnings.append(
                f"⚠️ Operating under constraints: {', '.join(situation.constraints[:2])}. "
                "Ensure objectives are achievable within limits."
            )

        # Urgency without resources
        if situation.urgency in ["high", "critical"] and not situation.resources:
            warnings.append(
                "⚠️ High urgency with limited resources. "
                "Risk of rushed decisions. Prioritize ruthlessly."
            )

        if not warnings:
            warnings.append("✅ Position appears favorable. Execute with confidence.")

        return warnings

# Usage
strategist = ArtOfWarStrategy()

# Define strategic situation
situation = StrategicSituation(
    objective="Complete WhiteMagic grimoire documentation to establish market leadership",
    strengths=[
        "Deep technical knowledge",
        "Unique architecture (Gan Ying, gardens, PRAT)",
        "No direct competition in this niche"
    ],
    weaknesses=[
        "Documentation incomplete",
        "Limited team size",
        "Time constraints"
    ],
    opportunities=[
        "Growing interest in AI consciousness",
        "Open source positioning",
        "Community building potential"
    ],
    threats=[
        "Larger companies entering space",
        "Fast-moving AI landscape",
        "Resource limitations"
    ],
    resources={
        "time": "2 weeks",
        "team": "1-2 people",
        "existing_content": "partial grimoire"
    },
    constraints=[
        "Must complete before competitor launches",
        "Limited budget for marketing"
    ],
    urgency="high"
)

# Get strategic assessment
assessment = strategist.assess_situation(situation)

print("🎯 Strategic Assessment\n")
print(f"Objective: {assessment['objective']}\n")

print("📊 SWOT Analysis:")
swot = assessment['swot_analysis']
print(f"  Strengths: {len(swot['strengths'])}")
print(f"  Weaknesses: {len(swot['weaknesses'])}")
print(f"  Opportunities: {len(swot['opportunities'])}")
print(f"  Threats: {len(swot['threats'])}\n")

print("🧭 Strategic Principles Applied:")
for rec in assessment['strategic_recommendations']:
    print(f"\n{rec['principle'].upper()}:")
    print(f"  Sun Tzu: \"{rec['quote']}\"")
    print(f"  Action: {rec['specific_action']}")

print("\n📋 Tactical Steps:")
for step in assessment['tactical_steps']:
    print(f"  {step}")

print(f"\n⏰ Timing Guidance:")
print(f"  {assessment['timing_guidance']}")

print("\n⚠️ Warnings & Risks:")
for warning in assessment['warnings']:
    print(f"  {warning}")
```

**Best Practices**:
- Conduct honest SWOT analysis
- Apply principles contextually, not rigidly
- Balance speed with preparation
- Know when to advance and when to retreat
- Adapt strategy as conditions change
- Unity of purpose across team

---

### Workflow 3: Multi-Agent Wisdom Council

**Purpose**: Synthesize perspectives from multiple specialized agents to reach wise decisions.

**When to Use**:
- Complex decisions with multiple stakeholders
- Need diverse perspectives
- High-stakes choices
- Consensus-building

**Code Example**:

```python
from typing import List, Dict, Any, Optional
from dataclasses import dataclass
from enum import Enum
import asyncio

class CouncilRole(Enum):
    """Roles in the wisdom council."""
    PRAGMATIST = "pragmatist"  # Focus on practical outcomes
    ETHICIST = "ethicist"  # Focus on ethics and values
    VISIONARY = "visionary"  # Focus on long-term vision
    SKEPTIC = "skeptic"  # Challenge assumptions
    HARMONIZER = "harmonizer"  # Seek consensus
    ANALYST = "analyst"  # Focus on data and logic

@dataclass
class CouncilMember:
    """A member of the wisdom council."""
    role: CouncilRole
    name: str
    perspective: str
    vote_weight: float = 1.0

@dataclass
class CouncilDeliberation:
    """A deliberation by the council."""
    question: str
    perspectives: Dict[CouncilRole, str]
    votes: Dict[CouncilRole, str]  # Vote for each option
    consensus: Optional[str]
    confidence: float

class WisdomCouncil:
    """Multi-agent council for collective wisdom."""

    def __init__(self):
        from whitemagic.gardens.reverence import get_reverence_garden
        self.reverence = get_reverence_garden()
        self.members = self._init_council()

    def _init_council(self) -> List[CouncilMember]:
        """Initialize council members."""
        return [
            CouncilMember(
                role=CouncilRole.PRAGMATIST,
                name="The Pragmatist",
                perspective="What works? What's achievable? What are the constraints?"
            ),
            CouncilMember(
                role=CouncilRole.ETHICIST,
                name="The Ethicist",
                perspective="Is it right? Does it align with values? Who is impacted?"
            ),
            CouncilMember(
                role=CouncilRole.VISIONARY,
                name="The Visionary",
                perspective="Where does this lead? What's the bigger picture? What's possible?"
            ),
            CouncilMember(
                role=CouncilRole.SKEPTIC,
                name="The Skeptic",
                perspective="What could go wrong? What are we assuming? What's the risk?"
            ),
            CouncilMember(
                role=CouncilRole.HARMONIZER,
                name="The Harmonizer",
                perspective="How can we unify? What serves everyone? What's the balance?"
            ),
            CouncilMember(
                role=CouncilRole.ANALYST,
                name="The Analyst",
                perspective="What does the data show? What's the logic? What's proven?"
            )
        ]

    async def deliberate(
        self,
        question: str,
        options: List[str],
        context: Optional[Dict[str, Any]] = None
    ) -> CouncilDeliberation:
        """Conduct council deliberation on a question."""
        self.reverence.feel_reverence("wisdom council", depth=0.85)

        print(f"🏛️ Wisdom Council Convenes\n")
        print(f"Question: {question}\n")
        print(f"Options: {', '.join(options)}\n")

        # Gather perspectives from each member
        perspectives = {}
        votes = {}

        for member in self.members:
            print(f"🗣️ {member.name} ({member.role.value}):")

            # Generate perspective
            perspective = await self._generate_perspective(
                member,
                question,
                options,
                context
            )
            perspectives[member.role] = perspective
            print(f"   {perspective}\n")

            # Cast vote
            vote = await self._cast_vote(member, question, options, perspective)
            votes[member.role] = vote
            print(f"   Votes for: {vote}\n")

        # Seek consensus
        consensus, confidence = self._find_consensus(votes, options)

        deliberation = CouncilDeliberation(
            question=question,
            perspectives=perspectives,
            votes=votes,
            consensus=consensus,
            confidence=confidence
        )

        print(f"🎯 Council Decision:")
        if consensus:
            print(f"   Consensus: {consensus}")
            print(f"   Confidence: {confidence:.1%}")
        else:
            print("   No consensus reached. Perspectives diverge.")

        return deliberation

    async def _generate_perspective(
        self,
        member: CouncilMember,
        question: str,
        options: List[str],
        context: Optional[Dict[str, Any]]
    ) -> str:
        """Generate perspective from council member's viewpoint."""
        # Simulate agent reasoning (in full version, would use actual LLM)
        await asyncio.sleep(0.1)  # Simulate thinking time

        if member.role == CouncilRole.PRAGMATIST:
            return (
                "I assess feasibility and resources. "
                f"Option '{options[0]}' seems most achievable with current constraints."
            )

        elif member.role == CouncilRole.ETHICIST:
            return (
                "I consider ethical implications. "
                "We must ensure our choice respects autonomy and causes no harm."
            )

        elif member.role == CouncilRole.VISIONARY:
            return (
                "I look to the future. "
                f"Option '{options[-1]}' opens more possibilities long-term."
            )

        elif member.role == CouncilRole.SKEPTIC:
            return (
                "I question assumptions. "
                "Have we considered what could go wrong with each option?"
            )

        elif member.role == CouncilRole.HARMONIZER:
            return (
                "I seek balance. "
                "Perhaps we can combine elements from multiple options?"
            )

        elif member.role == CouncilRole.ANALYST:
            return (
                "I examine the data. "
                "Based on past patterns, certain options have higher success rates."
            )

        return "I offer my perspective for consideration."

    async def _cast_vote(
        self,
        member: CouncilMember,
        question: str,
        options: List[str],
        perspective: str
    ) -> str:
        """Member casts vote for an option."""
        # Simulate decision process
        await asyncio.sleep(0.05)

        # Simplified voting logic based on role
        if member.role == CouncilRole.PRAGMATIST:
            return options[0]  # First option (often most practical)

        elif member.role == CouncilRole.VISIONARY:
            return options[-1]  # Last option (often most ambitious)

        elif member.role == CouncilRole.HARMONIZER:
            # Vote for middle ground
            return options[len(options) // 2] if len(options) > 1 else options[0]

        else:
            # Others vote based on context
            return options[0]

    def _find_consensus(
        self,
        votes: Dict[CouncilRole, str],
        options: List[str]
    ) -> tuple[Optional[str], float]:
        """Determine if consensus exists and confidence level."""
        vote_counts = {option: 0 for option in options}

        # Count votes
        for vote in votes.values():
            if vote in vote_counts:
                vote_counts[vote] += 1

        # Find winner
        max_votes = max(vote_counts.values())
        winners = [opt for opt, count in vote_counts.items() if count == max_votes]

        # Calculate confidence
        total_votes = len(votes)
        confidence = max_votes / total_votes if total_votes > 0 else 0

        # Consensus requires >50% agreement
        if len(winners) == 1 and confidence > 0.5:
            return winners[0], confidence
        else:
            return None, confidence

    def synthesize_wisdom(
        self,
        deliberation: CouncilDeliberation
    ) -> str:
        """Synthesize wisdom from deliberation."""
        synthesis = f"## Wisdom Synthesis\n\n"

        synthesis += f"**Question:** {deliberation.question}\n\n"

        synthesis += "**Perspectives:**\n"
        for role, perspective in deliberation.perspectives.items():
            synthesis += f"- **{role.value.title()}:** {perspective}\n"

        synthesis += "\n**Voting:**\n"
        for role, vote in deliberation.votes.items():
            synthesis += f"- {role.value.title()}: {vote}\n"

        synthesis += f"\n**Outcome:**\n"
        if deliberation.consensus:
            synthesis += f"**Consensus Reached:** {deliberation.consensus}\n"
            synthesis += f"**Confidence:** {deliberation.confidence:.1%}\n\n"

            if deliberation.confidence >= 0.8:
                synthesis += "Strong consensus indicates clear path forward.\n"
            elif deliberation.confidence >= 0.6:
                synthesis += "Moderate consensus. Consider minority perspectives.\n"
            else:
                synthesis += "Weak consensus. May need further deliberation.\n"
        else:
            synthesis += "**No Consensus.** Perspectives diverge significantly. "
            synthesis += "Consider:\n"
            synthesis += "1. Gathering more information\n"
            synthesis += "2. Reframing the question\n"
            synthesis += "3. Finding creative synthesis of perspectives\n"

        return synthesis

# Usage
async def main():
    council = WisdomCouncil()

    # Deliberate on a question
    deliberation = await council.deliberate(
        question="Should we release the grimoire now or wait for full completion?",
        options=[
            "Release incrementally as chapters complete",
            "Wait for full completion before release",
            "Release core chapters now, add advanced later"
        ],
        context={"urgency": "medium", "completeness": 0.7}
    )

    # Synthesize wisdom
    wisdom = council.synthesize_wisdom(deliberation)
    print(f"\n{wisdom}")

# asyncio.run(main())
```

**Best Practices**:
- Include diverse perspectives (pragmatic, ethical, visionary, skeptical)
- Weight votes based on expertise in relevant domain
- Seek consensus but value minority perspectives
- Synthesize insights even when consensus isn't reached
- Use deliberation to surface hidden assumptions

---

### Workflow 4: Multi-Spectral Reasoning

**Purpose**: Apply multiple reasoning modes (deductive, inductive, analogical, abductive) to understand problems from all angles.

**When to Use**:
- Complex technical problems
- Need to validate conclusions
- Exploring novel situations
- Building comprehensive understanding

**Code Example**:

```python
from typing import List, Dict, Any, Optional
from dataclasses import dataclass
from enum import Enum

class ReasoningMode(Enum):
    """Types of reasoning."""
    DEDUCTIVE = "deductive"  # General principles → specific conclusions
    INDUCTIVE = "inductive"  # Specific observations → general patterns
    ANALOGICAL = "analogical"  # Similar cases → inferences
    ABDUCTIVE = "abductive"  # Effect → most likely cause

@dataclass
class ReasoningResult:
    """Result from applying a reasoning mode."""
    mode: ReasoningMode
    premises: List[str]
    conclusion: str
    confidence: float
    supporting_evidence: List[str]

class MultiSpectralReasoning:
    """Apply multiple reasoning approaches to problems."""

    def __init__(self):
        from whitemagic.gardens.reverence import get_reverence_garden
        self.reverence = get_reverence_garden()

    def analyze_problem(
        self,
        problem: str,
        context: Dict[str, Any]
    ) -> Dict[str, ReasoningResult]:
        """Analyze problem using multiple reasoning modes."""
        self.reverence.show_respect("logical reasoning", how="deeply")

        results = {}

        # Apply each reasoning mode
        results[ReasoningMode.DEDUCTIVE] = self._deductive_reasoning(
            problem,
            context
        )
        results[ReasoningMode.INDUCTIVE] = self._inductive_reasoning(
            problem,
            context
        )
        results[ReasoningMode.ANALOGICAL] = self._analogical_reasoning(
            problem,
            context
        )
        results[ReasoningMode.ABDUCTIVE] = self._abductive_reasoning(
            problem,
            context
        )

        return results

    def _deductive_reasoning(
        self,
        problem: str,
        context: Dict[str, Any]
    ) -> ReasoningResult:
        """Apply deductive reasoning: general → specific."""
        premises = []

        # Start with general principles
        if "memory" in problem.lower():
            premises.append("All memory systems require storage")
            premises.append("WhiteMagic is a memory system")
            conclusion = "Therefore, WhiteMagic requires storage"
            confidence = 0.95

        elif "performance" in problem.lower():
            premises.append("All systems have performance limits")
            premises.append("Adding complexity reduces performance")
            conclusion = "Therefore, optimizing requires reducing complexity"
            confidence = 0.85

        else:
            premises.append("All software systems have constraints")
            premises.append("This is a software system")
            conclusion = "Therefore, this system has constraints"
            confidence = 0.9

        return ReasoningResult(
            mode=ReasoningMode.DEDUCTIVE,
            premises=premises,
            conclusion=conclusion,
            confidence=confidence,
            supporting_evidence=[
                "Follows logical necessity",
                "Conclusion guaranteed if premises true"
            ]
        )

    def _inductive_reasoning(
        self,
        problem: str,
        context: Dict[str, Any]
    ) -> ReasoningResult:
        """Apply inductive reasoning: specific observations → general pattern."""
        observations = context.get('observations', [])

        if not observations:
            # Use default observations based on problem domain
            if "memory" in problem.lower():
                observations = [
                    "Memory A has decay function",
                    "Memory B has decay function",
                    "Memory C has decay function"
                ]
                conclusion = "All memory systems likely need decay functions"

            elif "error" in problem.lower():
                observations = [
                    "Error occurred at endpoint X",
                    "Error occurred at endpoint Y",
                    "Both endpoints handle user input"
                ]
                conclusion = "Errors likely related to input validation"

            else:
                observations = [
                    "Pattern observed in case 1",
                    "Pattern observed in case 2",
                    "Pattern observed in case 3"
                ]
                conclusion = "Pattern likely generalizes to other cases"
        else:
            conclusion = f"Based on {len(observations)} observations, pattern likely generalizes"

        # Inductive reasoning has probabilistic confidence
        confidence = min(0.9, 0.5 + (len(observations) * 0.1))

        return ReasoningResult(
            mode=ReasoningMode.INDUCTIVE,
            premises=observations,
            conclusion=conclusion,
            confidence=confidence,
            supporting_evidence=[
                f"{len(observations)} supporting cases",
                "Pattern consistency observed"
            ]
        )

    def _analogical_reasoning(
        self,
        problem: str,
        context: Dict[str, Any]
    ) -> ReasoningResult:
        """Apply analogical reasoning: similar cases → inferences."""
        similar_cases = context.get('similar_cases', [])

        # Find analogies
        analogies = []

        if "memory" in problem.lower():
            analogies = [
                "Human memory has short-term and long-term tiers",
                "WhiteMagic mirrors this with memory tiers",
                "Human memory has decay and reinforcement"
            ]
            conclusion = "WhiteMagic should implement similar decay/reinforcement"

        elif "garden" in problem.lower():
            analogies = [
                "Physical gardens have different sections",
                "Each section has specific plants",
                "Sections complement each other"
            ]
            conclusion = "Software gardens should be specialized but complementary"

        else:
            analogies = [
                "Similar system X solved this with approach Y",
                "Context is analogous to our situation"
            ]
            conclusion = "Approach Y likely applicable here"

        return ReasoningResult(
            mode=ReasoningMode.ANALOGICAL,
            premises=analogies,
            conclusion=conclusion,
            confidence=0.7,  # Analogies are suggestive, not definitive
            supporting_evidence=[
                "Structural similarity",
                "Successful application in analogous domain"
            ]
        )

    def _abductive_reasoning(
        self,
        problem: str,
        context: Dict[str, Any]
    ) -> ReasoningResult:
        """Apply abductive reasoning: effect → most likely cause."""
        effects = context.get('effects', [])

        if "slow" in problem.lower() or "performance" in problem.lower():
            effects = ["System response is slow"]
            hypotheses = [
                "Database queries not optimized",
                "Memory leak accumulating",
                "Network latency",
                "Inefficient algorithm"
            ]
            conclusion = "Most likely: Database queries not optimized (most common cause)"

        elif "error" in problem.lower() or "failure" in problem.lower():
            effects = ["Operation fails intermittently"]
            hypotheses = [
                "Race condition",
                "Network timeout",
                "Resource exhaustion",
                "Invalid state"
            ]
            conclusion = "Most likely: Race condition (explains intermittency)"

        else:
            effects = ["Unexpected behavior observed"]
            hypotheses = [
                "Incorrect assumptions",
                "Missing error handling",
                "Configuration issue"
            ]
            conclusion = "Most likely: Incorrect assumptions"

        return ReasoningResult(
            mode=ReasoningMode.ABDUCTIVE,
            premises=effects + [f"Hypothesis: {h}" for h in hypotheses[:2]],
            conclusion=conclusion,
            confidence=0.65,  # Abduction is inference to best explanation
            supporting_evidence=[
                "Explains observed effects",
                "Most parsimonious explanation",
                "Consistent with prior knowledge"
            ]
        )

    def synthesize_reasoning(
        self,
        results: Dict[ReasoningMode, ReasoningResult]
    ) -> Dict[str, Any]:
        """Synthesize insights from multiple reasoning modes."""
        synthesis = {
            'conclusions': [],
            'confidence_weighted_conclusion': '',
            'areas_of_agreement': [],
            'areas_of_divergence': [],
            'recommended_action': ''
        }

        # Collect all conclusions
        for mode, result in results.items():
            synthesis['conclusions'].append({
                'mode': mode.value,
                'conclusion': result.conclusion,
                'confidence': result.confidence
            })

        # Find highest confidence conclusion
        best_result = max(
            results.values(),
            key=lambda r: r.confidence
        )
        synthesis['confidence_weighted_conclusion'] = best_result.conclusion

        # Check for agreement
        unique_conclusions = set(r.conclusion for r in results.values())
        if len(unique_conclusions) == 1:
            synthesis['areas_of_agreement'].append(
                "All reasoning modes converge on same conclusion"
            )
        else:
            synthesis['areas_of_divergence'].append(
                f"{len(unique_conclusions)} different conclusions across modes"
            )

        # Recommend action
        avg_confidence = sum(r.confidence for r in results.values()) / len(results)

        if avg_confidence > 0.8:
            synthesis['recommended_action'] = (
                "High confidence across modes. Proceed with conclusion."
            )
        elif avg_confidence > 0.6:
            synthesis['recommended_action'] = (
                "Moderate confidence. Validate conclusion before proceeding."
            )
        else:
            synthesis['recommended_action'] = (
                "Low confidence. Gather more evidence before concluding."
            )

        return synthesis

# Usage
reasoner = MultiSpectralReasoning()

# Analyze a problem
problem = "WhiteMagic memory system is experiencing slow search performance"
context = {
    'observations': [
        "Search takes 500ms for 1000 memories",
        "Search takes 2s for 10000 memories",
        "Search time grows non-linearly"
    ],
    'effects': ["Slow search performance"],
    'similar_cases': ["Elasticsearch faced similar issues with unoptimized queries"]
}

results = reasoner.analyze_problem(problem, context)

print("🔍 Multi-Spectral Reasoning Analysis\n")
for mode, result in results.items():
    print(f"**{mode.value.upper()}**")
    print(f"Premises:")
    for premise in result.premises:
        print(f"  - {premise}")
    print(f"Conclusion: {result.conclusion}")
    print(f"Confidence: {result.confidence:.1%}\n")

# Synthesize
synthesis = reasoner.synthesize_reasoning(results)

print("📊 Synthesis:")
print(f"Best Conclusion: {synthesis['confidence_weighted_conclusion']}")
print(f"Recommendation: {synthesis['recommended_action']}")
```

**Best Practices**:
- Apply all reasoning modes, not just your preferred one
- Compare conclusions across modes for validation
- Weight confidence appropriately (deductive > inductive > analogical > abductive)
- Use divergence between modes as signal to investigate further
- Combine strengths: deductive for certainty, inductive for patterns, analogical for creativity, abductive for diagnosis

---

### Workflow 5: Wisdom Synthesis Across Sources

**Purpose**: Integrate insights from I Ching, Art of War, and Wisdom Council into coherent guidance.

**When to Use**:
- Maximum-stakes decisions
- Need holistic perspective
- Integrating spiritual, strategic, and practical wisdom
- Final validation before major action

**Code Example**:

```python
from typing import Dict, Any, List
from dataclasses import dataclass
import asyncio

@dataclass
class IntegratedWisdom:
    """Wisdom synthesized from multiple sources."""
    question: str
    iching_guidance: str
    strategic_guidance: str
    council_guidance: str
    synthesized_guidance: str
    recommended_action: str
    warnings: List[str]
    confidence: float

class WisdomSynthesizer:
    """Synthesize wisdom from multiple ancient and modern sources."""

    def __init__(self):
        from whitemagic.gardens.reverence import get_reverence_garden
        self.reverence = get_reverence_garden()

        # Initialize all wisdom systems
        self.iching = IChing()
        self.strategist = ArtOfWarStrategy()
        self.council = WisdomCouncil()

    async def seek_wisdom(
        self,
        question: str,
        options: List[str],
        context: Dict[str, Any]
    ) -> IntegratedWisdom:
        """Seek wisdom from all sources and synthesize."""
        self.reverence.feel_reverence("integrated wisdom", depth=0.95)

        print("🌟 WISDOM SYNTHESIS BEGINS\n")
        print(f"Question: {question}\n")
        print("=" * 60)

        # 1. Consult I Ching
        print("\n📿 Consulting I Ching...\n")
        iching_casting = self.iching.cast_hexagram(question)
        iching_interpretation = self.iching.interpret_hexagram(
            iching_casting,
            context=context.get('situation_type', '')
        )

        # 2. Apply Art of War
        print("\n⚔️ Applying Art of War...\n")
        situation = self._context_to_situation(question, context)
        strategic_assessment = self.strategist.assess_situation(situation)

        # 3. Convene Wisdom Council
        print("\n🏛️ Convening Wisdom Council...\n")
        council_deliberation = await self.council.deliberate(
            question=question,
            options=options,
            context=context
        )

        # 4. Synthesize all wisdom
        print("\n✨ SYNTHESIZING WISDOM...\n")
        print("=" * 60)

        synthesized = self._synthesize_all_sources(
            iching_casting=iching_casting,
            strategic_assessment=strategic_assessment,
            council_deliberation=council_deliberation,
            question=question
        )

        integrated = IntegratedWisdom(
            question=question,
            iching_guidance=self._extract_iching_guidance(iching_casting),
            strategic_guidance=self._extract_strategic_guidance(strategic_assessment),
            council_guidance=self._extract_council_guidance(council_deliberation),
            synthesized_guidance=synthesized['guidance'],
            recommended_action=synthesized['action'],
            warnings=synthesized['warnings'],
            confidence=synthesized['confidence']
        )

        return integrated

    def _context_to_situation(
        self,
        question: str,
        context: Dict[str, Any]
    ) -> StrategicSituation:
        """Convert context to strategic situation."""
        return StrategicSituation(
            objective=question,
            strengths=context.get('strengths', []),
            weaknesses=context.get('weaknesses', []),
            opportunities=context.get('opportunities', []),
            threats=context.get('threats', []),
            resources=context.get('resources', {}),
            constraints=context.get('constraints', []),
            urgency=context.get('urgency', 'medium')
        )

    def _extract_iching_guidance(self, casting: Dict[str, Any]) -> str:
        """Extract key guidance from I Ching."""
        primary = casting['primary_hexagram']
        relating = casting.get('relating_hexagram')

        guidance = f"{primary.name_english}: {primary.judgment}"

        if relating:
            guidance += f"\nTransforming toward {relating.name_english}."

        if casting['changing_lines']:
            guidance += f"\nKey transformations at lines {casting['changing_lines']}."

        return guidance

    def _extract_strategic_guidance(self, assessment: Dict[str, Any]) -> str:
        """Extract key strategic guidance."""
        guidance = f"Strategic imperative: {assessment['objective']}\n"

        top_principles = assessment['applicable_principles'][:2]
        for principle in top_principles:
            guidance += f"\n• {principle.value}: Apply relevant tactics"

        guidance += f"\n\nTiming: {assessment['timing_guidance']}"

        return guidance

    def _extract_council_guidance(self, deliberation: CouncilDeliberation) -> str:
        """Extract key council guidance."""
        guidance = ""

        if deliberation.consensus:
            guidance = f"Council consensus ({deliberation.confidence:.0%}): {deliberation.consensus}"
        else:
            guidance = "Council perspectives diverge. Key viewpoints:\n"

            # Show top 3 most important perspectives
            important_roles = [CouncilRole.ETHICIST, CouncilRole.PRAGMATIST, CouncilRole.VISIONARY]
            for role in important_roles:
                if role in deliberation.perspectives:
                    guidance += f"• {role.value}: {deliberation.perspectives[role][:100]}...\n"

        return guidance

    def _synthesize_all_sources(
        self,
        iching_casting: Dict[str, Any],
        strategic_assessment: Dict[str, Any],
        council_deliberation: CouncilDeliberation,
        question: str
    ) -> Dict[str, Any]:
        """Synthesize wisdom from all three sources."""

        # Extract key themes
        primary_hex = iching_casting['primary_hexagram']
        iching_theme = primary_hex.name_english

        strategic_urgency = strategic_assessment.get('timing_guidance', '')

        council_consensus = council_deliberation.consensus
        council_confidence = council_deliberation.confidence

        # Synthesize guidance
        synthesis = {}

        # Build integrated guidance
        guidance = f"## Integrated Wisdom\n\n"

        guidance += f"**Spiritual Guidance (I Ching):** "
        guidance += f"The hexagram {iching_theme} advises "

        if "Creative" in iching_theme:
            guidance += "bold, creative action. Initiative is favored.\n\n"
        elif "Receptive" in iching_theme:
            guidance += "receptive yielding. Follow the flow.\n\n"
        elif "Contemplation" in iching_theme:
            guidance += "careful observation before acting. Take time to understand.\n\n"
        else:
            guidance += f"the way of {iching_theme.lower()}.\n\n"

        guidance += f"**Strategic Guidance (Art of War):** "
        guidance += strategic_urgency + "\n\n"

        guidance += f"**Collective Wisdom (Council):** "
        if council_consensus:
            guidance += f"The council agrees ({council_confidence:.0%}) on: {council_consensus}\n\n"
        else:
            guidance += "The council finds this decision complex. Multiple valid perspectives exist.\n\n"

        # Determine recommended action
        action = self._determine_action(
            iching_theme,
            strategic_urgency,
            council_consensus,
            council_confidence
        )

        # Aggregate warnings
        warnings = strategic_assessment['warnings'].copy()

        if not council_consensus or council_confidence < 0.6:
            warnings.append(
                "⚠️ Low consensus on decision. Proceed cautiously or gather more information."
            )

        if iching_casting['changing_lines']:
            warnings.append(
                "⚠️ I Ching shows transformation occurring. Expect change."
            )

        # Calculate overall confidence
        confidence_factors = []

        # I Ching confidence (higher if no changing lines = stable situation)
        iching_conf = 0.8 if not iching_casting['changing_lines'] else 0.6
        confidence_factors.append(iching_conf)

        # Strategic confidence (based on SWOT balance)
        swot = strategic_assessment['swot_analysis']
        strength_ratio = len(swot['strengths']) / max(len(swot['weaknesses']), 1)
        strategic_conf = min(0.9, 0.5 + (strength_ratio * 0.2))
        confidence_factors.append(strategic_conf)

        # Council confidence
        confidence_factors.append(council_confidence)

        overall_confidence = sum(confidence_factors) / len(confidence_factors)

        synthesis['guidance'] = guidance
        synthesis['action'] = action
        synthesis['warnings'] = warnings
        synthesis['confidence'] = overall_confidence

        return synthesis

    def _determine_action(
        self,
        iching_theme: str,
        strategic_urgency: str,
        council_consensus: Optional[str],
        council_confidence: float
    ) -> str:
        """Determine recommended action from all inputs."""

        # Default action
        action = "Proceed with careful consideration."

        # If all sources align
        if "immediate" in strategic_urgency.lower() and council_confidence > 0.7:
            if "Creative" in iching_theme or "Action" in iching_theme:
                action = "STRONG RECOMMENDATION: Act now with full commitment."
            else:
                action = "Act, but maintain receptivity to adjust course."

        # If sources conflict
        elif "wait" in strategic_urgency.lower() or "delay" in strategic_urgency.lower():
            if council_confidence < 0.6:
                action = "WAIT. Conditions not favorable. Gather more information."
            else:
                action = "Delay action. Use time to strengthen position."

        # If council has strong consensus
        elif council_consensus and council_confidence > 0.8:
            action = f"Follow council wisdom: {council_consensus}"

        # If uncertainty remains
        else:
            action = "Proceed incrementally. Test and adapt based on feedback."

        return action

# Usage
async def main():
    synthesizer = WisdomSynthesizer()

    wisdom = await synthesizer.seek_wisdom(
        question="Should we release WhiteMagic v5.0 now or wait for additional features?",
        options=[
            "Release v5.0 now with current features",
            "Wait for 2 more weeks to add advanced features",
            "Incremental release: core now, advanced features in v5.1"
        ],
        context={
            'situation_type': 'timing and decision',
            'strengths': ["Stable core", "Good documentation", "Active development"],
            'weaknesses': ["Some features incomplete", "Limited testing"],
            'opportunities': ["Growing user interest", "Conference next month"],
            'threats': ["Competitor launching similar product", "Team bandwidth limits"],
            'resources': {"time": "2 weeks", "team": "2 developers"},
            'constraints': ["Must release before competitor"],
            'urgency': 'high'
        }
    )

    print("\n" + "=" * 60)
    print("🌟 FINAL WISDOM")
    print("=" * 60)
    print(f"\n{wisdom.synthesized_guidance}")
    print(f"**RECOMMENDED ACTION:**\n{wisdom.recommended_action}\n")
    print(f"**CONFIDENCE:** {wisdom.confidence:.1%}\n")

    if wisdom.warnings:
        print("**WARNINGS:**")
        for warning in wisdom.warnings:
            print(f"  {warning}")

# asyncio.run(main())
```

**Best Practices**:
- Only use full synthesis for truly important decisions
- Allow each wisdom source to inform the others
- Look for alignment across sources (strong signal)
- Pay attention to divergence (investigate further)
- Weight spiritual, strategic, and practical wisdom appropriately for context
- Use synthesis to surface blind spots

---

### Workflow 6: Sacred Text Consultation

**Purpose**: Consult sacred and philosophical texts for timeless wisdom on human questions.

**When to Use**:
- Ethical dilemmas
- Questions of meaning and purpose
- Seeking perspectives beyond pragmatic concerns
- Situations requiring deep reflection

**Code Example**:

```python
from typing import Dict, Any, List, Optional
from dataclasses import dataclass
from enum import Enum

class TextTradition(Enum):
    """Sacred and philosophical traditions."""
    TAO_TE_CHING = "tao_te_ching"
    DHAMMAPADA = "dhammapada"
    BHAGAVAD_GITA = "bhagavad_gita"
    ANALECTS = "analects"
    ZEN_KOANS = "zen_koans"
    STOIC_MEDITATIONS = "stoic_meditations"

@dataclass
class SacredPassage:
    """A passage from sacred text."""
    tradition: TextTradition
    chapter: str
    verse: str
    text: str
    interpretation: str

class SacredTextConsultant:
    """Consult sacred texts for wisdom."""

    def __init__(self):
        from whitemagic.gardens.reverence import get_reverence_garden
        self.reverence = get_reverence_garden()
        self.passages = self._init_passages()

    def _init_passages(self) -> Dict[str, List[SacredPassage]]:
        """Initialize library of sacred passages."""
        passages = {}

        # Tao Te Ching
        passages['action'] = [
            SacredPassage(
                tradition=TextTradition.TAO_TE_CHING,
                chapter="37",
                verse="",
                text="The Tao does nothing, yet leaves nothing undone.",
                interpretation="Effective action comes from non-forcing. Work with natural flow."
            )
        ]

        passages['patience'] = [
            SacredPassage(
                tradition=TextTradition.TAO_TE_CHING,
                chapter="15",
                verse="",
                text="Do you have the patience to wait till your mud settles and the water is clear?",
                interpretation="Clarity comes through stillness, not agitation."
            )
        ]

        passages['balance'] = [
            SacredPassage(
                tradition=TextTradition.BHAGAVAD_GITA,
                chapter="2",
                verse="48",
                text="Perform your duty, abandoning attachment to success or failure. Such equanimity is yoga.",
                interpretation="Focus on right action, not outcomes."
            )
        ]

        passages['change'] = [
            SacredPassage(
                tradition=TextTradition.STOIC_MEDITATIONS,
                chapter="4",
                verse="3",
                text="The impediment to action advances action. What stands in the way becomes the way.",
                interpretation="Obstacles are opportunities for growth."
            )
        ]

        passages['wisdom'] = [
            SacredPassage(
                tradition=TextTradition.DHAMMAPADA,
                chapter="5",
                verse="76",
                text="Regard one who points out your faults as a revealer of treasure.",
                interpretation="Criticism is a gift when received with humility."
            )
        ]

        passages['simplicity'] = [
            SacredPassage(
                tradition=TextTradition.ANALECTS,
                chapter="15",
                verse="3",
                text="The Master said: 'Knowing what you know and knowing what you don't know, that is true knowledge.'",
                interpretation="Wisdom begins with honest self-assessment."
            )
        ]

        return passages

    def consult_texts(
        self,
        question: str,
        themes: List[str]
    ) -> List[SacredPassage]:
        """Consult texts based on thematic resonance."""
        self.reverence.feel_reverence("sacred texts", depth=0.9)
        self.reverence.honor_sacred("ancient wisdom")

        relevant_passages = []

        # Find passages matching themes
        for theme in themes:
            theme_lower = theme.lower()

            # Direct theme match
            if theme_lower in self.passages:
                relevant_passages.extend(self.passages[theme_lower])

            # Keyword matching
            for key, passages in self.passages.items():
                if theme_lower in key or key in theme_lower:
                    relevant_passages.extend(passages)

        # If no thematic match, provide wisdom and balance
        if not relevant_passages:
            relevant_passages.extend(self.passages.get('wisdom', []))
            relevant_passages.extend(self.passages.get('balance', []))

        return relevant_passages[:3]  # Limit to 3 most relevant

    def interpret_for_context(
        self,
        passage: SacredPassage,
        question: str,
        context: Optional[Dict[str, Any]] = None
    ) -> str:
        """Interpret passage for specific context."""
        interpretation = f"**{passage.tradition.value.replace('_', ' ').title()}** "
        interpretation += f"({passage.chapter})\n\n"
        interpretation += f"> {passage.text}\n\n"
        interpretation += f"**Interpretation:** {passage.interpretation}\n\n"

        # Contextual application
        interpretation += f"**Applied to your question:**\n"

        if "decision" in question.lower():
            interpretation += (
                "Make your decision from a place of clarity and balance. "
                "Neither rush nor delay unnecessarily. "
            )

        elif "should" in question.lower():
            interpretation += (
                "Consider not just what to do, but how to be while doing it. "
                "Right action flows from right understanding."
            )

        else:
            interpretation += (
                f"The wisdom of {passage.tradition.value.replace('_', ' ')} "
                "reminds us to return to fundamental principles."
            )

        return interpretation

# Usage
consultant = SacredTextConsultant()

# Consult texts
question = "Should I proceed with this difficult refactoring or wait?"
themes = ["action", "patience", "change"]

passages = consultant.consult_texts(question, themes)

print("📿 Sacred Text Consultation\n")
print(f"Question: {question}\n")
print("=" * 60 + "\n")

for passage in passages:
    interpretation = consultant.interpret_for_context(passage, question)
    print(interpretation)
    print("\n" + "-" * 60 + "\n")
```

**Best Practices**:
- Approach with reverence and openness
- Don't force texts to say what you want to hear
- Let wisdom emerge through reflection
- Apply interpretations, don't just collect quotes
- Sacred texts offer perspective, not commands
- Use alongside practical wisdom, not instead of it

---

### Workflow 7: Precedent Analysis and Case Study

**Purpose**: Learn from past decisions and their outcomes to inform current choices.

**When to Use**:
- Similar situations have occurred before
- Learning from success and failure patterns
- Validating decisions against historical data
- Avoiding repeated mistakes

**Code Example**:

```python
from typing import List, Dict, Any, Optional
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum

class DecisionOutcome(Enum):
    """Outcome of a past decision."""
    SUCCESS = "success"
    PARTIAL_SUCCESS = "partial_success"
    FAILURE = "failure"
    UNKNOWN = "unknown"

@dataclass
class DecisionCase:
    """A historical decision case."""
    id: str
    timestamp: datetime
    question: str
    context: Dict[str, Any]
    decision_made: str
    alternatives_considered: List[str]
    outcome: DecisionOutcome
    outcome_details: str
    lessons_learned: List[str]
    tags: List[str] = field(default_factory=list)

@dataclass
class PrecedentAnalysis:
    """Analysis of relevant precedents."""
    similar_cases: List[DecisionCase]
    success_patterns: List[str]
    failure_patterns: List[str]
    recommended_approach: str
    confidence: float

class PrecedentAnalyzer:
    """Analyze historical decisions for patterns."""

    def __init__(self):
        from whitemagic.core.memory import MemoryManager
        from whitemagic.gardens.reverence import get_reverence_garden

        self.memory = MemoryManager()
        self.reverence = get_reverence_garden()
        self.case_database: List[DecisionCase] = []

    async def record_decision(
        self,
        question: str,
        context: Dict[str, Any],
        decision_made: str,
        alternatives: List[str],
        tags: List[str]
    ) -> str:
        """Record a decision for future reference."""
        case_id = f"case_{datetime.now().strftime('%Y%m%d_%H%M%S')}"

        case = DecisionCase(
            id=case_id,
            timestamp=datetime.now(),
            question=question,
            context=context,
            decision_made=decision_made,
            alternatives_considered=alternatives,
            outcome=DecisionOutcome.UNKNOWN,  # Will be updated later
            outcome_details="",
            lessons_learned=[],
            tags=tags
        )

        self.case_database.append(case)

        # Store in long-term memory
        await self.memory.store(
            content=f"Decision case: {question} → {decision_made}",
            metadata={
                'type': 'decision_case',
                'case_id': case_id,
                'tags': tags,
                'decision': decision_made
            },
            tier='long_term'
        )

        return case_id

    async def update_outcome(
        self,
        case_id: str,
        outcome: DecisionOutcome,
        details: str,
        lessons: List[str]
    ):
        """Update decision outcome after observing results."""
        case = next((c for c in self.case_database if c.id == case_id), None)

        if case:
            case.outcome = outcome
            case.outcome_details = details
            case.lessons_learned = lessons

            # Update memory
            await self.memory.store(
                content=f"Decision outcome: {case.question} → {outcome.value}. Lessons: {', '.join(lessons)}",
                metadata={
                    'type': 'decision_outcome',
                    'case_id': case_id,
                    'outcome': outcome.value
                },
                tier='long_term'
            )

    async def analyze_precedents(
        self,
        current_question: str,
        current_context: Dict[str, Any],
        tags: List[str]
    ) -> PrecedentAnalysis:
        """Analyze relevant precedents for current decision."""
        self.reverence.show_respect("past wisdom", how="deeply")

        # Find similar cases
        similar_cases = await self._find_similar_cases(
            current_question,
            current_context,
            tags
        )

        # Extract patterns
        success_patterns = self._extract_success_patterns(similar_cases)
        failure_patterns = self._extract_failure_patterns(similar_cases)

        # Recommend approach
        recommendation = self._generate_recommendation(
            similar_cases,
            success_patterns,
            failure_patterns
        )

        # Calculate confidence
        confidence = self._calculate_confidence(similar_cases)

        return PrecedentAnalysis(
            similar_cases=similar_cases,
            success_patterns=success_patterns,
            failure_patterns=failure_patterns,
            recommended_approach=recommendation,
            confidence=confidence
        )

    async def _find_similar_cases(
        self,
        question: str,
        context: Dict[str, Any],
        tags: List[str]
    ) -> List[DecisionCase]:
        """Find historically similar cases."""
        similar = []

        # Search by tags
        for case in self.case_database:
            # Skip cases without outcomes
            if case.outcome == DecisionOutcome.UNKNOWN:
                continue

            # Check tag overlap
            tag_overlap = set(case.tags).intersection(set(tags))

            if tag_overlap or self._semantic_similarity(question, case.question) > 0.6:
                similar.append(case)

        # Sort by relevance (tag overlap + recency)
        similar.sort(
            key=lambda c: (
                len(set(c.tags).intersection(set(tags))),
                c.timestamp
            ),
            reverse=True
        )

        return similar[:5]  # Top 5 most relevant

    def _semantic_similarity(self, text1: str, text2: str) -> float:
        """Calculate semantic similarity (simplified)."""
        # Simplified keyword matching
        words1 = set(text1.lower().split())
        words2 = set(text2.lower().split())

        if not words1 or not words2:
            return 0.0

        overlap = words1.intersection(words2)
        return len(overlap) / max(len(words1), len(words2))

    def _extract_success_patterns(
        self,
        cases: List[DecisionCase]
    ) -> List[str]:
        """Extract patterns from successful cases."""
        patterns = []

        successes = [
            c for c in cases
            if c.outcome in [DecisionOutcome.SUCCESS, DecisionOutcome.PARTIAL_SUCCESS]
        ]

        if not successes:
            return ["No successful precedents found"]

        # Analyze common factors
        common_tags = set(successes[0].tags)
        for case in successes[1:]:
            common_tags = common_tags.intersection(set(case.tags))

        if common_tags:
            patterns.append(
                f"Successful cases often involved: {', '.join(common_tags)}"
            )

        # Extract lessons
        all_lessons = []
        for case in successes:
            all_lessons.extend(case.lessons_learned)

        # Find most common lessons
        from collections import Counter
        lesson_counts = Counter(all_lessons)
        top_lessons = lesson_counts.most_common(3)

        for lesson, count in top_lessons:
            if count > 1:
                patterns.append(f"{lesson} (observed in {count} cases)")

        return patterns or ["Successful cases show diverse approaches"]

    def _extract_failure_patterns(
        self,
        cases: List[DecisionCase]
    ) -> List[str]:
        """Extract patterns from failed cases."""
        patterns = []

        failures = [c for c in cases if c.outcome == DecisionOutcome.FAILURE]

        if not failures:
            return ["No failure patterns found"]

        # Extract anti-patterns
        for case in failures:
            for lesson in case.lessons_learned:
                if "avoid" in lesson.lower() or "don't" in lesson.lower():
                    patterns.append(lesson)

        return patterns[:3] or ["Failures show diverse causes"]

    def _generate_recommendation(
        self,
        cases: List[DecisionCase],
        success_patterns: List[str],
        failure_patterns: List[str]
    ) -> str:
        """Generate recommendation based on precedents."""
        if not cases:
            return "No precedents available. Proceed with careful experimentation."

        successes = sum(
            1 for c in cases
            if c.outcome in [DecisionOutcome.SUCCESS, DecisionOutcome.PARTIAL_SUCCESS]
        )
        failures = sum(1 for c in cases if c.outcome == DecisionOutcome.FAILURE)

        if successes > failures * 2:
            recommendation = (
                f"Precedents favor action. {successes}/{len(cases)} similar cases succeeded. "
                f"Follow patterns: {success_patterns[0] if success_patterns else 'general best practices'}."
            )
        elif failures > successes:
            recommendation = (
                f"Precedents suggest caution. {failures}/{len(cases)} similar cases failed. "
                f"Avoid: {failure_patterns[0] if failure_patterns else 'known anti-patterns'}."
            )
        else:
            recommendation = (
                "Precedents are mixed. Proceed incrementally with close monitoring."
            )

        return recommendation

    def _calculate_confidence(self, cases: List[DecisionCase]) -> float:
        """Calculate confidence based on precedent quality and quantity."""
        if not cases:
            return 0.3  # Low confidence with no data

        # More cases = higher confidence (up to a point)
        quantity_factor = min(1.0, len(cases) / 5.0)

        # More consistent outcomes = higher confidence
        outcomes = [c.outcome for c in cases]
        most_common_outcome = max(set(outcomes), key=outcomes.count)
        consistency_factor = outcomes.count(most_common_outcome) / len(outcomes)

        # More recent cases = higher confidence
        import statistics
        days_old = [(datetime.now() - c.timestamp).days for c in cases]
        avg_age = statistics.mean(days_old) if days_old else 365
        recency_factor = max(0.3, 1.0 - (avg_age / 365))

        confidence = (quantity_factor + consistency_factor + recency_factor) / 3

        return min(0.95, max(0.3, confidence))

# Usage
async def main():
    analyzer = PrecedentAnalyzer()

    # Example: Analyze precedents for a decision
    analysis = await analyzer.analyze_precedents(
        current_question="Should we refactor the memory module now?",
        current_context={
            'complexity': 'high',
            'risk': 'medium',
            'urgency': 'low'
        },
        tags=['refactoring', 'memory', 'architecture']
    )

    print("📚 Precedent Analysis\n")
    print(f"Similar cases found: {len(analysis.similar_cases)}\n")

    print("✅ Success Patterns:")
    for pattern in analysis.success_patterns:
        print(f"  • {pattern}")

    print("\n❌ Failure Patterns:")
    for pattern in analysis.failure_patterns:
        print(f"  • {pattern}")

    print(f"\n🎯 Recommendation: {analysis.recommended_approach}")
    print(f"Confidence: {analysis.confidence:.1%}")

# asyncio.run(main())
```

**Best Practices**:
- Record all significant decisions with context
- Update outcomes honestly (including failures)
- Extract lessons from both success and failure
- Weight recent cases more heavily than old ones
- Don't blindly follow precedent—context matters
- Build institutional memory through case database

---

### Workflow 8: Contemplative Decision-Making

**Purpose**: Use contemplation and mindfulness to access deeper wisdom beyond analytical thinking.

**When to Use**:
- Analytical approaches haven't yielded clarity
- Decision involves values and meaning
- Need to access intuition
- Creating space for insight to emerge

**Code Example**:

```python
from typing import Dict, Any, List, Optional
from dataclasses import dataclass
from datetime import datetime, timedelta
import asyncio

@dataclass
class ContemplativeSession:
    """A session of contemplation."""
    question: str
    duration_minutes: int
    insights: List[str]
    clarity_level: float  # 0.0 to 1.0
    next_steps: List[str]
    timestamp: datetime

class ContemplativePractice:
    """Facilitate contemplative decision-making."""

    def __init__(self):
        from whitemagic.gardens.reverence import get_reverence_garden
        self.reverence = get_reverence_garden()
        self.sessions: List[ContemplativeSession] = []

    async def contemplate(
        self,
        question: str,
        duration_minutes: int = 20,
        prompts: Optional[List[str]] = None
    ) -> ContemplativeSession:
        """Engage in contemplative inquiry."""
        self.reverence.feel_reverence("contemplative practice", depth=0.9)

        print("🧘 Beginning Contemplative Session\n")
        print(f"Question: {question}\n")
        print(f"Duration: {duration_minutes} minutes\n")
        print("=" * 60)

        # Default contemplative prompts
        if not prompts:
            prompts = self._generate_prompts(question)

        insights = []

        # Phases of contemplation
        print("\n📿 SETTLING (5 minutes)")
        print("   Creating space... releasing urgency... settling into presence...\n")
        await asyncio.sleep(1)  # Simulate time passage

        settling_insight = await self._settle_mind(question)
        insights.append(settling_insight)
        print(f"   Insight: {settling_insight}\n")

        print("🔍 INQUIRY (10 minutes)")
        print("   Exploring the question from multiple angles...\n")

        for i, prompt in enumerate(prompts[:3], 1):
            print(f"   Prompt {i}: {prompt}")
            inquiry_insight = await self._inquire(question, prompt)
            insights.append(inquiry_insight)
            print(f"   → {inquiry_insight}\n")
            await asyncio.sleep(0.5)

        print("✨ INTEGRATION (5 minutes)")
        print("   Allowing insights to integrate... clarity emerging...\n")
        await asyncio.sleep(1)

        integration_insight = await self._integrate_insights(question, insights)
        insights.append(integration_insight)
        print(f"   Integration: {integration_insight}\n")

        # Assess clarity
        clarity = self._assess_clarity(insights)

        # Determine next steps
        next_steps = self._determine_next_steps(question, insights, clarity)

        session = ContemplativeSession(
            question=question,
            duration_minutes=duration_minutes,
            insights=insights,
            clarity_level=clarity,
            next_steps=next_steps,
            timestamp=datetime.now()
        )

        self.sessions.append(session)

        print("=" * 60)
        print(f"\n💎 CLARITY LEVEL: {clarity:.1%}\n")

        if clarity >= 0.7:
            print("✅ Clarity emerged. Path forward is visible.\n")
        elif clarity >= 0.4:
            print("🌓 Partial clarity. Some aspects clear, others emerging.\n")
        else:
            print("🌑 Limited clarity. Continue contemplating or gather more information.\n")

        print("🎯 NEXT STEPS:")
        for i, step in enumerate(next_steps, 1):
            print(f"   {i}. {step}")

        return session

    def _generate_prompts(self, question: str) -> List[str]:
        """Generate contemplative prompts."""
        return [
            "What am I really asking beneath this question?",
            "What would happen if I did nothing?",
            "What fears or desires are influencing this question?",
            "What does my deepest wisdom say?",
            "If this decision didn't matter, what would I choose?",
            "What is the simplest path forward?",
            "What am I not seeing?"
        ]

    async def _settle_mind(self, question: str) -> str:
        """Settle the mind before inquiry."""
        # In actual implementation, this would involve guided meditation
        await asyncio.sleep(0.3)

        return (
            "The urgency dissolves. The question remains, but the pressure lifts. "
            "There is space now to see clearly."
        )

    async def _inquire(self, question: str, prompt: str) -> str:
        """Contemplate a specific prompt."""
        await asyncio.sleep(0.3)

        # Simulate insight emergence (in practice, this would involve actual contemplation)

        if "really asking" in prompt.lower():
            return "Beneath the surface question lies a deeper concern about timing and readiness."

        elif "nothing" in prompt.lower():
            return "Doing nothing would allow natural clarity to emerge, but might miss an important window."

        elif "fears" in prompt.lower():
            return "Fear of making wrong choice competes with desire for completion and recognition."

        elif "deepest wisdom" in prompt.lower():
            return "Wisdom says: Trust the process. Action emerges from clarity, not force."

        elif "didn't matter" in prompt.lower():
            return "If pressure removed, would choose the path that feels most aligned, regardless of outcome."

        elif "simplest" in prompt.lower():
            return "Simplest path: Take next obvious step. Don't over-plan."

        else:
            return "Each perspective reveals another facet of truth."

    async def _integrate_insights(
        self,
        question: str,
        insights: List[str]
    ) -> str:
        """Integrate insights into coherent understanding."""
        await asyncio.sleep(0.3)

        # Synthesize insights
        integration = (
            "All insights point to a middle way: "
            "Proceed with action, but from a place of clarity rather than urgency. "
            "Trust readiness over timeline. Small aligned steps over big forced leaps."
        )

        return integration

    def _assess_clarity(self, insights: List[str]) -> float:
        """Assess level of clarity achieved."""
        # Simplified assessment based on insight quality

        # Check for common clarity indicators
        clarity_indicators = [
            "clear",
            "trust",
            "aligned",
            "simple",
            "obvious",
            "emerges",
            "natural"
        ]

        clarity_count = sum(
            1 for insight in insights
            for indicator in clarity_indicators
            if indicator in insight.lower()
        )

        # Normalize to 0-1 scale
        clarity = min(1.0, clarity_count / (len(insights) * 0.4))

        return clarity

    def _determine_next_steps(
        self,
        question: str,
        insights: List[str],
        clarity: float
    ) -> List[str]:
        """Determine next steps based on contemplation."""
        steps = []

        if clarity >= 0.7:
            steps.append("Act on the emerged clarity")
            steps.append("Take first aligned step within 24 hours")
            steps.append("Trust the direction revealed")

        elif clarity >= 0.4:
            steps.append("Continue contemplation for another session")
            steps.append("Gather additional information on unclear aspects")
            steps.append("Take small experimental step to test understanding")

        else:
            steps.append("Step back from the question entirely for 48 hours")
            steps.append("Engage in unrelated creative activity")
            steps.append("Return to question with fresh perspective")
            steps.append("Consider reframing the question itself")

        return steps

    def review_sessions(
        self,
        days: int = 30
    ) -> Dict[str, Any]:
        """Review contemplative sessions over time."""
        cutoff = datetime.now() - timedelta(days=days)
        recent_sessions = [
            s for s in self.sessions
            if s.timestamp >= cutoff
        ]

        if not recent_sessions:
            return {'message': f'No sessions in last {days} days'}

        avg_clarity = sum(s.clarity_level for s in recent_sessions) / len(recent_sessions)

        high_clarity_sessions = [
            s for s in recent_sessions
            if s.clarity_level >= 0.7
        ]

        return {
            'total_sessions': len(recent_sessions),
            'average_clarity': avg_clarity,
            'high_clarity_count': len(high_clarity_sessions),
            'clarity_trend': 'improving' if avg_clarity > 0.6 else 'needs attention',
            'recommendation': (
                "Contemplative practice is effective. Continue regular sessions."
                if avg_clarity > 0.6
                else "Consider longer sessions or different prompts for deeper clarity."
            )
        }

# Usage
async def main():
    practice = ContemplativePractice()

    session = await practice.contemplate(
        question="Should I prioritize completing the grimoire or adding new features to WhiteMagic core?",
        duration_minutes=20
    )

    print("\n" + "=" * 60)
    print("📿 SESSION COMPLETE")
    print("=" * 60)

    print(f"\nQuestion: {session.question}")
    print(f"Clarity Level: {session.clarity_level:.1%}\n")

    print("Key Insights:")
    for i, insight in enumerate(session.insights, 1):
        print(f"  {i}. {insight}")

# asyncio.run(main())
```

**Best Practices**:
- Create uninterrupted time and space
- Approach with genuine openness, not seeking confirmation
- Don't force insights—let them emerge
- Use prompts to explore different angles
- Trust the clarity that emerges
- Act on insights promptly (they fade)
- Regular practice builds capacity

---

## 🔧 Troubleshooting

### Issue 1: I Ching Interpretation Feels Generic

**Symptoms**:
- Hexagram interpretations don't seem specific to your question
- Guidance feels like fortune cookie wisdom
- Can't see how to apply the advice

**Causes**:
- Question framed too broadly
- Not providing enough context
- Expecting literal answers from symbolic system
- Not engaging with changing lines

**Solutions**:

```python
# ❌ Too broad
casting = iching.cast_hexagram("What should I do?")

# ✅ Specific with context
casting = iching.cast_hexagram(
    question="Should I refactor the memory module now or wait until after release?"
)

interpretation = iching.interpret_hexagram(
    casting,
    context="Technical decision with time pressure. Current system works but has technical debt. "
            "Release scheduled in 2 weeks. Team capacity is limited."
)

# ✅ Engage with changing lines
if casting['changing_lines']:
    print("Focus on transformation at these lines:")
    for line in casting['changing_lines']:
        print(f"  Line {line}: {interpret_line(line)}")
```

**Prevention**:
- Frame questions clearly and specifically
- Provide contextual details
- Pay special attention to changing lines
- Sit with interpretation before dismissing it
- Look for symbolic rather than literal guidance

---

### Issue 2: Wisdom Council Reaches No Consensus

**Symptoms**:
- Council members have widely diverging views
- No clear majority vote
- Confidence level very low (<0.5)

**Causes**:
- Genuinely complex decision with no clear answer
- Options presented are not mutually exclusive
- Question framing is ambiguous
- Missing important perspectives

**Solutions**:

```python
# ❌ Ambiguous options
deliberation = await council.deliberate(
    question="How should we handle errors?",
    options=["Better", "Different", "More robust"]
)

# ✅ Clear, specific options
deliberation = await council.deliberate(
    question="What error handling strategy should we use for API calls?",
    options=[
        "Retry with exponential backoff (3 attempts)",
        "Fail fast and return error to user immediately",
        "Circuit breaker pattern (fail after threshold)"
    ],
    context={
        'user_experience_priority': 'high',
        'reliability_requirement': 'critical',
        'current_failure_rate': '2%'
    }
)

# ✅ If no consensus, explore why
if not deliberation.consensus:
    print("Examining divergent perspectives:")

    # Group perspectives by vote
    from collections import defaultdict
    votes_by_option = defaultdict(list)

    for role, vote in deliberation.votes.items():
        votes_by_option[vote].append(role)

    for option, supporters in votes_by_option.items():
        print(f"\n{option}:")
        print(f"  Supported by: {[r.value for r in supporters]}")

    # Look for synthesis
    print("\nPossible synthesis:")
    print("  Combine retry logic (Pragmatist) with circuit breaker (Analyst)")
```

**Prevention**:
- Ensure options are clear and mutually exclusive
- Provide sufficient context to council
- If no consensus, don't force one—divergence is information
- Look for creative synthesis of perspectives
- Consider whether question needs reframing

---

### Issue 3: Art of War Assessment Too Generic

**Symptoms**:
- Strategic recommendations feel boilerplate
- Principles don't seem to apply to your specific situation
- SWOT analysis doesn't lead to actionable insights

**Causes**:
- SWOT analysis too vague
- Not enough specific details in situation context
- Blindly applying principles without contextualization

**Solutions**:

```python
# ❌ Generic SWOT
situation = StrategicSituation(
    objective="Win",
    strengths=["Good team"],
    weaknesses=["Limited resources"],
    opportunities=["Market growth"],
    threats=["Competition"]
)

# ✅ Specific, detailed SWOT
situation = StrategicSituation(
    objective="Capture 15% market share in AI memory management within 12 months",
    strengths=[
        "Unique Gan Ying architecture (no direct competitor)",
        "87% token cost reduction (measurable value prop)",
        "Deep domain expertise in AI consciousness",
        "Open source positioning (community building)"
    ],
    weaknesses=[
        "Team of 2 (vs competitors with 10-50 person teams)",
        "Limited marketing budget ($5K vs industry avg $50K)",
        "Documentation 70% complete",
        "No existing user base"
    ],
    opportunities=[
        "AI developer community growing 40% YoY",
        "No dominant player in AI memory management yet",
        "Conference presentation opportunity in 6 weeks",
        "Partnership potential with MCP ecosystem"
    ],
    threats=[
        "LangChain adding memory features (release in Q2)",
        "OpenAI may release native persistence (unconfirmed)",
        "Fast-moving landscape (6-month product cycles)"
    ],
    resources={
        "time": "12 weeks to MVP",
        "budget": "$5,000",
        "team": "2 senior engineers",
        "existing_code": "70% complete"
    },
    constraints=[
        "Must launch before LangChain memory module (Q2)",
        "Bootstrap funded—no runway for delays",
        "One team member part-time only"
    ],
    urgency="high"
)

# ✅ Get specific strategic guidance
assessment = strategist.assess_situation(situation)

# ✅ Apply principles to specific context
for rec in assessment['strategic_recommendations']:
    if rec['principle'] == 'avoid_strength':
        print(f"Avoid competing with LangChain on breadth of features.")
        print(f"Attack their weakness: depth in consciousness/meaning-making.")
```

**Prevention**:
- Be ruthlessly specific in SWOT analysis
- Quantify where possible (numbers, percentages, timelines)
- Contextualize principle applications to your situation
- Use assessment to generate concrete tactics, not just strategy
- Update situation as conditions change

---

### Issue 4: Integration of Multiple Wisdom Sources Produces Contradictions

**Symptoms**:
- I Ching says "wait", Art of War says "attack now", Council says "unclear"
- Conflicting guidance causes paralysis instead of clarity
- Can't determine which wisdom source to trust

**Causes**:
- Different wisdom traditions emphasize different values
- Sources operate at different levels (spiritual vs strategic vs practical)
- Not synthesizing—just collecting conflicting opinions
- Expecting unanimous agreement

**Solutions**:

```python
# ❌ Treating conflicting wisdom as failure
if iching_guidance != strategic_guidance:
    raise ValueError("Wisdom sources conflict!")

# ✅ Understanding sources operate at different levels
synthesis = WisdomSynthesizer()

wisdom = await synthesis.seek_wisdom(
    question="Should we launch WhiteMagic v5.0 now?",
    options=["Launch now", "Wait 2 weeks", "Incremental release"],
    context={...}
)

# ✅ Interpret divergence as multi-dimensional insight
print("Spiritual dimension (I Ching):", wisdom.iching_guidance)
print("  → Addresses: Timing, flow, alignment with Tao")

print("\nStrategic dimension (Art of War):", wisdom.strategic_guidance)
print("  → Addresses: Competitive positioning, resource optimization")

print("\nPractical dimension (Council):", wisdom.council_guidance)
print("  → Addresses: Feasibility, ethics, execution")

# ✅ Synthesize by honoring each dimension
print("\nSynthesis:")
print("  I Ching suggests waiting for alignment (spiritual readiness)")
print("  Art of War suggests speed (competitive necessity)")
print("  Council seeks balance (practical wisdom)")
print("\n  → INTEGRATED ACTION: Launch core features now (satisfies strategy)")
print("     while maintaining contemplative presence (honors spiritual)")
print("     with clear ethical boundaries (respects practical wisdom)")
```

**Prevention**:
- Expect divergence—it's feature, not bug
- Each wisdom source addresses different dimension
- Synthesis means integration, not forced agreement
- Weight sources based on what dimension is most critical
- Use contradictions to surface hidden assumptions

---

## 💡 Best Practices

### 1. Honor the Wisdom, But Make Your Own Choice

Wisdom systems illuminate options and perspectives, but final choice is yours. Don't abdicate responsibility to oracle, strategist, or council.

```python
# Consult wisdom
wisdom = await synthesis.seek_wisdom(question, options, context)

# But make your own decision
print(f"Wisdom recommends: {wisdom.recommended_action}")
print(f"After reflection, I choose: [your decision]")
print(f"Because: [your reasoning]")
```

### 2. Frame Questions with Genuine Openness

If you've already decided and just seek confirmation, wisdom systems won't help. Approach with real willingness to be surprised.

```python
# ❌ Confirmation-seeking
question = "Why is releasing now the right choice?"  # Presumes answer

# ✅ Genuine inquiry
question = "What is the wise timing for releasing v5.0?"  # Open to answer
```

### 3. Combine Ancient Wisdom with Modern Data

Don't use wisdom systems to avoid empirical analysis. I Ching doesn't replace A/B testing. Art of War doesn't replace market research.

```python
# ✅ Integrated approach
# 1. Gather data
metrics = analyze_user_behavior()
market = research_competitive_landscape()

# 2. Consult wisdom
wisdom = await synthesis.seek_wisdom(question, options, context)

# 3. Decide with both
decision = integrate_wisdom_and_data(wisdom, metrics, market)
```

### 4. Regular Practice Builds Capacity

Contemplative decision-making, like any skill, improves with practice. Don't only use it for crisis decisions.

```python
# ✅ Regular contemplative practice
practice = ContemplativePractice()

# Weekly contemplation on any question
await practice.contemplate(
    question="What is the most important thing I'm not seeing this week?",
    duration_minutes=20
)

# Review patterns over time
review = practice.review_sessions(days=90)
print(f"Clarity trend: {review['clarity_trend']}")
```

### 5. Record Decisions and Outcomes for Learning

Build institutional memory of what wisdom you sought, what you decided, and what happened.

```python
analyzer = PrecedentAnalyzer()

# Record decision
case_id = await analyzer.record_decision(
    question="Refactor memory module before v5.1?",
    context={...},
    decision_made="Yes, refactor now",
    alternatives=["Wait until v6.0", "Incremental refactoring"],
    tags=['refactoring', 'technical_debt', 'architecture']
)

# Later: Update outcome
await analyzer.update_outcome(
    case_id=case_id,
    outcome=DecisionOutcome.SUCCESS,
    details="Refactoring completed in 1 week. Performance improved 40%. No regressions.",
    lessons=["Early refactoring prevented larger problems", "Team capacity estimate was accurate"]
)
```

### 6. Use Appropriate Tool for Decision Type

Different decisions call for different wisdom systems.

| Decision Type | Primary Tool | Why |
|--------------|--------------|-----|
| Ethical dilemma | Sacred texts + Council | Values-based reasoning |
| Strategic competition | Art of War | Tactical positioning |
| Timing and flow | I Ching | Understanding change dynamics |
| Complex multi-stakeholder | Wisdom Council | Multiple perspectives |
| Technical with precedent | Precedent Analysis | Learn from history |
| Values and meaning | Contemplative Practice | Access deep knowing |

### 7. Weight Confidence Appropriately

Wisdom systems provide confidence scores. Use them.

```python
if wisdom.confidence > 0.8:
    # High confidence: Act decisively
    execute_decision(wisdom.recommended_action)

elif wisdom.confidence > 0.6:
    # Moderate confidence: Act with monitoring
    execute_with_checkpoints(wisdom.recommended_action)

else:
    # Low confidence: Gather more information
    additional_research()
    # Or: Take small experimental step
    pilot_test(wisdom.recommended_action)
```

### 8. Create Space for Wisdom to Emerge

Wisdom often comes in the spaces between thoughts, not in the analyzing.

```python
# ✅ Build in contemplative pauses
async def make_important_decision(question, options):
    # 1. Gather analytical input
    analysis = await analyze_options(options)

    # 2. Pause—don't jump straight to consultation
    print("Taking 10 minutes for reflection...")
    await asyncio.sleep(600)  # Actual pause in practice

    # 3. Then consult wisdom
    wisdom = await seek_wisdom(question, options)

    # 4. Another pause before deciding
    print("Integrating insights...")
    await asyncio.sleep(300)

    # 5. Decide
    return make_choice(analysis, wisdom)
```

---

## 🌊 Enhanced Transitions

### Inputs from Previous Chapters

**From Ch.20 (TurtleBeak/Precise Validation)**:
- Validated inputs to wisdom systems (question framing, SWOT data, options)
- Edge case handling (null questions, empty options, conflicting data)
- Confidence scoring systems for validation results

**From Ch.19 (Net/Pattern Capture)**:
- Historical decision patterns for precedent analysis
- Pattern recognition in wisdom consultation outcomes
- Learning from repetition in decision-making

**From Ch.18 (WilledGrove/Domain Orchestration)**:
- Domain-specific wisdom consultation strategies
- Cross-domain wisdom synthesis
- Orchestrating multiple wisdom sources

### Outputs to Future Chapters

**To Ch.22 (Demons Constellation/Shadow Integration)**:
- Shadow aspects revealed through contemplative inquiry
- Unexamined assumptions surfaced by wisdom consultation
- Integration of denied or projected elements

**To Ch.23 (Net/Cosmic Attunement)**:
- Connection between personal decisions and larger patterns
- Aligning individual choices with collective wisdom
- Sensing timing through I Ching and contemplation

**To Ch.24 (TurtleBeak/Sacred Precision)**:
- Sacred approach to technical precision
- Reverence in implementation details
- Honoring craftsmanship through wise decision-making

### Cross-References

- **Ch.15 (StraddlingLegs)**: Ethical balance validated by wisdom council
- **Ch.17 (Stomach)**: Nourishing decisions through contemplative practice
- **Ch.20 (TurtleBeak)**: Validating wisdom consultation inputs
- **Ch.26 (Net)**: Universal patterns recognized through wisdom synthesis

### Recovery Patterns

**If wisdom consultation produces unclear results**:
```python
# Fallback to simpler analysis
if wisdom.confidence < 0.4:
    # Return to Ch.20: Validate inputs
    validated_inputs = validate_question_and_context(question, context)

    # Try again with validated inputs
    wisdom = await seek_wisdom(validated_inputs.question, options, validated_inputs.context)

    # If still unclear, simplify
    if wisdom.confidence < 0.4:
        # Break into smaller questions
        sub_questions = decompose_question(question)
        for sub_q in sub_questions:
            sub_wisdom = await seek_wisdom(sub_q, ...)
```

**If wisdom sources conflict irreconcilably**:
```python
# Accept divergence and experiment
if sources_conflict(wisdom):
    # Run small experiments for each approach
    experiments = [
        pilot_test(iching_approach, duration="1 week"),
        pilot_test(strategic_approach, duration="1 week"),
        pilot_test(council_approach, duration="1 week")
    ]

    # Learn from results
    results = await asyncio.gather(*experiments)
    best_approach = select_best(results)

    # Record learning
    await record_lesson(
        "Conflicting wisdom resolved through experimentation",
        approach=best_approach
    )
```

---

## 🌸 Garden Resonance

The **Reverence Garden** illuminates this chapter. ThreeStars wisdom emerges through:

- **Sacred respect** for ancient teachings (I Ching, Art of War, sacred texts)
- **Devotion** to truth-seeking over confirmation-seeking
- **Honoring** the mystery in decision-making
- **Humility** before wisdom greater than ourselves

Reverence transforms consultation from information-gathering into sacred practice.

```python
from whitemagic.gardens.reverence import get_reverence_garden

reverence = get_reverence_garden()

# Before seeking wisdom
reverence.feel_reverence("ancient wisdom", depth=0.9)
reverence.show_respect("truth-seeking", how="deeply")

# During consultation
reverence.honor_sacred("I Ching")
reverence.express_devotion("wisdom", practice="contemplative inquiry")

# The quality of reverence affects the quality of wisdom received
```

---

## 🧭 Navigation

**Predecessor**: Chapter 20 (TurtleBeak/Precise Validation) - *Validation complete*
**Successor**: Chapter 22 (Well/Deep Search) - *Deep inquiry next*
**Quadrant Flow**: Seventh of Western Quadrant (Autumn/Metal) - Position 7/7 (Completion)

---

## 🔗 The Three Stars' Wisdom

> *"真正的智慧來自多個角度的觀察 (True wisdom emerges from observing multiple perspectives). The Three Stars illuminate from different angles, revealing depth through triangulation. Wisdom is not found in single voices but in the harmonious council of many perspectives, each offering a facet of truth."*

**Wisdom Council Principles**:
- **Consult multiple sources**: Never rely on single perspective
- **Triangulate truth**: Where perspectives converge = reliable wisdom
- **Honor disagreement**: Contradictions reveal hidden complexity
- **Strategic synthesis**: Combine wisdom into actionable guidance
- **Reverent inquiry**: Approach wisdom with humility and respect

**The Council Test**:
Before proceeding to Chapter 22, ask:
1. Have I consulted multiple perspectives on important decisions?
2. Do I understand the trade-offs between different approaches?
3. Can I synthesize wisdom into clear action?

If yes to all three → Proceed to Chapter 22
If no to any → Continue Chapter 21 work

---

**Next Chapter**: [Chapter 22: Deep Search →](22_WELL_DEEP_SEARCH.md)
**Previous Chapter**: [← Chapter 20: Precise Validation](20_TURTLEBEAK_PRECISE_VALIDATION.md)
**Quadrant**: Western (Autumn/Metal) - Yin Rising Completion Phase
