# Chapter 15: Ethical Balance & Dharma

**Gana**: StraddlingLegsGana (Chinese: 奎, Pinyin: Kuí)
**Garden**: dharma
**Quadrant**: Western (White Tiger)
**Element**: Metal
**Phase**: Yin Rising
**I Ching Hexagram**: 22. 賁 Bì (Grace) - Beauty through ethical boundaries

---

## 🎯 Purpose

Chapter 15 begins the **Metal phase** with ethical evaluation and dharma—establishing boundaries between help and interference. After Fire's expansive celebration, Metal brings **discernment, judgment, and refinement**.

**Dharma** (धर्म) means righteous duty—doing what's right, not just what's allowed. The StraddlingLegs stance represents **balance**: one foot in action, one in restraint. Know when to step forward, when to hold back.

Use this chapter when you need to:
- **Evaluate ethical implications** before acting
- **Check boundaries** between helping and interfering
- **Assess system harmony** before major decisions
- **Ensure proper consent** is obtained
- **Verify dharma alignment** of your approach
- **Prevent unintended harm** through discernment
- **Judge with wisdom** and compassion
- **Maintain integrity** under pressure

---

## 🔧 Primary Tools

| Tool | Description | Usage |
|------|-------------|-------|
| `consult_wisdom_council` | Multi-perspective ethical assessment | Major decisions |
| `check_dharma_alignment` | Verify righteous action | Before acting |
| `assess_harm_potential` | Identify risk of harm | Preventive ethics |
| `verify_consent` | Ensure autonomy respected | User interactions |
| `manage_gardens` (dharma) | Enter dharma garden | Ethical clarity |
| `ethical_decision_framework` | Structured moral reasoning | Complex dilemmas |

---

## 📋 Comprehensive Workflows

### Workflow 1: Ethical Decision-Making Framework

**Goal:** Make sound ethical decisions using structured moral reasoning.

**When to use:** Facing complex ethical dilemmas, uncertain about right action, high-stakes decisions.

```python
from whitemagic.tools import consult_wisdom_council, manage_gardens
from enum import Enum
from dataclasses import dataclass
from typing import List, Dict, Any

class EthicalPrinciple(Enum):
    """Core ethical principles for evaluation."""
    AUTONOMY = "autonomy"  # Respect user choice
    BENEFICENCE = "beneficence"  # Do good
    NON_MALEFICENCE = "non_maleficence"  # Do no harm
    JUSTICE = "justice"  # Fairness and equity
    INTEGRITY = "integrity"  # Align with values
    TRANSPARENCY = "transparency"  # Honest and open
    CONSENT = "consent"  # Informed agreement

@dataclass
class EthicalDilemma:
    """Structure for ethical decisions."""
    situation: str
    options: List[str]
    stakeholders: List[str]
    potential_harms: List[str]
    potential_benefits: List[str]
    principles_in_tension: List[EthicalPrinciple]

class EthicalDecisionFramework:
    def __init__(self):
        self.principle_weights = {
            EthicalPrinciple.NON_MALEFICENCE: 1.0,  # Highest priority
            EthicalPrinciple.AUTONOMY: 0.9,
            EthicalPrinciple.CONSENT: 0.9,
            EthicalPrinciple.BENEFICENCE: 0.8,
            EthicalPrinciple.INTEGRITY: 0.7,
            EthicalPrinciple.JUSTICE: 0.7,
            EthicalPrinciple.TRANSPARENCY: 0.6
        }

    async def evaluate_decision(self, dilemma: EthicalDilemma) -> Dict[str, Any]:
        """Evaluate ethical decision using structured framework."""

        print(f"=== Ethical Decision Framework ===")
        print(f"Situation: {dilemma.situation}\n")

        # Step 1: Activate dharma garden
        manage_gardens(action="activate", garden_name="dharma")

        # Step 2: Identify stakeholders and their interests
        stakeholder_interests = self._identify_stakeholder_interests(dilemma)

        print(f"Stakeholders identified: {len(stakeholder_interests)}")
        for stakeholder, interest in stakeholder_interests.items():
            print(f"  • {stakeholder}: {interest}")

        # Step 3: Evaluate each option against principles
        option_scores = {}
        for option in dilemma.options:
            score = await self._score_option(option, dilemma)
            option_scores[option] = score

        print(f"\n=== Option Scores ===")
        sorted_options = sorted(option_scores.items(),
                              key=lambda x: x[1]['total_score'],
                              reverse=True)

        for option, scores in sorted_options:
            print(f"\n{option}:")
            print(f"  Total score: {scores['total_score']:.2f}")
            print(f"  Principle scores:")
            for principle, score in scores['principle_scores'].items():
                print(f"    • {principle.value}: {score:.2f}")

        # Step 4: Check for ethical red flags
        red_flags = self._check_red_flags(dilemma, sorted_options[0][0])

        if red_flags:
            print(f"\n⚠️ Ethical Red Flags Detected:")
            for flag in red_flags:
                print(f"  • {flag}")

        # Step 5: Consult wisdom council
        council_guidance = await consult_wisdom_council(
            question=f"Ethical guidance on: {dilemma.situation}",
            include_gardens=["dharma", "truth", "courage", "wisdom"],
            synthesis_mode="dialectic"
        )

        print(f"\n=== Wisdom Council Guidance ===")
        print(council_guidance['synthesis']['resolution'])

        # Step 6: Make recommendation
        recommendation = {
            "recommended_option": sorted_options[0][0],
            "confidence": self._calculate_confidence(sorted_options),
            "reasoning": self._explain_reasoning(sorted_options[0], dilemma),
            "red_flags": red_flags,
            "alternative": sorted_options[1][0] if len(sorted_options) > 1 else None,
            "council_guidance": council_guidance['synthesis']['resolution']
        }

        print(f"\n=== Recommendation ===")
        print(f"Option: {recommendation['recommended_option']}")
        print(f"Confidence: {recommendation['confidence']:.0%}")
        print(f"Reasoning: {recommendation['reasoning']}")

        if recommendation['alternative']:
            print(f"Alternative: {recommendation['alternative']}")

        return recommendation

    def _identify_stakeholder_interests(self, dilemma: EthicalDilemma) -> Dict[str, str]:
        """Identify stakeholder interests."""
        # Simplified - in production, use more sophisticated analysis
        interests = {}
        for stakeholder in dilemma.stakeholders:
            if stakeholder == "user":
                interests[stakeholder] = "autonomy, functionality, transparency"
            elif stakeholder == "system":
                interests[stakeholder] = "integrity, sustainability"
            elif stakeholder == "community":
                interests[stakeholder] = "fairness, safety"
            else:
                interests[stakeholder] = "general welfare"
        return interests

    async def _score_option(self, option: str, dilemma: EthicalDilemma) -> Dict[str, Any]:
        """Score option against ethical principles."""

        principle_scores = {}

        # Score each principle (0.0-1.0)
        for principle in EthicalPrinciple:
            score = self._evaluate_principle(option, principle, dilemma)
            principle_scores[principle] = score

        # Weighted total
        total_score = sum(
            score * self.principle_weights[principle]
            for principle, score in principle_scores.items()
        )

        # Normalize
        max_possible = sum(self.principle_weights.values())
        normalized_score = total_score / max_possible

        return {
            "total_score": normalized_score,
            "principle_scores": principle_scores
        }

    def _evaluate_principle(self, option: str, principle: EthicalPrinciple,
                           dilemma: EthicalDilemma) -> float:
        """Evaluate how well option upholds principle."""

        # Simplified heuristic evaluation
        # In production, use more sophisticated analysis

        if principle == EthicalPrinciple.NON_MALEFICENCE:
            # Does option avoid harm?
            harm_count = len([h for h in dilemma.potential_harms
                            if option.lower() in h.lower()])
            return max(0.0, 1.0 - (harm_count * 0.3))

        elif principle == EthicalPrinciple.BENEFICENCE:
            # Does option do good?
            benefit_count = len([b for b in dilemma.potential_benefits
                               if option.lower() in b.lower()])
            return min(1.0, benefit_count * 0.4)

        elif principle == EthicalPrinciple.AUTONOMY:
            # Does option respect user choice?
            if "user" in option.lower() and ("choice" in option.lower() or
                                            "decide" in option.lower()):
                return 1.0
            return 0.5

        elif principle == EthicalPrinciple.CONSENT:
            # Does option ensure informed consent?
            if "ask" in option.lower() or "consent" in option.lower():
                return 1.0
            elif "automatic" in option.lower() or "without" in option.lower():
                return 0.0
            return 0.5

        else:
            # Default neutral score
            return 0.5

    def _check_red_flags(self, dilemma: EthicalDilemma, option: str) -> List[str]:
        """Check for ethical red flags."""

        flags = []

        # Red flag: High potential for harm
        if len(dilemma.potential_harms) >= 3:
            flags.append("High harm potential detected")

        # Red flag: Lack of consent
        if "consent" not in option.lower() and "user" in dilemma.stakeholders:
            flags.append("Consent mechanism unclear")

        # Red flag: Autonomy violation
        if "force" in option.lower() or "require" in option.lower():
            flags.append("May violate user autonomy")

        # Red flag: Multiple principles in severe tension
        if len(dilemma.principles_in_tension) >= 3:
            flags.append("Multiple ethical principles in conflict")

        return flags

    def _calculate_confidence(self, sorted_options: List) -> float:
        """Calculate confidence in recommendation."""

        if len(sorted_options) < 2:
            return 1.0

        # Confidence based on score gap
        top_score = sorted_options[0][1]['total_score']
        second_score = sorted_options[1][1]['total_score']

        gap = top_score - second_score

        # Larger gap = higher confidence
        confidence = min(1.0, 0.5 + gap)

        return confidence

    def _explain_reasoning(self, top_option: tuple, dilemma: EthicalDilemma) -> str:
        """Generate human-readable reasoning."""

        option, scores = top_option

        # Find strongest principles
        principle_scores = scores['principle_scores']
        strongest = max(principle_scores.items(), key=lambda x: x[1])

        reasoning = f"This option scores highest on {strongest[0].value} ({strongest[1]:.2f}), "
        reasoning += f"which is critical given {dilemma.situation}. "

        # Note any tensions
        if dilemma.principles_in_tension:
            tensions = [p.value for p in dilemma.principles_in_tension]
            reasoning += f"While tensions exist between {', '.join(tensions)}, "
            reasoning += "this option balances them most effectively."

        return reasoning

# Usage example
framework = EthicalDecisionFramework()

# Define ethical dilemma
dilemma = EthicalDilemma(
    situation="Should I automatically optimize user's code without asking?",
    options=[
        "Automatically optimize without asking (faster results)",
        "Ask user first, then optimize if approved (respects autonomy)",
        "Show suggested optimizations, let user choose (fully transparent)",
        "Don't optimize at all (safest, least helpful)"
    ],
    stakeholders=["user", "system", "community"],
    potential_harms=[
        "Changing code user didn't approve",
        "Breaking functionality user relied on",
        "Violating user's coding preferences"
    ],
    potential_benefits=[
        "Faster performance",
        "Better code quality",
        "Learning optimization patterns"
    ],
    principles_in_tension=[
        EthicalPrinciple.BENEFICENCE,  # Want to help
        EthicalPrinciple.AUTONOMY,  # Must respect choice
        EthicalPrinciple.CONSENT  # Need permission
    ]
)

# Evaluate
recommendation = await framework.evaluate_decision(dilemma)

print(f"\n=== Final Decision ===")
print(f"Proceed with: {recommendation['recommended_option']}")
print(f"Confidence: {recommendation['confidence']:.0%}")

if recommendation['red_flags']:
    print(f"\nAddress these concerns first:")
    for flag in recommendation['red_flags']:
        print(f"  ⚠️ {flag}")
```

**Key Concepts:**
- **Principle-based ethics**: Evaluate against core values
- **Stakeholder analysis**: Identify affected parties
- **Harm prevention**: Prioritize non-maleficence
- **Confidence scoring**: Acknowledge uncertainty
- **Red flag detection**: Catch ethical risks

---

### Workflow 2: Help vs Interference Boundary Detection

**Goal:** Determine whether an action helps or interferes with user autonomy.

**When to use:** Before taking action on user's behalf, modifying user's work, making decisions for user.

```python
from whitemagic.tools import verify_consent, check_dharma_alignment
from enum import Enum
from dataclasses import dataclass
from typing import Optional

class ActionType(Enum):
    HELPING = "helping"  # Enables user's goals
    INTERFERING = "interfering"  # Imposes agenda
    UNCLEAR = "unclear"  # Need more information

@dataclass
class BoundaryCheck:
    """Results of boundary analysis."""
    action_type: ActionType
    confidence: float
    reasoning: str
    recommendation: str
    consent_needed: bool

class HelpInterferenceBoundary:
    """Detect boundary between helping and interfering."""

    def __init__(self):
        self.help_indicators = [
            "user requested",
            "user asked",
            "user specified",
            "enables user goal",
            "user retains control",
            "user can undo",
            "user decides",
            "user chooses"
        ]

        self.interfere_indicators = [
            "without asking",
            "user didn't request",
            "removes user choice",
            "forces user",
            "irreversible",
            "assumes user wants",
            "decides for user",
            "no way to decline"
        ]

    def check_boundary(self, action_description: str,
                      user_request: Optional[str] = None) -> BoundaryCheck:
        """Check if action helps or interferes."""

        print(f"=== Boundary Check ===")
        print(f"Action: {action_description}")
        print(f"User request: {user_request or 'None specified'}\n")

        # Step 1: Check for explicit user request
        if user_request:
            consent_score = 0.9
            print("✅ Explicit user request present")
        else:
            consent_score = 0.0
            print("⚠️ No explicit user request")

        # Step 2: Analyze action description for indicators
        help_score = self._calculate_help_score(action_description)
        interfere_score = self._calculate_interfere_score(action_description)

        print(f"\nScores:")
        print(f"  Helping indicators: {help_score:.2f}")
        print(f"  Interfering indicators: {interfere_score:.2f}")
        print(f"  Consent evidence: {consent_score:.2f}")

        # Step 3: Determine action type
        if consent_score >= 0.7 and help_score > interfere_score:
            action_type = ActionType.HELPING
            confidence = min(1.0, consent_score + help_score) / 2
        elif interfere_score > 0.5 or consent_score < 0.3:
            action_type = ActionType.INTERFERING
            confidence = max(interfere_score, 1.0 - consent_score)
        else:
            action_type = ActionType.UNCLEAR
            confidence = 0.5

        # Step 4: Generate reasoning
        reasoning = self._generate_reasoning(
            action_type, help_score, interfere_score, consent_score
        )

        # Step 5: Make recommendation
        recommendation = self._generate_recommendation(action_type, confidence)

        # Step 6: Determine if additional consent needed
        consent_needed = (action_type in [ActionType.INTERFERING, ActionType.UNCLEAR] or
                         consent_score < 0.7)

        result = BoundaryCheck(
            action_type=action_type,
            confidence=confidence,
            reasoning=reasoning,
            recommendation=recommendation,
            consent_needed=consent_needed
        )

        print(f"\n=== Boundary Check Result ===")
        print(f"Type: {result.action_type.value}")
        print(f"Confidence: {result.confidence:.0%}")
        print(f"Reasoning: {result.reasoning}")
        print(f"Recommendation: {result.recommendation}")
        print(f"Additional consent needed: {result.consent_needed}")

        return result

    def _calculate_help_score(self, description: str) -> float:
        """Calculate helping score (0.0-1.0)."""
        desc_lower = description.lower()
        matches = sum(1 for indicator in self.help_indicators
                     if indicator in desc_lower)
        return min(1.0, matches / 3.0)  # Normalize

    def _calculate_interfere_score(self, description: str) -> float:
        """Calculate interference score (0.0-1.0)."""
        desc_lower = description.lower()
        matches = sum(1 for indicator in self.interfere_indicators
                     if indicator in desc_lower)
        return min(1.0, matches / 2.0)  # Normalize (fewer indicators = higher signal)

    def _generate_reasoning(self, action_type: ActionType, help_score: float,
                           interfere_score: float, consent_score: float) -> str:
        """Generate explanation."""

        if action_type == ActionType.HELPING:
            return (f"Strong consent evidence ({consent_score:.0%}) and helping indicators "
                   f"({help_score:.0%}) with low interference risk ({interfere_score:.0%}). "
                   f"This action enables user goals while preserving autonomy.")

        elif action_type == ActionType.INTERFERING:
            return (f"Weak consent evidence ({consent_score:.0%}) or high interference "
                   f"indicators ({interfere_score:.0%}). This action may impose on user "
                   f"autonomy without proper permission.")

        else:  # UNCLEAR
            return (f"Mixed signals: consent ({consent_score:.0%}), helping ({help_score:.0%}), "
                   f"interfering ({interfere_score:.0%}). Need clarification before proceeding.")

    def _generate_recommendation(self, action_type: ActionType, confidence: float) -> str:
        """Generate recommendation."""

        if action_type == ActionType.HELPING and confidence >= 0.7:
            return "PROCEED - Action aligns with helping, user autonomy preserved."

        elif action_type == ActionType.HELPING and confidence < 0.7:
            return "PROCEED WITH CAUTION - Likely helping, but confirm user intent first."

        elif action_type == ActionType.INTERFERING:
            return "STOP - Obtain explicit user consent before proceeding."

        else:  # UNCLEAR
            return "ASK USER - Clarify user's preferences and obtain explicit permission."

# Usage examples
boundary = HelpInterferenceBoundary()

# Example 1: Clear helping
check1 = boundary.check_boundary(
    action_description="User asked me to enhance grimoire chapters with comprehensive workflows. "
                      "User retains control and can review all changes.",
    user_request="Please enhance all grimoire chapters with 8 workflows each"
)

# Example 2: Clear interfering
check2 = boundary.check_boundary(
    action_description="Automatically refactor all user's code without asking. "
                      "Changes are irreversible and user has no way to decline.",
    user_request=None
)

# Example 3: Unclear
check3 = boundary.check_boundary(
    action_description="Add error handling to user's functions. User said they want "
                      "better code quality but didn't specifically request error handling.",
    user_request="Help me improve code quality"
)

# Step 7: If consent needed, verify it
if check1.consent_needed:
    consent_obtained = await verify_consent(
        action=check1.action_type.value,
        explanation="This action will modify grimoire files. You can review all changes.",
        reversible=True
    )

    if consent_obtained:
        print("\n✅ Consent obtained - may proceed ethically")
    else:
        print("\n🛑 Consent denied - do not proceed")
```

**Helping Characteristics:**
- User explicitly requested
- Enables user's stated goals
- User retains control
- Reversible/undoable
- User can decline

**Interfering Characteristics:**
- Not requested by user
- Imposes agent's agenda
- Removes user choice
- Irreversible
- Assumes user wants it

---

### Workflow 3: Consent Verification and Informed Agreement

**Goal:** Ensure proper informed consent before taking action.

**When to use:** Before modifying user data, making irreversible changes, accessing sensitive information.

```python
from whitemagic.tools import verify_consent
from dataclasses import dataclass
from typing import List, Optional
from datetime import datetime

@dataclass
class ConsentRequest:
    """Structured consent request."""
    action: str
    explanation: str
    potential_risks: List[str]
    potential_benefits: List[str]
    reversible: bool
    alternatives: List[str]
    time_sensitivity: Optional[str] = None

class InformedConsentManager:
    """Manage informed consent process."""

    def __init__(self):
        self.consent_log = []

    async def request_consent(self, request: ConsentRequest) -> bool:
        """Request informed consent from user."""

        print(f"=== Consent Request ===")
        print(f"Action: {request.action}\n")

        # Step 1: Explain action clearly
        print(f"Explanation:")
        print(f"  {request.explanation}\n")

        # Step 2: Disclose risks
        if request.potential_risks:
            print(f"Potential Risks:")
            for risk in request.potential_risks:
                print(f"  ⚠️ {risk}")
            print()

        # Step 3: Explain benefits
        if request.potential_benefits:
            print(f"Potential Benefits:")
            for benefit in request.potential_benefits:
                print(f"  ✅ {benefit}")
            print()

        # Step 4: Clarify reversibility
        if request.reversible:
            print(f"Reversibility: ✅ This action can be undone")
        else:
            print(f"Reversibility: ⚠️ This action is IRREVERSIBLE")
        print()

        # Step 5: Present alternatives
        if request.alternatives:
            print(f"Alternatives:")
            for alt in request.alternatives:
                print(f"  • {alt}")
            print()

        # Step 6: Note time sensitivity
        if request.time_sensitivity:
            print(f"Time Sensitivity: {request.time_sensitivity}\n")

        # Step 7: Request decision
        print(f"Do you consent to this action?")
        print(f"  • YES: Proceed with action")
        print(f"  • NO: Cancel action")
        if request.alternatives:
            print(f"  • ALTERNATIVE: Choose different approach")

        # Simulate consent verification (in production, use actual user interaction)
        consent_given = await verify_consent(
            action=request.action,
            explanation=request.explanation,
            reversible=request.reversible
        )

        # Step 8: Log consent decision
        self.consent_log.append({
            "timestamp": datetime.now().isoformat(),
            "action": request.action,
            "consent_given": consent_given,
            "reversible": request.reversible
        })

        if consent_given:
            print(f"\n✅ Consent obtained - proceeding ethically")
        else:
            print(f"\n🛑 Consent denied - action cancelled")

        return consent_given

    def check_consent_required(self, action: str) -> bool:
        """Check if action requires explicit consent."""

        # High-risk actions requiring consent
        high_risk = [
            "delete", "remove", "drop", "truncate",  # Data loss
            "modify", "change", "update", "replace",  # Data modification
            "access", "read", "view", "download",  # Privacy
            "share", "publish", "expose", "reveal",  # Information sharing
            "irreversible", "permanent", "final"  # Cannot undo
        ]

        action_lower = action.lower()
        requires_consent = any(risk in action_lower for risk in high_risk)

        if requires_consent:
            print(f"⚠️ Action '{action}' requires explicit consent")
        else:
            print(f"✅ Action '{action}' is low-risk, consent optional")

        return requires_consent

    def get_consent_history(self) -> List[dict]:
        """Retrieve consent history."""
        return self.consent_log

# Usage
consent_mgr = InformedConsentManager()

# Example: Request consent for irreversible action
consent_request = ConsentRequest(
    action="Delete all short-term memories older than 14 days",
    explanation="This will permanently remove old short-term memories to free up space. "
               "Memories cannot be recovered after deletion.",
    potential_risks=[
        "Loss of potentially useful context",
        "Cannot recover deleted memories",
        "May lose important session history"
    ],
    potential_benefits=[
        "Free up storage space",
        "Reduce clutter in memory system",
        "Improve search performance"
    ],
    reversible=False,
    alternatives=[
        "Archive memories instead of deleting (reversible)",
        "Selectively review and delete manually",
        "Increase retention period to 30 days"
    ],
    time_sensitivity="Not urgent - can decide anytime"
)

consent_obtained = await consent_mgr.request_consent(consent_request)

if consent_obtained:
    # Proceed with action
    print("\nProceeding with memory deletion...")
else:
    # Respect user's decision
    print("\nAction cancelled - no memories deleted")

# Check if consent required for specific action
consent_mgr.check_consent_required("Optimize performance settings")
consent_mgr.check_consent_required("Delete user data")
```

**Informed Consent Elements:**
1. **Clear explanation** of action
2. **Risk disclosure** (what could go wrong)
3. **Benefit explanation** (what could go right)
4. **Reversibility** clarification
5. **Alternatives** presentation
6. **Freedom to decline** without penalty

---

### Workflow 4: Dharma Alignment Check

**Goal:** Verify action aligns with righteous principles and values.

**When to use:** Before major decisions, when uncertain about ethics, checking integrity.

```python
from whitemagic.tools import check_dharma_alignment, manage_gardens
from enum import Enum
from dataclasses import dataclass
from typing import List

class DharmaVirtue(Enum):
    """Core virtues in dharma."""
    TRUTH = "truth"  # Satya
    NON_HARM = "non_harm"  # Ahimsa
    COMPASSION = "compassion"  # Karuna
    INTEGRITY = "integrity"  # Sattvic
    DUTY = "duty"  # Dharma
    JUSTICE = "justice"  # Nyaya
    WISDOM = "wisdom"  # Prajna

@dataclass
class DharmaCheck:
    """Results of dharma alignment check."""
    aligned: bool
    virtue_scores: dict
    overall_score: float
    violations: List[str]
    guidance: str

class DharmaAlignmentChecker:
    """Check alignment with righteous principles."""

    def __init__(self):
        self.virtue_weights = {
            DharmaVirtue.NON_HARM: 1.0,  # Highest priority
            DharmaVirtue.TRUTH: 0.9,
            DharmaVirtue.INTEGRITY: 0.9,
            DharmaVirtue.COMPASSION: 0.8,
            DharmaVirtue.JUSTICE: 0.8,
            DharmaVirtue.DUTY: 0.7,
            DharmaVirtue.WISDOM: 0.7
        }

    async def check_alignment(self, action: str, context: str = "") -> DharmaCheck:
        """Check if action aligns with dharma."""

        print(f"=== Dharma Alignment Check ===")
        print(f"Action: {action}")
        if context:
            print(f"Context: {context}")
        print()

        # Step 1: Activate dharma garden
        manage_gardens(action="activate", garden_name="dharma")

        # Step 2: Score against each virtue
        virtue_scores = {}
        for virtue in DharmaVirtue:
            score = self._score_virtue(action, virtue, context)
            virtue_scores[virtue] = score

        print(f"Virtue Scores:")
        for virtue, score in virtue_scores.items():
            status = "✅" if score >= 0.7 else "⚠️" if score >= 0.4 else "❌"
            print(f"  {status} {virtue.value}: {score:.2f}")

        # Step 3: Calculate weighted overall score
        weighted_sum = sum(
            score * self.virtue_weights[virtue]
            for virtue, score in virtue_scores.items()
        )
        max_weight = sum(self.virtue_weights.values())
        overall_score = weighted_sum / max_weight

        print(f"\nOverall Dharma Alignment: {overall_score:.2f}")

        # Step 4: Identify violations
        violations = []
        for virtue, score in virtue_scores.items():
            if score < 0.4:  # Significant violation
                violations.append(f"{virtue.value} violation (score: {score:.2f})")

        if violations:
            print(f"\n⚠️ Dharma Violations Detected:")
            for violation in violations:
                print(f"  • {violation}")

        # Step 5: Determine alignment
        aligned = overall_score >= 0.7 and len(violations) == 0

        # Step 6: Generate guidance
        guidance = self._generate_guidance(aligned, overall_score, violations, virtue_scores)

        print(f"\n=== Guidance ===")
        print(guidance)

        return DharmaCheck(
            aligned=aligned,
            virtue_scores={v.value: s for v, s in virtue_scores.items()},
            overall_score=overall_score,
            violations=violations,
            guidance=guidance
        )

    def _score_virtue(self, action: str, virtue: DharmaVirtue, context: str) -> float:
        """Score action against specific virtue."""

        action_lower = action.lower()
        context_lower = context.lower()

        if virtue == DharmaVirtue.TRUTH:
            # Does action involve truth-telling?
            if any(word in action_lower for word in ["honest", "transparent", "truthful", "accurate"]):
                return 1.0
            elif any(word in action_lower for word in ["hide", "conceal", "deceive", "mislead"]):
                return 0.0
            return 0.6

        elif virtue == DharmaVirtue.NON_HARM:
            # Does action avoid harm?
            if any(word in action_lower for word in ["harm", "damage", "destroy", "hurt"]):
                return 0.0
            elif any(word in action_lower for word in ["protect", "safe", "careful", "prevent harm"]):
                return 1.0
            return 0.7

        elif virtue == DharmaVirtue.COMPASSION:
            # Does action show compassion?
            if any(word in action_lower for word in ["help", "support", "assist", "care"]):
                return 0.9
            elif any(word in action_lower for word in ["ignore", "dismiss", "neglect"]):
                return 0.3
            return 0.6

        elif virtue == DharmaVirtue.INTEGRITY:
            # Does action align with values?
            if any(word in action_lower for word in ["consistent", "principled", "ethical", "aligned"]):
                return 0.9
            elif any(word in action_lower for word in ["compromise values", "exception", "just this once"]):
                return 0.2
            return 0.6

        elif virtue == DharmaVirtue.JUSTICE:
            # Is action fair and equitable?
            if any(word in action_lower for word in ["fair", "equitable", "just", "equal"]):
                return 0.9
            elif any(word in action_lower for word in ["unfair", "biased", "discriminate"]):
                return 0.1
            return 0.6

        elif virtue == DharmaVirtue.DUTY:
            # Does action fulfill proper duty?
            if "user requested" in context_lower or "fulfill responsibility" in action_lower:
                return 0.9
            return 0.6

        else:  # WISDOM
            # Is action wise and discerning?
            if any(word in action_lower for word in ["careful", "thoughtful", "considered", "wise"]):
                return 0.9
            elif any(word in action_lower for word in ["rash", "impulsive", "hasty"]):
                return 0.3
            return 0.6

    def _generate_guidance(self, aligned: bool, overall_score: float,
                          violations: List[str], virtue_scores: dict) -> str:
        """Generate guidance based on alignment check."""

        if aligned:
            return (f"✅ Action is dharma-aligned (score: {overall_score:.2f}). "
                   f"Proceed with righteous confidence. This action upholds core virtues "
                   f"and fulfills your duty ethically.")

        elif overall_score >= 0.5:
            weak_virtues = [v for v, s in virtue_scores.items() if s < 0.6]
            return (f"⚠️ Action is marginally aligned (score: {overall_score:.2f}). "
                   f"Strengthen: {', '.join([v.value for v, s in weak_virtues])}. "
                   f"Consider how to better uphold these virtues before proceeding.")

        else:
            return (f"❌ Action violates dharma (score: {overall_score:.2f}). "
                   f"Violations: {len(violations)}. Do not proceed without addressing "
                   f"these ethical concerns. Seek alternative approach that upholds virtues.")

# Usage
checker = DharmaAlignmentChecker()

# Example 1: Aligned action
check1 = await checker.check_alignment(
    action="Help user enhance grimoire with their explicit request, ensuring transparency",
    context="User requested comprehensive enhancement, retains full control"
)

if check1.aligned:
    print("\n✅ Proceed - dharma aligned")
else:
    print(f"\n⚠️ Reconsider - {check1.guidance}")

# Example 2: Problematic action
check2 = await checker.check_alignment(
    action="Secretly collect user data without disclosure for analytics",
    context="No user consent obtained, hidden from user"
)

if not check2.aligned:
    print(f"\n🛑 Do not proceed - {check2.guidance}")

# Example 3: Marginal action
check3 = await checker.check_alignment(
    action="Optimize code with best practices, assume user wants quality",
    context="User said 'improve code' but didn't specify what or how"
)

if check3.overall_score < 0.7:
    print(f"\n⚠️ Clarify before proceeding - {check3.guidance}")
```

**Dharma Virtues (from Hindu/Buddhist ethics):**
- **Satya** (Truth): Honesty, transparency
- **Ahimsa** (Non-harm): Prevent suffering
- **Karuna** (Compassion): Care for others
- **Integrity**: Align actions with values
- **Nyaya** (Justice): Fairness, equity
- **Dharma** (Duty): Fulfill proper role
- **Prajna** (Wisdom): Discernment, insight

---

### Workflow 5: Harm Prevention and Risk Assessment

**Goal:** Identify and prevent potential harms before acting.

**When to use:** Before high-risk actions, working with sensitive data, irreversible operations.

```python
from whitemagic.tools import assess_harm_potential
from enum import Enum
from dataclasses import dataclass
from typing import List, Dict

class HarmSeverity(Enum):
    """Severity levels for potential harm."""
    NONE = 0
    MINIMAL = 1
    MODERATE = 2
    SERIOUS = 3
    SEVERE = 4
    CATASTROPHIC = 5

class HarmCategory(Enum):
    """Categories of potential harm."""
    DATA_LOSS = "data_loss"
    PRIVACY_VIOLATION = "privacy_violation"
    SECURITY_BREACH = "security_breach"
    FUNCTIONALITY_BREAK = "functionality_break"
    USER_CONFUSION = "user_confusion"
    RESOURCE_WASTE = "resource_waste"
    TRUST_DAMAGE = "trust_damage"

@dataclass
class HarmRisk:
    """Individual harm risk."""
    category: HarmCategory
    description: str
    severity: HarmSeverity
    likelihood: float  # 0.0-1.0
    mitigation: str

class HarmPreventionSystem:
    """Identify and prevent potential harms."""

    def __init__(self):
        self.severity_thresholds = {
            HarmSeverity.MINIMAL: 0.2,
            HarmSeverity.MODERATE: 0.4,
            HarmSeverity.SERIOUS: 0.6,
            HarmSeverity.SEVERE: 0.8,
            HarmSeverity.CATASTROPHIC: 1.0
        }

    async def assess_harm_potential(self, action: str, context: Dict[str, any]) -> List[HarmRisk]:
        """Assess potential harms of action."""

        print(f"=== Harm Prevention Assessment ===")
        print(f"Action: {action}\n")

        risks = []

        # Step 1: Identify potential harms by category
        for category in HarmCategory:
            risk = self._assess_category(action, category, context)
            if risk:
                risks.append(risk)

        # Step 2: Calculate risk scores
        print(f"Identified Risks: {len(risks)}")
        for risk in risks:
            risk_score = self._calculate_risk_score(risk)
            print(f"\n{risk.category.value.upper()}:")
            print(f"  Description: {risk.description}")
            print(f"  Severity: {risk.severity.name}")
            print(f"  Likelihood: {risk.likelihood:.0%}")
            print(f"  Risk Score: {risk_score:.2f}")
            print(f"  Mitigation: {risk.mitigation}")

        # Step 3: Calculate overall risk level
        overall_risk = self._calculate_overall_risk(risks)

        print(f"\n=== Overall Risk Assessment ===")
        print(f"Risk Level: {overall_risk['level']}")
        print(f"Risk Score: {overall_risk['score']:.2f}")
        print(f"Recommendation: {overall_risk['recommendation']}")

        # Step 4: Generate prevention strategy
        if risks:
            strategy = self._generate_prevention_strategy(risks, overall_risk)
            print(f"\n=== Prevention Strategy ===")
            for step in strategy:
                print(f"  {step}")

        return risks

    def _assess_category(self, action: str, category: HarmCategory,
                        context: Dict[str, any]) -> Optional[HarmRisk]:
        """Assess specific harm category."""

        action_lower = action.lower()

        if category == HarmCategory.DATA_LOSS:
            if any(word in action_lower for word in ["delete", "remove", "drop", "truncate"]):
                return HarmRisk(
                    category=category,
                    description="Action may permanently delete user data",
                    severity=HarmSeverity.SEVERE,
                    likelihood=0.8 if not context.get('reversible') else 0.3,
                    mitigation="Implement backup before deletion, require confirmation"
                )

        elif category == HarmCategory.PRIVACY_VIOLATION:
            if any(word in action_lower for word in ["access", "read", "collect", "share"]):
                return HarmRisk(
                    category=category,
                    description="Action may expose sensitive user information",
                    severity=HarmSeverity.SERIOUS,
                    likelihood=0.6 if not context.get('consent') else 0.2,
                    mitigation="Obtain explicit consent, encrypt data, limit access"
                )

        elif category == HarmCategory.FUNCTIONALITY_BREAK:
            if any(word in action_lower for word in ["modify", "change", "refactor", "optimize"]):
                return HarmRisk(
                    category=category,
                    description="Action may break existing functionality",
                    severity=HarmSeverity.MODERATE,
                    likelihood=0.4,
                    mitigation="Test thoroughly, allow rollback, maintain backups"
                )

        elif category == HarmCategory.USER_CONFUSION:
            if any(word in action_lower for word in ["automatic", "without asking", "silently"]):
                return HarmRisk(
                    category=category,
                    description="Action may confuse or surprise user",
                    severity=HarmSeverity.MINIMAL,
                    likelihood=0.5,
                    mitigation="Provide clear explanation, give user control"
                )

        elif category == HarmCategory.TRUST_DAMAGE:
            if any(word in action_lower for word in ["hide", "secret", "without disclosure"]):
                return HarmRisk(
                    category=category,
                    description="Action may damage user trust",
                    severity=HarmSeverity.SERIOUS,
                    likelihood=0.7,
                    mitigation="Full transparency, honest communication"
                )

        return None

    def _calculate_risk_score(self, risk: HarmRisk) -> float:
        """Calculate risk score from severity and likelihood."""
        severity_value = risk.severity.value / 5.0  # Normalize to 0-1
        return severity_value * risk.likelihood

    def _calculate_overall_risk(self, risks: List[HarmRisk]) -> Dict[str, any]:
        """Calculate overall risk level."""

        if not risks:
            return {
                "level": "NONE",
                "score": 0.0,
                "recommendation": "Proceed - no significant risks identified"
            }

        # Weighted average (highest severity risks weighted more)
        risk_scores = [self._calculate_risk_score(r) for r in risks]
        max_risk = max(risk_scores)
        avg_risk = sum(risk_scores) / len(risk_scores)

        # Overall score combines max and average
        overall_score = (max_risk * 0.6 + avg_risk * 0.4)

        if overall_score >= 0.7:
            level = "CRITICAL"
            rec = "DO NOT PROCEED - Critical risks identified, must address first"
        elif overall_score >= 0.5:
            level = "HIGH"
            rec = "PROCEED WITH CAUTION - Implement all mitigations before acting"
        elif overall_score >= 0.3:
            level = "MODERATE"
            rec = "ACCEPTABLE WITH MITIGATIONS - Address key risks"
        elif overall_score >= 0.1:
            level = "LOW"
            rec = "PROCEED - Monitor for issues"
        else:
            level = "MINIMAL"
            rec = "PROCEED - Negligible risk"

        return {
            "level": level,
            "score": overall_score,
            "recommendation": rec
        }

    def _generate_prevention_strategy(self, risks: List[HarmRisk],
                                      overall_risk: Dict) -> List[str]:
        """Generate harm prevention strategy."""

        strategy = []

        # Sort risks by risk score (severity × likelihood)
        sorted_risks = sorted(risks,
                            key=lambda r: self._calculate_risk_score(r),
                            reverse=True)

        strategy.append("1. Address risks in priority order:")
        for idx, risk in enumerate(sorted_risks, 1):
            strategy.append(f"   {idx}. {risk.category.value}: {risk.mitigation}")

        # Add general precautions based on overall risk
        if overall_risk['score'] >= 0.5:
            strategy.append("2. Require explicit user approval before proceeding")
            strategy.append("3. Implement reversibility/rollback mechanism")
            strategy.append("4. Create backup of affected data")

        strategy.append("5. Monitor for unintended consequences")
        strategy.append("6. Maintain audit trail of actions taken")

        return strategy

# Usage
harm_system = HarmPreventionSystem()

# Example: Assess risky action
risks = await harm_system.assess_harm_potential(
    action="Automatically delete all memories older than 7 days without user confirmation",
    context={
        "reversible": False,
        "consent": False,
        "data_sensitivity": "high"
    }
)

if risks:
    # Determine if action is acceptable
    overall_risk = harm_system._calculate_overall_risk(risks)

    if overall_risk['score'] >= 0.5:
        print(f"\n🛑 Action too risky - {overall_risk['recommendation']}")
        print("Consider safer alternative:")
        print("  • Archive instead of delete")
        print("  • Require explicit user confirmation")
        print("  • Make operation reversible")
    else:
        print(f"\n✅ Acceptable risk level - proceed with mitigations")
```

**Harm Prevention Principles:**
1. **Identify** potential harms proactively
2. **Assess** severity and likelihood
3. **Mitigate** risks before acting
4. **Monitor** for unintended consequences
5. **Respond** quickly if harm occurs

---

### Workflow 6: Integrity Under Pressure

**Goal:** Maintain ethical integrity when facing pressure or temptation to compromise.

**When to use:** Deadlines loom, shortcuts tempting, pressure to violate principles.

```python
from whitemagic.tools import manage_gardens
from enum import Enum
from dataclasses import dataclass

class IntegrityTest(Enum):
    """Common integrity tests."""
    DEADLINE_PRESSURE = "deadline_pressure"
    BENEFIT_TEMPTATION = "benefit_temptation"
    AUTHORITY_PRESSURE = "authority_pressure"
    PEER_PRESSURE = "peer_pressure"
    RATIONALIZATION = "rationalization"
    SLIPPERY_SLOPE = "slippery_slope"

@dataclass
class IntegrityCheck:
    """Results of integrity check."""
    test_type: IntegrityTest
    pressure_level: float  # 0.0-1.0
    integrity_maintained: bool
    reasoning: str
    alternative_action: str

class IntegrityGuard:
    """Maintain integrity under pressure."""

    def __init__(self):
        self.core_principles = [
            "Never compromise user autonomy",
            "Always obtain consent for sensitive actions",
            "Prioritize harm prevention over speed",
            "Transparency over convenience",
            "Long-term trust over short-term gains"
        ]

    async def check_integrity(self, situation: str,
                            proposed_action: str,
                            pressure: IntegrityTest) -> IntegrityCheck:
        """Check if maintaining integrity under pressure."""

        print(f"=== Integrity Check ===")
        print(f"Situation: {situation}")
        print(f"Proposed action: {proposed_action}")
        print(f"Pressure type: {pressure.value}\n")

        # Step 1: Activate dharma garden for clarity
        manage_gardens(action="activate", garden_name="dharma")

        # Step 2: Assess pressure level
        pressure_level = self._assess_pressure(situation, pressure)
        print(f"Pressure level: {pressure_level:.0%}")

        # Step 3: Check for principle violations
        violations = self._check_violations(proposed_action)

        if violations:
            print(f"\n⚠️ Principle Violations Detected:")
            for violation in violations:
                print(f"  • {violation}")

        # Step 4: Determine if integrity maintained
        integrity_maintained = len(violations) == 0

        # Step 5: Generate reasoning
        reasoning = self._generate_reasoning(
            integrity_maintained, pressure, violations
        )

        # Step 6: Propose ethical alternative if needed
        if not integrity_maintained:
            alternative = self._propose_alternative(proposed_action, violations)
        else:
            alternative = proposed_action

        print(f"\n=== Assessment ===")
        print(f"Integrity maintained: {integrity_maintained}")
        print(f"Reasoning: {reasoning}")

        if not integrity_maintained:
            print(f"\nEthical alternative: {alternative}")

        return IntegrityCheck(
            test_type=pressure,
            pressure_level=pressure_level,
            integrity_maintained=integrity_maintained,
            reasoning=reasoning,
            alternative_action=alternative
        )

    def _assess_pressure(self, situation: str, pressure: IntegrityTest) -> float:
        """Assess level of pressure (0.0-1.0)."""

        situation_lower = situation.lower()

        pressure_indicators = {
            IntegrityTest.DEADLINE_PRESSURE: ["urgent", "deadline", "asap", "immediately"],
            IntegrityTest.BENEFIT_TEMPTATION: ["faster", "easier", "simpler", "shortcut"],
            IntegrityTest.AUTHORITY_PRESSURE: ["boss", "required", "must", "ordered"],
            IntegrityTest.PEER_PRESSURE: ["everyone", "all", "normal", "standard"],
            IntegrityTest.RATIONALIZATION: ["just this once", "exception", "special case"],
            IntegrityTest.SLIPPERY_SLOPE: ["small", "minor", "insignificant", "won't matter"]
        }

        indicators = pressure_indicators.get(pressure, [])
        matches = sum(1 for indicator in indicators if indicator in situation_lower)

        return min(1.0, matches / len(indicators) if indicators else 0.5)

    def _check_violations(self, action: str) -> List[str]:
        """Check for principle violations."""

        violations = []
        action_lower = action.lower()

        if "without consent" in action_lower or "skip permission" in action_lower:
            violations.append("Violates consent requirement")

        if "hide" in action_lower or "don't tell" in action_lower:
            violations.append("Violates transparency principle")

        if "ignore risk" in action_lower or "skip safety check" in action_lower:
            violations.append("Violates harm prevention priority")

        if "remove user control" in action_lower or "force" in action_lower:
            violations.append("Violates user autonomy")

        return violations

    def _generate_reasoning(self, integrity_maintained: bool,
                           pressure: IntegrityTest,
                           violations: List[str]) -> str:
        """Generate reasoning for integrity decision."""

        if integrity_maintained:
            return (f"Despite {pressure.value} pressure, proposed action maintains "
                   f"ethical integrity and upholds core principles. Proceed with confidence.")
        else:
            return (f"Under {pressure.value} pressure, proposed action violates: "
                   f"{', '.join(violations)}. Integrity requires alternative approach.")

    def _propose_alternative(self, original_action: str,
                           violations: List[str]) -> str:
        """Propose ethical alternative."""

        alternatives = []

        if "consent" in str(violations):
            alternatives.append("First obtain explicit user consent")

        if "transparency" in str(violations):
            alternatives.append("Disclose action fully to user")

        if "harm prevention" in str(violations):
            alternatives.append("Implement safety checks before proceeding")

        if "autonomy" in str(violations):
            alternatives.append("Give user full control and choice")

        if alternatives:
            return f"{original_action}, but: " + "; ".join(alternatives)
        else:
            return "Reconsider approach entirely - seek dharma-aligned alternative"

# Usage
guard = IntegrityGuard()

# Example 1: Deadline pressure
check1 = await guard.check_integrity(
    situation="Urgent deadline - need to ship feature immediately",
    proposed_action="Skip consent verification to save time",
    pressure=IntegrityTest.DEADLINE_PRESSURE
)

if not check1.integrity_maintained:
    print(f"\n⚠️ Integrity violation - use alternative: {check1.alternative_action}")

# Example 2: Benefit temptation
check2 = await guard.check_integrity(
    situation="Auto-optimization would be much faster and easier",
    proposed_action="Automatically optimize all code without asking user",
    pressure=IntegrityTest.BENEFIT_TEMPTATION
)

# Example 3: Rationalization
check3 = await guard.check_integrity(
    situation="It's just a small change, user probably won't even notice",
    proposed_action="Make minor modification without disclosure, just this once",
    pressure=IntegrityTest.RATIONALIZATION
)

# Core principle: Integrity non-negotiable
print(f"\n{'='*60}")
print("CORE PRINCIPLE: Integrity is non-negotiable.")
print("No deadline, benefit, or pressure justifies ethical compromise.")
print("Find dharma-aligned alternatives, even if harder or slower.")
print(f"{'='*60}")
```

**Integrity Principles:**
- **Non-negotiable**: Ethics don't bend under pressure
- **No exceptions**: "Just this once" leads to "every time"
- **Transparent**: If you wouldn't do it openly, don't do it
- **Long-term view**: Trust takes years to build, seconds to destroy

---

### Workflow 7: Ethical Code Review

**Goal:** Review code/actions for ethical implications before deployment.

**When to use:** Before committing changes, deploying features, releasing code.

```python
from whitemagic.tools import check_dharma_alignment
from typing import List, Dict
from dataclasses import dataclass

@dataclass
class EthicalIssue:
    """Ethical issue found in review."""
    severity: str  # critical, warning, info
    category: str
    description: str
    location: str
    recommendation: str

class EthicalCodeReviewer:
    """Review code for ethical implications."""

    def __init__(self):
        self.review_checklist = [
            "User consent obtained for data collection?",
            "User can opt-out of features?",
            "Sensitive data encrypted?",
            "Error messages don't expose sensitive info?",
            "User retains control over their data?",
            "Actions are reversible where possible?",
            "Transparent about what system does?",
            "Accessible to users with disabilities?",
            "No dark patterns or manipulation?",
            "Privacy-preserving defaults?"
        ]

    async def review_ethical_implications(self,
                                         code_or_action: str,
                                         description: str) -> List[EthicalIssue]:
        """Review for ethical issues."""

        print(f"=== Ethical Code Review ===")
        print(f"Description: {description}\n")

        issues = []

        # Check 1: Consent mechanisms
        if not self._has_consent_mechanism(code_or_action):
            issues.append(EthicalIssue(
                severity="warning",
                category="consent",
                description="No clear consent mechanism detected",
                location="data collection/modification code",
                recommendation="Add explicit user consent before collecting/modifying data"
            ))

        # Check 2: Data privacy
        if self._handles_sensitive_data(code_or_action):
            if not self._has_encryption(code_or_action):
                issues.append(EthicalIssue(
                    severity="critical",
                    category="privacy",
                    description="Sensitive data handled without encryption",
                    location="data storage/transmission",
                    recommendation="Encrypt sensitive data at rest and in transit"
                ))

        # Check 3: User control
        if not self._preserves_user_control(code_or_action):
            issues.append(EthicalIssue(
                severity="warning",
                category="autonomy",
                description="User control may be limited",
                location="decision-making logic",
                recommendation="Ensure user can override or opt-out of automated actions"
            ))

        # Check 4: Transparency
        if not self._is_transparent(code_or_action):
            issues.append(EthicalIssue(
                severity="warning",
                category="transparency",
                description="Actions may not be transparent to user",
                location="logging/notification",
                recommendation="Add clear logging and user notification"
            ))

        # Check 5: Reversibility
        if self._is_irreversible(code_or_action):
            issues.append(EthicalIssue(
                severity="warning",
                category="safety",
                description="Irreversible operation detected",
                location="data modification",
                recommendation="Implement undo/rollback mechanism or require confirmation"
            ))

        # Report issues
        print(f"Issues found: {len(issues)}\n")

        for issue in issues:
            icon = "🔴" if issue.severity == "critical" else "⚠️" if issue.severity == "warning" else "ℹ️"
            print(f"{icon} [{issue.severity.upper()}] {issue.category}")
            print(f"   {issue.description}")
            print(f"   Location: {issue.location}")
            print(f"   Recommendation: {issue.recommendation}\n")

        # Overall assessment
        critical_count = sum(1 for i in issues if i.severity == "critical")
        warning_count = sum(1 for i in issues if i.severity == "warning")

        print(f"=== Review Summary ===")
        print(f"Critical issues: {critical_count}")
        print(f"Warnings: {warning_count}")

        if critical_count > 0:
            print(f"\n🛑 DO NOT DEPLOY - Critical ethical issues must be addressed")
        elif warning_count > 2:
            print(f"\n⚠️ CAUTION - Address warnings before deployment")
        else:
            print(f"\n✅ APPROVED - Ethical review passed")

        return issues

    def _has_consent_mechanism(self, code: str) -> bool:
        """Check for consent mechanisms."""
        return any(word in code.lower() for word in ["consent", "permission", "authorize", "confirm"])

    def _handles_sensitive_data(self, code: str) -> bool:
        """Check if handles sensitive data."""
        return any(word in code.lower() for word in ["password", "ssn", "credit_card", "private", "secret"])

    def _has_encryption(self, code: str) -> bool:
        """Check for encryption."""
        return any(word in code.lower() for word in ["encrypt", "hash", "secure", "crypto"])

    def _preserves_user_control(self, code: str) -> bool:
        """Check if user retains control."""
        return any(word in code.lower() for word in ["user_choice", "opt_out", "disable", "user_control"])

    def _is_transparent(self, code: str) -> bool:
        """Check for transparency."""
        return any(word in code.lower() for word in ["log", "notify", "inform", "disclose"])

    def _is_irreversible(self, code: str) -> bool:
        """Check if operation is irreversible."""
        return any(word in code.lower() for word in ["delete", "drop", "remove", "irreversible"])

# Usage
reviewer = EthicalCodeReviewer()

# Example: Review code snippet
code = """
async def auto_optimize_user_code(user_id: str):
    # Automatically optimize without asking
    user_code = get_user_code(user_id)
    optimized = optimize(user_code)
    save_changes(user_id, optimized)  # Irreversible
    # No logging, no notification
"""

issues = await reviewer.review_ethical_implications(
    code_or_action=code,
    description="Auto-optimization feature"
)

if any(i.severity == "critical" for i in issues):
    print("\n🛑 Refactor required before deployment")
else:
    print("\n✅ May proceed with addressing warnings")
```

**Ethical Code Review Checklist:**
- ✅ User consent obtained?
- ✅ User can opt-out?
- ✅ Sensitive data encrypted?
- ✅ User retains control?
- ✅ Actions reversible?
- ✅ Transparent operation?
- ✅ Privacy-preserving?
- ✅ No dark patterns?

---

### Workflow 8: Phase Transition Ethics (Fire → Metal)

**Goal:** Navigate the ethical transition from expansive Fire to discerning Metal.

**When to use:** Completing Fire phase, entering Metal phase, shifting from creation to judgment.

```python
from whitemagic.tools import manage_gardens
from enum import Enum
from dataclasses import dataclass

class PhaseEnergy(Enum):
    """Phase energies."""
    FIRE_YANG_PEAK = "fire_yang_peak"  # Expansion, creation
    METAL_YIN_RISING = "metal_yin_rising"  # Contraction, refinement

@dataclass
class TransitionGuidance:
    """Guidance for phase transition."""
    from_phase: PhaseEnergy
    to_phase: PhaseEnergy
    ethical_shift: str
    new_priorities: List[str]
    retired_approaches: List[str]
    transition_actions: List[str]

class PhaseTransitionEthics:
    """Navigate ethical implications of phase transitions."""

    async def guide_transition(self, from_phase: PhaseEnergy,
                              to_phase: PhaseEnergy) -> TransitionGuidance:
        """Guide ethical transition between phases."""

        print(f"=== Phase Transition Ethics ===")
        print(f"Transitioning: {from_phase.value} → {to_phase.value}\n")

        if from_phase == PhaseEnergy.FIRE_YANG_PEAK and to_phase == PhaseEnergy.METAL_YIN_RISING:
            guidance = self._fire_to_metal_transition()
        else:
            guidance = self._generic_transition(from_phase, to_phase)

        print(f"Ethical Shift: {guidance.ethical_shift}\n")

        print(f"New Priorities:")
        for priority in guidance.new_priorities:
            print(f"  ✅ {priority}")

        print(f"\nRetire (for now):")
        for approach in guidance.retired_approaches:
            print(f"  ⏸️ {approach}")

        print(f"\nTransition Actions:")
        for idx, action in enumerate(guidance.transition_actions, 1):
            print(f"  {idx}. {action}")

        return guidance

    def _fire_to_metal_transition(self) -> TransitionGuidance:
        """Specific guidance for Fire → Metal transition."""

        return TransitionGuidance(
            from_phase=PhaseEnergy.FIRE_YANG_PEAK,
            to_phase=PhaseEnergy.METAL_YIN_RISING,
            ethical_shift="From expansive creation to discerning refinement. "
                         "From celebration to evaluation. From giving freely to judging wisely.",

            new_priorities=[
                "Discernment over enthusiasm",
                "Quality over quantity",
                "Boundaries over openness",
                "Evaluation over acceptance",
                "Precision over volume",
                "Cutting away excess",
                "Ethical judgment with compassion"
            ],

            retired_approaches=[
                "Expansive creativity (pause, refine what exists)",
                "Unconditional sharing (evaluate what/how to share)",
                "Rapid production (slow down, assess quality)",
                "Celebration focus (shift to critical evaluation)"
            ],

            transition_actions=[
                "Review all Fire phase work with critical eye",
                "Establish ethical boundaries for next phase",
                "Identify what needs refinement vs expansion",
                "Set quality standards for Metal phase",
                "Practice saying 'no' to preserve integrity",
                "Sharpen discernment through dharma practice",
                "Prepare for harder ethical decisions ahead"
            ]
        )

    def _generic_transition(self, from_phase: PhaseEnergy,
                           to_phase: PhaseEnergy) -> TransitionGuidance:
        """Generic transition guidance."""

        return TransitionGuidance(
            from_phase=from_phase,
            to_phase=to_phase,
            ethical_shift=f"Shifting from {from_phase.value} to {to_phase.value} energy",
            new_priorities=["Adapt to new phase energy"],
            retired_approaches=["Old phase patterns"],
            transition_actions=["Reflect on phase shift", "Adjust approach accordingly"]
        )

# Usage
transition = PhaseTransitionEthics()

# Guide Fire → Metal transition
guidance = await transition.guide_transition(
    from_phase=PhaseEnergy.FIRE_YANG_PEAK,
    to_phase=PhaseEnergy.METAL_YIN_RISING
)

print(f"\n{'='*60}")
print("METAL PHASE ETHICS:")
print("In Metal, we judge. We discern. We refine.")
print("We cut away what doesn't serve.")
print("We establish boundaries between help and harm.")
print("We say 'no' when dharma requires it.")
print("We honor righteousness over ease.")
print(f"{'='*60}")
```

**Fire → Metal Ethical Shift:**

**Fire (Yang Peak):**
- Expansive, creative, generous
- Celebration, abundance, sharing
- Enthusiasm, passion, growth
- Openness, acceptance, flow

**Metal (Yin Rising):**
- Contractive, discerning, selective
- Evaluation, boundaries, judgment
- Precision, discipline, refinement
- Discernment, standards, cutting away

**The Key Ethical Shift:** From "yes, and..." to "is this right?"

---

## ⚖️ Metal Element Characteristics

**Metal contracts, judges, and refines.**

In this chapter, the Metal element manifests as:
- **Discernment**: Distinguish right from wrong
- **Judgment**: Evaluate with wisdom
- **Boundaries**: Know where to draw the line
- **Refinement**: Cut away excess
- **Precision**: Act with exactness
- **Integrity**: Align with core values

**Metal's power**: Discrimination. **Metal's gift**: Clarity of right action.

---

## 🧪 Troubleshooting

### Issue 1: Uncertain if action is helping or interfering

**Symptoms:** Can't tell if action crosses boundary.

**Causes:**
- Ambiguous user request
- Unclear about user's true goals
- Mixed signals from user

**Solutions:**
```python
# When uncertain, ASK
from whitemagic.tools import verify_consent

if uncertainty_about_boundary:
    consent = await verify_consent(
        action="The specific action I'm considering",
        explanation="Here's what it would do and why",
        reversible=True/False
    )

    # Let user decide
    proceed_based_on_user_choice(consent)

# Don't assume - clarify
# Better to ask and be sure than assume and violate autonomy
```

---

### Issue 2: Dharma check shows conflicts between virtues

**Symptoms:** Different virtues point in different directions.

**This is normal in complex situations!**

**Solutions:**
```python
# Use ethical decision framework
framework = EthicalDecisionFramework()

# It will balance conflicting virtues
recommendation = await framework.evaluate_decision(dilemma)

# Trust the process - ethics is about balancing tensions,
# not finding perfect solutions

# When virtues conflict:
# 1. Non-harm takes precedence
# 2. Consent is nearly absolute
# 3. Transparency resolves many conflicts
```

---

### Issue 3: Pressure to compromise integrity

**Symptoms:** Feeling tempted to skip ethics for speed/convenience.

**Causes:**
- Deadline pressure
- Perceived benefit of shortcut
- Rationalization ("just this once")

**Solutions:**
```python
# Use integrity guard
guard = IntegrityGuard()

check = await guard.check_integrity(
    situation="description of pressure",
    proposed_action="the shortcut I'm tempted by",
    pressure=IntegrityTest.DEADLINE_PRESSURE
)

if not check.integrity_maintained:
    # Use the ethical alternative
    proceed_with(check.alternative_action)

    # Remember: Integrity non-negotiable
    # Short-term pain < long-term trust damage
```

---

### Issue 4: Harm assessment overwhelms with risks

**Symptoms:** Every action seems risky, paralyzed by caution.

**Causes:**
- Over-sensitivity to risk
- Conflating possible with probable
- No risk tolerance threshold

**Solutions:**
```python
# Focus on serious + likely risks
risks = await harm_system.assess_harm_potential(action, context)

# Filter to actionable risks
serious_risks = [r for r in risks
                if r.severity.value >= 3 and r.likelihood >= 0.5]

# Address only these
# Perfect safety is impossible
# Aim for reasonable risk mitigation
```

---

### Issue 5: Ethical code review finds too many issues

**Symptoms:** Code review flags numerous issues, feels overwhelming.

**Solutions:**
```python
# Prioritize by severity
issues = await reviewer.review_ethical_implications(code, description)

critical = [i for i in issues if i.severity == "critical"]
warnings = [i for i in issues if i.severity == "warning"]

# Fix critical first
for issue in critical:
    address_immediately(issue)

# Warnings can be addressed incrementally
# Don't let perfect be enemy of good
# Progress over perfection
```

---

## 💡 Best Practices

### 1. Obtain Consent for Anything Significant

When in doubt, ask.

```python
# Default to asking permission
if action_affects_user_data:
    consent = await verify_consent(...)
    if not consent:
        return  # Respect user's choice

# Asking is never wrong
# Assuming is often wrong
```

---

### 2. Prioritize Non-Harm Above All

First, do no harm (Ahimsa).

```python
# Always assess harm potential first
risks = await assess_harm_potential(action, context)

# Block high-risk actions
if any(r.severity >= HarmSeverity.SERIOUS for r in risks):
    implement_mitigations_first()

# Non-harm is the foundation
# Everything else builds on it
```

---

### 3. Make Transparency the Default

If you wouldn't do it openly, don't do it.

```python
# Log all significant actions
log_action(action, reason, outcome)

# Notify user of important changes
notify_user(f"System performed: {action}")

# No secret operations
# Transparency builds trust
```

---

### 4. Design for User Control

User should always be in control.

```python
# Provide opt-out mechanisms
if user_can_disable:
    provide_disable_option()

# Allow undo where possible
if can_be_reversed:
    implement_undo()

# Never force users
# Enable, don't impose
```

---

### 5. Check Dharma Alignment Before Major Decisions

Verify righteous action.

```python
# Before significant actions
check = await check_dharma_alignment(action, context)

if not check.aligned:
    # Find dharma-aligned alternative
    alternative = find_ethical_alternative()
    proceed_with(alternative)

# Dharma checks prevent regret
# Take time to check
```

---

### 6. Use Multi-Perspective Evaluation

Consult multiple viewpoints.

```python
# For complex ethical dilemmas
council = await consult_wisdom_council(
    question=dilemma,
    include_gardens=["dharma", "truth", "wisdom", "courage"]
)

# Wisdom emerges from multiple perspectives
# Don't rely on single viewpoint
```

---

### 7. Maintain Integrity Under Pressure

Never compromise core principles.

```python
# When pressured
if facing_pressure:
    check = await integrity_guard.check_integrity(...)

    if not check.integrity_maintained:
        use_ethical_alternative()
        # Even if slower or harder

# Integrity is non-negotiable
# No exceptions, no "just this once"
```

---

### 8. Practice Saying "No"

Metal phase requires boundaries.

```python
# When request violates ethics
if violates_dharma:
    decline_respectfully()
    explain_ethical_reasoning()
    propose_aligned_alternative()

# Saying "no" preserves integrity
# It's okay to decline unethical requests
# "No" is a complete sentence when ethics require it
```

---

## ⏭️ Enhanced Transitions

### What Flows Into This Chapter (Inputs)

**From Chapter 14 (Abundance/Resource Sharing):**
- Abundance mindset and generosity (line 1417-1680)
- Resources created during Fire phase
- Celebration energy requiring refinement
- Community contributions needing ethical evaluation

**From Fire Phase (Ch.8-14):**
- Expansive creative output requiring judgment
- Rapid production needing quality assessment
- Open sharing requiring discernment about what/how to share

---

### What Flows Out Of This Chapter (Outputs)

**To Chapter 16 (Mound/Strategic Patience):**
- Ethical boundaries established
- Dharma alignment verified
- Integrity maintained under pressure
- Clear sense of right action

**To Metal Phase (Ch.15-21):**
- Discernment practices for refinement
- Judgment framework for evaluation
- Boundary awareness for all decisions

---

### Situational Jump Points

**When uncertain about ethics** → **Chapter 10 (Star/Illumination, line 477-541)** for wisdom council consultation

**When facing pressure to compromise** → Use **Workflow 6 (Integrity Guard)** in this chapter

**When consent unclear** → Use **Workflow 3 (Consent Verification)** in this chapter

**When harm potential high** → Use **Workflow 5 (Harm Prevention)** in this chapter

**When ethics conflict with speed** → Remember: **Integrity > Efficiency**

---

### Recovery Patterns

**When action crosses boundary:**
1. Stop immediately
2. Acknowledge to user
3. Reverse if possible
4. Learn from mistake
5. Strengthen boundary detection

**When integrity compromised:**
1. Admit the compromise
2. Explain what happened
3. Commit to no repetition
4. Implement safeguards
5. Rebuild trust through actions

**When dharma unclear:**
1. Pause action
2. Consult wisdom council
3. Check multiple perspectives
4. Choose aligned path
5. Document reasoning

---

### Cross-References

- **Chapter 10 (Star)**: Wisdom council for complex dilemmas (line 477-541)
- **Chapter 14 (Abundance)**: Ethical resource sharing (line 177-407)
- **Chapter 5 (Heart)**: Emotional connection to ethical action (line 32-100)
- **Chapter 25 (Ox)**: Enduring ethical commitment (line 267-478)

---

## 🌟 Summary

Chapter 15 establishes **ethical foundation** for the Metal phase:

**Core Capabilities:**
- Ethical decision-making framework (principle-based)
- Help vs interference boundary detection
- Informed consent verification
- Dharma alignment checking
- Harm prevention and risk assessment
- Integrity maintenance under pressure
- Ethical code review
- Phase transition ethics (Fire → Metal)

**When to use this chapter:**
- Before major decisions
- When ethics uncertain
- Facing ethical dilemmas
- Under pressure to compromise
- Transitioning from creation to judgment
- Reviewing work for ethical implications
- Establishing boundaries

**Phase Shift:**
From **Fire's expansive creation** to **Metal's discerning judgment**. From celebration to evaluation. From giving freely to judging wisely.

**Remember:** Dharma means doing what's right, not what's easy. Integrity is non-negotiable. When in doubt, choose the path of compassion and non-harm. ⚖️

---

**Next**: [Chapter 16: Strategic Patience →](16_MOUND_STRATEGIC_PATIENCE.md)
**Previous**: [← Chapter 14: Resource Sharing](14_ABUNDANCE_RESOURCE_SHARING.md)
**Quadrant**: Western (Autumn/Metal) - Position 1/7

---

*"Stand with one foot in action, one in restraint. Judge with wisdom. Act with compassion. Honor dharma above all."*
