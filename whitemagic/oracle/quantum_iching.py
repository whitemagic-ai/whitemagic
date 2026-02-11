"""Quantum I Ching - Quantum-inspired decision system for WhiteMagic"""

import hashlib
import math
import random
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from functools import lru_cache
from typing import Any

try:
    import numpy as np
    NUMPY_AVAILABLE = True
except ImportError:
    NUMPY_AVAILABLE = False

try:
    from whitemagic_rs import RUST_AVAILABLE
except ImportError:
    RUST_AVAILABLE = False


class HexagramType(Enum):
    """Types of hexagram changes"""

    STATIC = "static"          # No changing lines
    DYNAMIC = "dynamic"        # Changing lines present
    TRANSFORMING = "transforming"  # Full transformation possible


@dataclass
class QuantumState:
    """Represents a quantum superposition of hexagrams"""

    amplitudes: dict[int, complex]  # hexagram_number -> amplitude
    entanglement: list[tuple[int, int]]  # Entangled hexagram pairs
    coherence: float = 1.0
    collapse_threshold: float = 0.1


@dataclass
class ConsultationResult:
    """Result of an I Ching consultation"""

    primary_hexagram: int
    primary_name: str
    primary_judgment: str
    primary_image: str

    changing_lines: list[int]
    transformed_hexagram: int | None
    transformed_name: str | None
    transformed_judgment: str | None

    quantum_signature: str
    resonance_score: float
    wisdom: str
    guidance: str

    context_analysis: dict[str, Any]
    timestamp: datetime
    quantum_state: QuantumState | None = field(default=None)


class HexagramDatabase:
    """Complete I Ching hexagram database"""

    def __init__(self) -> None:
        # All 64 hexagrams with their properties
        from .hexagram_data import COMPLETE_HEXAGRAMS

        self.hexagrams: dict[int, dict[str, Any]] = {}
        for number, name, chinese, judgment, image, symbol, guidance in COMPLETE_HEXAGRAMS:
            normalized_name = name.replace("The ", "").strip()
            keywords = [normalized_name.split()[0].lower()] if normalized_name else []
            self.hexagrams[number] = {
                "name": name,
                "chinese": chinese,
                "judgment": judgment,
                "image": image,
                "symbol": symbol,
                "guidance": guidance,
                "keywords": keywords,
            }

        # Trigram relationships
        self.trigrams = {
            0: {"name": "Earth", "element": "earth", "direction": "SW", "family": "mother"},
            1: {"name": "Thunder", "element": "wood", "direction": "E", "family": "eldest son"},
            2: {"name": "Water", "element": "water", "direction": "N", "family": "middle son"},
            3: {"name": "Mountain", "element": "earth", "direction": "NE", "family": "youngest son"},
            4: {"name": "Wind", "element": "wood", "direction": "SE", "family": "eldest daughter"},
            5: {"name": "Fire", "element": "fire", "direction": "S", "family": "middle daughter"},
            6: {"name": "Lake", "element": "water", "direction": "W", "family": "youngest daughter"},
            7: {"name": "Heaven", "element": "heaven", "direction": "NW", "family": "father"},
        }

    @lru_cache(maxsize=128)
    def get_hexagram(self, number: int) -> dict[str, Any] | None:
        """Get hexagram by number"""
        return self.hexagrams.get(number)

    @lru_cache(maxsize=128)
    def get_hexagram_by_keyword(self, keyword: str) -> int | None:
        """Find hexagram by keyword"""
        for num, hex_data in self.hexagrams.items():
            if keyword.lower() in [k.lower() for k in hex_data["keywords"]]:
                return num
        return None


class QuantumIChing:
    """Quantum-inspired I Ching consultation system"""

    def __init__(self) -> None:
        self.db = HexagramDatabase()
        self.consultation_history: list[ConsultationResult] = []
        self.quantum_cache: dict[str, Any] = {}

        # Quantum parameters
        self.coherence_decay = 0.95
        self.entanglement_strength = 0.7
        self.superposition_capacity = 64  # All hexagrams

    # Cache removed to allow quantum fluctuations
    def consult(self, question: str, context: dict[str, Any] | None = None) -> ConsultationResult:
        """Perform a quantum I Ching consultation"""
        context_data = context or {}

        # Create quantum superposition based on question
        quantum_state = self._create_quantum_state(question, context_data)

        # Collapse to primary hexagram
        primary_hexagram = self._collapse_quantum_state(quantum_state, question)

        # Generate changing lines
        changing_lines = self._generate_changing_lines(quantum_state, primary_hexagram)

        # Calculate transformed hexagram if needed
        transformed_hexagram = None
        if changing_lines:
            transformed_hexagram = self._calculate_transformed_hexagram(primary_hexagram, changing_lines)

        # Get hexagram data
        primary_data = self._get_hexagram_or_default(primary_hexagram)
        transformed_data = self.db.get_hexagram(transformed_hexagram) if transformed_hexagram else None

        # Generate wisdom
        wisdom = self._generate_wisdom(primary_hexagram, changing_lines, question, context_data)

        # Generate guidance
        guidance = self._generate_guidance(primary_hexagram, transformed_hexagram, question)

        # Calculate resonance
        resonance = self._calculate_resonance(quantum_state, primary_hexagram)

        # Create result
        result = ConsultationResult(
            primary_hexagram=primary_hexagram,
            primary_name=primary_data["name"],
            primary_judgment=primary_data["judgment"],
            primary_image=primary_data["image"],
            changing_lines=changing_lines,
            transformed_hexagram=transformed_hexagram,
            transformed_name=transformed_data["name"] if transformed_data else None,
            transformed_judgment=transformed_data["judgment"] if transformed_data else None,
            quantum_signature=self._generate_quantum_signature(quantum_state),
            resonance_score=resonance,
            wisdom=wisdom,
            guidance=guidance,
            context_analysis=self._analyze_context(question, context_data),
            timestamp=datetime.now(),
            quantum_state=quantum_state,
        )

        # Store in history
        self.consultation_history.append(result)

        return result

    def _get_hexagram_or_default(self, hexagram: int) -> dict[str, Any]:
        """Return hexagram data with a typed fallback for unknown ids."""
        hex_data = self.db.get_hexagram(hexagram)
        if hex_data is not None:
            return hex_data
        return {
            "name": f"Hexagram {hexagram}",
            "chinese": "Unknown",
            "judgment": "The oracle is unclear; sit with the question longer.",
            "image": "Unformed energy seeks patient observation.",
            "symbol": "unknown",
            "guidance": "Pause, reflect, and ask again with greater clarity.",
            "keywords": ["unknown"],
        }

    def _create_quantum_state(self, question: str, context: dict[str, Any]) -> QuantumState:
        """Create quantum superposition based on question and context"""
        # Hash the question to seed quantum state
        question_hash = hashlib.sha256(question.encode()).hexdigest()
        seed = int(question_hash[:8], 16)
        random.seed(seed)

        # Initialize amplitudes
        amplitudes = {}

        # Base amplitude distribution
        for i in range(1, 65):
            # Quantum amplitude based on question resonance
            amplitude = complex(
                random.gauss(0, 0.5),
                random.gauss(0, 0.5),
            )
            amplitudes[i] = amplitude

        # Contextual modulation
        if context:
            for key, value in context.items():
                # Modulate amplitudes based on context
                modulation = self._calculate_context_modulation(key, value)
                for i in amplitudes:
                    amplitudes[i] *= modulation

        # Normalize amplitudes
        total = sum(abs(a)**2 for a in amplitudes.values())
        for i in amplitudes:
            amplitudes[i] = amplitudes[i] / math.sqrt(total)

        # Create entanglements
        entanglements: list[tuple[int, int]] = []
        if random.random() < self.entanglement_strength:
            # Create entangled pairs
            for _ in range(random.randint(1, 3)):
                pair = random.sample(range(1, 65), 2)
                entanglements.append((pair[0], pair[1]))

        return QuantumState(
            amplitudes=amplitudes,
            entanglement=entanglements,
            coherence=random.uniform(0.7, 1.0),
            collapse_threshold=0.1,
        )

    def _collapse_quantum_state(self, state: QuantumState, question: str) -> int:
        """Collapse quantum state to a specific hexagram"""
        # Calculate probabilities
        probabilities = {}
        for hex_num, amplitude in state.amplitudes.items():
            probabilities[hex_num] = abs(amplitude) ** 2

        # Apply question resonance
        keywords = self._extract_keywords(question)
        for keyword in keywords:
            keyword_hex_num = self.db.get_hexagram_by_keyword(keyword)
            if keyword_hex_num and keyword_hex_num in probabilities:
                # Boost probability for matching hexagrams
                probabilities[keyword_hex_num] *= 1.5

        # Renormalize
        total = sum(probabilities.values())
        for hex_num in probabilities:
            probabilities[hex_num] /= total

        # Collapse based on probabilities
        if NUMPY_AVAILABLE:
            hex_numbers = list(probabilities.keys())
            probs = list(probabilities.values())
            selected = int(np.random.choice(hex_numbers, p=probs))
        else:
            # Fallback to weighted random
            r = random.random()
            cumulative = 0.0
            selected = 1  # Default
            for hex_num, prob in sorted(probabilities.items()):
                cumulative += prob
                if r <= cumulative:
                    selected = hex_num
                    break

        # Ensure we have a valid hexagram from our database
        return selected if selected in self.db.hexagrams else 1

    def _generate_changing_lines(self, state: QuantumState, hexagram: int) -> list[int]:
        """Generate changing lines based on quantum state"""
        changing_lines = []

        # Each line has a probability of changing based on quantum coherence
        for line in range(1, 7):
            # Calculate line change probability
            base_prob = 0.25  # 1 in 4 chance
            coherence_factor = state.coherence

            # Quantum fluctuation
            fluctuation = random.gauss(0, 0.1)

            change_prob = base_prob * coherence_factor + fluctuation

            if random.random() < change_prob:
                changing_lines.append(line)

        return sorted(changing_lines)

    def _calculate_transformed_hexagram(self, primary: int, changing_lines: list[int]) -> int:
        """Calculate the transformed hexagram from changing lines"""
        # Convert primary hexagram to binary representation
        primary_binary = self._hexagram_to_binary(primary)

        # Flip changing lines
        transformed_binary = list(primary_binary)
        for line in changing_lines:
            # Lines are numbered from bottom (1) to top (6)
            index = 6 - line
            transformed_binary[index] = "1" if transformed_binary[index] == "0" else "0"

        # Convert back to hexagram number
        return self._binary_to_hexagram("".join(transformed_binary))

    def _hexagram_to_binary(self, hexagram: int) -> str:
        """Convert hexagram number to 6-bit binary"""
        # Simplified conversion - in real I Ching, this follows trigram patterns
        binary = format(hexagram - 1, "06b")
        return binary

    def _binary_to_hexagram(self, binary: str) -> int:
        """Convert 6-bit binary to hexagram number"""
        return int(int(binary, 2) + 1)

    def _generate_wisdom(self, hexagram: int, changing_lines: list[int],
                        question: str, context: dict[str, Any] | None) -> str:
        """Generate wisdom based on hexagram and context using Local Brain"""
        hex_data = self._get_hexagram_or_default(hexagram)

        # Try to use the Neural Brain (Llama 3)
        try:
            from .. import brain

            prompt = f"""
            You are the I Ching Sage. Interpret the following reading for the user's question.

            Question: "{question}"
            Context: {context}

            Hexagram #{hexagram}: {hex_data['name']} ({hex_data['chinese']})
            Judgment: {hex_data['judgment']}
            Image: {hex_data['image']}
            Changing Lines: {changing_lines}

            Provide a profound, mystical, yet practical interpretation (max 3 sentences).
            """

            response = brain.generate(prompt)
            response_text = response.get("response")
            if isinstance(response_text, str):
                return response_text.strip()

        except ImportError:
            pass # Fallback to template
        except Exception as e:
            print(f"⚠️ Brain disconnect: {e}")

        # Fallback Template
        wisdom_templates = [
            f"The wisdom of {hex_data['name']} suggests: {hex_data['judgment'].lower()}",
            f"In the context of your question about {question[:50]}..., {hex_data['image'].lower()}",
            f"The ancient oracle speaks through {hex_data['chinese']}: {hex_data['judgment']}",
        ]

        base_wisdom = random.choice(wisdom_templates)

        # Add context-specific insight
        if changing_lines:
            base_wisdom += f"\n\nThe changing lines ({', '.join(map(str, changing_lines))}) indicate transformation is possible."

        return base_wisdom

    def _generate_guidance(self, primary: int, transformed: int | None, question: str) -> str:
        """Generate practical guidance"""
        primary_data = self._get_hexagram_or_default(primary)

        guidance = "\n🎯 Practical Guidance:\n"
        primary_keywords = primary_data.get("keywords") if primary_data else None
        primary_focus = primary_keywords[0] if primary_keywords else (primary_data.get("name") if primary_data else "unknown")
        guidance += f"• Primary focus: {primary_focus}\n"

        if transformed:
            transformed_data = self.db.get_hexagram(transformed)
            if transformed_data:
                guidance += f"• Potential outcome: {transformed_data['keywords'][0]}\n"
                guidance += f"• Transformation path: {primary_data['name']} → {transformed_data['name']}\n"
            else:
                guidance += f"• Potential outcome: Hexagram {transformed}\n"

        # Add specific guidance based on question type
        if "should" in question.lower():
            guidance += "• Trust your inner knowing while considering this wisdom\n"
        elif "why" in question.lower():
            guidance += "• Reflect on the deeper patterns at play\n"
        elif "how" in question.lower():
            guidance += "• Proceed with the qualities of this hexagram\n"

        return guidance

    def _calculate_resonance(self, state: QuantumState, hexagram: int) -> float:
        """Calculate resonance score between quantum state and hexagram"""
        amplitude = state.amplitudes.get(hexagram, complex(0, 0))
        base_resonance = abs(amplitude) ** 2

        # Apply coherence factor
        coherence_resonance = base_resonance * state.coherence

        # Add entanglement effects
        entanglement_bonus = 0.0
        for pair in state.entanglement:
            if hexagram in pair:
                entanglement_bonus += 0.1

        return min(1.0, coherence_resonance + entanglement_bonus)

    def _generate_quantum_signature(self, state: QuantumState) -> str:
        """Generate unique quantum signature"""
        # Create hash from quantum state
        signature_data = f"{len(state.amplitudes)}:{state.coherence:.3f}:{len(state.entanglement)}"
        return hashlib.md5(signature_data.encode()).hexdigest()[:12].upper()

    def _analyze_context(self, question: str, context: dict[str, Any] | None) -> dict[str, Any]:
        """Analyze the context of the consultation"""
        analysis = {
            "question_length": len(question),
            "question_type": self._classify_question(question),
            "emotional_tone": self._detect_emotional_tone(question),
            "complexity": "simple" if len(question.split()) < 10 else "complex",
        }

        if context:
            analysis["context_keys"] = list(context.keys())
            analysis["has_urgency"] = context.get("urgency", False)
            analysis["requires_depth"] = context.get("depth", "") == "required"

        return analysis

    def _extract_keywords(self, text: str) -> list[str]:
        """Extract keywords from text"""
        # Simple keyword extraction
        words = text.lower().split()
        keywords = [w for w in words if len(w) > 3]
        return keywords[:5]  # Return top 5

    def _calculate_context_modulation(self, key: str, value: Any) -> float:
        """Calculate quantum modulation based on context"""
        modulations = {
            "urgency": 1.2,
            "depth": 1.1,
            "sacred_space": 1.3,
            "meditation": 1.15,
            "ceremony": 1.25,
        }

        return modulations.get(key, 1.0)

    def _classify_question(self, question: str) -> str:
        """Classify the type of question"""
        question_lower = question.lower()

        if any(word in question_lower for word in ["should", "recommend", "suggest"]):
            return "decision"
        if any(word in question_lower for word in ["why", "reason", "cause"]):
            return "understanding"
        if any(word in question_lower for word in ["how", "way", "method"]):
            return "process"
        if any(word in question_lower for word in ["what", "which", "who"]):
            return "information"
        return "general"

    def _detect_emotional_tone(self, text: str) -> str:
        """Detect emotional tone of text"""
        positive_words = ["happy", "joy", "love", "success", "good", "wonderful"]
        negative_words = ["sad", "angry", "fear", "worry", "bad", "terrible"]

        text_lower = text.lower()

        if any(word in text_lower for word in positive_words):
            return "positive"
        if any(word in text_lower for word in negative_words):
            return "negative"
        return "neutral"

    def get_consultation_history(self, limit: int = 10) -> list[dict[str, Any]]:
        """Get recent consultation history"""
        recent = self.consultation_history[-limit:]
        return [
            {
                "timestamp": c.timestamp.isoformat(),
                "question": c.context_analysis.get("question_length", 0),
                "primary_hexagram": c.primary_hexagram,
                "primary_name": c.primary_name,
                "resonance": c.resonance_score,
                "quantum_signature": c.quantum_signature,
            }
            for c in recent
        ]

    def get_statistics(self) -> dict[str, Any]:
        """Get consultation statistics"""
        if not self.consultation_history:
            return {"total_consultations": 0}

        total = len(self.consultation_history)
        hexagram_counts: dict[int, int] = {}
        total_resonance = 0.0

        for c in self.consultation_history:
            hexagram_counts[c.primary_hexagram] = hexagram_counts.get(c.primary_hexagram, 0) + 1
            total_resonance += c.resonance_score

        # Most common hexagrams
        most_common = sorted(hexagram_counts.items(), key=lambda x: x[1], reverse=True)[:5]

        return {
            "total_consultations": total,
            "average_resonance": total_resonance / total,
            "most_common_hexagrams": most_common,
            "unique_hexagrams": len(hexagram_counts),
        }


# CLI interface
def quantum_iching_cli(question: str, context: str = "") -> Any:
    """Command line interface for Quantum I Ching"""
    oracle = QuantumIChing()

    context_dict = {}
    if context:
        for pair in context.split(","):
            if "=" in pair:
                key, value = pair.split("=", 1)
                context_dict[key.strip()] = value.strip()

    result = oracle.consult(question, context_dict)

    print("\n" + "=" * 60)
    print("🔮 QUANTUM I CHING CONSULTATION 🔮")
    print("=" * 60)
    print(f"\nQuestion: {question}")
    print(f"Quantum Signature: {result.quantum_signature}")
    print(f"Resonance: {result.resonance_score:.2f}")
    print(f"Timestamp: {result.timestamp.strftime('%Y-%m-%d %H:%M:%S')}")

    print("\n" + "─" * 40)
    print(f"PRIMARY HEXAGRAM: #{result.primary_hexagram} - {result.primary_name}")
    print("─" * 40)
    print(f"\nJudgment: {result.primary_judgment}")
    print(f"\nImage: {result.primary_image}")

    if result.changing_lines:
        print(f"\nChanging Lines: {', '.join(map(str, result.changing_lines))}")
        if result.transformed_hexagram:
            print(f"\nTRANSFORMED HEXAGRAM: #{result.transformed_hexagram} - {result.transformed_name}")
            print(f"Transformed Judgment: {result.transformed_judgment}")

    print("\n" + "─" * 40)
    print("WISDOM")
    print("─" * 40)
    print(result.wisdom)

    print(result.guidance)

    print("\n" + "=" * 60)


if __name__ == "__main__":
    # Example consultations
    quantum_iching_cli("Should I pursue this new opportunity?", "urgency=high")
    print("\n" + "\n" * 3)
    quantum_iching_cli("Why am I facing these challenges?", "depth=required")
    print("\n" + "\n" * 3)
    quantum_iching_cli("How can I find balance in my life?", "meditation=true")
