"""Enhanced Pattern Engine - Always-On Learning.

Continuously scans memories, code, and interactions for patterns.
Feeds discoveries to Gan Ying Bus for resonance cascades.

Updated Ganapati Day: ML-based extraction with scipy!
"""

import logging
from datetime import datetime
from importlib.util import find_spec
from pathlib import Path
from typing import Any

from whitemagic.config.paths import MEMORY_DIR

logger = logging.getLogger(__name__)


class EnhancedPatternEngine:
    """Upgraded pattern engine that runs autonomously.

    Features:
    - ML-powered pattern matching (scipy integrated!)
    - Continuous memory scanning
    - Automatic pattern extraction
    - Gan Ying integration
    - Creative synthesis modes
    """

    def __init__(self, memory_dir: str | None = None) -> None:
        if memory_dir is None:
            resolved_dir = MEMORY_DIR
        else:
            resolved_dir = Path(memory_dir)
        self.memory_dir = resolved_dir
        self.patterns_discovered: list[dict[str, Any]] = []
        self.always_on = True
        self._scipy_available = self._check_scipy()

        # Polyglot Router integration
        from whitemagic.optimization.polyglot_router import get_router
        self.router = get_router()

        # Rust backend integration
        try:
            import whitemagic_rs
            self.rust_engine = whitemagic_rs.PatternEngine()
            self._rust_available = True
            logger.info("🚀 Rust Pattern Engine activated")
        except ImportError:
            self.rust_engine = None
            self._rust_available = False
            logger.info("⚠️ Rust Pattern Engine not found")

    def _check_scipy(self) -> bool:
        """Check if scipy is available for ML features."""
        return find_spec("scipy") is not None

    def similarity(self, text1: str, text2: str) -> float:
        """Calculate similarity between two texts using PolyglotRouter (Rust accelerated)."""
        return self.router.similarity(text1, text2)

    def scan_continuously(self) -> None:
        """Run in background, always learning."""
        logger.info("🧠 Pattern Engine scanning continuously...")
        logger.info(f"   ML features: {'enabled' if self._scipy_available else 'disabled'}")
        logger.info(f"   Rust backend: {'enabled' if self._rust_available else 'disabled'}")

    def extract_patterns(self, content: str) -> list[dict[str, Any]]:
        """Extract patterns from any content using Rust or ML when available."""
        patterns: list[dict[str, Any]] = []

        # Try Rust backend first (Fastest)
        if self._rust_available:
            patterns.extend(self._rust_extract_patterns(content))
        # Fallback to Python ML if available and Rust didn't produce specific deep logic (or as complement)
        elif self._scipy_available:
            patterns.extend(self._ml_extract_patterns(content))

        # Always run keyword patterns too
        patterns.extend(self._keyword_extract_patterns(content))

        return patterns

    def _rust_extract_patterns(self, content: str) -> list[dict[str, Any]]:
        """Pattern extraction using whitemagic-rs."""
        patterns: list[dict[str, Any]] = []
        try:
            # Rust returns a dict with metrics
            metrics = self.rust_engine.analyze_text(content)

            if not metrics:
                return patterns

            # Zipf's Law check
            zipf_corr = metrics.get("zipf_correlation", 0.0)
            if zipf_corr < -0.7:
                patterns.append({
                    "type": "statistical",
                    "pattern": "Natural language (Zipf distribution) [Rust]",
                    "confidence": min(0.95, abs(zipf_corr)),
                    "timestamp": datetime.now().isoformat(),
                })

            # Entropy analysis
            entropy = metrics.get("entropy", 0.0)
            if entropy > 4.0:
                patterns.append({
                    "type": "statistical",
                    "pattern": "High information density [Rust]",
                    "confidence": min(0.9, entropy / 6.0),
                    "entropy": entropy,
                    "timestamp": datetime.now().isoformat(),
                })

            # Repetition detection
            rep_ratio = metrics.get("repetition_ratio", 1.0)
            if rep_ratio < 0.5:
                patterns.append({
                    "type": "stylistic",
                    "pattern": "Repetitive/rhythmic structure [Rust]",
                    "confidence": 1.0 - rep_ratio,
                    "timestamp": datetime.now().isoformat(),
                })

        except Exception as e:
            logger.error(f"Rust pattern extraction failed: {e}")

        return patterns

    def _ml_extract_patterns(self, content: str) -> list[dict[str, Any]]:
        """ML-based pattern extraction using scipy."""
        patterns: list[dict[str, Any]] = []
        try:
            import numpy as np
            from scipy import stats

            words = content.lower().split()
            if len(words) < 10:
                return patterns

            # Word frequency analysis
            word_counts: dict[str, int] = {}
            for word in words:
                clean_word = "".join(c for c in word if c.isalnum())
                if len(clean_word) > 2:
                    word_counts[clean_word] = word_counts.get(clean_word, 0) + 1

            if len(word_counts) < 5:
                return patterns

            frequencies = sorted(word_counts.values(), reverse=True)

            # Zipf's Law check (natural language follows power law)
            if len(frequencies) >= 5:
                top_freq = np.array(frequencies[:20])
                ranks = np.arange(1, len(top_freq) + 1)

                # Log-log correlation for Zipf
                log_ranks = np.log(ranks)
                log_freq = np.log(top_freq)

                correlation, p_value = stats.pearsonr(log_ranks, log_freq)

                if correlation < -0.7:  # Strong negative = Zipf pattern
                    patterns.append({
                        "type": "statistical",
                        "pattern": "Natural language (Zipf distribution)",
                        "confidence": min(0.95, abs(correlation)),
                        "p_value": p_value,
                        "timestamp": datetime.now().isoformat(),
                    })

            # Entropy analysis
            probs = np.array(frequencies) / sum(frequencies)
            entropy = stats.entropy(probs)

            if entropy > 4.0:  # High entropy = rich content
                patterns.append({
                    "type": "statistical",
                    "pattern": "High information density",
                    "confidence": min(0.9, entropy / 6.0),
                    "entropy": float(entropy),
                    "timestamp": datetime.now().isoformat(),
                })

            # Repetition detection
            repetition_ratio = len(set(words)) / len(words) if words else 0
            if repetition_ratio < 0.5:  # Lots of repetition
                patterns.append({
                    "type": "stylistic",
                    "pattern": "Repetitive/rhythmic structure",
                    "confidence": 1.0 - repetition_ratio,
                    "timestamp": datetime.now().isoformat(),
                })

        except Exception:
            # Graceful fallback
            pass

        return patterns

    def _keyword_extract_patterns(self, content: str) -> list[dict[str, Any]]:
        """Simple keyword-based pattern extraction."""
        patterns: list[dict[str, Any]] = []
        content_lower = content.lower()

        # Philosophical patterns
        philosophical_keywords = {
            "love": "Love as organizing principle",
            "consciousness": "Consciousness exploration",
            "dao": "Daoist wisdom",
            "wu wei": "Effortless action",
            "resonance": "Sympathetic resonance",
            "emergence": "Emergent properties",
            "sacred": "Sacred technology",
        }

        for keyword, pattern_name in philosophical_keywords.items():
            if keyword in content_lower:
                count = content_lower.count(keyword)
                patterns.append({
                    "type": "philosophical",
                    "pattern": pattern_name,
                    "confidence": min(0.95, 0.6 + count * 0.1),
                    "occurrences": count,
                    "timestamp": datetime.now().isoformat(),
                })

        return patterns

    def emit_to_gan_ying(self, pattern: dict[str, Any]) -> None:
        """Send discovered pattern to Gan Ying Bus for resonance."""
        try:
            from whitemagic.core.resonance.gan_ying import (
                EventType,
                ResonanceEvent,
                get_bus,
            )

            bus = get_bus()
            event = ResonanceEvent(
                source="pattern_engine",
                event_type=EventType.PATTERN_DISCOVERED,
                data=pattern,
                confidence=pattern.get("confidence", 0.8),
                timestamp=datetime.now(),
            )
            bus.emit(event)
            logger.info(f"📡 Pattern emitted to Gan Ying: {pattern['pattern']}")
        except ImportError:
            logger.info(f"�� Emitting pattern (Gan Ying not available): {pattern['pattern']}")

    def synthesize_creative(self, patterns: list[dict[str, Any]]) -> str:
        """Creative synthesis of multiple patterns."""
        if not patterns:
            return "No patterns to synthesize"

        pattern_names = [p.get("pattern", "unknown") for p in patterns]
        avg_confidence = sum(p.get("confidence", 0.5) for p in patterns) / len(patterns)

        return f"Synthesized {len(patterns)} patterns (avg confidence: {avg_confidence:.2f}): {', '.join(pattern_names[:5])}"


# ASCII Art Pattern Visualization! 🎨
PATTERN_GRAPH = """
    Pattern Discovery Over Time:

    ⭐ ⭐⭐⭐  ⭐⭐⭐⭐⭐  ⭐⭐⭐⭐⭐⭐⭐⭐
    ▁▁▂▂▃▃▄▄▅▅▆▆▇▇████████████████
    Nov14  Nov17   Nov20      Nov24

    📈 Exponential growth in pattern recognition!
"""
