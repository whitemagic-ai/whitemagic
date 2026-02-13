"""Tests for the entropy & abstraction scorer."""

import pytest

from whitemagic.core.memory.entropy_scorer import (
    EntropyResult,
    EntropyScorer,
    EntropySweepReport,
)


@pytest.fixture
def scorer():
    return EntropyScorer()


class TestEntropyResult:
    def test_composite_blend(self):
        r = EntropyResult(entropy=1.0, abstraction=1.0)
        assert r.composite == 1.0

        r2 = EntropyResult(entropy=0.0, abstraction=0.0)
        assert r2.composite == 0.0

        r3 = EntropyResult(entropy=0.8, abstraction=0.5)
        # 0.6*0.8 + 0.4*0.5 = 0.48 + 0.20 = 0.68
        assert abs(r3.composite - 0.68) < 0.01

    def test_to_dict(self):
        r = EntropyResult(entropy=0.75, abstraction=0.6, word_count=100)
        d = r.to_dict()
        assert "entropy" in d
        assert "abstraction" in d
        assert "composite" in d
        assert d["word_count"] == 100


class TestEntropyScorer:
    def test_empty_text(self, scorer):
        result = scorer.score("")
        assert result.entropy == 0.5
        assert result.word_count == 0

    def test_very_short_text(self, scorer):
        result = scorer.score("hi")
        assert result.word_count <= 2

    def test_repetitive_text_low_entropy(self, scorer):
        text = "the the the the the the the the the the the the the the the the the the the the"
        result = scorer.score(text)
        assert result.entropy < 0.3, f"Repetitive text should have low entropy, got {result.entropy}"

    def test_diverse_text_high_entropy(self, scorer):
        text = (
            "WhiteMagic provides persistent memory associative recall ethical governance "
            "self-awareness sessions polyglot acceleration cognitive operating system "
            "tools organized ganas lunar mansions chinese astronomy covering domains "
            "intelligence infrastructure community special introspection"
        )
        result = scorer.score(text)
        assert result.entropy > 0.6, f"Diverse text should have high entropy, got {result.entropy}"

    def test_concrete_text_low_abstraction(self, scorer):
        text = (
            "File /opt/project/whitemagic/src/main.py was modified on 2026-02-10. "
            "Version 14.1.0 updated. URL https://example.com/api deployed. "
            "Hash 0xABCDEF1234 committed at line 42."
        )
        result = scorer.score(text)
        assert result.concrete_markers > 0
        assert result.abstraction < 0.5, f"Concrete text should have low abstraction, got {result.abstraction}"

    def test_abstract_text_high_abstraction(self, scorer):
        text = (
            "The fundamental principle of this architecture is that every system "
            "should provide governance through a framework of rules and constraints. "
            "The design philosophy enables integration and synthesis across boundaries, "
            "ensuring scalability and resilience through universal patterns and structures."
        )
        result = scorer.score(text)
        assert result.abstract_markers > 0
        assert result.abstraction > 0.4, f"Abstract text should have higher abstraction, got {result.abstraction}"

    def test_vocab_richness(self, scorer):
        # All unique words
        unique = "alpha bravo charlie delta echo foxtrot golf hotel india juliet kilo lima mike november oscar papa quebec romeo sierra tango"
        result = scorer.score(unique)
        assert result.vocab_richness > 0.8

    def test_score_increments_counter(self, scorer):
        assert scorer._total_scored == 0
        scorer.score("hello world this is a test")
        assert scorer._total_scored == 1

    def test_get_stats(self, scorer):
        scorer.score("test content here")
        stats = scorer.get_stats()
        assert stats["total_scored"] == 1


class TestRetentionEvaluator:
    def test_as_retention_evaluator(self, scorer):
        evaluator = scorer.as_retention_evaluator()
        assert callable(evaluator)

    def test_evaluator_returns_signal(self, scorer):
        from whitemagic.core.memory.unified_types import Memory, MemoryType

        evaluator = scorer.as_retention_evaluator()
        mem = Memory(
            id="test-1",
            content="A comprehensive guide to understanding system architecture and design patterns",
            memory_type=MemoryType.LONG_TERM,
            title="[GUIDE] Architecture Overview",
        )
        signal = evaluator(mem)
        assert signal.name == "entropy"
        assert 0.0 <= signal.score <= 1.0
        assert signal.weight == 0.15


class TestEntropySweepReport:
    def test_to_dict(self):
        report = EntropySweepReport(total_scored=10, avg_entropy=0.75)
        d = report.to_dict()
        assert d["total_scored"] == 10
        assert d["avg_entropy"] == 0.75


class TestSingleton:
    def test_get_entropy_scorer(self):
        from whitemagic.core.memory.entropy_scorer import get_entropy_scorer
        s1 = get_entropy_scorer()
        s2 = get_entropy_scorer()
        assert s1 is s2
