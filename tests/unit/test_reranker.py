"""Tests for cross-encoder reranking module."""


class TestLexicalRerank:
    """Test the BM25-style lexical reranker fallback."""

    def test_basic_rerank(self):
        from whitemagic.core.intelligence.reranker import lexical_rerank

        results = [
            {"id": "1", "title": "Python programming", "content": "Learn Python basics", "score": 0.5},
            {"id": "2", "title": "Java guide", "content": "Java enterprise patterns", "score": 0.8},
            {"id": "3", "title": "Python advanced", "content": "Advanced Python techniques and patterns", "score": 0.3},
        ]
        ranked = lexical_rerank("Python patterns", results, top_k=3)
        assert len(ranked) == 3
        # Python-related results should rank higher
        ids = [r.memory_id for r in ranked]
        assert ids[0] in ("1", "3")  # Python results should be on top

    def test_empty_results(self):
        from whitemagic.core.intelligence.reranker import lexical_rerank

        ranked = lexical_rerank("test", [], top_k=5)
        assert ranked == []

    def test_empty_query(self):
        from whitemagic.core.intelligence.reranker import lexical_rerank

        results = [
            {"id": "1", "title": "Test", "content": "Content", "score": 0.5},
        ]
        ranked = lexical_rerank("", results, top_k=5)
        assert len(ranked) == 1

    def test_top_k_limit(self):
        from whitemagic.core.intelligence.reranker import lexical_rerank

        results = [{"id": str(i), "title": f"Title {i}", "content": f"Content {i}", "score": 0.5} for i in range(10)]
        ranked = lexical_rerank("content", results, top_k=3)
        assert len(ranked) == 3

    def test_ranked_result_combined_score(self):
        from whitemagic.core.intelligence.reranker import RankedResult

        r = RankedResult(memory_id="1", title="T", content="C", original_score=1.0, rerank_score=1.0)
        assert r.combined_score == 1.0

        r2 = RankedResult(memory_id="2", title="T", content="C", original_score=0.0, rerank_score=0.0)
        assert r2.combined_score == 0.0


class TestRerank:
    """Test the auto-routing rerank function."""

    def test_auto_strategy_defaults_to_lexical(self):
        from whitemagic.core.intelligence.reranker import rerank

        results = [
            {"id": "1", "title": "Test", "content": "Hello world", "score": 0.5},
        ]
        ranked = rerank("hello", results, strategy="lexical")
        assert len(ranked) == 1
        assert ranked[0].memory_id == "1"

    def test_get_status(self):
        from whitemagic.core.intelligence.reranker import get_status

        status = get_status()
        assert "cross_encoder_available" in status
        assert "fallback" in status
        assert status["fallback"] == "lexical_bm25"
