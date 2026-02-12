"""Tests for the web research engine (web_fetch, web_search, research_topic).

Tests are split into:
- Unit tests (no network, using mocks)
- Integration tests (live network, marked with @pytest.mark.network)
"""

from __future__ import annotations

import os

import pytest


# ---------------------------------------------------------------------------
# Unit tests — HTML processing, data classes, helpers
# ---------------------------------------------------------------------------

class TestHTMLToText:
    """Test HTML → clean text conversion."""

    def test_basic_html(self):
        from whitemagic.gardens.browser.web_research import _html_to_text
        html = "<html><body><h1>Hello</h1><p>World</p></body></html>"
        text = _html_to_text(html)
        assert "Hello" in text
        assert "World" in text

    def test_strips_scripts_and_styles(self):
        from whitemagic.gardens.browser.web_research import _html_to_text
        html = """
        <html><body>
        <script>var x = 1;</script>
        <style>.foo { color: red; }</style>
        <p>Visible content</p>
        </body></html>
        """
        text = _html_to_text(html)
        assert "Visible content" in text
        assert "var x" not in text
        assert "color: red" not in text

    def test_max_chars_limit(self):
        from whitemagic.gardens.browser.web_research import _html_to_text
        html = "<p>" + "x" * 10000 + "</p>"
        text = _html_to_text(html, max_chars=500)
        assert len(text) <= 500

    def test_collapses_blank_lines(self):
        from whitemagic.gardens.browser.web_research import _html_to_text
        html = "<p>A</p><br><br><br><br><br><p>B</p>"
        text = _html_to_text(html)
        assert "\n\n\n" not in text


class TestExtractTitle:
    """Test title extraction from HTML."""

    def test_extracts_title(self):
        from whitemagic.gardens.browser.web_research import _extract_title
        html = "<html><head><title>My Page Title</title></head></html>"
        assert _extract_title(html) == "My Page Title"

    def test_no_title(self):
        from whitemagic.gardens.browser.web_research import _extract_title
        html = "<html><body>No title here</body></html>"
        assert _extract_title(html) == ""

    def test_html_entities(self):
        from whitemagic.gardens.browser.web_research import _extract_title
        html = "<title>Foo &amp; Bar</title>"
        assert _extract_title(html) == "Foo & Bar"


class TestDataClasses:
    """Test FetchResult, SearchResult, SearchResponse, ResearchReport."""

    def test_fetch_result_success(self):
        from whitemagic.gardens.browser.web_research import FetchResult
        r = FetchResult(url="https://example.com", status_code=200, content="hello", content_length=5)
        assert r.success is True
        d = r.to_dict()
        assert d["url"] == "https://example.com"
        assert d["success"] is True

    def test_fetch_result_error(self):
        from whitemagic.gardens.browser.web_research import FetchResult
        r = FetchResult(url="https://example.com", error="timeout")
        assert r.success is False

    def test_search_result_to_dict(self):
        from whitemagic.gardens.browser.web_research import SearchResult
        r = SearchResult(title="Test", url="https://test.com", snippet="A snippet", position=1)
        d = r.to_dict()
        assert d["title"] == "Test"
        assert d["position"] == 1

    def test_search_response_success(self):
        from whitemagic.gardens.browser.web_research import SearchResponse, SearchResult
        r = SearchResponse(
            query="test",
            results=[SearchResult(title="T", url="https://t.com")],
            total_results=1,
        )
        assert r.success is True

    def test_search_response_empty(self):
        from whitemagic.gardens.browser.web_research import SearchResponse
        r = SearchResponse(query="test")
        assert r.success is False

    def test_research_report_to_dict(self):
        from whitemagic.gardens.browser.web_research import ResearchFinding, ResearchReport
        report = ResearchReport(
            topic="AI",
            findings=[ResearchFinding(url="https://x.com", title="X", content="content here")],
            synthesis="Summary",
            sources_searched=5,
            sources_fetched=1,
        )
        d = report.to_dict()
        assert d["topic"] == "AI"
        assert d["sources_fetched"] == 1
        assert len(d["findings"]) == 1


class TestExtractKeyPoints:
    """Test key point extraction from content."""

    def test_extracts_relevant_sentences(self):
        from whitemagic.gardens.browser.web_research import _extract_key_points
        content = (
            "Python is a programming language. "
            "It is widely used in data science. "
            "Python supports multiple paradigms. "
            "The weather is nice today."
        )
        points = _extract_key_points(content, "Python programming")
        assert len(points) >= 1
        # Should find Python-related sentences, not weather
        assert any("Python" in p for p in points)

    def test_empty_content(self):
        from whitemagic.gardens.browser.web_research import _extract_key_points
        points = _extract_key_points("", "test")
        assert points == []


class TestSynthesis:
    """Test synthesis generation."""

    def test_synthesize_with_findings(self):
        from whitemagic.gardens.browser.web_research import ResearchFinding, _synthesize_findings
        findings = [
            ResearchFinding(url="https://a.com", title="Source A", content="Content A", key_points=["Point 1"]),
            ResearchFinding(url="https://b.com", title="Source B", content="Content B", key_points=["Point 2"]),
        ]
        synthesis = _synthesize_findings("test topic", findings)
        assert "test topic" in synthesis
        assert "Source A" in synthesis
        assert "Source B" in synthesis
        assert "Cross-Reference" in synthesis

    def test_synthesize_empty(self):
        from whitemagic.gardens.browser.web_research import _synthesize_findings
        synthesis = _synthesize_findings("test", [])
        assert "No sources" in synthesis


class TestExtractRelatedTopics:
    """Test related topic extraction."""

    def test_extracts_capitalized_phrases(self):
        from whitemagic.gardens.browser.web_research import _extract_related_topics
        content = [
            "The Model Context Protocol was developed by Anthropic. "
            "It works with Large Language Models and Machine Learning systems."
        ]
        topics = _extract_related_topics("MCP", content)
        assert any("Model Context Protocol" in t for t in topics)

    def test_excludes_topic_itself(self):
        from whitemagic.gardens.browser.web_research import _extract_related_topics
        content = ["Python Django is great. Python Flask is also good."]
        topics = _extract_related_topics("Python Django", content)
        assert not any("Python Django" == t for t in topics)


class TestBrowserSessionManager:
    """Test the persistent session manager."""

    def test_singleton(self):
        from whitemagic.gardens.browser.web_research import BrowserSessionManager
        m1 = BrowserSessionManager.get()
        m2 = BrowserSessionManager.get()
        assert m1 is m2

    def test_status_disconnected(self):
        from whitemagic.gardens.browser.web_research import BrowserSessionManager
        mgr = BrowserSessionManager.get()
        # Reset for clean state
        mgr._session = None
        status = mgr.status()
        assert status["connected"] is False


# ---------------------------------------------------------------------------
# Handler unit tests (mocked network)
# ---------------------------------------------------------------------------

class TestHandlers:
    """Test handler functions with mocked network."""

    def test_handle_web_fetch_requires_url(self):
        from whitemagic.tools.handlers.web_research import handle_web_fetch
        with pytest.raises(ValueError, match="url is required"):
            handle_web_fetch()

    def test_handle_web_search_requires_query(self):
        from whitemagic.tools.handlers.web_research import handle_web_search
        with pytest.raises(ValueError, match="query is required"):
            handle_web_search()

    def test_handle_web_search_and_read_requires_query(self):
        from whitemagic.tools.handlers.web_research import handle_web_search_and_read
        with pytest.raises(ValueError, match="query is required"):
            handle_web_search_and_read()

    def test_handle_research_topic_requires_topic(self):
        from whitemagic.tools.handlers.web_research import handle_research_topic
        with pytest.raises(ValueError, match="topic is required"):
            handle_research_topic()

    def test_handle_browser_session_status(self):
        from whitemagic.tools.handlers.web_research import handle_browser_session_status
        result = handle_browser_session_status()
        assert result["status"] == "success"
        assert "connected" in result


# ---------------------------------------------------------------------------
# Registry & dispatch wiring tests
# ---------------------------------------------------------------------------

class TestWiring:
    """Test that all tools are properly wired into dispatch, PRAT, and registry."""

    NEW_TOOLS = [
        "web_fetch",
        "web_search",
        "web_search_and_read",
        "research_topic",
        "browser_session_status",
    ]

    ALL_BROWSER_TOOLS = NEW_TOOLS + [
        "browser_navigate",
        "browser_click",
        "browser_type",
        "browser_extract_dom",
        "browser_screenshot",
        "browser_get_interactables",
    ]

    def test_dispatch_table_has_new_tools(self):
        from whitemagic.tools.dispatch_table import DISPATCH_TABLE
        for tool in self.NEW_TOOLS:
            assert tool in DISPATCH_TABLE, f"{tool} missing from DISPATCH_TABLE"

    def test_prat_router_maps_to_ghost(self):
        from whitemagic.tools.prat_router import TOOL_TO_GANA
        for tool in self.ALL_BROWSER_TOOLS:
            assert TOOL_TO_GANA.get(tool) == "gana_ghost", f"{tool} not mapped to gana_ghost"

    def test_registry_defs_has_all_tools(self):
        from whitemagic.tools.registry_defs.browser import BROWSER_TOOLS
        names = {t.name for t in BROWSER_TOOLS}
        for tool in self.ALL_BROWSER_TOOLS:
            assert tool in names, f"{tool} missing from registry_defs"

    def test_registry_defs_valid_categories(self):
        from whitemagic.tools.registry_defs.browser import BROWSER_TOOLS
        from whitemagic.tools.tool_types import ToolCategory
        for t in BROWSER_TOOLS:
            assert t.category == ToolCategory.BROWSER

    def test_lean_server_has_browser_tools(self):
        """Verify gana_ghost includes browser + web research tools in lean server."""
        # We can't easily import the lean server's static dict, so check PRAT reverse map
        from whitemagic.tools.prat_router import GANA_TO_TOOLS
        ghost_tools = GANA_TO_TOOLS.get("gana_ghost", [])
        for tool in self.ALL_BROWSER_TOOLS:
            assert tool in ghost_tools, f"{tool} missing from gana_ghost PRAT reverse map"


# ---------------------------------------------------------------------------
# Integration tests (live network — skip in CI)
# ---------------------------------------------------------------------------

@pytest.mark.network
@pytest.mark.skipif(
    os.getenv("WM_RUN_NETWORK_TESTS") != "1",
    reason="Live network tests are opt-in. Set WM_RUN_NETWORK_TESTS=1 to enable.",
)
class TestLiveWebSearch:
    """Live network tests — run with: WM_RUN_NETWORK_TESTS=1 pytest -m network"""

    def test_live_web_search(self):
        from whitemagic.gardens.browser.web_research import sync_web_search
        result = sync_web_search("Python programming language", num_results=3)
        assert result.success
        assert result.total_results >= 1
        assert result.results[0].title

    def test_live_web_fetch(self):
        from whitemagic.gardens.browser.web_research import sync_web_fetch
        result = sync_web_fetch("https://httpbin.org/html", max_chars=5000)
        assert result.success
        assert result.status_code == 200
        assert "Herman Melville" in result.content

    def test_live_research_topic(self):
        from whitemagic.gardens.browser.web_research import sync_research_topic
        report = sync_research_topic(
            "WebSocket protocol",
            num_search_results=3,
            max_sources_to_fetch=2,
            max_chars_per_source=5000,
        )
        assert report.sources_searched >= 1
        assert report.sources_fetched >= 1
        assert report.synthesis
