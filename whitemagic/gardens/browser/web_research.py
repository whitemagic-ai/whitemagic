"""Web Research Engine — Fast-path web fetching, search, and deep research.

Provides three tiers of web access:
1. web_fetch  — httpx + html2text for instant read-only content (no browser needed)
2. web_search — DuckDuckGo HTML scraping for zero-API-key web search
3. research_topic — Orchestrator combining search + fetch + Rabbit Hole synthesis

This module is designed to replace external search MCP tools (like Exa) with
WhiteMagic's own internal capabilities, giving AI agents self-contained web
research without API keys or cloud dependencies.

Integration:
    Wired into the Browser Garden and Gan Ying Bus for resonance.
    Results can be stored as memories for future recall.
"""

from __future__ import annotations

import asyncio
import logging
import re
import time
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass, field
from datetime import datetime, timezone
from typing import Any
from urllib.parse import quote_plus, urlparse

logger = logging.getLogger(__name__)

# --- Optional imports with graceful fallback ---

try:
    import httpx
    HAS_HTTPX = True
except ImportError:
    HAS_HTTPX = False

try:
    import html2text as _html2text
    HAS_HTML2TEXT = True
except ImportError:
    HAS_HTML2TEXT = False

try:
    from bs4 import BeautifulSoup
    HAS_BS4 = True
except ImportError:
    HAS_BS4 = False


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------

@dataclass
class FetchResult:
    """Result from fetching a single URL."""

    url: str
    title: str = ""
    content: str = ""
    content_length: int = 0
    status_code: int = 0
    duration_ms: float = 0.0
    error: str | None = None
    timestamp: str = field(default_factory=lambda: datetime.now(timezone.utc).isoformat())

    @property
    def success(self) -> bool:
        return self.error is None and self.status_code in range(200, 400)

    def to_dict(self) -> dict[str, Any]:
        return {
            "url": self.url,
            "title": self.title,
            "content": self.content[:500] + "..." if len(self.content) > 500 else self.content,
            "content_length": self.content_length,
            "status_code": self.status_code,
            "duration_ms": round(self.duration_ms, 1),
            "success": self.success,
            "error": self.error,
            "timestamp": self.timestamp,
        }


@dataclass
class SearchResult:
    """A single search result."""

    title: str
    url: str
    snippet: str = ""
    position: int = 0

    def to_dict(self) -> dict[str, Any]:
        return {
            "title": self.title,
            "url": self.url,
            "snippet": self.snippet,
            "position": self.position,
        }


@dataclass
class SearchResponse:
    """Response from a web search."""

    query: str
    results: list[SearchResult] = field(default_factory=list)
    total_results: int = 0
    duration_ms: float = 0.0
    error: str | None = None
    timestamp: str = field(default_factory=lambda: datetime.now(timezone.utc).isoformat())

    @property
    def success(self) -> bool:
        return self.error is None and len(self.results) > 0

    def to_dict(self) -> dict[str, Any]:
        return {
            "query": self.query,
            "results": [r.to_dict() for r in self.results],
            "total_results": self.total_results,
            "duration_ms": round(self.duration_ms, 1),
            "success": self.success,
            "error": self.error,
        }


@dataclass
class ResearchFinding:
    """A single finding from deep research."""

    url: str
    title: str
    content: str
    relevance: str = ""
    key_points: list[str] = field(default_factory=list)

    def to_dict(self) -> dict[str, Any]:
        return {
            "url": self.url,
            "title": self.title,
            "content_preview": self.content[:300] + "..." if len(self.content) > 300 else self.content,
            "content_length": len(self.content),
            "relevance": self.relevance,
            "key_points": self.key_points,
        }


@dataclass
class ResearchReport:
    """Complete research report from deep topic exploration."""

    topic: str
    findings: list[ResearchFinding] = field(default_factory=list)
    synthesis: str = ""
    related_topics: list[str] = field(default_factory=list)
    sources_searched: int = 0
    sources_fetched: int = 0
    duration_ms: float = 0.0
    error: str | None = None
    timestamp: str = field(default_factory=lambda: datetime.now(timezone.utc).isoformat())

    def to_dict(self) -> dict[str, Any]:
        return {
            "topic": self.topic,
            "findings": [f.to_dict() for f in self.findings],
            "synthesis": self.synthesis,
            "related_topics": self.related_topics,
            "sources_searched": self.sources_searched,
            "sources_fetched": self.sources_fetched,
            "duration_ms": round(self.duration_ms, 1),
            "error": self.error,
        }


# ---------------------------------------------------------------------------
# HTML → Clean Text
# ---------------------------------------------------------------------------

_DEFAULT_HEADERS = {
    "User-Agent": "WhiteMagic/15.0 (Research Agent; +https://github.com/whitemagic-ai/whitemagic)",
    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language": "en-US,en;q=0.9",
    "Accept-Encoding": "gzip, deflate",
}


def _html_to_text(html: str, max_chars: int = 50_000) -> str:
    """Convert HTML to clean markdown-ish text, optimized for AI consumption."""
    if HAS_HTML2TEXT:
        h = _html2text.HTML2Text()
        h.ignore_links = False
        h.ignore_images = True
        h.ignore_emphasis = False
        h.body_width = 0  # Don't wrap
        h.skip_internal_links = True
        h.inline_links = True
        h.protect_links = True
        h.unicode_snob = True
        text = h.handle(html)
    elif HAS_BS4:
        soup = BeautifulSoup(html, "lxml")
        # Remove script/style
        for tag in soup(["script", "style", "nav", "footer", "header"]):
            tag.decompose()
        text = soup.get_text(separator="\n", strip=True)
    else:
        # Desperate fallback: regex strip
        text = re.sub(r"<script[^>]*>.*?</script>", "", html, flags=re.DOTALL)
        text = re.sub(r"<style[^>]*>.*?</style>", "", html, flags=re.DOTALL)
        text = re.sub(r"<[^>]+>", " ", html)
        text = re.sub(r"\s+", " ", text).strip()

    # Collapse excessive blank lines
    text = re.sub(r"\n{3,}", "\n\n", text)

    return text[:max_chars].strip()


def _extract_title(html: str) -> str:
    """Extract page title from HTML."""
    match = re.search(r"<title[^>]*>(.*?)</title>", html, re.IGNORECASE | re.DOTALL)
    if match:
        title = match.group(1).strip()
        # Clean HTML entities
        title = re.sub(r"&amp;", "&", title)
        title = re.sub(r"&lt;", "<", title)
        title = re.sub(r"&gt;", ">", title)
        title = re.sub(r"&#\d+;", "", title)
        return title[:200]
    return ""


# ---------------------------------------------------------------------------
# web_fetch — Fast HTTP content retrieval
# ---------------------------------------------------------------------------

async def web_fetch(
    url: str,
    max_chars: int = 30_000,
    timeout: float = 15.0,
    follow_redirects: bool = True,
) -> FetchResult:
    """Fetch a URL and return clean text content.

    Uses httpx for fast async HTTP — no browser needed.
    Converts HTML to clean text optimized for AI token usage.

    Args:
        url: The URL to fetch
        max_chars: Maximum characters to return (default 30K)
        timeout: Request timeout in seconds
        follow_redirects: Follow HTTP redirects

    Returns:
        FetchResult with clean text content
    """
    if not HAS_HTTPX:
        return FetchResult(url=url, error="httpx not installed: pip install httpx")

    start = time.monotonic()

    try:
        async with httpx.AsyncClient(
            headers=_DEFAULT_HEADERS,
            follow_redirects=follow_redirects,
            timeout=timeout,
        ) as client:
            response = await client.get(url)
            duration = (time.monotonic() - start) * 1000

            content_type = response.headers.get("content-type", "")

            if "text/html" in content_type or "application/xhtml" in content_type:
                html = response.text
                title = _extract_title(html)
                content = _html_to_text(html, max_chars=max_chars)
            elif "text/plain" in content_type or "application/json" in content_type:
                title = urlparse(url).path.split("/")[-1] or url
                content = response.text[:max_chars]
            else:
                title = urlparse(url).path.split("/")[-1] or url
                content = f"[Binary content: {content_type}, {len(response.content)} bytes]"

            return FetchResult(
                url=str(response.url),
                title=title,
                content=content,
                content_length=len(content),
                status_code=response.status_code,
                duration_ms=duration,
            )

    except httpx.TimeoutException:
        return FetchResult(
            url=url,
            error=f"Timeout after {timeout}s",
            duration_ms=(time.monotonic() - start) * 1000,
        )
    except Exception as e:
        return FetchResult(
            url=url,
            error=str(e),
            duration_ms=(time.monotonic() - start) * 1000,
        )


# ---------------------------------------------------------------------------
# web_search — Brave API (preferred) → DuckDuckGo HTML (fallback)
# ---------------------------------------------------------------------------

async def _brave_search(
    query: str,
    num_results: int = 8,
    timeout: float = 10.0,
) -> SearchResponse | None:
    """Search via Brave Search API (free tier: 2000 queries/month).

    Returns None if Brave API key is not configured, allowing DDG fallback.
    Set BRAVE_SEARCH_API_KEY env var to enable.
    """
    import os
    api_key = os.environ.get("BRAVE_SEARCH_API_KEY", "")
    if not api_key or not HAS_HTTPX:
        return None

    start = time.monotonic()
    try:
        async with httpx.AsyncClient(timeout=timeout) as client:
            response = await client.get(
                "https://api.search.brave.com/res/v1/web/search",
                params={"q": query, "count": min(num_results, 20)},
                headers={
                    "Accept": "application/json",
                    "Accept-Encoding": "gzip",
                    "X-Subscription-Token": api_key,
                },
            )
            duration = (time.monotonic() - start) * 1000

            if response.status_code != 200:
                logger.debug(f"Brave search returned {response.status_code}, falling back to DDG")
                return None

            data = response.json()
            results: list[SearchResult] = []
            for i, item in enumerate(data.get("web", {}).get("results", [])):
                if i >= num_results:
                    break
                results.append(SearchResult(
                    title=item.get("title", ""),
                    url=item.get("url", ""),
                    snippet=item.get("description", ""),
                    position=i + 1,
                ))

            return SearchResponse(
                query=query,
                results=results,
                total_results=len(results),
                duration_ms=duration,
            )
    except Exception as e:
        logger.debug(f"Brave search failed ({e}), falling back to DDG")
        return None


async def web_search(
    query: str,
    num_results: int = 8,
    timeout: float = 10.0,
) -> SearchResponse:
    """Search the web using Brave API (preferred) or DuckDuckGo HTML (fallback).

    If BRAVE_SEARCH_API_KEY is set, uses Brave Search API (fast, reliable,
    2000 free queries/month). Otherwise falls back to DuckDuckGo HTML scraping.

    Args:
        query: Search query string
        num_results: Maximum number of results to return
        timeout: Request timeout in seconds

    Returns:
        SearchResponse with list of results
    """
    # Try Brave API first (more reliable, structured JSON)
    brave_result = await _brave_search(query, num_results, timeout)
    if brave_result is not None:
        return brave_result

    # Fallback: DuckDuckGo HTML scraping (no API key needed)
    if not HAS_HTTPX:
        return SearchResponse(query=query, error="httpx not installed: pip install httpx")
    if not HAS_BS4:
        return SearchResponse(query=query, error="beautifulsoup4 not installed: pip install beautifulsoup4")

    start = time.monotonic()

    try:
        encoded_query = quote_plus(query)
        # Use DuckDuckGo HTML-only endpoint (no JavaScript required)
        url = f"https://html.duckduckgo.com/html/?q={encoded_query}"

        headers = {
            **_DEFAULT_HEADERS,
            "Referer": "https://duckduckgo.com/",
        }

        async with httpx.AsyncClient(
            headers=headers,
            follow_redirects=True,
            timeout=timeout,
        ) as client:
            response = await client.get(url)
            duration = (time.monotonic() - start) * 1000

            if response.status_code != 200:
                return SearchResponse(
                    query=query,
                    error=f"Search returned status {response.status_code}",
                    duration_ms=duration,
                )

            soup = BeautifulSoup(response.text, "lxml")
            results: list[SearchResult] = []

            # DuckDuckGo HTML results are in .result elements
            for i, result_div in enumerate(soup.select(".result")):
                if i >= num_results:
                    break

                # Extract title and URL
                title_tag = result_div.select_one(".result__a")
                snippet_tag = result_div.select_one(".result__snippet")

                if not title_tag:
                    continue

                title = title_tag.get_text(strip=True)
                href = title_tag.get("href", "")

                # DuckDuckGo wraps URLs in redirects, extract actual URL
                if isinstance(href, str) and "uddg=" in href:
                    # Extract from redirect: //duckduckgo.com/l/?uddg=ENCODED_URL&...
                    from urllib.parse import parse_qs, urlparse as _urlparse
                    parsed = _urlparse(href)
                    params = parse_qs(parsed.query)
                    actual_urls = params.get("uddg", [])
                    if actual_urls:
                        href = actual_urls[0]
                elif isinstance(href, str) and href.startswith("//"):
                    href = "https:" + href

                snippet = snippet_tag.get_text(strip=True) if snippet_tag else ""

                if title and href:
                    results.append(SearchResult(
                        title=title,
                        url=str(href),
                        snippet=snippet,
                        position=i + 1,
                    ))

            return SearchResponse(
                query=query,
                results=results,
                total_results=len(results),
                duration_ms=duration,
            )

    except httpx.TimeoutException:
        return SearchResponse(
            query=query,
            error=f"Search timeout after {timeout}s",
            duration_ms=(time.monotonic() - start) * 1000,
        )
    except Exception as e:
        return SearchResponse(
            query=query,
            error=str(e),
            duration_ms=(time.monotonic() - start) * 1000,
        )


# ---------------------------------------------------------------------------
# research_topic — Deep research orchestrator
# ---------------------------------------------------------------------------

async def research_topic(
    topic: str,
    num_search_results: int = 6,
    max_sources_to_fetch: int = 4,
    max_chars_per_source: int = 15_000,
    timeout_per_fetch: float = 12.0,
) -> ResearchReport:
    """Deep research on a topic: search → fetch top results → synthesize.

    This is the high-level orchestrator that replaces multi-step
    Exa search + fetch workflows with a single call.

    Pipeline:
        1. Web search for the topic
        2. Fetch top N results in parallel
        3. Extract key points from each source
        4. Cross-reference and find patterns
        5. Generate synthesis with source comparison
        6. Identify related topics for further exploration

    Args:
        topic: The research topic or question
        num_search_results: How many search results to get
        max_sources_to_fetch: How many of the top results to fetch content from
        max_chars_per_source: Max characters per source
        timeout_per_fetch: Timeout per fetch in seconds

    Returns:
        ResearchReport with findings and synthesis
    """
    start = time.monotonic()

    # Step 1: Search
    search_result = await web_search(topic, num_results=num_search_results)

    if not search_result.success:
        return ResearchReport(
            topic=topic,
            error=f"Search failed: {search_result.error}",
            duration_ms=(time.monotonic() - start) * 1000,
        )

    # Step 2: Fetch top sources in parallel
    urls_to_fetch = [r.url for r in search_result.results[:max_sources_to_fetch]]
    search_meta = {r.url: r for r in search_result.results}

    fetch_tasks = [
        web_fetch(url, max_chars=max_chars_per_source, timeout=timeout_per_fetch)
        for url in urls_to_fetch
    ]

    fetched = await asyncio.gather(*fetch_tasks, return_exceptions=True)

    # Step 3: Process findings
    findings: list[ResearchFinding] = []
    all_content_blocks: list[str] = []

    for result in fetched:
        if isinstance(result, Exception):
            continue
        if not isinstance(result, FetchResult) or not result.success:
            continue
        if not result.content or len(result.content.strip()) < 50:
            continue

        # Extract key points (sentences containing the topic or key terms)
        key_points = _extract_key_points(result.content, topic)

        meta = search_meta.get(result.url)
        relevance = meta.snippet if meta else ""

        finding = ResearchFinding(
            url=result.url,
            title=result.title or (meta.title if meta else ""),
            content=result.content,
            relevance=relevance,
            key_points=key_points[:10],
        )
        findings.append(finding)
        all_content_blocks.append(result.content)

    # Step 4: Synthesize
    synthesis = _synthesize_findings(topic, findings)

    # Step 5: Extract related topics
    related = _extract_related_topics(topic, all_content_blocks)

    duration = (time.monotonic() - start) * 1000

    # Emit to Gan Ying if available
    _emit_research_event(topic, len(findings), duration)

    return ResearchReport(
        topic=topic,
        findings=findings,
        synthesis=synthesis,
        related_topics=related,
        sources_searched=len(search_result.results),
        sources_fetched=len(findings),
        duration_ms=duration,
    )


# ---------------------------------------------------------------------------
# Synthesis helpers
# ---------------------------------------------------------------------------

def _extract_key_points(content: str, topic: str, max_points: int = 10) -> list[str]:
    """Extract key sentences related to the topic from content."""
    # Split into sentences
    sentences = re.split(r'(?<=[.!?])\s+', content)

    topic_words = set(topic.lower().split())
    scored: list[tuple[float, str]] = []

    for sentence in sentences:
        sentence = sentence.strip()
        if len(sentence) < 30 or len(sentence) > 500:
            continue

        words = set(sentence.lower().split())
        overlap = len(topic_words & words)
        if overlap > 0:
            score = overlap / len(topic_words)
            scored.append((score, sentence))

    # Sort by relevance, return top N
    scored.sort(key=lambda x: x[0], reverse=True)
    return [s for _, s in scored[:max_points]]


def _synthesize_findings(topic: str, findings: list[ResearchFinding]) -> str:
    """Generate a synthesis from multiple research findings."""
    if not findings:
        return "No sources could be fetched for this topic."

    parts = [f"## Research Synthesis: {topic}\n"]
    parts.append(f"**{len(findings)} sources analyzed.**\n")

    # Source overview
    parts.append("### Sources\n")
    for i, f in enumerate(findings, 1):
        parts.append(f"{i}. [{f.title}]({f.url})")

    # Key points aggregation
    all_points = []
    for f in findings:
        all_points.extend(f.key_points)

    if all_points:
        parts.append("\n### Key Points Across Sources\n")
        seen = set()
        for point in all_points[:15]:
            # Deduplicate similar points
            key = point[:60].lower()
            if key not in seen:
                seen.add(key)
                parts.append(f"- {point}")

    # Cross-reference note
    if len(findings) >= 2:
        parts.append("\n### Cross-Reference Notes\n")
        parts.append(f"Information gathered from {len(findings)} independent sources. "
                     "Points appearing across multiple sources have higher confidence.")

    return "\n".join(parts)


def _extract_related_topics(topic: str, content_blocks: list[str], max_topics: int = 10) -> list[str]:
    """Extract related topics from content for further rabbit-hole exploration."""
    combined = " ".join(content_blocks)[:50_000]

    # Find capitalized multi-word phrases that aren't the topic itself
    pattern = r"\b([A-Z][a-z]+(?:\s+[A-Z][a-z]+)+)\b"
    matches = re.findall(pattern, combined)

    topic_lower = topic.lower()
    counts: dict[str, int] = {}
    for match in matches:
        if match.lower() != topic_lower and len(match) > 5:
            counts[match] = counts.get(match, 0) + 1

    # Sort by frequency, return top N
    sorted_topics = sorted(counts.items(), key=lambda x: x[1], reverse=True)
    return [t for t, _ in sorted_topics[:max_topics]]


def _emit_research_event(topic: str, num_findings: int, duration_ms: float) -> None:
    """Emit research completion to Gan Ying Bus."""
    try:
        from whitemagic.core.resonance.gan_ying import (
            EventType,
            ResonanceEvent,
            get_bus,
        )
        bus = get_bus()
        bus.emit(ResonanceEvent(
            source="browser.web_research",
            event_type=EventType.PATTERN_DETECTED,
            data={
                "topic": topic,
                "findings": num_findings,
                "duration_ms": round(duration_ms, 1),
                "timestamp": datetime.now(timezone.utc).isoformat(),
            },
            confidence=0.8,
        ))
    except (ImportError, Exception):
        pass


# ---------------------------------------------------------------------------
# Persistent Browser Session Manager
# ---------------------------------------------------------------------------

class BrowserSessionManager:
    """Singleton manager for persistent browser sessions.

    Solves the session-per-call problem where each browser tool handler
    creates and destroys its own session. This manager keeps a session
    alive across multiple tool calls for chained interactions.
    """

    _instance: BrowserSessionManager | None = None
    _lock = asyncio.Lock() if hasattr(asyncio, "Lock") else None

    def __init__(self) -> None:
        self._session: Any = None
        self._last_url: str = ""
        self._created_at: str = ""

    @classmethod
    def get(cls) -> BrowserSessionManager:
        if cls._instance is None:
            cls._instance = cls()
        return cls._instance

    async def get_session(self) -> Any:
        """Get or create a persistent browser session."""
        from .actions import BrowserSession

        if self._session is None:
            self._session = BrowserSession()
            try:
                await self._session.connect()
                self._created_at = datetime.now(timezone.utc).isoformat()
            except Exception as e:
                self._session = None
                raise RuntimeError(f"Cannot connect to Chrome: {e}. "
                                   "Start Chrome with: google-chrome --remote-debugging-port=9222") from e
        return self._session

    async def close(self) -> None:
        """Close the persistent session."""
        if self._session is not None:
            try:
                await self._session.disconnect()
            except Exception:
                pass
            self._session = None
            self._last_url = ""

    @property
    def is_connected(self) -> bool:
        return self._session is not None

    def status(self) -> dict[str, Any]:
        return {
            "connected": self.is_connected,
            "last_url": self._last_url,
            "created_at": self._created_at,
        }


# ---------------------------------------------------------------------------
# Synchronous wrappers for handler use
# ---------------------------------------------------------------------------

def _run_async(coro: Any) -> Any:
    """Run async coroutine from sync context."""
    try:
        asyncio.get_running_loop()
    except RuntimeError:
        return asyncio.run(coro)
    with ThreadPoolExecutor(max_workers=1) as executor:
        return executor.submit(asyncio.run, coro).result()


def sync_web_fetch(url: str, max_chars: int = 30_000, timeout: float = 15.0) -> FetchResult:
    """Synchronous wrapper for web_fetch."""
    result: FetchResult = _run_async(web_fetch(url, max_chars=max_chars, timeout=timeout))
    return result


def sync_web_search(query: str, num_results: int = 8, timeout: float = 10.0) -> SearchResponse:
    """Synchronous wrapper for web_search."""
    result: SearchResponse = _run_async(web_search(query, num_results=num_results, timeout=timeout))
    return result


def sync_research_topic(topic: str, **kwargs: Any) -> ResearchReport:
    """Synchronous wrapper for research_topic."""
    result: ResearchReport = _run_async(research_topic(topic, **kwargs))
    return result
