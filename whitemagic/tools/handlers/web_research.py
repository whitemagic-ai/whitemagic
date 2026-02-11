"""Web research tool handlers — web_fetch, web_search, research_topic."""
import asyncio
from collections.abc import Coroutine
from concurrent.futures import ThreadPoolExecutor
from typing import Any, TypeVar

T = TypeVar("T")


def _emit(event_type: str, data: dict[str, Any]) -> None:
    from whitemagic.tools.unified_api import _emit_gan_ying
    _emit_gan_ying(event_type, data)


def _run_async(coro: Coroutine[Any, Any, T]) -> T:
    try:
        asyncio.get_running_loop()
    except RuntimeError:
        return asyncio.run(coro)
    with ThreadPoolExecutor(max_workers=1) as executor:
        return executor.submit(asyncio.run, coro).result()


def handle_web_fetch(**kwargs: Any) -> dict[str, Any]:
    """Fetch a URL and return clean text content (no browser needed)."""
    url = kwargs.get("url", "")
    if not url:
        raise ValueError("url is required")

    max_chars = int(kwargs.get("max_chars", 30_000))
    timeout = float(kwargs.get("timeout", 15.0))

    from whitemagic.gardens.browser.web_research import FetchResult, web_fetch

    async def _fetch() -> FetchResult:
        return await web_fetch(url, max_chars=max_chars, timeout=timeout)

    fetch_result = _run_async(_fetch())

    # Build response with full content (not truncated like to_dict())
    result = {
        "url": fetch_result.url,
        "title": fetch_result.title,
        "content": fetch_result.content,
        "content_length": fetch_result.content_length,
        "status_code": fetch_result.status_code,
        "duration_ms": round(fetch_result.duration_ms, 1),
        "success": fetch_result.success,
        "error": fetch_result.error,
    }

    _emit("WEB_FETCH", {"url": url, "success": fetch_result.success, "length": fetch_result.content_length})
    return {"status": "success", **result}


def handle_web_search(**kwargs: Any) -> dict[str, Any]:
    """Search the web using DuckDuckGo (no API key needed)."""
    query = kwargs.get("query", "")
    if not query:
        raise ValueError("query is required")

    num_results = int(kwargs.get("num_results", 8))
    timeout = float(kwargs.get("timeout", 10.0))

    from whitemagic.gardens.browser.web_research import web_search

    async def _search() -> dict[str, Any]:
        result = await web_search(query, num_results=num_results, timeout=timeout)
        return result.to_dict()

    result = _run_async(_search())
    _emit("WEB_SEARCH", {"query": query, "results": result.get("total_results", 0)})
    return {"status": "success", **result}


def handle_web_search_and_read(**kwargs: Any) -> dict[str, Any]:
    """Search the web AND fetch content from top results in one call.

    Combines web_search + web_fetch for the most common research pattern.
    Returns search results with full page content for top hits.
    """
    query = kwargs.get("query", "")
    if not query:
        raise ValueError("query is required")

    num_results = int(kwargs.get("num_results", 5))
    max_fetch = int(kwargs.get("max_fetch", 3))
    max_chars_per_page = int(kwargs.get("max_chars_per_page", 15_000))

    from whitemagic.gardens.browser.web_research import web_search, web_fetch

    async def _search_and_read() -> dict[str, Any]:
        # Step 1: Search
        search_result = await web_search(query, num_results=num_results)
        if not search_result.success:
            return {"error": search_result.error, "results": []}

        # Step 2: Fetch top results in parallel
        urls = [r.url for r in search_result.results[:max_fetch]]
        fetch_tasks = [web_fetch(u, max_chars=max_chars_per_page) for u in urls]
        fetched = await asyncio.gather(*fetch_tasks, return_exceptions=True)

        # Step 3: Merge
        enriched = []
        for sr in search_result.results:
            entry: dict[str, Any] = sr.to_dict()
            entry["content"] = None
            enriched.append(entry)

        for i, fetch_result in enumerate(fetched):
            if isinstance(fetch_result, BaseException) or not hasattr(fetch_result, "success"):
                continue
            if fetch_result.success and i < len(enriched):
                enriched[i]["content"] = fetch_result.content  # type: ignore[union-attr]
                enriched[i]["content_length"] = fetch_result.content_length  # type: ignore[union-attr]
                enriched[i]["title"] = fetch_result.title or enriched[i].get("title", "")  # type: ignore[union-attr]

        return {
            "query": query,
            "results": enriched,
            "total_results": len(enriched),
            "fetched_count": sum(1 for e in enriched if e.get("content")),
        }

    result = _run_async(_search_and_read())
    _emit("WEB_SEARCH_AND_READ", {"query": query, "fetched": result.get("fetched_count", 0)})
    return {"status": "success", **result}


def handle_research_topic(**kwargs: Any) -> dict[str, Any]:
    """Deep research on a topic: search, fetch, synthesize.

    Single-call replacement for multi-step Exa workflows.
    Pipeline: search → fetch top results → extract key points → synthesize.
    """
    topic = kwargs.get("topic", "")
    if not topic:
        raise ValueError("topic is required")

    num_search_results = int(kwargs.get("num_search_results", 6))
    max_sources = int(kwargs.get("max_sources", 4))
    max_chars_per_source = int(kwargs.get("max_chars_per_source", 15_000))

    from whitemagic.gardens.browser.web_research import research_topic

    async def _research() -> dict[str, Any]:
        result = await research_topic(
            topic,
            num_search_results=num_search_results,
            max_sources_to_fetch=max_sources,
            max_chars_per_source=max_chars_per_source,
        )
        # Include full content in findings (not truncated)
        data = result.to_dict()
        data["findings"] = [
            {
                **f.to_dict(),
                "content": f.content,
                "content_length": len(f.content),
            }
            for f in result.findings
        ]
        return data

    result = _run_async(_research())
    _emit("RESEARCH_TOPIC", {
        "topic": topic,
        "sources": result.get("sources_fetched", 0),
        "duration_ms": result.get("duration_ms"),
    })
    return {"status": "success", **result}


def handle_browser_session_status(**kwargs: Any) -> dict[str, Any]:
    """Get the status of the persistent browser session."""
    from whitemagic.gardens.browser.web_research import BrowserSessionManager
    mgr = BrowserSessionManager.get()
    return {"status": "success", **mgr.status()}
