"""Browser automation tool handlers."""
import asyncio
from collections.abc import Coroutine
from concurrent.futures import ThreadPoolExecutor
from typing import Any, TypeVar, cast

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


def handle_browser_navigate(**kwargs: Any) -> dict[str, Any]:
    url = kwargs.get("url", "")
    if not url:
        raise ValueError("url is required")
    from whitemagic.gardens.browser import BrowserSession
    async def _navigate() -> dict[str, Any]:
        async with BrowserSession() as browser:
            result = await browser.navigate(url)
            return cast("dict[str, Any]", result.to_dict())
    result = _run_async(_navigate())
    _emit("BROWSER_NAVIGATE", {"url": url, "success": result.get("success")})
    return {"status": "success", **result}


def handle_browser_click(**kwargs: Any) -> dict[str, Any]:
    selector = kwargs.get("selector", "")
    if not selector:
        raise ValueError("selector is required")
    from whitemagic.gardens.browser import BrowserSession
    async def _click() -> dict[str, Any]:
        async with BrowserSession() as browser:
            result = await browser.click(selector)
            return cast("dict[str, Any]", result.to_dict())
    result = _run_async(_click())
    _emit("BROWSER_ACTION", {"action": "click", "selector": selector})
    return {"status": "success", **result}


def handle_browser_type(**kwargs: Any) -> dict[str, Any]:
    selector = kwargs.get("selector", "")
    text = kwargs.get("text", "")
    if not selector:
        raise ValueError("selector is required")
    from whitemagic.gardens.browser import BrowserSession
    async def _type() -> dict[str, Any]:
        async with BrowserSession() as browser:
            result = await browser.type(selector, text)
            return cast("dict[str, Any]", result.to_dict())
    result = _run_async(_type())
    _emit("BROWSER_ACTION", {"action": "type", "selector": selector})
    return {"status": "success", **result}


def handle_browser_extract_dom(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.gardens.browser import BrowserSession
    async def _extract() -> dict[str, Any]:
        async with BrowserSession() as browser:
            dom = await browser.extract_dom()
            if dom:
                return cast("dict[str, Any]", browser.summarize_dom(dom))
            return {"error": "No DOM extracted"}
    result = _run_async(_extract())
    return {"status": "success", **result}


def handle_browser_screenshot(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.gardens.browser import BrowserSession
    async def _screenshot() -> dict[str, Any]:
        async with BrowserSession() as browser:
            screenshot = await browser.screenshot()
            return {"format": screenshot.format, "width": screenshot.width, "height": screenshot.height, "data_length": len(screenshot.data)}
    result = _run_async(_screenshot())
    return {"status": "success", **result}


def handle_browser_get_interactables(**kwargs: Any) -> dict[str, Any]:
    from whitemagic.gardens.browser import BrowserSession
    async def _get_interactables() -> dict[str, Any]:
        async with BrowserSession() as browser:
            dom = await browser.extract_dom()
            if dom:
                interactables = browser.get_interactables(dom)
                return {"count": len(interactables), "elements": [{"tag": e.tag, "selector": e.selector, "text": e.text[:50]} for e in interactables[:20]]}
            return {"error": "No DOM extracted"}
    result = _run_async(_get_interactables())
    return {"status": "success", **result}
