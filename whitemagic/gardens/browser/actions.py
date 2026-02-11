"""Browser Actions for AI-Driven Automation.

High-level actions for AI agents to interact with web pages.
Based on the insight: "AI needs click/type/navigate, not raw CDP"

Usage:
    async with BrowserSession() as browser:
        await browser.navigate("https://example.com")
        await browser.click("#login-button")
        await browser.type("#username", "user@example.com")
        dom = await browser.extract_dom()
"""

from __future__ import annotations

import asyncio
from dataclasses import dataclass, field
from datetime import datetime, timezone
from typing import Any

from .cdp import CDPConnection, CDPResponse, connect_to_chrome
from .distiller import DOMDistiller, SimplifiedElement
from .screenshot import Screenshot, capture_element, capture_screenshot


@dataclass
class ActionResult:
    """Result of a browser action."""

    success: bool
    action: str
    selector: str | None = None
    value: str | None = None
    error: str | None = None
    duration_ms: float = 0.0
    timestamp: str = field(default_factory=lambda: datetime.now(timezone.utc).isoformat())

    def to_dict(self) -> dict[str, Any]:
        return {
            "success": self.success,
            "action": self.action,
            "selector": self.selector,
            "value": self.value,
            "error": self.error,
            "duration_ms": self.duration_ms,
            "timestamp": self.timestamp,
        }


class BrowserActions:
    """Browser action primitives using CDP.

    Provides clean abstractions over CDP for:
    - Navigation
    - Click interactions
    - Text input
    - Form submission
    - Scrolling
    """

    def __init__(self, cdp: CDPConnection) -> None:
        self.cdp = cdp
        self._distiller = DOMDistiller()

    @staticmethod
    def _extract_response_value(response: CDPResponse) -> Any:
        result = response.result if isinstance(response.result, dict) else {}
        inner = result.get("result")
        if isinstance(inner, dict):
            return inner.get("value")
        return None

    @staticmethod
    def _extract_response_object(response: CDPResponse) -> dict[str, Any]:
        value = BrowserActions._extract_response_value(response)
        if isinstance(value, dict):
            return value
        return {}

    async def navigate(self, url: str, wait_for_load: bool = True) -> ActionResult:
        """Navigate to a URL.

        Args:
            url: Target URL
            wait_for_load: Wait for page load event

        Returns:
            ActionResult with navigation status

        """
        start = asyncio.get_event_loop().time()

        try:
            # Enable page events if waiting
            if wait_for_load:
                await self.cdp.enable_page()

            response = await self.cdp.navigate(url)

            if not response.success:
                return ActionResult(
                    success=False,
                    action="navigate",
                    value=url,
                    error=str(response.error),
                )

            # Wait for load event
            if wait_for_load:
                await self._wait_for_load()

            duration = (asyncio.get_event_loop().time() - start) * 1000

            return ActionResult(
                success=True,
                action="navigate",
                value=url,
                duration_ms=duration,
            )

        except Exception as e:
            return ActionResult(
                success=False,
                action="navigate",
                value=url,
                error=str(e),
            )

    async def _wait_for_load(self, timeout: float = 30.0) -> bool:
        """Wait for page load event."""
        loaded = asyncio.Event()

        async def on_load(event: Any) -> None:
            loaded.set()

        self.cdp.on("Page.loadEventFired", on_load)

        try:
            await asyncio.wait_for(loaded.wait(), timeout=timeout)
            return True
        except asyncio.TimeoutError:
            return False
        finally:
            self.cdp.off("Page.loadEventFired", on_load)

    async def click(self, selector: str) -> ActionResult:
        """Click an element.

        Args:
            selector: CSS selector for element

        Returns:
            ActionResult with click status

        """
        start = asyncio.get_event_loop().time()

        try:
            # Use JavaScript to click (more reliable than CDP Input)
            js = f"""
            (function() {{
                const el = document.querySelector('{selector}');
                if (!el) return {{ error: 'Element not found' }};
                el.click();
                return {{ success: true }};
            }})()
            """

            response = await self.cdp.evaluate(js)

            if not response.success:
                return ActionResult(
                    success=False,
                    action="click",
                    selector=selector,
                    error=str(response.error),
                )

            result = self._extract_response_object(response)

            if result.get("error"):
                return ActionResult(
                    success=False,
                    action="click",
                    selector=selector,
                    error=result["error"],
                )

            duration = (asyncio.get_event_loop().time() - start) * 1000

            return ActionResult(
                success=True,
                action="click",
                selector=selector,
                duration_ms=duration,
            )

        except Exception as e:
            return ActionResult(
                success=False,
                action="click",
                selector=selector,
                error=str(e),
            )

    async def type(self, selector: str, text: str, clear_first: bool = True) -> ActionResult:
        """Type text into an input element.

        Args:
            selector: CSS selector for input
            text: Text to type
            clear_first: Clear existing content first

        Returns:
            ActionResult with type status

        """
        start = asyncio.get_event_loop().time()

        try:
            # Clear and type using JavaScript
            clear_js = "el.value = '';" if clear_first else ""
            escaped_text = text.replace("'", "\\'").replace("\n", "\\n")

            js = f"""
            (function() {{
                const el = document.querySelector('{selector}');
                if (!el) return {{ error: 'Element not found' }};
                el.focus();
                {clear_js}
                el.value = '{escaped_text}';
                el.dispatchEvent(new Event('input', {{ bubbles: true }}));
                el.dispatchEvent(new Event('change', {{ bubbles: true }}));
                return {{ success: true }};
            }})()
            """

            response = await self.cdp.evaluate(js)

            if not response.success:
                return ActionResult(
                    success=False,
                    action="type",
                    selector=selector,
                    value=text,
                    error=str(response.error),
                )

            result = self._extract_response_object(response)

            if result.get("error"):
                return ActionResult(
                    success=False,
                    action="type",
                    selector=selector,
                    value=text,
                    error=result["error"],
                )

            duration = (asyncio.get_event_loop().time() - start) * 1000

            return ActionResult(
                success=True,
                action="type",
                selector=selector,
                value=text,
                duration_ms=duration,
            )

        except Exception as e:
            return ActionResult(
                success=False,
                action="type",
                selector=selector,
                value=text,
                error=str(e),
            )

    async def select(self, selector: str, value: str) -> ActionResult:
        """Select an option from a dropdown.

        Args:
            selector: CSS selector for select element
            value: Option value to select

        Returns:
            ActionResult with select status

        """
        start = asyncio.get_event_loop().time()

        try:
            escaped_value = value.replace("'", "\\'")

            js = f"""
            (function() {{
                const el = document.querySelector('{selector}');
                if (!el) return {{ error: 'Element not found' }};
                if (el.tagName !== 'SELECT') return {{ error: 'Not a select element' }};
                el.value = '{escaped_value}';
                el.dispatchEvent(new Event('change', {{ bubbles: true }}));
                return {{ success: true }};
            }})()
            """

            response = await self.cdp.evaluate(js)

            if not response.success:
                return ActionResult(
                    success=False,
                    action="select",
                    selector=selector,
                    value=value,
                    error=str(response.error),
                )

            result = self._extract_response_object(response)

            if result.get("error"):
                return ActionResult(
                    success=False,
                    action="select",
                    selector=selector,
                    value=value,
                    error=result["error"],
                )

            duration = (asyncio.get_event_loop().time() - start) * 1000

            return ActionResult(
                success=True,
                action="select",
                selector=selector,
                value=value,
                duration_ms=duration,
            )

        except Exception as e:
            return ActionResult(
                success=False,
                action="select",
                selector=selector,
                value=value,
                error=str(e),
            )

    async def scroll(self, x: int = 0, y: int = 0) -> ActionResult:
        """Scroll the page.

        Args:
            x: Horizontal scroll pixels
            y: Vertical scroll pixels

        Returns:
            ActionResult with scroll status

        """
        start = asyncio.get_event_loop().time()

        try:
            js = f"window.scrollBy({x}, {y}); true;"
            response = await self.cdp.evaluate(js)

            duration = (asyncio.get_event_loop().time() - start) * 1000

            return ActionResult(
                success=response.success,
                action="scroll",
                value=f"({x}, {y})",
                duration_ms=duration,
                error=str(response.error) if not response.success else None,
            )

        except Exception as e:
            return ActionResult(
                success=False,
                action="scroll",
                value=f"({x}, {y})",
                error=str(e),
            )

    async def scroll_to_element(self, selector: str) -> ActionResult:
        """Scroll element into view."""
        start = asyncio.get_event_loop().time()

        try:
            js = f"""
            (function() {{
                const el = document.querySelector('{selector}');
                if (!el) return {{ error: 'Element not found' }};
                el.scrollIntoView({{ behavior: 'smooth', block: 'center' }});
                return {{ success: true }};
            }})()
            """

            response = await self.cdp.evaluate(js)

            if not response.success:
                return ActionResult(
                    success=False,
                    action="scroll_to",
                    selector=selector,
                    error=str(response.error),
                )

            result = self._extract_response_object(response)

            duration = (asyncio.get_event_loop().time() - start) * 1000

            return ActionResult(
                success=not result.get("error"),
                action="scroll_to",
                selector=selector,
                error=result.get("error"),
                duration_ms=duration,
            )

        except Exception as e:
            return ActionResult(
                success=False,
                action="scroll_to",
                selector=selector,
                error=str(e),
            )

    async def wait_for_element(
        self,
        selector: str,
        timeout: float = 10.0,
        visible: bool = False,
    ) -> ActionResult:
        """Wait for an element to appear.

        Args:
            selector: CSS selector
            timeout: Max wait time in seconds
            visible: Also wait for visibility

        Returns:
            ActionResult with wait status

        """
        start = asyncio.get_event_loop().time()
        interval = 0.1

        visibility_check = ""
        if visible:
            visibility_check = """
                const style = window.getComputedStyle(el);
                if (style.display === 'none' || style.visibility === 'hidden') return null;
            """

        while (asyncio.get_event_loop().time() - start) < timeout:
            js = f"""
            (function() {{
                const el = document.querySelector('{selector}');
                if (!el) return null;
                {visibility_check}
                return true;
            }})()
            """

            response = await self.cdp.evaluate(js)

            if response.success:
                value = self._extract_response_value(response)
                if value:
                    duration = (asyncio.get_event_loop().time() - start) * 1000
                    return ActionResult(
                        success=True,
                        action="wait_for",
                        selector=selector,
                        duration_ms=duration,
                    )

            await asyncio.sleep(interval)

        duration = (asyncio.get_event_loop().time() - start) * 1000
        return ActionResult(
            success=False,
            action="wait_for",
            selector=selector,
            error=f"Timeout after {timeout}s",
            duration_ms=duration,
        )

    async def get_text(self, selector: str) -> str | None:
        """Get text content of an element."""
        js = f"""
        (function() {{
            const el = document.querySelector('{selector}');
            return el ? el.textContent.trim() : null;
        }})()
        """

        response = await self.cdp.evaluate(js)

        if response.success:
            value = self._extract_response_value(response)
            if value is not None:
                return str(value)
        return None

    async def get_attribute(self, selector: str, attribute: str) -> str | None:
        """Get attribute value of an element."""
        js = f"""
        (function() {{
            const el = document.querySelector('{selector}');
            return el ? el.getAttribute('{attribute}') : null;
        }})()
        """

        response = await self.cdp.evaluate(js)

        if response.success:
            value = self._extract_response_value(response)
            if value is not None:
                return str(value)
        return None


class BrowserSession:
    """High-level browser session for AI agents.

    Combines CDP connection, actions, and DOM distillation
    into a clean interface for AI-driven automation.

    Usage:
        async with BrowserSession() as browser:
            await browser.navigate("https://example.com")
            dom = await browser.extract_dom()
            interactables = browser.get_interactables(dom)
            await browser.click(interactables[0].selector)
    """

    def __init__(
        self,
        host: str = "localhost",
        port: int = 9222,
        ws_url: str | None = None,
    ) -> None:
        """Initialize browser session.

        Args:
            host: Chrome debugging host
            port: Chrome debugging port
            ws_url: Direct WebSocket URL (overrides host/port)

        """
        self.host = host
        self.port = port
        self.ws_url = ws_url
        self._cdp: CDPConnection | None = None
        self._actions: BrowserActions | None = None
        self._distiller = DOMDistiller()

    async def connect(self) -> None:
        """Connect to Chrome."""
        if self.ws_url:
            self._cdp = CDPConnection(self.ws_url)
            await self._cdp.connect()
        else:
            self._cdp = await connect_to_chrome(self.host, self.port)

        self._actions = BrowserActions(self._cdp)

        # Enable required domains
        await self._cdp.enable_page()
        await self._cdp.enable_dom()

    async def disconnect(self) -> None:
        """Disconnect from Chrome."""
        if self._cdp:
            await self._cdp.disconnect()

    async def __aenter__(self) -> 'BrowserSession':
        await self.connect()
        return self

    async def __aexit__(self, exc_type: type[BaseException] | None, exc_val: BaseException | None, exc_tb: Any) -> None:
        await self.disconnect()

    # Navigation

    async def navigate(self, url: str) -> ActionResult:
        """Navigate to URL."""
        if not self._actions:
            raise RuntimeError("Not connected")
        return await self._actions.navigate(url)

    async def back(self) -> ActionResult:
        """Go back in history."""
        if not self._cdp:
            raise RuntimeError("Not connected")

        response = await self._cdp.send("Page.navigateHistory", {"delta": -1})
        return ActionResult(
            success=response.success,
            action="back",
            error=str(response.error) if not response.success else None,
        )

    async def forward(self) -> ActionResult:
        """Go forward in history."""
        if not self._cdp:
            raise RuntimeError("Not connected")

        response = await self._cdp.send("Page.navigateHistory", {"delta": 1})
        return ActionResult(
            success=response.success,
            action="forward",
            error=str(response.error) if not response.success else None,
        )

    async def refresh(self) -> ActionResult:
        """Refresh page."""
        if not self._cdp:
            raise RuntimeError("Not connected")

        response = await self._cdp.send("Page.reload")
        return ActionResult(
            success=response.success,
            action="refresh",
            error=str(response.error) if not response.success else None,
        )

    # Interactions

    async def click(self, selector: str) -> ActionResult:
        """Click element."""
        if not self._actions:
            raise RuntimeError("Not connected")
        return await self._actions.click(selector)

    async def type(self, selector: str, text: str) -> ActionResult:
        """Type into input."""
        if not self._actions:
            raise RuntimeError("Not connected")
        return await self._actions.type(selector, text)

    async def select(self, selector: str, value: str) -> ActionResult:
        """Select dropdown option."""
        if not self._actions:
            raise RuntimeError("Not connected")
        return await self._actions.select(selector, value)

    async def scroll(self, x: int = 0, y: int = 0) -> ActionResult:
        """Scroll page."""
        if not self._actions:
            raise RuntimeError("Not connected")
        return await self._actions.scroll(x, y)

    async def wait_for(self, selector: str, timeout: float = 10.0) -> ActionResult:
        """Wait for element."""
        if not self._actions:
            raise RuntimeError("Not connected")
        return await self._actions.wait_for_element(selector, timeout)

    # DOM Extraction

    async def extract_dom(self, depth: int = -1) -> SimplifiedElement | None:
        """Extract and distill the DOM.

        Returns simplified structure for AI consumption.
        """
        if not self._cdp:
            raise RuntimeError("Not connected")

        # Get full document
        response = await self._cdp.send("DOM.getDocument", {"depth": depth})

        if not response.success:
            return None

        result = response.result if isinstance(response.result, dict) else {}
        root = result.get("root")
        if not root:
            return None

        return self._distiller.distill(root)

    def get_interactables(self, dom: SimplifiedElement) -> list[SimplifiedElement]:
        """Get all interactive elements from distilled DOM."""
        return self._distiller.get_interactables(dom)

    def dom_to_text(self, dom: SimplifiedElement) -> str:
        """Convert distilled DOM to text for AI prompts."""
        return self._distiller.to_text(dom)

    def summarize_dom(self, dom: SimplifiedElement) -> dict[str, Any]:
        """Get summary of distilled DOM."""
        return self._distiller.summarize(dom)

    # Screenshots

    async def screenshot(self, format: str = "png") -> Screenshot:
        """Capture page screenshot."""
        if not self._cdp:
            raise RuntimeError("Not connected")
        return await capture_screenshot(self._cdp, format)

    async def screenshot_element(self, selector: str) -> Screenshot:
        """Capture element screenshot."""
        if not self._cdp:
            raise RuntimeError("Not connected")
        return await capture_element(self._cdp, selector)

    # Utilities

    async def get_url(self) -> str:
        """Get current page URL."""
        if not self._cdp:
            raise RuntimeError("Not connected")

        response = await self._cdp.evaluate("window.location.href")
        if response.success:
            value = BrowserActions._extract_response_value(response)
            if value is not None:
                return str(value)
        return ""

    async def get_title(self) -> str:
        """Get page title."""
        if not self._cdp:
            raise RuntimeError("Not connected")

        response = await self._cdp.evaluate("document.title")
        if response.success:
            value = BrowserActions._extract_response_value(response)
            if value is not None:
                return str(value)
        return ""

    async def execute_js(self, script: str) -> Any:
        """Execute JavaScript and return result."""
        if not self._cdp:
            raise RuntimeError("Not connected")

        response = await self._cdp.evaluate(script)
        if response.success:
            return BrowserActions._extract_response_value(response)
        return None
