"""Screenshot Utilities for Browser Automation.

Capture visual snapshots of pages and elements for AI vision analysis.
Based on the insight: "Screenshots + DOM = multimodal understanding"

Usage:
    screenshot = await capture_screenshot(cdp)
    element_shot = await capture_element(cdp, "#login-form")
"""

from __future__ import annotations

import base64
from dataclasses import dataclass, field
from datetime import datetime, timezone
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from .cdp import CDPConnection


def _as_dict(value: dict[str, Any] | None) -> dict[str, Any]:
    return value if isinstance(value, dict) else {}


@dataclass
class Screenshot:
    """Captured screenshot with metadata.

    The image data is base64-encoded PNG or JPEG.
    """

    data: str  # Base64-encoded image
    format: str = "png"
    width: int = 0
    height: int = 0
    timestamp: str = field(default_factory=lambda: datetime.now(timezone.utc).isoformat())
    element_selector: str | None = None  # If element screenshot

    def save(self, path: str) -> None:
        """Save screenshot to file."""
        with open(path, "wb") as f:
            f.write(base64.b64decode(self.data))

    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary."""
        return {
            "format": self.format,
            "width": self.width,
            "height": self.height,
            "timestamp": self.timestamp,
            "element_selector": self.element_selector,
            "data_length": len(self.data),
        }


async def capture_screenshot(
    cdp: 'CDPConnection',
    format: str = "png",
    quality: int = 80,
    full_page: bool = False,
) -> Screenshot:
    """Capture a screenshot of the current page.

    Args:
        cdp: CDP connection to Chrome
        format: Image format ("png" or "jpeg")
        quality: JPEG quality (1-100), ignored for PNG
        full_page: If True, capture entire scrollable page

    Returns:
        Screenshot with base64-encoded image data

    """
    params: dict[str, Any] = {"format": format}

    if format == "jpeg":
        params["quality"] = quality

    if full_page:
        params["captureBeyondViewport"] = True
        params["fromSurface"] = True

    response = await cdp.send("Page.captureScreenshot", params)

    if not response.success:
        raise RuntimeError(f"Screenshot failed: {response.error}")

    response_result = _as_dict(response.result)
    data = str(response_result.get("data", ""))

    # Get viewport dimensions
    layout = await cdp.send("Page.getLayoutMetrics")
    width = height = 0
    if layout.success:
        layout_result = _as_dict(layout.result)
        viewport = _as_dict(layout_result.get("cssVisualViewport"))
        width = int(viewport.get("clientWidth", 0))
        height = int(viewport.get("clientHeight", 0))

    return Screenshot(
        data=data,
        format=format,
        width=width,
        height=height,
    )


async def capture_element(
    cdp: 'CDPConnection',
    selector: str,
    format: str = "png",
    quality: int = 80,
    padding: int = 0,
) -> Screenshot:
    """Capture a screenshot of a specific element.

    Args:
        cdp: CDP connection to Chrome
        selector: CSS selector for the element
        format: Image format ("png" or "jpeg")
        quality: JPEG quality (1-100)
        padding: Extra pixels around element

    Returns:
        Screenshot of the element

    """
    # Get document root
    doc = await cdp.get_document()
    if not doc.success:
        raise RuntimeError("Failed to get document")

    doc_result = _as_dict(doc.result)
    root = _as_dict(doc_result.get("root"))
    root_id = root.get("nodeId")
    if not root_id:
        raise RuntimeError("No root node found")

    # Find element
    elem = await cdp.query_selector(root_id, selector)
    if not elem.success:
        raise RuntimeError(f"Element not found: {selector}")

    elem_result = _as_dict(elem.result)
    node_id = elem_result.get("nodeId")
    if not node_id:
        raise RuntimeError(f"No nodeId for selector: {selector}")

    # Get element bounding box
    box_result = await cdp.send("DOM.getBoxModel", {"nodeId": node_id})
    if not box_result.success:
        raise RuntimeError(f"Failed to get box model: {box_result.error}")

    box_data = _as_dict(box_result.result)
    model = _as_dict(box_data.get("model"))
    content = model.get("content", [])

    if not isinstance(content, list) or len(content) < 8:
        raise RuntimeError("Invalid box model")

    # content is [x1, y1, x2, y2, x3, y3, x4, y4] for quad
    x = min(content[0], content[2], content[4], content[6]) - padding
    y = min(content[1], content[3], content[5], content[7]) - padding
    width = max(content[0], content[2], content[4], content[6]) - x + padding
    height = max(content[1], content[3], content[5], content[7]) - y + padding

    # Capture with clip
    params: dict[str, Any] = {
        "format": format,
        "clip": {
            "x": max(0, x),
            "y": max(0, y),
            "width": width,
            "height": height,
            "scale": 1,
        },
    }

    if format == "jpeg":
        params["quality"] = quality

    response = await cdp.send("Page.captureScreenshot", params)

    if not response.success:
        raise RuntimeError(f"Element screenshot failed: {response.error}")

    return Screenshot(
        data=str(_as_dict(response.result).get("data", "")),
        format=format,
        width=int(width),
        height=int(height),
        element_selector=selector,
    )


async def get_viewport_size(cdp: 'CDPConnection') -> dict[str, int]:
    """Get current viewport dimensions."""
    layout = await cdp.send("Page.getLayoutMetrics")

    if not layout.success:
        return {"width": 0, "height": 0}

    layout_result = _as_dict(layout.result)
    viewport = _as_dict(layout_result.get("cssVisualViewport"))
    return {
        "width": int(viewport.get("clientWidth", 0)),
        "height": int(viewport.get("clientHeight", 0)),
    }


async def set_viewport_size(
    cdp: 'CDPConnection',
    width: int,
    height: int,
    device_scale_factor: float = 1.0,
    mobile: bool = False,
) -> bool:
    """Set viewport dimensions.

    Args:
        cdp: CDP connection
        width: Viewport width in pixels
        height: Viewport height in pixels
        device_scale_factor: Device scale (default 1.0)
        mobile: Emulate mobile device

    Returns:
        True if successful

    """
    response = await cdp.send("Emulation.setDeviceMetricsOverride", {
        "width": width,
        "height": height,
        "deviceScaleFactor": device_scale_factor,
        "mobile": mobile,
    })

    return response.success
