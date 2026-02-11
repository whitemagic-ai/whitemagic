"""DOM Distiller - Simplify DOM for AI Understanding.

Converts complex DOM trees into simplified structures that AI can reason about.
Based on the insight: "DOM distillation strips CSS/SVG, keeps interactables"

The goal is to reduce token usage while preserving:
- Interactive elements (buttons, links, inputs)
- Text content
- Semantic structure
- Element identifiers for action targeting
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any


class ElementType(Enum):
    """Types of simplified elements."""

    INTERACTIVE = auto()  # Clickable, typeable
    TEXT = auto()         # Text content
    CONTAINER = auto()    # Structural (div, section)
    FORM = auto()         # Form elements
    NAVIGATION = auto()   # Nav, menu
    MEDIA = auto()        # Images, videos
    IGNORE = auto()       # CSS, SVG, scripts


@dataclass
class SimplifiedElement:
    """A simplified DOM element for AI consumption.

    Contains only the information needed for:
    - Understanding page structure
    - Targeting elements for actions
    - Extracting content
    """

    tag: str
    element_type: ElementType
    selector: str  # CSS selector to target this element
    text: str = ""
    attributes: dict[str, str] = field(default_factory=dict)
    children: list["SimplifiedElement"] = field(default_factory=list)
    bounds: dict[str, float] | None = None  # x, y, width, height

    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary for serialization."""
        return {
            "tag": self.tag,
            "type": self.element_type.name,
            "selector": self.selector,
            "text": self.text[:200] if self.text else "",  # Truncate long text
            "attributes": self.attributes,
            "children": [c.to_dict() for c in self.children],
            "bounds": self.bounds,
        }

    def to_compact(self) -> str:
        """Compact string representation for AI.

        Format: [tag#id.class] "text" (selector)
        """
        parts = [f"[{self.tag}"]

        if "id" in self.attributes:
            parts.append(f"#{self.attributes['id']}")
        if "class" in self.attributes:
            classes = self.attributes["class"].split()[:2]  # First 2 classes
            parts.extend(f".{c}" for c in classes)

        parts.append("]")

        if self.text:
            text = self.text[:50].replace("\n", " ").strip()
            parts.append(f' "{text}"')

        if self.element_type == ElementType.INTERACTIVE:
            parts.append(f" → {self.selector}")

        return "".join(parts)


class DOMDistiller:
    """Distill complex DOM trees into AI-friendly structures.

    Strategies:
    1. Remove noise (scripts, styles, SVG internals)
    2. Preserve interactables with selectors
    3. Collapse redundant containers
    4. Extract meaningful text
    5. Generate stable CSS selectors
    """

    # Tags to completely ignore
    IGNORE_TAGS = {
        "script", "style", "noscript", "svg", "path", "g", "defs",
        "clippath", "mask", "symbol", "use", "lineargradient",
        "radialgradient", "filter", "fegaussianblur", "fecolormatrix",
        "template", "slot",
    }

    # Tags that are interactive
    INTERACTIVE_TAGS = {
        "a", "button", "input", "select", "textarea", "label",
        "option", "details", "summary", "dialog",
    }

    # Tags that are form-related
    FORM_TAGS = {"form", "fieldset", "legend", "optgroup"}

    # Tags that are navigational
    NAV_TAGS = {"nav", "menu", "menuitem"}

    # Tags that are media
    MEDIA_TAGS = {"img", "video", "audio", "picture", "source", "canvas"}

    # Attributes to preserve
    PRESERVE_ATTRS = {
        "id", "class", "href", "src", "alt", "title", "name",
        "type", "value", "placeholder", "aria-label", "role",
        "data-testid", "data-id",
    }

    def __init__(self, max_depth: int = 10, max_text_length: int = 500):
        """Initialize distiller.

        Args:
            max_depth: Maximum nesting depth to process
            max_text_length: Maximum text content to preserve per element

        """
        self.max_depth = max_depth
        self.max_text_length = max_text_length
        self._selector_counts: dict[str, int] = {}

    def distill(self, dom_node: dict[str, Any], depth: int = 0) -> SimplifiedElement | None:
        """Distill a DOM node into a simplified element.

        Args:
            dom_node: Raw DOM node from CDP
            depth: Current nesting depth

        Returns:
            SimplifiedElement or None if should be ignored

        """
        if depth > self.max_depth:
            return None

        node_type = dom_node.get("nodeType", 1)
        node_name = dom_node.get("nodeName", "").lower()

        # Skip ignored tags
        if node_name in self.IGNORE_TAGS:
            return None

        # Handle text nodes
        if node_type == 3:  # TEXT_NODE
            text = dom_node.get("nodeValue", "").strip()
            if text:
                return SimplifiedElement(
                    tag="#text",
                    element_type=ElementType.TEXT,
                    selector="",
                    text=text[:self.max_text_length],
                )
            return None

        # Skip non-element nodes
        if node_type != 1:  # ELEMENT_NODE
            return None

        # Determine element type
        element_type = self._classify_element(node_name, dom_node)

        # Extract relevant attributes
        attributes = self._extract_attributes(dom_node)

        # Generate CSS selector
        selector = self._generate_selector(node_name, attributes, dom_node)

        # Process children
        children = []
        for child in dom_node.get("children", []):
            child_elem = self.distill(child, depth + 1)
            if child_elem:
                children.append(child_elem)

        # Extract text content (direct text, not from children)
        text = self._extract_text(dom_node)

        # Skip empty containers with no interactive descendants
        if (element_type == ElementType.CONTAINER and
            not text and
            not any(c.element_type == ElementType.INTERACTIVE for c in children)):
            # Return children directly, collapsing this container
            if len(children) == 1:
                return children[0]
            elif len(children) == 0:
                return None

        return SimplifiedElement(
            tag=node_name,
            element_type=element_type,
            selector=selector,
            text=text,
            attributes=attributes,
            children=children,
        )

    def _classify_element(self, tag: str, node: dict[str, Any]) -> ElementType:
        """Classify element type."""
        if tag in self.IGNORE_TAGS:
            return ElementType.IGNORE
        if tag in self.INTERACTIVE_TAGS:
            return ElementType.INTERACTIVE
        if tag in self.FORM_TAGS:
            return ElementType.FORM
        if tag in self.NAV_TAGS:
            return ElementType.NAVIGATION
        if tag in self.MEDIA_TAGS:
            return ElementType.MEDIA

        # Check for interactive attributes
        attrs = node.get("attributes", [])
        attr_dict = dict(zip(attrs[::2], attrs[1::2])) if attrs else {}

        if attr_dict.get("onclick") or attr_dict.get("role") in ("button", "link"):
            return ElementType.INTERACTIVE

        return ElementType.CONTAINER

    def _extract_attributes(self, node: dict[str, Any]) -> dict[str, str]:
        """Extract relevant attributes."""
        attrs = node.get("attributes", [])
        if not attrs:
            return {}

        # Convert list to dict
        attr_dict = dict(zip(attrs[::2], attrs[1::2]))

        # Filter to preserved attributes
        return {k: v for k, v in attr_dict.items() if k in self.PRESERVE_ATTRS}

    def _generate_selector(
        self,
        tag: str,
        attributes: dict[str, str],
        node: dict[str, Any],
    ) -> str:
        """Generate a CSS selector for this element.

        Priority:
        1. ID (unique)
        2. data-testid (stable)
        3. Unique class combination
        4. Tag with nth-of-type
        """
        # Try ID
        if "id" in attributes:
            return f"#{attributes['id']}"

        # Try data-testid
        if "data-testid" in attributes:
            return f'[data-testid="{attributes["data-testid"]}"]'

        # Try unique class combination
        if "class" in attributes:
            classes = attributes["class"].split()
            if classes:
                # Use first meaningful class
                for cls in classes:
                    if not cls.startswith("_") and len(cls) > 2:
                        selector = f"{tag}.{cls}"
                        return selector

        # Fallback to tag with index
        self._selector_counts[tag] = self._selector_counts.get(tag, 0) + 1
        count = self._selector_counts[tag]
        return f"{tag}:nth-of-type({count})"

    def _extract_text(self, node: dict[str, Any]) -> str:
        """Extract direct text content."""
        texts = []
        for child in node.get("children", []):
            if child.get("nodeType") == 3:  # TEXT_NODE
                text = child.get("nodeValue", "").strip()
                if text:
                    texts.append(text)

        combined = " ".join(texts)
        return combined[:self.max_text_length] if combined else ""

    def to_text(self, element: SimplifiedElement, indent: int = 0) -> str:
        """Convert distilled DOM to text representation.

        Good for including in AI prompts.
        """
        lines = []
        prefix = "  " * indent

        # Add element line
        lines.append(f"{prefix}{element.to_compact()}")

        # Add children
        for child in element.children:
            lines.append(self.to_text(child, indent + 1))

        return "\n".join(lines)

    def get_interactables(self, element: SimplifiedElement) -> list[SimplifiedElement]:
        """Extract all interactive elements.

        Returns flat list of buttons, links, inputs, etc.
        """
        result = []

        if element.element_type == ElementType.INTERACTIVE:
            result.append(element)

        for child in element.children:
            result.extend(self.get_interactables(child))

        return result

    def summarize(self, element: SimplifiedElement) -> dict[str, Any]:
        """Generate a summary of the distilled DOM.

        Useful for understanding page structure at a glance.
        """
        interactables = self.get_interactables(element)

        # Categorize interactables
        buttons = [e for e in interactables if e.tag in ("button", "input") and e.attributes.get("type") in ("button", "submit")]
        links = [e for e in interactables if e.tag == "a"]
        inputs = [e for e in interactables if e.tag in ("input", "textarea", "select")]

        return {
            "total_interactables": len(interactables),
            "buttons": len(buttons),
            "links": len(links),
            "inputs": len(inputs),
            "top_level_tag": element.tag,
            "button_labels": [b.text[:30] for b in buttons[:5]],
            "link_texts": [link.text[:30] for link in links[:5]],
            "input_names": [i.attributes.get("name", i.attributes.get("placeholder", ""))[:30] for i in inputs[:5]],
        }
