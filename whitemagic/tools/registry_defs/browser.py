"""Registry definitions for browser and web research tools."""
from whitemagic.tools.tool_types import ToolCategory, ToolDefinition, ToolSafety

BROWSER_TOOLS: list[ToolDefinition] = [
    # --- Existing browser tools ---
    ToolDefinition(
        name="browser_navigate",
        description="Navigate the browser to a URL using Chrome DevTools Protocol.",
        category=ToolCategory.BROWSER,
        safety=ToolSafety.READ,
        input_schema={
            "type": "object",
            "properties": {
                "url": {"type": "string", "description": "URL to navigate to"},
            },
            "required": ["url"],
        },
    ),
    ToolDefinition(
        name="browser_click",
        description="Click an element on the page by CSS selector.",
        category=ToolCategory.BROWSER,
        safety=ToolSafety.WRITE,
        input_schema={
            "type": "object",
            "properties": {
                "selector": {"type": "string", "description": "CSS selector for element to click"},
            },
            "required": ["selector"],
        },
    ),
    ToolDefinition(
        name="browser_type",
        description="Type text into an input element by CSS selector.",
        category=ToolCategory.BROWSER,
        safety=ToolSafety.WRITE,
        input_schema={
            "type": "object",
            "properties": {
                "selector": {"type": "string", "description": "CSS selector for input element"},
                "text": {"type": "string", "description": "Text to type"},
            },
            "required": ["selector", "text"],
        },
    ),
    ToolDefinition(
        name="browser_extract_dom",
        description="Extract and distill the DOM of the current page for AI consumption.",
        category=ToolCategory.BROWSER,
        safety=ToolSafety.READ,
        input_schema={"type": "object", "properties": {}},
    ),
    ToolDefinition(
        name="browser_screenshot",
        description="Capture a screenshot of the current page.",
        category=ToolCategory.BROWSER,
        safety=ToolSafety.READ,
        input_schema={"type": "object", "properties": {}},
    ),
    ToolDefinition(
        name="browser_get_interactables",
        description="Get all interactive elements (buttons, links, inputs) from the current page.",
        category=ToolCategory.BROWSER,
        safety=ToolSafety.READ,
        input_schema={"type": "object", "properties": {}},
    ),

    # --- New web research tools ---
    ToolDefinition(
        name="web_fetch",
        description="Fetch a URL and return clean text content. Fast httpx-based fetcher — no browser needed. "
                    "Converts HTML to clean text optimized for AI token usage. Use this for reading articles, docs, and web pages.",
        category=ToolCategory.BROWSER,
        safety=ToolSafety.READ,
        input_schema={
            "type": "object",
            "properties": {
                "url": {"type": "string", "description": "URL to fetch"},
                "max_chars": {"type": "integer", "description": "Maximum characters to return (default 30000)", "default": 30000},
                "timeout": {"type": "number", "description": "Request timeout in seconds (default 15)", "default": 15.0},
            },
            "required": ["url"],
        },
    ),
    ToolDefinition(
        name="web_search",
        description="Search the web using DuckDuckGo. No API key needed. Returns titles, URLs, and snippets. "
                    "Use this to find information, articles, documentation, code examples, or anything on the public web.",
        category=ToolCategory.BROWSER,
        safety=ToolSafety.READ,
        input_schema={
            "type": "object",
            "properties": {
                "query": {"type": "string", "description": "Search query"},
                "num_results": {"type": "integer", "description": "Number of results to return (default 8)", "default": 8},
                "timeout": {"type": "number", "description": "Request timeout in seconds (default 10)", "default": 10.0},
            },
            "required": ["query"],
        },
    ),
    ToolDefinition(
        name="web_search_and_read",
        description="Search the web AND fetch content from top results in one call. "
                    "Combines web_search + web_fetch for the most common research pattern. "
                    "Returns search results with full page content for top hits.",
        category=ToolCategory.BROWSER,
        safety=ToolSafety.READ,
        input_schema={
            "type": "object",
            "properties": {
                "query": {"type": "string", "description": "Search query"},
                "num_results": {"type": "integer", "description": "Number of search results (default 5)", "default": 5},
                "max_fetch": {"type": "integer", "description": "Max results to fetch content from (default 3)", "default": 3},
                "max_chars_per_page": {"type": "integer", "description": "Max chars per page (default 15000)", "default": 15000},
            },
            "required": ["query"],
        },
    ),
    ToolDefinition(
        name="research_topic",
        description="Deep research on a topic: search → fetch top results → extract key points → synthesize. "
                    "Single-call replacement for multi-step search workflows. "
                    "Returns findings with full content, synthesis, and related topics for further exploration.",
        category=ToolCategory.BROWSER,
        safety=ToolSafety.READ,
        input_schema={
            "type": "object",
            "properties": {
                "topic": {"type": "string", "description": "Research topic or question"},
                "num_search_results": {"type": "integer", "description": "How many search results to get (default 6)", "default": 6},
                "max_sources": {"type": "integer", "description": "How many top results to fetch (default 4)", "default": 4},
                "max_chars_per_source": {"type": "integer", "description": "Max chars per source (default 15000)", "default": 15000},
            },
            "required": ["topic"],
        },
    ),
    ToolDefinition(
        name="browser_session_status",
        description="Get the status of the persistent browser session (CDP connection).",
        category=ToolCategory.BROWSER,
        safety=ToolSafety.READ,
        input_schema={"type": "object", "properties": {}},
    ),
]
