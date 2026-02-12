"""MCP Schema Adapter — sanitizes tool input_schema per client quirks.

Ported from the archived TypeScript MCP server's adaptSchemaForClient().
Different MCP clients (Gemini, DeepSeek, Qwen, Kimi, etc.) have strict
or divergent JSON Schema requirements that cause silent failures.
"""

import copy
import re
from typing import Any

# Client name patterns → adapter keys
_CLIENT_PATTERNS = {
    "gemini":   re.compile(r"gemini|google", re.I),
    "deepseek": re.compile(r"deepseek", re.I),
    "qwen":     re.compile(r"qwen|alibaba", re.I),
    "kimi":     re.compile(r"kimi|moonshot", re.I),
}


def _detect_client(client_name: str) -> str:
    """Map a client name/info string to a known adapter key."""
    for key, pattern in _CLIENT_PATTERNS.items():
        if pattern.search(client_name):
            return key
    return "default"


def _strip_keys(schema: dict[str, Any], keys: set) -> dict[str, Any]:
    """Recursively remove specific keys from a JSON Schema dict."""
    cleaned: dict[str, Any] = {}
    for k, v in schema.items():
        if k in keys:
            continue
        if isinstance(v, dict):
            cleaned[k] = _strip_keys(v, keys)
        elif isinstance(v, list):
            cleaned[k] = [
                _strip_keys(item, keys) if isinstance(item, dict) else item
                for item in v
            ]
        else:
            cleaned[k] = v
    return cleaned


def _adapt_gemini(schema: dict[str, Any]) -> dict[str, Any]:
    """Gemini rejects additionalProperties and chokes on default values."""
    return _strip_keys(schema, {"additionalProperties", "default"})


def _adapt_deepseek(schema: dict[str, Any]) -> dict[str, Any]:
    """DeepSeek struggles with default values and $ref."""
    return _strip_keys(schema, {"default", "$ref"})


def _adapt_qwen(schema: dict[str, Any]) -> dict[str, Any]:
    """Qwen/Alibaba: strip additionalProperties."""
    return _strip_keys(schema, {"additionalProperties"})


def _adapt_kimi(schema: dict[str, Any]) -> dict[str, Any]:
    """Kimi/Moonshot: strip default and additionalProperties."""
    return _strip_keys(schema, {"additionalProperties", "default"})


_ADAPTERS = {
    "gemini":   _adapt_gemini,
    "deepseek": _adapt_deepseek,
    "qwen":     _adapt_qwen,
    "kimi":     _adapt_kimi,
}


def adapt_schema(schema: dict[str, Any], client_name: str | None = None) -> dict[str, Any]:
    """Return a client-safe copy of the input schema.

    Args:
        schema: The original tool input_schema dict.
        client_name: Client identifier from MCP InitializeRequest (or env).

    Returns:
        A (possibly modified) copy safe for the target client.

    """
    if not client_name:
        return schema

    client_key = _detect_client(client_name)
    adapter = _ADAPTERS.get(client_key)
    if not adapter:
        return schema

    return adapter(copy.deepcopy(schema))
