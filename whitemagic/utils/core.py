"""WhiteMagic core utility functions.
Standardized robust datetime parsing, markdown utilities, and resource formatting.
"""
import re
from datetime import datetime
from typing import Any

import yaml


def parse_datetime(val: Any) -> datetime:
    """Robustly parse datetime from string or return if already datetime."""
    if isinstance(val, datetime):
        return val
    if isinstance(val, str):
        try:
            # Handle 'Z' suffix for ISO 8601
            return datetime.fromisoformat(val.replace("Z", "+00:00"))
        except (ValueError, TypeError):
            # Fallback for other formats
            return datetime.now()
    return datetime.now()

def format_datetime(dt: datetime | None = None) -> str:
    """Format datetime to ISO string."""
    if dt is None:
        dt = datetime.now()
    return dt.isoformat()

def now_iso() -> str:
    """Get current timestamp in ISO format."""
    return datetime.now().isoformat(timespec="seconds")

def slugify(text: str, max_length: int = 50) -> str:
    """Convert text to a URL-safe slug."""
    text = text.lower().strip()
    text = re.sub(r"[^\w\s-]", "", text)
    text = re.sub(r"[-\s]+", "_", text)
    return text[:max_length] or "memory"

def normalize_tags(tags: list[str], normalize: bool = True) -> list[str]:
    """Normalize tags to lowercase and remove duplicates."""
    if not normalize:
        return [tag.strip() for tag in tags if tag.strip()]
    normalized = []
    seen = set()
    for tag in tags:
        normalized_tag = tag.strip().lower()
        if normalized_tag and normalized_tag not in seen:
            normalized.append(normalized_tag)
            seen.add(normalized_tag)
    return normalized

def clean_markdown(text: str) -> str:
    """Clean markdown formatting for context generation."""
    stripped = text.strip()
    stripped = re.sub(r"^#{1,6}\s*", "", stripped, flags=re.MULTILINE)
    stripped = stripped.replace("**", "").replace("*", "").replace("__", "").replace("_", "")
    stripped = re.sub(r"`[^`]+`", "", stripped)
    stripped = re.sub(r"\s+", " ", stripped)
    return stripped.strip()

def truncate_text(text: str, max_chars: int) -> str:
    """Truncate text to maximum characters."""
    if not max_chars or len(text) <= max_chars:
        return text
    return text[: max_chars - 1].rstrip() + "…"

def summarize_text(text: str, max_words: int) -> str:
    """Summarize text to maximum words."""
    words = text.split()
    if len(words) <= max_words:
        return text
    return " ".join(words[:max_words]) + "…"

def parse_frontmatter(block: str) -> dict[str, Any]:
    """Parse YAML-style frontmatter block."""
    try:
        data = yaml.safe_load(block)
        return data if isinstance(data, dict) else {}
    except (yaml.YAMLError, Exception):
        return {}

def split_frontmatter(raw: str) -> tuple[dict[str, Any], str]:
    """Split markdown file into frontmatter and body."""
    raw = raw.strip()
    if raw.startswith("---"):
        parts = raw.split("---", 2)
        if len(parts) >= 3:
            return parse_frontmatter(parts[1]), parts[2].strip()
    return {}, raw

def create_frontmatter(title: str, timestamp: datetime, tags: list[str], extra_fields: dict[str, Any] | None = None) -> str:
    """Create YAML-style frontmatter block."""
    lines = ["---"]
    lines.append(f"title: {title}")
    lines.append(f"created: {timestamp.isoformat(timespec='seconds')}Z")
    if tags:
        tags_str = ", ".join(tags)
        lines.append(f"tags: [{tags_str}]")
    if extra_fields:
        for key, value in extra_fields.items():
            if key not in ("title", "created", "tags"):
                lines.append(f"{key}: {value}")
    lines.append("---")
    return "\n".join(lines)

def serialize_frontmatter(frontmatter: dict[str, Any], body: str) -> str:
    """Serialize a frontmatter dictionary and body back into a markdown file."""
    lines = ["---"]
    yaml_content = yaml.dump(frontmatter, default_flow_style=False, allow_unicode=True, sort_keys=False)
    lines.append(yaml_content.rstrip())
    lines.append("---")
    lines.append("")
    lines.append(body)
    return "\n".join(lines)

def create_preview(text: str, max_chars: int = 240) -> str:
    """Create a preview snippet from text."""
    text = text.strip()
    text = text.replace("\n", " ")
    text = re.sub(r"\s+", " ", text)
    return truncate_text(text, max_chars)

def format_size(bytes_size: int) -> str:
    """Format byte size in human-readable format."""
    size = float(bytes_size)
    for unit in ["B", "KB", "MB", "GB", "TB"]:
        if size < 1024.0:
            return f"{size:.1f} {unit}"
        size /= 1024.0
    return f"{size:.1f} PB"
