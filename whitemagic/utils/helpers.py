"""Helper utilities"""
from datetime import datetime


def now_iso() -> str:
    """Get current time in ISO format"""
    return datetime.now().isoformat()

def format_date(dt: str) -> str:
    """Format date string"""
    return dt

def calculate_ttl_days(created: str, ttl_days: int) -> int:
    """Calculate TTL days"""
    return ttl_days


def slugify(text: str) -> str:
    """Create slug from text"""
    return text.lower().replace(" ", "-")

def split_frontmatter(text: str) -> tuple:
    """Split frontmatter from content"""
    if text.startswith("---"):
        parts = text.split("---", 2)
        return (parts[1] if len(parts) > 1 else "", parts[2] if len(parts) > 2 else text)
    return ("", text)

def summarize_text(text: str, length: int = 100) -> str:
    """Summarize text"""
    return text[:length] + "..." if len(text) > length else text

def truncate_text(text: str, length: int = 50) -> str:
    """Truncate text"""
    return text[:length]
