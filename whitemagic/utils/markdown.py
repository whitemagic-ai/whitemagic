"""Markdown utilities"""
def clean_markdown(text: str) -> str:
    """Clean markdown text"""
    return text.strip()

def create_frontmatter(data: dict) -> str:
    """Create YAML frontmatter"""
    lines = ["---"]
    for k, v in data.items():
        lines.append(f"{k}: {v}")
    lines.append("---")
    return "\n".join(lines)

def parse_memory_content(text: str) -> dict:
    """Parse memory file content"""
    return {"content": text}

def sanitize_filename(name: str) -> str:
    """Sanitize filename"""
    return name.replace(" ", "_").replace("/", "-")

def create_preview(text: str, length: int = 100) -> str:
    """Create preview of text"""
    return text[:length] + "..." if len(text) > length else text

def normalize_tags(tags: list) -> list:
    """Normalize tag list"""
    return [str(t).lower().strip() for t in tags if t]
