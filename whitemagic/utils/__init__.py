"""WhiteMagic Utilities Package.
"""

from .core import (
    create_frontmatter,
    create_preview,
    format_size,
    normalize_tags,
    now_iso,
    parse_datetime,
    parse_frontmatter,
    serialize_frontmatter,
    slugify,
    split_frontmatter,
    summarize_text,
    truncate_text,
)
from .fileio import atomic_write, file_lock
from .import_optimizer import (
    LazyImport,
    lazy_import,
    optimize_imports,
    optimize_standard_imports,
)
from .observability import get_tracker, track_metric
from .text_cleaning import clean_markdown

__all__ = [
    "LazyImport",
    "atomic_write",
    "clean_markdown",
    "create_frontmatter",
    "create_preview",
    "file_lock",
    "format_size",
    "get_tracker",
    "lazy_import",
    "normalize_tags",
    "now_iso",
    "optimize_imports",
    "optimize_standard_imports",
    "parse_datetime",
    "parse_frontmatter",
    "serialize_frontmatter",
    "slugify",
    "split_frontmatter",
    "summarize_text",
    "track_metric",
    "truncate_text",
]
