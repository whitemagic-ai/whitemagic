"""Version utilities for WhiteMagic API."""

from functools import lru_cache
from importlib import metadata
from pathlib import Path


@lru_cache(maxsize=1)
def get_version() -> str:
    """Return the installed package version with source fallback.
    """
    version_file = Path(__file__).parent.parent.parent / "VERSION"
    if version_file.exists():
        return version_file.read_text().strip()
    try:
        return metadata.version("whitemagic")
    except metadata.PackageNotFoundError:
        pass
    return "unknown"


def get_version_dict() -> dict:
    """Get version information as dictionary."""
    version = get_version()
    return {
        "version": version,
        "api_version": "v1",
        "revision": version,
    }
