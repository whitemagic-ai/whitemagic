"""Plugin base classes."""

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Any


@dataclass
class PluginManifest:
    """Plugin metadata."""

    name: str
    version: str
    author: str = "Unknown"
    description: str = ""
    license: str = "MIT"
    requires: list[str] = field(default_factory=list)
    extension_points: list[str] = field(default_factory=list)
    config: dict[str, Any] = field(default_factory=dict)


class Plugin(ABC):
    """Base class for all plugins."""

    manifest: PluginManifest

    @abstractmethod
    def activate(self) -> None:
        """Called when plugin is enabled."""
        pass

    def deactivate(self) -> None:
        """Called when plugin is disabled."""
        pass

    def configure(self, config: dict[str, Any]) -> None:
        """Update plugin configuration."""
        pass
