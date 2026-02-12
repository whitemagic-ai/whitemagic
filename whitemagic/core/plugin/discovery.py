"""Plugin discovery."""

import logging
from pathlib import Path
from typing import Any, cast

import yaml

from whitemagic.config.paths import WM_ROOT

from .base import PluginManifest
from .registry import PluginInfo

# Handle tomllib import for different Python versions
try:
    import tomllib  # type: ignore[no-redef]
except ImportError:
    try:
        import tomli as tomllib  # type: ignore[no-redef,import-not-found]
    except ImportError:
        tomllib = None  # type: ignore[assignment]

logger = logging.getLogger(__name__)  # Corrected to logger


class PluginDiscovery:
    """Discover plugins from filesystem."""

    SEARCH_PATHS = [
        WM_ROOT / "plugins",
        Path.cwd() / "plugins",
    ]

    def discover_all(self) -> list[PluginInfo]:
        """Scan all search paths for plugins."""
        discovered = []

        for search_path in self.SEARCH_PATHS:
            if not search_path.exists():
                continue

            logger.debug(f"Scanning {search_path} for plugins")

            for item in search_path.iterdir():
                if not item.is_dir():
                    continue

                # Look for plugin.yaml manifest
                manifest_path = item / "plugin.yaml"
                if not manifest_path.exists():
                    # Try pyproject.toml
                    manifest_path = item / "pyproject.toml"
                    if not manifest_path.exists():
                        continue

                try:
                    manifest_data = self._load_manifest(manifest_path)
                    manifest = self._parse_manifest(manifest_data)

                    info = PluginInfo(
                        name=manifest.name,
                        version=manifest.version,
                        path=item,
                        manifest=manifest,
                        active=False,
                    )

                    discovered.append(info)
                    logger.info(f"Discovered plugin: {manifest.name} v{manifest.version}")

                except Exception as e:
                    logger.warning(f"Failed to load plugin from {item}: {e}")

        return discovered

    def _load_manifest(self, path: Path) -> dict[str, Any]:
        """Load and parse plugin manifest."""
        if path.name == "plugin.yaml":
            with open(path) as f:
                return cast("dict[str, Any]", yaml.safe_load(f) or {})
        elif path.name == "pyproject.toml":
            if tomllib is None:
                logger.warning("TOML support unavailable - install tomli for Python <3.11")
                return {}
            with open(path, "rb") as f:
                data = tomllib.load(f)
                result = data.get("tool", {}).get("whitemagic-plugin", {})
                return cast("dict[str, Any]", result)
        return cast("dict[str, Any]", {})

    def _parse_manifest(self, data: dict[str, Any]) -> PluginManifest:
        """Parse manifest data into PluginManifest."""
        return PluginManifest(
            name=data.get("name", "unknown"),
            version=data.get("version", "0.0.0"),
            author=data.get("author", "Unknown"),
            description=data.get("description", ""),
            license=data.get("license", "MIT"),
            requires=data.get("requires", []),
            extension_points=data.get("extension_points", []),
            config=data.get("config", {}),
        )
