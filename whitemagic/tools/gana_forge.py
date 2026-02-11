"""Gana Forge — Declarative Tool Extension Protocol.
====================================================
Implements the extensibility gap identified via Mahābhārata 12.108.17:

  "Establish dharmic procedures grounded in śāstra, then follow them."

Currently, adding a tool to a Gana requires modifying 5+ hardcoded source
files. The Forge allows any AI (or human) to define new tools via YAML
manifests in $WM_STATE_ROOT/extensions/, which are loaded at runtime and
injected into the PRAT routing without source code changes.

Manifests are validated by the Dharma engine before acceptance.

Lifecycle:
  1. Write YAML manifest to extensions/ directory
  2. Forge discovers and validates on startup (or via reload)
  3. Valid tools are injected into DISPATCH_TABLE and TOOL_TO_GANA
  4. Invalid manifests are logged but do not block startup
"""

import logging
import os
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)

# Default extensions directory
_DEFAULT_EXT_DIR = Path(
    os.environ.get("WM_STATE_ROOT", str(Path.home() / ".whitemagic"))
) / "extensions"

# Valid safety levels (must match registry SafetyLevel)
_VALID_SAFETY = {"read", "write", "delete", "admin"}

# All known Gana names
_VALID_GANAS = {
    "gana_horn", "gana_neck", "gana_root", "gana_room", "gana_heart",
    "gana_tail", "gana_winnowing_basket", "gana_ghost", "gana_willow",
    "gana_star", "gana_extended_net", "gana_wings", "gana_chariot",
    "gana_abundance", "gana_straddling_legs", "gana_mound", "gana_stomach",
    "gana_hairy_head", "gana_net", "gana_turtle_beak", "gana_three_stars",
    "gana_dipper", "gana_ox", "gana_girl", "gana_void", "gana_roof",
    "gana_encampment", "gana_wall",
}


def _parse_manifest(path: Path) -> dict[str, Any] | None:
    """Parse a YAML manifest file. Returns None on failure."""
    try:
        import yaml
    except ImportError:
        # PyYAML not installed — try json fallback
        import json
        try:
            data: dict[str, Any] = json.loads(path.read_text(encoding="utf-8"))
            return data
        except Exception as e:
            logger.warning("Forge: failed to parse %s as JSON: %s", path, e)
            return None

    try:
        data = yaml.safe_load(path.read_text(encoding="utf-8"))
        if not isinstance(data, dict):
            logger.warning("Forge: %s is not a dict", path)
            return None
        return data
    except Exception as e:
        logger.warning("Forge: failed to parse %s: %s", path, e)
        return None


def _validate_manifest(manifest: dict[str, Any], path: Path) -> list[str]:
    """Validate a tool manifest. Returns list of errors (empty = valid)."""
    errors: list[str] = []
    tool = manifest.get("tool", {})

    if not isinstance(tool, dict):
        return [f"{path}: 'tool' must be a dict"]

    name = tool.get("name")
    if not name or not isinstance(name, str):
        errors.append(f"{path}: 'tool.name' is required (string)")

    description = tool.get("description")
    if not description or not isinstance(description, str):
        errors.append(f"{path}: 'tool.description' is required (string)")

    gana = tool.get("gana")
    if not gana or gana not in _VALID_GANAS:
        errors.append(
            f"{path}: 'tool.gana' must be one of the 28 canonical Ganas, got '{gana}'"
        )

    safety = tool.get("safety", "read")
    if safety not in _VALID_SAFETY:
        errors.append(f"{path}: 'tool.safety' must be one of {_VALID_SAFETY}, got '{safety}'")

    handler = tool.get("handler")
    if not handler or not isinstance(handler, str):
        errors.append(f"{path}: 'tool.handler' is required (string, e.g. 'mymod:my_func')")

    return errors


def _resolve_handler(handler_spec: str) -> Any:
    """Resolve a handler specification like 'module.path:function_name'.

    Supports two formats:
    - 'module.path:function_name' — importlib resolution
    - 'builtin:echo' — returns a simple echo handler for testing
    """
    if handler_spec == "builtin:echo":
        def _echo_handler(**kwargs: Any) -> dict[str, Any]:
            return {"status": "success", "echo": kwargs}
        return _echo_handler

    if ":" not in handler_spec:
        logger.warning("Forge: handler '%s' must use 'module:function' format", handler_spec)
        return None

    module_path, func_name = handler_spec.rsplit(":", 1)
    try:
        import importlib
        mod = importlib.import_module(module_path)
        return getattr(mod, func_name)
    except Exception as e:
        logger.warning("Forge: failed to resolve handler '%s': %s", handler_spec, e)
        return None


def discover_extensions(ext_dir: Path | None = None) -> list[dict[str, Any]]:
    """Discover all YAML/JSON manifests in the extensions directory."""
    directory = ext_dir or _DEFAULT_EXT_DIR
    if not directory.exists():
        return []

    manifests: list[dict[str, Any]] = []
    for path in sorted(directory.iterdir()):
        if path.suffix in (".yaml", ".yml", ".json") and path.is_file():
            manifest = _parse_manifest(path)
            if manifest is not None:
                manifest["_source_path"] = str(path)
                manifests.append(manifest)

    return manifests


def load_extensions(ext_dir: Path | None = None) -> dict[str, Any]:
    """Load and validate all extension manifests.

    Returns:
        Summary dict with loaded tools, errors, and injection results.
    """
    manifests = discover_extensions(ext_dir)
    loaded: list[dict[str, Any]] = []
    errors: list[str] = []
    skipped: list[str] = []

    for manifest in manifests:
        source = manifest.get("_source_path", "unknown")
        validation_errors = _validate_manifest(manifest, Path(source))

        if validation_errors:
            errors.extend(validation_errors)
            skipped.append(source)
            continue

        tool = manifest["tool"]
        handler = _resolve_handler(tool["handler"])
        if handler is None:
            errors.append(f"{source}: handler '{tool['handler']}' could not be resolved")
            skipped.append(source)
            continue

        # Inject into dispatch table and PRAT mapping
        injected = _inject_tool(
            name=tool["name"],
            handler=handler,
            gana=tool["gana"],
            description=tool.get("description", ""),
            safety=tool.get("safety", "read"),
        )

        if injected:
            loaded.append({
                "name": tool["name"],
                "gana": tool["gana"],
                "safety": tool.get("safety", "read"),
                "source": source,
            })
            logger.info("Forge: loaded extension tool '%s' → %s", tool["name"], tool["gana"])
        else:
            errors.append(f"{source}: injection failed for '{tool['name']}'")
            skipped.append(source)

    return {
        "status": "success",
        "loaded": len(loaded),
        "skipped": len(skipped),
        "tools": loaded,
        "errors": errors,
    }


def _inject_tool(
    name: str,
    handler: Any,
    gana: str,
    description: str = "",
    safety: str = "read",
) -> bool:
    """Inject a tool into the dispatch table and PRAT mapping.

    This is the core extension mechanism — it modifies the runtime
    routing tables without touching source files.
    """
    try:
        # 1. Inject into DISPATCH_TABLE
        from whitemagic.tools.dispatch_table import DISPATCH_TABLE
        if name in DISPATCH_TABLE:
            logger.warning("Forge: tool '%s' already in dispatch table, skipping", name)
            return False
        DISPATCH_TABLE[name] = handler

        # 2. Inject into PRAT TOOL_TO_GANA mapping
        from whitemagic.tools.prat_router import TOOL_TO_GANA, GANA_TO_TOOLS
        if name not in TOOL_TO_GANA:
            TOOL_TO_GANA[name] = gana
        if gana in GANA_TO_TOOLS:
            if name not in GANA_TO_TOOLS[gana]:
                GANA_TO_TOOLS[gana].append(name)
        else:
            GANA_TO_TOOLS[gana] = [name]

        return True
    except Exception as e:
        logger.warning("Forge: injection failed for '%s': %s", name, e)
        return False


# ─── MCP Handlers ────────────────────────────────────────────────

def handle_forge_status(**kwargs: Any) -> dict[str, Any]:
    """Show current Forge status — loaded extensions and available manifests."""
    ext_dir = _DEFAULT_EXT_DIR
    manifests = discover_extensions(ext_dir)

    # Check what's already loaded
    loaded_names: list[str] = []
    try:
        from whitemagic.tools.prat_router import TOOL_TO_GANA
        # Find tools that aren't in the original hardcoded set
        # (We can't perfectly distinguish, but extensions typically have dots)
        loaded_names = [
            name for name in TOOL_TO_GANA
            if name.startswith("ext.") or name.startswith("custom.")
        ]
    except Exception:
        pass

    return {
        "status": "success",
        "extensions_dir": str(ext_dir),
        "extensions_dir_exists": ext_dir.exists(),
        "manifests_found": len(manifests),
        "manifest_files": [m.get("_source_path", "?") for m in manifests],
        "loaded_extension_tools": loaded_names,
        "usage": (
            "Place YAML manifests in ~/.whitemagic/extensions/ with format:\n"
            "tool:\n"
            "  name: custom.my_tool\n"
            "  description: What it does\n"
            "  gana: gana_ghost\n"
            "  safety: read\n"
            "  handler: 'my_module:my_function'"
        ),
    }


def handle_forge_reload(**kwargs: Any) -> dict[str, Any]:
    """Reload all extension manifests from disk."""
    return load_extensions()


def handle_forge_validate(**kwargs: Any) -> dict[str, Any]:
    """Validate extension manifests without loading them."""
    manifests = discover_extensions()
    results: list[dict[str, Any]] = []

    for manifest in manifests:
        source = manifest.get("_source_path", "unknown")
        validation_errors = _validate_manifest(manifest, Path(source))
        results.append({
            "file": source,
            "valid": len(validation_errors) == 0,
            "errors": validation_errors,
            "tool_name": manifest.get("tool", {}).get("name", "?"),
            "gana": manifest.get("tool", {}).get("gana", "?"),
        })

    valid_count = sum(1 for r in results if r["valid"])
    return {
        "status": "success",
        "total": len(results),
        "valid": valid_count,
        "invalid": len(results) - valid_count,
        "results": results,
    }
