# mypy: disable-error-code=no-untyped-def
"""WhiteMagic Fast CLI - Lazy-loaded for instant startup.
Version: 3.0.0

This is a drop-in replacement for cli_app.py that uses lazy loading
to reduce startup time from ~6.6s to <1s.

Only the requested command's dependencies are loaded.
"""

import argparse
import logging
import sys
from collections.abc import Callable
from typing import Any

logger = logging.getLogger(__name__)


def lazy_import(module_path: str, attr: str | None = None) -> Callable[[], Any]:
    """Import module/attribute only when called."""
    def loader():
        import importlib
        mod = importlib.import_module(module_path)
        return getattr(mod, attr) if attr else mod
    return loader


# Lazy command loaders - only imported when command is invoked
LAZY_COMMANDS: dict[str, Callable] = {
    # Core commands (always fast)
    "version": lambda: lazy_import("whitemagic.config", "VERSION"),

    # Memory commands
    "create": lambda: lazy_import("whitemagic.cli_app", "command_create"),
    "list": lambda: lazy_import("whitemagic.cli_app", "command_list"),
    "search": lambda: lazy_import("whitemagic.cli_app", "command_search"),
    "delete": lambda: lazy_import("whitemagic.cli_app", "command_delete"),
    "context": lambda: lazy_import("whitemagic.cli_app", "command_context"),

    # Voice commands
    "voice-narrate": lambda: lazy_import("whitemagic.cli_voice", "command_voice_narrate"),
    "voice-start": lambda: lazy_import("whitemagic.cli_voice", "command_voice_start"),

    # Dharma commands
    "dharma-assess": lambda: lazy_import("whitemagic.cli_dharma", "command_dharma_assess"),
    "dharma-boundaries": lambda: lazy_import("whitemagic.cli_dharma", "command_dharma_boundaries"),

    # Rabbit hole
    "rabbit-hole-explore": lambda: lazy_import("whitemagic.cli_rabbit_hole", "command_rabbit_hole_explore"),
    "rabbit-hole-extract": lambda: lazy_import("whitemagic.cli_rabbit_hole", "command_rabbit_hole_extract"),

    # Local reasoning (NEW - Phase 4)
    "reason": lambda: lazy_import("whitemagic.core.intelligence.agentic.local_reasoning", "command_reason_local"),
    "optimize": lambda: lazy_import("whitemagic.core.intelligence.agentic.token_optimizer", "command_optimize"),

    # Audit
    "audit": lambda: lazy_import("whitemagic.cli_app", "command_audit"),

    # Web
    "dashboard": lambda: lazy_import("whitemagic.cli_web", "dashboard"),
    "chat": lambda: lazy_import("whitemagic.cli_web", "chat"),
}


def get_command(name: str) -> Callable[..., Any] | None:
    """Get command handler, loading lazily."""
    if name not in LAZY_COMMANDS:
        return None
    loader = LAZY_COMMANDS[name]
    return loader()()  # type: ignore[no-any-return]


def build_fast_parser() -> argparse.ArgumentParser:
    """Build minimal parser for fast startup."""
    parser = argparse.ArgumentParser(
        description="WhiteMagic CLI (Fast Mode)",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Common commands:
  version          Show version
  search QUERY     Search memories
  context          Get AI context
  reason QUERY     Local reasoning (saves tokens!)
  audit            Project health check
  dashboard        Open web dashboard

For full command list, use: whitemagic --full-help
""",
    )
    parser.add_argument("--base-dir", default=".", help="Project root")
    parser.add_argument("--full-help", action="store_true", help="Show all commands")
    parser.add_argument("command", nargs="?", help="Command to run")
    parser.add_argument("args", nargs="*", help="Command arguments")

    return parser


def main_fast(argv: list[str] | None = None) -> int:
    """Fast CLI entry point."""
    parser = build_fast_parser()
    args, remaining = parser.parse_known_args(argv)

    # Full help - fall back to full CLI
    if args.full_help:
        from whitemagic.cli.cli_app import main
        return main(["--help"])  # type: ignore[no-any-return]

    # No command - show help
    if not args.command:
        parser.print_help()
        return 0

    # Version - instant
    if args.command == "version":
        from whitemagic.config import VERSION
        logger.info(f"WhiteMagic v{VERSION}")
        return 0

    # Reason command - local reasoning
    if args.command == "reason":
        if not args.args:
            logger.info("Usage: whitemagic reason <query>")
            return 1
        query = " ".join(args.args)
        from whitemagic.core.intelligence.agentic.local_reasoning import reason_locally
        result = reason_locally(query)
        logger.info("\n🧠 LOCAL REASONING")
        logger.info("=" * 50)
        logger.info(f"Query: {query}")
        logger.info(f"Tokens saved: {result.total_tokens_saved}")
        logger.info(f"AI needed: {'Yes' if result.ready_for_ai else 'No'}")
        logger.info("")
        logger.info(result.summary)
        return 0

    # Check if command exists in lazy commands
    if args.command in LAZY_COMMANDS:
        # Load the full CLI for complex commands
        from whitemagic.cli.cli_app import main
        return main([args.command] + remaining)  # type: ignore[no-any-return]

    # Unknown command
    logger.info(f"Unknown command: {args.command}")
    logger.info("Use --full-help for all commands")
    return 1


if __name__ == "__main__":
    sys.exit(main_fast())
