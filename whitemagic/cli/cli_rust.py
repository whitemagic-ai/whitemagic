# mypy: disable-error-code=no-untyped-def
"""CLI Rust Commands - v4.9.0 Async Acceleration
Fast search, compression, and memory consolidation via Rust.
"""

import json
import logging
from typing import Any

logger = logging.getLogger(__name__)


def _load_rust() -> tuple[Any | None, Exception | None]:
    """Load Rust bridge with graceful fallback."""
    try:
        from whitemagic_rs import whitemagic_rs as rs
        return rs, None
    except ImportError:
        try:
            import whitemagic_rs
            return whitemagic_rs, None
        except ImportError as exc:
            return None, exc


def rust_search(
    root_path: str,
    pattern: str,
    limit: int = 100,
    extensions: list[str] | None = None,
    max_file_size: int = 2_000_000,
    output_json: bool = False,
) -> int:
    """Fast file search via Rust."""
    rust, err = _load_rust()
    if not rust:
        logger.info(f"❌ Rust bridge not available: {err}")
        return 1

    try:
        results = rust.fast_search(
            root_path, pattern, limit,
            extensions, max_file_size,
        )
        if output_json:
            payload = [{"path": r[0], "score": r[1]} for r in results]
            logger.info(json.dumps(payload, indent=2))
        else:
            logger.info(f"🔍 Found {len(results)} results:")
            for path, score in results[:20]:
                logger.info(f"  {score:.2f}  {path}")
            if len(results) > 20:
                logger.info(f"  ... and {len(results) - 20} more")
        return 0
    except Exception as e:
        logger.info(f"❌ Search error: {e}")
        return 1


def rust_compress(input_path: str, output_path: str) -> int:
    """Compress a file via Rust."""
    rust, err = _load_rust()
    if not rust:
        logger.info(f"❌ Rust bridge not available: {err}")
        return 1

    try:
        compressed = rust.compress_file(input_path, output_path)
        logger.info(f"✅ Compressed: {compressed} bytes written to {output_path}")
        return 0
    except Exception as e:
        logger.info(f"❌ Compression error: {e}")
        return 1


def rust_consolidate(
    short_term_dir: str,
    top_n: int = 20,
    similarity_threshold: float = 0.7,
    output_json: bool = False,
) -> int:
    """Consolidate memories via Rust."""
    rust, err = _load_rust()
    if not rust:
        logger.info(f"❌ Rust bridge not available: {err}")
        return 1

    try:
        result = rust.consolidate_memories(
            short_term_dir, top_n, similarity_threshold,
        )
        payload = {
            "short_term_count": result[0],
            "long_term_created": result[1],
            "clusters_found": result[2],
            "duration_seconds": result[3],
            "top_memories": result[4],
        }
        if output_json:
            logger.info(json.dumps(payload, indent=2))
        else:
            logger.info("🧠 Consolidation complete:")
            logger.info(f"   Short-term: {payload['short_term_count']}")
            logger.info(f"   Long-term created: {payload['long_term_created']}")
            logger.info(f"   Clusters: {payload['clusters_found']}")
            logger.info(f"   Duration: {payload['duration_seconds']:.2f}s")
        return 0
    except Exception as e:
        logger.info(f"❌ Consolidation error: {e}")
        return 1


def rust_status() -> int:
    """Check Rust bridge status."""
    rust, err = _load_rust()
    if rust:
        logger.info("✅ Rust bridge available")
        logger.info(f"   Module: {rust}")
        try:
            exports = [x for x in dir(rust) if not x.startswith("_")]
            logger.info(f"   Exports: {len(exports)}")
        except Exception:
            pass
        return 0
    logger.info(f"❌ Rust bridge not available: {err}")
    return 1


# Click commands for CLI integration
def register_rust_commands(cli: Any) -> Any:
    """Register Rust commands with Click CLI."""
    import click

    @cli.group()
    def rust():
        """🦀 Rust acceleration tools (v4.9.0)"""

    @rust.command()
    @click.argument("root_path")
    @click.argument("pattern")
    @click.option("--limit", default=100, help="Max results")
    @click.option("--extensions", "-e", multiple=True, help="File extensions")
    @click.option("--max-size", default=2_000_000, help="Max file size")
    @click.option("--json", "output_json", is_flag=True, help="JSON output")
    def search(root_path, pattern, limit, extensions, max_size, output_json):
        """Fast file search via Rust SIMD."""
        ext_list = list(extensions) if extensions else None
        return rust_search(root_path, pattern, limit, ext_list, max_size, output_json)

    @rust.command()
    @click.argument("input_path")
    @click.argument("output_path")
    def compress(input_path, output_path):
        """Compress file via Rust."""
        return rust_compress(input_path, output_path)

    @rust.command()
    @click.argument("short_term_dir")
    @click.option("--top-n", default=20, help="Top N memories")
    @click.option("--threshold", default=0.7, help="Similarity threshold")
    @click.option("--json", "output_json", is_flag=True, help="JSON output")
    def consolidate(short_term_dir, top_n, threshold, output_json):
        """Consolidate memories via Rust."""
        return rust_consolidate(short_term_dir, top_n, threshold, output_json)

    @rust.command()
    def status():
        """Check Rust bridge status."""
        return rust_status()

    return rust
