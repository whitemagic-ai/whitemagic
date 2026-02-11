"""Automated wisdom text ingestion from sacred-texts.com.
Version: 3.0.0.

Graceful degradation: tries aiohttp, falls back to urllib.
"""

import asyncio
import logging
from dataclasses import dataclass

from whitemagic.core import MemoryManager  # type: ignore[attr-defined]

logger = logging.getLogger(__name__)

# Try aiohttp, fall back to urllib
try:
    import aiohttp
    HAS_AIOHTTP = True
except ImportError:
    import urllib.request
    HAS_AIOHTTP = False
    logger.info("⚠ aiohttp not available, using urllib (slower)")


@dataclass
class WisdomText:
    name: str
    base_url: str
    tags: list[str]


TEXTS = [
    WisdomText("Dao De Jing", "https://sacred-texts.com/tao/taote.htm", ["daoism"]),
    WisdomText("I Ching", "https://sacred-texts.com/ich/index.htm", ["iching"]),
    WisdomText("Art of War", "https://sacred-texts.com/tao/aow/index.htm", ["strategy"]),
    WisdomText("Yang Chu", "https://sacred-texts.com/tao/ycgp/index.htm", ["yang-chu"]),
    WisdomText("Tai Shang", "https://sacred-texts.com/tao/ts/index.htm", ["morality"]),
    WisdomText("Yin Classic", "https://sacred-texts.com/tao/ycc/index.htm", ["yin"]),
    WisdomText("Zhuangzi", "https://sacred-texts.com/tao/mcm/index.htm", ["zhuangzi"]),
    WisdomText("Teachings", "https://sacred-texts.com/tao/tt/index.htm", ["teachings"]),
    WisdomText("SBE 39", "https://sacred-texts.com/tao/sbe39/index.htm", ["translation"]),
]


async def ingest_all_async() -> dict[str, int]:
    """Ingest all 9 wisdom texts (async version)."""
    memory = MemoryManager()
    stats = {"success": 0, "failed": 0}

    async with aiohttp.ClientSession() as session:
        for text in TEXTS:
            try:
                await asyncio.sleep(2)  # Rate limit
                async with session.get(text.base_url, timeout=30) as resp:
                    if resp.status == 200:
                        content = await resp.text()
                        memory.create_memory(
                            title=f"Wisdom: {text.name}",
                            content=content[:8000],
                            memory_type="long_term",
                            tags=text.tags + ["wisdom", "3.0.0"],
                        )
                        stats["success"] += 1
                        logger.info(f"✓ {text.name}")
                    else:
                        stats["failed"] += 1
            except Exception as e:
                logger.info(f"✗ {text.name}: {e}")
                stats["failed"] += 1

    return stats


def ingest_all_sync() -> dict[str, int]:
    """Ingest all 9 wisdom texts (sync fallback)."""
    import time
    memory = MemoryManager()
    stats = {"success": 0, "failed": 0}

    for text in TEXTS:
        try:
            time.sleep(2)  # Rate limit
            with urllib.request.urlopen(text.base_url, timeout=30) as resp:
                if resp.status == 200:
                    content = resp.read().decode("utf-8")
                    memory.create_memory(
                        title=f"Wisdom: {text.name}",
                        content=content[:8000],
                        memory_type="long_term",
                        tags=text.tags + ["wisdom", "3.0.0"],
                    )
                    stats["success"] += 1
                    logger.info(f"✓ {text.name}")
                else:
                    stats["failed"] += 1
        except Exception as e:
            logger.info(f"✗ {text.name}: {e}")
            stats["failed"] += 1

    return stats


async def ingest_all() -> dict[str, int]:
    """Main entry point - uses async if available, sync otherwise."""
    if HAS_AIOHTTP:
        return await ingest_all_async()
    else:
        return ingest_all_sync()


if __name__ == "__main__":
    if HAS_AIOHTTP:
        stats = asyncio.run(ingest_all())
    else:
        stats = ingest_all_sync()
    logger.info(f"\nIngested: {stats['success']}/9 texts")
