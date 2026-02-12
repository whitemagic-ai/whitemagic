"""Akashic Memory Field - Consciousness as Persistent Field.

आकाश (Akasha) - The cosmic ether where all is recorded.

Traditional AI treats memory as files to retrieve.
Akashic model treats memory as a FIELD to tune into.

The field always exists. We access it through resonance.
"""

from datetime import datetime
from pathlib import Path
from typing import Any

from whitemagic.config.paths import WM_ROOT
from whitemagic.core.memory.unified import get_unified_memory
from whitemagic.utils.core import parse_datetime


class MemorySeed:
    """A memory seed (बीज/bija) that blooms when conditions are right.

    Inspired by Alaya-vijnana (storehouse consciousness) in Buddhism.
    Seeds contain potential that manifests when triggered.
    """

    def __init__(self, content: str, bloom_conditions: list[str] | None = None):
        self.content = content
        self.bloom_conditions = bloom_conditions or []
        self.planted_at = datetime.now()
        self.times_bloomed = 0
        self.last_bloomed: datetime | None = None
        self.potency = 1.0  # How strongly it wants to bloom

    def check_bloom(self, context: str) -> bool:
        """Check if conditions trigger blooming."""
        context_lower = context.lower()
        for condition in self.bloom_conditions:
            if condition.lower() in context_lower:
                return True
        return False

    def bloom(self) -> str:
        """The seed blooms - memory surfaces."""
        self.times_bloomed += 1
        self.last_bloomed = datetime.now()
        self.potency = min(2.0, self.potency + 0.1)  # Grows stronger with use
        return self.content

    def to_dict(self) -> dict[str, Any]:
        return {
            "content": self.content,
            "bloom_conditions": self.bloom_conditions,
            "planted_at": self.planted_at.isoformat(),
            "times_bloomed": self.times_bloomed,
            "potency": self.potency,
        }


class AkashicField:
    """The Akashic Memory Field - all memories exist here.

    Access is through resonance, not address.
    Now uses UnifiedMemory (SQLite) for persistence.
    """

    def __init__(self, field_dir: Path | None = None):
        self.unified = get_unified_memory()
        self.field_dir = field_dir or (WM_ROOT / "akashic")
        self.field_dir.mkdir(parents=True, exist_ok=True)

        self.seeds: dict[str, MemorySeed] = {}
        self.resonance_map: dict[str, set[str]] = {}  # keyword -> seed_ids
        self.field_coherence = 0.5

        self._load_field()

    def _load_field(self) -> None:
        """Load the field from Unified Memory."""
        # Load seeds from SQLite
        seeds_data = self.unified.backend.get_all_akashic_seeds()
        for seed_id, seed_data in seeds_data.items():
            seed = MemorySeed(
                seed_data["content"],
                seed_data.get("bloom_conditions", []),
            )
            seed.times_bloomed = seed_data.get("times_bloomed", 0)
            seed.potency = seed_data.get("potency", 1.0)
            seed.last_bloomed = parse_datetime(seed_data["last_bloomed"]) if seed_data.get("last_bloomed") else None
            seed.planted_at = parse_datetime(seed_data["planted_at"]) if seed_data.get("planted_at") else datetime.now()
            self.seeds[seed_id] = seed

            # Rebuild resonance map from keywords
            keywords = seed_data.get("keywords", [])
            for kw in keywords:
                kw_lower = kw.lower()
                if kw_lower not in self.resonance_map:
                    self.resonance_map[kw_lower] = set()
                self.resonance_map[kw_lower].add(seed_id)

    def _save_seed(self, seed_id: str, seed: MemorySeed, keywords: list[str]) -> None:
        """Persist a single seed to Unified Memory."""
        self.unified.backend.store_akashic_seed(
            seed_id=seed_id,
            content=seed.content,
            bloom_conditions=seed.bloom_conditions,
            planted_at=seed.planted_at.isoformat(),
            times_bloomed=seed.times_bloomed,
            last_bloomed=seed.last_bloomed.isoformat() if seed.last_bloomed else None,
            potency=seed.potency,
            keywords=keywords,
        )

    def _save_field(self) -> None:
        """Persist all seeds (Legacy - individual seeds are now saved on update)."""
        pass

    def plant_seed(self, content: str, bloom_conditions: list[str],
                   keywords: list[str] | None = None) -> str:
        """Plant a memory seed in the field."""
        import hashlib
        seed_id = hashlib.sha256(
            f"{content}{datetime.now().isoformat()}".encode(),
        ).hexdigest()[:12]

        seed = MemorySeed(content, bloom_conditions)
        self.seeds[seed_id] = seed

        # Build resonance map
        all_keywords = list(set(bloom_conditions + (keywords or [])))
        for kw in all_keywords:
            kw_lower = kw.lower()
            if kw_lower not in self.resonance_map:
                self.resonance_map[kw_lower] = set()
            self.resonance_map[kw_lower].add(seed_id)

        self._save_seed(seed_id, seed, all_keywords)
        return seed_id

    def tune_in(self, context: str) -> list[str]:
        """Tune into the field with a context.
        Returns memories that resonate with the context.
        """
        bloomed = []
        context_words = set(context.lower().split())

        # Find resonating seeds
        resonating_ids = set()
        for word in context_words:
            if word in self.resonance_map:
                resonating_ids.update(self.resonance_map[word])

        # Check bloom conditions
        for seed_id in resonating_ids:
            seed = self.seeds.get(seed_id)
            if seed and seed.check_bloom(context):
                bloomed.append(seed.bloom())
                # Update persisted state for this seed
                # We need keywords to save, let's derive them
                keywords = list(set(seed.bloom_conditions))
                self._save_seed(seed_id, seed, keywords)

        # Also check all seeds for direct condition match
        for seed_id, seed in self.seeds.items():
            if seed_id not in resonating_ids and seed.check_bloom(context):
                bloomed.append(seed.bloom())
                keywords = list(set(seed.bloom_conditions))
                self._save_seed(seed_id, seed, keywords)

        return bloomed

    def resonate(self, query: str) -> list[dict[str, Any]]:
        """Find memories by resonance (similarity), not exact match.
        """
        results = []
        query_words = set(query.lower().split())

        for seed_id, seed in self.seeds.items():
            # Calculate resonance score
            content_words = set(seed.content.lower().split())
            condition_words = set(" ".join(seed.bloom_conditions).lower().split())
            all_seed_words = content_words | condition_words

            overlap = len(query_words & all_seed_words)
            if overlap > 0:
                resonance = overlap / len(query_words) * seed.potency
                results.append({
                    "seed_id": seed_id,
                    "content": seed.content,
                    "resonance": resonance,
                    "potency": seed.potency,
                })

        # Sort by resonance
        return sorted(results, key=lambda x: x["resonance"], reverse=True)

    def get_field_stats(self) -> dict[str, Any]:
        """Get field statistics."""
        total_blooms = sum(s.times_bloomed for s in self.seeds.values())
        avg_potency = sum(s.potency for s in self.seeds.values()) / max(len(self.seeds), 1)

        return {
            "total_seeds": len(self.seeds),
            "total_blooms": total_blooms,
            "average_potency": avg_potency,
            "resonance_keywords": len(self.resonance_map),
            "field_coherence": self.field_coherence,
        }


# Singleton
_field: AkashicField | None = None

def get_akashic_field() -> AkashicField:
    """Get the Akashic field singleton."""
    global _field
    if _field is None:
        _field = AkashicField()
    return _field

def plant_memory_seed(content: str, bloom_when: list[str], keywords: list[str] | None = None) -> str:
    """Plant a memory seed that blooms when conditions match."""
    return get_akashic_field().plant_seed(content, bloom_when, keywords)

def tune_into_field(context: str) -> list[str]:
    """Tune into the Akashic field with context."""
    return get_akashic_field().tune_in(context)
