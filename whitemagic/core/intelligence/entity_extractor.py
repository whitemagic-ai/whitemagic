"""LLM-Driven Entity & Relation Extraction (v14.2).
===================================================
Uses Ollama (local) to extract structured entities and relations
from memory content, transforming statistical keyword associations
into a typed knowledge graph.

Extracts:
  - Entities: (name, type) — e.g., ("WhiteMagic", "PROJECT")
  - Relations: (subject, predicate, object) — e.g., ("Lucas", "CREATED", "WhiteMagic")

Graceful degradation: when Ollama is unavailable, falls back to
regex-based NER (proper nouns, capitalized phrases).

Usage:
    from whitemagic.core.intelligence.entity_extractor import get_entity_extractor
    extractor = get_entity_extractor()

    result = extractor.extract("Lucas created WhiteMagic on Oct 23, 2024.")
    # result.entities = [("Lucas", "PERSON"), ("WhiteMagic", "PROJECT")]
    # result.relations = [("Lucas", "CREATED", "WhiteMagic")]
"""

from __future__ import annotations

import json
import logging
import re
import threading
from dataclasses import dataclass, field
from typing import Any

logger = logging.getLogger(__name__)


@dataclass
class Entity:
    """An extracted entity."""

    name: str
    entity_type: str  # PERSON, PROJECT, CONCEPT, ORGANIZATION, TOOL, EVENT, LOCATION
    confidence: float = 1.0

    def to_dict(self) -> dict[str, Any]:
        return {"name": self.name, "type": self.entity_type, "confidence": self.confidence}


@dataclass
class Relation:
    """An extracted relation triple."""

    subject: str
    predicate: str  # CREATED, USES, PART_OF, CAUSES, FOLLOWED_BY, RELATED_TO, etc.
    object: str
    confidence: float = 1.0

    def to_dict(self) -> dict[str, Any]:
        return {
            "subject": self.subject,
            "predicate": self.predicate,
            "object": self.object,
            "confidence": self.confidence,
        }


@dataclass
class ExtractionResult:
    """Result of entity and relation extraction."""

    entities: list[Entity] = field(default_factory=list)
    relations: list[Relation] = field(default_factory=list)
    method: str = "none"  # "ollama", "regex", "none"

    def to_dict(self) -> dict[str, Any]:
        return {
            "entities": [e.to_dict() for e in self.entities],
            "relations": [r.to_dict() for r in self.relations],
            "method": self.method,
            "entity_count": len(self.entities),
            "relation_count": len(self.relations),
        }


_EXTRACTION_PROMPT = """Extract entities and relations from the following text.

Return ONLY a JSON object with this exact structure:
{
  "entities": [{"name": "...", "type": "PERSON|PROJECT|CONCEPT|ORGANIZATION|TOOL|EVENT|LOCATION"}],
  "relations": [{"subject": "...", "predicate": "CREATED|USES|PART_OF|CAUSES|FOLLOWED_BY|RELATED_TO|IMPLEMENTS|EXTENDS|CONTAINS", "object": "..."}]
}

Rules:
- Extract only clearly stated entities and relations
- Use the most specific entity type available
- Predicates should be UPPERCASE
- Keep entity names concise (1-3 words)
- Maximum 10 entities and 10 relations

Text:
"""


class EntityExtractor:
    """LLM-driven entity and relation extractor."""

    def __init__(
        self,
        ollama_url: str = "http://localhost:11434",
        model: str = "llama3.2",
        timeout: int = 30,
    ) -> None:
        self._ollama_url = ollama_url
        self._model = model
        self._timeout = timeout
        self._ollama_available: bool | None = None
        self._lock = threading.Lock()
        self._total_extractions = 0
        self._total_entities = 0
        self._total_relations = 0

    def _check_ollama(self) -> bool:
        """Check if Ollama is reachable."""
        if self._ollama_available is not None:
            return self._ollama_available
        try:
            import urllib.request
            req = urllib.request.Request(
                f"{self._ollama_url}/api/tags",
                method="GET",
            )
            with urllib.request.urlopen(req, timeout=5) as resp:
                if resp.status == 200:
                    self._ollama_available = True
                    return True
        except Exception:
            pass
        self._ollama_available = False
        logger.debug("Ollama not available — falling back to regex extraction")
        return False

    def extract(self, text: str, max_chars: int = 4000) -> ExtractionResult:
        """Extract entities and relations from text.

        Tries Ollama first, falls back to regex-based extraction.
        """
        if not text or not text.strip():
            return ExtractionResult()

        # Truncate to avoid overwhelming the LLM
        truncated = text[:max_chars]

        # Try Ollama LLM extraction
        if self._check_ollama():
            result = self._extract_ollama(truncated)
            if result and (result.entities or result.relations):
                with self._lock:
                    self._total_extractions += 1
                    self._total_entities += len(result.entities)
                    self._total_relations += len(result.relations)
                return result

        # Fallback: regex-based extraction
        result = self._extract_regex(truncated)
        with self._lock:
            self._total_extractions += 1
            self._total_entities += len(result.entities)
            self._total_relations += len(result.relations)
        return result

    def _extract_ollama(self, text: str) -> ExtractionResult | None:
        """Extract entities/relations using Ollama."""
        try:
            import urllib.request

            prompt = _EXTRACTION_PROMPT + text

            payload = json.dumps({
                "model": self._model,
                "prompt": prompt,
                "stream": False,
                "format": "json",
                "options": {"temperature": 0.1, "num_predict": 1024},
            }).encode("utf-8")

            req = urllib.request.Request(
                f"{self._ollama_url}/api/generate",
                data=payload,
                headers={"Content-Type": "application/json"},
            )

            with urllib.request.urlopen(req, timeout=self._timeout) as resp:
                raw = json.loads(resp.read().decode("utf-8"))
                response_text = raw.get("response", "")

            # Parse the JSON response
            data = json.loads(response_text)
            entities = [
                Entity(
                    name=e.get("name", ""),
                    entity_type=e.get("type", "CONCEPT"),
                    confidence=0.9,
                )
                for e in data.get("entities", [])
                if e.get("name")
            ]
            relations = [
                Relation(
                    subject=r.get("subject", ""),
                    predicate=r.get("predicate", "RELATED_TO"),
                    object=r.get("object", ""),
                    confidence=0.85,
                )
                for r in data.get("relations", [])
                if r.get("subject") and r.get("object")
            ]

            return ExtractionResult(
                entities=entities[:10],
                relations=relations[:10],
                method="ollama",
            )
        except Exception as e:
            logger.debug(f"Ollama extraction failed: {e}")
            return None

    def _extract_regex(self, text: str) -> ExtractionResult:
        """Fallback: regex-based entity extraction from text."""
        entities: list[Entity] = []
        seen_names: set[str] = set()

        # Pattern 1: Capitalized multi-word phrases (likely proper nouns)
        for match in re.finditer(r'\b([A-Z][a-z]+(?:\s+[A-Z][a-z]+)+)\b', text):
            name = match.group(1).strip()
            if name not in seen_names and len(name) > 3:
                entities.append(Entity(name=name, entity_type="CONCEPT", confidence=0.5))
                seen_names.add(name)

        # Pattern 2: ALL_CAPS words (likely acronyms/constants)
        for match in re.finditer(r'\b([A-Z]{2,}(?:_[A-Z]+)*)\b', text):
            name = match.group(1).strip()
            if name not in seen_names and len(name) > 1 and name not in {"THE", "AND", "FOR", "WITH", "FROM", "THIS", "THAT"}:
                entities.append(Entity(name=name, entity_type="CONCEPT", confidence=0.4))
                seen_names.add(name)

        # Pattern 3: CamelCase identifiers (code entities)
        for match in re.finditer(r'\b([A-Z][a-z]+(?:[A-Z][a-z]+)+)\b', text):
            name = match.group(1).strip()
            if name not in seen_names:
                entities.append(Entity(name=name, entity_type="TOOL", confidence=0.4))
                seen_names.add(name)

        # Limit to top 10
        entities = entities[:10]

        # Simple relation extraction: "X created/uses/extends Y" patterns
        relations: list[Relation] = []
        relation_patterns = [
            (r'(\w+)\s+created\s+(\w+)', "CREATED"),
            (r'(\w+)\s+uses?\s+(\w+)', "USES"),
            (r'(\w+)\s+extends?\s+(\w+)', "EXTENDS"),
            (r'(\w+)\s+implements?\s+(\w+)', "IMPLEMENTS"),
            (r'(\w+)\s+contains?\s+(\w+)', "CONTAINS"),
            (r'(\w+)\s+causes?\s+(\w+)', "CAUSES"),
        ]
        for pattern, predicate in relation_patterns:
            for match in re.finditer(pattern, text, re.IGNORECASE):
                relations.append(Relation(
                    subject=match.group(1),
                    predicate=predicate,
                    object=match.group(2),
                    confidence=0.3,
                ))
            if len(relations) >= 10:
                break

        return ExtractionResult(
            entities=entities,
            relations=relations[:10],
            method="regex",
        )

    def extract_and_store(
        self, memory_id: str, text: str,
    ) -> ExtractionResult:
        """Extract entities/relations and store them as typed associations.

        Writes extracted relations into the associations table with
        proper relation_type and direction fields.
        """
        result = self.extract(text)
        if not result.relations:
            return result

        try:
            from whitemagic.core.memory.unified import get_unified_memory
            um = get_unified_memory()
            pool = um.backend.pool

            from datetime import datetime
            now = datetime.now().isoformat()

            with pool.connection() as conn:
                for rel in result.relations:
                    # Store as a typed, directed association
                    conn.execute(
                        """INSERT OR IGNORE INTO associations
                           (source_id, target_id, strength, direction, relation_type,
                            edge_type, created_at, ingestion_time)
                           VALUES (?, ?, ?, ?, ?, ?, ?, ?)""",
                        (
                            memory_id,
                            f"entity:{rel.object.lower().replace(' ', '_')}",
                            rel.confidence,
                            "forward",
                            rel.predicate,
                            "semantic",
                            now,
                            now,
                        ),
                    )
                conn.commit()

            logger.info(
                f"Stored {len(result.relations)} extracted relations for {memory_id[:8]}",
            )
        except Exception as e:
            logger.debug(f"Failed to store extracted relations: {e}")

        return result

    def get_stats(self) -> dict[str, Any]:
        with self._lock:
            return {
                "total_extractions": self._total_extractions,
                "total_entities": self._total_entities,
                "total_relations": self._total_relations,
                "ollama_available": self._ollama_available,
                "model": self._model,
            }


# ---------------------------------------------------------------------------
# Singleton
# ---------------------------------------------------------------------------

_extractor: EntityExtractor | None = None
_extractor_lock = threading.Lock()


def get_entity_extractor(**kwargs: Any) -> EntityExtractor:
    """Get the global EntityExtractor singleton."""
    global _extractor
    if _extractor is None:
        with _extractor_lock:
            if _extractor is None:
                _extractor = EntityExtractor(**kwargs)
    return _extractor
