"""Knowledge Graph Extraction — Entity/Relation Extraction from Memories.
======================================================================
Extracts entities and relationships from memory content using:
  - spaCy NER if available
  - Regex-based fallback for common patterns

Stores a lightweight in-memory graph. Persisted to JSON.

Usage:
    from whitemagic.core.intelligence.knowledge_graph import get_knowledge_graph
    kg = get_knowledge_graph()
    kg.extract_from_text("memory_123", "Python is used by WhiteMagic for MCP tools")
    results = kg.query_entity("WhiteMagic")
"""
from __future__ import annotations

import json
import logging
import os
import re
import threading
from collections import defaultdict
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)

HAS_SPACY = False
_nlp = None
_nlp_lock = threading.Lock()

try:
    import spacy  # type: ignore[import-not-found]
    HAS_SPACY = True
except ImportError:
    pass


def _get_nlp() -> Any:
    global _nlp
    if _nlp is None and HAS_SPACY:
        with _nlp_lock:
            if _nlp is None:
                try:
                    _nlp = spacy.load("en_core_web_sm")
                except OSError:
                    try:
                        _nlp = spacy.load("en_core_web_md")
                    except OSError:
                        logger.debug("No spaCy model found — using regex fallback")
    return _nlp


@dataclass
class Entity:
    name: str
    entity_type: str  # PERSON, ORG, TECH, CONCEPT, TOOL, etc.
    mentions: int = 1
    sources: set[str] = field(default_factory=set)

    def to_dict(self) -> dict[str, Any]:
        return {
            "name": self.name,
            "type": self.entity_type,
            "mentions": self.mentions,
            "sources": list(self.sources)[:10],
        }


@dataclass
class Relation:
    subject: str
    predicate: str
    obj: str
    confidence: float = 0.8
    source: str = ""

    def to_dict(self) -> dict[str, Any]:
        return {
            "subject": self.subject,
            "predicate": self.predicate,
            "object": self.obj,
            "confidence": self.confidence,
            "source": self.source,
        }


# Regex patterns for fallback extraction
TECH_PATTERNS = re.compile(
    r"\b(Python|Rust|Zig|Mojo|Go|Elixir|Haskell|Julia|TypeScript|JavaScript|"
    r"React|Tauri|Redis|SQLite|gRPC|WebSocket|SIMD|OpenTelemetry|Docker|"
    r"WhiteMagic|MandalaOS|Nexus|Grimoire|Dharma|Karma|Harmony|Wu Xing|"
    r"Gan Ying|Galactic Map|Dream Cycle|BitNet|spaCy|sentence-transformers)\b",
    re.IGNORECASE,
)

RELATION_PATTERNS = [
    (re.compile(r"(\w[\w\s]{1,30})\s+(?:uses?|utilizes?)\s+(\w[\w\s]{1,30})", re.I), "uses"),
    (re.compile(r"(\w[\w\s]{1,30})\s+(?:depends?\s+on|requires?)\s+(\w[\w\s]{1,30})", re.I), "depends_on"),
    (re.compile(r"(\w[\w\s]{1,30})\s+(?:is\s+part\s+of|belongs?\s+to)\s+(\w[\w\s]{1,30})", re.I), "part_of"),
    (re.compile(r"(\w[\w\s]{1,30})\s+(?:implements?|provides?)\s+(\w[\w\s]{1,30})", re.I), "implements"),
    (re.compile(r"(\w[\w\s]{1,30})\s+(?:connects?\s+to|integrates?\s+with)\s+(\w[\w\s]{1,30})", re.I), "connects_to"),
]


class KnowledgeGraph:
    """In-memory knowledge graph with entity and relation extraction."""

    def __init__(self, storage_dir: str | None = None) -> None:
        self._lock = threading.Lock()
        self._storage = Path(storage_dir or os.path.join(
            os.environ.get("WM_STATE_ROOT", os.path.expanduser("~/.whitemagic")),
            "knowledge",
        ))
        self._storage.mkdir(parents=True, exist_ok=True)
        self._data_file = self._storage / "graph.json"

        self._entities: dict[str, Entity] = {}
        self._relations: list[Relation] = []
        self._adjacency: dict[str, set[str]] = defaultdict(set)

        self._load()

    def extract_from_text(self, source_id: str, text: str) -> dict[str, Any]:
        """Extract entities and relations from text."""
        entities_found = []
        relations_found = []

        nlp = _get_nlp()
        if nlp:
            doc = nlp(text[:10000])
            for ent in doc.ents:
                name = ent.text.strip()
                if len(name) < 2:
                    continue
                etype = ent.label_
                entities_found.append((name, etype))

        # Always run regex for tech terms
        for match in TECH_PATTERNS.finditer(text):
            entities_found.append((match.group(0), "TECH"))

        # Extract relations via patterns
        for pattern, predicate in RELATION_PATTERNS:
            for m in pattern.finditer(text):
                subj = m.group(1).strip()
                obj = m.group(2).strip()
                if len(subj) > 2 and len(obj) > 2:
                    relations_found.append((subj, predicate, obj))

        # Merge into graph
        with self._lock:
            for name, etype in entities_found:
                key = name.lower()
                if key in self._entities:
                    self._entities[key].mentions += 1
                    self._entities[key].sources.add(source_id)
                else:
                    self._entities[key] = Entity(
                        name=name, entity_type=etype,
                        mentions=1, sources={source_id},
                    )

            for subj, pred, obj in relations_found:
                rel = Relation(subject=subj, predicate=pred, obj=obj, source=source_id)
                self._relations.append(rel)
                self._adjacency[subj.lower()].add(obj.lower())
                self._adjacency[obj.lower()].add(subj.lower())

                if len(self._relations) > 10000:
                    self._relations = self._relations[-5000:]

        return {
            "entities_extracted": len(entities_found),
            "relations_extracted": len(relations_found),
            "total_entities": len(self._entities),
            "total_relations": len(self._relations),
        }

    def add_entity(
        self, name: str, entity_type: str = "CONCEPT",
        metadata: dict[str, Any] | None = None,
    ) -> None:
        """Add or update an entity directly (used by cross-system wiring)."""
        key = name.lower()
        with self._lock:
            if key in self._entities:
                self._entities[key].mentions += 1
            else:
                self._entities[key] = Entity(
                    name=name, entity_type=entity_type,
                    mentions=1, sources=set(),
                )
            if metadata and metadata.get("origin"):
                self._entities[key].sources.add(str(metadata["origin"]))

    def add_relation(
        self, source: str, relation: str, target: str,
        metadata: dict[str, Any] | None = None,
    ) -> None:
        """Add a relation directly (used by cross-system wiring)."""
        rel = Relation(
            subject=source, predicate=relation, obj=target,
            confidence=0.7,
            source=str((metadata or {}).get("origin", "cross_system")),
        )
        with self._lock:
            self._relations.append(rel)
            self._adjacency[source.lower()].add(target.lower())
            self._adjacency[target.lower()].add(source.lower())
            if len(self._relations) > 10000:
                self._relations = self._relations[-5000:]

    def query_entity(self, name: str) -> dict[str, Any]:
        """Query an entity and its connections."""
        key = name.lower()
        with self._lock:
            entity = self._entities.get(key)
            if not entity:
                return {"found": False, "name": name}

            neighbors = self._adjacency.get(key, set())
            related_entities = []
            for n in list(neighbors)[:20]:
                e = self._entities.get(n)
                if e:
                    related_entities.append(e.to_dict())

            relations = [r.to_dict() for r in self._relations
                        if r.subject.lower() == key or r.obj.lower() == key]

            return {
                "found": True,
                "entity": entity.to_dict(),
                "neighbors": related_entities,
                "relations": relations[-20:],
            }

    def top_entities(self, limit: int = 20) -> list[dict[str, Any]]:
        """Get most-mentioned entities."""
        with self._lock:
            sorted_ents = sorted(self._entities.values(), key=lambda e: e.mentions, reverse=True)
            return [e.to_dict() for e in sorted_ents[:limit]]

    def status(self) -> dict[str, Any]:
        with self._lock:
            return {
                "total_entities": len(self._entities),
                "total_relations": len(self._relations),
                "has_spacy": HAS_SPACY,
                "spacy_model_loaded": _nlp is not None,
                "data_file": str(self._data_file),
            }

    def _save(self) -> None:
        try:
            with self._lock:
                data = {
                    "entities": {k: v.to_dict() for k, v in self._entities.items()},
                    "relations": [r.to_dict() for r in self._relations[-5000:]],
                }
            with open(self._data_file, "w") as f:
                json.dump(data, f, indent=2)
        except Exception as e:
            logger.debug("KG save failed: %s", e)

    def _load(self) -> None:
        if not self._data_file.exists():
            return
        try:
            with open(self._data_file) as f:
                data = json.load(f)
            for k, v in data.get("entities", {}).items():
                self._entities[k] = Entity(
                    name=v["name"], entity_type=v["type"],
                    mentions=v.get("mentions", 1),
                    sources=set(v.get("sources", [])),
                )
            for r in data.get("relations", []):
                rel = Relation(
                    subject=r["subject"], predicate=r["predicate"],
                    obj=r["object"], confidence=r.get("confidence", 0.8),
                    source=r.get("source", ""),
                )
                self._relations.append(rel)
                self._adjacency[rel.subject.lower()].add(rel.obj.lower())
                self._adjacency[rel.obj.lower()].add(rel.subject.lower())
        except Exception as e:
            logger.debug("KG load failed: %s", e)

    def save(self) -> None:
        """Public save method."""
        self._save()


_kg: KnowledgeGraph | None = None
_kg_lock = threading.Lock()

def get_knowledge_graph() -> KnowledgeGraph:
    global _kg
    if _kg is None:
        with _kg_lock:
            if _kg is None:
                _kg = KnowledgeGraph()
    return _kg
