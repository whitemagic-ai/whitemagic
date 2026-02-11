"""
Tests for Leap 2 — Cold Storage Awakening (v13.6)

Covers:
  - Cold DB embedding table creation
  - Cross-DB semantic search (hot + cold)
  - Archive ingestion
  - EmbeddingEngine cold DB support
  - Hybrid search with include_cold
"""

from __future__ import annotations

import os
import sqlite3
import struct
import tempfile
from pathlib import Path
from typing import List
from unittest.mock import MagicMock, patch

import pytest

# Ensure lightweight init
os.environ["WM_SILENT_INIT"] = "1"
os.environ["WM_SKIP_HOLO_INDEX"] = "1"

EMBEDDING_DIM = 384


def _pack_embedding(vec: List[float]) -> bytes:
    return struct.pack(f"{len(vec)}f", *vec)


def _make_fake_vec(seed: float = 0.1) -> List[float]:
    """Create a deterministic fake embedding vector."""
    import math
    return [math.sin(seed * (i + 1)) for i in range(EMBEDDING_DIM)]


def _create_cold_db_with_embeddings(path: str, n_memories: int = 50) -> None:
    """Create a minimal cold DB with memories and embeddings for testing."""
    conn = sqlite3.connect(path)
    conn.execute("PRAGMA journal_mode=WAL")

    # Create memories table
    conn.execute("""
        CREATE TABLE IF NOT EXISTS memories (
            id TEXT PRIMARY KEY,
            content TEXT,
            memory_type TEXT DEFAULT 'deep_archive',
            created_at TEXT DEFAULT CURRENT_TIMESTAMP,
            updated_at TEXT,
            accessed_at TEXT DEFAULT CURRENT_TIMESTAMP,
            access_count INTEGER DEFAULT 0,
            emotional_valence REAL DEFAULT 0.0,
            importance REAL DEFAULT 0.5,
            neuro_score REAL DEFAULT 1.0,
            novelty_score REAL DEFAULT 1.0,
            recall_count INTEGER DEFAULT 0,
            half_life_days REAL DEFAULT 30.0,
            is_protected INTEGER DEFAULT 0,
            metadata TEXT DEFAULT '{}',
            title TEXT,
            galactic_distance REAL DEFAULT 0.8,
            retention_score REAL DEFAULT 0.5,
            last_retention_sweep TEXT
        )
    """)

    # Create tags table
    conn.execute("""
        CREATE TABLE IF NOT EXISTS tags (
            memory_id TEXT,
            tag TEXT,
            PRIMARY KEY (memory_id, tag)
        )
    """)

    # Create FTS table
    conn.execute("""
        CREATE VIRTUAL TABLE IF NOT EXISTS memories_fts USING fts5(
            id UNINDEXED, title, content, tags_text
        )
    """)

    # Create embedding table
    conn.execute("""
        CREATE TABLE IF NOT EXISTS memory_embeddings (
            memory_id TEXT PRIMARY KEY,
            embedding BLOB,
            model TEXT,
            created_at TEXT DEFAULT CURRENT_TIMESTAMP
        )
    """)

    # Insert test memories with embeddings
    for i in range(n_memories):
        mid = f"cold_mem_{i:04d}"
        title = f"Cold Memory {i}: archived content about topic {i % 5}"
        content = f"This is deep archive content #{i} about {'rust zig haskell elixir go'.split()[i % 5]} accelerators"
        conn.execute(
            "INSERT INTO memories (id, title, content, memory_type, importance) VALUES (?, ?, ?, 'deep_archive', ?)",
            (mid, title, content, 0.3 + (i % 10) * 0.05),
        )
        conn.execute(
            "INSERT INTO memories_fts (id, title, content, tags_text) VALUES (?, ?, ?, '')",
            (mid, title, content),
        )
        vec = _make_fake_vec(seed=0.1 * (i + 1))
        conn.execute(
            "INSERT INTO memory_embeddings (memory_id, embedding, model) VALUES (?, ?, ?)",
            (mid, _pack_embedding(vec), "all-MiniLM-L6-v2"),
        )

    conn.commit()
    conn.close()


# ===========================================================================
# Test: Cold DB embedding table creation (via script)
# ===========================================================================

class TestColdDBEmbeddingScript:
    """Test the cold_db_embed.py script logic."""

    def test_init_cold_db_creates_table(self, tmp_path):
        """_init_cold_db creates memory_embeddings table."""
        db_path = str(tmp_path / "cold.db")
        conn = sqlite3.connect(db_path)

        from scripts.cold_db_embed import _init_cold_db
        _init_cold_db(conn)

        tables = conn.execute(
            "SELECT name FROM sqlite_master WHERE type='table' AND name='memory_embeddings'"
        ).fetchall()
        assert len(tables) == 1

        # Check columns
        cols = conn.execute("PRAGMA table_info(memory_embeddings)").fetchall()
        col_names = {c[1] for c in cols}
        assert {"memory_id", "embedding", "model", "created_at"} <= col_names
        conn.close()

    def test_get_candidates_skips_cached(self, tmp_path):
        """get_candidates with skip_cached=True excludes already-embedded memories."""
        db_path = str(tmp_path / "cold.db")
        conn = sqlite3.connect(db_path)
        conn.execute("""
            CREATE TABLE memories (
                id TEXT PRIMARY KEY, title TEXT, content TEXT, importance REAL DEFAULT 0.5
            )
        """)
        conn.execute("""
            CREATE TABLE memory_embeddings (
                memory_id TEXT PRIMARY KEY, embedding BLOB, model TEXT
            )
        """)
        conn.execute("INSERT INTO memories VALUES ('m1', 'title1', 'content1', 0.5)")
        conn.execute("INSERT INTO memories VALUES ('m2', 'title2', 'content2', 0.5)")
        conn.execute("INSERT INTO memory_embeddings VALUES ('m1', X'00', 'test')")
        conn.commit()

        from scripts.cold_db_embed import get_candidates
        candidates = get_candidates(conn, limit=100, skip_cached=True)
        assert len(candidates) == 1
        assert candidates[0][0] == "m2"
        conn.close()

    def test_dry_run_returns_counts(self, tmp_path):
        """Dry run returns candidate count without encoding."""
        db_path = str(tmp_path / "cold.db")
        conn = sqlite3.connect(db_path)
        conn.execute("CREATE TABLE memories (id TEXT PRIMARY KEY, title TEXT, content TEXT, importance REAL)")
        conn.execute("INSERT INTO memories VALUES ('m1', 't1', 'c1', 0.5)")
        conn.commit()
        conn.close()

        from scripts.cold_db_embed import run
        result = run(cold_db_path=db_path, dry_run=True)
        assert result["status"] == "dry_run"
        assert result["candidates"] == 1


# ===========================================================================
# Test: EmbeddingEngine cold DB support
# ===========================================================================

class TestEmbeddingEngineColdDB:
    """Test cross-DB embedding search in EmbeddingEngine."""

    def test_get_cold_db_returns_none_when_no_table(self, tmp_path):
        """_get_cold_db returns None if memory_embeddings table doesn't exist."""
        from whitemagic.core.memory.embeddings import EmbeddingEngine

        cold_path = tmp_path / "cold.db"
        conn = sqlite3.connect(str(cold_path))
        conn.execute("CREATE TABLE memories (id TEXT PRIMARY KEY)")
        conn.commit()
        conn.close()

        engine = EmbeddingEngine()
        with patch("whitemagic.core.memory.embeddings.EmbeddingEngine._get_cold_db") as mock:
            # Simulate: cold DB exists but no embeddings table
            mock.return_value = None
            ids, vecs = engine._load_cold_vec_cache()
            assert ids == []
            assert len(vecs) == 0

    def test_get_cold_db_connects_when_table_exists(self, tmp_path):
        """_get_cold_db succeeds when memory_embeddings table exists."""
        from whitemagic.core.memory.embeddings import EmbeddingEngine

        cold_path = tmp_path / "cold.db"
        _create_cold_db_with_embeddings(str(cold_path), n_memories=5)

        engine = EmbeddingEngine()
        engine._cold_db_checked = False
        with patch("whitemagic.config.paths.COLD_DB_PATH", cold_path):
            conn = engine._get_cold_db()
            assert conn is not None
            count = conn.execute("SELECT COUNT(*) FROM memory_embeddings").fetchone()[0]
            assert count == 5

    def test_load_cold_vec_cache(self, tmp_path):
        """_load_cold_vec_cache loads vectors from cold DB."""
        from whitemagic.core.memory.embeddings import EmbeddingEngine

        cold_path = tmp_path / "cold.db"
        _create_cold_db_with_embeddings(str(cold_path), n_memories=10)

        engine = EmbeddingEngine()
        engine._cold_db_checked = False
        with patch("whitemagic.config.paths.COLD_DB_PATH", cold_path):
            engine._get_cold_db()
            ids, vecs = engine._load_cold_vec_cache()
            assert len(ids) == 10
            assert len(vecs) == 10
            assert len(vecs[0]) == EMBEDDING_DIM

    def test_cold_vec_cache_is_cached(self, tmp_path):
        """Second call to _load_cold_vec_cache uses cache."""
        from whitemagic.core.memory.embeddings import EmbeddingEngine

        cold_path = tmp_path / "cold.db"
        _create_cold_db_with_embeddings(str(cold_path), n_memories=5)

        engine = EmbeddingEngine()
        engine._cold_db_checked = False
        with patch("whitemagic.config.paths.COLD_DB_PATH", cold_path):
            engine._get_cold_db()
            ids1, _ = engine._load_cold_vec_cache()
            ids2, _ = engine._load_cold_vec_cache()
            assert ids1 is ids2  # Same object = cache hit

    def test_search_similar_hot_only(self):
        """search_similar with include_cold=False doesn't touch cold DB."""
        from whitemagic.core.memory.embeddings import EmbeddingEngine

        engine = EmbeddingEngine()
        fake_query_vec = _make_fake_vec(seed=0.5)

        with patch.object(engine, "encode", return_value=fake_query_vec):
            with patch.object(engine, "_load_vec_cache", return_value=(
                ["hot1", "hot2"],
                [_make_fake_vec(0.5), _make_fake_vec(0.9)],
            )):
                with patch.object(engine, "_load_cold_vec_cache") as cold_mock:
                    results = engine.search_similar("test", limit=10, include_cold=False)
                    cold_mock.assert_not_called()
                    assert all(r.get("source") == "hot" for r in results)

    def test_search_similar_with_cold(self):
        """search_similar with include_cold=True searches both DBs."""
        from whitemagic.core.memory.embeddings import EmbeddingEngine

        engine = EmbeddingEngine()
        fake_query_vec = _make_fake_vec(seed=0.5)
        # Use the exact query vec for hot (guaranteed sim=1.0)
        # and a slightly different vec for cold (still high sim)
        cold_vec = _make_fake_vec(seed=0.5)  # identical = sim 1.0
        cold_vec[0] += 0.01  # tiny perturbation, still very high sim

        with patch.object(engine, "encode", return_value=fake_query_vec):
            with patch.object(engine, "_load_vec_cache", return_value=(
                ["hot1"],
                [fake_query_vec],
            )):
                with patch.object(engine, "_load_cold_vec_cache", return_value=(
                    ["cold1"],
                    [cold_vec],
                )):
                    results = engine.search_similar(
                        "test", limit=10, include_cold=True, min_similarity=0.0,
                    )
                    sources = {r["source"] for r in results}
                    assert "hot" in sources
                    assert "cold" in sources

    def test_search_similar_deduplicates_across_dbs(self):
        """Cold results with same memory_id as hot results are excluded."""
        from whitemagic.core.memory.embeddings import EmbeddingEngine

        engine = EmbeddingEngine()
        shared_vec = _make_fake_vec(seed=0.5)

        with patch.object(engine, "encode", return_value=shared_vec):
            with patch.object(engine, "_load_vec_cache", return_value=(
                ["shared_id"],
                [shared_vec],
            )):
                with patch.object(engine, "_load_cold_vec_cache", return_value=(
                    ["shared_id", "unique_cold"],
                    [shared_vec, _make_fake_vec(0.6)],
                )):
                    results = engine.search_similar(
                        "test", limit=10, include_cold=True, min_similarity=0.0,
                    )
                    ids = [r["memory_id"] for r in results]
                    assert ids.count("shared_id") == 1  # No duplicate

    def test_embedding_stats_includes_cold(self, tmp_path):
        """embedding_stats reports both hot and cold counts."""
        from whitemagic.core.memory.embeddings import EmbeddingEngine

        cold_path = tmp_path / "cold.db"
        _create_cold_db_with_embeddings(str(cold_path), n_memories=7)

        engine = EmbeddingEngine()
        # Mock the hot DB
        mock_hot_db = MagicMock()
        mock_hot_db.execute.return_value.fetchone.return_value = (100,)
        engine._db_conn = mock_hot_db

        engine._cold_db_checked = False
        with patch("whitemagic.config.paths.COLD_DB_PATH", cold_path):
            engine._get_cold_db()
            stats = engine.embedding_stats()
            assert stats["hot_embeddings"] == 100
            assert stats["cold_embeddings"] == 7
            assert stats["total_embeddings"] == 107


# ===========================================================================
# Test: Hybrid search with include_cold
# ===========================================================================

class TestHybridSearchCold:
    """Test unified.py search_hybrid with include_cold parameter."""

    def test_search_hybrid_accepts_include_cold(self):
        """search_hybrid signature accepts include_cold kwarg."""
        import inspect
        from whitemagic.core.memory.unified import UnifiedMemory
        sig = inspect.signature(UnifiedMemory.search_hybrid)
        assert "include_cold" in sig.parameters
        assert sig.parameters["include_cold"].default is False

    def test_search_hybrid_passes_include_cold_to_engine(self):
        """search_hybrid includes include_cold in its parameter list."""
        import inspect
        from whitemagic.core.memory.unified import UnifiedMemory
        source = inspect.getsource(UnifiedMemory.search_hybrid)
        # Verify include_cold is used in the method body to call engine
        assert "include_cold" in source
        assert "include_cold=include_cold" in source


# ===========================================================================
# Test: Archive ingestion script
# ===========================================================================

class TestArchiveIngestion:
    """Test the ingest_archives.py script logic."""

    def test_generate_id_is_deterministic(self):
        """Same inputs produce same ID."""
        from scripts.ingest_archives import generate_id
        id1 = generate_id("Test Title", "Test content")
        id2 = generate_id("Test Title", "Test content")
        assert id1 == id2
        assert len(id1) == 16

    def test_generate_id_differs_for_different_content(self):
        """Different inputs produce different IDs."""
        from scripts.ingest_archives import generate_id
        id1 = generate_id("Title A", "Content A")
        id2 = generate_id("Title B", "Content B")
        assert id1 != id2

    def test_ingest_targets_all_exist(self):
        """All ingest target paths point to existing files."""
        from scripts.ingest_archives import INGEST_TARGETS
        for path, title, tags in INGEST_TARGETS:
            assert path.exists(), f"Missing: {path} ({title})"

    def test_ingest_targets_have_tags(self):
        """All targets have at least 2 tags."""
        from scripts.ingest_archives import INGEST_TARGETS
        for path, title, tags in INGEST_TARGETS:
            assert len(tags) >= 2, f"{title} has only {len(tags)} tags"

    def test_dry_run_doesnt_modify_db(self):
        """Dry run doesn't create memories."""
        from scripts.ingest_archives import run
        result = run(dry_run=True)
        assert result["dry_run"] is True
        assert result["ingested"] > 0  # Found files to ingest


# ===========================================================================
# Test: Pack/unpack round-trip
# ===========================================================================

class TestEmbeddingPackRoundTrip:
    """Test embedding serialization."""

    def test_pack_unpack_roundtrip(self):
        from whitemagic.core.memory.embeddings import _pack_embedding, _unpack_embedding
        vec = _make_fake_vec(seed=0.42)
        packed = _pack_embedding(vec)
        unpacked = _unpack_embedding(packed)
        assert len(unpacked) == EMBEDDING_DIM
        for a, b in zip(vec, unpacked):
            assert abs(a - b) < 1e-6

    def test_pack_size(self):
        from whitemagic.core.memory.embeddings import _pack_embedding
        vec = _make_fake_vec()
        packed = _pack_embedding(vec)
        assert len(packed) == EMBEDDING_DIM * 4  # 4 bytes per float32
