"""Vector Memory Search — Embedding-based similarity search.
Uses sentence-transformers if available, TF-IDF fallback otherwise.
Stores embeddings in SQLite. In-memory brute-force cosine search.
"""
from __future__ import annotations

import hashlib
import logging
import math
import os
import sqlite3
import struct
import threading
from collections import Counter
from dataclasses import dataclass
from typing import Any

logger = logging.getLogger(__name__)
HAS_SBERT = False
_sbert_model = None
_sbert_lock = threading.Lock()
_sbert_init_attempted = False
_sbert_error: str | None = None
try:
    from sentence_transformers import SentenceTransformer
    HAS_SBERT = True
except ImportError:
    pass


def _allow_remote_model_download() -> bool:
    raw = os.environ.get("WM_ALLOW_MODEL_DOWNLOAD", "0").strip().lower()
    return raw in {"1", "true", "yes", "on"}


def _get_sbert() -> Any:
    global _sbert_model, _sbert_error, _sbert_init_attempted
    if _sbert_model is None and HAS_SBERT and not _sbert_init_attempted:
        with _sbert_lock:
            if _sbert_model is None and not _sbert_init_attempted:
                _sbert_init_attempted = True
                name = os.environ.get("WM_EMBEDDING_MODEL", "all-MiniLM-L6-v2")
                try:
                    kwargs: dict[str, Any] = {}
                    if not _allow_remote_model_download():
                        # Default to local-only to avoid network stalls in offline environments.
                        kwargs["local_files_only"] = True
                    _sbert_model = SentenceTransformer(name, **kwargs)
                    _sbert_error = None
                except Exception as exc:
                    _sbert_error = str(exc)
                    logger.info("SBERT unavailable; using TF-IDF fallback: %s", exc)
                    pass
    return _sbert_model

class TFIDFEmbedder:
    def __init__(self, dim: int = 256) -> None: self._dim = dim
    def _tok(self, t: str) -> list[str]: return [w.strip(".,!?;:'\"()[]{}") for w in t.lower().split() if len(w)>2]
    def _h(self, t: str) -> int: return int(hashlib.md5(t.encode()).hexdigest(),16) % self._dim
    def encode(self, texts: list[str]) -> list[list[float]]:
        out = []
        for t in texts:
            tf = Counter(self._tok(t))
            v=[0.0]*self._dim
            for tok,c in tf.items():
                v[self._h(tok)]+=c
            n=math.sqrt(sum(x*x for x in v))
            out.append([x/n for x in v] if n>0 else v)
        return out

def _cosine(a: list[float], b: list[float]) -> float:
    try:
        from whitemagic.core.acceleration.simd_cosine import (
            cosine_similarity as _simd_cos,
        )
        return _simd_cos(a, b)
    except Exception:
        pass
    d=sum(x*y for x,y in zip(a,b))
    na=math.sqrt(sum(x*x for x in a))
    nb=math.sqrt(sum(x*x for x in b))
    return d/(na*nb) if na>0 and nb>0 else 0.0

@dataclass
class VSearchResult:
    memory_id: str
    score: float
    title: str=""
    snippet: str=""
    def to_dict(self) -> dict[str, Any]: return {"memory_id":self.memory_id,"score":round(self.score,4),"title":self.title,"snippet":self.snippet[:200]}

class VectorSearch:
    def __init__(self, db_path: str | None = None) -> None:
        self._lock = threading.Lock()
        self._db = db_path or os.path.join(os.environ.get("WM_STATE_ROOT",os.path.expanduser("~/.whitemagic")),"memory","embeddings.db")
        self._tfidf = TFIDFEmbedder()
        self._cache: dict[str,list[float]] = {}
        self._meta: dict[str,dict[str,str]] = {}
        self._init_db()
        self._load()

    def _init_db(self) -> None:
        os.makedirs(os.path.dirname(self._db),exist_ok=True)
        with sqlite3.connect(self._db) as c:
            c.execute("CREATE TABLE IF NOT EXISTS embeddings(memory_id TEXT PRIMARY KEY,vector BLOB,title TEXT DEFAULT '',snippet TEXT DEFAULT '')")

    def _load(self) -> None:
        try:
            with sqlite3.connect(self._db) as c:
                for mid,vb,t,s in c.execute("SELECT memory_id,vector,title,snippet FROM embeddings"):
                    n=len(vb)//4
                    vec=list(struct.unpack(f"{n}f",vb))
                    self._cache[mid]=vec
                    self._meta[mid]={"title":t or "","snippet":s or ""}
        except Exception:
            pass

    def _encode(self, texts: list[str]) -> list[list[float]]:
        m=_get_sbert()
        if m:
            return [e.tolist() for e in m.encode(texts,show_progress_bar=False)]
        return self._tfidf.encode(texts)

    def index_memory(self, memory_id: str, content: str, title: str = "") -> None:
        vecs = self._encode([content[:5000]])
        vec = vecs[0]
        blob = struct.pack(f"{len(vec)}f",*vec)
        snip = content[:300]
        with sqlite3.connect(self._db) as c:
            c.execute("INSERT OR REPLACE INTO embeddings(memory_id,vector,title,snippet) VALUES(?,?,?,?)",(memory_id,blob,title,snip))
        with self._lock:
            self._cache[memory_id]=vec
            self._meta[memory_id]={"title":title,"snippet":snip}

    def search(self, query: str, limit: int = 10) -> list[VSearchResult]:
        qvec = self._encode([query])[0]
        scored = []
        with self._lock:
            # Try Zig SIMD batch top-K for large corpora
            if len(self._cache) > 50:
                try:
                    from whitemagic.core.acceleration.simd_vector_batch import (
                        batch_topk_cosine,
                    )
                    ids = list(self._cache.keys())
                    vecs = list(self._cache.values())
                    topk = batch_topk_cosine(qvec, vecs, limit)
                    scored = [(ids[idx], score) for idx, score in topk]
                except Exception:
                    scored = []
            if not scored:
                for mid,vec in self._cache.items():
                    s = _cosine(qvec, vec)
                    scored.append((mid, s))
                scored.sort(key=lambda x:x[1], reverse=True)
        results = []
        for mid,s in scored[:limit]:
            m = self._meta.get(mid,{})
            results.append(VSearchResult(memory_id=mid,score=s,title=m.get("title",""),snippet=m.get("snippet","")))
        return results

    def index_count(self) -> int: return len(self._cache)

    def status(self) -> dict[str, Any]:
        return {
            "indexed": len(self._cache),
            "has_sbert": HAS_SBERT,
            "sbert_loaded": _sbert_model is not None,
            "sbert_init_attempted": _sbert_init_attempted,
            "sbert_error": _sbert_error,
            "allow_model_download": _allow_remote_model_download(),
            "model": os.environ.get("WM_EMBEDDING_MODEL", "all-MiniLM-L6-v2"),
            "db": self._db,
        }

_vs=None
_vs_lock=threading.Lock()
def get_vector_search() -> VectorSearch:
    global _vs
    if _vs is None:
        with _vs_lock:
            if _vs is None:
                _vs=VectorSearch()
    return _vs
