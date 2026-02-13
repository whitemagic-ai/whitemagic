"""Optimized Memory States (OMS) — Tradeable .mem Export Format (v15.2).
======================================================================
Exports a Galaxy as a portable, verifiable, tradeable .mem package.
Imports .mem packages into new Galaxies with Merkle verification.

The .mem format:
    manifest.json       — Metadata, pricing, compatibility, author DID
    memories.jsonl      — Memory entries (title, content, tags, coordinates, tier)
    associations.jsonl  — Edge list (source_id, target_id, weight, type)
    knowledge_graph.jsonl — Entities + relationships
    verification.json   — Merkle root of source data
    signature.json      — Ed25519 signature (optional)

Usage:
    from whitemagic.oms import get_oms_manager
    mgr = get_oms_manager()

    mgr.export_galaxy("research", "~/exports/research.mem", price_xrp=50.0)
    info = mgr.inspect("~/downloads/research.mem")
    mgr.verify("~/downloads/research.mem")
    mgr.import_mem("~/downloads/research.mem", galaxy="imported_research")
"""

from whitemagic.oms.manager import OMSManager, get_oms_manager

__all__ = ["OMSManager", "get_oms_manager"]
