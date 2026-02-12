import hashlib
import json
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import numpy as np


@dataclass
class DGAHistory:
    """Historical trace of a System DNA's lineage."""

    parent_signature: str | None
    mutation_delta: float
    timestamp: float
    version: str

class DGAEngine:
    """Digital Genetic Ancestry (DGA) Engine.
    Calculates 512-bit vectors representing an agent's 'System DNA'.

    The DNA is derived from:
    1. Base Architecture (Gana configuration)
    2. Active Sub-clusters (Memory focus)
    3. Performance Metrics (Phenotype)
    4. Parental Inheritance (Lineage)
    """

    def __init__(self, workspace_root: Path | None = None):
        self.workspace_root = workspace_root or Path.cwd()
        self.dna_dims = 512

    def generate_signature(self, parent_sig: str | None = None) -> str:
        """Generate a unique 512-bit DGA signature for this instance."""
        # 1. Capture Genotype (Config/Architecture)
        genotype = self._capture_genotype()

        # 2. Capture Phenotype (Performance/State)
        phenotype = self._capture_phenotype()

        # 3. Combine and Hash
        raw_data = {
            "genotype": genotype,
            "phenotype": phenotype,
            "parent": parent_sig,
            "timestamp": time.time(),
            "entropy": self._get_system_entropy(),
        }

        encoded = json.dumps(raw_data, sort_keys=True).encode()
        return hashlib.sha512(encoded).hexdigest()

    def calculate_distance(self, sig_a: str, sig_b: str) -> float:
        """Calculate evolutionary distance between two DGA signatures.
        Uses Cosine Distance on the normalized bit-vectors.
        """
        vec_a = self._sig_to_vector(sig_a)
        vec_b = self._sig_to_vector(sig_b)
        denominator = float(np.linalg.norm(vec_a) * np.linalg.norm(vec_b))
        if denominator == 0.0:
            return 1.0
        cosine_similarity = float(np.dot(vec_a, vec_b) / denominator)
        return float(1.0 - cosine_similarity)

    def _sig_to_vector(self, sig: str) -> np.ndarray:
        """Turn a hex signature into a numeric vector."""
        # Convert hex to bits, then bits to floats
        raw_bits = bin(int(sig, 16))[2:].zfill(512)
        return np.array([float(b) for b in raw_bits])  # type: ignore[no-any-return]

    def _capture_genotype(self) -> dict[str, Any]:
        """Aggregate configuration-level features."""
        try:
            # Look for Gana files/registry
            ganas_dir = self.workspace_root / "whitemagic" / "core" / "ganas"
            ganas = [f.stem for f in ganas_dir.glob("*.py") if not f.name.startswith("__")]
            return {
                "ganas_count": len(ganas),
                "ganas_list": sorted(ganas),
                "core_version": "6.0.0",
            }
        except Exception:
            return {"error": "failed_genotype_capture"}

    def _capture_phenotype(self) -> dict[str, Any]:
        """Aggregate runtime-level features."""
        return {
            "uptime": 0, # Should integrate with heartbeat
            "resonance_profile": "StandardV5",
        }

    def _get_system_entropy(self) -> str:
        """Gather pseudo-random system markers."""
        try:
            import os
            return str(os.getloadavg()) + str(os.cpu_count())
        except Exception:
            return "static_entropy"

def get_dga_engine() -> DGAEngine:
    return DGAEngine()

if __name__ == "__main__":
    engine = get_dga_engine()
    sig1 = engine.generate_signature()
    time.sleep(0.1)
    sig2 = engine.generate_signature(parent_sig=sig1)

    print(f"🧬 Ancestral DGA: {sig1[:16]}...")
    print(f"🌱 Forked DGA:    {sig2[:16]}...")
    print(f"📐 Distance:      {engine.calculate_distance(sig1, sig2):.4f}")
