"""Satkona Yang Pass — Holographic Alignment Scorer (Mojo)
==========================================================
Scores each memory cluster by its geometric alignment to known
strategy prototypes in 4D holographic space.

  yang_score[c] = max_p cosine(centroid[c], prototype[p]) × (1 + pr[c])

Input:  JSON string via argv[1]
          centroids   – flat Float64 array, stride 4, len = n_clusters*4
          prototypes  – flat Float64 array, stride 4, len = n_protos*4
          n_clusters  – Int
          n_protos    – Int

Output: stdout line  YANG:sim0,sim1,...   (raw max-cosine per cluster)
"""

import sys
import math
from python import Python


fn _cosine4(
    ax: Float64, ay: Float64, az: Float64, aw: Float64,
    bx: Float64, by: Float64, bz: Float64, bw: Float64,
) -> Float64:
    """4D cosine similarity — compiled tight loop."""
    var dot  = ax * bx + ay * by + az * bz + aw * bw
    var na   = math.sqrt(ax * ax + ay * ay + az * az + aw * aw)
    var nb   = math.sqrt(bx * bx + by * by + bz * bz + bw * bw)
    if na == 0.0 or nb == 0.0:
        return 0.0
    return dot / (na * nb)


fn main() raises:
    var json_mod = Python.import_module("json")
    var args     = sys.argv()

    if len(args) < 2:
        print("Usage: satkona_yang <json_payload>")
        return

    var data       = json_mod.loads(args[1])
    var n_clusters: Int = Int(String(data["n_clusters"]))
    var n_protos:   Int = Int(String(data["n_protos"]))

    # ── deserialize float arrays (PythonObject → String → Float64) ─
    var py_cent  = data["centroids"]
    var py_proto = data["prototypes"]

    var centroids  = List[Float64]()
    var prototypes = List[Float64]()

    for i in range(len(py_cent)):
        centroids.append(Float64(String(py_cent[i])))
    for i in range(len(py_proto)):
        prototypes.append(Float64(String(py_proto[i])))

    # ── hot loop: max cosine sim per cluster + fusion ─────────────
    var out = String("YANG:")
    for c in range(n_clusters):
        var cx = centroids[c * 4    ]
        var cy = centroids[c * 4 + 1]
        var cz = centroids[c * 4 + 2]
        var cw = centroids[c * 4 + 3]

        var best: Float64 = 0.0
        for p in range(n_protos):
            var sim = _cosine4(
                cx, cy, cz, cw,
                prototypes[p * 4    ],
                prototypes[p * 4 + 1],
                prototypes[p * 4 + 2],
                prototypes[p * 4 + 3],
            )
            if sim > best:
                best = sim

        if c > 0:
            out += ","
        out += String(best)

    print(out)
