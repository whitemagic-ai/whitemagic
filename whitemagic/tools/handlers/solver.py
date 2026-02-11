"""Whitemagic v13 - Universal Solver Tool Handler
Exposes the DharmicSolver (Frank-Wolfe optimization) as an MCP tool.
"""

import logging
from typing import Any

from whitemagic.core.intelligence.synthesis.solver_engine import DharmicSolver

logger = logging.getLogger(__name__)

def handle_solve_optimization(**kwargs: Any) -> dict[str, Any]:
    """Solve a constrained optimization problem.
    Arguments:
        nodes: List of strings (decision variables)
        edges: List of [parent, child] pairs (causal constraints)
        scores: Dict of node -> weight (objective targets)
        budget: Optional integer (max nodes to select)
        max_iters: Optional integer.

    """
    nodes = kwargs.get("nodes")
    if not nodes:
        return {
            "status": "error",
            "error_code": "NO_NODES",
            "message": "Optimization requires a list of nodes.",
        }

    # Standardize edges format (convert lists to tuples for the engine)
    raw_edges = kwargs.get("edges", [])
    edges = [(e[0], e[1]) for e in raw_edges if len(e) >= 2]

    scores = kwargs.get("scores", {})
    budget = kwargs.get("budget")
    max_iters = kwargs.get("max_iters", 50)

    try:
        solver = DharmicSolver()
        selected = solver.solve(
            nodes=nodes,
            edges=edges,
            scores=scores,
            budget=budget,
            max_iters=max_iters,
        )

        return {
            "status": "success",
            "selected_nodes": selected,
            "node_count": len(selected),
            "engine": "dharmic_solver_v3_fw",
        }
    except Exception as e:
        logger.exception("Solver failed")
        return {
            "status": "error",
            "error_code": "SOLVER_ERROR",
            "message": str(e),
        }
