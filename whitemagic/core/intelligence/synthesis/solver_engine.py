
import logging

import cvxpy as cp
import numpy as np

logger = logging.getLogger(__name__)

class DharmicSolver:
    """Dharmic Solver — Layer 3: Optimization
    Solves for the optimal configuration of concepts/actions.

    Goal: Maximize Relevance while respecting Causal DAG constraints.
    """

    def solve(self,
              nodes: list[str],
              edges: list[tuple[str, str]],
              scores: dict[str, float],
              max_iters: int = 50,
              lambda_reg: float = 0.01,
              budget: int | None = None) -> list[str]:
        """Solve the constrained optimization problem using Frank-Wolfe.

        Objective: Maximize scores^T x - lambda * sum(x_i * log(x_i))
        Subject to: x in Marginal Polytope (0 <= x_j <= x_i <= 1)
        """
        n = len(nodes)
        if n == 0:
            return []
        node_to_idx = {node: i for i, node in enumerate(nodes)}

        # Default budget: 1/3 of nodes or min(10, n)
        if budget is None:
            budget = min(10, n // 3 + 1)

        # 1. Initialize continuous selection vector (Marginal Polytope)
        x = np.zeros(n)
        relevance = np.array([scores.get(node, 0.0) for node in nodes])

        # 2. Entropy-Regularized Frank-Wolfe Loop
        logger.info(f"🌌 Universal Solver: Starting Frank-Wolfe optimization for {n} nodes (Budget: {budget}).")
        t = 0
        for t in range(max_iters):
            # Gradient: scores - lambda * (log(x) + 1)
            # We use a smoothed gradient to avoid log(0)
            grad = relevance - lambda_reg * (np.log(x + 1e-10) + 1)

            # Linear Minimization Oracle (LMO)
            s_best = self._linear_oracle(nodes, edges, node_to_idx, grad, budget)

            # Step size (Standard FW: 2 / (t + 2))
            gamma = 2.0 / (t + 2.0)

            # Update
            new_x = x + gamma * (s_best - x)

            # Check for convergence (Dual Gap approximation)
            if np.linalg.norm(new_x - x) < 1e-6:
                x = new_x
                break
            x = new_x

        # 3. Final Projection / Thresholding
        # We use 'Budget-Aware Rounding': Sort by value and take top 'budget' nodes if they are > epsilon
        indexed_values = sorted([(x[i], nodes[i]) for i in range(n)], key=lambda p: p[0], reverse=True)
        selected = [node for val, node in indexed_values[:budget] if val > 0.1]

        logger.info(f"✅ Universal Solver: Selected {len(selected)} nodes after {t+1} iterations.")
        return selected

    def _linear_oracle(self, nodes: list[str], edges: list[tuple[str, str]], node_to_idx: dict[str, int], grad: np.ndarray, budget: int) -> np.ndarray:
        """Linear Minimization Oracle over the Marginal Polytope.
        Uses CVXPY for the LMO step over the polytope defined by causal, budget, and dharmic constraints.
        """
        n = len(nodes)
        s = cp.Variable(n)

        # Objective: Maximize grad^T s (Equivalent to Min -grad^T s)
        objective = cp.Maximize(grad @ s)

        constraints = [s >= 0, s <= 1]

        # Budget constraint
        constraints.append(cp.sum(s) <= budget)

        # Causal constraints: s_child <= s_parent
        for parent, child in edges:
            if parent in node_to_idx and child in node_to_idx:
                constraints.append(s[node_to_idx[child]] <= s[node_to_idx[parent]])

        # Dharmic Invariants (Dharma Guardrails)
        try:
            from whitemagic.core.governance.dharma_constraints import get_dharma_bridge
            dharma = get_dharma_bridge()
            invariants = dharma.get_invariants(n)
            for inv in invariants:
                if inv["type"] == "balanced_hexagram":
                    indices = inv["indices"]
                    constraints.append(cp.sum(s[indices]) == 3)
        except Exception as e:
            logger.debug(f"Dharma constraints failed to apply: {e}")

        prob = cp.Problem(objective, constraints)
        prob.solve(solver=cp.GLPK_MI if "GLPK_MI" in cp.installed_solvers() else None)

        if s.value is None:
            return np.zeros(n)
        return s.value  # type: ignore[no-any-return]

if __name__ == "__main__":
    # Smoke test
    nodes = ["A", "B", "C"]
    edges = [("A", "B")] # B depends on A
    scores = {"A": 0.5, "B": 1.0, "C": 0.8}
    solver = DharmicSolver()
    result = solver.solve(nodes, edges, scores)
    print(f"Selected: {result}")
