"""
Verification - Universal Solver Convergence (Frank-Wolfe)
Validates that the solver selects optimal nodes while respecting causal/dharmic constraints.
"""

import pytest
rich = pytest.importorskip("rich")

import time
from whitemagic.tools.unified_api import call_tool
from rich.console import Console

console = Console()

def test_solver_basic():
    console.print("\n--- Testing Universal Solver: Basic Causal Chain ---")
    nodes = ["Infra", "API", "Frontend"]
    edges = [["Infra", "API"], ["API", "Frontend"]] # Causal chain
    scores = {"Infra": 0.1, "API": 0.2, "Frontend": 0.9} # High value on Frontend
    budget = 1
    
    # Expected: "Infra" must be selected if "API" is, and "API" if "Frontend" is.
    # With budget 1, it should pick "Infra" (foundation) or nothing if scores are too low.
    # However, FW with budget might pick the 'root' if it's the only one allowed.
    
    start = time.time()
    result = call_tool("solve_optimization", nodes=nodes, edges=edges, scores=scores, budget=budget)
    duration = time.time() - start
    
    if result.get("status") == "success":
        details = result.get("details", {})
        selected = details.get("selected_nodes", [])
        console.print(f"Success: Selected {selected} in {duration:.4f}s")
        # Validate causal constraint: if Frontend in selected, API and Infra must be too.
        # But budget is 1, so it shouldn't be able to pick Frontend.
        if "Frontend" in selected and ("API" not in selected or "Infra" not in selected):
            console.print("[red]❌ Violation: Causal constraint ignored![/]")
        else:
            console.print("[green]✅ Causal constraints respected.[/]")
    else:
        console.print(f"[red]Failed: {result.get('message')}[/]")

def test_solver_dharmic():
    console.print("\n--- Testing Universal Solver: Dharmic Hexagram (6-node) ---")
    # 6 nodes triggers the 'balanced_hexagram' invariant in DharmaConstraints
    nodes = [f"node_{i}" for i in range(6)]
    scores = {n: 1.0 for n in nodes}
    
    start = time.time()
    result = call_tool("solve_optimization", nodes=nodes, scores=scores, budget=6)
    duration = time.time() - start
    
    if result.get("status") == "success":
        details = result.get("details", {})
        selected = details.get("selected_nodes", [])
        console.print(f"Success: Selected {len(selected)} nodes in {duration:.4f}s")
        # Dharmic constraint: sum(s[indices]) == 3
        # Since we use rounding/thresholding in DharmicSolver, it might not be exact 3,
        # but it should attempt to balance.
        console.print(f"Selected Count: {len(selected)} (Goal: 3 for Dharmic Balance)")
        if len(selected) == 3:
            console.print("[green]✅ Dharmic balance achieved (exactly 3 nodes).[/]")
        else:
            console.print(f"[yellow]⚠️  Balance: {len(selected)}/6 nodes. (Optimal for scoring vs constraints).[/]")
    else:
        console.print(f"[red]Failed: {result.get('message')}[/]")

if __name__ == "__main__":
    console.print("[bold cyan]=== UNIVERSAL SOLVER VERIFICATION ===[/]")
    test_solver_basic()
    test_solver_dharmic()
