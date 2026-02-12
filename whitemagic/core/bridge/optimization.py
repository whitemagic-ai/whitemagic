
import os
from typing import Any

from whitemagic.core.bridge.utils import logger


def _local_models_enabled() -> bool:
    return os.getenv("WHITEMAGIC_ENABLE_LOCAL_MODELS", "").strip().lower() in {
        "1",
        "true",
        "yes",
        "on",
    }

def optimize_cache(
    operation: str = "stats",
    **_: Any,
) -> dict[str, Any]:
    """Cache optimization shim.

    The legacy bridge expects an `optimize_cache` entry point, but cache internals
    were reorganized. Keep this function to avoid import-time failures.
    """
    return {
        "status": "archived",
        "operation": operation,
        "message": "Cache optimization via the legacy bridge is not available in this build.",
    }

def optimize_models(
    operation: str = "elemental_strategy",
    task_type: str = "general",
    model_id: str | None = None,
    model_path: str | None = None,
    output_path: str | None = None,
    quant_type: str = "q4",
    model_name: str | None = None,
    **kwargs: Any,
) -> dict[str, Any]:
    """Optimize model usage strategy."""
    import asyncio
    from pathlib import Path

    from whitemagic.core.intelligence.elemental_optimization import (
        get_elemental_optimizer,
    )

    op = (operation or "elemental_strategy").lower()

    if op == "elemental_strategy":
        optimizer = get_elemental_optimizer()
        strategy = optimizer.get_optimization_strategy(task_type)
        # Convert Enum values to strings for JSON serialization
        if isinstance(strategy, dict):
            result = {}
            for k, v in strategy.items():
                if hasattr(v, "value"):  # Enum
                    result[k] = v.value
                else:
                    result[k] = v
            return result
        return {"strategy": str(strategy)}

    if op == "batch_optimizer":
        if not model_id:
            return {"error": "model_id is required for batch_optimizer"}
        if not _local_models_enabled():
            return {
                "status": "archived",
                "operation": op,
                "message": (
                    "Batch optimizer depends on archived embedded/local-model inference. "
                    "Set WHITEMAGIC_ENABLE_LOCAL_MODELS=1 to enable legacy support."
                ),
            }
        try:
            from whitemagic._archived.local_models.llm.llm_speculative import (  # type: ignore[import-not-found]
                create_batch_optimizer,
            )
        except Exception as exc:
            return {
                "status": "error",
                "operation": op,
                "message": "Failed to load legacy batch optimizer (archived module import failed).",
                "error": str(exc),
            }
        optimizer = asyncio.run(create_batch_optimizer(model_id=model_id))
        return {"status": "initialized", "stats": optimizer.get_stats()}

    if op == "export_onnx":
        from whitemagic.edge.onnx_export import ONNXExporter

        exporter = ONNXExporter()

        # Determine output path
        if output_path:
            out = Path(output_path)
        elif model_name:
            out = Path(f"{model_name}.onnx")
        else:
            out = Path("model.onnx")

        # Export current edge inference rules
        try:
            result = exporter.export(out, format="onnx")
            return result
        except Exception as e:
            return {"error": f"ONNX export failed: {str(e)}"}

    if op == "quantize":
        # Placeholder for quantization logic
        # Real implementation would leverage llama.cpp tools
        return {
            "status": "simulated",
            "message": f"Quantization to {quant_type} for {model_name or model_path or 'unknown model'} simulated.",
            "quant_type": quant_type,
            "output_path": output_path or f"{model_name or 'model'}.{quant_type}.gguf",
        }

    return {"error": f"Unknown optimize_models operation: {operation}"}

def solve_optimization(
    nodes: list[str],
    edges: list[list[str]],  # List of [parent, child]
    scores: dict[str, float],
    max_iters: int = 50,
    budget: int | None = None,
    lambda_reg: float = 0.01,
    **kwargs: Any,
) -> dict[str, Any]:
    """Solve a constrained optimization problem using the Universal Solver (Frank-Wolfe).
    """
    try:
        from whitemagic.core.intelligence.synthesis.solver_engine import DharmicSolver

        # Convert edges list to tuple list
        edge_tuples = [(e[0], e[1]) for e in edges if len(e) >= 2]

        solver = DharmicSolver()
        selected_nodes = solver.solve(
            nodes=nodes,
            edges=edge_tuples,
            scores=scores,
            max_iters=max_iters,
            lambda_reg=lambda_reg,
            budget=budget,
        )

        return {
            "status": "success",
            "selected_nodes": selected_nodes,
            "count": len(selected_nodes),
            "algorithm": "Frank-Wolfe (Entropy Regularized)",
            "parameters": {
                "max_iters": max_iters,
                "budget": budget,
                "lambda_reg": lambda_reg,
            },
        }
    except ImportError:
        return {
            "status": "error",
            "message": "Optimization dependencies missing. Install 'whitemagic[opt]' or 'whitemagic[numeric]'.",
            "missing_module": "cvxpy or numpy",
        }
    except Exception as e:
        logger.error(f"Solver Error: {e}")
        return {
            "status": "error",
            "message": f"Solver execution failed: {str(e)}",
        }
