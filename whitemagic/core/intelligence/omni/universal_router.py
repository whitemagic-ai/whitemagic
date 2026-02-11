"""Universal Router - The Cognitive Dispatch.
=========================================

"One thought, ten thousand connections."

The Universal Router serves as the primary entry point for the Omni-Tool architecture.
It accepts natural language intents and "routes" them to a sequence of Gana operations.

It operates in three stages:
1. CLASSIFY: Determine the domain and complexity of the request.
2. MAP: Select the optimal Gana chain (The "Constellation").
3. EXECUTE: Orchestrate the serial or parallel execution of the Ganas.
"""

import logging
from dataclasses import dataclass
from typing import Any

logger = logging.getLogger(__name__)

@dataclass
class GanaStep:
    """A single step in a routed chain."""

    mansion: str  # Name of the Lunar Mansion (e.g., "NET", "VOID")
    operation: str # search, analyze, transform, consolidate
    context_key: str # What specific aspect to focus on
    parameters: dict[str, Any]

@dataclass
class ExecutionChain:
    """A planned sequence of Gana operations."""

    intent: str
    steps: list[GanaStep]
    estimated_complexity: float
    required_capabilities: list[str]

class UniversalRouter:
    """Maps intents to Gana Constellations.
    """

    def __init__(self) -> None:
        self.known_skills: dict[str, Any] = {} # Cache for "Forged" skills
        self._bootstrap_default_routes()

    def _bootstrap_default_routes(self) -> None:
        """Load some hard-coded heuristical routes for bootstrapping."""
        self.default_routes = {
            "trade": [
                GanaStep("NET", "search", "market_data", {}),
                GanaStep("ROOT", "analyze", "dependencies", {}),
                GanaStep("VOID", "transform", "optimize_arbitrage", {}),
                GanaStep("TAIL", "transform", "execute_order", {}),
            ],
            "research": [
                GanaStep("NET", "search", "information_retrieval", {}),
                GanaStep("GHOST", "analyze", "introspection_check", {}), # Bias check
                GanaStep("WINNOWING_BASKET", "consolidate", "filter_noise", {}),
            ],
        }

    def route(self, intent: str) -> ExecutionChain:
        """Main entry point: Intent -> Chain.
        Uses Whitemagic Brain (LLM) to decide the best Gana Constellation.
        """
        # 1. Check Forged Skills first (The "Muscle Memory")
        # In a real implementation, we would fuzzy match against self.known_skills

        # 2. Consult the Oracle (LLM)
        logger.info(f"compass Routing intent via LLM: '{intent}'")

        try:
            from whitemagic.brain import generate  # type: ignore[import-not-found]

            prompt = f"""
            You are the Universal Router for Whitemagic. Map the following user intent to a sequence of Gana Operations.

            Available Ganas:
            - NET: Search/Capture data
            - ROOT: Analyze dependencies
            - VOID: Optimize/Transform (Math)
            - TAIL: Execute/Action
            - GHOST: Introspect/Check Bias
            - HORN: Initialize/Query

            Intent: "{intent}"

            Return ONLY a JSON list of steps. Format:
            [
              {{"mansion": "MANSION_NAME", "operation": "search|analyze|transform|consolidate", "context": "focus_area"}}
            ]
            """

            response = generate(prompt)

            if "response" in response:
                import json
                try:
                    # Clean the response to ensure valid JSON (basic cleanup)
                    json_str = response["response"].strip()
                    if json_str.startswith("```json"):
                        json_str = json_str[7:-3]

                    steps_data = json.loads(json_str)

                    steps = [
                        GanaStep(
                            mansion=s["mansion"].upper(),
                            operation=s["operation"],
                            context_key=s["context"],
                            parameters={},
                        ) for s in steps_data
                    ]

                    if steps:
                        return ExecutionChain(
                            intent=intent,
                            steps=steps,
                            estimated_complexity=len(steps) * 0.8,
                            required_capabilities=["llm_reasoning"],
                        )
                except Exception as e:
                    logger.warning(f"Failed to parse LLM route: {e}. Falling back to default.")

        except ImportError:
            logger.warning("Whitemagic Brain not available. Using heuristic fallback.")

        # 3. Fallback Heuristics
        steps = []
        intent_lower = intent.lower()

        if "trade" in intent_lower or "market" in intent_lower or "buy" in intent_lower:
            logger.info("  Matched domain: TRADING (heuristic)")
            steps = self.default_routes["trade"]

        elif "research" in intent_lower or "learn" in intent_lower or "find" in intent_lower:
            logger.info("  Matched domain: RESEARCH (heuristic)")
            steps = self.default_routes["research"]

        else:
            logger.info("  Matched domain: GENERAL (heuristic)")
            steps = [
                GanaStep("HORN", "search", "initiate_query", {"query": intent}),
                GanaStep("HEART", "analyze", "core_understanding", {}),
                GanaStep("ROOM", "transform", "lock_result", {}),
            ]

        return ExecutionChain(
            intent=intent,
            steps=steps,
            estimated_complexity=len(steps) * 0.5,
            required_capabilities=["basic_reasoning"],
        )

    async def execute(self, chain: ExecutionChain, initial_context: dict[str, Any] | None = None) -> dict[str, Any]:
        """Execute the routed chain using REAL Gana instances.
        """
        logger.info(f"🚀 Executing chain with {len(chain.steps)} steps...")

        context = {"original_intent": chain.intent}
        if initial_context:
            context.update(initial_context)
        from whitemagic.core.ganas.base import GanaCall, GanaOperation
        from whitemagic.core.ganas.registry import get_gana_for_tool

        for i, step in enumerate(chain.steps):
            logger.info(f"  [{i+1}/{len(chain.steps)}] Invoking {step.mansion} ({step.operation})...")

            # Map Mansion name to tool name (e.g., "NET" -> "gana_net")
            tool_name = f"gana_{step.mansion.lower()}"
            gana = get_gana_for_tool(tool_name)

            if gana:
                # Create the Call object
                GanaCall(
                    task=f"Omni-Tool Exec: {step.operation} on {step.context_key}",
                    state_vector=context,
                )

                # Convert string operation to Enum
                op_enum = GanaOperation(step.operation.lower())

                # Create dynamic kwargs for dispatch
                dispatch_kwargs: dict[str, Any] = {}
                if op_enum == GanaOperation.SEARCH:
                    # search(query: str, context=...)
                    dispatch_kwargs["query"] = step.context_key
                    dispatch_kwargs["context"] = context
                elif op_enum == GanaOperation.ANALYZE:
                    # analyze(data: Any, context=...)
                    dispatch_kwargs["data"] = context.get(step.context_key) or step.context_key
                    dispatch_kwargs["context"] = context
                elif op_enum == GanaOperation.TRANSFORM:
                    # transform(data: Any, transformation: str, context=...)
                    # For transform, context_key is the data key, parameters usually has 'transformation'
                    dispatch_kwargs["data"] = context.get(step.context_key)
                    dispatch_kwargs["transformation"] = step.parameters.get("transformation", "default")
                    dispatch_kwargs["context"] = context
                elif op_enum == GanaOperation.CONSOLIDATE:
                    # consolidate(items: List, context=...)
                    data_item = context.get(step.context_key)
                    if isinstance(data_item, list):
                        dispatch_kwargs["items"] = data_item
                    else:
                        dispatch_kwargs["items"] = [data_item] if data_item else []
                    dispatch_kwargs["context"] = context

                # Merge extra parameters but exclude keys we just set to avoid "multiple values" error
                for k, v in step.parameters.items():
                    if k not in dispatch_kwargs:
                        dispatch_kwargs[k] = v

                # Dispatch!
                try:
                    result = await gana.dispatch_operation(
                        op_enum,
                        **dispatch_kwargs,
                    )

                    # Store result in context
                    key = f"step_{i}_{step.mansion}"
                    context[key] = result
                    logger.info(f"    ✅ Result type: {type(result)}")

                except Exception as e:
                    logger.error(f"    ❌ Execution Failed: {e}")
                    context[f"error_{i}"] = str(e)
            else:
                logger.warning(f"    ⚠️ Gana not found for {tool_name}. Skipping.")
                context[f"skipped_{i}"] = f"Gana {step.mansion} not found"

        # Auto-Forge Skill (Muscle Memory)
        try:
            from whitemagic.core.intelligence.omni.skill_forge import get_skill_forge
            forge = get_skill_forge()

            # Simple success metric: All steps produced a result and no errors
            steps_count = len(chain.steps)
            success_count = sum(1 for i in range(steps_count) if f"error_{i}" not in context and f"skipped_{i}" not in context)
            success_ratio = success_count / steps_count if steps_count > 0 else 0

            if forge.assess_pattern(chain, success_ratio):
                skill_name = f"Skill_{hash(chain.intent) % 10000}_{chain.steps[0].mansion}"
                forge.forge(chain, skill_name)

        except Exception as e:
            logger.warning(f"Failed to auto-forge skill: {e}")

        logger.info("✅ Chain execution complete.")
        return {
            "status": "success",
            "final_context": context,
            "chain_id": id(chain),
        }

# Singleton accessor
_router = None
def get_universal_router() -> UniversalRouter:
    global _router
    if _router is None:
        _router = UniversalRouter()
    return _router
