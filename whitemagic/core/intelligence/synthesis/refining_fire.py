import hashlib
import logging
from datetime import datetime

from whitemagic.core.intelligence.synthesis.solution_library import (
    Solution,
    get_solution_library,
)
from whitemagic.core.memory.unified_types import Memory

logger = logging.getLogger(__name__)

class RefiningFire:
    """Refining Fire — The Tikkun Engine.
    Processes fading memories to extract eternal patterns (Sparks) before the record is lost.
    """

    def __init__(self) -> None:
        self.library = get_solution_library()
        self.refinement_threshold = 0.3 # Memories below this importance are considered "Husks"

    def refine_memory(self, memory: Memory) -> str | None:
        """Process a fading memory. If a 'Spark' is found, add it to the Solution Library.
        Returns the solution_id if a spark was extracted, else None.
        """
        if memory.importance > self.refinement_threshold and not memory.metadata.get("force_refinement"):
            return None

        content_str = str(memory.content)

        # 1. Pattern Detection (Heuristic-based for now)
        # In a more advanced version, this would use an LLM or a specialized pattern extractor.
        spark_found = False
        spark_title = ""
        spark_desc = ""
        spark_type = "solution"

        # A. Code/Technical Spark
        if "def " in content_str or "class " in content_str or "import " in content_str:
            spark_found = True
            spark_title = f"Refined Technical Pattern (from {memory.id})"
            spark_desc = f"Extracted code primitive during Tikkun refinement of memory {memory.id}."
            spark_type = "solution"

        # B. Strategic/Insight Spark
        elif memory.importance > 0.15 and len(content_str) > 100:
            # Look for keywords indicating an insight or decision
            keywords = ["decided", "learned", "observed", "resonance", "alignment", "conflict"]
            if any(k in content_str.lower() for k in keywords):
                spark_found = True
                spark_title = f"Refined Strategic Insight (from {memory.id})"
                spark_desc = f"Synthesized behavioral or strategic pattern extracted from fading memory {memory.id}."
                spark_type = "heuristic"

        if spark_found:
            # Create a stable solution ID
            sol_id = f"spark_{hashlib.md5(content_str.encode()).hexdigest()[:8]}"

            solution = Solution(
                id=sol_id,
                title=spark_title,
                description=spark_desc,
                code_snippet=content_str if spark_type == "solution" else None,
                pattern_type=spark_type,
                confidence=memory.importance + 0.2, # Give it a slight boost for surviving refinement
                tags=list(memory.tags) + ["refined", "tikkun"],
                metadata={
                    "source_memory": memory.id,
                    "refinement_time": datetime.now().isoformat(),
                    "original_importance": memory.importance,
                },
            )

            self.library.add_solution(solution)
            logger.info(f"🔥 Refining Fire: Extracted spark '{sol_id}' from husk {memory.id}")
            return sol_id

        return None

    def process_batch(self, memories: list[Memory]) -> dict[str, str]:
        """Process a list of memories and return a mapping of memory_id -> solution_id."""
        results = {}
        for mem in memories:
            sol_id = self.refine_memory(mem)
            if sol_id:
                results[mem.id] = sol_id
        return results

# Singleton
_refining_fire = None

def get_refining_fire() -> RefiningFire:
    global _refining_fire
    if _refining_fire is None:
        _refining_fire = RefiningFire()
    return _refining_fire
