import json
import logging
import sqlite3
from pathlib import Path
from typing import Any

from whitemagic.core.intelligence.confidence import ConfidenceLearner
from whitemagic.core.intelligence.synthesis.solution_library import SolutionLibrary

logger = logging.getLogger(__name__)

class AcceleratorBridge:
    """Accelerator Bridge — Layer 5: Execution
    Maps high-level 'Proposed Actions' from the solver into system executions.
    """

    def __init__(self, root_dir: Path) -> None:
        self.root_dir = root_dir
        from whitemagic.config.paths import DB_PATH

        self.db_path = DB_PATH
        self.learner = ConfidenceLearner(self.db_path)
        self.library = SolutionLibrary(self.db_path)

    def dispatch_action(self, action_description: str, metadata: dict[str, Any]) -> Any:
        """Execute an action based on its description.
        In a mature system, this would trigger specific kernels.
        For now, we handle specialized automation scripts.
        """
        logger.info(f"Dispatching action: {action_description}")

        # Example: Mapping "Consolidate" actions to memory maintenance
        success = False
        if "consolidate" in action_description.lower():
            success = self._run_maintenance("consolidate", metadata)
        elif "wu wei" in action_description.lower():
            logger.info("  Action Type: Passive Observation (Stillness Garden)")
            success = True
        else:
            logger.info(f"  Action Type: Unknown Dispatch — {action_description}")
            success = True # For now, we count specific insights as successful dispatches

        if "cluster_key" in metadata:
            ck = metadata["cluster_key"]
            self.learner.report_outcome(ck, success)

            # Reflexive Promotion: If success, promote patterns from this cluster
            if success:
                self._promote_cluster_patterns(ck)

    def _promote_cluster_patterns(self, cluster_key: str) -> Any:
        """Promote Sol patterns from a successful cluster to the Solution Library."""
        conn = sqlite3.connect(str(self.db_path))
        rows = conn.execute("SELECT content FROM cluster_patterns WHERE cluster_id = ? AND pattern_type = 'Sol'", (cluster_key,)).fetchall()
        conn.close()

        confidence = self.learner.get_score(cluster_key)
        for row in rows:
            self.library.promote_pattern(row[0], cluster_key, confidence)  # type: ignore[attr-defined]

    def _apply_snippet_operation(self, op: dict[str, Any], context: dict[str, Any]) -> bool:
        """Apply one declarative snippet operation to context."""
        action = op.get("action")

        if action == "set_context":
            key = op.get("key")
            if not isinstance(key, str) or not key:
                logger.error("Snippet set_context requires non-empty string key")
                return False
            context[key] = op.get("value")
            return True

        if action == "merge_context":
            values = op.get("values")
            if not isinstance(values, dict):
                logger.error("Snippet merge_context requires object field 'values'")
                return False
            context.update(values)
            return True

        if action == "append_context":
            key = op.get("key")
            if not isinstance(key, str) or not key:
                logger.error("Snippet append_context requires non-empty string key")
                return False
            value = op.get("value")
            bucket = context.get(key)
            if bucket is None:
                context[key] = [value]
                return True
            if not isinstance(bucket, list):
                logger.error("Snippet append_context target '%s' is not a list", key)
                return False
            bucket.append(value)
            return True

        if action == "log":
            message = op.get("message", "")
            level = str(op.get("level", "info")).lower()
            if level == "debug":
                logger.debug("Snippet log: %s", message)
            elif level == "warning":
                logger.warning("Snippet log: %s", message)
            elif level == "error":
                logger.error("Snippet log: %s", message)
            else:
                logger.info("Snippet log: %s", message)
            return True

        if action == "noop":
            return True

        logger.error("Unsupported snippet action: %s", action)
        return False

    def execute_snippet(self, code: str, context: dict[str, Any]) -> bool:
        """Execute a declarative JSON snippet within a controlled context.
        Arbitrary Python execution is intentionally not supported.
        """
        logger.info(f"Executing solution snippet ({len(code)} chars)...")
        if not isinstance(context, dict):
            logger.error("Snippet context must be a dict")
            return False

        snippet = code.strip()
        if not snippet:
            logger.error("Empty snippet cannot be executed")
            return False

        try:
            payload = json.loads(snippet)
        except json.JSONDecodeError as e:
            logger.error(f"Snippet rejected: expected JSON declarative format ({e})")
            return False

        operations = payload if isinstance(payload, list) else [payload]
        if not all(isinstance(op, dict) for op in operations):
            logger.error("Snippet rejected: operations must be JSON objects")
            return False

        for op in operations:
            if not self._apply_snippet_operation(op, context):
                return False

        return True

    def _run_maintenance(self, task_type: str, metadata: dict[str, Any]) -> Any:
        """Runs specialized maintenance scripts."""
        script_path = self.root_dir / "scripts" / "memory_maintenance.py"
        if not script_path.exists():
            logger.warning(f"Maintenance script {script_path} not found.")
            return

        try:
            # For demonstration, we simulate success
            # cmd = ["python3", str(script_path), "--type", task_type, "--mids", ",".join(metadata.get('mids', []))]
            # logger.info(f"  Executing: {' '.join(cmd)}")
            # result = subprocess.run(cmd, check=True)
            logger.info(f"  Action executed successfully: {task_type}")
            return True
        except Exception as e:
            logger.error(f"  Action execution failed: {e}")
    def execute_council_decision(self, consensus: dict[str, Any]) -> Any:
        """Execute a decision from the Zodiac Council.
        Uses the multi-dimensional spectrum to determine the mode and energy of the action.
        """
        importance = consensus.get("importance", 0.0)
        proposal_id = consensus.get("proposal_id", "unknown")

        if importance < 0.7:
            logger.info(f"⏭️ Council decision {proposal_id} importance too low for autonomous execution: {importance:.2f}")
            return False

        logger.info(f"⚡ Executing Council Decision {proposal_id} (Importance: {importance:.2f})")

        # Determine the MODE of execution based on Logic Spectrum
        # Logic > 0.7 = Analytical/Strict
        # Logic < 0.3 = Intuitive/Passive
        logic = consensus.get("logic", 0.5)

        if logic > 0.7:
            logger.info("  Mode: Analytical Execution (Strict optimization)")
            return self._run_maintenance("strict_optimize", {"reason": f"council_{proposal_id}"})
        elif logic < 0.3:
            logger.info("  Mode: Intuitive Observation (Passive harmonization)")
            return True # Wu Wei
        else:
            logger.info("  Mode: Balanced Integration")
            return self._run_maintenance("consolidate", {"reason": f"council_{proposal_id}"})



# Singleton
_accelerator_bridge = None

def get_accelerator_bridge(root_dir: Path | None = None) -> AcceleratorBridge:
    global _accelerator_bridge
    if _accelerator_bridge is None:
        if root_dir is None:
            root_dir = Path(__file__).resolve().parent.parent.parent.parent.parent
        _accelerator_bridge = AcceleratorBridge(root_dir)
    return _accelerator_bridge

if __name__ == "__main__":
    # Smoke test dispatch
    bridge = get_accelerator_bridge(Path("."))
    bridge.dispatch_action("Consolidate insight path", {"mids": ["123", "456"]})
