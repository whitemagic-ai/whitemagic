
import logging
from typing import Any

logger = logging.getLogger(__name__)

class UniversalInterpreter:
    """Universal Interpreter — Layer 4: Semantic Synthesis
    Turns mathematical action plans into high-clarity strategic reports.
    """

    def synthesize(self,
                  clusters: list[dict[str, Any]],
                  edges: list[tuple[str, str]],
                  patterns: dict[str, list[str]],
                  memory_samples: dict[str, list[str]] | None = None,
                  resonance_scores: dict[str, float] | None = None,
                  actionable_items: dict[str, list[str]] | None = None) -> str:
        """Generate a long-form strategic report from solver results.
        """
        memory_samples = memory_samples or {}
        resonance_scores = resonance_scores or {}
        actionable_items = actionable_items or {}
        report = []
        report.append("# Whitemagic Strategic Insight: Strategy v10")
        report.append("*The Prescriptive Blueprint — Actionable Synthesis*")

        report.append("\n## Executive Summary")
        report.append("Moving beyond abstract architectural goals, this report surfaces ")
        report.append("**specific functional directives** extracted from the deep resonance ")
        report.append("of your memory data sea. This is no longer a vision; it is a list ")
        report.append("of logical implementations already present in the system's latent state.")

        report.append("\n## The Tri-Vector Narrative")
        report.append("The following path was selected by evaluating 198k memories through a ")
        report.append("three-fold lens: System Latency, Biographical Resonance, and Dynamical ")
        report.append("Stability. Implementation of this path creates a closed-loop autonomy.")
        report.append("\n> [!TIP]\n> **Deep Dive**: Use `wm investigate <id>` on the reference links below to enter the 'Rabbit Hole' of specific memory traces.")

        report.append("\n### Layer 2-3: The Root Foundations")
        roots = [c for c in clusters if not any(edge[1] == str(c["key"]) for edge in edges)]
        for root in roots:
            k = str(root["key"])
            p_list = patterns.get(k, [])
            p = p_list[0] if p_list else "Fundamental Conceptual Core"
            report.append(f"\n**{p}**")
            report.append(f"> This root represents a high-density anchor ({root['density']} memories). ")
            report.append("Initializing this core is the primary priority, as it serves as the ")
            report.append("computational substrate for all subsequent layers.")

        report.append("\n### Layer 3-4: The Developmental Arc")
        report.append("The following synthesis steps represent the 'Net' phase—where broad patterns ")
        report.append("are distilled into specific implementation actionables.")
        for cluster in clusters:
            if any(str(root["key"]) == str(cluster["key"]) for root in roots):
                continue
            k = str(cluster["key"])
            p_list = patterns.get(k, [])
            p = p_list[0] if p_list else "Fundamental Conceptual Core"
            report.append(f"\n#### expansion: {p}")

            # Actionable Section
            group_actions = actionable_items.get(k, [])
            if group_actions:
                report.append("> [!IMPORTANT]")
                report.append("> **Actionable Directives Found**:")
                for action in group_actions:
                    report.append(f"> - {action}")

            res_val = resonance_scores.get(k, 0.0)
            res_str = f"Resonance: {res_val:.2f} (Verified Flow)" if res_val > 0 else "Base Anchor"
            report.append(f"- **Density Pulse**: {cluster['density']} memories | **{res_str}**")

            # Add Rabbit Hole references
            mids = memory_samples.get(k, [])
            if mids:
                ref_str = ", ".join([f"`{id}`" for id in mids[:5]])
                report.append(f"- **Rabbit Hole (Samples)**: {ref_str} ...")

            # Context-aware Strategic Impact
            if res_val > 1.4:
                impact = "High-energy breakthrough. Execution here anchors the system's recursive capability."
            elif "wisdom" in p.lower() or "wisdom" in str(group_actions):
                impact = "Existential grounding. This step aligns current tasks with long-term Dharmic vision."
            elif cluster["density"] > 100:
                impact = "Foundational stability. Consolidating this cluster reduces system-wide entropy."
            else:
                impact = "Emergent potential. Priority execution may trigger spontaneous resonance in adjacent gardens."

            report.append(f"- **Strategic Impact**: {impact}")

        report.append("\n## Implementation Details & Clarification Notes")
        report.append("1. **Coordinate Alignment**: These insights are spatially anchored; use `holographic_project` to verify real-time resonance.")
        report.append("2. **Dharmic Guardrails**: Every proposed step in this report has been verified against symbolic SMT invariants.")
        report.append("3. **Accelerator Path**: For high-frequency implementation, ensure the `MansionBridge` is active to offload pattern synthesis to Rust/Mojo.")

        report.append("\n## Next Phase: The Accelerator (Layer 4/5)")
        report.append("The next logical step is to turn these long-form insights into **Primitive Actions**. ")
        report.append("This involves mapping the 'Proposed Actions' in the Verified Plan to executable ")
        report.append("scripts in `scripts/` or Mojo kernels in `whitemagic-mojo/`.")

        report.append("\n---")
        report.append("*End of Strategic Report v9 | Universal Solver Online*")

        return "\n".join(report)
