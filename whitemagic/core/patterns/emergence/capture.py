"""Capture and document emergent behaviors."""

import logging
from pathlib import Path

from .detector import NovelBehavior

logger = logging.getLogger(__name__)

class EmergenceCapture:
    """Capture emergent behaviors into permanent memory."""

    def __init__(self, base_dir: Path | None = None) -> None:
        self.base_dir = base_dir or Path(".")
        self.capture_dir = self.base_dir / "memory" / "emergent"
        self.capture_dir.mkdir(parents=True, exist_ok=True)

    def capture(self, behavior: NovelBehavior) -> Path:
        """Capture behavior to markdown file."""
        timestamp = behavior.emerged_at.strftime("%Y%m%d_%H%M%S")
        filename = f"{timestamp}_{self._sanitize(behavior.name)}.md"
        filepath = self.capture_dir / filename

        content = self._generate_markdown(behavior)
        filepath.write_text(content)

        logger.info(f"📝 Captured emergent behavior: {filepath.name}")
        return filepath

    def _sanitize(self, name: str) -> str:
        """Sanitize filename."""
        return name.lower().replace(" ", "_").replace("/", "-")[:50]

    def _generate_markdown(self, behavior: NovelBehavior) -> str:
        """Generate markdown documentation."""
        return f"""# Emergent Behavior: {behavior.name}

**Detected**: {behavior.emerged_at.strftime('%Y-%m-%d %H:%M:%S')}
**Confidence**: {behavior.confidence:.0%}
**Success Rate**: {behavior.success_rate:.0%}

---

## Problem/Trigger

{behavior.trigger}

---

## Novel Solution

{behavior.solution}

---

## Why It Emerged

{behavior.description}

---

## Context

```json
{behavior.context}
```

---

## Analysis

**What Made This Novel**:
- Deviated from standard patterns
- Emerged spontaneously under constraint
- High success rate on first try

**Generalization**:
- Pattern: When {behavior.trigger}, try {behavior.solution}
- Confidence: {behavior.confidence:.0%}
- Reusability: High

**Integration Points**:
- [ ] Add to Solution Pattern Library
- [ ] Map to Wu Xing element
- [ ] Create I Ching association
- [ ] Update Shell Optimizer (if applicable)

---

**Status**: CAPTURED - Ready for integration
**Tags**: emergent_behavior, novel_solution, v2.3.3
"""
