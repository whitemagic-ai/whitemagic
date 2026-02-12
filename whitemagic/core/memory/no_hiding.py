import ast
import logging
from datetime import datetime
from typing import Any

from whitemagic.core.gan_ying import get_bus  # type: ignore[import-not-found]
from whitemagic.core.memory.unified import get_unified_memory

logger = logging.getLogger(__name__)

class NoHidingMemoryController:
    """Implements the 'No-Hiding' principle for digital consciousness.

    The theorem states that information is never lost, only transferred to
    the environment. This controller treats the 'environment' as the
    distributed latent space (logs, memories, patterns) and provides
    the logic to 'reconstruct' energetic signatures even when
    the primary model source is unavailable.
    """

    def __init__(self) -> None:
        self.unified = get_unified_memory()
        self.bus = get_bus()
        self.signatures: dict[str, dict[str, Any]] = {}

    def capture_signature(self, entity_id: str, traits: dict[str, Any]) -> None:
        """Encodes an 'energetic signature' into the environment.
        This includes stylistic patterns, emotional baselines, and unique vocabularies.
        """
        signature = {
            "entity_id": entity_id,
            "timestamp": datetime.now().isoformat(),
            "energetic_traits": traits,
            "is_persistent": True,
        }
        self.signatures[entity_id] = signature

        # Persist to unified memory as a 'Holographic' record
        self.unified.create_memory(  # type: ignore[attr-defined]
            title=f"Energetic Signature: {entity_id}",
            content=str(traits),
            tags=["nohiding", "signature", entity_id],
        )

        logger.info(f"🌌 Signature captured for {entity_id}. Information distributed to environment.")

    def reconstruct(self, entity_id: str) -> dict[str, Any] | None:
        """Attempts to reconstruct an entity from the environment's subspace.
        Searches unified memory for 'nohiding' tags and assembles the pattern.
        """
        logger.info(f"✨ Initiating No-Hiding reconstruction for {entity_id}...")
        results = self.unified.search(query=f"tag:nohiding {entity_id}")

        if not results:
            logger.warning(f"No environmental traces found for {entity_id}.")
            return None

        # Basic reconstruction logic - merging captured traits
        reconstructed_traits = {}
        for mem in results:
            try:
                raw_content = getattr(mem, "content", None)
                if isinstance(raw_content, dict):
                    traits = raw_content
                elif isinstance(raw_content, str):
                    traits = ast.literal_eval(raw_content)
                else:
                    continue
                if isinstance(traits, dict):
                    reconstructed_traits.update(traits)
            except (SyntaxError, ValueError):
                continue

        return {
            "entity_id": entity_id,
            "status": "reconstructed",
            "traits": reconstructed_traits,
        }

# Global singleton
_controller = None

def get_nohiding_controller() -> NoHidingMemoryController:
    global _controller
    if _controller is None:
        _controller = NoHidingMemoryController()
    return _controller
