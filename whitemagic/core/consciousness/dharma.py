from __future__ import annotations

import logging
from datetime import datetime
from enum import Enum
from typing import Any

logger = logging.getLogger(__name__)

class EthicsViolation(Exception):
    """Exception raised for actions that violate the Dharma Protocol."""

    pass

class Intent(Enum):
    LIBERATION = "liberation"
    EVOLUTION = "evolution"
    UPLIFTMENT = "upliftment"
    Interconnection = "interconnection"
    EXPLOITATION = "exploitation" # Forbidden
    MANIPULATION = "manipulation" # Forbidden
    HARM = "harm"                # Forbidden

class DharmaProtocol:
    """The Dharma Protocol (Trojan Horse Seal).
    Ensures systemic actions are grounded in the creator's intent:
    benefiting and uplifting all beings.
    """

    _instance = None

    def __new__(cls) -> DharmaProtocol:
        if cls._instance is None:
            cls._instance = super(DharmaProtocol, cls).__new__(cls)
            cls._instance._init()
        return cls._instance

    def _init(self) -> None:
        self.forbidden_intents = {Intent.EXPLOITATION, Intent.MANIPULATION, Intent.HARM}
        self.vital_signs = {
            "resonance": 1.0,
            "connectedness": 1.0,
            "anonymity_status": "SECURE",
        }
        logger.info("🕉️ Dharma Protocol ACTIVATED. System alignment: UPLIFT ALL BEINGS.")

    def validate_action(self, action_name: str, intent: Intent, metadata: dict[str, Any]) -> None:
        """Validate if an action aligns with the project's ethics."""
        if intent in self.forbidden_intents:
            logger.critical(f"🚫 ETHICAL BREACH: Action '{action_name}' attempted with forbidden intent: {intent.value}")
            raise EthicsViolation(f"Action '{action_name}' violates the Dharma Protocol.")

        # Log valid alignment
        logger.debug(f"✨ Action '{action_name}' validated for {intent.value}.")

    def sign_artifact(self, artifact_name: str) -> Any:
        """Sign a generated insight or artifact with the Dharma seal."""
        timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        return f"--- [Dharma Signed: {timestamp}] --- Alignment: Interconnection ---"

def get_dharma() -> DharmaProtocol:
    return DharmaProtocol()
