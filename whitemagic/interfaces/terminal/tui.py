"""TUI (Text User Interface) for approval workflow."""

import logging

from .models import ApprovalRequest, ApprovalResponse

logger = logging.getLogger(__name__)


class SimpleTUI:
    """Simple TUI approver (Rich-based TUI for future)."""

    def __init__(self) -> None:
        self.enabled = True

    def request_approval(self, request: ApprovalRequest) -> ApprovalResponse:
        """Request approval via terminal prompt."""
        logger.info(f"\n{'='*60}")
        logger.info("🔐 APPROVAL REQUIRED")
        logger.info(f"{'='*60}")
        logger.info(f"Command: {request.command}")
        logger.info(f"Mode: {request.mode.value}")
        if request.cwd:
            logger.info(f"CWD: {request.cwd}")
        if request.preview:
            logger.info("\nPreview:")
            logger.info(request.preview)
        logger.info(f"{'='*60}")

        while True:
            response = input("Approve? [y/N]: ").strip().lower()
            if response in ["y", "yes"]:
                return ApprovalResponse(approved=True, reason="User approved")
            elif response in ["n", "no", ""]:
                return ApprovalResponse(approved=False, reason="User denied")
            logger.info("Please enter 'y' or 'n'")
