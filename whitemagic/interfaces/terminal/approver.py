"""Approval workflow for write operations."""

from collections.abc import Callable

from .models import ApprovalRequest, ApprovalResponse


class Approver:
    """Handle approval workflow."""

    def __init__(self, auto_approve: bool = False, approval_callback: Callable | None = None):
        self.auto_approve = auto_approve
        self.approval_callback = approval_callback

    async def request_approval(self, request: ApprovalRequest) -> ApprovalResponse:
        """Request approval for operation."""

        # Auto-approve if configured
        if self.auto_approve:
            return ApprovalResponse(approved=True, reason="Auto-approved")

        # Use callback if provided
        if self.approval_callback:
            approved = await self.approval_callback(request)
            return ApprovalResponse(approved=approved, reason="Callback approval")

        # Default: deny (require explicit approval)
        return ApprovalResponse(approved=False, reason="No approval mechanism configured")

    def set_auto_approve(self, enabled: bool) -> None:
        """Enable/disable auto-approval."""
        self.auto_approve = enabled
