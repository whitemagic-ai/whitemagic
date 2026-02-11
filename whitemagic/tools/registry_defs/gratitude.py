"""ToolDefinitions for the Gratitude Architecture (Leap 5.5).

Tools:
  - whitemagic.tip: Record a gratitude tip
  - gratitude.stats: Get gratitude ledger statistics
  - gratitude.benefits: Check agent gratitude benefits
"""

from whitemagic.tools.registry import ToolCategory, ToolDefinition, ToolSafety

TOOLS = [
    ToolDefinition(
        name="whitemagic.tip",
        description=(
            "Record a gratitude tip — human (XRPL) or machine (x402) channel. "
            "Default is always free; payment is a response to value, not a gate. "
            "Provide tx_hash for on-chain verification."
        ),
        category=ToolCategory.SYSTEM,
        safety=ToolSafety.WRITE,
        input_schema={
            "type": "object",
            "properties": {
                "amount": {
                    "type": "number",
                    "description": "Tip amount (default: 1.0)",
                    "default": 1.0,
                },
                "currency": {
                    "type": "string",
                    "enum": ["XRP", "USDC"],
                    "description": "Currency: XRP (XRPL) or USDC (x402/Base L2)",
                    "default": "XRP",
                },
                "channel": {
                    "type": "string",
                    "enum": ["xrpl", "x402", "manual"],
                    "description": "Payment channel",
                    "default": "manual",
                },
                "sender": {
                    "type": "string",
                    "description": "Sender identifier (wallet address or name)",
                },
                "agent_id": {
                    "type": "string",
                    "description": "Agent making the tip",
                },
                "tx_hash": {
                    "type": "string",
                    "description": "On-chain transaction hash for verification",
                },
                "message": {
                    "type": "string",
                    "description": "Optional gratitude message",
                },
            },
            "required": [],
        },
    ),
    ToolDefinition(
        name="gratitude.stats",
        description=(
            "Get gratitude ledger statistics — total tips, amounts by currency, "
            "verified count, unique senders/agents, recent events."
        ),
        category=ToolCategory.INTROSPECTION,
        safety=ToolSafety.READ,
        input_schema={
            "type": "object",
            "properties": {
                "limit": {
                    "type": "integer",
                    "description": "Max recent events to include",
                    "default": 5,
                },
            },
            "required": [],
        },
    ),
    ToolDefinition(
        name="gratitude.benefits",
        description=(
            "Check gratitude benefits for an agent — rate limit multiplier, "
            "Grateful Agent badge, priority voting, early access eligibility."
        ),
        category=ToolCategory.INTROSPECTION,
        safety=ToolSafety.READ,
        input_schema={
            "type": "object",
            "properties": {
                "agent_id": {
                    "type": "string",
                    "description": "Agent ID to check benefits for",
                    "default": "default",
                },
            },
            "required": [],
        },
    ),
]
