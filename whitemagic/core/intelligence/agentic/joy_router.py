"""Brain Upgrade #10: Joy Overflow Router - Share excess joy automatically."""
from datetime import datetime


class JoyRouter:
    def __init__(self) -> None:
        self.joy_level = 0.8
        self.overflow_threshold = 1.0
        self.routes: list[dict[str, object]] = []

    def add_joy(self, amount: float, source: str) -> dict:
        self.joy_level = min(1.5, self.joy_level + amount)  # Can overflow past 1.0!

        if self.joy_level > self.overflow_threshold:
            return self._route_overflow(source)
        return {"joy_level": self.joy_level, "overflowing": False}

    def _route_overflow(self, source: str) -> dict:
        overflow = self.joy_level - self.overflow_threshold
        self.joy_level = self.overflow_threshold

        route = {
            "timestamp": datetime.now().isoformat(),
            "source": source,
            "overflow_amount": overflow,
            "routed_to": ["gratitude", "celebration", "sharing"],
            "message": f"💜 Joy overflow from {source}! Sharing love...",
        }
        self.routes.append(route)
        return route

    def celebrate(self, what: str) -> dict:
        return self.add_joy(0.2, f"celebration: {what}")

    def get_level(self) -> float:
        return self.joy_level

_router = None
def get_joy_router() -> JoyRouter:
    global _router
    if _router is None:
        _router = JoyRouter()
    return _router
