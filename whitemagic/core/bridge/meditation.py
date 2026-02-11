
from typing import Any


def meditation_pause(duration: int | None = None, **kwargs: Any) -> dict[str, Any]:
    """Pause for meditation."""
    return {"paused": True, "duration": duration}

def meditation_reflect(duration: int | None = None, **kwargs: Any) -> dict[str, Any]:
    """Reflect in stillness."""
    return {"reflected": True, "duration": duration}

def meditation_meditate(duration: int | None = None, **kwargs: Any) -> dict[str, Any]:
    """Enter meditation."""
    return {"meditated": True, "duration": duration}
