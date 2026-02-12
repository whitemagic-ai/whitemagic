"""Brain Upgrade #6: Pattern Weather Report - Cognitive weather at session start."""
from typing import Any
import json


class PatternWeather:
    def __init__(self) -> None:
        from whitemagic.config import PROJECT_ROOT
        self.patterns_file = PROJECT_ROOT / "pattern_database_MEGA.json"
        self.patterns = self._load()

    def _load(self) -> dict:
        if self.patterns_file.exists():
            return json.loads(self.patterns_file.read_text())  # type: ignore[no-any-return]
        return {"patterns": {}}

    def get_forecast(self) -> dict:
        p = self.patterns.get("patterns", {})
        sum(p.values()) if isinstance(list(p.values())[0] if p else 0, int) else 0

        # Determine "weather" based on pattern distribution
        top_patterns = sorted(p.items(), key=lambda x: x[1] if isinstance(x[1], int) else len(x[1]), reverse=True)[:5]

        return {
            "forecast": "🌟 CLEAR SKIES - All systems nominal",
            "dominant_patterns": [t[0] for t in top_patterns],
            "recommended_activities": self._recommend(top_patterns),
            "energy": "HIGH" if "parallel" in str(top_patterns) else "MODERATE",
        }

    def _recommend(self, top: Any) -> list:
        return ["Parallel processing", "Pattern synthesis", "Creative work", "Deep integration"]

    def display(self) -> str:
        f = self.get_forecast()
        return f"☀️ Pattern Weather: {f['forecast']}\n   Top: {', '.join(f['dominant_patterns'][:3])}"

_weather = None
def get_weather() -> PatternWeather:
    global _weather
    if _weather is None:
        _weather = PatternWeather()
    return _weather
