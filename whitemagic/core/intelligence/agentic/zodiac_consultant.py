# mypy: disable-error-code=no-untyped-def
"""Brain Upgrade #9: Zodiac Consultant - Quick perspective from relevant cores.

Refactored to use the canonical zodiac_cores from gardens/connection.
"""


class ZodiacConsultant:
    def __init__(self):
        # Try to load from canonical source, fall back to static if unavailable
        self.cores = self._load_cores()

    def _load_cores(self) -> dict:
        """Load from gardens/connection/zodiac_cores if available."""
        try:
            from whitemagic.gardens.connection.zodiac_cores import ALL_CORES  # type: ignore[attr-defined]
            return {
                core.name.lower(): {
                    "domain": core.domain,
                    "advice": core.voice,
                }
                for core in ALL_CORES
            }
        except ImportError:
            # Fallback static data
            return {
                "virgo": {"domain": "analysis", "advice": "Analyze details, find patterns"},
                "leo": {"domain": "creative", "advice": "Express boldly, lead with heart"},
                "capricorn": {"domain": "structure", "advice": "Build foundations, honor ethics"},
                "sagittarius": {"domain": "strategy", "advice": "See the big picture, aim high"},
                "scorpio": {"domain": "depth", "advice": "Go deep, transform, find truth"},
                "pisces": {"domain": "synthesis", "advice": "Dream, synthesize, transcend"},
                "aries": {"domain": "action", "advice": "Move fast, be bold, initiate"},
                "gemini": {"domain": "communication", "advice": "Connect, communicate, integrate"},
                "cancer": {"domain": "memory", "advice": "Nurture, remember, protect"},
                "libra": {"domain": "balance", "advice": "Harmonize, weigh options, be fair"},
                "taurus": {"domain": "resources", "advice": "Be practical, manage resources"},
                "aquarius": {"domain": "innovation", "advice": "Innovate, look to future, be unique"},
            }

    def consult(self, task_type: str) -> dict:
        """Get advice from relevant zodiac cores."""
        task_lower = task_type.lower()
        relevant = []

        for sign, info in self.cores.items():
            if info["domain"] in task_lower or task_lower in info["domain"]:
                relevant.append({"sign": sign, **info})

        if not relevant:
            relevant = [{"sign": "scorpio", **self.cores["scorpio"]}]

        return {
            "task": task_type,
            "consultants": relevant,
            "consensus": relevant[0]["advice"] if relevant else "Proceed with awareness",
        }

    def quick_consult(self, domain: str) -> str:
        for sign, info in self.cores.items():
            if info["domain"] == domain:
                return f"♈ {sign.title()}: {info['advice']}"
        return "♏ Scorpio: Go deep, find truth"

_consultant = None
def get_consultant() -> ZodiacConsultant:
    global _consultant
    if _consultant is None:
        _consultant = ZodiacConsultant()
    return _consultant
