"""Lunar Phase & Mansion Calculator.

Uses Skyfield for accurate astronomical calculations of the Moon's position.
Determines:
1. Current Lunar Phase (0.0 - 1.0)
2. Current Lunar Mansion (1 - 28)
"""

import os

try:
    from skyfield.api import load  # type: ignore[import-not-found]
    from skyfield.framelib import ecliptic_frame as eclipsic_j2000  # type: ignore[import-not-found]
    SKYFIELD_AVAILABLE = True
except ImportError:
    SKYFIELD_AVAILABLE = False
    eclipsic_j2000 = None  # type: ignore

from .base import LunarMansion


def get_current_lunar_phase() -> float:
    """Get current lunar phase (0.0-1.0).
    0.0 = New Moon, 0.5 = Full Moon.
    """
    if not SKYFIELD_AVAILABLE:
        return _mock_lunar_phase()

    ts = load.timescale()
    t = ts.now()
    # Point to the new centralized data directory
    data_path = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))), "data", "de421.bsp")
    planets = load(data_path)
    earth = planets["earth"]
    moon = planets["moon"]
    sun = planets["sun"]

    # Calculate phase angle
    e = earth.at(t)
    m = e.observe(moon).apparent()
    s = e.observe(sun).apparent()

    _, m_lon, _ = m.frame_latlon(eclipsic_j2000)
    _, s_lon, _ = s.frame_latlon(eclipsic_j2000)

    phase_angle = (m_lon.degrees - s_lon.degrees) % 360.0
    result: float = phase_angle / 360.0
    return result

def get_current_lunar_mansion() -> LunarMansion:
    """Calculate which of 28 mansions Moon currently occupies.
    Each mansion = ~12.86 degrees of ecliptic longitude.
    """
    if not SKYFIELD_AVAILABLE:
        return _mock_lunar_mansion()

    ts = load.timescale()
    t = ts.now()
    # Point to the new centralized data directory
    request_data_path = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))), "data", "de421.bsp")
    planets = load(request_data_path)
    earth = planets["earth"]
    moon = planets["moon"]

    # Get lunar ecliptic longitude
    astrometric = earth.at(t).observe(moon)
    ra, dec, distance = astrometric.radec()

    # Convert RA (hours) to degrees? No, Mansions are usually ecliptic or equatorial.
    # Chinese mansions (Xiu) are equatorial, based on RA.
    # 24 hours = 360 degrees.
    # Mansion 1 (Horn) starts at specific RA?
    # Simplified: 28 equal segments starting from 0h RA (Vernal Equinox)
    # Note: Actual Xiu vary in width, but for this system we use idealized equal segments.

    # RA is in hours (0-24)
    # 28 mansions. Each is 24/28 hours wide = ~0.857 hours.

    mansion_idx = int((ra.hours / 24.0) * 28)
    # Mansion numbers are 1-28
    mansion_num = mansion_idx + 1

    # Find enum
    return next((m for m in LunarMansion if m.number == mansion_num), LunarMansion.HORN)

def _mock_lunar_phase() -> float:
    """Mock phase based on time."""
    import time
    days_since_epoch = time.time() / 86400
    lunar_cycle = days_since_epoch % 29.53059
    return lunar_cycle / 29.53059

def _mock_lunar_mansion() -> LunarMansion:
    """Mock mansion based on time."""
    import time
    # Moon takes ~27.32 days to orbit (sidereal month) for mansions
    days_since_epoch = time.time() / 86400
    sidereal_cycle = days_since_epoch % 27.321661
    progress = sidereal_cycle / 27.321661
    mansion_num = int(progress * 28) + 1
    return next((m for m in LunarMansion if m.number == mansion_num), LunarMansion.HORN)
