"""Tests for holographic coordinate encoding — 5D coordinate round-trips."""
import pytest

from whitemagic.core.intelligence.hologram.encoder import CoordinateEncoder, HolographicCoordinate


@pytest.fixture
def encoder():
    return CoordinateEncoder()


def test_encode_returns_coordinate(encoder):
    """Encoding any memory dict returns a HolographicCoordinate with 5 floats."""
    memory = {"title": "test", "content": "hello world", "tags": []}
    coord = encoder.encode(memory)
    assert isinstance(coord, HolographicCoordinate)
    vec = coord.to_vector()
    assert len(vec) == 5
    for v in vec:
        assert isinstance(v, float)


def test_x_axis_logic_keywords(encoder):
    """Logic keywords should push X toward positive (logic side) in raw calculation."""
    logic_mem = {"title": "code review", "content": "function algorithm debug", "tags": ["code", "architecture"]}
    emotion_mem = {"title": "gratitude", "content": "feeling joy and wonder", "tags": ["joy", "love"]}

    # Test the raw axis calculation directly (bypasses polyglot router)
    logic_x = encoder._calculate_x(logic_mem)
    emotion_x = encoder._calculate_x(emotion_mem)

    assert logic_x > emotion_x


def test_y_axis_macro_keywords(encoder):
    """Macro/architecture tags should push Y toward positive."""
    macro_mem = {"title": "system overview", "content": "universal principle architecture", "tags": ["architecture", "wisdom"], "type": "long_term"}
    micro_mem = {"title": "error log", "content": "traceback at line 42", "tags": ["debug", "error"], "type": "short_term"}

    macro_coord = encoder.encode(macro_mem)
    micro_coord = encoder.encode(micro_mem)

    assert macro_coord.y > micro_coord.y


def test_z_axis_range(encoder):
    """Z axis should be in [-1.0, 1.0] range."""
    memory = {"title": "test", "content": "any", "tags": []}
    coord = encoder.encode(memory)
    assert -1.0 <= coord.z <= 1.0


def test_w_axis_minimum(encoder):
    """W axis should have a minimum of 0.1 (visibility floor)."""
    memory = {"title": "trivial", "content": "x", "tags": [], "importance": 0.0, "neuro_score": 0.0}
    coord = encoder.encode(memory)
    assert coord.w >= 0.1


def test_to_dict_round_trip(encoder):
    """to_dict() should return a dict with x, y, z, w, v keys."""
    memory = {"title": "test", "content": "hello", "tags": ["code"]}
    coord = encoder.encode(memory)
    d = coord.to_dict()
    assert set(d.keys()) == {"x", "y", "z", "w", "v"}
    # Round-trip: reconstruct from dict
    reconstructed = HolographicCoordinate(**d)
    assert reconstructed.x == coord.x
    assert reconstructed.y == coord.y
    assert reconstructed.z == coord.z
    assert reconstructed.w == coord.w
    assert reconstructed.v == coord.v


def test_encode_deterministic_for_same_input(encoder):
    """Same memory input should produce same X and Y (Z may differ due to time, W has jitter)."""
    memory = {"title": "stable", "content": "deterministic test", "tags": ["code"]}
    coord1 = encoder.encode(memory)
    coord2 = encoder.encode(memory)
    assert coord1.x == coord2.x
    assert coord1.y == coord2.y
