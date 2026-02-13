import pytest
from whitemagic.core.boundaries.boundary_engine import BoundaryEngine, BoundaryType

class TestBoundaryEngine:
    
    @pytest.fixture
    def engine(self):
        engine = BoundaryEngine()
        engine.violations = [] # Reset violations
        return engine

    def test_initialization(self, engine):
        """Test that the engine initializes with default boundaries."""
        assert len(engine.boundaries) > 0
        assert "session_tokens" in engine.boundaries
        assert "cascade_depth" in engine.boundaries

    def test_check_boundary_safe(self, engine):
        """Test checking a boundary that is within limits."""
        boundary_name = "session_tokens"
        engine.boundaries[boundary_name].current = 500
        engine.boundaries[boundary_name].limit = 1000
        
        # Check adding 100 tokens (total 600 < 1000)
        assert engine.check_boundary(boundary_name, increment=100) is True

    def test_check_boundary_violation(self, engine):
        """Test checking a boundary that exceeds limits."""
        boundary_name = "session_tokens"
        engine.boundaries[boundary_name].current = 900
        engine.boundaries[boundary_name].limit = 1000
        
        # Check adding 200 tokens (total 1100 > 1000)
        # Should return False (violation detected)
        assert engine.check_boundary(boundary_name, increment=200) is False

    def test_enforce_limit(self, engine):
        """Test enforcing a limit updates the current value."""
        boundary_name = "session_tokens"
        engine.reset_boundary(boundary_name)
        initial_val = engine.boundaries[boundary_name].current
        
        success = engine.enforce_limit(boundary_name, 100)
        assert success is True
        assert engine.boundaries[boundary_name].current == initial_val + 100

    def test_violation_recording(self, engine):
        """Test that violations are recorded."""
        boundary_name = "cascade_depth"
        engine.reset_boundary(boundary_name)
        limit = engine.boundaries[boundary_name].limit
        
        # Try to add more than the limit at once
        success = engine.enforce_limit(boundary_name, limit + 1)
        
        assert success is False
        assert len(engine.violations) >= 1
        assert engine.violations[-1].boundary.name == boundary_name

    def test_set_custom_boundary(self, engine):
        """Test setting a new custom boundary."""
        engine.set_boundary("test_limit", 50, BoundaryType.COST_LIMIT)
        assert "test_limit" in engine.boundaries
        assert engine.boundaries["test_limit"].limit == 50
