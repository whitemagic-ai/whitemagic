import pytest
from whitemagic.core.nurturing.nurturing_engine import NurturingEngine

class TestNurturingEngine:
    
    @pytest.fixture
    def engine(self):
        engine = NurturingEngine(storage_path=None) # In-memory only
        return engine

    def test_profile_creation(self, engine):
        """Test that user profiles are created correctly."""
        user_id = "test_user_123"
        profile = engine.get_or_create_profile(user_id)
        
        assert profile.user_id == user_id
        assert user_id in engine.profiles

    def test_style_detection(self, engine):
        """Test communication style detection."""
        user_id = "tech_user"
        
        # Technical messages
        messages = [
            "We need to debug the API function",
            "The algorithm complexity is O(n)",
            "Push the code to production"
        ]
        
        style = engine.detect_communication_style(user_id, messages)
        assert style == "technical"
        
        # Casual messages
        casual_user = "casual_user"
        casual_msgs = ["Hey that's cool", "lol thanks", "btw checking in"]
        style_casual = engine.detect_communication_style(casual_user, casual_msgs)
        assert style_casual == "casual"

    def test_preference_learning(self, engine):
        """Test that preferences are learned and updated."""
        user_id = "pref_user"
        
        # Learn a preference
        pref = engine.learn_preference(user_id, "response_length", "short", confidence=0.6)
        
        assert pref.value == "short"
        assert pref.observations == 1
        
        # Retrieve it
        profile = engine.get_or_create_profile(user_id)
        assert profile.get_preference("response_length") == "short"
        
        # Reinforce it
        engine.learn_preference(user_id, "response_length", "short")
        assert profile.preferences["response_length"].observations == 2
        assert profile.preferences["response_length"].confidence > 0.6

    def test_personalization(self, engine):
        """Test response personalization."""
        user_id = "short_user"
        
        # Set preference for short responses
        engine.learn_preference(user_id, "response_length", "short")
        
        long_response = "A" * 600
        personalized = engine.personalize_response(user_id, long_response)
        
        # Should be truncated
        assert len(personalized) < 500
        assert personalized.endswith("...")

    def test_warm_greeting(self, engine):
        """Test greeting generation."""
        user_id = "greet_user"
        engine.get_or_create_profile(user_id)
        
        greeting = engine.get_warmth_greeting(user_id)
        assert greeting is not None
        assert len(greeting) > 0
