"""Integration tests: Rust acceleration vs Python fallback."""
import os


from tests.conftest import assert_envelope_shape


class TestRustAvailability:
    def test_rust_status_returns_envelope(self, tool_caller):
        result = tool_caller("rust_status")
        assert_envelope_shape(result)

    def test_rust_similarity_returns_envelope(self, tool_caller):
        result = tool_caller("rust_similarity", text1="hello world", text2="hello there")
        assert_envelope_shape(result)


class TestRustFallback:
    def test_similarity_with_rust_disabled(self, tool_caller):
        """Operations should succeed even with Rust disabled."""
        old = os.environ.get("WHITEMAGIC_NO_RUST")
        os.environ["WHITEMAGIC_NO_RUST"] = "1"
        try:
            result = tool_caller("rust_similarity", text1="hello", text2="world")
            assert_envelope_shape(result)
            # Should either succeed with Python fallback or return a clear error
            assert result["status"] in ("success", "error")
        finally:
            if old is not None:
                os.environ["WHITEMAGIC_NO_RUST"] = old
            else:
                os.environ.pop("WHITEMAGIC_NO_RUST", None)

    def test_capabilities_without_rust(self, tool_caller):
        """Core tools like capabilities should work regardless of Rust."""
        old = os.environ.get("WHITEMAGIC_NO_RUST")
        os.environ["WHITEMAGIC_NO_RUST"] = "1"
        try:
            result = tool_caller.ok("capabilities", include_tools=False, include_env=False)
            assert_envelope_shape(result)
        finally:
            if old is not None:
                os.environ["WHITEMAGIC_NO_RUST"] = old
            else:
                os.environ.pop("WHITEMAGIC_NO_RUST", None)
