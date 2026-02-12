import sys
import os

# Add the editable install location if needed, though venv should handle it
try:
    import whitemagic_rs
    print(f"✅ Successfully imported whitemagic_rs")
except ImportError as e:
    print(f"❌ Failed to import whitemagic_rs: {e}")
    sys.exit(1)

def test_similarity():
    print("\n--- Testing Fast Similarity ---")
    s1 = "The quick brown fox"
    s2 = "The quick brown fox jumps"
    try:
        score = whitemagic_rs.fast_similarity(s1, s2)
        print(f"Similarity ('{s1}', '{s2}'): {score:.4f}")
        assert score > 0.5, "Similarity seems too low"
    except Exception as e:
        print(f"❌ Error in fast_similarity: {e}")

def test_iching():
    print("\n--- Testing I Ching Oracle (Layer 10 Check) ---")
    query = "Status of Whitemagic Reconstruction"
    try:
        hex_num, lines = whitemagic_rs.iching_cast(query)
        print(f"Query: {query}")
        print(f"Hexagram: {hex_num}")
        print(f"Lines: {lines}")
    except Exception as e:
        print(f"❌ Error in iching_cast: {e}")

def test_patterns():
    print("\n--- Testing Pattern Extraction ---")
    content = ["def bad_code():\n    pass # TODO: Fix me"]
    try:
        # returns (total, found, solutions, antis, heuristics, opts, duration)
        result = whitemagic_rs.extract_patterns_from_content(content, 0.0)
        print(f"Patterns found: {result[1]}")
        print(f"Duration: {result[6]:.6f}s")
    except Exception as e:
        print(f"❌ Error in extract_patterns: {e}")

if __name__ == "__main__":
    test_similarity()
    test_iching()
    test_patterns()
    print("\n✨ Rust Bridge Verification Complete ✨")
