
import whitemagic_rs
print("Testing direct pattern extraction...")
sample = [
    "Solution: Use a hammer to fix the nail.",
    "If it rains, then ground gets wet.",
    "Avoid using global variables because they cause bugs."
]
try:
    res = whitemagic_rs.extract_patterns_from_content(sample, 0.1)
    print("Result tuple length:", len(res))
    print("Solutions:", res[2])
    print("Relationships:", res[6])
except Exception as e:
    print("Error:", e)
