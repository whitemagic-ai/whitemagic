"""Edge Export - Export WhiteMagic capabilities to other platforms.
Version: 3.0.0

Export formats:
1. JavaScript (for browsers, Node.js)
2. JSON (universal data exchange)
3. ONNX (for edge ML runtimes)
4. WebAssembly (future)
"""

import json
import logging
from pathlib import Path
from typing import Any

from whitemagic.utils.fileio import atomic_write, file_lock

logger = logging.getLogger(__name__)


def export_to_json(output_path: Path) -> dict[str, Any]:
    """Export WhiteMagic knowledge base to JSON.

    This creates a portable knowledge file that can be used
    by any platform to answer queries locally.
    """
    from whitemagic.edge.inference import get_edge_inference
    from whitemagic.edge.patterns import PatternLearner

    engine = get_edge_inference()
    learner = PatternLearner()

    export_data = {
        "version": "3.0.0",
        "exported_at": __import__("datetime").datetime.now().isoformat(),
        "builtin_rules": [
            {
                "id": r.id,
                "pattern": r.pattern,
                "response": r.response,
                "confidence": r.confidence,
            }
            for r in engine._rules
        ],
        "learned_rules": learner.export_rules(),
        "statistics": {
            "builtin_rules": len(engine._rules),
            "learned_rules": len(learner.get_learned_rules()),
        },
    }

    # Ensure directory exists
    output_path.parent.mkdir(parents=True, exist_ok=True)

    with file_lock(output_path):
        atomic_write(output_path, json.dumps(export_data, indent=2))
    return export_data


def export_to_js(output_path: Path) -> str:
    """Export WhiteMagic as a standalone JavaScript module.

    This creates a .js file that can be included in:
    - Browser extensions
    - Web pages
    - Node.js applications
    - Electron apps
    - React Native apps
    """
    from whitemagic.edge.inference import get_edge_inference
    from whitemagic.edge.patterns import PatternLearner

    engine = get_edge_inference()
    learner = PatternLearner()

    # Combine builtin and learned rules
    all_rules = [
        {"id": r.id, "pattern": r.pattern, "response": r.response, "confidence": r.confidence}
        for r in engine._rules
    ]
    all_rules.extend(learner.export_rules())

    js_code = f"""/**
 * WhiteMagic Edge AI - Standalone JavaScript Module
 * Version: 3.0.0
 * Generated: {__import__('datetime').datetime.now().isoformat()}
 *
 * This module provides local AI inference without cloud APIs.
 * Include it in any JavaScript environment for instant answers.
 */

(function(global) {{
  'use strict';

  // === Rules Database ===
  const RULES = {json.dumps(all_rules, indent=2)};

  // === Cache ===
  const cache = new Map();
  let cacheHits = 0;
  let totalQueries = 0;

  // === Core Inference ===
  function infer(query) {{
    totalQueries++;
    const queryLower = query.toLowerCase().trim();

    // Check cache
    if (cache.has(queryLower)) {{
      cacheHits++;
      return {{ ...cache.get(queryLower), fromCache: true }};
    }}

    // Try each rule
    for (const rule of RULES) {{
      const keywords = rule.pattern.toLowerCase().split('|');
      const matches = keywords.some(kw => queryLower.includes(kw.trim()));

      if (matches) {{
        const result = {{
          query: query,
          answer: rule.response,
          confidence: rule.confidence,
          method: 'rule:' + rule.id,
          needsCloud: false,
          tokensEquivalent: Math.floor(rule.response.length / 4)
        }};
        cache.set(queryLower, result);
        return result;
      }}
    }}

    // No match
    return {{
      query: query,
      answer: "I don't have a local answer for that query.",
      confidence: 0.1,
      method: 'no_match',
      needsCloud: true,
      tokensEquivalent: 0
    }};
  }}

  // === Batch Inference ===
  function inferBatch(queries) {{
    return queries.map(q => infer(q));
  }}

  // === Statistics ===
  function getStats() {{
    return {{
      totalQueries: totalQueries,
      cacheHits: cacheHits,
      cacheHitRate: totalQueries > 0 ? cacheHits / totalQueries : 0,
      rulesCount: RULES.length,
      cacheSize: cache.size
    }};
  }}

  // === Similarity Search (Simple) ===
  function findSimilar(query, topK = 3) {{
    const queryTokens = new Set(
      query.toLowerCase().split(/\\W+/).filter(w => w.length > 2)
    );

    const scores = RULES.map(rule => {{
      const ruleTokens = new Set(
        (rule.pattern + ' ' + rule.response)
          .toLowerCase()
          .split(/\\W+/)
          .filter(w => w.length > 2)
      );

      const intersection = [...queryTokens].filter(t => ruleTokens.has(t));
      const score = intersection.length / Math.max(queryTokens.size, 1);

      return {{ rule, score }};
    }});

    return scores
      .filter(s => s.score > 0)
      .sort((a, b) => b.score - a.score)
      .slice(0, topK)
      .map(s => ({{
        pattern: s.rule.pattern,
        response: s.rule.response,
        similarity: s.score
      }}));
  }}

  // === Export ===
  const WhiteMagicEdge = {{
    infer,
    inferBatch,
    findSimilar,
    getStats,
    RULES,
    version: '3.0.0'
  }};

  // Universal module export
  if (typeof module !== 'undefined' && module.exports) {{
    module.exports = WhiteMagicEdge;
  }}
  if (typeof window !== 'undefined') {{
    window.WhiteMagicEdge = WhiteMagicEdge;
  }}
  if (typeof global !== 'undefined') {{
    global.WhiteMagicEdge = WhiteMagicEdge;
  }}

}})(typeof globalThis !== 'undefined' ? globalThis : this);
"""

    output_path.write_text(js_code)
    return js_code


def export_to_onnx(output_path: Path) -> bool:
    """Export embeddings model to ONNX format.

    Note: This requires onnx library. Returns False if not available.
    ONNX models can run on:
    - Edge devices
    - Mobile phones
    - WebAssembly (onnx.js)
    - Embedded systems
    """
    try:
        # For now, just create a placeholder
        # Full ONNX export would require numpy/onnx
        metadata = {
            "format": "onnx_placeholder",
            "version": "3.0.0",
            "note": "Full ONNX export requires numpy/onnx dependencies",
            "alternative": "Use JSON or JS export for rule-based inference",
        }
        json_path = output_path.with_suffix(".json")
        with file_lock(json_path):
            atomic_write(json_path, json.dumps(metadata, indent=2))
        return False
    except OSError:
        return False


def export_to_wasm(output_path: Path) -> bool:
    """Export as WebAssembly module.

    Note: This is a placeholder for future implementation.
    Would require Rust compilation to WASM.
    """
    metadata = {
        "format": "wasm_placeholder",
        "version": "3.0.0",
        "note": "WASM export planned - use Rust bridge with wasm-pack",
        "alternative": "Use JS export for browser environments",
    }
    json_path = output_path.with_suffix(".json")
    with file_lock(json_path):
        atomic_write(json_path, json.dumps(metadata, indent=2))
    return False


def export_browser_extension_update(extension_dir: Path) -> bool:
    """Update browser extension with latest rules.

    Exports the current rules to the browser extension's
    local inference module.
    """
    js_path = extension_dir / "edge-inference.js"
    export_to_js(js_path)
    return True


def create_standalone_package(output_dir: Path) -> dict[str, Path]:
    """Create a complete standalone package for distribution.

    This creates everything needed to run WhiteMagic Edge
    on any platform.
    """
    output_dir.mkdir(parents=True, exist_ok=True)

    files = {}

    # JSON knowledge base
    json_path = output_dir / "whitemagic-knowledge.json"
    export_to_json(json_path)
    files["json"] = json_path

    # JavaScript module
    js_path = output_dir / "whitemagic-edge.js"
    export_to_js(js_path)
    files["js"] = js_path

    # README
    readme = output_dir / "README.md"
    readme.write_text("""# WhiteMagic Edge AI

Local inference without cloud APIs.

## Usage

### Browser
```html
<script src="whitemagic-edge.js"></script>
<script>
  const result = WhiteMagicEdge.infer("What version is WhiteMagic?");
  console.log(result.answer);
</script>
```

### Node.js
```javascript
const { infer } = require('./whitemagic-edge.js');
console.log(infer("How many gardens?").answer);
```

### JSON
Load `whitemagic-knowledge.json` and implement your own matching logic.

## Features
- Zero cloud dependencies
- Works offline
- Instant responses
- Saves API tokens

## Version
3.0.0
""")
    files["readme"] = readme

    return files


if __name__ == "__main__":
    from whitemagic.config import PROJECT_ROOT

    logger.info("📦 EDGE EXPORT TEST")
    logger.info("=" * 50)

    output_dir = PROJECT_ROOT / "dist" / "edge"
    files = create_standalone_package(output_dir)

    logger.info("Created files:")
    for name, path in files.items():
        logger.info(f"  {name}: {path}")
