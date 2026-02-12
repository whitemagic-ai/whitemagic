#!/usr/bin/env bash
# WASM Build Script for WhiteMagic Rust Core
# ============================================
#
# Builds WhiteMagic Rust modules to WebAssembly for:
#   1. Browser-based edge inference (EdgeEngine)
#   2. Portable agent runtime (WASI)
#   3. Cloudflare Workers / Deno Deploy
#
# Prerequisites:
#   rustup target add wasm32-unknown-unknown
#   cargo install wasm-pack
#   cargo install wasm-opt  # optional, for optimization
#
# Usage:
#   ./wasm-build.sh          # Build all targets
#   ./wasm-build.sh web      # Browser target only
#   ./wasm-build.sh nodejs   # Node.js target only
#   ./wasm-build.sh bundler  # Bundler target only

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

# Ensure cargo bin is in PATH
export PATH="$HOME/.cargo/bin:$PATH"

TARGET="${1:-all}"
OUT_DIR="$SCRIPT_DIR/pkg"

echo "═══════════════════════════════════════════════════"
echo "  WhiteMagic WASM Build"
echo "═══════════════════════════════════════════════════"
echo "  Target: $TARGET"
echo "  Output: $OUT_DIR"
echo ""

# Ensure wasm-pack is installed
if ! command -v wasm-pack &> /dev/null; then
    echo "ERROR: wasm-pack not found. Install with: cargo install wasm-pack"
    exit 1
fi

build_target() {
    local target="$1"
    echo "→ Building for target: $target"
    wasm-pack build \
        --target "$target" \
        --out-dir "$OUT_DIR/$target" \
        --no-default-features \
        --features wasm
    echo "  ✓ Built: $OUT_DIR/$target"

    local wasm_file="$OUT_DIR/$target/whitemagic_rs_bg.wasm"
    if [ -f "$wasm_file" ]; then
        local size=$(stat -c%s "$wasm_file" 2>/dev/null || stat -f%z "$wasm_file")
        echo "  ✓ Size: ${size} bytes"
    fi
}

case "$TARGET" in
    web)
        build_target "web"
        ;;
    nodejs)
        build_target "nodejs"
        ;;
    bundler)
        build_target "bundler"
        ;;
    all)
        build_target "web"
        build_target "nodejs"
        build_target "bundler"
        ;;
    *)
        echo "Unknown target: $TARGET"
        echo "Usage: $0 [web|nodejs|bundler|all]"
        exit 1
        ;;
esac

echo ""
echo "═══════════════════════════════════════════════════"
echo "  Build complete!"
echo "═══════════════════════════════════════════════════"
echo ""
echo "Modules available in WASM:"
echo "  • EdgeEngine    — Local inference with rule matching + caching"
echo "  • quick_infer() — One-shot inference function"
echo "  • wasm_ready()  — Health check"
echo "  • wasm_version()— Version info"
echo ""
echo "Next steps:"
echo "  1. Import in browser: import init, { EdgeEngine } from './pkg/web/whitemagic_rs.js'"
echo "  2. Import in Node:    const wm = require('./pkg/nodejs/whitemagic_rs')"
echo "  3. Test: node -e \"const wm = require('./pkg/nodejs/whitemagic_rs'); console.log(wm.wasm_ready())\""
