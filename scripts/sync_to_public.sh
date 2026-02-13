#!/usr/bin/env bash
# sync_to_public.sh — One-way sync from whitemagicdev → whitemagicpublic
#
# Syncs SOURCE CODE, tests, and configs. Does NOT sync docs (they have
# different structures between dev and public) or private dev content.
#
# What gets synced:
#   - whitemagic/         (Python source)
#   - tests/              (test suite)
#   - whitemagic-rust/    (Rust source, not target/)
#   - haskell/            (Haskell source)
#   - elixir/             (Elixir source)
#   - whitemagic-go/      (Go source)
#   - whitemagic-zig/     (Zig source)
#   - whitemagic-mojo/    (Mojo source)
#   - whitemagic-julia/   (Julia source)
#   - mesh/               (Go mesh)
#   - sdk/                (SDK)
#   - nexus/              (Nexus frontend)
#   - examples/           (examples)
#   - eval/               (evaluation)
#   - Root configs        (pyproject.toml, Makefile, Dockerfile, etc.)
#
# What does NOT get synced:
#   - docs/               (public has reorganized subfolder structure)
#   - _aria/, _archives/, _memories/ (private dev content)
#   - SYSTEM_MAP.md       (public has trimmed version)
#   - ARCHIVE_AUDIT_REPORT.md (stripped from public)
#   - .git/, .venv/, build artifacts
#
# Usage:
#   ./scripts/sync_to_public.sh          # Dry run (shows what would change)
#   ./scripts/sync_to_public.sh --apply  # Actually sync

set -euo pipefail

DEV_DIR="$HOME/Desktop/whitemagicdev"
PUBLIC_DIR="$HOME/Desktop/whitemagicpublic"

if [[ ! -d "$DEV_DIR" ]]; then echo "ERROR: Dev directory not found: $DEV_DIR"; exit 1; fi
if [[ ! -d "$PUBLIC_DIR" ]]; then echo "ERROR: Public directory not found: $PUBLIC_DIR"; exit 1; fi

DRY_RUN="--dry-run"
if [[ "${1:-}" == "--apply" ]]; then
    DRY_RUN=""
    echo "=== APPLYING SYNC: $DEV_DIR → $PUBLIC_DIR ==="
else
    echo "=== DRY RUN: $DEV_DIR → $PUBLIC_DIR ==="
    echo "(use --apply to actually sync)"
fi

# Common rsync options
RSYNC_OPTS="-av $DRY_RUN"
COMMON_EXCLUDES=(
    --exclude='__pycache__/'
    --exclude='*.pyc'
    --exclude='.mypy_cache/'
    --exclude='.pytest_cache/'
    --exclude='*.egg-info/'
    --exclude='node_modules/'
    --exclude='.DS_Store'
    --exclude='target/'
    --exclude='_build/'
    --exclude='deps/'
    --exclude='dist-newstyle/'
    --exclude='*.db'
    --exclude='*.db-journal'
    --exclude='*.db-shm'
    --exclude='*.db-wal'
    --exclude='*.so'
    --exclude='*.dylib'
)

echo ""
echo "--- Source code directories ---"

# Sync source code directories (with --delete to mirror)
for dir in whitemagic tests whitemagic-rust haskell elixir whitemagic-go \
           whitemagic-zig whitemagic-mojo whitemagic-julia mesh sdk nexus \
           examples eval scripts .github .well-known .devcontainer; do
    if [[ -d "$DEV_DIR/$dir" ]]; then
        rsync $RSYNC_OPTS --delete "${COMMON_EXCLUDES[@]}" \
            "$DEV_DIR/$dir/" "$PUBLIC_DIR/$dir/"
    fi
done

echo ""
echo "--- Root config files ---"

# Sync specific root files (configs, legal, packaging)
ROOT_FILES=(
    AI_PRIMARY.md
    CHANGELOG.md
    CODE_OF_CONDUCT.md
    CONTRIBUTING.md
    DEPLOY.md
    Dockerfile
    LICENSE
    MANIFEST.in
    Makefile
    PRIVACY_POLICY.md
    README.md
    ABOUT.md
    SECURITY.md
    TERMS_OF_SERVICE.md
    VERSION
    docker-compose.yml
    llms-full.txt
    llms.txt
    mcp-registry.json
    pyproject.toml
    requirements-lock.txt
    server.json
    skill.md
    .dockerignore
    .env.example
    .gitignore
    .mcp.json.example
    .python-version
    wm
)

for f in "${ROOT_FILES[@]}"; do
    if [[ -f "$DEV_DIR/$f" ]]; then
        rsync $RSYNC_OPTS "$DEV_DIR/$f" "$PUBLIC_DIR/$f"
    fi
done

echo ""
if [[ -n "$DRY_RUN" ]]; then
    echo "=== DRY RUN COMPLETE ==="
    echo ""
    echo "NOT synced (managed separately):"
    echo "  - docs/          (public has reorganized subfolder structure)"
    echo "  - SYSTEM_MAP.md  (public has trimmed version)"
    echo ""
    echo "Review the above, then run: $0 --apply"
else
    echo "=== SYNC COMPLETE ==="
    echo ""
    echo "NOT synced (managed separately):"
    echo "  - docs/          (public has reorganized subfolder structure)"
    echo "  - SYSTEM_MAP.md  (public has trimmed version)"
    echo ""
    echo "Changes synced to: $PUBLIC_DIR"
    echo "Remember to commit and push from whitemagicpublic."
fi
