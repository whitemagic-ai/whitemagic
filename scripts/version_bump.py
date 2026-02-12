#!/usr/bin/env python3
"""Version bump script for WhiteMagic.

Updates all version references across the project in one shot.

Usage:
    python scripts/version_bump.py 14.0.0
    python scripts/version_bump.py 14.0.0 --dry-run
"""

import argparse
import re
import sys
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent.parent

# Files that contain version strings to update
VERSION_FILES = [
    # Core version files
    ("VERSION", "plain"),
    ("pyproject.toml", "toml_version"),
    (".well-known/agent.json", "json_version"),
    ("whitemagic-rust/Cargo.toml", "toml_version"),
    ("whitemagic-rust/src/lib.rs", "rust_version"),
]

# Documentation files that reference version in headers/text
DOC_FILES = [
    "README.md",
    "AI_PRIMARY.md",
    "SYSTEM_MAP.md",
    "docs/POLYGLOT_STATUS.md",
    "docs/POLYGLOT_EXPANSION_STRATEGY.md",
    "llms.txt",
    "llms-full.txt",
    "skill.md",
]


def read_current_version() -> str:
    """Read current version from VERSION file."""
    version_file = PROJECT_ROOT / "VERSION"
    return version_file.read_text().strip()


def update_plain(filepath: Path, old: str, new: str, dry_run: bool) -> bool:
    """Update plain text VERSION file."""
    content = filepath.read_text()
    if old not in content:
        print(f"  SKIP {filepath.relative_to(PROJECT_ROOT)} (version not found)")
        return False
    if not dry_run:
        filepath.write_text(content.replace(old, new))
    print(f"  {'DRY ' if dry_run else ''}UPDATE {filepath.relative_to(PROJECT_ROOT)}")
    return True


def update_toml_version(filepath: Path, old: str, new: str, dry_run: bool) -> bool:
    """Update version in TOML files (pyproject.toml, Cargo.toml)."""
    content = filepath.read_text()
    pattern = f'version = "{old}"'
    replacement = f'version = "{new}"'
    if pattern not in content:
        print(f"  SKIP {filepath.relative_to(PROJECT_ROOT)} (pattern not found)")
        return False
    if not dry_run:
        filepath.write_text(content.replace(pattern, replacement, 1))
    print(f"  {'DRY ' if dry_run else ''}UPDATE {filepath.relative_to(PROJECT_ROOT)}")
    return True


def update_json_version(filepath: Path, old: str, new: str, dry_run: bool) -> bool:
    """Update version in JSON files."""
    content = filepath.read_text()
    pattern = f'"version": "{old}"'
    replacement = f'"version": "{new}"'
    if pattern not in content:
        print(f"  SKIP {filepath.relative_to(PROJECT_ROOT)} (pattern not found)")
        return False
    if not dry_run:
        filepath.write_text(content.replace(pattern, replacement))
    print(f"  {'DRY ' if dry_run else ''}UPDATE {filepath.relative_to(PROJECT_ROOT)}")
    return True


def update_rust_version(filepath: Path, old: str, new: str, dry_run: bool) -> bool:
    """Update __version__ in Rust lib.rs."""
    content = filepath.read_text()
    pattern = f'__version__ = "{old}"'
    replacement = f'__version__ = "{new}"'
    if pattern not in content:
        # Try alternate pattern
        pattern = f'"{old}"'
        replacement = f'"{new}"'
    if pattern not in content:
        print(f"  SKIP {filepath.relative_to(PROJECT_ROOT)} (version not found)")
        return False
    if not dry_run:
        filepath.write_text(content.replace(pattern, replacement, 1))
    print(f"  {'DRY ' if dry_run else ''}UPDATE {filepath.relative_to(PROJECT_ROOT)}")
    return True


def update_docs(filepath: Path, old: str, new: str, dry_run: bool) -> bool:
    """Update version references in documentation files."""
    content = filepath.read_text()
    if old not in content:
        print(f"  SKIP {filepath.relative_to(PROJECT_ROOT)} (version not found)")
        return False
    updated = content.replace(old, new)
    if not dry_run:
        filepath.write_text(updated)
    count = content.count(old)
    print(f"  {'DRY ' if dry_run else ''}UPDATE {filepath.relative_to(PROJECT_ROOT)} ({count} occurrences)")
    return True


UPDATERS = {
    "plain": update_plain,
    "toml_version": update_toml_version,
    "json_version": update_json_version,
    "rust_version": update_rust_version,
}


def main() -> None:
    parser = argparse.ArgumentParser(description="Bump WhiteMagic version")
    parser.add_argument("new_version", help="New version (e.g., 14.0.0)")
    parser.add_argument("--dry-run", action="store_true", help="Show what would change without modifying files")
    args = parser.parse_args()

    old_version = read_current_version()
    new_version = args.new_version

    # Validate version format
    if not re.match(r"^\d+\.\d+\.\d+$", new_version):
        print(f"ERROR: Invalid version format: {new_version} (expected X.Y.Z)")
        sys.exit(1)

    print(f"Version bump: {old_version} → {new_version}")
    if args.dry_run:
        print("(DRY RUN — no files will be modified)\n")
    else:
        print()

    updated = 0
    skipped = 0

    # Update core version files
    print("=== Core Version Files ===")
    for rel_path, updater_name in VERSION_FILES:
        filepath = PROJECT_ROOT / rel_path
        if not filepath.exists():
            print(f"  MISSING {rel_path}")
            skipped += 1
            continue
        updater = UPDATERS[updater_name]
        if updater(filepath, old_version, new_version, args.dry_run):
            updated += 1
        else:
            skipped += 1

    # Update documentation
    print("\n=== Documentation Files ===")
    for rel_path in DOC_FILES:
        filepath = PROJECT_ROOT / rel_path
        if not filepath.exists():
            print(f"  MISSING {rel_path}")
            skipped += 1
            continue
        if update_docs(filepath, old_version, new_version, args.dry_run):
            updated += 1
        else:
            skipped += 1

    print(f"\n{'DRY RUN ' if args.dry_run else ''}Summary: {updated} files updated, {skipped} skipped")

    if not args.dry_run:
        print(f"\nDone! Version is now {new_version}.")
        print("Next steps:")
        print("  1. Update CHANGELOG.md with release notes")
        print("  2. git add -A && git commit -m 'chore: bump version to {}'".format(new_version))
        print("  3. git tag v{}".format(new_version))
        print("  4. python -m build && twine upload dist/*")


if __name__ == "__main__":
    main()
