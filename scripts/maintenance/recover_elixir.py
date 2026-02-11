import os
import re
import shutil
from pathlib import Path
import datetime

# Config
REPO_ROOT = Path(__file__).resolve().parents[2]
HISTORY_ROOT = os.getenv(
    "WM_WINDSURF_HISTORY_ROOT",
    str(Path.home() / ".config" / "Windsurf" / "User" / "History"),
)
DEST_ROOT = os.getenv("WM_ELIXIR_LIB_DEST", str(REPO_ROOT / "elixir" / "lib"))
MIX_DEST = os.getenv("WM_ELIXIR_DEST", str(REPO_ROOT / "elixir"))

def camel_to_snake(name):
    name = re.sub('(.)([A-Z][a-z]+)', r'\1_\2', name)
    return re.sub('([a-z0-9])([A-Z])', r'\1_\2', name).lower()

def extract_module_name(content):
    match = re.search(r'defmodule\s+([A-Z][a-zA-Z0-9\.]*)', content)
    if match:
        return match.group(1)
    return None

def recover_files():
    print(f"🕵️ Scanning history at {HISTORY_ROOT}...")

    if not os.path.exists(HISTORY_ROOT):
        print("History root not found. Set WM_WINDSURF_HISTORY_ROOT to the correct path.")
        return
    
    history_groups = {}
    for root, _, files in os.walk(HISTORY_ROOT):
        if not files:
            continue
        rel_path = os.path.relpath(root, HISTORY_ROOT)
        group_id = rel_path.split('/')[0] if '/' in rel_path else rel_path
        
        if group_id not in history_groups:
            history_groups[group_id] = []
            
        for f in files:
            full_path = os.path.join(root, f)
            history_groups[group_id].append(full_path)

    print(f"Found {len(history_groups)} unique file history entries.")

    count = 0
    for group_id, file_paths in history_groups.items():
        # Sort by time descending (newest first)
        sorted_files = sorted(file_paths, key=os.path.getmtime, reverse=True)
        
        found_in_group = False
        for file_path in sorted_files:
            if found_in_group: break
            
            try:
                # Try reading with fallback encodings
                try:
                    with open(file_path, 'r', encoding='utf-8') as f:
                        content = f.read()
                except UnicodeDecodeError:
                    continue # Skip binary/junk
                
                # Check for Elixir signatures
                if "defmodule" not in content and "use Mix.Project" not in content:
                    continue

                # Special case for mix.exs
                if "use Mix.Project" in content or "defmodule WhitemagicCore.MixProject" in content:
                    dest = os.path.join(MIX_DEST, "mix.exs")
                    # print(f"pkg: Found mix.exs candidate in {group_id}")
                    if not os.path.exists(dest): # Don't overwrite if found
                        print(f"📦 Recovered mix.exs -> {dest}")
                        with open(dest, 'w') as f:
                            f.write(content)
                        count += 1
                        found_in_group = True
                    continue

                # Standard Modules
                module_name = extract_module_name(content)
                if module_name:
                    parts = module_name.split('.')
                    snake_parts = [camel_to_snake(p) for p in parts]
                    
                    filename = snake_parts[-1] + ".ex"
                    dir_path = os.path.join(DEST_ROOT, *snake_parts[:-1])
                    
                    os.makedirs(dir_path, exist_ok=True)
                    final_path = os.path.join(dir_path, filename)
                    
                    # Only write if not exists or if this version is larger/better?
                    # For now just take the first valid one (newest)
                    if not os.path.exists(final_path):
                        print(f"✨ Recovered {module_name} -> {final_path}")
                        with open(final_path, 'w') as f:
                            f.write(content)
                        count += 1
                        found_in_group = True
                    
            except Exception as e:
                # print(f"⚠️ Error processing {file_path}: {e}")
                pass

    print(f"✅ Total recovered files: {count}")

if __name__ == "__main__":
    recover_files()
