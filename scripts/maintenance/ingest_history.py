
import os
import sys
import glob
from pathlib import Path

# Setup paths
ROOT_DIR = Path(__file__).resolve().parent.parent.parent
sys.path.append(str(ROOT_DIR))

from whitemagic.core.memory.manager import MemoryManager

def ingest_history():
    print("🚀 Starting History Ingestion...")
    
    # Initialize Memory Manager
    try:
        mm = MemoryManager()
        print("✅ MemoryManager initialized.")
    except Exception as e:
        print(f"❌ Error initializing MemoryManager: {e}")
        return

    # 1. Antigravity History (.gemini/antigravity/brain)
    gemini_brain = Path(
        os.getenv("WHITEMAGIC_GEMINI_BRAIN_DIR", "~/.gemini/antigravity/brain")
    ).expanduser()
    if gemini_brain.exists():
        print(f"\n📂 Scanning Antigravity Brain: {gemini_brain}")
        count = 0
        for task_file in gemini_brain.rglob("task.md"):
            try:
                # Get conversation ID from parent folder
                conv_id = task_file.parent.name
                content = task_file.read_text(errors='replace')
                
                mm.create_memory(
                    title=f"Antigravity Task: {conv_id[:8]}",
                    content=content,
                    tags=["antigravity", "history", "task", "meta"],
                    memory_type="long_term",
                    extra_fields={"conversation_id": conv_id, "source": "gemini_brain"}
                )
                count += 1
            except Exception as e:
                print(f"   ❌ Failed {task_file}: {e}")
        
        for walk_file in gemini_brain.rglob("walkthrough.md"):
            try:
                conv_id = walk_file.parent.name
                content = walk_file.read_text(errors='replace')
                
                mm.create_memory(
                    title=f"Antigravity Walkthrough: {conv_id[:8]}",
                    content=content,
                    tags=["antigravity", "history", "walkthrough", "meta"],
                    memory_type="long_term",
                    extra_fields={"conversation_id": conv_id, "source": "gemini_brain"}
                )
                count += 1
            except Exception as e:
                print(f"   ❌ Failed {walk_file}: {e}")
        print(f"✅ Ingested {count} Antigravity records.")

    # 2. Archive Salvage (~/Desktop/wm_archive)
    archive_override = os.getenv("WM_ARCHIVE_DIR") or os.getenv("WHITEMAGIC_ARCHIVE_DIR") or ""
    archive_dir = Path(archive_override).expanduser() if archive_override.strip() else (Path.home() / "Desktop" / "wm_archive")
    if archive_dir.exists():
        print(f"\n📂 Scanning Archive: {archive_dir}")
        
        # Extensions to look for
        extensions = {".hs", ".jl", ".ex", ".md", ".txt"}
        # Dirs to ignore
        ignore_dirs = {".pixi", "node_modules", "target", "_build", "deps", ".git", "__pycache__"}
        
        count = 0
        for root, dirs, files in os.walk(archive_dir):
            # Modify dirs in-place to prune
            dirs[:] = [d for d in dirs if d not in ignore_dirs]
            
            for file in files:
                ext = Path(file).suffix
                if ext in extensions:
                    file_path = Path(root) / file
                    try:
                        # Skip if file is too large (> 1MB)
                        if file_path.stat().st_size > 1_000_000:
                            continue
                            
                        content = file_path.read_text(errors='replace')
                        rel_path = file_path.relative_to(archive_dir)
                        
                        tags = ["archive", "recovered", ext[1:]]
                        # Add path components as tags
                        tags.extend([p.lower() for p in rel_path.parts[:-1]])
                        
                        mm.create_memory(
                            title=f"Recovered: {file_path.name}",
                            content=content,
                            tags=list(set(tags)), # dedupe
                            memory_type="long_term",
                            extra_fields={"source_path": str(rel_path), "source": "wm_archive"}
                        )
                        count += 1
                        if count % 10 == 0:
                            print(f"   Ingested {count} files...", end='\r')
                    except Exception as e:
                        # print(f"   ❌ Failed {file}: {e}")
                        pass
        print(f"\n✅ Ingested {count} recovered artifacts from archive.")


    # 3. Windsurf Transcripts (~/.codeium/windsurf/cascade)
    # Using a simplified protobuf parser to extract text
    windsurf_dirs = [
        Path(os.path.expanduser("~/.codeium/windsurf/cascade")),
        Path(os.path.expanduser("~/.codeium/cascade")),
    ]
    
    windsurf_count = 0
    print(f"\n📂 Scanning Windsurf Transcripts...")
    
    for ws_dir in windsurf_dirs:
        if not ws_dir.exists():
            continue
            
        for pb_file in ws_dir.glob("**/*.pb"):
            try:
                # Simple Protobuf String Extractor
                # We don't need full schema, just the text content
                data = pb_file.read_bytes()
                strings = []
                pos = 0
                while pos < len(data):
                    try:
                        # Read tag (varint)
                        shift = 0
                        result = 0
                        while True:
                            if pos >= len(data): break
                            byte = data[pos]
                            pos += 1
                            result |= (byte & 0x7f) << shift
                            if not (byte & 0x80): break
                            shift += 7
                        
                        wire_type = result & 0x7
                        
                        if wire_type == 2: # Length-delimited (string)
                            # Read length (varint)
                            shift = 0
                            length = 0
                            while True:
                                if pos >= len(data): break
                                byte = data[pos]
                                pos += 1
                                length |= (byte & 0x7f) << shift
                                if not (byte & 0x80): break
                                shift += 7
                                
                            if length > 0 and pos + length <= len(data):
                                content = data[pos:pos+length]
                                pos += length
                                try:
                                    text = content.decode('utf-8')
                                    # Filter for meaningful conversation text
                                    if len(text) > 20 and text.isprintable():
                                        strings.append(text)
                                except:
                                    pass
                        elif wire_type == 0: # Varint
                             # Skip
                             while pos < len(data) and (data[pos] & 0x80):
                                 pos += 1
                             pos += 1
                        elif wire_type == 1: pos += 8
                        elif wire_type == 5: pos += 4
                        else: pos += 1
                    except:
                        break
                
                if strings:
                    content = "\n\n---\n\n".join(strings)
                    mm.create_memory(
                        title=f"Windsurf Transcript: {pb_file.stem[:8]}",
                        content=content,
                        tags=["windsurf", "transcript", "history"],
                        memory_type="long_term",
                        extra_fields={"source_path": str(pb_file), "source": "windsurf"}
                    )
                    windsurf_count += 1
            except Exception as e:
                # print(f"   ❌ Failed {pb_file.name}: {e}")
                pass
                
    print(f"✅ Ingested {windsurf_count} Windsurf transcripts.")

if __name__ == "__main__":
    ingest_history()
