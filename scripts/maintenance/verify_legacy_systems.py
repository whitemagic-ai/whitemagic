
import sys
import os
import importlib
import pkgutil
import logging
from dataclasses import dataclass
from typing import List, Dict

# Setup paths
sys.path.append(os.path.join(os.getcwd(), "staging/core_system"))

# Configure logging
logging.basicConfig(level=logging.ERROR)

@dataclass
class SystemStatus:
    name: str
    path: str
    status: str
    error: str = ""
    classes: List[str] = None
    doc: str = ""

def check_module(module_name: str) -> SystemStatus:
    try:
        mod = importlib.import_module(module_name)
        
        # Get classes
        classes = []
        for name, obj in vars(mod).items():
            if isinstance(obj, type) and obj.__module__ == module_name:
                classes.append(name)
                
        return SystemStatus(
            name=module_name,
            path=mod.__file__,
            status="🟢 Active",
            classes=classes[:5], # First 5
            doc=(mod.__doc__ or "").strip().split('\n')[0][:100]
        )
    except ImportError as e:
        return SystemStatus(name=module_name, path="", status="🔴 Missing Dep", error=str(e))
    except Exception as e:
        return SystemStatus(name=module_name, path="", status="🟠 Broken", error=str(e))

def main():
    targets = [
        "whitemagic.core.resonance.gan_ying",
        "whitemagic.core.resonance.gan_ying_async",
        "whitemagic.core.ganas", # Package
        "whitemagic.grimoire",   # Package
        "whitemagic.gardens.metal.zodiac", # Package
        "whitemagic.zodiac",
        "whitemagic.systems.intake.holographic_intake",
        "whitemagic.core.patterns.emergence.dream_state",
        "whitemagic.systems.immune.antibodies",
        "whitemagic.parallel.grimoire_indexer"
    ]

    print("| System | Status | Classes/Content | Error/Note |")
    print("|---|---|---|---|")

    for target in targets:
        # Check if it's a package first
        try:
            # Try plain import
            status = check_module(target)
            
            # If it's a package (init), try to list submodules
            if status.status == "🟢 Active" and "__init__.py" in status.path:
                print(f"| **{target}** (Pkg) | {status.status} | {len(status.classes or [])} Classes | {status.doc} |")
                
                # Walk package
                pkg_path = os.path.dirname(status.path)
                for _, name, _ in pkgutil.iter_modules([pkg_path]):
                    sub_target = f"{target}.{name}"
                    sub_status = check_module(sub_target)
                    print(f"| └ `{name}` | {sub_status.status} | {', '.join(sub_status.classes or [])} | {sub_status.error or sub_status.doc} |")
            else:
                # Regular module
                print(f"| **{target}** | {status.status} | {', '.join(status.classes or [])} | {status.error or status.doc} |")

        except Exception as e:
            print(f"| **{target}** | 💀 Fatal | | {e} |")

if __name__ == "__main__":
    main()
