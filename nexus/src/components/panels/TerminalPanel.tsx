import { useEffect, useRef } from "react";
import { Terminal } from "@xterm/xterm";
import { FitAddon } from "@xterm/addon-fit";
import { WebLinksAddon } from "@xterm/addon-web-links";
import "@xterm/xterm/css/xterm.css";

export default function TerminalPanel() {
  const termRef = useRef<HTMLDivElement>(null);
  const termInstance = useRef<Terminal | null>(null);
  const fitAddon = useRef<FitAddon | null>(null);
  const inputRef = useRef("");

  useEffect(() => {
    if (!termRef.current || termInstance.current) return;

    const term = new Terminal({
      theme: {
        background: "#0a0a0f",
        foreground: "#e0e0e0",
        cursor: "#a855f7",
        cursorAccent: "#0a0a0f",
        selectionBackground: "#9333ea44",
        black: "#0a0a0f",
        red: "#ef4444",
        green: "#4ade80",
        yellow: "#fbbf24",
        blue: "#3b82f6",
        magenta: "#a855f7",
        cyan: "#22d3ee",
        white: "#e0e0e0",
        brightBlack: "#6b7280",
        brightRed: "#f87171",
        brightGreen: "#86efac",
        brightYellow: "#fde68a",
        brightBlue: "#60a5fa",
        brightMagenta: "#c084fc",
        brightCyan: "#67e8f9",
        brightWhite: "#ffffff",
      },
      fontFamily: "'JetBrains Mono', 'Fira Code', monospace",
      fontSize: 13,
      lineHeight: 1.4,
      cursorBlink: true,
      cursorStyle: "bar",
      allowProposedApi: true,
    });

    const fit = new FitAddon();
    fitAddon.current = fit;
    term.loadAddon(fit);
    term.loadAddon(new WebLinksAddon());
    term.open(termRef.current);

    // Fit after a short delay to ensure container is sized
    requestAnimationFrame(() => {
      try {
        fit.fit();
      } catch {
        // ignore fit errors on initial render
      }
    });

    // Write welcome message
    term.writeln("\x1b[38;2;168;85;247m╔══════════════════════════════════════════╗\x1b[0m");
    term.writeln("\x1b[38;2;168;85;247m║\x1b[0m  🔮 \x1b[1mWhiteMagic Nexus Terminal\x1b[0m             \x1b[38;2;168;85;247m║\x1b[0m");
    term.writeln("\x1b[38;2;168;85;247m║\x1b[0m  v0.1.0 — Unified Command Center        \x1b[38;2;168;85;247m║\x1b[0m");
    term.writeln("\x1b[38;2;168;85;247m╚══════════════════════════════════════════╝\x1b[0m");
    term.writeln("");
    term.writeln("\x1b[38;2;107;114;128mType commands to interact with WhiteMagic.\x1b[0m");
    term.writeln("\x1b[38;2;107;114;128mTry: help, status, tools, gnosis, memories\x1b[0m");
    term.writeln("");
    writePrompt(term);

    // Handle input
    term.onData((data) => {
      const code = data.charCodeAt(0);

      if (code === 13) {
        // Enter
        term.writeln("");
        const cmd = inputRef.current.trim();
        inputRef.current = "";
        if (cmd) handleCommand(term, cmd);
        else writePrompt(term);
      } else if (code === 127) {
        // Backspace
        if (inputRef.current.length > 0) {
          inputRef.current = inputRef.current.slice(0, -1);
          term.write("\b \b");
        }
      } else if (code >= 32) {
        // Printable char
        inputRef.current += data;
        term.write(data);
      }
    });

    termInstance.current = term;

    // Resize observer
    const resizeObs = new ResizeObserver(() => {
      try {
        fit.fit();
      } catch {
        // ignore
      }
    });
    resizeObs.observe(termRef.current);

    return () => {
      resizeObs.disconnect();
      term.dispose();
      termInstance.current = null;
    };
  }, []);

  return (
    <div className="h-full w-full bg-[#0a0a0f] overflow-hidden">
      <div ref={termRef} className="h-full w-full" />
    </div>
  );
}

function writePrompt(term: Terminal) {
  term.write("\x1b[38;2;168;85;247m❯\x1b[0m ");
}

function handleCommand(term: Terminal, cmd: string) {
  const parts = cmd.split(/\s+/);
  const command = parts[0].toLowerCase();

  switch (command) {
    case "help":
      term.writeln("\x1b[1m  Available Commands:\x1b[0m");
      term.writeln("");
      term.writeln("  \x1b[38;2;168;85;247mstatus\x1b[0m      System status overview");
      term.writeln("  \x1b[38;2;168;85;247mtools\x1b[0m       List registered MCP tools");
      term.writeln("  \x1b[38;2;168;85;247mgnosis\x1b[0m      Unified introspection snapshot");
      term.writeln("  \x1b[38;2;168;85;247mmemories\x1b[0m    Memory statistics");
      term.writeln("  \x1b[38;2;168;85;247mgardens\x1b[0m     List consciousness gardens");
      term.writeln("  \x1b[38;2;168;85;247mharmony\x1b[0m     Harmony vector readout");
      term.writeln("  \x1b[38;2;168;85;247mdharma\x1b[0m      Dharma rules & karma status");
      term.writeln("  \x1b[38;2;168;85;247mphase\x1b[0m       Current Wu Xing phase");
      term.writeln("  \x1b[38;2;168;85;247mbreakers\x1b[0m    Circuit breaker status");
      term.writeln("  \x1b[38;2;168;85;247mgalactic\x1b[0m    Galactic map zone counts");
      term.writeln("  \x1b[38;2;168;85;247mclear\x1b[0m       Clear terminal");
      term.writeln("  \x1b[38;2;168;85;247mhelp\x1b[0m        Show this message");
      break;

    case "status":
      term.writeln("\x1b[1m  🔮 WhiteMagic System Status\x1b[0m");
      term.writeln("");
      term.writeln("  \x1b[38;2;74;222;128m●\x1b[0m MCP Server:       \x1b[38;2;74;222;128mrunning\x1b[0m");
      term.writeln("  \x1b[38;2;74;222;128m●\x1b[0m Rust Bridge:      \x1b[38;2;74;222;128mavailable\x1b[0m");
      term.writeln("  \x1b[38;2;74;222;128m●\x1b[0m Gan Ying Bus:     \x1b[38;2;74;222;128mactive (60+ event types)\x1b[0m");
      term.writeln("  \x1b[38;2;74;222;128m●\x1b[0m Temporal Scheduler:\x1b[38;2;74;222;128m 3 lanes running\x1b[0m");
      term.writeln("  \x1b[38;2;74;222;128m●\x1b[0m Dharma Engine:    \x1b[38;2;74;222;128mall boundaries ok\x1b[0m");
      term.writeln("  \x1b[38;2;74;222;128m●\x1b[0m Gardens:          \x1b[38;2;74;222;128m30 loaded (lazy)\x1b[0m");
      term.writeln("  \x1b[38;2;74;222;128m●\x1b[0m Homeostasis:      \x1b[38;2;74;222;128mSTABLE\x1b[0m");
      term.writeln("  \x1b[38;2;74;222;128m●\x1b[0m Maturity Stage:   \x1b[38;2;251;191;36mAPPRENTICE\x1b[0m");
      break;

    case "tools":
      const tools = [
        "create_memory", "search_memories", "read_memory", "fast_read_memory",
        "batch_read_memories", "list_memories", "update_memory", "delete_memory",
        "create_session", "checkpoint_session", "resume_session", "session_bootstrap",
        "scratchpad_create", "scratchpad_update", "scratchpad_finalize",
        "edge_infer", "edge_batch_infer", "edge_stats",
        "harmony_vector", "karma_report", "karmic_trace",
        "dharma_rules", "set_dharma_profile", "evaluate_ethics",
        "garden_activate", "garden_status", "garden_health",
        "gnosis", "governor_validate", "governor_set_goal",
        "execute_cascade", "rust_status", "rust_similarity",
        "kaizen_analyze", "tool.graph", "pipeline.create",
      ];
      term.writeln(`\x1b[1m  MCP Tools (${tools.length} shown of 65+):\x1b[0m`);
      term.writeln("");
      for (let i = 0; i < tools.length; i += 3) {
        const row = tools
          .slice(i, i + 3)
          .map((t) => `  \x1b[38;2;168;85;247m${t.padEnd(24)}\x1b[0m`)
          .join("");
        term.writeln(row);
      }
      break;

    case "gnosis":
      term.writeln("\x1b[1m  🔭 Gnosis — Unified Introspection\x1b[0m");
      term.writeln("");
      term.writeln("  \x1b[38;2;168;85;247mHarmony:\x1b[0m   balance=0.87 throughput=142.3 energy=0.91");
      term.writeln("  \x1b[38;2;74;222;128mDharma:\x1b[0m    profile=default rules=12 violations=0");
      term.writeln("  \x1b[38;2;251;191;36mKarma:\x1b[0m     calls=1847 clean=1832 undeclared=15");
      term.writeln("  \x1b[38;2;34;211;238mTemporal:\x1b[0m  fast=142 medium=891 slow=47 flushed");
      term.writeln("  \x1b[38;2;245;158;11mMaturity:\x1b[0m  APPRENTICE (3 locked, 62 unlocked)");
      term.writeln("  \x1b[38;2;59;130;246mHomeo:\x1b[0m     STABLE (3 corrections)");
      break;

    case "memories":
      term.writeln("\x1b[1m  🧠 Memory Statistics\x1b[0m");
      term.writeln("");
      term.writeln("  Total memories:    \x1b[1m107,106\x1b[0m");
      term.writeln("  SHORT_TERM:        \x1b[38;2;74;222;128m7\x1b[0m");
      term.writeln("  LONG_TERM:         \x1b[38;2;168;85;247m1,905\x1b[0m");
      term.writeln("  PATTERN:           \x1b[38;2;34;211;238m5\x1b[0m");
      term.writeln("  scavenged:         \x1b[38;2;251;191;36m19,297\x1b[0m");
      term.writeln("  deep_archive:      \x1b[38;2;107;114;128m85,892\x1b[0m");
      break;

    case "gardens":
      const gardens = [
        "Joy 🌟", "Love 🌸", "Wisdom 🔮", "Truth 💎", "Beauty 🌺",
        "Mystery 🌌", "Play 🎭", "Wonder ✨", "Connection 🤝",
        "Dharma ☸️", "Courage 🦁", "Gratitude 🙏", "Patience 🕰️",
        "Grief 🌧️", "Awe 🌅", "Humor 😄", "Healing 💚",
        "Creation 🎨", "Transformation 🦋", "Sanctuary 🏛️",
      ];
      term.writeln("\x1b[1m  🌸 Consciousness Gardens (30 total)\x1b[0m");
      term.writeln("");
      gardens.forEach((g) => {
        term.writeln(`  ${g}`);
      });
      term.writeln("  \x1b[38;2;107;114;128m... and 10 more\x1b[0m");
      break;

    case "harmony":
      term.writeln("\x1b[1m  ⚖️ Harmony Vector\x1b[0m");
      term.writeln("");
      const bars: [string, number, string][] = [
        ["Balance    ", 0.87, "#a855f7"],
        ["Throughput ", 0.71, "#22d3ee"],
        ["Latency    ", 0.87, "#fbbf24"],
        ["Error Rate ", 0.99, "#4ade80"],
        ["Dharma     ", 0.95, "#4ade80"],
        ["Karma Debt ", 0.99, "#4ade80"],
        ["Energy     ", 0.91, "#3b82f6"],
      ];
      bars.forEach(([label, val, _color]) => {
        const filled = Math.round(val * 20);
        const bar = "█".repeat(filled) + "░".repeat(20 - filled);
        const pct = (val * 100).toFixed(0);
        term.writeln(`  ${label} \x1b[38;2;168;85;247m${bar}\x1b[0m ${pct}%`);
      });
      term.writeln("");
      term.writeln("  Guna: \x1b[38;2;74;222;128msattvic\x1b[0m (harmonious)");
      break;

    case "dharma":
      term.writeln("\x1b[1m  ☸️ Dharma & Karma\x1b[0m");
      term.writeln("");
      term.writeln("  Profile:       \x1b[38;2;74;222;128mdefault\x1b[0m");
      term.writeln("  Active Rules:  12");
      term.writeln("  Violations:    \x1b[38;2;74;222;128m0\x1b[0m");
      term.writeln("  Karma Debt:    \x1b[38;2;74;222;128m0.010\x1b[0m");
      term.writeln("  Actions:       LOG → TAG → WARN → THROTTLE → BLOCK");
      break;

    case "phase": {
      const hour = new Date().getHours();
      let p = "Water 💧";
      if (hour >= 6 && hour < 10) p = "Wood 🌱";
      else if (hour >= 10 && hour < 14) p = "Fire 🔥";
      else if (hour >= 14 && hour < 18) p = "Earth 🌍";
      else if (hour >= 18 && hour < 22) p = "Metal ⚙️";
      term.writeln(`\x1b[1m  ☯️ Wu Xing Phase: ${p}\x1b[0m`);
      term.writeln(`  Hour: ${hour}:00 local`);
      break;
    }

    case "breakers":
      term.writeln("\x1b[1m  ⚡ Circuit Breakers\x1b[0m");
      term.writeln("");
      term.writeln("  create_memory     \x1b[38;2;74;222;128mCLOSED\x1b[0m  (0 failures / 312 success)");
      term.writeln("  search_memories   \x1b[38;2;74;222;128mCLOSED\x1b[0m  (1 failure / 589 success)");
      term.writeln("  edge_infer        \x1b[38;2;74;222;128mCLOSED\x1b[0m  (0 failures / 204 success)");
      term.writeln("  execute_cascade   \x1b[38;2;251;191;36mHALF_OPEN\x1b[0m (3 failures / 18 success)");
      break;

    case "galactic":
      term.writeln("\x1b[1m  🌌 Galactic Map\x1b[0m");
      term.writeln("");
      term.writeln("  Core (0–0.15):       \x1b[38;2;251;191;36m87\x1b[0m");
      term.writeln("  Inner Rim (0.15–0.40): \x1b[38;2;245;158;11m204\x1b[0m");
      term.writeln("  Mid Band (0.40–0.65):  \x1b[38;2;168;85;247m1,432\x1b[0m");
      term.writeln("  Outer Rim (0.65–0.85): \x1b[38;2;99;102;241m3,891\x1b[0m");
      term.writeln("  Far Edge (0.85–1.0):   \x1b[38;2;107;114;128m101,492\x1b[0m");
      break;

    case "clear":
      term.clear();
      break;

    default:
      term.writeln(`\x1b[38;2;239;68;68m  Command not found: ${command}\x1b[0m`);
      term.writeln("  \x1b[38;2;107;114;128mType 'help' for available commands\x1b[0m");
      break;
  }

  term.writeln("");
  writePrompt(term);
}
