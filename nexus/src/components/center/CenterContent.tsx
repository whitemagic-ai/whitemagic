import { useNexusStore } from "../../store/nexus";
import MonacoEditor from "./MonacoEditor";
import Dashboard from "./Dashboard";
import MemoryGraph from "./MemoryGraph";
import ToolGraph from "./ToolGraph";

export default function CenterContent() {
  const tab = useNexusStore((s) => s.centerTab);

  return (
    <div className="h-full overflow-hidden">
      {tab === "editor" && <MonacoEditor />}
      {tab === "dashboard" && <Dashboard />}
      {tab === "graph" && <MemoryGraph />}
      {tab === "tools" && <ToolGraph />}
    </div>
  );
}
