import Header from "./components/Header";
import PanelLayout from "./components/panels/PanelLayout";
import LeftPanel from "./components/panels/LeftPanel";
import RightPanel from "./components/panels/RightPanel";
import BottomPanel from "./components/panels/BottomPanel";
import CenterContent from "./components/center/CenterContent";
import StatusBar from "./components/status/StatusBar";
import CommandPalette from "./components/CommandPalette";

export default function App() {
  return (
    <>
      <CommandPalette />
      <Header />
      <PanelLayout
        leftPanel={<LeftPanel />}
        rightPanel={<RightPanel />}
        bottomPanel={<BottomPanel />}
      >
        <CenterContent />
      </PanelLayout>
      <StatusBar />
    </>
  );
}
