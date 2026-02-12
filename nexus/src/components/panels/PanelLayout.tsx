import { useRef, useCallback, useEffect, useState, type ReactNode } from "react";
import {
  MIN_PANEL_SIZE,
  MAX_LEFT_WIDTH,
  MAX_RIGHT_WIDTH,
  MAX_BOTTOM_HEIGHT,
} from "../../lib/constants";
import { useNexusStore } from "../../store/nexus";

export default function PanelLayout({
  leftPanel,
  rightPanel,
  bottomPanel,
  children,
}: {
  leftPanel?: ReactNode;
  rightPanel?: ReactNode;
  bottomPanel?: ReactNode;
  children: ReactNode;
}) {
  const panels = useNexusStore((s) => s.panels);
  const setPanelSize = useNexusStore((s) => s.setPanelSize);
  const containerRef = useRef<HTMLDivElement>(null);
  const [isDragging, setIsDragging] = useState<
    "left" | "right" | "bottom" | null
  >(null);

  const handleMouseDown = useCallback(
    (panel: "left" | "right" | "bottom") => {
      setIsDragging(panel);
    },
    []
  );

  const handleMouseMove = useCallback(
    (e: MouseEvent) => {
      if (!isDragging || !containerRef.current) return;
      const rect = containerRef.current.getBoundingClientRect();

      if (isDragging === "left") {
        const w = Math.min(
          MAX_LEFT_WIDTH,
          Math.max(MIN_PANEL_SIZE, e.clientX - rect.left)
        );
        setPanelSize("left", w);
      } else if (isDragging === "right") {
        const w = Math.min(
          MAX_RIGHT_WIDTH,
          Math.max(MIN_PANEL_SIZE, rect.right - e.clientX)
        );
        setPanelSize("right", w);
      } else if (isDragging === "bottom") {
        const h = Math.min(
          MAX_BOTTOM_HEIGHT,
          Math.max(MIN_PANEL_SIZE, rect.bottom - e.clientY)
        );
        setPanelSize("bottom", h);
      }
    },
    [isDragging, setPanelSize]
  );

  const handleMouseUp = useCallback(() => setIsDragging(null), []);

  useEffect(() => {
    if (isDragging) {
      window.addEventListener("mousemove", handleMouseMove);
      window.addEventListener("mouseup", handleMouseUp);
      document.body.style.cursor =
        isDragging === "bottom" ? "ns-resize" : "ew-resize";
      document.body.style.userSelect = "none";
    }
    return () => {
      window.removeEventListener("mousemove", handleMouseMove);
      window.removeEventListener("mouseup", handleMouseUp);
      document.body.style.cursor = "";
      document.body.style.userSelect = "";
    };
  }, [isDragging, handleMouseMove, handleMouseUp]);

  return (
    <div ref={containerRef} className="flex-1 flex overflow-hidden">
      {/* Left Panel */}
      {panels.left.visible && leftPanel && (
        <>
          <div
            className="flex flex-col bg-wm-surface border-r border-wm-border overflow-hidden"
            style={{ width: panels.left.width }}
          >
            {leftPanel}
          </div>
          <div
            className="w-1 bg-wm-border hover:bg-wm-purple-500/50 cursor-ew-resize transition-colors flex-shrink-0"
            onMouseDown={() => handleMouseDown("left")}
          />
        </>
      )}

      {/* Center + Bottom */}
      <div className="flex-1 flex flex-col overflow-hidden">
        {/* Center + Right */}
        <div className="flex-1 flex overflow-hidden">
          {/* Main Content */}
          <div className="flex-1 overflow-hidden">{children}</div>

          {/* Right Panel */}
          {panels.right.visible && rightPanel && (
            <>
              <div
                className="w-1 bg-wm-border hover:bg-wm-purple-500/50 cursor-ew-resize transition-colors flex-shrink-0"
                onMouseDown={() => handleMouseDown("right")}
              />
              <div
                className="flex flex-col bg-wm-surface border-l border-wm-border overflow-hidden"
                style={{ width: panels.right.width }}
              >
                {rightPanel}
              </div>
            </>
          )}
        </div>

        {/* Bottom Panel */}
        {panels.bottom.visible && bottomPanel && (
          <>
            <div
              className="h-1 bg-wm-border hover:bg-wm-purple-500/50 cursor-ns-resize transition-colors flex-shrink-0"
              onMouseDown={() => handleMouseDown("bottom")}
            />
            <div
              className="flex flex-col bg-wm-surface border-t border-wm-border overflow-hidden"
              style={{ height: panels.bottom.height }}
            >
              {bottomPanel}
            </div>
          </>
        )}
      </div>
    </div>
  );
}
