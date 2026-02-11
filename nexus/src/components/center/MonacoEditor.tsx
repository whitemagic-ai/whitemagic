import Editor from "@monaco-editor/react";
import { X, FileText, Circle } from "lucide-react";
import { useEditorStore } from "../../store/editor";

export default function MonacoEditor() {
  const openFiles = useEditorStore((s) => s.openFiles);
  const activeFile = useEditorStore((s) => s.activeFile);
  const setActiveFile = useEditorStore((s) => s.setActiveFile);
  const closeFile = useEditorStore((s) => s.closeFile);
  const updateContent = useEditorStore((s) => s.updateContent);

  const currentFile = openFiles.find((f) => f.path === activeFile);

  if (!currentFile) {
    return (
      <div className="h-full flex items-center justify-center bg-wm-bg text-gray-500">
        <div className="text-center">
          <div className="text-5xl mb-4 opacity-20">🔮</div>
          <div className="text-sm">No files open</div>
          <div className="text-xs mt-1 text-gray-600">
            Use the explorer to open a file
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="h-full flex flex-col bg-wm-bg">
      {/* Tab bar */}
      <div className="flex items-center border-b border-wm-border overflow-x-auto flex-shrink-0">
        {openFiles.map((file) => (
          <button
            key={file.path}
            onClick={() => setActiveFile(file.path)}
            className={`group flex items-center gap-1.5 px-3 py-1.5 text-xs border-r border-wm-border whitespace-nowrap transition ${
              file.path === activeFile
                ? "bg-wm-bg text-wm-purple-400 border-b-2 border-b-wm-purple-400"
                : "bg-wm-surface text-gray-500 hover:text-gray-300"
            }`}
          >
            <FileText size={12} />
            <span>{file.name}</span>
            {file.dirty && (
              <Circle size={6} className="fill-wm-purple-400 text-wm-purple-400" />
            )}
            <span
              onClick={(e) => {
                e.stopPropagation();
                closeFile(file.path);
              }}
              className="ml-1 p-0.5 rounded hover:bg-white/10 opacity-0 group-hover:opacity-100 transition"
            >
              <X size={10} />
            </span>
          </button>
        ))}
      </div>

      {/* Monaco editor */}
      <div className="flex-1 overflow-hidden">
        <Editor
          height="100%"
          language={currentFile.language}
          value={currentFile.content}
          theme="nexus-dark"
          onChange={(value) => {
            if (value !== undefined && activeFile) {
              updateContent(activeFile, value);
            }
          }}
          beforeMount={(monaco) => {
            monaco.editor.defineTheme("nexus-dark", {
              base: "vs-dark",
              inherit: true,
              rules: [
                { token: "comment", foreground: "6b7280", fontStyle: "italic" },
                { token: "keyword", foreground: "c084fc" },
                { token: "string", foreground: "86efac" },
                { token: "number", foreground: "67e8f9" },
                { token: "type", foreground: "22d3ee" },
                { token: "function", foreground: "a78bfa" },
                { token: "variable", foreground: "e0e0e0" },
                { token: "operator", foreground: "94a3b8" },
              ],
              colors: {
                "editor.background": "#0a0a0f",
                "editor.foreground": "#e0e0e0",
                "editor.lineHighlightBackground": "#1e1e2e",
                "editor.selectionBackground": "#9333ea33",
                "editor.inactiveSelectionBackground": "#9333ea1a",
                "editorLineNumber.foreground": "#3b3b4f",
                "editorLineNumber.activeForeground": "#a855f7",
                "editorCursor.foreground": "#a855f7",
                "editorWidget.background": "#12121a",
                "editorWidget.border": "#1e1e2e",
                "editorSuggestWidget.background": "#12121a",
                "editorSuggestWidget.border": "#1e1e2e",
                "editorSuggestWidget.selectedBackground": "#9333ea33",
                "scrollbar.shadow": "#00000000",
                "scrollbarSlider.background": "#3b3b4f66",
                "scrollbarSlider.hoverBackground": "#4b4b5f88",
                "scrollbarSlider.activeBackground": "#9333ea66",
                "minimap.background": "#0a0a0f",
              },
            });
          }}
          options={{
            fontSize: 13,
            fontFamily: "'JetBrains Mono', 'Fira Code', monospace",
            fontLigatures: true,
            lineHeight: 20,
            minimap: { enabled: true, scale: 1, renderCharacters: false },
            scrollBeyondLastLine: false,
            smoothScrolling: true,
            cursorBlinking: "smooth",
            cursorSmoothCaretAnimation: "on",
            renderLineHighlight: "all",
            renderWhitespace: "selection",
            bracketPairColorization: { enabled: true },
            guides: { bracketPairs: true, indentation: true },
            padding: { top: 8 },
            overviewRulerBorder: false,
            hideCursorInOverviewRuler: true,
            contextmenu: true,
          }}
        />
      </div>

      {/* Editor status bar */}
      <div className="flex items-center justify-between px-3 py-1 border-t border-wm-border text-xs text-gray-500 flex-shrink-0 bg-wm-surface">
        <div className="flex items-center gap-3">
          <span className="text-wm-purple-400">{currentFile.language}</span>
          <span>UTF-8</span>
          <span>LF</span>
        </div>
        <div className="flex items-center gap-3">
          <span>{currentFile.dirty ? "Modified" : "Saved"}</span>
          <span className="text-wm-purple-400">Monaco</span>
        </div>
      </div>
    </div>
  );
}
