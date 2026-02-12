defmodule WhitemagicCore.Librarian do
  use GenServer
  require Logger
  alias WhitemagicCore.Ingestor

  defp memory_root do
    Application.get_env(:whitemagic_core, :memory_root) ||
      System.get_env("WM_MEMORY_ROOT") ||
      Path.join([
        System.get_env("WM_STATE_ROOT") || Path.join(System.user_home!(), ".whitemagic"),
        "memory"
      ])
  end

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    Logger.info("📚 Librarian Agent starting...")

    root = memory_root()
    case FileSystem.start_link(dirs: [root]) do
      {:ok, watcher_pid} ->
        FileSystem.subscribe(watcher_pid)
        Logger.info("👀 Watching for changes in #{root}")
        {:ok, %{watcher_pid: watcher_pid}}
      :ignore ->
        Logger.error("❌ FileSystem watcher ignored initialization. Check if inotify-tools is installed and accessible.")
        {:stop, :watcher_ignored}
      {:error, reason} ->
        Logger.error("❌ Failed to start FileSystem watcher: #{inspect(reason)}")
        {:stop, reason}
    end
  end

  @impl true
  def handle_info({:file_event, watcher_pid, {path, events}}, state) do
    Logger.debug("Librarian received event: #{inspect(events)} for path: #{path}")
    
    # Ensure we use absolute paths consistent with the database
    full_path = Path.expand(path)
    
    case Path.extname(full_path) do
      ".md" ->
        if :modified in events or :created in events or :renamed in events do
          Logger.info(" Memory Sync Triggered: #{Path.basename(full_path)}")
          Ingestor.ingest_file(full_path)
        else
          Logger.debug("  Skipping event types: #{inspect(events)}")
        end
      ext ->
        Logger.debug("  Ignoring non-markdown file: #{Path.basename(full_path)} (ext: #{ext})")
    end
    
    {:noreply, state}
  end

  @impl true
  def handle_info({:file_event, _pid, :stop}, state) do
    Logger.warning("  Librarian file watcher stopped!")
    {:noreply, state}
  end

  @impl true
  def handle_info(msg, state) do
    Logger.debug(" Librarian received unexpected message: #{inspect(msg)}")
    {:noreply, state}
  end
end
