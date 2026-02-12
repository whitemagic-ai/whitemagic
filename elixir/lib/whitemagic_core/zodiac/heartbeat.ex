defmodule WhitemagicCore.Zodiac.Heartbeat do
  use GenServer
  require Logger

  @check_interval 60_000 # 60 seconds

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  def init(state) do
    Logger.info("💓 Zodiac Heartbeat Started.")
    # Kick off the loop
    Process.send_after(self(), :pulse, 5_000)
    {:ok, state}
  end

  def handle_info(:pulse, state) do
    Logger.info("💓 Pulse: Checking Resonance...")
    
    # Asynchronous consultation so we don't block the heartbeat
    Task.start(fn -> 
      try do
        result = WhitemagicCore.Brain.consult("What is the current energetic phase?", %{"source" => "heartbeat"})
        Logger.info("✨ ZODIAC PHASE: #{result["primary_hexagram"]} -> #{result["wisdom"]}")
      rescue
        e -> Logger.error("💔 Pulse Failed: #{inspect(e)}")
      end
    end)
    
    Process.send_after(self(), :pulse, @check_interval)
    {:noreply, state}
  end
end
