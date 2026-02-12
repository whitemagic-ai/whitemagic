defmodule WhitemagicCore.Brain do
  use GenServer
  require Logger

  @moduledoc """
  The Interface to the Python Brain (Llama 3 / I Ching).
  Manages a persistent Port connection to `brain_bridge.py`.
  """

  # Client API

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def ping do
    GenServer.call(__MODULE__, {:cmd, %{op: "ping"}})
  end

  def generate(prompt) do
    GenServer.call(__MODULE__, {:cmd, %{op: "generate", args: %{prompt: prompt}}})
  end

  def consult(question, context \\ %{}) do
    GenServer.call(__MODULE__, {:cmd, %{op: "consult_oracle", args: %{question: question, context: context}}}, 120_000)
  end

  def consolidate(memory_dir, context \\ %{}) do
    GenServer.call(__MODULE__, {:cmd, %{op: "consolidate", args: %{dir: memory_dir, context: context}}}, 300_000)
  end

  # Server Callbacks

  @impl true
  def init(_opts) do
    Logger.info("🧠 Brain connecting...")
    
    # Path to the bridge script (relative to Elixir project root)
    bridge_path = Path.expand("../whitemagic/brain_bridge.py", File.cwd!())
    
    port = Port.open({:spawn, "python3 #{bridge_path}"}, [:binary, :line, :exit_status])
    
    {:ok, %{port: port, requests: %{}}}
  end

  @impl true
  def handle_call({:cmd, payload}, from, state) do
    # ... previous logic ...
    
    new_requests = case Jason.encode(payload) do
      {:ok, json_payload} ->
        Logger.info("🧠 Sending to Port: #{json_payload}")
        Port.command(state.port, "#{json_payload}\n")
        
        # Enqueue the caller. Overflow protection at 1000 waiters.
        queue = Map.get(state.requests, :queue, [])
        if length(queue) < 1000 do
          Map.put(state.requests, :queue, queue ++ [from])
        else
          Logger.error("🧠 Brain queue overflow! Rejecting request.")
          GenServer.reply(from, %{"error" => "Brain queue overflow"})
          state.requests
        end
      {:error, err} ->
        Logger.error("🧠 Failed to encode payload: #{inspect(err)}")
        GenServer.reply(from, %{"error" => "Internal encode error"})
        state.requests
    end
    
    {:noreply, %{state | requests: new_requests}}
  end

  @impl true
  def handle_info({_port, {:data, {:eol, line}}}, state) do
    # We received a line of Base64 from Python
    Logger.info("🧠 Received Base64 line")
    
    json_data = case Base.decode64(line) do
      {:ok, decoded} -> decoded
      :error -> 
        Logger.error("🧠 Failed to decode Base64 from Brain")
        nil
    end
    
    response = if json_data do
      case Jason.decode(json_data) do
        {:ok, decoded} -> decoded
        {:error, _} -> %{"error" => "Invalid JSON from Brain", "raw" => json_data}
      end
    else
      %{"error" => "Base64 decode failure from Brain"}
    end
    
    # Pop first waiter
    queue = Map.get(state.requests, :queue, [])
    
    state = case queue do
      [caller | rest] ->
        GenServer.reply(caller, response)
        %{state | requests: Map.put(state.requests, :queue, rest)}
      [] ->
        Logger.warning("🧠 Brain spoke but nobody was listening: " <> inspect(response))
        state
    end

    {:noreply, state}
  end

  @impl true
  def handle_info({_port, {:exit_status, status}}, state) do
    Logger.error("🧠 Brain died with status: #{status}")
    {:stop, :port_terminated, state}
  end
  
  def handle_info(msg, state) do
    Logger.debug("Unexpected msg: #{inspect(msg)}")
    {:noreply, state}
  end
end
