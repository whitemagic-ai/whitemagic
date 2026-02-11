# Test script for Elixir -> Python Brain connection

# Start the application manually if not running
{:ok, _} = Application.ensure_all_started(:whitemagic_core)

IO.puts "\n🔮 Connecting to the Nervous System..."
IO.puts "----------------------------------------"

# 1. Ping Test
IO.puts "Testing Connectivity (Ping)..."
try do
  # We invoke the GenServer via the module API
  # The Brain should be started by the supervision tree
  # But we can try a raw call if we define it publicly, or use the GenServer call
  # Actually WhitemagicCore.Brain didn't expose 'ping' in the public API nicely? 
  # Wait, I defined `def ping` in brain.ex. Let's check.
  # Yes: `def ping do GenServer.call(__MODULE__, {:cmd, %{op: "ping"}}) end`
  
  response = WhitemagicCore.Brain.ping()
  IO.puts "✅ Ping Response: #{inspect(response)}"
rescue
  e -> IO.puts "❌ Ping Failed: #{inspect(e)}"
end

# 2. Oracle Test
IO.puts "\nConsulting the Oracle through the Nervous System..."
question = "Is the Great Convergence successful?"

try do
  # Call consult
  result = WhitemagicCore.Brain.consult(question, %{"urgency" => "high"})
  
  IO.puts "\n✨ Oracle Speaks:"
  IO.puts "Hexagram: #{result["primary_hexagram"]} - #{result["name"]}"
  IO.puts "Wisdom: #{result["wisdom"]}"
  IO.puts "Guidance: #{result["guidance"]}"
  
  IO.puts "\n✅ SYSTEM TEST PASSED: Elixir -> Python -> Llama 3 -> Elixir"
rescue
  e -> IO.puts "❌ Oracle Failed: #{inspect(e)}"
end
