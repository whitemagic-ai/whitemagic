defmodule WhitemagicCore.Ingestor do
  alias WhitemagicCore.Memories

  alias WhitemagicCore.Repo
  alias WhitemagicCore.Memory

  require Logger

  @batch_size 100

  defp default_memory_root do
    System.get_env("WM_MEMORY_ROOT") ||
      Path.join([
        System.get_env("WM_STATE_ROOT") || Path.join(System.user_home!(), ".whitemagic"),
        "memory"
      ])
  end

  def ingest_all(root_path \\ nil) do
    root_path = root_path || default_memory_root()
    files = Path.wildcard("#{root_path}/**/*.{md,txt}")
    Logger.info("Found #{length(files)} files to ingest from #{root_path}")
    
    # Use Task.async_stream to parse files in parallel, then collect and batch insert
    files
    |> Task.async_stream(&parse_file/1, max_concurrency: System.schedulers_online() * 2, timeout: 30_000)
    |> Stream.filter(fn {:ok, result} -> result != nil end)
    |> Stream.map(fn {:ok, attrs} -> attrs end)
    |> Stream.chunk_every(@batch_size)
    |> Enum.each(&batch_insert/1)
  end

  @doc """
  🌀 OIP: Sows seeds and graduates them to the "Temple" (Permanent Storage)
  if resonance/importance thresholds are met.
  """
  def ingest_file(path) do
    case parse_file(path) do
      nil -> :ok
      attrs -> 
        case Memories.get_memory_by_path(path) do
          nil -> 
            Memories.create_memory(attrs)
            Logger.info("Created memory: #{attrs.title}")
            check_temple_graduation(attrs)
            check_garden_weeding(attrs)
          existing ->
            if existing.checksum != attrs.checksum do
              Memories.update_memory(existing, attrs)
              Logger.info("Updated memory: #{attrs.title}")
              check_temple_graduation(attrs)
              check_garden_weeding(attrs)
            end
        end
    end
  end

  defp check_garden_weeding(attrs) do
    # 🌀 ORO: Banishing Old Forms
    # "Let the old forms be banished. I begin anew."
    # If importance is very low (< 0.2) and resonance is low (< 0.1), mark for weeding.
    if attrs.importance < 0.2 and attrs.resonance < 0.1 do
      Logger.info("🥀 Memory Marked for Weeding (ORO): #{attrs.title}")
      broadcast_weeding(attrs)
    end
  end

  defp broadcast_weeding(attrs) do
    event = %{
      event_type: "MEMORY.GARDEN.WEEDING",
      source: "whitemagic-elixir",
      data: %{title: attrs.title, reason: "Low vitality (Imp < 0.2)"},
      confidence: 1.0
    }
    publish_to_ganying(event)
  end

  defp check_temple_graduation(attrs) do
    # 🏛️ Temple Graduation logic (OIP/ORO)
    # If importance > 0.8 or resonance > 0.7, it's a "Temple" memory
    if attrs.importance > 0.8 or attrs.resonance > 0.7 do
      Logger.info("🏛️ Memory Graduated to Temple: #{attrs.title}")
      # Broadcast graduation event to Ganying
      broadcast_graduation(attrs)
    end
  end

  defp broadcast_graduation(attrs) do
    event = %{
      event_type: "MEMORY.TEMPLE.GRADUATION",
      source: "whitemagic-elixir",
      data: %{title: attrs.title, importance: attrs.importance, resonance: attrs.resonance},
      confidence: 1.0
    }
    publish_to_ganying(event)
  end

  # ---------------------------------------------------------------------------
  # Redis publish helper — fires and forgets onto the "ganying" channel so the
  # Go mesh and Python Gan Ying bus both see Elixir-originated events.
  # ---------------------------------------------------------------------------
  defp publish_to_ganying(event) do
    case Jason.encode(event) do
      {:ok, json} ->
        case Redix.command(:redix, ["PUBLISH", "ganying", json]) do
          {:ok, _receivers} ->
            Logger.debug("📤 Published #{event.event_type} to ganying")
          {:error, reason} ->
            Logger.warning("📤 Redis publish failed: #{inspect(reason)}")
        end
      {:error, reason} ->
        Logger.warning("📤 JSON encode failed for ganying event: #{inspect(reason)}")
    end
  end

  defp parse_file(path) do
    case File.read(path) do
      {:ok, raw_content} ->
        {frontmatter, content} = split_frontmatter(raw_content)
        metadata = parse_frontmatter(frontmatter)
        
        now = NaiveDateTime.utc_now() |> NaiveDateTime.truncate(:second)

        %{
          id: Ecto.UUID.generate(),
          title: metadata["title"] || Path.basename(path, ".md"),
          content: content,
          format: "markdown",
          path: path,
          checksum: calculate_checksum(raw_content),
          tags: parse_tags(metadata["tags"]),
          importance: parse_float(metadata["importance"], 0.5),
          resonance: parse_float(metadata["resonance"], 0.0),
          created_at: now,
          updated_at: now
        }
      {:error, reason} ->
        Logger.error("Failed to read #{path}: #{inspect(reason)}")
        nil
    end
  end

  defp batch_insert(batch) do
    # Note: insert_all doesn't run validations or use autogenerate.
    # We provide IDs and timestamps manually in parse_file.
    # conflict_target: [:path] requires a unique index on path in SQLite.
    
    try do
      {count, _} = Repo.insert_all(Memory, batch, 
        on_conflict: {:replace, [:title, :content, :checksum, :tags, :importance, :resonance, :updated_at]},
        conflict_target: :path
      )
      Logger.info("Batch inserted/updated #{count} memories")
    rescue
      e -> 
        Logger.error("Batch insert failed: #{inspect(e)}. Falling back to individual inserts.")
        Enum.each(batch, fn attrs ->
          ingest_file(attrs.path)
        end)
    end
  end

  defp split_frontmatter(content) do
    case String.split(content, ~r/^---\s*$/m, parts: 3) do
      ["", frontmatter, body] -> {frontmatter, String.trim(body)}
      [body] -> {"", String.trim(body)}
      _ -> {"", String.trim(content)}
    end
  end

  defp parse_frontmatter(""), do: %{}
  defp parse_frontmatter(yaml) do
    case YamlElixir.read_from_string(yaml) do
      {:ok, map} when is_map(map) -> map
      _ -> %{}
    end
  end

  defp parse_tags(tags) when is_list(tags), do: tags
  defp parse_tags(tags) when is_binary(tags), do: String.split(tags, ",") |> Enum.map(&String.trim/1)
  defp parse_tags(_), do: []

  defp parse_float(val, _default) when is_float(val), do: val
  defp parse_float(val, _default) when is_number(val), do: val / 1.0
  defp parse_float(val, default) when is_binary(val) do
    case Float.parse(val) do
      {f, _} -> f
      :error -> default
    end
  end
  defp parse_float(_, default), do: default

  defp calculate_checksum(content) do
    :crypto.hash(:sha256, content) |> Base.encode16(case: :lower)
  end
end
