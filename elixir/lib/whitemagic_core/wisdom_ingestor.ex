defmodule WhitemagicCore.WisdomIngestor do
  alias WhitemagicCore.Memories
  require Logger

  defp wisdom_file do
    Application.get_env(:whitemagic_core, :wisdom_path) ||
      System.get_env("WM_WISDOM_PATH") ||
      Path.join([default_state_root(), "data", "wisdom.json"])
  end

  defp default_state_root do
    System.get_env("WM_STATE_ROOT") || Path.join(System.user_home!(), ".whitemagic")
  end

  def ingest do
    case File.read(wisdom_file()) do
      {:ok, body} ->
        data = Jason.decode!(body)
        collection = data["wisdom_collection"] || []
        Logger.info("🔮 Ingesting #{length(collection)} wisdom insights...")
        
        Enum.each(collection, &process_item/1)
        
      {:error, reason} ->
        Logger.error("Failed to read wisdom file: #{inspect(reason)}")
    end
  end

  defp process_item(item) do
    path = "wisdom_archive/" <> (item["file"] || "unknown_#{Ecto.UUID.generate()}.md")
    
    attrs = %{
      title: item["title"] || "Untitled Wisdom",
      content: item["content"] || "",
      format: "json_import",
      path: path,
      checksum: :crypto.hash(:sha256, item["content"] || "") |> Base.encode16(case: :lower),
      tags: (item["tags"] || []) ++ ["wisdom", "archive"],
      importance: 0.8,
      resonance: 0.5,
      created_at: parse_date(item["date"]),
      updated_at: DateTime.utc_now() |> DateTime.truncate(:second)
    }

    case Memories.get_memory_by_path(path) do
      nil -> Memories.create_memory(attrs)
      existing -> Memories.update_memory(existing, attrs)
    end
  end

  defp parse_date(nil), do: DateTime.utc_now() |> DateTime.truncate(:second)
  defp parse_date(date_str) do
    case DateTime.from_iso8601(date_str) do
      {:ok, dt, _} -> dt |> DateTime.truncate(:second)
      _ -> DateTime.utc_now() |> DateTime.truncate(:second)
    end
  end
end
