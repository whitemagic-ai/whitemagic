defmodule WhitemagicCore.Memory do
  use Ecto.Schema
  import Ecto.Changeset

  @moduledoc """
  Ecto schema for WhiteMagic Memories.
  Maps to the unified memory structure while allowing high-performance graph queries.
  """

  @primary_key {:id, :binary_id, autogenerate: true}
  schema "memories" do
    field :title, :string
    field :content, :string
    field :format, :string, default: "markdown" # markdown, json, yaml, blob
    field :path, :string
    field :checksum, :string
    
    # Metadata
    field :importance, :float, default: 0.5
    field :resonance, :float, default: 0.0
    field :tags, {:array, :string}, default: []
    
    # Vector Embedding (pgvector compatible)
    field :embedding, {:array, :float} 

    # Relationships
    has_many :links_outgoing, WhitemagicCore.MemoryLink, foreign_key: :source_id
    has_many :links_incoming, WhitemagicCore.MemoryLink, foreign_key: :target_id

    timestamps(inserted_at: :created_at)
  end

  @doc false
  def changeset(memory, attrs) do
    memory
    |> cast(attrs, [:title, :content, :format, :path, :checksum, :importance, :resonance, :tags, :embedding])
    |> validate_required([:title, :content, :format])
    |> validate_number(:importance, greater_than_or_equal_to: 0.0, less_than_or_equal_to: 1.0)
  end
end

defmodule WhitemagicCore.MemoryLink do
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :binary_id, autogenerate: true}
  schema "memory_links" do
    field :type, :string # related, contradicts, supports, evoltion_of
    field :weight, :float, default: 1.0
    
    belongs_to :source, WhitemagicCore.Memory
    belongs_to :target, WhitemagicCore.Memory

    timestamps()
  end

  def changeset(link, attrs) do
    link
    |> cast(attrs, [:type, :weight, :source_id, :target_id])
    |> validate_required([:source_id, :target_id, :type])
  end
end
