defmodule WhitemagicCore.Memories do
  @moduledoc """
  The Memories context.
  """

  import Ecto.Query, warn: false
  alias WhitemagicCore.Repo
  alias WhitemagicCore.Memory
  alias WhitemagicCore.MemoryLink

  @doc """
  Returns the list of memories.

  ## Examples

      iex> list_memories()
      [%Memory{}, ...]

  """
  def list_memories do
    Repo.all(Memory)
  end

  @doc """
  Gets a single memory.

  Raises `Ecto.NoResultsError` if the Memory does not exist.

  ## Examples

      iex> get_memory!(123)
      %Memory{}

      iex> get_memory!(456)
      ** (Ecto.NoResultsError)

  """
  def get_memory!(id), do: Repo.get!(Memory, id)

  @doc """
  Gets a single memory by path.
  """
  def get_memory_by_path(path) do
    Repo.get_by(Memory, path: path)
  end

  @doc """
  Creates a memory.

  ## Examples

      iex> create_memory(%{field: value})
      {:ok, %Memory{}}

      iex> create_memory(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_memory(attrs \\ %{}) do
    %Memory{}
    |> Memory.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a memory.

  ## Examples

      iex> update_memory(memory, %{field: new_value})
      {:ok, %Memory{}}

      iex> update_memory(memory, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_memory(%Memory{} = memory, attrs) do
    memory
    |> Memory.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a memory.

  ## Examples

      iex> delete_memory(memory)
      {:ok, %Memory{}}

      iex> delete_memory(memory)
      {:error, %Ecto.Changeset{}}

  """
  def delete_memory(%Memory{} = memory) do
    Repo.delete(memory)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking memory changes.

  ## Examples

      iex> change_memory(memory)
      %Ecto.Changeset{data: %Memory{}}

  """
  def change_memory(%Memory{} = memory, attrs \\ %{}) do
    Memory.changeset(memory, attrs)
  end

  # Links

  def create_link(attrs \\ %{}) do
    %MemoryLink{}
    |> MemoryLink.changeset(attrs)
    |> Repo.insert()
  end

  def list_links(memory_id) do
    query = from l in MemoryLink,
      where: l.source_id == ^memory_id or l.target_id == ^memory_id,
      preload: [:source, :target]
    
    Repo.all(query)
  end
end
