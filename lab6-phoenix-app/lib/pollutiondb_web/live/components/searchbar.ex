defmodule PollutiondbWeb.SearchBar do
  use Phoenix.LiveComponent

  def render(assigns) do
    ~H"""
    <div>
      <h2 class="text-2xl font-bold mt-6 mb-4">Search</h2>
      <form phx-change="search" class="mb-4">
        <input
          type="text"
          name="name"
          value={@name}
          class="border rounded w-full px-2 py-1"
          placeholder="Search by name"
        />
      </form>
    </div>
    """
  end
end
