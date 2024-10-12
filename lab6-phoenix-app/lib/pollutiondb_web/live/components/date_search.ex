defmodule PollutiondbWeb.DateSearch do
  use Phoenix.LiveComponent

  def render(assigns) do
    ~H"""
    <div>
      <h2 class="text-2xl font-bold mb-4">Search by Date</h2>
      <form phx-change="search" class="space-y-4">
        <div>
          <label class="block font-medium">Date</label>
          <input
            type="date"
            name="date"
            value={@date}
            class="border rounded w-full px-3 py-2"
          />
        </div>
      </form>
    </div>
    """
  end
end
