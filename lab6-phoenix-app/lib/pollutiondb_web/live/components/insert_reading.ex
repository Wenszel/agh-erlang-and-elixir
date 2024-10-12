defmodule PollutiondbWeb.InsertReading do
  use Phoenix.LiveComponent

  def render(assigns) do
    ~H"""
    <div>
      <h2 class="text-2xl font-bold mb-4">Add Reading</h2>
      <form phx-submit="insert" class="space-y-4">
        <div>
          <label class="block font-medium">Select Station</label>
          <select name="station_id" class="border rounded w-full px-2 py-1">
            <%= for station <- @stations do %>
              <option label={station.name} value={station.id} selected={station.id == @station_id} />
            <% end %>
          </select>
        </div>
        <div>
          <label class="block font-medium">Type</label>
          <input
            type="text"
            name="type"
            value={@type}
            class="border rounded w-full px-2 py-1"
            placeholder="Enter Reading Type"
          />
        </div>
        <div>
          <label class="block font-medium">Value</label>
          <input
            type="number"
            name="value"
            value={@value}
            class="border rounded w-full px-2 py-1"
            placeholder="Enter Reading Value"
          />
        </div>
        <div>
          <label class="block font-medium">Date</label>
          <input
            type="date"
            name="new_date"
            value={@new_date}
            class="border rounded w-full px-2 py-1"
          />
        </div>
        <div>
          <input
            type="submit"
            value="Add Reading"
            class="bg-gray-500 text-white px-4 py-2 rounded cursor-pointer hover:bg-blue-600"
          />
        </div>
      </form>
    </div>
    """
  end
end
