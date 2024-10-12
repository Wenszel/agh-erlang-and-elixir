defmodule PollutiondbWeb.InsertStation do
  use Phoenix.LiveComponent

  def render(assigns) do
    ~H"""
    <div>
      <h2 class="text-2xl font-bold mb-4">Create new station</h2>
      <form phx-submit="insert" class="space-y-4">
        <div>
          <label class="block font-medium">Name</label>
          <input type="text" name="new_name" value={@new_name} class="border rounded w-full px-2 py-1" placeholder="Enter Station name" />
        </div>
        <div>
          <label class="block font-medium">Latitude</label>
          <input type="number" name="lat" step="0.1" value={@lat} class="border rounded w-full px-2 py-1" placeholder="Enter Latitude" />
        </div>
        <div>
          <label class="block font-medium">Longitude</label>
          <input type="number" name="lon" step="0.1" value={@lon} class="border rounded w-full px-2 py-1" placeholder="Enter Longitude" />
        </div>
        <div>
          <input type="submit" value="Add Station" class="bg-gray-500 text-white px-4 py-2 rounded cursor-pointer hover:bg-blue-600" />
        </div>
      </form>
    </div>
    """
  end
end
