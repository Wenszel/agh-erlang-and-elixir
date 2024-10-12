defmodule PollutiondbWeb.CoordsRangeSearch do
  use Phoenix.LiveComponent

  def render(assigns) do
    ~H"""
    <div>
      <h2 class="text-2xl font-bold mb-4">Change Range</h2>
      <form phx-change="change range" class="mb-8">
        <div class="grid grid-cols-2 gap-4">
          <div>
            <label class="block">
              <span class="text-gray-700">Lat min</span>
              <input type="range" min="0" max="180" name="lat_min" value={@lat_min} class="w-full" />
              <span class="block mt-2">Value: <span id="lat_min" class="font-semibold"><%= @lat_min %></span></span>
            </label>
          </div>

          <div>
            <label class="block">
              <span class="text-gray-700">Lat max</span>
              <input type="range" min="0" max="180" name="lat_max" value={@lat_max} class="w-full" />
              <span class="block mt-2">Value: <span id="lat_max" class="font-semibold"><%= @lat_max %></span></span>
            </label>
          </div>

          <div>
            <label class="block">
              <span class="text-gray-700">Lon min</span>
              <input type="range" min="0" max="180" name="lon_min" value={@lon_min} class="w-full" />
              <span class="block mt-2">Value: <span id="lon_min" class="font-semibold"><%= @lon_min %></span></span>
            </label>
          </div>

          <div>
            <label class="block">
              <span class="text-gray-700">Lon max</span>
              <input type="range" min="0" max="180" name="lon_max" value={@lon_max} class="w-full" />
              <span class="block mt-2">Value: <span id="lon_max" class="font-semibold"><%= @lon_max %></span></span>
            </label>
          </div>
        </div>
      </form>
    </div>
    """
  end
end
