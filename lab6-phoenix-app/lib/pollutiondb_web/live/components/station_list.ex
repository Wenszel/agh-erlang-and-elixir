defmodule PollutiondbWeb.StationList do
  use Phoenix.LiveComponent

  def render(assigns) do
    ~H"""
    <div>
      <h2 class="text-2xl font-bold mt-6 mb-4">Stations</h2>
      <table class="min-w-full bg-white shadow-md rounded-lg overflow-scroll">
        <thead class="bg-gray-200 text-gray-600 uppercase text-sm leading-normal">
          <tr>
            <th class="py-3 px-6 text-left">Name</th>
            <th class="py-3 px-6 text-left">Longitude</th>
            <th class="py-3 px-6 text-left">Latitude</th>
          </tr>
        </thead>
        <tbody class="text-gray-700 text-sm font-light">
          <%= for station <- @stations do %>
            <tr class="border-b border-gray-200 hover:bg-gray-100">
              <td class="py-3 px-6 text-left"><%= station.name %></td>
              <td class="py-3 px-6 text-left"><%= station.lon %></td>
              <td class="py-3 px-6 text-left"><%= station.lat %></td>
            </tr>
          <% end %>
        </tbody>
      </table>
    </div>
    """
  end
end
