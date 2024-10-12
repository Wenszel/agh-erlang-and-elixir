defmodule PollutiondbWeb.ReadingList do
  use Phoenix.LiveComponent

  def render(assigns) do
    ~H"""
    <div>
    <h2 class="text-2xl font-bold mt-6 mb-4">Readings</h2>
    <table class="min-w-full bg-white border-collapse border border-gray-300 shadow-md rounded-lg">

      <thead class="bg-gray-200 text-gray-600 uppercase text-sm leading-normal">
        <tr>
          <th class="py-3 px-6 text-left border border-gray-300">Date</th>
          <th class="py-3 px-6 text-left border border-gray-300">Time</th>
           <th class="py-3 px-6 text-left border border-gray-300">Type</th>
          <th class="py-3 px-6 text-left border border-gray-300">Value</th>
           <th class="py-3 px-6 text-left border border-gray-300">Station</th>
       </tr>
      </thead>

      <tbody class="text-gray-700 text-sm font-light">
      <%= for reading <- @readings do %>
      <tr class="border-b border-gray-200 hover:bg-gray-100">
        <td class="py-3 px-6 text-left border border-gray-300"><%= reading.date %></td>
        <td class="py-3 px-6 text-left border border-gray-300"><%= reading.time %></td>
        <td class="py-3 px-6 text-left border border-gray-300"><%= reading.type %></td>
        <td class="py-3 px-6 text-left border border-gray-300"><%= reading.value %></td>
        <td class="py-3 px-6 text-left border border-gray-300"><%= reading.station.name %></td>
      </tr>
      <% end %>
      </tbody>
      </table>
    </div>
    """
  end
end
