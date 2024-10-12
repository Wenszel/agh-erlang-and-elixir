defmodule PollutiondbWeb.StationLive do
  use PollutiondbWeb, :live_view

  alias Pollutiondb.Station

  def mount(_params, _session, socket) do
    socket = assign(socket, stations: Station.get_all(), new_name: "", lat: "", lon: "", name: "")
    {:ok, socket}
  end

  def to_float(str, default) do
    case String.to_float(str) do
      float -> float
      :error -> default
    end
  end

  def handle_event("insert", %{"new_name" => new_name, "lat" => lat, "lon" => lon}, socket) do
    # IO.inspect(lat)
    # IO.inspect(lon)
    # IO.inspect(to_float(lat, 0.0))
    new_station = %Station{name: new_name, lat: to_float(lat, 0.0), lon: to_float(lon, 0.0)}
    Station.add(new_station)
    socket = assign(socket, stations: Station.get_all(), name: "", lat: "", lon: "")
    {:noreply, socket}
  end

  def handle_event("search", %{"name" => name}, socket) do
    stations = Station.get_by_name(name)
    socket = assign(socket, stations: stations)
    {:noreply, socket}
  end


  def render(assigns) do
    ~H"""

    <h2>Create new station</h2>
    <form phx-submit="insert">
      Name: <input type="text" name="new_name" value={@new_name} /><br/>
      Lat: <input type="number" name="lat" step="0.1" value={@lat} /><br/>
      Lon: <input type="number" name="lon" step="0.1" value={@lon} /><br/>
      <input type="submit" />
    </form>
    <h2>Search</h2>
    <form phx-change="search">
    <input type="text" name="name" value={@name} /><br/>
    </form>
    <table>
      <tr>
        <th>Name</th><th>Longitude</th><th>Latitude</th>
      </tr>
      <%= for station <- @stations do %>
      <tr>
          <td><%= station.name %></td>
          <td><%= station.lon %></td>
          <td><%= station.lat %></td>
        </tr>
      <% end %>
    </table>
    """
end
end
