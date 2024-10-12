defmodule PollutiondbWeb.StationRangeLive do
  use PollutiondbWeb, :live_view

  alias Pollutiondb.Station

  def mount(_params, _session, socket) do
    socket = assign(socket,
    stations: Station.get_all(),
        new_name: "",
        lat: "",
        lon: "",
        name: "",
        lat_min: 0.0,
        lat_max: 20.0,
        lon_min: 0.0,
        lon_max: 20.0)
    {:ok, socket}
  end

  def to_float(str, default) do
    case String.to_float(str) do
      float -> float
      :error -> default
    end
  end

  def handle_event("insert", %{"new_name" => new_name, "lat" => lat, "lon" => lon}, socket) do
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

  def handle_event("update", %{"lat_min" => lat_min, "lat_max" => lat_max, "lon_min" => lon_min, "lon_max" => lon_max}, socket) do
    stations = Station.find_by_location_range(lon_min, lon_max, lat_min, lat_max)
    socket = assign(socket, stations: stations, lat_min: lat_min, lat_max: lat_max, lon_min: lon_min, lon_max: lon_max)
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
    <form phx-change="update">
      Lat min
      <input type="range" min="0" max="180" name="lat_min" value={@lat_min}/>
      value: <span id="lat_min"><%= @lat_min %></span><br/>
      Lat max
      <input type="range" min="0" max="180" name="lat_max" value={@lat_max}/><br/>
      value: <span id="lat_max"><%= @lat_max %></span><br/>
      Lon min
      <input type="range" min="0" max="180" name="lon_min" value={@lon_min}/><br/>
      value: <span id="lon_min"><%= @lon_min %></span><br/>
      Lon max
      <input type="range" min="0" max="180" name="lon_max" value={@lon_max}/><br/>
      value: <span id="lon_max"><%= @lon_max %></span><br/>
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
