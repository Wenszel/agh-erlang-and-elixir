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

  def handle_event("search", %{"name" => name}, socket) do
    stations = Station.get_by_name(name)
    socket = assign(socket, stations: stations)
    {:noreply, socket}
  end

  def handle_event("change range", %{"lat_min" => lat_min, "lat_max" => lat_max, "lon_min" => lon_min, "lon_max" => lon_max}, socket) do
    stations = Station.find_by_location_range(lon_min, lon_max, lat_min, lat_max)
    socket = assign(socket, stations: stations, lat_min: lat_min, lat_max: lat_max, lon_min: lon_min, lon_max: lon_max)
    {:noreply, socket}
  end

  def render(assigns) do
    ~H"""
    <div class="max-w-4xl mx-auto">
      <.live_component module={PollutiondbWeb.SearchBar} id="searchbar" name={@name} />
      <.live_component module={PollutiondbWeb.CoordsRangeSearch} id="coords_range_search" lat_min={@lat_min} lat_max={@lat_max} lon_min={@lon_min} lon_max={@lon_max} />
      <.live_component module={PollutiondbWeb.StationList} id="stations" stations={@stations} />
    </div>
    """
end
end
