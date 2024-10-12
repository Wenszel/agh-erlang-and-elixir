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
    <div class="flex flex-col lg:flex-row justify-between">
      <div class="flex flex-col w-64 shadow-md p-4 bg-gray-200">
        <.live_component module={PollutiondbWeb.InsertStation} id="insert" new_name={@new_name} lat={@lat} lon={@lon} />
      </div>
      <div class="p-4 flex-1 justify-center">
        <.live_component module={PollutiondbWeb.SearchBar} id="searchbar" name={@name} />
        <.live_component module={PollutiondbWeb.StationList} id="stations" stations={@stations} />
      </div>
    </div>
    """
  end
end
