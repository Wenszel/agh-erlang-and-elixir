defmodule PollutiondbWeb.ReadingLive do
  use PollutiondbWeb, :live_view

  alias Pollutiondb.Reading
  alias Pollutiondb.Station

  def mount(_params, _session, socket) do
    socket = assign(socket,
      readings: Reading.get_last_ten_readings(),
      stations: Station.get_all(),
      station_id: "",
      type: "",
      value: "",
      new_date: "",
      date: "")
    {:ok, socket}
  end

  def to_int(str, default) do
    case String.to_integer(str) do
      int -> int
      :error -> default
    end
  end

  def to_date(str) do
    case Date.from_iso8601(str) do
      {:ok, date} -> date
      :error -> Date.utc_today()
    end
  end

  def handle_event("insert", %{"station_id" => station_id, "type" => type, "value" => value, "new_date" => new_date}, socket) do
    date = to_date(new_date)
    Reading.add(%Station{id: to_int(station_id, 1)}, date, Time.utc_now(), type, String.to_float(value))
    socket = assign(socket, readings: Reading.get_last_ten_readings())
    {:noreply, socket}
  end

  def handle_event("search", %{"date" => date}, socket) do
    readings = Reading.get_last_ten_readings_by_date(to_date(date))
    socket = assign(socket, readings: readings)
    {:noreply, socket}
  end

  def render(assigns) do
    ~H"""

    <h2>Search</h2>
    <form phx-submit="insert">
      <select name="station_id">
        <%= for station <- @stations do %>
          <option label={station.name} value={station.id} selected={station.id == @station_id}/>
        <% end %>
      </select>
      Type: <input type="text" name="type" value={@type} /><br/>
      Value: <input type="number" name="value" value={@value} /><br/>
      Date: <input type="date" name="new_date" value={@new_date} /><br/>
      <input type="submit" />
    </form>
    <form phx-change="search">
    Date: <input type="date" name="date" value={@date} /><br/>
    </form>
    <table>
      <tr>
        <th>Date</th><th>Time</th><th>Type</th><th>Value</th><th>Station</th>
      </tr>
      <%= for reading <- @readings do %>
      <tr>
          <td><%= reading.date %></td>
          <td><%= reading.time %></td>
          <td><%= reading.type %></td>
          <td><%= reading.value %></td>
          <td><%= reading.station.name %></td>
        </tr>
      <% end %>
    </table>
    """
end
end
