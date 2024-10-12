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
    <div class="flex flex-col lg:flex-row justify-between">
      <div class="flex flex-col w-64 shadow-md p-4 bg-gray-200 h-screen">
        <.live_component
          module={PollutiondbWeb.InsertReading}
          id="insert"
          stations={@stations}
          station_id={@station_id}
          type={@type}
          value={@value}
          new_date={@new_date}
        />
      </div>
      <div class="p-4 flex-1 justify-center">
        <.live_component module={PollutiondbWeb.DateSearch} id="datesearch" date={@date} />
        <.live_component module={PollutiondbWeb.ReadingList} id="readings" readings={@readings} />
      </div>
    </div>
    """
end
end
