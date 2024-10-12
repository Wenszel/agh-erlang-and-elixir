defmodule Parser do
  def parse_single_line(line) do
    [date, type, value, id, name, coords] = line |> String.split(";")

    %{
      :date =>
        {date
         |> String.slice(0..9)
         |> String.split("-")
         |> Enum.map(&String.to_integer(&1))
         |> List.to_tuple(),
        date
        |> String.slice(11..18)
        |> String.split(":")
        |> Enum.map(&String.to_integer(&1))
        |> List.to_tuple()},
      :type => String.to_atom(type),
      :value => String.to_float(value),
      :id => String.to_integer(id),
      :name => name,
      :coords =>
        coords
        |> String.split(",")
        |> Enum.map(&String.to_float(&1))
        |> List.to_tuple()
    }
  end

  def identify_stations(data) do
    data
    |> Enum.map(& {&1.id, &1.name, &1.coords})
    |> Enum.uniq()
    |> Enum.map(fn {id, name, {lat, long}} -> {"#{id} #{name}", lat, long} end)
  end


  def add_stations() do
    path = Path.expand("./data.csv", __DIR__)
    File.read!(path)
      |> String.split("\n")
      |> Enum.filter(&(&1 != ""))
      |> Enum.map(&Parser.parse_single_line/1)
      |> Parser.identify_stations()
      |> Enum.each(fn {name, lat, long} -> Pollutiondb.Station.add(name, long, lat) end)
  end

  def add_measurements() do
    path = Path.expand("./data.csv", __DIR__)
    File.read!(path)
      |> String.split("\n")
      |> Enum.filter(&(&1 != ""))
      |> Enum.map(&Parser.parse_single_line/1)
      |> Enum.each(fn %{date: {date, time}, type: type, value: value, name: name, id: id} ->
        station = Pollutiondb.Station.get_by_name("#{id} #{name}")
        Pollutiondb.Reading.add(station, Date.from_erl!(date), Time.from_erl!(time), Atom.to_string(type), value)
#        Pollutiondb.Reading.add(station, date, time, Atom.to_string(type), value)
      end)
  end
end