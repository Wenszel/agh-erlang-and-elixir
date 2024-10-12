defmodule Pollutiondb do
  @moduledoc """
  Documentation for `Pollutiondb`.
  """

  @doc """
  Hello world.

  ## Examples

      iex> Pollutiondb.hello()
      :world

  """
  def hello do
    stations = [
       %Pollutiondb.Station{name: "s1", lon: 1.1, lat: 1.1},
       %Pollutiondb.Station{name: "s2", lon: 2.1, lat: 2.1},
       %Pollutiondb.Station{name: "s3", lon: 3.1, lat: 3.1},
       %Pollutiondb.Station{name: "s4", lon: 4.1, lat: 4.1}
    ]
    Enum.each(stations, fn station -> Pollutiondb.Station.add(station) end)

    :world
  end
end
