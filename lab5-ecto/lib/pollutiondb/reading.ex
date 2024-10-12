defmodule Pollutiondb.Reading do
  use Ecto.Schema
  require Ecto.Query
  schema "readings" do
    field :date, :date
    field :time, :time
    field :type, :string
    field :value, :float
    belongs_to :station, Pollutiondb.Station
  end

  def add_now(station_id, type, value) do
    %Pollutiondb.Reading{
      date: Date.utc_today(),
      time: Time.truncate(Time.utc_now(), :second),
      type: type,
      value: value,
      station_id: station_id
    }
    |> Pollutiondb.Repo.insert
  end

  def find_by_date(date) do
    Pollutiondb.Repo.all(
      Ecto.Query.where(Pollutiondb.Reading, date: ^date)
    )
  end

  def add(station, date, time, type, value) do
    %Pollutiondb.Reading{
      date: date,
      time: time,
      type: type,
      value: value,
      station_id: station.id
    }
    |> Pollutiondb.Repo.insert
  end

end
