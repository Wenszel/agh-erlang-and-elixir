defmodule Pollutiondb.Station do
  use Ecto.Schema
  import Ecto.Query, only: [from: 2]
  alias Pollutiondb.Repo
  alias Pollutiondb.Station
  alias Pollutiondb.Reading

  schema "stations" do
    field :name, :string
    field :lon, :float
    field :lat, :float
    has_many :readings, Reading
  end

  def add(%Station{} = station) do
    station
    |> changeset(%{name: station.name, lon: station.lon, lat: station.lat})
    |> Repo.insert()
  end

  def add(name, lon, lat) do
    %Station{}
    |> changeset(%{name: name, lon: lon, lat: lat})
    |> Repo.insert()
  end

  def changeset(station, changesmap) do
    station
    |> Ecto.Changeset.cast(changesmap, [:name, :lon, :lat])
    |> Ecto.Changeset.validate_required([:name, :lon, :lat])
    |> Ecto.Changeset.validate_number(:lon, greater_than_or_equal_to: -180, less_than_or_equal_to: 180)
    |> Ecto.Changeset.validate_number(:lat, greater_than_or_equal_to: -90, less_than_or_equal_to: 90)
  end

  def get_all() do
    Repo.all(Station)
  end

  def get_by_name(name) do
    pattern = "%#{name}%"
    Repo.all(from s in Station, where: like(s.name, ^pattern))
  end

  def get_by_id(id) do
    Repo.get(Station, id)
  end

  def remove(id) do
    station = get_by_id(id)
    Repo.delete(station)
  end

  def find_by_location(lon, lat) do
    from(s in Station,
      where: s.lon == ^lon,
      where: s.lat == ^lat)
    |> Repo.all()
  end

  def find_by_location_range(lon_min, lon_max, lat_min, lat_max) do
    from(s in Station,
      where: s.lon >= ^lon_min and s.lon <= ^lon_max,
      where: s.lat >= ^lat_min and s.lat <= ^lat_max)
    |> Repo.all()
  end

  def update_name(station, newname) do
    station
    |> Ecto.Changeset.cast(%{name: newname}, [:name])
    |> Ecto.Changeset.validate_required([:name])
    |> Repo.update()
  end
end
