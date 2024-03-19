%%%-------------------------------------------------------------------
%%% @author wiktorsmaga
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. mar 2024 11:41 AM
%%%-------------------------------------------------------------------
-module(pollution).
-author("wiktorsmaga").
-record(monitor, {name, coordinates, measurements}).
-record(measurement, {type, value, time}).

-export([create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4, get_station_mean/3, get_daily_mean/3]).

create_monitor() -> [].

add_station(NewName, NewCoords, Monitor) ->
  case lists:any(
    fun(#monitor{name = Name, coordinates = Coords}) ->
      NewName == Name orelse Coords == NewCoords end, Monitor)
  of
    true -> {error, "Station with this name already exists"};
    false ->
      lists:append(Monitor, [#monitor{name = NewName, coordinates = NewCoords, measurements = []}])
  end.

contains_measurement(Type, Time, [{_, Type, Value, Time} | _]) -> {measurement, Type, Value, Time};
contains_measurement(Type, Time, [_ | T]) -> contains_measurement(Type, Time, T);
contains_measurement(_, _, []) -> false.

add_value(NameOrCoords, Time, Type, Value, Monitor) ->
  IsMember = lists:any(fun(#monitor{name = Name, coordinates = Coords, measurements = Measurements}) ->
    (Name == NameOrCoords orelse Coords == NameOrCoords)
      andalso contains_measurement(Type, Time, Measurements) == false
                       end, Monitor),
  case IsMember of
    false -> {error, "Bad args"};
    _ ->
          lists:map(
          fun
            (#monitor{name = Name, coordinates = Coords, measurements = Measurements}) when NameOrCoords == Name orelse NameOrCoords == Coords ->
              #monitor{name = Name, coordinates = Coords, measurements = lists:append([#measurement{type = Type, value = Value, time = Time}], Measurements)};
              (M) -> M
          end
          , Monitor)
  end.

remove_value(NameOrCoords, Time, Type, Monitor) ->
  IsMember = lists:any(fun(#monitor{name = Name, coordinates = Coords, measurements = Measurements}) ->
    (Name == NameOrCoords orelse Coords == NameOrCoords) andalso
      not (contains_measurement(Type, Time, Measurements) == false)
                       end, Monitor),
  case IsMember of
    true ->
      lists:map(
        fun(#monitor{name = Name, coordinates = Coords, measurements = Measurements})
          when NameOrCoords == Name orelse NameOrCoords == Coords ->
          #monitor{name = Name, coordinates = Coords, measurements = lists:delete(contains_measurement(Type, Time, Measurements), Measurements)}
        end
        , Monitor);
    false -> {error, "Bad args"}
  end.

get_value_from_measurements(_, _, []) -> {error, "no"};
get_value_from_measurements(Type, Time, [#measurement{type=Type, time=Time, value=Value} | _])
  -> Value;
get_value_from_measurements(Type, Time, [#measurement{type=_Type, time=_Time} | T])
  -> get_value_from_measurements(Type, Time, T).

get_one_value(_, _, _, []) -> {error, "no"};
get_one_value(NameOrCoords, Time, Type, [#monitor{name=Name, coordinates=Coords, measurements = Measurements} | _])
  when NameOrCoords == Name orelse NameOrCoords == Coords
  ->
  get_value_from_measurements(Type, Time, Measurements);
get_one_value(NoC, Time, Type, [_|T]) -> get_one_value(NoC, Time, Type, T).

get_station_mean(_, _, []) -> {error, "no"};
get_station_mean(NameOrCoords, SearchedType, [#monitor{name=Name, coordinates=Coords, measurements = Measurements} | _])
  when NameOrCoords == Name orelse NameOrCoords == Coords
  ->
    Count = lists:foldl(fun
                          (#measurement{type=Type}, Acc) when SearchedType == Type -> Acc + 1;
                          (_, Acc) -> Acc
                        end, 0, Measurements),
    if
      Count > 0 -> lists:foldl(fun
                                 (#measurement{type=Type, value=Value}, Acc) when Type == SearchedType -> Acc + Value;
                                 (_, Acc) -> Acc
                               end, 0, Measurements) / Count;
      true -> {error, "no"}
    end;
get_station_mean(NoC, Type, [_|T]) -> get_station_mean(NoC, Type, T).

is_measurement_on_given_day(Type, {Y, M, D}, [{_, Type, Value, {{Y, M, D}, _}} | _]) -> Value;
is_measurement_on_given_day(Type, Date, [_ | T]) -> is_measurement_on_given_day(Type, Date, T);
is_measurement_on_given_day(_, _, []) -> false.


get_number_of_measurements(_, _, Acc, []) -> Acc;
get_number_of_measurements(Type, Date, Acc, [#monitor{measurements = M} |T]) ->
  IsMember = is_measurement_on_given_day(Type, Date, M),
  case IsMember of
    false -> get_number_of_measurements(Type, Date, Acc, T);
    _ -> get_number_of_measurements(Type, Date, Acc + 1, T)
  end.

sum_measurements(_, _, Acc, []) -> Acc;
sum_measurements(Type, Date, Acc, [#monitor{measurements = M} |T]) ->
  IsMember = is_measurement_on_given_day(Type, Date, M),
  case IsMember of
    false -> sum_measurements(Type, Date, Acc, T);
    Value -> sum_measurements(Type, Date, Acc + Value, T)
  end.

get_daily_mean(Type, Date, Monitor) ->
  Count = get_number_of_measurements(Type, Date, 0, Monitor),
  Sum = sum_measurements(Type, Date, 0, Monitor),
  if
    Count == 0 -> {error, "No Elements"};
    true -> Sum / Count
  end.