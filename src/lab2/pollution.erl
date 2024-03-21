%%%-------------------------------------------------------------------
%%% @author wiktorsmaga
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. mar 2024 11:41 AM
%%%-------------------------------------------------------------------
-module(pollution).
-feature(maybe_expr,enable).
-author("wiktorsmaga").

-record(monitor, {coordinates_to_name, name_to_measurements}).
-record(measurement, {type, value, time}).

-export([create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4, get_station_mean/3, get_daily_mean/3]).

create_monitor() ->  #monitor{coordinates_to_name = #{}, name_to_measurements = #{}}.

add_station(Name, Coords, Monitor) ->
  IsName = maps:is_key(Name, Monitor#monitor.name_to_measurements),
  IsCoords = maps:is_key(Coords, Monitor#monitor.coordinates_to_name),
  case IsName orelse IsCoords of
    true -> {error, "already exisits"};
    false -> #monitor{
      name_to_measurements = maps:put(Name,[], Monitor#monitor.name_to_measurements),
      coordinates_to_name = maps:put(Coords, Name, Monitor#monitor.coordinates_to_name)
    }
  end.

handle_coordinates({Long, Lat}, Function, Monitor) ->
  case maps:find({Long, Lat}, Monitor#monitor.coordinates_to_name) of
    {ok, Name} -> Function(Name);
    error -> {error, "Station not found"}
  end.

contains_measurement(Type, Time, [#measurement{type=Type, time=Time} | _]) -> true;
contains_measurement(Type, Time, [_ | T]) -> contains_measurement(Type, Time, T);
contains_measurement(_, _, []) -> false.

get_measurement(Type, Time, [#measurement{type=Type, time=Time, value=Value} | _]) -> Value;
get_measurement(Type, Time, [_|T]) -> get_measurement(Type, Time, T);
get_measurement(_, _, []) -> {error, "Measurement not found"}.

append_measurements(Name, Measurement, Measurements, Monitor) ->
  #monitor {
    name_to_measurements = maps:put(Name, [
      #measurement{
        time = Measurement#measurement.time,
        type = Measurement#measurement.type,
        value = Measurement#measurement.value
    } ] ++ Measurements, Monitor#monitor.name_to_measurements),
    coordinates_to_name = Monitor#monitor.coordinates_to_name
  }.

add_value({Long, Lat}, Time, Type, Value, Monitor) ->
  handle_coordinates({Long, Lat}, fun (Name) -> add_value(Name, Time, Type, Value, Monitor) end, Monitor);
add_value(Name, Time, Type, Value, Monitor) ->
  maybe
    {ok, Measurements} ?= maps:find(Name, Monitor#monitor.name_to_measurements),
    false ?= contains_measurement(Type, Time, Measurements),
    append_measurements(Name, #measurement{ time = Time, type = Type, value = Value}, Measurements, Monitor)
  else
    error -> {error, "Station not found"};
    true -> {error, "Measurement already exists"}
  end.

find_measurement(Time, Type, [#measurement{type=Type, time=Time, value=Value} | _]) ->
  #measurement{type=Type, time=Time, value=Value};
find_measurement(SearchedTime, SearchedType, [#measurement{type=_, time=_} | T]) ->
  find_measurement(SearchedTime, SearchedType, T);
find_measurement(_, _, []) -> false.

remove_measurement(Time, Type, Measurements) ->
    lists:delete(find_measurement(Time, Type, Measurements), Measurements).

remove_value({Long, Lat}, Time, Type, Monitor) ->
  handle_coordinates({Long, Lat}, fun(Name) -> remove_value(Name, Time, Type, Monitor) end, Monitor);
remove_value(Name, Time, Type, Monitor) ->
  maybe
    {ok, Measurements} ?= maps:find(Name, Monitor#monitor.name_to_measurements),
    true ?= contains_measurement(Type, Time, Measurements),
    #monitor {
      name_to_measurements = maps:put(Name, remove_measurement(Time, Type, Measurements), Monitor#monitor.name_to_measurements),
      coordinates_to_name = Monitor#monitor.coordinates_to_name
    }
  else
    error -> {error, "Station not found"};
    false -> {error, "Measurement not found"}
  end.

get_one_value({Long, Lat}, Time, Type, Monitor) ->
  handle_coordinates({Long, Lat}, fun (Name) -> get_one_value(Name, Time, Type, Monitor) end, Monitor);
get_one_value(Name, Time, Type, Monitor) ->
  maybe
    {ok, Measurements} ?= maps:find(Name, Monitor#monitor.name_to_measurements),
    Value ?= get_measurement(Type, Time, Measurements),
    Value
  else
    error -> {error, "Station not found"};
    {error, Message} -> {error, Message}
  end.

safe_div(X, Y) when Y /= 0 -> X / Y;
safe_div(_, _) -> {error, "Div by 0"}.

get_station_mean({Long, Lat}, Type, Monitor) ->
  handle_coordinates({Long, Lat}, fun(Name) -> get_station_mean(Name, Type, Monitor) end, Monitor);
get_station_mean(Name, SearchedType, Monitor) ->
  maybe
    {ok, Measurements} ?= maps:find(Name, Monitor#monitor.name_to_measurements),
    {Sum, Count} = lists:foldl(fun
                        (#measurement{type=Type, value=Value}, {S, C})
                          when Type == SearchedType -> {S + Value, C+1};
                          (_, Acc)-> Acc end, {0, 0}, Measurements),
    safe_div(Sum, Count)
  else
    error -> {error, "Station not found"};
    {error, Message} -> {error, Message}
  end.

get_daily_mean(Type, Date, Monitor) ->
  {Sum, Count} = maps:fold(fun(_, Measurements, {Sum, Count}) ->
    {PartSum, PartCount} = lists:foldl(fun
                                         (M, {AccSum, AccCount})->
                                           {MDate, _} = M#measurement.time,
                                           case M#measurement.type == Type andalso MDate == Date of
                                             true -> {AccSum + M#measurement.value, AccCount + 1};
                                               false -> {AccSum, AccCount}
                                           end
                                        end, {0, 0}, Measurements),
    {Sum + PartSum, Count + PartCount}
                            end, {0, 0}, Monitor#monitor.name_to_measurements),
  safe_div(Sum, Count).