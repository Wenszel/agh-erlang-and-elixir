%%%-------------------------------------------------------------------
%%% @author wiktorsmaga
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. kwi 2024 12:19 PM
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("wiktorsmaga").

%% API
-export([
  start/0,
  add/2,
  add_value/4,
  get/0,
  stop/0,
  remove_value/3,
  get_one_value/3,
  get_station_mean/2,
  get_daily_mean/2,
  get_air_quality_index/2
]).

start() -> spawn(
  fun() ->
    register(server, self()),
    loop(pollution:create_monitor())
  end).

loop(Monitor) ->
    receive
      {add, Name, Coords} -> loop(pollution:add_station(Name, Coords, Monitor));
      {add_value, Name, Time, Type, Value} -> loop(pollution:add_value(Name, Time, Type, Value, Monitor));
      {remove_value, Name, Time, Type} -> loop(pollution:remove_value(Name, Time, Type, Monitor));
      {get_one_value, Name, Time, Type} -> loop(pollution:get_one_value(Name, Time, Type, Monitor));
      {get_station_mean, Name, Type} -> loop(pollution:get_station_mean(Name, Type, Monitor));
      {get_daily_mean, Time, Type} -> loop(pollution:get_daily_mean(Time, Type, Monitor));
      {get_air_quality_index, Time, Type} -> loop(pollution:get_air_quality_index(Time, Type, Monitor));
      get -> io:format("Monitor: ~p", [Monitor]), loop(Monitor);
      stop -> ok
    end.

add(Name, Coords) -> server ! {add, Name, Coords}.
get() -> server ! get.
stop() -> server ! stop.
add_value(Name, Time, Type, Value) -> server ! {add_value, Name, Time, Type, Value}.
remove_value(Name, Time, Type) -> server ! {remove_value, Name, Time, Type}.
get_one_value(Name, Time, Type) -> server ! {get_one_value, Name, Time, Type}.
get_station_mean(Name, Type) -> server ! {get_station_mean, Name, Type}.
get_daily_mean(Time, Type) -> server ! {get_daily_mean, Time, Type}.
get_air_quality_index(Time, Type) -> server ! {get_air_quality_index, Time, Type}.


