%%%-------------------------------------------------------------------
%%% @author wiktorsmaga
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. mar 2024 1:00 PM
%%%-------------------------------------------------------------------
-module(airConditionCalculatorWithFun).
-author("wiktorsmaga").

%% API
-export([get_data/0, get_all_readings/0, get_given_type/1, calc_mean/1]).

get_data() ->
  [
    {"Aleja Pokoju", {{2024, 3, 2}, {17, 51, 11}}, [
      {pm10, 20},
      {pm25, 15},
      {temperature, 25}
    ]},
    {"Aleja Pokoju",  {{2024, 4, 2}, {17, 51, 11}}, [
      {pm10, 18},
      {pm25, 12},
      {temperature, 24},
      {humidity, 60}
    ]},
    {"Aleja Pokoju",  {{2024, 5, 2}, {17, 51, 11}}, [
      {pm10, 22},
      {pm25, 17},
      {pm1, 10},
      {pressure, 1013}
    ]},
    {"Czarnowiejska",  {{2024, 3, 2}, {17, 51, 11}}, [
      {pm10, 21},
      {pm25, 16},
      {temperature, 26}
    ]},
    {"Czarnowiejska",  {{2024, 4, 2}, {17, 51, 11}}, [
      {pm10, 19},
      {pm25, 14},
      {temperature, 23},
      {humidity, 65}
    ]},
    {"Czarnowiejska",  {{2024, 5, 2}, {17, 51, 11}}, [
      {pm10, 23},
      {pm25, 18},
      {pm1, 11},
      {pressure, 1015}
    ]},

    {"Pawia",  {{2024, 2, 2}, {17, 51, 11}}, [
      {pm10, 19},
      {pm25, 14},
      {temperature, 23}
    ]},
    {"Pawia",  {{2024, 4, 2}, {17, 51, 11}}, [
      {pm10, 17},
      {pm25, 13},
      {temperature, 22},
      {humidity, 62}
    ]},
    {"Pawia",  {{2024, 4, 2}, {17, 51, 11}}, [
      {pm10, 20},
      {pm25, 15},
      {pm1, 9},
      {pressure, 1010}
    ]}
  ].

get_all_readings() -> lists:foldl(
  fun(X, Acc) ->
    Acc ++ X end, [],
    lists:map(fun({_, _, Array}) -> Array end, get_data())).

get_given_type(SearchedType) -> lists:filter(fun({Type, _}) -> SearchedType == Type end, get_all_readings()).

calc_mean(Type) ->
  Values = get_given_type(Type),
  lists:foldl(fun ({_, Val}, Acc) -> Acc+Val end, 0, Values) / length(Values).


