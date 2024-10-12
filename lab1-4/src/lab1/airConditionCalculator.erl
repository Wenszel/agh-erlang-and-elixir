%%%-------------------------------------------------------------------
%%% @author wiktorsmaga
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. mar 2024 1:00 PM
%%%-------------------------------------------------------------------
-module(airConditionCalculator).
-author("wiktorsmaga").

%% API
-export([get_data/0, number_of_readings/1, calculate_max/1, calculate_mean/2, increment_if_dates_equal/2]).

get_types_of_measurements() -> [pm10, pm25, pm1, temperature, humidity, pressure, test].

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


increment_if_dates_equal(Date, Date) -> 1;
increment_if_dates_equal(_, _)-> 0.

number_of_readings([], _, Count) -> Count;

number_of_readings([{_, {Date, _}, _} | T], SearchedDate, Count) ->
  number_of_readings(T, SearchedDate, Count + increment_if_dates_equal(Date, SearchedDate)).

number_of_readings(Date) -> number_of_readings(get_data(), Date, 0).


find_value_of_type([], _) -> nil;
find_value_of_type([{Type, Value} | _], Type) -> Value;
find_value_of_type([{_OtherType, _} | T], Type) -> find_value_of_type(T, Type).

find_maximum_of_type([], Maximum, _) -> Maximum;
find_maximum_of_type([{_, _, ArrayOfMeasurements} | T], Maximum, Type) ->
  case find_value_of_type(ArrayOfMeasurements, Type) of
    false ->
      find_maximum_of_type(T, Maximum, Type);
    Value when Value > Maximum ->
      find_maximum_of_type(T, Value, Type);
    Value when Value =< Maximum ->
      find_maximum_of_type(T, Maximum, Type)
  end.

find_maximum_of_type(L, Type) -> find_maximum_of_type(L, -999999, Type).

calculate_max(L, Type) -> handle_type_error(L, Type, fun find_maximum_of_type/2).
calculate_mean(L, Type) -> handle_type_error(L, Type, fun find_mean_of_type/2).
calculate_max(Type) -> calculate_max(get_data(), Type).

handle_type_error(L, Type, Function) ->
  case lists:member(Type, get_types_of_measurements()) of
    true -> Function(L, Type);
    false -> {error, "This is not a type"}
  end.

find_mean_of_type([], Total, Div, _) ->
  if
    Div == 0 -> {error, "No elements of given type"};
    true -> Total / Div
  end;

find_mean_of_type([{_, _, Array} | T], Total, Div, Type) ->
  Value = find_value_of_type(Array, Type),
  if
    Value /= nil -> find_mean_of_type(T, Total + Value, Div + 1, Type);
    _ -> find_mean_of_type(T, Total, Div, Type)
  end.
find_mean_of_type(L, Type) -> find_mean_of_type(L, 0, 0, Type).




