%%%-------------------------------------------------------------------
%%% @author wiktorsmaga
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. kwi 2024 7:25 PM
%%%-------------------------------------------------------------------
-module(sensor_dist).
-author("wiktorsmaga").

%% API
-export([get_rand_locations/0, dist/2, find_for_person/2, find_closest/2, find_closest_parallel/2]).

get_rand_locations() -> [{rand:uniform(10000), rand:uniform(10000)} || _ <- lists:seq(0,1000)].

dist({X1, Y1}, {X2, Y2}) -> math:sqrt(math:pow(X1 - X2, 2) + math:pow(Y1 - Y2, 2)).

find_for_person(PersonLocation, SensorLocations) ->
  lists:min([{dist(PersonLocation, Location), PersonLocation, Location} || Location <- SensorLocations]).

find_closest(PeopleLocations, SensorLocations) ->
  lists:min([{dist(PersonLocation, SensorLocation), PersonLocation, SensorLocation} || PersonLocation <- PeopleLocations, SensorLocation <- SensorLocations]).


find_for_person(PersonLocation, SensorLocations, ParentPID) -> ParentPID ! find_for_person(PersonLocation, SensorLocations), ok.

find_closest_parallel(PersonsLocations, SensorLocations) ->
  ParentPid = self(),
  [spawn(fun() -> find_for_person(PersonLocation, SensorLocations, ParentPid) end) || PersonLocation <- PersonsLocations],
  Results = receive_results(length(PersonsLocations), []),
  Closest = lists:min(Results),
  Closest.

receive_results(0, Acc) -> Acc;
receive_results(N, Acc) ->

  receive
      R -> receive_results(N - 1, [R | Acc])
  end.