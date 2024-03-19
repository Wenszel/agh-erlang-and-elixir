%%%-------------------------------------------------------------------
%%% @author wiktorsmaga
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. mar 2024 1:01 PM
%%%-------------------------------------------------------------------
-module(qsort).
-author("wiktorsmaga").

%% API
-export([qsort/1, lesser_than/2, greater_than/2, random_elems/3, compare_speeds/3]).

qsort([]) -> [];
qsort([Pivot | Tail]) -> qsort(lesser_than(Tail, Pivot)) ++ [Pivot] ++ qsort(greater_than(Tail, Pivot)).
lesser_than(L, Pivot) -> [X || X <- L, X < Pivot].
greater_than(L, Pivot) -> [X || X <- L, X >= Pivot].

random_elems(N, Min, Max) -> [rand:uniform(Max - Min + 1) + Min - 1 || _<-lists:seq(1, N)].

compare_speeds(List, Fun1, Fun2) ->
  {SpeedOne, _} = timer:tc(Fun1, [List]),
  {SpeedTwo, _}   = timer:tc(Fun2, [List]),
  if
    SpeedOne > SpeedTwo -> two;
    SpeedOne < SpeedTwo -> one;
    SpeedOne == SpeedTwo -> equal
  end.

%%AnonimFun = fun (Str) -> lists:map((X)-> case X of "o" -> "a"; "e" -> "o"; _ -> X end, Str) end.

