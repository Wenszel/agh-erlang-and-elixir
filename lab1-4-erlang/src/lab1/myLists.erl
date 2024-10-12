%%%-------------------------------------------------------------------
%%% @author wiktorsmaga
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Module implementing functions operating on lists.
%%%
%%% @end
%%% Created : 04 Mar 2024 11:46 AM
%%%-------------------------------------------------------------------
-module(myLists).
-author("wiktorsmaga").

%% API
-export([contains/2, duplicateElements/1, duplicateElementsAcc/1, sumFloat/1, sumFloatAcc/1]).


%% Implement the function contains/2, which checks if a value is in the list.
contains([], _) -> false;
contains([Val | _], Val) -> true;
contains([_ | T], Val) -> contains(T, Val).

%% Implement the function duplicateElements/1, which duplicates every element in a list.
duplicateElements([]) -> [];
duplicateElements([H | T]) -> [H, H] ++ duplicateElements(T).

%% Implement the function duplicateElementsAcc/1, which duplicates every element in a list using tail recursion.
duplicateElementsAcc([], L) -> L;
duplicateElementsAcc([H | T], L) -> duplicateElementsAcc(T,  L ++ [H, H]).
duplicateElementsAcc(L) -> duplicateElementsAcc(L, []).

%% Implement the function sumFloat/1, which sums up all floats in a list.
sumFloat([]) -> 0.0;
sumFloat([H | T]) when is_float(H) -> H + sumFloat(T);
sumFloat([_ | T]) -> sumFloat(T).

%% Implement the function sumFloatAcc/1, which sums up all floats in a list using tail recursion.
sumFloatAcc([], Sum) -> Sum;
sumFloatAcc([H | T], Sum) when is_float(H) -> sumFloatAcc(T, Sum + H);
sumFloatAcc([_ | T], Sum) -> sumFloatAcc(T, Sum).
sumFloatAcc(T) -> sumFloatAcc(T, 0.0).
