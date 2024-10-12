%%%-------------------------------------------------------------------
%%% @author wiktorsmaga
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. mar 2024 12:07 PM
%%%-------------------------------------------------------------------
-module(power).
-author("wiktorsmaga").

%% API
-export([power/2,  power_acc/3, power_acc/2]).

%% 1. Classic example of a power function
power(_, 0) -> 1;
power(Base, Exponent) when Exponent > 0 -> Base * power(Base, Exponent - 1).

%% 2. Improved power function utilizing tail recursion
power_acc(_, 0, Acc) -> Acc;
power_acc(Base, Exponent, Acc) when Exponent > 0 -> power_acc(Base, Exponent - 1, Acc * Base).

%% 3. Added default Acc value
power_acc(Base, Exponent) -> power_acc(Base, Exponent, 1).