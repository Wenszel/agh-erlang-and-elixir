%%%-------------------------------------------------------------------
%% @doc lab4app public API
%% @end
%%%-------------------------------------------------------------------

-module(lab4app_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lab4app_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
