%%%-------------------------------------------------------------------
%%% @author wiktorsmaga
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. kwi 2024 6:20 PM
%%%-------------------------------------------------------------------
-module(ping_pong).
-author("wiktorsmaga").

%% API
-export([start/0, stop/0, play/1]).

start() ->
  spawn(fun () -> register(ping, self()), loop(ping, 0) end),
  spawn(fun () -> register(pong, self()), loop(pong, 0) end).

stop() ->
    exit(whereis(ping), shutdown),
    exit(whereis(pong), shutdown).

play(N) -> whereis(ping) ! N.

loop(Name, Total) ->
  receive
    N when N > 0->
      timer:sleep(1000),
      case Name of
        ping -> whereis(pong) ! N - 1, Total;
        pong -> whereis(ping) ! N - 1, Total
      end,
      io:format("Received a message in ~s ~B. ~n", [Name, Total]),
      loop(Name, Total + N);
    _ ->
      io:format("END~n"),
      loop(Name, Total)
  after 20000 ->
    io:format("Ping process has been idle for 20 seconds and is stopping.~n")
  end.
