%%%-------------------------------------------------------------------
%%% @author wiktorsmaga
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(pollution_gen_server).

-behaviour(gen_server).

-export([
         start_link/0, 
         add_station/2, 
         add_value/4, 
         remove_value/3, 
         get_one_value/3,
         get_station_mean/2,
         get_daily_mean/2,
         get_air_quality_index/2,
         info/0, 
         crash/0
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {coordinates_to_name, name_to_measurements}).
%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

crash() ->
  gen_server:call(?SERVER, crash).

add_station(Name, Coords) -> 
  gen_server:cast(?SERVER, {add_station, Name, Coords}).

add_value(NameOrCoords, Time, Type, Value) -> 
  gen_server:cast(?SERVER, {add_value, NameOrCoords, Time, Type, Value}).

remove_value(NameOrCoords, Time, Type) -> 
  gen_server:cast(?SERVER, {remove_value, NameOrCoords, Time, Type}).

get_one_value(NameOrCoords, Time, Type) -> 
  gen_server:call(?SERVER, {get_one_value, NameOrCoords, Time, Type}).

get_station_mean(NameOrCoords, Type) -> 
  gen_server:call(?SERVER, {get_station_mean, NameOrCoords, Type}).

get_daily_mean(Type, Date) -> 
  gen_server:call(?SERVER, {get_daily_mean, Type, Date}).

get_air_quality_index(NameOrCoords, Hour) ->
  gen_server:call(?SERVER, {get_air_quality_index, NameOrCoords, Hour}).

info() -> 
  gen_server:cast(?SERVER, check_status).

init([]) ->
  {ok, pollution:create_monitor()}.

handle_cast({add_station, Name, Coords}, State) ->
  {noreply, pollution:add_station(Name, Coords, State)};

handle_cast({add_value, Coords, Time, Type, Value}, State) ->
    {noreply, pollution:add_value(Coords, Time, Type, Value, State)};

handle_cast({remove_value, NameOrCoords, Time, Type}, State) ->
    {noreply, pollution:remove_value(NameOrCoords, Time, Type, State)};

handle_cast(check_status, State) ->
  io:format("Checking status of the server. Current state: ~p~n", [State]),
  {noreply, State}.

handle_call({get_one_value, NameOrCoords, Time, Type}, _From, State) ->
    {reply, pollution:get_one_value(NameOrCoords, Time, Type, State), State};

handle_call({get_station_mean, NameOrCoords, Type}, _From, State) ->
    {reply, pollution:get_station_mean(NameOrCoords, Type, State), State};

handle_call({get_daily_mean, Type, Date}, _From, State) ->
    {reply, pollution:get_daily_mean(Type, Date, State), State};

handle_call({get_air_quality_index, NameOrCoords, Hour}, _From, State) ->
    {reply, pollution:get_air_quality_index(NameOrCoords, Hour, State), State};

handle_call(crash, _From, _State) ->
  exit(crash).

handle_info(info, State) ->
  {noreply, State}.

terminate(_Reason, _State = #state{}) ->
  ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.
