%%%-------------------------------------------------------------------
%% @doc lab4app top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(lab4app_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 1,
                 period => 5},
    %% Specyfikacja dla serwera pollution_gen_server
    ChildSpecs = [
        #{id => pollution_gen_server,
          start => {pollution_gen_server, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [pollution_gen_server]}
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions