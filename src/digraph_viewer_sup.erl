%%%-------------------------------------------------------------------
%% @doc digraph_viewer top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(digraph_viewer_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 10,
  MaxSecondsBetweenRestarts = 10,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  TrackerChild = {graph_tracker, {graph_tracker, start_link, []}, Restart, Shutdown, Type, [graph_tracker]},

  {ok, {SupFlags, [TrackerChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================
