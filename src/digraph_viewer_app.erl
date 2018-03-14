%%%-------------------------------------------------------------------
%% @doc digraph_viewer public API
%% @end
%%%-------------------------------------------------------------------

-module(digraph_viewer_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  ok = application:start(ranch),
  ok = application:start(cowlib),
  ok = application:start(cowboy),
  
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", cowboy_static, {priv_file, digraph_viewer, "index.html"}},
      {"/js/[...]", cowboy_static, {priv_dir, digraph_viewer, "js"}},
      {"/css/[...]", cowboy_static, {priv_dir, digraph_viewer, "css"}}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(my_http_listener,
    [{port, 8080}],
    #{env => #{dispatch => Dispatch}}
  ),
  
  digraph_viewer_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
