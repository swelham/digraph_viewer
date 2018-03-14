-module(graph_server).

-export([start_server/0]).

start_server() ->
  ok = application:start(ranch),
  ok = application:start(cowlib),
  ok = application:start(cowboy),
  
  Dispatch = cowboy_router:compile([
      {'_', [
        % {"/", cowboy_static, {priv_file, test_harness, "index.html"}},
        % {"/js/[...]", cowboy_static, {priv_dir, test_harness, "js"}},
        % {"/css/[...]", cowboy_static, {priv_dir, test_harness, "css"}},
        {"/data", graph_data_handler, []}
        % {"/graph/[...]", graph_handler, []}
      ]}
  ]),
  {ok, _} = cowboy:start_clear(my_http_listener,
      [{port, 8080}],
      #{env => #{dispatch => Dispatch}}
  ),
  ok.