-module(digraph_viewer).

-export([register/1]).

register(G) ->
  gen_server:cast(graph_tracker, {register, G}).