-module(digraph_viewer).

-export([register/1, graph_data/0]).

register(G) ->
  gen_server:cast(graph_tracker, {register, G}).

graph_data() ->
  Graphs = call_server({list}),
  lists:map(fun({Uuid, G}) -> 
    Id = uuid:uuid_to_string(Uuid, binary_standard),
    Info = digraph:info(G),
    {[{id, Id} | Info]}
  end, Graphs).

call_server(Request) ->
   gen_server:call(graph_tracker, Request).   
   