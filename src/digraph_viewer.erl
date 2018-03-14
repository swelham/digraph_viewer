-module(digraph_viewer).

-export([register/1, graph_data/0]).

register(G) ->
  gen_server:cast(graph_tracker, {register, G}).

graph_data() ->
  Graphs = call_server({list}),
  lists:map(fun({Uuid, _}) -> 
    Id = uuid_to_string(Uuid),
    {[{id, Id}]}
  end, Graphs).

call_server(Request) ->
   gen_server:call(graph_tracker, Request).
   
 uuid_to_string(Uuid) ->
   {Id, _} = uuid:get_v1(Uuid),
   IdStr = uuid:uuid_to_string(Id),
   erlang:iolist_to_binary(IdStr).
   