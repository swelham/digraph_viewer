-module(digraph_viewer).

-export([register/1, graph_data/0]).

register(G) ->
  gen_server:cast(graph_tracker, {register, G}).

graph_data() ->
  Graphs = call_server({list}),
  lists:map(fun({Uuid, G}) -> 
    Id = uuid:uuid_to_string(Uuid, binary_standard),
    Info = digraph:info(G),
    Nodes = collect_nodes(G),
    Links = collect_links(G),
    {[{id, Id}] ++ Info ++ [{nodes, Nodes}, {links, Links}]}
  end, Graphs).

call_server(Request) ->
   gen_server:call(graph_tracker, Request).   
   
collect_nodes(G) ->
  Nodes = digraph:vertices(G),
  lists:map(fun(Id) -> {[{id, Id}]} end, Nodes).

collect_links(G) ->
  Edges = digraph:edges(G),
  lists:map(fun(E) ->
    {_, Source, Target, _} = digraph:edge(G, E),
    {[{source, Source}, {target, Target}]}
  end, Edges).