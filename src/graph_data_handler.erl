-module(graph_data_handler).

-export([init/2]).

init(Req0, State) ->
  Data = digraph_viewer:graph_data(),
  Encoded = jiffy:encode(Data),
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"application/json">>},
    Encoded,
    Req0),
{ok, Req, State}.