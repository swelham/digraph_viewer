-module(graph_tracker).

-behaviour(gen_server).

-export([
  start_link/0,
  init/1,
  handle_call/3,
  handle_cast/2
]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  quickrand:seed(),
  {ok, []}.

handle_call(_Request, _From, State) ->
  {reply, [], State}.

handle_cast({register, G}, State) ->
  Uuid = uuid:new(self()),
  NewState = [{Uuid, G} | State],
  {noreply, NewState};
handle_cast(_Request, State) ->
  {noreply, State}.