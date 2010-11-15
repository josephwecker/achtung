-module(some_server).
-extends(p_gen_server).
-export([start/0, handle_cast/2]).

start() ->
  %                  Type, Name, Module, Opts
  %p_gen_server:start(local, some_server, ?MODULE, [])
  io:format("Starting up ~p", [?MODULE]),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

handle_cast("hiya", State) ->
  io:format("Well Hi back at you!", []),
  {noreply, State}.

