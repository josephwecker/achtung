-module(p_gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0, stop/0, stop/1, hi/0, server_name/0]).

server_name() -> ?MODULE.

start() ->
  io:format("Starting up ~p", [server_name()]),
  gen_server:start_link({local, server_name()}, server_name(), [], []).
stop() ->
  gen_server:stop(server_name()).
hi() ->
  io:format("~p", [module_info()]).
stop(Module) ->
  gen_server:cast(Module, stop).
init(_) ->
  {ok, []}.
handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};
handle_call(Request, _From, State) ->
  error_logger:warning_msg("~p: Unhandled call: ~p", [server_name(), Request]),
  {reply, ok, State}.

handle_cast(Msg, State) ->
  error_logger:warning_msg("~p: Unhandled cast: ~p", [server_name(), Msg]),
  {noreply, State}.

handle_info(Info, State) ->
  error_logger:warning_msg("~p: Unhandled info: ~p", [server_name(), Info]),
  {noreply, State}.
terminate(_Reason, _State) ->
  ok.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

