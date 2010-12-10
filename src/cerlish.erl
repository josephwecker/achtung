-module(cerlish).
-export([main/1]).

main(_Argv) ->
  io:format("~p", [indents:full_scan("Hi there\n And there\nYeah, hi")]).
