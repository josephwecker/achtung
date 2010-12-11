-module(cerlish).
-export([main/1]).

main(Opts) ->
  {Flags, Files} = parse_opts([], Opts),
  lists:foreach(fun(F)->cerlish(Flags,F) end, Files).

parse_opts(Acc, ["-o", Outputdir | R]) ->
  parse_opts([{outputdir, Outputdir} | Acc], R);
parse_opts(Acc, Files) ->
  {lists:reverse(Acc), Files}.

cerlish(Flags, F) ->
  ok.
