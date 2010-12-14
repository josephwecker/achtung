-module(cerlish).
-export([main/1]).

main(Opts) ->
  {Flags, Files} = parse_opts([], Opts),
  lists:foreach(fun(F)->cerlish(Flags,F) end, Files).

parse_opts(Acc, ["-o", Outputdir | R]) ->
  parse_opts([{outputdir, Outputdir} | Acc], R);
parse_opts(Acc, Files) ->
  {lists:reverse(Acc), Files}.

cerlish(_Flags, F) ->
  {ok, DentedBin} = indents:file_scan(F),
  %io:format("~s", [DentedBin]),
  AST = erlish:parse(binary_to_list(DentedBin)),
  %AST = [{attribute,1,file,{F,1}} |
  %  erlish:parse(binary_to_list(DentedBin))],
  %io:format("~n~p~n", [compile:forms(AST)]),
  io:format("~nPARSE RESULTS FOR ~s:~n~p~n", [F, AST]),
  io:format("~n(pausing)~n",[]),
  receive
    nothing -> ok
  after 1000 -> ok
  end.
