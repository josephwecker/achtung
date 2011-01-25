%% Assumes the AST is already in (final) optimized form

-module(ungpeg_write_compiler).
-compile(export_all).


% TODO: detect modulename from AST and default Dir to ./
to_erlang(ModuleName, Dir, AST) ->
  EntryFunNames = lists:map(fun({N,_})->fmt("~p/2",[N]) end, AST),
  Code = [
    fmt("-module(~s).\n",[str(ModuleName)]),
    fmt("-export([~s]).\n",[string:join(EntryFunNames,",")]),
    erlang_code(AST)
  ],
  F = Dir ++ str(ModuleName) ++ ".erl",
  file:write_file(F, Code),
  erl_tidy:file(F, [{backups,false},{dir,Dir}]),
  F.

erlang_code(AST) -> erlang_code(AST, []).
erlang_code([], Acc) -> lists:reverse(Acc);
erlang_code([{EntryPoint, E}|R], Acc) ->
  Out1 = fmt("~s(St,Final)->~p.~n", [str(EntryPoint),inner_expr(E)]),
  erlang_code(R,[Out1|Acc]).

% TODO:
%  - ord
%  - seq
%  - xord
%  - char
%  - call
%
inner_expr({ord,Attr,Exprs}) -> ok.
  % inner: seq / xord / char / call
  % case Bin of InpMatch1 -> ordExpr1; InpMatch2 -> ordExpr2 ...
  % case 
  %

%inner_expr(E) ->


fmt(Txt) -> io_lib:format(Txt,[]).
fmt(Txt,Dat) -> io_lib:format(Txt,Dat).

str(A) when is_atom(A) -> atom_to_list(A);
str(L) when is_list(L) -> L.


