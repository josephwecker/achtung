%% Assumes the AST is already in (final) optimized form

-module(ungpeg_write_compiler).
-compile(export_all).

-record(c,{idx=0,line=0,col=0,ch=0}).

% TODO: detect modulename from AST and default Dir to ./
to_erlang(ModuleName, Dir, AST) ->
  EntryFunNames = lists:map(fun({N,_})->f("~p/2",[N]) end, AST),
  Code = [
    f("-module(~s).\n",[str(ModuleName)]),
    f("-export([~s]).\n",[string:join(EntryFunNames,",")]),
    erlang_code(AST)
  ],
  F = Dir ++ str(ModuleName) ++ ".erl",
  file:write_file(F, Code),
  erl_tidy:file(F, [{backups,false},{dir,Dir}]),
  F.

erlang_code(AST) -> erlang_code(AST, []).
erlang_code([], Acc) -> lists:reverse(Acc);
erlang_code([{EntryPoint, E}|R], Acc) ->
  Out1 = f("~s(St,Final)->~p.~n", [str(EntryPoint),inner_expr(E)]),
  erlang_code(R,[Out1|Acc]).

% TODO:
%  - ord
%  - seq
%  - xord
%  - char
%  - call
%
inner_expr({char,Attrs,{Begin,End}}, final, final, PDrop, C) ->
  Drop = PDrop or val(Attrs,token),
  Size = case byte_size(Begin) == byte_size(End) of true->byte_size(Begin); % TODO: you are here!
  CanNL =
  f("case Bin of <<_:Idx~s/bytes,C~s,_/bytes>>"
    "when (C~s >= ~p) and (C~s =< ~p) ->"
    "{s,{Bin,
  
inner_expr({ord,Attr,Exprs}) -> ok.
  % inner: seq / xord / char / call
  % case Bin of InpMatch1 -> ordExpr1; InpMatch2 -> ordExpr2 ...
  % case 
  %

%inner_expr(E) ->


f(Txt) -> io_lib:format(Txt,[]).
f(Txt,Dat) -> io_lib:format(Txt,Dat).

str(A) when is_atom(A) -> atom_to_list(A);
str(L) when is_list(L) -> L.

val(P,Key) -> proplists:get_value(P,Key,false).
