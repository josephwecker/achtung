%% Assumes the AST is already in (final) optimized form

-module(ungpeg_gen).
-compile(export_all).

-record(c,{idx=0,line=0,col=0,ch=0,misc=0,acc=0}).

% TODO: detect modulename from AST and default Dir to ./
to_erlang(ModuleName, Dir, AST) ->
  EntryFunNames = lists:map(fun({N,_})->f("~p/1",[N]) end, AST),
  Code = [
    f("-module(~s).\n",[str(ModuleName)]),
    f("-export([~s]).\n",[string:join(EntryFunNames,",")]),
    f("-compile(nowarn_unused_vars).\n"),
    erlang_code(AST)
  ],
  F = Dir ++ str(ModuleName) ++ ".erl",
  file:write_file(F, Code),
  erl_tidy:file(F, [{backups,false},{dir,Dir},{paper,90},{ribbon,85}]),
  F.

erlang_code(AST) -> erlang_code(AST, []).
erlang_code([], Acc) -> lists:reverse(Acc);
erlang_code([{EntryPoint, E}|R], Acc) ->
  Out1 = f("~p({Bin,Idx0,Line0,Col0,Misc0,Acc0})->~s.~n", [EntryPoint,top_expr(E)]),
  erlang_code(R,[Out1|Acc]).

% TODO: ord, seq, xord, char, call, lit
%       all with and without finals, tokens, predicates, etc. etc.
top_expr({char,Attrs,Range}) ->
  inner_expr({char,Attrs,Range}, succ, fail, false, #c{}).
%top_expr({seq,Attrs,Exprs}) ->
%  Drop = val(Attrs,token),
%  do_seq_expr(
  % case t1 of succ -> case t2 ...
  %            fail -> fail
  % 


% TODO:
%  - opt
%  - in_predicate

% inner_expr(Expr, succ_action, fail_action, DropResult (don't accumulate), ID-tags)
% Relevant attributes at this point:
%  - token although parent token makes it irrelevant if it's there)
%  - orig (if token) to give it a token name
%
% ord(chr/chr/chr) and chr[r1r2r3] are the same and are both simplified into a
% single case statement

inner_expr({char,Attrs,[{Begin,End}]}, Succ, Fail, PDrop, C) ->
  IsOpt = val(Attrs,opt),
  SRet = case PDrop or val(Attrs,token) of
    true  -> f("~p",val(Attrs,orig));
    false -> f("[Ch~b|Acc~b]",[C#c.ch,C#c.acc])
  end,
  Size = case byte_size(<<Begin/utf8>>) == byte_size(<<End/utf8>>) of
    true  -> str(byte_size(<<Begin/utf8>>));
    false -> f("byte_size(<<Ch~b/utf8>>)",[C#c.ch])
  end,
  CanNL = (Begin =< $\n) and (End >= $\n),
  {SLine,SCol} = case CanNL of
    true -> {f("CLine~b",[C#c.line]),f("CCol~b",[C#c.col])};
    false-> {f("Line~b",[C#c.line]),f("Col~b+1",[C#c.col])}
  end,

  % OUTPUT
  f("case Bin of <<_:Idx~b/bytes,Ch~b/utf8,_/bytes>>", [C#c.idx, C#c.ch]) ++
  f("when (Ch~b >= ~b) and (Ch~b =< ~b) ->", [C#c.ch, Begin, C#c.ch, End]) ++
  case CanNL of
    false -> "";
    true ->
      f("{CLine~b,CCol~b}=case Ch~b of", [C#c.line, C#c.col, C#c.ch]) ++
      f("$\\n -> {Line~b+1,1};", C#c.line) ++
      f("_ -> {Line~b,Col~b+1} end,", [C#c.line, C#c.col])
  end ++
  case Succ of
    succ -> f("{s,{Bin,Idx~b+~s,~s,~s,Misc~b},~s};",
        [C#c.idx,Size,SLine,SCol,C#c.misc,SRet])
  end ++
  "_ -> " ++
  case {Fail, IsOpt} of
    {succ, _} ->
      f("{s,{Bin,Idx~b,Line~b,Col~b,Misc~b},Acc~b}",
        [C#c.idx,C#c.line,C#c.col,C#c.misc,C#c.acc]);
    {fail, true} ->
      f("{s,{Bin,Idx~b,Line~b,Col~b,Misc~b},Acc~b}",
        [C#c.idx,C#c.line,C#c.col,C#c.misc,C#c.acc]);
    {fail, false} ->
      f("{f,failed}");
    Other -> f("nyi")
  end ++
  f("end").


f(Txt) -> " " ++ io_lib:format(Txt,[]) ++ " ".
f(Txt,Dat) when not is_list(Dat) -> f(Txt,[Dat]);
f(Txt,Dat) ->
  " " ++ io_lib:format(Txt,
    [case D of D1 when is_list(D1)->lists:flatten(D1);O->O end||D<-Dat]) ++
  " ".

str(A) when is_atom(A) -> atom_to_list(A);
str(L) when is_list(L) -> L;
str(I) when is_integer(I) -> integer_to_list(I).

val(P,Key) -> proplists:get_value(Key,P,false).
