%% Assumes the AST is already in (final) optimized form

-module(ungpeg_write_compiler).
-compile(export_all).

-record(c,{idx=0,line=0,col=0,ch=0,msc=0,acc=0}).

% TODO: detect modulename from AST and default Dir to ./
to_erlang(ModuleName, Dir, AST) ->
  EntryFunNames = lists:map(fun({N,_})->f("~p/1",[N]) end, AST),
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
  Out1 = f("~p({Bin,Idx0,Line0,Col0,Misc0,Acc0})->~s.~n", [EntryPoint,inner_expr(E)]),
  erlang_code(R,[Out1|Acc]).

% TODO: ord, seq, xord, char, call, lit
%       all with and without finals, tokens, predicates, etc. etc.
inner_expr({char,Attrs,Range}) ->
  inner_expr({char,Attrs,Range}, final, final, false, #c{}).
%inner_expr({seq,Attrs,Exprs}) ->
%  Drop = val(Attrs,token),
%  do_seq_expr(
  % case t1 of succ -> case t2 ...
  %            fail -> fail
  % 


% TODO:
%  - opt
%  - in_predicate

% inner_expr(Expr, fail_action, succ_action, DropResult (don't accumulate), ID-tags)
inner_expr({char,Attrs,[{Begin,End}]}, final, final, PDrop, C) ->
  SRet = case PDrop or val(Attrs,token) of
    true  -> val(Attrs,orig_name);
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
      f("{CLine~b,CCol~b}=case Ch~b of "
        "$\\n -> {Line~b+1,1}; "
        "_ -> {Line~b,Col~b+1} end, ",
        [C#c.line,C#c.col,C#c.ch,C#c.line,C#c.line,C#c.col])
  end ++
  f("{s,{Bin,Idx~b+~s,~s,~s,Misc~b},~s}; ",
    [C#c.idx,Size,SLine,SCol,C#c.msc,SRet]) ++
  f("_ -> {f, nyi} ") ++
  f("end ").


f(Txt) -> io_lib:format(Txt,[]).
f(Txt,Dat) -> io_lib:format(Txt,Dat).

str(A) when is_atom(A) -> atom_to_list(A);
str(L) when is_list(L) -> L;
str(I) when is_integer(I) -> integer_to_list(I).

val(P,Key) -> proplists:get_value(Key,P,false).
