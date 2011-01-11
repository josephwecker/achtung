-module(memo_curry).
-compile(export_all).



space(Pos, <<" ",R/binary>>) ->
  Res = {succeed, original, R, Pos},
%  memoize_self(has_space_r1, has_space, Pos, Res),
  % transform_to(space_v2, 2)
  % curry_to(space_v3, 2)
  %add_clause("space("++Pos++",<<_,R/binary>>) -> {succeed, memoized, R, "++Pos++"}."),
  Res;
space(Pos, _) ->
  {fail, original, Pos}.


space_v2_succ(NUM, _) -> {succeed, memoized, NUM}.
space_v2_fail(NUM, _) -> {fail, memoized, NUM}.


seefun() ->
  Fun = fun() -> 2 + 2 end,
  erlang:fun_info(Fun).
  %{env, Env} = erlang:fun_info(Fun, env),
  %[Abs|_] = lists:reverse(Env),
  %Abs.
  

form(S) ->
  {ok, Toks, _} = erl_scan:string(S),
  {ok, Form} = erl_parse:parse_form(Toks),
  Form.
