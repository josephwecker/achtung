-module(tmptst).
-compile(export_all).
-record(person, {name, count=0, count2=0}).

-define(P, P#person).
-define(P_(I), P#person{I}).

-define(MATCHER(M,Succ,Fail),
  begin
      L__ = length(M),
      B__ = list_to_binary(M),
      fun(<<A__:L__/binary,_/binary>>) when A__=:=B__ -> Succ;
        (_) -> Fail
      end
  end).

%-define(MATCH(M), ?MATCHER(M,true,false)).
-define(MATCH(M), bin_matcher(M,true,false)).

-define(INC(Rec,Fld), Rec#person{Fld = Rec#person.Fld + 1}).

bin_matcher(M,Succ,Fail) ->
  B = iolist_to_binary(M),
  L = byte_size(B),
  fun(<<A:L/binary,_/binary>>) when A=:=B->Succ;(_)->Fail end.

do_it() ->
  R = #person{},
  R2 = ?INC(R,count),
  R3 = ?INC(R2,count2),
  io:format("~p~n", [R3]).

another() ->
  P = #person{},
  P2 = ?P{name="wow", count=?P.count - 23},
  io:format("~p~n---~n~p~n", [?P.count2, P2]).

third() ->
  %F = ?MATCH($#, "HI!"),
  S = "#",
  S2 = "hey",
  F = ?MATCH(S),
  F2 = ?MATCH(S2),
  F3 = ?MATCH(S ++ S2),
  io:format("~p~n", [F(<<"#he">>)]),
  io:format("~p~n", [F(<<"another">>)]),
  io:format("~p~n", [F2(<<"hey">>)]),
  io:format("~p~n", [F2(<<>>)]),
  io:format("~p~n", [F3(<<"#hey">>)]),
  io:format("~p~n", [F3(<<"#|hey">>)]).

