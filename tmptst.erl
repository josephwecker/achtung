-module(tmptst).
-compile(export_all).
-record(person, {name, count=0, count2=0}).

-define(P, P#person).
-define(P_(I), P#person{I}).
-define(INC(Rec,Fld), Rec#person{Fld = Rec#person.Fld + 1}).
-define(MATCHER(M,Succ,Fail), fun(<<M,_/binary>>)->Succ; (_)->Fail end).
-define(MATCH(M), ?MATCHER(M,true,false)).
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
  F = ?MATCH($#),
  F2 = ?MATCH("hey"),
  io:format("~p~n~p~n~p~n~p~n",
    [F(<<"#hey">>), F(<<"another">>),
      F2(<<"hey">>), F2(<<>>)]).
