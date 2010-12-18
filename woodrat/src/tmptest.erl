-module(tmptest).
-compile(export_all).


-define(MATCH1(Pos,RStart,REnd),
  begin
      <<_:Pos/binary,C:1/binary,_/binary>> = Bin,
      (C >= <<RStart>>) and (C =< <<REnd>>)
  end).



doit() ->
  {ok, Bin} = file:read_file("../test/indents-test-text.txt"),
  Size = size(Bin) - 1,
  Tests = [
    {fun()->rand_acc_1(Bin,Size,100000,[]) end, "out1.txt"},
    {fun()->rand_acc_2(Bin,Size,100000,[]) end, "out2.txt"},
    {fun()->rand_acc_3(Bin,Size,100000,[]) end, "out3.txt"}
      ],
  lists:foreach(fun({Fn,OutF}) ->
        {ok, Res} = eprof:profile(Fn),
        eprof:log(OutF),
        eprof:analyze(),
        io:format("~n~P~n", [Res,20])
    end, Tests).

% Goal: For N times, see if a random position in the binary matches
% [a-z] (maybe later [a-z]+) and return a list of results true/false


rand_acc_1(_,_,0,Acc) -> Acc;
rand_acc_1(Bin, Size, N, Acc) ->
  Pos = random:uniform(Size),
  <<_:Pos/binary,C:1/binary,_/binary>> = Bin,
  Res = (C >= <<$a>>) and (C =< <<$z>>),
  rand_acc_1(Bin, Size, N-1, [Res|Acc]).

rand_acc_2(_,_,0,Acc) -> Acc;
rand_acc_2(Bin, Size, N, Acc) ->
  Pos = random:uniform(Size),
  case Bin of
    <<_:Pos/binary,C:1/binary,_/binary>> when C >= <<$a>>, C =< <<$z>> ->
      rand_acc_2(Bin, Size, N-1, [true|Acc]);
    _ -> rand_acc_2(Bin, Size, N-1, [false|Acc])
  end.


rand_acc_3(_,_,0,Acc) -> Acc;
rand_acc_3(Bin, Size, N, Acc) ->
  Pos = random:uniform(Size),
  case binary:at(Bin, Pos) of
    L when L =< $z, L >= $a -> rand_acc_3(Bin, Size, N-1, [true|Acc]);
    _ -> rand_acc_3(Bin, Size, N-1, [false|Acc])
  end.

