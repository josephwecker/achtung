-module(tags).
-compile(export_all).


orig() ->
  [{num,49},
   {next,[{op,"-"},
          {num,50},
          {next,[{op,"-"},
                 {num,51},
                 {next,[{op,"-"},
                        {num,52},
                        {next,[{op,"-"},
                               {num,53},
                               {next,[]}]}]}]}]}].
%orig2() ->

right_to_left(L,N) -> right_to_left(lists:reverse(L),N,[]).

right_to_left(L,N,Acc) when length(L) > N ->
  {Left,Right} = lists:split(N,L),
  right_to_left(Right,N,[lists:reverse(Left)|Acc]);

right_to_left(L,_N,Acc) ->
  lists:reverse([L|Acc]).
