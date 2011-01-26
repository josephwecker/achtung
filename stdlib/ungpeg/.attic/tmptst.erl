-module(tmptst).
-compile(export_all).

test() ->
  {ok, Bin} = file:read_file("test/binmatch.txt"),

  % warmup
  do_test_1(Bin,0,[]),
  do_test_2(Bin,0,[]),

  io:format("~p~n",[byte_size(Bin)]),
  % test
  fprof:trace(start),
  do_test_2(Bin,0,[]),
  do_test_1(Bin,0,[]),
  do_test_1(Bin,0,[]),
  do_test_2(Bin,0,[]),
  do_test_2(Bin,0,[]),
  do_test_1(Bin,0,[]),
  do_test_2(Bin,0,[]),
  do_test_1(Bin,0,[]),
  do_test_1(Bin,0,[]),
  do_test_2(Bin,0,[]),
  do_test_2(Bin,0,[]),
  do_test_1(Bin,0,[]),
  %io:format("~p",[do_test_2(Bin,0,[])]),
  fprof:trace(stop),
  fprof:profile(),
  fprof:analyse([no_callers,{dest,"binmatch_res.erl"},{cols, 80},{totals,true}]).



% BEST ATM
do_test_1(Bin,Idx,Acc) ->
  case Bin of
    <<_:Idx/bytes, 97:8, _/bytes>>  -> do_test_1(Bin,Idx+1,[$a|Acc]);
    <<_:Idx/bytes, 98:8, _/bytes>>  -> do_test_1(Bin,Idx+1,[$b|Acc]);
    <<_:Idx/bytes, 1667523942:32, _/bytes>>  -> do_test_1(Bin,Idx+4,[$c|Acc]);
    <<_:Idx/bytes, Ch2/utf8, _/bytes>> ->
      do_test_1(Bin,Idx+byte_size(<<Ch2/utf8>>),[{fail,Ch2}|Acc]);
    _ -> {done, Idx, eof, lists:reverse(Acc)}
  end.

do_test_2(Bin,Idx,Acc) ->
  case Bin of
    <<_:Idx/bytes,97:8,_/bytes>>  -> do_test_2(Bin,Idx+1,[$a|Acc]);
    <<_:Idx/bytes,98:8,_/bytes>>  -> do_test_2(Bin,Idx+1,[$b|Acc]);
    <<_:Idx/bytes,1667523942:32, _/bytes>>  -> do_test_2(Bin,Idx+4,[$c|Acc]);
    <<_:Idx/bytes, Ch2/utf8, _/bytes>> ->
      do_test_2(Bin,Idx+byte_size(<<Ch2/utf8>>),[{fail,Ch2}|Acc]);
    <<_:Idx/bytes>> -> {done, Idx, eof, lists:reverse(Acc)}
  end.

